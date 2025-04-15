#![allow(clippy::items_after_test_module)]

const HELP_INNER: &str = r#"
Cargo subcommand that displays only the first page of diagnostics.

USAGE:
    cargo cut-diagnostics [OPTIONS] [SUBCOMMAND] [args]...

OPTIONS:
    --max-height height
        Set the maximum height the diagnostics will reach before being cut.
        Defaults to the height of the terminal minus 4. Mutually exclusive with
        --lines-around.
    --lines-around lines
        Set the number of lines around the diagnostics. The maximum height of
        the diagnostics will be the terminal height minus this value. Defaults
        to 4. Mutually exclusive with --max-height.
    --width width
        Set how many characters wide the terminal will be considered to be for
        the purposes of calculating diagnostic height. Defaults to the width of
        the terminal.
    --help, -h
        Display this help message and exit.
    --
        Stop processing command line arguments and treat the next argument as a
        subcommand.

EXAMPLES:
    Common usage with `cargo check`:
        cargo cut-diagnostics check
    Run clippy on every target in the package:
        cargo cut-diagnostics clippy --all-targets
"#;
const HELP: &str = HELP_INNER.trim_ascii_start();

/// A list of Cargo subcommands that show diagnostics and support `--message-format`.
///
/// Some Cargo subcommands can show diagnostics but do not support that option:
/// - `cargo fmt`
/// - `cargo install`
/// - `cargo package`
/// - `cargo publish`
///
/// Users are not likely to encouter diagnostics with these commands anyway however, so it's fine
/// to not attempt to cut off their output.
const SHOWS_DIAGNOSTICS: &[&[&str]] = &[
    &["b"],
    &["bench"],
    &["build"],
    &["c"],
    &["check"],
    &["clippy"],
    &["d"],
    &["doc"],
    &["fix"],
    &["init"],
    &["miri", "r"],
    &["miri", "run"],
    &["miri", "t"],
    &["miri", "test"],
    &["r"],
    &["run"],
    &["rustc"],
    &["rustdoc"],
    &["t"],
    &["test"],
];

/// A list of Cargo subcommands that show diagnostics, run an executable, and support `--quiet` and
/// `--no-run`. The `run` and `r` commands themselves are special-cased.
const SUPPORTS_NO_RUN: &[&[&str]] = &[
    &["b"],
    &["bench"],
    &["miri", "t"],
    &["miri", "test"],
    &["t"],
    &["test"],
];

const SPECIAL_FLAGS: &[&str] = &["--help", "-h", "--version", "-V"];

#[derive(Debug, PartialEq)]
struct Opts {
    width: Option<NonZero<u16>>,
    max_height: MaxHeight,
    subcommand: Vec<String>,
    subcommand_args: Vec<String>,
}

#[derive(Debug, PartialEq)]
enum MaxHeight {
    Absolute(u16),
    LinesAround(u16),
}

impl Default for MaxHeight {
    fn default() -> Self {
        Self::LinesAround(4)
    }
}

fn main() {
    let opts = match parse_opts(env::args()) {
        Ok(Some(opts)) => opts,
        Ok(None) => {
            print!("{}", HELP);
            return;
        }
        Err(e) => {
            eprintln!("error: {:?}", e);
            eprintln!("\nFor more information try --help");
            std::process::exit(2);
        }
    };
    std::process::exit(match cargo_cut_diagnostics(opts) {
        Ok(status) => status.code().unwrap_or_default(),
        Err(e) => {
            eprintln!("error: {:?}", e);
            1
        }
    });
}

fn cargo_cut_diagnostics(opts: Opts) -> anyhow::Result<ExitStatus> {
    let cargo = env::var_os("CARGO").unwrap_or_else(|| "cargo".into());

    let subcommand_flags = opts.subcommand_args.iter().take_while(|arg| *arg != "--");

    // First, only do anything at all if
    // - the command supports `--message-format`, and
    // - we're not running something like `build --help`.
    let should_override = SHOWS_DIAGNOSTICS
        .iter()
        .any(|&subcommand| subcommand == opts.subcommand)
        && subcommand_flags
            .clone()
            .all(|arg| !SPECIAL_FLAGS.contains(&&**arg));

    if !should_override {
        return Command::new(cargo)
            .args(&opts.subcommand)
            .args(&opts.subcommand_args)
            .status()
            .context("starting passthrough Cargo command");
    }

    let mut command = Command::new(cargo.clone());

    // If the command normally runs an executable and we can avoid running that executable, then do
    // that. This is so that running the executable can inherit the TTY instead of thinking that
    // stdout is a pipe. Note that we don't cut the warnings diagnostics in this case, but that is
    // probably what the user wants.
    let supports_no_run = SUPPORTS_NO_RUN
        .iter()
        .any(|&subcommand| subcommand == opts.subcommand)
        && subcommand_flags.clone().all(|flag| flag != "--no-run");
    let no_run = if supports_no_run {
        command.args(&opts.subcommand);
        command.arg("--no-run");
        true
    } else if opts.subcommand == ["run"] || opts.subcommand == ["r"] {
        command.arg("build");
        true
    } else {
        command.args(&opts.subcommand);
        false
    };

    let color = env::var_os("NO_COLOR").is_none()
        && (env::var_os("CLICOLOR_FORCE").is_some() || io::stdout().is_terminal());

    command.arg(if color {
        "--message-format=json-diagnostic-rendered-ansi"
    } else {
        "--message-format=json"
    });
    command.stdout(Stdio::piped());
    command.args(&opts.subcommand_args);

    let mut child = command.spawn().context("starting cargo command")?;
    let stdout = child.stdout.take().unwrap();

    // Read all the diagnostics from the command's stdout.
    let mut stdout = BufReader::new(stdout);
    let mut diagnostics = String::new();

    let mut buf = String::new();
    while stdout.read_line(&mut buf)? > 0 {
        match serde_json::from_str(&buf) {
            Ok(Message::CompilerMessage(msg)) => {
                let rendered = msg
                    .message
                    .rendered
                    .context("rustc did not provide rendered message")?;
                diagnostics.push_str(&rendered);
            }
            Ok(Message::BuildFinished(_)) => break,
            Ok(_) => {}
            Err(e) => {
                bail!(anyhow!(e).context("could not deserialize Cargo message"));
            }
        }
        buf.clear();
    }

    // Most Cargo commands shouldn't be printing stuff that isn't diagnostics to stdout, since
    // we pass in `--no-run` for the commands that support it. The exception is things like
    // `cargo miri run`, since there's no `cargo miri build`; in this case, we print the
    // diagnostics after the command has finished instead of the usual before. We keep track of how
    // many newlines are printed to avoid overflowing the terminal height.
    let mut emitted_lines = 0;
    let mut real_stdout = io::stdout().lock();
    loop {
        let buf = match stdout.fill_buf() {
            Ok([]) => break,
            Ok(buf) => buf,
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(e) => bail!(e),
        };
        emitted_lines += memchr_iter(b'\n', buf).count();
        real_stdout.write_all(buf)?;
        let len = buf.len();
        stdout.consume(len);
    }

    // Stdout is closed, so wait for the child to finish before printing diagnostics.
    let status = child.wait()?;

    if status.success() && no_run {
        // If we succeeded and now need to actually run the binary/tests/benchmarks, then we
        // swallow diagnostics because they're just warnings and will be printed again when we run
        // the normal command (even with --quiet).
        Command::new(cargo)
            .args(&opts.subcommand)
            .arg("--quiet")
            .args(&opts.subcommand_args)
            .status()
            .context("starting quiet Cargo command")
    } else {
        // Otherwise, there are errors to print and nothing else to run.

        // Removing trailing newlines avoids double-newlines at the end of the output.
        if diagnostics.ends_with('\n') {
            diagnostics.pop();
        }

        if !diagnostics.is_empty() {
            let mut stderr = io::stderr().lock();

            let mut terminal_size = None;
            let mut get_terminal_size = || -> anyhow::Result<_> {
                Ok(match terminal_size {
                    Some(size) => size,
                    None => {
                        let (terminal_size::Width(width), terminal_size::Height(height)) =
                            terminal_size_of(&stderr).context("could not get terminal size")?;
                        terminal_size = Some((width, height));
                        (width, height)
                    }
                })
            };

            let width = match opts.width {
                Some(width) => width.get(),
                None => get_terminal_size()?.0,
            };

            let max_height = match opts.max_height {
                MaxHeight::Absolute(max_height) => max_height,
                MaxHeight::LinesAround(lines) => get_terminal_size()?.1.saturating_sub(lines),
            };
            let max_height = usize::from(max_height).saturating_sub(emitted_lines);

            let lines = textwrap::wrap(&diagnostics, usize::from(width));
            for line in lines.iter().take(max_height) {
                writeln!(stderr, "{}", line)?;
            }
        }

        Ok(status)
    }
}

fn parse_opts(args: impl Iterator<Item = String>) -> anyhow::Result<Option<Opts>> {
    let mut args = args.skip(1).peekable();
    // Support running both as `cargo-cut-diagnostics` and as `cargo cut-diagnostics`.
    args.next_if(|x| x.as_str() == "cut-diagnostics");

    let mut max_height = None;
    let mut width = None;

    let subcommand = loop {
        let arg = args.next().context("no subcommand given")?;
        if !arg.starts_with('-') {
            break arg;
        }
        let mut parts = arg.splitn(2, '=');
        let option = parts.next().unwrap();

        let mut next = None;
        let is_fn_once = String::new();
        let value = || {
            let _is_fn_once = is_fn_once;
            parts.next().unwrap_or_else(|| {
                next = args.next();
                next.as_deref().unwrap_or_default()
            })
        };

        match option {
            "--max-height" => {
                ensure!(
                    max_height.is_none(),
                    "--max-height or --lines-around option given more than once"
                );
                let value = value();
                ensure!(!value.is_empty(), "argument to --max-height missing");
                let new_max_height = value
                    .parse()
                    .context("argument to --max-height not a number")?;
                max_height = Some(MaxHeight::Absolute(new_max_height));
            }
            "--lines-around" => {
                ensure!(
                    max_height.is_none(),
                    "--max-height or --lines-around option given more than once"
                );
                let value = value();
                ensure!(!value.is_empty(), "argument to --lines-around missing");
                let lines_around = value
                    .parse()
                    .context("argument to --lines-around not a number")?;
                max_height = Some(MaxHeight::LinesAround(lines_around));
            }
            "--width" => {
                ensure!(width.is_none(), "--width given more than once");
                let value = value();
                ensure!(!value.is_empty(), "argument to --width missing");
                width = Some(value.parse().context("argument to --width invalid")?);
            }
            "--help" | "-h" => {
                return Ok(None);
            }
            "--" => break args.next().context("no subcommand given")?,
            unrecognized => {
                bail!("unrecognized option '{}'", unrecognized);
            }
        }
    };

    let subcommand = if subcommand != "miri" {
        vec![subcommand]
    } else if let Some(miri_subcommand) = args.next() {
        vec![subcommand, miri_subcommand]
    } else {
        // Allow Miri to show its own error here.
        vec![subcommand]
    };

    Ok(Some(Opts {
        width,
        max_height: max_height.unwrap_or_default(),
        subcommand,
        subcommand_args: args.collect(),
    }))
}

#[cfg(test)]
mod tests {
    use crate::{MaxHeight, Opts};

    #[track_caller]
    fn parse_opts<I>(args: I) -> Option<Opts>
    where
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        super::parse_opts(args.into_iter().map(|s| s.as_ref().to_owned())).unwrap()
    }

    #[test]
    fn program_name_ignored() {
        assert_eq!(
            parse_opts(["", "cut-diagnostics", "build"]),
            parse_opts(["", "build"])
        );
    }

    #[test]
    fn opt_max_height() {
        let opts = parse_opts(["", "--max-height", "10", "check"]).unwrap();
        assert_eq!(opts, parse_opts(["", "--max-height=10", "check"]).unwrap());
        assert_eq!(opts.max_height, MaxHeight::Absolute(10));
        assert_eq!(opts.subcommand, ["check"]);
        assert_eq!(opts.subcommand_args, <Vec<String>>::new());
    }

    #[test]
    fn opt_lines_around() {
        let opts = parse_opts(["", "--lines-around", "2", "check"]).unwrap();
        assert_eq!(opts, parse_opts(["", "--lines-around=2", "check"]).unwrap());
        assert_eq!(opts.max_height, MaxHeight::LinesAround(2));
        assert_eq!(opts.subcommand, ["check"]);
        assert_eq!(opts.subcommand_args, <Vec<String>>::new());
    }

    #[test]
    fn subcommand_args() {
        let opts = parse_opts(["", "sub", "-arg1", "arg2"]).unwrap();
        assert_eq!(opts.max_height, MaxHeight::LinesAround(4));
        assert_eq!(opts.subcommand, ["sub"]);
        assert_eq!(opts.subcommand_args, vec!["-arg1", "arg2"]);
    }

    #[test]
    fn double_dash() {
        let opts = parse_opts(["", "--", "--lines-around=3"]).unwrap();
        assert_eq!(opts.subcommand, ["--lines-around=3"]);
        assert_eq!(opts.subcommand_args, <Vec<String>>::new());
    }

    #[test]
    fn miri() {
        let opts = parse_opts(["", "miri", "subcommand", "args"]).unwrap();
        assert_eq!(opts.subcommand, ["miri", "subcommand"]);
        assert_eq!(opts.subcommand_args, ["args"]);

        let opts = parse_opts(["", "--", "miri", "subcommand"]).unwrap();
        assert_eq!(opts.subcommand, ["miri", "subcommand"]);
        assert_eq!(opts.subcommand_args, <Vec<String>>::new());
    }

    #[test]
    fn help() {
        assert_eq!(parse_opts(["", "-h"]), None);
        assert_eq!(parse_opts(["", "--help"]), None);
        assert_eq!(parse_opts(["", "--lines-around=5", "--help"]), None);
        assert_eq!(parse_opts(["", "-h", "subcommand", "args"]), None);
    }
}

use anyhow::anyhow;
use anyhow::bail;
use anyhow::ensure;
use anyhow::Context as _;
use cargo_metadata::Message;
use memchr::memchr_iter;
use std::env;
use std::io;
use std::io::BufRead as _;
use std::io::BufReader;
use std::io::IsTerminal;
use std::io::Write as _;
use std::num::NonZero;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Stdio;
use std::str;
use terminal_size::terminal_size_of;
