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

/// A list of Cargo subcommands that show diagnostics and support
/// `--message-format=json-diagnostic-rendered-ansi`.
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

const SPECIAL_ARGS: &[&str] = &["--help", "-h", "--version", "-V"];

#[derive(Debug, PartialEq)]
struct Opts {
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
    let opts = match parse_opts(std::env::args()) {
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
    let mut command = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));

    command.args(&opts.subcommand);

    if SHOWS_DIAGNOSTICS
        .iter()
        .any(|&subcommand| subcommand == opts.subcommand)
        && opts
            .subcommand_args
            .iter()
            .take_while(|arg| *arg != "--")
            .all(|arg| !SPECIAL_ARGS.contains(&&**arg))
    {
        command.arg("--message-format=json-diagnostic-rendered-ansi");
        // Unfortunately this will disable colors for `cargo test` and `cargo run`. We can't just
        // pass `--color=always` in because they might not be using the default test harness.
        command.stdout(Stdio::piped());
    }

    command.args(&opts.subcommand_args);

    let mut child = command.spawn().context("could not start cargo command")?;

    // `child.stdout` can be `None` if it doesn't show diagnostics.
    if let Some(stdout) = child.stdout.take() {
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

        // Now that Cargo has finished emitting its diagnostics, pipe the rest of stdout directly.
        io::copy(&mut stdout, &mut io::stdout().lock())?;

        // Removing trailing newlines avoids double-newlines at the end of the output.
        if diagnostics.ends_with('\n') {
            diagnostics.pop();
        }

        if !diagnostics.is_empty() {
            let (terminal_size::Width(width), terminal_size::Height(height)) =
                terminal_size().context("could not get terminal size")?;

            let max_height = match opts.max_height {
                MaxHeight::Absolute(max_height) => max_height,
                MaxHeight::LinesAround(lines) => height.saturating_sub(lines),
            };

            let lines = textwrap::wrap(&diagnostics, usize::from(width));
            let stderr = io::stderr();
            let mut stderr = stderr.lock();
            for line in &lines[..cmp::min(lines.len(), usize::from(max_height))] {
                writeln!(stderr, "{}", line)?;
            }
        }
    }

    Ok(child.wait()?)
}

fn parse_opts(args: impl Iterator<Item = String>) -> anyhow::Result<Option<Opts>> {
    let mut args = args.skip(1).peekable();
    // Support running both as `cargo-cut-diagnostics` and as `cargo cut-diagnostics`.
    args.next_if(|x| x.as_str() == "cut-diagnostics");

    let mut max_height = None;

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
use std::cmp;
use std::io;
use std::io::BufRead as _;
use std::io::BufReader;
use std::io::Write as _;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Stdio;
use std::str;
use terminal_size::terminal_size;
