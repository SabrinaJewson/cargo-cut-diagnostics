# cargo-cut-diagnostics

A cargo subcommand that displays only the first page of diagnostics.

## Installation

```
cargo install --git https://github.com/SabrinaJewson/cargo-cut-diagnostics
```

## Usage

```
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
```

## See also

This project was inspired by [cargo-firstpage](https://github.com/cecton/cargo-firstpage), however:
- It features more command line options.
- It doesn't cause the native Cargo progress bar to disappear.
- I found it to handle Cargo output more reliably in general.
