//! Runs every bot contract case in sdk/contract/cases against this SDK.
//!
//! The case format and the matching rules are in sdk/contract/README.md.
//!
//! This test binary is also the contract bot: the harness starts it again with
//! `contract-bot` as its first argument, and it then plays one case. Being one
//! binary means the bot needs no build step of its own, and the harness owns
//! `main`, so the test runner prints nothing on the bot's stdout.
//!
//! The cases describe a Linux container (absolute `/` paths, SIGTERM, signal
//! exit statuses), so the harness is built on Unix only. Elsewhere the binary
//! still needs a `main`, and it runs nothing.

#[cfg(unix)]
mod contract_bot;
#[cfg(unix)]
mod harness;

#[cfg(unix)]
fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.get(1).map(String::as_str) == Some(harness::CONTRACT_BOT_ARG) {
        contract_bot::main(&args[2], &args[3]);
        return;
    }
    harness::run();
}

#[cfg(not(unix))]
fn main() {}
