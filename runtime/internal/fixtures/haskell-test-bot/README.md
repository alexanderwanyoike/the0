# Haskell Test Bot Fixture

This is a minimal Haskell bot for integration testing.

## Building the Binary

To run integration tests, you need to pre-build the binary:

```bash
cd runtime/internal/fixtures/haskell-test-bot
cabal build --enable-optimization=2
```

The binary will be created at:
`dist-newstyle/build/x86_64-linux/ghc-9.6.*/haskell-test-bot-0.1.0.0/x/haskell-test-bot/build/haskell-test-bot/haskell-test-bot`

## Structure

```
haskell-test-bot/
├── haskell-test-bot.cabal  # Package definition
├── app/
│   └── Main.hs             # Entry point
├── dist-newstyle/          # Build output (after building)
└── README.md
```
