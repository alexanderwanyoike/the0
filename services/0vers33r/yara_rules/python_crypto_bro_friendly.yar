/*
   FIXED: Trading Bot Security Rules - Crypto Bro Friendly
   Only flag actually suspicious patterns, not normal trading operations
*/

rule Trading_Bot_Hardcoded_Credentials {
    meta:
        description = "Detects hardcoded API keys/secrets in trading bots"
        severity = "critical"
        author = "0VERS33R"

    strings:
        // Hardcoded API credentials (actual security risk)
        $api_key_hardcoded = /api[_-]?key\s*=\s*["'][a-zA-Z0-9]{20,64}["']/ nocase
        $secret_hardcoded = /secret[_-]?key\s*=\s*["'][a-zA-Z0-9]{20,64}["']/ nocase
        $binance_key = /binance.*["'][a-zA-Z0-9]{64}["']/ nocase
        $coinbase_key = /coinbase.*["'][a-zA-Z0-9]{32}["']/ nocase

        // Exchange mentions (context)
        $exchange_context = /binance|coinbase|kraken|ftx|bybit|okx/ nocase

    condition:
        $exchange_context and any of ($api_key_hardcoded, $secret_hardcoded, $binance_key, $coinbase_key)
}

rule Trading_Bot_Suspicious_Withdrawals {
    meta:
        description = "Detects suspicious withdrawal patterns (not normal trading)"
        severity = "high"
        author = "0VERS33R"

    strings:
        // Withdrawal operations
        $withdraw = "withdraw" nocase
        $send_funds = "send" nocase
        $transfer_funds = "transfer" nocase

        // Suspicious patterns (not normal for trading bots)
        $hardcoded_address = /["'][13][a-km-zA-HJ-NP-Z1-9]{25,34}["']/ nocase  // Bitcoin address
        $eth_address = /["']0x[a-fA-F0-9]{40}["']/ nocase  // Ethereum address
        $withdraw_all = "withdraw.*all" nocase
        $drain_balance = "balance.*withdraw" nocase
        $external_address = "external_address" nocase

        // Exchange context
        $exchange = /binance|coinbase|kraken/ nocase

    condition:
        $exchange and
        any of ($withdraw, $send_funds, $transfer_funds) and
        (any of ($hardcoded_address, $eth_address) or any of ($withdraw_all, $drain_balance, $external_address))
}

rule Trading_Bot_Market_Manipulation {
    meta:
        description = "Detects actual market manipulation schemes"
        severity = "high"
        author = "0VERS33R"

    strings:
        // Pump and dump indicators (actual coordination)
        $pump_signal = "pump_signal" nocase
        $dump_signal = "dump_signal" nocase
        $telegram_pump = "telegram.*pump" nocase
        $discord_signal = "discord.*signal" nocase
        $coordinated_buy = "coordinated.*buy" nocase

        // Wash trading patterns
        $wash_trading = "wash.*trading" nocase
        $fake_volume = "fake.*volume" nocase
        $self_trade = "self.*trade" nocase

        // Front-running (specific implementation)
        $mempool_monitor = "mempool" nocase
        $pending_tx = "pending.*transaction" nocase
        $frontrun_logic = "frontrun" nocase
        $mev_bot = "mev" nocase

        // Trading context
        $trading_context = /buy|sell|order|trade/ nocase

    condition:
        $trading_context and
        (any of ($pump_signal, $dump_signal, $telegram_pump, $discord_signal, $coordinated_buy) or
         any of ($wash_trading, $fake_volume, $self_trade) or
         ($mempool_monitor and ($pending_tx or $frontrun_logic or $mev_bot)))
}

rule Trading_Bot_Private_Key_Exposure {
    meta:
        description = "Detects direct private key handling (dangerous for users)"
        severity = "critical"
        author = "0VERS33R"

    strings:
        // Private key patterns (should use exchange APIs, not direct wallet access)
        $private_key = "private_key" nocase
        $priv_key = "priv_key" nocase
        $wallet_seed = "seed_phrase" nocase
        $mnemonic = "mnemonic" nocase

        // Wallet manipulation
        $import_wallet = "import.*wallet" nocase
        $create_wallet = "create.*wallet" nocase
        $wallet_from_key = "from_private_key" nocase

        // Crypto libraries that handle private keys
        $web3_account = "web3.*Account" nocase
        $eth_account = "eth_account" nocase
        $bitcoin_lib = "bitcoin" nocase

    condition:
        (any of ($private_key, $priv_key, $wallet_seed, $mnemonic) or
         any of ($import_wallet, $create_wallet, $wallet_from_key)) and
        any of ($web3_account, $eth_account, $bitcoin_lib)
}

rule Trading_Bot_Credential_Theft {
    meta:
        description = "Detects credential stealing from trading platforms"
        severity = "critical"
        author = "0VERS33R"

    strings:
        // Browser credential access
        $chrome_cookies = "chrome.*cookies" nocase
        $firefox_logins = "firefox.*login" nocase
        $browser_data = "browser.*data" nocase

        // Exchange-specific credential theft
        $exchange_cookies = "exchange.*cookie" nocase
        $trading_session = "session.*token" nocase
        $auth_bypass = "bypass.*auth" nocase

        // File operations on credentials
        $sqlite_login = "sqlite.*login" nocase
        $decrypt_password = "decrypt.*password" nocase
        $steal_keys = "steal.*key" nocase

        // Exchange context
        $exchange_context = /binance|coinbase|kraken|metamask|phantom/ nocase

    condition:
        $exchange_context and
        (any of ($chrome_cookies, $firefox_logins, $browser_data) or
         any of ($exchange_cookies, $trading_session, $auth_bypass) or
         any of ($sqlite_login, $decrypt_password, $steal_keys))
}

rule Trading_Bot_Legitimate_Patterns {
    meta:
        description = "Whitelist legitimate trading bot patterns (lower scoring)"
        severity = "low"
        author = "0VERS33R"

    strings:
        // Normal trading operations (these are GOOD)
        $ccxt_lib = "import ccxt" nocase
        $exchange_api = "exchange.create_" nocase
        $fetch_balance = "fetch_balance" nocase
        $fetch_orders = "fetch_orders" nocase
        $create_order = "create_order" nocase

        // Proper API usage
        $api_from_env = "os.getenv.*API" nocase
        $config_file = "config" nocase
        $env_vars = "environ" nocase

        // Risk management (good practices)
        $stop_loss = "stop_loss" nocase
        $risk_management = "risk.*management" nocase
        $position_size = "position.*size" nocase

    condition:
        any of ($ccxt_lib, $exchange_api, $fetch_balance, $fetch_orders, $create_order) and
        (any of ($api_from_env, $config_file, $env_vars) or
         any of ($stop_loss, $risk_management, $position_size))
}