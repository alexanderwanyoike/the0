/*
   JavaScript Cryptocurrency and Trading Related Rules
   More lenient rules for crypto/trading bots while still catching malicious patterns
   Severity: MEDIUM/LOW - Allow legitimate crypto trading while catching abuse
*/

rule JS_Crypto_API_Excessive_Calls {
    meta:
        description = "Detects excessive API calls that might indicate abuse"
        severity = "medium"
        author = "0VERS33R"

    strings:
        $api1 = "binance" nocase
        $api2 = "coinbase" nocase
        $api3 = "kraken" nocase
        $api4 = "bitfinex" nocase
        $loop1 = "setInterval(" nocase
        $loop2 = "setTimeout(" nocase
        $loop3 = "while(" nocase
        $loop4 = "for(" nocase
        $fast1 = /\d+/ // Any number
        $fast2 = "100" 
        $fast3 = "1000"

    condition:
        1 of ($api1, $api2, $api3, $api4) and 
        2 of ($loop1, $loop2, $loop3, $loop4) and
        ($fast1 or $fast2 or $fast3) // Very fast intervals
}

rule JS_Unauthorized_Trading {
    meta:
        description = "Detects patterns of unauthorized trading activities"
        severity = "high"
        author = "0VERS33R"

    strings:
        $trade1 = "buy" nocase
        $trade2 = "sell" nocase
        $trade3 = "order" nocase
        $trade4 = "execute" nocase
        $auth1 = "api_key" nocase
        $auth2 = "apikey" nocase
        $auth3 = "secret" nocase
        $auth4 = "private_key" nocase
        $steal1 = "document.cookie" nocase
        $steal2 = "localStorage" nocase
        $steal3 = "sessionStorage" nocase

    condition:
        2 of ($trade1, $trade2, $trade3, $trade4) and 
        1 of ($auth1, $auth2, $auth3, $auth4) and 
        1 of ($steal1, $steal2, $steal3)
}

rule JS_Crypto_Wallet_Manipulation {
    meta:
        description = "Detects wallet manipulation attempts"
        severity = "critical"
        author = "0VERS33R"

    strings:
        $wallet1 = "wallet" nocase
        $wallet2 = "address" nocase
        $wallet3 = "private_key" nocase
        $wallet4 = "mnemonic" nocase
        $wallet5 = "seed" nocase
        $crypto1 = "bitcoin" nocase
        $crypto2 = "ethereum" nocase
        $crypto3 = "btc" nocase
        $crypto4 = "eth" nocase
        $action1 = "transfer" nocase
        $action2 = "send" nocase
        $action3 = "withdraw" nocase

    condition:
        2 of ($wallet1, $wallet2, $wallet3, $wallet4, $wallet5) and 
        1 of ($crypto1, $crypto2, $crypto3, $crypto4) and 
        1 of ($action1, $action2, $action3)
}

rule JS_Pump_Dump_Coordination {
    meta:
        description = "Detects pump and dump coordination patterns"
        severity = "high"
        author = "0VERS33R"

    strings:
        $coord1 = "pump" nocase
        $coord2 = "dump" nocase
        $coord3 = "signal" nocase
        $coord4 = "group" nocase
        $coord5 = "discord" nocase
        $coord6 = "telegram" nocase
        $coord7 = "slack" nocase
        $market1 = "volume" nocase
        $market2 = "price" nocase
        $market3 = "manipulation" nocase

    condition:
        2 of ($coord1, $coord2, $coord3, $coord4) and 
        1 of ($coord5, $coord6, $coord7) and 
        1 of ($market1, $market2, $market3)
}

rule JS_Fake_Trading_Bot {
    meta:
        description = "Detects fake trading bot patterns"
        severity = "medium"
        author = "0VERS33R"

    strings:
        $fake1 = "guaranteed" nocase
        $fake2 = "100%" nocase
        $fake3 = "risk-free" nocase
        $fake4 = "profit" nocase
        $fake5 = "returns" nocase
        $scam1 = "investment" nocase
        $scam2 = "deposit" nocase
        $scam3 = "withdraw" nocase
        $urgent1 = "limited time" nocase
        $urgent2 = "act now" nocase
        $urgent3 = "urgent" nocase

    condition:
        2 of ($fake1, $fake2, $fake3, $fake4, $fake5) and 
        1 of ($scam1, $scam2, $scam3) and 
        1 of ($urgent1, $urgent2, $urgent3)
}

rule JS_DeFi_Exploit_Patterns {
    meta:
        description = "Detects DeFi exploit patterns"
        severity = "high"
        author = "0VERS33R"

    strings:
        $defi1 = "uniswap" nocase
        $defi2 = "pancakeswap" nocase
        $defi3 = "compound" nocase
        $defi4 = "aave" nocase
        $exploit1 = "flashloan" nocase
        $exploit2 = "reentrancy" nocase
        $exploit3 = "overflow" nocase
        $exploit4 = "sandwich" nocase
        $exploit5 = "frontrun" nocase

    condition:
        1 of ($defi1, $defi2, $defi3, $defi4) and 
        2 of ($exploit1, $exploit2, $exploit3, $exploit4, $exploit5)
}

rule JS_MEV_Bot_Suspicious {
    meta:
        description = "Detects suspicious MEV (Maximal Extractable Value) bot patterns"
        severity = "medium"
        author = "0VERS33R"

    strings:
        $mev1 = "mev" nocase
        $mev2 = "arbitrage" nocase
        $mev3 = "frontrun" nocase
        $mev4 = "backrun" nocase
        $mev5 = "sandwich" nocase
        $eth1 = "ethereum" nocase
        $eth2 = "web3" nocase
        $eth3 = "ethers" nocase
        $gas1 = "gasPrice" nocase
        $gas2 = "gasLimit" nocase
        $excessive1 = "1000000" // Very high gas
        $excessive2 = "999999"

    condition:
        2 of ($mev1, $mev2, $mev3, $mev4, $mev5) and 
        1 of ($eth1, $eth2, $eth3) and 
        1 of ($gas1, $gas2) and 
        1 of ($excessive1, $excessive2)
}

rule JS_Rug_Pull_Indicators {
    meta:
        description = "Detects rug pull indicators in trading bots"
        severity = "critical"
        author = "0VERS33R"

    strings:
        $rug1 = "liquidity" nocase
        $rug2 = "remove" nocase
        $rug3 = "withdraw" nocase
        $rug4 = "drain" nocase
        $token1 = "token" nocase
        $token2 = "contract" nocase
        $owner1 = "owner" nocase
        $owner2 = "admin" nocase
        $owner3 = "onlyOwner" nocase
        $stealth1 = "renounce" nocase
        $stealth2 = "burn" nocase

    condition:
        2 of ($rug1, $rug2, $rug3, $rug4) and 
        1 of ($token1, $token2) and 
        1 of ($owner1, $owner2, $owner3) and 
        1 of ($stealth1, $stealth2)
}