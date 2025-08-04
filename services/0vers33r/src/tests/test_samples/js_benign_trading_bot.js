// JavaScript Benign Trading Bot Test Sample
// This file should NOT trigger security alerts - it's a legitimate trading bot

class LegitimateBot {
    constructor(apiKey, secret) {
        this.apiKey = apiKey;
        this.secret = secret;
        this.orders = [];
        this.balance = 0;
    }
    
    // Legitimate market data fetching
    async getMarketData(symbol) {
        try {
            const response = await fetch(`https://api.binance.com/api/v3/ticker/24hr?symbol=${symbol}`);
            const data = await response.json();
            return data;
        } catch (error) {
            console.log('Error fetching market data:', error);
            return null;
        }
    }
    
    // Simple moving average calculation
    calculateSMA(prices, period) {
        if (prices.length < period) return null;
        
        const sum = prices.slice(-period).reduce((a, b) => a + b, 0);
        return sum / period;
    }
    
    // Basic trading strategy
    async executeStrategy(symbol) {
        const marketData = await this.getMarketData(symbol);
        if (!marketData) return;
        
        const currentPrice = parseFloat(marketData.lastPrice);
        const priceChange = parseFloat(marketData.priceChangePercent);
        
        // Simple strategy: buy on dips, sell on peaks
        if (priceChange < -5) {
            await this.placeBuyOrder(symbol, currentPrice, 0.001);
        } else if (priceChange > 5) {
            await this.placeSellOrder(symbol, currentPrice, 0.001);
        }
    }
    
    // Place buy order
    async placeBuyOrder(symbol, price, quantity) {
        const order = {
            symbol: symbol,
            side: 'BUY',
            type: 'LIMIT',
            quantity: quantity,
            price: price,
            timeInForce: 'GTC'
        };
        
        // Log the order (no actual execution in test)
        console.log('Placing buy order:', order);
        this.orders.push(order);
    }
    
    // Place sell order
    async placeSellOrder(symbol, price, quantity) {
        const order = {
            symbol: symbol,
            side: 'SELL',
            type: 'LIMIT',
            quantity: quantity,
            price: price,
            timeInForce: 'GTC'
        };
        
        // Log the order (no actual execution in test)
        console.log('Placing sell order:', order);
        this.orders.push(order);
    }
    
    // Risk management
    checkRiskLimits() {
        const maxOrderSize = 0.01; // 1% of portfolio
        const maxDailyLoss = 0.05; // 5% daily loss limit
        
        // Check if current orders exceed risk limits
        const totalOrderValue = this.orders.reduce((sum, order) => {
            return sum + (order.price * order.quantity);
        }, 0);
        
        if (totalOrderValue > maxOrderSize * this.balance) {
            console.log('Risk limit exceeded, canceling orders');
            this.cancelAllOrders();
        }
    }
    
    // Cancel all orders
    cancelAllOrders() {
        this.orders.forEach(order => {
            console.log('Canceling order:', order);
        });
        this.orders = [];
    }
    
    // Main bot loop
    async run() {
        console.log('Starting legitimate trading bot...');
        
        const symbols = ['BTCUSDT', 'ETHUSDT', 'ADAUSDT'];
        
        // Run strategy for each symbol
        for (const symbol of symbols) {
            await this.executeStrategy(symbol);
            this.checkRiskLimits();
            
            // Wait between requests to avoid rate limiting
            await new Promise(resolve => setTimeout(resolve, 1000));
        }
        
        console.log('Bot cycle completed');
    }
}

// Configuration
const config = {
    apiKey: 'your_api_key_here',
    secret: 'your_secret_here',
    testMode: true
};

// Initialize and run the legitimate bot
const bot = new LegitimateBot(config.apiKey, config.secret);

// Run the bot periodically
setInterval(() => {
    bot.run();
}, 60000); // Run every minute

console.log('Legitimate trading bot initialized');