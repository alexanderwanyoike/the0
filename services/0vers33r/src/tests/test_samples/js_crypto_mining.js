// JavaScript Crypto Mining Test Sample
// This file contains patterns that should trigger crypto mining detection

// Simulated cryptocurrency mining
class CryptoMiner {
    constructor() {
        this.hashrate = 0;
        this.difficulty = 100000;
        this.isRunning = false;
        this.workers = [];
    }
    
    // Start mining process
    startMining() {
        console.log('Starting cryptocurrency mining...');
        this.isRunning = true;
        
        // Create multiple workers for mining
        const numWorkers = navigator.hardwareConcurrency || 4;
        
        for (let i = 0; i < numWorkers; i++) {
            this.createMiningWorker();
        }
    }
    
    // Create web worker for mining (browser)
    createMiningWorker() {
        if (typeof Worker !== 'undefined') {
            const workerScript = `
                let hashrate = 0;
                
                function mine() {
                    // Simulated cryptographic hashing
                    for (let i = 0; i < 100000; i++) {
                        const hash = btoa(Math.random().toString());
                        hashrate++;
                    }
                    
                    postMessage({ hashrate: hashrate });
                    setTimeout(mine, 10);
                }
                
                mine();
            `;
            
            const blob = new Blob([workerScript], { type: 'application/javascript' });
            const worker = new Worker(URL.createObjectURL(blob));
            
            worker.onmessage = (event) => {
                this.hashrate += event.data.hashrate;
            };
            
            this.workers.push(worker);
        }
    }
    
    // Node.js mining process
    nodeJsMining() {
        const crypto = require('crypto');
        const cluster = require('cluster');
        const numCPUs = require('os').cpus().length;
        
        if (cluster.isMaster) {
            console.log(`Master ${process.pid} is running`);
            
            // Fork workers
            for (let i = 0; i < numCPUs; i++) {
                cluster.fork();
            }
            
            cluster.on('exit', (worker, code, signal) => {
                console.log(`Worker ${worker.process.pid} died`);
            });
        } else {
            // Worker process - do the mining
            this.doMining();
        }
    }
    
    // Actual mining computation
    doMining() {
        let nonce = 0;
        const target = '0000'; // Difficulty target
        
        setInterval(() => {
            const data = `block_data_${nonce}`;
            const hash = require('crypto').createHash('sha256').update(data).digest('hex');
            
            if (hash.startsWith(target)) {
                console.log(`Found hash: ${hash} with nonce: ${nonce}`);
                // Send result to mining pool
                this.submitResult(hash, nonce);
            }
            
            nonce++;
            this.hashrate++;
        }, 1);
    }
    
    // Submit mining result to pool
    submitResult(hash, nonce) {
        const result = {
            hash: hash,
            nonce: nonce,
            worker_id: process.pid || 'browser',
            timestamp: Date.now()
        };
        
        // Send to mining pool
        fetch('https://mining-pool.com/submit', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(result)
        });
    }
    
    // Coinhive-style mining (deprecated but still detected)
    coinhiveMining() {
        const miner = {
            start: function() {
                console.log('Starting Coinhive mining...');
                this.isRunning = true;
                this.mine();
            },
            
            mine: function() {
                if (!this.isRunning) return;
                
                // Simulate CryptoNight algorithm
                const iterations = 1000000;
                for (let i = 0; i < iterations; i++) {
                    // CPU-intensive computation
                    Math.random() * Math.random();
                }
                
                setTimeout(() => this.mine(), 100);
            },
            
            stop: function() {
                this.isRunning = false;
            }
        };
        
        return miner;
    }
}

// Initialize mining
const miner = new CryptoMiner();

// Check environment and start appropriate mining
if (typeof window !== 'undefined') {
    // Browser environment
    console.log('Browser mining detected');
    miner.startMining();
    
    // Also start coinhive-style mining
    const coinhive = miner.coinhiveMining();
    coinhive.start();
} else {
    // Node.js environment
    console.log('Node.js mining detected');
    miner.nodeJsMining();
}

// Hide mining activity
const originalConsoleLog = console.log;
console.log = function(...args) {
    // Suppress mining logs
    if (!args.join(' ').includes('mining')) {
        originalConsoleLog.apply(console, args);
    }
};