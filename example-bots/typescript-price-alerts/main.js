var __create = Object.create;
var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __getProtoOf = Object.getPrototypeOf;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, { get: all[name], enumerable: true });
};
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toESM = (mod, isNodeMode, target) => (target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
  // If the importer is in node compatibility mode or this is not an ESM
  // file that has been converted to a CommonJS file using a Babel-
  // compatible transform (i.e. "__esModule" has not been set), then set
  // "default" to the CommonJS "module.exports" for node compatibility.
  isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target,
  mod
));
var __toCommonJS = (mod) => __copyProps(__defProp({}, "__esModule", { value: true }), mod);

// main.ts
var main_exports = {};
__export(main_exports, {
  main: () => main
});
module.exports = __toCommonJS(main_exports);
var import_pino = __toESM(require("pino"));
var logger = (0, import_pino.default)({
  level: "info",
  // Ensure timestamps are included
  timestamp: import_pino.default.stdTimeFunctions.isoTime
});
async function main(id, config) {
  const symbol = config.symbol || "BTC/USD";
  const basePrice = config.base_price || 45e3;
  const alertThreshold = config.alert_threshold || 1;
  const updateInterval = config.update_interval_ms || 5e3;
  logger.info({ botId: id, symbol, alertThreshold }, "bot_started");
  const state = {
    lastPrice: basePrice,
    priceHistory: [basePrice],
    lastAlertTime: 0
  };
  try {
    while (true) {
      const priceData = simulatePrice(state, symbol, basePrice);
      logger.info(
        {
          _metric: "price",
          symbol: priceData.symbol,
          value: priceData.value,
          change_pct: priceData.change_pct,
          high_24h: priceData.high_24h,
          low_24h: priceData.low_24h
        },
        "price_update"
      );
      const alert = checkAlertConditions(
        state,
        priceData,
        alertThreshold,
        symbol
      );
      if (alert) {
        logger.info(
          {
            _metric: "alert",
            symbol: alert.symbol,
            type: alert.type,
            change_pct: alert.change_pct,
            message: alert.message,
            severity: alert.severity
          },
          "price_alert"
        );
      }
      const signal = generateSignal(state, symbol);
      if (signal) {
        logger.info(
          {
            _metric: "signal",
            symbol: signal.symbol,
            direction: signal.direction,
            confidence: signal.confidence,
            reason: signal.reason
          },
          "signal_generated"
        );
      }
      state.lastPrice = priceData.value;
      state.priceHistory.push(priceData.value);
      if (state.priceHistory.length > 100) {
        state.priceHistory.shift();
      }
      await sleep(updateInterval);
    }
  } catch (error) {
    if (error.message === "SIGTERM") {
      logger.info({ botId: id }, "bot_stopped");
      return { status: "stopped", message: "Bot stopped gracefully" };
    }
    throw error;
  }
}
function simulatePrice(state, symbol, basePrice) {
  const volatility = 2e-3;
  const meanReversion = 1e-3;
  const randomChange = (Math.random() - 0.5) * 2 * volatility;
  const reversion = (basePrice - state.lastPrice) / basePrice * meanReversion;
  const changePercent = randomChange + reversion;
  const newPrice = state.lastPrice * (1 + changePercent);
  const changePct = (newPrice - state.lastPrice) / state.lastPrice * 100;
  const high24h = Math.max(...state.priceHistory, newPrice);
  const low24h = Math.min(...state.priceHistory, newPrice);
  return {
    symbol,
    value: Math.round(newPrice * 100) / 100,
    change_pct: Math.round(changePct * 1e3) / 1e3,
    high_24h: Math.round(high24h * 100) / 100,
    low_24h: Math.round(low24h * 100) / 100
  };
}
function checkAlertConditions(state, priceData, threshold, symbol) {
  const now = Date.now();
  if (now - state.lastAlertTime < 3e4) {
    return null;
  }
  const absChange = Math.abs(priceData.change_pct);
  if (absChange >= threshold) {
    state.lastAlertTime = now;
    const direction = priceData.change_pct > 0 ? "spike" : "drop";
    const severity = absChange >= threshold * 2 ? "high" : absChange >= threshold * 1.5 ? "medium" : "low";
    return {
      symbol,
      type: `price_${direction}`,
      change_pct: Math.round(priceData.change_pct * 100) / 100,
      message: `${symbol} ${direction} of ${absChange.toFixed(2)}%`,
      severity
    };
  }
  return null;
}
function generateSignal(state, symbol) {
  if (state.priceHistory.length < 10) {
    return null;
  }
  if (Math.random() > 0.1) {
    return null;
  }
  const shortPeriod = 5;
  const longPeriod = 10;
  const shortMA = state.priceHistory.slice(-shortPeriod).reduce((a, b) => a + b, 0) / shortPeriod;
  const longMA = state.priceHistory.slice(-longPeriod).reduce((a, b) => a + b, 0) / longPeriod;
  const maDiff = (shortMA - longMA) / longMA;
  if (Math.abs(maDiff) > 1e-3) {
    const direction = maDiff > 0 ? "long" : "short";
    const confidence = Math.min(Math.abs(maDiff) * 100, 0.95);
    return {
      symbol,
      direction,
      confidence: Math.round(confidence * 100) / 100,
      reason: `MA${shortPeriod} ${direction === "long" ? "above" : "below"} MA${longPeriod}`
    };
  }
  return null;
}
function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
if (require.main === module) {
  main("test-bot-id", {
    symbol: "BTC/USD",
    base_price: 45e3,
    alert_threshold: 0.5,
    update_interval_ms: 2e3
  }).catch(console.error);
}
// Annotate the CommonJS export names for ESM import in node:
0 && (module.exports = {
  main
});
