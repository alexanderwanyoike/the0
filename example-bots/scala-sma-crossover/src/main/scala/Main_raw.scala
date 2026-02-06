/**
 * SMA Crossover Bot (Scala 3)
 * ===========================
 * A realtime bot that implements Simple Moving Average crossover strategy
 * using live data from Yahoo Finance.
 *
 * This example demonstrates:
 * - Fetching real market data from Yahoo Finance REST API
 * - Calculating Simple Moving Averages (SMA) functionally
 * - Detecting SMA crossovers for trading signals
 * - Structured metric emission for dashboard visualization
 *
 * Metrics emitted:
 * - price: Current stock price with change percentage
 * - sma: Short and long SMA values
 * - signal: BUY/SELL signals when crossover detected
 */

import sttp.client3._
import io.circe._
import io.circe.parser._
import scala.util.{Try, Success, Failure}

case class BotState(
  prevShortSma: Option[Double] = None,
  prevLongSma: Option[Double] = None
)

object Main:
  def main(args: Array[String]): Unit =
    // Get configuration from environment
    val botId = sys.env.getOrElse("BOT_ID", "test-bot")
    val configJson = sys.env.getOrElse("BOT_CONFIG", "{}")

    val config = parse(configJson).getOrElse(Json.obj())

    // Extract configuration with defaults
    val symbol = config.hcursor.get[String]("symbol").getOrElse("AAPL")
    val shortPeriod = config.hcursor.get[Int]("short_period").getOrElse(5)
    val longPeriod = config.hcursor.get[Int]("long_period").getOrElse(20)
    val updateIntervalMs = config.hcursor.get[Int]("update_interval_ms").getOrElse(60000)

    emitLog("bot_started", Json.obj(
      "botId" -> Json.fromString(botId),
      "symbol" -> Json.fromString(symbol),
      "shortPeriod" -> Json.fromInt(shortPeriod),
      "longPeriod" -> Json.fromInt(longPeriod)
    ))

    val backend = HttpClientSyncBackend()
    var state = BotState()

    // Main loop
    while true do
      try
        state = processUpdate(backend, symbol, shortPeriod, longPeriod, state)
      catch
        case e: Exception =>
          emitLog("error", Json.obj("message" -> Json.fromString(e.getMessage)))

      Thread.sleep(updateIntervalMs)

  def processUpdate(
    backend: SttpBackend[Identity, Any],
    symbol: String,
    shortPeriod: Int,
    longPeriod: Int,
    state: BotState
  ): BotState =
    // Fetch historical data
    val prices = fetchYahooFinance(backend, symbol)

    if prices.length < longPeriod then
      emitLog("insufficient_data", Json.obj(
        "symbol" -> Json.fromString(symbol),
        "required" -> Json.fromInt(longPeriod),
        "available" -> Json.fromInt(prices.length)
      ))
      return state

    // Get current price
    val currentPrice = prices.last
    val previousPrice = if prices.length > 1 then prices(prices.length - 2) else currentPrice
    val changePct = if previousPrice != 0 then ((currentPrice - previousPrice) / previousPrice) * 100 else 0.0

    // Emit price metric
    emitMetric("price", Json.obj(
      "symbol" -> Json.fromString(symbol),
      "value" -> Json.fromDoubleOrNull(roundTo(currentPrice, 2)),
      "change_pct" -> Json.fromDoubleOrNull(roundTo(changePct, 3)),
      "timestamp" -> Json.fromString(getCurrentTimestamp)
    ))

    // Calculate SMAs
    val shortSma = calculateSMA(prices, shortPeriod)
    val longSma = calculateSMA(prices, longPeriod)

    // Emit SMA metric
    emitMetric("sma", Json.obj(
      "symbol" -> Json.fromString(symbol),
      "short_sma" -> Json.fromDoubleOrNull(roundTo(shortSma, 2)),
      "long_sma" -> Json.fromDoubleOrNull(roundTo(longSma, 2)),
      "short_period" -> Json.fromInt(shortPeriod),
      "long_period" -> Json.fromInt(longPeriod)
    ))

    // Check for crossover signal
    (state.prevShortSma, state.prevLongSma) match
      case (Some(prevShort), Some(prevLong)) =>
        checkCrossover(prevShort, prevLong, shortSma, longSma).foreach { signal =>
          val confidence = math.min(math.abs(shortSma - longSma) / longSma, 0.95)
          val direction = if signal == "BUY" then "above" else "below"

          emitMetric("signal", Json.obj(
            "type" -> Json.fromString(signal),
            "symbol" -> Json.fromString(symbol),
            "price" -> Json.fromDoubleOrNull(roundTo(currentPrice, 2)),
            "confidence" -> Json.fromDoubleOrNull(roundTo(confidence, 2)),
            "reason" -> Json.fromString(s"SMA$shortPeriod crossed $direction SMA$longPeriod")
          ))
        }
      case _ => ()

    // Update state
    state.copy(
      prevShortSma = Some(shortSma),
      prevLongSma = Some(longSma)
    )

  def fetchYahooFinance(backend: SttpBackend[Identity, Any], symbol: String): Vector[Double] =
    val url = s"https://query1.finance.yahoo.com/v8/finance/chart/$symbol?interval=1d&range=1mo"

    val request = basicRequest
      .get(uri"$url")
      .header("User-Agent", "the0-sma-bot/1.0")

    val response = request.send(backend)

    response.body match
      case Right(body) =>
        parse(body).toOption.flatMap { json =>
          val cursor = json.hcursor
          cursor
            .downField("chart")
            .downField("result")
            .downArray
            .downField("indicators")
            .downField("quote")
            .downArray
            .downField("close")
            .as[Vector[Option[Double]]]
            .toOption
            .map(_.flatten)
        }.getOrElse(Vector.empty)
      case Left(_) =>
        Vector.empty

  def calculateSMA(prices: Vector[Double], period: Int): Double =
    if prices.length < period then 0.0
    else prices.takeRight(period).sum / period

  def checkCrossover(
    prevShort: Double,
    prevLong: Double,
    currShort: Double,
    currLong: Double
  ): Option[String] =
    // Golden cross: short SMA crosses above long SMA
    if prevShort <= prevLong && currShort > currLong then Some("BUY")
    // Death cross: short SMA crosses below long SMA
    else if prevShort >= prevLong && currShort < currLong then Some("SELL")
    else None

  def emitMetric(metricType: String, data: Json): Unit =
    val output = data.deepMerge(Json.obj("_metric" -> Json.fromString(metricType)))
    println(output.noSpaces)

  def emitLog(event: String, data: Json): Unit =
    val output = data.deepMerge(Json.obj(
      "event" -> Json.fromString(event),
      "timestamp" -> Json.fromString(getCurrentTimestamp)
    ))
    println(output.noSpaces)

  def roundTo(value: Double, decimals: Int): Double =
    val multiplier = math.pow(10, decimals)
    math.round(value * multiplier) / multiplier

  def getCurrentTimestamp: String =
    val now = System.currentTimeMillis()
    s"${now / 1000}.${now % 1000}Z"
