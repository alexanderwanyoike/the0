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
 * - Persistent state for SMA values across restarts
 *
 * Metrics emitted:
 * - price: Current stock price with change percentage
 * - sma: Short and long SMA values
 * - signal: BUY/SELL signals when crossover detected
 *
 * State Usage:
 * - Persists previous SMA values for crossover detection across restarts
 * - Tracks total signal count for monitoring
 */

import sttp.client3._
import io.circe._
import io.circe.parser._
import the0.{Input, State}

case class BotState(
  prevShortSma: Option[Double] = None,
  prevLongSma: Option[Double] = None,
  signalCount: Long = 0
)

object Main:
  def main(args: Array[String]): Unit =
    // Get configuration using the0 SDK
    val (botId, configJson) = Input.parse()

    val config = parse(configJson).getOrElse(Json.obj())

    // Extract configuration with defaults
    val symbol = config.hcursor.get[String]("symbol").getOrElse("AAPL")
    val shortPeriod = config.hcursor.get[Int]("short_period").getOrElse(5)
    val longPeriod = config.hcursor.get[Int]("long_period").getOrElse(20)
    val updateIntervalMs = config.hcursor.get[Int]("update_interval_ms").getOrElse(60000)

    // Load persistent state from previous runs
    val persisted = State.get("bot_state").flatMap(parse(_).toOption)
    var botState = persisted match
      case Some(json) =>
        val cursor = json.hcursor
        BotState(
          prevShortSma = cursor.get[Double]("prev_short_sma").toOption.filter(_ != 0.0),
          prevLongSma = cursor.get[Double]("prev_long_sma").toOption.filter(_ != 0.0),
          signalCount = cursor.get[Long]("signal_count").getOrElse(0L)
        )
      case None => BotState()

    Input.log(s"Bot $botId started - $symbol SMA($shortPeriod/$longPeriod) - loaded ${botState.signalCount} signals")

    val backend = HttpClientSyncBackend()

    // Main loop
    var iteration = 0
    while true do
      try
        botState = processUpdate(backend, symbol, shortPeriod, longPeriod, botState)

        // Persist state every 10 iterations
        iteration += 1
        if iteration % 10 == 0 then
          State.set("bot_state", Json.obj(
            "prev_short_sma" -> Json.fromDoubleOrNull(botState.prevShortSma.getOrElse(0.0)),
            "prev_long_sma" -> Json.fromDoubleOrNull(botState.prevLongSma.getOrElse(0.0)),
            "signal_count" -> Json.fromLong(botState.signalCount)
          ).noSpaces)
      catch
        case e: Exception =>
          Input.log(s"Error: ${e.getMessage}")

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
      Input.log(s"Insufficient data: ${prices.length}/$longPeriod required for $symbol")
      return state

    // Get current price
    val currentPrice = prices.last
    val previousPrice = if prices.length > 1 then prices(prices.length - 2) else currentPrice
    val changePct = if previousPrice != 0 then ((currentPrice - previousPrice) / previousPrice) * 100 else 0.0

    // Emit price metric using SDK
    Input.metric("price", Json.obj(
      "symbol" -> Json.fromString(symbol),
      "value" -> Json.fromDoubleOrNull(roundTo(currentPrice, 2)),
      "change_pct" -> Json.fromDoubleOrNull(roundTo(changePct, 3))
    ).noSpaces)

    // Calculate SMAs
    val shortSma = calculateSMA(prices, shortPeriod)
    val longSma = calculateSMA(prices, longPeriod)

    // Emit SMA metric using SDK
    Input.metric("sma", Json.obj(
      "symbol" -> Json.fromString(symbol),
      "short_sma" -> Json.fromDoubleOrNull(roundTo(shortSma, 2)),
      "long_sma" -> Json.fromDoubleOrNull(roundTo(longSma, 2)),
      "short_period" -> Json.fromInt(shortPeriod),
      "long_period" -> Json.fromInt(longPeriod)
    ).noSpaces)

    // Check for crossover signal
    var newSignalCount = state.signalCount
    (state.prevShortSma, state.prevLongSma) match
      case (Some(prevShort), Some(prevLong)) =>
        checkCrossover(prevShort, prevLong, shortSma, longSma).foreach { signal =>
          newSignalCount += 1
          val confidence = math.min(math.abs(shortSma - longSma) / longSma * 100, 0.95)
          val direction = if signal == "BUY" then "above" else "below"

          Input.metric("signal", Json.obj(
            "type" -> Json.fromString(signal),
            "symbol" -> Json.fromString(symbol),
            "price" -> Json.fromDoubleOrNull(roundTo(currentPrice, 2)),
            "confidence" -> Json.fromDoubleOrNull(roundTo(confidence, 2)),
            "total_signals" -> Json.fromLong(newSignalCount),
            "reason" -> Json.fromString(s"SMA$shortPeriod crossed $direction SMA$longPeriod")
          ).noSpaces)
        }
      case _ => ()

    // Update state
    state.copy(
      prevShortSma = Some(shortSma),
      prevLongSma = Some(longSma),
      signalCount = newSignalCount
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

  def roundTo(value: Double, decimals: Int): Double =
    val multiplier = math.pow(10, decimals)
    math.round(value * multiplier) / multiplier
