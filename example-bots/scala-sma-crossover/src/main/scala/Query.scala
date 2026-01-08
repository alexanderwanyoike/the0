/**
 * SMA Crossover Query Handlers (Scala)
 * =====================================
 * Query endpoints for the SMA crossover bot.
 *
 * These handlers provide read-only access to bot state and computed data.
 * They can be executed via:
 *   - CLI: the0 bot query <bot_id> /status
 *   - API: POST /bot/:id/query
 *
 * Available queries:
 *   /status     - Get current SMA values and configuration
 *   /signals    - Get signal count with optional limit param
 *   /sma-values - Get current short/long SMA values
 */

import the0.{Query, State}
import io.circe.parser.parse

object QueryMain:
  def main(args: Array[String]): Unit =
    // /status - Get bot status and current SMA values
    Query.handler("/status") { req =>
      val persisted = State.get("bot_state").flatMap(parse(_).toOption)
      val cursor = persisted.map(_.hcursor)

      val prevShortSma = cursor.flatMap(_.get[Double]("prev_short_sma").toOption)
      val prevLongSma = cursor.flatMap(_.get[Double]("prev_long_sma").toOption)
      val signalCount = cursor.flatMap(_.get[Long]("signal_count").toOption).getOrElse(0L)

      s"""{
        "prev_short_sma": ${prevShortSma.map(_.toString).getOrElse("null")},
        "prev_long_sma": ${prevLongSma.map(_.toString).getOrElse("null")},
        "signal_count": $signalCount
      }"""
    }

    // /signals - Get signal statistics
    Query.handler("/signals") { req =>
      val limit = req.getOrElse("limit", "10").toIntOption.getOrElse(10)
      val persisted = State.get("bot_state").flatMap(parse(_).toOption)
      val signalCount = persisted.flatMap(_.hcursor.get[Long]("signal_count").toOption).getOrElse(0L)

      s"""{"signal_count": $signalCount, "limit_applied": $limit}"""
    }

    // /sma-values - Get current SMA values
    Query.handler("/sma-values") { req =>
      val persisted = State.get("bot_state").flatMap(parse(_).toOption)
      val cursor = persisted.map(_.hcursor)

      val shortSma = cursor.flatMap(_.get[Double]("prev_short_sma").toOption)
      val longSma = cursor.flatMap(_.get[Double]("prev_long_sma").toOption)

      s"""{
        "short_sma": ${shortSma.map(_.toString).getOrElse("null")},
        "long_sma": ${longSma.map(_.toString).getOrElse("null")}
      }"""
    }

    Query.run()
