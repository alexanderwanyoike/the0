package the0

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal
import ujson.Value

/**
 * The contract bot: runs one sdk/contract case against this SDK.
 *
 * Usage: the0.ContractBot <case.json> <query result path>
 *
 * ContractSpec starts it with the case's environment. The case's script (bot
 * entrypoint) or handlers (query entrypoint) are interpreted op by op, each op
 * making the SDK call sdk/contract/README.md maps it to.
 */
object ContractBot {

  def main(args: Array[String]): Unit = {
    val contractCase = ujson.read(Files.readString(Paths.get(args(0))))
    Query.queryResultPath = args(1)

    if (contractCase("entrypoint").str == "bot") {
      runSteps(contractCase.obj.get("script").map(_.arr.toList).getOrElse(Nil), None)
    } else {
      for ((path, steps) <- contractCase("handlers").obj) {
        Query.handler(path)(request => ujson.write(runSteps(steps.arr.toList, Some(request))))
      }
      Query.run()
    }
  }

  private def runSteps(steps: List[Value], request: Option[Query.Request]): Value = {
    val bound = mutable.Map.empty[String, Value]

    @tailrec
    def loop(rest: List[Value]): Value = rest match {
      case Nil => ujson.Null
      case step :: _ if step("op").str == "return" =>
        resolve(step.obj.getOrElse("value", ujson.Null), bound)
      case step :: tail =>
        val kind = step.obj.get("catch").map(_.str)
        val value =
          try perform(step, request, bound)
          catch {
            case e: Throwable if kind.exists(catches(_, e)) => ujson.Obj("caught" -> kind.get)
          }
        step.obj.get("as").foreach(name => bound(name.str) = value)
        loop(tail)
    }

    loop(steps)
  }

  private def catches(kind: String, e: Throwable): Boolean = kind match {
    case "read_only_state" => e.isInstanceOf[Query.ReadOnlyStateError]
    case "any" => NonFatal(e)
    case other => abort(s"unknown catch kind $other")
  }

  private def resolve(value: Value, bound: collection.Map[String, Value]): Value = value match {
    case ujson.Obj(fields) if fields.keySet == Set("$ref") => bound(fields("$ref").str)
    case ujson.Obj(fields) => ujson.Obj.from(fields.map((key, item) => key -> resolve(item, bound)))
    case ujson.Arr(items) => ujson.Arr.from(items.map(resolve(_, bound)))
    case other => other
  }

  private def perform(step: Value, request: Option[Query.Request], bound: collection.Map[String, Value]): Value = {
    val args = step.obj.collect {
      case (key, value) if !Set("op", "as", "catch").contains(key) => key -> resolve(value, bound)
    }
    def arg(name: String): Value = args(name)
    def optional(name: String): Option[Value] = args.get(name)

    step("op").str match {
      case "parse" =>
        val (botId, config) = Input.parse()
        ujson.Obj("bot_id" -> botId, "config" -> ujson.read(config))
      case "success" =>
        optional("data") match {
          case Some(data) => Input.success(arg("message").str, ujson.write(data))
          case None => Input.success(arg("message").str)
        }
        ujson.Null
      case "error" =>
        optional("data") match {
          case Some(data) => Input.error(arg("message").str, ujson.write(data))
          case None => Input.error(arg("message").str)
        }
      case "result" =>
        Input.resultRaw(ujson.write(arg("value")))
        ujson.Null
      case "metric" =>
        Input.metric(arg("type").str, ujson.write(arg("data")))
        ujson.Null
      case "log" =>
        log(arg("message").str, optional("data").map(ujson.write(_)), optional("level").map(level => logLevel(level.str)))
        ujson.Null
      case "raise" =>
        throw new RuntimeException(arg("message").str)

      case "state.get" =>
        optional("default") match {
          case Some(default) => ujson.read(State.getOrElse(arg("key").str, ujson.write(default)))
          case None => State.get(arg("key").str).fold[Value](ujson.Null)(ujson.read(_))
        }
      case "state.set" =>
        State.set(arg("key").str, ujson.write(arg("value")))
        ujson.Null
      case "state.delete" => ujson.Bool(State.delete(arg("key").str))
      case "state.list" => ujson.Arr.from(State.list().map(ujson.Str(_)))
      case "state.exists" => ujson.Bool(State.exists(arg("key").str))
      case "state.clear" =>
        State.clear()
        ujson.Null

      case op =>
        val req = request.getOrElse(abort(s"op $op is only valid inside a query handler"))
        op match {
          case "query.params" => ujson.Obj.from(req.jsonParams.map((key, value) => key -> toUjson(value)))
          case "query.param" => req.getJson(arg("key").str).map(toUjson).orElse(optional("default")).getOrElse(ujson.Null)
          case "query.config" => ujson.read(Query.getConfig)
          case _ => abort(s"unknown op $op")
        }
    }
  }

  private def log(message: String, data: Option[String], level: Option[Input.LogLevel]): Unit = (data, level) match {
    case (Some(data), Some(level)) => Input.log(message, data, level)
    case (Some(data), None) => Input.log(message, data)
    case (None, Some(level)) => Input.log(message, level = level)
    case (None, None) => Input.log(message)
  }

  private def logLevel(name: String): Input.LogLevel = name match {
    case "info" => Input.LogLevel.Info
    case "warn" => Input.LogLevel.Warn
    case "error" => Input.LogLevel.Error
    case other => abort(s"unknown log level $other")
  }

  private def toUjson(value: JsonValue): Value = value match {
    case JsonValue.Null => ujson.Null
    case JsonValue.Bool(b) => ujson.Bool(b)
    case JsonValue.Num(n) => ujson.Num(n.toDouble)
    case JsonValue.Str(s) => ujson.Str(s)
    case JsonValue.Arr(items) => ujson.Arr.from(items.map(toUjson))
    case JsonValue.Obj(fields) => ujson.Obj.from(fields.map((key, item) => key -> toUjson(item)))
  }

  // Exits rather than throws, so a mistake in a case or in this bot cannot pass for a handler error.
  private def abort(message: String): Nothing = {
    System.err.println(s"contract bot: $message")
    sys.exit(2)
  }
}
