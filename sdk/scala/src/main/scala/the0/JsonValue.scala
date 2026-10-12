package the0

/**
 * A JSON value, such as a query param keeps it.
 *
 * Example:
 * {{{
 * req.getJson("limit") match {
 *   case Some(JsonValue.Num(n)) => n.toInt
 *   case _ => 10
 * }
 * }}}
 */
enum JsonValue {
  case Null
  case Bool(value: Boolean)
  case Num(value: BigDecimal)
  case Str(value: String)
  case Arr(items: Vector[JsonValue])
  case Obj(fields: Map[String, JsonValue])
}
