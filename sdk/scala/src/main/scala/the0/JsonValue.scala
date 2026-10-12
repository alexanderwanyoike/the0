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

  /**
   * The value as compact JSON text.
   */
  private[the0] def render: String = this match {
    case Null => "null"
    case Bool(value) => value.toString
    case Num(value) => value.toString
    case Str(value) => JsonValue.quote(value)
    case Arr(items) => items.map(_.render).mkString("[", ",", "]")
    case Obj(fields) => fields.map((key, value) => s"${JsonValue.quote(key)}:${value.render}").mkString("{", ",", "}")
  }
}

object JsonValue {

  private def quote(text: String): String = {
    val out = new StringBuilder("\"")
    text.foreach {
      case '"' => out.append("\\\"")
      case '\\' => out.append("\\\\")
      case '\n' => out.append("\\n")
      case '\r' => out.append("\\r")
      case '\t' => out.append("\\t")
      case '\b' => out.append("\\b")
      case '\f' => out.append("\\f")
      case c if c < ' ' => out.append(f"\\u${c.toInt}%04x")
      case c => out.append(c)
    }
    out.append('"').toString
  }
}
