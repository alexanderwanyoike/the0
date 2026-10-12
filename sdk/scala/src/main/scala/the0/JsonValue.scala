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

  /**
   * Parse JSON text (RFC 8259), or None when the text is not JSON.
   */
  private[the0] def parse(text: String): Option[JsonValue] =
    try {
      val parser = new Parser(text)
      val value = parser.value()
      parser.end()
      Some(value)
    } catch {
      case _: NotJson => None
    }

  private final class NotJson extends Exception(null, null, false, false)

  private final class Parser(text: String) {
    private var pos = 0

    def end(): Unit = {
      skipWhitespace()
      if (pos != text.length) fail()
    }

    def value(): JsonValue = {
      skipWhitespace()
      peek match {
        case '{' => obj()
        case '[' => arr()
        case '"' => Str(string())
        case 't' => literal("true", Bool(true))
        case 'f' => literal("false", Bool(false))
        case 'n' => literal("null", Null)
        case _ => number()
      }
    }

    private def obj(): JsonValue = {
      pos += 1
      val fields = scala.collection.immutable.VectorMap.newBuilder[String, JsonValue]
      skipWhitespace()
      if (peek == '}') pos += 1
      else {
        var more = true
        while (more) {
          skipWhitespace()
          val key = string()
          skipWhitespace()
          expect(':')
          fields += key -> value()
          more = separator('}')
        }
      }
      Obj(fields.result())
    }

    private def arr(): JsonValue = {
      pos += 1
      val items = Vector.newBuilder[JsonValue]
      skipWhitespace()
      if (peek == ']') pos += 1
      else {
        var more = true
        while (more) {
          items += value()
          more = separator(']')
        }
      }
      Arr(items.result())
    }

    // After a member: true on a comma, false on the closing bracket.
    private def separator(close: Char): Boolean = {
      skipWhitespace()
      val c = peek
      pos += 1
      if (c == ',') true else if (c == close) false else fail()
    }

    private def string(): String = {
      expect('"')
      val out = new StringBuilder
      var open = true
      while (open) {
        val c = peek
        pos += 1
        c match {
          case '"' => open = false
          case '\\' => out.append(escape())
          case c if c < ' ' => fail()
          case c => out.append(c)
        }
      }
      out.toString
    }

    private def escape(): Char = {
      val c = peek
      pos += 1
      c match {
        case '"' => '"'
        case '\\' => '\\'
        case '/' => '/'
        case 'b' => '\b'
        case 'f' => '\f'
        case 'n' => '\n'
        case 'r' => '\r'
        case 't' => '\t'
        case 'u' =>
          if (pos + 4 > text.length) fail()
          val hex = text.substring(pos, pos + 4)
          if (!hex.forall(Character.digit(_, 16) >= 0)) fail()
          pos += 4
          Integer.parseInt(hex, 16).toChar
        case _ => fail()
      }
    }

    private def number(): JsonValue = {
      val start = pos
      if (peekIs('-')) pos += 1
      if (peekIs('0')) pos += 1 else digits()
      if (peekIs('.')) {
        pos += 1
        digits()
      }
      if (peekIs('e') || peekIs('E')) {
        pos += 1
        if (peekIs('+') || peekIs('-')) pos += 1
        digits()
      }
      Num(BigDecimal(text.substring(start, pos)))
    }

    private def digits(): Unit = {
      val start = pos
      while (pos < text.length && text.charAt(pos) >= '0' && text.charAt(pos) <= '9') pos += 1
      if (pos == start) fail()
    }

    private def literal(word: String, value: JsonValue): JsonValue = {
      if (!text.startsWith(word, pos)) fail()
      pos += word.length
      value
    }

    private def expect(c: Char): Unit = {
      if (peek != c) fail()
      pos += 1
    }

    private def peek: Char = if (pos < text.length) text.charAt(pos) else fail()

    private def peekIs(c: Char): Boolean = pos < text.length && text.charAt(pos) == c

    private def skipWhitespace(): Unit =
      while (pos < text.length && " \t\n\r".indexOf(text.charAt(pos)) >= 0) pos += 1

    private def fail(): Nothing = throw new NotJson
  }

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
