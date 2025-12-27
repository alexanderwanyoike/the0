object Main extends App {
  val botId = sys.env.getOrElse("BOT_ID", "unknown")
  val config = sys.env.getOrElse("BOT_CONFIG", "{}")

  System.err.println(s"BOT_ID: $botId")
  System.err.println(s"BOT_CONFIG: $config")

  // Output success JSON
  println(s"""{"status":"success","message":"Scala bot executed","bot_id":"$botId"}""")
}
