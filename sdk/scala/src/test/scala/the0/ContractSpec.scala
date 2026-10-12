package the0

import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path, Paths}
import java.time.Duration
import java.util.Comparator
import java.util.concurrent.TimeUnit
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.{Try, Using}
import ujson.Value

/**
 * Runs every bot contract case in sdk/contract/cases against this SDK.
 *
 * The case format and the matching rules are in sdk/contract/README.md.
 */
class ContractSpec extends AnyFunSuite {
  import ContractSpec.*

  test("finds the cases") {
    assert(casePaths.nonEmpty, s"no contract cases in $CasesDir")
  }

  test("lists only real cases as known failures") {
    assert(KnownFailures.keySet.diff(casePaths.map(caseName).toSet).isEmpty)
  }

  for (path <- casePaths) {
    val name = caseName(path)
    test(s"contract case $name") {
      if (KnownFailures.contains(name)) pendingUntilFixed(check(path))
      else check(path)
    }
  }

  private def check(path: Path): Unit = {
    val (problems, output) = runCase(path)
    if (problems.nonEmpty) fail((problems :+ output).mkString("\n"))
  }
}

object ContractSpec {

  // build.sbt passes both paths; the fallbacks serve a test runner that does not fork.
  private val CasesDir: Path = Paths.get(sys.props.getOrElse("the0.contract.cases", "../contract/cases"))
  private val Classpath: String = sys.props.getOrElse("the0.contract.classpath", sys.props("java.class.path"))
  private val Java: String = Paths.get(sys.props("java.home"), "bin", "java").toString

  private val RunTimeout = Duration.ofSeconds(60)
  private val ServerStartTimeout = Duration.ofSeconds(30)
  // sdk/contract/README.md, step 5, says why a server case may need a fresh port.
  private val PortAttempts = 3

  // Cases this SDK does not pass yet, each with the reason; sdk/contract/README.md
  // says how the list is kept.
  private val KnownFailures: Map[String, String] = Map.empty

  private val Timestamp = """^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$""".r

  private def casePaths: List[Path] =
    Using.resource(Files.list(CasesDir))(_.iterator.asScala.filter(_.toString.endsWith(".json")).toList.sorted)

  private def caseName(path: Path): String = path.getFileName.toString.stripSuffix(".json")

  private def show(value: Value): String = ujson.write(value)

  /** Every way actual fails to match expected, as readable lines. */
  private def mismatches(expected: Value, actual: Value, where: String): List[String] = {
    def differs = List(s"$where: expected ${show(expected)}, got ${show(actual)}")
    expected match {
      case ujson.Obj(fields) if fields.contains("$match") => matcherMismatches(expected, actual, where)
      case ujson.Obj(fields) =>
        actual match {
          case ujson.Obj(got) =>
            fields.toList.flatMap { (key, want) =>
              got.get(key).fold(List(s"$where.$key: missing"))(mismatches(want, _, s"$where.$key"))
            } ++ got.toList.collect {
              case (key, value) if !fields.contains(key) => s"$where.$key: unexpected, got ${show(value)}"
            }
          case _ => List(s"$where: expected an object, got ${show(actual)}")
        }
      case ujson.Arr(items) =>
        actual match {
          case ujson.Arr(got) if got.length == items.length =>
            items.indices.toList.flatMap(i => mismatches(items(i), got(i), s"$where[$i]"))
          case _ => differs
        }
      case _ => if (expected == actual) Nil else differs
    }
  }

  private def matcherMismatches(expected: Value, actual: Value, where: String): List[String] = {
    val ok = expected("$match").str match {
      case "timestamp" => actual.strOpt.exists(Timestamp.matches)
      case "string" => actual.strOpt.isDefined
      case "contains" => actual.strOpt.exists(_.contains(expected("value").str))
      case "nonzero" => actual.numOpt.exists(_ != 0)
      case "unordered" => unorderedMatches(expected("items").arr.toList, actual)
      case kind => return List(s"$where: unknown matcher $kind")
    }
    if (ok) Nil else List(s"$where: expected ${show(expected)}, got ${show(actual)}")
  }

  private def unorderedMatches(items: List[Value], actual: Value): Boolean = actual.arrOpt match {
    case Some(got) if got.length == items.length =>
      items
        .foldLeft(Option(got.toList)) { (remaining, item) =>
          remaining.flatMap { left =>
            val found = left.indexWhere(value => mismatches(item, value, "").isEmpty)
            if (found < 0) None else Some(left.patch(found, Nil, 1))
          }
        }
        .isDefined
    case _ => false
  }

  private def lineMismatches(expected: List[Value], text: String, stream: String): List[String] = {
    val lines = text.linesIterator.filter(_.trim.nonEmpty).toList
    if (lines.length != expected.length) List(s"$stream: expected ${expected.length} lines, got ${lines.length}")
    else
      lines.zip(expected).zipWithIndex.flatMap { case ((line, want), index) =>
        Try(ujson.read(line)).toOption match {
          case Some(got) => mismatches(want, got, s"$stream[$index]")
          case None => List(s"$stream[$index]: not JSON: $line")
        }
      }
  }

  private def fileMismatches(expected: Value, file: Path, name: String): List[String] =
    if (expected.isNull) {
      if (Files.exists(file)) List(s"$name: expected no file, found one") else Nil
    } else if (!Files.exists(file)) {
      List(s"$name: no file written")
    } else {
      Try(ujson.read(Files.readString(file))).fold(e => List(s"$name: not JSON: ${e.getMessage}"), mismatches(expected, _, name))
    }

  private def readState(stateDir: Path): Value =
    if (!Files.isDirectory(stateDir)) ujson.Obj()
    else
      Using.resource(Files.list(stateDir)) { files =>
        ujson.Obj.from(
          files.iterator.asScala
            .filter(_.toString.endsWith(".json"))
            .map(file => file.getFileName.toString.stripSuffix(".json") -> ujson.read(Files.readString(file)))
            .toList
        )
      }

  private def freePort(): Int =
    Using.resource(new ServerSocket(0, 0, InetAddress.getLoopbackAddress))(_.getLocalPort)

  private def canConnect(port: Int): Boolean =
    Using(new Socket()) { socket =>
      socket.connect(new InetSocketAddress(InetAddress.getLoopbackAddress, port), 1000)
    }.isSuccess

  private final case class Response(status: Int, headers: java.net.http.HttpHeaders, body: String)

  private def send(port: Int, request: Value): Response = {
    val (body, contentType) = request.obj.get("body") match {
      case Some(json) => (HttpRequest.BodyPublishers.ofString(ujson.write(json)), Some("application/json"))
      case None =>
        request.obj.get("raw_body") match {
          case Some(raw) =>
            val contentType = request.obj.get("content_type").map(_.str).getOrElse("text/plain")
            (HttpRequest.BodyPublishers.ofString(raw.str), Some(contentType))
          case None => (HttpRequest.BodyPublishers.noBody(), None)
        }
    }
    val builder = HttpRequest
      .newBuilder(URI.create(s"http://127.0.0.1:$port${request("target").str}"))
      .timeout(RunTimeout)
      .method(request("method").str, body)
    contentType.foreach(builder.header("Content-Type", _))
    // A client per request, so each request goes on a new connection.
    val client = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()
    val response = client.send(builder.build(), HttpResponse.BodyHandlers.ofString())
    Response(response.statusCode(), response.headers(), response.body())
  }

  /** The directories the runtime gives a bot, rooted in a temporary directory. */
  private final class Sandbox(contractCase: Value, val port: Option[Int]) {
    val root: Path = Files.createTempDirectory("the0-contract-")
    val codeDir: Path = root.resolve("bot")
    val stateDir: Path = root.resolve("state").resolve(".the0-state")
    val queryResult: Path = root.resolve("query").resolve("result.json")
    val stdout: Path = root.resolve("stdout")
    val stderr: Path = root.resolve("stderr")

    Files.createDirectories(codeDir)
    Files.createDirectories(queryResult.getParent)
    contractCase.obj.get("state_before").foreach { state =>
      Files.createDirectories(stateDir)
      for ((key, value) <- state.obj) Files.writeString(stateDir.resolve(s"$key.json"), ujson.write(value))
    }

    def env: Map[String, String] = {
      val placeholders = Map(
        "{{code_mount_dir}}" -> codeDir.toString.stripPrefix("/"),
        "{{state_dir}}" -> stateDir.toString,
        "{{port}}" -> port.fold("")(_.toString)
      )
      contractCase("env").obj.toMap.map { (key, value) =>
        key -> placeholders.foldLeft(value.str) { case (text, (placeholder, actual)) => text.replace(placeholder, actual) }
      }
    }

    def remove(): Unit =
      Using.resource(Files.walk(root))(_.sorted(Comparator.reverseOrder[Path]()).iterator.asScala.foreach(Files.delete))
  }

  private final case class Run(exitCode: Option[Int], stdout: String, stderr: String, problems: List[String], exitedBeforeServing: Boolean) {
    def output: String = s"exit code: ${exitCode.getOrElse("none")}\n--- stdout\n$stdout--- stderr\n$stderr"
  }

  private def runCase(path: Path): (List[String], String) = {
    val contractCase = ujson.read(Files.readString(path))
    val servesHttp = contractCase.obj.contains("requests")

    @tailrec
    def attempt(number: Int): (List[String], String) = {
      val sandbox = new Sandbox(contractCase, if (servesHttp) Some(freePort()) else None)
      val outcome =
        try {
          val run = if (servesHttp) runServer(contractCase, path, sandbox) else runOnce(path, sandbox)
          if (run.exitedBeforeServing && number < PortAttempts) None
          else Some((run.problems ++ compare(contractCase, sandbox, run), run.output))
        } finally sandbox.remove()
      outcome match {
        case Some(result) => result
        case None => attempt(number + 1)
      }
    }

    attempt(1)
  }

  private def start(path: Path, sandbox: Sandbox): Process = {
    val builder = new ProcessBuilder(Java, "-cp", Classpath, "the0.ContractBot", path.toString, sandbox.queryResult.toString)
    builder.environment().clear()
    builder.environment().putAll(sandbox.env.asJava)
    builder.redirectOutput(sandbox.stdout.toFile).redirectError(sandbox.stderr.toFile).start()
  }

  private def finish(process: Process, sandbox: Sandbox, problems: List[String], exitedBeforeServing: Boolean = false): Run =
    Run(
      Some(process.exitValue()),
      Files.readString(sandbox.stdout),
      Files.readString(sandbox.stderr),
      problems,
      exitedBeforeServing
    )

  private def runOnce(path: Path, sandbox: Sandbox): Run = {
    val process = start(path, sandbox)
    if (process.waitFor(RunTimeout.toSeconds, TimeUnit.SECONDS)) finish(process, sandbox, Nil)
    else {
      process.destroyForcibly().waitFor()
      finish(process, sandbox, List(s"the contract bot did not exit within ${RunTimeout.toSeconds}s"))
    }
  }

  private def runServer(contractCase: Value, path: Path, sandbox: Sandbox): Run = {
    val port = sandbox.port.get
    val process = start(path, sandbox)
    try {
      waitForPort(port, process) match {
        case Some(notReady) => finishServer(process, sandbox, List(notReady), exitedBeforeServing = !process.isAlive)
        case None =>
          val problems = contractCase("requests").arr.toList.zipWithIndex.flatMap { (request, index) =>
            requestMismatches(port, request, s"requests[$index] ${request("method").str} ${request("target").str}")
          }
          finishServer(process, sandbox, problems)
      }
    } finally process.destroyForcibly()
  }

  private def finishServer(process: Process, sandbox: Sandbox, problems: List[String], exitedBeforeServing: Boolean = false): Run = {
    process.destroy()
    if (!process.waitFor(10, TimeUnit.SECONDS)) process.destroyForcibly().waitFor()
    finish(process, sandbox, problems, exitedBeforeServing)
  }

  private def waitForPort(port: Int, process: Process): Option[String] = {
    val deadline = System.nanoTime() + ServerStartTimeout.toNanos
    @tailrec
    def poll(): Option[String] =
      if (!process.isAlive) Some(s"query server exited with code ${process.exitValue()} before accepting connections")
      else if (canConnect(port)) None
      else if (System.nanoTime() > deadline)
        Some(s"query server did not accept connections within ${ServerStartTimeout.toSeconds}s")
      else {
        Thread.sleep(50)
        poll()
      }
    poll()
  }

  private def requestMismatches(port: Int, request: Value, where: String): List[String] = {
    val expected = request("expect")
    Try(send(port, request)).fold(
      e => List(s"$where: request failed: $e"),
      response => {
        val headerProblems = expected.obj.get("headers").toList.flatMap(_.obj.toList).flatMap { (name, value) =>
          response.headers.firstValue(name).toScala match {
            case Some(actual) => mismatches(value, ujson.Str(actual), s"$where header $name")
            case None => List(s"$where header $name: missing")
          }
        }
        val bodyProblems = Try(ujson.read(response.body)).fold(
          _ => List(s"$where body: not JSON: ${response.body}"),
          mismatches(expected("body"), _, s"$where body")
        )
        mismatches(expected("status"), ujson.Num(response.status), s"$where status") ++ headerProblems ++ bodyProblems
      }
    )
  }

  private def compare(contractCase: Value, sandbox: Sandbox, run: Run): List[String] = {
    val expect = contractCase.obj.get("expect").map(_.obj).getOrElse(collection.mutable.LinkedHashMap.empty[String, Value])
    expect.get("exit_code").toList.flatMap(mismatches(_, run.exitCode.fold[Value](ujson.Null)(ujson.Num(_)), "exit_code")) ++
      expect.get("stdout").toList.flatMap(want => lineMismatches(want.arr.toList, run.stdout, "stdout")) ++
      expect.get("stderr").toList.flatMap(want => lineMismatches(want.arr.toList, run.stderr, "stderr")) ++
      expect.get("result").toList.flatMap(fileMismatches(_, sandbox.codeDir.resolve("result.json"), "result")) ++
      expect.get("query_result").toList.flatMap(fileMismatches(_, sandbox.queryResult, "query_result")) ++
      expect.get("state_after").toList.flatMap(mismatches(_, readState(sandbox.stateDir), "state_after"))
  }
}
