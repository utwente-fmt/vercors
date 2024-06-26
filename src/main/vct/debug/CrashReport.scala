package vct.debug

import hre.io.{CollectString, Readable}
import hre.log.LogHistory
import vct.main.BuildInfo
import vct.options.Options

import java.net.{URI, URL, URLEncoder}
import java.nio.charset.StandardCharsets

object CrashReport {
  val GITHUB_URI = "https://github.com/utwente-fmt/vercors/issues/new"

  private def enc(s: String): String =
    URLEncoder.encode(s, StandardCharsets.UTF_8)

  def makeGithubLink(
      err: Throwable,
      args: Array[String],
      options: Options,
  ): String = {
    var (title, body) = make(err, args, options)

    var uri: String = ""
    var lastBody: String = ""

    val params =
      s"labels=${enc("A-Bug")}&title=${enc(title)}&body=${enc(body)}"
    uri = GITHUB_URI + "?" + params
    lastBody = body
    body = body.replaceFirst("\n[^\n]*$", "")

    while (uri.length > 8000 && body.length < lastBody.length) {
      val params =
        s"labels=${enc("A-Bug")}&title=${enc(title)}&body=${enc(body)}"
      uri = GITHUB_URI + "?" + params
      lastBody = body
      body = body.replaceFirst("\n[^\n]*$", "")
    }

    uri
  }

  def make(
      err: Throwable,
      args: Array[String],
      options: Options,
  ): (String, String) =
    s"Crash report: ${err.getMessage.take(100)}" ->
      (makeError(err) + makeVersion() + makeOptions(args) +
        makeInputs(options) + makeLog())

  def makeError(err: Throwable): String =
    s"""# Crash Message
       |```
       |${err.getMessage}
       |${makeStackTrace(err.getStackTrace)}
       |```
       |
       |""".stripMargin

  def makeStackTrace(elems: Array[StackTraceElement]): String = {
    if (elems.length < 15)
      elems.map(elem => s"  at $elem").mkString("\n")
    else
      (elems.take(12).map(elem => s"  at $elem") ++ Seq("  ...") ++
        elems.takeRight(2).map(elem => s"  at $elem")).mkString("\n")
  }

  def makeVersion(): String =
    s"""# Version Information
       |* ${BuildInfo.name} version `${BuildInfo.version}`
       |* At commit ${BuildInfo.currentCommit} from branch `${BuildInfo
        .currentBranch}` (changes=${BuildInfo.gitHasChanges})
       |
       |""".stripMargin

  def makeOptions(args: Array[String]): String =
    s"""# Arguments
       |${args.mkString("`", "` `", "`")}
       |
       |""".stripMargin

  def makeInputs(options: Options): String =
    s"""# File Inputs
       |${options.inputs.map(makeInput).mkString("\n")}
       |
       |""".stripMargin

  def makeInput(readable: Readable): String =
    if (readable.isRereadable)
      s"""<details>
         |<summary>${readable.fileName}</summary>
         |
         |```${readable.fileName.split('.').last}
         |${readable.readToCompletion()}
         |```
         |</details>
         |""".stripMargin
    else
      s"`${readable.fileName}` cannot be read"

  def makeLog(): String =
    s"""# Full Log
       |<details>
       |
       |```
       |${LogHistory.messages.toString}
       |```
       |</details>
       |""".stripMargin
}
