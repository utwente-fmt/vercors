package vct.parsers
import hre.config.Configuration
import hre.io.{RWFile, Readable}
import org.antlr.v4.runtime.{CharStream, CharStreams}
import vct.parsers.transform.{BlameProvider, OriginProvider}
import vct.result.VerificationError.{Unreachable, UserError}

import java.io.{File, FileNotFoundException, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

case object CParser {
  case class PreprocessorError(path: Path, errorCode: Int, error: String) extends UserError {
    override def code: String = "preprocessorError"
    override def text: String =
      s"Preprocesing file $path failed with exit code $errorCode:\n$error"
  }
}

case class ColCParser(override val originProvider: OriginProvider,
                      override val blameProvider: BlameProvider,
                      cc: Path,
                      systemInclude: Path,
                      otherIncludes: Seq[Path],
                      defines: Map[String, String]) extends Parser(originProvider, blameProvider) {
  def interpret(localInclude: Seq[Path], input: String, output: String): Process = {
    var command = Seq(cc.toString)

    command ++= Seq("-nostdinc", "-nocudainc", "-nocudalib", "--cuda-host-only")
    command ++= Seq("-isystem", systemInclude.toAbsolutePath.toString)

    command ++= localInclude.map("-I" + _.toAbsolutePath)
    command ++= otherIncludes.map("-I" + _.toAbsolutePath.toString)
    command ++= defines.map { case (k, v) => s"-D$k=$v" }
    command ++= Seq("-o", output)
    command :+= input

    new ProcessBuilder(command:_*).start()
  }

  override def parse[G](stream: CharStream): ParseResult[G] = {
    throw Unreachable("Should not parse C files from an ANTLR CharStream: they need to be interpreted first!")
  }

  override def parse[G](readable: Readable): ParseResult[G] =
    try {
      readable.read { reader =>
        val interpreted = File.createTempFile("vercors-interpreted-", ".i")
        interpreted.deleteOnExit()

        val process = interpret(localInclude=Nil, input="-", output=interpreted.toString)
        new Thread(() => reader.transferTo(new OutputStreamWriter(process.getOutputStream, StandardCharsets.UTF_8))).start()

        val result = ColIParser(???, ???).parse[G](RWFile(interpreted))
        process.destroy()
        result
      }
    } catch {
      case _: FileNotFoundException => throw FileNotFound(readable.fileName)
    }
}
