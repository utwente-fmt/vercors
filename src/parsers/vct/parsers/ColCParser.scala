package vct.parsers
import com.typesafe.scalalogging.LazyLogging
import hre.io.{RWFile, Readable}
import org.antlr.v4.runtime.{CharStream, CharStreams}
import vct.col.origin.{Origin, ReadableOrigin}
import vct.parsers.CParser.PreprocessorError
import vct.parsers.transform.BlameProvider
import vct.result.VerificationError.{Unreachable, UserError}

import java.io.{FileNotFoundException, InputStreamReader, OutputStreamWriter, StringWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path, Paths}

case object CParser {
  case class PreprocessorError(fileName: String, errorCode: Int, error: String) extends UserError {
    override def code: String = "preprocessorError"
    override def text: String =
      s"Preprocesing file $fileName failed with exit code $errorCode:\n$error"
  }
}

case class ColCParser(override val origin: Origin,
                      override val blameProvider: BlameProvider,
                      cc: Path,
                      systemInclude: Path,
                      otherIncludes: Seq[Path],
                      defines: Map[String, String]) extends Parser(origin, blameProvider) with LazyLogging {
  def interpret(localInclude: Seq[Path], input: String, output: String): Process = {
    var command = Seq(cc.toString, "-C", "-E")

    command ++= Seq("-nostdinc", "-nocudainc", "-nocudalib", "--cuda-host-only")
    command ++= Seq("-isystem", systemInclude.toAbsolutePath.toString)

    command ++= localInclude.map("-I" + _.toAbsolutePath)
    command ++= otherIncludes.map("-I" + _.toAbsolutePath.toString)
    command ++= defines.map { case (k, v) => s"-D$k=$v" }
    command ++= Seq("-o", output)
    command :+= input

    logger.debug(command.toString())

    new ProcessBuilder(command:_*).start()
  }

  override def parse[G](stream: CharStream): ParseResult[G] = {
    throw Unreachable("Should not parse C files from an ANTLR CharStream: they need to be interpreted first!")
  }

  override def parse[G](readable: Readable): ParseResult[G] =
    try {
      val interpreted = Files.createTempFile("vercors-interpreted-", ".i")

      try {
        val process = interpret(
          localInclude = Option(Paths.get(readable.fileName).getParent).toSeq,
          input = "-",
          output = interpreted.toString
        )
        new Thread(() => {
          val writer = new OutputStreamWriter(process.getOutputStream, StandardCharsets.UTF_8)
          try {
            readable.read { reader =>
              val written = reader.transferTo(writer)
              logger.debug(s"Wrote $written bytes to clang")
            }
          } finally {
            writer.close()
          }
        }, "[VerCors] clang stdout writer").start()
        process.waitFor()

        if (process.exitValue() != 0) {
          val writer = new StringWriter()
          new InputStreamReader(process.getInputStream).transferTo(writer)
          new InputStreamReader(process.getErrorStream).transferTo(writer)
          writer.close()
          throw PreprocessorError(readable.fileName, process.exitValue(), writer.toString)
        }

        val ireadable = RWFile(interpreted, doWatch = false)
        val result = ColIParser(Origin(Seq(ReadableOrigin(ireadable))), blameProvider, Some(origin)).parse[G](ireadable)
        result
      } finally {
        Files.delete(interpreted)
      }
    } catch {
      case _: FileNotFoundException | _: NoSuchFileException => throw FileNotFound(readable.fileName)
    }
}
