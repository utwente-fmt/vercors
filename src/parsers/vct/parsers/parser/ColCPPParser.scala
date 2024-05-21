package vct.parsers.parser

import com.typesafe.scalalogging.LazyLogging
import hre.io.{RWFile, Readable}
import org.antlr.v4.runtime.CharStream
import vct.col.origin.{Origin, ReadableOrigin}
import vct.parsers.debug.DebugOptions
import vct.parsers.err.FileNotFound
import vct.parsers.parser.CParser.PreprocessorError
import vct.parsers.transform.BlameProvider
import vct.parsers.{ParseResult, Parser}
import vct.result.VerificationError.{Unreachable, UserError}

import java.io.{FileNotFoundException, InputStreamReader, OutputStreamWriter, Reader, StringWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path, Paths}

case object CPPParser {
  case class PreprocessorError(fileName: String, errorCode: Int, error: String) extends UserError {
    override def code: String = "preprocessorError"
    override def text: String =
      s"Preprocesing file $fileName failed with exit code $errorCode:\n$error"
  }
}

case class ColCPPParser(debugOptions: DebugOptions,
                        blameProvider: BlameProvider,
                        cc: Path,
                        systemInclude: Path,
                        otherIncludes: Seq[Path],
                        defines: Map[String, String]) extends Parser with LazyLogging {

  def interpret(input: String, output: String): Process = {
    var command = Seq(cc.toString, "-C", "-E")

    command ++= Seq("-nostdinc")
    command ++= Seq("-isystem", systemInclude.toAbsolutePath.toString)

    command ++= otherIncludes.map("-I" + _.toAbsolutePath.toString)
    command ++= defines.map { case (k, v) => s"-D$k=$v" }
    command ++= Seq("-o", output)
    command :+= input

    logger.debug(command.toString())

    new ProcessBuilder(command:_*).start()
  }

  override def parseReader[G](reader: Reader, baseOrigin: Origin = Origin(Nil)): ParseResult[G] = {
    val interpreted = Files.createTempFile("vercors-interpreted-", ".ipp")
    try {
      val process = interpret(
        input = "-",
        output = interpreted.toString
      )
      new Thread(() => {
        val writer = new OutputStreamWriter(process.getOutputStream, StandardCharsets.UTF_8)
        try {
          val written = reader.transferTo(writer)
          logger.debug(s"Wrote $written bytes to clang++")
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
        throw PreprocessorError(baseOrigin.shortPositionText, process.exitValue(), writer.toString)
      }

      val ireadable = RWFile(interpreted, doWatch = false)
      val result = ColIPPParser(debugOptions, blameProvider, Some(baseOrigin)).parse[G](ireadable)
      result
    } finally {
      Files.delete(interpreted)
    }
  }
}
