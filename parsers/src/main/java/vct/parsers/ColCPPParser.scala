package vct.parsers

import java.nio.file.{Path, Paths}
import com.typesafe.scalalogging.LazyLogging
import hre.io.{RWFile, Readable}
import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated._
import vct.parsers.CPPParser.PreprocessorError
import vct.parsers.transform.{BlameProvider, CPPToCol, InterpretedFileOriginProvider, OriginProvider}
import vct.result.VerificationError.{Unreachable, UserError}

import java.io.{File, FileNotFoundException, InputStreamReader, OutputStreamWriter, StringWriter}
import java.nio.charset.StandardCharsets


case object CPPParser {
  case class PreprocessorError(fileName: String, errorCode: Int, error: String) extends UserError {
    override def code: String = "preprocessorError"
    override def text: String =
      s"Preprocesing file $fileName failed with exit code $errorCode:\n$error"
  }
}

case class ColCPPParser(override val originProvider: OriginProvider,
                      override val blameProvider: BlameProvider,
                      cc: Path,
                      systemInclude: Path,
                      otherIncludes: Seq[Path],
                      defines: Map[String, String]) extends Parser(originProvider, blameProvider) with LazyLogging {

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

  override def parseCPPI[G](stream: CharStream): ParseResult[G] = {
    try {
      val lexer = new LangCPPLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      originProvider.setTokenStream(tokens)
      val errors = expectedErrors(tokens, LangCPPLexer.EXPECTED_ERROR_CHANNEL, LangCPPLexer.VAL_EXPECT_ERROR_OPEN, LangCPPLexer.VAL_EXPECT_ERROR_CLOSE)
      val parser = new CPPParser(tokens)
      val ec = errorCounter(parser, lexer, originProvider)
      val tree = parser.translationUnit()
      ec.report()
      val decls = CPPToCol[G](originProvider, blameProvider, errors).convert(tree)
      ParseResult(decls, errors.map(_._3))
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }

  override def parse[G](readable: Readable): ParseResult[G] =
    try {
      val interpreted = File.createTempFile("vercors-interpreted-", ".i")
      interpreted.deleteOnExit()

      val process = interpret(localInclude=Seq(Paths.get(readable.fileName).getParent), input="-", output=interpreted.toString)
      new Thread(() => {
        val writer = new OutputStreamWriter(process.getOutputStream, StandardCharsets.UTF_8)
        readable.read { reader =>
          val written = reader.transferTo(writer)
          logger.debug(s"Wrote $written bytes to clang")
          writer.close()
        }
      }).start()
      process.waitFor()

      if(process.exitValue() != 0) {
        val writer = new StringWriter()
        new InputStreamReader(process.getInputStream).transferTo(writer)
        new InputStreamReader(process.getErrorStream).transferTo(writer)
        writer.close()
        throw PreprocessorError(readable.fileName, process.exitValue(), writer.toString)
      }

      val result = ColCPPIParser(InterpretedFileOriginProvider(originProvider, RWFile(interpreted)), blameProvider).parse[G](RWFile(interpreted))
      result
    } catch {
      case _: FileNotFoundException => throw FileNotFound(readable.fileName)
    }


}
