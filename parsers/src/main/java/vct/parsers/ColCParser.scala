package vct.parsers
import hre.config.Configuration
import org.antlr.v4.runtime.CharStream
import vct.col.ast.GlobalDeclaration
import vct.parsers.transform.{BlameProvider, OriginProvider}
import vct.result.VerificationResult.Unreachable

import java.io.{File, InputStream}
import java.nio.file.Path
import scala.jdk.CollectionConverters._

case class ColCParser() extends Parser {
  def interpret(localInclude: Seq[Path], input: String, output: String): Process = {
    // TODO PB: this is not great, should really parse it or so.
    var command = Configuration.cpp_command.get().split(' ').toSeq

    command ++= Seq("-nostdinc", "-nocudainc", "-nocudalib", "--cuda-host-only")
    command ++= Seq("-isystem", Configuration.getCIncludePath.getAbsolutePath)

    command ++= localInclude.map("-I" + _.toAbsolutePath)
    command ++= Configuration.cpp_include_path.asScala.map("-I" + _)
    command ++= Configuration.cpp_defines.asScala.map("-D" + _)
    command ++= Seq("-o", output)
    command :+= input

    new ProcessBuilder(command:_*).start()
  }

  override def parse[G](stream: CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult[G] = {
    throw Unreachable("Should not parse C files from an ANTLR CharStream: they need to be interpreted first!")
  }

  override def parse[G](stream: InputStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult[G] = {
    val process = interpret(localInclude=Nil, input="-", output="-")
    new Thread(() => stream.transferTo(process.getOutputStream)).start()
    val result = ColIParser().parse[G](process.getInputStream, originProvider, blameProvider)
    process.destroy()
    result
  }

  override def parse[G](f: File)(originProvider: OriginProvider = null,
                              blameProvider: BlameProvider = null): ParseResult[G] = {
    // TODO PB: this needs some more thinking: we're ignoring the providers here. Really this whole abstraction doesn't fit for C.
    val interpreted = File.createTempFile("vercors-interpreted-", ".i")
    val process = interpret(localInclude=Option(f.toPath.getParent).toSeq, input=f.getAbsolutePath, output=interpreted.getAbsolutePath)
    process.waitFor()
    ColIParser().parse(interpreted)()
  }
}
