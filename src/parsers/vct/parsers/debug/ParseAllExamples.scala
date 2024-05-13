package vct.parsers.debug

import org.antlr.v4.runtime.atn.{ATNSimulator, ParserATNSimulator}
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, Recognizer}
import vct.antlr4.generated.{CPPParser, CParser, JavaParser, LangCLexer, LangCPPLexer, LangJavaLexer, LangPVLLexer, PVLParser}

import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.Using

object ParseAllExamples {
  def track[T <: Recognizer[_, _ <: ATNSimulator]](recognizer: T): T = {
    recognizer
  }

  def tryParse(path: Path, ext: String): Unit =
    ext match {
      case "pvl" =>
        track(new PVLParser(new CommonTokenStream(new LangPVLLexer(CharStreams.fromReader(Files.newBufferedReader(path)))))).program()
      case "java" =>
        track(new JavaParser(new CommonTokenStream(new LangJavaLexer(CharStreams.fromReader(Files.newBufferedReader(path)))))).compilationUnit()
      case "c" | "cu" | "cl" | "h" =>
        val interp = new ProcessBuilder("clang", "-E", "-nostdinc", "-nocudainc", "-nocudalib", "--cuda-host-only", "-o", "-", path.toString)
        val reader = new InputStreamReader(interp.start().getInputStream, StandardCharsets.UTF_8)
        track(new CParser(new CommonTokenStream(new LangCLexer(CharStreams.fromReader(reader))))).compilationUnit()
      case "cpp" =>
        val interp = new ProcessBuilder("clang", "-E", "-nostdinc", "-nocudainc", "-nocudalib", "--cuda-host-only", "-o", "-", path.toString)
        val reader = new InputStreamReader(interp.start().getInputStream, StandardCharsets.UTF_8)
        track(new CPPParser(new CommonTokenStream(new LangCPPLexer(CharStreams.fromReader(reader))))).translationUnit()
    }

  def getExt(path: Path): Option[String] = {
    val parts = path.getFileName.toString.split('.')
    if(parts.length == 2 && !parts.head.isBlank) {
      val ext = parts(1)
      Some(ext).filter(Set("pvl", "java", "c", "cu", "cl", "h", "cpp").contains)
    } else
      None
  }

  def replaceContextCache(cls: Class[_ <: Recognizer[_, _ <: ATNSimulator]]): Unit = {
    val field = cls.getField("_sharedContextCache")
    field.setAccessible(true)
    field.set(null, new ProfilingPredictionContextCache)
  }

  def main(args: Array[String]): Unit = {
    val recogs: Seq[Class[_ <: Recognizer[_, _ <: ATNSimulator]]] = Seq(
      classOf[PVLParser],
      classOf[JavaParser],
      classOf[CParser],
      classOf[CPPParser],
      classOf[LangPVLLexer],
      classOf[LangJavaLexer],
      classOf[LangCLexer],
      classOf[LangCPPLexer],
    )

    for(recog <- recogs) {
      replaceContextCache(recog)
    }

    val paths = Files.walk(Paths.get("examples")).toScala(Seq)
    val files = paths.collect { case path if getExt(path).nonEmpty => getExt(path).get -> path }
    for((ext -> file, i) <- files.zipWithIndex) {
      println(s"[${i+1}/${files.size}] $file")
      tryParse(file, ext)
    }
    System.gc()
    println("ready")
    synchronized {
      wait()
    }
  }
}
