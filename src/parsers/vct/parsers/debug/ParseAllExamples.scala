package vct.parsers.debug

import hre.io.RWFile
import org.antlr.v4.runtime.atn.PredictionMode
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Parser}
import vct.antlr4.generated._
import vct.col.origin.{Origin, ReadableOrigin}
import vct.parsers.CollectingErrorListener

import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.StreamConverters.StreamHasToScala

object ParseAllExamples {
  def track[T <: Parser](read: hre.io.Readable, recognizer: T): T = {
    recognizer.getInterpreter.setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION)
    recognizer.removeErrorListeners()
    recognizer.addErrorListener(CollectingErrorListener(Origin(Seq(ReadableOrigin(read)))))
    recognizer
  }

  def tryParse(path: Path, ext: String): Unit = {
    val f = RWFile(path)
    val tmp = Files.createTempFile("vercors-", ".txt")
    f.read { reader =>
      ext match {
        case "pvl" =>
          track(f, new PVLParser(new CommonTokenStream(new LangPVLLexer(CharStreams.fromReader(reader))))).program()
        case "java" =>
          track(f, new JavaParser(new CommonTokenStream(new LangJavaLexer(CharStreams.fromReader(reader))))).compilationUnit()
        case "c" | "cu" | "cl" | "h" =>
          val interp = new ProcessBuilder("clang", "-E", "-nostdinc", "-nocudainc", "-nocudalib", "--cuda-host-only", "-o", tmp.toString, path.toString)
          interp.start().waitFor()
          RWFile(tmp).read { reader =>
            track(RWFile(tmp), new CParser(new CommonTokenStream(new LangCLexer(CharStreams.fromReader(reader))))).compilationUnit()
          }
        case "cpp" =>
          val interp = new ProcessBuilder("clang", "-E", "-nostdinc", "-nocudainc", "-nocudalib", "--cuda-host-only", "-o", tmp.toString, path.toString)
          interp.start().waitFor()
          RWFile(tmp).read { reader =>
            track(RWFile(tmp), new CPPParser(new CommonTokenStream(new LangCPPLexer(CharStreams.fromReader(reader))))).translationUnit()
          }
      }
    }
  }

  val ALLOWED_EXTS =
    Set("pvl", "java", "c", "cu", "cl", "h", "cpp")

  def getExt(path: Path): Option[String] = {
    val parts = path.getFileName.toString.split('.')
    if(parts.length == 2 && !parts.head.isBlank) {
      val ext = parts(1)
      Some(ext).filter(ALLOWED_EXTS.contains)
    } else
      None
  }

  /**
   * Parse all files in a given directory, or just one file.
   * Argument 1: file or directory to parse files from (optional, default: examples)
   * Argument 2: set of extensions to filter for (optional, default: pvl,java,c,cu,cl,h,cpp)
   */
  def main(args: Array[String]): Unit = {
    val basePath = if(args.isDefinedAt(0)) args(0) else "examples"
    val exts = if(args.isDefinedAt(1)) args(1).split(',').toSet else ALLOWED_EXTS

    val paths = Files.walk(Paths.get(basePath)).toScala(Seq)
    val files = paths.collect { case path if getExt(path).nonEmpty && exts.contains(getExt(path).get) => getExt(path).get -> path }
    for((ext -> file, i) <- files.sortBy(_._2.toString).zipWithIndex) {
      println(s"[${i+1}/${files.size}] $file")
      tryParse(file, ext)
    }
    System.gc()
    println("[waiting - connect e.g. VisualVM for memory diagnostics]")
    synchronized {
      wait()
    }
  }
}
