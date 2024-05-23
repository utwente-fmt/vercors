package vct.importer

import hre.io.RWFile
import vct.col.ast.JavaNamespace
import vct.col.origin.{Origin, ReadableOrigin}
import vct.col.resolve.ExternalJavaLoader
import vct.parsers.debug.DebugOptions
import vct.parsers.transform.BlameProvider
import vct.parsers.err.FileNotFound
import vct.parsers.parser
import vct.parsers.parser.ColJavaParser

import java.io.File.{separator => FILE_SEPARATOR}
import java.nio.file.Path

case class JavaLibraryLoader(blameProvider: BlameProvider, debugOptions: DebugOptions) extends ExternalJavaLoader {
  override def load[G](base: Path, name: Seq[String]): Option[JavaNamespace[G]] = try {
    val f = RWFile(base.resolve((name.init :+ name.last + ".java").mkString(FILE_SEPARATOR)))
    parser.ColJavaParser(debugOptions, blameProvider).parse[G](f).decls match {
      case Seq(ns: JavaNamespace[G]) => Some(ns)
      case _ => None
    }
  } catch {
    case _: FileNotFound => None
  }
}
