package vct.java

import hre.io.RWFile
import vct.col.ast.JavaNamespace
import vct.col.resolve.ExternalJavaLoader
import vct.parsers.transform.{BlameProvider, ReadableOriginProvider}
import vct.parsers.{ColJavaParser, FileNotFound}

import java.io.File
import java.nio.file.Path

case class JavaLibraryLoader(base: Path, blameProvider: BlameProvider) extends ExternalJavaLoader {
  override def load[G](name: Seq[String]): Option[JavaNamespace[G]] = try {
    val f = RWFile(base.resolve((name.init :+ name.last + ".java").mkString(File.separator)).toFile)
    ColJavaParser(ReadableOriginProvider(f), blameProvider).parse[G](f).decls match {
      case Seq(ns: JavaNamespace[G]) => Some(ns)
      case _ => None
    }
  } catch {
    case _: FileNotFound => None
  }
}
