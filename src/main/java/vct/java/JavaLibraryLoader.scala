package vct.java

import hre.config.Configuration
import hre.io.RWFile
import hre.lang.Failure
import vct.col.ast.JavaNamespace
import vct.col.resolve.ExternalJavaLoader
import vct.main.Vercors
import vct.parsers.transform.{BlameProvider, ReadableOriginProvider}
import vct.parsers.{ColJavaParser, FileNotFound}

import java.io.File
import java.nio.file.{Path, Paths}

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
