package vct.java

import hre.config.Configuration
import hre.lang.Failure
import vct.col.ast.JavaNamespace
import vct.col.resolve.ExternalJavaLoader
import vct.parsers.{FileNotFound, Parsers}

import java.io.File
import java.nio.file.{Path, Paths}

case class JavaLibraryLoader(base: Path) extends ExternalJavaLoader {
  override def load[G](name: Seq[String]): Option[JavaNamespace[G]] = try {
    val f = base.resolve((name.init :+ name.last + ".java").mkString(File.separator))
    Parsers.parse[G](f).decls match {
      case Seq(ns: JavaNamespace[G]) => Some(ns)
      case _ => None
    }
  } catch {
    case _: FileNotFound => None
    case _: Failure => None
  }
}
