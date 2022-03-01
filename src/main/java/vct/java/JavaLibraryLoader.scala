package vct.java

import hre.config.Configuration
import hre.lang.Failure
import vct.col.ast.JavaNamespace
import vct.col.resolve.ExternalJavaLoader
import vct.parsers.{FileNotFound, Parsers}

import java.nio.file.Paths

case object JavaLibraryLoader extends ExternalJavaLoader {
  override def load[G](name: Seq[String]): Option[JavaNamespace[G]] = try {
    val f = Configuration.getFileOrAbort(Paths.get("/jdk", (name.init :+ name.last + ".java") : _*))
    Parsers.parse[G](f.toPath).decls match {
      case Seq(ns: JavaNamespace[G]) => Some(ns)
      case _ => None
    }
  } catch {
    case _: FileNotFound => None
    case _: Failure => None
  }
}
