package vct.java

import vct.col.ast.JavaNamespace
import vct.col.resolve.ExternalJavaLoader
import vct.parsers.{FileNotFound, Parsers}

import java.nio.file.Paths

case object JavaLibraryLoader extends ExternalJavaLoader {
  override def load[G](name: Seq[String]): Option[JavaNamespace[G]] = try {
    Parsers.parse[G](Paths.get("/home/pieter/vercors/src/main/universal/res/jdk", (name.init :+ name.last + ".java") : _*)).decls match {
      case Seq(ns: JavaNamespace[G]) => Some(ns)
      case _ => None
    }
  } catch {
    case _: FileNotFound => None
  }
}
