package vct.importer

import hre.io.RWFile
import vct.col.ast.JavaNamespace
import vct.col.resolve.ExternalJavaLoader
import vct.parsers.transform.{BlameProvider, ReadableOriginProvider}
import vct.parsers.{ColJavaParser, FileNotFound}

import java.io.File
import java.nio.file.Path

case class JavaLibraryLoader(blameProvider: BlameProvider) extends ExternalJavaLoader {
  override def load[G](base: Path, name: Seq[String]): Option[JavaNamespace[G]] = try {
    val f = RWFile(base.resolve((name.init :+ name.last + ".java").mkString(File.separator)).toFile)
    ColJavaParser(ReadableOriginProvider(f), blameProvider).parse[G](f).decls match {
      case Seq(ns: JavaNamespace[G]) => Some(ns)
      case _ => None
    }
  } catch {
    case _: FileNotFound => None
  }

  override def loadPkg[G](base: Path, pkg: Seq[String]): Seq[JavaNamespace[G]] = {
    var resolved = base
    for(elem <- pkg) {
      resolved = resolved.resolve(elem)
    }
    resolved.toFile.listFiles().map { f =>
      f.getName match {
        case s"${className}.java" =>
          val fqn = pkg :+ className
          load[G](base, fqn)
        case _ => None
      }
    }.collect { case Some(ns) => ns }.toSeq
  }
}
