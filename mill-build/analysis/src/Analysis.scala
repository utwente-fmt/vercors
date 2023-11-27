package vct.col.ast.analysis

import scala.util.Using
import java.nio.file.{Path, Paths, Files}
import java.io.FileOutputStream
import upickle.default._

import vct.col.ast.structure._
import Util._

object Analysis {
  def main(args0: Array[String]): Unit = {
    val args = args0.toIndexedSeq
    val output = Paths.get(args.head)
    val input = args.tail.map(Paths.get(_))

    val hierarchy = Try("analysis") {
      val decls = input.map(read).get.flatten
      val typeLookup = decls.groupBy(_.name.base)

      val resolvedDecls = decls.map(StatAnalysis(typeLookup).getDeclaration).get

      HierarchyAnalysis.get(resolvedDecls).get
    } match {
      case Failures(failures) =>
        for(failure <- failures) {
          printFailure(failure)
        }
        sys.exit(1)
      case Ok(decls) => decls
    }

    write(output.resolve("col-decl-families.json"), hierarchy.declaredNodes.map(_._1))
    write(output.resolve("col-struct-families.json"), hierarchy.structuralNodes.map(_._1))
    write(output.resolve("col-definitions.json"), (hierarchy.declaredNodes ++ hierarchy.structuralNodes).flatMap(_._2))
    write(output.resolve("col-families.json"), (hierarchy.declaredNodes ++ hierarchy.structuralNodes).map { case category -> defns => category -> defns.map(_.name) })
  }

  def read(path: Path): Result[Seq[RawStatAnalysis.RawStat]] = Try(s"reading $path") {
    FileAnalysis.namedDecls(scala.meta.Input.File(path)).get
  }

  def write[T: Writer](path: Path, out: T): Unit =
    Using(Files.newOutputStream(path)) { writer =>
      writeToOutputStream(out, writer)
    }
}