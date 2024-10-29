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
    val output = os.Path(args.head)
    val input = args.tail.map(os.Path(_))

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

    val h = hierarchy

    val nodes = (h.declaredNodes ++ h.structuralNodes).flatMap(_._2)
    val nodeNames = nodes.map(_.name)
    write(output / "cross-node.json", nodeNames)

    for(node <- nodes) {
      write(output / "cross-node" / node.name.path / "node.json", node)
    }

    val families =
      hierarchy.declaredNodes.map { case family -> defns => FamilyDefinition(family, DeclaredNode, defns.map(_.name)) } ++
        hierarchy.structuralNodes.map { case family -> defns => FamilyDefinition(family, StructuralNode, defns.map(_.name)) }
    val familyNames = families.map(_.name)
    write(output / "cross-family.json", familyNames)

    for(family <- families) {
      write(output / "cross-family" / family.name.path / "family.json", family)
    }

    write(output / "all-families.json", AllFamilies(h.declaredNodes.map(_._1), h.structuralNodes.map(_._1)))

    write(output / "all-definitions.json", nodes)
  }

  def read(path: os.Path): Result[Seq[RawStatAnalysis.RawStat]] = Try(s"reading $path") {
    FileAnalysis.namedDecls(scala.meta.Input.File(path.toNIO)).get
  }

  def write[T: Writer](path: os.Path, out: T): Unit =
    Using(os.write.outputStream(path, createFolders = true)) { writer =>
      writeToOutputStream(out, writer)
    }
}