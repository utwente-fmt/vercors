package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants.{AbstractRewriter, OpsPackage, Declaration, Long, Map}
import vct.col.ast.helpers.defn.Naming.{declareType, opsFamilyTrait, scalapbType, typ}
import vct.col.ast.structure
import vct.col.ast.structure.{DeclaredNode, FamilyGenerator, StructuralNode}

import java.nio.file.Path
import scala.meta._

class OpsFamily extends FamilyGenerator {
  override def generate(out: Path, family: structure.Name, kind: structure.NodeKind, nodes: Seq[structure.Name]): Unit =
    ResultStream.write(out.resolve(s"${family.base}FamilyOps.scala"),
      source"""
        package $OpsPackage

        trait ${opsFamilyTrait(family)}[G]
          extends ..${templates(family, kind)}
        { this: ${typ(family)}[G] =>
          override def rewriteDefault[Post]()(implicit rw: $AbstractRewriter[G, Post]): ${typ(family)}[Post]
          override def serializeFamily(decls: $Map[$Declaration[G], $Long]): ${scalapbType(family)}
        }
      """
    )

  def templates(name: structure.Name, kind: structure.NodeKind): List[Init] =
    kind match {
      case StructuralNode => List.empty
      case DeclaredNode => List(
        Init(t"${declareType(name)}[G]", Name.Anonymous(), List.empty),
      )
    }
}
