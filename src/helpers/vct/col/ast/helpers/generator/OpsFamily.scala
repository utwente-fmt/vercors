package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants.{AbstractRewriter, OpsPackage}
import vct.col.ast.helpers.defn.Naming.{opsFamilyTrait, typ}
import vct.col.ast.structure.{FamilyGenerator, Name}

import java.nio.file.Path
import scala.meta._

class OpsFamily extends FamilyGenerator {
  override def generate(out: Path, family: Name, nodes: Seq[Name]): Unit =
    ResultStream.write(out.resolve(s"${family.base}FamilyOps.scala"),
      source"""
        package $OpsPackage

        trait ${opsFamilyTrait(family)}[G]
        { this: ${typ(family)}[G] =>
          def rewriteDefault[Post]()(implicit rw: $AbstractRewriter[G, Post]): ${typ(family)}[Post]
        }
      """
    )
}
