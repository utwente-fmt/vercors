package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants.OpsPackage
import vct.col.ast.helpers.defn.Naming._
import vct.col.ast.structure.{NodeDefinition, NodeGenerator}

import java.nio.file.Path
import scala.meta._

class Ops extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit =
    ResultStream.write(out.resolve(s"${node.name.base}Ops.scala"),
      source"""
        package $OpsPackage

        trait ${opsTrait(node)}[G]
          extends ${Init(t"${compareType(node)}[G]", Name.Anonymous(), Nil)}
          with ${Init(t"${rewriteType(node)}[G]", Name.Anonymous(), Nil)}
        { this: ${typ(node)}[G] => }
      """
    )
}
