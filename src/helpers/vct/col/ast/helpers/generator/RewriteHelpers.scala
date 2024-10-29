package vct.col.ast.helpers.generator

import vct.col.ast.structure.{AllNodesGenerator, NodeDefinition}

import java.nio.file.Path
import scala.meta._

class RewriteHelpers extends AllNodesGenerator {
  override def generate(out: Path, definitions: Seq[NodeDefinition]): Unit =
    ResultStream.write(
      out.resolve("RewriteHelpers.scala"),
      source"""
        package vct.col.ast

        @deprecated("node.rewrite(...) methods are defined directly on nodes now, so importing this does nothing.")
        object RewriteHelpers {
          ..${definitions.map(defn => {
          q"""
              @deprecated("node.rewrite(...) methods are defined directly on nodes now, so importing this does nothing.")
              object ${Term.Name("Rewrite" + defn.name.base)}
            """
        }).toList}
        }
      """,
    )
}
