package vct.col.ast.helpers.generator

import vct.col.ast.structure.{NodeDefinition, NodeGenerator}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using
import scala.meta._
import scala.meta.internal.prettyprinters.TreeSyntax
import scala.meta.dialects

class Compare extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit = {
    Using(Files.newBufferedWriter(out.resolve(s"${node.name.base}Compare.scala"), StandardCharsets.UTF_8)) { fileWriter =>
      try {
        val result = TreeSyntax[Tree](dialects.Scala213)(getTree(node))
        ResultStream.write(fileWriter, result)
      } catch {
        case t: Throwable => fileWriter.append(t.toString)
      }
    }
  }

  def getTree(node: NodeDefinition): Source = {
    source"""
      package vct.col.ast.compare

      trait ${Type.Name(node.name.base + "Compare")} {
       def compare(other: Node[G]): Boolean = false
      }
    """
  }
}
