package vct.rewrite.cfg

import hre.io.RWFile
import vct.col.ast.{BooleanValue, Expr, Procedure}
import vct.col.origin.Origin

import java.io.Writer
import java.nio.file.Path
import scala.collection.mutable

case class CFGPrinter[G]() {
  private val node_names: mutable.Map[CFGEntry[G], String] = mutable
    .HashMap[CFGEntry[G], String]()
  private val no_condition: Expr[G] = BooleanValue(value = true)(Origin(Seq()))
  private var node_index: Int = 0

  private def print_cfg(writer: Writer): Unit = {
    writer.append("digraph {\n")
    // Write all nodes
    node_names.foreach(naming =>
      writer.append(naming._2).append(s" [label=${"\""}")
        .append(naming._1.toString).append(s"${"\""}];\n")
    )
    // Write all edges
    node_names.foreach(naming =>
      naming._1.get_successors.foreach(edge =>
        writer.append(naming._2).append(" -> ").append(node_names(edge.target))
          .append(s" [label=${"\""}")
          .append(edge.condition.getOrElse(no_condition).toInlineString)
          .append(s"${"\""}];\n")
      )
    )
    writer.append("}")
  }

  def print_ast_as_cfg(entry_point: Procedure[G], path: Path): Unit = {
    val cfg_root: CFGNode[G] = CFGGenerator().generate(entry_point)
    find_all_nodes(cfg_root)
    RWFile(path).write(w => print_cfg(w))
  }

  private def find_all_nodes(node: CFGEntry[G]): Unit = {
    if (!node_names.contains(node)) {
      // Give new name to node
      val node_name: String = s"n$node_index"
      node_index += 1
      node_names.addOne((node, node_name))

      // Recursively call for successors
      node.get_successors.foreach(e => find_all_nodes(e.target))
    }
  }
}
