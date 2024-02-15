package vct.main.stages

import hre.stages.Stage
import sourcecode.Name
import vct.col.ast.{InstanceMethod, Node}
import vct.col.rewrite.Generation
import vct.options.Options
import vct.rewrite.cfg.CFGPrinter

import java.nio.file.Path

case object PrintCFG {
  def ofOptions(options: Options): Stage[Node[_ <: Generation], Unit] = {
    PrintCFG(options.cfgOutput)
  }
}

case class PrintCFG(out: Path) extends Stage[Node[_ <: Generation], Unit] {

  override def friendlyName: String = "Printing control flow graph"

  override def progressWeight: Int = 0

  override def run(in1: Node[_ <: Generation]): Unit = {
    // TODO: Burn this and do it properly
    val main_method = in1.transSubnodes.collectFirst{ case m: InstanceMethod[_] if m.o.getPreferredName.getOrElse(Name()).toString.equals("main") => m }.get
    CFGPrinter().print_ast_as_cfg(main_method, out)
  }
}
