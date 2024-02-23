package vct.main.stages

import hre.stages.Stage
import vct.col.ast.{InstanceField, InstanceMethod, Node}
import vct.col.rewrite.Generation
import vct.options.Options
import vct.rewrite.rasi.{ConcreteVariable, FieldVariable, IndexedVariable, RASIGenerator}

import java.nio.file.Path

case object GenerateRASI {
  def ofOptions(options: Options): Stage[Node[_ <: Generation], Unit] = {
    GenerateRASI(options.vesuvRasiVariables, options.vesuvOutput)
  }
}

case class GenerateRASI(vars: Option[Seq[String]], out: Path) extends Stage[Node[_ <: Generation], Unit] {

  override def friendlyName: String = "Printing control flow graph"

  override def progressWeight: Int = 0

  override def run(in1: Node[_ <: Generation]): Unit = {
    val in = in1.asInstanceOf[Node[Generation]]
    val main_method = in.transSubnodes.collectFirst{ case m: InstanceMethod[_] if m.o.getPreferredName.get.snake.equals("main") => m }.get
    val variables: Set[ConcreteVariable[Generation]] = vars.getOrElse(Seq()).map(s => resolve_variable(in, s)).toSet
    RASIGenerator().test(main_method, variables, out)
  }

  private def resolve_variable(in: Node[Generation], name: String): ConcreteVariable[Generation] = {
    val name_len = name.indexOf("[")
    val var_name = if (name_len == -1) name else name.substring(0, name_len)
    val index: Option[Integer] = if (name_len == -1) None else Some(Integer.valueOf(name.substring(name_len + 2, name.length - 1)))
    val instance_field = in.transSubnodes.collectFirst{ case f: InstanceField[_] if f.o.getPreferredName.get.snake.equals(var_name) => f }.get
    index match {
      case Some(i) => IndexedVariable(instance_field, i)
      case None => FieldVariable(instance_field)
    }
  }
}
