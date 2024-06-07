package vct.main.stages

import hre.stages.Stage
import vct.col.ast.{Expr, InstanceField, InstanceMethod, InstancePredicate, Node, Predicate, Program, Verification, VerificationContext}
import vct.col.origin.{LabelContext, Origin, PreferredName}
import vct.col.print.Ctx
import vct.col.rewrite.Generation
import vct.options.Options
import vct.rewrite.rasi.{ConcreteVariable, FieldVariable, IndexedVariable, RASIGenerator, SizeVariable}

import java.nio.file.Path

case object GenerateRASI {
  def ofOptions(options: Options): Stage[Node[_ <: Generation], Unit] = {
    GenerateRASI(
      options.vesuvRasiVariables,
      options.vesuvOutput,
      test = options.vesuvRasiTest,
    )
  }
}

case class GenerateRASI(vars: Option[Seq[String]], out: Path, test: Boolean)
    extends Stage[Node[_ <: Generation], Unit] {

  override def friendlyName: String =
    "Generate reachable abstract states invariant"

  override def progressWeight: Int = 0

  override def run(in1: Node[_ <: Generation]): Unit = {
    val in = in1.asInstanceOf[Node[Generation]]
    val main_method =
      in.transSubnodes.collectFirst {
        case m: InstanceMethod[_]
            if m.o.getPreferredName.get.snake.equals("main") =>
          m
      }.get
    val variables: Set[ConcreteVariable[Generation]] =
      vars.getOrElse(Seq()).map(s => resolve_variable(in, s)).toSet
    val parameter_invariant: InstancePredicate[Generation] =
      get_parameter_invariant(in)
    if (test) {
      new RASIGenerator().test(main_method, variables, parameter_invariant, out)
    } else {
      val rasi: Expr[Generation] = new RASIGenerator()
        .execute(main_method, variables, parameter_invariant, in)
      implicit val o: Origin = Origin(Seq(LabelContext("rasi-generation"))).withContent(PreferredName(Seq("reachable_abstract_states_invariant")))
      val predicate: Predicate[Generation] = new Predicate(Seq(), Some(rasi))
      val verification: Verification[Generation] = Verification(Seq(VerificationContext(Program(Seq(predicate))(o))), Seq())
      Output(Some(out), Ctx.PVL, false).run(verification)
    }
  }

  private def resolve_variable(
      in: Node[Generation],
      name: String,
  ): ConcreteVariable[Generation] = {
    if (name.contains("|")) {
      val var_name = name.substring(1, name.length - 1)
      return SizeVariable(in.transSubnodes.collectFirst {
        case f: InstanceField[_]
            if f.o.getPreferredName.get.snake.equals(var_name) =>
          f
      }.get)
    }
    val name_len = name.indexOf("[")
    val var_name =
      if (name_len == -1)
        name
      else
        name.substring(0, name_len)
    val index: Option[Integer] =
      if (name_len == -1)
        None
      else
        Some(Integer.valueOf(name.substring(name_len + 1, name.length - 1)))
    val instance_field =
      in.transSubnodes.collectFirst {
        case f: InstanceField[_]
            if f.o.getPreferredName.get.snake.equals(var_name) =>
          f
      }.get
    index match {
      case Some(i) => IndexedVariable(instance_field, i)
      case None => FieldVariable(instance_field)
    }
  }

  private def get_parameter_invariant(
      in: Node[Generation]
  ): InstancePredicate[Generation] = {
    in.transSubnodes.collectFirst {
      case p: InstancePredicate[_]
          if p.o.getPreferredName.get.snake.equals("parameter_invariant") =>
        p
    }.get
  }
}
