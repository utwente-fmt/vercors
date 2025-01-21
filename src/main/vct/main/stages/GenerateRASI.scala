package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.io.LiteralReadable
import hre.stages.Stage
import vct.col.ast.{
  Declaration,
  Deref,
  Expr,
  InstanceField,
  InstanceMethod,
  InstancePredicate,
  IntType,
  Node,
  Predicate,
  Procedure,
  Program,
  TBool,
  Verification,
  VerificationContext,
}
import vct.col.origin.{LabelContext, Origin, PreferredName}
import vct.col.print.Ctx
import vct.col.rewrite.Generation
import vct.options.Options
import vct.rewrite.rasi.{
  ConcreteVariable,
  FieldSimpleVariable,
  FieldIndexedVariable,
  RASIGenerator,
  FieldSizeVariable,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

case object GenerateRASI {
  def ofOptions(options: Options): Stage[Node[_ <: Generation], Unit] = {
    GenerateRASI(
      options.vesuvRasiVariables,
      options.vesuvRasiSplitVariables,
      options.vesuvOutput,
      test = options.vesuvRasiTest,
    )
  }
}

case class GenerateRASI(
    vars: Option[Seq[String]],
    split: Option[Seq[String]],
    out: Path,
    test: Boolean,
) extends Stage[Node[_ <: Generation], Unit] with LazyLogging {

  override def friendlyName: String =
    "Generate reachable abstract states invariant"

  override def progressWeight: Int = 0

  override def run(in1: Node[_ <: Generation]): Unit = {
    val in = in1.asInstanceOf[Node[Generation]]
    val main_method =
      in.collectFirst { case m: Procedure[_] if m.vesuv_entry => m }.get
    val (variables, tracked_sequences)
        : (Set[ConcreteVariable[Generation]], Set[InstanceField[Generation]]) =
      resolve_variables(in, vars.getOrElse(Seq()))
    val split_on_variables: Option[Set[ConcreteVariable[Generation]]] = split
      .map(s => resolve_split_variables(in, s))
    val parameter_invariant: Option[InstancePredicate[Generation]] =
      get_parameter_invariant(in)
    if (test) {
      new RASIGenerator().test(
        main_method,
        variables,
        parameter_invariant,
        out,
        tracked_sequences,
      )
    } else {
      val rasis: Seq[(String, Expr[Generation])] = new RASIGenerator().execute(
        main_method,
        variables,
        split_on_variables,
        parameter_invariant,
        in,
        tracked_sequences,
      )
      val predicates: Seq[Predicate[Generation]] = rasis
        .map(t => rasi_predicate(t._1, t._2))
      implicit val o: Origin = Origin(Seq(LabelContext("rasi-generation")))
      val verification: Verification[Generation] = Verification(
        Seq(VerificationContext(Program(predicates)(o))),
        Seq(),
      )

      val name_map: Map[Declaration[_], String] = Map
        .from(predicates.flatMap(p =>
          p.collect {
            case Deref(_, ref) =>
              ref.decl -> ref.decl.o.getPreferredName.get.snake
            case p: Predicate[_] => p -> p.o.getPreferredName.get.snake
          }
        ))
      print(verification, name_map)
    }
  }

  private def rasi_predicate(
      name: String,
      rasi: Expr[Generation],
  ): Predicate[Generation] = {
    implicit val o: Origin = Origin(Seq(LabelContext("rasi-generation")))
      .withContent(PreferredName(Seq(name)))
    new Predicate(Seq(), Some(rasi), threadLocal = false, inline = true)
  }

  private def print(
      in: Verification[_ <: Generation],
      name_map: Map[Declaration[_], String],
  ): Unit = {
    val ctx = Ctx(syntax = Ctx.PVL, names = name_map)

    val buf = new StringBuffer()
    in.write(buf)(ctx)
    val path = s"invariant.pvl"
    val txt = LiteralReadable(path, buf.toString)

    logger.info(s"Writing ${txt.fileName} to $out")
    Files.write(out, txt.data.getBytes(StandardCharsets.UTF_8))
  }

  private def resolve_split_variables(
      in: Node[Generation],
      names: Seq[String],
  ): Set[ConcreteVariable[Generation]] = resolve_variables(in, names)._1

  private def resolve_variables(
      in: Node[Generation],
      names: Seq[String],
  ): (Set[ConcreteVariable[Generation]], Set[InstanceField[Generation]]) = {
    var concrete_variables: Set[ConcreteVariable[Generation]] = Set
      .empty[ConcreteVariable[Generation]]
    var tracked_sequences: Set[InstanceField[Generation]] = Set
      .empty[InstanceField[Generation]]

    for (name <- names) {
      // Handle size variables as special cases
      if (name.contains("|")) {
        val var_name = field_name(name.substring(1, name.length - 1))
        concrete_variables += FieldSizeVariable(in.collectFirst {
          case f: InstanceField[_] if name_matches(f.o, var_name) => f
        }.get)
      } else {
        val name_len = name.indexOf("[")
        val var_name =
          if (name_len == -1)
            field_name(name)
          else
            field_name(name.substring(0, name_len))
        val index: Option[Integer] =
          if (name_len == -1)
            None
          else
            Some(Integer.valueOf(name.substring(name_len + 1, name.length - 1)))
        val instance_field: InstanceField[Generation] =
          in.collectFirst {
            case f: InstanceField[_] if name_matches(f.o, var_name) => f
          }.get
        index match {
          case Some(i) =>
            concrete_variables += FieldIndexedVariable(instance_field, i)
          case None =>
            instance_field.t match {
              case _: IntType[_] | TBool() =>
                concrete_variables += FieldSimpleVariable(instance_field)
              case _ => tracked_sequences += instance_field
            }
        }
      }
    }

    (concrete_variables, tracked_sequences)
  }

  private def field_name(name: String): String =
    name.split(".").last // TODO: Consider the entire path to relax assumption
  //                              on differently-named variables

  private def get_parameter_invariant(
      in: Node[Generation]
  ): Option[InstancePredicate[Generation]] = {
    in.collectFirst {
      case p: InstancePredicate[_]
          if p.o.getPreferredName.get.snake.equals("parameter_invariant") =>
        p
    }
  }

  private def name_matches(o: Origin, name: String): Boolean = {
    val preferred_name = o.getPreferredName.get
    name.equals(preferred_name.snake) || name.equals(preferred_name.usnake) ||
    name.equals(preferred_name.camel) || name.equals(preferred_name.ucamel)
  }
}
