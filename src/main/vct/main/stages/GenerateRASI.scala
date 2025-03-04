package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.io.LiteralReadable
import hre.stages.Stage
import vct.col.ast._
import vct.col.origin.{
  LabelContext,
  Origin,
  PreferredName,
  RequiredName,
  SourceName,
}
import vct.col.print.Ctx
import vct.col.rewrite.Generation
import vct.options.Options
import vct.rewrite.rasi.{
  FieldIndexedVariable,
  FieldSimpleVariable,
  FieldSizeVariable,
  FieldVariable,
  RASIGenerator,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.annotation.tailrec

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
    val in: Node[Generation] = in1.asInstanceOf[Node[Generation]]
    val main_method: Procedure[Generation] =
      in.collectFirst { case m: Procedure[_] if m.vesuv_entry => m }.get
    val (variables, tracked_sequences)
        : (Set[FieldVariable[Generation]], Set[InstanceField[Generation]]) =
      resolve_variables(main_method, vars.getOrElse(Seq()))
    val split_on_variables: Option[Set[FieldVariable[Generation]]] = split
      .map(s => resolve_variables(main_method, s)._1)
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
            case Deref(_, ref) => ref.decl -> extract_name(ref.decl.o)
            case p: Predicate[_] => p -> extract_name(p.o)
          }
        ))
      print(verification, name_map)
    }
  }

  private def extract_name(o: Origin): String = {
    o.find[SourceName].map(s => s.name).getOrElse(
      o.find[RequiredName].map(r => r.requiredName)
        .getOrElse(o.getPreferredName.get.snake)
    )
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

  private def resolve_variables(
      main_method: Procedure[Generation],
      names: Seq[String],
  ): (Set[FieldVariable[Generation]], Set[InstanceField[Generation]]) = {
    var concrete_variables: Set[FieldVariable[Generation]] = Set
      .empty[FieldVariable[Generation]]
    var tracked_sequences: Set[InstanceField[Generation]] = Set
      .empty[InstanceField[Generation]]

    val main_cls: ByReferenceClass[Generation] = get_cls_from_type(
      main_method.collectFirst { case v: Variable[Generation] => v.t }.get
    )

    for (name <- names) {
      // Handle size variables as special cases
      if (name.contains("|")) {
        val var_name: Seq[String] = field_path(
          name.substring(1, name.length - 1)
        )
        concrete_variables +=
          FieldSizeVariable(resolve_field_by_name(main_cls, var_name))
      } else {
        val name_len = name.indexOf("[")
        val var_name: Seq[String] =
          if (name_len == -1)
            field_path(name)
          else
            field_path(name.substring(0, name_len))
        val index: Option[Integer] =
          if (name_len == -1)
            None
          else
            Some(Integer.valueOf(name.substring(name_len + 1, name.length - 1)))
        val instance_field: InstanceField[Generation] = resolve_field_by_name(
          main_cls,
          var_name,
        )
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

  private def field_path(name: String): Seq[String] = name.split("\\.")

  @tailrec
  private def resolve_field_by_name(
      cls: ByReferenceClass[Generation],
      names: Seq[String],
  ): InstanceField[Generation] = {
    val field: InstanceField[Generation] =
      cls.decls.collectFirst {
        case f: InstanceField[_] if name_matches(f.o, names.head) => f
      }.get

    if (names.length == 1)
      field
    else
      resolve_field_by_name(get_cls_from_type(field.t), names.tail)
  }

  // TODO: This only works in the VESUV structure!
  private def get_cls_from_type(
      t: Type[Generation]
  ): ByReferenceClass[Generation] =
    t.asInstanceOf[TByReferenceClass[Generation]].cls.decl
      .asInstanceOf[ByReferenceClass[Generation]]

  private def get_parameter_invariant(
      in: Node[Generation]
  ): Option[InstancePredicate[Generation]] = {
    in.collectFirst {
      case p: InstancePredicate[_]
          if p.o.getPreferredName.get.snake.equals("parameter_invariant") =>
        p
    }
  }

  private def name_matches(o: Origin, name: String): Boolean =
    extract_name(o).equals(name)
}
