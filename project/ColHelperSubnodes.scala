import ColDefs._
import MetaUtil.NonemptyMatch

import scala.meta._

case class ColHelperSubnodes(info: ColDescription) {
  def subnodePatternByType(typ: Type): Option[Term => Term] =
    typ match {
      case Type.Apply(Type.Name("Seq"), List(arg)) =>
        subnodePatternByType(arg).map(elem =>
          arg => q"$arg.flatMap(element => ${elem(q"element")})")
      case Type.Apply(Type.Name(otherCollection), List(arg)) if Set("Set", "Option").contains(otherCollection) =>
        subnodePatternByType(arg).map(elem =>
          arg => q"$arg.toSeq.flatMap(element => ${elem(q"element")})")
      case Type.Tuple(List(t1, t2)) =>
        (subnodePatternByType(t1), subnodePatternByType(t2)) match {
          case (None, None) => None
          case (Some(f), None) => Some(arg => f(q"$arg._1"))
          case (None, Some(f)) => Some(arg => f(q"$arg._2"))
          case (Some(f), Some(g)) => Some(arg => q"${f(q"$arg._1")} ++ ${g(q"$arg._2")}")
        }
      case Type.Name(typ) if info.supports("NodeFamily")(typ) || info.supports(DECLARATION)(typ) =>
        Some(node => q"Seq($node)")
      case Type.Name("Int") | Type.Name("String") | Type.Name("Boolean") | Type.Apply(Type.Name("Ref"), _) =>
        None
      case other =>
        MetaUtil.fail(
          s"Tried to derive the subnodes for unknown type: $other",
          node=Some(other)
        )
    }

  def subnodePattern(cls: ClassDef): Case = {
    val subnodesByField = cls.params.map(param => subnodePatternByType(param.decltpe.get))

    if(cls.mods.collectFirst{case Mod.Case() => ()}.nonEmpty) {
      Case(
        Pat.Extract(cls.term, cls.params.zip(subnodesByField).map {
          case (_, None) => Pat.Wildcard()
          case (param, Some(_)) => Pat.Var(Term.Name(param.name.value))
        }),
        None,
        cls.params.zip(subnodesByField).collect {
          case (param, Some(nodes)) => nodes(Term.Name(param.name.value))
        } match {
          case Seq() => q"Seq()"
          case nonEmpty => nonEmpty.tail.foldLeft(nonEmpty.head)((left, right) => q"$left ++ $right")
        }
      )
    } else {
      Case(
        Pat.Typed(Pat.Var(q"node"), cls.typ),
        None,
        cls.params.zip(subnodesByField).collect {
          case (param, Some(nodes)) => nodes(Term.Select(q"node", Term.Name(param.name.value)))
        } match {
          case Seq() => q"Seq()"
          case nonEmpty => nonEmpty.tail.foldLeft(nonEmpty.head)((left, right) => q"$left ++ $right")
        }
      )
    }
  }

  def make(): List[Stat] = List(q"""
    object Subnodes {
      def subnodes(node: Node): Seq[Node] = ${NonemptyMatch("subnodes", q"node", info.defs.map(subnodePattern).toList)}
    }
  """)
}
