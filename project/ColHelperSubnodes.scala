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
      case Type.Tuple(ts) =>
        val pats = ts.map(subnodePatternByType)
        val accessPats: Seq[Option[Term => Term]] = for((pat, idx) <- pats.zipWithIndex)
          yield pat match {
            case Some(f) => Some((arg: Term) => f(Term.Select(arg, Term.Name(s"_${idx+1}"))))
            case None => None
          }
        accessPats.collect { case Some(f) => f } match {
          case Nil => None
          case x :: xs => Some(arg => xs.foldLeft(x(arg))((init, next) => q"$init ++ ${next(arg)}"))
        }
      case Type.Apply(Type.Name(typ), List(Type.Name("G"))) if info.supports("NodeFamily")(typ) || info.supports(DECLARATION)(typ) =>
        Some(node => q"Seq($node)")
      case Type.Name("Int") | Type.Name("String") | Type.Name("Boolean") | Type.Name("BigInt") | Type.Apply(Type.Name("Referrable"), List(Type.Name("G"))) | Type.Apply(Type.Name("Ref"), _) =>
        None
      case Type.Apply(Type.Name("Either"), List(t1, t2)) =>
        val f1 = subnodePatternByType(t1).getOrElse((elem: Term) => q"Nil")
        val f2 = subnodePatternByType(t2).getOrElse((elem: Term) => q"Nil")
        Some(arg => q"$arg.left.map(elem => ${f1(q"elem")}).map(elem => ${f2(q"elem")})")
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
        Pat.Typed(Pat.Var(q"node"), t"${cls.typ}[G]"),
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
      def subnodes[G](node: Node[G]): Seq[Node[G]] = ${NonemptyMatch("subnodes", q"node", info.defs.map(subnodePattern).toList)}
    }
  """)
}
