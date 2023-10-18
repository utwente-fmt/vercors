import ColDefs._
import ColHelperUtil.NonemptyMatch

import scala.meta._

case class ColHelperSubnodes(info: ColDescription) extends ColHelperMaker {
  def subnodePatternByType(typ: Type): Option[Term => Term] =
    typ match {
      case Type.Apply(Type.Name("Seq"), List(arg)) =>
        subnodePatternByType(arg)
          .map(elem => arg => q"$arg.flatMap(element => ${elem(q"element")})")
      case Type.Apply(Type.Name(otherCollection), List(arg))
          if Set("Set", "Option").contains(otherCollection) =>
        subnodePatternByType(arg).map(elem =>
          arg => q"$arg.toSeq.flatMap(element => ${elem(q"element")})"
        )
      case Type.Tuple(ts) =>
        val pats = ts.map(subnodePatternByType)
        val accessPats: Seq[Option[Term => Term]] =
          for ((pat, idx) <- pats.zipWithIndex)
            yield pat match {
              case Some(f) =>
                Some((arg: Term) =>
                  f(Term.Select(arg, Term.Name(s"_${idx + 1}")))
                )
              case None => None
            }
        accessPats.collect { case Some(f) => f } match {
          case Nil => None
          case x :: xs =>
            Some(arg =>
              xs.foldLeft(x(arg))((init, next) => q"$init ++ ${next(arg)}")
            )
        }
      case Type.Apply(Type.Name(typ), List(Type.Name("G")))
          if info.supports("NodeFamily")(typ) ||
            info.supports("Declaration")(typ) =>
        Some(node => q"Seq($node)")
      case Type.Name("Int") | Type.Name("String") | Type.Name("Boolean") | Type
            .Name("BigInt") | Type.Name("BigDecimal") | Type
            .Apply(Type.Name("Referrable"), List(Type.Name("G"))) | Type
            .Apply(Type.Name("Ref"), _) | Type.Name("ExpectedError") | Type
            .Name("BitString") =>
        None
      case Type.Apply(Type.Name("Either"), List(t1, t2)) =>
        val f1 = subnodePatternByType(t1).getOrElse((elem: Term) => q"Nil")
        val f2 = subnodePatternByType(t2).getOrElse((elem: Term) => q"Nil")
        Some(arg =>
          q"$arg.left.map(elem => ${f1(q"elem")}).map(elem => ${f2(q"elem")})"
        )
      case other =>
        ColHelperUtil.fail(
          s"Tried to derive the subnodes for unknown type: $other",
          node = Some(other),
        )
    }

  def subnodesObject(cls: ClassDef): (String, List[Stat]) =
    (cls.baseName + "Subnodes") -> List(q"""
    object ${Term.Name(cls.baseName + "Subnodes")} {
      def subnodes[G](node: ${cls.typ}[G]): Seq[Node[G]] = ${cls.params
        .zip(cls.params.map(param => subnodePatternByType(param.decltpe.get)))
        .collect { case (param, Some(nodes)) =>
          nodes(Term.Select(q"node", Term.Name(param.name.value)))
        } match {
        case Seq() => q"Nil"
        case nonEmpty =>
          nonEmpty.tail.foldLeft(nonEmpty.head)((left, right) => q"$left ++ $right")
      }}
    }
  """)

  def subnodeMapping(cls: ClassDef): Term =
    q"""
    classOf[${cls.typ}[_]] -> ((anyNode: Node[_]) => ${Term
        .Name(cls.baseName + "Subnodes")}.subnodes(anyNode.asInstanceOf[${cls.typ}[_]]))
  """

  def make(): List[(String, List[Stat])] =
    List("Subnodes" -> List(q"""
    object Subnodes {
      val subnodesLookupTable: Map[java.lang.Class[_], Node[_] => Seq[Node[_]]] = Map(..${info
        .defs.map(subnodeMapping).toList})

      def subnodes[G](node: Node[G]): Seq[Node[G]] = subnodesLookupTable(node.getClass)(node).asInstanceOf[Seq[Node[G]]]
    }
  """)) ++ info.defs.map(subnodesObject).toList
}
