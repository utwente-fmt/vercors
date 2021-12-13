import ColDefs._
import MetaUtil.NonemptyMatch

import scala.meta._

case class ColHelperComparator(info: ColDescription) {
  var split: Int = 0
  var extra: List[Stat] = Nil

  def valueEqual(t: Type, left: Term, right: Term): Term = t match {
    case Type.Apply(Type.Name(name), List(Type.Name("G"))) if info.supports("Node")(name) => q"true"

    case Type.Apply(Type.Name("Ref"), _) => q"true"

    case Type.Name("Int") | Type.Name("BigInt") | Type.Name("String") | Type.Name("Boolean") | Type.Apply(Type.Name("Referrable"), List(Type.Name("G"))) => q"$left == $right"

    case Type.Apply(Type.Name("Seq"), List(inner)) =>
      q"$left.size == $right.size && $left.zip($right).forall { case (left, right) => ${valueEqual(inner, q"left", q"right")} }"

    case Type.Apply(Type.Name("Set"), _) =>
      q"$left == $right"

    case Type.Apply(Type.Name("Option"), List(inner)) =>
      q"($left.isEmpty && $right.isEmpty) || ($left.nonEmpty && $right.nonEmpty && ${valueEqual(inner, q"$left.get", q"$right.get")})"

    case Type.Tuple(args) =>
      args.zipWithIndex.map {
        case (inner, idx) =>
          val access = Term.Name(s"_${idx+1}")
          valueEqual(inner, Term.Select(left, access), Term.Select(right, access))
      }.filter(_ != q"true").reduceOption((l, r) => q"$l && $r").getOrElse(q"true")
  }

  def refEqual(t: Type, left: Term, right: Term): Term = t match {
    case Type.Apply(Type.Name("Ref"), _) => q"if($left.decl == $right.decl) Nil else Seq(($left.decl, $right.decl))"

    case Type.Apply(Type.Name(name), List(Type.Name("G"))) if info.supports("Node")(name) => q"Nil"
    case Type.Name("Int") | Type.Name("BigInt") | Type.Name("String") | Type.Name("Boolean") | Type.Apply(Type.Name("Referrable"), List(Type.Name("G"))) => q"Nil"

    case Type.Apply(Type.Name("Seq"), List(inner)) =>
      q"$left.zip($right).flatMap { case (left, right) => ${refEqual(inner, q"left", q"right")} }"

    case Type.Apply(Type.Name("Set"), _) => q"Nil"

    case Type.Apply(Type.Name("Option"), List(inner)) =>
      q"if($left.nonEmpty) ${refEqual(inner, q"$left.get", q"$right.get")} else Nil"

    case Type.Tuple(args) =>
      args.zipWithIndex.map {
        case (inner, idx) =>
          val access = Term.Name(s"_${idx+1}")
          refEqual(inner, Term.Select(left, access), Term.Select(right, access))
      }.filter(_ != q"Nil").reduceOption((l, r) => q"$l ++ $r").getOrElse(q"Nil")
  }

  def nodeEqual(t: Type, left: Term, right: Term): Term = t match {
    case Type.Apply(Type.Name(name), List(Type.Name("G"))) if info.supports("Node")(name) => q"compare($left, $right)"

    case Type.Apply(Type.Name("Ref"), _) => q"LazyList.empty"
    case Type.Name("Int") | Type.Name("BigInt") | Type.Name("String") | Type.Name("Boolean") | Type.Apply(Type.Name("Referrable"), List(Type.Name("G"))) => q"LazyList.empty"

    case Type.Apply(Type.Name("Seq"), List(inner)) =>
      q"$left.zip($right).to(LazyList).flatMap { case (left, right) => ${nodeEqual(inner, q"left", q"right")} }"

    case Type.Apply(Type.Name("Set"), _) => q"LazyList.empty"

    case Type.Apply(Type.Name("Option"), List(inner)) =>
      q"if($left.nonEmpty) ${nodeEqual(inner, q"$left.get", q"$right.get")} else LazyList.empty"

    case Type.Tuple(args) =>
      args.zipWithIndex.map {
        case (inner, idx) =>
          val access = Term.Name(s"_${idx+1}")
          nodeEqual(inner, Term.Select(left, access), Term.Select(right, access))
      }.filter(_ != q"LazyList.empty").reduceOption((l, r) => q"$l #::: $r").getOrElse(q"LazyList.empty")
  }

  def makeCase(defn: ClassDef): Case = {
    val left = Term.Name("left")
    val right = Term.Name("right")
    val paramEquals = (param: Term.Param) => valueEqual(
      t = param.decltpe.get,
      left = Term.Select(left, Term.Name(param.name.value)),
      right = Term.Select(right, Term.Name(param.name.value)))

    val paramRefCmp = (param: Term.Param) => refEqual(
      t = param.decltpe.get,
      left = Term.Select(left, Term.Name(param.name.value)),
      right = Term.Select(right, Term.Name(param.name.value)))

    val paramNodeCmp = (param: Term.Param) => nodeEqual(
      t = param.decltpe.get,
      left = Term.Select(left, Term.Name(param.name.value)),
      right = Term.Select(right, Term.Name(param.name.value)))

    val equal = defn.params.map(paramEquals).filter(_ != q"true").reduceOption[Term]((l, r) => q"$l && $r").getOrElse(q"true")
    val refCmp = defn.params.map(paramRefCmp).filter(_ != q"Nil").reduceOption[Term]((l, r) => q"$l ++ $r").getOrElse(q"Nil")
    val children = defn.params.map(paramNodeCmp).filter(_ != q"LazyList.empty")
      .reduceOption[Term]((l, r) => q"$l #::: $r").getOrElse(q"LazyList.empty")

    val params = List(
      Term.Param(Nil, Term.Name("left"), Some(t"${defn.typ}[L]"), None),
      Term.Param(Nil, Term.Name("right"), Some(t"${defn.typ}[R]"), None),
    )

    split += 1

    val splitMethod =q"""
      def ${Term.Name(s"split$split")}[L, R](..$params): LazyList[(Node[L], Node[R])] =
        if($equal) $refCmp.to(LazyList) #::: $children else LazyList((left, right))
    """

    extra +:= splitMethod

    Case(
      pat = Pat.Tuple(List(Pat.Typed(Pat.Var(left), t"${defn.typ}[L]"), Pat.Typed(Pat.Var(right), t"${defn.typ}[R]"))),
      cond = None,
      body = Term.Apply(Term.Name(s"split$split"), List(q"left", q"right")))
  }

  def cases: List[Case] = (info.defs.map(makeCase) :+ Case(p"(left, right)", None, q"(left, right) #:: LazyList.empty")).toList

  def make(): List[Stat] = List(q"""
    object Comparator {
      def compare[L, R](left: Node[L], right: Node[R]): LazyList[(Node[L], Node[R])] = ${NonemptyMatch("comparator", q"(left, right)", cases)}

      ..$extra
    }
  """)
}
