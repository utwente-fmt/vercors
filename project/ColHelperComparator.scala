import ColDefs._
import ColHelperUtil.NonemptyMatch

import scala.meta._

case class ColHelperComparator(info: ColDescription) {
  def valueEqual(t: Type, left: Term, right: Term): Term = t match {
    case Type.Apply(Type.Name(name), List(Type.Name("G"))) if info.supports("Node")(name) => q"true"

    case Type.Apply(Type.Name("Ref"), _) => q"true"

    case Type.Name("Int") | Type.Name("BigInt") | Type.Name("BigDecimal") | Type.Name("String") | Type.Name("Boolean") | Type.Apply(Type.Name("Referrable"), List(Type.Name("G"))) | Type.Name("ExpectedError") => q"$left == $right"

    case Type.Apply(Type.Name("Seq"), List(inner)) =>
      q"$left.size == $right.size && $left.zip($right).forall { case (left, right) => ${valueEqual(inner, q"left", q"right")} }"

    case Type.Apply(Type.Name("Set"), _) =>
      q"$left == $right"

    case Type.Apply(Type.Name("Option"), List(inner)) =>
      q"($left.isEmpty && $right.isEmpty) || ($left.nonEmpty && $right.nonEmpty && ${valueEqual(inner, q"$left.get", q"$right.get")})"

    case Type.Apply(Type.Name("Either"), List(t1, t2)) =>
      // TODO (RR): Rewrite in above form
      q"""
        ($left.isLeft, $right.isLeft) match {
          case (true, true) => ${valueEqual(t1, q"$left.left.get", q"$right.left.get")}
          case (false, false) => ${valueEqual(t2, q"$left.get", q"$right.get")}
          case _ => false
        }
       """
      // TODO (RR): Add simplification rules for this case below?

    case Type.Tuple(args) =>
      args.zipWithIndex.map {
        case (inner, idx) =>
          val access = Term.Name(s"_${idx+1}")
          valueEqual(inner, Term.Select(left, access), Term.Select(right, access))
      }.filter(_ != q"true").reduceOption((l, r) => q"$l && $r").getOrElse(q"true")
  }

  def refEqual(t: Type, left: Term, right: Term): Term = t match {
    case Type.Apply(Type.Name("Ref"), _) => q"LazyList(Comparator.MatchingReference($left.decl, $right.decl))"

    case Type.Apply(Type.Name(name), List(Type.Name("G"))) if info.supports("Node")(name) => q"LazyList.empty"
    case Type.Name("Int") | Type.Name("BigInt") | Type.Name("BigDecimal") | Type.Name("String") | Type.Name("Boolean") | Type.Apply(Type.Name("Referrable"), List(Type.Name("G"))) | Type.Name("ExpectedError") => q"LazyList.empty"

    case Type.Apply(Type.Name("Seq"), List(inner)) =>
      q"$left.zip($right).to(LazyList).flatMap { case (left, right) => ${refEqual(inner, q"left", q"right")} }"

    case Type.Apply(Type.Name("Set"), _) => q"LazyList.empty"

    case Type.Apply(Type.Name("Option"), List(inner)) =>
      q"if($left.nonEmpty) ${refEqual(inner, q"$left.get", q"$right.get")} else LazyList.empty"
      // TODO (RR): Fix the below "improvement"?
      // q"if($left.nonEmpty) right.nonEmpty && ${refEqual(inner, q"$left.get", q"$right.get")} else LazyList.empty"

    case Type.Apply(Type.Name("Either"), List(t1, t2)) =>
      q"""
        ($left.isLeft, $right.isLeft) match {
          case (true, true) => ${refEqual(t1, q"$left.left.get", q"$right.left.get")}
          case (false, false) => ${refEqual(t2, q"$left.get", q"$right.get")}
          case _ => false
        }
       """

    case Type.Tuple(args) =>
      args.zipWithIndex.map {
        case (inner, idx) =>
          val access = Term.Name(s"_${idx+1}")
          refEqual(inner, Term.Select(left, access), Term.Select(right, access))
      }.reduce((l, r) => q"$l #::: $r")
  }

  def nodeEqual(t: Type, left: Term, right: Term): Term = t match {
    case Type.Apply(Type.Name(name), List(Type.Name("G"))) if info.supports("Node")(name) => q"Comparator.compare($left, $right)"

    case Type.Apply(Type.Name("Ref"), _) => q"LazyList.empty"
    case Type.Name("Int") | Type.Name("BigInt") | Type.Name("BigDecimal") | Type.Name("String") | Type.Name("Boolean") | Type.Apply(Type.Name("Referrable"), List(Type.Name("G"))) | Type.Name("ExpectedError") => q"LazyList.empty"

    case Type.Apply(Type.Name("Seq"), List(inner)) =>
      q"$left.zip($right).to(LazyList).flatMap { case (left, right) => ${nodeEqual(inner, q"left", q"right")} }"

    case Type.Apply(Type.Name("Set"), _) => q"LazyList.empty"

    case Type.Apply(Type.Name("Option"), List(inner)) =>
      q"if($left.nonEmpty) ${nodeEqual(inner, q"$left.get", q"$right.get")} else LazyList.empty"

    case Type.Apply(Type.Name("Either"), List(t1, t2)) =>
      q"""
        ($left.isLeft, $right.isLeft) match {
          case (true, true) => ${nodeEqual(t1, q"$left.left.get", q"$right.left.get")}
          case (false, false) => ${nodeEqual(t2, q"$left.get", q"$right.get")}
          case _ => false
        }
       """

    case Type.Tuple(args) =>
      args.zipWithIndex.map {
        case (inner, idx) =>
          val access = Term.Name(s"_${idx+1}")
          nodeEqual(inner, Term.Select(left, access), Term.Select(right, access))
      }.filter(_ != q"LazyList.empty").reduceOption((l, r) => q"$l #::: $r").getOrElse(q"LazyList.empty")
  }

  def recurse(term: Term): Term = term match {
    // Exception: immediately push .to(LazyList) inwards, since we can further simplify the subterms.
    case q"(if($cond) $whenTrue else $whenFalse).to(LazyList)" =>
      simplify(q"if($cond) $whenTrue.to(LazyList) else $whenFalse.to(LazyList)")

    case q"if($cond) $whenTrue else $whenFalse" =>
      q"if(${simplify(cond)}) ${simplify(whenTrue)} else ${simplify(whenFalse)}"
    case q"$zipped.flatMap({ case ($l, $r) => $exp })" =>
      q"${simplify(zipped)}.flatMap({ case ($l, $r) => ${simplify(exp)} })"
    case q"$zipped.forall({ case($l, $r) => $exp })" =>
      q"${simplify(zipped)}.forall({ case($l, $r) => ${simplify(exp)} })"
    case q"LazyList(..$terms)" => q"LazyList(..${terms.map(simplify)})"
    case q"$l #:: $r" => q"${simplify(l)} #:: ${simplify(r)}"
    case q"$l #::: $r" => q"${simplify(l)} #::: ${simplify(r)}"
    case q"$l && $r" => q"${simplify(l)} && ${simplify(r)}"
    case q"$l || $r" => q"${simplify(l)} || ${simplify(r)}"
    case q"$l == $r" => q"${simplify(l)} == ${simplify(r)}"
    case q"$l.$r" => q"${simplify(l)}.$r"
    case q"($l, $r)" => q"(${simplify(l)}, ${simplify(r)})"
    case q"$inner.to(LazyList)" => q"${simplify(inner)}.to(LazyList)"
    case q"$l.zip($r)" => q"${simplify(l)}.zip(${simplify(r)})"
    case _: Term.Name | q"true" | q"false" => term
    case q"Comparator.compare(..$_)" => term
    case q"Comparator.MatchingDeclaration(..$_)" => term
    case q"Comparator.MatchingReference(..$_)" => term
    case q"Comparator.StructuralDifference(..$_)" => term
    case other =>
      println(s"Warning: Not recursing simplifier into unknown term $term")
      other
  }

  def simplifyFlatly(term: Term): Term = simplifyFlatly(term match {
    case q"true && $bool" => bool
    case q"$bool && true" => bool
    case q"false && $bool" => q"false"
    case q"$bool && false" => q"false"

    case q"true || $bool" => q"true"
    case q"$bool || true" => q"true"
    case q"false || $bool" => bool
    case q"$bool || false" => bool

    case q"LazyList.empty #::: $xs" => xs
    case q"$xs #::: LazyList.empty" => xs

    case q"if(true) $whenTrue else $_" => whenTrue
    case q"if(false) $_ else $whenFalse" => whenFalse

    case q"LazyList($x) #::: $xs" => q"$x #:: $xs"
    case q"($a #::: $b) #::: $c" => q"$a #::: $b #::: $c"

    case q"$zipped.flatMap({ case ($l, $r) => LazyList.empty })" => q"LazyList.empty"
    case q"$zipped.forall({ case ($l, $r) => true })" => q"true"

    case q"if($cond) $whenTrue else $whenFalse" if whenTrue.show[Structure] == whenFalse.show[Structure] =>
      whenTrue

    case q"$l1.isEmpty && $r1.isEmpty || $l2.nonEmpty && $r2.nonEmpty" if l1.show[Structure] == l2.show[Structure] && r1.show[Structure] == r2.show[Structure] =>
      q"$l1.isEmpty == $r1.isEmpty"

    case other => return other
  })

  def simplify(term: Term): Term =
     simplifyFlatly(recurse(term))

  def makeCase(defn: ClassDef): (String, List[Stat]) = {
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

    val equal = defn.params.map(paramEquals).reduceOption[Term]((l, r) => q"$l && $r").getOrElse(q"true")
    val refCmp = defn.params.map(paramRefCmp).reduceOption[Term]((l, r) => q"$l #::: $r").getOrElse(q"LazyList.empty")
    val subnodesCmp = defn.params.map(paramNodeCmp).reduceOption[Term]((l, r) => q"$l #::: $r").getOrElse(q"LazyList.empty")

    val params = List(
      Term.Param(Nil, Term.Name("left"), Some(t"${defn.typ}[L]"), None),
      Term.Param(Nil, Term.Name("right"), Some(t"${defn.typ}[R]"), None),
    )

    val impl = simplify(
      if(info.supports("Declaration")(defn.baseName))
        q"if($equal) Comparator.MatchingDeclaration(left, right) #:: $refCmp #::: $subnodesCmp else LazyList(Comparator.StructuralDifference(left, right))"
      else
        q"if($equal) $refCmp #::: $subnodesCmp else LazyList(Comparator.StructuralDifference(left, right))"
    )

    val apply = q"""def apply[L, R](anyLeft: Node[L], anyRight: Node[R]): LazyList[Comparator.Difference[L, R]] = {
      val left = anyLeft.asInstanceOf[${defn.typ}[L]]
      val right = anyRight.asInstanceOf[${defn.typ}[R]]
      $impl
    }"""

    val objectName = "Compare" + defn.baseName

    objectName -> List(Defn.Object(Nil, Term.Name(objectName),
      Template(Nil, List(Init(t"Comparator.Compare", Name.Anonymous(), Nil)), Self(Name.Anonymous(), None), List(apply))))
  }

  def makeComparator(): (String, List[Stat]) = "Comparator" -> List(q"""
    object Comparator {
      sealed trait Difference[L, R]
      case class MatchingDeclaration[L, R](left: Declaration[L], right: Declaration[R]) extends Difference[L, R]
      case class MatchingReference[L, R](left: Declaration[L], right: Declaration[R]) extends Difference[L, R]
      case class StructuralDifference[L, R](left: Node[L], right: Node[R]) extends Difference[L, R]

      trait Compare {
        def apply[L, R](anyLeft: Node[L], anyRight: Node[R]): LazyList[Difference[L, R]]
      }

      val compareLookupTable: Map[java.lang.Class[_], Compare] =
        Map(..${info.defs.map(defn =>
          q"classOf[${defn.typ}[_]] -> ${Term.Name("Compare" + defn.baseName)}").toList})

      def compare[L, R](left: Node[L], right: Node[R]): LazyList[Difference[L, R]] =
        if(left.getClass == right.getClass) compareLookupTable(left.getClass)(left, right)
        else LazyList(StructuralDifference(left, right))
    }
  """)

  def make(): List[(String, List[Stat])] =
    List(makeComparator()) ++ info.defs.map(makeCase)
}
