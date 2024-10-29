package vct.col.ast.helpers.defn

import Constants._
import Naming._

import scala.annotation.tailrec
import scala.meta._

object Simplify {
  def simplify(t: Term): Term = simplifyFlatly(recurse(t))

  private def recurse(term: Term): Term =
    term match {
      // Exception: immediately push .to(LazyList) inwards, since we can further simplify the subterms.
      case q"(if($cond) $whenTrue else $whenFalse).to(${`LazyListObj`})" =>
        simplify(
          q"if($cond) $whenTrue.to($LazyListObj) else $whenFalse.to($LazyListObj)"
        )

      case q"if($cond) $whenTrue else $whenFalse" =>
        q"if(${simplify(cond)}) ${simplify(whenTrue)} else ${simplify(whenFalse)}"
      case q"$xs.flatMap($arg => $exp)" =>
        q"${simplify(xs)}.flatMap($arg => ${simplify(exp)})"
      case q"$zipped.flatMap({ case ($l, $r) => $exp })" =>
        q"${simplify(zipped)}.flatMap({ case ($l, $r) => ${simplify(exp)} })"
      case q"$xs.map($arg => $exp)" =>
        q"${simplify(xs)}.map($arg => ${simplify(exp)})"
      case q"$zipped.map({ case ($l, $r) => $exp })" =>
        q"${simplify(zipped)}.map({ case ($l, $r) => ${simplify(exp)} })"
      case q"$zipped.forall({ case($l, $r) => $exp })" =>
        q"${simplify(zipped)}.forall({ case($l, $r) => ${simplify(exp)} })"
      case q"$_.LazyList(..$terms)" => q"$LazyListObj(..${terms.map(simplify)})"
      case q"$_.Seq(..$terms)" => q"$SeqObj(..${terms.map(simplify)})"
      case q"$l #:: $r" => q"${simplify(l)} #:: ${simplify(r)}"
      case q"$l #::: $r" => q"${simplify(l)} #::: ${simplify(r)}"
      case q"$l ++ $r" => q"${simplify(l)} ++ ${simplify(r)}"
      case q"$l && $r" => q"${simplify(l)} && ${simplify(r)}"
      case q"$l || $r" => q"${simplify(l)} || ${simplify(r)}"
      case q"$l == $r" => q"${simplify(l)} == ${simplify(r)}"
      case q"$l.$r" => q"${simplify(l)}.$r"
      case q"($l, $r)" => q"(${simplify(l)}, ${simplify(r)})"
      case q"$inner.to($_.LazyList)" => q"${simplify(inner)}.to($LazyListObj)"
      case q"$l.zip($r)" => q"${simplify(l)}.zip(${simplify(r)})"

      case q"$_.Seq.empty[$_]" => term
      case _: Term.Name | q"true" | q"false" | q"this" => term
      case q"$_.compare($_)" => term
      case q"$_.MatchingDeclaration(..$_)" => term
      case q"$_.MatchingReference(..$_)" => term
      case q"$_.StructuralDifference(..$_)" => term
      case other =>
        println(
          s"[warn] [ColHelper] Not recursing simplifier into unknown term $term"
        )
        other
    }

  @tailrec
  private def simplifyFlatly(term: Term): Term =
    simplifyFlatly(term match {
      case q"true && $bool" => bool
      case q"$bool && true" => bool
      case q"false && $bool" => q"false"
      case q"$bool && false" => q"false"

      case q"true || $bool" => q"true"
      case q"$bool || true" => q"true"
      case q"false || $bool" => bool
      case q"$bool || false" => bool

      case q"$_.LazyList() #::: $xs" => xs
      case q"$xs #::: $_.LazyList()" => xs
      case q"$_.Seq.empty[$_] ++ $xs" => xs
      case q"$xs ++ $_.Seq.empty[$_]" => xs

      case q"if(true) $whenTrue else $_" => whenTrue
      case q"if(false) $_ else $whenFalse" => whenFalse

      case q"$_.flatMap({ case ($l, $r) => $_.LazyList() })" =>
        q"$LazyListObj()"
      case q"$_.flatMap($_ => $_.Seq.empty[$t])" => q"$SeqObj.empty[$t]"
      case q"$xs.flatMap($arg => $_.Seq($y))" => q"$xs.map($arg => $y)"
      case q"$_.forall({ case ($l, $r) => true })" => q"true"

      case q"if($cond) $whenTrue else $whenFalse"
          if whenTrue.getClass == whenFalse.getClass &&
            whenTrue.show[Structure] == whenFalse.show[Structure] =>
        whenTrue

      case q"$xs.map(${Term.Param(_, Term.Name(x), _, _)} => ${Term.Name(y)})"
          if x == y =>
        xs

      case q"($a #::: $b) #::: $c" => q"$a #::: $b #::: $c"
      case q"$a ++ ($b ++ $c)" => q"$a ++ $b ++ $c"

      case q"$_.LazyList($x) #::: $xs" => q"$x #:: $xs"
      case q"$_.Seq($x) ++ $xs" => q"$x +: $xs"

      case other => return other
    })
}
