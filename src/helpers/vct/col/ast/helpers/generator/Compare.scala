package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants.{CompareResult, LazyList, LazyListObj, MatchingDeclaration, MatchingDeclarationObj, MatchingReferenceObj, Node, StructuralDifferenceObj}
import vct.col.ast.helpers.defn.Naming.{compareTrait, typ}
import vct.col.ast.helpers.defn.Types
import vct.col.ast.structure
import vct.col.ast.structure.{NodeDefinition, NodeGenerator}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using
import scala.meta._
import scala.meta.internal.prettyprinters.TreeSyntax
import scala.meta.dialects

class Compare extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition, isDeclaration: Boolean): Unit =
    ResultStream.write(out.resolve(s"${node.name.base}Comapre.scala"), getTree(node, isDeclaration))

  def getTree(node: NodeDefinition, isDeclaration: Boolean): Source = {
    source"""
      package vct.col.ast.ops.compare

      trait ${compareTrait(node)}[L] { this: ${typ(node)}[L] =>
        def compare[R](other: $Node[R]): $LazyList[$CompareResult[L, R]] =
          if(this.getClass != other.getClass) $LazyListObj($StructuralDifferenceObj(this, other))
          else {
            val `~x` = other.asInstanceOf[${typ(node)}[R]]
            ${compare(q"this", q"`~x`", node, isDeclaration)}
          }
      }
    """
  }

  def compare(left: Term, right: Term, node: NodeDefinition, isDeclaration: Boolean): Term = {
    val valuesEqual = compareValues(left, right, node)
    val refs = compareRefs(left, right, node)
    val subnodes = compareSubnodes(left, right, node)

    simplify(
      q"""
        if($valuesEqual) ${
          if(isDeclaration) q"$MatchingDeclarationObj($left, $right) #:: $refs #::: $subnodes"
          else q"$refs #::: $subnodes"
        } else $LazyListObj($StructuralDifferenceObj($left, $right))
      """
    )
  }

  def compareValues(left: Term, right: Term, node: NodeDefinition): Term =
    node.fields.map {
      case (fieldName, t) =>
        val field = Term.Name(fieldName)
        compareValues(q"$left.$field", q"$right.$field", t)
    }.reduceOption((l, r) => q"$l && $r").getOrElse(q"true")

  def compareValues(left: Term, right: Term, t: structure.Type): Term =
    t match {
      case structure.Type.Node(_) => q"true"
      case structure.Type.Ref(_) => q"true"
      case Types.PrimitiveType() => q"$left == $right"
      case Types.OptionType(t) => q"$left.isEmpty == $right.isEmpty && ($left.isEmpty || ${compareValues(q"$left.get", q"$right.get", t)})"

      case Types.EitherType(l, r) =>
        q"""
          $left.isLeft == $right.isLeft &&
          (
            ($left.isLeft && ${compareValues(q"$left.left", q"$right.left", l)}) ||
            ($left.isRight && ${compareValues(q"$left.right", q"$right.right", r)})
          )
        """

      case Types.TupleTypes(ts) =>
        ts.zipWithIndex.map {
          case (t, i) =>
            val field = Term.Name(s"_${i+1}")
            compareValues(q"$left.$field", q"$right.$field", t)
        }.reduce((l, r) => q"$l && $r")

      case Types.SeqType(t) => q"$left.size == $right.size && $left.zip($right).forall { case (l0, r0) => ${compareValues(q"l0", q"r0", t)} }"

      case _ => q"???"
    }

  def compareRefs(left: Term, right: Term, node: NodeDefinition): Term =
    node.fields.foldRight[Term](q"$LazyListObj()") {
      case ((field, t), acc) =>
        val fieldValues = compareRefs(q"$left.${Term.Name(field)}", q"$right.${Term.Name(field)}", t)
        q"$fieldValues #::: $acc"
    }

  def compareRefs(left: Term, right: Term, t: structure.Type): Term =
    t match {
      case structure.Type.Node(_) => q"$LazyListObj()"
      case structure.Type.Ref(_) => q"$LazyListObj($MatchingReferenceObj($left.decl, $right.decl))"
      case Types.PrimitiveType() => q"$LazyListObj()"
      case Types.OptionType(t) => q"if($left.isDefined) ${compareRefs(q"$left.get", q"$right.get", t)} else $LazyListObj()"
      case Types.EitherType(l, r) =>
        q"if($left.isLeft) ${compareRefs(q"$left.left", q"$right.left", l)} else ${compareRefs(q"$left.right", q"$right.right", r)}"
      case Types.TupleTypes(ts) =>
        ts.zipWithIndex.map {
          case (t, i) =>
            val field = Term.Name(s"_${i+1}")
            compareRefs(q"$left.$field", q"$right.$field", t)
        }.reduce((l, r) => q"$l #::: $r")
      case Types.SeqType(t) => q"$left.zip($right).to($LazyListObj).flatMap { case (l0, r0) => ${compareRefs(q"l0", q"r0", t)} }"

      case _ => q"???"
    }

  def compareSubnodes(left: Term, right: Term, node: NodeDefinition): Term =
    node.fields.foldRight[Term](q"$LazyListObj()") {
      case ((field, t), acc) =>
        val fieldValues = compareSubnodes(q"$left.${Term.Name(field)}", q"$right.${Term.Name(field)}", t)
        q"$fieldValues #::: $acc"
    }

  def compareSubnodes(left: Term, right: Term, t: structure.Type): Term =
    t match {
      case structure.Type.Node(_) => q"$left.compare($right)"
      case structure.Type.Ref(_) => q"$LazyListObj()"
      case Types.PrimitiveType() => q"$LazyListObj()"
      case Types.OptionType(t) => q"if($left.isDefined) ${compareSubnodes(q"$left.get", q"$right.get", t)} else $LazyListObj()"
      case Types.EitherType(l, r) =>
        q"if($left.isLeft) ${compareSubnodes(q"$left.left", q"$right.left", l)} else ${compareSubnodes(q"$left.right", q"$right.right", r)}"
      case Types.TupleTypes(ts) =>
        ts.zipWithIndex.map {
          case (t, i) =>
            val field = Term.Name(s"_${i+1}")
            compareSubnodes(q"$left.$field", q"$right.$field", t)
        }.reduce((l, r) => q"$l #::: $r")
      case Types.SeqType(t) => q"$left.zip($right).to($LazyListObj).flatMap { case (l0, r0) => ${compareSubnodes(q"l0", q"r0", t)} }"

      case _ => q"???"
    }

  def simplify(t: Term): Term = simplifyFlatly(recurse(t))

  def recurse(term: Term): Term = term match {
    // Exception: immediately push .to(LazyList) inwards, since we can further simplify the subterms.
    case q"(if($cond) $whenTrue else $whenFalse).to(${`LazyListObj`})" =>
      simplify(q"if($cond) $whenTrue.to($LazyListObj) else $whenFalse.to($LazyListObj)")

    case q"if($cond) $whenTrue else $whenFalse" =>
      q"if(${simplify(cond)}) ${simplify(whenTrue)} else ${simplify(whenFalse)}"
    case q"$zipped.flatMap({ case ($l, $r) => $exp })" =>
      q"${simplify(zipped)}.flatMap({ case ($l, $r) => ${simplify(exp)} })"
    case q"$zipped.forall({ case($l, $r) => $exp })" =>
      q"${simplify(zipped)}.forall({ case($l, $r) => ${simplify(exp)} })"
    case q"$_.LazyList(..$terms)" => q"$LazyListObj(..${terms.map(simplify)})"
    case q"$l #:: $r" => q"${simplify(l)} #:: ${simplify(r)}"
    case q"$l #::: $r" => q"${simplify(l)} #::: ${simplify(r)}"
    case q"$l && $r" => q"${simplify(l)} && ${simplify(r)}"
    case q"$l || $r" => q"${simplify(l)} || ${simplify(r)}"
    case q"$l == $r" => q"${simplify(l)} == ${simplify(r)}"
    case q"$l.$r" => q"${simplify(l)}.$r"
    case q"($l, $r)" => q"(${simplify(l)}, ${simplify(r)})"
    case q"$inner.to($_.LazyList)" => q"${simplify(inner)}.to($LazyListObj)"
    case q"$l.zip($r)" => q"${simplify(l)}.zip(${simplify(r)})"
    case _: Term.Name | q"true" | q"false" | q"this" => term
    case q"$_.compare($_)" => term
    case q"$_.MatchingDeclaration(..$_)" => term
    case q"$_.MatchingReference(..$_)" => term
    case q"$_.StructuralDifference(..$_)" => term
    case other =>
      println(s"[warn] [ColHelper] Not recursing simplifier into unknown term $term")
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

    case q"$_.LazyList() #::: $xs" => xs
    case q"$xs #::: $_.LazyList()" => xs

    case q"if(true) $whenTrue else $_" => whenTrue
    case q"if(false) $_ else $whenFalse" => whenFalse

    case q"$_.LazyList($x) #::: $xs" => q"$x #:: $xs"
    case q"($a #::: $b) #::: $c" => q"$a #::: $b #::: $c"

    case q"$_.flatMap({ case ($l, $r) => $_.LazyList() })" => q"$LazyListObj()"
    case q"$_.forall({ case ($l, $r) => true })" => q"true"

    case q"if($cond) $whenTrue else $whenFalse" if whenTrue.getClass == whenFalse.getClass && whenTrue.show[Structure] == whenFalse.show[Structure] =>
      whenTrue

    case other => return other
  })
}
