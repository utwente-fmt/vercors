package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants.{ComparePackage, CompareResult, LazyList, LazyListObj, MatchingDeclarationObj, MatchingReferenceObj, Node, StructuralDifferenceObj}
import vct.col.ast.helpers.defn.Naming.{compareTrait, typ}
import vct.col.ast.helpers.defn.Simplify.simplify
import vct.col.ast.structure
import vct.col.ast.structure.{DeclaredNode, NodeDefinition, NodeGenerator}

import java.nio.file.Path
import scala.meta._

class Compare extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit =
    ResultStream.write(out.resolve(s"${node.name.base}Compare.scala"), getTree(node))

  def getTree(node: NodeDefinition): Source = {
    source"""
      package $ComparePackage

      trait ${compareTrait(node)}[L] { this: ${typ(node)}[L] =>
        def compare[R](other: $Node[R]): $LazyList[$CompareResult[L, R]] =
          if(this.getClass != other.getClass) $LazyListObj($StructuralDifferenceObj(this, other))
          else {
            val `~x` = other.asInstanceOf[${typ(node)}[R]]
            ${compare(q"this", q"`~x`", node)}
          }
      }
    """
  }

  def compare(left: Term, right: Term, node: NodeDefinition): Term = {
    val valuesEqual = compareValues(left, right, node)
    val refs = compareRefs(left, right, node)
    val subnodes = compareSubnodes(left, right, node)

    simplify(
      q"""
        if($valuesEqual) ${
          if(node.kind == DeclaredNode) q"$MatchingDeclarationObj($left, $right) #:: $refs #::: $subnodes"
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
      case structure.Type.Node(_) |
           structure.Type.Declaration(_) |
           structure.Type.DeclarationSeq(_) => q"true"

      case structure.Type.Ref(_) | structure.Type.MultiRef(_) => q"true"

      case _: structure.Type.PrimitiveType => q"$left == $right"

      case structure.Type.Option(t) => q"$left.isEmpty == $right.isEmpty && ($left.isEmpty || ${compareValues(q"$left.get", q"$right.get", t)})"

      case structure.Type.Either(l, r) =>
        q"""
          $left.isLeft == $right.isLeft &&
          (
            ($left.isLeft && ${compareValues(q"$left.left", q"$right.left", l)}) ||
            ($left.isRight && ${compareValues(q"$left.right", q"$right.right", r)})
          )
        """

      case structure.Type.Tuple(ts) =>
        ts.zipWithIndex.map {
          case (t, i) =>
            val field = Term.Name(s"_${i+1}")
            compareValues(q"$left.$field", q"$right.$field", t)
        }.reduce((l, r) => q"$l && $r")

      case structure.Type.Seq(t) => q"$left.size == $right.size && $left.zip($right).forall { case (l0, r0) => ${compareValues(q"l0", q"r0", t)} }"
    }

  def compareRefs(left: Term, right: Term, node: NodeDefinition): Term =
    node.fields.foldRight[Term](q"$LazyListObj()") {
      case ((field, t), acc) =>
        val fieldValues = compareRefs(q"$left.${Term.Name(field)}", q"$right.${Term.Name(field)}", t)
        q"$fieldValues #::: $acc"
    }

  def compareRefs(left: Term, right: Term, t: structure.Type): Term =
    t match {
      case structure.Type.Node(_) | structure.Type.Declaration(_) | structure.Type.DeclarationSeq(_) => q"$LazyListObj()"
      case structure.Type.Ref(_) | structure.Type.MultiRef(_) => q"$LazyListObj($MatchingReferenceObj($left.decl, $right.decl))"
      case _: structure.Type.PrimitiveType => q"$LazyListObj()"
      case structure.Type.Option(t) => q"if($left.isDefined) ${compareRefs(q"$left.get", q"$right.get", t)} else $LazyListObj()"
      case structure.Type.Either(l, r) =>
        q"if($left.isLeft) ${compareRefs(q"$left.left", q"$right.left", l)} else ${compareRefs(q"$left.right", q"$right.right", r)}"
      case structure.Type.Tuple(ts) =>
        ts.zipWithIndex.map {
          case (t, i) =>
            val field = Term.Name(s"_${i+1}")
            compareRefs(q"$left.$field", q"$right.$field", t)
        }.reduce((l, r) => q"$l #::: $r")
      case structure.Type.Seq(t) => q"$left.zip($right).to($LazyListObj).flatMap { case (l0, r0) => ${compareRefs(q"l0", q"r0", t)} }"
    }

  def compareSubnodes(left: Term, right: Term, node: NodeDefinition): Term =
    node.fields.foldRight[Term](q"$LazyListObj()") {
      case ((field, t), acc) =>
        val fieldValues = compareSubnodes(q"$left.${Term.Name(field)}", q"$right.${Term.Name(field)}", t)
        q"$fieldValues #::: $acc"
    }

  def compareSubnodes(left: Term, right: Term, t: structure.Type): Term =
    t match {
      case structure.Type.Node(_) | structure.Type.Declaration(_) => q"$left.compare($right)"
      case structure.Type.Ref(_) | structure.Type.MultiRef(_) => q"$LazyListObj()"
      case _: structure.Type.PrimitiveType => q"$LazyListObj()"
      case structure.Type.Option(t) => q"if($left.isDefined) ${compareSubnodes(q"$left.get", q"$right.get", t)} else $LazyListObj()"
      case structure.Type.Either(l, r) =>
        q"if($left.isLeft) ${compareSubnodes(q"$left.left", q"$right.left", l)} else ${compareSubnodes(q"$left.right", q"$right.right", r)}"
      case structure.Type.Tuple(ts) =>
        ts.zipWithIndex.map {
          case (t, i) =>
            val field = Term.Name(s"_${i+1}")
            compareSubnodes(q"$left.$field", q"$right.$field", t)
        }.reduce((l, r) => q"$l #::: $r")
      case structure.Type.Seq(t) => q"$left.zip($right).to($LazyListObj).flatMap { case (l0, r0) => ${compareSubnodes(q"l0", q"r0", t)} }"
      case structure.Type.DeclarationSeq(t) => q"$left.zip($right).to($LazyListObj).flatMap { case (l0, r0) => l0.compare(r0) }"
    }
}
