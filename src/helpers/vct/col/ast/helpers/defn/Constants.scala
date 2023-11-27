package vct.col.ast.helpers.defn

import vct.col.ast.helpers.defn.Naming.typ
import vct.col.ast.structure
import vct.col.ast.structure.Constants.RootNodeName

import scala.meta._

object Constants {
  val LazyList: Type = t"_root_.scala.collection.immutable.LazyList"
  val LazyListObj: Term = q"_root_.scala.collection.immutable.LazyList"
  val LeftObj: Term = q"_root_.scala.util.Left"
  val RightObj: Term = q"_root_.scala.util.Right"

  val Node: Type = typ(RootNodeName)
  val Origin: Type = t"_root_.vct.col.origin.Origin"
  val Blame: Type = t"_root_.vct.col.origin.Blame"
  val RefType: Type = t"_root_.vct.col.ref.Ref"

  val AbstractRewriter: Type = t"_root_.vct.col.ast.AbstractRewriter"

  val CompareResult = t"_root_.vct.col.compare.CompareResult"
  val MatchingDeclaration = t"_root_.vct.col.compare.MatchingDeclaration"
  val MatchingDeclarationObj = q"_root_.vct.col.compare.MatchingDeclaration"
  val MatchingReference = t"_root_.vct.col.compare.MatchingReference"
  val MatchingReferenceObj = q"_root_.vct.col.compare.MatchingReference"
  val StructuralDifference = t"_root_.vct.col.compare.StructuralDifference"
  val StructuralDifferenceObj = q"_root_.vct.col.compare.StructuralDifference"

  val OpsPackage = q"vct.col.ast.ops"
  val RewritePackage = q"$OpsPackage.rewrite"
  val ComparePackage = q"$OpsPackage.rewrite"
}
