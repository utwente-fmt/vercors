package vct.col.ast.helpers.defn

import vct.col.ast.helpers.defn.Naming.typ
import vct.col.ast.structure
import vct.col.ast.structure.Constants.RootNodeName

import scala.meta._

object Constants {
  // Imported by default: scala._
  val ScNothing: structure.Name = structure.Name(Seq("_root_", "scala", "Nothing"))
  val ScUnit: structure.Name = structure.Name(Seq("_root_", "scala", "Unit"))
  val ScBoolean: structure.Name = structure.Name(Seq("_root_", "scala", "Boolean"))
  val ScByte: structure.Name = structure.Name(Seq("_root_", "scala", "Byte"))
  val ScShort: structure.Name = structure.Name(Seq("_root_", "scala", "Short"))
  val ScInt: structure.Name = structure.Name(Seq("_root_", "scala", "Int"))
  val ScLong: structure.Name = structure.Name(Seq("_root_", "scala", "Long"))
  val ScFloat: structure.Name = structure.Name(Seq("_root_", "scala", "Float"))
  val ScDouble: structure.Name = structure.Name(Seq("_root_", "scala", "Double"))
  val ScChar: structure.Name = structure.Name(Seq("_root_", "scala", "Char"))
  val BigIntPredef: structure.Name = structure.Name(Seq("_root_", "scala", "BigInt"))
  val BigDecimalPredef: structure.Name = structure.Name(Seq("_root_", "scala", "BigDecimal"))

  val ScSeqPredef: structure.Name = structure.Name(Seq("_root_", "scala", "Seq"))
  val ScEitherPredef: structure.Name = structure.Name(Seq("_root_", "scala", "Either"))
  val ScOption: structure.Name = structure.Name(Seq("_root_", "scala", "Option"))

  // Imported by default: java.lang._
  val ScString: structure.Name = structure.Name(Seq("_root_", "java", "lang", "String"))

  // Imported by default: scala.Predef._
  // ... nothing yet ...

  // Convenience: all following types are also considered imported by default
  val BitString: structure.Name = structure.Name(Seq("_root_", "hre", "data", "BitString"))

  val ScBigInt: structure.Name = structure.Name(Seq("_root_", "scala", "math", "BigInt"))
  val ScBigDecimal: structure.Name = structure.Name(Seq("_root_", "scala", "math", "BigDecimal"))

  val ScSeq: structure.Name = structure.Name(Seq("_root_", "scala", "collection", "immutable", "Seq"))
  val ScEither: structure.Name = structure.Name(Seq("_root_", "scala", "util", "Either"))

  val ScTupleMin = 1
  val ScTupleMax = 22
  def ScTuple(i: Int): structure.Name = structure.Name(Seq("_root_", "scala", s"Tuple$i"))

  val LazyList: Type = t"_root_.scala.collection.immutable.LazyList"
  val LazyListObj: Term = q"_root_.scala.collection.immutable.LazyList"
  val LeftObj: Term = q"_root_.scala.util.Left"
  val RightObj: Term = q"_root_.scala.util.Right"

  val Node: Type = typ(RootNodeName)
  val Origin: Type = t"_root_.vct.col.origin.Origin"
  val Blame: Type = t"_root_.vct.col.origin.Blame"
  val RefType: Type = t"_root_.vct.col.ref.Ref"

  val AbstractRewriter: Type = t"_root_.vct.col.ast.rewrite.AbstractRewriter"

  val CompareResult = t"_root_.vct.col.compare.CompareResult"
  val MatchingDeclaration = t"_root_.vct.col.compare.MatchingDeclaration"
  val MatchingDeclarationObj = q"_root_.vct.col.compare.MatchingDeclaration"
  val MatchingReference = t"_root_.vct.col.compare.MatchingReference"
  val MatchingReferenceObj = q"_root_.vct.col.compare.MatchingReference"
  val StructuralDifference = t"_root_.vct.col.compare.StructuralDifference"
  val StructuralDifferenceObj = q"_root_.vct.col.compare.StructuralDifference"

}
