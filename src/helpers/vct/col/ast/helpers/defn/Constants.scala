package vct.col.ast.helpers.defn

import vct.col.ast.helpers.defn.Naming.typ
import vct.col.ast.structure
import vct.col.ast.structure.Constants.{DeclarationName, RootNodeName}

import scala.meta._

object Constants {
  val NothingType: Type = t"_root_.scala.Nothing"
  val SeqType: Type = t"_root_.scala.Seq"
  val SeqObj: Term = q"_root_.scala.Seq"
  val Unit: Type = t"_root_.scala.Unit"
  val Any: Type = t"_root_.scala.Any"
  val Boolean: Type = t"_root_.scala.Boolean"
  val Long: Type = t"_root_.scala.Long"
  val OptionType: Type = t"_root_.scala.Option"
  val BigInt: Type = t"_root_.scala.BigInt"
  val BigInteger: Type = t"_root_.java.math.BigInteger"
  val BigDecimal: Type = t"_root_.scala.BigDecimal"
  val JBigDecimal: Type = t"_root_.java.math.BigDecimal"
  val BitString: Type = t"_root_.hre.data.BitString"
  val LazyList: Type = t"_root_.scala.collection.immutable.LazyList"
  val LazyListObj: Term = q"_root_.scala.collection.immutable.LazyList"
  val MutMap: Type = t"_root_.scala.collection.mutable.Map"
  val Map: Type = t"_root_.scala.collection.immutable.Map"
  val ClassTag: Type = t"_root_.scala.reflect.ClassTag"

  val LeftObj: Term = q"_root_.scala.util.Left"
  val RightObj: Term = q"_root_.scala.util.Right"

  val copyByteStringFrom: Term =
    q"_root_.com.google.protobuf.ByteString.copyFrom"

  val Node: Type = typ(RootNodeName)
  val Declaration: Type = typ(DeclarationName)
  val ExprName: structure.Name = structure
    .Name(Seq("_root_", "vct", "col", "ast", "Expr"))
  val Expr: Type = typ(ExprName)
  val Coercion: Type = t"_root_.vct.col.ast.Coercion"
  val ApplyCoercionPat: (Pat, Pat) => Pat =
    (e, c) => p"_root_.vct.col.ast.ApplyCoercion($e, $c)"
  val Origin: Type = t"_root_.vct.col.origin.Origin"
  val OriginObj: Term = q"_root_.vct.col.origin.Origin"
  val Blame: Type = t"_root_.vct.col.origin.Blame"
  val VerificationFailure: Type = t"_root_.vct.col.origin.VerificationFailure"
  val InconsistentSuccessionTypesObj: Term =
    q"_root_.vct.col.err.InconsistentSuccessionTypes"
  val RefType: Type = t"_root_.vct.col.ref.Ref"
  val LazyRef: Type = t"_root_.vct.col.ref.LazyRef"
  val SuccessorProvider: Type = t"_root_.vct.col.rewrite.SuccessorProvider"
  val SuccessorProviderChain: Type = t"_root_.vct.col.rewrite.SuccessorProviderChain"
  val SuccessorProviderNothing: Type = t"_root_.vct.col.rewrite.SuccessorProviderNothing"
  val SuccessorProviderTrafo: Type = t"_root_.vct.col.rewrite.SuccessorProviderTrafo"

  val AbstractRewriter: Type = t"_root_.vct.col.ast.AbstractRewriter"
  val Scopes: Type = t"_root_.vct.col.util.Scopes"
  val ScopesObj: Term = q"_root_.vct.col.util.Scopes"
  val AllScopes: Type = t"_root_.vct.col.ast.AllScopes"
  val AllScopesObj: Term = q"_root_.vct.col.ast.AllScopes"
  val AllFrozenScopes: Type = t"_root_.vct.col.ast.AllFrozenScopes"
  val SuccessorsProvider: Type = t"_root_.vct.col.ast.SuccessorsProvider"
  val SerializeBlame: Term = q"_root_.vct.col.serialize.SerializeBlame"
  val SerializeOrigin: Term = q"_root_.vct.col.serialize.SerializeOrigin"

  val CompareResult = t"_root_.vct.col.compare.CompareResult"
  val MatchingDeclaration = t"_root_.vct.col.compare.MatchingDeclaration"
  val MatchingDeclarationObj = q"_root_.vct.col.compare.MatchingDeclaration"
  val MatchingReference = t"_root_.vct.col.compare.MatchingReference"
  val MatchingReferenceObj = q"_root_.vct.col.compare.MatchingReference"
  val StructuralDifference = t"_root_.vct.col.compare.StructuralDifference"
  val StructuralDifferenceObj = q"_root_.vct.col.compare.StructuralDifference"

  val NodeMessageView = t"_root_.vct.col.serialize.NodeMessageView"

  val OpsPackage = q"vct.col.ast.ops"
  val RewritePackage = q"$OpsPackage.rewrite"
  val ComparePackage = q"$OpsPackage.compare"
  val SubnodesPackage = q"$OpsPackage.subnodes"
  val SerializePackage = q"$OpsPackage.serialize"
  val DeserializePackage = q"$OpsPackage.deserialize"
  val DeclarePackage = q"$OpsPackage.declare"
}
