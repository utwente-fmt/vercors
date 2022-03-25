package vct.col.newrewrite

import vct.col.ast
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast.RewriteHelpers._
import vct.col.ast.{APerm, ActionApply, ActionPerm, AddrOf, AmbiguousMember, AmbiguousMinus, AmbiguousMult, AmbiguousPlus, AmbiguousResult, AmbiguousSubscript, AmbiguousThis, Apply, ApplyCoercion, ArraySubscript, AssignExpression, BagAdd, BagLargestCommon, BagMemberCount, BagMinus, BinExpr, Binder, CExpr, CType, Cast, CompositeType, Concat, Cons, Constant, CurPerm, CurrentThreadId, DeclaredType, Deref, DerefPointer, DividingExpr, Drop, EitherLeft, EitherOp, EitherRight, Empty, EmptyProcess, Expr, FunctionOf, HPerm, Head, HeapDeref, Held, IdleToken, InlinePattern, InstanceOf, IntegerValue, InternedString, IsLeft, IsRight, JavaExpr, JavaType, JoinToken, Length, LiteralBag, LiteralMap, LiteralSeq, LiteralSet, LiteralTuple, Local, Locator, MapCmp, MapCons, MapGet, MapMember, MapOp, MapRemove, MatrixRepeat, MatrixSum, ModelAbstractState, ModelChoose, ModelCreate, ModelDeref, ModelDestroy, ModelMerge, ModelNew, ModelPerm, ModelSplit, ModelState, NewArray, NewObject, NoPerm, Old, OptGet, OptGetOrElse, OptNone, OptSome, PVLExpr, PVLType, Perm, PermPointer, PermPointerIndex, Permutation, PointerAdd, PointerSubscript, PointsTo, PrimitiveType, ProcessApply, ProcessChoice, ProcessPar, ProcessSelect, ProcessSeq, ReadPerm, RemoveAt, Result, Scale, ScopedExpr, Select, SeqMember, SeqSubscript, SeqUpdate, SetIntersection, SetMember, SetMinus, SetUnion, SilverExpr, SilverType, Size, Slice, Star, StringLiteral, TArray, TInt, TNotAValue, TPointer, TSeq, TString, TType, TUnion, TVar, Tail, Take, Then, ThisModel, ThisObject, TupGet, Type, TypeOf, TypeValue, UnExpr, Unfolding, UntypedLiteralBag, UntypedLiteralSeq, UntypedLiteralSet, ValidArray, ValidMatrix, Values, VectorRepeat, VectorSum, Wand, With, WritePerm}
import vct.col.util.AstBuildHelpers._

case object EncodeString extends RewriterBuilder {
  override def key: String = "encodeString"

  override def desc: String = "Encodes spec string using seq<int>."

}

case class EncodeString[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case StringLiteral(data) => LiteralSeq[Post](TInt(), data.map((c: Char) => const(c.toInt)(expr.o)))(expr.o)
    case e => rewriteDefault(e)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TString() => TSeq[Post](TInt[Post]()(t.o))(t.o)
    case t => rewriteDefault(t)
  }
}