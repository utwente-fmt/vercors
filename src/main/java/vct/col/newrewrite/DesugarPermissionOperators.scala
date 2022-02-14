package vct.col.newrewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteBuilders._
import vct.col.origin.{FramedArrIndex, FramedArrLength, Origin}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

case object DesugarPermissionOperators extends RewriterBuilder

case class DesugarPermissionOperators[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case PointsTo(loc1, perm1, value1) =>
        val (loc, perm, value) = (dispatch(loc1), dispatch(perm1), dispatch(value1))
        Perm(loc, perm) &* loc === value
      case ValidArray(arr1, len1) =>
        val (arr, len) = (dispatch(arr1), dispatch(len1))
        (arr !== Null()) && Length(arr)(FramedArrLength) === len
      case ValidMatrix(mat1, dim01, dim11) =>
        val (mat, dim0, dim1) = (dispatch(mat1), dispatch(dim01), dispatch(dim11))
        mat !== Null() && Length(mat)(FramedArrLength) === dim0 &*
          starall(TInt(), row =>
            (const(0) <= row && row < dim0) ==>
              arrayPerm(mat, row, ReadPerm())
          ) &* forall(TInt(), row =>
            (const(0) <= row && row < dim0) ==>
              (ArraySubscript(mat, row)(FramedArrIndex) !== Null())
          ) &* forall(TInt(), row =>
            (const(0) <= row && row < dim0) ==>
              (Length(ArraySubscript(mat, row)(FramedArrIndex))(FramedArrLength) === dim1)
          ) &* forall(TInt(), row0 => forall(TInt(), row1 =>
            (const(0) <= row0 && row0 < dim0 && const(0) <= row1 && row1 < dim0) ==>
              ((ArraySubscript(mat, row0)(FramedArrIndex) === ArraySubscript(mat, row1)(FramedArrIndex)) ==> (row0 === row1))
          ))
      case PermPointer(p, len, perm) =>
        // TODO PB: need some concept of blocks to do this in the new pointer encoding
        ???
      case PermPointerIndex(p, idx, perm) =>
        ???
      case other => rewriteDefault(other)
    }
  }
}
