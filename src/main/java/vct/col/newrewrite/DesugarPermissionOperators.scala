package vct.col.newrewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteBuilders._
import vct.col.origin.{FramedArrIndex, FramedArrLength, Origin}
import vct.col.rewrite.Rewriter

case class DesugarPermissionOperators() extends Rewriter {
  override def dispatch(e: Expr): Expr = {
    implicit val o: Origin = e.o
    e match {
      case PointsTo(loc, perm, value) =>
        Perm(loc, perm) &* loc === value
      case ValidArray(arr, len) =>
        arr !== Null() && Length(arr)(FramedArrLength) === len
      case ValidMatrix(mat, dim0, dim1) =>
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
    }
  }
}
