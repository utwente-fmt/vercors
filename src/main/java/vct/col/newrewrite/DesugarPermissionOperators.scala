package vct.col.newrewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteBuilders._
import vct.col.newrewrite.DesugarPermissionOperators.{FramedArraySubscriptBlame, FramedPointerDerefBlame, PredicateValueError}
import vct.col.origin.{ArrayInsufficientPermission, ArrayLocationError, ArraySubscriptError, Blame, FramedArrIndex, FramedArrLength, IteratedArrayInjective, Origin, PointerBounds, PointerDerefError, PointerInsufficientPermission, PointerLocationError, PointerSubscriptError, PointsToDeref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

case object DesugarPermissionOperators extends RewriterBuilder {
  override def key: String = "desugarPerm"
  override def desc: String = "Desugar assorted syntactic sugar for permission predicates."

  case class FramedArraySubscriptBlame(blame: Blame[ArrayLocationError]) extends Blame[ArraySubscriptError] {
    override def blame(error: ArraySubscriptError): Unit = {
      error match {
        case error: ArrayLocationError => blame.blame(error)
        case ArrayInsufficientPermission(node) => PointsToDeref.blame(error)
      }
    }
  }

  case class FramedPointerDerefBlame(blame: Blame[PointerLocationError]) extends Blame[PointerDerefError] {
    override def blame(error: PointerDerefError): Unit = {
      error match {
        case error: PointerLocationError => blame.blame(error)
        case PointerInsufficientPermission(node) => PointsToDeref.blame(error)
      }
    }
  }

  case class PredicateValueError(loc: Location[_]) extends UserError {
    override def code: String = "predicateValue"

    override def text: String = loc.o.messageInContext("The predicate has a location but does not point to a value.")
  }

}

case class DesugarPermissionOperators[Pre <: Generation]() extends Rewriter[Pre] {

  def extractValueFromLocation(loc: Location[Pre]): Expr[Pre] = {
    loc match {
      case FieldLocation(obj, field) => Deref(obj, field)(PointsToDeref)(loc.o)
      case ModelLocation(obj, field) => ModelDeref(obj, field)(PointsToDeref)(loc.o)
      case SilverFieldLocation(obj, field) => SilverDeref(obj, field)(PointsToDeref)(loc.o)
      case node @ ArrayLocation(array, subscript) => ArraySubscript(array, subscript)(FramedArraySubscriptBlame(node.blame))(loc.o)
      case node @ PointerLocation(pointer) => DerefPointer(pointer)(FramedPointerDerefBlame(node.blame))(loc.o)
      case PredicateLocation(predicate, args) => throw PredicateValueError(loc)
      case InstancePredicateLocation(predicate, obj, args) => throw PredicateValueError(loc)
      case AmbiguousLocation(expr) => expr
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case PointsTo(loc1, perm1, value1) =>
        val (loc, perm, value) = (dispatch(loc1), dispatch(perm1), dispatch(value1))
        Perm(loc, perm) &* (dispatch(extractValueFromLocation(loc1)) === value)
      case ValidArray(arr1, len1) =>
        val (arr, len) = (dispatch(arr1), dispatch(len1))
        (arr !== Null()) && (Length(arr)(FramedArrLength) === len)
      case ValidMatrix(mat1, dim01, dim11) =>
        val (mat, dim0, dim1) = (dispatch(mat1), dispatch(dim01), dispatch(dim11))
        (mat !== Null()) && (Length(mat)(FramedArrLength) === dim0) &*
          starall(IteratedArrayInjective, TInt(), row =>
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
