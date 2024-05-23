package vct.col.ast.lang.c

import vct.col.ast.{AmbiguousMult, CTVector, Mult, Node, SizeOf}
import vct.col.ast.ops.CTVectorOps
import vct.col.ast.util.ExpressionEqualityCheck.isConstantInt
import vct.col.print.{Ctx, Doc, Text}
import vct.result.VerificationError.UserError

object CTVector {
  case class WrongVectorType(decl: Node[_]) extends UserError {
    override def code: String = "wrongVectorType"

    override def text: String =
      decl.o.messageInContext(s"This has a vector type that is not supported.")
  }
}

trait CTVectorImpl[G] extends CTVectorOps[G] {
  this: CTVector[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("vector") <> "<" <> innerType <> "," <> size <> ">"

  lazy val intSize =
    size match {
      case AmbiguousMult(SizeOf(ts), e)
          if ts == innerType && isConstantInt(e).isDefined =>
        isConstantInt(e).get
      case AmbiguousMult(e, SizeOf(ts))
          if ts == innerType && isConstantInt(e).isDefined =>
        isConstantInt(e).get
      case Mult(e, SizeOf(ts))
          if ts == innerType && isConstantInt(e).isDefined =>
        isConstantInt(e).get
      case Mult(SizeOf(ts), e)
          if ts == innerType && isConstantInt(e).isDefined =>
        isConstantInt(e).get
      case _ => throw CTVector.WrongVectorType(this)
    }
}
