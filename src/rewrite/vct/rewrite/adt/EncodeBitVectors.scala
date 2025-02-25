package vct.rewrite.adt

import hre.data.BitString
import vct.col.ast.{
  Applicable,
  Asserting,
  BitAnd,
  BitNot,
  BitOr,
  BitShl,
  BitShr,
  BitUShr,
  BitXor,
  Declaration,
  Expr,
  Function,
  Minus,
  Node,
  PolarityDependent,
  Result,
  Select,
  SmtlibBitvecLiteral,
  SmtlibBv2Nat,
  SmtlibBvAnd,
  SmtlibBvNot,
  SmtlibBvOr,
  SmtlibBvShl,
  SmtlibBvShr,
  SmtlibInt2Bv,
  SuccessorsProvider,
  SuccessorsProviderTrafo,
  TBool,
  TInt,
  TSmtlibBitVector,
  UnitAccountedPredicate,
  Variable,
  Z3BvSLt,
  Z3BvSShr,
  Z3BvXor,
}
import vct.col.origin.{
  AbstractApplicable,
  AssertFailed,
  Blame,
  IntegerOutOfBounds,
  LabelContext,
  Origin,
  PanicBlame,
  TrueSatisfiable,
}
import vct.col.rewrite.{
  Generation,
  NonLatchingRewriter,
  Rewriter,
  RewriterBuilder,
}
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

case object EncodeBitVectors extends RewriterBuilder {
  private case class OutOfBoundsBlame(
      node: Node[_],
      blame: Blame[IntegerOutOfBounds],
  )(implicit val bits: Int)
      extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      blame.blame(IntegerOutOfBounds(node, bits))
  }

  private val BaseOrigin: Origin = Origin(
    Seq(LabelContext("Bit vector helpers"))
  )

  override def key: String = "encodeBitVectors"

  override def desc: String = "Encodes bit vector operations into SMT-LIB types"
}

case class EncodeBitVectors[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeBitVectors._

  private val isInboundsMap: mutable.HashMap[(Int, Boolean), Function[Post]] =
    mutable.HashMap()
  private val assumeInboundsMap
      : mutable.HashMap[(Int, Boolean), Function[Post]] = mutable.HashMap()

  private def to(
      e: Expr[Post],
      blame: Blame[IntegerOutOfBounds],
  )(implicit bits: Int, signed: Boolean): Expr[Post] = {
    val stripped = StripAsserting().dispatch(e)
    SmtlibInt2Bv(
      Asserting(ensureInRange(stripped), stripped)(OutOfBoundsBlame(e, blame))(
        e.o
      ),
      bits,
    )(e.o)
  }

  private def from(
      e: Expr[Post],
      doAssume: Boolean,
  )(implicit bits: Int, signed: Boolean): Expr[Post] = {
    implicit val o: Origin = e.o
    if (signed) {
      Select(
        Z3BvSLt(
          if (doAssume)
            assumeInRange(e)
          else
            e,
          SmtlibBitvecLiteral(BitString("0".repeat(bits))),
        ),
        SmtlibBv2Nat(e) - const(BigInt(2).pow(bits)),
        SmtlibBv2Nat(e),
      )
    } else {
      SmtlibBv2Nat(
        if (doAssume)
          assumeInRange(e)
        else
          e
      )
    }
  }

  private case class StripAsserting[G]() extends NonLatchingRewriter[G, G]() {
    case class SuccOrIdentity()
        extends SuccessorsProviderTrafo[G, G](allScopes) {
      override def postTransform[T <: Declaration[G]](
          pre: Declaration[G],
          post: Option[T],
      ): Option[T] = Some(post.getOrElse(pre.asInstanceOf[T]))
    }
    override def succProvider: SuccessorsProvider[G, G] = SuccOrIdentity()

    override def dispatch(e: Expr[G]): Expr[G] =
      e match {
        case Asserting(_, body) => dispatch(body)
        case _ => e.rewriteDefault()
      }
  }

  private def simplifyBV(e: Expr[Post])(implicit signed: Boolean) =
    e match {
      case SmtlibInt2Bv(
            Asserting(
              _,
              Select(
                Z3BvSLt(Asserting(_, e0), SmtlibBitvecLiteral(_)),
                Minus(SmtlibBv2Nat(e1), _),
                SmtlibBv2Nat(e3),
              ),
            ),
            _,
          ) if signed && e0 == e1 && e1 == e3 =>
        e0
      case SmtlibInt2Bv(Asserting(_, SmtlibBv2Nat(e0)), _) if !signed => e0
      case _ => e
    }

  private def ensureInRange(
      e: Expr[Post]
  )(implicit bits: Int, signed: Boolean): Expr[Post] = {
    functionInvocation[Post](
      TrueSatisfiable,
      isInboundsMap.getOrElseUpdate(
        (bits, signed), {
          implicit val o: Origin = BaseOrigin
          val (min, max): (Expr[Post], Expr[Post]) =
            if (signed) {
              (
                const(-BigInt(2).pow(bits - 1)),
                const(BigInt(2).pow(bits - 1) - 1),
              )
            } else { (const(0), const(BigInt(2).pow(bits) - 1)) }
          val x = new Variable[Post](TInt())(BaseOrigin.where(name = "x"))
          globalDeclarations.declare(withResult((result: Result[Post]) => {
            function(
              AbstractApplicable,
              TrueSatisfiable,
              TBool(),
              Seq(x),
              ensures = UnitAccountedPredicate(
                (result === (min <= x.get && x.get <= max)) &&
                  (result ===
                    (from(SmtlibInt2Bv(x.get, bits), doAssume = false) ===
                      x.get))
              ),
            )(BaseOrigin.where(name = s"bv${bits}_is_inbounds"))
          }))
        },
      ).ref,
      args = Seq(e),
    )(e.o)
  }

  private def assumeInRange(
      e: Expr[Post]
  )(implicit bits: Int, signed: Boolean): Expr[Post] = {
    val stripped = StripAsserting().dispatch(e)
    Asserting(
      functionInvocation[Post](
        TrueSatisfiable,
        assumeInboundsMap.getOrElseUpdate(
          (bits, signed), {
            implicit val o: Origin = BaseOrigin
            val x =
              new Variable[Post](TSmtlibBitVector(bits))(
                BaseOrigin.where(name = "x")
              )
            globalDeclarations.declare(
              function[Post](
                PanicBlame("Postcondition is assert true"),
                TrueSatisfiable,
                TBool(),
                Seq(x),
                body = Some(tt),
                ensures = UnitAccountedPredicate(PolarityDependent(
                  SmtlibInt2Bv(from(x.get, doAssume = false), bits) === x.get,
                  tt,
                )),
              )(BaseOrigin.where(name = s"bv${bits}_assume_inbounds"))
            )
          },
        ).ref,
        args = Seq(stripped),
      )(e.o),
      e,
    )(PanicBlame("Assert true"))(e.o)
  }

  private def binOp(
      op: (Expr[Post], Expr[Post]) => Expr[Post],
      l: Expr[Pre],
      r: Expr[Pre],
      b: Int,
      s: Boolean,
      blame: Blame[IntegerOutOfBounds],
  ): Expr[Post] = {
    implicit val bits: Int = b
    implicit val signed: Boolean = s
    from(
      op(
        simplifyBV(to(dispatch(l), blame)),
        simplifyBV(to(dispatch(r), blame)),
      ),
      doAssume = true,
    )
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case op @ BitAnd(l, r, b, s) =>
        binOp(SmtlibBvAnd[Post](_, _), l, r, b, s, op.blame)
      case op @ BitOr(l, r, b, s) =>
        binOp(SmtlibBvOr[Post](_, _), l, r, b, s, op.blame)
      case op @ BitXor(l, r, b, s) =>
        binOp(Z3BvXor[Post](_, _), l, r, b, s, op.blame)
      case op @ BitShl(l, r, b, s) =>
        binOp(SmtlibBvShl[Post](_, _), l, r, b, s, op.blame)
      case op @ BitShr(l, r, b) =>
        binOp(Z3BvSShr[Post](_, _), l, r, b, s = true, op.blame)
      case op @ BitUShr(l, r, b, s) =>
        binOp(SmtlibBvShr[Post](_, _), l, r, b, s, op.blame)
      case op @ BitNot(arg, b, s) =>
        implicit val bits: Int = b
        implicit val signed: Boolean = s
        from(
          SmtlibBvNot(simplifyBV(to(dispatch(arg), op.blame))),
          doAssume = true,
        )
      case _ => super.dispatch(e)
    }
  }
}
