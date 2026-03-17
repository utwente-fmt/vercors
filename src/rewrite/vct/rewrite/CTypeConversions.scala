package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast.`type`.typeclass.TFloats
import vct.col.ast._
import vct.col.origin.{Blame, Origin, PanicBlame, UnsafeCoercion}
import vct.col.rewrite.error.ExtraNode
import vct.col.rewrite.{Generation, RewriterBuilder, RewriterBuilderArg2}
import vct.col.typerules.{CoercingRewriter, CoercionUtils, TypeSize}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}

import scala.annotation.tailrec

case object CTypeConversions extends RewriterBuilderArg2[Boolean, Boolean] {
  override def key: String = "cTypeConversions"
  override def desc: String =
    "Casts from integers to and from floats, booleans, and pointers"

  case class MinimalSize() extends UserError {
    override def code: String = "incompleteSizeInformation"

    override def text: String =
      "Insufficient size information to check integer bounds, make sure to set a target with `--target`"
  }

  case class ConversionImplementationDefined(
      v: Expr[_],
      vt: TCInt[_],
      tt: TCInt[_],
  ) extends UserError {
    override def code: String = "implementationDefinedConversion"

    override def text: String =
      v.o.messageInContext(
        s"This value with type integer (bits=${vt.storedBits},signed=${vt.signed}) is converted to type integer (bits=${tt
            .storedBits},signed=${tt.signed}) which has implementation defined behaviour."
      )
  }
}

case class CTypeConversions[Pre <: Generation](
    checkIntegerBounds: Boolean,
    unsetTarget: Boolean,
) extends CoercingRewriter[Pre] {
  import CTypeConversions._

  private val globalBlame: ScopedStack[Blame[UnsafeCoercion]] = ScopedStack()
  private val returnContext: ScopedStack[Type[Pre]] = ScopedStack()
  private val inPure: ScopedStack[Unit] = ScopedStack()

  override def postCoerce(program: Program[Pre]): Program[Post] = {
    globalBlame.having(program.blame) {
      program.rewrite(declarations =
        globalDeclarations.dispatch(program.declarations)
      )
    }
  }
  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoerceCFloatCInt(_) => CastFloat(e, TInt())
      case CoerceCIntCFloat(target) => CastFloat(e, dispatch(target))
      case CoerceDecreasePrecision(_, target) => CastFloat(e, dispatch(target))
      case CoerceCIntBool() => Neq(e, const(0))
      case CoerceBoolCInt(_) => Select(e, const(1), const(0))
      case CoercePointerBool(t) =>
        t match {
          case pt: PointerType[Pre] if pt.isNonNull => ff
          case pt: PointerArrayType[Pre] if pt.isNonNull => ff
          case _: PointerType[Pre] | _: PointerArrayType[Pre] =>
            PointerNeq(e, Null(), const(0))
          case _ => throw ExtraNode
        }
      case c if ignoreMappedCoercion(c) => e
      case other => super.applyCoercion(e, other)
    }

  def isIgnoredCoercion(c: Coercion[_]): Boolean =
    c match {
      case CoerceCFloatFloat(_, _) => true
      case CoerceCIntInt(_) => true
      case CoerceIncreasePrecision(_, _) => true
      case CoerceIncreasePrecision(_, _) => true
      case _ => false
    }

  def ignoreMappedCoercion(c: Coercion[_]): Boolean =
    c match {
      case CoerceMapSeq(inner, _, _) if isIgnoredCoercion(inner) => true
      case CoerceMapSet(inner, _, _) if isIgnoredCoercion(inner) => true
      case CoerceMapBag(inner, _, _) if isIgnoredCoercion(inner) => true
      case CoerceMapMap(inner, _, _) if isIgnoredCoercion(inner) => true
      case CoerceMapTuple(inner, _, _) if inner.forall(isIgnoredCoercion) =>
        true
      case CoerceMapVector(inner, _, _, _) if isIgnoredCoercion(inner) => true
      case CoerceMapOption(inner, _, _) if isIgnoredCoercion(inner) => true
      case _ => false
    }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case i @ TCInt()
          if inPure.isEmpty && checkIntegerBounds && !unsetTarget =>
        i.storedBits match {
          case TypeSize.Unknown() =>
            throw Unreachable("Unknown size should never appear")
          case TypeSize.Exact(size) =>
            if (i.signed) {
              TCheckedInt(
                -BigInt(2).pow(size.intValue - 1),
                BigInt(2).pow(size.intValue - 1),
              )(globalBlame.top)
            } else {
              TCheckedInt(BigInt(0), BigInt(2).pow(size.intValue))(
                globalBlame.top
              )
            }
          case TypeSize.Minimally(size) => throw MinimalSize()
        }
      case TCInt() => TInt()
      // This is wrong, but since we translate to rationals anyways, this does not matter.
      // Getting everything to type check otherwise is a pain, since in "coerce" we always coerce
      // to an arbitrary big float.
      case TCFloat(_, _) => TFloats.ieee754_32bit
      case TFloat(_, _) => TFloats.ieee754_32bit
      case other => other.rewriteDefault()
    }

  @tailrec
  private def stripQualifiers(t: Type[Pre]): Type[Pre] =
    t match {
      case TUnique(inner, _) => stripQualifiers(inner)
      case TConst(inner) => stripQualifiers(inner)
      case other => other
    }

  private def applyTwoWayPromotions[T](
      l: Expr[Pre],
      r: Expr[Pre],
      cons: (Expr[Pre], Expr[Pre]) => T,
  ): T = {
    (stripQualifiers(l.t), stripQualifiers(r.t)) match {
      case (lt: TCInt[Pre], rt: TCInt[Pre]) =>
        if (inPure.nonEmpty || unsetTarget)
          return cons(l, r)
        lazy val castToL = cons(l, Cast(r, TypeValue(lt)(r.o))(r.o))
        lazy val castToR = cons(Cast(l, TypeValue(rt)(l.o))(l.o), r)
        (lt.signed, rt.signed) match {
          case (false, false) | (true, true) if lt.rank > rt.rank => castToL
          case (false, false) | (true, true) if rt.rank > lt.rank => castToR
          case (false, false) | (true, true) => cons(l, r)
          case (false, true) =>
            if (lt.rank >= rt.rank)
              castToL
            else if (
              rt.storedBits > lt.storedBits
            ) // Strictly greater than implies that the maximum of the unsigned value fits in the signed one
              castToR
            else {
              if (!checkIntegerBounds)
                logger.warn(
                  s"Expression ${cons(l, r)} might overflow due to a conversion from signed to unsigned, to ensure soundness set a target and enable integer bounds checking"
                )
              val cint = TCInt[Pre]()(rt.o)
              cint.storedBits = rt.storedBits
              cint.signed = false
              cint.rank = rt.rank
              cons(
                Cast(l, TypeValue(cint)(l.o))(l.o),
                Cast(r, TypeValue(cint)(r.o))(r.o),
              )
            }

          case (true, false) =>
            if (rt.rank >= lt.rank)
              castToR
            else if (
              lt.storedBits > rt.storedBits
            ) // Strictly greater than implies that the maximum of the unsigned value fits in the signed one
              castToL
            else {
              if (!checkIntegerBounds)
                logger.warn(
                  s"Expression ${cons(l, r)} might overflow due to a conversion from signed to unsigned, to ensure soundness set a target and enable integer bounds checking"
                )
              val cint = TCInt[Pre]()(lt.o)
              cint.storedBits = lt.storedBits
              cint.signed = false
              cint.rank = lt.rank
              cons(
                Cast(l, TypeValue(cint)(l.o))(l.o),
                Cast(r, TypeValue(cint)(r.o))(r.o),
              )
            }
        }
      case (TBool(), rt: TCInt[Pre]) =>
        cons(Cast(l, TypeValue(rt)(l.o))(l.o), r)
      case (lt: TCInt[Pre], TBool()) =>
        cons(l, Cast(r, TypeValue(lt)(r.o))(r.o))
      case (TBool(), TBool()) => cons(l, r)
    }
  }

  private def applyOneWayPromotions[T](
      v: Expr[Pre],
      target: Type[Pre],
      cons: Expr[Pre] => T,
  ): T = {
    (v.t, target) match {
      case (vt: TCInt[Pre], tt: TCInt[Pre]) =>
        if (inPure.nonEmpty || unsetTarget)
          return cons(v)
        lazy val cast = cons(Cast(v, TypeValue(tt)(v.o))(v.o))
        // If target type is unsigned the cast is always safe since this
        if (!tt.signed) { cast }
        else if (checkIntegerBounds) {
          if (
            (vt.storedBits > tt.storedBits ||
              (!vt.signed && vt.storedBits == tt.storedBits)) &&
            isInSignedRangeConstant(v, tt.storedBits).isEmpty
          ) { throw ConversionImplementationDefined(v, vt, tt) }
          else { cast }
        } else {
          if (
            (vt.rank > tt.rank || (vt.rank == tt.rank && !vt.signed)) &&
            isInSignedRangeConstant(v, tt.storedBits).isEmpty
          ) {
            logger.warn(
              s"Expression ${cons(v)} might have implementation defined behaviour if $v is out of bounds for the target type"
            )
          }
          cast
        }
      case (TBool(), _: TCInt[Pre]) | (_: TCInt[Pre], TBool()) =>
        cons(Cast(v, TypeValue(target)(v.o))(v.o))
      case _ => cons(v)
    }
  }

  private def isInSignedRangeConstant(
      e: Expr[Pre],
      size: TypeSize,
  ): Option[(BigInt, BigInt)] = {
    getConstant(e).flatMap(c =>
      size match {
        case TypeSize.Unknown() =>
          throw Unreachable("Unknown size should never appear")
        case TypeSize.Minimally(_) => throw MinimalSize()
        case TypeSize.Exact(bits) =>
          if (c.bitLength < bits) { Some((bits, c)) }
          else { None }
      }
    )
  }

  private def getConstant(e: Expr[Pre]): Option[BigInt] =
    e match {
      case op @ UMinus(arg) => getConstant(arg).map(-_)
      case op @ AmbiguousMult(l, r) =>
        getConstant(l).zip(getConstant(r)).map { case (l, r) => l * r }
      case op @ AmbiguousDiv(l, r) =>
        getConstant(l).zip(getConstant(r)).map { case (l, r) =>
          l / r +
            (if (l % r >= 0) { 0 }
             else if (r > 0) { -1 }
             else { 1 })
        }
      case op @ AmbiguousMod(l, r) =>
        getConstant(l).zip(getConstant(r)).map { case (l, r) => l.mod(r) }
      case op @ AmbiguousTruncDiv(l, r) =>
        getConstant(l).zip(getConstant(r)).map { case (l, r) => l / r }
      case op @ AmbiguousTruncMod(l, r) =>
        getConstant(l).zip(getConstant(r)).map { case (l, r) => l % r }
      case op @ AmbiguousPlus(l, r) =>
        getConstant(l).zip(getConstant(r)).map { case (l, r) => l + r }
      case op @ AmbiguousMinus(l, r) =>
        getConstant(l).zip(getConstant(r)).map { case (l, r) => l - r }
      case CIntegerValue(v, _) => Some(v)
      case IntegerValue(v) => Some(v)
      case _ => None
    }

  private def knownUnsigned(e: Expr[Pre]): Boolean =
    stripQualifiers(e.t) match {
      case t @ TCInt() if !t.signed => true
      case _ => false
    }

  // For division and modulo if we know that neither operand is negative we know the result must fit in the required space
  private def surelyInUnsignedRange(e: Expr[Pre]): Boolean =
    e match {
      case op @ AmbiguousDiv(l, r) =>
        (getConstant(l), getConstant(r)) match {
          case (Some(l), Some(r)) if l >= 0 && r >= 0 => true
          case (Some(l), _) if l >= 0 && knownUnsigned(r) => true
          case (_, Some(r)) if r >= 0 && knownUnsigned(l) => true
          case (None, None) if knownUnsigned(l) && knownUnsigned(r) => true
          case _ => false
        }
      case op @ AmbiguousMod(l, r) =>
        (getConstant(l), getConstant(r)) match {
          case (Some(l), _) if l >= 0 && knownUnsigned(r) => true
          case (_, Some(r)) if r >= 0 && knownUnsigned(l) => true
          case (None, None) if knownUnsigned(l) && knownUnsigned(r) => true
          case _ => false
        }
      case op @ AmbiguousTruncDiv(l, r) =>
        (getConstant(l), getConstant(r)) match {
          case (Some(l), _) if l >= 0 && knownUnsigned(r) => true
          case (_, Some(r)) if r >= 0 && knownUnsigned(l) => true
          case (None, None) if knownUnsigned(l) && knownUnsigned(r) => true
          case _ => false
        }
      case op @ AmbiguousTruncMod(l, r) =>
        (getConstant(l), getConstant(r)) match {
          case (Some(l), _) if l >= 0 && knownUnsigned(r) => true
          case (_, Some(r)) if r >= 0 && knownUnsigned(l) => true
          case (None, None) if knownUnsigned(l) && knownUnsigned(r) => true
          case _ => false
        }
      case _ => false
    }

  private def applyWrapAround(e: BinExpr[Pre]): Expr[Pre] = {
    implicit val o: Origin = e.o
    e.getNumericType match {
      case i @ TCInt() =>
        if (i.signed || unsetTarget)
          e
        else {
          val constant = getConstant(e)
          if (constant.isDefined) {
            Cast(
              UncheckedMath(const(
                constant.get.mod(BigInt(2).pow(i.storedBits.getExact.intValue))
              )),
              TypeValue(i),
            )
          } else {
            Cast(
              UncheckedMath(
                Mod(e, const(BigInt(2).pow(i.storedBits.getExact.intValue)))(
                  PanicBlame("t.storedBits.exact should not be 0")
                )
              ),
              TypeValue(i),
            )
          }
        }
    }
  }

  override def preCoerce(e: Expr[Pre]): Expr[Pre] = {
    e match {
      // All the operators which need conversions
      case op @ AmbiguousMult(l, r) if op.isCIntOp =>
        applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousMult(_, _)(op.o),
        )
      case op @ AmbiguousDiv(l, r) if op.isCIntOp =>
        applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousDiv(_, _)(op.blame)(op.o),
        )
      case op @ AmbiguousMod(l, r) if op.isCIntOp =>
        applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousMod(_, _)(op.blame)(op.o),
        )
      case op @ AmbiguousTruncMod(l, r) if op.isCIntOp =>
        applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousTruncMod(_, _)(op.blame)(op.o),
        )
      case op @ AmbiguousPlus(l, r) if op.isCIntOp =>
        applyWrapAround(applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousPlus(_, _)(op.blame)(op.o),
        ))
      case op @ AmbiguousMinus(l, r) if op.isCIntOp =>
        applyWrapAround(applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousMinus(_, _)(op.blame)(op.o),
        ))
      case op @ AmbiguousLess(l, r, size) if op.isCIntOp =>
        applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousLess(_, _, size)(op.o),
        )
      case op @ AmbiguousGreater(l, r, size) if op.isCIntOp =>
        applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousGreater(_, _, size)(op.o),
        )
      case op @ AmbiguousLessEq(l, r, size) if op.isCIntOp =>
        applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousLessEq(_, _, size)(op.o),
        )
      case op @ AmbiguousGreaterEq(l, r, size) if op.isCIntOp =>
        applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousGreaterEq(_, _, size)(op.o),
        )
      case op @ AmbiguousEq(l, r, vt, size) if op.isCIntOp =>
        applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousEq(_, _, vt, size)(op.o),
        )
      case op @ AmbiguousNeq(l, r, vt, size) if op.isCIntOp =>
        applyTwoWayPromotions(
          preCoerce(l),
          preCoerce(r),
          AmbiguousNeq(_, _, vt, size)(op.o),
        )
      case op @ UMinus(arg) =>
        val newArg = preCoerce(arg)
        val coerce = CoercionUtils.getAnyCoercion(newArg.t, TCInt())
        if (coerce.isDefined) {
          val cint = coerce.get.target.asInstanceOf[TCInt[Pre]]
          if (!cint.signed) {
            val newType = TCInt[Pre]()
            newType.storedBits = cint.storedBits
            newType.rank = cint.rank
            newType.signed = true
            Cast(UMinus(newArg)(op.o), TypeValue(newType)(op.o))(op.o)
          } else { UMinus(newArg)(op.o) }
        } else { UMinus(newArg)(op.o) }
      // TODO: Not doing the bitwise operators since we handled those earlier (might want to move that here?)
      // case op@BitAnd(l, r, bits, signed) if op.isCIntOp =>applyTwoWayPromotions(l, r, BitAnd(_, _, bits, signed)(op.o))
      // case op@BitXor(l, r, bits, signed) if op.isCIntOp =>applyTwoWayPromotions(l, r, BitXor(_, _, bits, signed)(op.o))
      // case op@BitOr(l, r, bits, signed) if op.isCIntOp =>applyTwoWayPromotions(l, r, BitOr(_, _)(op.o))
      case Select(cond, t, f) =>
        applyOneWayPromotions(preCoerce(cond), TBool(), Select(_, t, f)(e.o))
      // Assignments
      case a @ PreAssignExpression(target, value) =>
        applyOneWayPromotions(
          preCoerce(value),
          target.t,
          PreAssignExpression(target, _)(a.blame)(a.o),
        )
      case a @ PostAssignExpression(target, value) =>
        applyOneWayPromotions(
          preCoerce(value),
          target.t,
          PostAssignExpression(target, _)(a.blame)(a.o),
        )
      // Calls
      // TODO: We don't do given/yields and outArgs here, assuming that those don't do implicit conversions
      case inv: Invocation[Pre] =>
        implicit val o: Origin = inv.o
        val applicable = inv.ref.decl
        val newArgs = inv.args.zip(inv.ref.decl.args).map { case (v, t) =>
          applyOneWayPromotions(preCoerce(v), t.t, it => it)
        }
        inv match {
          case m: AnyMethodInvocation[Pre] =>
            m match {
              case i: ProcedureInvocation[Pre] =>
                i.copy(args = newArgs)(i.blame)
              case i: MethodInvocation[Pre] => i.copy(args = newArgs)(i.blame)
              case i: ConstructorInvocation[Pre] =>
                i.copy(args = newArgs)(i.blame)
            }
          case f: AnyFunctionInvocation[Pre] =>
            f match {
              case i @ FunctionInvocation(
                    ref,
                    args,
                    typeArgs,
                    givenMap,
                    yields,
                    reveal,
                  ) =>
                i.copy(args = newArgs)(i.blame)
              case i @ InstanceFunctionInvocation(
                    obj,
                    ref,
                    args,
                    typeArgs,
                    givenMap,
                    yields,
                  ) =>
                i.copy(args = newArgs)(i.blame)
            }
        }
      case _ => super.preCoerce(e)
    }
  }

  override def preCoerce(s: Statement[Pre]): Statement[Pre] =
    s match {
      case a @ Assign(target, value) =>
        applyOneWayPromotions(
          preCoerce(value),
          target.t,
          Assign(target, _)(a.blame)(a.o),
        )
      case a @ AssignInitial(target, value) =>
        applyOneWayPromotions(
          preCoerce(value),
          target.t,
          AssignInitial(target, _)(a.blame)(a.o),
        )
      case Branch(branches) =>
        Branch(branches.map { case (cond, s) =>
          applyOneWayPromotions(preCoerce(cond), TBool(), (_, s))
        })(s.o)
      // If returnContext is empty then this must be a different sort of return which we don't have in C (I've specifically seen JavaBIP examples fail)
      case Return(value) if returnContext.nonEmpty =>
        applyOneWayPromotions(value, returnContext.top, Return(_)(s.o))
      case _ => super.preCoerce(s)
    }

  override def postCoerce(s: Statement[Pre]): Statement[Post] =
    s match {
      case assert @ Assert(res) =>
        assert.rewrite(res = inPure.having(()) { dispatch(res) })
      case exhale @ Exhale(res) =>
        exhale.rewrite(res = inPure.having(()) { dispatch(res) })
      case inhale @ Inhale(res) =>
        inhale.rewrite(res = inPure.having(()) { dispatch(res) })
      case assume @ Assume(assn) =>
        assume.rewrite(assn = inPure.having(()) { dispatch(assn) })
      case _ => super.postCoerce(s)
    }

  override def postCoerce(d: Declaration[Pre]): Unit = {
    d match {
      case axiom: ADTAxiom[Pre] => inPure.having(()) { super.postCoerce(axiom) }
      case resource: AbstractPredicate[Pre] =>
        inPure.having(()) { super.postCoerce(resource) }
      case app: ContractApplicable[Pre] =>
        returnContext.having(app.returnType) {
          allScopes.anySucceed(
            app,
            app.rewrite(contract = inPure.having(()) { dispatch(app.contract) }),
          )
        }
      case _ => super.postCoerce(d)
    }
  }

  override def postCoerce(contract: LoopContract[Pre]): LoopContract[Post] = {
    contract match {
      case inv @ LoopInvariant(invariant, _) =>
        inv.rewrite(invariant = inPure.having(()) { dispatch(invariant) })
      case contract @ IterationContract(requires, ensures, _) =>
        contract.rewrite(
          requires = inPure.having(()) { dispatch(requires) },
          ensures = inPure.having(()) { dispatch(ensures) },
        )
    }
  }

  private def applyCast(e: Expr[Pre], t: TCInt[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e.t match {
      case et: TCInt[Pre]
          if !unsetTarget && et.storedBits == t.storedBits &&
            et.signed == t.signed =>
        dispatch(e)
      case et: TCInt[Pre] if !unsetTarget && !t.signed =>
        t.storedBits match {
          case TypeSize.Unknown() =>
            throw Unreachable("Unknown size should never appear")
          case TypeSize.Minimally(_) => throw MinimalSize()
          case TypeSize.Exact(size) =>
            val constant = getConstant(e)
            if (constant.isDefined) {
              UncheckedMath(const(
                constant.get.mod(BigInt(2).pow(t.storedBits.getExact.intValue))
              ))
            } else if (surelyInUnsignedRange(e)) { dispatch(e) }
            else {
              UncheckedMath(
                Mod(
                  dispatch(e),
                  const(BigInt(2).pow(t.storedBits.getExact.intValue)),
                )(PanicBlame("t.storedBits.exact should not be 0"))
              )
            }
        }
      case et: TCInt[Pre] if !unsetTarget && et.storedBits < t.storedBits =>
        dispatch(e)
      // This can happen if this is a user-specified cast
      case et: TCInt[Pre] if !unsetTarget =>
        val constant = isInSignedRangeConstant(e, t.storedBits);
        if (constant.isDefined) {
          val (size, value) = constant.get
          CheckedIntegerValue(
            value,
            -BigInt(2).pow(size.intValue - 1),
            BigInt(2).pow(size.intValue - 1),
          )(globalBlame.top)
        } else { throw ConversionImplementationDefined(e, et, t) }
      case _: TCInt[Pre] => dispatch(e)
      case TBool() => Select(dispatch(e), const(1), const(0))
      // Assume that we've already added done something with this expression (for example add a Mod) which means it doesn't have to be rechecked
      // This all a bit weird because I'm abusing the preCoerce step here
      case TInt() =>
        e match {
          case UncheckedMath(m @ Mod(l, r)) =>
            // Skip pre-coercion for l since it was already done
            UncheckedMath(Mod(super.postCoerce(l), dispatch(r))(m.blame)(m.o))(
              e.o
            )
          case _ =>
            throw Unreachable(
              s"Expected to only get here if we have a Cast(UncheckedMath(Mod(expression, MAX_INT))) structure but got `$e`"
            )
        }
    }
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] =
    e match {
      case UncheckedMath(inner) =>
        UncheckedMath(inPure.having(()) { dispatch(inner) })(e.o)
      case AmbiguousEq(a, b, TCInt(), size) =>
        AmbiguousEq(dispatch(a), dispatch(b), TInt(), size.map(dispatch))(e.o)
      case AmbiguousNeq(a, b, TCInt(), size) =>
        AmbiguousNeq(dispatch(a), dispatch(b), TInt(), size.map(dispatch))(e.o)
      case Cast(v, tv @ TypeValue(t @ TCInt())) =>
        Cast(applyCast(v, t), TypeValue(dispatch(t))(tv.o))(e.o)
      case Cast(WithExactType(v, TCInt()), tv @ TypeValue(TBool())) =>
        Neq(dispatch(v), const(0)(v.o))(v.o)
      case Cast(v, tv @ TypeValue(TBool())) if v.t.asPointer.isDefined =>
        if (v.t.asPointer.get.isNonNull) { ff }
        else { PointerNeq(dispatch(v), Null()(e.o), const(0)(e.o))(e.o) }
      case CIntegerValue(v, i @ TCInt())
          if inPure.isEmpty && checkIntegerBounds && !unsetTarget =>
        i.storedBits match {
          case TypeSize.Unknown() =>
            throw Unreachable("Unknown size should never appear")
          case TypeSize.Exact(size) =>
            if (i.signed) {
              CheckedIntegerValue(
                v,
                -BigInt(2).pow(size.intValue - 1),
                BigInt(2).pow(size.intValue - 1),
              )(globalBlame.top)(e.o)
            } else {
              CheckedIntegerValue(v, BigInt(0), BigInt(2).pow(size.intValue))(
                globalBlame.top
              )(e.o)
            }
          case TypeSize.Minimally(size) => throw MinimalSize()
        }
      case CIntegerValue(v, _) => IntegerValue(v)(e.o)
      case asserting @ Asserting(condition, _) =>
        asserting.rewrite(condition = inPure.having(()) { dispatch(condition) })
      case assuming @ Assuming(assn, _) =>
        assuming.rewrite(assn = inPure.having(()) { dispatch(assn) })
      case binder: Binder[Pre] =>
        binder match {
          case _: Exists[Pre] | _: Forall[Pre] | _: Starall[Pre] |
              _: ForPerm[Pre] | _: ForPermWithValue[Pre] | _: Sum[Pre] |
              _: Product[Pre] =>
            inPure.having(()) { super.postCoerce(binder) }
          case let @ Let(_, _, _) => super.postCoerce(let)
        }
      case _ => super.postCoerce(e)
    }
}
