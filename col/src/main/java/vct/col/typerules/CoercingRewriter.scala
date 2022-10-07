package vct.col.typerules

import com.typesafe.scalalogging.LazyLogging
import hre.util.FuncTools
import vct.col.ast._
import vct.col.ast.`type`.TFloats
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{SystemError, Unreachable}

case class NopCoercingRewriter[Pre <: Generation]() extends CoercingRewriter[Pre]() {
  override def applyCoercion(e: Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = e
}

case object CoercingRewriter {
  sealed trait CoercionError extends SystemError {
    override def text: String =
      "Internal type error: CoercionErrors must not bubble. " + (this match {
        case IncoercibleDummy => "(No alternative matched, see stack trace)"
        case Incoercible(e, target) => s"Expression $e could not be coerced to $target"
        case IncoercibleText(e, message) => s"Expression $e could not be coerced. $message."
      })
  }

  case object IncoercibleDummy extends CoercionError
  case class Incoercible(e: Expr[_], target: Type[_]) extends CoercionError
  case class IncoercibleText(e: Expr[_], message: String) extends CoercionError

  case class CoercionOrigin(of: Expr[_]) extends Origin {
    override def preferredName: String = "unknown"
    override def shortPosition: String = of.o.shortPosition
    override def context: String = of.o.context
    override def inlineContext: String = of.o.inlineContext
  }
}

abstract class CoercingRewriter[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  import CoercingRewriter._

  val coercedDeclaration: SuccessionMap[Declaration[Pre], Declaration[Pre]] = SuccessionMap()

  class CoercedSuccessorsProvider extends SuccessorsProviderTrafo[Pre, Pre](null) {
    override def preTransform[I <: Declaration[Pre], O <: Declaration[Pre]](pre: I): Option[O] =
      Some(coercedDeclaration(pre).asInstanceOf[O])
  }

  override def succProvider: SuccessorsProvider[Pre, Post] =
    SuccessorsProviderChain(new CoercedSuccessorsProvider, allScopes.freeze)

  /**
    * Apply a particular coercion to an expression.
    * SAFETY: all promoting coercions must be injective; otherwise the default mapping coercion of sets is unsound.
    * @param e the expression to coerce
    * @param coercion the coercion
    * @return the coerced expression
    */
  def applyCoercion(e: Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = {
    coercion match {
      case CoerceIdentity(_) => e
      case CoercionSequence(cs) => cs.foldLeft(e) { case (e, c) => applyCoercion(e, c) }
      case CoerceNothingSomething(_) => e
      case CoerceSomethingAny(_) => e
      case CoerceMapOption(inner, _, _) =>
        Select(Eq(e, OptNone()), OptNone(), OptSome(applyCoercion(OptGet(e)(NeverNone), inner)))
      case CoerceMapEither((innerLeft, innerRight), _, _) =>
        Select(IsRight(e),
          EitherRight(applyCoercion(GetRight(e)(FramedGetRight), innerRight)),
          EitherLeft(applyCoercion(GetLeft(e)(FramedGetLeft), innerLeft)),
        )
      case CoerceMapSeq(inner, source, target) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable[Post](TSeq(dispatch(source)))
          val i = new Variable[Post](TInt())
          val result_i = SeqSubscript(result, i.get)(FramedSeqIndex)
          val v_i = SeqSubscript(v.get, i.get)(FramedSeqIndex)

          function(
            blame = AbstractApplicable,
            contractBlame = TrueSatisfiable,
            returnType = TSeq(dispatch(target)),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(Size(v.get), Size(result)) &&
              Forall(Seq(i), Seq(Seq(result_i)),
                (const[Post](0) <= i.get && i.get < Size(result)) ==>
                  (result_i === applyCoercion(v_i, inner)))
            ),
          )
        })

        globalDeclarations.declare(f)
        FunctionInvocation[Post](f.ref, Seq(e), Nil, Nil, Nil)(PanicBlame("default coercion for seq<_> requires nothing."))
      case CoerceMapSet(inner, source, target) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable(TSet(dispatch(source)))
          val elem = new Variable(dispatch(source))

          function(
            blame = AbstractApplicable,
            contractBlame = TrueSatisfiable,
            returnType = TSet(dispatch(target)),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(Size(result), Size(v.get)) &&
                Forall(Seq(elem), Seq(Seq(SetMember(elem.get, result))),
                  Eq(SetMember(applyCoercion(elem.get, inner), result), SetMember(elem.get, v.get)))
            ),
          )
        })

        globalDeclarations.declare(f)
        FunctionInvocation[Post](f.ref, Seq(e), Nil, Nil, Nil)(PanicBlame("Default coercion for set<_> requires nothing."))
      case CoerceMapBag(inner, source, target) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable(TBag(dispatch(source)))
          val elem = new Variable(dispatch(source))

          function(
            blame = AbstractApplicable,
            contractBlame = TrueSatisfiable,
            returnType = TBag(dispatch(target)),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(Size(result), Size(v.get)) &&
                Forall(Seq(elem), Seq(Seq(BagMemberCount(elem.get, result))),
                  Eq(BagMemberCount(applyCoercion(elem.get, inner), result), BagMemberCount(elem.get, v.get)))
            ),
          )
        })

        globalDeclarations.declare(f)
        FunctionInvocation[Post](f.ref, Seq(e), Nil, Nil, Nil)(PanicBlame("Default coercion for bag<_> requires nothing."))
      case CoerceMapMatrix(inner, source, target) =>
        ???
      case CoerceMapMap(inner, (sourceKey, sourceValue), (targetKey, targetValue)) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable(TMap(dispatch(sourceKey), dispatch(sourceValue)))
          val k = new Variable(dispatch(sourceKey))

          function(
            blame = AbstractApplicable,
            contractBlame = TrueSatisfiable,
            returnType = TMap(dispatch(targetKey), dispatch(targetValue)),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(MapKeySet(result), MapKeySet(v.get)) &&
                Forall(Seq(k), Seq(Seq(MapGet(result, k.get)(TriggerPatternBlame))),
                  SetMember(k.get, MapKeySet(result)) ==> Eq(MapGet(result, k.get)(FramedMapGet), MapGet(v.get, k.get)(FramedMapGet)))
            ),
          )
        })

        globalDeclarations.declare(f)
        FunctionInvocation[Post](f.ref, Seq(e), Nil, Nil, Nil)(PanicBlame("Default coercion for map<_, _> requires nothing."))
      case CoerceMapTuple(inner, sourceTypes, targetTypes) =>
        LiteralTuple(targetTypes.map(dispatch), inner.zipWithIndex.map { case (c, i) => applyCoercion(TupGet(e, i), c) })
      case CoerceMapType(inner, source, target) =>
        ???

      case CoerceBoolResource() => e
      case CoerceBoundIntFrac() => e
      case CoerceBoundIntZFrac(_) => e
      case CoerceJoinUnion(_, _, _) => e
      case CoerceSelectUnion(inner, _, _, _) => applyCoercion(e, inner)

      case CoerceSupports(_, _) => e
      case CoerceJavaSupports(_, _) => e
      case CoerceCPrimitiveToCol(_, _) => e
      case CoerceColToCPrimitive(_, _) => e
      case CoerceNullRef() => e
      case CoerceNullArray(_) => e
      case CoerceNullClass(_) => e
      case CoerceNullJavaClass(_) => e
      case CoerceNullPointer(_) => e
      case CoerceFracZFrac() => e
      case CoerceZFracRat() => e
      case CoerceFloatRat() => e
      case CoerceWidenBound(_, _) => e
      case CoerceUnboundInt(_) => e

      case CoerceIntRat() => e
      case CoerceRatZFrac() => e
      case CoerceZFracFrac() => e

      case CoerceJavaTClassTPinnedDecl(_, _) => e
      case CoerceTPinnedDeclJavaTClass(_, _) => e
      case CoerceTClassTPinnedDecl(_, _) => e
      case CoerceTPinnedDeclTClass(_, _) => e
    }
  }

  def coerceAny(node: NodeFamily[Pre]): NodeFamily[Pre] = node match {
    case node: Verification[Pre] => node
    case node: VerificationContext[Pre] => node
    case node: Program[Pre] => node
    case node: Statement[Pre] => coerce(node)
    case node: Expr[Pre] => coerce(node)
    case node: Type[Pre] => node
    case node: DecreasesClause[Pre] => node
    case node: AccountedPredicate[Pre] => node
    case node: ApplicableContract[Pre] => node
    case node: LoopContract[Pre] => node
    case node: ParRegion[Pre] => coerce(node)
    case node: CatchClause[Pre] => node
    case node: SignalsClause[Pre] => node
    case node: FieldFlag[Pre] => node
    case node: IterVariable[Pre] => node
    case node: CDeclarator[Pre] => node
    case node: CDeclarationSpecifier[Pre] => node
    case node: CTypeQualifier[Pre] => node
    case node: CPointer[Pre] => node
    case node: CInit[Pre] => node
    case node: CDeclaration[Pre] => node
    case node: GpuMemoryFence[Pre] => node
    case node: JavaModifier[Pre] => node
    case node: JavaImport[Pre] => node
    case node: JavaName[Pre] => node
    case node: JavaVariableDeclaration[Pre] => node
    case node: Coercion[Pre] => node
    case node: PinnedDecl[Pre] => node
    case node: Location[Pre] => node
    case node: BipPortType[Pre] => node
  }

  def preCoerce(e: Expr[Pre]): Expr[Pre] = e
  def postCoerce(e: Expr[Pre]): Expr[Post] = rewriteDefault(e)
  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case ApplyCoercion(e, coercion) => applyCoercion(dispatch(e), coercion)(e.o)
    case other => postCoerce(coerce(preCoerce(other)))
  }

  def preCoerce(stat: Statement[Pre]): Statement[Pre] = stat
  def postCoerce(stat: Statement[Pre]): Statement[Post] = rewriteDefault(stat)
  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    postCoerce(coerce(preCoerce(stat)))

  def preCoerce(decl: Declaration[Pre]): Declaration[Pre] = decl
  def postCoerce(decl: Declaration[Pre]): Unit = rewriteDefault(decl)
  override def dispatch(decl: Declaration[Pre]): Unit = {
    val coercedDecl = coerce(preCoerce(decl))
    coercedDeclaration(decl) = coercedDecl
    postCoerce(coercedDecl)
  }

  def preCoerce(region: ParRegion[Pre]): ParRegion[Pre] = region
  def postCoerce(region: ParRegion[Pre]): ParRegion[Post] = rewriteDefault(region)
  override def dispatch(region: ParRegion[Pre]): ParRegion[Post] =
    postCoerce(coerce(preCoerce(region)))

  def coerce(value: Expr[Pre], target: Type[Pre]): Expr[Pre] =
    ApplyCoercion(value, CoercionUtils.getCoercion(value.t, target) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(value, target)
    })(CoercionOrigin(value))

  def coerceArgs(args: Seq[Expr[Pre]], app: Applicable[Pre]): Seq[Expr[Pre]] =
    args.zip(app.args).map {
      case (value, arg) => coerce(value, arg.t)
    }

  def coerceArgs(args: Seq[Expr[Pre]], app: ContractApplicable[Pre], tArgs: Seq[Type[Pre]]): Seq[Expr[Pre]] =
    args.zip(app.args).map {
      case (value, arg) => coerce(value, arg.t.particularize(app.typeArgs.zip(tArgs).toMap))
    }

  def coerceGiven(givenMap: Seq[(Ref[Pre, Variable[Pre]], Expr[Pre])]): Seq[(Ref[Pre, Variable[Pre]], Expr[Pre])] =
    givenMap.map {
      case (Ref(v), e) => (v.ref, coerce(e, v.t))
    }

  def coerceYields(yields: Seq[(Ref[Pre, Variable[Pre]], Ref[Pre, Variable[Pre]])], blame: => Expr[_]): Seq[(Ref[Pre, Variable[Pre]], Ref[Pre, Variable[Pre]])] =
    yields.map {
      case (Ref(target), Ref(yieldArg)) => CoercionUtils.getCoercion[Pre](yieldArg.t, target.t) match {
        case None => throw IncoercibleText(blame, "The target for a yielded argument does not exactly match the yields type.")
        case Some(CoerceIdentity(_)) => (target.ref, yieldArg.ref)
        case Some(_) => throw IncoercibleText(blame, "The target for a yielded argument does not exactly match the yields type.")
      }
    }

  def rat(e: Expr[Pre]): Expr[Pre] = coerce(e, TRational[Pre]())
  def bool(e: Expr[Pre]): Expr[Pre] = coerce(e, TBool[Pre]())
  def res(e: Expr[Pre]): Expr[Pre] = coerce(e, TResource[Pre]())
  def int(e: Expr[Pre]): Expr[Pre] = coerce(e, TInt[Pre]())
  def string(e: Expr[Pre]): Expr[Pre] = coerce(e, TString[Pre]())
  def javaString(e: Expr[Pre]): Expr[Pre] = coerce(e, TPinnedDecl[Pre](JavaLangString[Pre](), Nil))
  def float(e: Expr[Pre]): Expr[Pre] = coerce(e, TFloats.max[Pre])
  def process(e: Expr[Pre]): Expr[Pre] = coerce(e, TProcess[Pre]())
  def ref(e: Expr[Pre]): Expr[Pre] = coerce(e, TRef[Pre]())
  def option(e: Expr[Pre]): (Expr[Pre], TOption[Pre]) =
    CoercionUtils.getAnyOptionCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected an option here, but got ${e.t}")
    }
  def tuple(e: Expr[Pre]): (Expr[Pre], TTuple[Pre]) =
    CoercionUtils.getAnyTupleCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a tuple here, but got ${e.t}")
    }
  def seq(e: Expr[Pre]): (Expr[Pre], TSeq[Pre]) =
    CoercionUtils.getAnySeqCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a sequence here, but got ${e.t}")
    }
  def set(e: Expr[Pre]): (Expr[Pre], TSet[Pre]) =
    CoercionUtils.getAnySetCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a set here, but got ${e.t}")
    }
  def bag(e: Expr[Pre]): (Expr[Pre], TBag[Pre]) =
    CoercionUtils.getAnyBagCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a bag here, but got ${e.t}")
    }
  def map(e: Expr[Pre]): (Expr[Pre], TMap[Pre]) =
    CoercionUtils.getAnyMapCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a map here, but got ${e.t}")
    }
  def sized(e: Expr[Pre]): (Expr[Pre], SizedType[Pre]) =
    CoercionUtils.getAnySizedCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a collection type here, but got ${e.t}")
    }
  def array(e: Expr[Pre]): (Expr[Pre], TArray[Pre]) =
    CoercionUtils.getAnyArrayCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected an array here, but got ${e.t}")
    }
  def arrayMatrix(e: Expr[Pre]): (Expr[Pre], TArray[Pre]) =
    CoercionUtils.getAnyMatrixArrayCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a two-dimensional array here, but got ${e.t}")
    }
  def pointer(e: Expr[Pre]): (Expr[Pre], TPointer[Pre]) =
    CoercionUtils.getAnyPointerCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a pointer here, but got ${e.t}")
    }
  def cls(e: Expr[Pre]): (Expr[Pre], Type[Pre]) =
    CoercionUtils.getAnyClassCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a class here, but got ${e.t}")
    }
  def matrix(e: Expr[Pre]): (Expr[Pre], TMatrix[Pre]) =
    CoercionUtils.getAnyMatrixCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a matrix here, but got ${e.t}")
    }
  def model(e: Expr[Pre]): (Expr[Pre], TModel[Pre]) =
    CoercionUtils.getAnyModelCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected a model here, but got ${e.t}")
    }
  def either(e: Expr[Pre]): (Expr[Pre], TEither[Pre]) =
    CoercionUtils.getAnyEitherCoercion(e.t) match {
      case Some((coercion, t)) => (ApplyCoercion(e, coercion)(CoercionOrigin(e)), t)
      case None => throw IncoercibleText(e, s"Expected an either here, but got ${e.t}")
    }

  def firstOkHelper[T](thing: Either[Seq[CoercionError], T], onError: => T): Either[Seq[CoercionError], T] =
    thing match {
      case Left(errs) => try {
        Right(onError)
      } catch {
        case err: CoercionError => Left(errs :+ err)
      }
      case Right(value) => Right(value)
    }

  implicit class FirstOkHelper[T](res: Either[Seq[CoercionError], T]) {
    def onCoercionError(f: => T): Either[Seq[CoercionError], T] =
      res match {
        case Left(errs) => try {
          Right(f)
        } catch {
          case err: CoercionError => Left(errs :+ err)
        }
        case Right(value) => Right(value)
      }
  }

  def firstOk[T](expr: Expr[Pre], message: => String,
                  alt1: => T = throw IncoercibleDummy,
                  alt2: => T = throw IncoercibleDummy,
                  alt3: => T = throw IncoercibleDummy,
                  alt4: => T = throw IncoercibleDummy,
                  alt5: => T = throw IncoercibleDummy,
                  alt6: => T = throw IncoercibleDummy,
                  alt7: => T = throw IncoercibleDummy,
                  alt8: => T = throw IncoercibleDummy,
                  alt9: => T = throw IncoercibleDummy,
                  alt10: => T = throw IncoercibleDummy,
                  alt11: => T = throw IncoercibleDummy) : T = {
      Left(Nil)
      .onCoercionError(alt1)
      .onCoercionError(alt2)
      .onCoercionError(alt3)
      .onCoercionError(alt4)
      .onCoercionError(alt5)
      .onCoercionError(alt6)
      .onCoercionError(alt7)
      .onCoercionError(alt8)
    match {
      case Left(errs) =>
        for(err <- errs) {
          logger.debug(err.text)
        }
        throw IncoercibleText(expr, message)
      case Right(value) => value
    }
  }

  def coerce(e: Expr[Pre]): Expr[Pre] = {
    implicit val o: Origin = e.o

    e match {
      case ApplyCoercion(_, _) =>
        throw Unreachable("All instances of ApplyCoercion should be immediately rewritten by CoercingRewriter.dispatch.")

      case ActionApply(action, args) =>
        ActionApply(action, coerceArgs(args, action.decl))
      case ActionPerm(loc, perm) =>
        ActionPerm(loc, rat(perm))
      case AddrOf(e) =>
        AddrOf(e)
      case ADTFunctionInvocation(typeArgs, ref, args) => typeArgs match {
        case Some((adt, typeArgs)) =>
          ADTFunctionInvocation(Some((adt, typeArgs)), ref, args.zip(ref.decl.args).map {
            case (value, arg) =>
              try {
                coerce(value, arg.t.particularize(adt.decl.typeArgs.zip(typeArgs).toMap))
              } catch {
                case x: CoercionError =>
                  println(x.text)
                  ???
              }
          })
        case None =>
          ADTFunctionInvocation(None, ref, coerceArgs(args, ref.decl))
      }
      case AmbiguousComputationalAnd(left, right) =>
        firstOk(e, s"Expected both operands to be of type integer or boolean, but got ${left.t} and ${right.t}.",
          AmbiguousComputationalAnd(int(left), int(right)),
          AmbiguousComputationalAnd(bool(left), bool(right)),
        )
      case AmbiguousComputationalOr(left, right) =>
        firstOk(e, s"Expected both operands to be of type integer or boolean, but got ${left.t} and ${right.t}.",
          AmbiguousComputationalOr(int(left), int(right)),
          AmbiguousComputationalOr(bool(left), bool(right)),
        )
      case AmbiguousComputationalXor(left, right) =>
        firstOk(e, s"Expected both operands to be of type integer or boolean, but got ${left.t} and ${right.t}.",
          AmbiguousComputationalXor(int(left), int(right)),
          AmbiguousComputationalXor(bool(left), bool(right)),
        )
      case AmbiguousGreater(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          AmbiguousGreater(int(left), int(right)),
          AmbiguousGreater(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            AmbiguousGreater(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            AmbiguousGreater(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case AmbiguousGreaterEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          AmbiguousGreaterEq(int(left), int(right)),
          AmbiguousGreaterEq(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            AmbiguousGreaterEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            AmbiguousGreaterEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case AmbiguousLess(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          AmbiguousLess(int(left), int(right)),
          AmbiguousLess(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            AmbiguousLess(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            AmbiguousLess(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case AmbiguousLessEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          AmbiguousLessEq(int(left), int(right)),
          AmbiguousLessEq(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            AmbiguousLessEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            AmbiguousLessEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case minus @ AmbiguousMinus(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set or a bag; or a pointer and integer, but got ${left.t} and ${right.t}.",
          Minus(int(left), int(right)),
          Minus(rat(left), rat(right)),
          AmbiguousMinus(pointer(left)._1, int(right))(minus.blame), {
            val (coercedLeft, TSet(elementLeft)) = set(left)
            val (coercedRight, TSet(elementRight)) = set(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousMinus(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))(minus.blame)
          }, {
            val (coercedLeft, TBag(elementLeft)) = bag(left)
            val (coercedRight, TBag(elementRight)) = bag(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousMinus(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))(minus.blame)
          }
        )
      case AmbiguousMember(x, xs) =>
        firstOk(xs, s"Expected collection to be a sequence, set, bag or map, but got ${xs.t}.", {
          val (coercedXs, TSeq(element)) = seq(xs)
          val sharedType = Types.leastCommonSuperType(x.t, element)
          AmbiguousMember(coerce(x, sharedType), coerce(coercedXs, TSeq(sharedType)))
        }, {
          val (coercedXs, TSet(element)) = set(xs)
          val sharedType = Types.leastCommonSuperType(x.t, element)
          AmbiguousMember(coerce(x, sharedType), coerce(coercedXs, TSet(sharedType)))
        }, {
          val (coercedXs, TBag(element)) = bag(xs)
          val sharedType = Types.leastCommonSuperType(x.t, element)
          AmbiguousMember(coerce(x, sharedType), coerce(coercedXs, TBag(sharedType)))
        }, {
          val (coercedXs, TMap(element, _)) = map(xs)
          AmbiguousMember(coerce(x, element), coercedXs)
        })
      case AmbiguousMult(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a process, a set or a bag but got ${left.t} and ${right.t}.",
          AmbiguousMult(int(left), int(right)),
          AmbiguousMult(rat(left), rat(right)),
          AmbiguousMult(process(left), process(right)), {
            val (coercedLeft, TSet(elementLeft)) = set(left)
            val (coercedRight, TSet(elementRight)) = set(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousMult(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, TBag(elementLeft)) = bag(left)
            val (coercedRight, TBag(elementRight)) = bag(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousMult(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          }
        )
      case AmbiguousOr(left, right) =>
        firstOk(e, s"Expected both operands to be boolean or a process, but got ${left.t} and ${right.t}.",
          AmbiguousOr(bool(left), bool(right)),
          AmbiguousOr(process(left), process(right)),
        )
      case plus @ AmbiguousPlus(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a process, a sequence, set, or bag; or a pointer and integer, but got ${left.t} and ${right.t}.",
          AmbiguousPlus(int(left), int(right))(plus.blame),
          AmbiguousPlus(float(left), float(right))(plus.blame),
          AmbiguousPlus(rat(left), rat(right))(plus.blame),
          AmbiguousPlus(process(left), process(right))(plus.blame),
          AmbiguousPlus(javaString(left), javaString(right))(plus.blame),
          AmbiguousPlus(pointer(left)._1, int(right))(plus.blame), {
            val (coercedLeft, TSeq(elementLeft)) = seq(left)
            val (coercedRight, TSeq(elementRight)) = seq(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousPlus(coerce(coercedLeft, TSeq(sharedType)), coerce(coercedRight, TSeq(sharedType)))(plus.blame)
          }, {
            val (coercedLeft, TSet(elementLeft)) = set(left)
            val (coercedRight, TSet(elementRight)) = set(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousPlus(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))(plus.blame)
          }, {
            val (coercedLeft, TBag(elementLeft)) = bag(left)
            val (coercedRight, TBag(elementRight)) = bag(right)
            val sharedType = Types.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousPlus(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))(plus.blame)
          },
        )
      case AmbiguousResult() => e
      case sub @ AmbiguousSubscript(collection, index) =>
        firstOk(e, s"Expected collection to be a sequence, array, pointer or map, but got ${collection.t}.",
          AmbiguousSubscript(seq(collection)._1, int(index))(sub.blame),
          AmbiguousSubscript(array(collection)._1, int(index))(sub.blame),
          AmbiguousSubscript(pointer(collection)._1, int(index))(sub.blame),
          AmbiguousSubscript(map(collection)._1, coerce(index, map(collection)._2.key))(sub.blame),
        )
      case AmbiguousThis() => e
      case And(left, right) =>
        And(bool(left), bool(right))
      case any @ Any() =>
        Any()(any.blame)
      case a @ ArraySubscript(arr, index) =>
        ArraySubscript(array(arr)._1, int(index))(a.blame)
      case BagAdd(xs, ys) =>
        val (left, TBag(leftT)) = bag(xs)
        val (right, TBag(rightT)) = bag(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        BagAdd(coerce(left, TBag(sharedElement)), coerce(right, TBag(sharedElement)))
      case BagLargestCommon(xs, ys) =>
        val (left, TBag(leftT)) = bag(xs)
        val (right, TBag(rightT)) = bag(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        BagLargestCommon(coerce(left, TBag(sharedElement)), coerce(right, TBag(sharedElement)))
      case BagMemberCount(x, xs) =>
        val (coercedBag, TBag(element)) = bag(xs)
        val sharedType = Types.leastCommonSuperType(x.t, element)
        BagMemberCount(coerce(x, sharedType), coerce(coercedBag, TBag(sharedType)))
      case BagMinus(xs, ys) =>
        val (left, TBag(leftT)) = bag(xs)
        val (right, TBag(rightT)) = bag(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        BagMinus(coerce(left, TBag(sharedElement)), coerce(right, TBag(sharedElement)))
      case BitAnd(left, right) =>
        BitAnd(int(left), int(right))
      case BitNot(arg) =>
        BitNot(int(arg))
      case BitOr(left, right) =>
        BitOr(int(left), int(right))
      case BitShl(left, right) =>
        BitShl(int(left), int(right))
      case BitShr(left, right) =>
        BitShr(int(left), int(right))
      case BitUShr(left, right) =>
        BitUShr(int(left), int(right))
      case BitXor(left, right) =>
        BitXor(int(left), int(right))
      case Cast(value, typeValue) =>
        Cast(value, typeValue)
      case CastFloat(e, t) =>
        CastFloat(float(e), t)
      case CCast(e, t) => CCast(e, t)
      case inv @ CInvocation(applicable, args, givenArgs, yields) =>
        CInvocation(applicable, args, givenArgs, yields)(inv.blame)
      case CLocal(name) => e
      case ComputationalAnd(left, right) =>
        ComputationalAnd(bool(left), bool(right))
      case ComputationalOr(left, right) =>
        ComputationalOr(bool(left), bool(right))
      case ComputationalXor(left, right) =>
        ComputationalXor(bool(left), bool(right))
      case Concat(xs, ys) =>
        val (coercedXs, TSeq(xElement)) = seq(xs)
        val (coercedYs, TSeq(yElement)) = seq(ys)
        val sharedType = Types.leastCommonSuperType(xElement, yElement)
        Concat(coerce(xs, TSeq(sharedType)), coerce(ys, TSeq(sharedType)))
      case Cons(x, xs) =>
        val (coercedXs, TSeq(element)) = seq(xs)
        val sharedType = Types.leastCommonSuperType(x.t, element)
        Cons(coerce(x, sharedType), coerce(xs, TSeq(sharedType)))
      case StringConcat(left, right) =>
        StringConcat(string(left), string(right))
      case JavaStringConcat(left, right) =>
        JavaStringConcat(javaString(left), javaString(right))
      case acc @ CStructAccess(struct, field) =>
        CStructAccess(struct, field)(acc.blame)
      case CStructDeref(struct, field) =>
        CStructDeref(struct, field)
      case CurPerm(loc) =>
        CurPerm(loc)
      case CurrentThreadId() =>
        CurrentThreadId()
      case deref @ Deref(obj, ref) =>
        Deref(cls(obj)._1, ref)(deref.blame)
      case deref @ DerefPointer(p) =>
        DerefPointer(pointer(p)._1)(deref.blame)
      case div @ Div(left, right) =>
        firstOk(e, s"Expected both operands to be rational.",
          // PB: horrible hack: Div ends up being silver.PermDiv, which expects an integer divisor. In other cases,
          // we just hope the silver type-check doesn't complain, since in z3 it is uniformly `/` for mixed integers
          // and rationals.
          Div(rat(left), int(right))(div.blame),
          Div(rat(left), rat(right))(div.blame),
        )
      case Drop(xs, count) =>
        Drop(seq(xs)._1, int(count))
      case Empty(obj) =>
        Empty(sized(obj)._1)
      case EmptyProcess() => EmptyProcess()
      case use @ EnumUse(enum, const) => use
      case Eq(left, right) =>
        val sharedType = Types.leastCommonSuperType(left.t, right.t)
        Eq(coerce(left, sharedType), coerce(right, sharedType))
      case EitherLeft(e) =>
        EitherLeft(e)
      case EitherRight(e) =>
        EitherRight(e)
      case Exists(bindings, triggers, body) =>
        Exists(bindings, triggers, bool(body))
      case Exp(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Exp(int(left), int(right)),
          Exp(rat(left), rat(right)),
        )
      case div @ FloorDiv(left, right) =>
        FloorDiv(int(left), int(right))(div.blame)
      case Forall(bindings, triggers, body) =>
        Forall(bindings, triggers, bool(body))
      case inv @ FunctionInvocation(ref, args, typeArgs, givenMap, yields) =>
        FunctionInvocation(ref, coerceArgs(args, ref.decl, typeArgs), typeArgs, coerceGiven(givenMap), coerceYields(yields, inv))(inv.blame)
      case get @ GetLeft(e) =>
        GetLeft(either(e)._1)(get.blame)
      case get @ GetRight(e) =>
        GetRight(either(e)._1)(get.blame)
      case GlobalThreadId() =>
        GlobalThreadId()
      case GpgpuCudaKernelInvocation(kernel, blocks, threads, args, givenArgs, yields) =>
        GpgpuCudaKernelInvocation(kernel, int(blocks), int(threads), args, givenArgs, yields)
      case Greater(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Greater(int(left), int(right)),
          Greater(rat(left), rat(right)),
        )
      case GreaterEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          GreaterEq(int(left), int(right)),
          GreaterEq(rat(left), rat(right)),
        )
      case head @ Head(xs) =>
        Head(seq(xs)._1)(head.blame)
      case Held(obj) =>
        Held(cls(obj)._1)
      case IdleToken(thread) =>
        IdleToken(cls(thread)._1)
      case Implies(left, right) =>
        Implies(bool(left), res(right))
      case FunctionOf(e, ref) =>
        FunctionOf(e, ref)
      case IndeterminateInteger(min, max) =>
        IndeterminateInteger(int(min), int(max))
      case InlinePattern(inner, parent, group) =>
        InlinePattern(inner, parent, group)
      case inv @ InstanceFunctionInvocation(obj, ref, args, typeArgs, givenMap, yields) =>
        InstanceFunctionInvocation(cls(obj)._1, ref, coerceArgs(args, ref.decl, typeArgs), typeArgs, coerceGiven(givenMap), coerceYields(yields, inv))(inv.blame)
      case InstanceOf(value, typeValue) =>
        InstanceOf(value, typeValue)
      case InstancePredicateApply(obj, ref, args, perm) =>
        InstancePredicateApply(cls(obj)._1, ref, coerceArgs(args, ref.decl), rat(perm))
      case CoalesceInstancePredicateApply(obj, ref, args, perm) =>
        CoalesceInstancePredicateApply(cls(obj)._1, ref, coerceArgs(args, ref.decl), rat(perm))
      case IsLeft(e) =>
        IsLeft(either(e)._1)
      case IsRight(e) =>
        IsRight(either(e)._1)
      case lit @ JavaClassLiteral(t) => lit
      case deref @ JavaDeref(obj, field) => e
      case inv @ JavaInvocation(obj, typeParams, method, arguments, givenArgs, yields) => e
      case JavaLiteralArray(exprs) =>
        JavaLiteralArray(exprs)
      case JavaLocal(name) => e
      case JavaNewClass(args, typeArgs, name, givenMap, yields) => e
      case JavaNewDefaultArray(baseType, specifiedDims, moreDims) => e
      case JavaNewLiteralArray(baseType, dims, initializer) => e
      case str @ JavaStringLiteral(_) => str
      case str @ StringLiteral(_) => str
      case str @ InternedString(_, _) => str
      case JoinToken(thread) =>
        JoinToken(cls(thread)._1)
      case length @ Length(arr) =>
        Length(array(arr)._1)(length.blame)
      case Less(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Less(int(left), int(right)),
          Less(rat(left), rat(right)),
        )
      case LessEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          LessEq(int(left), int(right)),
          LessEq(rat(left), rat(right)),
        )
      case Let(binding, value, main) =>
        Let(binding, coerce(value, binding.t), main)
      case LiteralBag(element, values) =>
        LiteralBag(element, values.map(coerce(_, element)))
      case LiteralMap(k, v, values) =>
        LiteralMap(k, v, values.map {
          case (valueK, valueV) => (coerce(valueK, k), coerce(valueV, v))
        })
      case LiteralSeq(element, values) =>
        LiteralSeq(element, values.map(coerce(_, element)))
      case LiteralSet(element, values) =>
        LiteralSet(element, values.map(coerce(_, element)))
      case LiteralTuple(ts, values) =>
        LiteralTuple(ts, values.zip(ts).map {
          case (v, t) => coerce(v, t)
        })
      case Local(ref) =>
        Local(ref)
      case LocalThreadId() =>
        LocalThreadId()
      case MapCons(m, k, v) =>
        val (coercedMap, mapType) = map(m)
        val sharedType = Types.leastCommonSuperType(mapType.value, v.t)
        MapCons(coerce(coercedMap, TMap(mapType.key, sharedType)), coerce(k, mapType.key), coerce(v, sharedType))
      case MapDisjoint(left, right) =>
        val (coercedLeft, leftType) = map(left)
        val (coercedRight, rightType) = map(right)

        if(leftType.key != rightType.key)
          throw IncoercibleText(e, s"Expected both operands to have a map type of which the key type is equal, " +
            s"but got ${leftType.key} and ${rightType.key}")

        val sharedType = Types.leastCommonSuperType(leftType.value, rightType.value)
        val mapType = TMap(leftType.key, sharedType)
        MapDisjoint(coerce(coercedLeft, mapType), coerce(coercedRight, mapType))
      case MapEq(left, right) =>
        val (coercedLeft, leftType) = map(left)
        val (coercedRight, rightType) = map(right)

        if(leftType.key != rightType.key)
          throw IncoercibleText(e, s"Expected both operands to have a map type of which the key type is equal, " +
            s"but got ${leftType.key} and ${rightType.key}")

        val sharedType = Types.leastCommonSuperType(leftType.value, rightType.value)
        val mapType = TMap(leftType.key, sharedType)
        MapEq(coerce(coercedLeft, mapType), coerce(coercedRight, mapType))
      case get @ MapGet(m, k) =>
        val (coercedMap, mapType) = map(m)
        MapGet(coercedMap, coerce(k, mapType.key))(get.blame)
      case MapItemSet(m) =>
        MapItemSet(map(m)._1)
      case MapKeySet(m) =>
        MapKeySet(map(m)._1)
      case MapMember(x, xs) =>
        val (coercedMap, mapType) = map(xs)
        MapMember(coerce(x, mapType.key), coercedMap)
      case MapRemove(m, k) =>
        val (coercedMap, mapType) = map(m)
        MapRemove(coercedMap, coerce(k, mapType.key))
      case MapValueSet(m) =>
        MapValueSet(map(m)._1)
      case MatrixCompare(left, right) =>
        val (coercedLeft, leftType) = matrix(left)
        val (coercedRight, rightType) = matrix(right)
        val sharedType = Types.leastCommonSuperType(leftType.element, rightType.element)
        MatrixCompare(coerce(coercedLeft, TMatrix(sharedType)), coerce(coercedRight, TMatrix(sharedType)))
      case MatrixRepeat(e) =>
        MatrixRepeat(e)
      case MatrixSum(indices, mat) =>
        MatrixSum(coerce(indices, TSeq[Pre](TInt())), coerce(mat, TSeq[Pre](TRational())))
      case inv @ MethodInvocation(obj, ref, args, outArgs, typeArgs, givenMap, yields) =>
        MethodInvocation(cls(obj)._1, ref, coerceArgs(args, ref.decl, typeArgs), outArgs, typeArgs, coerceGiven(givenMap), coerceYields(yields, inv))(inv.blame)
      case Minus(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Minus(int(left), int(right)),
          Minus(rat(left), rat(right)),
        )
      case div @ Mod(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Mod(int(left), int(right))(div.blame),
          Mod(rat(left), rat(right))(div.blame),
        )
      case ModelAbstractState(m, state) =>
        ModelAbstractState(model(m)._1, bool(state))
      case ModelChoose(m, perm, totalProcess, choice) =>
        ModelChoose(model(m)._1, rat(perm), process(totalProcess), process(choice))
      case ModelCreate(m, init) =>
        ModelCreate(model(m)._1, process(init))
      case deref @ ModelDeref(obj, ref) =>
        ModelDeref(model(obj)._1, ref)(deref.blame)
      case ModelDestroy(m) =>
        ModelDestroy(model(m)._1)
      case ModelMerge(m, leftPerm, leftProcess, rightPerm, rightProcess) =>
        ModelMerge(model(m)._1, rat(leftPerm), process(leftProcess), rat(rightPerm), process(rightProcess))
      case ModelNew(ref) =>
        ModelNew(ref)
      case ModelPerm(loc, perm) =>
        ModelPerm(loc, rat(perm))
      case ModelSplit(m, leftPerm, leftProcess, rightPerm, rightProcess) =>
        ModelSplit(model(m)._1, rat(leftPerm), process(leftProcess), rat(rightPerm), process(rightProcess))
      case ModelState(m, perm, state) =>
        ModelState(model(m)._1, rat(perm), process(state))
      case Mult(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Mult(int(left), int(right)),
          Mult(rat(left), rat(right)),
        )
      case Neq(left, right) =>
        val sharedType = Types.leastCommonSuperType(left.t, right.t)
        Neq(coerce(left, sharedType), coerce(right, sharedType))
      case NewArray(element, dims, moreDims) =>
        NewArray(element, dims.map(int), moreDims)
      case NewObject(cls) =>
        NewObject(cls)
      case NoPerm() =>
        NoPerm()
      case Not(arg) =>
        Not(bool(arg))
      case Null() =>
        Null()
      case old @ Old(expr, at) =>
        Old(expr, at)(old.blame)
      case get @ OptGet(opt) =>
        OptGet(option(opt)._1)(get.blame)
      case OptGetOrElse(opt, alt) =>
        val (coercedOpt, optType) = option(opt)
        val sharedType = Types.leastCommonSuperType(alt.t, optType.element)
        OptGetOrElse(coerce(coercedOpt, TOption(sharedType)), coerce(alt, sharedType))
      case OptNone() =>
        OptNone()
      case OptSome(e) =>
        OptSome(e)
      case Or(left, right) =>
        Or(bool(left), bool(right))
      case Perm(loc, perm) =>
        Perm(loc, rat(perm))
      case PermPointer(p, len, perm) =>
        PermPointer(pointer(p)._1, int(len), rat(perm))
      case PermPointerIndex(p, idx, perm) =>
        PermPointerIndex(pointer(p)._1, int(idx), rat(perm))
      case Permutation(left, right) =>
        val (coercedLeft, leftType) = seq(left)
        val (coercedRight, rightType) = seq(right)
        val sharedType = Types.leastCommonSuperType(leftType.element, rightType.element)
        Permutation(coerce(left, TSeq(sharedType)), coerce(right, TSeq(sharedType)))
      case Plus(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, but got ${left.t} and ${right.t}.",
          Plus(int(left), int(right)),
          Plus(rat(left), rat(right)),
        )
      case add @ PointerAdd(p, offset) =>
        PointerAdd(pointer(p)._1, int(offset))(add.blame)
      case len @ PointerBlockLength(p) =>
        PointerBlockLength(pointer(p)._1)(len.blame)
      case off @ PointerBlockOffset(p) =>
        PointerBlockOffset(pointer(p)._1)(off.blame)
      case len @ PointerLength(p) =>
        PointerLength(pointer(p)._1)(len.blame)
      case get @ PointerSubscript(p, index) =>
        PointerSubscript(pointer(p)._1, int(index))(get.blame)
      case PointsTo(loc, perm, value) =>
        PointsTo(loc, rat(perm), coerce(value, loc.t))
      case ass @ PostAssignExpression(target, value) =>
        PostAssignExpression(target, coerce(value, target.t))(ass.blame)
      case ass @ PreAssignExpression(target, value) =>
        PreAssignExpression(target, coerce(value, target.t))(ass.blame)
      case PredicateApply(ref, args, perm) =>
        PredicateApply(ref, coerceArgs(args, ref.decl), rat(perm))
      case inv @ ProcedureInvocation(ref, args, outArgs, typeArgs, givenMap, yields) =>
        ProcedureInvocation(ref, coerceArgs(args, ref.decl, typeArgs), outArgs, typeArgs, coerceGiven(givenMap), coerceYields(yields, inv))(inv.blame)
      case ProcessApply(process, args) =>
        ProcessApply(process, coerceArgs(args, process.decl))
      case ProcessChoice(left, right) =>
        ProcessChoice(process(left), process(right))
      case ProcessPar(left, right) =>
        ProcessPar(process(left), process(right))
      case ProcessSelect(cond, whenTrue, whenFalse) =>
        ProcessSelect(bool(cond), process(whenTrue), process(whenFalse))
      case ProcessSeq(left, right) =>
        ProcessSeq(process(left), process(right))
      case Product(bindings, condition, main) =>
        Product(bindings, bool(condition), int(main))
      case PVLDeref(obj, field) => e
      case PVLInvocation(obj, method, args, typeArgs, givenArgs, yields) => e
      case PVLLocal(name) => e
      case PVLNew(t, args, givenMap, yields) => e
      case Range(from, to) =>
        Range(int(from), int(to))
      case ReadPerm() =>
        ReadPerm()
      case RemoveAt(xs, i) =>
        RemoveAt(seq(xs)._1, int(i))
      case Result(ref) =>
        Result(ref)
      case s @ Scale(scale, r) =>
        Scale(rat(scale), res(r))(s.blame)
      case ScaleByParBlock(ref, r) =>
        ScaleByParBlock(ref, res(r))
      case ScopedExpr(locals, body) =>
        ScopedExpr(locals, body)
      case Select(condition, whenTrue, whenFalse) =>
        val sharedType = Types.leastCommonSuperType(whenTrue.t, whenFalse.t)
        Select(bool(condition), coerce(whenTrue, sharedType), coerce(whenFalse, sharedType))
      case SeqMember(x, xs) =>
        val (coercedSeq, seqType) = seq(xs)
        val sharedType = Types.leastCommonSuperType(x.t, seqType.element)
        SeqMember(coerce(x, sharedType), coerce(coercedSeq, TSeq(sharedType)))
      case get @ SeqSubscript(xs, index) =>
        SeqSubscript(seq(xs)._1, int(index))(get.blame)
      case update @ SeqUpdate(xs, i, x) =>
        val (coercedSeq, seqType) = seq(xs)
        val sharedType = Types.leastCommonSuperType(x.t, seqType.element)
        SeqUpdate(coerce(coercedSeq, TSeq(sharedType)), int(i), coerce(x, sharedType))
      case SetIntersection(xs, ys) =>
        val (left, TSet(leftT)) = set(xs)
        val (right, TSet(rightT)) = set(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        SetIntersection(coerce(left, TSet(sharedElement)), coerce(right, TSet(sharedElement)))
      case SetMember(x, xs) =>
        val (coercedSet, setType) = set(xs)
        val sharedType = Types.leastCommonSuperType(x.t, setType.element)
        SetMember(coerce(x, sharedType), coerce(coercedSet, TSet(sharedType)))
      case SetMinus(xs, ys) =>
        val (left, TSet(leftT)) = set(xs)
        val (right, TSet(rightT)) = set(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        SetMinus(coerce(left, TSet(sharedElement)), coerce(right, TSet(sharedElement)))
      case SetUnion(xs, ys) =>
        val (left, TSet(leftT)) = set(xs)
        val (right, TSet(rightT)) = set(ys)
        val sharedElement = Types.leastCommonSuperType(leftT, rightT)
        SetUnion(coerce(left, TSet(sharedElement)), coerce(right, TSet(sharedElement)))
      case SharedMemSize(xs) =>
        firstOk(e, s"Expected operand to be a pointer or array, but got ${xs.t}.",
          SharedMemSize(array(xs)._1),
          SharedMemSize(pointer(xs)._1),
        )
      case SilverBagSize(xs) =>
        SilverBagSize(bag(xs)._1)
      case SilverCurFieldPerm(obj, field) =>
        SilverCurFieldPerm(ref(obj), field)
      case SilverCurPredPerm(ref, args) =>
        SilverCurPredPerm(ref, coerceArgs(args, ref.decl))
      case deref @ SilverDeref(obj, field) =>
        SilverDeref(ref(obj), field)(deref.blame)
      case SilverIntToRat(perm) =>
        SilverIntToRat(int(perm))
      case SilverMapSize(xs) =>
        SilverMapSize(map(xs)._1)
      case SilverNull() =>
        SilverNull()
      case SilverPartialADTFunctionInvocation(name, args, partialTypeArgs) => e
      case SilverSetSize(xs) =>
        SilverSetSize(set(xs)._1)
      case SilverSeqSize(xs) =>
        SilverSeqSize(seq(xs)._1)
      case SilverUntypedNonemptyLiteralMap(values) =>
        SilverUntypedNonemptyLiteralMap(values)
      case Size(obj) =>
        Size(sized(obj)._1)
      case Slice(xs, from, to) =>
        Slice(seq(xs)._1, int(from), int(to))
      case Star(left, right) =>
        Star(res(left), res(right))
      case starall @ Starall(bindings, triggers, body) =>
        Starall(bindings, triggers, res(body))(starall.blame)
      case SubBag(left, right) =>
        val (coercedLeft, leftBag) = bag(left)
        val (coercedRight, rightBag) = bag(right)
        val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
        SubBag(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
      case SubBagEq(left, right) =>
        val (coercedLeft, leftBag) = bag(left)
        val (coercedRight, rightBag) = bag(right)
        val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
        SubBagEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
      case SubSet(left, right) =>
        val (coercedLeft, leftSet) = set(left)
        val (coercedRight, rightSet) = set(right)
        val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
        SubSet(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
      case SubSetEq(left, right) =>
        val (coercedLeft, leftSet) = set(left)
        val (coercedRight, rightSet) = set(right)
        val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
        SubSetEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
      case SubType(left, right) =>
        SubType(left, right)
      case Sum(bindings, condition, main) =>
        Sum(bindings, bool(condition), int(main))
      case SuperType(left, right) =>
        SuperType(left, right)
      case Tail(xs) =>
        Tail(seq(xs)._1)
      case Take(xs, count) =>
        Take(seq(xs)._1, int(count))
      case Then(value, post) =>
        Then(value, post)
      case ThisModel(ref) =>
        ThisModel(ref)
      case ThisObject(ref) =>
        ThisObject(ref)
      case TupGet(tup, index) =>
        TupGet(tuple(tup)._1, index)
      case TypeOf(expr) =>
        TypeOf(expr)
      case TypeValue(value) =>
        TypeValue(value)
      case UMinus(arg) =>
        firstOk(e, s"Expected operand to be numeric, but got ${arg.t}.",
          UMinus(int(arg)),
          UMinus(rat(arg)),
        )
      case Unfolding(pred, body) =>
        Unfolding(res(pred), body)
      case UntypedLiteralBag(values) =>
        val sharedType = Types.leastCommonSuperType(values.map(_.t))
        UntypedLiteralBag(values.map(coerce(_, sharedType)))
      case UntypedLiteralSeq(values) =>
        val sharedType = Types.leastCommonSuperType(values.map(_.t))
        UntypedLiteralSeq(values.map(coerce(_, sharedType)))
      case UntypedLiteralSet(values) =>
        val sharedType = Types.leastCommonSuperType(values.map(_.t))
        UntypedLiteralSet(values.map(coerce(_, sharedType)))
      case ValidArray(arr, len) =>
        ValidArray(array(arr)._1, int(len))
      case ValidMatrix(mat, w, h) =>
        ValidMatrix(arrayMatrix(mat)._1, int(w), int(h))
      case value: BooleanValue[Pre] => e
      case value: IntegerValue[Pre] => e
      case value: FloatValue[Pre] => e
      case value @ Value(loc) =>
        Value(loc)
      case values @ Values(arr, from, to) =>
        Values(array(arr)._1, int(from), int(to))(values.blame)
      case VectorCompare(left, right) =>
        val (coercedLeft, leftType) = seq(left)
        val (coercedRight, rightType) = seq(right)
        val sharedType = Types.leastCommonSuperType(leftType.element, rightType.element)
        val seqType = TSeq(sharedType)
        VectorCompare(coerce(coercedLeft, seqType), coerce(coercedRight, seqType))
      case VectorRepeat(e) =>
        VectorRepeat(e)
      case VectorSum(indices, vec) =>
        VectorSum(coerce(indices, TSeq[Pre](TInt())), coerce(vec, TSeq[Pre](TRational())))
      case Void() =>
        Void()
      case Wand(left, right) =>
        Wand(res(left), res(right))
      case With(pre, value) =>
        With(pre, value)
      case WritePerm() =>
        WritePerm()
      case localIncoming: BipLocalIncomingData[Pre] => localIncoming
    }
  }

  def coerce(stat: Statement[Pre]): Statement[Pre] = {
    implicit val o: Origin = stat.o
    stat match {
      case a @ Assert(assn) => Assert(res(assn))(a.blame)
      case a @ Assign(target, value) =>
        try { Assign(target, coerce(value, target.t))(a.blame) } catch {
          case err: Incoercible =>
            println(err.text)
            throw err
        }
      case Assume(assn) => Assume(bool(assn))
      case Block(statements) => Block(statements)
      case Branch(branches) => Branch(branches.map { case (cond, effect) => (bool(cond), effect) })
      case Break(label) => Break(label)
      case Case(pattern) => Case(pattern)
      case CDeclarationStatement(decl) => CDeclarationStatement(decl)
      case CGoto(label) => CGoto(label)
      case c @ Commit(obj) => Commit(cls(obj)._1)(c.blame)
      case Continue(label) => Continue(label)
      case DefaultCase() => DefaultCase()
      case Eval(expr) => Eval(expr)
      case e @ Exhale(assn) => Exhale(res(assn))(e.blame)
      case f @ Fold(assn) => Fold(res(assn))(f.blame)
      case f @ Fork(obj) => Fork(cls(obj)._1)(f.blame)
      case proof @ FramedProof(pre, body, post) => FramedProof(res(pre), body, res(post))(proof.blame)
      case Goto(lbl) => Goto(lbl)
      case GpgpuAtomic(impl, before, after) => GpgpuAtomic(impl, before, after)
      case b @ GpgpuBarrier(requires, ensures, specifier) => GpgpuBarrier(res(requires), res(ensures), specifier)(b.blame)
      case Havoc(loc) => Havoc(loc)
      case IndetBranch(branches) => IndetBranch(branches)
      case Inhale(assn) => Inhale(res(assn))
      case inv @ InvokeProcedure(ref, args, outArgs, typeArgs, givenMap, yields) =>
        InvokeProcedure(ref, coerceArgs(args, ref.decl, typeArgs), outArgs, typeArgs, coerceGiven(givenMap), coerceYields(yields, args.head))(inv.blame)
      case inv @ InvokeMethod(obj, ref, args, outArgs, typeArgs, givenMap, yields) =>
        InvokeMethod(cls(obj)._1, ref, coerceArgs(args, ref.decl, typeArgs), outArgs, typeArgs, coerceGiven(givenMap), coerceYields(yields, args.head))(inv.blame)
      case JavaLocalDeclarationStatement(decl) => JavaLocalDeclarationStatement(decl)
      case j @ Join(obj) => Join(cls(obj)._1)(j.blame)
      case Label(decl, stat) => Label(decl, stat)
      case LocalDecl(local) => LocalDecl(local)
      case Lock(obj) => Lock(cls(obj)._1)
      case Loop(init, cond, update, contract, body) => Loop(init, bool(cond), update, contract, body)
      case ModelDo(model, perm, after, action, impl) => ModelDo(model, rat(perm), after, action, impl)
      case n @ Notify(obj) => Notify(cls(obj)._1)(n.blame)
      case at @ ParAtomic(inv, content) => ParAtomic(inv, content)(at.blame)
      case bar @ ParBarrier(block, invs, requires, ensures, content) => ParBarrier(block, invs, res(requires), res(ensures), content)(bar.blame)
      case p @ ParInvariant(decl, inv, content) => ParInvariant(decl, res(inv), content)(p.blame)
      case ParStatement(impl) => ParStatement(impl)
      case Recv(ref) => Recv(ref)
      case r @ Refute(assn) => Refute(res(assn))(r.blame)
      case Return(result) => Return(result) // TODO coerce return, make AmbiguousReturn?
      case Scope(locals, body) => Scope(locals, body)
      case send @ Send(decl, offset, resource) => Send(decl, offset, res(resource))(send.blame)
      case ass @ SilverFieldAssign(obj, field, value) => SilverFieldAssign(ref(obj), field, coerce(value, field.decl.t))(ass.blame)
      case SilverLocalAssign(v, value) => SilverLocalAssign(v, coerce(value, v.decl.t))
      case SilverNewRef(v, fields) => SilverNewRef(v, fields)
      case SpecIgnoreEnd() => SpecIgnoreEnd()
      case SpecIgnoreStart() => SpecIgnoreStart()
      case Switch(expr, body) => Switch(expr, body)
      case s @ Synchronized(obj, body) => Synchronized(cls(obj)._1, body)(s.blame)
      case t @ Throw(obj) => Throw(cls(obj)._1)(t.blame)
      case TryCatchFinally(body, after, catches) => TryCatchFinally(body, after, catches)
      case u @ Unfold(assn) => Unfold(res(assn))(u.blame)
      case u @ Unlock(obj) => Unlock(cls(obj)._1)(u.blame)
      case VecBlock(iters, requires, ensures, content) => VecBlock(iters, res(requires), res(ensures), content)
      case w @ Wait(obj) => Wait(cls(obj)._1)(w.blame)
      case w @ WandApply(assn) => WandApply(res(assn))(w.blame)
      case w @ WandPackage(expr, stat) => WandPackage(res(expr), stat)(w.blame)
    }
  }

  def coerce(decl: Declaration[Pre]): Declaration[Pre] = {
    implicit val o: Origin = decl.o
    decl match {
      case unit: CTranslationUnit[Pre] =>
        new CTranslationUnit(unit.declarations)
      case rule: SimplificationRule[Pre] =>
        new SimplificationRule[Pre](bool(rule.axiom))
      case dataType: AxiomaticDataType[Pre] =>
        dataType
      case clazz: Class[Pre] =>
        new Class[Pre](clazz.declarations, clazz.supports, res(clazz.intrinsicLockInvariant))
      case enum: Enum[Pre] =>
        enum
      case enumConstant: EnumConstant[Pre] =>
        enumConstant
      case model: Model[Pre] =>
        model
      case function: Function[Pre] =>
        new Function[Pre](function.returnType, function.args, function.typeArgs, function.body.map(coerce(_, function.returnType)), function.contract, function.inline, function.threadLocal)(function.blame)
      case procedure: Procedure[Pre] =>
        procedure
      case predicate: Predicate[Pre] =>
        new Predicate[Pre](predicate.args, predicate.body.map(res), predicate.threadLocal, predicate.inline)
      case definition: CFunctionDefinition[Pre] =>
        definition
      case declaration: CGlobalDeclaration[Pre] =>
        declaration
      case namespace: JavaNamespace[Pre] =>
        namespace
      case clazz: JavaClass[Pre] =>
        new JavaClass[Pre](clazz.name, clazz.modifiers, clazz.typeParams, res(clazz.intrinsicLockInvariant), clazz.ext, clazz.imp, clazz.decls)
      case interface: JavaInterface[Pre] =>
        interface
      case interface: JavaAnnotationInterface[Pre] =>
        interface
      case field: SilverField[Pre] =>
        field
      case function: InstanceFunction[Pre] =>
        new InstanceFunction[Pre](function.returnType, function.args, function.typeArgs, function.body.map(coerce(_, function.returnType)), function.contract, function.inline, function.threadLocal)(function.blame)
      case method: InstanceMethod[Pre] =>
        method
      case predicate: InstancePredicate[Pre] =>
        new InstancePredicate[Pre](predicate.args, predicate.body.map(res), predicate.threadLocal, predicate.inline)
      case field: InstanceField[Pre] =>
        field
      case method: RunMethod[Pre] =>
        method
      case initialization: JavaSharedInitialization[Pre] =>
        initialization
      case fields: JavaFields[Pre] =>
        new JavaFields[Pre](fields.modifiers, fields.t, fields.decls.map {
          case JavaVariableDeclaration(name, dims, None) => JavaVariableDeclaration(name, dims, None)
          case JavaVariableDeclaration(name, dims, Some(v)) =>
            JavaVariableDeclaration(name, dims, Some(coerce(v, FuncTools.repeat[Type[Pre]](TArray(_), dims, fields.t))))
        })
      case constructor: JavaConstructor[Pre] =>
        constructor
      case method: JavaMethod[Pre] =>
        method
      case param: JavaParam[Pre] =>
        param
      case method: JavaAnnotationMethod[Pre] =>
        method
      case constructor: PVLConstructor[Pre] =>
        constructor
      case field: ModelField[Pre] =>
        field
      case proc: ModelProcess[Pre] =>
        new ModelProcess[Pre](proc.args, process(proc.impl), bool(proc.requires), bool(proc.ensures), proc.modifies, proc.accessible)(proc.blame)
      case action: ModelAction[Pre] =>
        new ModelAction[Pre](action.args, bool(action.requires), bool(action.ensures), action.modifies, action.accessible)
      case axiom: ADTAxiom[Pre] =>
        new ADTAxiom[Pre](bool(axiom.axiom))
      case function: ADTFunction[Pre] =>
        function
      case variable: Variable[Pre] =>
        variable
      case decl: LabelDecl[Pre] =>
        decl
      case decl: SendDecl[Pre] =>
        decl
      case decl: ParBlockDecl[Pre] =>
        decl
      case decl: ParInvariantDecl[Pre] =>
        decl
      case param: CParam[Pre] =>
        param
      case decl: CLocalDeclaration[Pre] =>
        decl
      case declaration: JavaLocalDeclaration[Pre] =>
        new JavaLocalDeclaration[Pre](declaration.modifiers, declaration.t, declaration.decls.map {
          case JavaVariableDeclaration(name, dims, None) => JavaVariableDeclaration(name, dims, None)
          case JavaVariableDeclaration(name, dims, Some(v)) =>
            JavaVariableDeclaration(name, dims, Some(coerce(v, FuncTools.repeat[Type[Pre]](TArray(_), dims, declaration.t))))
        })
      case bc: BipComponent[Pre] =>
        new BipComponent(bc.constructors, res(bc.invariant), bc.initial)
      case bsp: BipStatePredicate[Pre] =>
        new BipStatePredicate(bool(bsp.expr))
      case trans: BipTransition[Pre] =>
        new BipTransition(trans.port, trans.source, trans.target, trans.data, trans.guard,
          bool(trans.requires),
          bool(trans.ensures),
          trans.body
        )(trans.blame)
      case data: BipIncomingData[Pre] => data
      case data: BipOutgoingData[Pre] => data
      case data: BipData[Pre] => data
      case guard: BipGuard[Pre] => guard
      case port: BipPort[Pre] => port
    }
  }

  def coerce(region: ParRegion[Pre]): ParRegion[Pre] = {
    implicit val o: Origin = region.o
    region match {
      case region @ ParParallel(regions) => ParParallel(regions)(region.blame)
      case region @ ParSequential(regions) => ParSequential(regions)(region.blame)
      case region @ ParBlock(decl, iters, context_everywhere, requires, ensures, content) =>
        ParBlock(decl, iters, res(context_everywhere), res(requires), res(ensures), content)(region.blame)
    }
  }
}
