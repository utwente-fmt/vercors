package vct.col.coerce

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.{Generation, NonLatchingRewriter, Rewriter}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Types
import vct.result.VerificationResult.SystemError

import scala.collection.mutable.ArrayBuffer

case class NopCoercingRewriter[Pre <: Generation]() extends CoercingRewriter[Pre]() {
  globalScopes.push(ArrayBuffer())

  override def coerce(e: Expr[Pre], coercion: Coercion[Pre])(implicit o: Origin): Expr[Pre] = e
}

case object CoercingRewriter {
  sealed trait CoercionError extends SystemError {
    override def text: String =
      "Internal type error: CoercionErrors must not bubble."
  }

  case object IncoercibleDummy extends CoercionError
  case class Incoercible(e: Expr[_], target: Type[_]) extends CoercionError
  case class IncoercibleText(e: Expr[_], message: String) extends CoercionError

  case object CoercionOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def messageInContext(message: String): String =
      s"At node generated for a coercion: $message"
  }
}

abstract class CoercingRewriter[Pre <: Generation]() extends Rewriter[Pre] {
  import CoercingRewriter._

  /**
    * Apply a particular coercion to an expression.
    * SAFETY: all promoting coercions must be injective; otherwise the default mapping coercion of sets is unsound.
    * @param e the expression to coerce
    * @param coercion the coercion
    * @param sc the scope in which to declare functions
    * @return the coerced expression
    */
  def coerce(e: Expr[Pre], coercion: Coercion[Pre])(implicit o: Origin): Expr[Pre] = {
    coercion match {
      case Coercion.Identity() => e
      case Coercion.Compose(left, right) => coerce(coerce(e, right), left)
      case Coercion.NothingSomething(_) => e
      case Coercion.SomethingAny(_) => e
      case Coercion.MapOption(_, _, inner) =>
        Select(Eq(e, OptNone()), OptNone(), coerce(OptGet(e)(NeverNone), inner))
      case Coercion.MapEither(source, target, innerLeft, innerRight) =>
        Select(IsLeft(e), coerce(GetLeft(e)(FramedGetLeft), innerLeft), coerce(GetRight(e)(FramedGetRight), innerRight))
      case Coercion.MapSeq(source, target, inner) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable[Post](dispatch(source))
          val i = new Variable[Post](TInt())
          val result_i = SeqSubscript(result, i.get)(FramedSeqIndex)

          val pre_v = Local(transmutePostRef[Variable, Variable[Pre], Variable[Post]](v.ref))
          val pre_i = Local(transmutePostRef[Variable, Variable[Pre], Variable[Post]](v.ref))
          val pre_v_i = SeqSubscript(pre_v, pre_i)(FramedSeqIndex)

          function(
            blame = AbstractApplicable,
            returnType = dispatch(target),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(Size(v.get), Size(result)) &&
              Forall(Seq(i), Seq(Seq(result_i)),
                (const[Post](0) < i.get && i.get < Size(result)) ==>
                  result_i === dispatch(coerce(pre_v_i, inner)))
            ),
          )
        })

        f.declareDefault(this)
        FunctionInvocation(transmutePostRef[Function, Function[Pre], Function[Post]](f.ref), Seq(e), Nil)(PanicBlame("default coercion for seq<_> requires nothing."))
      case Coercion.MapSet(source, target, inner) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable(dispatch(source))
          val elem = new Variable(dispatch(source.element))
          val pre_elem = Local(transmutePostRef[Variable, Variable[Pre], Variable[Post]](elem.ref))

          function(
            blame = AbstractApplicable,
            returnType = dispatch(target),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(Size(result), Size(v.get)) &&
                Forall(Seq(elem), Seq(Seq(SetMember(elem.get, result))),
                  Eq(SetMember(dispatch(coerce(pre_elem, inner)), result), SetMember(elem.get, v.get)))
            ),
          )
        })

        f.declareDefault(this)
        FunctionInvocation(transmutePostRef[Function, Function[Pre], Function[Post]](f.ref), Seq(e), Nil)(PanicBlame("Default coercion for set<_> requires nothing."))
      case Coercion.MapBag(source, target, inner) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable(dispatch(source))
          val elem = new Variable(dispatch(source.element))
          val pre_elem = Local(transmutePostRef[Variable, Variable[Pre], Variable[Post]](elem.ref))

          function(
            blame = AbstractApplicable,
            returnType = dispatch(target),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(Size(result), Size(v.get)) &&
                Forall(Seq(elem), Seq(Seq(BagMemberCount(elem.get, result))),
                  Eq(BagMemberCount(dispatch(coerce(pre_elem, inner)), result), BagMemberCount(elem.get, v.get)))
            ),
          )
        })

        f.declareDefault(this)
        FunctionInvocation(transmutePostRef[Function, Function[Pre], Function[Post]](f.ref), Seq(e), Nil)(PanicBlame("Default coercion for bag<_> requires nothing."))
      case Coercion.MapMatrix(source, target, inner) =>
        ???
      case Coercion.MapMap(source, target, inner) =>
        val f: Function[Post] = withResult((result: Result[Post]) => {
          val v = new Variable(dispatch(source))
          val k = new Variable(dispatch(source.key))
          val pre_v = Local(transmutePostRef[Variable, Variable[Pre], Variable[Post]](v.ref))
          val pre_k = Local(transmutePostRef[Variable, Variable[Pre], Variable[Post]](k.ref))

          function(
            blame = AbstractApplicable,
            returnType = dispatch(target),
            args = Seq(v),
            ensures = UnitAccountedPredicate(
              Eq(MapKeySet(result), MapKeySet(v.get)) &&
                Forall(Seq(k), Seq(Seq(MapGet(result, k.get)(TriggerPatternBlame))),
                  SetMember(k.get, MapKeySet(result)) ==> Eq(MapGet(result, k.get)(FramedMapGet), dispatch(MapGet(pre_v, pre_k)(FramedMapGet))))
            ),
          )
        })

        f.declareDefault(this)
        FunctionInvocation(transmutePostRef[Function, Function[Pre], Function[Post]](f.ref), Seq(e), Nil)(PanicBlame("Default coercion for map<_, _> requires nothing."))
      case Coercion.MapTuple(source, target, left, right) =>
        LiteralTuple(target.elements, Seq(coerce(TupGet(e, 0), left), coerce(TupGet(e, 1), right)))
      case Coercion.MapType(source, target, inner) =>
        ???

      case Coercion.BoolResource() => e
      case Coercion.BoundIntFrac() => e
      case Coercion.BoundIntZFrac(_) => e
      case Coercion.JoinUnion(_, _, inner) => e
      case Coercion.SelectUnion(_, _, _, inner) => coerce(e, inner)

      case Coercion.Supports(_, _) => e
      case Coercion.JavaSupports(_, _) => e
      case Coercion.CPrimitiveToCol(_, _) => e
      case Coercion.ColToCPrimitive(_, _) => e
      case Coercion.NullRef() => e
      case Coercion.NullArray(_) => e
      case Coercion.NullClass(_) => e
      case Coercion.NullJavaClass(_) => e
      case Coercion.NullPointer(_) => e
      case Coercion.FracZFrac() => e
      case Coercion.ZFracRat() => e
      case Coercion.FloatRat(_) => e
      case Coercion.WidenBound(_, _) => e
      case Coercion.UnboundInt(_) => e

      case Coercion.IntRat() => e
      case Coercion.RatZFrac() => e
      case Coercion.ZFracFrac() => e
    }
  }

  def coerceAny(node: NodeFamily[Pre]): NodeFamily[Pre] = node match {
    case node: Program[Pre] => node
    case node: Statement[Pre] => node
    case node: Expr[Pre] => coerce(node)
    case node: Type[Pre] => node
    case node: AccountedPredicate[Pre] => node
    case node: ApplicableContract[Pre] => node
    case node: LoopContract[Pre] => node
    case node: ParRegion[Pre] => node
    case node: CatchClause[Pre] => node
    case node: SignalsClause[Pre] => node
    case node: FieldFlag[Pre] => node
    case node: IterVariable[Pre] => node
    case node: SilverPredicateAccess[Pre] => node
    case node: CDeclarator[Pre] => node
    case node: CDeclarationSpecifier[Pre] => node
    case node: CTypeQualifier[Pre] => node
    case node: CPointer[Pre] => node
    case node: CInit[Pre] => node
    case node: JavaModifier[Pre] => node
    case node: JavaImport[Pre] => node
    case node: JavaName[Pre] => node
  }

  def preCoerce(e: Expr[Pre]): Expr[Pre] = e
  def postCoerce(e: Expr[Pre]): Expr[Post] = rewriteDefault(e)
  override def dispatch(e: Expr[Pre]): Expr[Post] =
    postCoerce(coerce(preCoerce(e)))

  def preCoerce(decl: Declaration[Pre]): Declaration[Pre] = decl
  def postCoerce(decl: Declaration[Pre]): Unit = rewriteDefault(decl)
  override def dispatch(decl: Declaration[Pre]): Unit =
    postCoerce(coerce(preCoerce(decl)))

  def coerce(value: Expr[Pre], target: Type[Pre]): Expr[Pre] =
    coerce(value, Coercion.getCoercion(value.t, target) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(value, target)
    })(CoercionOrigin)

  def coerceArgs(args: Seq[Expr[Pre]], app: Applicable[Pre]): Seq[Expr[Pre]] =
    args.zip(app.args).map {
      case (value, arg) => coerce(value, arg.t)
    }

  def coerceArgs(args: Seq[Expr[Pre]], app: ContractApplicable[Pre], tArgs: Seq[Type[Pre]]): Seq[Expr[Pre]] =
    args.zip(app.args).map {
      case (value, arg) => coerce(value, arg.t.particularize(app.typeArgs.zip(tArgs).toMap))
    }

  def rat(e: Expr[Pre]): Expr[Pre] = coerce(e, TRational[Pre]())
  def bool(e: Expr[Pre]): Expr[Pre] = coerce(e, TBool[Pre]())
  def res(e: Expr[Pre]): Expr[Pre] = coerce(e, TResource[Pre]())
  def int(e: Expr[Pre]): Expr[Pre] = coerce(e, TInt[Pre]())
  def process(e: Expr[Pre]): Expr[Pre] = coerce(e, TProcess[Pre]())
  def ref(e: Expr[Pre]): Expr[Pre] = coerce(e, TRef[Pre]())
  def option(e: Expr[Pre]): (Expr[Pre], TOption[Pre]) =
    Coercion.getAnyOptionCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected an option here, but got ${e.t}")
    }
  def tuple(e: Expr[Pre]): (Expr[Pre], TTuple[Pre]) =
    Coercion.getAnyTupleCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a tuple here, but got ${e.t}")
    }
  def seq(e: Expr[Pre]): (Expr[Pre], TSeq[Pre]) =
    Coercion.getAnySeqCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a sequence here, but got ${e.t}")
    }
  def set(e: Expr[Pre]): (Expr[Pre], TSet[Pre]) =
    Coercion.getAnySetCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a set here, but got ${e.t}")
    }
  def bag(e: Expr[Pre]): (Expr[Pre], TBag[Pre]) =
    Coercion.getAnyBagCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a bag here, but got ${e.t}")
    }
  def map(e: Expr[Pre]): (Expr[Pre], TMap[Pre]) =
    Coercion.getAnyMapCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a map here, but got ${e.t}")
    }
  def collection(e: Expr[Pre]): (Expr[Pre], SizedType[Pre]) =
    Coercion.getAnyCollectionCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a collection type here, but got ${e.t}")
    }
  def array(e: Expr[Pre]): (Expr[Pre], TArray[Pre]) =
    Coercion.getAnyArrayCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected an array here, but got ${e.t}")
    }
  def arrayMatrix(e: Expr[Pre]): (Expr[Pre], TArray[Pre]) =
    Coercion.getAnyMatrixArrayCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a two-dimensional array here, but got ${e.t}")
    }
  def pointer(e: Expr[Pre]): (Expr[Pre], TPointer[Pre]) =
    Coercion.getAnyPointerCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a pointer here, but got ${e.t}")
    }
  def cls(e: Expr[Pre]): (Expr[Pre], TClass[Pre]) =
    Coercion.getAnyClassCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a class here, but got ${e.t}")
    }
  def matrix(e: Expr[Pre]): (Expr[Pre], TMatrix[Pre]) =
    Coercion.getAnyMatrixCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a matrix here, but got ${e.t}")
    }
  def model(e: Expr[Pre]): (Expr[Pre], TModel[Pre]) =
    Coercion.getAnyModelCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a model here, but got ${e.t}")
    }
  def either(e: Expr[Pre]): (Expr[Pre], TEither[Pre]) =
    Coercion.getAnyEitherCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected an either here, but got ${e.t}")
    }

  def firstOk[T](expr: Expr[Pre], message: => String,
                 alt1: => T = throw IncoercibleDummy,
                 alt2: => T = throw IncoercibleDummy,
                 alt3: => T = throw IncoercibleDummy,
                 alt4: => T = throw IncoercibleDummy,
                 alt5: => T = throw IncoercibleDummy,
                 alt6: => T = throw IncoercibleDummy,
                 alt7: => T = throw IncoercibleDummy,
                 alt8: => T = throw IncoercibleDummy): T = {
    try {
      alt1
    } catch {
      case _: CoercionError => try {
        alt2
      } catch {
        case _: CoercionError => try {
          alt3
        } catch {
          case _: CoercionError => try {
            alt4
          } catch {
            case _: CoercionError => try {
              alt5
            } catch {
              case _: CoercionError => try {
                alt6
              } catch {
                case _: CoercionError => try {
                  alt7
                } catch {
                  case _: CoercionError => try {
                    alt8
                  } catch {
                    case _: CoercionError =>
                      throw IncoercibleText(expr, message)
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  def coerce(e: Expr[Pre]): Expr[Pre] = {
    implicit val o: Origin = e.o

    e match {
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
                  println(x)
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
        firstOk(e, s"Expected both operands to be numeric or a process, but got ${left.t} and ${right.t}.",
          AmbiguousMult(int(left), int(right)),
          AmbiguousMult(rat(left), rat(right)),
          AmbiguousMult(process(left), process(right)),
        )
      case AmbiguousOr(left, right) =>
        firstOk(e, s"Expected both operands to be boolean or a process, but got ${left.t} and ${right.t}.",
          AmbiguousOr(bool(left), bool(right)),
          AmbiguousOr(process(left), process(right)),
        )
      case plus @ AmbiguousPlus(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a process, a sequence, set, or bag; or a pointer and integer, but got ${left.t} and ${right.t}.",
          AmbiguousPlus(int(left), int(right))(plus.blame),
          AmbiguousPlus(rat(left), rat(right))(plus.blame),
          AmbiguousPlus(process(left), process(right))(plus.blame),
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
          }
        )
      case AmbiguousResult() => e
      case sub @ AmbiguousSubscript(collection, index) =>
        val coercedIndex = int(index)
        firstOk(e, s"Expected collection to be a sequence, array, pointer or map, but got ${collection.t}.",
          AmbiguousSubscript(seq(collection)._1, coercedIndex)(sub.blame),
          AmbiguousSubscript(array(collection)._1, coercedIndex)(sub.blame),
          AmbiguousSubscript(pointer(collection)._1, coercedIndex)(sub.blame),
          AmbiguousSubscript(map(collection)._1, coercedIndex)(sub.blame),
        )
      case AmbiguousThis() => e
      case And(left, right) =>
        And(bool(left), bool(right))
      case Any() =>
        Any()
      case APerm(loc, perm) =>
        APerm(loc, rat(perm))
      case a @ ArraySubscript(arr, index) =>
        ArraySubscript(array(arr)._1, int(index))(a.blame)
      case BagMemberCount(x, xs) =>
        val (coercedBag, TBag(element)) = bag(xs)
        val sharedType = Types.leastCommonSuperType(x.t, element)
        BagMemberCount(coerce(x, sharedType), coerce(coercedBag, TBag(sharedType)))
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
          Div(int(left), int(right))(div.blame),
          Div(int(left), rat(right))(div.blame),
          Div(rat(left), int(right))(div.blame),
          Div(rat(left), rat(right))(div.blame),
        )
      case Drop(xs, count) =>
        Drop(seq(xs)._1, int(count))
      case Empty(obj) =>
        Empty(collection(obj)._1)
      case EmptyProcess() => EmptyProcess()
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
      case inv @ FunctionInvocation(ref, args, typeArgs) =>
        FunctionInvocation(ref, coerceArgs(args, ref.decl, typeArgs), typeArgs)(inv.blame)
      case get @ GetLeft(e) =>
        GetLeft(either(e)._1)(get.blame)
      case get @ GetRight(e) =>
        GetRight(either(e)._1)(get.blame)
      case GpgpuCudaKernelInvocation(kernel, blocks, threads, args, givenArgs, yields) =>
        GpgpuCudaKernelInvocation(kernel, int(blocks), int(threads), args, givenArgs, yields)
      case Greater(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          Greater(int(left), int(right)),
          Greater(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            Greater(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            Greater(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case GreaterEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          GreaterEq(int(left), int(right)),
          GreaterEq(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            GreaterEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            GreaterEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case head @ Head(xs) =>
        Head(seq(xs)._1)(head.blame)
      case Held(obj) =>
        Held(cls(obj)._1)
      case HPerm(loc, perm) =>
        HPerm(loc, rat(perm))
      case IdleToken(thread) =>
        IdleToken(cls(thread)._1)
      case Implies(left, right) =>
        Implies(bool(left), res(right))
      case FunctionOf(e, ref) =>
        FunctionOf(e, ref)
      case InlinePattern(inner) =>
        InlinePattern(inner)
      case inv @ InstanceFunctionInvocation(obj, ref, args, typeArgs) =>
        InstanceFunctionInvocation(cls(obj)._1, ref, coerceArgs(args, ref.decl, typeArgs), typeArgs)(inv.blame)
      case InstanceOf(value, typeValue) =>
        InstanceOf(value, typeValue)
      case InstancePredicateApply(obj, ref, args, perm) =>
        InstancePredicateApply(cls(obj)._1, ref, coerceArgs(args, ref.decl), rat(perm))
      case IsLeft(e) =>
        IsLeft(either(e)._1)
      case IsRight(e) =>
        IsRight(either(e)._1)
      case deref @ JavaDeref(obj, field) => e
      case inv @ JavaInvocation(obj, typeParams, method, arguments, givenArgs, yields) => e
      case JavaLiteralArray(exprs) =>
        JavaLiteralArray(exprs)
      case JavaLocal(name) => e
      case JavaNewClass(args, typeArgs, name) => e
      case JavaNewDefaultArray(baseType, specifiedDims, moreDims) => e
      case JavaNewLiteralArray(baseType, dims, initializer) => e
      case JoinToken(thread) =>
        JoinToken(cls(thread)._1)
      case length @ Length(arr) =>
        Length(array(arr)._1)(length.blame)
      case Less(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          Less(int(left), int(right)),
          Less(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            Less(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            Less(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case LessEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          LessEq(int(left), int(right)),
          LessEq(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
            LessEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
            LessEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
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
      case MapSize(m) =>
        MapSize(map(m)._1)
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
      case inv @ MethodInvocation(obj, ref, args, outArgs, typeArgs) =>
        MethodInvocation(cls(obj)._1, ref, coerceArgs(args, ref.decl, typeArgs), outArgs, typeArgs)(inv.blame)
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
      case inv @ ProcedureInvocation(ref, args, outArgs, typeArgs) =>
        ProcedureInvocation(ref, coerceArgs(args, ref.decl, typeArgs), outArgs, typeArgs)(inv.blame)
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
      case PVLNew(t, args) => e
      case Range(from, to) =>
        Range(int(from), int(to))
      case ReadPerm() =>
        ReadPerm()
      case RemoveAt(xs, i) =>
        RemoveAt(seq(xs)._1, int(i))
      case Result(ref) =>
        Result(ref)
      case Scale(scale, r) =>
        Scale(rat(scale), res(r))
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
      case SetMember(x, xs) =>
        val (coercedSet, setType) = set(xs)
        val sharedType = Types.leastCommonSuperType(x.t, setType.element)
        SetMember(coerce(x, sharedType), coerce(coercedSet, TSet(sharedType)))
      case SilverCurFieldPerm(obj, field) =>
        SilverCurFieldPerm(ref(obj), field)
      case SilverCurPredPerm(ref, args) =>
        SilverCurPredPerm(ref, coerceArgs(args, ref.decl))
      case deref @ SilverDeref(obj, field) =>
        SilverDeref(ref(obj), field)(deref.blame)
      case SilverIntToRat(perm) =>
        SilverIntToRat(int(perm))
      case Size(obj) =>
        Size(collection(obj)._1)
      case Slice(xs, from, to) =>
        Slice(seq(xs)._1, int(from), int(to))
      case Star(left, right) =>
        Star(res(left), res(right))
      case Starall(bindings, triggers, body) =>
        Starall(bindings, triggers, res(body))
      case SubSet(left, right) =>
        firstOk(e, s"Expected both operands to be a set or bag, but got ${left.t} and ${right.t}.", {
          val (coercedLeft, leftSet) = set(left)
          val (coercedRight, rightSet) = set(right)
          val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
          SubSet(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
        }, {
          val (coercedLeft, leftBag) = bag(left)
          val (coercedRight, rightBag) = bag(right)
          val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
          SubSet(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
        })
      case SubSetEq(left, right) =>
        firstOk(e, s"Expected both operands to be a set or bag, but got ${left.t} and ${right.t}.", {
          val (coercedLeft, leftSet) = set(left)
          val (coercedRight, rightSet) = set(right)
          val sharedType = Types.leastCommonSuperType(leftSet.element, rightSet.element)
          SubSetEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
        }, {
          val (coercedLeft, leftBag) = bag(left)
          val (coercedRight, rightBag) = bag(right)
          val sharedType = Types.leastCommonSuperType(leftBag.element, rightBag.element)
          SubSetEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
        })
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
    }
  }

  def coerce(decl: Declaration[Pre]): Declaration[Pre] =
    decl
}
