package vct.col.coerce

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.Rewriter
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable.ArrayBuffer

case object NopCoercingRewriter extends CoercingRewriter() {
  globalScopes += ArrayBuffer()

  override def coerce(e: Expr, coercion: Coercion)(implicit o: Origin): Expr = e
}

case object CoercingRewriter {
  sealed trait CoercionError extends RuntimeException
  case object IncoercibleDummy extends CoercionError
  case class Incoercible(e: Expr, target: Type) extends CoercionError
  case class IncoercibleText(e: Expr, message: String) extends CoercionError

  case object CoercionOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def messageInContext(message: String): String =
      s"At node generated for a coercion: $message"
  }
}

abstract class CoercingRewriter() extends Rewriter {
  import CoercingRewriter._

  /**
    * Apply a particular coercion to an expression.
    * SAFETY: all promoting coercions must be injective; otherwise the default mapping coercion of sets is unsound.
    * @param e the expression to coerce
    * @param coercion the coercion
    * @param sc the scope in which to declare functions
    * @return the coerced expression
    */
  def coerce(e: Expr, coercion: Coercion)(implicit o: Origin): Expr = {
    coercion match {
      case Coercion.Identity => e
      case Coercion.Compose(left, right) => coerce(coerce(e, right), left)
      case Coercion.NothingSomething(_) => e
      case Coercion.SomethingAny => e
      case Coercion.MapOption(_, _, inner) =>
        Select(Eq(e, OptNone()), OptNone(), coerce(OptGet(e)(NeverNone), inner))
      case Coercion.MapSeq(source, target, inner) =>
        val result = AmbiguousResult()
        result.ref = Some(target)
        val v = new Variable(source)
        val i = new Variable(TInt())
        val v_i = SeqSubscript(v.get, i.get)(FramedSeqIndex)
        val result_i = SeqSubscript(result, i.get)(FramedSeqIndex)

        val f = function(
          blame = AbstractApplicable,
          returnType = target,
          args = Seq(v),
          ensures =
            Eq(Size(v.get), Size(result)) &&
            Forall(Seq(i), Seq(Seq(result_i)),
              (const(0) < i.get && i.get < Size(result)) ==>
                result_i === coerce(v_i, inner)),
        )

        f.declareDefault(this)
        FunctionInvocation(f.ref, Seq(e), Nil)(PanicBlame("default coercion for seq<_> requires nothing."))
      case Coercion.MapSet(source, target, inner) =>
        val result = AmbiguousResult()
        result.ref = Some(target)
        val v = new Variable(source)
        val elem = new Variable(source.element)

        val f = function(
          blame = AbstractApplicable,
          returnType = target,
          args = Seq(v),
          ensures =
            Eq(Size(result), Size(v.get)) &&
              Forall(Seq(elem), Seq(Seq(SetMember(elem.get, result))),
                Eq(SetMember(coerce(elem.get, inner), result), SetMember(elem.get, v.get)))
        )

        f.declareDefault(this)
        FunctionInvocation(f.ref, Seq(e), Nil)(PanicBlame("Default coercion for set<_> requires nothing."))
      case Coercion.MapBag(source, target, inner) =>
        val result = AmbiguousResult()
        result.ref = Some(target)
        val v = new Variable(source)
        val elem = new Variable(source.element)

        val f = function(
          blame = AbstractApplicable,
          returnType = target,
          args = Seq(v),
          ensures =
            Eq(Size(result), Size(v.get)) &&
            Forall(Seq(elem), Seq(Seq(BagMemberCount(elem.get, result))),
              Eq(BagMemberCount(coerce(elem.get, inner), result), BagMemberCount(elem.get, v.get)))
        )

        f.declareDefault(this)
        FunctionInvocation(f.ref, Seq(e), Nil)(PanicBlame("Default coercion for bag<_> requires nothing."))
      case Coercion.MapMatrix(source, target, inner) =>
        ???
      case Coercion.MapMap(source, target, inner) =>
        val result = AmbiguousResult()
        result.ref = Some(target)
        val v = new Variable(source)
        val k = new Variable(source.key)

        val f = function(
          blame = AbstractApplicable,
          returnType = target,
          args = Seq(v),
          ensures =
            Eq(MapKeySet(result), MapKeySet(v.get)) &&
              Forall(Seq(k), Seq(Seq(MapGet(result, k.get)(TriggerPatternBlame))),
                SetMember(k.get, MapKeySet(result)) ==> Eq(MapGet(result, k.get)(FramedMapGet), MapGet(v.get, k.get)(FramedMapGet)))
        )

        f.declareDefault(this)
        FunctionInvocation(f.ref, Seq(e), Nil)(PanicBlame("Default coercion for map<_, _> requires nothing."))
      case Coercion.MapTuple(source, target, left, right) =>
        LiteralTuple(target.elements, Seq(coerce(TupGet(e, 0), left), coerce(TupGet(e, 1), right)))
      case Coercion.MapType(source, target, inner) =>
        ???

      case Coercion.BoolResource => e
      case Coercion.BoundIntFrac => e
      case Coercion.BoundIntZFrac(_) => e
      case Coercion.JoinUnion(_, _, inner) => e
      case Coercion.SelectUnion(_, _, _, inner) => coerce(e, inner)

      case Coercion.Supports(_, _) => e
      case Coercion.JavaSupports(_, _) => e
      case Coercion.NullRef => e
      case Coercion.NullArray(_) => e
      case Coercion.NullClass(_) => e
      case Coercion.NullJavaClass(_) => e
      case Coercion.NullPointer(_) => e
      case Coercion.FracZFrac => e
      case Coercion.ZFracRat => e
      case Coercion.FloatRat(_) => e
      case Coercion.WidenBound(_, _) => e
      case Coercion.UnboundInt(_) => e

      case Coercion.IntRat => e
      case Coercion.RatZFrac => e
      case Coercion.ZFracFrac => e
    }
  }

  def coerceAny(node: NodeFamily): NodeFamily = node match {
    case node: Program => node
    case node: Statement => node
    case node: Expr => coerce(node)
    case node: Type => node
    case node: ApplicableContract => node
    case node: LoopContract => node
    case node: ParBlock => node
    case node: CatchClause => node
    case node: SignalsClause => node
    case node: FieldFlag => node
    case node: IterVariable => node
    case node: SilverPredicateAccess => node
    case node: CDeclarator => node
    case node: CDeclarationSpecifier => node
    case node: CTypeQualifier => node
    case node: CPointer => node
    case node: CInit => node
    case node: JavaModifier => node
    case node: JavaImport => node
    case node: JavaName => node
  }

  def preCoerce(e: Expr): Expr = e
  def postCoerce(e: Expr): Expr = rewriteDefault(e)
  override def dispatch(e: Expr): Expr =
    postCoerce(coerce(preCoerce(e)))

  def preCoerce(decl: Declaration): Declaration = decl
  def postCoerce(decl: Declaration): Unit = rewriteDefault(decl)
  override def dispatch(decl: Declaration): Unit =
    postCoerce(coerce(preCoerce(decl)))

  def coerce(value: Expr, target: Type): Expr =
    coerce(value, Coercion.getCoercion(value.t, target).getOrElse(throw Incoercible(value, target)))(CoercionOrigin)

  def coerceArgs(args: Seq[Expr], app: Applicable): Seq[Expr] =
    args.zip(app.args).map {
      case (value, arg) => coerce(value, arg.t)
    }

  def coerceArgs(args: Seq[Expr], app: ContractApplicable, tArgs: Seq[Type]): Seq[Expr] =
    args.zip(app.args).map {
      case (value, arg) => coerce(value, arg.t.particularize(app.typeArgs.zip(tArgs).toMap))
    }

  def rat(e: Expr): Expr = coerce(e, TRational())
  def bool(e: Expr): Expr = coerce(e, TBool())
  def res(e: Expr): Expr = coerce(e, TResource())
  def int(e: Expr): Expr = coerce(e, TInt())
  def process(e: Expr): Expr = coerce(e, TProcess())
  def ref(e: Expr): Expr = coerce(e, TRef())
  def option(e: Expr): (Expr, TOption) =
    Coercion.getAnyOptionCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected an option here, but got ${e.t}")
    }
  def tuple(e: Expr): (Expr, TTuple) =
    Coercion.getAnyTupleCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a tuple here, but got ${e.t}")
    }
  def seq(e: Expr): (Expr, TSeq) =
    Coercion.getAnySeqCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a sequence here, but got ${e.t}")
    }
  def set(e: Expr): (Expr, TSet) =
    Coercion.getAnySetCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a set here, but got ${e.t}")
    }
  def bag(e: Expr): (Expr, TBag) =
    Coercion.getAnyBagCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a bag here, but got ${e.t}")
    }
  def map(e: Expr): (Expr, TMap) =
    Coercion.getAnyMapCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a map here, but got ${e.t}")
    }
  def collection(e: Expr): (Expr, CollectionType) =
    Coercion.getAnyCollectionCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a collection type here, but got ${e.t}")
    }
  def array(e: Expr): (Expr, TArray) =
    Coercion.getAnyArrayCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected an array here, but got ${e.t}")
    }
  def arrayMatrix(e: Expr): (Expr, TArray) =
    Coercion.getAnyMatrixArrayCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a two-dimensional array here, but got ${e.t}")
    }
  def pointer(e: Expr): (Expr, TPointer) =
    Coercion.getAnyPointerCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a pointer here, but got ${e.t}")
    }
  def cls(e: Expr): (Expr, TClass) =
    Coercion.getAnyClassCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a class here, but got ${e.t}")
    }
  def matrix(e: Expr): (Expr, TMatrix) =
    Coercion.getAnyMatrixCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a matrix here, but got ${e.t}")
    }
  def model(e: Expr): (Expr, TModel) =
    Coercion.getAnyModelCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion)(CoercionOrigin), t)
      case None => throw IncoercibleText(e, s"Expected a model here, but got ${e.t}")
    }

  def firstOk[T](expr: Expr, message: => String,
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

  def coerce(e: Expr): Expr = {
    implicit val o: Origin = e.o

    e match {
      case ActionApply(action, args) =>
        ActionApply(succ(action.decl), coerceArgs(args, action.decl))
      case ActionPerm(loc, perm) =>
        ActionPerm(loc, rat(perm))
      case AddrOf(e) =>
        AddrOf(e)
      case ADTFunctionInvocation(typeArgs, ref, args) => typeArgs match {
        case Some((adt, typeArgs)) =>
          ADTFunctionInvocation(Some((succ(adt.decl), typeArgs)), succ(ref.decl), args.zip(ref.decl.args).map {
            case (value, arg) => coerce(value, arg.t.particularize(adt.decl.typeArgs.zip(typeArgs).toMap))
          })
        case None =>
          ADTFunctionInvocation(None, succ(ref.decl), coerceArgs(args, ref.decl))
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
          val sharedType = Type.leastCommonSuperType(x.t, element)
          AmbiguousMember(coerce(x, sharedType), coerce(coercedXs, TSeq(sharedType)))
        }, {
          val (coercedXs, TSet(element)) = set(xs)
          val sharedType = Type.leastCommonSuperType(x.t, element)
          AmbiguousMember(coerce(x, sharedType), coerce(coercedXs, TSet(sharedType)))
        }, {
          val (coercedXs, TBag(element)) = bag(xs)
          val sharedType = Type.leastCommonSuperType(x.t, element)
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
      case AmbiguousPlus(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a process, a sequence, set, or bag; or a pointer and integer, but got ${left.t} and ${right.t}.",
          AmbiguousPlus(int(left), int(right)),
          AmbiguousPlus(rat(left), rat(right)),
          AmbiguousPlus(process(left), process(right)),
          AmbiguousPlus(pointer(left)._1, int(right)), {
            val (coercedLeft, TSeq(elementLeft)) = seq(left)
            val (coercedRight, TSeq(elementRight)) = seq(right)
            val sharedType = Type.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousPlus(coerce(coercedLeft, TSeq(sharedType)), coerce(coercedRight, TSeq(sharedType)))
          }, {
            val (coercedLeft, TSeq(elementLeft)) = seq(left)
            val (coercedRight, TSeq(elementRight)) = seq(right)
            val sharedType = Type.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousPlus(coerce(coercedLeft, TSeq(sharedType)), coerce(coercedRight, TSeq(sharedType)))
          }, {
            val (coercedLeft, TSeq(elementLeft)) = seq(left)
            val (coercedRight, TSeq(elementRight)) = seq(right)
            val sharedType = Type.leastCommonSuperType(elementLeft, elementRight)
            AmbiguousPlus(coerce(coercedLeft, TSeq(sharedType)), coerce(coercedRight, TSeq(sharedType)))
          }
        )
      case AmbiguousResult() => e
      case AmbiguousSubscript(collection, index) =>
        val coercedIndex = int(index)
        firstOk(e, s"Expected collection to be a sequence, array, pointer or map, but got ${collection.t}.",
          AmbiguousSubscript(seq(collection)._1, coercedIndex),
          AmbiguousSubscript(array(collection)._1, coercedIndex),
          AmbiguousSubscript(pointer(collection)._1, coercedIndex),
          AmbiguousSubscript(map(collection)._1, coercedIndex),
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
        val sharedType = Type.leastCommonSuperType(x.t, element)
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
        ???
      case inv @ CInvocation(applicable, args, givenArgs, yields) =>
        CInvocation(applicable, args, givenArgs, yields)
      case CLocal(name) => e
      case Concat(xs, ys) =>
        val (coercedXs, TSeq(xElement)) = seq(xs)
        val (coercedYs, TSeq(yElement)) = seq(ys)
        val sharedType = Type.leastCommonSuperType(xElement, yElement)
        Concat(coerce(xs, TSeq(sharedType)), coerce(ys, TSeq(sharedType)))
      case Cons(x, xs) =>
        val (coercedXs, TSeq(element)) = seq(xs)
        val sharedType = Type.leastCommonSuperType(x.t, element)
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
        Deref(cls(obj)._1, succ(ref.decl))(deref.blame)
      case deref @ DerefPointer(p) =>
        DerefPointer(pointer(p)._1)(deref.blame)
      case div @ Div(left, right) =>
        Div(rat(left), rat(right))(div.blame)
      case Drop(xs, count) =>
        Drop(seq(xs)._1, int(count))
      case Empty(obj) =>
        Empty(collection(obj)._1)
      case EmptyProcess() => EmptyProcess()
      case Eq(left, right) =>
        val sharedType = Type.leastCommonSuperType(left.t, right.t)
        Eq(coerce(left, sharedType), coerce(right, sharedType))
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
        FunctionInvocation(succ(ref.decl), coerceArgs(args, ref.decl, typeArgs), typeArgs)(inv.blame)
      case GpgpuCudaKernelInvocation(kernel, blocks, threads, args, givenArgs, yields) =>
        GpgpuCudaKernelInvocation(kernel, int(blocks), int(threads), args, givenArgs, yields)
      case Greater(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          Greater(int(left), int(right)),
          Greater(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Type.leastCommonSuperType(leftSet.element, rightSet.element)
            Greater(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Type.leastCommonSuperType(leftBag.element, rightBag.element)
            Greater(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case GreaterEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          GreaterEq(int(left), int(right)),
          GreaterEq(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Type.leastCommonSuperType(leftSet.element, rightSet.element)
            GreaterEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Type.leastCommonSuperType(leftBag.element, rightBag.element)
            GreaterEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case Head(xs) =>
        Head(seq(xs)._1)
      case Held(obj) =>
        Held(cls(obj)._1)
      case HPerm(loc, perm) =>
        HPerm(loc, rat(perm))
      case IdleToken(thread) =>
        IdleToken(cls(thread)._1)
      case Implies(left, right) =>
        Implies(bool(left), res(right))
      case InlinePattern(inner) =>
        InlinePattern(inner)
      case inv @ InstanceFunctionInvocation(obj, ref, args, typeArgs) =>
        InstanceFunctionInvocation(cls(obj)._1, succ(ref.decl), coerceArgs(args, ref.decl, typeArgs), typeArgs)(inv.blame)
      case InstanceOf(value, typeValue) =>
        ???
      case InstancePredicateApply(obj, ref, args) =>
        InstancePredicateApply(cls(obj)._1, succ(ref.decl), coerceArgs(args, ref.decl))
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
            val sharedType = Type.leastCommonSuperType(leftSet.element, rightSet.element)
            Less(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Type.leastCommonSuperType(leftBag.element, rightBag.element)
            Less(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
          },
        )
      case LessEq(left, right) =>
        firstOk(e, s"Expected both operands to be numeric, a set, or a bag, but got ${left.t} and ${right.t}.",
          LessEq(int(left), int(right)),
          LessEq(rat(left), rat(right)), {
            val (coercedLeft, leftSet) = set(left)
            val (coercedRight, rightSet) = set(right)
            val sharedType = Type.leastCommonSuperType(leftSet.element, rightSet.element)
            LessEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
          }, {
            val (coercedLeft, leftBag) = bag(left)
            val (coercedRight, rightBag) = bag(right)
            val sharedType = Type.leastCommonSuperType(leftBag.element, rightBag.element)
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
        Local(succ(ref.decl))
      case MapCons(m, k, v) =>
        val (coercedMap, mapType) = map(m)
        val sharedType = Type.leastCommonSuperType(mapType.value, v.t)
        MapCons(coerce(coercedMap, TMap(mapType.key, sharedType)), coerce(k, mapType.key), coerce(v, sharedType))
      case MapDisjoint(left, right) =>
        val (coercedLeft, leftType) = map(left)
        val (coercedRight, rightType) = map(right)

        if(leftType.key != rightType.key)
          throw IncoercibleText(e, s"Expected both operands to have a map type of which the key type is equal, " +
            s"but got ${leftType.key} and ${rightType.key}")

        val sharedType = Type.leastCommonSuperType(leftType.value, rightType.value)
        val mapType = TMap(leftType.key, sharedType)
        MapDisjoint(coerce(coercedLeft, mapType), coerce(coercedRight, mapType))
      case MapEq(left, right) =>
        val (coercedLeft, leftType) = map(left)
        val (coercedRight, rightType) = map(right)

        if(leftType.key != rightType.key)
          throw IncoercibleText(e, s"Expected both operands to have a map type of which the key type is equal, " +
            s"but got ${leftType.key} and ${rightType.key}")

        val sharedType = Type.leastCommonSuperType(leftType.value, rightType.value)
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
        val sharedType = Type.leastCommonSuperType(leftType.element, rightType.element)
        MatrixCompare(coerce(coercedLeft, TMatrix(sharedType)), coerce(coercedRight, TMatrix(sharedType)))
      case MatrixRepeat(e) =>
        MatrixRepeat(e)
      case MatrixSum(indices, mat) =>
        MatrixSum(coerce(indices, TSeq(TInt())), coerce(mat, TSeq(TRational())))
      case inv @ MethodInvocation(obj, ref, args, outArgs, typeArgs) =>
        MethodInvocation(cls(obj)._1, succ(ref.decl), coerceArgs(args, ref.decl, typeArgs), outArgs.map(v => succ(v.decl)), typeArgs)(inv.blame)
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
        ModelDeref(model(obj)._1, succ(ref.decl))(deref.blame)
      case ModelDestroy(m) =>
        ModelDestroy(model(m)._1)
      case ModelMerge(m, leftPerm, leftProcess, rightPerm, rightProcess) =>
        ModelMerge(model(m)._1, rat(leftPerm), process(leftProcess), rat(rightPerm), process(rightProcess))
      case ModelNew(ref) =>
        ModelNew(succ(ref.decl))
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
        val sharedType = Type.leastCommonSuperType(left.t, right.t)
        Neq(coerce(left, sharedType), coerce(right, sharedType))
      case NewArray(element, dims, moreDims) =>
        NewArray(element, dims.map(int), moreDims)
      case NewObject(cls) =>
        NewObject(succ(cls.decl))
      case NoPerm() =>
        NoPerm()
      case Not(arg) =>
        Not(bool(arg))
      case Null() =>
        Null()
      case old @ Old(expr, at) =>
        Old(expr, at.map(ref => succ(ref.decl)))(old.blame)
      case get @ OptGet(opt) =>
        OptGet(option(opt)._1)(get.blame)
      case OptGetOrElse(opt, alt) =>
        val (coercedOpt, optType) = option(opt)
        val sharedType = Type.leastCommonSuperType(alt.t, optType.element)
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
        val sharedType = Type.leastCommonSuperType(leftType.element, rightType.element)
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
      case PostAssignExpression(target, value) =>
        PostAssignExpression(target, coerce(value, target.t))
      case PreAssignExpression(target, value) =>
        PreAssignExpression(target, coerce(value, target.t))
      case PredicateApply(ref, args) =>
        PredicateApply(succ(ref.decl), coerceArgs(args, ref.decl))
      case inv @ ProcedureInvocation(ref, args, outArgs, typeArgs) =>
        ProcedureInvocation(succ(ref.decl), coerceArgs(args, ref.decl, typeArgs), outArgs.map(v => succ(v.decl)), typeArgs)(inv.blame)
      case ProcessApply(process, args) =>
        ProcessApply(succ(process.decl), coerceArgs(args, process.decl))
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
      case Scale(scale, r) =>
        Scale(rat(scale), res(r))
      case Select(condition, whenTrue, whenFalse) =>
        val sharedType = Type.leastCommonSuperType(whenTrue.t, whenFalse.t)
        Select(bool(condition), coerce(whenTrue, sharedType), coerce(whenFalse, sharedType))
      case SeqMember(x, xs) =>
        val (coercedSeq, seqType) = seq(xs)
        val sharedType = Type.leastCommonSuperType(x.t, seqType.element)
        SeqMember(coerce(x, sharedType), coerce(coercedSeq, TSeq(sharedType)))
      case get @ SeqSubscript(xs, index) =>
        SeqSubscript(seq(xs)._1, int(index))(get.blame)
      case update @ SeqUpdate(xs, i, x) =>
        val (coercedSeq, seqType) = seq(xs)
        val sharedType = Type.leastCommonSuperType(x.t, seqType.element)
        SeqUpdate(coerce(coercedSeq, TSeq(sharedType)), int(i), coerce(x, sharedType))
      case SetMember(x, xs) =>
        val (coercedSet, setType) = set(xs)
        val sharedType = Type.leastCommonSuperType(x.t, setType.element)
        SetMember(coerce(x, sharedType), coerce(coercedSet, TSet(sharedType)))
      case SilverCurFieldPerm(obj, field) =>
        SilverCurFieldPerm(ref(obj), succ(field.decl))
      case SilverCurPredPerm(ref, args) =>
        SilverCurPredPerm(succ(ref.decl), coerceArgs(args, ref.decl))
      case deref @ SilverDeref(obj, field) =>
        SilverDeref(ref(obj), succ(field.decl))(deref.blame)
      case SilverPerm(obj, field, perm) =>
        SilverPerm(ref(obj), succ(field.decl), rat(perm))
      case SilverPredPerm(access) =>
        SilverPredPerm(access)
      case SilverUnfolding(access, body) =>
        SilverUnfolding(access, body)
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
          val sharedType = Type.leastCommonSuperType(leftSet.element, rightSet.element)
          SubSet(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
        }, {
          val (coercedLeft, leftBag) = bag(left)
          val (coercedRight, rightBag) = bag(right)
          val sharedType = Type.leastCommonSuperType(leftBag.element, rightBag.element)
          SubSet(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
        })
      case SubSetEq(left, right) =>
        firstOk(e, s"Expected both operands to be a set or bag, but got ${left.t} and ${right.t}.", {
          val (coercedLeft, leftSet) = set(left)
          val (coercedRight, rightSet) = set(right)
          val sharedType = Type.leastCommonSuperType(leftSet.element, rightSet.element)
          SubSetEq(coerce(coercedLeft, TSet(sharedType)), coerce(coercedRight, TSet(sharedType)))
        }, {
          val (coercedLeft, leftBag) = bag(left)
          val (coercedRight, rightBag) = bag(right)
          val sharedType = Type.leastCommonSuperType(leftBag.element, rightBag.element)
          SubSetEq(coerce(coercedLeft, TBag(sharedType)), coerce(coercedRight, TBag(sharedType)))
        })
      case SubType(left, right) =>
        ???
      case Sum(bindings, condition, main) =>
        Sum(bindings, bool(condition), int(main))
      case SuperType(left, right) =>
        ???
      case Tail(xs) =>
        Tail(seq(xs)._1)
      case Take(xs, count) =>
        Take(seq(xs)._1, int(count))
      case Then(value, post) =>
        Then(value, post)
      case TupGet(tup, index) =>
        TupGet(tuple(tup)._1, index)
      case TypeOf(expr) =>
        ???
      case TypeValue(value) =>
        ???
      case UMinus(arg) =>
        firstOk(e, s"Expected operand to be numeric, but got ${arg.t}.",
          UMinus(int(arg)),
          UMinus(rat(arg)),
        )
      case Unfolding(pred, body) =>
        Unfolding(res(pred), body)
      case UntypedLiteralBag(values) =>
        val sharedType = Type.leastCommonSuperType(values.map(_.t))
        UntypedLiteralBag(values.map(coerce(_, sharedType)))
      case UntypedLiteralSeq(values) =>
        val sharedType = Type.leastCommonSuperType(values.map(_.t))
        UntypedLiteralSeq(values.map(coerce(_, sharedType)))
      case UntypedLiteralSet(values) =>
        val sharedType = Type.leastCommonSuperType(values.map(_.t))
        UntypedLiteralSet(values.map(coerce(_, sharedType)))
      case ValidArray(arr, len) =>
        ValidArray(array(arr)._1, int(len))
      case ValidMatrix(mat, w, h) =>
        ValidMatrix(arrayMatrix(mat)._1, int(w), int(h))
      case value: Constant.BooleanValue => e
      case value: Constant.IntegerValue => e
      case Values(arr, from, to) =>
        Values(array(arr)._1, int(from), int(to))
      case VectorCompare(left, right) =>
        val (coercedLeft, leftType) = seq(left)
        val (coercedRight, rightType) = seq(right)
        val sharedType = Type.leastCommonSuperType(leftType.element, rightType.element)
        val seqType = TSeq(sharedType)
        VectorCompare(coerce(coercedLeft, seqType), coerce(coercedRight, seqType))
      case VectorRepeat(e) =>
        VectorRepeat(e)
      case VectorSum(indices, vec) =>
        VectorSum(coerce(indices, TSeq(TInt())), coerce(vec, TSeq(TRational())))
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

  def coerce(decl: Declaration): Declaration =
    decl
}
