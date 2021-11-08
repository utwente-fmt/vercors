package vct.col.ast

import vct.col.ast.util.SuccessionMap

abstract class CoercingRewriter() extends Rewriter {
  sealed trait CoercionError extends RuntimeException
  case object IncoercibleDummy extends CoercionError
  case class Incoercible(e: Expr, target: Type) extends CoercionError
  case class IncoercibleText(e: Expr, message: String) extends CoercionError

  def coerce(e: Expr, coercion: Coercion): Expr =
    e

  def preCoerce(e: Expr): Expr = e
  def postCoerce(e: Expr): Expr = rewriteDefault(e)
  override def dispatch(e: Expr): Expr =
    postCoerce(coerce(preCoerce(e)))

  def coerce(value: Expr, target: Type): Expr =
    coerce(value, Coercion.getCoercion(value.t, target).get)

  def coerceArgs(args: Seq[Expr], app: Applicable): Seq[Expr] =
    args.zip(app.args).map {
      case (value, arg) => coerce(value, arg.t)
    }

  def rat(e: Expr): Expr = coerce(e, TRational())
  def bool(e: Expr): Expr = coerce(e, TBool())
  def int(e: Expr): Expr = coerce(e, TInt())
  def process(e: Expr): Expr = coerce(e, TProcess())
  def seq(e: Expr): (Expr, TSeq) =
    Coercion.getAnySeqCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion), t)
      case None => throw IncoercibleText(e, s"Expected a sequence here, but got ${e.t}")
    }
  def set(e: Expr): (Expr, TSet) =
    Coercion.getAnySetCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion), t)
      case None => throw IncoercibleText(e, s"Expected a set here, but got ${e.t}")
    }
  def bag(e: Expr): (Expr, TBag) =
    Coercion.getAnyBagCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion), t)
      case None => throw IncoercibleText(e, s"Expected a bag here, but got ${e.t}")
    }
  def map(e: Expr): (Expr, TMap) =
    Coercion.getAnyMapCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion), t)
      case None => throw IncoercibleText(e, s"Expected a map here, but got ${e.t}")
    }
  def array(e: Expr): (Expr, TArray) =
    Coercion.getAnyArrayCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion), t)
      case None => throw IncoercibleText(e, s"Expected an array here, but got ${e.t}")
    }
  def pointer(e: Expr): (Expr, TPointer) =
    Coercion.getAnyPointerCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion), t)
      case None => throw IncoercibleText(e, s"Expected a pointer here, but got ${e.t}")
    }
  def cls(e: Expr): (Expr, TClass) =
    Coercion.getAnyClassCoercion(e.t) match {
      case Some((coercion, t)) => (coerce(e, coercion), t)
      case None => throw IncoercibleText(e, s"Expected a class here, but got ${e.t}")
    }

  def firstOk[T](expr: Expr, message: String,
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
        firstOk(e, "Expected both operands to be of type integer or boolean",
          AmbiguousComputationalAnd(int(left), int(right)),
          AmbiguousComputationalAnd(bool(left), bool(right)),
        )
      case AmbiguousComputationalOr(left, right) =>
        firstOk(e, "Expected both operands to be of type integer or boolean",
          AmbiguousComputationalOr(int(left), int(right)),
          AmbiguousComputationalOr(bool(left), bool(right)),
        )
      case AmbiguousComputationalXor(left, right) =>
        firstOk(e, "Expected both operands to be of type integer or boolean",
          AmbiguousComputationalXor(int(left), int(right)),
          AmbiguousComputationalXor(bool(left), bool(right)),
        )
      case AmbiguousMember(x, xs) =>
        firstOk(xs, "Expected collection to be a sequence, set, bag or map", {
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
        firstOk(e, "Expected both operands to be numeric or a process",
          AmbiguousMult(int(left), int(right)),
          AmbiguousMult(rat(left), rat(right)),
          AmbiguousMult(process(left), process(right)),
        )
      case AmbiguousOr(left, right) =>
        firstOk(e, "Expected both operands to be boolean or a process",
          AmbiguousOr(bool(left), bool(right)),
          AmbiguousOr(process(left), process(right)),
        )
      case AmbiguousPlus(left, right) =>
        firstOk(e, "Expected both operands to be numeric, a process, a sequence, set, or bag; or a pointer and integer",
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
        firstOk(e, "Expected collection to be a sequence, array, pointer or map",
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
        val (bag, TBag(element)) = bag(xs)
        val sharedType = Type.leastCommonSuperType(x.t, element)
        BagMemberCount(coerce(x, sharedType), coerce(bag, TBag(sharedType)))
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
      case Drop(xs, count) =>
      case Empty(obj) =>
      case EmptyProcess() =>
      case Eq(left, right) =>
      case Exists(bindings, triggers, body) =>
      case Exp(left, right) =>
      case FloorDiv(left, right) =>
      case Forall(bindings, triggers, body) =>
      case FunctionInvocation(ref, args, typeArgs) =>
      case GpgpuCudaKernelInvocation(kernel, blocks, threads, args, givenArgs, yields) =>
      case Greater(left, right) =>
      case GreaterEq(left, right) =>
      case Head(xs) =>
      case Held(obj) =>
      case HPerm(loc, perm) =>
      case IdleToken(thread) =>
      case Implies(left, right) =>
      case InlinePattern(inner) =>
      case InstanceFunctionInvocation(obj, ref, args, typeArgs) =>
      case InstanceOf(value, typeValue) =>
      case InstancePredicateApply(obj, ref, args) =>
      case JavaDeref(obj, field) =>
      case JavaInvocation(obj, typeParams, method, arguments, givenArgs, yields) =>
      case JavaLiteralArray(exprs) =>
      case JavaLocal(name) =>
      case JavaNewClass(args, typeArgs, name) =>
      case JavaNewDefaultArray(baseType, specifiedDims, moreDims) =>
      case JavaNewLiteralArray(baseType, dims, initializer) =>
      case JoinToken(thread) =>
      case Length(arr) =>
      case Less(left, right) =>
      case LessEq(left, right) =>
      case Let(binding, value, main) =>
      case LiteralBag(element, values) =>
      case LiteralMap(k, v, values) =>
      case LiteralSeq(element, values) =>
      case LiteralSet(element, values) =>
      case LiteralTuple(ts, values) =>
      case Local(ref) =>
      case MapCons(map, k, v) =>
      case MapDisjoint(left, right) =>
      case MapEq(left, right) =>
      case MapGet(map, k) =>
      case MapItemSet(map) =>
      case MapKeySet(map) =>
      case MapMember(x, xs) =>
      case MapRemove(map, k) =>
      case MapSize(map) =>
      case MapValueSet(map) =>
      case MatrixCompare(left, right) =>
      case MatrixRepeat(e) =>
      case MatrixSum(indices, mat) =>
      case MethodInvocation(obj, ref, args, outArgs, typeArgs) =>
      case Minus(left, right) =>
      case Mod(left, right) =>
      case ModelAbstractState(model, state) =>
      case ModelChoose(model, perm, totalProcess, choice) =>
      case ModelCreate(model, init) =>
      case ModelDeref(obj, ref) =>
      case ModelDestroy(model) =>
      case ModelMerge(model, leftPerm, leftProcess, rightPerm, rightProcess) =>
      case ModelNew(ref) =>
      case ModelPerm(loc, perm) =>
      case ModelSplit(model, leftPerm, leftProcess, rightPerm, rightProcess) =>
      case ModelState(model, perm, state) =>
      case Mult(left, right) =>
      case Neq(left, right) =>
      case NewArray(element, dims, moreDims) =>
      case NewObject(cls) =>
      case NoPerm() =>
      case Not(arg) =>
      case Null() =>
      case Old(expr, at) =>
      case OptGet(opt) =>
      case OptGetOrElse(opt, alt) =>
      case OptNone() =>
      case OptSome(e) =>
      case Or(left, right) =>
      case Perm(loc, perm) =>
      case PermPointer(p, len, perm) =>
      case PermPointerIndex(p, idx, perm) =>
      case Permutation(xs, ys) =>
      case Plus(left, right) =>
      case PointerSubscript(pointer, index) =>
      case PointsTo(loc, perm, value) =>
      case PostAssignExpression(target, value) =>
      case PreAssignExpression(target, value) =>
      case PredicateApply(ref, args) =>
      case ProcedureInvocation(ref, args, outArgs, typeArgs) =>
      case ProcessApply(process, args) =>
      case ProcessChoice(left, right) =>
      case ProcessPar(left, right) =>
      case ProcessSelect(cond, whenTrue, whenFalse) =>
      case ProcessSeq(left, right) =>
      case Product(bindings, condition, main) =>
      case PVLDeref(obj, field) =>
      case PVLInvocation(obj, method, args, typeArgs, givenArgs, yields) =>
      case PVLLocal(name) =>
      case PVLNew(t, args) =>
      case Range(from, to) =>
      case ReadPerm() =>
      case RemoveAt(xs, i) =>
      case Scale(scale, res) =>
      case Select(condition, whenTrue, whenFalse) =>
      case SeqMember(x, xs) =>
      case SeqSubscript(seq, index) =>
      case SeqUpdate(xs, i, x) =>
      case SetMember(x, xs) =>
      case SilverCurFieldPerm(obj, field) =>
      case SilverCurPredPerm(ref, args) =>
      case SilverDeref(obj, field) =>
      case SilverPerm(obj, field, perm) =>
      case SilverPredPerm(access) =>
      case SilverUnfolding(access, body) =>
      case Size(obj) =>
      case Slice(xs, from, to) =>
      case Star(left, right) =>
      case Starall(bindings, triggers, body) =>
      case SubSet(left, right) =>
      case SubSetEq(left, right) =>
      case SubType(left, right) =>
      case Sum(bindings, condition, main) =>
      case SuperType(left, right) =>
      case Tail(xs) =>
      case Take(xs, count) =>
      case Then(value, post) =>
      case TupGet(tup, index) =>
      case TypeOf(expr) =>
      case TypeValue(value) =>
      case UMinus(arg) =>
      case Unfolding(pred, body) =>
      case UntypedLiteralBag(values) =>
      case UntypedLiteralSeq(values) =>
      case UntypedLiteralSet(values) =>
      case ValidArray(arr, len) =>
      case ValidMatrix(mat, w, h) =>
      case value: Constant.BooleanValue =>
      case value: Constant.IntegerValue =>
      case Values(arr, from, to) =>
      case VectorCompare(left, right) =>
      case VectorRepeat(e) =>
      case VectorSum(indices, vec) =>
      case Void() =>
      case Wand(left, right) =>
      case With(pre, value) =>
      case WritePerm() =>
    }
  }
}
