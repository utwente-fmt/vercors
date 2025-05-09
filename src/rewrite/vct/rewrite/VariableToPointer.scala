package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.ref._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError

import scala.collection.mutable

case object VariableToPointer extends RewriterBuilder {
  override def key: String = "variableToPointer"

  override def desc: String =
    "Translate locals and globals to a pointer when their addresses are taken"

  case class UnsupportedAddrOf(loc: Expr[_]) extends UserError {
    override def code: String = "unsupportedAddrOf"

    override def text: String =
      loc.o.messageInContext(
        "Taking an address of this expression is not supported"
      )
  }

  private case class CannotTakeAddressInFunction(arg: Variable[_])
      extends UserError {
    override def code: String = "addrOfFuncArg"

    override def text: String =
      arg.o.messageInContext(
        "Taking the address of a pure function's argument is not supported"
      )
  }
}

case class VariableToPointer[Pre <: Generation]() extends Rewriter[Pre] {

  import VariableToPointer._

  trait PointerSort
  case class Normal(unique: Option[BigInt]) extends PointerSort
  case class Const() extends PointerSort

  val addressedSet: mutable.Map[Node[Pre], PointerSort] =
    new mutable.HashMap[Node[Pre], PointerSort]()
  val heapVariableMap: SuccessionMap[HeapVariable[Pre], HeapVariable[Post]] =
    SuccessionMap()
  val variableMap: SuccessionMap[Variable[Pre], Variable[Post]] =
    SuccessionMap()
  val noTransform: ScopedStack[scala.collection.Set[Variable[Pre]]] =
    ScopedStack()

  def getPointerSort(isConst: Boolean, unique: Option[BigInt]): PointerSort =
    if (!isConst)
      Normal(unique)
    else
      Const()

  def makePointer(innerType: Type[Post], pt: PointerSort): PointerType[Post] =
    pt match {
      case Normal(unique) => TNonNullPointer[Post](innerType, unique)
      case Const() => TNonNullConstPointer[Post](innerType)
    }

  def isConstPointer(pt: PointerSort) =
    pt match {
      case Const() => true
      case _ => false
    }

  def makeNewPointerArray(t: Type[Post])(implicit o: Origin): NewPointer[Post] =
    t match {
      case TNonNullPointer(innerType, unique) =>
        NewNonNullPointerArray[Post](innerType, const(1), unique)(PanicBlame(
          "Size is > 0"
        ))
      case TNonNullConstPointer(innerType) =>
        NewNonNullConstPointerArray[Post](innerType, const(1))(PanicBlame(
          "Size is > 0"
        ))
    }

  // TODO: Replace the asByReferenceClass checks with something that more clearly communicates that we want to exclude all reference types
  def getAddresses(
      e: Node[Pre],
      isConst: Boolean = false,
  ): Option[(Node[Pre], PointerSort)] =
    e match {
      case Local(Ref(v)) if v.t.asByReferenceClass.isEmpty =>
        Some(v, getPointerSort(isConst, None))
      case DerefHeapVariable(Ref(v)) if v.t.asByReferenceClass.isEmpty =>
        Some(v, getPointerSort(isConst, None))
      case AddrOfConstCast(e) => getAddresses(e, isConst = true)
      case AddrOfUniqueCast(Local(Ref(v)), unique) =>
        Some(v, getPointerSort(isConst, Some(unique)))
      case AddrOfUniqueCast(_, _) => ???
      case _ => None
    }

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    // TODO: Replace the asByReferenceClass checks with something that more clearly communicates that we want to exclude all reference types
    addressedSet.addAll(program.flatCollect { case AddrOf(e) =>
      getAddresses(e)
    })
    super.dispatch(program)
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case func: Function[Pre] => {
        val arg = func.args.find(addressedSet.contains(_))
        if (arg.nonEmpty) { throw CannotTakeAddressInFunction(arg.get) }
        globalDeclarations.succeed(func, func.rewriteDefault())
      }
      case proc: Procedure[Pre] => {
        val skipVars = mutable.Set[Variable[Pre]]()
        val extraVars = mutable
          .ArrayBuffer[(Variable[Post], Variable[Post], PointerSort)]()
        // Relies on args being evaluated before body
        allScopes.anySucceed(
          proc,
          proc.rewrite(
            args =
              variables.collect {
                proc.args.map { v =>
                  val newV = variables.succeed(v, v.rewriteDefault())
                  if (addressedSet.contains(v)) {
                    variableMap(v) =
                      new Variable[Post](
                        makePointer(dispatch(v.t), addressedSet(v))
                      )(v.o)
                    skipVars += v
                    extraVars += ((newV, variableMap(v), addressedSet(v)))
                  }
                }
              }._1,
            body = {
              if (proc.body.isEmpty) { None }
              else {
                if (extraVars.isEmpty) { Some(dispatch(proc.body.get)) }
                else {
                  variables.scope {
                    val locals =
                      variables.collect {
                        extraVars.map { case (_, pointer, _) =>
                          variables.declare(pointer)
                        }
                      }._1
                    val block =
                      Block(extraVars.map {
                        case (normal, pointer, Normal(_)) =>
                          Assign(
                            DerefPointer(pointer.get(normal.o))(PanicBlame(
                              "Non-null pointer should always be initialized successfully"
                            ))(normal.o),
                            normal.get(normal.o),
                          )(AssignLocalOk)(proc.o)
                        case (normal, pointer, Const()) =>
                          implicit val o: Origin = normal.o
                          // Const pointers are sequences, so we need to assume their values
                          Assume(
                            DerefPointer(pointer.get)(PanicBlame(
                              "Non-null pointer should always be initialized successfully"
                            )) === normal.get
                          )
                      }.toSeq :+ dispatch(proc.body.get))(proc.o)
                    Some(Scope(locals, block)(proc.o))
                  }
                }
              }
            },
            contract = {
              noTransform.having(skipVars) { dispatch(proc.contract) }
            },
          ),
        )
      }
      case v: HeapVariable[Pre] if addressedSet.contains(v) =>
        heapVariableMap(v) = globalDeclarations.succeed(
          v,
          new HeapVariable(makePointer(dispatch(v.t), addressedSet(v)), None)(
            v.o
          ),
        )
      case v: Variable[Pre] if addressedSet.contains(v) =>
        variableMap(v) = variables.succeed(
          v,
          new Variable(makePointer(dispatch(v.t), addressedSet(v)))(v.o),
        )
      case other => allScopes.anySucceed(other, other.rewriteDefault())
    }

  def assignToConst(target: Expr[Pre]): Boolean =
    target match {
      case Local(v)
          if addressedSet.contains(v.decl) &&
            isConstPointer(addressedSet(v.decl)) =>
        true
      case HeapLocal(v)
          if addressedSet.contains(v.decl) &&
            isConstPointer(addressedSet(v.decl)) =>
        true
      case _ => false
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = stat.o
    stat match {
      case assign @ Assign(target, value) if assignToConst(target) =>
        // We cannot assign towards a const pointer, since it is modelled as sequence. So we have to assume its value
        Assume[Post](dispatch(target) === dispatch(value))
      case s: Scope[Pre] =>
        s.rewrite(
          locals = variables.dispatch(s.locals),
          body = Block(s.locals.filter { local => addressedSet.contains(local) }
            .map { local =>
              implicit val o: Origin = local.o
              Assign(
                Local[Post](variableMap.ref(local)),
                makeNewPointerArray(variableMap(local).t),
              )(PanicBlame("Initialisation should always succeed"))
            } ++ Seq(dispatch(s.body))),
        )
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = expr.o
    expr match {
      case deref @ DerefHeapVariable(Ref(v)) if addressedSet.contains(v) =>
        DerefPointer(
          DerefHeapVariable[Post](heapVariableMap.ref(v))(deref.blame)
        )(PanicBlame("Should always be accessible"))
      case Local(Ref(v))
          if addressedSet.contains(v) && !noTransform.exists(_.contains(v)) =>
        DerefPointer(Local[Post](variableMap.ref(v)))(PanicBlame(
          "Should always be accessible"
        ))
      case newObject @ NewObject(Ref(cls: ByValueClass[Pre])) =>
        val obj = new Variable[Post](TByValueClass(succ(cls), Seq()))
        ScopedExpr(
          Seq(obj),
          With(
            Block(
              Seq(assignLocal(obj.get, newObject.rewriteDefault())) ++
                cls.declarations.flatMap {
                  case f: InstanceField[Pre] =>
                    if (f.t.asClass.isDefined) {
                      Seq(
                        Assign(
                          Deref[Post](obj.get, anySucc(f))(PanicBlame(
                            "Initialisation should always succeed"
                          )),
                          dispatch(NewObject[Pre](f.t.asClass.get.cls)),
                        )(PanicBlame("Initialisation should always succeed"))
                      )
                    } else { Seq() }
                  case _ => Seq()
                }
            ),
            obj.get,
          ),
        )
      case Perm(PointerLocation(AddrOf(DerefHeapVariable(Ref(v)))), perm)
          if addressedSet.contains(v) =>
        val newPerm = dispatch(perm)
        Star(
          Perm(HeapVariableLocation[Post](heapVariableMap.ref(v)), newPerm),
          Perm(
            PointerLocation(DerefHeapVariable[Post](heapVariableMap.ref(v))(
              PanicBlame("Access is framed")
            ))(PanicBlame("Cannot be null")),
            newPerm,
          ),
        )
      case Value(PointerLocation(AddrOf(DerefHeapVariable(Ref(v)))))
          if addressedSet.contains(v) =>
        Star(
          Value(HeapVariableLocation[Post](heapVariableMap.ref(v))),
          Value(
            PointerLocation(DerefHeapVariable[Post](heapVariableMap.ref(v))(
              PanicBlame("Access is framed")
            ))(PanicBlame("cannot be null"))
          ),
        )
      case a @ AddrOf(AddrOfConstCast(e)) => a.rewrite(e = dispatch(e))
      case a @ AddrOf(AddrOfUniqueCast(e, _)) => a.rewrite(e = dispatch(e))
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(loc: Location[Pre]): Location[Post] = {
    implicit val o: Origin = loc.o
    loc match {
      case HeapVariableLocation(Ref(v)) if addressedSet.contains(v) =>
        PointerLocation(
          DerefHeapVariable[Post](heapVariableMap.ref(v))(PanicBlame(
            "Should always be accessible"
          ))
        )(PanicBlame("Should always be accessible"))
      case PointerLocation(AddrOf(DerefHeapVariable(Ref(v))))
          if addressedSet.contains(v) =>
        HeapVariableLocation[Post](heapVariableMap.ref(v))
      case PointerLocation(AddrOf(local @ Local(_))) =>
        throw UnsupportedAddrOf(local)
      case other => other.rewriteDefault()
    }
  }
}
