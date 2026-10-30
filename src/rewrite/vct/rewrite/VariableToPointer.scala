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

  object NonNullAlwaysInitialized
      extends PanicBlame(
        "Non-null pointer should always be initialized successfully"
      )
}

case class VariableToPointer[Pre <: Generation]() extends Rewriter[Pre] {

  import VariableToPointer._

  trait PointerSort
  case class Normal(unique: Option[BigInt]) extends PointerSort
  case class Const() extends PointerSort

  val addressedSet: mutable.Map[Node[Pre], PointerSort] =
    new mutable.HashMap[Node[Pre], PointerSort]()
  val variableMap: SuccessionMap[Variable[Pre], Variable[Post]] =
    SuccessionMap()
  val heapVarMap: SuccessionMap[Variable[Pre], LocalHeapVariable[Post]] =
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

  def makeNewPointer(
      t: Type[Post]
  )(implicit o: Origin): PointerConstructor[Post] =
    t match {
      case TNonNullPointer(innerType, unique) =>
        NewNonNullPointer[Post](innerType, const(1), unique)(PanicBlame(
          "Size is > 0"
        ))
      case TNonNullConstPointer(innerType) =>
        NewNonNullConstPointer[Post](innerType, const(1))(PanicBlame(
          "Size is > 0"
        ))
    }

  def getAddresses(
      e: Node[Pre],
      isConst: Boolean = false,
  ): Option[(Node[Pre], PointerSort)] =
    e match {
      // Nullable PointerArrays (i.e. those in parameters) are not special cased in EncodePointerArrays
      case Local(Ref(v))
          if v.t.asByReferenceClass.isEmpty &&
            (v.t.asPointerArray.isEmpty || !v.t.asPointerArray.get.isNonNull) =>
        Some(v, getPointerSort(isConst, None))
      case AddrOfConstCast(e) => getAddresses(e, isConst = true)
      case AddrOfUniqueCast(Local(Ref(v)), unique) =>
        Some(v, getPointerSort(isConst, Some(unique)))
      case AddrOfUniqueCast(_, _) => ???
      case _ => None
    }

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val gArgs = program.flatCollect { case c: ApplicableContract[Pre] =>
      c.givenArgs ++ c.yieldsArgs
    }.toSet[Node[Pre]]
    // Filter out variables of ghost arguments
    addressedSet.addAll(
      program.flatCollect { case AddrOf(e) => getAddresses(e) }
        .filter(a => !gArgs.contains(a._1))
    )
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
          .ArrayBuffer[(Variable[Post], LocalHeapVariable[Post], PointerSort)]()
        // Relies on args being evaluated before body
        allScopes.anySucceed(
          proc,
          proc.rewrite(
            args =
              variables.collect {
                proc.args.map { v =>
                  val newV = variables.succeed(v, v.rewriteDefault())
                  if (addressedSet.contains(v)) {
                    // These need to be localHeapVariables, otherwise the encoding of ByValueClasses does not work properly
                    heapVarMap(v) =
                      new LocalHeapVariable[Post](
                        makePointer(dispatch(v.t), addressedSet(v))
                      )(v.o)
                    skipVars += v
                    extraVars += ((newV, heapVarMap(v), addressedSet(v)))
                  }
                }
              }._1,
            body = {
              if (proc.body.isEmpty) { None }
              else {
                if (extraVars.isEmpty) { Some(dispatch(proc.body.get)) }
                else {
                  // Add declarations & assignments for the localHeapVariables to the start of the body
                  localHeapVariables.scope {
                    variables.scope {
                      val localHeapDecls =
                        extraVars.map { case (_, v, _) =>
                          HeapLocalDecl(v)(v.o)
                        }.toSeq
                      val block =
                        Block(localHeapDecls ++ extraVars.map {
                          case (normal, pointer, Normal(_)) =>
                            Assign(
                              pointer.get(NonNullAlwaysInitialized)(normal.o),
                              normal.get(normal.o),
                            )(AssignLocalOk)(proc.o)
                          case (normal, pointer, Const()) =>
                            implicit val o: Origin = normal.o
                            // Const pointers are sequences, so we need to assume their values
                            Assume(
                              pointer
                                .get(NonNullAlwaysInitialized)(pointer.o) ===
                                normal.get
                            )
                        }.toSeq :+ dispatch(proc.body.get))(proc.o)
                      Some(Scope(Seq.empty, block)(proc.o))
                    }
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
                makeNewPointer(variableMap(local).t),
              )(PanicBlame("Initialisation should always succeed"))
            } ++ Seq(dispatch(s.body))),
        )
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = expr.o
    expr match {
      case Local(Ref(v))
          if addressedSet.contains(v) && !noTransform.exists(_.contains(v)) =>
        val inVarMap = variableMap.contains(v)
        val inHeapVarMap = heapVarMap.contains(v)
        val local =
          if (inVarMap && !inHeapVarMap) { Local[Post](variableMap.ref(v)) }
          else if (inHeapVarMap && !inVarMap) {
            HeapLocal[Post](heapVarMap.ref(v))
          } else {
            ??? // Something is wrong
          }
        DerefPointer(local)(PanicBlame("Should always be accessible"))
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
      case a @ AddrOf(AddrOfConstCast(e)) => a.rewrite(e = dispatch(e))
      case a @ AddrOf(AddrOfUniqueCast(e, _)) => a.rewrite(e = dispatch(e))
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(loc: Location[Pre]): Location[Post] = {
    implicit val o: Origin = loc.o
    loc match {
      case PointerLocation(AddrOf(local @ Local(_))) =>
        throw UnsupportedAddrOf(local)
      case other => other.rewriteDefault()
    }
  }
}
