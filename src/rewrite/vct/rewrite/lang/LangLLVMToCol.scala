package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{Expr, _}
import vct.col.origin._
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.resolve.ctx.RefLLVMFunctionDefinition
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{CurrentProgramContext, SubstituteReferences, SuccessionMap}
import vct.result.VerificationError.{SystemError, Unreachable, UserError}

import scala.collection.mutable

case object LangLLVMToCol {
  private final case class UnexpectedLLVMNode(node: Node[_])
      extends SystemError {
    override def text: String =
      context[CurrentProgramContext].map(_.highlight(node)).getOrElse(node.o)
        .messageInContext(
          "VerCors assumes this node does not occur here in llvm input."
        )
  }

  private final case class UnsupportedLoopForm(loop: LLVMLoop[_])
      extends SystemError {
    override def text: String =
      context[CurrentProgramContext].map(_.highlight(loop)).getOrElse(loop.o)
        .messageInContext(
          "VerCors assumes that LLVM-loops only have one backedge."
        )
  }

  private final case class NonConstantStructIndex(origin: Origin)
      extends UserError {
    override def code: String = "nonConstantStructIndex"

    override def text: String =
      origin.messageInContext(
        s"This struct indexing operation (getelementptr) uses a non-constant struct index which we do not support."
      )
  }

  private final case class UnsupportedArrayIndex(origin: Origin)
      extends UserError {
    override def code: String = "unsupportedArrayIndex"

    override def text: String =
      origin.messageInContext(
        s"This array-indexing operation (getelementptr) is currently not supported."
      )
  }

  private final case class UnsupportedSignExtension(sext: LLVMSignExtend[_])
      extends UserError {
    override def code: String = "unsupportedSignExtension"

    override def text: String =
      sext.o.messageInContext(
        s"Unsupported sign extension from '${sext.inputType}' to '${sext.outputType}'"
      )
  }

  private final case class UnsupportedZeroExtension(zext: LLVMZeroExtend[_])
      extends UserError {
    override def code: String = "unsupportedZeroExtension"

    override def text: String =
      zext.o.messageInContext(
        s"Unsupported zero extension from '${zext.inputType}' to '${zext.outputType}'"
      )
  }

  private final case class UnsupportedTruncate(trunc: LLVMTruncate[_])
      extends UserError {
    override def code: String = "unsupportedTruncate"

    override def text: String =
      trunc.o.messageInContext(
        s"Unsupported truncation from '${trunc.inputType}' to '${trunc.outputType}'"
      )
  }

  private final case class UnsupportedExtractValueType(o: Origin)
      extends UserError {
    override def code: String = "unsupportedExtractValueType"

    override def text: String =
      o.messageInContext(s"Unsupported aggregate-type used in extractvalue")
  }

  private final case class UnsupportedMemset(memset: LLVMMemset[_])
      extends UserError {
    override def code: String = "unsupportedMemset"

    override def text: String =
      memset.o.messageInContext(s"Unsupported memset operation")
  }

  private final case class UnreachableReached(
      unreachable: LLVMBranchUnreachable[_]
  ) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      unreachable.blame.blame(UnreachableReachedError(unreachable))
  }

  private val pallasResArgPermOrigin: Origin = Origin(Seq(
    PreferredName(Seq("resArg context")),
    LabelContext("Generated context for resArg"),
  ))

  private val overflowOpInitializerOrigin: Origin = Origin(Seq(
    PreferredName(Seq("initTuple")),
    LabelContext("Generated initializer for arith-op with overflow"),
  ))

  private val nondetValueOrigin: Origin = Origin(Seq(
    LabelContext("Getter for nondeterministic value"),
    PreferredName(Seq("getNondet")),
  ))

  // TODO: This should be replaced with the correct blames!
  private object InvalidGEP
      extends PanicBlame("Invalid use of getelementpointer!")
}

case class LangLLVMToCol[Pre <: Generation](rw: LangSpecificToCol[Pre])
    extends LazyLogging {

  import LangLLVMToCol._

  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  private val llvmFunctionMap
      : SuccessionMap[LLVMFunctionDefinition[Pre], Procedure[Post]] =
    SuccessionMap()
  private val specFunctionMap
      : SuccessionMap[LLVMSpecFunction[Pre], Function[Post]] = SuccessionMap()
  private val globalVariableMap
      : SuccessionMap[LLVMGlobalVariable[Pre], HeapVariable[Post]] =
    SuccessionMap()
  private val structMap: SuccessionMap[LLVMTStruct[Pre], Class[Post]] =
    SuccessionMap()
  private val structFieldMap
      : SuccessionMap[(LLVMTStruct[Pre], Int), InstanceField[Post]] =
    SuccessionMap()

  private val globalVariableInferredType
      : mutable.HashMap[LLVMGlobalVariable[Pre], Type[Pre]] = mutable.HashMap()
  private val localVariableInferredType
      : mutable.HashMap[Variable[Pre], Type[Pre]] = mutable.HashMap()
  private val loopBlocks: mutable.ArrayBuffer[LLVMBasicBlock[Pre]] = mutable
    .ArrayBuffer()
  private val elidedBackEdges: mutable.Set[LabelDecl[Pre]] = mutable.Set()

  /** Encoding the pallas specification requires changing the types of some
    * variables:
    *   - bool --> resource
    *   - ptr fracT --> TRational
    */
  private val typeSubstitutions: mutable.Map[Variable[Pre], Type[Pre]] = mutable
    .Map()

  private val wrappersInAssume: mutable.Set[LLVMFunctionDefinition[Pre]] =
    mutable.Set()

  // Keeps track if the currently transformed function is a wrapper-function.
  private val inWrapperFunction: ScopedStack[Boolean] = ScopedStack()

  // Local variables that were allocated using alloca in the current function.
  private val allocaVars: ScopedStack[mutable.Set[Variable[Pre]]] =
    ScopedStack()

  // When a loop is constructed, this keeps track of the variables that
  // are assigned using store-instructions.
  private val assignedInLoop: ScopedStack[mutable.Set[Variable[Pre]]] =
    ScopedStack()

  // Initializer-functions for the tuples that are returned by the llvm intrinsics
  // for arithmetic operaitons with overflows.
  private val overflowOpInitializers
      : mutable.Map[LLVMTStruct[Pre], Procedure[Post]] = mutable.Map()

  // Functions that are used to get a nondeterministic value of a given type.
  // Used to encode the unreachable-instruction.
  private val nondetGetters: mutable.Map[Type[Post], Function[Post]] = mutable
    .Map()

  // Return type of the LLVMFunction that is currently rewritten
  private val funcRetType: ScopedStack[Type[Post]] = ScopedStack()

  def gatherPallasTypeSubst(program: Program[Pre]): Unit = {
    // Get all variables that are assigned a new type directly
    program.collect {
      // Resource
      case Assign(Local(Ref(v)), LLVMPerm(_, _)) =>
        typeSubstitutions(v) = TResource()
      case Assign(Local(Ref(v)), LLVMStar(Ref(left), Ref(right))) =>
        typeSubstitutions(v) = TResource()
        typeSubstitutions(left) = TResource()
        typeSubstitutions(right) = TResource()
      // Rational
      case LLVMFracOf(Ref(v), _, _) => typeSubstitutions(v) = TRational()
      case LLVMPerm(_, Ref(v)) => typeSubstitutions(v) = TRational()
      // Tuples
      case op: LLVMArithOpWithOverflow[Pre] =>
        op.target match {
          case Local(Ref(v)) =>
            v.t match {
              case sT: LLVMTStruct[Pre] =>
                typeSubstitutions(v) = TTuple(
                  Seq(sT.elements(0), sT.elements(1))
                )
            }
        }
    }

    // Propagate the new types across trivial assignments.
    // TODO: Improve this. This does not cover all cases and is slow.
    //  It would be nicer to do this in a separate pass before the type-inference.
    var oldSize = -1
    while (typeSubstitutions.size != oldSize) {
      oldSize = typeSubstitutions.size
      program.collect {
        case Assign(Local(Ref(targetVar)), Local(Ref(sourceVar))) =>
          typeSubstitutions.get(sourceVar)
            .foreach(sT => typeSubstitutions(targetVar) = sT)
      }
    }
  }

  def gatherBackEdges(program: Program[Pre]): Unit = {
    program.collect { case loop: LLVMLoop[Pre] =>
      elidedBackEdges.add(loop.header.decl)
    }
  }

  def gatherTypeHints(program: Program[Pre]): Unit = {
    // TODO: We also need to do something where we only keep structurally distinct types
    def moreSpecific(self: Type[Pre], other: Type[Pre]): Boolean = {
      (self, other) match {
        case (a, b) if a == b => false
        case (LLVMTPointer(None), _) => false
        case (LLVMTPointer(Some(TVoid())), _) => false
        case (TPointer(TVoid(), _), _) => false
        case (_, LLVMTPointer(None)) => true
        case (_, LLVMTPointer(Some(TVoid()))) => true
        case (_, TPointer(TVoid(), _)) => true
        case (LLVMTPointer(Some(a)), LLVMTPointer(Some(b))) =>
          moreSpecific(a, b)
        case (LLVMTPointer(Some(a)), TPointer(b, _)) => moreSpecific(a, b)
        case (TPointer(a, _), LLVMTPointer(Some(b))) => moreSpecific(a, b)
        case (TPointer(a, _), TPointer(b, _)) => moreSpecific(a, b)
        case (LLVMTStruct(_, _, a), LLVMTStruct(_, _, b)) =>
          a.headOption.exists(ta => b.exists(tb => moreSpecific(ta, tb)))
        case (LLVMTStruct(_, _, _), _) => true
        case (LLVMTArray(_, a), LLVMTArray(_, b)) => moreSpecific(a, b)
        case (LLVMTArray(_, _), _) => true
        case _ => false
      }
    }

    // TODO: This sorting is non-stable which might cause nondeterministic bugs if there's something wrong with moreSpecific
    def findMostSpecific(
        types: mutable.ArrayBuffer[Type[Pre]]
    ): Option[Type[Pre]] = {
      types.map(Some(_)).reduce[Option[Type[Pre]]] { (a, b) =>
        (a, b) match {
          case (None, _) | (_, None) => None
          case (Some(a), Some(b))
              // TODO: This should be removed as soon as we have proper contracts we load from LLVM instead of mixing PVL and LLVM. Comparing in Post is really bad
              if a == b || rw.dispatch(a) == rw.dispatch(b) ||
                moreSpecific(a, b) =>
            Some(a)
          case (Some(a), Some(b)) if moreSpecific(b, a) => Some(b)
          case _ => None
        }
      }
    }

    def findSuperType(a: Type[Pre], b: Type[Pre]): Option[Type[Pre]] = {
      (a, b) match {
        case (a, b) if a == b => Some(a)
        case (LLVMTPointer(None), _) => Some(a)
        case (LLVMTPointer(Some(TVoid())), _) => Some(a)
        case (TPointer(TVoid(), _), _) => Some(a)
        case (_, LLVMTPointer(None)) => Some(b)
        case (_, LLVMTPointer(Some(TVoid()))) => Some(b)
        case (_, TPointer(TVoid(), _)) => Some(b)
        case (LLVMTPointer(Some(a)), LLVMTPointer(Some(b))) =>
          Some(LLVMTPointer(findSuperType(a, b)))
        case (LLVMTPointer(Some(a)), TPointer(b, _)) =>
          Some(LLVMTPointer(findSuperType(a, b)))
        case (TPointer(a, _), LLVMTPointer(Some(b))) =>
          Some(LLVMTPointer(findSuperType(a, b)))
        case (TPointer(a, _), TPointer(b, _)) =>
          Some(LLVMTPointer(findSuperType(a, b)))
        case _ => None
      }
    }

    def findAcceptable(types: mutable.ArrayBuffer[Type[Pre]]): Type[Pre] = {
      types.reduce { (a, b) =>
        findSuperType(a, b).getOrElse(
          throw Unreachable(
            s"Failed to find super type of '$a' and '$b' even though both sides should be pointers"
          )
        )
      }
    }

    class TypeGuess(
        val depends: mutable.Set[Object] = mutable.Set(),
        val dependents: mutable.Set[Object] = mutable.Set(),
        val getGuesses: mutable.ArrayBuffer[Unit => Type[Pre]] = mutable
          .ArrayBuffer(),
        var currentType: Type[Pre],
    ) {
      def add(dependencies: Set[Object], inferType: Unit => Type[Pre]): Unit = {
        depends.addAll(dependencies)
        getGuesses.addOne(inferType)
      }

      def update(): Boolean = {
        val guessBuffer = getGuesses.map(_())
        val superType = findMostSpecific(guessBuffer)
        if (superType.isEmpty) {
          val newType = findAcceptable(guessBuffer)
          val updated = currentType != newType
          currentType = newType
          updated
        } else {
          val updated = currentType != superType.get
          currentType = superType.get
          updated
        }
      }
    }

    val typeGuesses: mutable.HashMap[Object, TypeGuess] = mutable.HashMap()

    def findDependencies(expr: Expr[Pre]): Set[Object] = {
      expr.collect {
        case Local(Ref(v)) => v
        case LLVMPointerValue(Ref(g)) => g
        // These two below probably don't do anything
        case v: Variable[Pre] => v
        case v: LLVMGlobalVariable[Pre] => v
      }.toSet
    }

    def replaceWithGuesses(
        value: Expr[Pre],
        dependencies: Set[Object],
    ): Expr[Pre] = {
      val subMap = dependencies.filter(typeGuesses.contains).collect {
        case v: Variable[Pre] if typeGuesses(v).currentType != v.t =>
          (v, new Variable[Pre](typeGuesses(v).currentType)(v.o))
        case v: LLVMGlobalVariable[Pre]
            if typeGuesses(v).currentType != v.variableType =>
          (
            v,
            new LLVMGlobalVariable[Pre](
              typeGuesses(v).currentType,
              v.value,
              v.constant,
            )(v.o),
          )
      }
      if (subMap.isEmpty) { value }
      else { SubstituteReferences(subMap.toMap).dispatch(value) }
    }

    def getVariable(expr: Expr[Pre]): Option[Object] = {
      expr match {
        case Local(Ref(v)) => Some(v)
        case LLVMPointerValue(Ref(g)) => Some(g)
        case _ => None
      }
    }

    def addTypeGuess(
        obj: Object,
        dependencies: Set[Object],
        inferType: Unit => Type[Pre],
    ): Unit =
      typeGuesses
        .getOrElseUpdate(obj, new TypeGuess(currentType = inferType(())))
        .add(dependencies, inferType)

    // TODO: This could be made more generic and also work with Assign nodes
    program.collect {
      case func: LLVMFunctionDefinition[Pre] =>
        func.args.zipWithIndex.foreach { case (a, i) =>
          addTypeGuess(
            a,
            Set.empty,
            _ => func.importedArguments.map(_(i).t).getOrElse(a.t),
          )
        }
        // If the function has an sret-argument, infer type from that.
        func.returnInParam match {
          case Some((idx, t)) => addTypeGuess(func.args(idx), Set.empty, _ => t)
          case None =>
        }
      case alloc: LLVMAllocA[Pre] =>
        addTypeGuess(
          alloc.variable.decl,
          Set.empty,
          _ => LLVMTPointer(Some(alloc.allocationType)),
        )
      case gep: LLVMGetElementPointer[Pre] =>
        getVariable(gep.pointer).foreach(v =>
          addTypeGuess(v, Set.empty, _ => LLVMTPointer(Some(gep.structureType)))
        )
      case load: LLVMLoad[Pre] =>
        getVariable(load.pointer).foreach(v =>
          addTypeGuess(
            v,
            Set(load.variable.decl),
            _ =>
              LLVMTPointer(Some(
                typeGuesses.get(load.variable.decl).map(_.currentType)
                  .getOrElse(load.variable.decl.t)
              )),
          )
        )
        addTypeGuess(load.variable.decl, Set.empty, _ => load.variable.decl.t)
      case store: LLVMStore[Pre] =>
        val dependencies = findDependencies(store.value)
        getVariable(store.pointer).foreach(v =>
          addTypeGuess(
            v,
            dependencies,
            _ =>
              LLVMTPointer(
                Some(replaceWithGuesses(store.value, dependencies).t)
              ),
          )
        )
        getVariable(store.value).foreach(v =>
          getVariable(store.pointer).foreach(p =>
            addTypeGuess(
              v,
              Set(p),
              _ =>
                typeGuesses.get(p).map(_.currentType) match {
                  case Some(LLVMTPointer(Some(innerType))) => innerType
                  case _ => store.value.t
                },
            )
          )
        )
      case inv: LLVMFunctionInvocation[Pre] =>
        val calledFunc = inv.ref.decl
        val isWrapperFunc = calledFunc.pallasExprWrapperFor.isDefined
        calledFunc.importedArguments.getOrElse(calledFunc.args).zipWithIndex
          .foreach { case (arg, idx) =>
            // Infer type of variable that is used as arg in function call
            // from function definition
            getVariable(inv.args(idx))
              .foreach(v => addTypeGuess(v, Set.empty, _ => arg.t))

            // If the invoked function is a wrapper function, we infer the
            // type of the pointer-typed argument from the call-site.
            if (isWrapperFunc && arg.t.asPointer.isDefined) {
              val dependencies = findDependencies(inv.args(idx))
              // TODO: Check if this can be simplified I.e. the expression
              //  should almost always be resolvable with getVariable
              addTypeGuess(
                arg,
                dependencies,
                _ => replaceWithGuesses(inv.args(idx), dependencies).t,
              )
            }
          }
      // Propagate pointer types across \old
      case Assign(Local(Ref(tVar)), LLVMOld(Ref(sVar))) =>
        addTypeGuess(
          tVar,
          Set(sVar),
          _ => typeGuesses.get(sVar).map(_.currentType).getOrElse(tVar.t),
        )
    }

    typeGuesses.foreachEntry((k, v) =>
      v.depends.filter(typeGuesses.contains)
        .foreach(typeGuesses.get(_).foreach(_.dependents.add(k)))
    )
    val updateQueue = mutable.ArrayDeque.from(typeGuesses.keys)

    while (updateQueue.nonEmpty) {
      val obj = updateQueue.removeHead()
      val guess = typeGuesses(obj)
      if (guess.update()) { updateQueue.appendAll(guess.dependents) }
    }

    typeGuesses.foreachEntry((e, t) =>
      e match {
        case v: Variable[Pre] => localVariableInferredType(v) = t.currentType
        case v: LLVMGlobalVariable[Pre] =>
          globalVariableInferredType(v) = t.currentType
      }
    )
  }

  def gatherWrappersInAssume(program: Program[Pre]): Unit = {
    program.collect {
      case Assume(LLVMFunctionInvocation(Ref(f), _, _, _))
          if f.pallasExprWrapperFor.isDefined =>
        wrappersInAssume.add(f);
    }
  }

  def rewriteLocal(local: LLVMLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    Local(rw.succ(local.ref.get.decl))
  }

  /** Return the type of the given variable after applying type-substitutions
    * and type-inference.
    */
  private def getLocalVarType(v: Variable[Pre]): Type[Pre] = {
    typeSubstitutions.getOrElse(v, localVariableInferredType.getOrElse(v, v.t))
  }

  def rewriteLocalVariable(v: Variable[Pre]): Unit = {
    implicit val o: Origin = v.o
    rw.variables.succeed(v, new Variable[Post](rw.dispatch(getLocalVarType(v))))
  }

  def rewriteFunctionDef(func: LLVMFunctionDefinition[Pre]): Unit = {
    implicit val o: Origin = func.o
    val procedure = rw.labelDecls.scope {
      allocaVars.having(mutable.Set[Variable[Pre]]()) {
        val newArgs = func.importedArguments.getOrElse(func.args).map { it =>
          // Apply type-inference to function-arguments
          new Variable(
            rw.dispatch(localVariableInferredType.getOrElse(it, it.t))
          )(it.o)
        }
        val argList =
          rw.variables.collect {
            func.args.zip(newArgs).foreach { case (a, b) =>
              rw.variables.succeed(a, b)
            }
          }._1
        // If func returns its result in an argument, this is a reference to that argument
        val cRetArg =
          func.returnInParam match {
            case Some((idx, _)) => Some(argList(idx).ref)
            case None => None
          }
        val isWrapper = func.pallasExprWrapperFor.isDefined
        val returnT =
          if (isWrapper && !wrappersInAssume.contains(func)) {
            TResource[Post]()
          } else {
            rw.dispatch(func.importedReturnType.getOrElse(func.returnType))
          }
        funcRetType.having(returnT) {
          rw.globalDeclarations.declare(
            new Procedure[Post](
              returnType = returnT,
              args = argList,
              outArgs = Nil,
              typeArgs = Nil,
              body =
                inWrapperFunction.having(isWrapper) {
                  func.functionBody match {
                    case None => None
                    case Some(functionBody) =>
                      if (func.pure)
                        Some(GotoEliminator(functionBody match {
                          case scope: Scope[Pre] => scope;
                          case other => throw UnexpectedLLVMNode(other)
                        }).eliminate())
                      else
                        Some(rw.dispatch(functionBody))
                  }
                },
              contract =
                func.contract match {
                  case contract: VCLLVMFunctionContract[Pre] =>
                    rw.dispatch(contract.data.get)
                  case contract: PallasFunctionContract[Pre] =>
                    extendContractWithSretPerm(contract.content, cRetArg)
                },
              pure = func.pure,
              pallasWrapper = isWrapper,
              pallasFunction = true,
            )(func.blame)
          )
        }
      }
    }
    llvmFunctionMap.update(func, procedure)
  }

  /** If the function returns in an argument, extend the contract with
    * context_everywhere \pointer(retArg, 1, write);
    */
  private def extendContractWithSretPerm(
      c: ApplicableContract[Pre],
      retArg: Option[Ref[Post, Variable[Post]]],
  ): ApplicableContract[Post] = {
    retArg match {
      case Some(arg) =>
        implicit val o: Origin = pallasResArgPermOrigin
        c.rewrite(contextEverywhere =
          (Local(arg) !== Null()) &* Perm(
            AmbiguousLocation(DerefPointer(Local(arg))(LLVMSretPerm))(
              LLVMSretPerm
            ),
            WritePerm[Post](),
          ) &* rw.dispatch(c.contextEverywhere)
        )
      case None => rw.dispatch(c)
    }
  }

  def rewriteAmbiguousFunctionInvocation(
      inv: LLVMAmbiguousFunctionInvocation[Pre]
  ): Invocation[Post] = {
    implicit val o: Origin = inv.o
    inv.ref.get.decl match {
      case func: LLVMFunctionDefinition[Pre] =>
        new ProcedureInvocation[Post](
          ref = new LazyRef[Post, Procedure[Post]](llvmFunctionMap(func)),
          args = inv.args.map(rw.dispatch),
          givenMap = inv.givenMap.map { case (Ref(v), e) =>
            (rw.succ(v), rw.dispatch(e))
          },
          yields = inv.yields.map { case (e, Ref(v)) =>
            (rw.dispatch(e), rw.succ(v))
          },
          outArgs = Seq.empty,
          typeArgs = Seq.empty,
        )(inv.blame)
      case func: LLVMSpecFunction[Pre] =>
        new FunctionInvocation[Post](
          ref = new LazyRef[Post, Function[Post]](specFunctionMap(func)),
          args = inv.args.map(rw.dispatch),
          givenMap = inv.givenMap.map { case (Ref(v), e) =>
            (rw.succ(v), rw.dispatch(e))
          },
          yields = inv.yields.map { case (e, Ref(v)) =>
            (rw.dispatch(e), rw.succ(v))
          },
          typeArgs = Seq.empty,
        )(inv.blame)
    }

  }

  def rewriteFunctionInvocation(
      inv: LLVMFunctionInvocation[Pre]
  ): ProcedureInvocation[Post] = {
    implicit val o: Origin = inv.o

    new ProcedureInvocation[Post](
      ref = new LazyRef[Post, Procedure[Post]](llvmFunctionMap(inv.ref.decl)),
      args = inv.args.zipWithIndex.map {
        // TODO: This is really ugly, can we do the type inference in the resolve step and then do coercions to do this?
        case (a, i) =>
          val requiredType = localVariableInferredType
            .getOrElse(inv.ref.decl.args(i), inv.ref.decl.args(i).t)
          val givenType = getInferredType(a)
          if (
            givenType != requiredType && givenType.asPointer.isDefined &&
            requiredType.asPointer.isDefined
          ) {
            PointerCast(
              rw.dispatch(a),
              rw.dispatch(requiredType),
              rw.c.sizeOf(givenType.asPointer.get.element, inv.o),
              rw.c.sizeOf(requiredType.asPointer.get.element, inv.o),
            )
          } else { rw.dispatch(a) }
      },
      givenMap = inv.givenMap.map { case (Ref(v), e) =>
        (rw.succ(v), rw.dispatch(e))
      },
      yields = inv.yields.map { case (e, Ref(v)) =>
        (rw.dispatch(e), rw.succ(v))
      },
      outArgs = Seq.empty,
      typeArgs = Seq.empty,
    )(inv.blame)
  }

  def rewriteGlobal(decl: LLVMGlobalSpecification[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl.data.get.foreach { decl =>
      rw.globalDeclarations.declare(decl match {
        case function: LLVMSpecFunction[Pre] =>
          val rwFunction =
            new Function[Post](
              rw.dispatch(function.returnType),
              rw.variables.collect { function.args.foreach(rw.dispatch) }._1,
              rw.variables.collect { function.typeArgs.foreach(rw.dispatch) }
                ._1,
              function.body match {
                case Some(body) => Some(rw.dispatch(body))
                case None => None
              },
              rw.dispatch(function.contract),
              function.inline,
              function.threadLocal,
            )(function.blame)
          specFunctionMap.update(function, rwFunction)
          rwFunction
        case other => throw UnexpectedLLVMNode(other)
      })
    }
  }

  def rewriteFunctionPointer(
      pointer: LLVMFunctionPointerValue[Pre]
  ): LLVMFunctionPointerValue[Post] = {
    implicit val o: Origin = pointer.o
    new LLVMFunctionPointerValue[Post](value =
      new LazyRef[Post, GlobalDeclaration[Post]](llvmFunctionMap(
        pointer.value.decl.asInstanceOf[LLVMFunctionDefinition[Pre]]
      ))
    )
  }

  def rewriteStruct(t: LLVMTStruct[Pre]): Unit = {
    val LLVMTStruct(name, packed, elements) = t
    val casts =
      rw.c.sizeOf(t, t.o) +: rw.c.getFirstTypes(t).map { ct =>
        rw.c.sizeOf(ct, t.o)
      }
    val newStruct =
      new ByValueClass[Post](
        Seq(),
        rw.classDeclarations.collect {
          elements.zipWithIndex.foreach { case (fieldType, idx) =>
            structFieldMap((t, idx)) =
              new InstanceField(rw.dispatch(fieldType), flags = Nil)(
                fieldType.o
              )
            rw.classDeclarations.declare(structFieldMap((t, idx)))
          }
        }._1,
        t.packed,
        casts,
      )(t.o.withContent(TypeName("struct")))

    rw.globalDeclarations.declare(newStruct)
    structMap(t) = newStruct
  }

  def rewriteGlobalVariable(decl: LLVMGlobalVariable[Pre]): Unit = {
    // TODO: Handle the initializer
    // TODO: Include array and vector bounds somehow
    globalVariableInferredType.getOrElse(decl, decl.variableType) match {
      case struct: LLVMTStruct[Pre] =>
        rewriteStruct(struct)
        globalVariableMap.update(
          decl,
          rw.globalDeclarations.declare(
            new HeapVariable[Post](
              new TNonNullPointer[Post](
                new TByValueClass[Post](
                  new DirectRef[Post, Class[Post]](structMap(struct)),
                  Seq(),
                )(struct.o),
                None,
              )(struct.o),
              decl.value.map(rw.dispatch),
            )(decl.o)
          ),
        )
      case array: LLVMTArray[Pre] =>
        globalVariableMap.update(
          decl,
          rw.globalDeclarations.declare(
            new HeapVariable[Post](
              new TPointer[Post](rw.dispatch(array.elementType), None)(array.o),
              None,
            )(decl.o)
          ),
        )
      case vector: LLVMTVector[Pre] =>
        globalVariableMap.update(
          decl,
          rw.globalDeclarations.declare(
            new HeapVariable[Post](
              new TPointer[Post](rw.dispatch(vector.elementType), None)(
                vector.o
              ),
              None,
            )(decl.o)
          ),
        )
      case int: LLVMTInt[Pre] =>
        globalVariableMap.update(
          decl,
          rw.globalDeclarations
            .declare(new HeapVariable[Post](rw.dispatch(int), None)(decl.o)),
        )
      case _ => ???
    }
  }

  def rewritePointerChain(
      pointer: Expr[Post],
      t: Type[Pre],
      indices: Seq[Expr[Pre]],
  )(implicit o: Origin): Expr[Post] = {
    if (indices.isEmpty) { return pointer }
    t match {
      case struct: LLVMTStruct[Pre] => {
        if (!structMap.contains(struct)) { rewriteStruct(struct) }
        indices.head match {
          case value: LLVMIntegerValue[Pre] =>
            rewritePointerChain(
              Deref[Post](
                pointer,
                structFieldMap.ref((struct, value.value.intValue)),
              )(InvalidGEP),
              struct.elements(value.value.intValue),
              indices.tail,
            )
          case value: IntegerValue[Pre] =>
            rewritePointerChain(
              Deref[Post](
                pointer,
                structFieldMap.ref((struct, value.value.intValue)),
              )(InvalidGEP),
              struct.elements(value.value.intValue),
              indices.tail,
            )
          case _ => throw NonConstantStructIndex(o)
        }
      }
      case array: LLVMTArray[Pre] => ???
      case vector: LLVMTVector[Pre] => ???
    }
  }

  private def derefUntil(
      pointer: Expr[Post],
      currentType: Type[Pre],
      untilType: Type[Pre],
  ): Option[(Expr[Post], Type[Pre])] = {
    implicit val o: Origin = pointer.o
    currentType match {
      case _ if currentType == untilType => Some((AddrOf(pointer), currentType))
      case LLVMTPointer(None) =>
        Some((pointer, LLVMTPointer[Pre](Some(untilType))))
      case LLVMTPointer(Some(inner)) if inner == untilType =>
        Some((pointer, currentType))
      case LLVMTPointer(Some(LLVMTArray(numElements, elementType))) => {
        derefUntil(
          PointerSubscript[Post](
            DerefPointer(pointer)(pointer.o),
            IntegerValue(BigInt(0)),
          )(pointer.o),
          elementType,
          untilType,
        ).map { case (expr, inner) =>
          (expr, LLVMTPointer[Pre](Some(LLVMTArray(numElements, inner))))
        }
      }
      case LLVMTArray(numElements, elementType) => {
        derefUntil(
          PointerSubscript[Post](pointer, IntegerValue(BigInt(0)))(pointer.o),
          elementType,
          untilType,
        ).map { case (expr, inner) =>
          (expr, LLVMTArray[Pre](numElements, inner))
        }
      }
      case LLVMTPointer(Some(LLVMTVector(numElements, elementType))) => {
        derefUntil(
          PointerSubscript[Post](
            DerefPointer(pointer)(pointer.o),
            IntegerValue(BigInt(0)),
          )(pointer.o),
          elementType,
          untilType,
        ).map { case (expr, inner) =>
          (expr, LLVMTPointer[Pre](Some(LLVMTVector(numElements, inner))))
        }
      }
      case LLVMTVector(numElements, elementType) => {
        derefUntil(
          PointerSubscript[Post](pointer, IntegerValue(BigInt(0)))(pointer.o),
          elementType,
          untilType,
        ).map { case (expr, inner) =>
          (expr, LLVMTVector[Pre](numElements, inner))
        }
      }
      case LLVMTPointer(Some(struct @ LLVMTStruct(name, packed, elements))) => {
        derefUntil(
          Deref[Post](
            DerefPointer(pointer)(pointer.o),
            structFieldMap.ref((struct, 0)),
          )(pointer.o),
          elements.head,
          untilType,
        ).map { case (expr, inner) =>
          (
            expr,
            LLVMTPointer[Pre](Some(
              LLVMTStruct(name, packed, inner +: elements.tail)
            )),
          )
        }
      }
      case struct @ LLVMTStruct(name, packed, elements) => {
        derefUntil(
          Deref[Post](pointer, structFieldMap.ref((struct, 0)))(pointer.o),
          elements.head,
          untilType,
        ).map { case (expr, inner) =>
          (expr, LLVMTStruct[Pre](name, packed, inner +: elements.tail))
        }
      }
      // Save the expensive check for last. This check is for when we're mixing PVL and LLVM types
      // TODO: This check should be removed ASAP when we get real LLVM contracts since comparing types in Post is bad
      case LLVMTPointer(Some(inner))
          if rw.dispatch(inner) == rw.dispatch(untilType) =>
        Some((pointer, currentType))
      case _ => None
    }
  }

  def rewriteGetElementPointer(gep: LLVMGetElementPointer[Pre]): Expr[Post] = {
    implicit val o: Origin = gep.o
    val t = gep.structureType
    t match {
      case integer: LLVMTInt[Pre] =>
        // Encode simple array-indexing
        if (gep.indices.size != 1) { throw UnsupportedArrayIndex(o) }
        // Check that the inferred type of the pointer matches the return=-type of gep
        val ptrType = {
          gep.pointer match {
            case Local(Ref(v)) if localVariableInferredType.get(v).isDefined =>
              localVariableInferredType.get(v).get
            case _ => gep.pointer.t
          }
        }
        ptrType match {
          case LLVMTPointer(Some(t2)) if t == t2 => // All is fine
          case _ => throw UnsupportedArrayIndex(o)
        }
        PointerAdd[Post](
          rw.dispatch(gep.pointer),
          rw.dispatch(gep.indices.head),
        )(InvalidGEP)
      case struct: LLVMTStruct[Pre] => {
        // TODO: We don't support variables in GEP yet and this just assumes all the indices are integer constants
        // TODO: Use an actual Blame
        // Acquire the actual struct through a PointerAdd
        gep.pointer.t match {
          case LLVMTPointer(None) =>
            val structPointer =
              DerefPointer(
                PointerAdd(
                  rw.dispatch(gep.pointer),
                  rw.dispatch(gep.indices.head),
                )(InvalidGEP)
              )(InvalidGEP)
            AddrOf(rewritePointerChain(structPointer, struct, gep.indices.tail))
          case LLVMTPointer(Some(inner)) if inner == t =>
            val structPointer =
              DerefPointer(
                PointerAdd(
                  rw.dispatch(gep.pointer),
                  rw.dispatch(gep.indices.head),
                )(InvalidGEP)
              )(InvalidGEP)
            AddrOf(rewritePointerChain(structPointer, struct, gep.indices.tail))
          case LLVMTPointer(Some(_)) =>
            val pointerInferredType = getInferredType(gep.pointer)
            val (pointer, inferredType) = derefUntil(
              rw.dispatch(gep.pointer),
              pointerInferredType,
              t,
            ).getOrElse((
              PointerCast(
                rw.dispatch(gep.pointer),
                rw.dispatch(t),
                rw.c.sizeOf(gep.pointer.t.asPointer.get.element, gep.o),
                rw.c.sizeOf(t.asPointer.get.element, gep.o),
              ),
              t,
            ))
            val structPointer =
              DerefPointer(
                PointerAdd(pointer, rw.dispatch(gep.indices.head))(InvalidGEP)
              )(InvalidGEP)
            val ret = AddrOf(
              rewritePointerChain(structPointer, struct, gep.indices.tail)
            )
            ret
        }
      }
      case array: LLVMTArray[Pre] => ???
      case vector: LLVMTVector[Pre] => ???
    }
    // Deref might not be the correct thing to use here since technically the pointer is only dereferenced in the load or store instruction
  }

  def derefStructIndexChain(value: Expr[Post], t: Type[Pre], indices: Seq[Int])(
      implicit o: Origin
  ): Expr[Post] = {
    if (indices.isEmpty) { return value }
    t match {
      case struct: LLVMTStruct[Pre] =>
        if (!structMap.contains(struct)) { rewriteStruct(struct) }
        val idx = indices.head
        derefStructIndexChain(
          Deref[Post](value, structFieldMap.ref((struct, idx)))(InvalidGEP),
          struct.elements(idx),
          indices.tail,
        )
      case _ => throw UnsupportedExtractValueType(o)
    }
  }

  def rewriteExtractValue(extrVal: LLVMExtractValue[Pre]): Expr[Post] = {
    implicit val o: Origin = extrVal.o

    extrVal.value match {
      case Local(Ref(v))
          if getLocalVarType(v).isInstanceOf[TTuple[Pre]] &&
            extrVal.indices.size == 1 =>
        // Special case for results of arithmetic ops with overflow-flag (encoded as tuple)
        TupGet[Post](rw.dispatch(extrVal.value), extrVal.indices.head)
      case _ =>
        derefStructIndexChain(
          rw.dispatch(extrVal.value),
          extrVal.aggregateType,
          extrVal.indices,
        )
    }
  }

  def rewriteSignExtend(sext: LLVMSignExtend[Pre]): Expr[Post] = {
    implicit val o: Origin = sext.o
    // As long as we don't support integers as bitvectors this is mostly a no-op
    (sext.inputType, sext.outputType) match {
      // Both sides should become TInt
      case (LLVMTInt(_), LLVMTInt(_)) => rw.dispatch(sext.value)
      // Since this is sign extension we want all bits to be 1 if the value was true hence -1
      case (TBool(), LLVMTInt(_)) =>
        Select(rw.dispatch(sext.value) === tt, const(-1), const(0))
      case (_, _) => throw UnsupportedSignExtension(sext)
    }
  }

  def rewriteZeroExtend(zext: LLVMZeroExtend[Pre]): Expr[Post] = {
    implicit val o: Origin = zext.o
    // As long as we don't support integers as bitvectors this is mostly a no-op
    (zext.inputType, zext.outputType) match {
      // Both sides should become TInt
      case (LLVMTInt(_), LLVMTInt(_)) => rw.dispatch(zext.value)
      case (TBool(), LLVMTInt(_)) =>
        Select(rw.dispatch(zext.value) === tt, const(1), const(0))
      case (_, _) => throw UnsupportedZeroExtension(zext)
    }
  }

  def rewriteTruncate(trunc: LLVMTruncate[Pre]): Expr[Post] = {
    implicit val o: Origin = trunc.o
    // As long as we don't support integers as bitvectors this is mostly a no-op
    (trunc.inputType, trunc.outputType) match {
      // Both sides should become TInt
      case (LLVMTInt(_), LLVMTInt(_)) => rw.dispatch(trunc.value)
      case (LLVMTInt(_), TBool()) =>
        Select(rw.dispatch(trunc.value) === const(0), ff, tt)
      case (_, _) => throw UnsupportedTruncate(trunc)
    }
  }

  def rewriteFloatExtend(fpext: LLVMFloatExtend[Pre]): Expr[Post] = {
    implicit val o: Origin = fpext.o
    CastFloat(rw.dispatch(fpext.value), rw.dispatch(fpext.t))
  }

  private def getInitializerForArithOpWithOverflow(
      structT: LLVMTStruct[Pre]
  ): Procedure[Post] = {
    if (!overflowOpInitializers.contains(structT)) {
      implicit val o: Origin = overflowOpInitializerOrigin
      val (resT, flagT) =
        structT match {
          case LLVMTStruct(_, _, Seq(res: LLVMTInt[Pre], flag: TBool[Pre])) =>
            (res, flag)
        }
      val resArg =
        new Variable[Post](rw.dispatch(resT))(o.where(name = "resArg"))
      val flagArg =
        new Variable[Post](rw.dispatch(flagT))(o.where(name = "flagArg"))
      val tupleT = TTuple(Seq(resT, flagT))

      val initializer = rw.globalDeclarations.declare {
        withResult((result: Result[Post]) => {
          val ensuresClauses = Seq(
            TupGet[Post](result, 0) === Local(resArg.ref),
            TupGet[Post](result, 1) === Local(flagArg.ref),
          )

          new Procedure[Post](
            returnType = rw.dispatch(tupleT),
            args = Seq(resArg, flagArg),
            outArgs = Nil,
            typeArgs = Nil,
            body = None,
            contract = contract[Post](
              blame = AbstractApplicable,
              ensures = UnitAccountedPredicate(foldStar(ensuresClauses)),
            ),
            pure = true,
          )(PanicBlame("Generated initializer does not raise errors"))
        })
      }
      overflowOpInitializers(structT) = initializer
    }
    overflowOpInitializers(structT)
  }

  private def rewriteArithOpWithOverflow(
      instr: LLVMArithOpWithOverflow[Pre],
      op: (Expr[Post], Expr[Post]) => Expr[Post],
  ): Statement[Post] = {
    implicit val o: Origin = instr.o
    // TODO: Do not ignore the signedness
    val targetStructT = instr.target.t match { case s: LLVMTStruct[Pre] => s }
    val initFunc = getInitializerForArithOpWithOverflow(targetStructT)
    val initCall = procedureInvocation[Post](
      blame = PanicBlame("Generated initializer does not fail"),
      ref = initFunc.ref,
      args = Seq(op(rw.dispatch(instr.left), rw.dispatch(instr.right)), ff),
    )
    val assign = Assign(rw.dispatch(instr.target), initCall)(instr.blame)
    assign
  }

  def rewriteAddWithOverflow(add: LLVMAddWithOverflow[Pre]): Statement[Post] = {
    implicit val o: Origin = add.o
    rewriteArithOpWithOverflow(add, (l, r) => l + r)
  }

  def rewriteSubWithOverflow(sub: LLVMSubWithOverflow[Pre]): Statement[Post] = {
    implicit val o: Origin = sub.o
    rewriteArithOpWithOverflow(sub, (l, r) => l - r)
  }

  def rewriteMultWithOverflow(
      mult: LLVMMultWithOverflow[Pre]
  ): Statement[Post] = {
    implicit val o: Origin = mult.o
    rewriteArithOpWithOverflow(mult, (l, r) => l * r)
  }

  def rewriteUnreachable(
      unreachable: LLVMBranchUnreachable[Pre]
  ): Statement[Post] = {
    implicit val o: Origin = unreachable.o
    val a = Assert[Post](ff)(UnreachableReached(unreachable))
    // If we are in a wrapper-function, the type needs to be set to bool.
    // The default-type of Resource causes isses in the col->viper conversion
    val t =
      if (!inWrapperFunction.isEmpty && inWrapperFunction.top) { TBool[Post]() }
      else { funcRetType.top }
    val nondetGetter = getNondetValFunc(t)
    val r = Return[Post](
      functionInvocation[Post](blame = TrueSatisfiable, ref = nondetGetter.ref)
    )
    Block(Seq(a, r))
  }

  private def getNondetValFunc(t: Type[Post]): Function[Post] = {
    if (!nondetGetters.contains(t)) {
      val getterFunc = rw.globalDeclarations.declare(
        function[Post](
          blame = AbstractApplicable,
          contractBlame = TrueSatisfiable,
          returnType = t,
        )(nondetValueOrigin)
      )
      nondetGetters(t) = getterFunc
    }
    nondetGetters(t)
  }

  private def getInferredType(e: Expr[Pre]): Type[Pre] =
    e match {
      case Local(Ref(v)) => getLocalVarType(v)
      // localVariableInferredType.getOrElse(v, e.t)
      // Making assumption here that LLVMPointerValue only contains LLVMGlobalVariables whereas LLVMGlobalVariableImpl assumes it can also contain HeapVariables
      case LLVMPointerValue(Ref(v)) =>
        globalVariableInferredType
          .getOrElse(v.asInstanceOf[LLVMGlobalVariable[Pre]], e.t)
      case res: LLVMResult[Pre] => res.t
      case DerefPointer(inner) =>
        val innerT = getInferredType(inner)
        innerT match {
          case LLVMTPointer(Some(innerPtrT)) => innerPtrT
          case _ => e.t
        }
      case _ => e.t
    }

  def rewriteStore(store: LLVMStore[Pre]): Statement[Post] = {
    implicit val o: Origin = store.o
    val pointerInferredType = getInferredType(store.pointer)
    val valueInferredType = getInferredType(store.value)
    val pointer = derefUntil(
      rw.dispatch(store.pointer),
      pointerInferredType,
      valueInferredType,
    ).map { case (pointer, typ) =>
      if (typ == pointerInferredType) { DerefPointer(pointer)(store.blame) }
      else {
        DerefPointer(PointerCast(
          pointer,
          rw.dispatch(typ),
          rw.c.sizeOf(store.pointer.t.asPointer.get.element, store.o),
          rw.c.sizeOf(typ.asPointer.get.element, store.o),
        ))(store.blame)
      }
    }.getOrElse {
      if (store.value.t.asPointer.isDefined) {
        // TODO: How do we deal with this
        ???
      } else {
        DerefPointer(PointerCast(
          rw.dispatch(store.pointer),
          TPointer(rw.dispatch(valueInferredType), None),
          rw.c.sizeOf(store.pointer.t.asPointer.get.element, store.o),
          rw.c.sizeOf(valueInferredType, store.o),
        ))(store.blame)
      }
    }
    val strippedPtr =
      pointer match {
        case DerefPointer(AddrOf(e)) => e
        case p => p
      }
    // TODO: Fix assignfailed blame
    Assign(strippedPtr, rw.dispatch(store.value))(store.blame)
  }

  def rewriteLoad(load: LLVMLoad[Pre]): Statement[Post] = {
    implicit val o: Origin = load.o
    val pointerInferredType = getInferredType(load.pointer)
    val destinationInferredType = localVariableInferredType
      .getOrElse(load.variable.decl, load.loadType)
    val (pointer, inferredType) = derefUntil(
      rw.dispatch(load.pointer),
      pointerInferredType,
      destinationInferredType,
    ).map { case (pointer, typ) => (DerefPointer(pointer)(load.blame), typ) }
      .getOrElse {
        if (destinationInferredType.asPointer.isDefined) {
          // We need to dereference before casting
          (
            PointerCast(
              DerefPointer(rw.dispatch(load.pointer))(load.blame),
              rw.dispatch(destinationInferredType),
              rw.c.sizeOf(load.pointer.t.asPointer.get.element, load.o),
              rw.c.sizeOf(destinationInferredType.asPointer.get.element, load.o),
            ),
            pointerInferredType,
          )
        } else {
          (
            DerefPointer(PointerCast(
              rw.dispatch(load.pointer),
              TPointer(rw.dispatch(destinationInferredType), None),
              rw.c.sizeOf(load.pointer.t.asPointer.get.element, load.o),
              rw.c.sizeOf(destinationInferredType, load.o),
            ))(load.blame),
            pointerInferredType,
          )
        }
      }
    assignLocal(Local(rw.succ(load.variable.decl)), pointer)
  }

  def rewriteAllocA(alloc: LLVMAllocA[Pre]): Statement[Post] = {
    implicit val o: Origin = alloc.o
    /*
    Alloca-instructions should only occur in wrapper-functions when a
    specification-function is called whose result is returned using a
    sret-argument. In these cases the initialization of the alloca is
    not needed and causes problems when converting the wrapper into
    an expression
     */
    if (!inWrapperFunction.isEmpty && inWrapperFunction.top) {
      // Skip the initialization if we are in a wrapper function.
      return Block(Seq())
    }
    allocaVars.top.add(alloc.variable.decl)

    val t =
      localVariableInferredType.getOrElse(
        alloc.variable.decl,
        LLVMTPointer(Some(alloc.allocationType)),
      ).asPointer.get.element
    val newT = rw.dispatch(t)
    val v = Local[Post](rw.succ(alloc.variable.decl))
    val elements = rw.dispatch(alloc.numElements)
    assignLocal(
      v,
      NewNonNullPointerArray[Post](newT, elements, None)(PanicBlame(
        "allocation should never fail"
      )),
    )
  }

  def rewriteMemset(memset: LLVMMemset[Pre]): Statement[Post] = {
    implicit val o: Origin = memset.o
    // Curently only memset with constant value of 0 is supported
    memset.value match {
      case LLVMIntegerValue(v, _) if v.intValue == 0 =>
      case _ => throw UnsupportedMemset(memset)
    }
    // TODO: Make this more more generic
    //  Currently, only very basic type-wrapper structs are supported (i.e. a packed struct with one integer value)
    val numBytes =
      memset.len match {
        case LLVMIntegerValue(bytes, _) => bytes
        case _ => throw UnsupportedMemset(memset)
      }
    val structType =
      memset.dest match {
        case Local(Ref(v)) =>
          v.t match {
            case LLVMTPointer(Some(s: LLVMTStruct[Pre])) => s
            case _ => throw throw UnsupportedMemset(memset)
          }
        case _ => throw UnsupportedMemset(memset)
      }
    if (!structType.packed || !(structType.elements.size == 1)) {
      throw UnsupportedMemset(memset)
    }
    structType.elements.head match {
      case LLVMTInt(bits) if (bits / 8) == numBytes =>
      case _ => throw UnsupportedMemset(memset)
    }

    // Set field of the struct to 0
    val structField = structFieldMap((structType, 0))
    Assign[Post](
      Deref[Post](
        DerefPointer(rw.dispatch(memset.dest))(memset.blame),
        structField.ref,
      )(memset.blame),
      rw.dispatch(memset.value),
    )(memset.blame)
  }

  def rewritePointerValue(pointer: LLVMPointerValue[Pre]): Expr[Post] = {
    implicit val o: Origin = pointer.o
    // Will be transformed by VariableToPointer pass
    new AddrOf[Post](
      DerefHeapVariable[Post](globalVariableMap.ref(
        pointer.value.decl.asInstanceOf[LLVMGlobalVariable[Pre]]
      ))(pointer.o)
    )
  }

  def rewriteResult(res: LLVMResult[Pre]): LLVMIntermediaryResult[Post] = {
    requireInWrapper(res)
    implicit val o: Origin = res.o
    LLVMIntermediaryResult(
      applicable =
        new LazyRef[Post, Procedure[Post]](llvmFunctionMap(res.func.decl)),
      sretArg =
        res.func.decl.returnInParam match {
          case Some((idx, _)) =>
            val oldArg = res.func.decl.args(idx)
            Some(rw.succ(oldArg))
          case None => None
        },
    )
  }

  def rewriteFracOf(fracOf: LLVMFracOf[Pre]): Statement[Post] = {
    requireInWrapper(fracOf)
    implicit val o: Origin = fracOf.o
    // fracOf(v, num, denom) --> v = num / denom.
    assignLocal(
      Local(rw.succ(fracOf.sret.decl)),
      new RatDiv[Post](rw.dispatch(fracOf.num), rw.dispatch(fracOf.denom))(
        fracOf.blame
      ),
    )
  }

  def rewritePerm(llvmPerm: LLVMPerm[Pre]): Expr[Post] = {
    requireInWrapper(llvmPerm)
    implicit val o: Origin = llvmPerm.o
    val locExpr = Local[Post](rw.succ(llvmPerm.loc.decl))
    Perm[Post](
      PointerLocation[Post](locExpr)(llvmPerm.blame),
      Local[Post](rw.succ(llvmPerm.perm.decl)),
    )
  }

  def rewritePtrBlockLength(llvmPBL: LLVMPtrBlockLength[Pre]): Expr[Post] = {
    requireInWrapper(llvmPBL)
    implicit val o: Origin = llvmPBL.o
    PointerBlockLength[Post](Local[Post](rw.succ(llvmPBL.ptr.decl)))(
      llvmPBL.blame
    )
  }

  def rewritePtrBlockOffset(llvmPBO: LLVMPtrBlockOffset[Pre]): Expr[Post] = {
    requireInWrapper(llvmPBO)
    implicit val o: Origin = llvmPBO.o
    PointerBlockOffset[Post](Local[Post](rw.succ(llvmPBO.ptr.decl)))(
      llvmPBO.blame
    )
  }

  def rewritePtrLength(llvmPL: LLVMPtrLength[Pre]): Expr[Post] = {
    requireInWrapper(llvmPL)
    implicit val o: Origin = llvmPL.o
    PointerLength[Post](Local[Post](rw.succ(llvmPL.ptr.decl)))(llvmPL.blame)
  }

  def rewriteImplies(llvmImply: LLVMImplies[Pre]): Expr[Post] = {
    requireInWrapper(llvmImply)
    implicit val o: Origin = llvmImply.o
    Implies[Post](
      Local[Post](rw.succ(llvmImply.left.decl)),
      Local[Post](rw.succ(llvmImply.right.decl)),
    )
  }

  def rewriteAnd(llvmAnd: LLVMAnd[Pre]): Expr[Post] = {
    requireInWrapper(llvmAnd)
    implicit val o: Origin = llvmAnd.o
    And[Post](
      Local[Post](rw.succ(llvmAnd.left.decl)),
      Local[Post](rw.succ(llvmAnd.right.decl)),
    )
  }

  def rewriteOr(llvmOr: LLVMOr[Pre]): Expr[Post] = {
    requireInWrapper(llvmOr)
    implicit val o: Origin = llvmOr.o
    Or[Post](
      Local[Post](rw.succ(llvmOr.left.decl)),
      Local[Post](rw.succ(llvmOr.right.decl)),
    )
  }

  def rewriteStar(llvmStar: LLVMStar[Pre]): Expr[Post] = {
    requireInWrapper(llvmStar)
    implicit val o: Origin = llvmStar.o
    Star[Post](
      Local[Post](rw.succ(llvmStar.left.decl)),
      Local[Post](rw.succ(llvmStar.right.decl)),
    )
  }

  def rewriteOld(llvmOld: LLVMOld[Pre]): Expr[Post] = {
    requireInWrapper(llvmOld)
    implicit val o: Origin = llvmOld.o
    LLVMOld[Post](rw.succ(llvmOld.v.decl))
  }

  def result(ref: RefLLVMFunctionDefinition[Pre])(
      implicit o: Origin
  ): Expr[Post] = Result[Post](llvmFunctionMap.ref(ref.decl))

  private def phiTmpVarOrigin() =
    Origin(Seq(
      PreferredName(Seq("phiTmp")),
      LabelContext(s"Generated tmp-var for phi-assignment"),
    ))

  private def phiTmpVarAssignOrigin() =
    Origin(Seq(LabelContext(s"Generated assignment to tmp-var for phi-node")))

  private def buildPhiAssignments(
      basicBlock: LLVMBasicBlock[Pre]
  ): Scope[Post] = {
    implicit val o: Origin = basicBlock.o
    // We split the phi-assignments to ensure that cases where the value
    // of a phi-node is used in an assignment to another phi-node get encoded
    // correctly.
    // I.e. we first generate a block where we assign the values of all
    // phi-assignments to temporary variables, and then a block where
    // we assign the values of the temporary variables to the actual
    // target of the phi-assignment.
    var tmpAssignments = Seq[Statement[Post]]()
    var phiAssignments = Seq[Statement[Post]]()
    var tmpVars = Seq[Variable[Post]]()
    basicBlock.phiAssignments.foreach { a =>
      a match {
        case a @ Assign(Local(Ref(targetVar)), expr) =>
          // Build temporary assignment
          val vT = rw.dispatch(getLocalVarType(targetVar))
          val tmpVar = new Variable[Post](vT)(phiTmpVarOrigin())
          tmpVars = tmpVars :+ tmpVar
          tmpAssignments =
            tmpAssignments :+ Assign(
              Local[Post](tmpVar.ref)(phiTmpVarOrigin()),
              rw.dispatch(expr),
            )(a.blame)(a.o)
          // Build assignment of tmp-var to actual var.
          phiAssignments =
            phiAssignments :+ Assign[Post](
              rw.dispatch(a.target),
              Local[Post](tmpVar.ref)(phiTmpVarOrigin()),
            )(PanicBlame("Generated assign may not fail"))(
              phiTmpVarAssignOrigin()
            )
        case _ => throw UnexpectedLLVMNode(a)
      }
    }
    val newBlock = Block[Post](tmpAssignments ++ phiAssignments)
    Scope[Post](tmpVars, newBlock)
  }

  private def blockToLabel(
      block: LLVMBasicBlock[Pre],
      isLoopLatch: Boolean = false,
  ): Statement[Post] = {
    implicit val o: Origin = block.o
    var bodyStmnts = Seq(rw.dispatch(block.body), buildPhiAssignments(block))
    // If the block is a loop-latch, we ignore the terminating goto, as this is implicitly included in the Loop
    if (!isLoopLatch) {
      bodyStmnts = bodyStmnts :+ rw.dispatch(block.terminator)
    }
    val newBody = Block[Post](bodyStmnts)

    if (elidedBackEdges.contains(block.label)) { newBody }
    else { Label(rw.labelDecls.dispatch(block.label), newBody)(block.o) }
  }

  private def countBackedges(loop: LLVMLoop[Pre]): Int = {
    loop.blocks.get.map(b =>
      b.collect {
        case Goto(Ref(lbl)) if lbl == loop.header.decl => 1
        case _ => 0
      }.sum
    ).sum
  }

  def rewriteBasicBlock(block: LLVMBasicBlock[Pre]): Statement[Post] = {
    if (loopBlocks.contains(block))
      return Block(Nil)(DiagnosticOrigin)
    if (block.loop.isEmpty) { blockToLabel(block) }
    else {
      val loop = block.loop.get
      if (countBackedges(loop) != 1) { throw UnsupportedLoopForm(loop) }
      loopBlocks.addAll(loop.blocks.get)
      // Determine which variables are assigned using store-instructions
      val assignedVars = mutable.Set[Variable[Pre]]()
      loop.blocks.getOrElse(mutable.Set.empty).foreach { b =>
        b.body.collect { case LLVMStore(_, Local(Ref(v)), _) =>
          assignedVars.add(v)
        }
      }
      Label(
        rw.labelDecls.dispatch(block.label),
        Loop(
          Block(Nil)(block.o),
          tt[Post],
          Block(Nil)(block.o),
          assignedInLoop.having(assignedVars) { rw.dispatch(loop.contract) },
          Block(
            blockToLabel(loop.headerBlock.get) +: loop.blocks.get.filterNot {
              b => b == loop.headerBlock.get || b == loop.latchBlock.get
            }.map(b => blockToLabel(b)) :+
              blockToLabel(loop.latchBlock.get, true)
          )(block.o),
        )(block.o),
      )(block.o)
    }
  }

  def rewriteLoopContract(
      llvmContract: LLVMLoopContract[Pre]
  ): LoopInvariant[Post] = {
    implicit val o: Origin = llvmContract.o
    // Add Permission for alloca-variables
    var extendedInv = rw.dispatch(llvmContract.invariant)
    allocaVars.topOption.getOrElse(mutable.Set.empty).foreach { v =>
      // If the variable is assigned to, assert write-perm, otherwise read perm
      val perm =
        if (assignedInLoop.topOption.getOrElse(mutable.Set.empty).contains(v)) {
          WritePerm[Post]()
        } else { ReadPerm[Post]() }
      extendedInv =
        Perm(
          PointerLocation[Post](Local(rw.succ(v)))(PanicBlame(
            "Generated locals always have permission"
          )),
          perm,
        ) &* extendedInv
    }
    LoopInvariant[Post](extendedInv, None)(llvmContract.blame)
  }

  /*
  Elimination works by replacing every goto with the block its referring too
  effectively transforming the CFG into a tree. More efficient restructuring algorithms but this works for now.

  This of course only works for acyclic CFGs as otherwise replacement would be infinitely recursive.
  Loop restructuring should be handled by Pallas as it has much more analytical and contextual information about
  the program.
   */
  private case class GotoEliminator(bodyScope: Scope[Pre]) extends LazyLogging {
    private val labelDeclMap: Map[LabelDecl[Pre], LLVMBasicBlock[Pre]] =
      bodyScope.body match {
        case block: Block[Pre] =>
          block.statements.map {
            case bb: LLVMBasicBlock[Pre] => (bb.label, bb)
            case other => throw UnexpectedLLVMNode(other)
          }.toMap
        case other => throw UnexpectedLLVMNode(other)
      }

    def eliminate(): Scope[Post] = {
      bodyScope match {
        case scope: Scope[Pre] =>
          Scope[Post](
            rw.variables.collect { scope.locals.foreach(rw.dispatch) }._1,
            scope.body match {
              case bodyBlock: Block[Pre] =>
                Block[Post](bodyBlock.statements.head match {
                  case label: LLVMBasicBlock[Pre] => Seq(eliminate(label))
                  case other => throw UnexpectedLLVMNode(other)
                })(scope.body.o)
              case other => throw UnexpectedLLVMNode(other)
            },
          )(scope.o)
        case other => throw UnexpectedLLVMNode(other)
      }
    }

    private def eliminate(bb: LLVMBasicBlock[Pre]): Block[Post] = {
      implicit val o: Origin = bb.o
      bb.terminator match {
        case goto: Goto[Pre] =>
          Block[Post](
            Seq(rw.dispatch(bb.body), buildPhiAssignments(bb)) ++
              eliminate(labelDeclMap(goto.lbl.decl)).statements
          )
        case ret: Return[Pre] =>
          Block[Post](
            Seq(rw.dispatch(bb.body), buildPhiAssignments(bb), rw.dispatch(ret))
          )
        case branch: Branch[Pre] =>
          Block[Post](Seq(
            rw.dispatch(bb.body),
            buildPhiAssignments(bb),
            eliminate(branch),
          ))
        case unr: LLVMBranchUnreachable[Pre] =>
          Block[Post](
            Seq(rw.dispatch(bb.body), buildPhiAssignments(bb), rw.dispatch(unr))
          )
        case other => throw UnexpectedLLVMNode(other)
      }
    }

    private def eliminate(branch: Branch[Pre]): Branch[Post] = {
      implicit val o: Origin = branch.o
      Branch[Post](branch.branches.map(bs =>
        (
          rw.dispatch(bs._1),
          bs._2 match {
            case goto: Goto[Pre] => eliminate(labelDeclMap(goto.lbl.decl))
            case other => throw UnexpectedLLVMNode(other)
          },
        )
      ))
    }
  }

  private def requireInWrapper(node: Node[_]): Unit = {
    if (inWrapperFunction.isEmpty || !inWrapperFunction.top) {
      throw UnexpectedLLVMNode(node)
    }
  }

  def structType(t: LLVMTStruct[Pre]): Type[Post] = {
    // TODO: Remove this. The check for the golbalDeclarations is required because
    //  the function-type is called in the post-comparisson in derefUntil, outside of
    //  a scope. Once that is removed, this should no longe be required.
    if (!rw.globalDeclarations.isEmpty) { // <--
      if (!structMap.contains(t)) { rewriteStruct(t) }
    }
    val targetClass = new LazyRef[Post, Class[Post]](structMap(t))
    TByValueClass[Post](targetClass, Seq())(t.o)
  }

  def pointerType(t: LLVMTPointer[Pre]): Type[Post] =
    t.innerType match {
      case Some(innerType) => TPointer[Post](rw.dispatch(innerType), None)(t.o)
      case None => TPointer[Post](TVoid(), None)(t.o)
    }

  def arrayType(t: LLVMTArray[Pre]): Type[Post] =
    TPointer(rw.dispatch(t.elementType), None)(t.o)

  def vectorType(t: LLVMTVector[Pre]): Type[Post] =
    TPointer(rw.dispatch(t.elementType), None)(t.o)
}
