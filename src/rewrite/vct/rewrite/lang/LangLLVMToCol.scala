package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.expr.op.BinOperatorTypes
import vct.col.ast.serialize.LlvmStructDeclaration
import vct.col.ast.{Expr, _}
import vct.col.origin._
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.resolve.ctx.RefLLVMFunctionDefinition
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.typerules.CoercionUtils
import vct.col.util.AstBuildHelpers._
import vct.col.util.{CurrentProgramContext, SubstituteReferences, SuccessionMap}
import vct.result.VerificationError.{SystemError, Unreachable, UserError}
import vct.rewrite.lang.LangSpecificToCol.InvalidPointerComparison

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

  private final case class UnsupportedMemcpy(memcpy: LLVMMemcpy[_])
      extends UserError {
    override def code: String = "unsupportedMemcpy"

    override def text: String =
      memcpy.o.messageInContext(s"Unsupported memcpy operation")
  }

  private final case class InvalidPointerEquality(
      o: Origin,
      lt: Type[_],
      rt: Type[_],
  ) extends UserError {
    override def code: String = "invalidPointerEquality"

    override def text: String =
      o.messageInContext(
        s"Expected types `$lt` and `$rt` to be interchangeable, there might be too little information for type inference"
      )
  }

  private final case class UnsupportedWrapperReturnT(
      wrapper: LLVMFunctionDefinition[_]
  ) extends UserError {
    override def code: String = "unsupportedWrapperRetT"

    override def text: String =
      wrapper.o.messageInContext(s"Unsupported ghost-type")
  }

  private final case class UnreachableReached(
      unreachable: LLVMBranchUnreachable[_]
  ) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      unreachable.blame.blame(UnreachableReachedError(unreachable))
  }

  private final case class PointerSubscriptToInsufficientPermissionBlame(
      blame: Blame[PointerSubscriptError]
  ) extends Blame[ClassDerefError] {
    override def blame(error: ClassDerefError): Unit = {
      blame.blame(PointerInsufficientPermission(error match {
        case ClassNull(node) => node
        case InsufficientPermission(node) => node
      }))
    }
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

}

case class LangLLVMToCol[Pre <: Generation](rw: LangSpecificToCol[Pre])
    extends LazyLogging {

  import LangLLVMToCol._

  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  private val llvmFunctionMap
      : SuccessionMap[LLVMFunctionDefinition[Pre], Procedure[Post]] =
    SuccessionMap()
  private val llvmPredicateMap: SuccessionMap[LLVMFunctionDefinition[
    Pre
  ], LLVMPredicateDefinition[Post]] = SuccessionMap()
  private val specFunctionMap
      : SuccessionMap[LLVMSpecFunction[Pre], Function[Post]] = SuccessionMap()
  private val globalVariableMap
      : SuccessionMap[LLVMGlobalVariable[Pre], HeapVariable[Post]] =
    SuccessionMap()
  private val structMap
      : SuccessionMap[LLVMStructDeclaration[Pre], Class[Post]] = SuccessionMap()
  private val structFieldMap
      : SuccessionMap[(LLVMStructDeclaration[Pre], Int), InstanceField[Post]] =
    SuccessionMap()

  private val globalVariableInferredType
      : mutable.HashMap[LLVMGlobalVariable[Pre], Type[Pre]] = mutable.HashMap()
  private val localVariableInferredType
      : mutable.HashMap[Variable[Pre], Type[Pre]] = mutable.HashMap()
  private val inferredReturnType
      : mutable.HashMap[LLVMFunctionDefinition[Pre], Type[Pre]] = mutable
    .HashMap()
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

  /** Used to rewrite the byval-attributes of LLVM functions. Byval-arguments
    * are rewritten into non-pointer types. In the body of the function an
    * intermediary variable is inserted that is assigned the address of the
    * argument. This intermediary is marked as the successor of the argument, so
    * that references in the body point to the intermediary variable. However,
    * this does not work for contracts because the intermediary is not in scope.
    * We therefore manually add an addrOf to uses of the variable in the
    * contract. This relies on the assumption that the contract only consists of
    * calls to wrapper functions where the variable is passed as a Local.
    */
  // old_arg --> new_arg
  private val byValArgs
      : ScopedStack[SuccessionMap[Variable[Pre], Variable[Post]]] =
    ScopedStack()

  // Keeps track if rewrite is currently in a contract
  private val inContract: ScopedStack[Boolean] = ScopedStack()

  // Keeps track if the currently transformed function is a definition of specifications
  // (i.e. a wrapper-function or a predicate definition).
  private val inSpecDefFunction: ScopedStack[Boolean] = ScopedStack()

  // If the function that is currently being rewritten is a wrapper with
  // a sret-argument, this stack points to that sret-argument.
  private val currentWrapperSret
      : ScopedStack[Option[LLVMFunctionArgument[Pre]]] = ScopedStack()

  // Tracks if the current function has ghost-arguments that have the
  // byval-attribute.
  private val byvalGhostArgs: ScopedStack[Set[Variable[Pre]]] = ScopedStack()

  // Local variables that were allocated using alloca in the current function.
  private val allocaVars: ScopedStack[mutable.Set[Variable[Pre]]] =
    ScopedStack()

  // When a loop is constructed, this keeps track of the variables that
  // are assigned using store-instructions.
  private val assignedInLoop: ScopedStack[mutable.Set[Variable[Pre]]] =
    ScopedStack()
  private val usedInLoop: ScopedStack[mutable.Set[Variable[Pre]]] =
    ScopedStack()

  // Tracks the label of the current loop
  private val currentLoopLabel: ScopedStack[LabelDecl[Pre]] = ScopedStack()

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

  private var heapVariables: Seq[Variable[Pre]] = Seq()

  private val heapVariableSucc
      : SuccessionMap[Variable[Pre], LocalHeapVariable[Post]] = SuccessionMap()

  def gatherPallasTypeSubst(program: Program[Pre]): Unit = {
    // Get all variables that are assigned a new type directly
    program.collect {
      // Resource
      case Assign(Local(Ref(v)), LLVMPerm(_, _)) =>
        typeSubstitutions(v) = TResource()
      case Assign(
            Local(Ref(v)),
            LLVMStar(Local(Ref(left)), Local(Ref(right))),
          ) =>
        typeSubstitutions(v) = TResource()
        typeSubstitutions(left) = TResource()
        typeSubstitutions(right) = TResource()
      case Assign(
            Local(Ref(v)),
            LLVMImplies(Local(Ref(left)), Local(Ref(right))),
          ) if typeSubstitutions.get(right).contains(TResource[Pre]()) =>
        typeSubstitutions(v) = TResource()
      case Assign(Local(Ref(v)), inv: LLVMFunctionInvocation[Pre])
          if inv.ref.decl.isPredicate =>
        typeSubstitutions(v) = TResource()
      // Rational
      case LLVMFracOf(Ref(v), _, _) => typeSubstitutions(v) = TRational()
      case LLVMPerm(_, Local(Ref(v))) => typeSubstitutions(v) = TRational()
      // Tuples
      case op: LLVMArithOpWithOverflow[Pre] =>
        op.target match {
          case Local(Ref(v)) =>
            v.t match {
              case LLVMTStruct(Ref(sDecl)) =>
                typeSubstitutions(v) = TTuple(
                  Seq(sDecl.elements.head.t, sDecl.elements(1).t)
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
        case Assign(
              Local(Ref(v)),
              LLVMImplies(Local(Ref(left)), Local(Ref(right))),
            ) if typeSubstitutions.get(right).contains(TResource[Pre]()) =>
          typeSubstitutions(v) = TResource()
      }
    }
  }

  // We're lifitng all AllocA'd variables to heap variables
  def gatherHeapVariables(program: Program[Pre]): Unit = {
    heapVariables =
      program.collect { case LLVMAllocA(Ref(v), _, n) => Seq(v) }.flatten
  }

  def gatherBackEdges(program: Program[Pre]): Unit = {
    program.collect { case loop: LLVMLoop[Pre] =>
      elidedBackEdges.add(loop.header.decl)
    }
  }

  def gatherTypeHints(program: Program[Pre]): Unit = {

    // Touch all references to struct declarations within LLVMTStruct-types.
    // Otherwise, the type-equality does not work because the LazyRef might not have been resolved.
    program.collect { case sType: LLVMTStruct[Pre] => sType.ref.decl }

    // TODO: We also need to do something where we only keep structurally distinct types
    // Returns if self is more specific than other
    def moreSpecific(self: Type[Pre], other: Type[Pre]): Boolean = {
      (self, other) match {
        case (a, b) if a == b => false
        // While the int is "more specific" we want keep the TBool since it is semantically more what we want
        case (TBool(), LLVMTInt(_)) => true
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
        // Define a named struct to be more specific than a structurally equivalent literal struct.
        case (LLVMTStruct(Ref(s1)), LLVMTStruct(Ref(s2)))
            if moreSpecificLitStruct(s1, s2) =>
          true
        case (LLVMTStruct(Ref(s1)), LLVMTStruct(Ref(s2)))
            if moreSpecificLitStruct(s2, s1) =>
          false
        case (a: LLVMTStruct[Pre], b: LLVMTStruct[Pre]) =>
          a.ref.decl.elements.headOption.exists(ta =>
            b.ref.decl.elements.exists(tb => moreSpecific(ta.t, tb.t))
          )
        case (LLVMTStruct(_), _) => true
        case (LLVMTArray(_, a), LLVMTArray(_, b)) => moreSpecific(a, b)
        case (LLVMTArray(_, _), _) => true
        case _ => false
      }
    }

    // Returns true if other is a literal struct type and self if a structurally
    // equivalent non-literal struct.
    def moreSpecificLitStruct(
        self: LLVMStructDeclaration[Pre],
        other: LLVMStructDeclaration[Pre],
    ): Boolean = {
      !self.isLiteral && other.isLiteral && self.packed == other.packed &&
      self.elements == other.elements && self.sizeInBits == other.sizeInBits
    }

    // TODO: This sorting is non-stable which might cause nondeterministic bugs if there's something wrong with moreSpecific
    def findMostSpecific(
        types: mutable.ArrayBuffer[Type[Pre]]
    ): Option[Type[Pre]] = {
      types.map(Some(_)).reduce[Option[Type[Pre]]] { (a, b) =>
        (a, b) match {
          case (None, _) | (_, None) => None
          // TODO: This can be removed as soon as we have proper contracts for LLVM
          case (Some(a), Some(b)) if pvlLLVMEqual(a, b) => Some(a)
          case (Some(a), Some(b)) if moreSpecific(a, b) => Some(a)
          case (Some(a), Some(b)) if moreSpecific(b, a) => Some(b)
          case _ => None
        }
      }
    }

    // TODO: This should be simplified once the support for mixing PVL and LLVM is no longer needed
    // Defines which LLVM and PVL types are considered equal when LLVM and PVL are mixed
    def pvlLLVMEqual(a: Type[Pre], b: Type[Pre]): Boolean = {
      def isVoidPtr(t: Type[Pre]): Boolean = {
        t match {
          case LLVMTPointer(None) => true
          case LLVMTPointer(Some(TVoid())) => true
          case TPointer(TVoid(), _) => true
          case _ => false
        }
      }

      (a, b) match {
        case (t1, t2) if t1 == t2 => true
        case (LLVMTStruct(Ref(s1)), LLVMTStruct(Ref(s2))) =>
          s1 ==
            s2 // Required because equality is false if refs are not evaluated
        case (TInt(), LLVMTInt(_)) => true
        case (LLVMTInt(_), TInt()) => true
        case (TChar(), LLVMTInt(_)) => true
        case (LLVMTInt(_), TChar()) => true
        case (TFloat(_, _), LLVMTFloat(_)) => true
        case (LLVMTFloat(_), TFloat(_, _)) => true
        case (p1, p2) if isVoidPtr(p1) && isVoidPtr(p2) => true
        case (LLVMTPointer(Some(t1)), LLVMTPointer(Some(t2))) =>
          pvlLLVMEqual(t1, t2)
        case (LLVMTPointer(Some(t1)), TPointer(t2, _)) => pvlLLVMEqual(t1, t2)
        case (TPointer(t1, _), LLVMTPointer(Some(t2))) => pvlLLVMEqual(t1, t2)
        case _ => false
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
      types.reduceLeft { (a, b) =>
        findSuperType(a, b).getOrElse(
          throw Unreachable(
            s"Failed to find super type of '$a' and '$b' even though both sides should be pointers"
          )
        )
      }
    }

    class TypeGuess(
        val depends: mutable.Set[Object] = mutable.LinkedHashSet(),
        val dependents: mutable.Set[Object] = mutable.LinkedHashSet(),
        val getGuesses: mutable.ArrayBuffer[Unit => Seq[Type[Pre]]] = mutable
          .ArrayBuffer(),
        var currentType: Type[Pre],
    ) {
      var nextType: Option[Type[Pre]] = None

      def add(
          dependencies: Set[Object],
          inferType: Unit => Seq[Type[Pre]],
      ): Unit = {
        depends.addAll(dependencies)
        getGuesses.addOne(inferType)
      }

      def update(): Boolean = {
        if (nextType.isDefined)
          return false;
        val guessBuffer = getGuesses.flatMap(_(()))
        val superType = findMostSpecific(guessBuffer)
        if (superType.isEmpty) {
          val newType = findAcceptable(guessBuffer)
          val updated = currentType != newType
          nextType = Some(newType)
          updated
        } else {
          val updated = currentType != superType.get
          nextType = Some(superType.get)
          updated
        }
      }

      def next(): Unit = {
        currentType = nextType.getOrElse(currentType)
        nextType = None
      }
    }

    val typeGuesses: mutable.LinkedHashMap[Object, TypeGuess] = mutable
      .LinkedHashMap()

    // Given a LLVMResult, find all variables in other clauses of the correpsonding contract that
    // are assigned the value of LLVMResult.
    def findResultUses(expr: Expr[Pre], target: Expr[Pre]): Set[Object] = {
      expr match {
        case LLVMResult(Ref(pFunc)) =>
          pFunc.contract.collect { case LLVMWrapperInvocation(Ref(wF), _) =>
            val vars = wF.functionBody.get.collect {
              case Assign(t @ Local(Ref(tVar)), LLVMResult(_)) if t != target =>
                tVar
            }
            vars
          }.flatten.toSet
        case _ => Set.empty
      }
    }

    // TODO: We could extend this so that a LLVMResult requires the same type for all uses in the contract of a function!?
    def findDependencies(expr: Expr[Pre]): Set[Object] = {
      expr.collect {
        case Local(Ref(v)) => v
        case LLVMPointerValue(Ref(g)) => g
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

    // Returns variable and functions to strip and "rewrap" the type
    def getVariablePossiblyWrapped(
        expr: Expr[Pre]
    ): Option[(Object, Type[Pre] => Type[Pre], Type[Pre] => Type[Pre])] =
      expr match {
        case Local(Ref(v)) => Some((v, t => t, t => t))
        case LLVMPointerValue(Ref(g)) => Some((g, t => t, t => t))
        case DerefPointer(p) =>
          getVariablePossiblyWrapped(p).map { case (v, strip, wrap) =>
            (
              v,
              { t: Type[Pre] => strip(t).asPointer.get.element },
              { t: Type[Pre] => LLVMTPointer(Some(wrap(t))) },
            )
          }
        // case _ => None
      }

    def addTypeGuess(
        obj: Object,
        dependencies: Set[Object],
        inferType: Unit => Seq[Type[Pre]],
    ): Unit = {
      typeGuesses
        .getOrElseUpdate(obj, new TypeGuess(currentType = inferType(()).head))
        .add(dependencies, inferType)
    }

    // TODO: This could be made more generic and also work with Assign nodes
    program.collect {
      case Assign(target, value)
          if target.t.isInstanceOf[LLVMTPointer[Pre]] ||
            value.t.isInstanceOf[LLVMTPointer[Pre]] =>
        getVariable(target).foreach(v => {
          val rUses = findResultUses(value, target)
          val dependencies = findDependencies(value).union(rUses)
          addTypeGuess(
            v,
            dependencies,
            _ =>
              Seq(replaceWithGuesses(value, dependencies).t, value.t) ++
                // When the value contains a LLVMResult, we add guesses to make
                // sure that the type is consistent with the inferred type of
                // other uses of LLVMResult in other contract clauses
                rUses.filter(typeGuesses.contains)
                  .map(v => typeGuesses.get(v).get.currentType).toSeq,
          )
        })
      case contr: PallasFunctionContract[Pre] =>
        (contr.llvmGivenArgs ++ contr.llvmYieldsArgs).filter(_.isByVal)
          .foreach { case arg =>
            addTypeGuess(
              arg.v,
              Set.empty,
              _ => Seq(LLVMTPointer(arg.byValType)),
            )
          }
      case func: LLVMFunctionDefinition[Pre] =>
        func.args.zipWithIndex.foreach { case (a, i) =>
          addTypeGuess(
            a,
            Set.empty,
            _ => Seq(func.importedArguments.map(_(i).t).getOrElse(a.t), a.t),
          )
        }
        // If arguments have the byval-attribute, infer type from that
        func.llvmArgs.filter(_.isByVal).foreach { case arg =>
          addTypeGuess(arg.v, Set.empty, _ => Seq(LLVMTPointer(arg.byValType)))
        }

        // If the function has an sret-argument, infer type from that.
        func.sretArg match {
          case Some(retArg) =>
            addTypeGuess(
              retArg.v,
              Set.empty,
              _ => Seq(LLVMTPointer(retArg.sretType)),
            )
          case None =>
        }
      case alloc: LLVMAllocA[Pre] =>
        addTypeGuess(alloc.variable.decl, Set.empty, _ => Seq(alloc.returnType))
      case gep: LLVMGetElementPointer[Pre] =>
        getVariable(gep.pointer).foreach(v =>
          addTypeGuess(
            v,
            Set.empty,
            _ => Seq(LLVMTPointer(Some(gep.structureType))),
          )
        )
      case load: LLVMLoad[Pre] =>
        getVariable(load.pointer).foreach(v =>
          addTypeGuess(
            v,
            Set(load.variable.decl),
            _ =>
              Seq(
                LLVMTPointer(Some(
                  typeGuesses.get(load.variable.decl).map(_.currentType)
                    .getOrElse(load.variable.decl.t)
                )),
                LLVMTPointer(Some(load.variable.decl.t)),
              ),
          )
        )
        addTypeGuess(load.variable.decl, Set.empty, _ => Seq(load.loadType))
        // We don't want to override loads of a primitive type (we might not have permission to load more than the first field)
        if (load.loadType.asPointer.isDefined) {
          val dependencies = findDependencies(load.pointer)
          addTypeGuess(
            load.variable.decl,
            dependencies,
            _ =>
              Seq(
                replaceWithGuesses(load.pointer, dependencies).t
                  .asInstanceOf[LLVMTPointer[Pre]].innerType
                  .getOrElse(load.variable.decl.t),
                load.variable.decl.t,
                load.pointer.t.asInstanceOf[LLVMTPointer[Pre]].innerType
                  .getOrElse(load.variable.decl.t),
              ),
          )
        }
      case store: LLVMStore[Pre] =>
        val dependencies = findDependencies(store.value)
        getVariable(store.pointer).foreach(v =>
          addTypeGuess(
            v,
            dependencies,
            _ =>
              Seq(
                LLVMTPointer(
                  Some(replaceWithGuesses(store.value, dependencies).t)
                ),
                LLVMTPointer(Some(store.value.t)),
              ),
          )
        )
        getVariable(store.value)
          .foreach(v => addTypeGuess(v, Set.empty, _ => Seq(store.value.t)))

        // We don't want to override stores of a primitive type (storing more than the first field is changing the semantics)
        if (store.value.t.asPointer.isDefined) {
          getVariable(store.value).foreach(v =>
            getVariable(store.pointer).foreach(p =>
              addTypeGuess(
                v,
                Set(p),
                _ =>
                  Seq(
                    typeGuesses.get(p).map(_.currentType) match {
                      case Some(LLVMTPointer(Some(innerType))) => innerType
                      case _ => store.value.t
                    },
                    store.value.t,
                  ),
              )
            )
          )
        }
      case inv: LLVMFunctionInvocation[Pre] =>
        val calledFunc = inv.ref.decl
        calledFunc.importedArguments.getOrElse(calledFunc.args).zipWithIndex
          .foreach { case (arg, idx) =>
            // Infer type of variable that is used as arg in function call
            // from function definition
            if (inv.args(idx).t.asPointer.isDefined) {
              getVariable(inv.args(idx))
                .foreach(v => addTypeGuess(v, Set.empty, _ => Seq(arg.t)))
            }
          }
      case inv: LLVMWrapperInvocation[Pre] =>
        val calledFunc = inv.ref.decl
        calledFunc.argsWithoutSret.map(_.v).zip(inv.callArgs).foreach {
          case (defArg, invArg) =>
            // Infer type of variable that is used as an argument in the invocation
            // from the wrapper definition
            if (invArg.t.asPointer.isDefined) {
              getVariable(invArg)
                .foreach(v => addTypeGuess(v, Set.empty, _ => Seq(defArg.t)))
            }

            if (defArg.t.asPointer.isDefined) {
              // Infer the type of the argument in the wrapper-definition
              // from the call-site.
              val dependencies = findDependencies(invArg)
              addTypeGuess(
                defArg,
                dependencies,
                _ => Seq(replaceWithGuesses(invArg, dependencies).t, invArg.t),
              )

              getVariablePossiblyWrapped(invArg)
                .foreach { case (v, strip, wrap) =>
                  addTypeGuess(
                    v,
                    Set(defArg),
                    _ =>
                      Seq(
                        wrap(
                          typeGuesses.get(defArg).map(_.currentType)
                            .getOrElse(defArg.t)
                        ),
                        wrap(invArg.t),
                      ),
                  )
                }
            }
        }
      // Propagate pointer types across \old
      case Assign(Local(Ref(tVar)), LLVMOld(Local(Ref(sVar)))) =>
        addTypeGuess(
          tVar,
          Set(sVar),
          _ =>
            Seq(
              typeGuesses.get(sVar).map(_.currentType).getOrElse(tVar.t),
              tVar.t,
            ),
        )
    }

    typeGuesses.foreachEntry((k, v) =>
      v.depends.filter(typeGuesses.contains)
        .foreach(typeGuesses.get(_).foreach(_.dependents.add(k)))
    )
    var nextQueue = mutable.ArrayDeque.from(typeGuesses.keys)
    var updateQueue = mutable.ArrayDeque[Object]()

    while (nextQueue.nonEmpty) {
      val temp = updateQueue
      updateQueue = nextQueue
      nextQueue = temp
      while (updateQueue.nonEmpty) {
        val obj = updateQueue.removeHead()
        val guess = typeGuesses(obj)
        if (guess.update()) { nextQueue.appendAll(guess.dependents) }
      }
      typeGuesses.keys.foreach(typeGuesses(_).next())
    }

    typeGuesses.foreachEntry((e, t) =>
      e match {
        case v: Variable[Pre] => localVariableInferredType(v) = t.currentType
        case v: LLVMGlobalVariable[Pre] =>
          globalVariableInferredType(v) = t.currentType
      }
    )

    // For external functions that return a pointer, we try to infer their type from the provided contract.
    program.foreach {
      case f: LLVMFunctionDefinition[Pre]
          if f.functionBody.isEmpty && f.returnType.asPointer.isDefined =>
        val infTypes = program.collect {
          case Assign(Local(Ref(tVar)), LLVMResult(Ref(f)))
              if localVariableInferredType.contains(tVar) =>
            localVariableInferredType(tVar)
        }
        // This might be too strict in some cases
        val rType = findMostSpecific(infTypes.to(ArrayBuffer))
        if (rType.isDefined) { inferredReturnType(f) = rType.get }
      case _ =>
    }
  }

  def gatherWrappersInAssume(program: Program[Pre]): Unit = {
    program.collect { case Assume(LLVMWrapperInvocation(Ref(f), _)) =>
      wrappersInAssume.add(f);
    }
  }

  def rewriteLocal(local: Local[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    val v = local.ref.decl
    if (
      (inSpecDefFunction.isEmpty || !inSpecDefFunction.top) &&
      heapVariables.contains(v)
    ) { HeapLocal(heapVariableSucc.ref(v)) }
    else {
      if (inContract.nonEmpty && inContract.top && byValArgs.top.contains(v)) {
        // Case where we are in a contract and v is a byval-argument
        // In this case we cannot access the generated intermediary-var,
        // so an AddrOf is added manually
        AddrOf(Local(byValArgs.top.ref(v)))
      } else if (byvalGhostArgs.nonEmpty && byvalGhostArgs.top.contains(v)) {
        // Case where v is a ghost-arg with the byval-attribute
        // In this case, we also do not have an intermediary-var to use
        AddrOf(Local(rw.succ(v)))
      } else { Local(rw.succ(v)) }
    }
  }

  def rewriteNamedLocal(local: LLVMLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    val v = local.ref.get.decl
    // Keep this in sync with rewriteLocal!!!!
    if (
      (inSpecDefFunction.isEmpty || !inSpecDefFunction.top) &&
      heapVariables.contains(v)
    ) { HeapLocal(heapVariableSucc.ref(v)) }
    else {
      if (inContract.nonEmpty && inContract.top && byValArgs.top.contains(v)) {
        // Case where we are in a contract and v is a byval-argument
        AddrOf(Local(byValArgs.top.ref(v)))
      } else if (byvalGhostArgs.nonEmpty && byvalGhostArgs.top.contains(v)) {
        AddrOf(Local(rw.succ(v)))
      } else { Local(rw.succ(v)) }
    }
  }

  /** Return the type of the given variable after applying type-substitutions
    * and type-inference.
    */
  private def getLocalVarType(v: Variable[Pre]): Type[Pre] = {
    typeSubstitutions.getOrElse(v, localVariableInferredType.getOrElse(v, v.t))
  }

  def rewriteLocalVariable(v: Variable[Pre]): Unit = {
    implicit val o: Origin = v.o
    // Need to check for wrapper functions since there alloca is skipped
    if (
      (!inSpecDefFunction.isEmpty && inSpecDefFunction.top) ||
      !heapVariables.contains(v)
    ) {
      rw.variables
        .succeed(v, new Variable[Post](rw.dispatch(getLocalVarType(v))))
    }
  }

  private def getArgType(v: Variable[Pre], isByVal: Boolean): Type[Pre] = {
    // Apply regular type inference
    val inferredT = localVariableInferredType.getOrElse(v, v.t)
    // If the argument has the byval attribute, remove ptr from its type
    if (isByVal)
      inferredT.asPointer.get.element
    else
      inferredT
  }

  private def addByValIntermediaries(
      intermediaries: Seq[(Variable[Post], Variable[Post])],
      body: Statement[Post],
  ): Scope[Post] = {
    Scope[Post](
      intermediaries.map(_._1),
      Block(intermediaries.map { case (iVar, arg) =>
        Assign(
          Local[Post](iVar.ref)(arg.o),
          AddrOf[Post](Local[Post](arg.ref)(arg.o))(arg.o),
        )(PanicBlame("Generated byval-assign should not fail."))(arg.o)
      } :+ body)(body.o),
    )(body.o)
  }

  def rewriteFunctionDef(func: LLVMFunctionDefinition[Pre]): Unit = {
    implicit val o: Origin = func.o

    if (func.isPredicate) {
      rewritePredicateDef(func)
      return
    }

    checkSretInWrapperValid(func)
    val sretGhostWrapper = func.isGhostWrapper && func.sretArg.nonEmpty

    // If the function has a contract that is marked as assumed, drop the body.
    val assumeBody =
      func.contract match {
        case c: PallasFunctionContract[Pre] if c.assumed => true
        case _ => false
      }
    if (assumeBody && func.functionBody.isDefined) {
      val fName = func.o.getPreferredNameOrElse().ucamel
      logger.warn(s"Assuming contract-compliance for function $fName")
    }

    val bvGhostArgs =
      func.contract match {
        case c: PallasFunctionContract[Pre] =>
          (c.llvmYieldsArgs ++ c.llvmGivenArgs).filter(_.isByVal).map(_.v).toSet
        case _ => Set.empty[Variable[Pre]]
      }

    val wrapperSretArg =
      if (sretGhostWrapper) { func.sretArg }
      else { None }
    val procedure = rw.labelDecls.scope {
      byvalGhostArgs.having(bvGhostArgs) {
        allocaVars.having(mutable.Set[Variable[Pre]]()) {
          currentWrapperSret.having(wrapperSretArg) {
            val bvArgs = func.byValArgs.map(_.v).toSet

            // For ghost-wrappers, we skip the sret-arg
            val llvmArgs =
              if (func.isGhostWrapper) { func.argsWithoutSret }
              else { func.llvmArgs }
            // If imported arguments are provided, the types of the new args
            // are taken from there.
            val oldArgs = func.importedArguments.getOrElse(llvmArgs.map(_.v))
            val newArgs = oldArgs.map { it =>
              new Variable(rw.dispatch(getArgType(it, bvArgs.contains(it))))(
                it.o
              )
            }
            val bvArgMap = SuccessionMap[Variable[Pre], Variable[Post]]()
            val argList =
              rw.variables.collect {
                llvmArgs.zip(newArgs).foreach { case (oldArg, newArg) =>
                  // For the byval-arguments we do not register the successor.
                  // Later, an intermediary variable is introduced that will be used as the successor.
                  if (!oldArg.isByVal) {
                    rw.variables.succeed(oldArg.v, newArg)
                  } else {
                    rw.variables.declare(newArg)
                    bvArgMap.update(oldArg.v, newArg)
                  }
                }
              }._1
            // If func returns its result in an argument, this is a reference to that argument
            val isWrapper = func.isWrapper || func.isGhostWrapper
            val returnT = rewriteFunctionReturnT(func)

            // For all byval-args, create an intermediary var
            val byValIntermediaries = getByValIntermediaries(llvmArgs, newArgs)

            funcRetType.having(returnT) {
              byValArgs.having(bvArgMap) {
                rw.globalDeclarations.declare(
                  new Procedure[Post](
                    returnType = returnT,
                    args = argList,
                    outArgs = Nil,
                    typeArgs = Nil,
                    body =
                      if (assumeBody) { None }
                      else {
                        inSpecDefFunction.having(isWrapper) {
                          func.functionBody.map { functionBody =>
                            val rewrittenBody =
                              if (func.pure) {
                                GotoEliminator(functionBody match {
                                  case scope: Scope[Pre] => scope;
                                  case other => throw UnexpectedLLVMNode(other)
                                }).eliminate()
                              } else { rw.dispatch(functionBody) }
                            addWrapperSretScope(addByValIntermediaries(
                              byValIntermediaries,
                              rewrittenBody,
                            ))
                          }
                        }
                      },
                    contract =
                      func.contract match {
                        case contract: VCLLVMFunctionContract[Pre] =>
                          rw.dispatch(contract.data.get)
                        case contract: PallasFunctionContract[Pre] =>
                          rewritePallasFunctionContract(
                            contract,
                            func.sretArg.map(a => rw.succ(a.v)),
                          )
                      },
                    pure = func.pure,
                    pallasWrapper = isWrapper,
                    pallasFunction = true,
                  )(func.blame)
                )
              }
            }
          }
        }
      }
      // }
    }
    llvmFunctionMap.update(func, procedure)
  }

  // Check that wrapper-functions only have a sret-argument that is supported.
  private def checkSretInWrapperValid(
      func: LLVMFunctionDefinition[Pre]
  ): Unit = {
    if (func.sretArg.isEmpty || (!func.isWrapper && !func.isGhostWrapper))
      return

    // sret not allowed on non-ghost wrappers
    if (func.isWrapper) { throw UnsupportedWrapperReturnT(func); }

    // Ghost-wrappers only allow sequences as sret-arguments
    func.sretArg.get.sretType.get match {
      case TSeq(_) => // Ok
      case _ => // NotOk
        throw UnsupportedWrapperReturnT(func);
    }
  }

  // For ghost-wrappers that have a sret-argument, the arg is removed from
  // the argument-list. Instead, the function body is wrapped in a scope that
  // declares the corresponding variable as a local.
  // Additionally, an intermediary variable is added that is assigned the
  // address of the sret-local so that the types still match.
  private def addWrapperSretScope(body: Statement[Post]): Statement[Post] = {
    if (currentWrapperSret.isEmpty || currentWrapperSret.top.isEmpty) {
      return body;
    }
    val oldSret = currentWrapperSret.top.get
    val (newV, iVar) =
      rw.variables.collect {
        // Variable that used to be the sret-arg
        val v = new Variable(rw.dispatch(oldSret.sretType.get))(oldSret.v.o)
        // Intermediary that is assigned &oldVar
        val intermediary = new Variable(TPointer(v.t, None))(oldSret.v.o)

        rw.variables.declare(v)
        rw.variables.succeed(oldSret.v, intermediary)
        (v, intermediary)
      }._2
    Scope[Post](
      Seq(newV, iVar),
      Block(Seq(
        Assign(
          Local[Post](iVar.ref)(oldSret.o),
          AddrOf[Post](Local[Post](newV.ref)(oldSret.o))(oldSret.o),
        )(PanicBlame("Generated sret-assign should not fail."))(oldSret.o),
        body,
      ))(body.o),
    )(body.o)
  }

  private def rewriteArgList(
      args: Seq[LLVMFunctionArgument[Pre]]
  ): Seq[Variable[Post]] = {
    // For byval-args that are regular (i.e. no ghost arguments), intermediaries will be inserted that serve as
    // the new successor. So we do not register the successor for these here.
    rw.variables.collect {
      args.foreach { a =>
        val newArg = new Variable(rw.dispatch(getArgType(a.v, a.isByVal)))(a.o)
        if (!a.isByVal) { rw.variables.succeed(a.v, newArg) }
        else { rw.variables.declare(newArg) }
      }
    }._1
  }

  private def rewriteFunctionReturnT(
      f: LLVMFunctionDefinition[Pre]
  ): Type[Post] = {
    if (f.isWrapper && !wrappersInAssume.contains(f)) { TResource[Post]() }
    else if (f.isGhostWrapper && f.sretArg.nonEmpty) {
      // For ghost-wrappers with sret, the type is changed from void to the sret-type.
      rw.dispatch(f.sretArg.get.sretType.get)
    } else {
      rw.dispatch(
        f.importedReturnType
          .getOrElse(inferredReturnType.getOrElse(f, f.returnType))
      )
    }
  }

  def rewritePallasFunctionContract(
      c: PallasFunctionContract[Pre],
      retArg: Option[Ref[Post, Variable[Post]]],
  ): ApplicableContract[Post] = {
    inContract.having(true) {

      val givenArgs = rewriteArgList(c.llvmGivenArgs)
      val yieldsArgs = rewriteArgList(c.llvmYieldsArgs)

      // Update map to ensure that references to byval-args are correctly rewritten
      // Assumes that byValArgs was populated by the LLVMFunctionDefinition rewrite
      (c.llvmGivenArgs.zip(givenArgs) ++ c.llvmYieldsArgs.zip(yieldsArgs))
        .filter(_._1.isByVal).foreach { case (vOld, vNew) =>
          byValArgs.top.update(vOld.v, vNew)
          rw.variables.succeedOnly(vOld.v, vNew)
        }

      /* If the function returns in an argument, extend the contract with
       * context_everywhere retArg != NULL ** Perm(retArg, write)
       */
      val contextEverywhere =
        retArg match {
          case Some(arg) =>
            implicit val o: Origin = pallasResArgPermOrigin
            PointerNeq(Local(arg), Null(), const(0)) &* Perm(
              AmbiguousLocation(DerefPointer(Local(arg))(LLVMSretPerm)),
              WritePerm[Post](),
            )
          case None => tt[Post]
        }

      ApplicableContract[Post](
        requires = rw.dispatch(c.requires),
        ensures = rw.dispatch(c.ensures),
        contextEverywhere = contextEverywhere,
        kernelInvariant = tt,
        signals = Seq.empty,
        givenArgs = givenArgs,
        yieldsArgs = yieldsArgs,
        decreases = None,
      )(c.blame)(c.o)
    }
  }

  def rewriteStructDecl(sDecl: LLVMStructDeclaration[Pre]): Unit = {
    implicit val o: Origin = sDecl.o
    val newStruct =
      new ByValueClass[Post](
        Seq(),
        rw.classDeclarations.collect {
          sDecl.elements.zipWithIndex.foreach { case (field, idx) =>
            structFieldMap((sDecl, idx)) =
              new InstanceField(rw.dispatch(field.t), Nil)(field.o)
            rw.classDeclarations.declare(structFieldMap((sDecl, idx)))
          }
        }._1,
        sDecl.packed,
        const(sDecl.sizeInBits / 8)(sDecl.o),
        sDecl.elements.collect { field => rw.c.sizeOf(field.t, field.o) },
      )(
        sDecl.o.withContent(TypeName("struct"))
          .where(name = sDecl.name.headOption.getOrElse("unknown"))
      )

    rw.globalDeclarations.declare(newStruct)
    structMap(sDecl) = newStruct
  }

  def rewritePredicateDef(pred: LLVMFunctionDefinition[Pre]): Unit = {
    implicit val o: Origin = pred.o
    val isInlinePred =
      pred.functionType match { case t: PredicateDefinition[Pre] => t.inlined }
    // Turn LLVMFunctionDefinitions that encode predicate definitions into
    // LLVMPredicateDefinitions. These are turned into a ´real´ predicate
    // in a separate pass
    val newPred = rw.labelDecls.scope {
      byvalGhostArgs.having(Set.empty) {
        currentWrapperSret.having(None) {
          val newArgs = rewriteArgList(pred.llvmArgs)
          val bvArgMap = SuccessionMap[Variable[Pre], Variable[Post]]
          pred.llvmArgs.zip(newArgs).filter(_._1.isByVal).foreach {
            case (oldArg, newArg) => bvArgMap.update(oldArg.v, newArg)
          }

          // Generate intermediary variables for the byval args
          val bvIntermediaries = getByValIntermediaries(pred.llvmArgs, newArgs)

          // TODO: Check if we need to set funcRetType
          byValArgs.having(bvArgMap) {
            rw.globalDeclarations.declare {
              new LLVMPredicateDefinition[Post](
                args = newArgs,
                body =
                  inSpecDefFunction.having(true) {
                    allocaVars.having(mutable.Set[Variable[Pre]]()) {
                      pred.body match {
                        case None => None
                        case Some(fBody) =>
                          Some(addByValIntermediaries(
                            bvIntermediaries,
                            GotoEliminator(fBody match {
                              case scope: Scope[Pre] => scope;
                              case other => throw UnexpectedLLVMNode(other)
                            }).eliminate(),
                          ))
                      }
                    }
                  },
                inline = isInlinePred,
              )
            }
          }
        }
      }
    }

    llvmPredicateMap.update(pred, newPred)
  }

  private def getByValIntermediaries(
      oldArgs: Seq[LLVMFunctionArgument[Pre]],
      newArgs: Seq[Variable[Post]],
  ): Seq[(Variable[Post], Variable[Post])] = {
    rw.variables.collect {
      oldArgs.zip(newArgs).filter(_._1.isByVal).map { case (oldArg, newArg) =>
        val iVar =
          new Variable(TPointer(rw.dispatch(oldArg.byValType.get), None))(
            oldArg.o
          )
        rw.variables.succeedOnly(oldArg.v, iVar)
        (iVar, newArg)
      }
    }._2
  }

  private def addCast(arg: Expr[Pre], v: Variable[Pre])(
      implicit o: Origin
  ): Expr[Post] = {
    arg match {
      case dp @ DerefPointer(p) =>
        val pt = getInferredType(p)
        val et = pt.asPointer.get.element
        val vt = getLocalVarType(v)
        if (CoercionUtils.getAnyCoercion(et, vt).isDefined) { rw.dispatch(arg) }
        else if (
          vt == TVoid[Pre]() || et == TVoid[Pre]() ||
          CoercionUtils.firstElementIsType(et, vt) ||
          CoercionUtils.firstElementIsType(vt, et)
        ) {
          DerefPointer(
            PointerCast(
              rw.dispatch(arg),
              TPointer(rw.dispatch(vt), None),
              rw.c.sizeOf(et, p.o),
              rw.c.sizeOf(vt, v.o),
            )(dp.o)
          )(dp.blame)(dp.o)
        } else { throw InvalidPointerEquality(o, vt, et) }
      case _ if arg.t.asPointer.isDefined =>
        val pt = getInferredType(arg)
        val pet = pt.asPointer.get.element
        val vt = getLocalVarType(v)
        val vet = vt.asPointer.get.element
        if (CoercionUtils.getAnyCoercion(pet, vet).isDefined) {
          rw.dispatch(arg)
        } else if (
          vet == TVoid[Pre]() || pet == TVoid[Pre]() ||
          CoercionUtils.firstElementIsType(pet, vet) ||
          CoercionUtils.firstElementIsType(vet, pet)
        ) {
          PointerCast(
            rw.dispatch(arg),
            rw.dispatch(vt),
            rw.c.sizeOf(pet, arg.o),
            rw.c.sizeOf(vet, v.o),
          )(arg.o)
        } else { throw InvalidPointerEquality(o, vet, pet) }
      case _ => rw.dispatch(arg)
    }
  }

  def rewriteAmbiguousFunctionInvocation(
      inv: LLVMAmbiguousFunctionInvocation[Pre]
  ): Expr[Post] = {
    implicit val o: Origin = inv.o

    val `given` = inv.givenMap.map { case (Ref(v), e) =>
      (rw.succ[Variable[Post]](v), addCast(e, v))
    }
    val yields = inv.yields.map { case (e, Ref(v)) =>
      (addCast(e, v), rw.succ[Variable[Post]](v))
    }

    inv.ref.get.decl match {
      case func: LLVMFunctionDefinition[Pre] =>
        val newArgs = inv.args.zip(func.llvmArgs).map { case (e, arg) =>
          val c = addCast(e, arg.v)
          if (arg.byValType.nonEmpty)
            DerefPointer(c)(InvocationBlameAdapter(inv.blame))(arg.o)
          else
            c
        }
        if (!func.isPredicate)
          new ProcedureInvocation[Post](
            ref = new LazyRef[Post, Procedure[Post]](llvmFunctionMap(func)),
            args = newArgs,
            givenMap = `given`,
            yields = yields,
            outArgs = Seq.empty,
            typeArgs = Seq.empty,
          )(inv.blame)
        else
          PredicateApplyExpr[Post](new LLVMPredicateApply[Post](
            ref =
              new LazyRef[Post, LLVMPredicateDefinition[Post]](llvmPredicateMap(
                func
              )),
            args = newArgs,
          ))
      case func: LLVMSpecFunction[Pre] =>
        new FunctionInvocation[Post](
          ref = new LazyRef[Post, Function[Post]](specFunctionMap(func)),
          args = inv.args.zip(func.args).map(p => addCast(p._1, p._2)),
          givenMap = given,
          yields = yields,
          typeArgs = Seq.empty,
        )(inv.blame)
    }

  }

  def rewriteFunctionInvocation(
      inv: LLVMFunctionInvocation[Pre]
  ): Expr[Post] = {
    implicit val o: Origin = inv.o

    val newArgs = inv.args.zip(inv.ref.decl.llvmArgs).map { case (e, arg) =>
      val c = addCast(e, arg.v)
      if (arg.byValType.nonEmpty)
        DerefPointer(c)(InvocationBlameAdapter(inv.blame))(arg.o)
      else
        c
    }

    if (!inv.ref.decl.isPredicate) {
      val `given` = inv.givenMap.map { case (Ref(v), e) =>
        (rw.succ[Variable[Post]](v), addCast(e, v))
      }
      val yields = inv.yields.map { case (e, Ref(v)) =>
        (addCast(e, v), rw.succ[Variable[Post]](v))
      }

      new ProcedureInvocation[Post](
        ref = new LazyRef[Post, Procedure[Post]](llvmFunctionMap(inv.ref.decl)),
        args = newArgs,
        givenMap = `given`,
        yields = yields,
        outArgs = Seq.empty,
        typeArgs = Seq.empty,
      )(inv.blame)
    } else {
      new PredicateApplyExpr[Post](new LLVMPredicateApply[Post](
        ref =
          new LazyRef[Post, LLVMPredicateDefinition[Post]](llvmPredicateMap(
            inv.ref.decl
          )),
        args = newArgs,
      ))
    }

  }

  def rewriteWrapperInvocation(inv: LLVMWrapperInvocation[Pre]): Expr[Post] = {
    implicit val o: Origin = inv.o

    // The callArgs of the WrapperInvocation do not contain the sret-arg!
    // So we need to skip this when building the call.
    val newArgs = inv.callArgs.zip(inv.ref.decl.argsWithoutSret).map {
      case (e, arg) =>
        val c = addCast(e, arg.v)
        if (arg.byValType.nonEmpty) {
          // Add deref to account for the changed signature of the function-def
          DerefPointer(c)(InvocationBlameAdapter(inv.blame))(arg.o)
        } else
          c
    }

    new ProcedureInvocation[Post](
      ref = new LazyRef[Post, Procedure[Post]](llvmFunctionMap(inv.ref.decl)),
      args = newArgs,
      givenMap = Seq.empty,
      yields = Seq.empty,
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
    val fDef = pointer.value.decl.asInstanceOf[LLVMFunctionDefinition[Pre]]
    if (fDef.isPredicate) { throw UnexpectedLLVMNode(fDef) }

    new LLVMFunctionPointerValue[Post](value =
      new LazyRef[Post, GlobalDeclaration[Post]](llvmFunctionMap(fDef))
    )
  }

  def rewriteGlobalVariable(decl: LLVMGlobalVariable[Pre]): Unit = {
    // TODO: Handle the initializer
    // TODO: Include array and vector bounds somehow
    val (newT, newInit) =
      globalVariableInferredType.getOrElse(decl, decl.variableType) match {
        case struct: LLVMTStruct[Pre] =>
          (
            new TNonNullPointer[Post](
              new TByValueClass[Post](
                new DirectRef[Post, Class[Post]](structMap(struct.ref.decl)),
                Seq(),
              )(struct.o),
              None,
            )(struct.o),
            decl.value.map(rw.dispatch),
          )
        case array: LLVMTArray[Pre] =>
          (
            new TPointer[Post](rw.dispatch(array.elementType), None)(array.o),
            None,
          )
        case vector: LLVMTVector[Pre] =>
          (
            new TPointer[Post](rw.dispatch(vector.elementType), None)(vector.o),
            None,
          )
        case int: LLVMTInt[Pre] => (rw.dispatch(int), None)
        case _ => ???
      }
    globalVariableMap.update(
      decl,
      rw.globalDeclarations.declare(new HeapVariable(newT, newInit)(decl.o)),
    )
  }

  def rewritePointerChain(
      pointer: Expr[Post],
      t: Type[Pre],
      indices: Seq[Expr[Pre]],
      blame: Blame[ClassDerefError],
  )(implicit o: Origin): Expr[Post] = {
    if (indices.isEmpty) { return pointer }
    t match {
      case LLVMTStruct(Ref(struct)) =>
        val value =
          indices.head match {
            case value: LLVMIntegerValue[Pre] => value.value.intValue
            case value: IntegerValue[Pre] => value.value.intValue
            case _ => throw NonConstantStructIndex(o)
          }
        rewritePointerChain(
          Deref[Post](pointer, structFieldMap.ref((struct, value)))(blame),
          struct.elements(value).t,
          indices.tail,
          blame,
        )
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
      case LLVMTPointer(None) => None
      case LLVMTPointer(Some(inner)) if inner == untilType =>
        Some((pointer, currentType))
      case LLVMTPointer(Some(TBool()))
          if untilType.isInstanceOf[LLVMTInt[Pre]] =>
        Some((pointer, currentType))
      case LLVMTPointer(Some(LLVMTArray(numElements, elementType))) =>
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
      case LLVMTArray(numElements, elementType) =>
        derefUntil(
          PointerSubscript[Post](pointer, IntegerValue(BigInt(0)))(pointer.o),
          elementType,
          untilType,
        ).map { case (expr, inner) =>
          (expr, LLVMTArray[Pre](numElements, inner))
        }
      case LLVMTPointer(Some(LLVMTVector(numElements, elementType))) =>
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
      case LLVMTVector(numElements, elementType) =>
        derefUntil(
          PointerSubscript[Post](pointer, IntegerValue(BigInt(0)))(pointer.o),
          elementType,
          untilType,
        ).map { case (expr, inner) =>
          (expr, LLVMTVector[Pre](numElements, inner))
        }
      case LLVMTPointer(
            Some(tS @ LLVMTStruct(Ref(sDecl: LLVMStructDeclaration[Pre])))
          ) =>
        derefUntil(
          Deref[Post](
            DerefPointer(pointer)(pointer.o),
            structFieldMap.ref((sDecl, 0)),
          )(pointer.o),
          sDecl.elements.head.t,
          untilType,
        ).map { case (expr, inner) => (expr, LLVMTPointer[Pre](Some(tS))) }
      case sT @ LLVMTStruct(Ref(struct)) =>
        derefUntil(
          Deref[Post](pointer, structFieldMap.ref((struct, 0)))(pointer.o),
          struct.elements.head.t,
          untilType,
        ).map { case (expr, inner) => (expr, sT) }
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
    // TODO: Bring this more in line with LLVM.getGEPResultType
    val t = gep.structureType
    val offsetPointer =
      PointerAdd[Post](rw.dispatch(gep.pointer), rw.dispatch(gep.indices.head))(
        PointerSubscriptToAddBlame(gep.blame)
      )
    t match {
      case integer: LLVMTInt[Pre] =>
        // Encode simple array-indexing
        if (gep.indices.size != 1) { throw UnsupportedArrayIndex(o) }
        // Check that the inferred type of the pointer matches the return=-type of gep
        val ptrType = {
          gep.pointer match {
            case Local(Ref(v)) if localVariableInferredType.contains(v) =>
              localVariableInferredType(v)
            case _ => gep.pointer.t
          }
        }
        ptrType match {
          case LLVMTPointer(Some(t2)) if t == t2 => // All is fine
          case _ => throw UnsupportedArrayIndex(o)
        }
        offsetPointer
      case struct: LLVMTStruct[Pre] =>
        // TODO: We don't support variables in GEP yet and this just assumes all the indices are integer constants
        // Acquire the actual struct through a PointerAdd
        gep.pointer.t match {
          case LLVMTPointer(None) =>
            val structPointer = DerefPointer(offsetPointer)(gep.blame)
            AddrOf(rewritePointerChain(
              structPointer,
              struct,
              gep.indices.tail,
              PointerSubscriptToInsufficientPermissionBlame(gep.blame),
            ))
          case LLVMTPointer(Some(inner)) if inner == t =>
            val structPointer = DerefPointer(offsetPointer)(gep.blame)
            AddrOf(rewritePointerChain(
              structPointer,
              struct,
              gep.indices.tail,
              PointerSubscriptToInsufficientPermissionBlame(gep.blame),
            ))
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
                rw.c.sizeOf(t, gep.o),
              ),
              t,
            ))
            val structPointer =
              DerefPointer(PointerAdd(pointer, rw.dispatch(gep.indices.head))(
                PointerSubscriptToAddBlame(gep.blame)
              ))(gep.blame)
            val ret = AddrOf(rewritePointerChain(
              structPointer,
              struct,
              gep.indices.tail,
              PointerSubscriptToInsufficientPermissionBlame(gep.blame),
            ))
            ret
        }
      case array: LLVMTArray[Pre] =>
        // TODO (AS): Instead of doing this here we can just extend rewritePointerChain (which should enable multi-dimensional arrays too)
        val arrayPointer = DerefPointer(offsetPointer)(gep.blame)
        assert(array.elementType == gep.resultType)
        assert(gep.indices.length == 2)
        PointerAdd(arrayPointer, rw.dispatch(gep.indices(1)))(
          PointerSubscriptToAddBlame(gep.blame)
        )
      case vector: LLVMTVector[Pre] => ???
    }
    // Deref might not be the correct thing to use here since technically the pointer is only dereferenced in the load or store instruction
  }

  def derefStructIndexChain(
      value: Expr[Post],
      t: Type[Pre],
      indices: Seq[Int],
      blame: Blame[ClassDerefError],
  )(implicit o: Origin): Expr[Post] = {
    if (indices.isEmpty) { return value }
    t match {
      case LLVMTStruct(Ref(struct)) =>
        val idx = indices.head
        derefStructIndexChain(
          Deref[Post](value, structFieldMap.ref((struct, idx)))(blame),
          struct.elements(idx).t,
          indices.tail,
          blame,
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
          extrVal.blame,
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
    (getInferredType(zext.value), zext.outputType) match {
      // Both sides should become TInt
      case (LLVMTInt(_), LLVMTInt(_)) => rw.dispatch(zext.value)
      case (TBool(), LLVMTInt(_)) =>
        Select(rw.dispatch(zext.value), const(1), const(0))
      case (TBool(), TBool()) => rw.dispatch(zext.value)
      case (_, _) => throw UnsupportedZeroExtension(zext)
    }
  }

  def rewriteTruncate(trunc: LLVMTruncate[Pre]): Expr[Post] = {
    implicit val o: Origin = trunc.o
    // As long as we don't support integers as bitvectors this is mostly a no-op
    (getInferredType(trunc.value), trunc.outputType) match {
      // Both sides should become TInt
      case (LLVMTInt(_), LLVMTInt(_)) => rw.dispatch(trunc.value)
      case (LLVMTInt(_), TBool()) =>
        Select(rw.dispatch(trunc.value) === const(0), ff, tt)
      case (TBool(), TBool()) => rw.dispatch(trunc.value)
      case (_, _) => throw UnsupportedTruncate(trunc)
    }
  }

  def rewriteFloatExtend(fpext: LLVMFloatExtend[Pre]): Expr[Post] = {
    implicit val o: Origin = fpext.o
    CastFloat(rw.dispatch(fpext.value), rw.dispatch(fpext.t))
  }

  def rewriteIntegerPointerCast(
      cast: LLVMIntegerPointerCast[Pre]
  ): Expr[Post] = {
    implicit val o: Origin = cast.o
    val inputType = getInferredType(cast.value)
    val outputType = getInferredType(cast)
    val size =
      if (cast.inputType.asPointer.isDefined) {
        rw.c.sizeOf(inputType.asPointer.get.element, o)
      } else { rw.c.sizeOf(outputType.asPointer.get.element, o) }
    IntegerPointerCast(rw.dispatch(cast.value), rw.dispatch(outputType), size)
  }

  private def getInitializerForArithOpWithOverflow(
      structT: LLVMTStruct[Pre]
  ): Procedure[Post] = {
    if (!overflowOpInitializers.contains(structT)) {
      implicit val o: Origin = overflowOpInitializerOrigin
      val (resT, flagT) =
        structT.ref.decl.elements match {
          case Seq(
                LLVMFieldDefinition(_, _, res: LLVMTInt[Pre]),
                LLVMFieldDefinition(_, _, flag: TBool[Pre]),
              ) =>
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
      if (!inSpecDefFunction.isEmpty && inSpecDefFunction.top) { TBool[Post]() }
      else { funcRetType.top }
    val nondetGetter = getNondetValFunc(t)
    val r = Return[Post](
      functionInvocation[Post](blame = TrueSatisfiable, ref = nondetGetter.ref)
    )
    Block(Seq(a, r))
  }

  def rewriteReturn(llvmRet: LLVMReturn[Pre]): Statement[Post] = {
    implicit val o: Origin = llvmRet.o
    if (currentWrapperSret.nonEmpty && currentWrapperSret.top.nonEmpty) {
      // In a ghost-wrapper with sret-argument
      // ´return void´ --> ´return sret_arg´
      llvmRet.result match {
        case Void() => // OK
        case r => throw UnexpectedLLVMNode(r)
      }
      Return[Post](
        DerefPointer[Post](Local(rw.succ(currentWrapperSret.top.get.v)))(
          PanicBlame("Generated sret-deref may not fail.")
        )
      )
    } else {
      // ´Normal´ case
      Return[Post](rw.dispatch(llvmRet.result))
    }
  }

  def rewriteGhostAssign(gAssign: LLVMGhostAssign[Pre]): Statement[Post] = {
    implicit val o: Origin = gAssign.o
    gAssign.value match {
      case inv: LLVMWrapperInvocation[Pre] if inv.ref.decl.sretArg.nonEmpty =>
        // Wrapper with sret --> Handle the changed return-type
        Assign(
          DerefPointer(rw.dispatch(gAssign.target))(PanicBlame(
            "Generated deref may not fail."
          )),
          rw.dispatch(gAssign.value),
        )(gAssign.blame)
      case _ =>
        // Regular case
        Assign(rw.dispatch(gAssign.target), rw.dispatch(gAssign.value))(
          gAssign.blame
        )
    }
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
          case t: PointerType[Pre] => t.element
          case _ => e.t
        }
      // All BinExprs that use getNumericType
      case b @ AmbiguousMinus(l, r) =>
        AmbiguousMinus(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
        )(b.blame)(b.o).t
      case b @ AmbiguousMult(l, r) =>
        AmbiguousMult(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
        )(b.o).t
      case b @ AmbiguousPlus(l, r) =>
        AmbiguousPlus(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
        )(b.blame)(b.o).t
      case b @ BitShr(l, r, bits) =>
        BitShr(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
          bits,
        )(b.blame)(b.o).t
      case b @ BitOr(l, r, bits, signed) =>
        BitOr(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
          bits,
          signed,
        )(b.blame)(b.o).t
      case b @ BitShl(l, r, bits, signed) =>
        BitShl(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
          bits,
          signed,
        )(b.blame)(b.o).t
      case b @ BitUShr(l, r, bits, signed) =>
        BitUShr(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
          bits,
          signed,
        )(b.blame)(b.o).t
      case b @ BitXor(l, r, bits, signed) =>
        BitXor(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
          bits,
          signed,
        )(b.blame)(b.o).t
      case b: NumericBinExpr[Pre] =>
        BinOperatorTypes.getNumericType(
          getInferredType(b.left),
          getInferredType(b.right),
          b.o,
        )
      case b @ SmtlibPow(l, r) =>
        SmtlibPow(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
        )(b.o).t
      case b @ AmbiguousComputationalAnd(l, r) =>
        AmbiguousComputationalAnd(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
        )(b.o).t
      case b @ AmbiguousComputationalOr(l, r) =>
        AmbiguousComputationalOr(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
        )(b.o).t
      case b @ AmbiguousComputationalXor(l, r) =>
        AmbiguousComputationalXor(
          DummyConstant(getInferredType(l)),
          DummyConstant(getInferredType(r)),
        )(b.o).t
      case _ => e.t
    }

  def rewriteStore(store: LLVMStore[Pre]): Statement[Post] = {
    implicit val o: Origin = store.o
    val pointerInferredType = getInferredType(store.pointer)
    val valueInferredType = getInferredType(store.value)
    val (pointer, pointerType) = derefUntil(
      rw.dispatch(store.pointer),
      pointerInferredType,
      valueInferredType,
    ).map { case (pointer, typ) =>
      if (typ == pointerInferredType) {
        (DerefPointer(pointer)(store.blame), typ)
      } else {
        (
          DerefPointer(PointerCast(
            pointer,
            rw.dispatch(typ),
            rw.c.sizeOf(store.pointer.t.asPointer.get.element, store.o),
            rw.c.sizeOf(typ.asPointer.get.element, store.o),
          ))(store.blame),
          pointerInferredType,
        )
      }
    }.getOrElse {
      (
        DerefPointer(PointerCast(
          rw.dispatch(store.pointer),
          TPointer(rw.dispatch(valueInferredType), None),
          rw.c.sizeOf(pointerInferredType.asPointer.get.element, store.o),
          rw.c.sizeOf(valueInferredType, store.o),
        ))(store.blame),
        pointerInferredType,
      )
    }
    val strippedPtr =
      pointer match {
        case DerefPointer(AddrOf(e)) => e
        case p => p
      }
    // TODO: Fix assignfailed blame
    if (
      pointerType.asPointer.get.element == TBool[Pre]() &&
      valueInferredType.isInstanceOf[LLVMTInt[Pre]]
    ) {
      Assign(strippedPtr, rw.dispatch(store.value) !== const(0))(store.blame)
    } else { Assign(strippedPtr, rw.dispatch(store.value))(store.blame) }
  }

  def rewriteLoad(load: LLVMLoad[Pre]): Statement[Post] = {
    implicit val o: Origin = load.o
    val pointerInferredType = getInferredType(load.pointer)
    val destinationInferredType = localVariableInferredType
      .getOrElse(load.variable.decl, load.loadType)
    val pointer = derefUntil(
      rw.dispatch(load.pointer),
      pointerInferredType,
      destinationInferredType,
    ).map { case (pointer, typ) =>
      if (
        typ.asPointer.get.element == TBool[Pre]() &&
        destinationInferredType.isInstanceOf[LLVMTInt[Pre]]
      ) { Select(DerefPointer(pointer)(load.blame), const(1), const(0)) }
      else { DerefPointer(pointer)(load.blame) }
    }.getOrElse {
      if (destinationInferredType.asPointer.isDefined) {
        // We need to dereference before casting
        PointerCast(
          DerefPointer(rw.dispatch(load.pointer))(load.blame),
          rw.dispatch(destinationInferredType),
          rw.c.sizeOf(load.pointer.t.asPointer.get.element, load.o),
          rw.c.sizeOf(destinationInferredType.asPointer.get.element, load.o),
        )
      } else {
        DerefPointer(PointerCast(
          rw.dispatch(load.pointer),
          TPointer(rw.dispatch(destinationInferredType), None),
          rw.c.sizeOf(load.pointer.t.asPointer.get.element, load.o),
          rw.c.sizeOf(destinationInferredType, load.o),
        ))(load.blame)
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
    if (!inSpecDefFunction.isEmpty && inSpecDefFunction.top) {
      // Skip the initialization if we are in a wrapper function.
      return Block(Seq())
    }

    val t =
      localVariableInferredType.getOrElse(alloc.variable.decl, alloc.returnType)
        .asPointer.get.element
    val newT = rw.dispatch(t)

    if (heapVariables.contains(alloc.variable.decl)) {
      val lhv = new LocalHeapVariable(TNonNullPointer(newT, None))
      heapVariableSucc(alloc.variable.decl) = lhv
      val decl = HeapLocalDecl(lhv)
      t match {
        case arr: LLVMTArray[Pre] =>
          val newArrT = arrayType(arr)
          val pb = PanicBlame("Just allocated pointer should be assignable")
          Block(Seq(
            decl,
            Assign(
              lhv.get(pb),
              NewPointerArray(
                newArrT.element,
                newArrT.dimensions.map(_.get),
                None,
              )(PanicBlame("Invalid array size allocation")),
            )(pb),
          ))
        case _ => decl
      }
    } else {
      allocaVars.top.add(alloc.variable.decl)
      val v = Local[Post](rw.succ(alloc.variable.decl))
      val elements = rw.dispatch(alloc.numElements)
      assignLocal(
        v,
        NewNonNullPointer[Post](newT, elements, None)(PanicBlame(
          "allocation should never fail"
        )),
      )
    }
  }

  def rewriteMemset(memset: LLVMMemset[Pre]): Statement[Post] = {
    implicit val o: Origin = memset.o

    // Curently only memset with constant value of 0 is supported
    memset.value match {
      case LLVMIntegerValue(v, _) if v.intValue == 0 =>
      case _ => throw UnsupportedMemset(memset)
    }
    // TODO: Make this more more generic
    // Currently only structs where all fields are integers are supported.
    // Also, the number of bytes of the memset must exactly match the size
    // of the struct type.
    val numBytes =
      memset.len match {
        case LLVMIntegerValue(bytes, _) => bytes
        case _ => throw UnsupportedMemset(memset)
      }
    memset.dest match {
      case Local(Ref(v)) =>
        getLocalVarType(v) match {
          case LLVMTPointer(Some(s: LLVMTStruct[Pre]))
              if (s.ref.decl.sizeInBits + 7) / 8 == numBytes.intValue =>
            memsetStruct(memset, s)
          case LLVMTPointer(Some(LLVMTInt(bitWidth)))
              if (bitWidth + 7) / 8 == numBytes.intValue =>
            Assign(
              DerefPointer(rw.dispatch(memset.dest))(memset.blame),
              const(0),
            )(memset.blame)
          case _ => throw UnsupportedMemset(memset)
        }
      case _ => throw UnsupportedMemset(memset)
    }
  }

  private def memsetStruct(
      memset: LLVMMemset[Pre],
      structType: LLVMTStruct[Pre],
  ): Statement[Post] = {
    implicit val o: Origin = memset.o
    val structDecl = structType.ref.decl
    // Set all fields of the struct to 0
    val fieldAssignments = structDecl.elements.zipWithIndex.map {
      case (field, idx) =>
        val intT =
          field.t match {
            case t: LLVMTInt[Pre] => t
            case _ => throw UnsupportedMemset(memset)
          }
        val structField = structFieldMap((structDecl, idx))
        Assign[Post](
          Deref[Post](
            DerefPointer(rw.dispatch(memset.dest))(memset.blame),
            structField.ref,
          )(memset.blame),
          rw.dispatch(LLVMIntegerValue[Pre](0, intT)),
        )(memset.blame)
    }
    Block(fieldAssignments)
  }

  def rewriteMemcpy(memcpy: LLVMMemcpy[Pre]): Statement[Post] = {
    implicit val o: Origin = memcpy.o

    val srcType = getInferredType(memcpy.src).asPointer.get.element
    val dstType = getInferredType(memcpy.dst).asPointer.get.element
    if (srcType != dstType)
      throw UnsupportedMemcpy(memcpy)

    // TODO: Array case should be done with some memcpy function (such that we can return a different heap, assume would just lead to inconsistencies)
    srcType match {
      case s: LLVMTStruct[Pre] =>
        memcpyStruct(
          memcpy,
          rw.dispatch(memcpy.src),
          rw.dispatch(memcpy.dst),
          s,
        )
      case _ => throw UnsupportedMemcpy(memcpy)
    }
  }

  private def memcpyStruct(
      memcpy: LLVMMemcpy[Pre],
      src: Expr[Post],
      dst: Expr[Post],
      s: LLVMTStruct[Pre],
  ): Statement[Post] = {
    implicit val o: Origin = memcpy.o
    val sDecl = s.ref.decl
    Block[Post](sDecl.elements.zipWithIndex.map { case (f, i) =>
      val srcField =
        Deref[Post](
          DerefPointer[Post](src)(memcpy.blame),
          structFieldMap.ref((sDecl, i)),
        )(memcpy.blame)
      val dstField =
        Deref[Post](
          DerefPointer[Post](dst)(memcpy.blame),
          structFieldMap.ref((sDecl, i)),
        )(memcpy.blame)
      f.t match {
        case inner: LLVMTStruct[Pre] =>
          memcpyStruct(memcpy, srcField, dstField, inner)
        case _: LLVMTArray[Pre] | _: LLVMTVector[Pre] =>
          throw UnsupportedMemcpy(memcpy)
        case _ => Assign(dstField, srcField)(memcpy.blame)
      }
    })
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
    if (res.func.decl.isPredicate) { throw UnexpectedLLVMNode(res) }
    LLVMIntermediaryResult(
      applicable =
        new LazyRef[Post, Procedure[Post]](llvmFunctionMap(res.func.decl)),
      sretArg = res.func.decl.sretArg.flatMap(rArg => Some(rw.succ(rArg.v))),
    )
  }

  def rewriteFracOf(fracOf: LLVMFracOf[Pre]): Statement[Post] = {
    requireInWrapper(fracOf)
    implicit val o: Origin = fracOf.o
    // fracOf(v, num, denom) --> v = num / denom.
    val value =
      new RatDiv[Post](rw.dispatch(fracOf.num), rw.dispatch(fracOf.denom))(
        fracOf.blame
      )
    assignLocal(Local[Post](rw.succ(fracOf.sret.decl)), value)
  }

  def rewritePerm(llvmPerm: LLVMPerm[Pre]): Expr[Post] = {
    requireInWrapper(llvmPerm)
    implicit val o: Origin = llvmPerm.o
    Perm[Post](
      AmbiguousLocation[Post](
        DerefPointer(rw.dispatch(llvmPerm.loc))(llvmPerm.blame)
      ),
      rw.dispatch(llvmPerm.perm),
    )
  }

  def rewritePtrBlockLength(llvmPBL: LLVMPtrBlockLength[Pre]): Expr[Post] = {
    requireInWrapper(llvmPBL)
    implicit val o: Origin = llvmPBL.o
    PointerBlockLength[Post](rw.dispatch(llvmPBL.ptr))(llvmPBL.blame)
  }

  def rewritePtrBlockOffset(llvmPBO: LLVMPtrBlockOffset[Pre]): Expr[Post] = {
    requireInWrapper(llvmPBO)
    implicit val o: Origin = llvmPBO.o
    PointerBlockOffset[Post](rw.dispatch(llvmPBO.ptr))(llvmPBO.blame)
  }

  def rewritePtrLength(llvmPL: LLVMPtrLength[Pre]): Expr[Post] = {
    requireInWrapper(llvmPL)
    implicit val o: Origin = llvmPL.o
    PointerLength[Post](rw.dispatch(llvmPL.ptr))(llvmPL.blame)
  }

  def rewriteImplies(llvmImply: LLVMImplies[Pre]): Expr[Post] = {
    requireInWrapper(llvmImply)
    implicit val o: Origin = llvmImply.o
    Implies[Post](rw.dispatch(llvmImply.left), rw.dispatch(llvmImply.right))
  }

  def rewriteAnd(llvmAnd: LLVMAnd[Pre]): Expr[Post] = {
    requireInWrapper(llvmAnd)
    implicit val o: Origin = llvmAnd.o
    And[Post](rw.dispatch(llvmAnd.left), rw.dispatch(llvmAnd.right))
  }

  def rewriteOr(llvmOr: LLVMOr[Pre]): Expr[Post] = {
    requireInWrapper(llvmOr)
    implicit val o: Origin = llvmOr.o
    Or[Post](rw.dispatch(llvmOr.left), rw.dispatch(llvmOr.right))
  }

  def rewriteStar(llvmStar: LLVMStar[Pre]): Expr[Post] = {
    requireInWrapper(llvmStar)
    implicit val o: Origin = llvmStar.o
    Star[Post](rw.dispatch(llvmStar.left), rw.dispatch(llvmStar.right))
  }

  def rewriteOld(llvmOld: LLVMOld[Pre]): Expr[Post] = {
    requireInWrapper(llvmOld)
    implicit val o: Origin = llvmOld.o
    LLVMOld[Post](rw.dispatch(llvmOld.v))
  }

  def rewriteSeqNew(seqNew: LLVMSeqNew[Pre]): Statement[Post] = {
    requireInWrapper(seqNew)
    implicit val o: Origin = seqNew.o

    Assign[Post](
      rw.dispatch(seqNew.target),
      LiteralSeq[Post](rw.dispatch(seqNew.cType), Seq.empty),
    )(seqNew.blame)
  }

  def rewriteSeqSize(seqSize: LLVMSeqSize[Pre]): Expr[Post] = {
    requireInWrapper(seqSize)
    implicit val o: Origin = seqSize.o
    Size[Post](DerefPointer[Post](rw.dispatch(seqSize.seq))(seqSize.blame))
  }

  def rewriteSeqEq(seqEq: LLVMSeqEq[Pre]): Expr[Post] = {
    requireInWrapper(seqEq)
    implicit val o: Origin = seqEq.o
    Eq[Post](
      DerefPointer[Post](rw.dispatch(seqEq.s1))(seqEq.blame),
      DerefPointer[Post](rw.dispatch(seqEq.s2))(seqEq.blame),
    )
  }

  def rewriteSeqGet(seqGet: LLVMSeqGet[Pre]): Expr[Post] = {
    requireInWrapper(seqGet)
    implicit val o: Origin = seqGet.o

    SeqSubscript(
      DerefPointer(rw.dispatch(seqGet.seq))(seqGet.blame),
      rw.dispatch(seqGet.idx),
    )(seqGet.blame)
  }

  def rewriteSeqSlice(seqSlice: LLVMSeqSlice[Pre]): Expr[Post] = {
    requireInWrapper(seqSlice)
    implicit val o: Origin = seqSlice.o

    Slice(
      DerefPointer(rw.dispatch(seqSlice.seq))(seqSlice.blame),
      rw.dispatch(seqSlice.sIdx),
      rw.dispatch(seqSlice.eIdx),
    )
  }

  def rewriteSeqPrepend(seqPrep: LLVMSeqPrepend[Pre]): Expr[Post] = {
    requireInWrapper(seqPrep)
    implicit val o: Origin = seqPrep.o

    Cons(
      rw.dispatch(seqPrep.elem),
      DerefPointer(rw.dispatch(seqPrep.seq))(seqPrep.blame),
    )
  }

  def rewriteSeqUpdate(seqUpdate: LLVMSeqUpdate[Pre]): Expr[Post] = {
    requireInWrapper(seqUpdate)
    implicit val o: Origin = seqUpdate.o

    SeqUpdate(
      DerefPointer(rw.dispatch(seqUpdate.seq))(seqUpdate.blame),
      rw.dispatch(seqUpdate.idx),
      rw.dispatch(seqUpdate.elem),
    )
  }

  def correctPointerComparison[T <: Expr[Post]](
      left: Expr[Pre],
      right: Expr[Pre],
      op: (Expr[Post], Expr[Post], Option[Expr[Post]]) => T,
  )(implicit o: Origin): T = {
    val lt = getInferredType(left)
    val rt = getInferredType(right)
    val nl = rw.dispatch(left)
    val nr = rw.dispatch(right)

    def cast(e: Expr[Post], fromType: Type[Pre], toType: Type[Pre]) =
      PointerCast(
        e,
        TPointer(rw.dispatch(toType), None),
        rw.c.sizeOf(fromType, o),
        rw.c.sizeOf(toType, o),
      )

    (lt, rt) match {
      case (l, r) if l == r =>
        op(nl, nr, l.asPointer.map(p => rw.c.sizeOf(p.element, o)))
      case (LLVMTPointer(None), LLVMTPointer(None)) =>
        op(nl, nr, Some(rw.c.sizeOf(TAnyValue(), o)))
      case (LLVMTPointer(Some(lt)), LLVMTPointer(None)) =>
        op(nl, cast(nr, TAnyValue(), lt), Some(rw.c.sizeOf(lt, o)))
      case (LLVMTPointer(None), LLVMTPointer(Some(rt))) =>
        op(cast(nl, TAnyValue(), rt), nr, Some(rw.c.sizeOf(rt, o)))
      case (LLVMTPointer(Some(lt)), LLVMTPointer(Some(rt))) =>
        if (CoercionUtils.firstElementIsType(lt, rt)) {
          op(nl, cast(nr, rt, lt), Some(rw.c.sizeOf(lt, o)))
        } else if (CoercionUtils.firstElementIsType(rt, lt)) {
          op(cast(nl, lt, rt), nr, Some(rw.c.sizeOf(rt, o)))
        } else { throw InvalidPointerEquality(o, lt, rt) }
      case (l, r) if l.asPointer.isDefined && r.asPointer.isDefined =>
        if (
          CoercionUtils
            .getAnyCoercion(l.asPointer.get.element, r.asPointer.get.element)
            .isDefined
        ) { op(nl, nr, Some(rw.c.sizeOf(l.asPointer.get.element, o))) }
        else { throw InvalidPointerComparison(o) }
      case (_, _) => op(nl, nr, None)
    }
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
    else {
      Label(
        rw.labelDecls.dispatch(block.label),
        newBody,
        LoopInvariant(tt, None)(TrueSatisfiable)(block.o),
      )(block.o)
    }
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
      val usedVars = mutable.Set[Variable[Pre]]()
      loop.blocks.getOrElse(mutable.Set.empty).foreach { b =>
        b.body.collect {
          case LLVMStore(_, Local(Ref(v)), _) => assignedVars.add(v)
          case Local(Ref(v)) => usedVars.add(v)
        }
      }
      currentLoopLabel.having(block.label) {
        Label(
          rw.labelDecls.dispatch(block.label),
          Loop(
            Block(Nil)(block.o),
            tt[Post],
            Block(Nil)(block.o),
            assignedInLoop.having(assignedVars) {
              usedInLoop.having(usedVars) { rw.dispatch(loop.contract) }
            },
            Block(
              blockToLabel(loop.headerBlock.get) +: loop.blocks.get.filterNot {
                b => b == loop.headerBlock.get || b == loop.latchBlock.get
              }.map(b => blockToLabel(b)) :+
                blockToLabel(loop.latchBlock.get, true)
            )(block.o),
          )(block.o),
          LoopInvariant(tt, None)(TrueSatisfiable)(block.o),
        )(block.o)
      }
    }
  }

  def rewriteLoopContract(
      llvmContract: LLVMLoopContract[Pre]
  ): LoopInvariant[Post] = {
    implicit val o: Origin = llvmContract.o
    // Add Permission for alloca-variables
    var extendedInv = rw.dispatch(llvmContract.invariant)
    val locPermBlame = PanicBlame("Generated locals always have permission")
    allocaVars.topOption.getOrElse(mutable.Set.empty)
      .intersect(usedInLoop.topOption.getOrElse(mutable.Set.empty))
      .foreach { v =>
        if (
          !assignedInLoop.topOption.getOrElse(mutable.Set.empty).contains(v)
        ) {
          // If the variable is not assigned to, specify that the value does not change
          // TODO: We might have to check that the pointer to v is not passed to other functions in the loop
          // \old(*v, loop_header) == *v
          val oldClause =
            Old[Post](
              DerefPointer[Post](Local(rw.succ(v)))(locPermBlame),
              Option(rw.succ(currentLoopLabel.top)),
            )(PanicBlame("Header-label always precedes loop")) ===
              DerefPointer[Post](Local(rw.succ(v)))(locPermBlame)
          extendedInv = oldClause &* extendedInv
        }

        val permClause = Perm(
          AmbiguousLocation[Post](
            DerefPointer(Local[Post](rw.succ(v)))(locPermBlame)
          ),
          WritePerm[Post](),
        )
        extendedInv = permClause &* extendedInv
      }
    LoopInvariant[Post](extendedInv, None)(llvmContract.blame)
  }

  def rewriteIntegerValue(iVal: LLVMIntegerValue[Pre]): Expr[Post] = {
    implicit val o: Origin = iVal.o

    iVal match {
      case LLVMIntegerValue(v, LLVMTInt(1)) => BooleanValue(v != 0)
      case _ => IntegerValue(iVal.value)
    }
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
        case ret: LLVMReturn[Pre] =>
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
    if (inSpecDefFunction.isEmpty || !inSpecDefFunction.top) {
      throw UnexpectedLLVMNode(node)
    }
  }

  def structType(t: LLVMTStruct[Pre]): Type[Post] = {
    val targetClass = new LazyRef[Post, Class[Post]](structMap(t.ref.decl))
    TByValueClass[Post](targetClass, Seq())(t.o)
  }

  def intType(t: LLVMTInt[Pre]): Type[Post] = {
    t match {
      case LLVMTInt(1) => TBool()(t.o)
      case _ => TInt()(t.o)
    }
  }

  def pointerType(t: LLVMTPointer[Pre]): Type[Post] =
    t.innerType match {
      case Some(innerType) => TPointer[Post](rw.dispatch(innerType), None)(t.o)
      case None => TPointer[Post](TVoid(), None)(t.o)
    }

  def arrayType(t: LLVMTArray[Pre]): TPointerArray[Post] = {
    var current: Type[Pre] = t
    var dimensions = Seq[Expr[Post]]()
    while (current.isInstanceOf[LLVMTArray[Pre]]) {
      val LLVMTArray(elems, inner) = current
      dimensions = dimensions :+ const[Post](elems)(inner.o)
      current = inner
    }
    TPointerArray[Post](rw.dispatch(current), dimensions.map(Some(_)), None)(
      t.o
    )
  }

  def vectorType(t: LLVMTVector[Pre]): Type[Post] =
    TPointer(rw.dispatch(t.elementType), None)(t.o)
}
