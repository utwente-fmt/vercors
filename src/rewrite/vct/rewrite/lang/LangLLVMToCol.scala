package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin, PanicBlame, TypeName}
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.resolve.ctx.RefLLVMFunctionDefinition
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{CurrentProgramContext, SubstituteReferences, SuccessionMap}
import vct.result.VerificationError.{SystemError, UserError}

import scala.collection.mutable

case object LangLLVMToCol {
  case class UnexpectedLLVMNode(node: Node[_]) extends SystemError {
    override def text: String =
      context[CurrentProgramContext].map(_.highlight(node)).getOrElse(node.o)
        .messageInContext(
          "VerCors assumes this node does not occur here in llvm input."
        )
  }

  case class NonConstantStructIndex(origin: Origin) extends UserError {
    override def code: String = "nonConstantStructIndex"

    override def text: String =
      origin.messageInContext(
        s"This struct indexing operation (getelementptr) uses a non-constant struct index which we do not support."
      )
  }
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
        case (_, LLVMTPointer(None)) => true
        case (LLVMTPointer(Some(a)), LLVMTPointer(Some(b))) =>
          moreSpecific(a, b)
        case (LLVMTPointer(Some(a)), TPointer(b)) => moreSpecific(a, b)
        case (TPointer(a), LLVMTPointer(Some(b))) => moreSpecific(a, b)
        case (TPointer(a), TPointer(b)) => moreSpecific(a, b)
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
              if a == b || rw.dispatch(a) == rw.dispatch(b) ||
                moreSpecific(a, b) =>
            Some(a)
          case (Some(a), Some(b)) if moreSpecific(b, a) => Some(b)
          case _ => None
        }
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
        val superType = findMostSpecific(getGuesses.map(_()))
        if (superType.isEmpty) { false }
        else {
          val updated = currentType == superType.get
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
      else {
        // TODO: Support multiple guesses?
        SubstituteReferences(subMap.toMap).dispatch(value)
      }
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
      case inv: LLVMFunctionInvocation[Pre] =>
        inv.ref.decl.importedArguments.getOrElse(inv.ref.decl.args).zipWithIndex
          .foreach { case (a, i) =>
            getVariable(inv.args(i))
              .foreach(v => addTypeGuess(v, Set.empty, _ => a.t))
          }
    }

    typeGuesses.foreachEntry((k, v) =>
      v.depends.filter(typeGuesses.contains)
        .foreach(typeGuesses.get(_).foreach(_.dependents.add(k)))
    )
    val updateQueue = mutable.ArrayDeque.from(typeGuesses.keys)

    while (updateQueue.nonEmpty) {
      val obj = updateQueue.removeLast()
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

  def rewriteLocal(local: LLVMLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    Local(rw.succ(local.ref.get.decl))
  }

  def rewriteLocalVariable(v: Variable[Pre]): Unit = {
    implicit val o: Origin = v.o;
    rw.variables.succeed(
      v,
      new Variable[Post](rw.dispatch(
        localVariableInferredType.getOrElse(v, v.t)
      )),
    )
  }

  def rewriteFunctionDef(func: LLVMFunctionDefinition[Pre]): Unit = {
    implicit val o: Origin = func.o
    val procedure = rw.labelDecls.scope {
      val newArgs = func.importedArguments.getOrElse(func.args).map { it =>
        it.rewriteDefault()
      }
      rw.globalDeclarations.declare(
        new Procedure[Post](
          returnType = rw
            .dispatch(func.importedReturnType.getOrElse(func.returnType)),
          args =
            rw.variables.collect {
              func.args.zip(newArgs).foreach { case (a, b) =>
                rw.variables.succeed(a, b)
              }
            }._1,
          outArgs = Nil,
          typeArgs = Nil,
          body =
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
            },
          contract = rw.dispatch(func.contract.data.get),
          pure = func.pure,
        )(func.blame)
      )
    }
    llvmFunctionMap.update(func, procedure)
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
        Seq(),
      )(t.o.withContent(TypeName("struct")))

    rw.globalDeclarations.declare(newStruct)
    structMap(t) = newStruct
  }

  def rewriteGlobalVariable(decl: LLVMGlobalVariable[Pre]): Unit = {
    // TODO: Handle the initializer
    // TODO: Include array and vector bounds somehow
    globalVariableInferredType.getOrElse(decl, decl.variableType) match {
      case struct: LLVMTStruct[Pre] => {
        rewriteStruct(struct)
        globalVariableMap.update(
          decl,
          rw.globalDeclarations.declare(
            new HeapVariable[Post](
              new TNonNullPointer[Post](
                new TByValueClass[Post](
                  new DirectRef[Post, Class[Post]](structMap(struct)),
                  Seq(),
                )(struct.o)
              )(struct.o)
            )(decl.o)
          ),
        )
      }
      case array: LLVMTArray[Pre] => {
        globalVariableMap.update(
          decl,
          rw.globalDeclarations.declare(
            new HeapVariable[Post](
              new TPointer[Post](rw.dispatch(array.elementType))(array.o)
            )(decl.o)
          ),
        )
      }
      case vector: LLVMTVector[Pre] => {
        globalVariableMap.update(
          decl,
          rw.globalDeclarations.declare(
            new HeapVariable[Post](
              new TPointer[Post](rw.dispatch(vector.elementType))(vector.o)
            )(decl.o)
          ),
        )
      }
      case _ => { ??? }
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
              )(o),
              struct.elements(value.value.intValue),
              indices.tail,
            )
          case value: IntegerValue[Pre] =>
            rewritePointerChain(
              Deref[Post](
                pointer,
                structFieldMap.ref((struct, value.value.intValue)),
              )(o),
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
                )(o)
              )(o)
            AddrOf(rewritePointerChain(structPointer, struct, gep.indices.tail))
          case LLVMTPointer(Some(inner)) if inner == t =>
            val structPointer =
              DerefPointer(
                PointerAdd(
                  rw.dispatch(gep.pointer),
                  rw.dispatch(gep.indices.head),
                )(o)
              )(o)
            AddrOf(rewritePointerChain(structPointer, struct, gep.indices.tail))
          case LLVMTPointer(Some(_)) =>
            val pointerInferredType = getInferredType(gep.pointer)
            val (pointer, inferredType) = derefUntil(
              rw.dispatch(gep.pointer),
              pointerInferredType,
              t,
            ).getOrElse(
              (Cast(rw.dispatch(gep.pointer), TypeValue(rw.dispatch(t))), t)
            )
            val structPointer =
              DerefPointer(
                PointerAdd(pointer, rw.dispatch(gep.indices.head))(o)
              )(o)
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

  private def getInferredType(e: Expr[Pre]): Type[Pre] =
    e match {
      case Local(Ref(v)) => localVariableInferredType.getOrElse(v, e.t)
      // Making assumption here that LLVMPointerValue only contains LLVMGlobalVariables whereas LLVMGlobalVariableImpl assumes it can also contain HeapVariables
      case LLVMPointerValue(Ref(v)) =>
        globalVariableInferredType
          .getOrElse(v.asInstanceOf[LLVMGlobalVariable[Pre]], e.t)
      case _ => e.t
    }

  def rewriteStore(store: LLVMStore[Pre]): Statement[Post] = {
    implicit val o: Origin = store.o
    val pointerInferredType = getInferredType(store.pointer)
    val valueInferredType = getInferredType(store.value)
    val (pointer, inferredType) = derefUntil(
      rw.dispatch(store.pointer),
      pointerInferredType,
      valueInferredType,
    ).map { case (pointer, typ) => (DerefPointer(pointer)(store.blame), typ) }
      .getOrElse {
        if (store.value.t.asPointer.isDefined) {
          // TODO: How do we deal with this
          ???
        } else {
          (
            DerefPointer(Cast(
              rw.dispatch(store.pointer),
              TypeValue(TPointer(rw.dispatch(valueInferredType))),
            ))(store.blame),
            pointerInferredType,
          )
        }
      }
    // TODO: Fix assignfailed blame
    Assign(pointer, rw.dispatch(store.value))(store.blame)
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
            Cast(
              DerefPointer(rw.dispatch(load.pointer))(load.blame),
              TypeValue(rw.dispatch(destinationInferredType)),
            ),
            pointerInferredType,
          )
        } else {
          (
            DerefPointer(Cast(
              rw.dispatch(load.pointer),
              TypeValue(TPointer(rw.dispatch(destinationInferredType))),
            ))(load.blame),
            pointerInferredType,
          )
        }
      }
    assignLocal(Local(rw.succ(load.variable.decl)), pointer)
  }

  def rewriteAllocA(alloc: LLVMAllocA[Pre]): Statement[Post] = {
    implicit val o: Origin = alloc.o
    val t =
      localVariableInferredType.getOrElse(
        alloc.variable.decl,
        LLVMTPointer(Some(alloc.allocationType)),
      ).asPointer.get.element
    val newT = rw.dispatch(t)
    val v = Local[Post](rw.succ(alloc.variable.decl))
    val elements = rw.dispatch(alloc.numElements)
    t match {
      case structType: LLVMTStruct[Pre] =>
        Block(Seq(
          assignLocal(
            v,
            NewNonNullPointerArray[Post](newT, elements)(PanicBlame(
              "allocation should never fail"
            )),
          ),
          Assign(
            DerefPointer(v)(PanicBlame("pointer is framed in allocation")),
            NewObject[Post](structMap.ref(structType)),
          )(PanicBlame("assignment should never fail")),
        ))
      case _ =>
        assignLocal(
          v,
          NewNonNullPointerArray[Post](newT, elements)(PanicBlame(
            "allocation should never fail"
          )),
        )
    }
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

  def result(ref: RefLLVMFunctionDefinition[Pre])(
      implicit o: Origin
  ): Expr[Post] = Result[Post](llvmFunctionMap.ref(ref.decl))

  private def blockToLabel(block: LLVMBasicBlock[Pre]): Statement[Post] =
    if (elidedBackEdges.contains(block.label)) { rw.dispatch(block.body) }
    else {
      Label(rw.labelDecls.dispatch(block.label), rw.dispatch(block.body))(
        block.o
      )
    }

  def rewriteBasicBlock(block: LLVMBasicBlock[Pre]): Statement[Post] = {
    if (loopBlocks.contains(block))
      return Block(Nil)(DiagnosticOrigin)
    if (block.loop.isEmpty) { blockToLabel(block) }
    else {
      val loop = block.loop.get
      loopBlocks.addAll(loop.blocks.get)
      Loop(
        Block(Nil)(block.o),
        tt[Post],
        Block(Nil)(block.o),
        rw.dispatch(loop.contract),
        Block(blockToLabel(loop.headerBlock.get) +: loop.blocks.get.filterNot {
          b => b == loop.headerBlock.get || b == loop.latchBlock.get
        }.map(blockToLabel) :+ blockToLabel(loop.latchBlock.get))(block.o),
      )(block.o)
    }
  }

  def rewriteGoto(goto: Goto[Pre]): Statement[Post] = {
    if (elidedBackEdges.contains(goto.lbl.decl)) {
      // TODO: Verify that the correct block always follows this one
      Block(Nil)(goto.o)
    } else { goto.rewriteDefault() }
  }

  /*
  Elimination works by replacing every goto with the block its referring too
  effectively transforming the CFG into a tree. More efficient restructuring algorithms but this works for now.

  This of course only works for acyclic CFGs as otherwise replacement would be infinitely recursive.
  Loop restructuring should be handled by Pallas as it has much more analytical and contextual information about
  the program.
   */
  case class GotoEliminator(bodyScope: Scope[Pre]) extends LazyLogging {
    val labelDeclMap: Map[LabelDecl[Pre], LLVMBasicBlock[Pre]] =
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

    def eliminate(bb: LLVMBasicBlock[Pre]): Block[Post] = {
      implicit val o: Origin = bb.o
      bb.body match {
        case block: Block[Pre] =>
          block.statements.last match {
            case goto: Goto[Pre] =>
              Block[Post](
                block.statements.dropRight(1).map(rw.dispatch) ++
                  eliminate(labelDeclMap(goto.lbl.decl)).statements
              )
            case _: Return[Pre] =>
              rw.dispatch(block) match {
                case block: Block[Post] => block
                case other => throw UnexpectedLLVMNode(other)
              }
            case branch: Branch[Pre] =>
              Block[Post](
                block.statements.dropRight(1).map(rw.dispatch) :+
                  eliminate(branch)
              )
            case other => throw UnexpectedLLVMNode(other)
          }
        case other => throw UnexpectedLLVMNode(other)
      }
    }

    def eliminate(branch: Branch[Pre]): Branch[Post] = {
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

  def structType(t: LLVMTStruct[Pre]): Type[Post] = {
    val targetClass = new LazyRef[Post, Class[Post]](structMap(t))
    TByValueClass[Post](targetClass, Seq())(t.o)
  }

  def pointerType(t: LLVMTPointer[Pre]): Type[Post] =
    t.innerType match {
      case Some(innerType) => TPointer[Post](rw.dispatch(innerType))(t.o)
      case None => TPointer[Post](TAny())(t.o)
    }

  def arrayType(t: LLVMTArray[Pre]): Type[Post] =
    TPointer(rw.dispatch(t.elementType))(t.o)

  def vectorType(t: LLVMTVector[Pre]): Type[Post] =
    TPointer(rw.dispatch(t.elementType))(t.o)
}
