package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.origin.{Origin, PanicBlame, SourceName}
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.resolve.ctx.RefLLVMFunctionDefinition
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers.{VarBuildHelpers, assignLocal, const, tt}
import vct.col.util.{CurrentProgramContext, SuccessionMap}
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

  private val globalVariableTypeGuesses
      : mutable.HashMap[LLVMGlobalVariable[Pre], mutable.HashSet[Type[Pre]]] =
    mutable.HashMap()
  private val structFieldTypeGuesses
      : mutable.HashMap[(LLVMTStruct[Pre], Int), mutable.HashSet[Type[Pre]]] =
    mutable.HashMap()
  private val localTypeGuesses
      : mutable.HashMap[Variable[Pre], mutable.HashSet[Type[Pre]]] = mutable
    .HashMap()

  def rewriteLocal(local: LLVMLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    Local(rw.succ(local.ref.get.decl))
  }

  def rewriteFunctionDef(func: LLVMFunctionDefinition[Pre]): Unit = {
    implicit val o: Origin = func.o
    val importedDecl = rw.importedDeclarations.find {
      case procedure: Procedure[Pre] =>
        func.contract.name == procedure.o.get[SourceName].name
    }
    val procedure = rw.labelDecls.scope {
      rw.globalDeclarations.declare(if (importedDecl.isDefined) {
        val importedProcedure = importedDecl.get.asInstanceOf[Procedure[Pre]]
        val newArgs = importedProcedure.args.map { it => it.rewriteDefault() }
        new Procedure[Post](
          returnType = rw.dispatch(importedProcedure.returnType),
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
      } else {
        new Procedure[Post](
          returnType = rw.dispatch(func.returnType),
          args = rw.variables.collect { func.args.foreach(rw.dispatch) }._1,
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
      })
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
      new Class[Post](
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
        tt[Post],
      )(t.o)

    rw.globalDeclarations.declare(newStruct)
    structMap(t) = newStruct
  }

  def rewriteGlobalVariable(decl: LLVMGlobalVariable[Pre]): Unit = {
    // TODO: Handle the initializer
    // TODO: Include array and vector bounds somehow
    decl.variableType match {
      case struct: LLVMTStruct[Pre] => {
        rewriteStruct(struct)
        globalVariableMap.update(
          decl,
          rw.globalDeclarations.declare(
            new HeapVariable[Post](
              new TClass[Post](
                new DirectRef[Post, Class[Post]](structMap(struct)),
                Seq(),
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

  def derefUntil(
      pointer: Expr[Post],
      currentType: Type[Pre],
      untilType: Type[Pre],
  ): (Expr[Post], Type[Pre]) = {
    implicit val o: Origin = pointer.o
    currentType match {
      case _ if currentType == untilType => (AddrOf(pointer), currentType)
      case LLVMTPointer(None) => (pointer, LLVMTPointer[Pre](Some(untilType)))
      case LLVMTPointer(Some(inner)) if inner == untilType =>
        (pointer, currentType)
      case LLVMTPointer(Some(LLVMTArray(numElements, elementType))) => {
        val (expr, inner) = derefUntil(
          PointerSubscript[Post](
            DerefPointer(pointer)(pointer.o),
            IntegerValue(BigInt(0)),
          )(pointer.o),
          elementType,
          untilType,
        )
        (expr, LLVMTPointer[Pre](Some(LLVMTArray(numElements, inner))))
      }
      case LLVMTArray(numElements, elementType) => {
        val (expr, inner) = derefUntil(
          PointerSubscript[Post](pointer, IntegerValue(BigInt(0)))(pointer.o),
          elementType,
          untilType,
        )
        (expr, LLVMTArray[Pre](numElements, inner))
      }
      case LLVMTPointer(Some(LLVMTVector(numElements, elementType))) => {
        val (expr, inner) = derefUntil(
          PointerSubscript[Post](
            DerefPointer(pointer)(pointer.o),
            IntegerValue(BigInt(0)),
          )(pointer.o),
          elementType,
          untilType,
        )
        (expr, LLVMTPointer[Pre](Some(LLVMTVector(numElements, inner))))
      }
      case LLVMTVector(numElements, elementType) => {
        val (expr, inner) = derefUntil(
          PointerSubscript[Post](pointer, IntegerValue(BigInt(0)))(pointer.o),
          elementType,
          untilType,
        )
        (expr, LLVMTVector[Pre](numElements, inner))
      }
      case LLVMTPointer(Some(struct @ LLVMTStruct(name, packed, elements))) => {
        val (expr, inner) = derefUntil(
          Deref[Post](
            DerefPointer(pointer)(pointer.o),
            structFieldMap.ref((struct, 0)),
          )(pointer.o),
          elements.head,
          untilType,
        )
        (
          expr,
          LLVMTPointer[Pre](Some(
            LLVMTStruct(name, packed, inner +: elements.tail)
          )),
        )
      }
      case struct @ LLVMTStruct(name, packed, elements) => {
        val (expr, inner) = derefUntil(
          Deref[Post](pointer, structFieldMap.ref((struct, 0)))(pointer.o),
          elements.head,
          untilType,
        )
        (expr, LLVMTStruct[Pre](name, packed, inner +: elements.tail))
      }
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
        // TODO: Can we somehow wrap the rw.dispatch(gep.pointer) to add the known type structureType?
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
            val (pointer, inferredType) = derefUntil(
              rw.dispatch(gep.pointer),
              gep.pointer.t,
              t,
            )
            addTypeGuess(gep.pointer, inferredType)
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

  def rewriteStore(store: LLVMStore[Pre]): Statement[Post] = {
    implicit val o: Origin = store.o
    val (pointer, inferredType) = derefUntil(
      rw.dispatch(store.pointer),
      store.pointer.t,
      store.value.t,
    )
    addTypeGuess(store.pointer, inferredType)
    Assign(DerefPointer(pointer)(store.o), rw.dispatch(store.value))(store.o)
  }

  def rewriteLoad(load: LLVMLoad[Pre]): Expr[Post] = {
    val (pointer, inferredType) = derefUntil(
      rw.dispatch(load.pointer),
      load.pointer.t,
      load.loadType,
    )
    addTypeGuess(load.pointer, inferredType)
    DerefPointer(pointer)(load.o)(load.o)
  }

  def rewriteAllocA(alloc: LLVMAllocA[Pre]): Expr[Post] = {
    implicit val o: Origin = alloc.o
    val t = rw.dispatch(alloc.allocationType)
    val v = new Variable[Post](TPointer(t))(alloc.o)
    alloc.allocationType match {
      case structType: LLVMTStruct[Pre] =>
        With(
          Block(Seq(
            LocalDecl(v),
            assignLocal(
              v.get,
              NewPointerArray[Post](
                rw.dispatch(alloc.allocationType),
                rw.dispatch(alloc.numElements),
              )(PanicBlame("allocation should never fail")),
            ),
            Assign(
              DerefPointer(v.get)(alloc.o),
              NewObject[Post](structMap.ref(structType)),
            )(PanicBlame("assignment should never fail")),
          )),
          v.get,
        )
      case _ =>
        NewPointerArray[Post](t, rw.dispatch(alloc.numElements))(PanicBlame(
          "allocation should never fail"
        ))
    }
  }

  private def addTypeGuess(pointer: Expr[Pre], inferredType: Type[Pre]): Unit =
    pointer match {
      case Local(Ref(v)) =>
        localTypeGuesses.getOrElseUpdate(v, { mutable.HashSet() })
          .add(LLVMTPointer[Pre](Some(inferredType)))
      case LLVMPointerValue(Ref(g)) =>
        globalVariableTypeGuesses.getOrElseUpdate(
          g.asInstanceOf[LLVMGlobalVariable[Pre]],
          { mutable.HashSet() },
        ).add(inferredType)
      case it => {
        println(it)
        ???
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

  /*
  Elimination works by replacing every goto with the block its referring too
  effectively transforming the CFG into a tree. More efficient restructuring algorithms but this works for now.

  This of course only works for acyclic CFGs as otherwise replacement would be infinitely recursive.
  Loop restructuring should be handled by pallas as it has much more analytical and contextual information about
  the program.
   */
  case class GotoEliminator(bodyScope: Scope[Pre]) extends LazyLogging {
    val labelDeclMap: Map[LabelDecl[Pre], Label[Pre]] =
      bodyScope.body match {
        case block: Block[Pre] =>
          block.statements.map {
            case label: Label[Pre] => (label.decl, label)
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
                  case label: Label[Pre] => Seq(eliminate(label))
                  case other => throw UnexpectedLLVMNode(other)
                })(scope.body.o)
              case other => throw UnexpectedLLVMNode(other)
            },
          )(scope.o)
        case other => throw UnexpectedLLVMNode(other)
      }
    }

    def eliminate(label: Label[Pre]): Block[Post] = {
      implicit val o: Origin = label.o
      label.stat match {
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
    TClass[Post](targetClass, Seq())(t.o)
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
