package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.ast.lang.c.CTVector.WrongVectorType
import vct.col.ast.util.ExpressionEqualityCheck.isConstantInt
import vct.rewrite.lang.LangSpecificToCol.NotAValue
import vct.col.origin._
import vct.col.ref.{LazyRef, Ref}
import vct.col.resolve.lang.C
import vct.col.resolve.ctx._
import vct.col.resolve.lang.C.nameFromDeclarator
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.typerules.{CoercionUtils, TypeSize}
import vct.col.util.{SuccessionMap, Substitute}
import vct.col.util.AstBuildHelpers._
import vct.result.Message
import vct.result.VerificationError.{Unreachable, UserError}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable

case object LangCToCol {
  private case class MultipleSharedMemoryDeclaration(decl: Node[_])
      extends UserError {
    override def code: String = "multipleSharedMemoryDeclaration"
    override def text: String =
      decl.o.messageInContext(
        s"We don't support declaring multiple shared memory variables at a single line."
      )
  }

  private case class WrongGPUKernelParameterType(param: CParam[_])
      extends UserError {
    override def code: String = "wrongParameterType"
    override def text: String =
      param.o.messageInContext(
        s"This parameter has a type that is not allowed as parameter in a GPU kernel."
      )
  }

  private case class WrongGPUType(param: CParam[_]) extends UserError {
    override def code: String = "wrongGPUType"
    override def text: String =
      param.o.messageInContext(
        s"This parameter has a type that is not allowed outside of a GPU kernel."
      )
  }

  private case class WrongCType(decl: Declaration[_]) extends UserError {
    override def code: String = "wrongCType"
    override def text: String =
      decl.o
        .messageInContext(s"This declaration has a type that is not supported.")
  }

  private case class WrongStructType(decl: Node[_]) extends UserError {
    override def code: String = "wrongStructType"

    override def text: String =
      decl.o.messageInContext(s"This has a struct type that is not supported.")
  }

  private case class WrongUniqueFieldStruct(decl: Node[_]) extends UserError {
    override def code: String = "wrongUniqueFieldStruct"

    override def text: String =
      decl.o.messageInContext(
        s"Cannot add a unique pointer field towards a field which already had one for that field."
      )
  }

  private case class WrongOpenCLLiteralVector(e: Node[_]) extends UserError {
    override def code: String = "wrongOpenCLLiteralVector"

    override def text: String =
      e.o.messageInContext(s"This OpenCL literal vector is not supported.")
  }

  private case class TypeUsedAsValue(decl: Node[_]) extends UserError {
    override def code: String = "typeUsedAsValue"

    override def text: String =
      decl.o.messageInContext(s"This type name is incorrectly used a value.")
  }

  private case class WrongGPULocalType(local: CLocalDeclaration[_])
      extends UserError {
    override def code: String = "wrongGPULocalType"
    override def text: String =
      local.o.messageInContext(
        s"This local declaration has a type that is not allowed inside a GPU kernel."
      )
  }

  private case class NotDynamicSharedMem(e: Expr[_]) extends UserError {
    override def code: String = "notDynamicSharedMem"
    override def text: String =
      e.o.messageInContext(
        s"`\\shared_mem_size` should reference a dynamic shared memory location."
      )
  }

  private case class WrongBarrierSpecifier(b: GpgpuBarrier[_])
      extends UserError {
    override def code: String = "wrongBarrierSpecifier"
    override def text: String =
      b.o.messageInContext(s"The barrier has incorrect specifiers.")
  }

  private case class UnsupportedBarrierPermission(e: Node[_])
      extends UserError {
    override def code: String = "unsupportedBarrierPermission"
    override def text: String =
      e.o.messageInContext(s"This is unsupported for barrier for now.")
  }

  private case class RedistributingBarrier(
      v: CNameTarget[_],
      barrier: GpgpuBarrier[_],
      global: Boolean,
  ) extends UserError {
    def memFence: String =
      if (global)
        "CLK_GLOBAL_MEM_FENCE"
      else
        "CLK_LOCAL_MEM_FENCE"
    override def code: String = "redistributingBarrier"
    override def text: String =
      barrier.o.messageInContext(
        s"Trying to redistribute the variable `$v` in a GPU barrier, but need the fence `$memFence` to do this."
      )
  }

  private case class CDoubleContracted(
      decl: CGlobalDeclaration[_],
      defn: CFunctionDefinition[_],
  ) extends UserError {
    override def code: String = "multipleContracts"
    override def text: String =
      Message.messagesInContext(
        defn.o -> "This method has a non-empty contract at its definition, ...",
        decl.o -> "... but its forward declaration also has a contract.",
      )
  }

  private case class KernelNotInjective(kernel: CGpgpuKernelSpecifier[_])
      extends Blame[ReceiverNotInjective] {
    override def blame(error: ReceiverNotInjective): Unit =
      kernel.blame
        .blame(KernelPredicateNotInjective(Left(kernel), error.resource))
  }

  private case class KernelParFailure(kernel: CGpgpuKernelSpecifier[_])
      extends Blame[ParBlockFailure] {
    override def blame(error: ParBlockFailure): Unit =
      error match {
        case ParPredicateNotInjective(_, predicate) =>
          kernel.blame
            .blame(KernelPredicateNotInjective(Left(kernel), predicate))
        case ParPreconditionFailed(_, _) =>
          PanicBlame(
            "Kernel parallel block precondition cannot fail, since an identical predicate is required before."
          ).blame(error)
        case ParBlockPostconditionFailed(failure, _) =>
          kernel.blame.blame(KernelPostconditionFailed(failure, Left(kernel)))
        case ParBlockMayNotThrow(_) =>
          PanicBlame(
            "Please don't throw exceptions from a gpgpu kernel, it's not polite."
          ).blame(error)
      }
  }

  private case class KernelBarrierFailure(barrier: GpgpuBarrier[_])
      extends Blame[ParBarrierFailure] {
    override def blame(error: ParBarrierFailure): Unit =
      error match {
        case ParBarrierNotEstablished(failure, _) =>
          barrier.blame.blame(KernelBarrierNotEstablished(failure, barrier))
        case ParBarrierInconsistent(failure, _) =>
          barrier.blame.blame(KernelBarrierInconsistent(failure, barrier))
        case ParBarrierMayNotThrow(_) =>
          PanicBlame(
            "Please don't throw exceptions from a gpgpu barrier, it's not polite."
          ).blame(error)
        case ParBarrierInvariantBroken(failure, _) =>
          barrier.blame.blame(KernelBarrierInvariantBroken(failure, barrier))
      }
  }

  private case class ArrayMallocFailed(inv: CInvocation[_])
      extends Blame[ArraySizeError] {
    override def blame(error: ArraySizeError): Unit =
      error match {
        case ArraySize(arr) => inv.blame.blame(MallocSize(inv))
        case other => throw Unreachable(s"Invalid invocation failure: $other")
      }
  }

  private case class VectorBoundFailed(subscript: AmbiguousSubscript[_])
      extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit =
      error match {
        case PreconditionFailed(path, _, _) =>
          path match {
            case FailLeft +: _ =>
              subscript.blame.blame(VectorBoundNegative(subscript))
            case FailRight +: _ =>
              subscript.blame.blame(VectorBoundExceedsLength(subscript))
            case _ => PanicBlame("Should not occur")
          }
        case other => throw Unreachable(s"Invalid invocation failure: $other")
      }
  }

  private case class UnsupportedCast(c: CCast[_]) extends UserError {
    override def code: String = "unsupportedCast"
    override def text: String =
      c.o.messageInContext("This cast is not supported")
  }

  private case class UnsupportedMalloc(c: Expr[_]) extends UserError {
    override def code: String = "unsupportedMalloc"
    override def text: String =
      c.o.messageInContext(
        "Only 'malloc' of the format '(t *) malloc(x*sizeof(t)' is supported."
      )
  }

  private case class TypeSizeMayBeZeroBlame(
      c: CCast[_],
      blame: Blame[FrontendInvocationError],
  ) extends Blame[DivByZero] {
    override def blame(error: DivByZero): Unit =
      blame.blame(TypeSizeMayBeZero(c))
  }

  private case class InvalidPointerComparison(cmp: Expr[_]) extends UserError {
    override def code: String = "incompatiblePointerComparison"
    override def text: String =
      cmp.o.messageInContext(
        "Comparison between pointers of different types is not supported"
      )
  }

}

case class LangCToCol[Pre <: Generation](rw: LangSpecificToCol[Pre])
    extends LazyLogging {
  import LangCToCol._
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  private val cFunctionSuccessor
      : SuccessionMap[CFunctionDefinition[Pre], Procedure[Post]] =
    SuccessionMap()
  private val cFunctionDeclSuccessor
      : SuccessionMap[(CGlobalDeclaration[Pre], Int), Procedure[Post]] =
    SuccessionMap()
  private val cNameSuccessor: SuccessionMap[CNameTarget[Pre], Variable[Post]] =
    SuccessionMap()
  private val cLocalHeapNameSuccessor
      : SuccessionMap[CNameTarget[Pre], LocalHeapVariable[Post]] =
    SuccessionMap()
  private val cGlobalNameSuccessor
      : SuccessionMap[CNameTarget[Pre], HeapVariable[Post]] = SuccessionMap()
  private val cStructSuccessor
      : SuccessionMap[CGlobalDeclaration[Pre], Class[Post]] = SuccessionMap()
  private val cStructFieldsSuccessor: SuccessionMap[
    (CGlobalDeclaration[Pre], CStructMemberDeclarator[Pre]),
    InstanceField[Post],
  ] = SuccessionMap()
  private val cCurrentDefinitionParamSubstitutions
      : ScopedStack[Map[CParam[Pre], CParam[Pre]]] = ScopedStack()
  private val sizeOfFunctions: SuccessionMap[Type[Pre], Function[Post]] =
    SuccessionMap()

  private val cudaCurrentThreadIdx: ScopedStack[CudaVec] = ScopedStack()
  private val cudaCurrentBlockIdx: ScopedStack[CudaVec] = ScopedStack()
  private val cudaCurrentBlockDim: ScopedStack[CudaVec] = ScopedStack()
  private val cudaCurrentGridDim: ScopedStack[CudaVec] = ScopedStack()
  private val cudaConstantDims: ScopedStack[ConstantCudaVec] = ScopedStack()

  private val cudaCurrentGrid: ScopedStack[ParBlockDecl[Post]] = ScopedStack()
  private val cudaCurrentBlock: ScopedStack[ParBlockDecl[Post]] = ScopedStack()

  private val dynamicSharedMemNames: mutable.Set[CNameTarget[Pre]] = mutable
    .Set()
  private val dynamicSharedMemLengthVar
      : mutable.Map[CNameTarget[Pre], Variable[Post]] = mutable.Map()
  private val staticSharedMemNames
      : mutable.Map[CNameTarget[Pre], (BigInt, Option[Blame[ArraySizeError]])] =
    mutable.Map()
  private val globalMemNames: mutable.Set[RefCParam[Pre]] = mutable.Set()
  private var kernelSpecifier: Option[CGpgpuKernelSpecifier[Pre]] = None
  private val functionPointers
      : mutable.Map[CFunctionDefinition[Pre], Function[Post]] = mutable.Map()

  private def CStructOrigin(sdecl: CStructDeclaration[_]): Origin =
    sdecl.o.sourceName(sdecl.name.get).withContent(TypeName("struct"))

  private def CStructFieldOrigin(cdecl: CDeclarator[_]): Origin =
    cdecl.o.sourceName(nameFromDeclarator(cdecl))

  private def CudaIndexVariableOrigin(dim: RefCudaVecDim[_]): Origin =
    Origin(Seq(
      PreferredName(Seq(dim.vec.name, dim.name)),
      LabelContext(s"${dim.vec.name}/${dim.name}"),
    ))

  class CudaVec(ref: RefCudaVec[Pre])(implicit val o: Origin) {
    val indices: ListMap[RefCudaVecDim[Pre], Variable[Post]] = ListMap(
      Seq(RefCudaVecX[Pre](ref), RefCudaVecY[Pre](ref), RefCudaVecZ[Pre](ref))
        .map(dim =>
          dim -> new Variable[Post](TCInt())(CudaIndexVariableOrigin(dim))
        ): _*
    )
  }

  class ConstantCudaVec(var indicesConstant: Seq[RefCudaVecDim[Pre]])(
      implicit val o: Origin
  ) {
    var indices: ListMap[RefCudaVecDim[Pre], Variable[Post]] = ListMap(
      indicesConstant.map(dim =>
        dim -> new Variable[Post](TCInt())(CudaIndexVariableOrigin(dim))
      ): _*
    )
    // Always keep the x var, if all variables in the block/grid are constant
    if (
      indicesConstant.collect {
        case v: RefCudaVecDim[Pre]
            if v.vec.isInstanceOf[RefCudaThreadIdx[Pre]] =>
          v
      }.size == 3
    ) { indices = indices.removed(RefCudaVecX[Pre](RefCudaThreadIdx[Pre]())) }
    if (
      indicesConstant.collect {
        case v: RefCudaVecDim[Pre]
            if v.vec.isInstanceOf[RefCudaBlockIdx[Pre]] =>
          v
      }.size == 3
    ) { indices = indices.removed(RefCudaVecX[Pre](RefCudaBlockIdx[Pre]())) }

    def contains(e: RefCudaVecDim[Pre]): Boolean = indicesConstant.contains(e)
  }

  private def hasNoSharedMemNames(node: Node[Pre]): Boolean = {
    val allowedNonRefs = Set(
      "get_local_id",
      "get_group_id",
      "get_local_size",
      "get_num_groups",
      "get_global_size",
    )

    def varIsNotShared(l: CLocal[Pre]): Boolean = {
      l.ref match {
        case Some(ref: CNameTarget[Pre])
            if dynamicSharedMemNames.contains(ref) ||
              staticSharedMemNames.contains(ref) =>
          return false
        case None =>
          if (!allowedNonRefs.contains(l.name))
            Unreachable("Should not happen")
        case _ =>
      }
      true
    }

    node match {
      // SharedMemSize gets rewritten towards the length of a shared memory name, so is valid in global context
      case _: SharedMemSize[Pre] =>
      case l: CLocal[Pre] => return varIsNotShared(l)
      case e =>
        if (!e.subnodes.forall(hasNoSharedMemNames))
          return false
    }
    true
  }

  def rewriteUnit(cUnit: CTranslationUnit[Pre]): Unit = {
    cUnit.declarations.foreach(rw.dispatch)
  }

  def sharedSize(shared: SharedMemSize[Pre]): Expr[Post] = {
    val SharedMemSize(pointer) = shared

    val res =
      pointer match {
        case loc: CLocal[Pre] =>
          loc.ref flatMap { dynamicSharedMemLengthVar.get } map { v =>
            Local[Post](v.ref)(shared.o)
          }
        case _ => None
      }
    res.getOrElse(throw NotDynamicSharedMem(pointer))
  }

  private def isRatFloatOrInt(t: Type[Pre]): Boolean =
    getBaseType(t) match {
      case _: FloatType[Pre] => true
      case _: TRational[Pre] => true
      case _: IntType[Pre] => true
      case _ => false
    }

  private def isInt(t: Type[Pre]): Boolean =
    getBaseType(t) match {
      case _: IntType[Pre] => true
      case _ => false
    }

  private def isFloat(t: Type[Pre]): Boolean =
    getBaseType(t) match {
      case _: FloatType[Pre] => true
      case _ => false
    }

  @tailrec
  private def getBaseType[G](t: Type[G]): Type[G] =
    t match {
      case CPrimitiveType(specs) =>
        getBaseType(C.getPrimitiveType(specs, None, Some(t)))
      case TUnique(it, _) => getBaseType(it)
      case TConst(it) => getBaseType(it)
      case t @ TClassUnique(cls, _) => t.inner
      case CTStructUnique(it, _, _) => getBaseType(it)
      case _ => t
    }

  @tailrec
  private def isConst[G](t: Type[G]): Boolean =
    t match {
      case CPrimitiveType(specs) =>
        isConst(C.getPrimitiveType(specs, None, Some(t)))
      case TConst(it) => true
      case _ => false
    }

  @tailrec
  private def getUnique[G](t: Type[G]): Option[BigInt] =
    t match {
      case CPrimitiveType(specs) =>
        getUnique(C.getPrimitiveType(specs, None, Some(t)))
      case TUnique(it, unique) => Some(unique)
      case _ => None
    }

  private def getStructType[G](t: Type[G]): CType[G] =
    t match {
      case CPrimitiveType(specs) => ???
      // getStructType(C.getPrimitiveType(specs, None, Some(t)))
      case st: CTStructUnique[G] => st
      case st: CTStruct[G] => st
      case _ => throw WrongStructType(t)
    }

  private def getBaseStructTypeWithUnique[G](
      t: Type[G],
      m: Map[Ref[G, CStructMemberDeclarator[G]], BigInt] =
        Map[Ref[G, CStructMemberDeclarator[G]], BigInt](),
  ): Option[(CTStruct[G], Map[Ref[G, CStructMemberDeclarator[G]], BigInt])] =
    t match {
      case CPrimitiveType(specs) => ???
//        getBaseStructTypeWithUnique(C.getPrimitiveType(specs, Some(t)), m)
      case CTStructUnique(it, ref, unique) =>
        if (m.contains(ref))
          throw WrongUniqueFieldStruct(t)
        getBaseStructTypeWithUnique(it, m + (ref -> unique))
      case ts: CTStruct[G] => Some(ts, m)
      case _ => None
    }

  private def castIsId(exprType: Type[Pre], castType: Type[Pre]): Boolean = {
    (getBaseType(castType), getBaseType(exprType)) match {
      case (tc, te) if tc == te => true
      case (TCInt(), TBoundedInt(_, _)) => true
      case (TBoundedInt(_, _), TCInt()) => true
      case _ => false
    }
  }

  def sizeOf(t: Type[Pre], sizeOfOrigin: Origin): Expr[Post] = {
    implicit val o: Origin = t.o
    t.bits match {
      case TypeSize.Exact(size) => c_const(size / 8)(sizeOfOrigin)
      case b @ (TypeSize.Unknown() | TypeSize.Minimally(_)) =>
        functionInvocation(
          TrueSatisfiable,
          sizeOfFunctions.getOrElseUpdate(
            t, {
              rw.globalDeclarations.declare(withResult((result: Result[Post]) =>
                function[Post](
                  AbstractApplicable,
                  TrueSatisfiable,
                  TCInt(),
                  ensures = UnitAccountedPredicate(b match {
                    case TypeSize.Unknown() => tt
                    case TypeSize.Minimally(size) => result >= c_const(size / 8)
                  }),
                )(o.where(name = s"sizeOf_$t"))
              ))
            },
          ).ref[Function[Post]],
        )(sizeOfOrigin)
    }
  }

  def rewriteComparison(cmp: Expr[Pre]): Expr[Post] = {
    val (l, r) =
      cmp match {
        case AmbiguousEq(left, right, _, None) => (left, right)
        case AmbiguousNeq(left, right, _, None) => (left, right)
        case AmbiguousGreater(left, right, None) => (left, right)
        case AmbiguousLess(left, right, None) => (left, right)
        case AmbiguousGreaterEq(left, right, None) => (left, right)
        case AmbiguousLessEq(left, right, None) => (left, right)
        case _ => return cmp.rewriteDefault()
      }

    val leftPtr = l.t.asPointer
    val rightPtr = r.t.asPointer
    if (leftPtr.isDefined && rightPtr.isDefined) {
      if (
        CoercionUtils.getAnyCoercion(leftPtr.get.element, rightPtr.get.element)
          .isDefined
      ) {
        val elementSize = Some(sizeOf(leftPtr.get.element, cmp.o))
        cmp match {
          case e @ AmbiguousEq(_, _, _, _) =>
            e.rewrite(elementSize = elementSize)
          case e @ AmbiguousNeq(_, _, _, _) =>
            e.rewrite(elementSize = elementSize)
          case e @ AmbiguousGreater(_, _, _) =>
            e.rewrite(elementSize = elementSize)
          case e @ AmbiguousLess(_, _, _) =>
            e.rewrite(elementSize = elementSize)
          case e @ AmbiguousGreaterEq(_, _, _) =>
            e.rewrite(elementSize = elementSize)
          case e @ AmbiguousLessEq(_, _, _) =>
            e.rewrite(elementSize = elementSize)
        }
      } else { throw InvalidPointerComparison(cmp) }
    } else { cmp.rewriteDefault() }
  }

  def cast(c: CCast[Pre]): Expr[Post] =
    c match {
      case CCast(e, t) if castIsId(e.t, t) => rw.dispatch(c.expr)
      case CCast(e, t @ TCInt()) if isInt(e.t) =>
        Cast(rw.dispatch(e), TypeValue(rw.dispatch(t))(c.o))(c.o)
      case CCast(e, t)
          if (isFloat(t) && isRatFloatOrInt(e.t)) ||
            (isRatFloatOrInt(t) && isFloat(e.t)) =>
        // We can convert between rationals, integers and floats
        CastFloat[Post](rw.dispatch(c.expr), rw.dispatch(t))(c.o)

      case cast @ CCast(_, TOpenCLVector(_, _)) =>
        createOpenCLLiteralVector(cast)
      case CCast(
            inv @ CInvocation(
              CLocal("__vercors_malloc"),
              Seq(arg),
              Nil,
              Nil,
              false,
            ),
            tcast,
          ) =>
        val t2 =
          tcast match {
            case CTPointer(t2) => t2
            case _ => throw UnsupportedMalloc(c)
          }
        val (t1, size) =
          arg match {
            case SizeOf(t1) if castIsId(t1, t2) => (t1, c_const[Post](1)(c.o))
            case AmbiguousMult(l, SizeOf(t1)) if castIsId(t1, t2) =>
              (t1, rw.dispatch(l))
            case AmbiguousMult(SizeOf(t1), r) if castIsId(t1, t2) =>
              (t1, rw.dispatch(r))
            case _ =>
              (
                t2,
                FloorDiv(rw.dispatch(arg), sizeOf(t2, c.o))(
                  TypeSizeMayBeZeroBlame(c, inv.blame)
                )(c.o),
              )
          }
        NewPointerArray(rw.dispatch(t2), size, None)(ArrayMallocFailed(inv))(
          c.o
        )
      case CCast(CInvocation(CLocal("__vercors_malloc"), _, _, _, _), _) =>
        throw UnsupportedMalloc(c)
      case CCast(n @ Null(), t) if t.asPointer.isDefined => rw.dispatch(n)
      case CCast(e, t) if e.t.asPointer.isDefined && t.asPointer.isDefined =>
        val eElement = e.t.asPointer.get.element
        val tElement = t.asPointer.get.element
        val newEElement = getBaseType(rw.dispatch(eElement))
        val newTElement = getBaseType(rw.dispatch(tElement))
        if (
          newEElement == TVoid[Post]() || newTElement == TVoid[Post]() ||
          CoercionUtils.firstElementIsType(newEElement, newTElement) ||
          CoercionUtils.firstElementIsType(newTElement, newEElement)
        ) {
          PointerCast(
            rw.dispatch(e),
            rw.dispatch(t),
            sizeOf(eElement, c.o),
            sizeOf(tElement, c.o),
          )(c.o)
        } else { throw UnsupportedCast(c) }
      case CCast(e, t @ TCInt()) if e.t.asPointer.isDefined =>
        if (isUniquePointerElement(e.t.asPointer.get.element))
          throw UnsupportedCast(c)
        IntegerPointerCast(
          rw.dispatch(e),
          rw.dispatch(t),
          getStride(e.t.asPointer.get.element, c.o),
        )(c.o)
      case CCast(e, t @ CTPointer(innerType))
          if getBaseType(e.t).isInstanceOf[TCInt[Pre]] &&
            !isUniquePointerElement(innerType) =>
        IntegerPointerCast(
          rw.dispatch(e),
          TPointer(rw.dispatch(innerType), None),
          getStride(innerType, c.o),
        )(c.o)
      case _ => throw UnsupportedCast(c)
    }

  // Traverse all type qualifiers
  private def isUniquePointerElement(t: Type[Pre]): Boolean =
    t match {
      case TUnique(_, _) => true
      case TConst(inner) => isUniquePointerElement(inner)
      case CTStructUnique(inner, _, _) => isUniquePointerElement(inner)
      case _ => false
    }

  private def getStride(t: Type[Pre], o: Origin): Expr[Post] =
    t match {
      case TVoid() => c_const(1)(o)
      case _ => sizeOf(t, o)
    }

  private def rewriteGPUParam(
      cParam: CParam[Pre],
      kernelSpecifier: CGpgpuKernelSpecifier[Pre],
  ): Unit = {
    cParam.drop()
    implicit val o: Origin = cParam.o
    val varO = o.sourceName(C.getDeclaratorInfo(cParam.declarator).name)
    val cRef = RefCParam(cParam)
    val tp = new TypeProperties(cParam.specifiers, cParam)

    kernelSpecifier match {
      case OpenCLKernel() =>
        if (tp.isGlobal && tp.arrayOrPointer && !tp.extern)
          globalMemNames.add(cRef)
        else if (
          tp.isShared && tp.arrayOrPointer && !tp.extern &&
          tp.innerType.isDefined
        ) {
          addDynamicShared(cRef, tp.innerType.get, varO)
          // Return, since shared memory locations are declared and initialized at thread block level, not kernel level
          return
        } else if (!tp.shared && !tp.global && !tp.arrayOrPointer && !tp.extern)
          ()
        else
          throw WrongGPUKernelParameterType(cParam)
      case CUDAKernel() =>
        if (!tp.global && !tp.shared && tp.arrayOrPointer && !tp.extern)
          globalMemNames.add(cRef)
        else if (!tp.shared && !tp.global && !tp.arrayOrPointer && !tp.extern)
          ()
        else
          throw WrongGPUKernelParameterType(cParam)
    }

    val v =
      new Variable[Post](cParam.specifiers.collectFirst {
        case t: CSpecificationType[Pre] => rw.dispatch(t.t)
      }.get)(varO)
    cNameSuccessor(cRef) = v
    rw.variables.declare(v)
  }

  def rewriteParam(cParam: CParam[Pre]): Unit = {
    if (kernelSpecifier.isDefined)
      return rewriteGPUParam(cParam, kernelSpecifier.get)
    cParam.specifiers.collectFirst {
      case GPULocal() => throw WrongGPUType(cParam)
      case GPUGlobal() => throw WrongGPUType(cParam)
    }
    val prop = new TypeProperties(cParam.specifiers, cParam)
    if (!prop.validCParam)
      throw WrongCType(cParam)
    val specType = rw.dispatch(prop.mainType.get)

    cParam.drop()
    val v =
      new Variable[Post](specType)(
        cParam.o.sourceName(C.getDeclaratorInfo(cParam.declarator).name)
      )
    cNameSuccessor(RefCParam(cParam)) = v
    rw.variables.declare(v)
  }

  def rewriteFunctionDef(func: CFunctionDefinition[Pre]): Unit = {
    func.drop()
    val info = C.getDeclaratorInfo(func.declarator)
    val returnType =
      func.specs.collectFirst { case t: CSpecificationType[Pre] =>
        rw.dispatch(t.t)
      }.get
    val pure = func.specs.collectFirst { case CPure() => () }.isDefined
    val inline = func.specs.collectFirst { case CInline() => () }.isDefined
    val opaque = func.specs.collectFirst { case COpaque() => () }.isDefined

    val (contract, subs: Map[CParam[Pre], CParam[Pre]]) =
      func.ref match {
        case Some(RefCGlobalDeclaration(decl, idx))
            if decl.decl.contract.nonEmpty =>
          if (func.contract.nonEmpty)
            throw CDoubleContracted(decl, func)

          val declParams =
            C.getDeclaratorInfo(decl.decl.inits(idx).decl).params.get
          val defnParams = info.params.get

          (decl.decl.contract, declParams.zip(defnParams).toMap)
        case _ => (func.contract, Map.empty)
      }

    val namedO = func.o.sourceName(info.name)
    val proc =
      cCurrentDefinitionParamSubstitutions.having(subs) {
        rw.globalDeclarations.declare(
          func.specs.collectFirst { case k: CGpgpuKernelSpecifier[Pre] =>
            kernelProcedure(namedO, contract, info, Some(func.body), k)
          }.getOrElse({
            val params =
              rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1
            rw.labelDecls.scope {
              new Procedure[Post](
                returnType = returnType,
                args = params,
                outArgs = Nil,
                typeArgs = Nil,
                body = Some(rw.dispatch(func.body)),
                contract = rw.dispatch(contract),
                inline = inline,
                pure = pure,
                opaque = opaque,
              )(func.blame)(namedO)
            }
          })
        )
      }

    cFunctionSuccessor(func) = proc

    func.ref match {
      case Some(RefCGlobalDeclaration(decl, idx)) =>
        cFunctionDeclSuccessor((decl, idx)) = proc
      case None => // ok
    }
  }

  def all(
      blame: Blame[ReceiverNotInjective]
  )(idx: CudaVec, dim: CudaVec, e: Expr[Post]): Expr[Post] = {
    foldStar(unfoldStar(e).map(allOneExpr(blame)(idx, dim, _)))(e.o)
  }

  private def allOneExpr(
      blame: Blame[ReceiverNotInjective]
  )(idx: CudaVec, dim: CudaVec, e: Expr[Post]): Expr[Post] = {
    implicit val o: Origin = e.o

    val vars = findVars(e)
    val nonConstant = idx.indices.zip(dim.indices.values).filter {
      case ((cudaIndex, _), _) => !cudaConstantDims.top.contains(cudaIndex)
    }.map { case ((_, index), dim) => (index, dim) }

    val (filteredIdx, otherIdx) = nonConstant.partition { case (i, _) =>
      vars.contains(i)
    }
    // Substitute all constant values (except threadIdx.x and blockIdx.x if all threads/blocks are constant)
    val sub = Substitute[Post](
      cudaConstantDims.top.indices.values.map(v => (v.get, c_const[Post](0)))
        .toMap
    )
    val eSub = sub.dispatch(e)

    val (prevBindings, b, prevBlame) =
      eSub match {
        case s @ Starall(bindings, Nil, b) => (bindings, b, Some(s.blame))
        case Forall(bindings, Nil, b) => (bindings, b, None)
        case b => (Seq(), b, None)
      }

    val body =
      otherIdx.map { case (_, range) => range }
        .foldLeft(b)((newE, scaleFactor) =>
          Scale(scaleFactor.get, newE)(PanicBlame("Framed positive"))
        )
    if (filteredIdx.isEmpty) {
      if (prevBindings.isEmpty)
        body
      else {
        prevBlame match {
          case Some(oldBlame) => Starall(prevBindings, Nil, body)(oldBlame)
          case None => Forall(prevBindings, Nil, body)
        }
      }
    } else {
      Starall(
        filteredIdx.map { case (v, _) => v }.toSeq ++ prevBindings,
        Nil,
        Implies(
          foldAnd(filteredIdx.map { case (idx, dim) =>
            c_const[Post](0) <= idx.get && idx.get < dim.get
          }),
          body,
        ),
      )(blame)
    }
  }

  def findVars(
      e: Node[Post],
      vars: Set[Variable[Post]] = Set(),
  ): Set[Variable[Post]] =
    e match {
      case Local(ref) => vars + ref.decl
      case _ => e.collect { case Local(Ref(v)) => v }.toSet
    }

  def allThreadsInBlock(
      blame: Blame[ReceiverNotInjective]
  )(e: Expr[Pre]): Expr[Post] = {
    val thread = new CudaVec(RefCudaThreadIdx())(e.o)
    cudaCurrentThreadIdx.having(thread) {
      all(blame)(thread, cudaCurrentBlockDim.top, rw.dispatch(e))
    }
  }

  def allThreadsInGrid(
      blame: Blame[ReceiverNotInjective]
  )(e: Expr[Pre]): Expr[Post] = {
    val block = new CudaVec(RefCudaBlockIdx())(e.o)
    cudaCurrentBlockIdx.having(block) {
      all(blame)(block, cudaCurrentGridDim.top, allThreadsInBlock(blame)(e))
    }
  }

  def getCDecl(d: CNameTarget[Pre]): CDeclarator[Pre] =
    d match {
      case RefCParam(decl) => decl.declarator
      case RefCFunctionDefinition(decl) => decl.declarator
      case RefCGlobalDeclaration(decls, initIdx) =>
        decls.decl.inits(initIdx).decl
      case RefCLocalDeclaration(decls, initIdx) =>
        decls.decl.inits(initIdx).decl
      case _ => throw Unreachable("Should not happen")
    }

  def declareSharedMemory()
      : (Seq[Variable[Post]], (Seq[Variable[Post]], Seq[Statement[Post]])) =
    rw.variables.collect {
      var declarations: Seq[Variable[Post]] = Seq()
      var inits: Seq[Statement[Post]] = Seq()
      dynamicSharedMemNames.foreach(d => {
        implicit val o: Origin = getCDecl(d).o
        val varO: Origin = o
          .where(name = s"${C.getDeclaratorInfo(getCDecl(d)).name}_size")
        val v = new Variable[Post](TCInt())(varO)
        dynamicSharedMemLengthVar(d) = v
        rw.variables.declare(v)
        val assign: Statement[Post] = assignLocal(
          Local(cNameSuccessor(d).ref),
          NewNonNullPointerArray[Post](
            cNameSuccessor(d).t.asPointer.get.element,
            Local(v.ref),
            None,
          )(PanicBlame("Shared memory sizes cannot be negative.")),
        )
        declarations ++= Seq(cNameSuccessor(d))
        inits ++= Seq(assign)
      })
      staticSharedMemNames.foreach { case (d, (size, blame)) =>
        implicit val o: Origin = getCDecl(d).o
        val assign: Statement[Post] = assignLocal(
          Local(cNameSuccessor(d).ref),
          // Since we set the size and blame together, we can assume the blame is not None
          NewNonNullPointerArray[Post](
            cNameSuccessor(d).t.asPointer.get.element,
            c_const(size),
            None,
          )(blame.get),
        )
        declarations ++= Seq(cNameSuccessor(d))
        inits ++= Seq(assign)
      }

      (declarations, inits)
    }

  // Returns the threadIdx or blockIdx if its dim is a constant 1
  // E.g. blockDim.x == 1 returns threadIdx.x
  def dimsOfSize1Expr(e: Expr[Pre]): Option[RefCudaVecDim[Pre]] = {
    // If it is block or grid dim, get the idx from it
    def getIdx(dim: RefCudaVec[Pre]): Option[RefCudaVec[Pre]] =
      dim match {
        case RefCudaBlockDim() => Some(RefCudaThreadIdx())
        case RefCudaGridDim() => Some(RefCudaBlockIdx())
        case _ => None
      }

    def cudaVec(target: CDerefTarget[Pre]): Option[RefCudaVecDim[Pre]] =
      target match {
        case RefCudaVecX(vec) => getIdx(vec).map(RefCudaVecX[Pre])
        case RefCudaVecY(vec) => getIdx(vec).map(RefCudaVecY[Pre])
        case RefCudaVecZ(vec) => getIdx(vec).map(RefCudaVecZ[Pre])
        case _ => None
      }

    e match {
      case AmbiguousEq(acc: CFieldAccess[Pre], CIntegerValue(i, _), _, _)
          if i == 1 =>
        cudaVec(acc.ref.get)
      case AmbiguousEq(CIntegerValue(i, _), acc: CFieldAccess[Pre], _, _)
          if i == 1 =>
        cudaVec(acc.ref.get)
      case _ => None
    }
  }

  def dimsOfSize1(
      contract: ApplicableContract[Pre]
  ): Seq[RefCudaVecDim[Pre]] = {
    val UnitAccountedPredicate(contractRequires: Expr[Pre]) = contract.requires
    (unfoldStar(contractRequires).flatMap(dimsOfSize1Expr) ++
      unfoldStar(contract.contextEverywhere).flatMap(dimsOfSize1Expr)).distinct
  }

  def kernelProcedure(
      o: Origin,
      contract: ApplicableContract[Pre],
      info: C.DeclaratorInfo[Pre],
      body: Option[Statement[Pre]],
      kernelSpec: CGpgpuKernelSpecifier[Pre],
  ): Procedure[Post] = {
    dynamicSharedMemNames.clear()
    staticSharedMemNames.clear()
    kernelSpecifier = Some(kernelSpec)
    val constantIdx = new ConstantCudaVec(dimsOfSize1(contract))(o)

    val blockDim = new CudaVec(RefCudaBlockDim())(o)
    val gridDim = new CudaVec(RefCudaGridDim())(o)
    cudaConstantDims.having(constantIdx) {
      cudaCurrentBlockDim.having(blockDim) {
        cudaCurrentGridDim.having(gridDim) {
          val args =
            rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1
          val implFiltered = body.map(init => filterSharedDecl(init))

          rw.variables.collect {
            dynamicSharedMemNames
              .foreach(d => rw.variables.declare(cNameSuccessor(d)))
          }
          rw.variables.collect {
            staticSharedMemNames
              .foreach(d => rw.variables.declare(cNameSuccessor(d._1)))
          }
          val (
            sharedMemSizes,
            (
              sharedMemDecls: Seq[Variable[Post]],
              sharedMemInits: Seq[Statement[Post]],
            ),
          ) = declareSharedMemory()

          val newArgs =
            blockDim.indices.values.toSeq ++ gridDim.indices.values.toSeq ++
              args ++ sharedMemSizes
          val newGivenArgs = rw.variables.dispatch(contract.givenArgs)
          val newYieldsArgs = rw.variables.dispatch(contract.yieldsArgs)
          // We add the requirement that a GPU kernel must always have threads (non zero block or grid dimensions)
          val nonZeroThreads: Expr[Post] =
            foldStar(
              (blockDim.indices.values ++ gridDim.indices.values)
                .map(v => Less(c_const(0)(o), v.get(o))(o)).toSeq
            )(o)
          val UnitAccountedPredicate(contractRequires: Expr[Pre]) =
            contract.requires
          val UnitAccountedPredicate(contractEnsures: Expr[Pre]) =
            contract.ensures

          val parBody = body.map(impl => {
            implicit val o: Origin = impl.o
            val threadIdx = new CudaVec(RefCudaThreadIdx())
            val blockIdx = new CudaVec(RefCudaBlockIdx())
            val gridDecl = new ParBlockDecl[Post]()
            val blockDecl = new ParBlockDecl[Post]()

            cudaCurrentThreadIdx.having(threadIdx) {
              cudaCurrentBlockIdx.having(blockIdx) {
                cudaCurrentGrid.having(gridDecl) {
                  cudaCurrentBlock.having(blockDecl) {
                    val contextBlock = foldStar(
                      unfoldStar(contract.contextEverywhere)
                        .filter(hasNoSharedMemNames)
                        .map(allThreadsInBlock(KernelNotInjective(kernelSpec)))
                    )
                    def getIters(idxs: CudaVec, dims: CudaVec)
                        : Seq[IterVariable[Post]] = {
                      var innerIters =
                        idxs.indices.zip(dims.indices.values)
                          .filter { case ((cudaIndex, _), _) =>
                            !constantIdx.contains(cudaIndex)
                          }.map { case ((_, index), dim) =>
                            IterVariable(index, c_const(0), dim.get)
                          }.toSeq
                      // Always have atleast the x dimension
                      if (innerIters.isEmpty) {
                        innerIters = Seq(IterVariable(
                          idxs.indices.head._2,
                          c_const(0),
                          dims.indices.head._2.get,
                        ))
                      }
                      innerIters
                    }

                    // Substitute all constant values (except threadIdx.x and blockIdx.x if all threads/blocks are constant)
                    val sub = Substitute[Post](
                      cudaConstantDims.top.indices.values
                        .map(v => (v.get, c_const[Post](0))).toMap
                    )

                    val innerContent = ParStatement(
                      ParBlock(
                        decl = blockDecl,
                        iters = getIters(threadIdx, blockDim),
                        // Context is already inherited
                        context_everywhere = Star(
                          nonZeroThreads,
                          sub.dispatch(rw.dispatch(contract.contextEverywhere)),
                        ),
                        requires = sub.dispatch(rw.dispatch(contractRequires)),
                        ensures = sub.dispatch(rw.dispatch(contractEnsures)),
                        content = rw.dispatch(implFiltered.get),
                      )(KernelParFailure(kernelSpec))
                    )
                    val parStmt = ParStatement[Post](
                      ParBlock(
                        decl = gridDecl,
                        iters = getIters(blockIdx, gridDim),
                        // Context is added to requires and ensures here
                        context_everywhere = tt,
                        requires = Star(
                          nonZeroThreads,
                          Star(
                            contextBlock,
                            foldStar(
                              unfoldStar(contractRequires)
                                .filter(hasNoSharedMemNames)
                                .map(allThreadsInBlock(
                                  KernelNotInjective(kernelSpec)
                                ))
                            ),
                          ),
                        ),
                        ensures = Star(
                          nonZeroThreads,
                          Star(
                            contextBlock,
                            foldStar(
                              unfoldStar(contractEnsures)
                                .filter(hasNoSharedMemNames)
                                .map(allThreadsInBlock(
                                  KernelNotInjective(kernelSpec)
                                ))
                            ),
                          ),
                        ),
                        // Add shared memory initialization before beginning of inner parallel block
                        content = Scope[Post](
                          sharedMemDecls,
                          Block[Post](sharedMemInits ++ Seq(innerContent)),
                        ),
                      )(KernelParFailure(kernelSpec))
                    )
                    // For constant
                    val constants: Seq[Variable[Post]] =
                      constantIdx.indices.values.toSeq
                    if (constants.nonEmpty) {
                      Scope[Post](
                        constants,
                        Block[Post](
                          constants
                            .map(c => assignLocal(c.get, c_const[Post](0))) ++
                            Seq(parStmt)
                        ),
                      )
                    } else { parStmt }
                  }
                }
              }
            }
          })

          val gridContext: Expr[Post] =
            foldStar(
              unfoldStar(contract.contextEverywhere).filter(hasNoSharedMemNames)
                .map(allThreadsInGrid(KernelNotInjective(kernelSpec)))
            )(o)
          val requires: Expr[Post] =
            foldStar(
              Seq(gridContext, nonZeroThreads) ++ unfoldStar(contractRequires)
                .filter(hasNoSharedMemNames)
                .map(allThreadsInGrid(KernelNotInjective(kernelSpec)))
            )(o)
          val ensures: Expr[Post] =
            foldStar(
              Seq(gridContext, nonZeroThreads) ++ unfoldStar(contractEnsures)
                .filter(hasNoSharedMemNames)
                .map(allThreadsInGrid(KernelNotInjective(kernelSpec)))
            )(o)
          val result =
            new Procedure[Post](
              returnType = TVoid(),
              args = newArgs,
              outArgs = Nil,
              typeArgs = Nil,
              body = parBody,
              contract =
                ApplicableContract(
                  UnitAccountedPredicate(requires)(o),
                  UnitAccountedPredicate(ensures)(o),
                  // Context everywhere is already passed down in the body
                  tt,
                  contract.signals.map(rw.dispatch),
                  newGivenArgs,
                  newYieldsArgs,
                  contract.decreases.map(rw.dispatch),
                )(contract.blame)(contract.o),
            )(AbstractApplicable)(o)
          kernelSpecifier = None

          result
        }
      }
    }
  }

  class TypeProperties(
      specs: Seq[CDeclarationSpecifier[Pre]],
      decl: Declaration[Pre],
  ) {
    var arrayOrPointer = false
    var global = false
    var shared = false
    var extern = false
    var innerType: Option[Type[Pre]] = None
    var arraySize: Option[Expr[Pre]] = None
    var mainType: Option[Type[Pre]] = None

    var sizeBlame: Option[Blame[ArraySizeError]] = None
    var pure = false
    var inline = false
    var static = false

    specs.foreach {
      case GPULocal() => shared = true
      case GPUGlobal() => global = true
      case CSpecificationType(t) if t.asPointer.isDefined =>
        val (inner, size, blame) = getInnerPointerInfo(t).get
        arrayOrPointer = true
        innerType = Some(inner)
        mainType = Some(t)
        sizeBlame = blame
        arraySize = size
      case CSpecificationType(t) => mainType = Some(t)
      case CExtern() => extern = true
      case CPure() => pure = true
      case CInline() => inline = true
      case CStatic() => static = true
      // We want to make sure we process everything
      case _ => ???
    }

    def isShared: Boolean = shared && !global
    def isGlobal: Boolean = !shared && global
    def validNonGpu: Boolean = !global && !shared && mainType.isDefined
    def validReturn: Boolean = validNonGpu && !static
    def validCParam: Boolean = !pure && !inline && validNonGpu && !static
    def validCLocal = validCParam
  }

  def addDynamicShared(
      cRef: CNameTarget[Pre],
      t: Type[Pre],
      o: Origin,
  ): Unit = {
    dynamicSharedMemNames.add(cRef)
    val v = new Variable[Post](TPointer[Post](rw.dispatch(t), None))(o)
    cNameSuccessor(cRef) = v
  }

  def addStaticShared(
      cRef: CNameTarget[Pre],
      t: Type[Pre],
      o: Origin,
      declStatement: CLocalDeclaration[Pre],
      arraySize: Option[Expr[Pre]],
      sizeBlame: Option[Blame[ArraySizeError]],
  ): Unit =
    arraySize match {
      case Some(CIntegerValue(size, _)) =>
        val v = new Variable[Post](TPointer[Post](rw.dispatch(t), None))(o)
        staticSharedMemNames(cRef) = (size, sizeBlame)
        cNameSuccessor(cRef) = v
      case _ => throw WrongGPULocalType(declStatement)
    }

  def isShared(s: Statement[Pre]): Boolean = {
    val decl =
      s match {
        case CDeclarationStatement(decl) => decl
        case Block(Seq(s_inner)) => return isShared(s_inner)
        case _ => return false
      }

    if (decl.decl.inits.size != 1)
      throw MultipleSharedMemoryDeclaration(decl)

    val prop = new TypeProperties(decl.decl.specs, decl)
    if (!prop.shared)
      return false
    val init: CInit[Pre] = decl.decl.inits.head
    val varO = decl.o.sourceName(C.getDeclaratorInfo(init.decl).name)
    val cRef = RefCLocalDeclaration(decl, 0)

    kernelSpecifier match {
      case Some(CUDAKernel()) =>
        if (
          !prop.global && prop.arrayOrPointer && prop.innerType.isDefined &&
          prop.extern
        ) {
          addDynamicShared(cRef, prop.innerType.get, varO)
          return true
        }
        if (
          !prop.global && prop.arrayOrPointer && prop.innerType.isDefined &&
          !prop.extern
        ) {
          addStaticShared(
            cRef,
            prop.innerType.get,
            varO,
            decl,
            prop.arraySize,
            prop.sizeBlame,
          )
          return true
        }
      case Some(OpenCLKernel()) =>
        if (
          !prop.global && prop.arrayOrPointer && prop.innerType.isDefined &&
          !prop.extern
        ) {
          addStaticShared(
            cRef,
            prop.innerType.get,
            varO,
            decl,
            prop.arraySize,
            prop.sizeBlame,
          )
          return true
        }
      case None =>
        throw Unreachable(
          f"This should have been called from inside a GPU kernel scope."
        )
    }

    // We are shared, but couldn't add it
    throw WrongGPULocalType(decl)
  }

  def filterSharedDecl(s: Statement[Pre]): Statement[Pre] = {
    s match {
      case Scope(locals, block @ Block(stats)) =>
        Scope(locals, Block(stats.filterNot(isShared))(block.o))(s.o)
      case _ => s
    }
  }

  def getFirstTypes(aggregate: Type[Pre]): Seq[Type[Pre]] =
    aggregate match {
      case struct: CTStruct[Pre] =>
        val decls =
          struct.ref.decl.decl match {
            case CDeclaration(
                  _,
                  _,
                  Seq(CStructDeclaration(Some(_), decls)),
                  Seq(),
                ) =>
              decls
            case _ => throw WrongStructType(struct.ref.decl)
          }
        decls.headOption.map { fieldDecl =>
          val CStructMemberDeclarator(
            specs: Seq[CDeclarationSpecifier[Pre]],
            Seq(_),
          ) = fieldDecl
          val t =
            specs.collectFirst { case t: CSpecificationType[Pre] =>
              t.t match {
                case TUnique(inner, _) => inner
                case inner => inner
              }
            }.get
          t +: getFirstTypes(t)
        }.getOrElse(Nil)
      case TArray(element) => element +: getFirstTypes(element)
      case LLVMTStruct(_, _, elements) =>
        elements.headOption.map { field => field +: getFirstTypes(field) }
          .getOrElse(Nil)
      case LLVMTStruct(_, _, elements) =>
        elements.head +: getFirstTypes(elements.head)
      case LLVMTArray(_, elementType) =>
        elementType +: getFirstTypes(elementType)
      case _ => Nil
    }

  def rewriteStruct(decl: CGlobalDeclaration[Pre]): Unit = {
    val (decls, sdecl) =
      decl.decl match {
        case CDeclaration(
              _,
              _,
              Seq(sdecl @ CStructDeclaration(Some(_), decls)),
              Seq(),
            ) =>
          (decls, sdecl)
        case _ => throw WrongStructType(decl)
      }

    val casts =
      sizeOf(CTStruct(decl.ref), decl.o) +: getFirstTypes(CTStruct(decl.ref))
        .map { t => (sizeOf(t, decl.o)) }
    val newStruct =
      new ByValueClass[Post](
        Seq(),
        rw.classDeclarations.collect {
          decls.foreach { fieldDecl =>
            val CStructMemberDeclarator(
              specs: Seq[CDeclarationSpecifier[Pre]],
              Seq(x),
            ) = fieldDecl
            fieldDecl.drop()
            val (t, unique) =
              specs.collectFirst { case t: CSpecificationType[Pre] =>
                t.t match {
                  case TUnique(inner, unique) =>
                    (rw.dispatch(inner), Some(unique))
                  case inner => (rw.dispatch(inner), None)
                }
              }.get
            cStructFieldsSuccessor((decl, fieldDecl)) =
              new InstanceField(
                t = t,
                flags =
                  if (unique.isDefined)
                    Seq(Unique[Post](unique.get)(CStructFieldOrigin(x)))
                  else
                    Nil,
              )(CStructFieldOrigin(x))
            rw.classDeclarations
              .declare(cStructFieldsSuccessor((decl, fieldDecl)))
          }
        }._1,
        false,
        casts,
      )(CStructOrigin(sdecl))

    rw.globalDeclarations.declare(newStruct)
    cStructSuccessor(decl) = newStruct
  }

  // TODO: This seems to make a heap variable for every typedef which should not be needed
  def rewriteGlobalDecl(decl: CGlobalDeclaration[Pre]): Unit = {
    val isStruct =
      decl.decl.specs.collectFirst { case t: CStructDeclaration[Pre] => () }
        .isDefined
    if (isStruct) { rewriteStruct(decl); return }
    val pure = decl.decl.specs.collectFirst { case CPure() => () }.isDefined
    val inline = decl.decl.specs.collectFirst { case CInline() => () }.isDefined
    val opaque = decl.decl.specs.collectFirst { case COpaque() => () }.isDefined

    val t =
      decl.decl.specs.collectFirst { case t: CSpecificationType[Pre] =>
        rw.dispatch(t.t)
      }.get
    for ((init, idx) <- decl.decl.inits.zipWithIndex if init.ref.isEmpty) {
      // If the reference is empty , skip the declaration: the definition is used instead.
      val info = C.getDeclaratorInfo(init.decl)
      info.params match {
        case Some(params) =>
          cFunctionDeclSuccessor((decl, idx)) = rw.globalDeclarations.declare(
            decl.decl.specs.collectFirst { case k: CGpgpuKernelSpecifier[Pre] =>
              kernelProcedure(init.o, decl.decl.contract, info, None, k)
            }.getOrElse(
              new Procedure[Post](
                returnType = t,
                args = rw.variables.collect { params.foreach(rw.dispatch) }._1,
                outArgs = Nil,
                typeArgs = Nil,
                body = None,
                contract = rw.dispatch(decl.decl.contract),
                pure = pure,
                inline = inline,
                opaque = opaque,
              )(AbstractApplicable)(init.o.sourceName(info.name))
            )
          )
        case None =>
          val newT =
            if (isConst(t)) { t }
            else { TNonNullPointer(t, getUnique(t)) }
          cGlobalNameSuccessor(RefCGlobalDeclaration(decl, idx)) = rw
            .globalDeclarations
            .declare(new HeapVariable(newT, init.init.map(rw.dispatch))(
              init.o.sourceName(info.name)
            ))
      }
    }
  }

  def assignliteralArray(
      array: Variable[Post],
      exprs: Seq[Expr[Pre]],
      origin: Origin,
  ): Seq[Statement[Post]] = {
    implicit val o: Origin = origin
    (exprs.zipWithIndex.map { case (value, index) =>
      // Since we model const arrays with sequences internally, we cannot assign to them, so we assume their values
      Assume[Post](
        AmbiguousSubscript(array.get, c_const(index))(PanicBlame(
          "The explicit initialization of an array in C should never exceed the bounds of the array"
        )) === rw.dispatch(value)
      )
    })
  }

  def rewriteVectorDeclaration(
      decl: CLocalDeclaration[Pre],
      t: CTVector[Pre],
      init: CInit[Pre],
  ): Statement[Post] = {
    // LangTypesToCol makes it so that each declaration only has one init
    val info = C.getDeclaratorInfo(init.decl)
    implicit val o: Origin = init.o
    val v = new Variable[Post](rw.dispatch(t))(init.o.sourceName(info.name))
    cNameSuccessor(RefCLocalDeclaration(decl, 0)) = v
    val size = t.intSize
    init.init.get match {
      case CLiteralArray(exprs) if exprs.size == size =>
        Block(Seq(
          LocalDecl(v),
          assignInitial(
            v.get,
            LiteralVector[Post](
              rw.dispatch(t.innerType),
              exprs.map(rw.dispatch),
            ),
          ),
        ))
      case _ => throw WrongCType(decl)
    }
  }

  // TODO: (AS) Fixed-size arrays seem to become pointers but they're actually value types
  def rewriteArrayDeclaration(
      decl: CLocalDeclaration[Pre],
      cta: CTArray[Pre],
      init: CInit[Pre],
  ): Statement[Post] = {
    // LangTypesToCol makes it so that each declaration only has one init
    val init = decl.decl.inits.head
    val info = C.getDeclaratorInfo(init.decl)
    implicit val o: Origin = init.o

    decl.decl.specs match {
      case Seq(CSpecificationType(cta @ CTArray(sizeOption, oldT))) =>
        val t = rw.dispatch(oldT)
        val v = new Variable[Post](TPointer(t, None))(o.sourceName(info.name))
        cNameSuccessor(RefCLocalDeclaration(decl, 0)) = v

        (sizeOption, init.init) match {
          case (None, None) => throw WrongCType(decl)
          case (Some(size), None) =>
            val newArr =
              NewNonNullPointerArray[Post](t, rw.dispatch(size), None)(
                cta.blame
              )
            Block(Seq(LocalDecl(v), assignLocal(v.get, newArr)))
          case (None, Some(CLiteralArray(exprs))) =>
            val newArr =
              NewNonNullPointerArray[Post](t, c_const[Post](exprs.size), None)(
                cta.blame
              )
            Block(
              Seq(LocalDecl(v), assignLocal(v.get, newArr)) ++
                assignliteralArray(v, exprs, o)
            )
          case (Some(size), Some(CLiteralArray(exprs))) =>
            val realSize = isConstantInt(size).filter(_ >= 0)
              .getOrElse(throw WrongCType(decl))
            if (realSize < exprs.size)
              logger.warn(s"Excess elements in array initializer: '${decl}'")
            val newArr =
              NewNonNullPointerArray[Post](t, c_const[Post](realSize), None)(
                cta.blame
              )
            Block(
              Seq(LocalDecl(v), assignLocal(v.get, newArr)) ++
                assignliteralArray(v, exprs.take(realSize.intValue), o)
            )
          case _ => throw WrongCType(decl)
        }
      case _ => throw WrongCType(decl)
    }
  }

  def rewriteStructDeclaration(
      decl: CLocalDeclaration[Pre],
      structT: Type[Pre],
  ): Statement[Post] = {
    val init = decl.decl.inits.head
    val info = C.getDeclaratorInfo(init.decl)
    val t = structType(getStructType(structT))

    implicit val o: Origin = init.o
    val v =
      new LocalHeapVariable[Post](TNonNullPointer(t, None))(o.sourceName(
        info.name
      ))
    cLocalHeapNameSuccessor(RefCLocalDeclaration(decl, 0)) = v

    if (init.init.isDefined) {
      Block(Seq(
        HeapLocalDecl(v),
        Assign(
          v.get(PanicBlame(
            "Dereferencing freshly declared struct should never fail"
          )),
          rw.dispatch(init.init.get),
        )(AssignLocalOk),
      ))
    } else { HeapLocalDecl(v) }
  }

  def rewriteLocal(decl: CLocalDeclaration[Pre]): Statement[Post] = {
    decl.drop()
    val prop = new TypeProperties(decl.decl.specs, decl)
    if (!prop.validCLocal)
      throw WrongCType(decl)
    // LangTypesToCol makes it so that each declaration only has one init
    val init = decl.decl.inits.head

    val t =
      prop.mainType.get match {
        case t: CTVector[Pre] if init.init.collect({
              case _: CLiteralArray[Pre] => true
            }).isDefined =>
          return rewriteVectorDeclaration(decl, t, init)
        case t: CTArray[Pre] => return rewriteArrayDeclaration(decl, t, init)
        case t: CTStruct[Pre] => return rewriteStructDeclaration(decl, t)
        case t: CTStructUnique[Pre] => return rewriteStructDeclaration(decl, t)
        case t => rw.dispatch(t)
      }

    val info = C.getDeclaratorInfo(init.decl)
    val v = new Variable[Post](t)(init.o.sourceName(info.name))
    cNameSuccessor(RefCLocalDeclaration(decl, 0)) = v
    implicit val o: Origin = init.o
    init.init.map(value =>
      Block(Seq(LocalDecl(v), assignInitial(v.get, rw.dispatch(value))))
    ).getOrElse(LocalDecl(v))
  }

  def rewriteGoto(goto: CGoto[Pre]): Statement[Post] =
    Goto[Post](rw.succ(goto.ref.getOrElse(???)))(goto.o)

  def gpuBarrier(barrier: GpgpuBarrier[Pre]): Statement[Post] = {
    implicit val o: Origin = barrier.o

    var globalFence = false
    var localFence = false

    barrier.specifiers.foreach {
      case GpuLocalMemoryFence() => localFence = true
      case GpuGlobalMemoryFence() => globalFence = true
      case GpuZeroMemoryFence(i) =>
        if (i != 0)
          throw WrongBarrierSpecifier(barrier)
    }
    if (!globalFence || !localFence) {
      val redist = permissionScanner(barrier)
      if (!globalFence)
        redist.intersect(globalMemNames.toSet)
          .foreach(v => throw RedistributingBarrier(v, barrier, global = true))
      if (!localFence)
        redist
          .intersect(dynamicSharedMemNames.union(dynamicSharedMemNames).toSet)
          .foreach(v => throw RedistributingBarrier(v, barrier, global = false))
    }

    ParBarrier[Post](
      block = cudaCurrentBlock.top.ref,
      invs = Nil,
      requires = rw.dispatch(barrier.requires),
      ensures = rw.dispatch(barrier.ensures),
      content = Block(Nil),
    )(KernelBarrierFailure(barrier))
  }

  def getInnerPointerInfo(
      t: Type[Pre]
  ): Option[(Type[Pre], Option[Expr[Pre]], Option[Blame[ArraySizeError]])] =
    getBaseType(t) match {
      case TPointer(it, _) => Some((it, None, None))
      case CTPointer(it) => Some((it, None, None))
      case a @ CTArray(size, it) => Some((it, size, Some(a.blame)))
      case _ => None
    }

  def isVector(t: Type[Pre]): Boolean =
    getBaseType(t) match {
      case _: CTVector[Pre] => true
      case _ => false
    }

  def isArray(t: Type[Pre]): Boolean =
    getBaseType(t) match {
      case _: CTArray[Pre] => true
      case _ => false
    }

  def isStruct(t: Type[Pre]): Boolean = getStruct(t).isDefined

  def getStruct(t: Type[Pre]): Option[CTStruct[Pre]] =
    getBaseType(t) match {
      case t: CTStruct[Pre] => Some(t)
      case _ => None
    }

  def isPointer(t: Type[Pre]): Boolean =
    getBaseType(t) match {
      case TPointer(_, _) => true
      case CTPointer(_) => true
      case _ => false
    }

  def isNumeric(t: Type[Pre]): Boolean =
    getBaseType(t) match {
      case _: NumericType[Pre] => true
      case _ => false
    }

  def searchNames(e: Expr[Pre], original: Node[Pre]): Seq[CNameTarget[Pre]] =
    e match {
      case arr: CLocal[Pre] => Seq(arr.ref.get)
      case PointerAdd(arr: CLocal[Pre], _) => Seq(arr.ref.get)
      case AmbiguousSubscript(arr: CLocal[Pre], _) => Seq(arr.ref.get)
      case AmbiguousPlus(l, r) if isPointer(l.t) && isNumeric(r.t) =>
        searchNames(l, original)
      case AddrOf(inner) => searchNames(inner, original)
      case _ => throw UnsupportedBarrierPermission(original)
    }

  def searchNames(
      loc: Location[Pre],
      original: Node[Pre],
  ): Seq[CNameTarget[Pre]] =
    loc match {
      case ArrayLocation(arr: CLocal[Pre], _) => Seq(arr.ref.get)
      case PointerLocation(arr: CLocal[Pre]) => Seq(arr.ref.get)
      case PointerLocation(PointerAdd(arr: CLocal[Pre], _)) => Seq(arr.ref.get)
      case AmbiguousLocation(expr) => searchNames(expr, original)
      case _ => throw UnsupportedBarrierPermission(original)
    }

  def searchPermission(e: Node[Pre]): Seq[CNameTarget[Pre]] = {
    e match {
      case e: Expr[Pre] if e.t != TResource[Pre]() => Seq()
      case Perm(loc, _) => searchNames(loc, e)
      case PointsTo(loc, _, _) => searchNames(loc, e)
      case CurPerm(loc) => searchNames(loc, e)
      case PermPointer(pointer, _, _) => searchNames(pointer, e)
      case PermPointerIndex(pointer, _, _) => searchNames(pointer, e)
      case _ => e.subnodes.flatMap(searchPermission)
    }
  }

  def permissionScanner(barrier: GpgpuBarrier[Pre]): Set[CNameTarget[Pre]] = {
    val pres = unfoldStar(barrier.requires).toSet
    val posts = unfoldStar(barrier.ensures).toSet
    val context = pres intersect posts
    // Only scan the non context permissions
    val nonContext = (pres union posts) diff context
    nonContext.flatMap(searchPermission)
  }

  def result(ref: RefCFunctionDefinition[Pre])(implicit o: Origin): Expr[Post] =
    Result[Post](cFunctionSuccessor.ref(ref.decl))

  def result(
      ref: RefCGlobalDeclaration[Pre]
  )(implicit o: Origin): Expr[Post] = {
    val maybeDefinition = ref.decls.decl.inits(ref.initIdx).ref
    maybeDefinition match {
      case Some(defn) => Result[Post](cFunctionSuccessor.ref(defn.decl))
      case None =>
        Result[Post](cFunctionDeclSuccessor.ref((ref.decls, ref.initIdx)))
    }
  }

  def local(local: CLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    local.ref.get match {
      case spec: SpecNameTarget[Pre] => rw.specLocal(spec, local, local.blame)
      case _: SpecInvocationTarget[Pre] => throw NotAValue(local)
      case ref: RefCParam[Pre] =>
        if (cCurrentDefinitionParamSubstitutions.nonEmpty)
          Local(cNameSuccessor.ref(RefCParam(
            cCurrentDefinitionParamSubstitutions.top
              .getOrElse(ref.decl, ref.decl)
          )))
        else
          Local(cNameSuccessor.ref(ref))
      case RefCFunctionDefinition(func) =>
        functionInvocation(
          TrueSatisfiable,
          functionPointers.getOrElseUpdate(
            func, {
              rw.globalDeclarations.declare(
                function[Post](
                  AbstractApplicable,
                  TrueSatisfiable,
                  TPointer(TVoid(), None),
                )(func.o)
              )
            },
          ).ref,
        )
      case ref: RefCStruct[Pre] => throw NotAValue(local)
      case ref @ RefCGlobalDeclaration(decl, initIdx) =>
        C.getDeclaratorInfo(decl.decl.inits(initIdx).decl).params match {
          case None =>
            val t =
              decl.decl.specs.collectFirst { case t: CSpecificationType[Pre] =>
                t.t
              }.get
            val derefHeap =
              DerefHeapVariable[Post](cGlobalNameSuccessor.ref(ref))(
                local.blame
              )
            if (isConst(t)) { derefHeap }
            else { DerefPointer(derefHeap)(local.blame) }
          case Some(_) =>
            // Is this ever possible? I.e. would it not be a RefCFunctionDefinition?
            throw NotAValue(local)
        }
      case ref: RefCLocalDeclaration[Pre]
          if cLocalHeapNameSuccessor.contains(ref) =>
        DerefPointer(HeapLocal[Post](cLocalHeapNameSuccessor.ref(ref)))(
          local.blame
        )
      case ref: RefCLocalDeclaration[Pre] => Local(cNameSuccessor.ref(ref))
      case _: RefCudaVec[Pre] => throw NotAValue(local)
    }
  }

  def deref(deref: CFieldAccess[Pre]): Expr[Post] = {
    implicit val o: Origin = deref.o

    def getCuda(dim: RefCudaVecDim[Pre]): Expr[Post] =
      dim.vec match {
        case _ if cudaConstantDims.top.indices.contains(dim) =>
          cudaConstantDims.top.indices(dim).get
        case RefCudaThreadIdx() => cudaCurrentThreadIdx.top.indices(dim).get
        case RefCudaBlockIdx() => cudaCurrentBlockIdx.top.indices(dim).get
        case RefCudaBlockDim() => cudaCurrentBlockDim.top.indices(dim).get
        case RefCudaGridDim() => cudaCurrentGridDim.top.indices(dim).get
      }

    def getVector(i: BigInt): Expr[Post] =
      AmbiguousSubscript(rw.dispatch(deref.obj), const(i))(PanicBlame(
        "Should be okay"
      ))

    deref.ref.get match {
      case spec: SpecDerefTarget[Pre] =>
        rw.specDeref(deref.obj, spec, deref, deref.blame)
      case target: SpecInvocationTarget[Pre] => ???
      case dim: RefCudaVecDim[Pre] => getCuda(dim)
      case mem: RefOpenCLVectorMembers[Pre] =>
        if (mem.idx.size == 1)
          getVector(mem.idx.head)
        else {
          val elementType =
            deref.obj.t match {
              case v: TOpenCLVector[Pre] => v.innerType
              case _ => ???
            }
          LiteralVector[Post](rw.dispatch(elementType), mem.idx.map(getVector))
        }
      case struct: RefCStruct[Pre] => ???
      case struct: RefCStructField[Pre] =>
        val struct_ref =
          getBaseType(deref.obj.t) match {
            case CTStruct(struct_ref) => struct_ref
            case _: TNotAValue[Pre] => throw TypeUsedAsValue(deref.obj)
            case _ => ???
          }
        Deref[Post](
          rw.dispatch(deref.obj),
          cStructFieldsSuccessor.ref((struct_ref.decl, struct.decls)),
        )(deref.blame)
    }
  }

  def deref(deref: CStructDeref[Pre]): Expr[Post] = {
    implicit val o: Origin = deref.o

    deref.ref.get match {
      case RefModelField(decl) =>
        ModelDeref[Post](rw.currentThis.top, rw.succ(decl))(deref.blame)
      case BuiltinField(f) => rw.dispatch(f(deref.struct))
      case target: SpecInvocationTarget[Pre] => ???
      case struct: RefCStruct[Pre] => ???
      case struct: RefCStructField[Pre] =>
        val b: Blame[PointerDerefError] = deref.blame
        val structRef =
          getBaseType(deref.struct.t) match {
            case CTPointer(struct) =>
              getBaseStructTypeWithUnique(struct).map({ case (t, _) => t.ref })
                .getOrElse(throw WrongStructType(deref.struct.t))
            case CTArray(_, struct) =>
              getBaseStructTypeWithUnique(struct).map({ case (t, _) => t.ref })
                .getOrElse(throw WrongStructType(deref.struct.t))
            case t => throw WrongStructType(t)
          }
        Deref[Post](
          DerefPointer(rw.dispatch(deref.struct))(b),
          cStructFieldsSuccessor.ref((structRef.decl, struct.decls)),
        )(deref.blame)(deref.o)
    }
  }

  private def createUpdateVectorFunction(size: Int): Function[Post] = {
    implicit val o: Origin = Origin(Seq(LabelContext("vector update method")))
    /* for instance for size 4:
    requires i >= 0 && i < 4;
    pure vector<T,4> updateVector(vector<T,4> x, int i, T v) =
        (i == 0) ? vector<T>{v, x[1], x[2], x[3]} :
        (i == 1) ? vector<T>{x[0], v, x[2], x[3]} :
        (i == 2) ? vector<T>{x[0], x[1], v, x[3]} :
        vector<T>{x[0], x[1], x[2], v};
     */
    if (size <= 0)
      ???

    rw.globalDeclarations.declare({
      val elementTypeVar =
        new Variable[Post](TType(TAnyValue()))(o.where(name = "T"))
      val elementType = TVar[Post](elementTypeVar.ref)
      val vectorT = TVector(size, elementType)

      val x = new Variable[Post](vectorT)(o.where(name = "x"))
      val i = new Variable[Post](TCInt())(o.where(name = "i"))
      val v = new Variable[Post](elementType)(o.where(name = "v"))
      val req = Seq(c_const[Post](0) <= i.get, i.get < c_const[Post](size))

      val vals: Seq[Expr[Post]] = Seq.range(0, size).map(j =>
        AmbiguousSubscript(x.get, c_const[Post](j))(PanicBlame("Checked"))
      )
      def f: Int => Expr[Post] =
        j => LiteralVector(elementType, vals.updated(j, v.get))
      val body =
        Seq.range(0, size - 1)
          .foldRight(f(size - 1))((j: Int, res: Expr[Post]) =>
            Select(i.get === c_const[Post](j), f(j), res)
          )

      function(
        blame = AbstractApplicable,
        contractBlame = PanicBlame("Can be satisfiable"),
        returnType = vectorT,
        args = Seq(x, i, v),
        typeArgs = Seq(elementTypeVar),
        body = Some(body),
        requires = SplitAccountedPredicate(
          UnitAccountedPredicate(req(0)),
          UnitAccountedPredicate(req(1)),
        ),
      )
    })
  }

  val updateVectorFunction: mutable.Map[Int, Function[Post]] = mutable.Map()

  def assignSubscriptVector(assign: PreAssignExpression[Pre]): Expr[Post] = {

    val sub @ AmbiguousSubscript(v, i) = assign.target
    val t =
      v.t match {
        case t: CTVector[Pre] => t
        case _ => ???
      }

    val size = t.intSize.toInt
    val index = rw.dispatch(i)
    val innerType = t.innerType

    val newV = rw.dispatch(v)
    val f = updateVectorFunction
      .getOrElseUpdate(size, createUpdateVectorFunction(size))
    val update =
      functionInvocation[Post](
        VectorBoundFailed(sub),
        f.ref,
        Seq(newV, index, rw.dispatch(assign.value)),
        Seq(rw.dispatch(innerType)),
      )(assign.o)

    PreAssignExpression(newV, update)(assign.blame)(assign.o)
  }

  // If we have vector<int, 4> v; v.xwy = vector<int>{0, 1, 2}
  // We want to get v = vector<int>{0, 2, v[2], 1};
  def assignOpenCLVector(assign: PreAssignExpression[Pre]): Expr[Post] = {
    val CFieldAccess(obj, idxs) = assign.target
    val t =
      obj.t match {
        case t: TOpenCLVector[Pre] => t
        case _ => ???
      }

    val intIdxs = C.openCLVectorAccessString(idxs, t.size)
      .getOrElse(throw WrongOpenCLLiteralVector(assign))

    val lhs_vec = rw.dispatch(obj)
    val rhs = rw.dispatch(assign.value)
    val innerType = rw.dispatch(t.innerType)

    if (intIdxs.length == 1) {
      // Only one index needs to change
      val vals: Seq[Expr[Post]] =
        Seq.tabulate(t.size.toInt)((i: Int) => {
          val c = intIdxs.count(j => j == i)
          if (c == 0)
            AmbiguousSubscript(lhs_vec, c_const[Post](i)(assign.target.o))(
              PanicBlame("Type checked")
            )(assign.o)
          else
            rhs
        })

      return PreAssignExpression(
        lhs_vec,
        LiteralVector(innerType, vals)(assign.o),
      )(assign.blame)(assign.o)
    }

    // Store the rhs in a variable, it might be a method call that alters the state
    val vectorT = TVector[Post](intIdxs.length, innerType)
    val rhs_val = new Variable[Post](vectorT)(assign.value.o)
    val before: Statement[Post] =
      Block[Post](Seq(
        LocalDecl(rhs_val)(rhs.o),
        assignInitial(rhs_val.get(rhs.o), rhs)(rhs.o),
      ))(assign.o)

    // Assign the correct values
    val vals: Seq[Expr[Post]] =
      Seq.tabulate(t.size.toInt)((i: Int) => {
        val c = intIdxs.count(j => j == i)
        if (c > 1)
          throw WrongOpenCLLiteralVector(assign)
        else if (c == 0)
          AmbiguousSubscript(lhs_vec, c_const[Post](i)(assign.target.o))(
            PanicBlame("Type checked")
          )(assign.o)
        else {
          val j = intIdxs.indexOf(i)
          AmbiguousSubscript(
            rhs_val.get(rhs.o),
            c_const[Post](j)(assign.target.o),
          )(PanicBlame("Type checked"))(assign.o)
        }
      })

    With(
      before,
      PreAssignExpression(lhs_vec, LiteralVector(innerType, vals)(assign.o))(
        assign.blame
      )(assign.o),
    )(assign.o)
  }

  private def isCPointer(t: Type[_]) =
    getBaseType(t) match {
      case CTPointer(_) => true
      case _ => false
    }

  private def indexVectors(
      e: Expr[Post],
      askedType: Type[Post],
      cast: CCast[Pre],
  ): Seq[Expr[Post]] =
    e.t match {
      case t if t == askedType => Seq(e)
      case t: TVector[Post] =>
        Seq.tabulate(t.size.toInt)(i => i)
          .flatMap(i =>
            indexVectors(
              AmbiguousSubscript(e, c_const(i)(e.o))(PanicBlame(
                "Type Checked"
              ))(e.o),
              askedType,
              cast,
            )
          )
      case _ => throw WrongOpenCLLiteralVector(cast)
    }

  private def unwrapVectorArg(
      e: Expr[Post],
      askedType: Type[Post],
      cast: CCast[Pre],
  ): (Seq[Statement[Post]], Seq[Expr[Post]]) =
    e.t match {
      case t if t == askedType => (Nil, Seq(e))
      case _: TVector[Post] =>
        implicit val o: Origin = e.o
        val (befores, varE) =
          e match {
            case v: Local[Post] => (Nil, v)
            case _ =>
              val v = new Variable[Post](e.t)
              val befores: Seq[Statement[Post]] = Seq(
                LocalDecl(v),
                assignInitial(v.get, e),
              )
              (befores, v.get)
          }
        (befores, indexVectors(varE, askedType, cast))
      case _ => throw WrongOpenCLLiteralVector(cast)
    }

  private def unwrapWith(e: Expr[Pre]): Seq[Expr[Pre]] =
    e match {
      // This will break if someone uses With inside a vector literal
      case With(Eval(before), after) => unwrapWith(before) :+ after
      case e => Seq(e)
    }

  private def createOpenCLLiteralVector(cast: CCast[Pre]): Expr[Post] = {
    implicit val o: Origin = cast.o
    cast match {
      case CCast(expr, TOpenCLVector(size, innerType)) =>
        val (befores: Seq[Statement[Post]], newArgs: Seq[Expr[Post]]) =
          unwrapWith(expr).map(a =>
            unwrapVectorArg(rw.dispatch(a), rw.dispatch(innerType), cast)
          ).foldLeft[(Seq[Statement[Post]], Seq[Expr[Post]])](Nil, Nil)(
            (base, extra) => (base._1 ++ extra._1, base._2 ++ extra._2)
          )
        if (newArgs.length != size)
          throw WrongOpenCLLiteralVector(cast)
        val result = LiteralVector[Post](rw.dispatch(innerType), newArgs)
        if (befores.nonEmpty)
          With(Block(befores), result)
        else { result }
      case other => ???
    }
  }

  def invocation(inv: CInvocation[Pre]): Expr[Post] = {
    val CInvocation(applicable, args, givenMap, yields, reveal) = inv

    val newArgs = args.map(a => rw.dispatch(a))

    implicit val o: Origin = inv.o
    inv.ref.get match {
      case spec: SpecInvocationTarget[Pre] =>
        rw.specInvocation(
          None,
          spec,
          Nil,
          args,
          givenMap,
          yields,
          reveal,
          inv,
          inv.blame,
        )
      case ref: RefCFunctionDefinition[Pre] =>
        ProcedureInvocation[Post](
          cFunctionSuccessor.ref(ref.decl),
          newArgs,
          Nil,
          Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
          reveal = reveal,
        )(inv.blame)
      case e: RefCGlobalDeclaration[Pre] => globalInvocation(e, inv, newArgs)
    }
  }

  /** Rewrites a CudaKernelInvocation to a procedure.
    * @param kernel
    *   \- the invocation we want to rewrite
    * @return
    *   a Procedure[Post]
    */
  def cudaKernelInvocation(
      kernel: GpgpuCudaKernelInvocation[Pre]
  ): Expr[Post] = {
    val GpgpuCudaKernelInvocation(
      ker,
      blocks,
      threads,
      args,
      givenMap,
      yields,
    ) = kernel
    implicit val o: Origin = kernel.o
    val one = c_const[Post](1)
    kernel.ref.get match {
      case target: SpecInvocationTarget[_] => ???
      case ref: RefCFunctionDefinition[Pre] =>
        ProcedureInvocation[Post](
          cFunctionSuccessor.ref(ref.decl),
          rw.dispatch(blocks) +: one +: one +: rw.dispatch(threads) +: one +:
            one +: args.map(rw.dispatch),
          Nil,
          Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
        )(kernel.blame)
      case e: RefCGlobalDeclaration[Pre] => ???
    }
  }

  /** Returns the local id of a thread in a given dimension.
    * @param index
    *   \- the dimension for which we want the thread id
    * @param origin
    *   \- the origin of the expr node
    * @return
    *   the value of the thread ID in the given dimension
    */
  def getCudaLocalThread(index: Int, origin: Origin): Expr[Post] = {
    implicit val o: Origin = origin
    val idxs = Seq(
      RefCudaVecX[Pre](RefCudaThreadIdx()),
      RefCudaVecY[Pre](RefCudaThreadIdx()),
      RefCudaVecZ[Pre](RefCudaThreadIdx()),
    )
    val v = idxs(index)
    cudaConstantDims.top.indices
      .getOrElse(v, cudaCurrentThreadIdx.top.indices(v)).get
  }

  /** Returns the group id of a thread in a given dimension.
    * @param index
    *   \- the dimension for which we want the thread id
    * @param origin
    *   \- the origin of the expr node
    * @return
    *   the value of the thread ID in the given dimension
    */
  def getCudaGroupThread(index: Int, origin: Origin): Expr[Post] = {
    implicit val o: Origin = origin
    val idxs = Seq(
      RefCudaVecX[Pre](RefCudaBlockIdx()),
      RefCudaVecY[Pre](RefCudaBlockIdx()),
      RefCudaVecZ[Pre](RefCudaBlockIdx()),
    )
    val v = idxs(index)
    cudaConstantDims.top.indices
      .getOrElse(v, cudaCurrentBlockIdx.top.indices(v)).get
  }

  /** Returns the local size of a given dimension.
    * @param index
    *   \- the dimension for which we query its size
    * @param origin
    *   \- the origin of the expr node
    * @return
    *   the value of the size of the given dimension.
    */
  def getCudaLocalSize(index: Int, origin: Origin): Expr[Post] = {
    implicit val o: Origin = origin
    cudaCurrentBlockDim.top.indices.values.toSeq.apply(index).get
  }

  /** Returns the global size of a given dimension.
    * @param index
    *   \- the dimension for which we query the global size.
    * @param o
    *   \- the origin of the expr node
    * @return
    *   the value of the global size of the given dimension.
    */
  def getCudaGroupSize(index: Int, o: Origin): Expr[Post] = {
    implicit val origin: Origin = o
    cudaCurrentGridDim.top.indices.values.toSeq.apply(index).get
  }

  /** Rewrites a LocalThreadId and translates it to a linear ID value. The
    * expression is equivalent to the openCL method "get_local_linear_id".
    * @param local
    *   \- the node we want to rewrite
    * @return
    *   an expression equal to the linear ID of the local thread
    */
  def cudaLocalThreadId(local: LocalThreadId[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    Plus(
      Mult(
        Mult(
          getCudaLocalThread(2, o),
          getCudaLocalSize(1, o),
        ), // get_local_id(2) * get_local_size(1)
        getCudaLocalSize(0, o), //  * get_local_size(0)
      ), //  +
      Plus(
        Mult(
          getCudaLocalThread(1, o),
          getCudaLocalSize(0, o),
        ), //  get_local_id(1) * get_local_size(0)
        getCudaLocalThread(0, o), //  + get_local_id(0)
      ),
    )
  }

  /** Rewrites a GlobalThreadId into a linear ID value. The expression is
    * equivalent to the openCL method "get_global_linear_id". We assume the
    * global_offset is 0 in our calculation.
    * @param global
    *   \- the node we want to rewrite
    * @return
    *   an expression equal to the linear global id of the thread.
    */
  def cudaGlobalThreadId(global: GlobalThreadId[Pre]): Expr[Post] = {
    implicit val o: Origin = global.o
    val res = (getGlobalId(2) * getGlobalSize(1) * getGlobalSize(0)) +
      getGlobalId(1) * getGlobalSize(0) + getGlobalId(0)
    return res;
  }

  def getGlobalId(idx: Int)(implicit o: Origin): Expr[Post] = {
    getCudaGroupThread(idx, o) * getCudaLocalSize(idx, o) +
      getCudaLocalThread(idx, o)
  }

  def getGlobalSize(idx: Int)(implicit o: Origin): Expr[Post] = {
    getCudaLocalSize(idx, o) * getCudaGroupSize(idx, o)
  }

  def globalInvocation(
      e: RefCGlobalDeclaration[Pre],
      inv: CInvocation[Pre],
      rewrittenArgs: Seq[Expr[Post]],
  ): Expr[Post] = {
    val CInvocation(_, args, givenMap, yields, reveal) = inv
    val RefCGlobalDeclaration(decls, initIdx) = e
    implicit val o: Origin = inv.o

    (e.name, args, givenMap, yields) match {
      case (_, _, g, y) if g.nonEmpty || y.nonEmpty =>
      case ("__vercors_free", Seq(xs), _, _) if isPointer(xs.t) =>
        return FreePointer[Post](rw.dispatch(xs))(inv.blame)(inv.o)
      case _ => ()
    }

    val arg =
      if (args.size == 1) {
        args.head match {
          case CIntegerValue(i, _) if i >= 0 && i < 3 => Some(i.toInt)
          case _ => None
        }
      } else
        None

    (e.name, arg) match {
      case ("get_local_id", Some(i)) => getCudaLocalThread(i, o)
      case ("get_group_id", Some(i)) => getCudaGroupThread(i, o)
      case ("get_local_size", Some(i)) => getCudaLocalSize(i, o)
      case ("get_global_size", Some(i)) =>
        getCudaGroupSize(i, o) * getCudaLocalSize(i, o)
      case ("get_num_groups", Some(i)) => getCudaGroupSize(i, o)
      case ("get_global_id", Some(i)) =>
        getCudaLocalSize(i, o) * getCudaGroupThread(i, o) +
          getCudaLocalThread(i, o)
      case _ =>
        ProcedureInvocation[Post](
          cFunctionDeclSuccessor.ref((decls, initIdx)),
          rewrittenArgs,
          Nil,
          Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
          reveal = reveal,
        )(inv.blame)
    }
  }

  def pointerType(t: CPointerType[Pre]): Type[Post] =
    t match {
      case CTPointer(innerType) => TPointer(rw.dispatch(innerType), None)
      case CTArray(_, innerType) => TPointer(rw.dispatch(innerType), None)
    }

  def vectorType(t: CType[Pre]): Type[Post] = {
    val (intSize, innerType) =
      t match {
        case t: CTVector[Pre] => (t.intSize, t.innerType)
        case t: TOpenCLVector[Pre] => (t.size, t.innerType)
        case _ => throw WrongVectorType(t)
      }
    rw.dispatch(innerType) match {
      case innerType @ (TCInt() | TCFloat(_, _)) =>
        TVector(intSize, innerType)(t.o)
      case _ => throw WrongVectorType(t)
    }
  }

  def arrayType(t: CTArray[Pre]): Type[Post] = {
    // The size of an array for an parameter is ignored
    TPointer(rw.dispatch(t.innerType), None)
  }

  def structType(t: CType[Pre]): TClass[Post] =
    t match {
      case ts @ CTStruct(ref) =>
        val targetClass =
          new LazyRef[Post, Class[Post]](cStructSuccessor(ref.decl))
        TByValueClass[Post](targetClass, Seq())(t.o)
      case t @ CTStructUnique(_, _, _) =>
        val (CTStruct(ref), map) = getBaseStructTypeWithUnique(t).get
        val targetClass =
          new LazyRef[Post, Class[Post]](cStructSuccessor(ref.decl))
        val uniqueMap = map.toSeq.map { case (fieldRef, unique) =>
          (
            new LazyRef[Post, InstanceField[Post]](cStructFieldsSuccessor(
              (ref.decl, fieldRef.decl)
            )),
            unique,
          )
        }
        TClassUnique[Post](
          TByValueClass[Post](targetClass, Seq())(t.o),
          uniqueMap,
        )(t.o)
      case _ => ???
    }
}
