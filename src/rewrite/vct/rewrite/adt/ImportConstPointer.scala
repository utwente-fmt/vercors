package vct.col.rewrite.adt

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers.{ExprBuildHelpers, const}


case object ImportConstPointer extends ImportADTBuilder("const_pointer") {
  case class PointerNullOptNone(inner: Blame[PointerNull], expr: Expr[_])
      extends Blame[OptionNone] {
    override def blame(error: OptionNone): Unit = inner.blame(PointerNull(expr))
  }

  case class PointerBoundsPreconditionFailed(
      inner: Blame[PointerBounds],
      expr: Node[_],
  ) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(PointerBounds(expr))
  }

  case class DerefPointerBoundsPreconditionFailed(
      inner: Blame[PointerDerefError],
      expr: Expr[_],
  ) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(PointerInsufficientPermission(expr))
  }

  case class PointerFieldInsufficientPermission(
      inner: Blame[PointerInsufficientPermission],
      expr: Expr[_],
  ) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit =
      inner.blame(PointerInsufficientPermission(expr))
  }
}

case class ImportConstPointer[Pre <: Generation](importer: ImportADTImporter)
    extends ImportADT[Pre](importer) {
  import ImportConstPointer._

  private lazy val pointerFile = parse("const_pointer")

  private lazy val pointerAdt = find[AxiomaticDataType[Post]](
    pointerFile,
    "const_pointer",
  )
  private lazy val pointerOf = find[ADTFunction[Post]](pointerAdt, "const_pointer_of")
  private lazy val pointerBlock = find[ADTFunction[Post]](
    pointerAdt,
    "const_pointer_block",
  )
  private lazy val pointerOffset = find[ADTFunction[Post]](
    pointerAdt,
    "const_pointer_offset",
  )
  private lazy val pointerDeref = find[Function[Post]](pointerFile, "const_ptr_deref")
  private lazy val pointerAdd = find[Function[Post]](pointerFile, "const_ptr_add")

  def isConstPointer(e: Expr[Pre]): Boolean = e.t match {
    case TConstPointer(_) => true
    case _ => false
  }

  def getInner(t: Type[Pre]): Type[Pre] = t match {
    case TConstPointer(inner) => inner
    case _ => ???
  }

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoerceNullPointer(TConstPointer(_)) => OptNone()
      case other => super.applyCoercion(e, other)
    }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case TConstPointer(inner) => TOption(TAxiomatic(pointerAdt.ref, Seq(dispatch(inner))))
      case other => other.rewriteDefault()
    }

  override def postCoerce(location: Location[Pre]): Location[Post] =
    location match {
      case loc @ PointerLocation(pointer) if isConstPointer(pointer) =>
        ??? // Should not happen?
      case other => other.rewriteDefault()
    }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case sub @ PointerSubscript(pointer, index) if isConstPointer(pointer) =>
        val inner = dispatch(getInner(pointer.t))
        FunctionInvocation[Post](
          ref = pointerDeref.ref,
          args = Seq(
            FunctionInvocation[Post](
              ref = pointerAdd.ref,
              args = Seq(
                OptGet(dispatch(pointer))(
                  PointerNullOptNone(sub.blame, pointer)
                ),
                dispatch(index),
              ),
              typeArgs = Seq(inner),
              Nil,
              Nil,
            )(NoContext(PointerBoundsPreconditionFailed(sub.blame, index)))
          ),
          typeArgs = Seq(inner),
          Nil,
          Nil,
        )(PanicBlame("ptr_deref requires nothing."))
      case add @ PointerAdd(pointer, offset) if isConstPointer(pointer) =>
        val inner = dispatch(getInner(pointer.t))
        OptSome(
          FunctionInvocation[Post](
            ref = pointerAdd.ref,
            args = Seq(
              OptGet(dispatch(pointer))(PointerNullOptNone(add.blame, pointer)),
              dispatch(offset),
            ),
            typeArgs = Seq(inner),
            Nil,
            Nil,
          )(NoContext(PointerBoundsPreconditionFailed(add.blame, pointer)))
        )
      case deref @ DerefPointer(pointer) if isConstPointer(pointer) =>
//        val c_pointer = OptGet(dispatch(pointer))(PointerNullOptNone(sub.blame, pointer))
//        //        val blame = NoContext(PointerBoundsPreconditionFailed(sub.blame, index))
//        SeqSubscript(c_pointer, dispatch(index))(PanicBlame("TODO: pointer subscript out of bounds"))
        val inner = dispatch(getInner(pointer.t))
        FunctionInvocation[Post](
          ref = pointerDeref.ref,
          args = Seq(
            FunctionInvocation[Post](
              ref = pointerAdd.ref,
              // Always index with zero, otherwise quantifiers with pointers do not get triggered
              args = Seq(
                OptGet(dispatch(pointer))(
                  PointerNullOptNone(deref.blame, pointer)
                ),
                const(0),
              ),
              typeArgs = Seq(inner),
              Nil,
              Nil,
            )(NoContext(
              DerefPointerBoundsPreconditionFailed(deref.blame, pointer)
            ))
          ),
          typeArgs = Seq(inner),
          Nil,
          Nil,
        )(PanicBlame("ptr_deref requires nothing."))
      case len @ PointerBlockLength(pointer) if isConstPointer(pointer) =>
        val inner = dispatch(getInner(pointer.t))
        Size(ADTFunctionInvocation[Post](
            typeArgs = Some((pointerAdt.ref, Seq(inner))),
            ref = pointerBlock.ref,
            args = Seq(
              OptGet(dispatch(pointer))(PointerNullOptNone(len.blame, pointer))
            ),
          ))
      case off @ PointerBlockOffset(pointer) if isConstPointer(pointer) =>
        val inner = dispatch(getInner(pointer.t))
        ADTFunctionInvocation[Post](
          typeArgs = Some((pointerAdt.ref, Seq(inner))),
          ref = pointerOffset.ref,
          args = Seq(
            OptGet(dispatch(pointer))(PointerNullOptNone(off.blame, pointer))
          ),
        )
      case pointerLen @ PointerLength(pointer) if isConstPointer(pointer) =>
        postCoerce(
          PointerBlockLength(pointer)(pointerLen.blame) -
            PointerBlockOffset(pointer)(pointerLen.blame)
        )
      case other => other.rewriteDefault()
    }
  }
}
