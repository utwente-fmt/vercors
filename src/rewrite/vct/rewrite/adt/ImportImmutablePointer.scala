package vct.col.rewrite.adt

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.Generation
import vct.col.rewrite.adt.ImportPointer.DerefAddToSubscriptBlame
import vct.col.util.AstBuildHelpers.{ExprBuildHelpers, const}

case object ImportImmutablePointer
    extends ImportADTBuilder("immutable_pointer") {
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

case class ImportImmutablePointer[Pre <: Generation](
    importer: ImportADTImporter
) extends ImportADT[Pre](importer) {
  import ImportImmutablePointer._

  private lazy val pointerFile = parse("immutable_pointer")

  private lazy val pointerAdt = find[AxiomaticDataType[Post]](
    pointerFile,
    "immutable_pointer",
  )
  private lazy val pointerOf = find[ADTFunction[Post]](
    pointerAdt,
    "immutable_pointer_of",
  )
  private lazy val pointerBlock = find[ADTFunction[Post]](
    pointerAdt,
    "immutable_pointer_block",
  )
  private lazy val pointerOffset = find[ADTFunction[Post]](
    pointerAdt,
    "immutable_pointer_offset",
  )
  private lazy val pointerDeref = find[Function[Post]](
    pointerFile,
    "immutable_ptr_deref",
  )
  private lazy val pointerAdd = find[Function[Post]](
    pointerFile,
    "immutable_ptr_add",
  )

  def isImmutablePointer(e: Expr[Pre]): Boolean =
    e.t match {
      case TImmutablePointer(_) => true
      case TNonNullImmutablePointer(_) => true
      case _ => false
    }

  def getInner(t: Type[Pre]): Type[Pre] =
    t match {
      case TImmutablePointer(inner) => inner
      case TNonNullImmutablePointer(inner) => inner
      case _ => ???
    }

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoerceNullPointer(TImmutablePointer(_)) => OptNone()
      case CoerceNonNullPointer(TImmutablePointer(_)) => OptSome(e)
      case other => super.applyCoercion(e, other)
    }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case TImmutablePointer(inner) =>
        TOption(TAxiomatic(pointerAdt.ref, Seq(dispatch(inner))))
      case TNonNullImmutablePointer(inner) =>
        TAxiomatic(pointerAdt.ref, Seq(dispatch(inner)))
      case other => other.rewriteDefault()
    }

  def isNullable(t: Type[Pre]): Boolean =
    t match {
      case TImmutablePointer(_) => true
      case TNonNullImmutablePointer(_) => false
    }

  def getPointer(pointer: Expr[Pre], blame: Blame[OptionNone])(
      implicit o: Origin
  ): Expr[Post] = {
    val nullable = isNullable(pointer.t)
    if (nullable) { OptGet(dispatch(pointer))(blame) }
    else { dispatch(pointer) }
  }

  override def preCoerce(e: Expr[Pre]): Expr[Pre] = {
    implicit val o: Origin = e.o
    e match {
      case d @ DerefPointer(a @ PointerAdd(p, i)) =>
        PointerSubscript(p, i)(DerefAddToSubscriptBlame(d.blame, a.blame))
      case _ => super.preCoerce(e)
    }
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case sub @ PointerSubscript(pointer, index)
          if isImmutablePointer(pointer) =>
        val inner = dispatch(getInner(pointer.t))
        FunctionInvocation[Post](
          ref = pointerDeref.ref,
          args = Seq(
            FunctionInvocation[Post](
              ref = pointerAdd.ref,
              args = Seq(
                getPointer(pointer, PointerNullOptNone(sub.blame, pointer)),
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
      case add @ PointerAdd(pointer, offset) if isImmutablePointer(pointer) =>
        val inner = dispatch(getInner(pointer.t))
        val inv =
          FunctionInvocation[Post](
            ref = pointerAdd.ref,
            args = Seq(
              getPointer(pointer, PointerNullOptNone(add.blame, pointer)),
              dispatch(offset),
            ),
            typeArgs = Seq(inner),
            Nil,
            Nil,
          )(NoContext(PointerBoundsPreconditionFailed(add.blame, pointer)))
        pointer.t match {
          case TImmutablePointer(_) => OptSome(inv)
          case TNonNullImmutablePointer(_) => inv
        }
      case deref @ DerefPointer(pointer) if isImmutablePointer(pointer) =>
        val inner = dispatch(getInner(pointer.t))
        FunctionInvocation[Post](
          ref = pointerDeref.ref,
          args = Seq(
            FunctionInvocation[Post](
              ref = pointerAdd.ref,
              // Always index with zero, otherwise quantifiers with pointers do not get triggered
              args = Seq(
                getPointer(pointer, PointerNullOptNone(deref.blame, pointer)),
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
      case len @ PointerBlockLength(pointer) if isImmutablePointer(pointer) =>
        val inner = dispatch(getInner(pointer.t))
        Size(ADTFunctionInvocation[Post](
          typeArgs = Some((pointerAdt.ref, Seq(inner))),
          ref = pointerBlock.ref,
          args = Seq(
            getPointer(pointer, PointerNullOptNone(len.blame, pointer))
          ),
        ))
      case off @ PointerBlockOffset(pointer) if isImmutablePointer(pointer) =>
        val inner = dispatch(getInner(pointer.t))
        ADTFunctionInvocation[Post](
          typeArgs = Some((pointerAdt.ref, Seq(inner))),
          ref = pointerOffset.ref,
          args = Seq(
            getPointer(pointer, PointerNullOptNone(off.blame, pointer))
          ),
        )
      case pointerLen @ PointerLength(pointer) if isImmutablePointer(pointer) =>
        postCoerce(
          PointerBlockLength(pointer)(pointerLen.blame) -
            PointerBlockOffset(pointer)(pointerLen.blame)
        )
      case to @ ToNonNull(value) if value.t.asPointer.get.isImmutable =>
        getPointer(value, PointerNullOptNone(to.blame, value))
      case other => other.rewriteDefault()
    }
  }
}
