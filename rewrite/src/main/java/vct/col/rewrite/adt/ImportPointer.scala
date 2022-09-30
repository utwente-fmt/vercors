package vct.col.rewrite.adt

import vct.col.ast._
import ImportADT.typeText
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.Generation

import scala.collection.mutable

case object ImportPointer extends ImportADTBuilder("pointer") {
  case class PointerField(t: Type[_]) extends Origin {
    override def preferredName: String = typeText(t)
    override def shortPosition: String = "generated"
    override def context: String = s"[At field generated for pointer location of type $t]"
    override def inlineContext: String = s"[Field generated for pointer location of type $t]"
  }

  case class PointerNullOptNone(inner: Blame[PointerNull], expr: Expr[_]) extends Blame[OptionNone] {
    override def blame(error: OptionNone): Unit =
      inner.blame(PointerNull(expr))
  }

  case class PointerBoundsPreconditionFailed(inner: Blame[PointerBounds], expr: Node[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(PointerBounds(expr))
  }

  case class PointerFieldInsufficientPermission(inner: Blame[PointerInsufficientPermission], expr: Expr[_]) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit =
      inner.blame(PointerInsufficientPermission(expr))
  }
}

case class ImportPointer[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  import ImportPointer._

  private lazy val pointerFile = parse("pointer")

  private lazy val blockAdt = find[AxiomaticDataType[Post]](pointerFile, "block")
  private lazy val blockBase = find[ADTFunction[Post]](blockAdt, "base_addr")
  private lazy val blockLength = find[ADTFunction[Post]](blockAdt, "block_length")
  private lazy val blockLoc = find[ADTFunction[Post]](blockAdt, "loc")
  private lazy val pointerAdt = find[AxiomaticDataType[Post]](pointerFile, "pointer")
  private lazy val pointerOf = find[ADTFunction[Post]](pointerAdt, "pointer_of")
  private lazy val pointerBlock = find[ADTFunction[Post]](pointerAdt, "pointer_block")
  private lazy val pointerOffset = find[ADTFunction[Post]](pointerAdt, "pointer_offset")
  private lazy val pointerDeref = find[Function[Post]](pointerFile, "ptr_deref")
  private lazy val pointerAdd = find[Function[Post]](pointerFile, "ptr_add")

  val pointerField: mutable.Map[Type[Post], SilverField[Post]] = mutable.Map()

  private def getPointerField(ptr: Expr[Pre]): Ref[Post, SilverField[Post]] = {
    val tElement = dispatch(ptr.t.asPointer.get.element)
    pointerField.getOrElseUpdate(tElement, {
      globalDeclarations.declare(new SilverField(tElement)(PointerField(tElement)))
    }).ref
  }

  override def applyCoercion(e: Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = coercion match {
    case CoerceNullPointer(_) => OptNone()
    case other => super.applyCoercion(e, other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TPointer(_) => TOption(TAxiomatic(pointerAdt.ref, Nil))
    case other => rewriteDefault(other)
  }

  override def dispatch(location: Location[Pre]): Location[Post] = location match {
    case loc@PointerLocation(pointer) =>
      SilverFieldLocation(
        obj = FunctionInvocation[Post](
          ref = pointerDeref.ref,
          args = Seq(OptGet(dispatch(pointer))(PointerNullOptNone(loc.blame, pointer))(pointer.o)),
          typeArgs = Nil, Nil, Nil,
        )(PanicBlame("ptr_deref requires nothing."))(pointer.o),
        field = getPointerField(pointer),
      )(loc.o)
    case other => rewriteDefault(other)
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case sub@PointerSubscript(pointer, index) =>
        SilverDeref(
          obj = FunctionInvocation[Post](
            ref = pointerDeref.ref,
            args = Seq(FunctionInvocation[Post](
              ref = pointerAdd.ref,
              args = Seq(OptGet(dispatch(pointer))(PointerNullOptNone(sub.blame, pointer)), dispatch(index)),
              typeArgs = Nil, Nil, Nil)(NoContext(PointerBoundsPreconditionFailed(sub.blame, index)))),
            typeArgs = Nil, Nil, Nil,
          )(PanicBlame("ptr_deref requires nothing.")),
          field = getPointerField(pointer),
        )(PointerFieldInsufficientPermission(sub.blame, sub))
      case add@PointerAdd(pointer, offset) =>
        OptSome(FunctionInvocation[Post](
          ref = pointerAdd.ref,
          args = Seq(OptGet(dispatch(pointer))(PointerNullOptNone(add.blame, pointer)), dispatch(offset)),
          typeArgs = Nil, Nil, Nil,
        )(NoContext(PointerBoundsPreconditionFailed(add.blame, pointer))))
      case deref@DerefPointer(pointer) =>
        SilverDeref(
          obj = FunctionInvocation[Post](
            ref = pointerDeref.ref,
            args = Seq(OptGet(dispatch(pointer))(PointerNullOptNone(deref.blame, pointer))),
            typeArgs = Nil, Nil, Nil,
          )(PanicBlame("ptr_deref requires nothing.")),
          field = getPointerField(pointer),
        )(PointerFieldInsufficientPermission(deref.blame, deref))
      case len@PointerBlockLength(pointer) =>
        ADTFunctionInvocation[Post](
          typeArgs = Some((blockAdt.ref, Nil)),
          ref = blockLength.ref,
          args = Seq(ADTFunctionInvocation[Post](
            typeArgs = Some((pointerAdt.ref, Nil)),
            ref = pointerBlock.ref,
            args = Seq(OptGet(dispatch(pointer))(PointerNullOptNone(len.blame, pointer)))
          ))
        )
      case off@PointerBlockOffset(pointer) =>
        ADTFunctionInvocation[Post](
          typeArgs = Some((pointerAdt.ref, Nil)),
          ref = pointerOffset.ref,
          args = Seq(OptGet(dispatch(pointer))(PointerNullOptNone(off.blame, pointer)))
        )
      case other => rewriteDefault(other)
    }
  }
}


