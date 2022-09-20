package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.newrewrite.adt.AImportADT.typeText
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.Generation

import scala.collection.mutable

case object ImportArray extends ImportADTBuilder("array") {
  case class ArrayField(t: Type[_]) extends Origin {
    override def preferredName: String = typeText(t)
    override def shortPosition: String = "generated"
    override def context: String = s"[At field generated for array location of type $t]"
    override def inlineContext: String = s"[Field generated for array location of type $t]"
  }

  case class ArrayNullOptNone(inner: Blame[ArrayNull], expr: Expr[_]) extends Blame[OptionNone] {
    override def blame(error: OptionNone): Unit =
      inner.blame(ArrayNull(expr))
  }

  case class ArrayBoundsPreconditionFailed(inner: Blame[ArrayBounds], subscript: Node[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(ArrayBounds(subscript))
  }

  case class ArrayFieldInsufficientPermission(inner: Blame[ArrayInsufficientPermission], expr: Expr[_]) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit =
      inner.blame(ArrayInsufficientPermission(expr))
  }
}

case class ImportArray[Pre <: Generation](importer: ImportADTImporter) extends AImportADT[Pre](importer) {
  import ImportArray._

  private lazy val arrayFile = parse("array")

  private lazy val arrayAdt = find[AxiomaticDataType[Post]](arrayFile, "array")
  private lazy val arrayAxLoc = find[ADTFunction[Post]](arrayAdt, "array_loc")
  private lazy val arrayLen = find[ADTFunction[Post]](arrayAdt, "alen")
  private lazy val arrayLoc = find[Function[Post]](arrayFile, "aloc")

  val arrayField: mutable.Map[Type[Post], SilverField[Post]] = mutable.Map()

  private def getArrayField(arr: Expr[Pre]): Ref[Post, SilverField[Post]] = {
    val tElement = dispatch(arr.t.asArray.get.element)
    arrayField.getOrElseUpdate(tElement, {
      globalDeclarations.declare(new SilverField(tElement)(ArrayField(tElement)))
    }).ref
  }

  override def applyCoercion(e: Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = coercion match {
    case CoerceNullArray(_) => OptNone()
    case other => super.applyCoercion(e, other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TArray(_) => TOption(TAxiomatic(arrayAdt.ref, Nil))
    case other => rewriteDefault(other)
  }

  override def dispatch(location: Location[Pre]): Location[Post] = location match {
    case loc@ArrayLocation(arr, index) =>
      SilverFieldLocation(
        obj = FunctionInvocation[Post](
          ref = arrayLoc.ref,
          args = Seq(
            OptGet(dispatch(arr))(ArrayNullOptNone(loc.blame, arr))(arr.o),
            dispatch(index)),
          typeArgs = Nil, Nil, Nil)(NoContext(ArrayBoundsPreconditionFailed(loc.blame, loc)))(loc.o),
        field = getArrayField(arr),
      )(loc.o)
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case sub@ArraySubscript(arr, index) =>
        SilverDeref(
          obj = FunctionInvocation[Post](
            ref = arrayLoc.ref,
            args = Seq(
              OptGet(dispatch(arr))(ArrayNullOptNone(sub.blame, arr))(arr.o),
              dispatch(index)),
            typeArgs = Nil, Nil, Nil)(NoContext(ArrayBoundsPreconditionFailed(sub.blame, sub))),
          field = getArrayField(arr))(ArrayFieldInsufficientPermission(sub.blame, sub))
      case length@Length(arr) =>
        ADTFunctionInvocation(None, arrayLen.ref, Seq(
          OptGet(dispatch(arr))(ArrayNullOptNone(length.blame, arr))(arr.o)
        ))
      case other => rewriteDefault(other)
    }
  }
}

