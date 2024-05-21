package vct.col.rewrite.adt

import vct.col.ast._
import ImportADT.typeText
import vct.col.ast.RewriteHelpers._
import hre.util.ScopedStack
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.Generation

import scala.collection.mutable

case object ImportArray extends ImportADTBuilder("array") {
  private def ArrayField(t: Type[_]): Origin = Origin(
    Seq(
      PreferredName(Seq(typeText(t))),
      LabelContext("array field"),
    )
  )

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

case class ImportArray[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  import ImportArray._

  private lazy val arrayFile = parse("array")

  private lazy val arrayAdt = find[AxiomaticDataType[Post]](arrayFile, "array")
  private lazy val arrayAxLoc = find[ADTFunction[Post]](arrayAdt, "array_loc")
  private lazy val arrayLen = find[ADTFunction[Post]](arrayAdt, "alen")
  private lazy val arrayLoc = find[Function[Post]](arrayFile, "aloc")

  val arrayField: mutable.Map[Type[Pre], SilverField[Post]] = mutable.Map()

  private def getArrayField(arr: Expr[Pre]): Ref[Post, SilverField[Post]] = {
    val tElement = arr.t.asArray.get.element
    arrayField.getOrElseUpdate(tElement, {
      globalDeclarations.declare(new SilverField(dispatch(tElement))(ArrayField(tElement)))
    }).ref
  }

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = coercion match {
    case CoerceNullArray(_) => OptNoneTyped(TAxiomatic(arrayAdt.ref, Nil))
    case other => super.applyCoercion(e, other)
  }

  override def postCoerce(t: Type[Pre]): Type[Post] = t match {
    case TArray(_) => TOption(TAxiomatic(arrayAdt.ref, Nil))
    case other => rewriteDefault(other)
  }

  override def postCoerce(location: Location[Pre]): Location[Post] = location match {
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

  def rewriteTopLevelArraySubscriptsInTrigger(e: Expr[Pre]): Expr[Post] = e match {
    case sub @ ArraySubscript(arr, index) =>
      implicit val o: Origin = e.o
      FunctionInvocation[Post](
        ref = arrayLoc.ref,
        args = Seq(
          OptGet(dispatch(arr))(ArrayNullOptNone(sub.blame, arr))(arr.o),
          dispatch(index)),
          typeArgs = Nil, Nil, Nil)(NoContext(ArrayBoundsPreconditionFailed(sub.blame, sub)))
    case other => rewriteDefault(other)
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case f @ Forall(_, triggers, _) =>
        f.rewrite(triggers =
          triggers.map(_.map(rewriteTopLevelArraySubscriptsInTrigger))
        )
      case s @ Starall(_, triggers, _) =>
        s.rewrite(triggers =
          triggers.map(_.map(rewriteTopLevelArraySubscriptsInTrigger))
        )
      case e @ Exists(_, triggers, _) =>
        e.rewrite(triggers =
          triggers.map(_.map(rewriteTopLevelArraySubscriptsInTrigger))
        )
      case sub@ArraySubscript(arr, index) =>
        SilverDeref(
          FunctionInvocation[Post](
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

