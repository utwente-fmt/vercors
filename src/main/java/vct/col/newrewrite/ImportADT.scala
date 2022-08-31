package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.check.CheckError
import vct.col.coerce.CoercingRewriter
import vct.col.newrewrite.ImportADT.{InvalidImportedAdt, ArrayField, PointerField, OptionNonePreconditionFailed, MapKeyErrorPreconditionFailed, ArrayNullPreconditionFailed, ArrayBoundsPreconditionFailed, ArrayFieldInsufficientPermission, PointerNullPreconditionFailed, PointerBoundsPreconditionFailed, PointerFieldInsufficientPermission, RatZFracPreconditionFailed, RatFracPreconditionFailed, ZFracFracPreconditionFailed, NotLeftPreconditionFailed, NotRightPreconditionFailed}
import vct.col.newrewrite.error.ExtraNode
import vct.col.origin._
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{Generation, RewriterBuilderArg}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

import scala.collection.mutable
import scala.reflect.ClassTag

trait ImportADTImporter {
  def loadAdt[G](name: String): Program[G]
}

case object ImportADT extends RewriterBuilderArg[ImportADTImporter] {
  override def key: String = "adt"
  override def desc: String = "Import types into vercors that are defined externally, usually via an axiomatic datatype."

  private def typeText(t: Type[_]): String = t match {
    case _: TNotAValue[_] => throw ExtraNode
    case TVoid() => "void"
    case TBool() => "bool"
    case TFloat() => "float"
    case TChar() => "char"
    case TString() => "string"
    case TRef() => "ref"
    case TArray(element) => "arr_" + typeText(element)
    case TPointer(element) => "ptr_" + typeText(element)
    case TProcess() => "proc"
    case TModel(Ref(model)) => model.o.preferredName
    case TAxiomatic(Ref(adt), args) => args match {
      case Nil => adt.o.preferredName
      case ts => adt.o.preferredName + "$" + ts.map(typeText).mkString("__") + "$"
    }
    case TOption(element) => "opt_" + typeText(element)
    case TTuple(elements) => "tup$" + elements.map(typeText).mkString("__") + "$"
    case TEither(left, right) => "either$" + typeText(left) + "__" + typeText(right) + "$"
    case TSeq(element) => "seq_" + typeText(element)
    case TSet(element) => "set_" + typeText(element)
    case TBag(element) => "bag_" + typeText(element)
    case TMatrix(element) => "mat_" + typeText(element)
    case TType(t) => "typ_" + typeText(t)
    case TAny() => "any"
    case TNothing() => "nothing"
    case TNull() => "null"
    case TResource() => "res"
    case TInt() => "int"
    case TBoundedInt(gte, lt) => "int"
    case TRational() => "rat"
    case TFraction() => "fract"
    case TZFraction() => "zfract"
    case TMap(key, value) => "map$" + typeText(key) + "__" + typeText(value) + "$"
    case TClass(Ref(cls)) => cls.o.preferredName
    case TVar(Ref(v)) => v.o.preferredName
    case TUnion(ts) => "union$" + ts.map(typeText).mkString("__") + "$"
    case _: JavaType[_] => throw ExtraNode
    case _: CType[_] => throw ExtraNode
    case _: PVLType[_] => throw ExtraNode
  }

  case class InvalidImportedAdt(errors: Seq[CheckError]) extends UserError {
    override def code: String = "invalidAdt"
    override def text: String =
      "Errors were encountered while importing the definition of an internal axiomatic datatype:\n" +
        errors.map(_.toString).mkString("\n")
  }

  case class ArrayField(t: Type[_]) extends Origin {
    override def preferredName: String = typeText(t)
    override def shortPosition: String = "generated"
    override def context: String = s"[At field generated for array location of type $t]"
    override def inlineContext: String = s"[Field generated for array location of type $t]"
  }

  case class PointerField(t: Type[_]) extends Origin {
    override def preferredName: String = typeText(t)
    override def shortPosition: String = "generated"
    override def context: String = s"[At field generated for pointer location of type $t]"
    override def inlineContext: String = s"[Field generated for pointer location of type $t]"
  }

  case class OptionNonePreconditionFailed(access: OptGet[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      access.blame.blame(OptionNone(access))
  }

  case class MapKeyErrorPreconditionFailed(access: MapGet[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      access.blame.blame(MapKeyError(access))
  }

  case class ArrayNullPreconditionFailed(inner: Blame[ArrayNull], expr: Expr[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(ArrayNull(expr))
  }

  case class ArrayBoundsPreconditionFailed(inner: Blame[ArrayBounds], subscript: ArraySubscript[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(ArrayBounds(subscript))
  }

  case class ArrayFieldInsufficientPermission(inner: Blame[ArrayInsufficientPermission], expr: Expr[_]) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit =
      inner.blame(ArrayInsufficientPermission(expr))
  }

  case class PointerNullPreconditionFailed(inner: Blame[PointerNull], expr: Expr[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(PointerNull(expr))
  }

  case class PointerBoundsPreconditionFailed(inner: Blame[PointerBounds], expr: Expr[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(PointerBounds(expr))
  }

  case class PointerFieldInsufficientPermission(inner: Blame[PointerInsufficientPermission], expr: Expr[_]) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit =
      inner.blame(PointerInsufficientPermission(expr))
  }

  case class RatZFracPreconditionFailed(inner: Blame[CoerceRatZFracFailed], expr: Expr[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(CoerceRatZFracFailed(expr))
  }

  case class RatFracPreconditionFailed(inner: Blame[CoerceRatFracFailed], expr: Expr[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(CoerceRatFracFailed(expr))
  }

  case class ZFracFracPreconditionFailed(inner: Blame[CoerceZFracFracFailed], expr: Expr[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(CoerceZFracFracFailed(expr))
  }

  case class NotLeftPreconditionFailed(get: GetLeft[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      get.blame.blame(NotLeft(get))
  }

  case class NotRightPreconditionFailed(get: GetRight[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      get.blame.blame(NotRight(get))
  }
}

case class ImportADT[Pre <: Generation](importer: ImportADTImporter) extends CoercingRewriter[Pre] {
  val arrayField: mutable.Map[Type[Post], SilverField[Post]] = mutable.Map()
  val pointerField: mutable.Map[Type[Post], SilverField[Post]] = mutable.Map()

  private def parse(name: String): Seq[GlobalDeclaration[Post]] = {
    val program = importer.loadAdt[Pre](name)
    program.declarations.foreach(dispatch)
    program.declarations.map(lookupSuccessor(_).get.asInstanceOf[GlobalDeclaration[Post]])
  }

  private lazy val nothingFile = parse("nothing")
  private lazy val nullFile = parse("null")
  private lazy val voidFile = parse("void")
  private lazy val anyFile = parse("any")
  private lazy val fracFile = parse("frac")
  private lazy val zfracFile = parse("zfrac")
  private lazy val tupleFile = parse("tuple")
  private lazy val optionFile = parse("option")
  private lazy val eitherFile = parse("either")
  private lazy val mapCompatFile = parse("map_compat")
  private lazy val arrayFile = parse("array")
  private lazy val pointerFile = parse("pointer")

  def find[T](decls: Seq[Declaration[Post]], name: String)(implicit tag: ClassTag[T]): T =
    decls.collectFirst {
      case decl: T if decl.o.isInstanceOf[SourceNameOrigin] && decl.o.asInstanceOf[SourceNameOrigin].name == name =>
        decl
    }.get

  def find[T](decls: Declarator[Post], name: String)(implicit tag: ClassTag[T]): T =
    find(decls.declarations, name)(tag)

  private lazy val nothingAdt = find[AxiomaticDataType[Post]](nothingFile, "nothing")
  private lazy val nothingAs = find[Function[Post]](nothingFile, "nothing_as")

  private lazy val voidAdt = find[AxiomaticDataType[Post]](voidFile, "void")
  private lazy val voidUnit = find[ADTFunction[Post]](voidAdt, "unit")

  private lazy val nullAdt = find[AxiomaticDataType[Post]](nullFile, "t_null")
  private lazy val nullValue = find[ADTFunction[Post]](nullAdt, "v_null")

  private lazy val anyAdt = find[AxiomaticDataType[Post]](anyFile, "any")
  private lazy val anyFrom = find[Function[Post]](anyFile, "as_any")

  private lazy val fracAdt = find[AxiomaticDataType[Post]](fracFile, "frac")
  private lazy val fracVal = find[ADTFunction[Post]](fracAdt, "frac_val")
  private lazy val fracNew = find[Function[Post]](fracFile, "new_frac")

  private lazy val zfracAdt = find[AxiomaticDataType[Post]](zfracFile, "zfrac")
  private lazy val zfracVal = find[ADTFunction[Post]](zfracAdt, "zfrac_val")
  private lazy val zfracNew = find[Function[Post]](zfracFile, "new_zfrac")

  private lazy val tupleAdt = find[AxiomaticDataType[Post]](tupleFile, "tuple")
  private lazy val tupleTup = find[ADTFunction[Post]](tupleAdt, "tup")
  private lazy val tupleFst = find[ADTFunction[Post]](tupleAdt, "fst")
  private lazy val tupleSnd = find[ADTFunction[Post]](tupleAdt, "snd")

  private lazy val optionAdt = find[AxiomaticDataType[Post]](optionFile, "option")
  private lazy val optionNone = find[ADTFunction[Post]](optionAdt, "None")
  private lazy val optionSome = find[ADTFunction[Post]](optionAdt, "some")
  private lazy val optionAxGet = find[ADTFunction[Post]](optionAdt, "option_get")
  private lazy val optionGet = find[Function[Post]](optionFile, "opt_get")
  private lazy val optionGetOrElse = find[Function[Post]](optionFile, "opt_or_else")

  private lazy val eitherAdt = find[AxiomaticDataType[Post]](eitherFile, "either")
  private lazy val eitherLeft = find[ADTFunction[Post]](eitherAdt, "left")
  private lazy val eitherRight = find[ADTFunction[Post]](eitherAdt, "right")
  private lazy val eitherIsRight = find[ADTFunction[Post]](eitherAdt, "is_right")
  private lazy val eitherAxGetLeft = find[ADTFunction[Post]](eitherAdt, "either_get_left")
  private lazy val eitherAxGetRight = find[ADTFunction[Post]](eitherAdt, "either_get_right")
  private lazy val eitherGetLeft = find[Function[Post]](eitherFile, "get_left")
  private lazy val eitherGetRight = find[Function[Post]](eitherFile, "get_right")

  private lazy val mapCompatAdt = find[AxiomaticDataType[Post]](mapCompatFile, "map_compat")
  private lazy val mapItems = find[ADTFunction[Post]](mapCompatAdt, "map_items")
  private lazy val mapDisjoint = find[ADTFunction[Post]](mapCompatAdt, "map_disjoint")
  private lazy val mapRemove = find[ADTFunction[Post]](mapCompatAdt, "map_remove")

  private lazy val arrayAdt = find[AxiomaticDataType[Post]](arrayFile, "array")
  private lazy val arrayAxLoc = find[ADTFunction[Post]](arrayAdt, "array_loc")
  private lazy val arrayLen = find[ADTFunction[Post]](arrayAdt, "alen")
  private lazy val arrayLoc = find[Function[Post]](arrayFile, "aloc")

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

  val globalBlame: ScopedStack[Blame[UnsafeCoercion]] = ScopedStack()

  def optNone(t: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((optionAdt.ref, Seq(t))),
      optionNone.ref, Nil,
    )

  def optSome(e: Expr[Post], t: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((optionAdt.ref, Seq(t))),
      optionSome.ref, Seq(e),
    )

  def optGet(e: Expr[Post], t: Type[Post], blame: Blame[PreconditionFailed])(implicit o: Origin): Expr[Post] =
    FunctionInvocation[Post](optionGet.ref, Seq(e), Seq(t), Nil, Nil)(NoContext(blame))

  def eitherIsRight(e: Expr[Post], leftType: Type[Post], rightType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation(
      Some((
        eitherAdt.ref,
        Seq(leftType, rightType)
      )),
      eitherIsRight.ref,
      Seq(e),
    )

  def eitherGetLeft(e: Expr[Post], leftType: Type[Post], rightType: Type[Post], blame: Blame[PreconditionFailed])(implicit o: Origin): Expr[Post] =
    FunctionInvocation[Post](eitherGetLeft.ref,
      Seq(e),
      Seq(leftType, rightType), Nil, Nil,
    )(NoContext(blame))

  def eitherGetRight(e: Expr[Post], leftType: Type[Post], rightType: Type[Post], blame: Blame[PreconditionFailed])(implicit o: Origin): Expr[Post] =
    FunctionInvocation[Post](eitherGetRight.ref,
      Seq(e),
      Seq(leftType, rightType), Nil, Nil,
    )(NoContext(blame))

  def eitherLeft(e: Expr[Post], leftType: Type[Post], rightType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((
        eitherAdt.ref,
        Seq(leftType, rightType),
      )),
      eitherLeft.ref,
      Seq(e),
    )

  def eitherRight(e: Expr[Post], leftType: Type[Post], rightType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((
        eitherAdt.ref,
        Seq(leftType, rightType),
      )),
      eitherRight.ref,
      Seq(e),
    )

  def tupFst(e: Expr[Post], fstType: Type[Post], sndType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((tupleAdt.ref, Seq(fstType, sndType))),
      tupleFst.ref, Seq(e),
    )

  def tupSnd(e: Expr[Post], fstType: Type[Post], sndType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((tupleAdt.ref, Seq(fstType, sndType))),
      tupleSnd.ref, Seq(e),
    )

  def tup(fst: Expr[Post], fstType: Type[Post], snd: Expr[Post], sndType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((tupleAdt.ref, Seq(fstType, sndType))),
      tupleTup.ref, Seq(fst, snd),
    )

  override def applyCoercion(e: Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = coercion match {
    case CoerceNothingSomething(target) =>
      FunctionInvocation[Post](nothingAs.ref, Seq(e), Seq(dispatch(target)), Nil, Nil)(PanicBlame("coercing from nothing requires nothing."))
    case CoerceSomethingAny(source) =>
      FunctionInvocation[Post](anyFrom.ref, Seq(e), Seq(dispatch(source)), Nil, Nil)(PanicBlame("coercing to any requires nothing."))

    case CoerceMapOption(inner, source, target) =>
      Select(
        Eq(e, optNone(dispatch(source))),
        optNone(dispatch(target)),
        optSome(applyCoercion(optGet(e, dispatch(source), NeverNone), inner), dispatch(target))
      )
    case CoerceMapEither((innerLeft, innerRight), (sourceLeft, sourceRight), (targetLeft, targetRight)) =>
      Select(
        eitherIsRight(e, dispatch(sourceLeft), dispatch(sourceRight)),
        eitherRight(applyCoercion(eitherGetRight(e, dispatch(sourceLeft), dispatch(sourceRight), FramedGetRight), innerRight), dispatch(targetLeft), dispatch(targetRight)),
        eitherLeft(applyCoercion(eitherGetLeft(e, dispatch(sourceLeft), dispatch(sourceRight), FramedGetLeft), innerRight), dispatch(targetLeft), dispatch(targetRight)),
      )
    case CoerceMapTuple(Seq(inner1, inner2), Seq(s1, s2), Seq(t1, t2)) =>
      tup(
        applyCoercion(tupFst(e, dispatch(s1), dispatch(s2)), inner1),
        dispatch(t1),
        applyCoercion(tupSnd(e, dispatch(s1), dispatch(s2)), inner2),
        dispatch(t2),
      )

    case CoerceNullArray(_) =>
      ADTFunctionInvocation[Post](
        Some((
          optionAdt.ref,
          Seq(TAxiomatic(arrayAdt.ref, Nil)),
        )),
        optionNone.ref, Nil
      )
    case CoerceNullPointer(_) =>
      ADTFunctionInvocation[Post](
        Some((
          optionAdt.ref,
          Seq(TAxiomatic(pointerAdt.ref, Nil)),
        )),
        optionNone.ref, Nil
      )
    case CoerceNullRef() =>
      SilverNull()

    case CoerceZFracRat() =>
      ADTFunctionInvocation[Post](Some((zfracAdt.ref, Nil)), zfracVal.ref, Seq(e))
    case CoercionSequence(Seq(CoerceFracZFrac(), CoerceZFracRat())) =>
      ADTFunctionInvocation(Some((fracAdt.ref, Nil)), fracVal.ref, Seq(e))
    case CoerceFracZFrac() =>
      val rat = ADTFunctionInvocation[Post](Some((fracAdt.ref, Nil)), fracVal.ref, Seq(e))
      FunctionInvocation[Post](zfracNew.ref, Seq(rat), Nil, Nil, Nil)(PanicBlame("a frac always fits in a zfrac."))

    case CoerceRatZFrac() =>
      FunctionInvocation[Post](zfracNew.ref, Seq(e), Nil, Nil, Nil)(NoContext(RatZFracPreconditionFailed(globalBlame.top, e)))
    case CoercionSequence(Seq(CoerceRatZFrac(), CoerceZFracFrac())) =>
      FunctionInvocation[Post](fracNew.ref, Seq(e), Nil, Nil, Nil)(NoContext(RatZFracPreconditionFailed(globalBlame.top, e)))
    case CoerceZFracFrac() =>
      val rat = ADTFunctionInvocation[Post](Some((zfracAdt.ref, Nil)), zfracVal.ref, Seq(e))
      FunctionInvocation[Post](fracNew.ref, Seq(rat), Nil, Nil, Nil)(NoContext(ZFracFracPreconditionFailed(globalBlame.top, e)))

    case CoerceBoundIntFrac() =>
      FunctionInvocation[Post](fracNew.ref, Seq(WritePerm()), Nil, Nil, Nil)(PanicBlame("The constant 1 always fits in a frac."))
    case CoerceBoundIntZFrac(_) =>
      FunctionInvocation[Post](zfracNew.ref, Seq(Select(e === const(0), NoPerm(), WritePerm())), Nil, Nil, Nil)(PanicBlame("The constants 0 and 1 always fit in a zfrac."))

    case _ => super.applyCoercion(e, coercion)
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    globalBlame.having(program.blame) {
      program.rewrite(declarations = collectInScope(globalScopes) {
        parse("viper_order")
        anyFile
        program.declarations.foreach(dispatch)
      })
    }
  }

  override def dispatch(t: Type[Pre]): Type[Post] = {
    implicit val o: Origin = t.o
    t match {
      case TType(TAny()) => TType(TAxiomatic(new LazyRef(anyAdt), Nil))
      case TNothing() => TAxiomatic(nothingAdt.ref, Nil)
      case TVoid() => TAxiomatic(voidAdt.ref, Nil)
      case TNull() => TAxiomatic(nullAdt.ref, Nil)
      case TAny() => TAxiomatic(anyAdt.ref, Nil)
      case TFraction() => TAxiomatic(fracAdt.ref, Nil)
      case TZFraction() => TAxiomatic(zfracAdt.ref, Nil)
      case TTuple(Seq(t1, t2)) => TAxiomatic(tupleAdt.ref, Seq(dispatch(t1), dispatch(t2)))
      case TEither(left, right) => TAxiomatic(eitherAdt.ref, Seq(dispatch(left), dispatch(right)))
      case TOption(element) => TAxiomatic(optionAdt.ref, Seq(dispatch(element)))
      case TArray(_) => TAxiomatic(optionAdt.ref, Seq(TAxiomatic(arrayAdt.ref, Nil)))
      case TPointer(_) => TAxiomatic(optionAdt.ref, Seq(TAxiomatic(pointerAdt.ref, Nil)))
      case other => rewriteDefault(other)
    }
  }

  private def mapTypeArgs(map: Expr[Pre]): Seq[Type[Post]] = {
    val mapType = map.t.asMap.get
    Seq(dispatch(mapType.key), dispatch(mapType.value))
  }

  private def getArrayField(arr: Expr[Pre]): Ref[Post, SilverField[Post]] = {
    val tElement = dispatch(arr.t.asArray.get.element)
    arrayField.getOrElseUpdate(tElement, {
      val field = new SilverField(tElement)(ArrayField(tElement))
      field.declareDefault(this)
      field
    }).ref
  }

  private def getPointerField(ptr: Expr[Pre]): Ref[Post, SilverField[Post]] = {
    val tElement = dispatch(ptr.t.asPointer.get.element)
    pointerField.getOrElseUpdate(tElement, {
      val field = new SilverField(tElement)(PointerField(tElement))
      field.declareDefault(this)
      field
    }).ref
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o

    e match {
      case Null() =>
        // Uncoerced, so will become TNull
        ADTFunctionInvocation(None, nullValue.ref, Nil)
      case Void() =>
        ADTFunctionInvocation(None, voidUnit.ref, Nil)
      case LiteralTuple(Seq(t1, t2), Seq(v1, v2)) =>
        tup(dispatch(v1), dispatch(t1), dispatch(v2), dispatch(t2))
      case TupGet(tup, 0) =>
        val ts = tup.t.asTuple.get.elements.map(dispatch)
        tupFst(dispatch(tup), ts(0), ts(1))
      case TupGet(tup, 1) =>
        val ts = tup.t.asTuple.get.elements.map(dispatch)
        tupSnd(dispatch(tup), ts(0), ts(1))
      case OptNone() =>
        optNone(TAxiomatic(nothingAdt.ref, Nil))
      case OptSome(element) =>
        optSome(dispatch(element), dispatch(element.t))
      case access @ OptGet(opt) =>
        optGet(dispatch(opt), dispatch(opt.t.asOption.get.element), OptionNonePreconditionFailed(access))
      case get @ OptGetOrElse(opt, alt) =>
        FunctionInvocation[Post](optionGetOrElse.ref,
          Seq(dispatch(opt), dispatch(alt)),
          Seq(dispatch(get.t)), Nil, Nil,
        )(PanicBlame("opt_or_else requires nothing."))
      case EitherLeft(e) =>
        eitherLeft(dispatch(e), dispatch(e.t), dispatch(TNothing()))
      case EitherRight(e) =>
        eitherRight(dispatch(e), dispatch(TNothing()), dispatch(e.t))
      case get @ GetLeft(e) =>
        eitherGetLeft(dispatch(e), dispatch(get.eitherType.left), dispatch(get.eitherType.right), NotLeftPreconditionFailed(get))
      case get @ GetRight(e) =>
        eitherGetRight(dispatch(e), dispatch(get.eitherType.left), dispatch(get.eitherType.right), NotRightPreconditionFailed(get))
      case is @ IsLeft(e) =>
        Not(eitherIsRight(dispatch(e), dispatch(is.eitherType.left), dispatch(is.eitherType.right)))
      case is @ IsRight(e) =>
        eitherIsRight(dispatch(e), dispatch(is.eitherType.left), dispatch(is.eitherType.right))
      case MapItemSet(map) =>
        ADTFunctionInvocation[Post](
          Some((mapCompatAdt.ref, mapTypeArgs(map))),
          mapItems.ref,
          Seq(dispatch(map))
        )
      case disj @ MapDisjoint(left, right) =>
        ADTFunctionInvocation[Post](
          Some((mapCompatAdt.ref, Seq(dispatch(disj.commonMapType.key), dispatch(disj.commonMapType.value)))),
          mapDisjoint.ref,
          Seq(dispatch(left), dispatch(right)),
        )
      case MapRemove(map, k) =>
        ADTFunctionInvocation[Post](
          Some((mapCompatAdt.ref, mapTypeArgs(map))),
          mapRemove.ref,
          Seq(dispatch(map), dispatch(k)),
        )
      case sub @ ArraySubscript(arr, index) =>
        SilverDeref(
          obj = FunctionInvocation[Post](
            ref = arrayLoc.ref,
            args = Seq(
              FunctionInvocation[Post](optionGet.ref, Seq(dispatch(arr)), Seq(TAxiomatic(arrayAdt.ref, Nil)), Nil, Nil)(
                NoContext(ArrayNullPreconditionFailed(sub.blame, arr))),
              dispatch(index)),
            typeArgs = Nil, Nil, Nil)(NoContext(ArrayBoundsPreconditionFailed(sub.blame, sub))),
          field = getArrayField(arr))(ArrayFieldInsufficientPermission(sub.blame, sub))
      case length @ Length(arr) =>
        ADTFunctionInvocation(None, arrayLen.ref, Seq(
          FunctionInvocation[Post](optionGet.ref, Seq(dispatch(arr)), Seq(TAxiomatic[Post](arrayAdt.ref, Nil)), Nil, Nil)(
            NoContext(ArrayNullPreconditionFailed(length.blame, length)))
        ))
      case sub @ PointerSubscript(pointer, index) =>
        SilverDeref(
          obj = FunctionInvocation[Post](
            ref = pointerDeref.ref,
            args = Seq(FunctionInvocation[Post](
              ref = pointerAdd.ref,
              args = Seq(FunctionInvocation[Post](
                ref = optionGet.ref,
                args = Seq(dispatch(pointer)),
                typeArgs = Seq(TAxiomatic[Post](pointerAdt.ref, Nil)), Nil, Nil,
              )(NoContext(PointerNullPreconditionFailed(sub.blame, pointer))), dispatch(index)),
              typeArgs = Nil, Nil, Nil)(NoContext(PointerBoundsPreconditionFailed(sub.blame, index)))),
            typeArgs = Nil, Nil, Nil,
          )(PanicBlame("ptr_deref requires nothing.")),
          field = getPointerField(pointer),
        )(PointerFieldInsufficientPermission(sub.blame, sub))
      case add @ PointerAdd(pointer, offset) =>
        FunctionInvocation[Post](
          ref = pointerAdd.ref,
          args = Seq(FunctionInvocation[Post](
            ref = optionGet.ref,
            args = Seq(dispatch(pointer)),
            typeArgs = Seq(TAxiomatic[Post](pointerAdt.ref, Nil)), Nil, Nil,
          )(NoContext(PointerNullPreconditionFailed(add.blame, pointer))), dispatch(offset)),
          typeArgs = Nil, Nil, Nil,
        )(NoContext(PointerBoundsPreconditionFailed(add.blame, pointer)))
      case deref @ DerefPointer(pointer) =>
        SilverDeref(
          obj = FunctionInvocation[Post](
            ref = pointerDeref.ref,
            args = Seq(FunctionInvocation[Post](
              ref = optionGet.ref,
              args = Seq(dispatch(pointer)),
              typeArgs = Seq(TAxiomatic[Post](pointerAdt.ref, Nil)), Nil, Nil,
            )(NoContext(PointerNullPreconditionFailed(deref.blame, pointer)))),
            typeArgs = Nil, Nil, Nil,
          )(PanicBlame("ptr_deref requires nothing.")),
          field = getPointerField(pointer),
        )(PointerFieldInsufficientPermission(deref.blame, deref))
      case other => rewriteDefault(other)
    }
  }

  // PB: dumb hack alert: TVoid and Return(Void()) is (for viper) a marker to indicate that there is no return type.
  override def postCoerce(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] if method.returnType == TVoid[Pre]() =>
      method.rewrite(returnType = TVoid()).succeedDefault(decl)
    case other => super.postCoerce(other)
  }

  override def postCoerce(stat: Statement[Pre]): Statement[Post] = stat match {
    case ret @ Return(v @ Void()) => ret.rewrite(result=Void()(v.o))
    case other => rewriteDefault(other)
  }
}
