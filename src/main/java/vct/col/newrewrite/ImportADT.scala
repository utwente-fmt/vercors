package vct.col.newrewrite

import hre.config.Configuration
import hre.util.ScopedStack
import vct.col.coerce.Coercion.{FracZFrac, NullArray, NullPointer, ZFracRat}
import vct.col.ast._
import vct.col.newrewrite.ImportADT.{ArrayBoundsPreconditionFailed, ArrayField, ArrayFieldInsufficientPermission, ArrayNullPreconditionFailed, InvalidImportedAdt, MapKeyErrorPreconditionFailed, NotLeftPreconditionFailed, NotRightPreconditionFailed, OptionNonePreconditionFailed, PointerBoundsPreconditionFailed, PointerField, PointerFieldInsufficientPermission, PointerNullPreconditionFailed, RatZFracPreconditionFailed, ZFracFracPreconditionFailed}
import vct.col.newrewrite.error.{ExcludedByPassOrder, ExtraNode}
import vct.parsers.Parsers
import RewriteHelpers._
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.check.CheckError
import vct.col.coerce.{CoercingRewriter, Coercion}
import vct.col.newrewrite.lang.{LangSpecificToCol, LangTypesToCol}
import vct.col.origin._
import vct.col.ref.{LazyRef, Ref}
import vct.col.resolve.{ResolveReferences, ResolveTypes}
import vct.col.rewrite.{Generation, InitialGeneration, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.MethodBuildHelpers
import vct.result.VerificationResult.UserError

import scala.collection.mutable
import scala.reflect.ClassTag

case object ImportADT extends RewriterBuilder {
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
    case TAxiomatic(Ref(adt), args) => adt.o.preferredName + "$" + args.map(typeText).mkString("__") + "$"
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
    override def messageInContext(message:  String): String =
      s"At field generated for array location of type $t: $message"
  }

  case class PointerField(t: Type[_]) extends Origin {
    override def preferredName: String = typeText(t)
    override def messageInContext(message:  String): String =
      s"At field generated for pointer location of type $t: $message"
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

  case class ArrayBoundsPreconditionFailed(inner: Blame[ArrayBounds], idx: Expr[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(ArrayBounds(idx))
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

case class ImportADT[Pre <: Generation]() extends CoercingRewriter[Pre] {
  val arrayField: mutable.Map[Type[Post], SilverField[Post]] = mutable.Map()
  val pointerField: mutable.Map[Type[Post], SilverField[Post]] = mutable.Map()

  private def parse(name: String): Seq[GlobalDeclaration[Post]] = {
    val decls = Parsers.parse[InitialGeneration](Configuration.getAdtFile(s"$name.pvl").toPath).decls
    val moreDecls = ResolveTypes.resolve(Program(decls, None)(DiagnosticOrigin)(DiagnosticOrigin))
    val typedProgram = LangTypesToCol().dispatch(Program(decls ++ moreDecls, None)(DiagnosticOrigin)(DiagnosticOrigin))
    val errors = ResolveReferences.resolve(typedProgram)
    if(errors.nonEmpty) throw InvalidImportedAdt(errors)
    val regularProgram = LangSpecificToCol().dispatch(typedProgram)
    val program = regularProgram.asInstanceOf[Program[Pre]]
    program.declarations.foreach(dispatch)
    program.declarations.map(successionMap(_).asInstanceOf[GlobalDeclaration[Post]])
  }

  private lazy val nothingFile = parse("nothing")
  private lazy val voidFile = parse("void")
  private lazy val anyFile = parse("any")
  private lazy val fracFile = parse("frac")
  private lazy val zfracFile = parse("zfrac")
  private lazy val tupleFile = parse("tuple")
  private lazy val optionFile = parse("option")
  private lazy val eitherFile = parse("either")
  private lazy val mapFile = parse("map")
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

  private lazy val mapAdt = find[AxiomaticDataType[Post]](mapFile, "map")
  private lazy val mapEmpty = find[ADTFunction[Post]](mapAdt, "map_empty")
  private lazy val mapCons = find[ADTFunction[Post]](mapAdt, "map_cons")
  private lazy val mapKeys = find[ADTFunction[Post]](mapAdt, "map_keys")
  private lazy val mapAxConsInvK = find[ADTFunction[Post]](mapAdt, "map_cons_inv_k")
  private lazy val mapAxConsInvV = find[ADTFunction[Post]](mapAdt, "map_cons_inv_v")
  private lazy val mapAxConsInvTail = find[ADTFunction[Post]](mapAdt, "map_cons_inv_tail")
  private lazy val mapSize = find[Function[Post]](mapFile, "map_size")
  private lazy val mapConsGetK = find[Function[Post]](mapFile, "map_cons_get_k")
  private lazy val mapConsGetV = find[Function[Post]](mapFile, "map_cons_get_v")
  private lazy val mapConsGetTail = find[Function[Post]](mapFile, "map_cons_get_tail")
  private lazy val mapGet = find[Function[Post]](mapFile, "map_get")
  private lazy val mapValues = find[Function[Post]](mapFile, "map_values")
  private lazy val mapItems = find[Function[Post]](mapFile, "map_items")
  private lazy val mapEquals = find[Function[Post]](mapFile, "map_equals")
  private lazy val mapDisjoint = find[Function[Post]](mapFile, "map_disjoint")
  private lazy val mapRemove = find[Function[Post]](mapFile, "map_remove")

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

  private def preAdt(ref: Ref[Post, AxiomaticDataType[Post]]): Ref[Pre, AxiomaticDataType[Pre]] =
    transmutePostRef[AxiomaticDataType, AxiomaticDataType[Pre], AxiomaticDataType[Post]](ref)

  private def preAdtFunc(ref: Ref[Post, ADTFunction[Post]]): Ref[Pre, ADTFunction[Pre]] =
    transmutePostRef[ADTFunction, ADTFunction[Pre], ADTFunction[Post]](ref)

  private def preFunc(ref: Ref[Post, Function[Post]]): Ref[Pre, Function[Pre]] =
    transmutePostRef[Function, Function[Pre], Function[Post]](ref)

  override def applyCoercion(e: Expr[Pre], coercion: Coercion[Pre])(implicit o: Origin): Expr[Pre] = coercion match {
    case Coercion.NothingSomething(target) =>
      FunctionInvocation(preFunc(nothingAs.ref), Seq(e), Seq(target))(PanicBlame("coercing from nothing requires nothing."))
    case Coercion.SomethingAny(source) =>
      FunctionInvocation(preFunc(anyFrom.ref), Seq(e), Seq(source))(PanicBlame("coercing to any requires nothing."))

    case Coercion.NullArray(_) =>
      ADTFunctionInvocation(
        Some((
          preAdt(optionAdt.ref),
          Seq(TAxiomatic(preAdt(arrayAdt.ref), Nil)),
        )),
        preAdtFunc(optionNone.ref), Nil
      )
    case Coercion.NullPointer(_) =>
      ADTFunctionInvocation(
        Some((
          preAdt(optionAdt.ref),
          Seq(TAxiomatic(preAdt(pointerAdt.ref), Nil)),
        )),
        preAdtFunc(optionNone.ref), Nil
      )

    case Coercion.ZFracRat() =>
      ADTFunctionInvocation(Some((preAdt(zfracAdt.ref), Nil)), preAdtFunc(zfracVal.ref), Seq(e))
    case Coercion.Compose(Coercion.ZFracRat(), Coercion.FracZFrac()) if e == ReadPerm[Pre]() =>
      e
    case Coercion.Compose(Coercion.ZFracRat(), Coercion.FracZFrac()) =>
      ADTFunctionInvocation(Some((preAdt(fracAdt.ref), Nil)), preAdtFunc(fracVal.ref), Seq(e))
    case Coercion.FracZFrac() =>
      val rat = ADTFunctionInvocation(Some((preAdt(fracAdt.ref), Nil)), preAdtFunc(fracVal.ref), Seq(e))
      FunctionInvocation(preFunc(zfracNew.ref), Seq(rat), Nil)(PanicBlame("a frac always fits in a zfrac."))

    case Coercion.RatZFrac() =>
      FunctionInvocation(preFunc(zfracNew.ref), Seq(e), Nil)(NoContext(RatZFracPreconditionFailed(globalBlame.top, e)))
    case Coercion.Compose(Coercion.ZFracFrac(), Coercion.RatZFrac()) =>
      FunctionInvocation(preFunc(fracNew.ref), Seq(e), Nil)(NoContext(RatZFracPreconditionFailed(globalBlame.top, e)))
    case Coercion.ZFracFrac() =>
      val rat = ADTFunctionInvocation(Some((preAdt(zfracAdt.ref), Nil)), preAdtFunc(zfracVal.ref), Seq(e))
      FunctionInvocation(preFunc(fracNew.ref), Seq(rat), Nil)(NoContext(ZFracFracPreconditionFailed(globalBlame.top, e)))

    case _ => super.applyCoercion(e, coercion)
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    globalBlame.having(program.blame) {
      program.rewrite(declarations = collectInScope(globalScopes) {
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
      case TAny() => TAxiomatic(anyAdt.ref, Nil)
      case TFraction() => TAxiomatic(fracAdt.ref, Nil)
      case TZFraction() => TAxiomatic(zfracAdt.ref, Nil)
      case TTuple(Seq(t1, t2)) => TAxiomatic(tupleAdt.ref, Seq(dispatch(t1), dispatch(t2)))
      case TEither(left, right) => TAxiomatic(eitherAdt.ref, Seq(dispatch(left), dispatch(right)))
      case TOption(element) => TAxiomatic(optionAdt.ref, Seq(dispatch(element)))
      case TMap(k, v) => TAxiomatic(mapAdt.ref, Seq(dispatch(k), dispatch(v)))
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
      case Void() =>
        ADTFunctionInvocation(None, voidUnit.ref, Nil)
      case LiteralTuple(Seq(t1, t2), Seq(v1, v2)) =>
        ADTFunctionInvocation(
          Some((tupleAdt.ref, Seq(dispatch(t1), dispatch(t2)))),
          tupleTup.ref, Seq(dispatch(v1), dispatch(v2)),
        )
      case TupGet(tup, 0) =>
        ADTFunctionInvocation(
          Some((tupleAdt.ref, tup.t.asTuple.get.elements.map(dispatch))),
          tupleFst.ref, Seq(dispatch(tup)),
        )
      case TupGet(tup, 1) =>
        ADTFunctionInvocation(
          Some((tupleAdt.ref, tup.t.asTuple.get.elements.map(dispatch))),
          tupleSnd.ref, Seq(dispatch(tup)),
        )
      case OptNone() =>
        ADTFunctionInvocation(
          Some((optionAdt.ref, Seq(TAxiomatic(nothingAdt.ref, Nil)))),
          optionNone.ref, Nil,
        )
      case OptSome(element) =>
        val newElement = dispatch(element)
        ADTFunctionInvocation(
          Some((optionAdt.ref, Seq(newElement.t))),
          optionSome.ref, Seq(newElement),
        )
      case access @ OptGet(opt) =>
        FunctionInvocation[Post](optionGet.ref, Seq(dispatch(opt)), Seq(dispatch(opt.t.asOption.get.element)))(
          NoContext(OptionNonePreconditionFailed(access)))
      case get @ OptGetOrElse(opt, alt) =>
        FunctionInvocation[Post](optionGetOrElse.ref,
          Seq(dispatch(opt), dispatch(alt)),
          Seq(dispatch(get.t)),
        )(PanicBlame("opt_or_else requires nothing."))
      case get @ GetLeft(e) =>
        FunctionInvocation[Post](eitherGetLeft.ref,
          Seq(dispatch(e)),
          Seq(dispatch(get.eitherType.left), dispatch(get.eitherType.right)),
        )(NoContext(NotLeftPreconditionFailed(get)))
      case get @ GetRight(e) =>
        FunctionInvocation[Post](eitherGetRight.ref,
          Seq(dispatch(e)),
          Seq(dispatch(get.eitherType.left), dispatch(get.eitherType.right)),
        )(NoContext(NotRightPreconditionFailed(get)))
      case is @ IsLeft(e) =>
        Not(ADTFunctionInvocation(
          Some((
            eitherAdt.ref,
            Seq(dispatch(is.eitherType.left), dispatch(is.eitherType.right))
          )),
          eitherIsRight.ref,
          Seq(dispatch(e)),
        ))
      case is @ IsRight(e) =>
        ADTFunctionInvocation(
          Some((
            eitherAdt.ref,
            Seq(dispatch(is.eitherType.left), dispatch(is.eitherType.right))
          )),
          eitherIsRight.ref,
          Seq(dispatch(e)),
        )
      case LiteralMap(k, v, values) =>
        val typeArgs: Some[(Ref[Post, AxiomaticDataType[Post]], Seq[Type[Post]])] =
          Some((mapAdt.ref, Seq(dispatch(k), dispatch(v))))

        values.foldLeft(
          ADTFunctionInvocation[Post](typeArgs, mapEmpty.ref, Nil)
        )((m, pair) =>
          ADTFunctionInvocation[Post](typeArgs, mapCons.ref, Seq(dispatch(pair._1), dispatch(pair._2), m))
        )
      case MapKeySet(map) =>
        ADTFunctionInvocation(
          Some((mapAdt.ref, mapTypeArgs(map))), mapKeys.ref, Seq(dispatch(map))
        )
      case MapSize(map) =>
        FunctionInvocation[Post](mapSize.ref, Seq(dispatch(map)), mapTypeArgs(map))(PanicBlame("map_size requires nothing."))
      case access @ MapGet(map, k) =>
        FunctionInvocation[Post](mapGet.ref, Seq(dispatch(map), dispatch(k)), mapTypeArgs(map))(
          NoContext(MapKeyErrorPreconditionFailed(access)))
      case MapValueSet(map) =>
        FunctionInvocation[Post](mapValues.ref, Seq(dispatch(map)), mapTypeArgs(map))(PanicBlame("map_values requires nothing."))
      case MapItemSet(map) =>
        FunctionInvocation[Post](mapItems.ref, Seq(dispatch(map)), mapTypeArgs(map))(PanicBlame("map_items requires nothing."))
      case MapEq(left, right) =>
        FunctionInvocation[Post](mapEquals.ref, Seq(dispatch(left), dispatch(right)), ???)(PanicBlame("map_equals requires nothing."))
      case MapDisjoint(left, right) =>
        FunctionInvocation[Post](mapDisjoint.ref, Seq(dispatch(left), dispatch(right)), ???)(PanicBlame("map_disjoint requires nothing."))
      case MapRemove(map, k) =>
        FunctionInvocation[Post](mapRemove.ref, Seq(dispatch(map), dispatch(k)), mapTypeArgs(map))(PanicBlame("map_remove requires nothing."))
      case sub @ ArraySubscript(arr, index) =>
        SilverDeref(
          obj = FunctionInvocation[Post](
            ref = arrayLoc.ref,
            args = Seq(
              FunctionInvocation[Post](optionGet.ref, Seq(dispatch(arr)), Seq(TAxiomatic(arrayAdt.ref, Nil)))(
                NoContext(ArrayNullPreconditionFailed(sub.blame, arr))),
              dispatch(index)),
            typeArgs = Nil)(NoContext(ArrayBoundsPreconditionFailed(sub.blame, index))),
          field = getArrayField(arr))(ArrayFieldInsufficientPermission(sub.blame, sub))
      case length @ Length(arr) =>
        ADTFunctionInvocation(None, arrayLen.ref, Seq(
          FunctionInvocation[Post](optionGet.ref, Seq(dispatch(arr)), Seq(TAxiomatic[Post](arrayAdt.ref, Nil)))(
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
                typeArgs = Seq(TAxiomatic[Post](pointerAdt.ref, Nil)),
              )(NoContext(PointerNullPreconditionFailed(sub.blame, pointer))), dispatch(index)),
              typeArgs = Nil)(NoContext(PointerBoundsPreconditionFailed(sub.blame, index)))),
            typeArgs = Nil,
          )(PanicBlame("ptr_deref requires nothing.")),
          field = getPointerField(pointer),
        )(PointerFieldInsufficientPermission(sub.blame, sub))
      case add @ PointerAdd(pointer, offset) =>
        FunctionInvocation[Post](
          ref = pointerAdd.ref,
          args = Seq(FunctionInvocation[Post](
            ref = optionGet.ref,
            args = Seq(dispatch(pointer)),
            typeArgs = Seq(TAxiomatic[Post](pointerAdt.ref, Nil)),
          )(NoContext(PointerNullPreconditionFailed(add.blame, pointer))), dispatch(offset)),
          typeArgs = Nil,
        )(NoContext(PointerBoundsPreconditionFailed(add.blame, pointer)))
      case deref @ DerefPointer(pointer) =>
        SilverDeref(
          obj = FunctionInvocation[Post](
            ref = pointerDeref.ref,
            args = Seq(FunctionInvocation[Post](
              ref = optionGet.ref,
              args = Seq(dispatch(pointer)),
              typeArgs = Seq(TAxiomatic[Post](pointerAdt.ref, Nil)),
            )(NoContext(PointerNullPreconditionFailed(deref.blame, pointer)))),
            typeArgs = Nil,
          )(PanicBlame("ptr_deref requires nothing.")),
          field = getPointerField(pointer),
        )(PointerFieldInsufficientPermission(deref.blame, deref))
      case other => rewriteDefault(other)
    }
  }

  // PB: dumb hack alert: TVoid and Return(Void()) is (for viper) a marker to indicate that there is no return type.
  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] if method.returnType == TVoid[Pre]() =>
      method.rewrite(returnType = TVoid()).succeedDefault(this, decl)
    case other => rewriteDefault(other)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case ret @ Return(v @ Void()) => ret.rewrite(result=Void()(v.o))
    case other => rewriteDefault(other)
  }
}
