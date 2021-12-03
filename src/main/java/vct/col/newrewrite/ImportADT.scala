package vct.col.newrewrite

import hre.config.Configuration
import hre.util.ScopedStack
import vct.col.coerce.Coercion.{FracZFrac, NullArray, NullPointer, ZFracRat}
import vct.col.ast._
import vct.col.newrewrite.ImportADT.{ArrayBoundsPreconditionFailed, ArrayField, ArrayFieldInsufficientPermission, ArrayNullPreconditionFailed, InvalidImportedAdt, MapKeyErrorPreconditionFailed, NotLeftPreconditionFailed, NotRightPreconditionFailed, OptionNonePreconditionFailed, PointerBoundsPreconditionFailed, PointerField, PointerFieldInsufficientPermission, PointerNullPreconditionFailed, RatZFracPreconditionFailed, ZFracFracPreconditionFailed}
import vct.col.newrewrite.error.{ExcludedByPassOrder, ExtraNode}
import vct.parsers.Parsers
import RewriteHelpers._
import vct.col.check.CheckError
import vct.col.coerce.{CoercingRewriter, Coercion}
import vct.col.newrewrite.lang.{LangSpecificToCol, LangTypesToCol}
import vct.col.origin._
import vct.col.resolve.{ResolveReferences, ResolveTypes}
import vct.result.VerificationResult.UserError

import scala.collection.mutable
import scala.reflect.ClassTag

case object ImportADT {
  private def typeText(t: Type): String = t match {
    case _: ExtraType => throw ExtraNode
    case TNotAValue() => throw ExtraNode
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
  }

  case class InvalidImportedAdt(errors: Seq[CheckError]) extends UserError {
    override def code: String = "invalidAdt"
    override def text: String =
      "Errors were encountered while importing the definition of an internal axiomatic datatype:\n" +
        errors.map(_.toString).mkString("\n")
  }

  case class ArrayField(t: Type) extends Origin {
    override def preferredName: String = typeText(t)
    override def messageInContext(message:  String): String =
      s"At field generated for array location of type $t: $message"
  }

  case class PointerField(t: Type) extends Origin {
    override def preferredName: String = typeText(t)
    override def messageInContext(message:  String): String =
      s"At field generated for pointer location of type $t: $message"
  }

  case class OptionNonePreconditionFailed(access: OptGet) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      access.blame.blame(OptionNone(access))
  }

  case class MapKeyErrorPreconditionFailed(access: MapGet) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      access.blame.blame(MapKeyError(access))
  }

  case class ArrayNullPreconditionFailed(inner: Blame[ArrayNull], expr: Expr) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(ArrayNull(expr))
  }

  case class ArrayBoundsPreconditionFailed(inner: Blame[ArrayBounds], idx: Expr) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(ArrayBounds(idx))
  }

  case class ArrayFieldInsufficientPermission(inner: Blame[ArrayInsufficientPermission], expr: Expr) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit =
      inner.blame(ArrayInsufficientPermission(expr))
  }

  case class PointerNullPreconditionFailed(inner: Blame[PointerNull], expr: Expr) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(PointerNull(expr))
  }

  case class PointerBoundsPreconditionFailed(inner: Blame[PointerBounds], expr: Expr) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(PointerBounds(expr))
  }

  case class PointerFieldInsufficientPermission(inner: Blame[PointerInsufficientPermission], expr: Expr) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit =
      inner.blame(PointerInsufficientPermission(expr))
  }

  case class RatZFracPreconditionFailed(inner: Blame[CoerceRatZFracFailed], expr: Expr) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(CoerceRatZFracFailed(expr))
  }

  case class RatFracPreconditionFailed(inner: Blame[CoerceRatFracFailed], expr: Expr) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(CoerceRatFracFailed(expr))
  }

  case class ZFracFracPreconditionFailed(inner: Blame[CoerceZFracFracFailed], expr: Expr) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(CoerceZFracFracFailed(expr))
  }

  case class NotLeftPreconditionFailed(get: GetLeft) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      get.blame.blame(NotLeft(get))
  }

  case class NotRightPreconditionFailed(get: GetRight) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      get.blame.blame(NotRight(get))
  }
}

case class ImportADT() extends CoercingRewriter {
  val arrayField: mutable.Map[Type, SilverField] = mutable.Map()
  val pointerField: mutable.Map[Type, SilverField] = mutable.Map()

  private def parse(name: String): Seq[GlobalDeclaration] = {
    val decls = Parsers.parse(Configuration.getAdtFile(s"$name.pvl").toPath).decls
    val moreDecls = ResolveTypes.resolve(Program(decls)(DiagnosticOrigin)(DiagnosticOrigin))
    val typedProgram = LangTypesToCol().dispatch(Program(decls ++ moreDecls)(DiagnosticOrigin)(DiagnosticOrigin))
    val errors = ResolveReferences.resolve(typedProgram)
    if(errors.nonEmpty) throw InvalidImportedAdt(errors)
    val program = LangSpecificToCol().dispatch(typedProgram)
    program.declarations.foreach(dispatch)
    program.declarations.map(successionMap(_).asInstanceOf[GlobalDeclaration])
  }

  private lazy val fracFile = parse("frac")
  private lazy val zfracFile = parse("zfrac")
  private lazy val tupleFile = parse("tuple")
  private lazy val optionFile = parse("option")
  private lazy val eitherFile = parse("either")
  private lazy val mapFile = parse("map")
  private lazy val arrayFile = parse("array")
  private lazy val pointerFile = parse("pointer")

  def find[T](decls: Seq[Declaration], name: String)(implicit tag: ClassTag[T]): T =
    decls.collectFirst {
      case decl: T if decl.o.isInstanceOf[SourceNameOrigin] && decl.o.asInstanceOf[SourceNameOrigin].name == name =>
        decl
    }.get

  def find[T](decls: Declarator, name: String)(implicit tag: ClassTag[T]): T =
    find(decls.declarations, name)(tag)

  private lazy val fracAdt = find[AxiomaticDataType](fracFile, "frac")
  private lazy val fracVal = find[ADTFunction](fracAdt, "frac_val")
  private lazy val fracNew = find[Function](fracFile, "new_frac")

  private lazy val zfracAdt = find[AxiomaticDataType](zfracFile, "zfrac")
  private lazy val zfracVal = find[ADTFunction](zfracAdt, "zfrac_val")
  private lazy val zfracNew = find[Function](zfracFile, "new_zfrac")

  private lazy val tupleAdt = find[AxiomaticDataType](tupleFile, "tuple")
  private lazy val tupleTup = find[ADTFunction](tupleAdt, "tup")
  private lazy val tupleFst = find[ADTFunction](tupleAdt, "fst")
  private lazy val tupleSnd = find[ADTFunction](tupleAdt, "snd")

  private lazy val optionAdt = find[AxiomaticDataType](optionFile, "option")
  private lazy val optionNone = find[ADTFunction](optionAdt, "None")
  private lazy val optionSome = find[ADTFunction](optionAdt, "some")
  private lazy val optionAxGet = find[ADTFunction](optionAdt, "option_get")
  private lazy val optionGet = find[Function](optionFile, "opt_get")
  private lazy val optionGetOrElse = find[Function](optionFile, "opt_or_else")

  private lazy val eitherAdt = find[AxiomaticDataType](eitherFile, "either")
  private lazy val eitherLeft = find[ADTFunction](eitherAdt, "left")
  private lazy val eitherRight = find[ADTFunction](eitherAdt, "right")
  private lazy val eitherIsRight = find[ADTFunction](eitherAdt, "is_right")
  private lazy val eitherAxGetLeft = find[ADTFunction](eitherAdt, "either_get_left")
  private lazy val eitherAxGetRight = find[ADTFunction](eitherAdt, "either_get_right")
  private lazy val eitherGetLeft = find[Function](eitherFile, "get_left")
  private lazy val eitherGetRight = find[Function](eitherFile, "get_right")

  private lazy val mapAdt = find[AxiomaticDataType](mapFile, "map")
  private lazy val mapEmpty = find[ADTFunction](mapAdt, "map_empty")
  private lazy val mapCons = find[ADTFunction](mapAdt, "map_cons")
  private lazy val mapKeys = find[ADTFunction](mapAdt, "map_keys")
  private lazy val mapAxConsInvK = find[ADTFunction](mapAdt, "map_cons_inv_k")
  private lazy val mapAxConsInvV = find[ADTFunction](mapAdt, "map_cons_inv_v")
  private lazy val mapAxConsInvTail = find[ADTFunction](mapAdt, "map_cons_inv_tail")
  private lazy val mapSize = find[Function](mapFile, "map_size")
  private lazy val mapConsGetK = find[Function](mapFile, "map_cons_get_k")
  private lazy val mapConsGetV = find[Function](mapFile, "map_cons_get_v")
  private lazy val mapConsGetTail = find[Function](mapFile, "map_cons_get_tail")
  private lazy val mapGet = find[Function](mapFile, "map_get")
  private lazy val mapValues = find[Function](mapFile, "map_values")
  private lazy val mapItems = find[Function](mapFile, "map_items")
  private lazy val mapEquals = find[Function](mapFile, "map_equals")
  private lazy val mapDisjoint = find[Function](mapFile, "map_disjoint")
  private lazy val mapRemove = find[Function](mapFile, "map_remove")

  private lazy val arrayAdt = find[AxiomaticDataType](arrayFile, "array")
  private lazy val arrayAxLoc = find[ADTFunction](arrayAdt, "array_loc")
  private lazy val arrayLen = find[ADTFunction](arrayAdt, "alen")
  private lazy val arrayLoc = find[Function](arrayFile, "aloc")

  private lazy val blockAdt = find[AxiomaticDataType](pointerFile, "block")
  private lazy val blockBase = find[ADTFunction](blockAdt, "base_addr")
  private lazy val blockLength = find[ADTFunction](blockAdt, "block_length")
  private lazy val blockLoc = find[ADTFunction](blockAdt, "loc")
  private lazy val pointerAdt = find[AxiomaticDataType](pointerFile, "pointer")
  private lazy val pointerOf = find[ADTFunction](pointerAdt, "pointer_of")
  private lazy val pointerBlock = find[ADTFunction](pointerAdt, "pointer_block")
  private lazy val pointerOffset = find[ADTFunction](pointerAdt, "pointer_offset")
  private lazy val pointerDeref = find[Function](pointerFile, "ptr_deref")
  private lazy val pointerAdd = find[Function](pointerFile, "ptr_add")

  val globalBlame: ScopedStack[Blame[UnsafeCoercion]] = ScopedStack()

  override def coerce(e: Expr, coercion: Coercion)(implicit o: Origin): Expr = coercion match {
    case Coercion.NullArray(_) => OptNone()
    case Coercion.NullPointer(_) => OptNone()

    case Coercion.ZFracRat =>
      ADTFunctionInvocation(Some((zfracAdt.ref, Nil)), zfracVal.ref, Seq(e))
    case Coercion.Compose(Coercion.ZFracRat, Coercion.FracZFrac) =>
      ADTFunctionInvocation(Some((fracAdt.ref, Nil)), fracVal.ref, Seq(e))
    case Coercion.FracZFrac =>
      val rat = ADTFunctionInvocation(Some((fracAdt.ref, Nil)), fracVal.ref, Seq(e))
      FunctionInvocation(zfracNew.ref, Seq(rat), Nil)(PanicBlame("a frac always fits in a zfrac."))

    case Coercion.RatZFrac =>
      FunctionInvocation(zfracNew.ref, Seq(e), Nil)(RatZFracPreconditionFailed(globalBlame.head, e))
    case Coercion.Compose(Coercion.ZFracFrac, Coercion.RatZFrac) =>
      FunctionInvocation(fracNew.ref, Seq(e), Nil)(RatZFracPreconditionFailed(globalBlame.head, e))
    case Coercion.ZFracFrac =>
      val rat = ADTFunctionInvocation(Some((zfracAdt.ref, Nil)), zfracVal.ref, Seq(e))
      FunctionInvocation(fracNew.ref, Seq(rat), Nil)(ZFracFracPreconditionFailed(globalBlame.head, e))

    case _ => super.coerce(e, coercion)
  }

  override def dispatch(program: Program): Program =
    globalBlame.having(program.blame) {
      program.rewrite()
    }

  override def dispatch(t: Type): Type = {
    implicit val o: Origin = t.o
    t match {
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

  private def mapTypeArgs(map: Expr): Seq[Type] = {
    val mapType = map.t.asMap.get
    Seq(dispatch(mapType.key), dispatch(mapType.value))
  }

  private def getArrayField(arr: Expr): SilverField = {
    val tElement = dispatch(arr.t.asArray.get.element)
    arrayField.getOrElseUpdate(tElement, {
      val field = new SilverField(tElement)(ArrayField(tElement))
      field.declareDefault(this)
      field
    })
  }

  private def getPointerField(ptr: Expr): SilverField = {
    val tElement = dispatch(ptr.t.asPointer.get.element)
    pointerField.getOrElseUpdate(tElement, {
      val field = new SilverField(tElement)(PointerField(tElement))
      field.declareDefault(this)
      field
    })
  }

  override def postCoerce(e: Expr): Expr = {
    implicit val o: Origin = e.o

    e match {
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
          Some((optionAdt.ref, Seq(TNothing()))),
          optionNone.ref, Nil,
        )
      case OptSome(element) =>
        val newElement = dispatch(element)
        ADTFunctionInvocation(
          Some((optionAdt.ref, Seq(newElement.t))),
          optionSome.ref, Seq(newElement),
        )
      case access @ OptGet(opt) =>
        FunctionInvocation(optionGet.ref, Seq(dispatch(opt)), Seq(dispatch(opt.t.asOption.get.element)))(
          OptionNonePreconditionFailed(access))
      case get @ OptGetOrElse(opt, alt) =>
        FunctionInvocation(optionGetOrElse.ref,
          Seq(dispatch(opt), dispatch(alt)),
          Seq(dispatch(get.t)),
        )(PanicBlame("opt_or_else requires nothing."))
      case get @ GetLeft(e) =>
        FunctionInvocation(eitherGetLeft.ref,
          Seq(dispatch(e)),
          Seq(dispatch(get.eitherType.left), dispatch(get.eitherType.right)),
        )(NotLeftPreconditionFailed(get))
      case get @ GetRight(e) =>
        FunctionInvocation(eitherGetRight.ref,
          Seq(dispatch(e)),
          Seq(dispatch(get.eitherType.left), dispatch(get.eitherType.right)),
        )(NotRightPreconditionFailed(get))
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
        val typeArgs = Some((mapAdt.ref[AxiomaticDataType], Seq(dispatch(k), dispatch(v))))
        values.foldLeft(
          ADTFunctionInvocation(typeArgs, mapEmpty.ref, Nil)
        )((m, pair) =>
          ADTFunctionInvocation(typeArgs, mapCons.ref, Seq(dispatch(pair._1), dispatch(pair._2), m))
        )
      case MapKeySet(map) =>
        ADTFunctionInvocation(
          Some((mapAdt.ref, mapTypeArgs(map))), mapKeys.ref, Seq(dispatch(map))
        )
      case MapSize(map) =>
        FunctionInvocation(mapSize.ref, Seq(dispatch(map)), mapTypeArgs(map))(PanicBlame("map_size requires nothing."))
      case access @ MapGet(map, k) =>
        FunctionInvocation(mapGet.ref, Seq(dispatch(map), dispatch(k)), mapTypeArgs(map))(
          MapKeyErrorPreconditionFailed(access))
      case MapValueSet(map) =>
        FunctionInvocation(mapValues.ref, Seq(dispatch(map)), mapTypeArgs(map))(PanicBlame("map_values requires nothing."))
      case MapItemSet(map) =>
        FunctionInvocation(mapItems.ref, Seq(dispatch(map)), mapTypeArgs(map))(PanicBlame("map_items requires nothing."))
      case MapEq(left, right) =>
        FunctionInvocation(mapEquals.ref, Seq(dispatch(left), dispatch(right)), ???)(PanicBlame("map_equals requires nothing."))
      case MapDisjoint(left, right) =>
        FunctionInvocation(mapDisjoint.ref, Seq(dispatch(left), dispatch(right)), ???)(PanicBlame("map_disjoint requires nothing."))
      case MapRemove(map, k) =>
        FunctionInvocation(mapRemove.ref, Seq(dispatch(map), dispatch(k)), mapTypeArgs(map))(PanicBlame("map_remove requires nothing."))
      case sub @ ArraySubscript(arr, index) =>
        SilverDeref(
          obj = FunctionInvocation(
            ref = arrayLoc.ref,
            args = Seq(
              FunctionInvocation(optionGet.ref, Seq(dispatch(arr)), Seq(TAxiomatic(arrayAdt.ref, Nil)))(
                ArrayNullPreconditionFailed(sub.blame, arr)),
              dispatch(index)),
            typeArgs = Nil)(ArrayBoundsPreconditionFailed(sub.blame, index)),
          field = getArrayField(arr).ref)(ArrayFieldInsufficientPermission(sub.blame, sub))
      case length @ Length(arr) =>
        ADTFunctionInvocation(None, arrayLen.ref, Seq(
          FunctionInvocation(optionGet.ref, Seq(dispatch(arr)), Seq(TAxiomatic(arrayAdt.ref, Nil)))(
            ArrayNullPreconditionFailed(length.blame, length))
        ))
      case sub @ PointerSubscript(pointer, index) =>
        SilverDeref(
          obj = FunctionInvocation(
            ref = pointerDeref.ref,
            args = Seq(FunctionInvocation(
              ref = pointerAdd.ref,
              args = Seq(FunctionInvocation(
                ref = optionGet.ref,
                args = Seq(dispatch(pointer)),
                typeArgs = Seq(TAxiomatic(pointerAdt.ref, Nil)),
              )(PointerNullPreconditionFailed(sub.blame, pointer)), dispatch(index)),
              typeArgs = Nil)(PointerBoundsPreconditionFailed(sub.blame, index))),
            typeArgs = Nil,
          )(PanicBlame("ptr_deref requires nothing.")),
          field = getPointerField(pointer).ref,
        )(PointerFieldInsufficientPermission(sub.blame, sub))
      case add @ PointerAdd(pointer, offset) =>
        FunctionInvocation(
          ref = pointerAdd.ref,
          args = Seq(FunctionInvocation(
            ref = optionGet.ref,
            args = Seq(dispatch(pointer)),
            typeArgs = Seq(TAxiomatic(pointerAdt.ref, Nil)),
          )(PointerNullPreconditionFailed(add.blame, pointer)), dispatch(offset)),
          typeArgs = Nil,
        )(PointerBoundsPreconditionFailed(add.blame, pointer))
      case deref @ DerefPointer(pointer) =>
        SilverDeref(
          obj = FunctionInvocation(
            ref = pointerDeref.ref,
            args = Seq(FunctionInvocation(
              ref = optionGet.ref,
              args = Seq(dispatch(pointer)),
              typeArgs = Seq(TAxiomatic(pointerAdt.ref, Nil)),
            )(PointerNullPreconditionFailed(deref.blame, pointer))),
            typeArgs = Nil,
          )(PanicBlame("ptr_deref requires nothing.")),
          field = getPointerField(pointer).ref,
        )(PointerFieldInsufficientPermission(deref.blame, deref))
      case other => rewriteDefault(other)
    }
  }
}
