package vct.col.rewrite.adt

import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite._
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

case object ImportVector extends ImportADTBuilder("vector") {
  case class VectorBoundsPreconditionFailed(inner: Blame[VectorBoundFailure], subscript: VectorSubscript[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit = error.path match {
      case FailLeft +: _ => inner.blame(VectorBoundNegative(subscript))
      case FailRight +: _ => inner.blame(VectorBoundExceedsLength(subscript))
      case _ => ???
    }
  }
}

case class ImportVector[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  import ImportVector._
  private lazy val vectorFile = parse("vector")
  private lazy val vectorAdt = find[AxiomaticDataType[Post]](vectorFile, "vector")
  private lazy val vector_loc = find[ADTFunction[Post]](vectorAdt, "vector_loc")

  val makeVectorMethods: mutable.Map[BigInt, Function[Post]] = mutable.Map()

  val vectorOpsMethods: mutable.Map[(BigInt, String, Type[Post]), Function[Post]] = mutable.Map()


  val vLocMethods: mutable.Map[BigInt, Function[Post]] = mutable.Map()

  def vlocMake(size: BigInt): Function[Post] = {
    implicit val o: Origin = Origin(Seq(LabelContext("vector access method")))
    /* for instance for size 4:
     decreases;
     requires 0 <= i;
     requires i < 4;
    pure T v4loc<T>(`vector`<T> a, int i) = `vector`<T>.vector_loc(a, i);
    */
    globalDeclarations.declare(withResult((result: Result[Post]) => {
      val elementTypeVar = new Variable[Post](TType(TAnyValue()))(o.where(name= "T"))
      val elementType = TVar[Post](elementTypeVar.ref)
      val tVec = TAxiomatic[Post](vectorAdt.ref, Seq(elementType))

      val a = new Variable[Post](tVec)(o.where(name= "a"))
      val i = new Variable[Post](TInt())(o.where(name= "i"))

      val requires: Seq[Expr[Post]] = Seq(const[Post](0) <= i.get, i.get < const[Post](size))
      val body = ADTFunctionInvocation[Post](Some((vectorAdt.ref, Seq(elementType))), vector_loc.ref, Seq(a.get, i.get))

      function(
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        typeArgs = Seq(elementTypeVar),
        body = Some(body),
        returnType = elementType,
        args = Seq(a, i),
        requires = SplitAccountedPredicate(UnitAccountedPredicate(requires(0)), UnitAccountedPredicate(requires(1))),
        ensures = UnitAccountedPredicate(tt)
      )(o.where(name= "v" + size.toString + "loc"))
    }))
  }

  def makeVectorCreation(size: BigInt): Function[Post] = {
    implicit val o: Origin = Origin(Seq(LabelContext("vector creation method")))
    /* for instance for size 4:
     decreases;
     ensures vloc<T>(\result, 0) == x0;
     ensures vloc<T>(\result, 1) == x1;
     ensures vloc<T>(\result, 2) == x2;
     ensures vloc<T>(\result, 3) == x3;
    pure `vector`<T> make_v4<T>(T x0, T x1, T x2, T x3);
    */
    globalDeclarations.declare(withResult((result: Result[Post]) => {
      val elementTypeVar = new Variable[Post](TType(TAnyValue()))(o.where(name= "T"))
      val elementType = TVar[Post](elementTypeVar.ref)

      val xs = Seq.tabulate(size.toInt)(x => new Variable[Post](elementType)(o.where(name= f"x$x")))
      val ensures: Seq[Expr[Post]] = xs.zipWithIndex.map({case (x, i) => Eq(
        makeSubScript(result, const[Post](i), elementType, size, PanicBlame("Cannot Fail"), o), x.get)})

      function(
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        typeArgs = Seq(elementTypeVar),
        returnType = TAxiomatic[Post](vectorAdt.ref, Seq(elementType)),
        args = xs,
        requires = UnitAccountedPredicate(tt),
        ensures = UnitAccountedPredicate(foldAnd(ensures))
      )(o.where(name= "make_v" + size.toString))
    }))
  }

  def makeVectorOp(size: BigInt, elementType: Type[Post], operator: (Expr[Post], Expr[Post]) => BinExpr[Post], operatorName: String ): Function[Post] = {
    implicit val o: Origin = Origin(Seq(LabelContext("vector creation method")))
    /* for instance for size 4:
     decreases;
     ensures vloc<T>(\result, 0) == vloc<T>(x, 0) `op` vloc<T>(y, 0);
     ensures vloc<T>(\result, 1) == vloc<T>(x, 1) `op` vloc<T>(y, 1);
     ensures vloc<T>(\result, 2) == vloc<T>(x, 2) `op` vloc<T>(y, 2);
     ensures vloc<T>(\result, 3) == vloc<T>(x, 3) `op` vloc<T>(y, 3);
    pure `vector`<T> op<T>(`vector`<T> x, `vector`<T> y);
    */
    globalDeclarations.declare(withResult((result: Result[Post]) => {
      val vectorType = TAxiomatic[Post](vectorAdt.ref, Seq(elementType))

      val x = new Variable[Post](vectorType)(o.where(name= f"x"))
      val y = new Variable[Post](vectorType)(o.where(name= f"y"))
      val ensures: Seq[Expr[Post]] = Seq.tabulate(size.toInt)(i => Eq(
        makeSubScript(result, const[Post](i), elementType, size, PanicBlame("Cannot Fail"), o), // \result[i]
          operator(
            makeSubScript(x.get, const[Post](i), elementType, size, PanicBlame("Cannot Fail"), o), // x[i]
            makeSubScript(y.get, const[Post](i), elementType, size, PanicBlame("Cannot Fail"), o)  // y[i]
          )
      ))

      function(
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        typeArgs = Nil,
        returnType = TAxiomatic[Post](vectorAdt.ref, Seq(elementType)),
        args = Seq(x, y),
        requires = UnitAccountedPredicate(tt),
        ensures = UnitAccountedPredicate(foldAnd(ensures))
      )(o.where(name= f"${operatorName}_v" + size.toString))
    }))
  }

  override def postCoerce(t: Type[Pre]): Type[Post] = t match {
    case TVector(size, inner) => TAxiomatic[Post](vectorAdt.ref, Seq(dispatch(inner)))(t.o)
    case other => other.rewriteDefault()
  }

  def typeArgs(xs: Expr[Pre]): Option[(Ref[Post, AxiomaticDataType[Post]], Seq[Type[Post]])] =
    typeArgs(xs.t.asVector.get.element)

  def typeArgs(t: Type[Pre]): Option[(Ref[Post, AxiomaticDataType[Post]], Seq[Type[Post]])] =
    Some((vectorAdt.ref, Seq(dispatch(t))))

  def makeSubScript(vec: Expr[Post], index: Expr[Post], elementType: Type[Post], size: BigInt, blame: Blame[InvocationFailure], o: Origin) = {
    val vloc = vLocMethods.getOrElseUpdate(size, vlocMake(size))

    FunctionInvocation[Post](
      typeArgs = Seq(elementType),
      ref = vloc.ref,
      args = Seq(vec, index),
      givenMap = Nil,
      yields = Nil)(blame)(o)
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = e match {
    case LiteralVector(element, xs) =>
      val makeVector = makeVectorMethods.getOrElseUpdate(xs.size, makeVectorCreation(xs.size))

      FunctionInvocation[Post](
        typeArgs = Seq(dispatch(element)),
        ref = makeVector.ref,
        args = xs.map[Expr[Post]](dispatch),
        givenMap = Nil,
        yields = Nil
      )(PanicBlame("No requires"))(e.o)
    case sub@VectorSubscript(vec, index) =>
      val (size, elementT) = vec.t match {
        case TVector(size, inner) => (size, inner)
        case _ => ???
      }
      makeSubScript(dispatch(vec), dispatch(index), dispatch(elementT), size,
        NoContext(VectorBoundsPreconditionFailed(sub.blame, sub)), e.o)
    case e: VectorBinExpr[Pre] =>
      val (size, elementTold) = e.t match {
        case TVector(size, inner) => (size, inner)
        case _ => ???
      }
      val elementT = dispatch(elementTold)


      val vectorOpFunction =
        e match {
          case _: VectorMult[Pre] =>
            val o: Origin = Origin(Seq(LabelContext("vector mult method")))
            vectorOpsMethods.getOrElseUpdate((size, "mult", elementT), makeVectorOp(size, elementT, (l,r) => Mult[Post](l, r)(o), "mult"))
          case _: VectorPlus[Pre] =>
            val o: Origin = Origin(Seq(LabelContext("vector plus method")))
            vectorOpsMethods.getOrElseUpdate((size, "plus", elementT), makeVectorOp(size, elementT, (l,r) => Plus[Post](l, r)(o), "plus"))
          case _: VectorMinus[Pre] =>
            val o: Origin = Origin(Seq(LabelContext("vector minus method")))
            vectorOpsMethods.getOrElseUpdate((size, "minus", elementT), makeVectorOp(size, elementT, (l,r) => Minus[Post](l, r)(o), "minus"))
        }

      FunctionInvocation[Post](
        typeArgs = Nil,
        ref = vectorOpFunction.ref,
        args = Seq(dispatch(e.left), dispatch(e.right)),
        givenMap = Nil,
        yields = Nil
      )(PanicBlame("No requires"))(e.o)

    case other => other.rewriteDefault()
  }
}
