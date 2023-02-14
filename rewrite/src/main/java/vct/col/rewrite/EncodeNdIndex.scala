package vct.col.rewrite

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.LazyRef
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

case object EncodeNdIndex extends RewriterBuilder {
  override def key: String = "ndIndex"
  override def desc: String = "Encode the helper for multi-dimensional index expressions (like i*N+j < N*M)"
}

case class EncodeNdIndex[Pre <: Generation]() extends Rewriter[Pre] {
  class IndexAdt(dim: Int) {
    case class IndexAdtOrigin(preferredName: String) extends Origin {
      override def context: String = "At: [generated node for index adt]"
      override def inlineContext: String = "[Generated node for index adt]"
      override def shortPosition: String = "generated"
    }

    implicit val o: Origin = IndexAdtOrigin("i")

    val lengthAdtRef = new LazyRef[Post, AxiomaticDataType[Post]](lengthAdt)
    val lengthType = TAxiomatic[Post](lengthAdtRef, Nil)
    val lengthTypeArg = Some((lengthAdtRef, Nil))

    val mkLength = new ADTFunction[Post](Seq.fill(dim)(new Variable(TInt())), lengthType)(IndexAdtOrigin(s"length${dim}"))
    val mkLengthInvs = Seq.fill(dim)(new ADTFunction[Post](Seq(new Variable(lengthType)(IndexAdtOrigin("len"))), TInt())(IndexAdtOrigin(s"length${dim}_inv")))
    val mkLengthInvAxs = mkLengthInvs.zipWithIndex.map {
      case (inv, i) =>
        val vars = Seq.fill(dim)(new Variable[Post](TInt()))
        val mk = ADTFunctionInvocation[Post](lengthTypeArg, mkLength.ref, vars.map(_.get))
        val unmk = ADTFunctionInvocation[Post](lengthTypeArg, inv.ref, Seq(mk))
        new ADTAxiom(Forall(vars, Seq(Seq(mk)), Eq(unmk, vars(i).get)))
    }

    val lengthVal = new ADTFunction[Post](Seq(new Variable(lengthType)), TInt())(IndexAdtOrigin(s"length${dim}_val"))
    val lengthAx = {
      val vars = Seq.fill(dim)(new Variable[Post](TInt()))
      val mk = ADTFunctionInvocation[Post](lengthTypeArg, mkLength.ref, vars.map(_.get))
      val valOf = ADTFunctionInvocation[Post](lengthTypeArg, lengthVal.ref, Seq(mk))
      val computed = vars.map[Expr[Post]](_.get).reduce(Mult(_, _))
      new ADTAxiom(Forall(vars, Seq(Seq(valOf)), Eq(valOf, computed)))
    }

    val lengthDecls = Seq(mkLength) ++ mkLengthInvs ++ mkLengthInvAxs ++ Seq(lengthVal, lengthAx)
    val lengthAdt: AxiomaticDataType[Post] = new AxiomaticDataType(lengthDecls, Nil)(IndexAdtOrigin(s"Length${dim}"))

    val indexAdtRef = new LazyRef[Post, AxiomaticDataType[Post]](indexAdt)
    val indexType = TAxiomatic(indexAdtRef, Nil)
    val indexTypeArg = Some((indexAdtRef, Nil))

    val mkIndexTs: Seq[Type[Post]] = Seq.fill(dim)(TInt[Post]()) :+ lengthType
    val mkIndex = new ADTFunction[Post](
      args = mkIndexTs.map(new Variable(_)),
      returnType = indexType
    )(IndexAdtOrigin(s"index${dim}"))
    val mkIndexInvs = mkIndexTs.map(t => new ADTFunction[Post](Seq(new Variable(indexType)(IndexAdtOrigin("idx"))), t)(IndexAdtOrigin("index_inv")))
    val mkIndexInvAxs = mkIndexInvs.zipWithIndex.map {
      case (inv, i) =>
        val vars = mkIndexTs.map(t => new Variable[Post](t))
        val mk = ADTFunctionInvocation[Post](indexTypeArg, mkIndex.ref, vars.map(_.get))
        val unmk = ADTFunctionInvocation[Post](indexTypeArg, inv.ref, Seq(mk))
        new ADTAxiom(Forall(vars, Seq(Seq(mk)), Eq(unmk, vars(i).get)))
    }

    val indexVal = new ADTFunction[Post](Seq(new Variable(indexType)), TInt())(IndexAdtOrigin(s"index${dim}_val"))
    val indexAxs = {
      val indexVars = Seq.fill(dim)(new Variable[Post](TInt()))
      val lengthVars = Seq.fill(dim)(new Variable[Post](TInt()))
      val lengthMk = ADTFunctionInvocation[Post](lengthTypeArg, mkLength.ref, lengthVars.map(_.get))
      val valOfLength = ADTFunctionInvocation[Post](lengthTypeArg, lengthVal.ref, Seq(lengthMk))
      val mk = ADTFunctionInvocation[Post](indexTypeArg, mkIndex.ref, indexVars.map(_.get) :+ lengthMk)
      val valOf = ADTFunctionInvocation[Post](indexTypeArg, indexVal.ref, Seq(mk))
      val computed = indexVars.zipWithIndex.map {
        case (indexVar, i) =>
          (indexVar +: lengthVars.drop(i + 1)).map[Expr[Post]](_.get).reduce(Mult(_, _))
      }.reduce(Plus(_, _))

      val axValDef = new ADTAxiom(Forall(indexVars ++ lengthVars, Seq(Seq(valOf)), Eq(valOf, computed)))

      val linearCondition = indexVars.zip(lengthVars).map {
        case (index, length) => const[Post](0) <= index.get && index.get < length.get
      }.reduce(And(_, _))

      val axLinearLess = new ADTAxiom(Forall(indexVars ++ lengthVars, Seq(Seq(valOf)), Implies(linearCondition, valOf < valOfLength)))

      Seq(axValDef, axLinearLess)
    }

    val indexDecls = Seq(mkIndex) ++ mkIndexInvs ++ mkIndexInvAxs ++ Seq(indexVal) ++ indexAxs
    val indexAdt: AxiomaticDataType[Post] = new AxiomaticDataType(indexDecls, Nil)(IndexAdtOrigin(s"Index${dim}"))

    globalDeclarations.declare(lengthAdt)
    globalDeclarations.declare(indexAdt)

    def getLength(dims: Seq[Expr[Post]]): Expr[Post] =
      ADTFunctionInvocation[Post](lengthTypeArg, mkLength.ref, dims)

    def getLengthVal(dims: Seq[Expr[Post]]): Expr[Post] =
      ADTFunctionInvocation[Post](lengthTypeArg, lengthVal.ref, Seq(getLength(dims)))

    def getIndexVal(indices: Seq[Expr[Post]], dims: Seq[Expr[Post]]): Expr[Post] =
      ADTFunctionInvocation[Post](indexTypeArg, indexVal.ref,
        Seq(ADTFunctionInvocation[Post](indexTypeArg, mkIndex.ref, indices :+ getLength(dims))))
  }

  val indexAdt: mutable.Map[Int, IndexAdt] = mutable.Map()
  def getIndexAdt(i: Int): IndexAdt = indexAdt.getOrElseUpdate(i, new IndexAdt(i))

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case NdLength(dims) => getIndexAdt(dims.size).getLengthVal(dims.map(dispatch))
    case NdIndex(indices, dims) => getIndexAdt(indices.size).getIndexVal(indices.map(dispatch), dims.map(dispatch))
    case other => rewriteDefault(other)
  }
}
