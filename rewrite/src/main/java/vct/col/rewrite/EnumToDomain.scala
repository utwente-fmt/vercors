package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast
import vct.col.ast.{ADTAxiom, ADTFunction, ADTFunctionInvocation, Assign, AxiomaticDataType, Block, BooleanValue, Branch, Declaration, Enum, EnumConstant, EnumUse, Expr, Function, FunctionInvocation, Local, Statement, TAxiomatic, TBool, TEnum, Type, UnitAccountedPredicate, Variable}
import vct.col.origin.{BlameUnreachable, Origin, PanicBlame}
import vct.col.rewrite.EnumToDomain.OracleOrigin
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

import scala.collection.mutable

case object EnumToDomain extends RewriterBuilder {
  override def key: String = "enumToDomain"
  override def desc: String = "Encodes COL enum as a domain"

  case class OracleOrigin(inner: Origin) extends Origin {
    override def preferredName: String = "knowledge" + inner.preferredName.capitalize
    override def context: String = inner.context
    override def inlineContext: String = inner.inlineContext
    override def shortPosition: String = inner.shortPosition
  }
}

case class EnumToDomain[Pre <: Generation]() extends Rewriter[Pre] {
  val enumSucc: SuccessionMap[ast.Enum[Pre], AxiomaticDataType[Post]] = SuccessionMap()
  val constSucc: SuccessionMap[EnumConstant[Pre], ADTFunction[Post]] = SuccessionMap()
  val oracles: mutable.Map[ast.Enum[Pre], ast.Function[Post]] = mutable.Map()

  val currentEnum: ScopedStack[ast.Enum[Pre]] = ScopedStack()
  val currentAssign: ScopedStack[Assign[Pre]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case enum: ast.Enum[Pre] =>
      implicit val o = enum.o
      currentEnum.having(enum) {
        aDTDeclarations.scope {
          enumSucc(enum) = globalDeclarations.declare(new AxiomaticDataType(
            aDTDeclarations.collect {
              enum.constants.foreach(dispatch)
            }._1, Seq()))
        }
      }
    case const: EnumConstant[Pre] =>
      constSucc(const) = aDTDeclarations.declare(
        new ADTFunction(Seq(), TAxiomatic(enumSucc.ref[Post, AxiomaticDataType[Post]](currentEnum.top), Seq()))(const.o)
      )
    case _ => rewriteDefault(decl)
  }

  def getOracle(enum: Enum[Pre]): Function[Post] = {
    oracles.getOrElseUpdate(enum, {
      implicit val o = enum.o
      val e = new Variable[Post](TAxiomatic(enumSucc.ref(enum), Seq()))
      globalDeclarations.declare(withResult[Post, Function[Post]](result => function[Post](
        args = Seq(e),
        returnType = TAxiomatic(enumSucc.ref(enum), Seq()),
        ensures = UnitAccountedPredicate(
          (result === e.get) &&
          foldOr(enum.constants.map { someConst =>
            e.get ===
              ADTFunctionInvocation(None, constSucc.ref[Post, ADTFunction[Post]](someConst), Seq())
          })),
        blame = PanicBlame(""),
        contractBlame = PanicBlame("")
      )(OracleOrigin(o))))
    })
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case EnumUse(_, const) =>
      ADTFunctionInvocation(None, constSucc.ref[Post, ADTFunction[Post]](const.decl), Seq())(e.o)
    case other =>
      // TODO: Rework all this with own equality function trigger on that. See: https://github.com/utwente-fmt/vercors/blob/ast-backend-experiment/src/main/universal/res/adt/seq.pvl, seq_equal
      val newOther = rewriteDefault(other)
      e.t match {
        case TEnum(enum) if currentAssign.isEmpty => FunctionInvocation[Post](
          getOracle(enum.decl).ref[Function[Post]], Seq(newOther), Nil, Nil, Nil
      )(PanicBlame("Cannot fail precondition"))(e.o)
        case _ => newOther
      }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case s: Assign[Pre] => currentAssign.having(s) {
      rewriteDefault(s)
    }
    case _ => rewriteDefault(stat)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TEnum(enum) => TAxiomatic(enumSucc.ref(enum.decl), Seq())
    case other => rewriteDefault(other)
  }
}
