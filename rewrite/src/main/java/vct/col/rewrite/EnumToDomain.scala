package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast
import vct.col.ast._
import vct.col.origin.{Origin, PanicBlame, PreferredNameOrigin}
import vct.col.rewrite.EnumToDomain.{EqOptOrigin, EqOrigin, ToIntOrigin}
import vct.col.typerules.CoercingRewriter
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

import scala.collection.mutable

case object EnumToDomain extends RewriterBuilder {
  override def key: String = "enumToDomain"
  override def desc: String = "Encodes COL enum as a domain"

  case class EqOrigin(inner: Origin) extends PreferredNameOrigin {
    val name: String = s"${inner.preferredName}_eq"
  }

  case class EqOptOrigin(inner: Origin) extends PreferredNameOrigin {
    val name: String = s"${inner.preferredName}_eqOpt"
  }

  case class ToIntOrigin(inner: Origin) extends PreferredNameOrigin {
    val name: String = s"${inner.preferredName}_toInt"
  }
}

case class EnumToDomain[Pre <: Generation]() extends CoercingRewriter[Pre] {
  val enumSucc: SuccessionMap[Enum[Pre], AxiomaticDataType[Post]] = SuccessionMap()
  val constSucc: SuccessionMap[EnumConstant[Pre], ADTFunction[Post]] = SuccessionMap()
  val eqDefs: SuccessionMap[Enum[Pre], ADTFunction[Post]] = SuccessionMap()
  val eqOptDefs: SuccessionMap[Enum[Pre], Function[Post]] = SuccessionMap()

  val oracles: mutable.Map[Enum[Pre], Function[Post]] = mutable.Map()

  val currentEnum: ScopedStack[Enum[Pre]] = ScopedStack()

  def enumEq(a: Expr[Post], b: Expr[Post])(implicit enum: Enum[Pre], o: Origin): Expr[Post] =
    ADTFunctionInvocation(None, eqDefs.ref[Post, ADTFunction[Post]](enum), Seq(a, b))(o)

  def enumOptEq(a: Expr[Post], b: Expr[Post])(implicit enum: Enum[Pre], o: Origin): Expr[Post] =
    FunctionInvocation(eqOptDefs.ref[Post, Function[Post]](enum), Seq(a, b), Seq(), Seq(), Seq())(PanicBlame("Precondition is trivial"))

  def getConst(c: EnumConstant[Pre])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation(None, constSucc.ref[Post, ADTFunction[Post]](c), Seq())

  def T(enum: Enum[Pre])(implicit o: Origin): Type[Post] =
    TAxiomatic(enumSucc.ref[Post, AxiomaticDataType[Post]](enum), Seq())

  override def postCoerce(decl: Declaration[Pre]): Unit = decl match {
    case enum: ast.Enum[Pre] =>
      implicit val o = enum.o
      implicit val enumImp: Enum[Pre] = enum
      currentEnum.having(enum) {
        aDTDeclarations.scope {
          enumSucc(enum) = globalDeclarations.declare(new AxiomaticDataType(
            aDTDeclarations.collect {
              enum.constants.foreach(dispatch)

              val eqDef: ADTFunction[Post] = aDTDeclarations.declare(new ADTFunction(Seq(new Variable(T(enum)), new Variable(T(enum))), TBool())(EqOrigin(enum.o)))
              eqDefs(enum) = eqDef
              val toIntDef: ADTFunction[Post] = aDTDeclarations.declare(new ADTFunction(Seq(new Variable(T(enum))), TInt())(ToIntOrigin(enum.o)))
              val toInt: Expr[Post] => Expr[Post] = e => ADTFunctionInvocation(None, toIntDef.ref[ADTFunction[Post]], Seq(e))

              // eqDef
              aDTDeclarations.declare(new ADTAxiom(
                foralls[Post](Seq(T(enum), T(enum)), { case Seq(a, b) =>
                  (InlinePattern(enumEq(a, b))
                  ===
                  (toInt(a) === toInt(b)))
                })))

              // eqPost
              aDTDeclarations.declare(new ADTAxiom(
                foralls[Post](Seq(T(enum), T(enum)), { case Seq(a, b) =>
                  (InlinePattern(enumEq(a, b))
                    ==>
                    (a === b))
                })))

              /// toIntAlts
              aDTDeclarations.declare(new ADTAxiom(
                foldAnd(
                  enum.constants.zipWithIndex.map { case (eConst, i) =>
                    (toInt(getConst(eConst)) === const(i))
                  }
                )
              ))

              // toIntRange
              aDTDeclarations.declare(new ADTAxiom(
                forall[Post](T(enum), e => (const(0) <= InlinePattern(toInt(e))) && (toInt(e) < const(enum.constants.length)))
              ))
            }._1, Seq()))
        }
      }

      // eqOptDef
      val ax = new Variable(TOption(T(enum)))
      val bx = new Variable(TOption(T(enum)))
      eqOptDefs(enum) = globalDeclarations.declare(function[Post](
        args = Seq(ax, bx),
        returnType = TBool(),
        body = Some(Select(
          (ax.get === OptNone()) || (bx.get === OptNone()),
          ax.get === bx.get,
          enumEq(OptGet(ax.get)(PanicBlame("None check is done")), OptGet(bx.get)(PanicBlame("None check is done"))))),
        blame = PanicBlame("Contract should be ok"),
        contractBlame = PanicBlame("Contract should be satisfiable")
      )(EqOptOrigin(enum.o)))

    case const: EnumConstant[Pre] =>
      constSucc(const) = aDTDeclarations.declare(new ADTFunction(Seq(), T(currentEnum.top)(const.o))(const.o))
    case _ => rewriteDefault(decl)
  }

  object EqTL {
    def unapply[G](e: Expr[G]): Option[(Expr[G], Type[G])] = e match {
      case Eq(a, _) => Some((e, a.t))
      case Neq(a, _) => Some((e, a.t))
      case _ => None
    }
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = e match {
    case EnumUse(_, const) => OptSome(getConst(const.decl)(e.o))(e.o)
    case EqTL(Eq(a, b), TEnum(enum)) => enumOptEq(dispatch(a), dispatch(b))(enum.decl, e.o)
    case EqTL(Neq(a, b), TEnum(enum)) => implicit val o = e.o; !enumOptEq(dispatch(a), dispatch(b))(enum.decl, e.o)
    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TEnum(enum) => TOption(T(enum.decl)(t.o))(t.o)
    case other => rewriteDefault(other)
  }

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = coercion match {
    case CoerceNullEnum(_) =>
      OptNone()(e.o)
    case other => super.applyCoercion(e, other)
  }
}
