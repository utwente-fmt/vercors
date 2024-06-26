package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast
import vct.col.ast._
import vct.col.origin.{Origin, PanicBlame}
import vct.col.rewrite.EnumToDomain.{EqOptOrigin, EqOrigin, ToIntOrigin}
import vct.col.typerules.CoercingRewriter
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

import scala.collection.mutable

case object EnumToDomain extends RewriterBuilder {
  override def key: String = "enumToDomain"
  override def desc: String = "Encodes COL enum as a domain"

  def EqOrigin(inner: Origin): Origin = inner.where(prefix = "eq")

  def EqOptOrigin(inner: Origin): Origin = inner.where(prefix = "eqOpt")

  def ToIntOrigin(inner: Origin): Origin = inner.where(prefix = "toInt")
}

case class EnumToDomain[Pre <: Generation]() extends CoercingRewriter[Pre] {
  val enumSucc: SuccessionMap[EnumDecl[Pre], AxiomaticDataType[Post]] =
    SuccessionMap()
  val constSucc: SuccessionMap[EnumConstant[Pre], ADTFunction[Post]] =
    SuccessionMap()
  val eqDefs: SuccessionMap[EnumDecl[Pre], ADTFunction[Post]] = SuccessionMap()
  val eqOptDefs: SuccessionMap[EnumDecl[Pre], Function[Post]] = SuccessionMap()

  val oracles: mutable.Map[EnumDecl[Pre], Function[Post]] = mutable.Map()

  val currentEnum: ScopedStack[EnumDecl[Pre]] = ScopedStack()

  def enumEq(
      a: Expr[Post],
      b: Expr[Post],
  )(implicit enumDecl: EnumDecl[Pre], o: Origin): Expr[Post] =
    ADTFunctionInvocation(
      None,
      eqDefs.ref[Post, ADTFunction[Post]](enumDecl),
      Seq(a, b),
    )(o)

  def enumOptEq(
      a: Expr[Post],
      b: Expr[Post],
  )(implicit enumDecl: EnumDecl[Pre], o: Origin): Expr[Post] =
    FunctionInvocation(
      eqOptDefs.ref[Post, Function[Post]](enumDecl),
      Seq(a, b),
      Seq(),
      Seq(),
      Seq(),
    )(PanicBlame("Precondition is trivial"))

  def getConst(c: EnumConstant[Pre])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation(
      None,
      constSucc.ref[Post, ADTFunction[Post]](c),
      Seq(),
    )

  def T(enumDecl: EnumDecl[Pre])(implicit o: Origin): Type[Post] =
    TAxiomatic(enumSucc.ref[Post, AxiomaticDataType[Post]](enumDecl), Seq())

  override def postCoerce(decl: Declaration[Pre]): Unit =
    decl match {
      case enumDecl: EnumDecl[Pre] =>
        implicit val o = enumDecl.o
        implicit val enumImp: EnumDecl[Pre] = enumDecl
        currentEnum.having(enumDecl) {
          aDTDeclarations.scope {
            enumSucc(enumDecl) = globalDeclarations
              .declare(new AxiomaticDataType(
                aDTDeclarations.collect {
                  enumDecl.constants.foreach(dispatch)

                  val eqDef: ADTFunction[Post] = aDTDeclarations.declare(
                    new ADTFunction(
                      Seq(new Variable(T(enumDecl)), new Variable(T(enumDecl))),
                      TBool(),
                    )(EqOrigin(enumDecl.o))
                  )
                  eqDefs(enumDecl) = eqDef
                  val toIntDef: ADTFunction[Post] = aDTDeclarations.declare(
                    new ADTFunction(Seq(new Variable(T(enumDecl))), TInt())(
                      ToIntOrigin(enumDecl.o)
                    )
                  )
                  val toInt: Expr[Post] => Expr[Post] =
                    e =>
                      ADTFunctionInvocation(
                        None,
                        toIntDef.ref[ADTFunction[Post]],
                        Seq(e),
                      )

                  // eqDef
                  aDTDeclarations.declare(new ADTAxiom(foralls[Post](
                    Seq(T(enumDecl), T(enumDecl)),
                    {
                      case Seq(a, b) => (
                        InlinePattern(enumEq(a, b)) === (toInt(a) === toInt(b))
                      )
                    },
                  )))

                  // eqPost
                  aDTDeclarations.declare(new ADTAxiom(foralls[Post](
                    Seq(T(enumDecl), T(enumDecl)),
                    {
                      case Seq(a, b) => (
                        InlinePattern(enumEq(a, b)) ==> (a === b)
                      )
                    },
                  )))

                  /// toIntAlts
                  aDTDeclarations.declare(new ADTAxiom(foldAnd(
                    enumDecl.constants.zipWithIndex.map {
                      case (eConst, i) => (toInt(getConst(eConst)) === const(i))
                    }
                  )))

                  // toIntRange
                  aDTDeclarations.declare(new ADTAxiom(forall[Post](
                    T(enumDecl),
                    e =>
                      (const(0) <= InlinePattern(toInt(e))) &&
                        (toInt(e) < const(enumDecl.constants.length)),
                  )))
                }._1,
                Seq(),
              ))
          }
        }

        // eqOptDef
        val ax = new Variable(TOption(T(enumDecl)))
        val bx = new Variable(TOption(T(enumDecl)))
        eqOptDefs(enumDecl) = globalDeclarations.declare(
          function[Post](
            args = Seq(ax, bx),
            returnType = TBool(),
            body = Some(Select(
              (ax.get === OptNone()) || (bx.get === OptNone()),
              ax.get === bx.get,
              enumEq(
                OptGet(ax.get)(PanicBlame("None check is done")),
                OptGet(bx.get)(PanicBlame("None check is done")),
              ),
            )),
            blame = PanicBlame("Contract should be ok"),
            contractBlame = PanicBlame("Contract should be satisfiable"),
          )(EqOptOrigin(enumDecl.o))
        )

      case const: EnumConstant[Pre] =>
        constSucc(const) = aDTDeclarations
          .declare(new ADTFunction(Seq(), T(currentEnum.top)(const.o))(const.o))
      case _ => rewriteDefault(decl)
    }

  object EqTL {
    def unapply[G](e: Expr[G]): Option[(Expr[G], Type[G])] =
      e match {
        case Eq(a, _) => Some((e, a.t))
        case Neq(a, _) => Some((e, a.t))
        case _ => None
      }
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] =
    e match {
      case EnumUse(_, const) => OptSome(getConst(const.decl)(e.o))(e.o)
      case EqTL(Eq(a, b), TEnum(enumDecl)) =>
        enumOptEq(dispatch(a), dispatch(b))(enumDecl.decl, e.o)
      case EqTL(Neq(a, b), TEnum(enumDecl)) =>
        implicit val o = e.o;
        !enumOptEq(dispatch(a), dispatch(b))(enumDecl.decl, e.o)
      case other => rewriteDefault(other)
    }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case TEnum(enumDecl) => TOption(T(enumDecl.decl)(t.o))(t.o)
      case other => rewriteDefault(other)
    }

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoerceNullEnum(_) => OptNone()(e.o)
      case other => super.applyCoercion(e, other)
    }
}
