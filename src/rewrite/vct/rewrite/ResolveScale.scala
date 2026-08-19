package vct.col.rewrite

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.ResolveScale.{
  CheckScale,
  ScaleNegativePreconditionFailed,
  Scaling,
  WrongScale,
}
import vct.col.origin.{
  Blame,
  DiagnosticOrigin,
  LabelContext,
  NoContext,
  Origin,
  PanicBlame,
  PreconditionFailed,
  PreferredName,
  ScaleNegative,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.col.ast.util.{AnnotationVariableInfoGetter, ExpressionEqualityCheck}
import vct.result.VerificationError.UserError

case object ResolveScale extends RewriterBuilder {
  override def key: String = "scale"
  override def desc: String =
    "Inline the scale operator into resource expressions."

  case class WrongScale(scale: Expr[_]) extends UserError {
    override def code: String = "wrongScale"
    override def text: String =
      scale.o.messageInContext("This kind of expression cannot be scaled.")
  }

  private def CheckScale(preferredName: String = "unknown"): Origin =
    Origin(Seq(PreferredName(Seq(preferredName)), LabelContext("scale check")))

  case class ScaleNegativePreconditionFailed(scale: Scale[_])
      extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      scale.blame.blame(ScaleNegative(scale))
  }

  case class Scaling[G](scale: Expr[G], isPositive: Boolean)
}

case class ResolveScale[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  private var equalityChecker: ExpressionEqualityCheck[Pre] =
    ExpressionEqualityCheck()
  private val infoGetter: AnnotationVariableInfoGetter[Pre] =
    new AnnotationVariableInfoGetter[Pre]()
  private val scalings: ScopedStack[Scaling[Post]] = ScopedStack()
  var gaveWarning = false

  lazy val checkScaleFunc: Function[Post] = {
    implicit val o: Origin = CheckScale()

    val v = new Variable[Post](TRational())(CheckScale("amount"))

    globalDeclarations.declare(withResult((result: Result[Post]) => {
      function[Post](
        blame = PanicBlame("scale ensures nothing"),
        contractBlame = PanicBlame("scale only requires a positive rational"),
        args = Seq(v),
        returnType = TRational(),
        body = Some(v.get),
        requires = UnitAccountedPredicate(v.get >= const(0)),
        ensures = UnitAccountedPredicate(result >= const(0)),
      )(CheckScale("scale"))
    }))
  }

  def scaleValue(e: Scale[Pre]): Scaling[Post] = {
    var isNonNegative: Boolean = false
    var isPositive: Boolean = false

    e.scale match {
      case RatDiv(IntegerValue(i), x) if i >= 1 =>
        if (equalityChecker.lessThenEq(const(1)(e.o), x).getOrElse(false)) {
          isNonNegative = true
          isPositive = true
        }
      case e =>
        isNonNegative = equalityChecker.lessThenEq(const(0)(e.o), e)
          .getOrElse(false)
        isPositive = equalityChecker.lessThenEq(const(1)(e.o), e)
          .getOrElse(false)
    }
    var newScale = dispatch(e.scale)
    // When we do not know if it can be negative, we should check
    if (!isNonNegative) {
      newScale =
        FunctionInvocation(
          checkScaleFunc.ref[Function[Post]],
          Seq(newScale),
          Nil,
          Nil,
          Nil,
        )(NoContext(ScaleNegativePreconditionFailed(e)))(e.scale.o)
    }
    Scaling(newScale, isPositive)
  }

  def multiplyScales(): Expr[Post] = {
    val remaining = scalings.toSeq.filter(!_.isPositive)
    if (remaining.size > 1 && !gaveWarning) {
      logger.warn(
        "There are multiple scalings defined for permissions, which multiplies non-concrete numbers." +
          "Typically this will lead to incompleteness during verification."
      )
      gaveWarning = true
    }
    val x +: xs = scalings.toSeq
    xs.foldLeft(x.scale)((l, r) => {
      implicit val o: Origin = r.scale.o
      l * r.scale
    })
  }

  def addScalesImplies(e: Expr[Post]): Expr[Post] = {
    val remaining = scalings.toSeq.filter(!_.isPositive)

    if (remaining.size > 1 && !gaveWarning) {
      logger.warn(
        "There are multiple scalings defined for permissions, which we could not prove to be nonzero." +
          "Typically, this will lead to incompleteness during verification." +
          "Consider adding annotations that require that your parallel blocks have a non-zero size."
      )
      gaveWarning = true
    }

    if (remaining.isEmpty)
      return e
    implicit val o: Origin = remaining.head.scale.o
    var amount = remaining.head.scale
    if (remaining.size > 1) {
      amount =
        remaining.tail.foldLeft(amount)((l, r) => {
          implicit val o: Origin = r.scale.o
          l * r.scale
        })
    }
    (amount > NoPerm()) ==> e
  }

  def scale(res: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = res.o
    res match {
      case s: Scale[Pre] => scalings.having(scaleValue(s)) { scale(s.res) }
      case e if TBool().superTypeOf(e.t) => dispatch(e)
      case Perm(loc, p) => Perm(dispatch(loc), multiplyScales() * dispatch(p))
      case Value(loc) => addScalesImplies(Value(dispatch(loc)))
      case AutoValue(loc) => addScalesImplies(AutoValue(dispatch(loc)))
      case Star(left, right) => scale(left) &* scale(right)
      case Implies(cond, cons) => Implies(dispatch(cond), scale(cons))
      case Select(cond, whenTrue, whenFalse) =>
        Select(dispatch(cond), scale(whenTrue), scale(whenFalse))
      case s: Starall[Pre] => s.rewrite(body = scale(s.body))
      case l: Let[Pre] => l.rewrite(main = scale(l.main))
      case InlinePattern(inner, parent, group) =>
        InlinePattern(scale(inner), parent, group)
      case a @ Asserting(condition, body) =>
        a.rewrite(condition = scale(condition), body = scale(body))
      case a @ Assuming(assn, inner) =>
        a.rewrite(assn = scale(assn), inner = scale(inner))
      case pd @ PolarityDependent(onInhale, onExhale) =>
        pd.rewrite(onInhale = scale(onInhale), onExhale = scale(onExhale))
      case other => throw WrongScale(other)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    implicit val o: Origin = e.o
    e match {
      case s: Scale[Pre] => scalings.having(scaleValue(s)) { scale(s.res) }
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    val inner =
      stat match {
        case Exhale(e) => e
        case Inhale(e) => e
        case proof: FramedProof[Pre] => return checkFramedProof(proof)
        case _ => return stat.rewriteDefault()
      }
    infoGetter.setupInfo()
    unfoldStar(inner).foreach(infoGetter.addInfo(_))
    equalityChecker = ExpressionEqualityCheck(Some(infoGetter.finalInfo(Seq())))
    val result = stat.rewriteDefault()
    infoGetter.setupInfo()
    equalityChecker = ExpressionEqualityCheck()
    result
  }

  def checkFramedProof(proof: FramedProof[Pre]): FramedProof[Post] = {
    infoGetter.setupInfo()
    unfoldStar(proof.pre).foreach(infoGetter.addInfo(_))
    equalityChecker = ExpressionEqualityCheck(Some(infoGetter.finalInfo(Seq())))
    val pre = dispatch(proof.pre)
    infoGetter.setupInfo()
    unfoldStar(proof.post).foreach(infoGetter.addInfo(_))
    equalityChecker = ExpressionEqualityCheck(Some(infoGetter.finalInfo(Seq())))
    val post = dispatch(proof.post)

    equalityChecker = ExpressionEqualityCheck()
    proof.rewrite(pre = pre, post = post)
  }

  override def dispatch(
      contract: ApplicableContract[Pre]
  ): ApplicableContract[Post] = {
    infoGetter.setupInfo()
    unfoldStar(contract.contextEverywhere).foreach(infoGetter.addInfo(_))
    equalityChecker = ExpressionEqualityCheck(Some(infoGetter.finalInfo(Seq())))
    val contextEverywhere = dispatch(contract.contextEverywhere)
    val contextInfo = infoGetter.clone()

    // Reuse information from context everywhere
    infoGetter.setupInfo()
    unfoldPredicate(contract.requires).flatMap(unfoldStar)
      .foreach(infoGetter.addInfo(_))
    equalityChecker = ExpressionEqualityCheck(
      Some(infoGetter.finalInfo(Seq(contextInfo)))
    )
    val requires = dispatch(contract.requires)

    // Again reuse information from context everywhere
    infoGetter.setupInfo()
    unfoldPredicate(contract.ensures).flatMap(unfoldStar)
      .foreach(infoGetter.addInfo(_))
    equalityChecker = ExpressionEqualityCheck(
      Some(infoGetter.finalInfo(Seq(contextInfo)))
    )
    val ensures = dispatch(contract.ensures)

    // One more time reusing info from context everywhere
    infoGetter.setupInfo()
    unfoldStar(contract.kernelInvariant).foreach(infoGetter.addInfo(_))
    equalityChecker = ExpressionEqualityCheck(
      Some(infoGetter.finalInfo(Seq(contextInfo)))
    )
    val kernelInvariant = dispatch(contract.kernelInvariant)
    equalityChecker = ExpressionEqualityCheck()
    infoGetter.setupInfo()

    contract.rewrite(
      requires = requires,
      ensures = ensures,
      contextEverywhere = contextEverywhere,
      kernelInvariant = kernelInvariant,
    )
  }
}
