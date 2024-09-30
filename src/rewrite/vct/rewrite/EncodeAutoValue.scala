package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.compare.Compare
import vct.col.origin.{
  AbstractApplicable,
  AutoValueLeakCheckFailed,
  Blame,
  LabelContext,
  Origin,
  PanicBlame,
  PostBlameSplit,
  PostconditionFailed,
  PreferredName,
  TrueSatisfiable,
}
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.result.VerificationError.UserError

import scala.collection.mutable

case object EncodeAutoValue extends RewriterBuilder {
  override def key: String = "encodeAutoValue"

  override def desc: String = "Encodes the AutoValue resource"

  private def AnyReadOrigin: Origin =
    Origin(Seq(PreferredName(Seq("read$")), LabelContext("encodeAutoValue")))

  case class InvalidAutoValue(e: Expr[_]) extends UserError {
    override def code: String = "invalidAutoValue"

    override def text: String =
      e.o.messageInContext(
        "The AutoValue resource may only be in pre and postconditions in expressions containing only ?:, **, ==>, and let"
      )
  }

  case class CombinedAutoValue(autoNode: Node[_], regularNode: Node[_])
      extends UserError {
    override def code: String = "combinedAutoValue"

    override def text: String =
      vct.result.Message.messagesInContext(
        (
          regularNode.o,
          "The AutoValue resource may not be combined with permission resources for what is potentially the same field...",
        ),
        (autoNode.o, "The AutoValue resource was here"),
      )
  }

  private sealed class PreOrPost

  private final case class InPrecondition() extends PreOrPost

  private final case class InPostcondition() extends PreOrPost

  case class AutoValueLeakCheckFailedBlame(app: ContractApplicable[_])
      extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      app.blame.blame(AutoValueLeakCheckFailed(error.failure, app))
  }
}

case class EncodeAutoValue[Pre <: Generation]() extends Rewriter[Pre] {

  import EncodeAutoValue._

  private val conditionContext: ScopedStack[
    (
        PreOrPost,
        mutable.ArrayBuffer[(Expr[Pre], Expr[Post])],
        mutable.HashSet[Location[Post]],
        mutable.HashSet[Location[Post]],
    )
  ] = ScopedStack()
  private val inFunction: ScopedStack[Unit] = ScopedStack()

  private lazy val anyRead: Function[Post] = {
    implicit val o: Origin = AnyReadOrigin
    globalDeclarations.declare(withResult((result: Result[Post]) =>
      function(
        AbstractApplicable,
        TrueSatisfiable,
        returnType = TRational(),
        ensures = UnitAccountedPredicate(
          result > NoPerm() &* result < WritePerm()
        ),
      )
    ))
  }

  override def dispatch(node: Declaration[Pre]): Unit =
    node match {
      case app: AbstractFunction[Pre] =>
        inFunction.having(()) {
          allScopes.anySucceed(
            app,
            app.rewrite(blame =
              PostBlameSplit.left(AutoValueLeakCheckFailedBlame(app), app.blame)
            ),
          )
        }
      case app: AbstractMethod[Pre] =>
        allScopes.anySucceed(
          app,
          app.rewrite(blame =
            PostBlameSplit.left(AutoValueLeakCheckFailedBlame(app), app.blame)
          ),
        )
      case _ => super.dispatch(node)
    }

  override def dispatch(
      node: ApplicableContract[Pre]
  ): ApplicableContract[Post] = {
    implicit val o: Origin = node.o
    val preMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
    val postMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
    node.rewrite(
      requires =
        conditionContext.having(
          (InPrecondition(), preMap, mutable.HashSet(), mutable.HashSet())
        ) { dispatch(node.requires) },
      ensures = {
        val predicate =
          conditionContext.having(
            ((InPostcondition(), postMap, mutable.HashSet(), mutable.HashSet()))
          ) { dispatch(node.ensures) }
        val filtered = preMap.filterNot(pre =>
          postMap.exists(post => {
            Compare.isIsomorphic(pre._1, post._1, matchFreeVariables = true)
          })
        ).map(pre => pre._2)
        SplitAccountedPredicate(
          UnitAccountedPredicate(foldStar(filtered.toSeq)),
          predicate,
        )
      },
    )
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    if (conditionContext.isEmpty) {
      e match {
        case AutoValue(_) => throw InvalidAutoValue(e)
        case _ => e.rewriteDefault()
      }
    } else {
      e match {
        case AutoValue(loc) if inFunction.nonEmpty => Value(dispatch(loc))
        case AutoValue(loc) => {
          val x = new Variable[Post](TRef())
          val postLoc = dispatch(loc)
          val (genericLocation, obj) =
            postLoc match {
              case SilverFieldLocation(obj, field) =>
                (SilverFieldLocation(x.get, field), obj)
            }
          conditionContext.top match {
            case (InPrecondition(), preMap, avLocations, locations) =>
              preMap +=
                ((
                  e,
                  PolarityDependent[Post](
                    onInhale = tt,
                    onExhale = Perm(
                      postLoc,
                      functionInvocation(AbstractApplicable, anyRead.ref),
                    ),
                  ),
                ))
              avLocations += postLoc
              if (locations.contains(postLoc)) {
                throw CombinedAutoValue(e, locations.find(_ == postLoc).get)
              }
              PolarityDependent(
                onInhale = Perm(
                  postLoc,
                  functionInvocation(AbstractApplicable, anyRead.ref),
                ),
                onExhale =
                  !ForPerm(Seq(x), genericLocation, ff) &*
                    !ForPerm(Seq(x), genericLocation, x.get !== obj),
              )
            case (InPostcondition(), postMap, avLocations, locations) =>
              postMap += ((e, tt))
              avLocations += postLoc
              if (locations.contains(postLoc)) {
                throw CombinedAutoValue(e, locations.find(_ == postLoc).get)
              }
              PolarityDependent(
                onInhale =
                  !ForPerm(Seq(x), genericLocation, ff) &*
                    !ForPerm(Seq(x), genericLocation, x.get !== obj),
                onExhale = Perm(
                  postLoc,
                  functionInvocation(AbstractApplicable, anyRead.ref),
                ),
              )
          }
        }
        case Perm(loc, perm) => {
          val postLoc = dispatch(loc)
          conditionContext.top match {
            case (_, _, avLocations, locations) => {
              locations += postLoc;
              if (avLocations.contains(postLoc)) {
                throw CombinedAutoValue(avLocations.find(_ == postLoc).get, e)
              }
              Perm(postLoc, dispatch(perm))
            }
          }
        }
        case Let(binding, value, main) =>
          variables.scope {
            val top = conditionContext.pop()
            val (b, v) =
              try { (variables.dispatch(binding), dispatch(value)) }
              finally { conditionContext.push(top) }
            val mMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
            val m =
              conditionContext.having((
                conditionContext.top._1,
                mMap,
                conditionContext.top._3,
                conditionContext.top._4,
              )) { dispatch(main) }
            if (mMap.isEmpty) { Let(b, v, m) }
            else {
              mMap.foreach(postM =>
                conditionContext.top._2
                  .append((Let(binding, value, postM._1), Let(b, v, postM._2)))
              )
              conditionContext.top._1 match {
                case InPrecondition() => Let(b, v, m)
                case InPostcondition() =>
                  Let(
                    b,
                    Old(v, None)(PanicBlame(
                      "Old should always be valid in a postcondition"
                    )),
                    m,
                  )
              }
            }
          }
        case Select(condition, left, right) =>
          val top = conditionContext.pop()
          val c =
            try { dispatch(condition) }
            finally { conditionContext.push(top) }
          val lMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
          val rMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
          val l =
            conditionContext.having((
              conditionContext.top._1,
              lMap,
              conditionContext.top._3,
              conditionContext.top._4,
            )) { dispatch(left) }
          val r =
            conditionContext.having((
              conditionContext.top._1,
              rMap,
              conditionContext.top._3,
              conditionContext.top._4,
            )) { dispatch(right) }
          if (lMap.isEmpty && rMap.isEmpty)
            Select(c, l, r)
          else {
            lMap.foreach(postL =>
              conditionContext.top._2.append((
                Select(condition, postL._1, tt),
                Select(
                  Old(c, None)(PanicBlame(
                    "Old should always be valid in a postcondition"
                  )),
                  postL._2,
                  tt,
                ),
              ))
            )
            rMap.foreach(postR =>
              conditionContext.top._2.append((
                Select(condition, tt, postR._1),
                Select(
                  Old(c, None)(PanicBlame(
                    "Old should always be valid in a postcondition"
                  )),
                  tt,
                  postR._2,
                ),
              ))
            )
            conditionContext.top._1 match {
              case InPrecondition() => Select(c, l, r)
              case InPostcondition() =>
                Select(
                  Old(c, None)(PanicBlame(
                    "Old should always be valid in a postcondition"
                  )),
                  l,
                  r,
                )
            }
          }
        case Implies(left, right) =>
          val rMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
          val top = conditionContext.pop()
          val l =
            try { dispatch(left) }
            finally { conditionContext.push(top); }
          val r =
            conditionContext.having((
              conditionContext.top._1,
              rMap,
              conditionContext.top._3,
              conditionContext.top._4,
            )) { dispatch(right) }
          if (rMap.nonEmpty) {
            conditionContext.top._1 match {
              case InPrecondition() =>
                rMap.foreach(postR =>
                  conditionContext.top._2
                    .append((Implies(left, postR._1), Implies(l, postR._2)))
                )
                Implies(l, r)
              case InPostcondition() =>
                // We use the values in conditionContext just for checking
                // duplicates therefore we don't include the old here since
                // otherwise it wouldn't match the precondition case
                rMap.foreach(postR =>
                  conditionContext.top._2.append((Implies(left, postR._1), tt))
                )
                Implies(
                  Old(l, None)(PanicBlame(
                    "Old should always be valid in a postcondition"
                  )),
                  r,
                )
            }
          } else { Implies(l, r) }
        case Star(left, right) =>
          val top = conditionContext.pop()
          try {
            val lMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
            val rMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
            val l =
              conditionContext.having((top._1, lMap, top._3, top._4)) {
                dispatch(left)
              }
            val r =
              conditionContext.having((top._1, rMap, top._3, top._4)) {
                dispatch(right)
              }
            top._2.addAll(lMap)
            top._2.addAll(rMap)
            Star(l, r)
          } finally { conditionContext.push(top); }
        case _ =>
          val top = conditionContext.pop()
          try { e.rewriteDefault() }
          finally { conditionContext.push(top); }
      }
    }
  }
}
