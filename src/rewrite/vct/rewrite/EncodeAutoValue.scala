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

  private case class ConditionContext[Pre <: Generation](
      context: PreOrPost,
      prePostMap: mutable.ArrayBuffer[(Expr[Pre], Expr[Rewritten[Pre]])] =
        mutable.ArrayBuffer[(Expr[Pre], Expr[Rewritten[Pre]])](),
      autoValueLocations: mutable.HashSet[Location[Rewritten[Pre]]] = mutable
        .HashSet[Location[Rewritten[Pre]]](),
      normalLocations: mutable.HashSet[Location[Rewritten[Pre]]] = mutable
        .HashSet[Location[Rewritten[Pre]]](),
  )
}

case class EncodeAutoValue[Pre <: Generation]() extends Rewriter[Pre] {

  import EncodeAutoValue._

  private val conditionContext: ScopedStack[Option[ConditionContext[Pre]]] =
    ScopedStack()
  conditionContext.push(None)
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
        conditionContext.having(Some(
          ConditionContext(InPrecondition(), prePostMap = preMap)
        )) { dispatch(node.requires) },
      ensures = {
        val predicate =
          conditionContext.having(Some(
            ConditionContext(InPostcondition(), prePostMap = postMap)
          )) { dispatch(node.ensures) }
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
    if (conditionContext.top.isEmpty) {
      e match {
        case AutoValue(_) => throw InvalidAutoValue(e)
        case _ => e.rewriteDefault()
      }
    } else {
      val context = conditionContext.top.get
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
          context match {
            case ConditionContext(
                  InPrecondition(),
                  preMap,
                  avLocations,
                  locations,
                ) =>
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
            case ConditionContext(
                  InPostcondition(),
                  postMap,
                  avLocations,
                  locations,
                ) =>
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
          context match {
            case ConditionContext(_, _, avLocations, locations) => {
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
            localHeapVariables.scope {
              val (b, v) =
                conditionContext.having(None) {
                  (variables.dispatch(binding), dispatch(value))
                }
              val mMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
              val m =
                conditionContext.having(Some(context.copy(prePostMap = mMap))) {
                  dispatch(main)
                }
              if (mMap.isEmpty) { Let(b, v, m) }
              else {
                mMap.foreach(postM =>
                  context.prePostMap.append(
                    (Let(binding, value, postM._1), Let(b, v, postM._2))
                  )
                )
                context.context match {
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
          }
        case Select(condition, left, right) =>
          val c = conditionContext.having(None) { dispatch(condition) }
          val lMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
          val rMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
          val l =
            conditionContext.having(Some(context.copy(prePostMap = lMap))) {
              dispatch(left)
            }
          val r =
            conditionContext.having(Some(context.copy(prePostMap = rMap))) {
              dispatch(right)
            }
          if (lMap.isEmpty && rMap.isEmpty)
            Select(c, l, r)
          else {
            lMap.foreach(postL =>
              context.prePostMap.append((
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
              context.prePostMap.append((
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
            context.context match {
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
          val l = conditionContext.having(None) { dispatch(left) }
          val r =
            conditionContext.having(Some(context.copy(prePostMap = rMap))) {
              dispatch(right)
            }
          if (rMap.nonEmpty) {
            context.context match {
              case InPrecondition() =>
                rMap.foreach(postR =>
                  context.prePostMap
                    .append((Implies(left, postR._1), Implies(l, postR._2)))
                )
                Implies(l, r)
              case InPostcondition() =>
                // We use the values in conditionContext just for checking
                // duplicates therefore we don't include the old here since
                // otherwise it wouldn't match the precondition case
                rMap.foreach(postR =>
                  context.prePostMap.append((Implies(left, postR._1), tt))
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
          val lMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
          val rMap = mutable.ArrayBuffer[(Expr[Pre], Expr[Post])]()
          val l =
            conditionContext.having(Some(context.copy(prePostMap = lMap))) {
              dispatch(left)
            }
          val r =
            conditionContext.having(Some(context.copy(prePostMap = rMap))) {
              dispatch(right)
            }
          context.prePostMap.addAll(lMap)
          context.prePostMap.addAll(rMap)
          Star(l, r)
        case _ => conditionContext.having(None) { e.rewriteDefault() }
      }
    }
  }
}
