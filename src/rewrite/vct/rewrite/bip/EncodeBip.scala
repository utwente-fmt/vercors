package vct.col.rewrite.bip

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{
  Generation,
  Rewriter,
  RewriterBuilder,
  RewriterBuilderArg,
}
import vct.col.util.AstBuildHelpers.{contract, _}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{SystemError, Unreachable, UserError}
import BIP._
import vct.result.Message

import scala.collection.immutable.ListMap
import scala.collection.mutable

case object EncodeBip extends RewriterBuilderArg[VerificationResults] {
  override def key: String = "encodeBip"
  override def desc: String = "encodes BIP semantics explicitly"

  object ClassBipComponent {
    def unapply[G](cls: Class[G]): Option[(Class[G], BipComponent[G])] = {
      cls.decls.collectFirst({ case bc: BipComponent[G] => (cls, bc) })
    }
  }

  case class TransitionPostconditionFailed(
      results: VerificationResults,
      transition: BipTransition[_],
  ) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit =
      error match {
        case cf: ContractedFailure =>
          cf match {
            case PostconditionFailed(Seq(FailLeft), failure, _) =>
              results.report(transition, ComponentInvariantNotMaintained)
              transition.blame
                .blame(BipComponentInvariantNotMaintained(failure, transition))
            case PostconditionFailed(Seq(FailRight, FailLeft), failure, _) =>
              results.report(transition, StateInvariantNotMaintained)
              transition.blame
                .blame(BipStateInvariantNotMaintained(failure, transition))
            case PostconditionFailed(Seq(FailRight, FailRight), failure, _) =>
              results.report(transition, PostconditionNotVerified)
              transition.blame
                .blame(BipTransitionPostconditionFailure(failure, transition))
            case PostconditionFailed(_, _, _) => throw BlamePathError
            case ctx: TerminationMeasureFailed =>
              PanicBlame("BIP transition does not have termination measures")
                .blame(ctx)
            case ctx: ContextEverywhereFailedInPost =>
              PanicBlame("BIP transition does not have context everywhere")
                .blame(ctx)
          }
        case ctx: SignalsFailed =>
          PanicBlame("BIP transition does not have signals").blame(ctx)
        case ctx: ExceptionNotInSignals =>
          PanicBlame("BIP transition does not have signals").blame(ctx)
      }
  }

  case class ForwardComponentInvariantFailed(
      results: VerificationResults,
      component: BipComponent[_],
      constructor: BipConstructor[_],
  ) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit = {
      results.report(component, ComponentInvariantNotMaintained)
      constructor.blame
        .blame(BipComponentInvariantNotEstablished(error.failure, constructor))
    }
  }

  case class ForwardStateInvariantFailed(
      results: VerificationResults,
      component: BipComponent[_],
      constructor: BipConstructor[_],
  ) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit = {
      results.report(component, StateInvariantNotMaintained)
      constructor.blame
        .blame(BipStateInvariantNotEstablished(error.failure, constructor))
    }
  }

  case class ExecuteOnBlame[T <: VerificationFailure](blame: Blame[T])(
      callback: => Unit
  ) extends Blame[T] {
    override def blame(error: T): Unit = {
      callback
      blame.blame(error)
    }
  }

  case class ForwardUnsatisfiableBlame(
      results: VerificationResults,
      node: Node[_],
  ) extends Blame[NontrivialUnsatisfiable] {
    node match {
      case _: BipGuard[_] | _: BipTransition[_] | _: BipOutgoingData[_] =>
      case _ =>
        throw Unreachable(
          "Can only construct this blame with bip guard, bip transition, bip outgoing data"
        )
    }

    override def blame(error: NontrivialUnsatisfiable): Unit =
      node match {
        case g: BipGuard[_] =>
          g.blame.blame(BipGuardPreconditionUnsatisfiable(g))
        case transition: BipTransition[_] =>
          results.reportPreconditionNotVerified(transition)
          results.report(transition, UpdateFunctionFailure)
          transition.blame
            .blame(BipTransitionPreconditionUnsatisfiable(transition))
        case data: BipOutgoingData[_] =>
          data.blame.blame(BipOutgoingDataPreconditionUnsatisfiable(data))
        case _ =>
          throw Unreachable(
            "This blame is not constructed for other types of node."
          )
      }
  }

  case class MissingData(guard: BipGuard[_], transition: BipTransition[_])
      extends UserError {
    // Guard at POS needs data that transition at POS is not supplying!
    override def code: String = "bipMissingData"
    override def text: String = {
      Message.messagesInContext(
        (guard.o, "Data required by this guard..."),
        (transition.o, "... is not supplied by this transition"),
      )
    }
  }

  case class OverwritingBipResultError() extends SystemError {
    override def text: String = messageContext("Oh no overwriting bip stuff")
  }

  case class UnexpectedBipResultError() extends SystemError {
    override def text: String = messageContext("Oh no unexpected bip stuff")
  }

  private def DataWireValueCarrierOrigin(wire: BipGlueDataWire[_]): Origin = {
    wire.o.where(prefix = "result")
  }

  private def BipSynchronizationOrigin(
      s: BipTransitionSynchronization[_]
  ): Origin = {
    s.o.where(name =
      "synchron___" + s.transitions.map { case Ref(t) =>
        "transition_" + t.signature.asciiSignature
      }.mkString("_$_")
    )
  }

  private def SynchronizationComponentVariableOrigin(
      s: BipTransitionSynchronization[_],
      c: BipComponent[_],
  ): Origin = { s.o.where(name = c.fqn.mkString(".")) }

  case class ExhalingTransitionPreconditionFailed(
      results: BIP.VerificationResults,
      s: BipTransitionSynchronization[_],
      t: BipTransition[_],
  ) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit = {
      results.reportPreconditionNotVerified(t)
      s.blame.blame(TransitionPreconditionFailed(s, t, error.failure))
    }
  }

  private def ImplCheckBipTransitionOrigin(
      c: BipComponent[_],
      t: BipTransition[_],
  ): Origin = {
    t.o.where(name =
      s"transitionImplementationCheck__${c.fqn.mkString("_")}__${t.o
          .getPreferredNameOrElse().camel}_${t.signature.asciiSignature}"
    )
  }
}

case class EncodeBip[Pre <: Generation](results: VerificationResults)
    extends Rewriter[Pre] with LazyLogging {
  import vct.col.rewrite.bip.EncodeBip._

  implicit class LocalExprBuildHelpers[G](left: Expr[G]) {
    def &**(right: Expr[G])(implicit origin: Origin): Expr[G] =
      (left, right) match {
        case (BooleanValue(true), BooleanValue(true)) => tt[G]
        case (BooleanValue(true), e) => e
        case (e, BooleanValue(true)) => e
        case _ => Star[G](left, right)
      }
  }

  var program: Program[Pre] = null

  var currentThis: ScopedStack[Expr[Post]] = ScopedStack()

  val currentComponent: ScopedStack[BipComponent[Pre]] = ScopedStack()
  val rewritingBipTransitionBody: ScopedStack[BipTransition[Pre]] =
    ScopedStack()
  val rewritingBipConstructorBody: ScopedStack[BipComponent[Pre]] =
    ScopedStack()

  lazy val classes =
    program.transSubnodes.collect { case c: Class[Pre] => c }.toIndexedSeq
  lazy val components = classes.collect {
    case ClassBipComponent(cls, component) => component
  }
  lazy val allPorts = classes.flatMap { cls =>
    cls.decls.collect { case port: BipPort[Pre] => port }
  }
  lazy val transitionToClassComponent
      : Map[BipTransition[Pre], (Class[Pre], BipComponent[Pre])] =
    classes.collect { case ClassBipComponent(cls, component) =>
      cls.decls.collect { case transition: BipTransition[Pre] =>
        (transition, (cls, component))
      }
    }.flatten.toMap
  lazy val dataToClassComponent
      : Map[BipData[Pre], (Class[Pre], BipComponent[Pre])] =
    classes.collect { case ClassBipComponent(cls, component) =>
      cls.decls.collect { case data: BipData[Pre] => (data, (cls, component)) }
    }.flatten.toMap
  lazy val guardToClass: Map[BipGuard[Pre], Class[Pre]] =
    classes.flatMap { cls =>
      cls.decls.collect { case guard: BipGuard[Pre] => (guard, cls) }
    }.toMap
  lazy val componentToClass: Map[BipComponent[Pre], Class[Pre]] =
    classes.collect { case ClassBipComponent(cls, component) =>
      (component, cls)
    }.toMap
  lazy val constructorToClassComponent
      : Map[BipConstructor[Pre], (Class[Pre], BipComponent[Pre])] =
    classes.collect { case ClassBipComponent(cls, component) =>
      cls.collect { case constructor: BipConstructor[Pre] =>
        (constructor, (cls, component))
      }
    }.flatten.toMap
  lazy val portToComponent: Map[BipPort[Pre], BipComponent[Pre]] =
    components.flatMap { component =>
      classOf(component).decls.collect { case p: BipPort[Pre] =>
        (p, component)
      }
    }.toMap

  def classOf(c: BipComponent[Pre]): Class[Pre] = componentToClass(c)
  def classOf(p: BipPort[Pre]): Class[Pre] =
    componentToClass(portToComponent(p))
  def classOf(g: BipGuard[Pre]): Class[Pre] = guardToClass(g)
  def classOf(p: BipConstructor[Pre]): Class[Pre] =
    constructorToClassComponent(p)._1
  def classOf(t: BipTransition[Pre]): Class[Pre] =
    transitionToClassComponent(t)._1
  def classOf(d: BipData[Pre]): Class[Pre] = dataToClassComponent(d)._1
  def componentOf(p: BipConstructor[Pre]): BipComponent[Pre] =
    constructorToClassComponent(p)._2
  def componentOf(t: BipTransition[Pre]): BipComponent[Pre] =
    transitionToClassComponent(t)._2
  def componentOf(d: BipData[Pre]): BipComponent[Pre] =
    dataToClassComponent(d)._2

  val guardSucc: SuccessionMap[BipGuard[Pre], InstanceMethod[Post]] =
    SuccessionMap()
  val transitionSucc: SuccessionMap[BipTransition[Pre], InstanceMethod[Post]] =
    SuccessionMap()
  val outgoingDataSucc
      : SuccessionMap[BipOutgoingData[Pre], InstanceMethod[Post]] =
    SuccessionMap()
  val synchronizationSucc
      : SuccessionMap[BipTransitionSynchronization[Pre], Procedure[Post]] =
    SuccessionMap()
  val constructorSucc: SuccessionMap[BipConstructor[Pre], Procedure[Post]] =
    SuccessionMap()

  // References to incoming datas can be translated in two ways:
  sealed trait IncomingDataContext
  // The incoming data is stored in some variable
  case class RewriteToVariableContext(
      m: Map[BipIncomingData[Pre], Variable[Post]]
  ) extends IncomingDataContext
  // The incoming data must be acquired by calling an outgoing data on a certain object, likely some variable, but we keep it abstract here.
  // The important part is that, in the post ast, the type of the expr supports the method resulting from the outgoing data.
  case class RewriteToOutgoingDataContext(
      m: Map[BipIncomingData[Pre], (Expr[Post], BipOutgoingData[Pre])]
  ) extends IncomingDataContext

  val incomingDataContext: ScopedStack[IncomingDataContext] = ScopedStack()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    program = p
    super.dispatch(p)
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case t @ ThisObject(_) =>
        currentThis.topOption match {
          case Some(res) => res
          case None => t.rewrite()
        }

      case l @ BipLocalIncomingData(Ref(data)) =>
        incomingDataContext.top match {
          case RewriteToVariableContext(m) => m(data).get(l.o)
          case RewriteToOutgoingDataContext(m) =>
            methodInvocation(
              obj = m(data)._1,
              ref = outgoingDataSucc
                .ref[Post, InstanceMethod[Post]](m(data)._2),
              blame = PanicBlame("Querying outgoing data here cannot fail"),
            )(l.o)
        }

      case invocation @ BipGuardInvocation(obj, Ref(guard)) =>
        methodInvocation(
          obj = dispatch(obj),
          ref = guardSucc.ref[Post, InstanceMethod[Post]](guard),
          // Sneakily reuse the encoding we define above by retrofitting the refs from the guard definition into a pre-AST expression
          args = guard.data.map { ref =>
            BipLocalIncomingData[Pre](ref)(invocation.o)
          }.map(dispatch),
          blame = PanicBlame(
            "Guard invocation cannot fail as it can only depend on component invariant"
          ),
        )(expr.o)

      case _ => rewriteDefault(expr)
    }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      // Should be implemented through synchronizations within this pass, so they should be dropped
      case port: BipPort[Pre] => port.drop()

      // Should be implemented as arguments passed to update functions within this pass, so they should be dropped
      case id: BipIncomingData[Pre] => id.drop()

      case data: BipOutgoingData[Pre] =>
        implicit val o = DiagnosticOrigin
        assert(data.pure, "outgoing datas are only allowed to be pure")
        outgoingDataSucc(data) = classDeclarations.declare(
          new InstanceMethod[Post](
            dispatch(data.t),
            Seq(),
            Seq(),
            Seq(),
            Some(dispatch(data.body)),
            contract = contract[Post](
              ForwardUnsatisfiableBlame(null /* not needed */, data),
              requires = UnitAccountedPredicate(
                dispatch(currentComponent.top.invariant)
              ),
            ),
            pure = true,
          )(PanicBlame("Postcondition of data cannot fail"))(data.o)
        )

      // Is encoded in contracts, so dropped
      case sp: BipStatePredicate[Pre] => sp.drop()

      case component: BipComponent[Pre] => component.drop()

      case guard: BipGuard[Pre] =>
        implicit val o = guard.o
        val incomingDataVariables = ListMap.from(guard.data.map {
          case Ref(data) => (data, new Variable(dispatch(data.t)))
        })
        labelDecls.scope {
          incomingDataContext
            .having(RewriteToVariableContext(incomingDataVariables)) {
              guardSucc(guard) = classDeclarations.declare(
                new InstanceMethod[Post](
                  TBool()(guard.o),
                  incomingDataVariables.values.toSeq,
                  Nil,
                  Nil,
                  Some(dispatch(guard.body)),
                  contract[Post](
                    ForwardUnsatisfiableBlame(null /* not needed */, guard),
                    requires = UnitAccountedPredicate(
                      dispatch(currentComponent.top.invariant)
                    ),
                  ),
                  pure = guard.pure,
                )(PanicBlame("Postcondition of guard cannot fail"))(guard.o)
              )
            }
        }

      case ClassBipComponent(cls, component) =>
        currentComponent.having(component) { rewriteDefault(cls) }

      case constructor: BipConstructor[Pre] =>
        val component = componentOf(constructor)
        results.declare(component)
        implicit val o = DiagnosticOrigin
        val ref = succ[Class[Post]](classOf(constructor))
        val t = TByReferenceClass[Post](ref, Seq())
        rewritingBipConstructorBody.having(component) {
          constructorSucc(constructor) = globalDeclarations.declare(
            new Procedure[Post](
              returnType = TVoid(),
              args = variables.collect { constructor.args.map(dispatch) }._1,
              outArgs = Nil,
              typeArgs = Nil,
              body = {
                val resVar = new Variable[Post](t)
                val res = Local[Post](resVar.ref)
                currentThis.having(res) {
                  Some(Scope(
                    Seq(resVar),
                    Block(Seq(
                      assignLocal(res, NewObject(ref)),
                      dispatch(constructor.body),
                      Exhale(dispatch(component.initial.decl.expr))(
                        ForwardStateInvariantFailed(
                          results,
                          component,
                          constructor,
                        )
                      ),
                      Exhale(dispatch(component.invariant))(
                        ForwardComponentInvariantFailed(
                          results,
                          component,
                          constructor,
                        )
                      ),
                    )),
                  ))
                }
              },
              contract(
                requires = UnitAccountedPredicate(
                  dispatch(constructor.requires)
                ),
                blame = constructor.blame,
              ),
            )(PanicBlame(
              "Unexpected error, failing precondition on JavaBIP constructor should not happen"
            ))(constructor.o)
          )
        }

      case transition: BipTransition[Pre] =>
        implicit val o = DiagnosticOrigin

        val component = currentComponent.top
        // Mark that the default case is that verification is succesfull
        results.declare(component, transition)

        // Better debugging
        val incomingDataToVariable = ListMap.from(transition.data.map {
          case Ref(data) => (data, new Variable(dispatch(data.t)))
        })

        labelDecls.scope {
          rewritingBipTransitionBody.having(transition) {
            transitionSucc(transition) = classDeclarations.declare(
              new InstanceMethod[Post](
                TVoid(),
                incomingDataToVariable.values.toSeq,
                Nil,
                Nil,
                incomingDataContext
                  .having(RewriteToVariableContext(incomingDataToVariable)) {
                    Some(dispatch(transition.body))
                  },
                contract[Post](
                  ForwardUnsatisfiableBlame(results, transition),
                  requires = UnitAccountedPredicate(
                    dispatch(component.invariant) &**
                      dispatch(transition.source.decl.expr) &**
                      incomingDataContext.having(RewriteToVariableContext(
                        incomingDataToVariable
                      )) {
                        dispatch(transition.guard) &**
                          dispatch(transition.requires)
                      }
                  ),
                  ensures =
                    // Establish component invariant
                    SplitAccountedPredicate(
                      UnitAccountedPredicate(dispatch(component.invariant)),
                      // Establish state invariant
                      SplitAccountedPredicate(
                        UnitAccountedPredicate(
                          dispatch(transition.target.decl.expr)
                        ),
                        // Establish update function postcondition
                        UnitAccountedPredicate(dispatch(transition.ensures)),
                      ),
                    ),
                ),
              )(TransitionPostconditionFailed(results, transition))(
                ImplCheckBipTransitionOrigin(component, transition)
              )
            )
          }
        }

      case synchronization: BipTransitionSynchronization[Pre] =>
        synchronization.drop()
        implicit val o = DiagnosticOrigin
        val transitions = synchronization.transitions.map { case Ref(t) => t }

        /* Each transition that participates in the synchronization, applies to a specific component.
         We model those components as COL classes. Each synchronization is modeled as a procedure
         that takes for each participating component, an argument to a class instance representing that component.
         We define those variables/arguments here.
         */
        val varOf: Map[Declaration[Pre], Local[Post]] =
          transitions.flatMap { transition =>
            val v =
              new Variable[Post](
                TByReferenceClass(succ[Class[Post]](classOf(transition)), Seq())
              )(SynchronizationComponentVariableOrigin(
                synchronization,
                componentOf(transition),
              ))
            Seq((transition, v.get), (classOf(transition), v.get))
          }.toMap

        /* - Pre:
           - Of each class var of transition:
             - Non null
             - Component invariant
             - State invariant
             - Guard (done further down)
         */
        val preconditionComponents: Expr[Post] = foldStar(
          synchronization.transitions.map { case Ref(transition) =>
            currentThis.having(varOf(transition)) {
              (varOf(transition) !== Null()) &**
                dispatch(componentOf(transition).invariant) &**
                dispatch(transition.source.decl.expr)
            }
          }
        )

        /* Here we construct a mapping of incoming data to invocations of the outgoing data methods, as indicated by data wires
         This is needed because in the precondition the variables for exchange of data are not yet assigned, so we just execute
         the guards over literal outgoing data invocations. These are then used when guard invocations are translated
         */
        val incomingDataTranslation = ListMap.from(synchronization.wires.map {
          case BipGlueDataWire(Ref(out), Ref(in)) =>
            (in, (varOf(classOf(out)), out))
        })

        val preconditionGuards: Expr[Post] = foldStar(
          synchronization.transitions.map { case Ref(transition) =>
            currentThis.having(varOf(transition)) {
              incomingDataContext
                .having(RewriteToOutgoingDataContext(incomingDataTranslation)) {
                  dispatch(transition.guard)
                }
            }
          }
        )

        /*
      - Evaluate data wires and assign to var representing output data. Make mapping from data wire to output data var
         */
        val incomingDataToVariableContext = ListMap
          .from(synchronization.wires.map {
            case BipGlueDataWire(Ref(dataOut), Ref(dataIn)) =>
              (dataIn, new Variable[Post](dispatch(dataOut.t))(dataIn.o))
          })
        val initBlock: Seq[Statement[Post]] =
          incomingDataContext
            .having(RewriteToOutgoingDataContext(incomingDataTranslation)) {
              incomingDataToVariableContext.map { case (dataIn, variable) =>
                assignLocal[Post](
                  variable.get,
                  dispatch(BipLocalIncomingData(dataIn.ref)),
                )
              }
            }.toSeq

        /*
      - Of each class var of transition:
        - Exhale, with class var & data wires substituted in:
          - component invariant
          - source state invariant
        - Exhale, also with substitutions, containing precondition.
          - Separate so we can detect the error easily
         */
        val exhales: Seq[Exhale[Post]] = synchronization.transitions.flatMap {
          case Ref(transition) =>
            incomingDataContext
              .having(RewriteToVariableContext(incomingDataToVariableContext)) {
                val componentStateGuardExhale =
                  Exhale(currentThis.having(varOf(transition)) {
                    foldStar(
                      Seq(
                        dispatch(componentOf(transition).invariant),
                        dispatch(transition.source.decl.expr),
                      ) :+ dispatch(transition.guard)
                    )
                  })(PanicBlame(
                    "Component invariant, state invariant, and transition guard should hold"
                  ))

                /* At the time of writing, exhales do not support a system like UnitAccountedPredicate, meaning there's
             know what part of the exhale failed. Additionally, at the time of writing, the precondition of a transition
             is always a boolean. Hence, we split

             exhale (componentInvariant ** stateInvariant ** guard) ** precondition

             into:

             exhale precondition
             exhale (componentInvariant ** stateInvariant ** guard)

             This ensures that while evaluating the precondition, all permissions that might be necessary to evaluate
             it are still present. Additionally, if it fails, you know the precondition is broken, and not some other subexpression.

             We don't have to split up the component, state, guard, any further, because those are always implied
             by the precondition of the synchron.
                 */
                val preconditionExhale =
                  Exhale(currentThis.having(varOf(transition)) {
                    dispatch(transition.requires)
                  })(ExhalingTransitionPreconditionFailed(
                    results,
                    synchronization,
                    transition,
                  ))

                Seq(preconditionExhale, componentStateGuardExhale)
              }
        }

        /*
        - Inhale, with class var & data wires substituted in:
          - component invariant
          - target state invariant
          - postcondition
         */
        val inhales: Seq[Inhale[Post]] = synchronization.transitions.map {
          case Ref(transition) =>
            Inhale(currentThis.having(varOf(transition)) {
              dispatch(componentOf(transition).invariant) &**
                dispatch(transition.target.decl.expr) &**
                dispatch(transition.ensures)
            })
        }

        /*
      - Post:
        - Of each class var of transition:
          - Component invariant
          - New state invariant
         */
        val postcondition: Expr[Post] = foldStar(
          synchronization.transitions.map { case Ref(transition) =>
            currentThis.having(varOf(transition)) {
              dispatch(componentOf(transition).invariant) &**
                dispatch(transition.target.decl.expr)
            }
          }
        )

        synchronizationSucc(synchronization) = globalDeclarations.declare(
          procedure(
            args = transitions.map(varOf(_).ref.decl),
            body = Some(Scope(
              incomingDataToVariableContext.values.toSeq,
              Block(initBlock ++ exhales ++ inhales),
            )),
            requires = UnitAccountedPredicate(
              preconditionComponents &** preconditionGuards
            ),
            ensures = UnitAccountedPredicate(postcondition),
            blame = PanicBlame("Can this contract fail?"),
            contractBlame = PanicBlame("Can it be unsatisfiable?"),
          )(BipSynchronizationOrigin(synchronization))
        )

      case _: BipPortSynchronization[Pre] =>
        throw Unreachable("Should be translated away at this point")

      case _ => rewriteDefault(decl)
    }

  /* If:
     - the body of a transition is being rewritten, or
     - the body of a constructor is being rewritten,
    wrap each blame into another blame. This blame writes reports failure in transition verification results when triggered
   */
  override def dispatch[T <: VerificationFailure](blame: Blame[T]): Blame[T] = {
    assert(
      Seq(rewritingBipTransitionBody, rewritingBipConstructorBody)
        .map(_.topOption).count(_.isDefined) <= 1,
      "should not be rewriting constructors and transitions simultaneously",
    )
    rewritingBipTransitionBody.topOption
      .orElse(rewritingBipConstructorBody.topOption) match {
      case Some(bt: BipTransition[Pre]) =>
        ExecuteOnBlame(blame) { results.report(bt, UpdateFunctionFailure) }
      case Some(bt: BipComponent[Pre]) =>
        ExecuteOnBlame(blame) { results.report(bt, ConstructorFailure) }
      case _ => super.dispatch(blame)
    }
  }
}
