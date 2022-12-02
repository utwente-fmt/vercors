package vct.col.rewrite.bip

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg}
import vct.col.util.AstBuildHelpers.{contract, _}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{SystemError, Unreachable, UserError}
import BIP._

import scala.collection.immutable.ListMap
import scala.collection.mutable

// TODO (RR): Proof obligation that if a port is enabled, only one transition can ever be enabled

case object EncodeBip extends RewriterBuilderArg[VerificationResults] {
  override def key: String = "encodeBip"
  override def desc: String = "encodes BIP semantics explicitly"

  object ClassBipComponent {
    def unapply[G](cls: Class[G]): Option[(Class[G], BipComponent[G])] = {
      cls.declarations.collectFirst({
        case bc: BipComponent[G] => (cls, bc)
      })
    }
  }

  /* TODO (RR): The next three classes seem repetitive. Can probably factor out a common core,
      e.g., handle postcondition failed, panic on the rest? That does hurt understandability.
   */
  case class TransitionPostconditionFailed(results: VerificationResults, transition: BipTransition[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case cf: ContractedFailure => cf match {
        case PostconditionFailed(Seq(FailLeft), failure, _) =>
          results.report(transition, ComponentInvariantNotMaintained)
          transition.blame.blame(BipComponentInvariantNotMaintained(failure, transition))
        case PostconditionFailed(Seq(FailRight, FailLeft), failure, _) =>
          results.report(transition, StateInvariantNotMaintained)
          transition.blame.blame(BipStateInvariantNotMaintained(failure, transition))
        case PostconditionFailed(Seq(FailRight, FailRight), failure, _) =>
          results.report(transition, PostconditionNotVerified)
          transition.blame.blame(BipTransitionPostconditionFailure(failure, transition))
        case ctx: ContextEverywhereFailedInPost => PanicBlame("BIP transition does not have context everywhere").blame(ctx)
      }
      case ctx: SignalsFailed => PanicBlame("BIP transition does not have signals").blame(ctx)
      case ctx: ExceptionNotInSignals => PanicBlame("BIP transition does not have signals").blame(ctx)
    }
  }

  // TODO: Refactor with postblamesplit
  case class ConstructorPostconditionFailed(results: VerificationResults, component: BipComponent[_], proc: Procedure[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case cf: ContractedFailure => cf match {
        case PostconditionFailed(Seq(FailLeft), failure, _) => // Failed establishing component invariant
          results.report(component, ComponentInvariantNotMaintained)
          proc.blame.blame(BipComponentInvariantNotEstablished(failure, proc))
        case PostconditionFailed(Seq(FailRight, FailLeft), failure, _) => // Failed establishing state invariant
          results.report(component, StateInvariantNotMaintained)
          proc.blame.blame(BipStateInvariantNotEstablished(failure, proc))
        case PostconditionFailed(FailRight +: FailRight +: path, failure, node) => // Failed postcondition
          results.report(component, PostconditionNotVerified)
          proc.blame.blame(PostconditionFailed(path, failure, node))
        case ctx: ContextEverywhereFailedInPost => proc.blame.blame(ctx)
      }
      case ctx: SignalsFailed => proc.blame.blame(ctx)
      case ctx: ExceptionNotInSignals => proc.blame.blame(ctx)
    }
  }

  case class ExecuteOnBlame[T <: VerificationFailure](blame: Blame[T])(callback: => Unit) extends Blame[T] {
    override def blame(error: T): Unit = {
      callback
      blame.blame(error)
    }
  }

  case class ForwardUnsatisfiableBlame(results: VerificationResults, node: Node[_]) extends Blame[NontrivialUnsatisfiable] {
    node match {
      case _: BipGuard[_] | _: BipTransition[_] | _: BipOutgoingData[_] =>
      case _ => throw Unreachable("Can only construct this blame with bip guard, bip transition, bip outgoing data")
    }

    override def blame(error: NontrivialUnsatisfiable): Unit = node match {
      case g: BipGuard[_] => g.blame.blame(BipGuardPreconditionUnsatisfiable(g))
      case t: BipTransition[_] =>
        // TODO: Add test where unsatisfaible precondition is detected/signalled in report
        results.reportPreconditionNotVerified(t)
        results.report(t, UpdateFunctionFailure)
        t.blame.blame(BipTransitionPreconditionUnsatisfiable(t))
      case t: BipOutgoingData[_] => t.blame.blame(BipOutgoingDataPreconditionUnsatisfiable(t))
    }
  }

  case class MissingData(guard: BipGuard[_], transition: BipTransition[_]) extends UserError {
    // Guard at POS needs data that transition at POS is not supplying!
    override def code: String = "bipMissingData"
    override def text: String = {
      Origin.messagesInContext(Seq(
        (guard.o, "Data required by this guard..."),
        (transition.o, "... is not supplied by this transition")
      ))
    }
  }

  case class OverwritingBipResultError() extends SystemError {
    override def text: String = "Oh no overwriting bip stuff"
  }

  case class UnexpectedBipResultError() extends SystemError {
    override def text: String = "Oh no unexpected bip stuff"
  }

  case class DataWireValueCarrierOrigin(wire: BipGlueDataWire[_]) extends Origin {
    override def preferredName: String = wire.o.preferredName + "_result"
    override def context: String = wire.o.context
    override def inlineContext: String = wire.o.inlineContext
    override def shortPosition: String = wire.o.shortPosition
  }

  case class BipSynchronizationOrigin(s: BipTransitionSynchronization[_]) extends Origin {
    override def preferredName: String = "synchron___" +
      s.transitions.map { case Ref(t) => "transition_" + t.signature.asciiSignature }.mkString("_$_")
    override def context: String = s.o.context
    override def inlineContext: String = s.o.inlineContext
    override def shortPosition: String = s.o.shortPosition
  }

  case class SynchronizationComponentVariableOrigin(s: BipTransitionSynchronization[_], c: BipComponent[_]) extends Origin {
    override def preferredName: String = c.fqn.mkString(".")
    override def context: String = s.o.context
    override def inlineContext: String = s.o.inlineContext
    override def shortPosition: String = s.o.shortPosition
  }

  case class ExhalingTransitionPreconditionFailed(results: BIP.VerificationResults, s: BipTransitionSynchronization[_], t: BipTransition[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit = {
      results.reportPreconditionNotVerified(t)
      s.blame.blame(TransitionPreconditionFailed(s, t, error.failure))
    }
  }
}

case class EncodeBip[Pre <: Generation](results: VerificationResults) extends Rewriter[Pre] with LazyLogging {
  import vct.col.rewrite.bip.EncodeBip._

  implicit class LocalExprBuildHelpers[G](left: Expr[G]) {
    def &**(right: Expr[G])(implicit origin: Origin): Expr[G] = (left, right) match {
      case (BooleanValue(true), BooleanValue(true)) => tt[G]
      case (BooleanValue(true), e) => e
      case (e, BooleanValue(true)) => e
      case _ => Star[G](left, right)
    }
  }

  var program: Program[Pre] = null

  var replaceThis: ScopedStack[(ThisObject[Pre], Expr[Post])] = ScopedStack()

  val currentComponent: ScopedStack[BipComponent[Pre]] = ScopedStack()
  val currentBipDeclaration: ScopedStack[Declaration[Pre]] = ScopedStack()

  lazy val classes = program.transSubnodes.collect { case c: Class[Pre] => c }.toIndexedSeq
  lazy val components = classes.collect { case ClassBipComponent(cls, component) => component }
  lazy val allPorts = classes.flatMap { cls => cls.declarations.collect { case port: BipPort[Pre] => port } }
  lazy val transitionToClassComponent: Map[BipTransition[Pre], (Class[Pre], BipComponent[Pre])] = classes.collect {
    case ClassBipComponent(cls, component) =>
      cls.declarations.collect { case transition: BipTransition[Pre] => (transition, (cls, component)) }
  }.flatten.toMap
  lazy val dataToClass: Map[BipData[Pre], Class[Pre]] = classes.flatMap { cls =>
    cls.declarations.collect { case data: BipData[Pre] => (data, cls) }
  }.toMap
  lazy val guardToClass: Map[BipGuard[Pre], Class[Pre]] = classes.flatMap { cls =>
    cls.declarations.collect { case guard: BipGuard[Pre] => (guard, cls) }
  }.toMap
  lazy val componentToClass: Map[BipComponent[Pre], Class[Pre]] = classes.collect {
    case ClassBipComponent(cls, component) => (component, cls)
  }.toMap
  lazy val procedureToClassComponent: Map[Procedure[Pre], (Class[Pre], BipComponent[Pre])] = classes.collect {
    case ClassBipComponent(cls, component) => component.constructors.map { case Ref(constructor) =>
      (constructor, (cls, component))
    }
  }.flatten.toMap
  lazy val portToComponent: Map[BipPort[Pre], BipComponent[Pre]] = components.flatMap { component =>
    classOf(component).declarations.collect { case p: BipPort[Pre] => (p, component) }
  }.toMap

  def classOf(c: BipComponent[Pre]): Class[Pre] = componentToClass(c)
  def classOf(p: BipPort[Pre]): Class[Pre] = componentToClass(portToComponent(p))
  def classOf(g: BipGuard[Pre]): Class[Pre] = guardToClass(g)
  def classOf(p: Procedure[Pre]): Class[Pre] = procedureToClassComponent(p)._1
  def classOf(t: BipTransition[Pre]): Class[Pre] = transitionToClassComponent(t)._1
  def classOf(d: BipData[Pre]): Class[Pre] = dataToClass(d)
  def componentOf(p: Procedure[Pre]): BipComponent[Pre] = procedureToClassComponent(p)._2
  def componentOf(t: BipTransition[Pre]): BipComponent[Pre] = transitionToClassComponent(t)._2
  def isComponentConstructor(p: Procedure[Pre]): Boolean = procedureToClassComponent.contains(p)

  val guardSucc: SuccessionMap[BipGuard[Pre], InstanceMethod[Post]] = SuccessionMap()
  val transitionSucc: SuccessionMap[BipTransition[Pre], InstanceMethod[Post]] = SuccessionMap()
  val outgoingDataSucc: SuccessionMap[BipOutgoingData[Pre], InstanceMethod[Post]] = SuccessionMap()
  val synchronizationSucc: SuccessionMap[BipTransitionSynchronization[Pre], Procedure[Post]] = SuccessionMap()

  sealed trait IncomingDataContext
  // The incoming data is stored in some variable
  case class RewriteToVariableContext(m: Map[BipIncomingData[Pre], Variable[Post]]) extends IncomingDataContext
  // The incoming data must be acquired by calling an outgoing data on a certain object, likely some variable, but we keep it abstract here.
  // The important part is that, in the post ast, the type of the expr supports the method resulting from the outgoing data.
  case class RewriteToOutgoingDataContext(m: Map[BipIncomingData[Pre], (Expr[Post], BipOutgoingData[Pre])]) extends IncomingDataContext

  val incomingDataContext: ScopedStack[IncomingDataContext] = ScopedStack()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    program = p
    super.dispatch(p)
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case thisObj: ThisObject[Pre] => replaceThis.topOption match {
      case Some((otherThis, res)) if thisObj == otherThis => res
      case None => thisObj.rewrite()
    }

    case l @ BipLocalIncomingData(Ref(data)) => incomingDataContext.top match {
      case RewriteToVariableContext(m) => m(data).get(l.o)
      case RewriteToOutgoingDataContext(m) => methodInvocation(
        obj = m(data)._1,
        ref = outgoingDataSucc.ref[Post, InstanceMethod[Post]](m(data)._2),
        blame = PanicBlame("Querying outgoing data here cannot fail")
      )(l.o)
    }

    // TODO (RR): I actually want the "this" not hardcoded in the bip guard invocation, as actually the this is contextual, so why put it in a node...
    case invocation @ BipGuardInvocation(obj, Ref(guard)) =>
      methodInvocation(
        obj = dispatch(obj),
        ref = guardSucc.ref[Post, InstanceMethod[Post]](guard),
        // Sneakily reuse the encoding we define above by retrofitting the refs from the guard definition into a pre-AST expression
        args = guard.data.map { ref => BipLocalIncomingData[Pre](ref)(invocation.o) }.map(dispatch),
        blame = PanicBlame("Guard invocation cannot fail as it can only depend on component invariant")
        )(expr.o)

    case inv @ BipGuardInvocation(_, _) =>
      // Bip guard invocations can only be done by bip transitions, or inside synchrons
      throw Unreachable(inv.o.messageInContext("Bug: the following guard is unexpectedly called outside transition context"))

    case _ => rewriteDefault(expr)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    // Should be implemented through synchronizations within this pass, so they should be dropped
    case port: BipPort[Pre] => port.drop()

    // Should be implemented as arguments passed to update functions within this pass, so they should be dropped
    case id: BipIncomingData[Pre] => id.drop()

    case data: BipOutgoingData[Pre] =>
      implicit val o = DiagnosticOrigin
      assert(data.pure)

      currentBipDeclaration.having(data) {
        outgoingDataSucc(data) = classDeclarations.declare(new InstanceMethod[Post](
          dispatch(data.t),
          Seq(),
          Seq(),
          Seq(),
          Some(dispatch(data.body)),
          contract = contract[Post](ForwardUnsatisfiableBlame(null /* not needed */, data),
            requires = UnitAccountedPredicate(dispatch(currentComponent.top.invariant))),
          pure = true,
        )(PanicBlame("Postcondition of data cannot fail")))
      }

    // Is encoded in contracts, so dropped
    case sp: BipStatePredicate[Pre] => sp.drop()

    case component: BipComponent[Pre] =>
      component.drop()

    case guard: BipGuard[Pre] =>
      implicit val o = guard.o
      val incomingDataVariables = ListMap.from(guard.data.map { case Ref(data) => (data, new Variable(dispatch(data.t))) })
      labelDecls.scope {
        currentBipDeclaration.having(guard) {
          incomingDataContext.having(RewriteToVariableContext(incomingDataVariables)) {
            guardSucc(guard) = classDeclarations.declare(new InstanceMethod[Post](
              TBool()(guard.o),
              incomingDataVariables.values.toSeq,
              Nil, Nil,
              Some(dispatch(guard.body)),
              contract[Post](ForwardUnsatisfiableBlame(null /* not needed */, guard),
                requires = UnitAccountedPredicate(dispatch(currentComponent.top.invariant))),
              pure = guard.pure
            )(PanicBlame("Postcondition of guard cannot fail"))(guard.o))
          }
        }
      }

    case ClassBipComponent(cls, component) =>
      currentComponent.having(component) {
        rewriteDefault(cls)
      }

    case proc: Procedure[Pre] if isComponentConstructor(proc) =>
      val component = componentOf(proc)
      results.declare(component)
      implicit val o = DiagnosticOrigin
      currentBipDeclaration.having(component) {
        withResult { res: Result[Post] =>
          val subst = (ThisObject[Pre](classOf(proc).ref)(DiagnosticOrigin), res)
          val contract = proc.contract.rewrite(
            ensures =
            // Establish component invariant
            SplitAccountedPredicate(UnitAccountedPredicate(replaceThis.having(subst) {
              dispatch(component.invariant)
            }),
            // Establish state invariant
            SplitAccountedPredicate(UnitAccountedPredicate(replaceThis.having(subst) {
              dispatch(component.initial.decl.expr)
            }),
            // Also include everything that was generated extra for the constructor
            dispatch(proc.contract.ensures)))
          )
          globalDeclarations.succeed(proc,
            proc.rewrite(contract = contract, blame = ConstructorPostconditionFailed(results, component, proc)))
        }
      }

    case transition: BipTransition[Pre] =>
      implicit val o = DiagnosticOrigin

      val component = currentComponent.top
      // Mark that the default case is that verification is succesfull
      results.declare(component, transition)

      val incomingDataToVariable = ListMap.from(transition.data.map { case Ref(data) =>
        (data, new Variable(dispatch(data.t)))
      })

      labelDecls.scope {
        currentBipDeclaration.having(transition) {
          transitionSucc(transition) = classDeclarations.declare(new InstanceMethod[Post](
            TVoid(),
            incomingDataToVariable.values.toSeq,
            Nil,
            Nil,
            Some(dispatch(transition.body)),
            contract[Post](ForwardUnsatisfiableBlame(results, transition),
              requires = UnitAccountedPredicate(
                dispatch(component.invariant) &**
                  dispatch(transition.source.decl.expr) &**
                  incomingDataContext.having(RewriteToVariableContext(incomingDataToVariable)) {
                    dispatch(transition.guard) &**
                    dispatch(transition.requires)
                  }),
              ensures =
                // Establish component invariant
                SplitAccountedPredicate(UnitAccountedPredicate(dispatch(component.invariant)),
                // Establish state invariant
                SplitAccountedPredicate(UnitAccountedPredicate(dispatch(transition.target.decl.expr)),
                // Establish update function postcondition
                UnitAccountedPredicate(dispatch(transition.ensures))))
            )
          )(TransitionPostconditionFailed(results, transition))(transition.o))
        }
      }

    case synchronization: BipTransitionSynchronization[Pre] =>
      synchronization.drop()
      currentBipDeclaration.having(synchronization) {
        implicit val o = DiagnosticOrigin
        val transitions = synchronization.transitions.map { case Ref(t) => t }

        /* Each transition that participates in the synchronization, applies to a specific component.
           We model those components as COL classes. Each synchronization is modeled as a procedure
           that takes for each participating component, an argument to a class instance representing that component.
           We define those variables/arguments here.
         */
        val varOf: Map[Declaration[Pre], Local[Post]] = transitions.flatMap { transition =>
          val v = new Variable[Post](TClass(succ[Class[Post]](classOf(transition))))(
            SynchronizationComponentVariableOrigin(synchronization, componentOf(transition)))
          Seq((transition, v.get), (classOf(transition), v.get))
        }.toMap

        /* - Pre:
             - Of each class var of transition:
               - Non null
               - Component invariant
               - State invariant
               - Guard
          */
        val preconditionComponents: Expr[Post] = foldStar(synchronization.transitions.map { case Ref(transition) =>
          val clsThisSubst = (ThisObject[Pre](classOf(transition).ref)(DiagnosticOrigin), varOf(transition))

          replaceThis.having(clsThisSubst) {
            (varOf(transition) !== Null()) &**
              dispatch(componentOf(transition).invariant) &**
              dispatch(transition.source.decl.expr)
          }
        })

        /* Here we construct a mapping of incoming data to invocations of the outgoing data methods, as indicated by data wires
           This is needed because in the precondition the variables for exchange of data are not yet assigned, so we just execute
           the guards over literal outgoing data invocations. These are then used when guard invocations are translated
         */
        val incomingDataTranslation = ListMap.from(synchronization.wires.map {
          case BipGlueDataWire(Ref(out), Ref(in)) => (in, (varOf(classOf(out)), out))
        })

        val preconditionGuards: Expr[Post] = foldStar(synchronization.transitions.map { case Ref(transition) =>
          val clsThisSubst = (ThisObject[Pre](classOf(transition).ref)(DiagnosticOrigin), varOf(transition))
          replaceThis.having(clsThisSubst) {
            incomingDataContext.having(RewriteToOutgoingDataContext(incomingDataTranslation)) {
              dispatch(transition.guard)
            }
          }
        })

        /*
        - Evaluate data wires and assign to var representing output data. Make mapping from data wire to output data var
         */
        // TODO (RR): Was doing some mind bendy thing by reusing the translation nicely. Not sure if it's smart to try to be smart. Finish this later.
//        val wireResults = synchronization.wires.map { case BipGlueDataWire(Ref(dataOut), Ref(dataIn)) =>
//          val v = new Variable[Post](dispatch(dataOut.t))(dataIn.o)
//
//          val init = assignLocal[Post](v.get, methodInvocation(
//            obj = varOf(classOf(dataOut)),
//            ref = outgoingDataSucc.ref[Post, InstanceMethod[Post]](dataOut),
//            blame = PanicBlame("Precondition of outgoing data cannot fail"))(dataOut.o))
//
//          (dataIn, v, init)
//        }
//        val initBlock: Seq[Assign[Post]] = wireResults.map { case (_, _, init) => init }

        val incomingDataToVariableContext = ListMap.from(synchronization.wires.map { case BipGlueDataWire(Ref(dataOut), Ref(dataIn)) =>
          (dataIn, new Variable[Post](dispatch(dataOut.t))(dataIn.o))
        })
        val initBlock = incomingDataContext.having(RewriteToOutgoingDataContext(incomingDataTranslation)) {
          incomingDataToVariableContext.keys.map { dataIn =>
            // Sneakily again reuse some translation conveniently defined entirely someplace else.
            assignLocal[Post](incomingDataToVariableContext(dataIn).get, dispatch(BipLocalIncomingData(dataIn.ref)))
          }
        }

        /* Construct another mapping, this time using the wire vars from directly above.
           Important, because below also the precondition might refer to incomingdata variables.
         */
//        val incomingDataToVariableContext = RewriteToVariableContext(ListMap.from(wireResults.map { case (dataIn, v, _) => (dataIn, v) }))

        /*
        - Of each class var of transition:
          - Exhale, with class var & data wires substituted in:
            - component invariant
            - source state invariant
          - Exhale, also with substitutions, containing precondition.
            - Separate so we can detect the error easily
        */
        val exhales: Seq[Exhale[Post]] = synchronization.transitions.flatMap { case Ref(transition) =>
          val clsThisSubst = (ThisObject[Pre](classOf(transition).ref)(DiagnosticOrigin), varOf(transition))

          incomingDataContext.having(RewriteToVariableContext(incomingDataToVariableContext)) {
            val regularExhale = Exhale(replaceThis.having(clsThisSubst) { foldStar(
              Seq(dispatch(componentOf(transition).invariant), dispatch(transition.source.decl.expr)) :+
                  dispatch(transition.guard)
            )})(PanicBlame("Component invariant, state invariant, and transition guard should hold"))

            val preconditionExhale = Exhale(replaceThis.having(clsThisSubst) {
                  dispatch(transition.requires) // TODO: Need to dispatch on all blames here as well?
              })(ExhalingTransitionPreconditionFailed(results, synchronization, transition))

            Seq(regularExhale, preconditionExhale)
            // TODO: switch to the bottom one when bug is fixed. For that, the replaceThis/incomingDataSubstitutions/some other concerns system needs to be cleaned up.
  //          Seq(preconditionExhale, regularExhale)
          }
        }

        /*
          - Inhale, with class var & data wires substituted in:
            - component invariant
            - target state invariant
            - postcondition
        */
        val inhales: Seq[Inhale[Post]] = synchronization.transitions.map { case Ref(transition) =>
          val clsThisSubst = (ThisObject[Pre](classOf(transition).ref)(DiagnosticOrigin), varOf(transition))

          Inhale(replaceThis.having(clsThisSubst) {
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
        val postcondition: Expr[Post] = foldStar(synchronization.transitions.map { case Ref(transition) =>
          val clsThisSubst = (ThisObject[Pre](classOf(transition).ref)(DiagnosticOrigin), varOf(transition))

          replaceThis.having(clsThisSubst) {
            dispatch(componentOf(transition).invariant) &**
            dispatch(transition.target.decl.expr)
          }
        })

        synchronizationSucc(synchronization) =
          globalDeclarations.declare(procedure(
            args = transitions.map(varOf(_).ref.decl),
            body = Some(
              Scope(incomingDataToVariableContext.m.values.toSeq, Block(initBlock ++ exhales ++ inhales))),
            requires = UnitAccountedPredicate(preconditionComponents &** preconditionGuards),
            ensures = UnitAccountedPredicate(postcondition),
            blame = PanicBlame("Can this contract fail?"),
            contractBlame = PanicBlame("Can it be unsatisfiable?")
          )(BipSynchronizationOrigin(synchronization)))
      }

    case _: BipPortSynchronization[Pre] => throw Unreachable("Should be translated away at this point")

    case _ => rewriteDefault(decl)
  }

  // If we're inside a bip transition, wrap each blame into a blame that, when activated/blamed, writes true in the big map of transition verification results
  override def dispatch[T <: VerificationFailure](blame: Blame[T]): Blame[T] = currentBipDeclaration.topOption match {
    case Some(bt: BipTransition[Pre]) => ExecuteOnBlame(blame) { results.report(bt, UpdateFunctionFailure) }
    case Some(bt: BipComponent[Pre]) => ExecuteOnBlame(blame) { results.report(bt, ConstructorFailure) }
    case _ => super.dispatch(blame)
  }
}
