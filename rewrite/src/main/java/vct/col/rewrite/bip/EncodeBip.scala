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

import scala.collection.mutable

// TODO (RR): Proof obligation that if a port is enabled, only one transition can ever be enabled

case object EncodeBip extends RewriterBuilderArg[VerificationResults] {
  override def key: String = "encodeBip"
  override def desc: String = "encodes BIP semantics explicitly"

  object IsBipComponent {
    def unapply[G](cls: Class[G]): Option[(Class[G], BipComponent[G])] = {
      cls.declarations.collectFirst({
        case bc: BipComponent[G] => (cls, bc)
      })
    }
  }

  case class BipGuardInvocationFailed(transition: BipTransition[_]) extends Blame[InstanceInvocationFailure] {
    override def blame(error: InstanceInvocationFailure): Unit = error match {
      case ctx: InstanceNull => PanicBlame("Guard invoked by BIP transition can never cause instance null error").blame(ctx)
      case PreconditionFailed(_, failure, _) => transition.blame.blame(BipGuardInvocationFailure(failure, transition))
      case ContextEverywhereFailedInPre(failure, _) => transition.blame.blame(BipGuardInvocationFailure(failure, transition))
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

  case class ConstructorPostconditionFailed(results: VerificationResults, component: BipComponent[_], proc: Procedure[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case cf: ContractedFailure => cf match {
        case PostconditionFailed(Seq(FailLeft), failure, _) => // Failed establishing component invariant
          results.report(component, ComponentInvariantNotMaintained)
          proc.blame.blame(BipComponentInvariantNotEstablished(failure, component))
        case PostconditionFailed(Seq(FailRight, FailLeft), failure, _) => // Failed establishing state invariant
          results.report(component, StateInvariantNotMaintained)
          proc.blame.blame(BipStateInvariantNotEstablished(failure, component))
        case PostconditionFailed(FailRight +: FailRight +: path, failure, node) => // Failed postcondition
          results.report(component, PostconditionNotVerified)
          proc.blame.blame(PostconditionFailed(path, failure, node))
        // TODO (RR): Probably should disallow contracts on constructor?
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

  case class GuardPostconditionFailed(guard: BipGuard[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case cf: ContractedFailure => cf match {
        case PostconditionFailed(_, failure, node) => guard.blame.blame(BipGuardPostconditionFailure(failure, guard))
        case ContextEverywhereFailedInPost(failure, node) => PanicBlame("BIP guard does not have context everywhere")
      }

      // These are all impossible...?
      case SignalsFailed(failure, node) => ???
      case ExceptionNotInSignals(node) => ???
      case failure: BipTransitionFailure => ???
      case failure: BipGuardFailure => ???
    }
  }

  case class ForwardUnsatisfiableBlame(node: Node[_]) extends Blame[NontrivialUnsatisfiable] {
    node match {
      case _: BipGuard[_] | _: BipTransition[_] | _: BipOutgoingData[_] =>
      case _ => throw Unreachable("Can only construct this blame with bip guard, bip transition, bip outgoing data")
    }

    override def blame(error: NontrivialUnsatisfiable): Unit = node match {
      case g: BipGuard[_] => g.blame.blame(BipGuardPreconditionUnsatisfiable(g))
      case t: BipTransition[_] => t.blame.blame(BipTransitionPreconditionUnsatisfiable(t))
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

  var procConstructorInfo: mutable.Map[Procedure[Pre], (Class[Pre], BipComponent[Pre])] = mutable.Map()
  var replaceThis: ScopedStack[(ThisObject[Pre], Result[Post])] = ScopedStack()
  val currentComponent: ScopedStack[BipComponent[Pre]] = ScopedStack()
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()
  val currentBipDeclaration: ScopedStack[Declaration[Pre]] = ScopedStack()
  // TODO (RR): Make these vars lazy so construction only happens if there's bip stuff present in the ast?
  var portToComponent: Map[BipPort[Pre], BipComponent[Pre]] = Map()
  var portToTransitions: Map[BipPort[Pre], Seq[BipTransition[Pre]]] = Map()
  var componentToClass: Map[BipComponent[Pre], Class[Pre]] = Map()


  val guardSucc: SuccessionMap[BipGuard[Pre], InstanceMethod[Post]] = SuccessionMap()
  val transitionSucc: SuccessionMap[BipTransition[Pre], InstanceMethod[Post]] = SuccessionMap()
  val incomingDataSucc: SuccessionMap[(Declaration[Pre], BipIncomingData[Pre]), Variable[Post]] = SuccessionMap()
  val outgoingDataSucc: SuccessionMap[BipOutgoingData[Pre], InstanceMethod[Post]] = SuccessionMap()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    p.subnodes.foreach {
      case IsBipComponent(cls, component) =>
        component.constructors.foreach { p => procConstructorInfo(p.decl) = (cls, component) }
        componentToClass = componentToClass.updated(component, cls)
      case _ =>
    }

    val ports = p.subnodes.collect { case port: BipPort[Pre] => port }

    portToComponent = ports.map { port =>
      val component = p.subnodes.collectFirst {
        case IsBipComponent(cls, component) if cls.declarations.contains(port) => component
      }.get
      port -> component
    }.toMap

    portToTransitions = ports.map { port =>
      val transitions: Seq[BipTransition[Pre]] = p.subnodes.collect {
        case transition: BipTransition[Pre] if transition.port.decl == port => transition
      }
      port -> transitions
    }.toMap

    super.dispatch(p)
  }


  override def dispatch(expr: Expr[Pre]): Expr[Post] = (expr, currentBipDeclaration.topOption) match {
    case (thisObj: ThisObject[Pre], _) => replaceThis.topOption match {
      case Some((otherThis, res)) if thisObj == otherThis => res
      case None => thisObj.rewrite()
    }
    case (l @ BipLocalIncomingData(Ref(data)), Some(decl)) =>
      Local(incomingDataSucc.ref[Post, Variable[Post]]((decl, data)))(l.o)
    case (inv @ BipGuardInvocation(Ref(guard)), Some(bt: BipTransition[Pre])) =>
      MethodInvocation(
        obj = ThisObject(succ[Class[Post]](currentClass.top))(expr.o),
        ref = guardSucc.ref[Post, InstanceMethod[Post]](guard),
        args = guard.data.map{ case Ref(data) =>
          Local(incomingDataSucc.ref[Post, Variable[Post]](bt, data))(inv.o)
        },
        Nil, Nil, Nil, Nil
      )(BipGuardInvocationFailed(bt))(expr.o)
    case (BipGuardInvocation(_), _) =>
      ??? // Bip guard invocations can only be done by bip transitions
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
          contract = contract[Post](ForwardUnsatisfiableBlame(data),
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
      labelDecls.scope {
        currentBipDeclaration.having(guard) {
          guardSucc(guard) = classDeclarations.declare(new InstanceMethod[Post](
            TBool()(guard.o),
            variables.collect {
              guard.data.foreach { case Ref(data) =>
                incomingDataSucc((guard, data)) = variables.declare(new Variable(dispatch(data.t)))
              }
            }._1,
            Nil, Nil,
            Some(dispatch(guard.body)),
            contract[Post](ForwardUnsatisfiableBlame(guard),
              requires = UnitAccountedPredicate(dispatch(currentComponent.top.invariant)),
              ensures = UnitAccountedPredicate(dispatch(guard.ensures))
            ),
            pure = guard.pure
          )(GuardPostconditionFailed(guard))(guard.o))
        }
      }

    case cls: Class[Pre] =>
      currentClass.having(cls) {
        cls.declarations.collectFirst { case bc: BipComponent[Pre] => bc } match {
          case Some(component) => currentComponent.having(component) {
            rewriteDefault(cls)
          }
          case None => rewriteDefault(cls)
        }
      }

    case proc: Procedure[Pre] if procConstructorInfo.contains(proc) =>
      val (cls, component) = procConstructorInfo(proc)
      results.declare(component)
      implicit val o = DiagnosticOrigin
      withResult { res: Result[Post] =>
        val subst = (ThisObject[Pre](cls.ref)(DiagnosticOrigin), res)
        val contract = proc.contract.rewrite(
          ensures =
            // Establish component invariant
            SplitAccountedPredicate(UnitAccountedPredicate(replaceThis.having(subst) { dispatch(component.invariant) } ),
            // Establish state invariant
            SplitAccountedPredicate(UnitAccountedPredicate(replaceThis.having(subst) { dispatch(component.initial.decl.expr) }),
            // Also include everything that was generated extra for the constructor
            dispatch(proc.contract.ensures)))
        )
        globalDeclarations.succeed(proc,
          proc.rewrite(contract = contract, blame = ConstructorPostconditionFailed(results, component, proc)))
      }

    case transition: BipTransition[Pre] =>
      implicit val o = DiagnosticOrigin

      val component = currentComponent.top
      // Mark that the default case is that verification is succesfull
      results.declare(component, transition)

      labelDecls.scope {
        currentBipDeclaration.having(transition) {
          transitionSucc(transition) = classDeclarations.declare(new InstanceMethod[Post](
            TVoid(),
            variables.collect {
              transition.data.foreach { case Ref(data) =>
                incomingDataSucc((transition, data)) = variables.declare(new Variable(dispatch(data.t)))
              }
            }._1,
            Nil,
            Nil,
            Some(dispatch(transition.body)),
            contract[Post](dispatch(ForwardUnsatisfiableBlame(transition)),
              requires = UnitAccountedPredicate(
                dispatch(component.invariant)
                  &** dispatch(transition.source.decl.expr)
                  &** dispatch(transition.requires)
                  &** dispatch(transition.guard)
              ),
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

    case synchronization: BipSynchronization[Pre] =>
      synchronization.drop()
      logger.warn(s"Dropping synchronization at ${synchronization.o.shortPosition}")

    case _ => rewriteDefault(decl)
  }

  // If we're inside a bip transition, wrap each blame into a blame that, when activated/blamed, writes true in the big map of transition verification results
  override def dispatch[T <: VerificationFailure](blame: Blame[T]): Blame[T] = currentBipDeclaration.topOption match {
    case Some(bt: BipTransition[Pre]) => ExecuteOnBlame(blame) { results.report(bt, UpdateFunctionFailure) }
    case _ => super.dispatch(blame)
  }

//  def generateSynchronization(synchron: BipSynchron[Pre],
//                              component1: BipComponent[Pre], transition1: BipTransition[Pre],
//                              component2: BipComponent[Pre], transition2: BipTransition[Pre]): Unit = {
//    // Ensure that 1 is sending to 2, and not otherwise
//    if (transition1.data.nonEmpty) {
//      throw Unreachable("Sending transition cannot (yet) have incoming data")
//    }
//
//    // Find getter on sender that provides data, if present
//    // val sendingData = transition2.data.map(incomingDataToOutgoing(_.data.decl))
//
//    implicit val o: Origin = DiagnosticOrigin
//    val cls1 = componentToClass(component1)
//    val cls2 = componentToClass(component2)
//    val c = new Variable[Post](TClass(succ[Class[Post]](cls1)))
//    val d = new Variable[Post](TClass(succ[Class[Post]](cls2)))
//
//    // Can do substitute, and then a plain dispatch. Or: just do it inline. Or: a nested rewriter.
//    component1.invariant
//
////    synchronSucc(synchron) = globalDeclarations.declare(procedure[Post](
////      args = ???,
////      requires = UnitAccountedPredicate[Post]((c.get !== Null()) && (d.get !== Null())),
////      ensures = ???,
////      body = Some(???),
////      blame = ???,
////      contractBlame = ???
////    )(synchron.o))
//  }
}
