package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.newrewrite.EncodeBip.{BipGuardInvocationFailed, ConstructorPostconditionFailed, GuardPostconditionFailed, IsBipComponent, TransitionPostconditionFailed}
import vct.col.origin.{BipComponentInvariantNotEstablished, BipComponentInvariantNotMaintained, BipGuardFailure, BipGuardInvocationFailure, BipGuardPostconditionFailure, BipStateInvariantNotEstablished, BipStateInvariantNotMaintained, BipTransitionFailure, BipTransitionPostconditionFailure, Blame, CallableFailure, ContextEverywhereFailedInPost, ContextEverywhereFailedInPre, ContractedFailure, DiagnosticOrigin, ExceptionNotInSignals, FailLeft, FailRight, InstanceInvocationFailure, InstanceNull, InvocationFailure, Origin, PanicBlame, PostconditionFailed, PreconditionFailed, SignalsFailed}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable

import scala.collection.immutable.ListMap
import scala.collection.mutable

// TODO (RR): Proof obligation that if a port is enabled, only one transition can ever be enabled

case object EncodeBip extends RewriterBuilder {
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

  /* TODO (RR): The next three classes seem repetetive. Can probably factor out a common core,
      e.g., handle postcondition failed, panic on the rest? That does hurt understandability.
   */
  case class TransitionPostconditionFailed(transition: BipTransition[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case cf: ContractedFailure => cf match {
        case PostconditionFailed(Seq(FailLeft), failure, _) =>
          transition.blame.blame(BipComponentInvariantNotMaintained(failure, transition))
        case PostconditionFailed(Seq(FailRight, FailLeft), failure, _) =>
          transition.blame.blame(BipStateInvariantNotMaintained(failure, transition))
        case PostconditionFailed(Seq(FailRight, FailRight), failure, _) =>
          transition.blame.blame(BipTransitionPostconditionFailure(failure, transition))
        case ctx: ContextEverywhereFailedInPost => PanicBlame("BIP transition does not have context everywhere").blame(ctx)
      }
      case ctx: SignalsFailed => PanicBlame("BIP transition does not have signals").blame(ctx)
      case ctx: ExceptionNotInSignals => PanicBlame("BIP transition does not have signals").blame(ctx)
    }
  }

  case class ConstructorPostconditionFailed(component: BipComponent[_], proc: Procedure[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case cf: ContractedFailure => cf match {
        case PostconditionFailed(Seq(FailLeft), failure, _) => // Failed establishing component invariant
          proc.blame.blame(BipComponentInvariantNotEstablished(failure, component))
        case PostconditionFailed(Seq(FailRight, FailLeft), failure, _) => // Failed establishing state invariant
          proc.blame.blame(BipStateInvariantNotEstablished(failure, component))
        case PostconditionFailed(FailRight +: FailRight +: path, failure, node) => // Failed postcondition
          proc.blame.blame(PostconditionFailed(path, failure, node))
        // TODO (RR): Probably should disallow contracts on constructor?
        case ctx: ContextEverywhereFailedInPost => proc.blame.blame(ctx)
      }
      case ctx: SignalsFailed => proc.blame.blame(ctx)
      case ctx: ExceptionNotInSignals => proc.blame.blame(ctx)
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
      case ExceptionNotInSignals(failure, node) => ???
      case failure: BipTransitionFailure => ???
      case failure: BipGuardFailure => ???
    }
  }
}

case class EncodeBip[Pre <: Generation]() extends Rewriter[Pre] {

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
  // TODO (RR): Make these vars lazy so construction only happens if there's bip stuff present in the ast?
  var portToComponent: ListMap[BipPort[Pre], BipComponent[Pre]] = ListMap()
  var portToTransitions: ListMap[BipPort[Pre], Seq[BipTransition[Pre]]] = ListMap()
  var componentToClass: ListMap[BipComponent[Pre], Class[Pre]] = ListMap()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    p.subnodes.foreach {
      case IsBipComponent(cls, component) =>
        component.constructors.foreach { p => procConstructorInfo(p.decl) = (cls, component) }
        componentToClass = componentToClass.updated(component, cls)
      case _ =>
    }

    val ports = p.subnodes.collect { case port: BipPort[Pre] => port }

    portToComponent = ListMap.newBuilder(ports.map { port =>
      val component = p.subnodes.collectFirst {
        case IsBipComponent(cls, component) if cls.declarations.contains(port) => component
      }.get
      port -> component
    }).result()

    portToTransitions = ListMap.newBuilder(ports.map { port =>
      val transitions: Seq[BipTransition[Pre]] = p.subnodes.collect {
        case transition: BipTransition[Pre] if transition.port.decl == port => transition
      }
      port -> transitions
    }).result()

    super.dispatch(p)
  }


  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case thisObj: ThisObject[Pre] => replaceThis.topOption match {
      case Some((otherThis, res)) if thisObj == otherThis => res
      case None => thisObj.rewrite()
    }
    case BipLocalIncomingData(ref) => Local[Post](succ(ref.decl))(expr.o)
    case _ => rewriteDefault(expr)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    // The next two should be resolved within this pass, so they should be dropped
    case data: BipData[Pre] => data.drop()
    case port: BipPort[Pre] => port.drop()

    case id: BipIncomingData[Pre] => new Variable(dispatch(id.t))(id.o).succeedDefault(id)
    case od: BipOutgoingData[Pre] =>
      // TODO (RR): Encode as instance function
      od.drop()

    // Is encoded in contracts, so dropped
    case sp: BipStatePredicate[Pre] => sp.drop()

    case component: BipComponent[Pre] =>
      component.drop()

    case guard: BipGuard[Pre] =>
      implicit val o = guard.o
      new InstanceMethod[Post](
        TBool()(guard.o),
        collectInScope(this.variableScopes) { guard.data.map(dispatch) },
        Nil, Nil,
        Some(dispatch(guard.body)),
        contract[Post](
          requires = UnitAccountedPredicate(dispatch(currentComponent.top.invariant)),
          ensures = UnitAccountedPredicate(dispatch(guard.ensures))
        ),
        pure = guard.pure
      )(GuardPostconditionFailed(guard))(guard.o).succeedDefault(guard)

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
        proc.rewrite(contract = contract, blame = ConstructorPostconditionFailed(component, proc)).succeedDefault(proc)
      }

    case bt: BipTransition[Pre] =>
      implicit val o = DiagnosticOrigin
      val component = currentComponent.top
      new InstanceMethod[Post](
        TVoid(),
        collectInScope(variableScopes) { bt.data.foreach(dispatch) },
        Nil,
        Nil,
        Some(dispatch(bt.body)),
        contract[Post](
          requires = UnitAccountedPredicate(
            dispatch(component.invariant)
              &** dispatch(bt.source.decl.expr)
              &** dispatch(bt.requires)
              &** (bt.guard.map { f =>
                val bg = f.decl
                // For each @Data that the guard needs, find the appropriate @Data parameter from the transition
                val vars = bg.data.map(guardData => bt.data.find(guardData.data == _.data).get)
                methodInvocation(
                  BipGuardInvocationFailed(bt),
                  ThisObject(succ[Class[Post]](currentClass.top)),
                  succ[InstanceMethod[Post]](f.decl),
                  args = vars.map(succ[Variable[Post]]).map(Local[Post](_)))
              }.getOrElse(tt))
          ),
          ensures =
            // Establish component invariant
            SplitAccountedPredicate(UnitAccountedPredicate(dispatch(component.invariant)),
            // Establish state invariant
            SplitAccountedPredicate(UnitAccountedPredicate(dispatch(bt.target.decl.expr)),
            // Establish update function postcondition
            UnitAccountedPredicate(dispatch(bt.ensures))))
          )
        )(TransitionPostconditionFailed(bt))(bt.o).succeedDefault(bt)

    case synchron: BipSynchron[Pre] =>
      val p1 = synchron.p1.decl
      val p2 = synchron.p2.decl

      val comp1 = portToComponent(p1)
      val comp2 = portToComponent(p2)

      portToTransitions(p1).foreach { transition1 =>
        portToTransitions(p2).foreach { transition2 =>
          generateSynchronization(synchron, comp1, transition1, comp2, transition2)
        }
      }

    case _ => rewriteDefault(decl)
  }

  def generateSynchronization(synchron: BipSynchron[Pre],
                              component1: BipComponent[Pre], transition1: BipTransition[Pre],
                              component2: BipComponent[Pre], transition2: BipTransition[Pre]): Unit = {
    // Ensure that 1 is sending to 2, and not otherwise
    if (transition1.data.nonEmpty) {
      throw Unreachable("Sending transition cannot (yet) have incoming data")
    }

    // Find getter on sender that provides data, if present
    // val sendingData = transition2.data.map(incomingDataToOutgoing(_.data.decl))

    implicit val o: Origin = DiagnosticOrigin
    val cls1 = componentToClass(component1)
    val cls2 = componentToClass(component2)
    val c = new Variable[Post](TClass(succ[Class[Post]](cls1)))
    val d = new Variable[Post](TClass(succ[Class[Post]](cls2)))

    // Can do substitute, and then a plain dispatch. Or: just do it inline. Or: a nested rewriter.
    component1.invariant

    procedure[Post](
      args = ???,
      requires = UnitAccountedPredicate[Post]((c.get !== Null()) && (d.get !== Null())),
      ensures = ???,
      body = Some(???),
      blame = ???
    )(synchron.o).succeedDefault(synchron)
  }
}
