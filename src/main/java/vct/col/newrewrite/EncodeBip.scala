package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.newrewrite.EncodeBip.{BipGuardInvocationFailed, BipTransitionPostconditionFailed, IsBipComponent}
import vct.col.origin.{BipGuardInvocationFailure, Blame, CallableFailure, ContextEverywhereFailedInPre, DiagnosticOrigin, InstanceInvocationFailure, Origin, PanicBlame, PostconditionFailed, PreconditionFailed}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

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

//  case class ContextEverywherePreconditionFailed(inv: InvokingNode[_]) extends Blame[PreconditionFailed] {
  case class BipGuardInvocationFailed(transition: BipTransition[_]) extends Blame[InstanceInvocationFailure] {
    override def blame(error: InstanceInvocationFailure): Unit =
      transition.blame.blame(BipGuardInvocationFailure(transition, error))
//      inv.blame.blame(ContextEverywhereFailedInPre(error.failure, inv))
  }

  case class BipTransitionPostconditionFailed(transition: BipTransition[_]) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =

//      transition.blame.blame(BipGuardInvocationFailure(transition, error))
    //      inv.blame.blame(ContextEverywhereFailedInPre(error.failure, inv))
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

  override def dispatch(p: Program[Pre]): Program[Post] = {
    p.subnodes.foreach {
      case IsBipComponent(cls, bc) =>
        bc.constructors.foreach { p => procConstructorInfo(p.decl) = (cls, bc) }
      case _ =>
    }

    super.dispatch(p)
  }


  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case thisObj: ThisObject[Pre] => replaceThis.topOption match {
      case Some((otherThis, res)) if thisObj == otherThis => res
      case None => thisObj.rewrite()
    }
    case _ => rewriteDefault(expr)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case id: BipIncomingData[Pre] => id.drop()
    case od: BipOutgoingData[Pre] => od.drop()
    case sp: BipStatePredicate[Pre] => sp.drop()
    case component: BipComponent[Pre] => component.drop()
    case guard: BipGuard[Pre] => guard.drop()

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
      withResult { res: Result[Post] =>
        val subst = (ThisObject[Pre](cls.ref)(DiagnosticOrigin), res)
        val contract = proc.contract.rewrite(
          ensures = SplitAccountedPredicate(
            UnitAccountedPredicate(replaceThis.having(subst) { dispatch(component.invariant) })(DiagnosticOrigin),
            dispatch(proc.contract.ensures))(DiagnosticOrigin)
        )
        proc.rewrite(contract = contract).succeedDefault(proc)
      } (DiagnosticOrigin)

    case bt: BipTransition[Pre] =>
      implicit val o = DiagnosticOrigin
      assert(bt.guard.isEmpty)
      val component = currentComponent.top
      new InstanceMethod[Post](
        // TODO: guards
        TVoid(),
        collectInScope(variableScopes) { bt.data.map(_._2).foreach(dispatch) },
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
                // For each @Data that the guard needs, find the appropriate @Data paramter from the transition
                val vars = bg.data.map { case (dataRef, _) => bt.data.find(dataRef.decl == _._1.decl).get._2 }
                methodInvocation(
                  BipGuardInvocationFailed(bt),
                  ThisObject(succ[Class[Post]](currentClass.top)),
                  succ[InstanceMethod[Post]](f.decl),
                  args = vars.map(succ[Variable[Post]]).map(Local[Post](_)))
              }.getOrElse(tt))
          ),
          ensures = UnitAccountedPredicate(
            dispatch(component.invariant)
              &** dispatch(bt.target.decl.expr)
              &** dispatch(bt.ensures)
          )
        )
//      )(bt.blame.asInstanceOf[Blame[CallableFailure]] /* TODO: FISHY! */)(bt.o).succeedDefault(bt)
        // TODO: Need to relatethe "UnitAccounted" things I make above in the blame handler here...? EncodeArrayValues is probably a good example to look at
        )(BipTransitionPostconditionFailed(bt))(bt.o).succeedDefault(bt)

    case _ => rewriteDefault(decl)
  }
}
