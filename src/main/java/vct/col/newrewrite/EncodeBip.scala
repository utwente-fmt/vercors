package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.newrewrite.EncodeBip.IsBipComponent
import vct.col.origin.DiagnosticOrigin
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
}

case class EncodeBip[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeCurrentThread._

  var procConstructorInfo: mutable.Map[Procedure[Pre], (Class[Pre], BipComponent[Pre])] = mutable.Map()
  var replaceThis: ScopedStack[(ThisObject[Pre], Result[Post])] = ScopedStack()

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
    case bt: BipTransition[Pre] => bt.drop()
    case id: BipIncomingData[Pre] => id.drop()
    case od: BipOutgoingData[Pre] => od.drop()
    case sp: BipStatePredicate[Pre] => sp.drop()
    case component: BipComponent[Pre] => component.drop()
    case guard: BipGuard[Pre] => guard.drop()

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

    case _ => rewriteDefault(decl)
  }
}
