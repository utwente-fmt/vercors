package vct.col.ast.util

import vct.col.ast
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast._
import vct.col.check.{AbstractPredicate, CheckContext, CheckError, NotAPredicateApplication}
import vct.col.resolve.{RefInstancePredicate, RefPredicate}

import scala.annotation.tailrec

trait CheckFoldUnfoldTarget[G] extends NodeFamilyImpl[G] { this: NodeFamily[G] =>
  def res: Expr[G]

  private def checkNonAbstract(predicate: ast.AbstractPredicate[G], blame: Expr[G]): Option[CheckError] =
    predicate.body match {
      case None => Some(AbstractPredicate(blame))
      case Some(_) => None
    }

  @tailrec
  private def check(e: Expr[G]): Option[CheckError] = e match {
    case Scale(_, res) => check(res)
    case apply: ApplyAnyPredicate[G] => checkNonAbstract(apply.ref.decl, apply)
    case inv: PVLInvocation[G] => inv.ref.get match {
      case RefPredicate(decl) => checkNonAbstract(decl, inv)
      case RefInstancePredicate(decl) => checkNonAbstract(decl, inv)
      case _ => Some(NotAPredicateApplication(e))
    }
    case inv: JavaInvocation[G] => inv.ref.get match {
      case RefPredicate(decl) => checkNonAbstract(decl, inv)
      case RefInstancePredicate(decl) => checkNonAbstract(decl, inv)
      case _ => Some(NotAPredicateApplication(e))
    }
    case inv: CInvocation[G] => inv.ref.get match {
      case RefPredicate(decl) => checkNonAbstract(decl, inv)
      case RefInstancePredicate(decl) => checkNonAbstract(decl, inv)
      case _ => Some(NotAPredicateApplication(e))
    }
    case _ => Some(NotAPredicateApplication(e))
  }

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++ check(res).toSeq
}
