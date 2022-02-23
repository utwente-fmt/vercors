package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.temporaryimplpackage.expr.ExprImpl
import vct.col.ast._
import vct.col.check.{CheckContext, CheckError, NotAHeapLocation}
import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, JavaTypeNameTarget, RefADTFunction, RefAxiomaticDataType, RefClass, RefField, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefJavaClass, RefJavaField, RefJavaLocalDeclaration, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefUnloadedJavaNamespace, RefVariable, SpecDerefTarget, SpecInvocationTarget, SpecNameTarget, SpecTypeNameTarget}

trait LocatorImpl[G] extends ExprImpl[G] { this: Locator[G] =>
  def loc: Expr[G]

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++ (loc match {
      case _: Deref[G] => Nil
      case deref: JavaDeref[G] => deref.ref.get match {
        case RefModelField(_) => Nil
        case RefJavaField(_, _) => Nil
        case _ => Seq(NotAHeapLocation(this))
      }
      case deref: PVLDeref[G] => deref.ref.get match {
        case RefModelField(_) => Nil
        case RefField(_) => Nil
        case _ => Seq(NotAHeapLocation(this))
      }
      case deref: CStructAccess[G] => deref.ref.get match {
        case RefModelField(_) => Nil
        case _ => Seq(NotAHeapLocation(this))
      }
      case _: SilverDeref[G] => Nil

      case name: JavaLocal[G] => name.ref.get match {
        case RefModelField(_) => Nil
        case RefJavaField(_, _) => Nil
        case _ => Seq(NotAHeapLocation(this))
      }
      case name: PVLLocal[G] => name.ref.get match {
        case RefModelField(decl) => Nil
        case RefField(decl) => Nil
        case _ => Seq(NotAHeapLocation(this))
      }

      case _: ArraySubscript[G] => Nil
      case sub: AmbiguousSubscript[G] if sub.isArrayOp => Nil

      case _ => Seq(NotAHeapLocation(this))
    })
}
