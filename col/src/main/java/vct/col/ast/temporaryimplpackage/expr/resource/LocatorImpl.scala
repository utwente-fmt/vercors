package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.temporaryimplpackage.expr.ExprImpl
import vct.col.ast._
import vct.col.check.{CheckContext, CheckError, NotAHeapLocation}
import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, JavaTypeNameTarget, RefADTFunction, RefAxiomaticDataType, RefClass, RefField, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefJavaClass, RefJavaField, RefJavaLocalDeclaration, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefUnloadedJavaNamespace, RefVariable, SpecDerefTarget, SpecInvocationTarget, SpecNameTarget, SpecTypeNameTarget}

trait LocatorImpl[G] extends ExprImpl[G] { this: Locator[G] =>
  def loc: Expr[G]

  def isHeapLocation(e: Expr[G]): Boolean = e match {
    case InlinePattern(inner) =>
      isHeapLocation(inner)
    case _: Deref[G] => true
    case _: ModelDeref[G] => true
    case deref: JavaDeref[G] => deref.ref.get match {
      case RefModelField(_) => true
      case RefJavaField(_, _) => true
      case _ => false
    }
    case deref: PVLDeref[G] => deref.ref.get match {
      case RefModelField(_) => true
      case RefField(_) => true
      case _ => false
    }
    case deref: CStructAccess[G] => deref.ref.get match {
      case RefModelField(_) => true
      case _ => false
    }
    case _: SilverDeref[G] => true

    case name: JavaLocal[G] => name.ref.get match {
      case RefModelField(_) => true
      case RefJavaField(_, _) => true
      case _ => false
    }
    case name: PVLLocal[G] => name.ref.get match {
      case RefModelField(decl) => true
      case RefField(decl) => true
      case _ => false
    }
    case name: JavaInvocation[G] => name.ref.get match {
      case RefInstancePredicate(_) => true
      case _ => false
    }
    case name: PVLInvocation[G] => name.ref.get match {
      case RefInstancePredicate(_) => true
      case _ => false
    }
    case name: CInvocation[G] => name.ref.get match {
      case RefInstancePredicate(_) => true
      case _ => false
    }

    case _: ApplyAnyPredicate[G] => true

    case _: ArraySubscript[G] => true
    case _: PointerSubscript[G] => true
    case sub: AmbiguousSubscript[G] if sub.isArrayOp || sub.isPointerOp => true

    case _ => false
  }

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++ (if(isHeapLocation(loc)) Nil else Seq(NotAHeapLocation(this)))
}
