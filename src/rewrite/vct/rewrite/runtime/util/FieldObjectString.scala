package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.{Class, Declaration, Deref, InstanceField, Local, Program, ThisObject}
import vct.result.VerificationError.Unreachable


case class FieldObjectString[G]() {

  val instanceField: ScopedStack[String] = ScopedStack()

  def determineObjectReference(d: Deref[G]): String = {
    d.obj match {
      case _: ThisObject[G] => "this"
      case local: Local[G] => dispatchLocal(local)
      case deref: Deref[G] => s"${determineObjectReferenceRecursive(deref)}"
      case _ => "unknown"
    }
  }

  private def determineObjectReferenceRecursive(d: Deref[G]): String = {
    d.obj match {
      case _: ThisObject[G] => s"this.${d.ref.decl.o.getPreferredNameOrElse()}"
      case local: Local[G] => dispatchLocal(local)
      case deref: Deref[G] => s"${determineObjectReferenceRecursive(deref)}.${dispatchDeref(d)}"
      case _ => "unknown"
    }
  }

  private def dispatchLocal(local: Local[G]): String = {
    local.ref.decl.o.getPreferredNameOrElse()
  }

  private def dispatchDeref(deref: Deref[G]): String = {
    deref.ref.decl.o.getPreferredNameOrElse()
  }


}
