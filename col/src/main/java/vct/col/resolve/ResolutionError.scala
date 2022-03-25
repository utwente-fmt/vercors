package vct.col.resolve

import vct.col.ast._
import vct.col.origin._
import vct.result.VerificationError.{SystemError, UserError}

trait ResolutionError extends UserError

case class NoSuchNameError(kind: String, name: String, use: Node[_]) extends ResolutionError {
  override def text: String = use.o.messageInContext(s"Could not find $kind named $name.")
  override def code: String = "noSuchName"
}

case class NoSuchConstructor(use: Node[_]) extends ResolutionError {
  override def text: String = use.o.messageInContext("Could not find a constructor matching the supplied arguments.")
  override def code: String = "noSuchConstructor"
}

case class NameLost(o: Origin) extends SystemError {
  override def text: String = o.messageInContext("The origin of this node no longer carries its original name.")
}

case class AnonymousMethodsUnsupported(decl: CDeclarator[_]) extends ResolutionError {
  override def text: String = decl.o.messageInContext("Anonymous function declarations are not supported.")
  override def code: String = "anonFunction"
}

case class HasNoFields(obj: Expr[_]) extends ResolutionError {
  override def text: String = obj.o.messageInContext("This object has no fields.")
  override def code: String = "hasNoFields"
}

case class NotApplicable(obj: Expr[_]) extends ResolutionError {
  override def text: String = obj.o.messageInContext("It is not possible to call this object")
  override def code: String = "notApplicable"
}

case class ResultOutsideMethod(res: AmbiguousResult[_]) extends ResolutionError {
  override def text: String = res.o.messageInContext("\\result may not occur outside a method.")
  override def code: String = "resultOutsideMethod"
}

case class NoGivenYields(invocation: Node[_]) extends ResolutionError {
  override def code: String = "noGivenYields"
  override def text: String = invocation.o.messageInContext("This kind of application or invocation does not take parameters via given or yields.")
}