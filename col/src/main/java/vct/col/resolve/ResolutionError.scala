package vct.col.resolve

import vct.col.ast.{AmbiguousResult, CDeclarator, Expr, Node, Origin}
import vct.result.VerificationResult.{SystemError, UserError}

trait ResolutionError extends UserError

case class NoSuchNameError(kind: String, name: String, use: Node) extends ResolutionError {
  override def text: String = use.o.messageInContext(s"Could not find $kind named $name.")
}

case class NameLost(o: Origin) extends SystemError {
  override def text: String = o.messageInContext("The origin of this node no longer carries its original name.")
}

case class AnonymousMethodsUnsupported(decl: CDeclarator) extends ResolutionError {
  override def text: String = decl.o.messageInContext("Anonymous function declarations are not supported.")
}

case class HasNoFields(obj: Expr) extends ResolutionError {
  override def text: String = obj.o.messageInContext("This object has no fields.")
}

case class NotApplicable(obj: Expr) extends ResolutionError {
  override def text: String = obj.o.messageInContext("It is not possible to call this object")
}

case class ResultOutsideMethod(res: AmbiguousResult) extends ResolutionError {
  override def text: String = res.o.messageInContext("\\result may not occur outside a method.")
}