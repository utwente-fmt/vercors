package vct.col.ast

sealed trait CheckError
case class TypeError(expr: Expr, expectedType: Type) extends CheckError
case class TypeErrorText(expr: Expr, message: Type => String) extends CheckError
case class OutOfScopeError(ref: Ref) extends CheckError
case class IncomparableTypes(left: Expr, right: Expr) extends CheckError

case class CheckContext(scopes: Seq[Set[Declaration]] = Seq(), currentApplicable: Option[Applicable] = None) {
  def withScope(decls: Set[Declaration]): CheckContext =
    CheckContext(scopes :+ decls, currentApplicable)

  def withApplicable(applicable: Applicable): CheckContext =
    CheckContext(scopes, Some(applicable))

  def inScope(ref: Ref): Seq[CheckError] =
    if(scopes.exists(_.contains(ref.decl))) {
      Seq()
    } else {
      Seq(OutOfScopeError(ref))
    }
}

/* list out the varags-like checks explicitly, because we want parameters to be lazily evaluated with "=>" */
abstract class Check(one: => Seq[CheckError] = Seq(),
                     two: => Seq[CheckError] = Seq(),
                     three: => Seq[CheckError] = Seq(),
                     four: => Seq[CheckError] = Seq()) {
  def check(context: CheckContext): Seq[CheckError] =
    one ++ two ++ three ++ four
}

trait NoCheck {
  def check(context: CheckContext): Seq[CheckError] = Nil
}