package vct.col.ast.temporaryimplpackage.declaration

import vct.col.ast.Declaration
import vct.col.check.{CheckContext, CheckError, TypeError, TypeErrorText}
import vct.col.coerce.{CoercingRewriter, NopCoercingRewriter}
import vct.col.ref.{DirectRef, Ref}
import vct.col.rewrite.ScopeContext

import scala.reflect.ClassTag

trait DeclarationImpl { this: Declaration =>
  def succeedDefault(scope: ScopeContext, pred: Declaration): Unit = {
    declareDefault(scope)
    scope.successionMap(pred) = this
  }

  def declareDefault(scope: ScopeContext): Unit

  /**
    * Create a Ref to this declaration. This is often useful in a place where the type of the ref can be directly
    * inferred, e.g. `FunctionInvocation(func.ref, ...)`. The witness to `this.type <:< T` demands that the
    * inferred T at least supports the type of this declaration.
    */
  def ref[T <: Declaration](implicit tag: ClassTag[T], witness: this.type <:< T): Ref[T] = new DirectRef[T](this)

  override def check(context: CheckContext): Seq[CheckError] =
    try {
      NopCoercingRewriter.coerce(this)
      Nil
    } catch {
      case CoercingRewriter.Incoercible(e, t) => Seq(TypeError(e, t))
      case CoercingRewriter.IncoercibleText(e, m) => Seq(TypeErrorText(e, _ => m))
    }
}