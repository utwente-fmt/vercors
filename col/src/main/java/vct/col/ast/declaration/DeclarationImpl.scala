package vct.col.ast.declaration

import vct.col.ast.Declaration
import vct.col.check.{CheckContext, CheckError, TypeError, TypeErrorText}
import vct.col.debug.Dropped
import vct.col.ref.{DirectRef, Ref}
import vct.col.rewrite.{InitialGeneration, ScopeContext}
import vct.col.typerules.{CoercingRewriter, NopCoercingRewriter}

import scala.reflect.ClassTag

trait DeclarationImpl[G] { this: Declaration[G] =>
  def drop(): Unit = debugRewriteState = Dropped

  /**
    * Create a Ref to this declaration. This is often useful in a place where the type of the ref can be directly
    * inferred, e.g. `FunctionInvocation(func.ref, ...)`. The witness to `this.type <:< T` demands that the
    * inferred T at least supports the type of this declaration.
    */
  def ref[Decl <: Declaration[G]](implicit tag: ClassTag[Decl], witness: this.type <:< Decl): Ref[G, Decl] = new DirectRef[G, Decl](this)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    try {
      NopCoercingRewriter().coerce(this.asInstanceOf[Declaration[InitialGeneration]])
      Nil
    } catch {
      case CoercingRewriter.Incoercible(e, t) => Seq(TypeError(e, t))
      case CoercingRewriter.IncoercibleText(e, m) => Seq(TypeErrorText(e, _ => m))
    }
}