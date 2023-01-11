package vct.col.ast.expr.apply

import vct.col.ast.{Invocation, Type}

trait InvocationImpl[G] extends ApplyImpl[G] { this: Invocation[G] =>
  override lazy val t: Type[G] = super.t.particularize(ref.decl.typeArgs.zip(typeArgs).toMap)
}