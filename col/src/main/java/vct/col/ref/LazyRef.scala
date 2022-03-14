package vct.col.ref

import vct.col.ast.Declaration
import vct.col.ref

import scala.reflect.ClassTag

class LazyRef[G, Decl <: Declaration[G]](lazyDecl: => Declaration[G])(implicit tag: ClassTag[Decl]) extends Ref[G, Decl] {
  // Sometimes Nothing ends up in Decl, which is never useful, so we try to crash a bit earlier when that happens.
  require(tag != ClassTag.Nothing)

  // Capture lazyDecl into a lambda, so that lazyDecl is not implicitly added as a field in the LazyRef class.
  private var computeDecl: () => Declaration[G] = () => lazyDecl

  // Make decl lazy, so its evaluation is delayed, but only performed once.
  lazy val decl: Decl = {
    val result = computeDecl()
    // Clear out the value of computeDecl once we have computed the declaration. If we wouldn't do this, we would create
    // long chains of LazyRef(() => LazyRef(() => LazyRef(...).decl).decl).decl, which would be hard on the garbage
    // collector.
    computeDecl = null
    result match {
      case decl: /*tagged*/ Decl => decl
      case other => throw ref.MistypedRef(other, tag)
    }
  }
}
