package vct.col.util

import vct.col.ast.Declaration
import vct.col.ref.{LazyRef, Ref}

import scala.reflect.ClassTag

case class DeclarationBox[G, Decl <: Declaration[G]]()(
    implicit tag: ClassTag[Decl]
) {
  private var decl: Option[Decl] = None

  def ref: Ref[G, Decl] = new LazyRef(decl.get)

  def isFilled: Boolean = decl.isDefined
  def isEmpty: Boolean = decl.isEmpty

  def get: Decl = decl.get

  def fill[T <: Decl](decl: T): T = {
    this.decl = Some(decl)
    decl
  }
}
