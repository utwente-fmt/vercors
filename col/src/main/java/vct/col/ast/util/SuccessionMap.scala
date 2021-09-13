package vct.col.ast.util

import vct.col.ast.{Declaration, LazyRef, Ref}

import scala.collection.mutable
import scala.reflect.ClassTag

case class SuccessionMap[K, V <: Declaration]()(implicit tag: ClassTag[V]) extends mutable.HashMap[K, V] {
  def ref(k: K): LazyRef[V] = new LazyRef[V](this(k))
}
