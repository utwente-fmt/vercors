package viper.api.backend.silicon

import viper.silver.ast.Node

import scala.collection.mutable

case object CachedExpRender {
  private val cache: ThreadLocal[mutable.WeakHashMap[Node, String]] =
    ThreadLocal.withInitial(() => mutable.WeakHashMap())

  def apply(e: Node): String = cache.get().getOrElseUpdate(e, e.toString())
}
