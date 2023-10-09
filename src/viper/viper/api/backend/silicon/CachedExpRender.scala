package viper.api.backend.silicon

import viper.silver.ast.Node

import scala.collection.mutable

case object CachedExpRender {
  private val cache: mutable.WeakHashMap[Node, String] = mutable.WeakHashMap()

  def apply(e: Node): String =
    cache.getOrElseUpdate(e, e.toString())
}
