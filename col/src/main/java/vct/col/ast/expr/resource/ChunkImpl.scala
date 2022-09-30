package vct.col.ast.expr.resource

import vct.col.ast.{Chunk, TResource}

trait ChunkImpl[G] { this: Chunk[G] =>
  override def t: TResource[G] = TResource()
}
