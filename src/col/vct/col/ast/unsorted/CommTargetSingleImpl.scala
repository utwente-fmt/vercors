package vct.col.ast.unsorted

import vct.col.ast.{CommTargetEndpoint, CommTargetIndex, CommTargetSingle, Endpoint}
import vct.col.ref.Ref

trait CommTargetSingleImpl[G] { this: CommTargetSingle[G] =>
  def ref: Ref[G, Endpoint[G]]
}
