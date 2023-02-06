package vct.col.rewrite.veymont

import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object AddVeyMontNodes extends RewriterBuilder {

  override def key: String = "addVeyMontNodes"

  override def desc: String = "Add nodes for VeyMont specific structures"
}
case class AddVeyMontNodes[Pre <: Generation]() extends Rewriter[Pre] {

}
