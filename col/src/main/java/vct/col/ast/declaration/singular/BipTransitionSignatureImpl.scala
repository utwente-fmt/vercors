package vct.col.ast.declaration.singular

import vct.col.ast.BipTransitionSignature

trait BipTransitionSignatureImpl[G] { this: BipTransitionSignature[G] =>
  def shortSignature: String = s"($portName, $sourceStateName, $targetStateName, ${textualGuard.getOrElse("no guard")}"
}
