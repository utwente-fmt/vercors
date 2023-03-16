package vct.col.ast.declaration.singular

import vct.col.ast.BipTransitionSignature

trait BipTransitionSignatureImpl[G] { this: BipTransitionSignature[G] =>
  def shortSignature: String = s"($portName, $sourceStateName, $targetStateName${", " + textualGuard.getOrElse("no guard")})"
  def asciiSignature: String = s"port_${portName}_from_${sourceStateName}_to_$targetStateName${textualGuard.map(g => s"_if_guard").getOrElse("")}"
}
