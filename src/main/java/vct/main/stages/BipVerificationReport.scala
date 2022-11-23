package vct.main.stages

import hre.stages.Stage
import vct.col.rewrite.bip.BIP.Standalone
import vct.col.rewrite.bip.{BIP, BipVerificationResults}

case class BipVerificationReport[T](results: BipVerificationResults) extends Stage[T, BIP.Standalone.VerificationReport] {
  override def friendlyName: String = "Transform BIP verification report from COL form to standalone form"
  override def progressWeight: Int = 1
  override def run(in: T): Standalone.VerificationReport = results.toStandalone()
}
