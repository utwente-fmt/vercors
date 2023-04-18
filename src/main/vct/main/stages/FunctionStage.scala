package vct.main.stages

import hre.stages.Stage

case class FunctionStage[T, S](f: T => S) extends Stage[T, S] {
  override def friendlyName: String = "..."
  override def progressWeight: Int = 1
  override def run(in: T): S = f(in)
}
