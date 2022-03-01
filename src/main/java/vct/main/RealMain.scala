package vct.main

import vct.options.{Mode, Options}
import vct.result.VerificationResult
import vct.test.CommandLineTesting

case object RealMain {
  def main(args: Array[String]): Unit = try {
    Options.parse(args) match {
      case None => // usage was printed
      case Some(options) => selectMode(options)
    }
  } catch {
    case err: VerificationResult.SystemError =>
      println(err.text)
      err.printStackTrace()
  }

  def selectMode(options: Options): Unit =
    options.mode match {
      case Mode.Verify => Vercors(options).go()
      case Mode.VeyMont => ???
      case Mode.BatchTest => ???
    }
}
