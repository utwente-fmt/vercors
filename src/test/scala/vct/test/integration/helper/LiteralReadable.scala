package vct.test.integration.helper

import hre.io.Readable

import java.io.{Reader, StringReader}

case class LiteralReadable(fileName: String, data: String) extends Readable {
  override def isRereadable: Boolean = true

  override protected def getReader: Reader =
    new StringReader(data)
}
