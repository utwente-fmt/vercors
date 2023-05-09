package vct.debug

import vct.col.ast.Verification
import vct.col.print
import vct.col.print.{Cons, Ctx, Doc, Empty, Group, Line, Nest, NodeDoc, NonWsLine, Text}
import vct.main.stages.Transformation
import vct.options.types.PathOrStd

import scala.collection.mutable.ArrayBuffer

object TransformationDiffChain {

}

class TransformationDiffChain(out: PathOrStd) extends Transformation.Log {
  import TransformationDiffChain._

  private val baseCtx = Ctx(width = 60)

  var log: ArrayBuffer[(String, Doc)] = ArrayBuffer()

  override def accept(afterPass: String, verification: Verification[_]): Unit = {
    implicit val ctx: Ctx = baseCtx.namesIn(verification)
    log += ((afterPass, verification.show))
  }

  override def finish(): Unit = {
    for((key, doc) <- log) {
      println(key)
      println(doc.toStringWithContext(baseCtx))
    }
  }
}
