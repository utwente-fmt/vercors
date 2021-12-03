package vct.col.util

import vct.col.ast.{Declaration, LazyRef}
import vct.col.util.SuccessionMap.NoSuchSuccessor
import vct.result.VerificationResult.{SystemError, UserError}

import scala.collection.mutable
import scala.reflect.ClassTag

case object SuccessionMap {
  private var counter: Int = 0

  case class NoSuchSuccessor[K, V <: Declaration](map: SuccessionMap[K, V],
                                                  missingKey: K,
                                                  mapTrace: StackTraceElement,
                                                  refTrace: StackTraceElement,
                                                  queryTrace: StackTraceElement,
                                                  count: Int) extends SystemError {
    private def stackTraceString(trace: Seq[StackTraceElement]): String =
      trace.map(element => element.toString).mkString("\n")

    override def text: String =
      "For the map created at: " + mapTrace.toString +
        "\nA lazy reference was created at: " + refTrace.toString +
        "\nBut when it was queried at: " + queryTrace.toString +
        s"\nThe key was not found: $missingKey.\nFor debug purposes the reference number is $count."
  }
}

case class SuccessionMap[K, V <: Declaration]() extends mutable.HashMap[K, V] {
  private def firstElementPastMe: StackTraceElement =
    Thread.currentThread().getStackTrace
      .dropWhile(element =>
        element.getClassName == "vct.col.util.SuccessionMap" ||
          element.getClassName == "java.lang.Thread" ||
          element.getClassName == "vct.col.ast.LazyRef" ||
          element.getMethodName == "succ"
      ).head

  val mapCreationTrace: StackTraceElement = firstElementPastMe

  def ref[V2 <: Declaration]
         (k: K)
         (implicit tag: ClassTag[V2])
         : LazyRef[V2] = {
    val refCreationTrace = firstElementPastMe

    val count = SuccessionMap.synchronized {
      val value = SuccessionMap.counter
      SuccessionMap.counter += 1
      value
    }

    new LazyRef[V2](
      get(k) match {
        case Some(value) => value
        case None => throw NoSuchSuccessor(
          map = this,
          missingKey = k,
          mapTrace = mapCreationTrace,
          refTrace = refCreationTrace,
          queryTrace = firstElementPastMe,
          count = count)
      }
    )
  }
}
