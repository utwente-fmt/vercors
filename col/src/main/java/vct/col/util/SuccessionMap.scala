package vct.col.util

import hre.util.{FuncTools, ScopedStack}
import vct.col.ast.Declaration
import vct.col.debug.Succeeded
import vct.col.ref.LazyRef
import vct.col.util.SuccessionMap.NoSuchSuccessor
import vct.result.VerificationResult.{SystemError, UserError}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

case object SuccessionMap {
  case class Scope(var idx: Int = 0, var triggerIdx: Option[Int] = None) {
    def next(): Int = this.synchronized {
      val result = idx

      if(triggerIdx.nonEmpty && triggerIdx.get == result) {
        throw MissingPredecessor
      }

      idx += 1
      result
    }
  }

  val scopes: ScopedStack[Scope] = ScopedStack()

  def breakOnMissingPredecessor[T](f: => T): T = {
    scopes.having(Scope()) {
      try {
        f // First computation
      } catch {
        case NoSuchSuccessor(_, _, debugIdx) =>
          println("== Error; restarting computation to determine cause. ==")
          scopes.top.triggerIdx = Some(debugIdx)
          scopes.top.idx = 0

          try {
            f // Second computation, expect throw MissingPredecessor
          } catch {
            case MissingPredecessor => throw MissingPredecessor
            case _: Throwable => throw NonDeterminsticEvaluation
          }

          throw NonDeterminsticEvaluation
      }
    }
  }

  case object NonDeterminsticEvaluation extends SystemError {
    override def text: String = "We tried to derive the creation of a reference pointing to a successor that will not be populated, " +
      "but the second computation does not yield the same error. Is the function supplied to breakOnMissingPredecessor not deterministic?"
  }

  case object MissingPredecessor extends SystemError {
    override def text: String = "Stack trace for the location that creates a reference for a successor that will not be populated."
  }

  case class NoSuchSuccessor[K, V <: Declaration[_]](map: SuccessionMap[K, V], missingKey: K, debugIdx: Int) extends SystemError {
    override def text: String = s"Key not found: $missingKey"
  }
}

case class SuccessionMap[K, V <: Declaration[_]]() {
  private val storages: ArrayBuffer[mutable.HashMap[K, V]] = ArrayBuffer()
  private val localStorage: ThreadLocal[mutable.HashMap[K, V]] =
    ThreadLocal.withInitial(() => storages.synchronized {
      val store = mutable.HashMap[K, V]()
      storages += store
      store
    })

  private def storage: mutable.HashMap[K, V] = localStorage.get()

  def get(k: K): Option[V] = storages.synchronized {
    FuncTools.firstOption(storages.toSeq, (store: mutable.HashMap[K, V]) => store.get(k))
  }

  def apply(k: K): V = get(k).get

  def getOrElseUpdate(k: K, v: => V): V = storages.synchronized {
    get(k).getOrElse {
      storage(k) = v
      v
    }
  }

  def update(k: K, v: V): Unit = {
    k match {
      case decl: Declaration[_] => decl.debugRewriteState = Succeeded
      case _ =>
    }

    storage.update(k, v)
  }

  def contains(k: K): Boolean = get(k).isDefined

  def ref[G, V2 <: Declaration[G]](k: K)(implicit tag: ClassTag[V2], witness: V <:< Declaration[G]): LazyRef[G, V2] = {
    val debugIdx = SuccessionMap.scopes.topOption.map(_.next()).getOrElse(-1)

    new LazyRef[G, V2](
      get(k) match {
        case Some(value) => value
        case None => throw NoSuchSuccessor(this, k, debugIdx)
      }
    )
  }
}
