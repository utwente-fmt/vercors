package vct.test

import java.util.concurrent.{Callable, LinkedBlockingDeque}

import hre.lang.System.Output

import scala.collection.JavaConverters._
import scala.collection.mutable

case class ThreadPool[T <: Callable[S], S](threadCount: Int, tasks: Seq[T]) {
  val taskQueue: mutable.ArrayBuffer[T] = mutable.ArrayBuffer()
  val taskResult: LinkedBlockingDeque[(T, S, Seq[T], Int)] = new LinkedBlockingDeque()
  // ordered buffer, so the oldest work appears in front
  val runningWork: mutable.ArrayBuffer[T] = mutable.ArrayBuffer()

  def fetchWork(): Option[T] = this.synchronized {
    if(taskQueue.isEmpty) {
      None
    } else {
      val result = taskQueue.remove(0)
      runningWork += result
      Some(result)
    }
  }

  def replaceWork(work: T, result: S): Option[T] = this.synchronized {
    runningWork -= work
    val newWork = fetchWork()
    taskResult.push((work, result, runningWork.clone(), taskQueue.size))
    newWork
  }

  def start(): Unit = {
    taskQueue ++= tasks
    val threads = (0 until threadCount).map(_ => new Thread(() => {
      var work: Option[T] = fetchWork()
      while(work.nonEmpty) {
        work = replaceWork(work.get, work.get.call())
      }
    }))
    threads.foreach(_.start())
  }

  def results(): Stream[(T, S, Seq[T], Int)] =
    tasks.toStream.map(_ => {
      val resultToPut = taskResult.take()
      resultToPut
    })
}
