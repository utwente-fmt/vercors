package vct.test

import java.util.concurrent.{Callable, LinkedBlockingDeque}

import hre.lang.System.Output

import scala.jdk.CollectionConverters._
import scala.collection.mutable

/**
  * Custom thread pool implementation that returns a stream with detailed progress information
  * @param threadCount The number of threads to spin up once started
  * @param tasks The tasks to run
  * @tparam T Task type, should be Callable[?]
  * @tparam S Only there to capture the result type of Callable
  */
case class ThreadPool[T <: Callable[S], S](threadCount: Int, tasks: Seq[T]) {
  val taskQueue: mutable.ArrayBuffer[T] = mutable.ArrayBuffer()
  val taskResult: LinkedBlockingDeque[(T, S, Seq[T], Int)] = new LinkedBlockingDeque()
  // ordered buffer, so the oldest work appears in front
  val runningWork: mutable.ArrayBuffer[T] = mutable.ArrayBuffer()

  private def fetchWork(): Option[T] = this.synchronized {
    if(taskQueue.isEmpty) {
      None
    } else {
      val result = taskQueue.remove(0)
      runningWork += result
      Some(result)
    }
  }

  private def replaceWork(work: T, result: S): Option[T] = this.synchronized {
    runningWork -= work
    val newWork = fetchWork()
    taskResult.push((work, result, runningWork.clone().toSeq, taskQueue.size))
    newWork
  }

  /** Start worker threads in the background */
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

  /** Present the results of the tasks as a stream out of order: returns task, task result, currently running tasks and
    * remaining number of queued tasks */
  def results(): Stream[(T, S, Seq[T], Int)] =
    tasks.indices.toStream.map(_ => taskResult.take())
}
