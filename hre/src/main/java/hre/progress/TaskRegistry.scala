package hre.progress

import hre.perf.{Profile, ResourceUsage}
import hre.progress.task.{RootTask, Task}

import scala.collection.mutable.ArrayBuffer

case object TaskRegistry {
  private var mainThreadId = -1L
  private var mainThreadRootTask: Option[RootTask] = None
  val threadTaskStack: ThreadLocal[ArrayBuffer[Task]] = ThreadLocal.withInitial(() => ArrayBuffer())

  def install(): Unit = {
    mainThreadId = Thread.currentThread().getId
    mainThreadRootTask = Some(RootTask())
    getRootTask.start()
  }

  def finish(): Unit = {
    getRootTask.end()
  }

  def abort(): Unit = {
    getRootTask.abort()
  }

  def isMainThread: Boolean = Thread.currentThread().getId == mainThreadId
  def getRootTask: RootTask = mainThreadRootTask.get

  /**
   * SAFETY: If called from a non-main thread, a task with an explicit superTask must be started first. This is for
   * example the case when a thread is started as Progress.{foreach,map}(xs.par, _) { ... }: the superTask is
   * establised in the thread setting up the parallel run.
   */
  def currentTaskInThread: Task =
    threadTaskStack.get().last

  def ownUsage(): ResourceUsage =
    if(isMainThread) ResourceUsage.getProcess.get
    else ResourceUsage.getCallingThread.getOrElse(ResourceUsage.zero)

  def reportUsage(usage: ResourceUsage, trail: Seq[String]): Unit = {
    Profile.update(trail, usage, doUpdateChildUsage = isMainThread)
  }
}
