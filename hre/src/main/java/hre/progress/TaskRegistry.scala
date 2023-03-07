package hre.progress

import hre.perf.{Profile, ResourceUsage}
import hre.progress.task.{RootTask, Task}

case object TaskRegistry {
  private var mainThreadId = -1L
  private var mainThreadRootTask: RootTask = null
  val mostRecentlyStartedTaskInThread: ThreadLocal[Task] = new ThreadLocal()

  def install(): Unit = {
    mainThreadId = Thread.currentThread().getId
    mainThreadRootTask = RootTask()
    mainThreadRootTask.start()
  }

  def finish(): Unit = {
    mainThreadRootTask.end()
  }

  def isMainThread: Boolean = Thread.currentThread().getId == mainThreadId
  def getRootTask: RootTask = mainThreadRootTask

  def ownUsage(): ResourceUsage =
    if(isMainThread) ResourceUsage.getProcess.get
    else ResourceUsage.getCallingThread.get

  def reportUsage(usage: ResourceUsage, trail: Seq[String]): Unit = {
    Profile.update(trail, usage, doUpdateChildUsage = isMainThread)
  }
}
