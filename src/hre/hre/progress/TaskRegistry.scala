package hre.progress

import hre.middleware.{Middleware, MiddlewareObject}
import hre.perf.{Profile, ResourceUsage}
import hre.progress.task.{AbstractTask, RootTask, Task}

import scala.collection.mutable.ArrayBuffer

case object TaskRegistry extends MiddlewareObject[TaskRegistry] {
  def enabled: Boolean = installed

  def isMainThread: Boolean = Thread.currentThread().getId == get.mainThreadId
  def rootTask: Option[RootTask] = get.mainThreadRootTask
  def getRootTask: RootTask = rootTask.get

  /** SAFETY: If called from a non-main thread, a task with an explicit
    * superTask must be started first. This is for example the case when a
    * thread is started as Progress.{foreach,map}(xs.par, _) { ... }: the
    * superTask is established in the thread setting up the parallel run.
    */
  def currentTaskInThread: AbstractTask = get.threadTaskStack.get().last

  def push(task: AbstractTask): Unit =
    if (enabled)
      get.threadTaskStack.get() += task

  def pop(task: AbstractTask): Unit =
    if (enabled)
      if (Thread.currentThread().getId == task.getOwnerThread) {
        assert(get.threadTaskStack.get().lastOption.contains(task))
        get.threadTaskStack.get() -= task
      }

  def ownUsage(): ResourceUsage =
    if (enabled && Profile.installed) {
      if (isMainThread)
        ResourceUsage.getProcess
      else
        ResourceUsage.getCallingThread
    } else
      ResourceUsage.zero

  def reportUsage(usage: ResourceUsage, trail: Seq[String]): Unit =
    Profile.update(trail, usage, doUpdateChildUsage = isMainThread)
}

case class TaskRegistry() extends Middleware {
  private var mainThreadId = -1L
  private var mainThreadRootTask: Option[RootTask] = None
  val threadTaskStack: ThreadLocal[ArrayBuffer[AbstractTask]] = ThreadLocal
    .withInitial(() => ArrayBuffer())

  def install(): Unit =
    synchronized {
      mainThreadId = Thread.currentThread().getId
      mainThreadRootTask = Some(RootTask())
      mainThreadRootTask.get.start()
    }

  def uninstall(): Unit =
    synchronized {
      mainThreadRootTask.foreach(_.abort())
      mainThreadRootTask = None
    }
}
