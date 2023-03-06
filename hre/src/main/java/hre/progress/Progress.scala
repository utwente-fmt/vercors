package hre.progress

import hre.progress.task.SimpleNamedTask

import scala.collection.parallel.CollectionConverters.IterableIsParallelizable

case object Progress {
  def install(forceProgress: Boolean): Unit = {
    TaskRegistry.install()
    Layout.install(forceProgress)
  }

  def update(): Unit = Layout.update()

  def foreach[T](xs: Iterable[T], desc: T => String)(f: T => Unit): Unit = {
    val superTask = TaskRegistry.mostRecentlyStartedTaskInThread.get()
    xs.foreach(x => {
      SimpleNamedTask(superTask, desc(x)).frame {
        f(x)
      }
    })
  }

  def parForeach[T](xs: Iterable[T], desc: T => String)(f: T => Unit): Unit = {
    val superTask = TaskRegistry.mostRecentlyStartedTaskInThread.get()
    xs.par.foreach { x =>
      SimpleNamedTask(superTask, desc(x)).frame {
        f(x)
      }
    }
  }

  def map[T, S](xs: Iterable[T], desc: T => String)(f: T => S): Iterable[S] = {
    val superTask = TaskRegistry.mostRecentlyStartedTaskInThread.get()
    xs.map(x => {
      SimpleNamedTask(superTask, desc(x)).frame {
        f(x)
      }
    })
  }
}