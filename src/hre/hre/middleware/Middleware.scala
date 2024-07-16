package hre.middleware

import vct.result.VerificationError.SystemError

/** A middleware for VerCors is a concern that is cross-cutting, needs
  * installing and potentially cleanup after finishing, and needs to be
  * accessible globally for convenience.
  *
  * This object accounts all the installed middleware, and in a normal run
  * simply installs them, then uninstalls them in reverse order. If VerCors is
  * killed via a signal, or stops in another abnormal way, we try to unwind the
  * middlewares by calling uninstall in a shutdown hook. This enables behaviours
  * such as being able to recover a profile even when VerCors is cancelled with
  * Ctrl+C.
  *
  * Additionally it enables the main method of VerCors to be more hygienic: save
  * for the shutdown behaviour it is perfectly possible to achieve a similar
  * result by stacking try/finally blocks around the meat and potatoes of
  * VerCors. This way it is a bit more clear that the middleware gets out of the
  * way of the important bits.
  *
  * A middleware consists of two parts: a [[MiddlewareObject]] and a
  * [[Middleware]]. The [[MiddlewareObject]] contains an instance of
  * [[Middleware]] when it is installed: a singleton pattern. This is so that
  * [[Middleware]] does not need to have a fine-grained notion of whether it is
  * installed or not. Nevertheless the [[MiddlewareObject]] can define nice
  * syntactic sugar that addresses the installed [[Middleware]].
  */
object Middleware {
  private var _installed: List[Middleware] = List.empty
  private var _shutdownHook: Option[Thread] = None

  object NotInstalled extends SystemError {
    override def text: String =
      "Attempted to use a middleware that is not installed"
  }

  object IncorrectStructure extends SystemError {
    override def text: String =
      "Middlewares were uninstalled in the wrong order"
  }

  private def uninstallOnShutdown(): Unit =
    Middleware.synchronized {
      for (middleware <- _installed)
        middleware.uninstallOnShutdown()

      _installed = List.empty
    }

  private def ensureHook(): Unit =
    Middleware.synchronized {
      if (_shutdownHook.isEmpty) {
        _shutdownHook = Some(new Thread(
          () => uninstallOnShutdown(),
          "[VerCors] Middleware shutdown hook",
        ))
        Runtime.getRuntime.addShutdownHook(_shutdownHook.get)
      }
    }

  def using[T](
      installers: (Boolean, MiddlewareObject[_ <: Middleware])*
  )(f: => T): T = {
    ensureHook()

    val middlewares = installers.flatMap {
      case (true, installer) =>
        val middleware = installer.instantiate()
        Middleware.synchronized {
          middleware.install()
          _installed = middleware :: _installed
        }
        Seq(middleware)
      case (false, _) => Nil
    }

    try { f }
    finally {
      for (middleware <- middlewares.reverse) {
        Middleware.synchronized {
          // In the case that _installed is empty the shutdownHook has run.
          if (_installed.nonEmpty && _installed.head != middleware)
            throw IncorrectStructure

          _installed = _installed.tail
          middleware.uninstall()
        }
      }
    }
  }
}

/** An installer for a [[Middleware]] that stores a singleton.
  */
trait MiddlewareObject[T <: Middleware] {
  private var _instance: Option[T] = None
  def apply(): T
  private[middleware] def instantiate(): T = {
    _instance = Some(apply())
    _instance.get
  }
  def installed: Boolean = _instance.isDefined
  def instance: Option[T] = _instance
  def get: T = _instance.getOrElse(throw Middleware.NotInstalled)
}

/** A middleware that can be installed and uninstalled.
  *
  * SAFETY: it must be safe to call [[Middleware.uninstall]] concurrently with
  * normal usage of the middleware. This is because the shutdown hook is run
  * concurrently with the normal program (!). Additionally it may be possible
  * that the uninstall call from the shutdown hook is concurrent with the
  * uninstall call from the normal progress of the program.
  */
trait Middleware {
  protected def install(): Unit
  protected def uninstall(): Unit
  protected def uninstallOnShutdown(): Unit = uninstall()
}
