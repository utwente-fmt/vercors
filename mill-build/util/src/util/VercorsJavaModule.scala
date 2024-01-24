package util

import mill.scalalib.Dep
import mill.{Agg, PathRef, T, pathReadWrite}

trait VercorsJavaModule extends JavaModule with ReleaseModule { outer =>
  def key: String
  def deps: T[Agg[Dep]]
  def sourcesDir = T { settings.src / key }
  override def sources = T.sources { sourcesDir() }
  def packedResources = T.sources { settings.res / key }
  override def docResources = T.sources { settings.docs / key }
  override def unmanagedClasspath = T {
    if(os.exists(settings.lib / key))
      Agg.from(os.list(settings.lib / key).filter(_.ext == "jar").map(PathRef(_)))
    else Agg.empty
  }
  override def ivyDeps = settings.deps.common ++ deps()

  override def classPathFileElements = T { runClasspath().map(_.path.toString) }
}