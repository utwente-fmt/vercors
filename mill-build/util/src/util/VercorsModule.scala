package util

import mill.scalalib.{DepSyntax, TestModule}
import mill.{Agg, T, pathReadWrite}

trait VercorsModule extends ScalaModule with VercorsJavaModule { outer =>
  trait Tests extends ScalaTests with TestModule.ScalaTest with VercorsJavaModule {
    def key = outer.key
    override def sourcesDir = T { settings.test / key }
    override def sources = T.sources { sourcesDir() }
    def deps = T { Agg.empty }
    override def ivyDeps = settings.deps.common ++ Agg(ivy"org.scalatest::scalatest:3.2.7") ++ outer.deps() ++ deps()
  }
}