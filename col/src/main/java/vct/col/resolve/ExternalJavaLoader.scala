package vct.col.resolve

import vct.col.ast.JavaNamespace

trait ExternalJavaLoader {
  def load[G](name: Seq[String]): Option[JavaNamespace[G]]
}
