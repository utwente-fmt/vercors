package vct.col.resolve

import vct.col.ast.JavaNamespace

trait ExternalJavaLoader {
  def load[G](name: Seq[String]): Option[JavaNamespace[G]]
  def loadPkg[G](name: Seq[String]): Seq[JavaNamespace[G]]
}
