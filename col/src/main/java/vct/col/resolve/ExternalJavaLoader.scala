package vct.col.resolve

import vct.col.ast.JavaNamespace

import java.nio.file.Path

trait ExternalJavaLoader {
  def load[G](base: Path, name: Seq[String]): Option[JavaNamespace[G]]
  def loadPkg[G](base: Path, name: Seq[String]): Seq[JavaNamespace[G]]
}
