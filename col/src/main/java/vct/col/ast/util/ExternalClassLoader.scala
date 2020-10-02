package vct.col.ast.util

import vct.col.ast.stmt.decl.{ASTClass, NameSpace}

trait ExternalClassLoader {
  def load(name: Seq[String], ns: Option[NameSpace] = None): Option[ASTClass]

  def load(name: Array[String], ns: NameSpace): ASTClass =
    load(name.toSeq, Option(ns)).orNull
}
