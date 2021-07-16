package vct.col.ast.util

import vct.col.ast.stmt.decl.{ASTClass, NameSpace}

/**
 * Generic interface to obtain external classes, e.g. specified by import. Only used by the Java frontend.
 */
trait ExternalClassLoader {
  /**
   * Attempt to import a class by (potentially fully qualified) name in an (optional) namespace
   *
   * @param name Either a single name (Seq("String")) or a fully qualfied name (Seq("java", "lang", "String"))
   * @param ns   The namespace of the importing class
   * @return The class, if found.
   */
  def load(name: Seq[String], ns: Option[NameSpace] = None): Option[ASTClass]

  def load(name: Array[String], ns: NameSpace): ASTClass =
    load(name.toSeq, Option(ns)).orNull
}
