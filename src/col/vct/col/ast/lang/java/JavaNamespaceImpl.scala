package vct.col.ast.lang.java

import vct.col.ast.{JavaName, JavaNamespace}
import vct.col.origin.DiagnosticOrigin
import vct.col.print._
import vct.col.resolve.lang.Java
import vct.col.ast.ops.JavaNamespaceOps

trait JavaNamespaceImpl[G] extends JavaNamespaceOps[G] { this: JavaNamespace[G] =>
  lazy val name: Option[String] = pkg match {
    case Some(value) => Some(value.names.mkString("."))
    case None => None
  }

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      if(pkg.isEmpty) Empty else Text("package") <+> pkg.get <> ";",
      Doc.stack(imports),
      Doc.stack(declarations),
    ))
}