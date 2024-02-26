package vct.col.ast.unsorted

import vct.col.ast.{CFunctionTypeExtensionModifier, CTypeAttribute}
import vct.col.ast.ops.CFunctionTypeExtensionModifierOps
import vct.col.print.Doc.fold
import vct.col.print._

trait CFunctionTypeExtensionModifierImpl[G] extends CFunctionTypeExtensionModifierOps[G] { this: CFunctionTypeExtensionModifier[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    val attr = extensions.collect{case a: CTypeAttribute[G] => a}.map(a => Text(a.name) <> "(" <> Doc.args(a.args) <> ")")
    Text("__attribute__ ((") <> fold(attr)(_ <+> _) <> "))"
  }
}
