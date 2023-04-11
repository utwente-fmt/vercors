package vct.col.ast.lang

import vct.col.ast.JavaImport
import vct.col.print.{Ctx, Doc, Text, Empty}

trait JavaImportImpl[G] { this: JavaImport[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(if(isStatic) "import static" else "import") <+>
      name <> (if(star) Text(".*") else Empty)
}