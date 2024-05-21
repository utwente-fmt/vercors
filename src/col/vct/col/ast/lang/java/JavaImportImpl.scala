package vct.col.ast.lang.java

import vct.col.ast.JavaImport
import vct.col.print.{Ctx, Doc, Text, Empty}
import vct.col.ast.ops.{JavaImportOps, JavaImportFamilyOps}

trait JavaImportImpl[G] extends JavaImportOps[G] with JavaImportFamilyOps[G] { this: JavaImport[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(if(isStatic) "import static" else "import") <+>
      name <> (if(star) Text(".*") else Empty) <> ";"
}