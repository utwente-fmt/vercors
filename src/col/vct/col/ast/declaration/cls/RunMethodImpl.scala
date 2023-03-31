package vct.col.ast.declaration.cls

import vct.col.ast.RunMethod
import vct.col.print._

trait RunMethodImpl[G] { this: RunMethod[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("run") <> body.map(Text(" ") <> _.layoutAsBlock).getOrElse(Text(";"))
}
