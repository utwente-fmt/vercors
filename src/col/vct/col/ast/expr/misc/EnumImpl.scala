package vct.col.ast.expr.misc

import vct.col.ast._
import vct.col.origin.SourceName
import vct.col.print._
import vct.col.resolve.ctx.RefEnumConstant
import vct.col.ast.ops.EnumOps

trait EnumImpl[G] extends EnumOps[G] {
  this: Enum[G] =>
  def getConstant(name: String): Option[RefEnumConstant[G]] =
    constants.collectFirst {
      case c if c.o.find[SourceName].contains(SourceName(name)) =>
        RefEnumConstant(Some(this), c)
    }

  override def layout(implicit ctx: Ctx): Doc =
    Text("enum") <+> ctx.name(this) <+> "{" <>>
      Doc.stack(constants.map(_.show <> ",")) <+/> "}"
}
