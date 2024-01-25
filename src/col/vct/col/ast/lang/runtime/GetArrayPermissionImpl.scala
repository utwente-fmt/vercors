package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait GetArrayPermissionImpl[G] {
  this: GetArrayPermission[G] =>

  override def precedence: Int = Precedence.ATOMIC

  override def t: Type[G] = TFraction[G]()
  private def defaultFraction: RuntimeFractionZero[G] = RuntimeFractionZero[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Nest(objectLocation.show <> Text(s".__runtime__$id.get(") <> location <> Text(").getOrDefault(") <> threadId.show <> Text(",") <+> defaultFraction.show <> Text(")"))
}