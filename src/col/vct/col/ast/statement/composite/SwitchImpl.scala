package vct.col.ast.statement.composite

import vct.col.ast.{Case, DefaultCase, Switch}
import vct.col.print._

import scala.annotation.tailrec
import vct.col.ast.ops.SwitchOps

trait SwitchImpl[G] extends SwitchOps[G] {
  this: Switch[G] =>
  def isCase(s: Show): Boolean =
    s match {
      case DefaultCase() | Case(_) => true
      case _ => false
    }

  def isNotCase(s: Show): Boolean = !isCase(s)

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("switch") <+> "(" <> Doc.arg(expr) <> ")") <+> "{" <>>
      body.foldBlock(_ <+/> _) <+/> "}"
}
