package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text, Empty}

trait CodeStringCheckPermissionExprImpl[G] {
  this: CodeStringCheckPermissionExpr[G] =>

  override def t: Type[G] = TBool[G]()

  def layoutPermission(implicit ctx: Ctx): Doc = {
    permission match {
      case _: WritePerm[G] => Text("== 1")
      case _: ReadPerm[G] => Text("> 0")
      case d : Div[G] => Text(" == Fraction.getFraction(") <> d.left.show <> "," <+> d.right.show <> ")"
      case _ => Empty
    }
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Doc.arg(objectLocation) <> Text(s".get($id)") <> Text(".get(Thread.currentThread().getId()") <> layoutPermission)
}