package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.result.VerificationError.Unreachable

trait PredicateEqualsImpl[G] {
  this: PredicateEquals[G] =>


  def getExpression(implicit ctx: Ctx): Doc = {
    currentObject match {
      case d: Deref[G] => d.obj match {
        case l: Local[G] => l.ref.decl.t match {
          case cls: TClass[G] => Group(Text("") <> currentObject <> Text(".equals(") <> arg <> Text(")"))
          case _ => Group(Text("") <> currentObject <> Text("==") <> arg <> Text(")"))
        }
        case _ => Group(Text("") <> currentObject <> Text("==") <> arg <> Text(")"))
      }
      case _ => Group(Text("") <> currentObject <> Text("==") <> arg <> Text(")"))
    }
  }

  override def layout(implicit ctx: Ctx): Doc = {
    Group(Text("if(") <+> getExpression <+> "){" <+> Group(Text("return false;") <+/> "}"))
  }
}