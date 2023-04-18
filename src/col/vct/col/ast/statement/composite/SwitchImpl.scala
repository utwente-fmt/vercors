package vct.col.ast.statement.composite

import vct.col.ast.{Case, DefaultCase, Switch}
import vct.col.print._

import scala.annotation.tailrec

trait SwitchImpl[G] { this: Switch[G] =>
  def isCase(s: Show): Boolean = s match {
    case DefaultCase() | Case(_) => true
    case _ => false
  }

  def isNotCase(s: Show): Boolean = !isCase(s)

  @tailrec
  private def layoutContentWithCaseLabel(content: Seq[Show], acc: Show)(implicit ctx: Ctx): Show = {
    if(content.isEmpty) acc
    else {
      val label = content.head
      val more = content.tail
      val (uncased, casePrefix) = more.span(isNotCase)
      val newAcc = acc.show <+/> label <> Nest(Line <> Doc.stack(uncased))
      layoutContentWithCaseLabel(casePrefix, newAcc)
    }
  }

  def layoutContent(implicit ctx: Ctx): Doc = {
    val elements = body.blockElementsForLayout
    val (uncased, casePrefix) = elements.span(isNotCase)

    if(casePrefix.isEmpty) {
      // PB: very weird switch, or we just can't peek through blocks
      Line <> Doc.stack(uncased)
    } else {
      Nest(Line <> Doc.stack(uncased)) <> layoutContentWithCaseLabel(casePrefix, Empty)
    }
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("switch") <+> "(" <> Doc.arg(expr) <> ")") <+> "{" <> Nest(layoutContent) <+/> "}"
}