package vct.col.ast.statement.composite

import vct.col.ast._
import vct.col.check.CheckContext
import vct.col.origin.Origin
import vct.col.print
import vct.col.print._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

trait EnhancedLoopImpl[G] { this: EnhancedLoop[G] =>

  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    context.withScope(Seq(arg), toScan = Seq(body))

  override def layout(implicit ctx: Ctx): Doc = {
    Group(Text("for") <+> "(" <> Nest(NonWsLine <> arg.show <+> ":" <+> iter.show) </> Text(")")) <+> body.layoutAsBlock
  }
}