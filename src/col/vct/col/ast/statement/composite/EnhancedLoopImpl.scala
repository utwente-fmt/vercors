package vct.col.ast.statement.composite

import vct.col.ast._
import vct.col.check.CheckContext
import vct.col.origin.Origin
import vct.col.print
import vct.col.print._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError
import vct.col.ast.ops.EnhancedLoopOps

trait EnhancedLoopImpl[G] extends EnhancedLoopOps[G] { this: EnhancedLoop[G] =>

  override def enterCheckContextScopes(context: CheckContext[G]): Seq[CheckContext.ScopeFrame[G]] =
    context.withScope(Seq(arg), toScan = Seq(body))

  override def layout(implicit ctx: Ctx): Doc = {
    Group(Text("for") <+> "(" <> Nest(NonWsLine <> arg.show <+> ":" <+> iter.show) </> Text(")")) <+> body.layoutAsBlock
  }
}