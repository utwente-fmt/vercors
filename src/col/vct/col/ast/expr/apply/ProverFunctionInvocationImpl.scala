package vct.col.ast.expr.apply

import vct.col.ast.ProverFunctionInvocation
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Precedence, Text}

trait ProverFunctionInvocationImpl[G] { this: ProverFunctionInvocation[G] =>
  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(
        Text(ctx.name(ref)) <>
          "("
      ) <> Doc.args(args) <> ")"
    )

}
