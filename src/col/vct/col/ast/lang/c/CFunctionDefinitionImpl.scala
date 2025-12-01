package vct.col.ast.lang.c

import vct.col.ast.{CFunctionDefinition, CGpgpuKernelSpecifier, Node}
import vct.col.ast.ops.CFunctionDefinitionOps
import vct.col.check.{CheckContext}
import vct.col.print.{Ctx, Doc, Group}

trait CFunctionDefinitionImpl[G] extends CFunctionDefinitionOps[G] {
  this: CFunctionDefinition[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Doc.spread(specs) <>> declarator) <+> body.layoutAsBlock,
    ))

  override def checkContextRecursor[T](
      context: CheckContext[G],
      f: (CheckContext[G], Node[G]) => T,
  ): Seq[T] = {
    val gpuKernel =
      specs.collectFirst { case k: CGpgpuKernelSpecifier[G] => () }.isDefined

    Seq(
      f(enterCheckContext(context).withGPUKernel(gpuKernel), contract),
      f(enterCheckContext(context), declarator),
      f(enterCheckContext(context), body),
    ) ++ specs.map(f(enterCheckContext(context), _))
  }
}
