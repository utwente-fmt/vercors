package vct.col.ast.lang.cpp

import vct.col.ast.{CGpgpuKernelSpecifier, CPPLambdaDefinition, CPPTLambda, Node, Type}
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CPPLambdaDefinitionOps
import vct.col.check.CheckContext

trait CPPLambdaDefinitionImpl[G] extends CPPLambdaDefinitionOps[G] {
  this: CPPLambdaDefinition[G] =>
  override lazy val t: Type[G] = CPPTLambda[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(contract, declarator.show <+> body.layoutAsBlock))


  override def checkContextRecursor[T](
        context: CheckContext[G],
        f: (CheckContext[G], Node[G]) => T,
      ): Seq[T] = {
    val gpuKernel = true
    Seq(
      f(enterCheckContext(context).withGPUKernel(gpuKernel), contract),
      f(enterCheckContext(context), declarator),
      f(enterCheckContext(context), body),
    )
  }
}
