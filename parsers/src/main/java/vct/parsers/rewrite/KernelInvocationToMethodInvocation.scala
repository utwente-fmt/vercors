package vct.parsers.rewrite

import vct.col.ast.expr.KernelInvocation
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.AbstractRewriter

case class KernelInvocationToMethodInvocation(override val source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(kernelInvocation: KernelInvocation): Unit = {
    val methodInvocation = create.invokation(null, null, kernelInvocation.method, kernelInvocation.args.map(rewrite(_)):_*)

    Seq(
      create.assignment(create.unresolved_name("opencl_gcount"), rewrite(kernelInvocation.blockCount)),
      create.assignment(create.unresolved_name("opencl_gsize"), rewrite(kernelInvocation.threadCount))
    ).foreach(methodInvocation.get_before.addStatement(_))

    result = methodInvocation
  }
}
