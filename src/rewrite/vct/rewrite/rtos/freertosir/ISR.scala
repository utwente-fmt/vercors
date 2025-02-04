package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.rewrite.rtos.Utils

case class ISR[O](isr: CFunctionDefinition[O]) {
  def transform[N]: Class[N] = ???
}
case object ISR {
  def of[O](
             invocation: CInvocation[O],
             decls: Seq[CFunctionDefinition[O]],
           ): ISR[O] = {
    Utils.creation_arg_assert(
      invocation,
      1,
      "ISR creation has wrong number of arguments!",
    )

    val call_arg: CInvocation[O] = invocation.args.head
      .asInstanceOf[CInvocation[O]]

    ISR(Utils.resolve_function(call_arg, decls, "ISR handler method"))
  }
}