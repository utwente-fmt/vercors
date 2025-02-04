package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.Ref
import vct.rewrite.rtos.Utils

case class Task[O](
                    func: CFunctionDefinition[O],
                    param: Expr[O],
                    priority: Int,
                  ) {
  def transform[N](scheduler_ref: Ref[N, Class[N]], tid: Int, last_eid: Int): Class[N] = ???
}
case object Task {
  def of[O](
             invocation: CInvocation[O],
             decls: Seq[CFunctionDefinition[O]],
           ): Task[O] = {
    Utils.creation_arg_assert(
      invocation,
      2,
      "Task creation has wrong number of arguments!",
    )

    val call_arg: CInvocation[O] = invocation.args.head
      .asInstanceOf[CInvocation[O]]
    val priority_arg: Expr[O] = invocation.args(1)

    Utils.creation_arg_assert(call_arg, 1, "No task parameters given!")

    Task(
      Utils.resolve_function(call_arg, decls, "task method"),
      call_arg.args.head,
      Utils.resolve_integer(priority_arg, "task priority"),
    )
  }
}