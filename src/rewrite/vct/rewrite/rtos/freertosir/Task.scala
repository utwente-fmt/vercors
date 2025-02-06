package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.Ref
import vct.rewrite.rtos.{ObjectInfo, Transformer, Utils}

case class Task[O, N](
    decl: Option[CLocal[O]],
    func: CFunctionDefinition[O],
    param: Expr[O],
    priority: Int,
) extends FreeRTOSConstruct[O, N] {
  override def convert(col_ir: Transformer[O, N], idx: Int): ObjectInfo[O, N] = ???

  def transform(
      scheduler_ref: Ref[N, Class[N]],
      tid: Int,
      last_eid: Int,
  ): Class[N] = ???
}
case object Task {
  def of[O, N](
      variable: Option[CLocal[O]],
      invocation: CInvocation[O],
      decls: Seq[CFunctionDefinition[O]],
  ): Task[O, N] = {
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
      variable,
      Utils.resolve_function(call_arg, decls, "task method"),
      call_arg.args.head,
      Utils.resolve_integer(priority_arg, "task priority"),
    )
  }
}
