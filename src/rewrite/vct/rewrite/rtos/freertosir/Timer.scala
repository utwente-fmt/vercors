package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.Ref
import vct.rewrite.rtos.{ObjectInfo, Utils}

case class Timer[O](
    decl: Option[CLocal[O]],
    callback: CFunctionDefinition[O],
    period: Int,
    reload: Boolean,
    priority: Int,
) extends FreeRTOSConstruct[O] {
  override def convert[N]: ObjectInfo[O, N] = ???

  def transform[N](
      scheduler_ref: Ref[N, Class[N]],
      tid: Int,
      assigned_eid: Int,
  ): Class[N] = ???
}
case object Timer {
  def of[O](
      variable: Option[CLocal[O]],
      invocation: CInvocation[O],
      decls: Seq[CFunctionDefinition[O]],
  ): Timer[O] = {
    Utils.creation_arg_assert(
      invocation,
      4,
      "Timer creation has wrong number of arguments!",
    )

    val period_arg: Expr[O] = invocation.args.head
    val reload_arg: Expr[O] = invocation.args(1)
    val priority_arg: Expr[O] = invocation.args(2)
    val call_arg: CInvocation[O] = invocation.args(3)
      .asInstanceOf[CInvocation[O]]

    Timer(
      variable,
      Utils.resolve_function(call_arg, decls, "timer callback function"),
      Utils.resolve_integer(period_arg, "timer period"),
      Utils.resolve_integer(reload_arg, "timer reload") != 0,
      Utils.resolve_integer(priority_arg, "timer priority"),
    )
  }
}
