package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.Ref
import vct.rewrite.rtos.Utils

case class StreamBuffer(size: Int, trigger_bytes: Int) {
  def transform[N](scheduler_ref: Ref[N, Class[N]], read_event: Int, write_event: Int): Class[N] = ???
}
case object StreamBuffer {
  def of[O](invocation: CInvocation[O]): StreamBuffer = {
    Utils.creation_arg_assert(
      invocation,
      2,
      "Stream buffer creation has wrong number of arguments!",
    )

    val size_arg: Expr[O] = invocation.args.head
    val trigger_arg: Expr[O] = invocation.args(1)

    StreamBuffer(
      Utils.resolve_integer(size_arg, "stream buffer size"),
      Utils.resolve_integer(trigger_arg, "stream buffer trigger level"),
    )
  }
}