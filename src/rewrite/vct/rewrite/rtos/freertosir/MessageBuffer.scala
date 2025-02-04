package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.Ref
import vct.rewrite.rtos.Utils

case class MessageBuffer(size: Int) {
  def transform[N](scheduler_ref: Ref[N, Class[N]], read_event: Int, write_event: Int): Class[N] = ???
}
case object MessageBuffer {
  def of[O](invocation: CInvocation[O]): MessageBuffer = {
    Utils.creation_arg_assert(
      invocation,
      1,
      "Message buffer has wrong number of arguments!",
    )

    val size_arg: Expr[O] = invocation.args.head

    MessageBuffer(Utils.resolve_integer(size_arg, "message buffer size"))
  }
}