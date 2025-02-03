package vct.rewrite.rtos

import vct.col.ast._

case class Task[O](
    func: CFunctionDefinition[O],
    param: Expr[O],
    priority: Int,
) {
  def transform[N](tid: Int, last_eid: Int): Class[N] = ???
}
case object Task {
  def of[G](
      invocation: CInvocation[G],
      decls: Seq[CFunctionDefinition[G]],
  ): Task[G] = {
    if (invocation.args.length != 2)
      throw new IllegalArgumentException(
        "Task creation has wrong number of arguments!"
      )

    val call_arg: CInvocation[G] = invocation.args.head
      .asInstanceOf[CInvocation[G]]
    val priority_arg: Expr[G] = invocation.args(1)

    val func: CFunctionDefinition[G] = decls.find(f =>
      Utils.get_declarator_name(f.declarator)
        .equals(Utils.get_applicable_name(call_arg.applicable))
    ).getOrElse(
      throw new IllegalArgumentException("Couldn't find task method!")
    )

    val param: Expr[G] = call_arg.args.head

    val priority: Int =
      Utils.try_expr_to_int(priority_arg) match {
        case Some(i) => i
        case None =>
          throw new IllegalArgumentException(
            "Task priority " + priority_arg.toInlineString +
              " cannot be resolved to integer!"
          )
      }

    Task(func, param, priority)
  }
}

case class Timer[O](
    callback: CFunctionDefinition[O],
    period: Int,
    reload: Boolean,
    priority: Int,
) {
  def transform[N](tid: Int, assigned_eid: Int): Class[N] = ???
}
case object Timer {
  def of[G](
      invocation: CInvocation[G],
      decls: Seq[CFunctionDefinition[G]],
  ): Timer[G] = {
    if (invocation.args.length != 4)
      throw new IllegalArgumentException(
        "Timer creation has wrong number of arguments!"
      )

    val period_arg: Expr[G] = invocation.args.head
    val reload_arg: Expr[G] = invocation.args(1)
    val priority_arg: Expr[G] = invocation.args(2)
    val call_arg: CInvocation[G] = invocation.args(3)
      .asInstanceOf[CInvocation[G]]

    val period: Int = Utils.try_expr_to_int(period_arg).getOrElse(
      throw new IllegalArgumentException(
        "Could not resolve timer period expression " + period_arg.toInlineString
      )
    )

    val reload: Boolean =
      Utils.try_expr_to_int(reload_arg).getOrElse(
        throw new IllegalArgumentException(
          "Could not resolve timer reload expression " +
            reload_arg.toInlineString
        )
      ) != 0

    val priority: Int = Utils.try_expr_to_int(priority_arg).getOrElse(
      throw new IllegalArgumentException(
        "Could not resolve timer period expression " +
          priority_arg.toInlineString
      )
    )

    val callback: CFunctionDefinition[G] = decls.find(f =>
      Utils.get_declarator_name(f.declarator)
        .equals(Utils.get_applicable_name(call_arg.applicable))
    ).getOrElse(
      throw new IllegalArgumentException("Couldn't find timer callback method!")
    )

    Timer(callback, period, reload, priority)
  }
}

case class ISR[O](isr: CFunctionDefinition[O]) {
  def transform[N]: Class[N] = ???
}
case object ISR {
  def of[G](
      invocation: CInvocation[G],
      decls: Seq[CFunctionDefinition[G]],
  ): ISR[G] = {
    if (invocation.args.length != 1)
      throw new IllegalArgumentException(
        "ISR creation has wrong number of arguments!"
      )

    val call_arg: CInvocation[G] = invocation.args.head
      .asInstanceOf[CInvocation[G]]

    val func: CFunctionDefinition[G] = decls.find(f =>
      Utils.get_declarator_name(f.declarator)
        .equals(Utils.get_applicable_name(call_arg.applicable))
    ).getOrElse(
      throw new IllegalArgumentException("Couldn't find ISR handler method!")
    )

    ISR(func)
  }
}

case class EventGroup[G]()

case class Semaphore[G]()

case class Queue[G](capacity: Int, typ: Type[G])
case object Queue {
  def of[G](invocation: CInvocation[G]): Queue[G] = {
    if (invocation.args.length != 2)
      throw new IllegalArgumentException(
        "Queue creation has wrong number of arguments!"
      )

    val size_arg: Expr[G] = invocation.args.head
    val type_arg: Expr[G] = invocation.args(1)

    val capacity: Int = Utils.try_expr_to_int(size_arg).getOrElse(
      throw new IllegalArgumentException(
        "Could not resolve timer period expression " + size_arg.toInlineString
      )
    )

    val typ: Type[G] =
      type_arg match {
        case SizeOf(t) =>
          t match {
            case CPrimitiveType(specs) if specs.collectFirst {
                  case b: CBool[G] => b
                }.nonEmpty =>
              TBool[G]
            case _ => TInt[G]
          }
        case _ => TInt[G]
      }

    Queue(capacity, typ)
  }
}

case class StreamBuffer[G]()

case class MessageBuffer[G]()
