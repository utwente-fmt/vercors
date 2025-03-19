package vct.rewrite.rtos

import vct.col.ast.{CLocal, Class, Expr, InstanceField}
import vct.col.rewrite.{Generation, Rewritten}

case class ObjectInfo[O <: Generation](
    decl: Option[CLocal[O]],
    field: InstanceField[Rewritten[O]],
    program_counter: Option[InstanceField[Rewritten[O]]],
    cls: Class[Rewritten[O]],
    args: Seq[Expr[Rewritten[O]]],
    perms: Expr[Rewritten[O]],
    precondition_in_scheduler: Option[Expr[Rewritten[O]]],
    task_id: Option[Int], // defined if timer_event is defined
    task_priority: Option[Int], // defined iff task_id is defined
    timer_period: Option[Int], // None if the timer is not running at the start
    timer_event: Option[Int], // defined if timer_period is defined
    launch: Boolean,
)
