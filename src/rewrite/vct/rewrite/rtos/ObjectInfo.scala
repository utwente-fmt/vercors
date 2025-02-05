package vct.rewrite.rtos

import vct.col.ast.{CDeclaration, Class, Expr, InstanceField}

case class ObjectInfo[N](
    decl: Option[CDeclaration[N]],
    field: InstanceField[N],
    cls: Class[N],
    args: Seq[Expr[N]],
    perms: Expr[N],
    precondition_in_scheduler: Option[Expr[N]],
    task_id: Option[Int], // defined if timer_event is defined
    task_priority: Option[Int], // defined iff task_id is defined
    timer_period: Option[Int], // None if the timer is not running at the start
    timer_event: Option[Int], // defined if timer_period is defined
    launch: Boolean,
)
