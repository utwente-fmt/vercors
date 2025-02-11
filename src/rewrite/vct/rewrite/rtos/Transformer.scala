package vct.rewrite.rtos

import vct.col.ast._
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.freertosir.{
  EventGroup,
  ISR,
  MessageBuffer,
  Queue,
  Semaphore,
  StreamBuffer,
  Task,
  Timer,
}

import scala.collection.mutable

class Transformer[O, N](
    tasks: Seq[Task[O, N]],
    timers: Seq[Timer[O, N]],
    isrs: Seq[ISR[O, N]],
    event_groups: Seq[EventGroup[O, N]],
    semaphores: Seq[Semaphore[O, N]],
    queues: Seq[Queue[O, N]],
    stream_buffers: Seq[StreamBuffer[O, N]],
    message_buffers: Seq[MessageBuffer[O, N]],
) {
  private var scheduler: Option[Class[N]] = None
  private var schedulerPerms: Option[InstancePredicate[N]] = None
  private var eventPerms: Option[InstancePredicate[N]] = None
  private var priorityPerms: Option[InstancePredicate[N]] = None
  private var globalInvariant: Option[InstancePredicate[N]] = None
  private var eventState: Option[InstanceField[N]] = None
  private var taskState: Option[InstanceField[N]] = None
  private var taskPriority: Option[InstanceField[N]] = None
  private var taskWaitTime: Option[InstanceField[N]] = None
  private var runnableQueue: Option[InstanceField[N]] = None
  private var simulateTimePassing: Option[InstanceMethod[N]] = None
  private var executionTime: Option[InstanceMethod[N]] = None

  private val freertos_api: mutable.Map[
    (CLocal[O], String),
    (InstanceField[N], InstanceMethod[N]),
  ] = mutable.Map
    .empty[(CLocal[O], String), (InstanceField[N], InstanceMethod[N])]

  private var isr_locks: Seq[InstanceField[N]] = Seq()
  private val output_fields: mutable.Map[InstanceField[N], InstanceField[N]] =
    mutable.Map.empty[InstanceField[N], InstanceField[N]]
  private val read_event: mutable.Map[InstanceField[N], Int] = mutable.Map
    .empty[InstanceField[N], Int]
  private val write_event: mutable.Map[InstanceField[N], Int] = mutable.Map
    .empty[InstanceField[N], Int]
  private val cond_to_call: mutable.Map[InstanceMethod[N], Seq[Expr[N]] => Expr[N]] = mutable
    .Map.empty[InstanceMethod[N], Seq[Expr[N]] => Expr[N]]

  // TODO: var_to_tid and var_to_timer_event are never updated! Do some bookkeeping before the transformations
  private val var_to_tid: mutable.Map[CLocal[O], Int] = mutable.Map
    .empty[CLocal[O], Int]
  private val var_to_timer_event: mutable.Map[CLocal[O], Int] = mutable.Map
    .empty[CLocal[O], Int]
  private var n_events: Int = 0
  private var n_tasks: Int = 0

  def get_encoded_system: Seq[Class[N]] = {
    val scheduler_generator: SchedulerGenerator[O, N] =
      new SchedulerGenerator[O, N]

    // TODO: Event groups are transformed into events, not classes... or should it?
    event_groups.zipWithIndex.map(t => t._1.convert(this, t._2))

    // Convert objects in FreeRTOS design to PVL
    val ir: Seq[ObjectInfo[O, N]] = {
      // First ISRs - they cannot use the FreeRTOS API
      isrs.zipWithIndex.map(t => t._1.convert(this, t._2)) ++
        // Then FreeRTOS API
        semaphores.zipWithIndex.map(t => t._1.convert(this, t._2)) ++
        queues.zipWithIndex.map(t => t._1.convert(this, t._2)) ++
        stream_buffers.zipWithIndex.map(t => t._1.convert(this, t._2)) ++
        message_buffers.zipWithIndex.map(t => t._1.convert(this, t._2)) ++
        // Then timers
        timers.zipWithIndex.map(t => t._1.convert(this, t._2)) ++
        // And finally tasks
        tasks.zipWithIndex.map(t => t._1.convert(this, t._2))
    }

    scheduler = Some(scheduler_generator.generate(ir, n_events))
    // The generation will have populated the remaining fields
    schedulerPerms = Some(scheduler_generator.get_schedulerPerms)
    eventPerms = Some(scheduler_generator.get_eventPerms)
    priorityPerms = Some(scheduler_generator.get_priorityPerms)
    globalInvariant = Some(scheduler_generator.get_globalInvariant)
    eventState = Some(scheduler_generator.get_eventState)
    taskState = Some(scheduler_generator.get_taskState)
    taskPriority = Some(scheduler_generator.get_taskPriority)
    taskWaitTime = Some(scheduler_generator.get_taskWaitTime)
    runnableQueue = Some(scheduler_generator.get_runnableQueue)
    simulateTimePassing = Some(scheduler_generator.get_simulateTimePassing)
    executionTime = Some(scheduler_generator.get_executionTime)

    ir.map(o => o.cls) :+ scheduler.get
  }

  def get_scheduler: Class[N] = scheduler.get
  def get_schedulerPerms: InstancePredicate[N] = schedulerPerms.get
  def get_eventPerms: InstancePredicate[N] = eventPerms.get
  def get_priorityPerms: InstancePredicate[N] = priorityPerms.get
  def get_globalInvariant: InstancePredicate[N] = globalInvariant.get
  def get_eventState: InstanceField[N] = eventState.get
  def get_taskState: InstanceField[N] = taskState.get
  def get_taskPriority: InstanceField[N] = taskPriority.get
  def get_taskWaitTime: InstanceField[N] = taskWaitTime.get
  def get_runnableQueue: InstanceField[N] = runnableQueue.get
  def get_simulateTimePassing: InstanceMethod[N] = simulateTimePassing.get
  def get_executionTime: InstanceMethod[N] = executionTime.get

  def reserve_event_id: Int = {
    val res = n_events
    n_events += 1
    res
  }

  def reserve_task_id: Int = {
    val res = n_tasks
    n_tasks += 1
    res
  }

  def add_to_api(
      cvar: CLocal[O],
      func_name: String,
      ir_field: InstanceField[N],
      ir_method: InstanceMethod[N],
  ): Unit = freertos_api.put((cvar, func_name), (ir_field, ir_method))

  def get_api(
      cvar: CLocal[O],
      func_name: String,
  ): (InstanceField[N], InstanceMethod[N]) =
    freertos_api.getOrElse(
      (cvar, func_name),
      throw new IllegalStateException(
        "Trying to resolve function " + func_name + " before it is generated!"
      ),
    )

  def add_output_field(
      field: InstanceField[N],
      output: InstanceField[N],
  ): Unit = output_fields.put(field, output)

  def get_output_field(field: InstanceField[N]): InstanceField[N] =
    output_fields.getOrElse(
      field,
      throw new IllegalStateException(
        "Trying to get output field of " + field.toInlineString +
          " before it is generated!"
      ),
    )

  def add_read_event(field: InstanceField[N], eid: Int): Unit =
    read_event.put(field, eid)

  def get_read_event(field: InstanceField[N]): Int =
    read_event.getOrElse(
      field,
      throw new IllegalStateException(
        "Instance " + field.toInlineString + " does not have a read event."
      ),
    )

  def add_write_event(field: InstanceField[N], eid: Int): Unit =
    write_event.put(field, eid)

  def get_write_event(field: InstanceField[N]): Int =
    write_event.getOrElse(
      field,
      throw new IllegalStateException(
        "Instance " + field.toInlineString + " does not have a write event."
      ),
    )

  def add_call_condition(method: InstanceMethod[N], cond_gen: Seq[Expr[N]] => Expr[N]): Unit =
    cond_to_call.put(method, cond_gen)

  def get_call_condition(method: InstanceMethod[N]): Seq[Expr[N]] => Expr[N] =
    cond_to_call.getOrElse(method, _ => tt)

  def get_tid(variable: CLocal[O]): Int =
    var_to_tid.getOrElse(
      variable,
      throw new IllegalStateException(
        "Trying to get task ID for " + variable.name +
          " but it does not have one!"
      ),
    )

  def get_n_tasks: Int = tasks.size + timers.size

  def get_timer_eid(variable: CLocal[O]): Int =
    var_to_timer_event.getOrElse(
      variable,
      throw new IllegalStateException(
        "Trying to get timer event ID for " + variable.name +
          " but it does not have one!"
      ),
    )

  def add_isr_lock(lock: InstanceField[N]): Unit = isr_locks = isr_locks :+ lock

  def get_isr_locks: Seq[InstanceField[N]] = isr_locks
}
