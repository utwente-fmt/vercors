package vct.rewrite.rtos

import vct.col.ast._
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
  private var scheduler: Class[N] = ???
  private var schedulerPerms: InstancePredicate[N] = ???
  private var eventPerms: InstancePredicate[N] = ???
  private var priorityPerms: InstancePredicate[N] = ???
  private var globalInvariant: InstancePredicate[N] = ???
  private var eventState: InstanceField[N] = ???
  private var taskState: InstanceField[N] = ???
  private var taskPriority: InstanceField[N] = ???
  private var taskWaitTime: InstanceField[N] = ???
  private var runnableQueue: InstanceField[N] = ???
  private var simulateTimePassing: InstanceMethod[N] = ???
  private var executionTime: InstanceMethod[N] = ???
  private val freertos_api: mutable.Map[
    (CLocal[O], String),
    (InstanceField[N], InstanceMethod[N]),
  ] = mutable.Map
    .empty[(CLocal[O], String), (InstanceField[N], InstanceMethod[N])]
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

    scheduler = scheduler_generator.generate(ir, n_events)
    // The generation will have populated the remaining fields
    schedulerPerms = scheduler_generator.get_schedulerPerms
    eventPerms = scheduler_generator.get_eventPerms
    priorityPerms = scheduler_generator.get_priorityPerms
    globalInvariant = scheduler_generator.get_globalInvariant
    eventState = scheduler_generator.get_eventState
    taskState = scheduler_generator.get_taskState
    taskPriority = scheduler_generator.get_taskPriority
    taskWaitTime = scheduler_generator.get_taskWaitTime
    runnableQueue = scheduler_generator.get_runnableQueue
    simulateTimePassing = scheduler_generator.get_simulateTimePassing
    executionTime = scheduler_generator.get_executionTime

    ir.map(o => o.cls) :+ scheduler
  }

  def get_scheduler: Class[N] = scheduler
  def get_schedulerPerms: InstancePredicate[N] = schedulerPerms
  def get_eventPerms: InstancePredicate[N] = eventPerms
  def get_priorityPerms: InstancePredicate[N] = priorityPerms
  def get_globalInvariant: InstancePredicate[N] = globalInvariant
  def get_eventState: InstanceField[N] = eventState
  def get_taskState: InstanceField[N] = taskState
  def get_taskPriority: InstanceField[N] = taskPriority
  def get_taskWaitTime: InstanceField[N] = taskWaitTime
  def get_runnableQueue: InstanceField[N] = runnableQueue
  def get_simulateTimePassing: InstanceMethod[N] = simulateTimePassing
  def get_executionTime: InstanceMethod[N] = executionTime

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
}
