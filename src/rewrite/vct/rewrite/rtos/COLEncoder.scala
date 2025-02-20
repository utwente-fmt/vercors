package vct.rewrite.rtos

import vct.col.ast._
import vct.col.ref.DirectRef
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers.ff
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

class COLEncoder[O <: Generation](
    global_vars: Seq[(CInit[O], Type[Rewritten[O]])],
    abstract_functions: Seq[CGlobalDeclaration[O]],
    functions: Seq[CFunctionDefinition[O]],
    tasks: Seq[Task[O]],
    timers: Seq[Timer[O]],
    isrs: Seq[ISR[O]],
    event_groups: Seq[EventGroup[O]],
    semaphores: Seq[Semaphore[O]],
    queues: Seq[Queue[O]],
    stream_buffers: Seq[StreamBuffer[O]],
    message_buffers: Seq[MessageBuffer[O]],
) {
  type N = Rewritten[O]

  private var scheduler: Option[Class[N]] = None
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
  private var instantiateEventTriggers: Option[InstanceMethod[N]] = None

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
  private val cond_to_call
      : mutable.Map[InstanceMethod[N], (Expr[N], Seq[Expr[N]]) => Expr[N]] =
    mutable.Map.empty[InstanceMethod[N], (Expr[N], Seq[Expr[N]]) => Expr[N]]
  private val var_to_tid: mutable.Map[CLocal[O], Int] = mutable.Map
    .empty[CLocal[O], Int]
  private val var_to_timer_event: mutable.Map[CLocal[O], Int] = mutable.Map
    .empty[CLocal[O], Int]
  private val var_to_timer_period: mutable.Map[CLocal[O], Int] = mutable.Map
    .empty[CLocal[O], Int]
  private val var_to_timer_reload: mutable.Map[CLocal[O], Boolean] = mutable.Map
    .empty[CLocal[O], Boolean]
  private val isr_fields: mutable.Map[
    String,
    (InstanceField[N], InstanceField[N], Option[Expr[N]]),
  ] = mutable.Map
    .empty[String, (InstanceField[N], InstanceField[N], Option[Expr[N]])]
  private val global_fields
      : mutable.Map[String, (InstanceField[N], Option[Expr[N]])] = mutable.Map
    .empty[String, (InstanceField[N], Option[Expr[N]])]
  private var n_events: Int = 0
  private var n_tasks: Int = 0

  def get_encoded_system: Seq[GlobalDeclaration[N]] = {
    val scheduler_generator: SchedulerGenerator[O] = new SchedulerGenerator[O]

    preprocess_tasks()

    // TODO: Event groups are transformed into events, not classes... or should they?
    event_groups.zipWithIndex.map(t => t._1.convert(this, t._2))

    // Convert objects in FreeRTOS design to PVL
    val ir: Seq[ObjectInfo[O]] = {
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

    scheduler = Some(
      scheduler_generator.generate(ir, n_events, global_fields.values.toSeq)
    )
    // The generation will have populated the remaining fields
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
    instantiateEventTriggers = Some(
      scheduler_generator.get_instantiateEventTriggers
    )

    ir.map(o => o.cls) ++
      Seq(scheduler.get, create_entry(scheduler_generator.get_start))
  }

  private def preprocess_tasks(): Unit = {
    tasks.foreach(t => {
      val tid = n_tasks
      n_tasks += 1
      t.set_tid(tid)
      if (t.decl.nonEmpty)
        var_to_tid.put(t.decl.get, tid)
    })
    timers.foreach(t => {
      val tid = n_tasks
      n_tasks += 1
      val eid = reserve_event_id
      t.set_tid(tid)
      t.set_eid(eid)
      if (t.decl.nonEmpty) {
        var_to_tid.put(t.decl.get, tid)
        var_to_timer_event.put(t.decl.get, eid)
        var_to_timer_period.put(t.decl.get, t.period)
        var_to_timer_reload.put(t.decl.get, t.reload)
      }
    })
  }

  private def create_entry(start: InstanceMethod[N]): VeSUVMainMethod[N] = {
    val cls_type: TByReferenceClass[N] = TByReferenceClass(
      new DirectRef[N, Class[N]](scheduler.get),
      Seq(),
    )
    val s_var: Variable[N] = new Variable(cls_type)(Utils.origen("scheduler"))
    new VeSUVMainMethod(Some(
      Block(Seq[Statement[N]](
        LocalDecl(s_var)(Utils.origen),
        Assign(
          Utils.local_of(s_var),
          PVLNew(cls_type, Seq(), Seq(), Seq(), Seq())(Utils.blame)(
            Utils.origen
          ),
        )(Utils.blame)(Utils.origen),
        Utils.stmt_invoke(
          new DirectRef[N, InstanceMethod[N]](start),
          Seq(),
          Some(Utils.local_of(s_var)),
        ),
      ))(Utils.origen)
    ))(Utils.blame)(Utils.origen)
  }

  def get_scheduler: Class[N] = scheduler.get
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
  def get_instantiateEventTriggers: InstanceMethod[N] =
    instantiateEventTriggers.get

  def reserve_event_id: Int = {
    val res = n_events
    n_events += 1
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

  def add_call_condition(
      method: InstanceMethod[N],
      cond_gen: (Expr[N], Seq[Expr[N]]) => Expr[N],
  ): Unit = cond_to_call.put(method, cond_gen)

  def get_call_condition(
      method: InstanceMethod[N]
  ): (Expr[N], Seq[Expr[N]]) => Expr[N] =
    cond_to_call.getOrElse(method, (_, _) => ff)

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

  def get_timer_period(variable: CLocal[O]): Int =
    var_to_timer_period.getOrElse(
      variable,
      throw new IllegalStateException(
        "Trying to get timer period for " + variable.name +
          " but it does not have one!"
      ),
    )

  def get_timer_reload(variable: CLocal[O]): Boolean =
    var_to_timer_reload.getOrElse(
      variable,
      throw new IllegalStateException(
        "Trying to get timer reload status for " + variable.name +
          " but it does not have one!"
      ),
    )

  def add_isr_lock(lock: InstanceField[N]): Unit = isr_locks = isr_locks :+ lock

  def get_isr_locks: Seq[InstanceField[N]] = isr_locks

  def get_function_definition(name: String): Option[CFunctionDefinition[O]] =
    functions.collectFirst {
      case f: CFunctionDefinition[O]
          if Utils.get_declarator_name(f.declarator).equals(name) =>
        f
    }

  def get_abstract_function(name: String): Option[CGlobalDeclaration[O]] =
    abstract_functions.find(d =>
      d.decl.inits.length == 1 &&
        Utils.get_declarator_name(d.decl.inits.head.decl).equals(name)
    )

  def register_isr_field(
      name: String,
      isr: InstanceField[N],
      rewriter: Rewriter[O],
  ): InstanceField[N] = {
    if (isr_fields.contains(name)) {
      if (!isr_fields(name)._1.equals(isr))
        throw new IllegalStateException(
          "Multiple ISRs accessing the same field " + name +
            " is not supported!"
        )
      else
        return isr_fields(name)._2
    }

    val (decl: CInit[O], typ: Type[N]) = get_decl_matching(name)
    val new_field: InstanceField[N] =
      new InstanceField(typ, Seq())(Utils.origen(name))
    val init: Option[Expr[N]] = decl.init.map(e => rewriter.dispatch(e))
    isr_fields.put(name, (isr, new_field, init))
    new_field
  }

  def get_isr_fields(
      isr: InstanceField[N]
  ): Seq[(InstanceField[N], Option[Expr[N]])] =
    isr_fields.values.toSeq.filter(t => t._1.equals(isr)).map(t => (t._2, t._3))

  def access_global_variable(
      name: String,
      scheduler: InstanceField[N],
      rewriter: Rewriter[O],
  ): Expr[N] = {
    val (decl: CInit[O], typ: Type[N]) = get_decl_matching(name)
    if (isr_fields.contains(name)) {
      val isr: InstanceField[N] = isr_fields(name)._1
      val field: InstanceField[N] = isr_fields(name)._2
      Utils.deref_of(
        field,
        Some(Utils.deref_of(isr, Some(Utils.deref_of(scheduler)))),
      )
    } else if (global_fields.contains(name)) {
      Utils.deref_of(global_fields(name)._1, Some(Utils.deref_of(scheduler)))
    } else {
      val new_field: InstanceField[N] =
        new InstanceField(typ, Seq())(Utils.origen(name))
      val init: Option[Expr[N]] = decl.init.map(e => rewriter.dispatch(e))
      global_fields.put(name, (new_field, init))
      Utils.deref_of(new_field, Some(Utils.deref_of(scheduler)))
    }
  }

  private def get_decl_matching(name: String): (CInit[O], Type[N]) =
    global_vars.find(v => Utils.get_declarator_name(v._1.decl).equals(name))
      .getOrElse(
        throw new IllegalArgumentException(
          "Could not find a global variable named " + name
        )
      )
}
