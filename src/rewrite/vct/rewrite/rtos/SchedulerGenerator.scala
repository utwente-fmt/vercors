package vct.rewrite.rtos

import vct.col.ast._
import vct.col.ref.{DirectRef, Ref}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers.tt

class SchedulerGenerator[O <: Generation] {
  type N = Rewritten[O]

  private var eventState: Option[InstanceField[N]] = None
  private var taskState: Option[InstanceField[N]] = None
  private var taskPriority: Option[InstanceField[N]] = None
  private var taskWaitTime: Option[InstanceField[N]] = None
  private var runnableQueue: Option[InstanceField[N]] = None
  private var priorityPerms: Option[InstancePredicate[N]] = None
  private var eventPerms: Option[InstancePredicate[N]] = None
  private var globalInvariant: Option[InstancePredicate[N]] = None
  private var awokenAfterDelay: Option[InstanceMethod[N]] = None
  private var simulateTimePassing: Option[InstanceMethod[N]] = None
  private var executionTime: Option[InstanceMethod[N]] = None
  private var instantiateEventTriggers: Option[InstanceMethod[N]] = None
  private var start: Option[InstanceMethod[N]] = None

  def get_eventState: InstanceField[N] = eventState.get
  def get_taskState: InstanceField[N] = taskState.get
  def get_taskPriority: InstanceField[N] = taskPriority.get
  def get_taskWaitTime: InstanceField[N] = taskWaitTime.get
  def get_runnableQueue: InstanceField[N] = runnableQueue.get
  def get_priorityPerms: InstancePredicate[N] = priorityPerms.get
  def get_eventPerms: InstancePredicate[N] = eventPerms.get
  def get_globalInvariant: InstancePredicate[N] = globalInvariant.get
  def get_awokenAfterDelay: InstanceMethod[N] = awokenAfterDelay.get
  def get_simulateTimePassing: InstanceMethod[N] = simulateTimePassing.get
  def get_executionTime: InstanceMethod[N] = executionTime.get
  def get_instantiateEventTriggers: InstanceMethod[N] =
    instantiateEventTriggers.get
  def get_start: InstanceMethod[N] = start.get

  def generate(
      objs: Seq[ObjectInfo[O]],
      n_events: Int,
      global_fields: Seq[(InstanceField[N], Option[Expr[N]])],
  ): Class[N] = {
    // Support calculations
    val n_tasks = objs.count(o => o.task_id.nonEmpty)

    // Scheduling variables
    eventState = Some(
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("eventState"))
    )
    taskState = Some(
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("taskState"))
    )
    taskPriority = Some(
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("taskPriority"))
    )
    taskWaitTime = Some(
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("taskWaitTime"))
    )
    runnableQueue = Some(
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("runnableQueue"))
    )

    // Helper predicates
    val vesuv_limit_entries: InstancePredicate[N] = create_vesuv_limit_entries()
    val vesuv_limit_entries_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](vesuv_limit_entries)
    val vesuv_injective: InstancePredicate[N] = create_vesuv_injective()
    val vesuv_injective_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](vesuv_injective)

    // Scheduler predicates
    priorityPerms = Some(
      new InstancePredicate(
        Seq(),
        Some(
          Star(
            Perm(Utils.loc_of(taskPriority.get), Utils.write)(Utils.origen),
            Eq(Utils.size(taskPriority.get), Utils.int_val(n_tasks))(
              Utils.origen
            ),
          )(Utils.origen)
        ),
        false,
        true,
      )(Utils.origen("priorityPerms"))
    )
    val priorityPerms_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](priorityPerms.get)
    eventPerms = Some(
      new InstancePredicate(
        Seq(),
        Some(
          Star(
            Perm(Utils.loc_of(eventState.get), Utils.write)(Utils.origen),
            Eq(Utils.size(eventState.get), Utils.int_val(n_events))(
              Utils.origen
            ),
          )(Utils.origen)
        ),
        false,
        true,
      )(Utils.origen("eventPerms"))
    )
    val eventPerms_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](eventPerms.get)
    val schedulerPerms: InstancePredicate[N] = create_schedulerPerms(
      n_tasks,
      eventPerms_ref,
      priorityPerms_ref,
      vesuv_limit_entries_ref,
      vesuv_injective_ref,
    )
    val schedulerPerms_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](schedulerPerms)
    val globalPerms: InstancePredicate[N] =
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(
          Seq(Utils.predicate_apply(Utils.thiz, schedulerPerms_ref, Seq())) ++
            objs.map(o => o.perms) ++
            global_fields
              .map(f => Perm(Utils.loc_of(f._1), Utils.write)(Utils.origen))
        )),
        false,
        true,
      )(Utils.origen("globalPerms"))
    val globalPerms_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](globalPerms)
    val globalProperties: InstancePredicate[N] =
      new InstancePredicate(Seq(), Some(tt.asInstanceOf[Expr[N]]), false, true)(
        Utils.origen("globalProperties")
      )
    val globalProperties_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](globalProperties)
    globalInvariant = Some(
      new InstancePredicate(
        Seq(),
        Some(
          Star(
            Utils.predicate_apply(Utils.thiz, globalPerms_ref, Seq()),
            Utils.predicate_apply(Utils.thiz, globalProperties_ref, Seq()),
          )(Utils.origen)
        ),
        false,
        true,
      )(Utils.origen("globalInvariant"))
    )
    val globalInvariant_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](globalInvariant.get)

    // Constructor
    val schedulerConstructor: PVLConstructor[N] = create_constructor(
      objs,
      n_events,
      n_tasks,
      global_fields.filter(t => t._2.nonEmpty).map(t => (t._1, t._2.get)),
    )

    // Helper methods
    val nextEventDelay: InstanceMethod[N] = create_nextEventDelay(
      eventPerms_ref
    )
    val advanceTime: InstanceMethod[N] = create_advanceTime(eventPerms_ref)
    val newRunnableTasks: InstanceMethod[N] = create_newRunnableTasks(
      schedulerPerms_ref,
      vesuv_limit_entries_ref,
      vesuv_injective_ref,
    )
    val resumeTasks: InstanceMethod[N] = create_resumeTasks(schedulerPerms_ref)
    val resetEvents: InstanceMethod[N] = create_resetEvents(eventPerms_ref)
    val selectNextTask: InstanceMethod[N] = create_selectNextTask(
      schedulerPerms_ref
    )
    awokenAfterDelay = Some(create_awokenAfterDelay(
      schedulerPerms_ref,
      vesuv_limit_entries_ref,
      vesuv_injective_ref,
    ))
    simulateTimePassing = Some(create_simulateTimePassing(schedulerPerms_ref))
    executionTime = Some(create_executionTime())
    instantiateEventTriggers = Some(
      create_instantiateEventTriggers(eventPerms_ref, n_events)
    )

    // Methods
    val schedule: InstanceMethod[N] = create_schedule(
      new DirectRef[N, InstanceMethod[N]](nextEventDelay),
      new DirectRef[N, InstanceMethod[N]](advanceTime),
      new DirectRef[N, InstanceMethod[N]](newRunnableTasks),
      new DirectRef[N, InstanceMethod[N]](resumeTasks),
      new DirectRef[N, InstanceMethod[N]](resetEvents),
      new DirectRef[N, InstanceMethod[N]](selectNextTask),
      globalInvariant_ref,
    )
    start = Some(create_start(
      objs.filter(o => o.launch).map(o => o.field),
      objs.filter(o => o.launch)
        .map(o => o.precondition_in_scheduler.getOrElse(tt)),
      objs.filter(o => o.program_counter.nonEmpty).map(o =>
        Star(
          Utils
            .half_perm_of(o.program_counter.get, Some(Utils.deref_of(o.field))),
          Eq(
            Utils
              .deref_of(o.program_counter.get, Some(Utils.deref_of(o.field))),
            Utils.int_val(0),
          )(Utils.origen),
        )(Utils.origen)
      ),
      new DirectRef[N, InstanceMethod[N]](schedule),
    ))

    // Finalize class
    val decls: Seq[ClassDeclaration[N]] =
      // Scheduling variables
      Seq(
        eventState.get,
        taskState.get,
        taskPriority.get,
        taskWaitTime.get,
        runnableQueue.get,
      ) ++
        // Object fields
        objs.map(o => o.field) ++
        // Other global fields
        global_fields.map(t => t._1) ++
        // Predicates
        Seq(
          vesuv_limit_entries,
          vesuv_injective,
          priorityPerms.get,
          eventPerms.get,
          schedulerPerms,
          globalPerms,
          globalProperties,
          globalInvariant.get,
        ) ++
        // Methods
        Seq(
          schedulerConstructor,
          start.get,
          schedule,
          nextEventDelay,
          advanceTime,
          newRunnableTasks,
          resumeTasks,
          resetEvents,
          selectNextTask,
          awokenAfterDelay.get,
          simulateTimePassing.get,
          executionTime.get,
          instantiateEventTriggers.get,
        )

    new ByReferenceClass(
      Seq(),
      decls,
      Seq(),
      Utils.predicate_apply(Utils.thiz, globalInvariant_ref, Seq()),
    )(Utils.origen("FreeRTOSScheduler"))
  }

  private def create_vesuv_limit_entries(): InstancePredicate[N] = {
    val xs: Variable[N] = new Variable(Utils.tseqint)(Utils.origen("xs"))
    val l: Variable[N] = new Variable(Utils.tint)(Utils.origen("l"))
    val u: Variable[N] = new Variable(Utils.tint)(Utils.origen("u"))

    val i: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))

    val body: Expr[N] = Utils.single_var_forall(
      i,
      Utils.int_val(0),
      Size(Utils.local_of(xs))(Utils.origen),
      And(
        GreaterEq(
          SeqSubscript(Utils.local_of(xs), Utils.local_of(i))(Utils.blame)(
            Utils.origen
          ),
          Utils.local_of(l),
        )(Utils.origen),
        Less(
          SeqSubscript(Utils.local_of(xs), Utils.local_of(i))(Utils.blame)(
            Utils.origen
          ),
          Utils.local_of(u),
        )(Utils.origen),
      )(Utils.origen),
    )

    new InstancePredicate(Seq(xs, l, u), Some(body), false, true)(
      Utils.origen("vesuv_limit_entries")
    )
  }

  private def create_vesuv_injective(): InstancePredicate[N] = {
    val xs: Variable[N] = new Variable(Utils.tseqint)(Utils.origen("xs"))

    val i: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val j: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))

    val body: Expr[N] = Utils.single_var_forall(
      i,
      Utils.int_val(0),
      Size(Utils.local_of(xs))(Utils.origen),
      Forall(
        Seq(j),
        Seq(),
        Implies(
          And(
            Less(Utils.local_of(i), Utils.local_of(j))(Utils.origen),
            Less(Utils.local_of(j), Size(Utils.local_of(xs))(Utils.origen))(
              Utils.origen
            ),
          )(Utils.origen),
          Neq(
            SeqSubscript(Utils.local_of(xs), Utils.local_of(i))(Utils.blame)(
              Utils.origen
            ),
            SeqSubscript(Utils.local_of(xs), Utils.local_of(j))(Utils.blame)(
              Utils.origen
            ),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen),
    )

    new InstancePredicate(Seq(xs), Some(body), false, true)(
      Utils.origen("vesuv_injective")
    )
  }

  private def create_schedulerPerms(
      n_tasks: Int,
      eventPerms_ref: Ref[N, InstancePredicate[N]],
      priorityPerms_ref: Ref[N, InstancePredicate[N]],
      vesuv_limit_entries_ref: Ref[N, InstancePredicate[N]],
      vesuv_injective_ref: Ref[N, InstancePredicate[N]],
  ): InstancePredicate[N] = {
    // Quantifier variables
    val i1: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i2: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i3: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))

    val conds: Seq[Expr[N]] = Seq[Expr[N]](
      Utils.predicate_apply(Utils.thiz, priorityPerms_ref, Seq()),
      Utils.predicate_apply(Utils.thiz, eventPerms_ref, Seq()),
      Perm(Utils.loc_of(taskState.get), Utils.write)(Utils.origen),
      Eq(Utils.size(taskState.get), Utils.int_val(n_tasks))(Utils.origen),
      Utils.predicate_apply(
        Utils.thiz,
        vesuv_limit_entries_ref,
        Seq(
          Utils.deref_of(taskState.get),
          Utils.int_val(-2),
          Utils.size(eventState.get),
        ),
      ),
      Perm(Utils.loc_of(taskWaitTime.get), Utils.write)(Utils.origen),
      Eq(Utils.size(taskWaitTime.get), Utils.int_val(n_tasks))(Utils.origen),
      Utils.single_var_forall(
        i1,
        Utils.int_val(0),
        Utils.size(taskWaitTime.get),
        GreaterEq(
          Utils.subscript_expr(taskWaitTime.get, Utils.local_of(i1)),
          Utils.int_val(0),
        )(Utils.origen),
      ),
      Perm(Utils.loc_of(runnableQueue.get), Utils.write)(Utils.origen),
      LessEq(Utils.size(runnableQueue.get), Utils.size(taskState.get))(
        Utils.origen
      ),
      Utils.predicate_apply(
        Utils.thiz,
        vesuv_limit_entries_ref,
        Seq(
          Utils.deref_of(runnableQueue.get),
          Utils.int_val(0),
          Utils.size(taskPriority.get),
        ),
      ),
      Utils.predicate_apply(
        Utils.thiz,
        vesuv_injective_ref,
        Seq(Utils.deref_of(runnableQueue.get)),
      ),
      Utils.single_var_forall(
        i2,
        Utils.int_val(0),
        Utils.size(runnableQueue.get),
        Eq(
          Utils.subscript_expr(
            taskState.get,
            Utils.subscript_expr(runnableQueue.get, Utils.local_of(i2)),
          ),
          Utils.int_val(-1),
        )(Utils.origen),
      ),
      Utils.single_var_forall(
        i3,
        Utils.int_val(0),
        Utils.size(taskState.get),
        Implies(
          Eq(
            Utils.subscript_expr(taskState.get, Utils.local_of(i3)),
            Utils.int_val(-1),
          )(Utils.origen),
          SeqMember(Utils.local_of(i3), Utils.deref_of(runnableQueue.get))(
            Utils.origen
          ),
        )(Utils.origen),
      ),
    )

    new InstancePredicate(Seq(), Some(Utils.fold_star(conds)), false, true)(
      Utils.origen("schedulerPerms")
    )
  }

  private def create_constructor(
      objs: Seq[ObjectInfo[O]],
      n_events: Int,
      n_tasks: Int,
      fields_to_initialize: Seq[(InstanceField[N], Expr[N])],
  ): PVLConstructor[N] = {
    // Supporting calculations
    val launch_objs: Seq[ObjectInfo[O]] = objs.filter(o => o.launch)
    val pc_objs: Seq[ObjectInfo[O]] = objs
      .filter(o => o.program_counter.nonEmpty)
    // Find timers that have already been activated => their initial event state is their delay, not -1
    val timer_delay: Map[Int, Int] = Map.from(
      objs.filter(o => o.timer_period.nonEmpty)
        .map(o => o.timer_event.get -> o.timer_period.get)
    )
    // Find all timer IDs => their task status is set to wait for their event
    val timer_event: Map[Int, Int] = Map.from(
      objs.filter(o => o.timer_event.nonEmpty)
        .map(o => o.task_id.get -> o.timer_event.get)
    )

    val eventStateInit: Seq[Int] = Seq.fill(n_events)(-1).zipWithIndex
      .map(t => timer_delay.getOrElse(t._2, t._1))
    val taskStateInit: Seq[Int] = Seq.fill(n_tasks)(-1).zipWithIndex
      .map(t => timer_event.getOrElse(t._2, t._1))
    val taskPriorityInit: Seq[Int] = objs.filter(o => o.task_id.nonEmpty)
      .sortBy(o => o.task_id.get).map(o => o.task_priority.get)
    val taskWaitTimeInit: Seq[Int] = Seq.fill(n_tasks)(0)
    val runnableQueueInit: Seq[Int] = taskStateInit.zipWithIndex
      .filter(t => t._1 == -1).map(t => t._2)

    // Constructor contract
    val ensures: Expr[N] = Utils.fold_star(
      (launch_objs.map(o => o.precondition_in_scheduler.getOrElse(tt)) ++
        pc_objs.map(o =>
          Star(
            Utils.half_perm_of(
              o.program_counter.get,
              Some(Utils.deref_of(o.field)),
            ),
            Eq(
              Utils
                .deref_of(o.program_counter.get, Some(Utils.deref_of(o.field))),
              Utils.int_val(0),
            )(Utils.origen),
          )(Utils.origen)
        ) ++
        launch_objs
          .map(o => IdleToken(Utils.deref_of(o.field))(Utils.origen))) :+
        Committed(Utils.thiz)(Utils.blame)(Utils.origen)
    )

    // Constructor body construction
    val statements: Seq[Statement[N]] =
      Seq(
        Assign[N](
          Utils.deref_of(eventState.get),
          Utils.seq_val(eventStateInit.map(i => Utils.int_val(i))),
        )(Utils.blame)(Utils.origen),
        Assign[N](
          Utils.deref_of(taskState.get),
          Utils.seq_val(taskStateInit.map(i => Utils.int_val(i))),
        )(Utils.blame)(Utils.origen),
        Assign[N](
          Utils.deref_of(taskPriority.get),
          Utils.seq_val(taskPriorityInit.map(i => Utils.int_val(i))),
        )(Utils.blame)(Utils.origen),
        Assign[N](
          Utils.deref_of(taskWaitTime.get),
          Utils.seq_val(taskWaitTimeInit.map(i => Utils.int_val(i))),
        )(Utils.blame)(Utils.origen),
        Assign[N](
          Utils.deref_of(runnableQueue.get),
          Utils.seq_val(runnableQueueInit.map(i => Utils.int_val(i))),
        )(Utils.blame)(Utils.origen),
      ) ++ runnableQueueInit.zipWithIndex.map(t =>
        Assert[N](
          Eq(Utils.subscript(runnableQueue.get, t._2), Utils.int_val(t._1))(
            Utils.origen
          )
        )(Utils.blame)(Utils.origen)
      ) ++ objs.map(o =>
        Assign(
          Utils.deref_of(o.field),
          PVLNew(
            TByReferenceClass(new DirectRef[N, Class[N]](o.cls), Seq())(
              Utils.origen
            ),
            Seq(),
            o.args,
            Seq(),
            Seq(),
          )(Utils.blame)(Utils.origen),
        )(Utils.blame)(Utils.origen)
      ) ++ fields_to_initialize.map(t =>
        Assign(Utils.deref_of(t._1), t._2)(Utils.blame)(Utils.origen)
      ) :+ Commit(Utils.thiz)(Utils.blame)(Utils.origen)

    val body: Statement[N] = Block(statements)(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(tt, ensures),
      Seq(),
      Seq(),
      Some(body),
    )(Utils.blame)(Utils.origen)
  }

  private def create_nextEventDelay(
      eventPerms_ref: Ref[N, InstancePredicate[N]]
  ): InstanceMethod[N] = {
    // Quantifier variables
    val i1: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i2: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i31: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i32: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))

    // requires eventPerms();
    val requires: Expr[N] = Utils
      .predicate_apply(Utils.thiz, eventPerms_ref, Seq())

    // ensures (\forall int i; 0 <= i && i < |eventState| ==> eventState[i] < 0 || \result <= eventState[i]);
    val ensures1: Expr[N] = Utils.single_var_forall(
      i1,
      Utils.int_val(0),
      Utils.size(eventState.get),
      Or(
        Less(
          Utils.subscript_expr(eventState.get, Utils.local_of(i1)),
          Utils.int_val(0),
        )(Utils.origen),
        LessEq(
          Utils.result,
          Utils.subscript_expr(eventState.get, Utils.local_of(i1)),
        )(Utils.origen),
      )(Utils.origen),
    )

    // ensures     (\forall int i; 0 <= i && i < |eventState| ==> eventState[i] < 0)
    //         ==> \result == -1;
    val ensures2: Expr[N] =
      Implies(
        Utils.single_var_forall(
          i2,
          Utils.int_val(0),
          Utils.size(eventState.get),
          Less(
            Utils.subscript_expr(eventState.get, Utils.local_of(i2)),
            Utils.int_val(0),
          )(Utils.origen),
        ),
        Eq(Utils.result, Utils.int_val(-1))(Utils.origen),
      )(Utils.origen)

    // ensures     (\exists int i; 0 <= i && i < |eventState| && eventState[i] >= 0)
    //         ==> (\exists int i; 0 <= i && i < |eventState| && eventState[i] >= 0 && \result == eventState[i]);
    val ensures3: Expr[N] =
      Implies(
        Utils.single_var_exists(
          i31,
          Utils.int_val(0),
          Utils.size(eventState.get),
          GreaterEq(
            Utils.subscript_expr(eventState.get, Utils.local_of(i31)),
            Utils.int_val(0),
          )(Utils.origen),
        ),
        Utils.single_var_exists(
          i32,
          Utils.int_val(0),
          Utils.size(eventState.get),
          And(
            GreaterEq(
              Utils.subscript_expr(eventState.get, Utils.local_of(i32)),
              Utils.int_val(0),
            )(Utils.origen),
            Eq(
              Utils.result,
              Utils.subscript_expr(eventState.get, Utils.local_of(i32)),
            )(Utils.origen),
          )(Utils.origen),
        ),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        requires,
        Utils.fold_star(Seq[Expr[N]](ensures1, ensures2, ensures3)),
      ),
      false,
      true,
    )(Utils.blame)(Utils.origen("nextEventDelay"))
  }

  private def create_advanceTime(
      eventPerms_ref: Ref[N, InstancePredicate[N]]
  ): InstanceMethod[N] = {
    val advance: Variable[N] = new Variable(Utils.tint)(Utils.origen("advance"))

    // Quantifier variables
    val i2: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i3: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))

    // requires eventPerms();
    // ensures eventPerms();
    val context: Expr[N] = Utils
      .predicate_apply(Utils.thiz, eventPerms_ref, Seq())

    // ensures |eventState| == \old(|eventState|);
    val ensures1: Expr[N] = Utils.unchanged(Utils.size(eventState.get))

    // ensures (\forall int i; 0 <= i && i < \old(|eventState|) && \old(eventState[i]) < 0 ==> eventState[i] == -1);
    val ensures2: Expr[N] = Utils.single_var_forall(
      i2,
      Utils.int_val(0),
      Utils.old(Utils.size(eventState.get)),
      Implies(
        Less(
          Utils.old(Utils.subscript_expr(eventState.get, Utils.local_of(i2))),
          Utils.int_val(0),
        )(Utils.origen),
        Eq(
          Utils.subscript_expr(eventState.get, Utils.local_of(i2)),
          Utils.int_val(-1),
        )(Utils.origen),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < \old(|eventState|) && \old(eventState[i]) >= 0 ==> eventState[i] == \old(eventState[i]) - advance);
    val ensures3: Expr[N] = Utils.single_var_forall(
      i3,
      Utils.int_val(0),
      Utils.old(Utils.size(eventState.get)),
      Implies(
        GreaterEq(
          Utils.old(Utils.subscript_expr(eventState.get, Utils.local_of(i3))),
          Utils.int_val(0),
        )(Utils.origen),
        Eq(
          Utils.subscript_expr(eventState.get, Utils.local_of(i3)),
          Minus(
            Utils.old(Utils.subscript_expr(eventState.get, Utils.local_of(i3))),
            Utils.local_of(advance),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen),
    )

    new InstanceMethod(
      Utils.tvoid,
      Seq(advance),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq[Expr[N]](context, ensures1, ensures2, ensures3)),
      ),
      false,
      false,
    )(Utils.blame)(Utils.origen("advanceTime"))
  }

  private def create_newRunnableTasks(
      schedulerPerms_ref: Ref[N, InstancePredicate[N]],
      vesuv_limit_entries_ref: Ref[N, InstancePredicate[N]],
      vesuv_injective_ref: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // Quantifier variables
    val i3: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i4: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))

    // requires schedulerPerms();
    val requires: Expr[N] = Utils
      .predicate_apply(Utils.thiz, schedulerPerms_ref, Seq())

    // ensures |runnableQueue| + |\result| <= |taskPriority|;
    val ensures1: Expr[N] =
      LessEq(
        Plus(Utils.size(runnableQueue.get), Size(Utils.result)(Utils.origen))(
          Utils.origen
        ),
        Utils.size(taskPriority.get),
      )(Utils.origen)

    // ensures vesuv_limit_entries(\result, 0, |taskState|);
    val ensures2: Expr[N] = Utils.predicate_apply(
      Utils.thiz,
      vesuv_limit_entries_ref,
      Seq(Utils.result, Utils.int_val(0), Utils.size(taskState.get)),
    )

    // ensures vesuv_injective(\result);
    val ensures3: Expr[N] = Utils
      .predicate_apply(Utils.thiz, vesuv_injective_ref, Seq(Utils.result))

    // ensures (\forall int i; 0 <= i && i < |taskState| && taskState[i] >= 0 && eventState[taskState[i]] == 0 ==> i in \result);
    val ensures4: Expr[N] = Utils.single_var_forall(
      i3,
      Utils.int_val(0),
      Utils.size(taskState.get),
      Implies(
        And(
          GreaterEq(
            Utils.subscript_expr(taskState.get, Utils.local_of(i3)),
            Utils.int_val(0),
          )(Utils.origen),
          Eq(
            Utils.subscript_expr(
              eventState.get,
              Utils.subscript_expr(taskState.get, Utils.local_of(i3)),
            ),
            Utils.int_val(0),
          )(Utils.origen),
        )(Utils.origen),
        SeqMember(Utils.local_of(i3), Utils.result)(Utils.origen),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < |taskState| && !(taskState[i] >= 0 && eventState[taskState[i]] == 0) ==> !(i in \result));
    val ensures5: Expr[N] = Utils.single_var_forall(
      i4,
      Utils.int_val(0),
      Utils.size(taskState.get),
      Implies(
        Not(
          And(
            GreaterEq(
              Utils.subscript_expr(taskState.get, Utils.local_of(i4)),
              Utils.int_val(0),
            )(Utils.origen),
            Eq(
              Utils.subscript_expr(
                eventState.get,
                Utils.subscript_expr(taskState.get, Utils.local_of(i4)),
              ),
              Utils.int_val(0),
            )(Utils.origen),
          )(Utils.origen)
        )(Utils.origen),
        Not(SeqMember(Utils.local_of(i4), Utils.result)(Utils.origen))(
          Utils.origen
        ),
      )(Utils.origen),
    )

    new InstanceMethod(
      Utils.tseqint,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        requires,
        Utils.fold_star(
          Seq[Expr[N]](ensures1, ensures2, ensures3, ensures4, ensures5)
        ),
      ),
      false,
      true,
    )(Utils.blame)(Utils.origen("newRunnableTasks"))
  }

  private def create_resumeTasks(
      schedulerPerms_ref: Ref[N, InstancePredicate[N]]
  ): InstanceMethod[N] = {
    // Quantifier variables
    val i2: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i3: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))

    // requires schedulerPerms();
    // ensures schedulerPerms();
    val context: Expr[N] = Utils
      .predicate_apply(Utils.thiz, schedulerPerms_ref, Seq())

    // ensures    eventState == \old(eventState)
    //         && taskPriority == \old(taskPriority)
    //         && runnableQueue == \old(runnableQueue)
    //         && |taskState| == \old(|taskState|)
    //         && |taskWaitTime| == \old(|taskWaitTime|);
    val ensures1: Expr[N] = Utils.fold_and(Seq[Expr[N]](
      Utils.unchanged(Utils.deref_of(eventState.get)),
      Utils.unchanged(Utils.deref_of(taskPriority.get)),
      Utils.unchanged(Utils.deref_of(runnableQueue.get)),
      Utils.unchanged(Utils.size(taskState.get)),
      Utils.unchanged(Utils.size(taskWaitTime.get)),
    ))

    // ensures (\forall int i; 0 <= i && i < \old(|taskState|) && \old(taskState[i]) >= 0 && \old(eventState[taskState[i]]) == 0
    //                                                      ==> taskState[i] == -1 && taskWaitTime[i] == 0);
    val ensures2: Expr[N] = Utils.single_var_forall(
      i2,
      Utils.int_val(0),
      Utils.old(Utils.size(taskState.get)),
      Implies(
        And(
          GreaterEq(
            Utils.old(Utils.subscript_expr(taskState.get, Utils.local_of(i2))),
            Utils.int_val(0),
          )(Utils.origen),
          Eq(
            Utils.old(Utils.subscript_expr(
              eventState.get,
              Utils.subscript_expr(taskState.get, Utils.local_of(i2)),
            )),
            Utils.int_val(0),
          )(Utils.origen),
        )(Utils.origen),
        And(
          Eq(
            Utils.subscript_expr(taskState.get, Utils.local_of(i2)),
            Utils.int_val(-1),
          )(Utils.origen),
          Eq(
            Utils.subscript_expr(taskWaitTime.get, Utils.local_of(i2)),
            Utils.int_val(0),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < \old(|taskState|) && !(\old(taskState[i]) >= 0 && \old(eventState[taskState[i]]) == 0)
    //                                                      ==> taskState[i] == \old(taskState[i]) && taskWaitTime[i] == \old(taskWaitTime[i]));
    val ensures3: Expr[N] = Utils.single_var_forall(
      i3,
      Utils.int_val(0),
      Utils.old(Utils.size(taskState.get)),
      Implies(
        Not(
          And(
            GreaterEq(
              Utils
                .old(Utils.subscript_expr(taskState.get, Utils.local_of(i3))),
              Utils.int_val(0),
            )(Utils.origen),
            Eq(
              Utils.old(Utils.subscript_expr(
                eventState.get,
                Utils.subscript_expr(taskState.get, Utils.local_of(i3)),
              )),
              Utils.int_val(0),
            )(Utils.origen),
          )(Utils.origen)
        )(Utils.origen),
        And(
          Utils
            .unchanged(Utils.subscript_expr(taskState.get, Utils.local_of(i3))),
          Utils.unchanged(
            Utils.subscript_expr(taskWaitTime.get, Utils.local_of(i3))
          ),
        )(Utils.origen),
      )(Utils.origen),
    )

    new InstanceMethod(
      Utils.tvoid,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq[Expr[N]](context, ensures1, ensures2, ensures3)),
      ),
      false,
      false,
    )(Utils.blame)(Utils.origen("resumeTasks"))
  }

  private def create_resetEvents(
      eventPerms_ref: Ref[N, InstancePredicate[N]]
  ): InstanceMethod[N] = {
    // Quantifier variables
    val i2: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i3: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))

    // requires eventPerms();
    // ensures eventPerms();
    val context: Expr[N] = Utils
      .predicate_apply(Utils.thiz, eventPerms_ref, Seq())

    // ensures |eventState| == \old(|eventState|);
    val ensures1: Expr[N] = Utils.unchanged(Utils.size(eventState.get))

    // ensures (\forall int i; 0 <= i && i < \old(|eventState|) && \old(eventState[i]) == 0 ==> eventState[i] == -1);
    val ensures2: Expr[N] = Utils.single_var_forall(
      i2,
      Utils.int_val(0),
      Utils.old(Utils.size(eventState.get)),
      Implies(
        Eq(
          Utils.old(Utils.subscript_expr(eventState.get, Utils.local_of(i2))),
          Utils.int_val(0),
        )(Utils.origen),
        Eq(
          Utils.subscript_expr(eventState.get, Utils.local_of(i2)),
          Utils.int_val(-1),
        )(Utils.origen),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < \old(|eventState|) && \old(eventState[i]) != 0 ==> eventState[i] == \old(eventState[i]));
    val ensures3: Expr[N] = Utils.single_var_forall(
      i3,
      Utils.int_val(0),
      Utils.old(Utils.size(eventState.get)),
      Implies(
        Neq(
          Utils.old(Utils.subscript_expr(eventState.get, Utils.local_of(i3))),
          Utils.int_val(0),
        )(Utils.origen),
        Utils
          .unchanged(Utils.subscript_expr(eventState.get, Utils.local_of(i3))),
      )(Utils.origen),
    )

    new InstanceMethod(
      Utils.tvoid,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq[Expr[N]](context, ensures1, ensures2, ensures3)),
      ),
      false,
      false,
    )(Utils.blame)(Utils.origen("resetEvents"))
  }

  private def create_selectNextTask(
      schedulerPerms_ref: Ref[N, InstancePredicate[N]]
  ): InstanceMethod[N] = {
    // Quantifier variables
    val i2: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i31: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i32: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i33: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))

    // requires schedulerPerms();
    val requires: Expr[N] = Utils
      .predicate_apply(Utils.thiz, schedulerPerms_ref, Seq())

    // ensures -1 <= \result && \result < |runnableQueue|;
    val ensures1: Expr[N] =
      And(
        LessEq(Utils.int_val(-1), Utils.result)(Utils.origen),
        Less(Utils.result, Utils.size(runnableQueue.get))(Utils.origen),
      )(Utils.origen)

    // ensures \result == -1 ==> (|runnableQueue| == 0 || (\exists int i; 0 <= i && i < |taskState| && taskState[i] == -2));
    val ensures2: Expr[N] =
      Implies(
        Eq(Utils.result, Utils.int_val(-1))(Utils.origen),
        Or(
          Eq(Utils.size(runnableQueue.get), Utils.int_val(0))(Utils.origen),
          Utils.single_var_exists(
            i2,
            Utils.int_val(0),
            Utils.size(taskState.get),
            Eq(
              Utils.subscript_expr(taskState.get, Utils.local_of(i2)),
              Utils.int_val(-2),
            )(Utils.origen),
          ),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \result != -1 ==>    (\forall int i; 0 <= i && i < \result ==> taskPriority[runnableQueue[i]] < taskPriority[runnableQueue[\result]])
    //                           && (\forall int i; \result < i && i < |runnableQueue| ==> taskPriority[runnableQueue[i]] <= taskPriority[runnableQueue[\result]])
    //                           && (\forall int i; 0 <= i && i < |taskState| ==> taskState[i] != -2);
    val ensures3: Expr[N] =
      Implies(
        Neq(Utils.result, Utils.int_val(-1))(Utils.origen),
        Utils.fold_and(Seq[Expr[N]](
          Utils.single_var_forall(
            i31,
            Utils.int_val(0),
            Utils.result,
            Less(
              Utils.subscript_expr(
                taskPriority.get,
                Utils.subscript_expr(runnableQueue.get, Utils.local_of(i31)),
              ),
              Utils.subscript_expr(
                taskPriority.get,
                Utils.subscript_expr(runnableQueue.get, Utils.result),
              ),
            )(Utils.origen),
          ),
          Forall(
            Seq(i32),
            Seq(),
            Implies(
              And(
                Less(Utils.result, Utils.local_of(i32))(Utils.origen),
                Less(Utils.local_of(i32), Utils.size(runnableQueue.get))(
                  Utils.origen
                ),
              )(Utils.origen),
              LessEq(
                Utils.subscript_expr(
                  taskPriority.get,
                  Utils.subscript_expr(runnableQueue.get, Utils.local_of(i32)),
                ),
                Utils.subscript_expr(
                  taskPriority.get,
                  Utils.subscript_expr(runnableQueue.get, Utils.result),
                ),
              )(Utils.origen),
            )(Utils.origen),
          )(Utils.origen),
          Utils.single_var_forall(
            i33,
            Utils.int_val(0),
            Utils.size(taskState.get),
            Neq(
              Utils.subscript_expr(taskState.get, Utils.local_of(i33)),
              Utils.int_val(-2),
            )(Utils.origen),
          ),
        )),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        requires,
        Utils.fold_star(Seq(ensures1, ensures2, ensures3)),
      ),
      false,
      true,
    )(Utils.blame)(Utils.origen("selectNextTask"))
  }

  private def create_awokenAfterDelay(
      schedulerPerms_ref: Ref[N, InstancePredicate[N]],
      vesuv_limit_entries_ref: Ref[N, InstancePredicate[N]],
      vesuv_injective_ref: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    val delay: Variable[N] = new Variable(Utils.tint)(Utils.origen("delay"))

    // Quantifier variables
    val i1: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i2: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i6: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val j6: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))

    // requires schedulerPerms();
    val requires: Expr[N] = Utils
      .predicate_apply(Utils.thiz, schedulerPerms_ref, Seq())

    // ensures (\forall int i; 0 <= i && i < |taskState| ==>
    //                        (taskState[i] >= 0 && eventState[taskState[i]] >= 0 && eventState[taskState[i]] <= delay) ==> i in \result);
    val ensures1: Expr[N] = Utils.single_var_forall(
      i1,
      Utils.int_val(0),
      Utils.size(taskState.get),
      Implies(
        Utils.fold_and(Seq[Expr[N]](
          GreaterEq(
            Utils.subscript_expr(taskState.get, Utils.local_of(i1)),
            Utils.int_val(0),
          )(Utils.origen),
          GreaterEq(
            Utils.subscript_expr(
              eventState.get,
              Utils.subscript_expr(taskState.get, Utils.local_of(i1)),
            ),
            Utils.int_val(0),
          )(Utils.origen),
          LessEq(
            Utils.subscript_expr(
              eventState.get,
              Utils.subscript_expr(taskState.get, Utils.local_of(i1)),
            ),
            Utils.local_of(delay),
          )(Utils.origen),
        )),
        SeqMember(Utils.local_of(i1), Utils.result)(Utils.origen),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < |taskState| ==>
    //                        (taskState[i] < 0 || eventState[taskState[i]] < 0 || eventState[taskState[i]] > delay) ==> !(i in \result));
    val ensures2: Expr[N] = Utils.single_var_forall(
      i2,
      Utils.int_val(0),
      Utils.size(taskState.get),
      Implies(
        Utils.fold_or(Seq[Expr[N]](
          Less(
            Utils.subscript_expr(taskState.get, Utils.local_of(i2)),
            Utils.int_val(0),
          )(Utils.origen),
          Less(
            Utils.subscript_expr(
              eventState.get,
              Utils.subscript_expr(taskState.get, Utils.local_of(i2)),
            ),
            Utils.int_val(0),
          )(Utils.origen),
          Greater(
            Utils.subscript_expr(
              eventState.get,
              Utils.subscript_expr(taskState.get, Utils.local_of(i2)),
            ),
            Utils.local_of(delay),
          )(Utils.origen),
        )),
        Not(SeqMember(Utils.local_of(i2), Utils.result)(Utils.origen))(
          Utils.origen
        ),
      )(Utils.origen),
    )

    // ensures vesuv_limit_entries(\result, 0, |taskState|);
    val ensures3: Expr[N] = Utils.predicate_apply(
      Utils.thiz,
      vesuv_limit_entries_ref,
      Seq(Utils.result, Utils.int_val(0), Utils.size(taskState.get)),
    )

    // ensures vesuv_injective(\result);
    val ensures4: Expr[N] = Utils
      .predicate_apply(Utils.thiz, vesuv_injective_ref, Seq(Utils.result))

    // ensures |runnableQueue| + |\result| <= |taskPriority|;
    val ensures5: Expr[N] =
      LessEq(
        Plus(Utils.size(runnableQueue.get), Size(Utils.result)(Utils.origen))(
          Utils.origen
        ),
        Utils.size(taskPriority.get),
      )(Utils.origen)

    // ensures (\forall int i; 0 <= i && i < |\result| ==> (\forall int j; 0 <= j && j < i ==> (eventState[\result[j]] <= eventState[\result[i]])));
    val ensures6: Expr[N] = Utils.single_var_forall(
      i6,
      Utils.int_val(0),
      Size(Utils.result)(Utils.origen),
      Utils.single_var_forall(
        j6,
        Utils.int_val(0),
        Utils.local_of(i6),
        LessEq(
          SeqSubscript(
            Utils.deref_of(eventState.get),
            SeqSubscript(Utils.result, Utils.local_of(j6))(Utils.blame)(
              Utils.origen
            ),
          )(Utils.blame)(Utils.origen),
          SeqSubscript(
            Utils.deref_of(eventState.get),
            SeqSubscript(Utils.result, Utils.local_of(i6))(Utils.blame)(
              Utils.origen
            ),
          )(Utils.blame)(Utils.origen),
        )(Utils.origen),
      ),
    )

    new InstanceMethod(
      Utils.tseqint,
      Seq(delay),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        requires,
        Utils.fold_star(
          Seq(ensures1, ensures2, ensures3, ensures4, ensures5, ensures6)
        ),
      ),
      false,
      true,
    )(Utils.blame)(Utils.origen("awokenAfterDelay"))
  }

  private def create_simulateTimePassing(
      schedulerPerms_ref: Ref[N, InstancePredicate[N]]
  ): InstanceMethod[N] = {
    val delay: Variable[N] = new Variable(Utils.tint)(Utils.origen("delay"))

    // Quantifier variables
    val i2: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i3: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val j21: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))
    val j22: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))
    val j23: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))

    // requires schedulerPerms();
    // ensures schedulerPerms();
    val context: Expr[N] = Utils
      .predicate_apply(Utils.thiz, schedulerPerms_ref, Seq())

    // ensures    runnableQueue == \old(runnableQueue)
    //         && taskPriority == \old(taskPriority)
    //         && |eventState| == \old(|eventState|)
    //         && |taskState| == \old(|taskState|)
    //         && |taskWaitTime| == \old(|taskWaitTime|);
    val ensures1: Expr[N] = Utils.fold_and(Seq[Expr[N]](
      Utils.unchanged(Utils.deref_of(runnableQueue.get)),
      Utils.unchanged(Utils.deref_of(taskPriority.get)),
      Utils.unchanged(Utils.size(eventState.get)),
      Utils.unchanged(Utils.size(taskState.get)),
      Utils.unchanged(Utils.size(taskWaitTime.get)),
    ))

    // ensures (\forall int i; 0 <= i && i < \old(|eventState|) ==>
    //                 (   (    \old(eventState[i]) <= -1
    //                      ==> (   {: eventState[i] :} == -1
    //                           && (\forall int j; 0 <= j && j < \old(|taskState|) && \old(taskState[j]) == i ==>
    //                                      ({: taskState[j] :} == \old(taskState[j]) && taskWaitTime[j] == \old(taskWaitTime[j]))
    //                              )))
    //                  && (    (0 <= \old(eventState[i]) && \old(eventState[i]) <= delay)
    //                      ==> (   eventState[i] == -1
    //                           && (\forall int j; 0 <= j && j < \old(|taskState|) && \old(taskState[j]) == i ==>
    //                                      ({: taskState[j] :} == -1 && taskWaitTime[j] == delay - \old(eventState[i]))
    //                              )))
    //                  && (    \old(eventState[i]) > delay
    //                      ==> (   eventState[i] == \old(eventState[i]) - delay
    //                           && (\forall int j; 0 <= j && j < \old(|taskState|) && \old(taskState[j]) == i ==>
    //                                      ({:1: taskState[j] :} == \old(taskState[j]) && {:2: taskWaitTime[j] :} == \old(taskWaitTime[j]))
    //                              )))
    //                 )
    //         );
    val ensures2: Expr[N] = Utils.single_var_forall(
      i2,
      Utils.int_val(0),
      Utils.old(Utils.size(eventState.get)),
      Utils.fold_and(Seq[Expr[N]](
        Implies(
          LessEq(
            Utils.old(Utils.subscript_expr(eventState.get, Utils.local_of(i2))),
            Utils.int_val(-1),
          )(Utils.origen),
          And(
            Eq(
              Utils.subscript_expr(eventState.get, Utils.local_of(i2)),
              Utils.int_val(-1),
            )(Utils.origen),
            Utils.single_var_forall(
              j21,
              Utils.int_val(0),
              Utils.old(Utils.size(taskState.get)),
              Implies(
                Eq(
                  Utils.old(
                    Utils.subscript_expr(taskState.get, Utils.local_of(j21))
                  ),
                  Utils.local_of(i2),
                )(Utils.origen),
                And(
                  Utils.unchanged(
                    Utils.subscript_expr(taskState.get, Utils.local_of(j21))
                  ),
                  Utils.unchanged(
                    Utils.subscript_expr(taskWaitTime.get, Utils.local_of(j21))
                  ),
                )(Utils.origen),
              )(Utils.origen),
            ),
          )(Utils.origen),
        )(Utils.origen),
        Implies(
          And(
            LessEq(
              Utils.int_val(0),
              Utils
                .old(Utils.subscript_expr(eventState.get, Utils.local_of(i2))),
            )(Utils.origen),
            LessEq(
              Utils
                .old(Utils.subscript_expr(eventState.get, Utils.local_of(i2))),
              Utils.local_of(delay),
            )(Utils.origen),
          )(Utils.origen),
          And(
            Eq(
              Utils.subscript_expr(eventState.get, Utils.local_of(i2)),
              Utils.int_val(-1),
            )(Utils.origen),
            Utils.single_var_forall(
              j22,
              Utils.int_val(0),
              Utils.old(Utils.size(taskState.get)),
              Implies(
                Eq(
                  Utils.old(
                    Utils.subscript_expr(taskState.get, Utils.local_of(j22))
                  ),
                  Utils.local_of(i2),
                )(Utils.origen),
                And(
                  Eq(
                    Utils.subscript_expr(taskState.get, Utils.local_of(j22)),
                    Utils.int_val(-1),
                  )(Utils.origen),
                  Eq(
                    Utils.subscript_expr(taskWaitTime.get, Utils.local_of(j22)),
                    Minus(
                      Utils.local_of(delay),
                      Utils.old(
                        Utils.subscript_expr(eventState.get, Utils.local_of(i2))
                      ),
                    )(Utils.origen),
                  )(Utils.origen),
                )(Utils.origen),
              )(Utils.origen),
            ),
          )(Utils.origen),
        )(Utils.origen),
        Implies(
          Greater(
            Utils.old(Utils.subscript_expr(eventState.get, Utils.local_of(i2))),
            Utils.local_of(delay),
          )(Utils.origen),
          And(
            Eq(
              Utils.subscript_expr(eventState.get, Utils.local_of(i2)),
              Minus(
                Utils.old(
                  Utils.subscript_expr(eventState.get, Utils.local_of(i2))
                ),
                Utils.local_of(delay),
              )(Utils.origen),
            )(Utils.origen),
            Utils.single_var_forall(
              j23,
              Utils.int_val(0),
              Utils.old(Utils.size(taskState.get)),
              Implies(
                Eq(
                  Utils.old(
                    Utils.subscript_expr(taskState.get, Utils.local_of(j23))
                  ),
                  Utils.local_of(i2),
                )(Utils.origen),
                And(
                  Utils.unchanged(
                    Utils.subscript_expr(taskState.get, Utils.local_of(j23))
                  ),
                  Utils.unchanged(
                    Utils.subscript_expr(taskWaitTime.get, Utils.local_of(j23))
                  ),
                )(Utils.origen),
              )(Utils.origen),
            ),
          )(Utils.origen),
        )(Utils.origen),
      )),
    )

    // ensures (\forall int i; 0 <= i && i < \old(|taskState|) && \old(taskState[i]) < 0 ==>
    //                (taskState[i] == \old(taskState[i]) && taskWaitTime[i] == \old(taskWaitTime[i]) + delay)
    //        );
    val ensures3: Expr[N] = Utils.single_var_forall(
      i3,
      Utils.int_val(0),
      Utils.old(Utils.size(taskState.get)),
      Implies(
        Less(
          Utils.old(Utils.subscript_expr(taskState.get, Utils.local_of(i3))),
          Utils.int_val(0),
        )(Utils.origen),
        And(
          Utils
            .unchanged(Utils.subscript_expr(taskState.get, Utils.local_of(i3))),
          Eq(
            Utils.subscript_expr(taskWaitTime.get, Utils.local_of(i3)),
            Plus(
              Utils.old(
                Utils.subscript_expr(taskWaitTime.get, Utils.local_of(i3))
              ),
              Utils.local_of(delay),
            )(Utils.origen),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen),
    )

    new InstanceMethod(
      Utils.tvoid,
      Seq(delay),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3)),
      ),
      false,
      false,
    )(Utils.blame)(Utils.origen("simulateTimePassing"))
  }

  private def create_executionTime(): InstanceMethod[N] = {
    val bcet: Variable[N] = new Variable(Utils.tint)(Utils.origen("bcet"))
    val wcet: Variable[N] = new Variable(Utils.tint)(Utils.origen("wcet"))

    // ensures bcet <= \result && \result <= wcet;
    val ensures: Expr[N] =
      And(
        LessEq(Utils.local_of(bcet), Utils.result)(Utils.origen),
        LessEq(Utils.result, Utils.local_of(wcet))(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(bcet, wcet),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(tt, ensures),
      false,
      true,
    )(Utils.blame)(Utils.origen("executionTime"))
  }

  private def create_instantiateEventTriggers(
      eventPerms_ref: Ref[N, InstancePredicate[N]],
      n_events: Int,
  ): InstanceMethod[N] = {
    // requires eventPerms();
    val requires1: Expr[N] = Utils
      .predicate_apply(Utils.thiz, eventPerms_ref, Seq())

    // requires eventState[0] >= -1 && ... && eventState[__n__] >= -1;
    val requires2: Expr[N] = Utils.fold_and(Seq.range(0, n_events).map(i =>
      GreaterEq(Utils.subscript(eventState.get, i), Utils.int_val(-1))(
        Utils.origen
      )
    ))

    new InstanceMethod(
      Utils.tvoid,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(Star(requires1, requires2)(Utils.origen), tt),
      false,
      true,
    )(Utils.blame)(Utils.origen("instantiateEventTriggers"))
  }

  private def create_schedule(
      nextEventDelay: Ref[N, InstanceMethod[N]],
      advanceTime: Ref[N, InstanceMethod[N]],
      newRunnableTasks: Ref[N, InstanceMethod[N]],
      resumeTasks: Ref[N, InstanceMethod[N]],
      resetEvents: Ref[N, InstanceMethod[N]],
      selectNextTask: Ref[N, InstanceMethod[N]],
      globalInvariant_ref: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    val schedulerDelay: Variable[N] =
      new Variable(Utils.tint)(Utils.origen("schedulerDelay"))
    val schedulerNext: Variable[N] =
      new Variable(Utils.tint)(Utils.origen("schedulerNext"))
    val awoken: Variable[N] =
      new Variable(Utils.tseqint)(Utils.origen("awoken"))

    // Quantifier variables
    val i: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))

    val lock_held: Expr[N] =
      Star(
        Held(Utils.thiz)(Utils.origen),
        Utils.predicate_apply(Utils.thiz, globalInvariant_ref, Seq()),
      )(Utils.origen)

    // int schedulerDelay;
    // int schedulerNext;
    // schedulerDelay = nextEventDelay();
    // if (schedulerDelay == 0 || (\forall int i; 0 <= i && i < |taskState| ==> taskState[i] >= 0)) {
    //     // assert schedulerDelay != -1;
    //     advanceTime(schedulerDelay);
    //     runnableQueue = runnableQueue + newRunnableTasks();
    //     resumeTasks();
    //     resetEvents();
    // }
    // schedulerNext = selectNextTask();
    // if (schedulerNext != -1) {
    //     taskState = taskState.update(runnableQueue[schedulerNext], -2);
    //     runnableQueue = runnableQueue.removeAt(schedulerNext);
    // }
    val body: Statement[N] =
      Block(Seq(
        LocalDecl(schedulerDelay)(Utils.origen),
        LocalDecl(schedulerNext)(Utils.origen),
        Assign(
          Utils.local_of(schedulerDelay),
          Utils.invoke(nextEventDelay, Seq()),
        )(Utils.blame)(Utils.origen),
        Branch(Seq((
          Or(
            Eq(Utils.local_of(schedulerDelay), Utils.int_val(0))(Utils.origen),
            Utils.single_var_forall(
              i,
              Utils.int_val(0),
              Utils.size(taskState.get),
              GreaterEq(
                Utils.subscript_expr(taskState.get, Utils.local_of(i)),
                Utils.int_val(0),
              )(Utils.origen),
            ),
          )(Utils.origen),
          Block(Seq(
            LocalDecl(awoken)(Utils.origen),
            Assert(Neq(Utils.local_of(schedulerDelay), Utils.int_val(-1))(
              Utils.origen
            ))(Utils.blame)(Utils.origen),
            Utils.stmt_invoke(advanceTime, Seq(Utils.local_of(schedulerDelay))),
            Assign(
              Utils.local_of(awoken),
              Utils.invoke(newRunnableTasks, Seq()),
            )(Utils.blame)(Utils.origen),
            Utils.stmt_invoke(resumeTasks, Seq()),
            Assign(
              Utils.deref_of(runnableQueue.get),
              Concat(Utils.deref_of(runnableQueue.get), Utils.local_of(awoken))(
                Utils.origen
              ),
            )(Utils.blame)(Utils.origen),
            Utils.stmt_invoke(resetEvents, Seq()),
          ))(Utils.origen),
        )))(Utils.origen),
        Assign(
          Utils.local_of(schedulerNext),
          Utils.invoke(selectNextTask, Seq()),
        )(Utils.blame)(Utils.origen),
        Branch(Seq((
          Neq(Utils.local_of(schedulerNext), Utils.int_val(-1))(Utils.origen),
          Block(Seq(
            Assign(
              Utils.deref_of(taskState.get),
              SeqUpdate(
                Utils.deref_of(taskState.get),
                Utils.subscript_expr(
                  runnableQueue.get,
                  Utils.local_of(schedulerNext),
                ),
                Utils.int_val(-2),
              )(Utils.origen),
            )(Utils.blame)(Utils.origen),
            Assign(
              Utils.deref_of(runnableQueue.get),
              RemoveAt(
                Utils.deref_of(runnableQueue.get),
                Utils.local_of(schedulerNext),
              )(Utils.origen),
            )(Utils.blame)(Utils.origen),
          ))(Utils.origen),
        )))(Utils.origen),
      ))(Utils.origen)

    new InstanceMethod(
      Utils.tvoid,
      Seq(),
      Seq(),
      Seq(),
      Some(body),
      Utils.to_app_contract(lock_held, lock_held),
      false,
      false,
    )(Utils.blame)(Utils.origen("schedule"))
  }

  private def create_start(
      to_launch: Seq[InstanceField[N]],
      preconditions: Seq[Expr[N]],
      pc_perms: Seq[Expr[N]],
      schedule: Ref[N, InstanceMethod[N]],
  ): InstanceMethod[N] = {
    val requires: Expr[N] = Utils.fold_star(
      (preconditions ++ pc_perms ++
        to_launch.map(f => IdleToken(Utils.deref_of(f))(Utils.origen))) :+
        Committed(Utils.thiz)(Utils.blame)(Utils.origen)
    )

    // lock this;
    // fork obj1;
    // fork obj2;
    // ...
    // fork objn;
    // unlock this;
    // while (true) {
    //     lock this;
    //     schedule();
    //     unlock this;
    // }
    val statements: Seq[Statement[N]] =
      Seq[Statement[N]](Lock(Utils.thiz)(Utils.blame)(Utils.origen)) ++
        to_launch
          .map(f => Fork[N](Utils.deref_of(f))(Utils.blame)(Utils.origen)) ++
        Seq[Statement[N]](
          Unlock(Utils.thiz)(Utils.blame)(Utils.origen),
          Loop(
            Utils.skip,
            tt,
            Utils.skip,
            Utils.to_loop_invariant(tt),
            Block(Seq[Statement[N]](
              Lock(Utils.thiz)(Utils.blame)(Utils.origen),
              Utils.stmt_invoke(schedule, Seq()),
              Unlock(Utils.thiz)(Utils.blame)(Utils.origen),
            ))(Utils.origen),
          )(Utils.origen),
        )

    val body: Statement[N] = Block(statements)(Utils.origen)

    new InstanceMethod(
      Utils.tvoid,
      Seq(),
      Seq(),
      Seq(),
      Some(body),
      Utils.to_app_contract(requires, tt),
      false,
      false,
    )(Utils.blame)(Utils.origen("start"))
  }
}
