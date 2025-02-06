package vct.rewrite.rtos

import vct.col.ast._
import vct.col.ref.{DirectRef, Ref}
import vct.col.util.AstBuildHelpers.tt

class SchedulerGenerator[O, N] {
  private var eventState: InstanceField[N] = ???
  private var taskState: InstanceField[N] = ???
  private var taskPriority: InstanceField[N] = ???
  private var taskWaitTime: InstanceField[N] = ???
  private var runnableQueue: InstanceField[N] = ???
  private var priorityPerms: InstancePredicate[N] = ???
  private var eventPerms: InstancePredicate[N] = ???
  private var schedulerPerms: InstancePredicate[N] = ???
  private var globalInvariant: InstancePredicate[N] = ???
  private var simulateTimePassing: InstanceMethod[N] = ???
  private var executionTime: InstanceMethod[N] = ???

  def get_eventState: InstanceField[N] = eventState
  def get_taskState: InstanceField[N] = taskState
  def get_taskPriority: InstanceField[N] = taskPriority
  def get_taskWaitTime: InstanceField[N] = taskWaitTime
  def get_runnableQueue: InstanceField[N] = runnableQueue
  def get_priorityPerms: InstancePredicate[N] = priorityPerms
  def get_eventPerms: InstancePredicate[N] = eventPerms
  def get_schedulerPerms: InstancePredicate[N] = schedulerPerms
  def get_globalInvariant: InstancePredicate[N] = globalInvariant
  def get_simulateTimePassing: InstanceMethod[N] = simulateTimePassing
  def get_executionTime: InstanceMethod[N] = executionTime

  def generate(objs: Seq[ObjectInfo[O, N]], n_events: Int): Class[N] = {
    // Support calculations
    val n_tasks = objs.count(o => o.task_id.nonEmpty)

    // Scheduling variables
    eventState = new InstanceField(Utils.tseqint, Seq())(Utils.origen("eventState"))
    taskState = new InstanceField(Utils.tseqint, Seq())(Utils.origen("taskState"))
    taskPriority = new InstanceField(Utils.tseqint, Seq())(Utils.origen("taskPriority"))
    taskWaitTime = new InstanceField(Utils.tseqint, Seq())(Utils.origen("taskWaitTime"))
    runnableQueue = new InstanceField(Utils.tseqint, Seq())(Utils.origen("runnableQueue"))

    // Scheduler predicates
    priorityPerms =
      new InstancePredicate(
        Seq(),
        Some(
          Star(
            Perm(Utils.loc_of(taskPriority), Utils.write)(Utils.origen),
            Eq(Utils.size(taskPriority), Utils.int_val(n_tasks))(Utils.origen),
          )(Utils.origen)
        ),
        false,
        true,
      )(Utils.origen("priorityPerms"))
    val priorityPerms_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](priorityPerms)
    eventPerms =
      new InstancePredicate(
        Seq(),
        Some(
          Star(
            Perm(Utils.loc_of(eventState), Utils.write)(Utils.origen),
            Eq(Utils.size(eventState), Utils.int_val(n_events))(Utils.origen),
          )(Utils.origen)
        ),
        false,
        true,
      )(Utils.origen("eventPerms"))
    val eventPerms_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](eventPerms)
    schedulerPerms = create_schedulerPerms(
      eventPerms_ref,
      priorityPerms_ref,
    )
    val schedulerPerms_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](schedulerPerms)
    val globalPerms: InstancePredicate[N] =
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(
          Seq(Utils.predicate_apply(Utils.thiz, schedulerPerms_ref, Seq())) ++
            objs.map(o => o.perms)
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
    globalInvariant =
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
      )(Utils.origen)
    val globalInvariant_ref: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](globalInvariant)

    // Constructor
    val schedulerConstructor: PVLConstructor[N] = create_constructor(
      objs,
      n_events,
      n_tasks,
    )

    // Helper methods
    val nextEventDelay: InstanceMethod[N] = create_nextEventDelay(
      eventPerms_ref
    )
    val advanceTime: InstanceMethod[N] = create_advanceTime(
      eventPerms_ref
    )
    val resumeTasks: InstanceMethod[N] = create_resumeTasks(
      schedulerPerms_ref
    )
    val resetEvents: InstanceMethod[N] = create_resetEvents(
      eventPerms_ref
    )
    val selectNextTask: InstanceMethod[N] = create_selectNextTask(
      schedulerPerms_ref
    )
    simulateTimePassing = create_simulateTimePassing(schedulerPerms_ref)
    executionTime = create_executionTime()

    // Methods
    val schedule: InstanceMethod[N] = create_schedule(
      new DirectRef[N, InstanceMethod[N]](nextEventDelay),
      new DirectRef[N, InstanceMethod[N]](advanceTime),
      new DirectRef[N, InstanceMethod[N]](resumeTasks),
      new DirectRef[N, InstanceMethod[N]](resetEvents),
      new DirectRef[N, InstanceMethod[N]](selectNextTask),
      globalInvariant_ref,
    )
    val start: InstanceMethod[N] = create_start(
      objs.filter(o => o.launch).map(o => o.field),
      objs.filter(o => o.launch)
        .map(o => o.precondition_in_scheduler.getOrElse(tt)),
      new DirectRef[N, InstanceMethod[N]](schedule),
    )

    // Finalize class
    val decls: Seq[ClassDeclaration[N]] =
      // Scheduling variables
      Seq(eventState, taskState, taskPriority, taskWaitTime, runnableQueue) ++
        // Object fields
        objs.map(o => o.field) ++
        // Predicates
        Seq(
          priorityPerms,
          eventPerms,
          schedulerPerms,
          globalPerms,
          globalProperties,
        ) ++
        // Methods
        Seq(
          schedulerConstructor,
          start,
          schedule,
          nextEventDelay,
          advanceTime,
          resumeTasks,
          resetEvents,
          selectNextTask,
          simulateTimePassing,
          executionTime,
        )

    new ByReferenceClass(
      Seq(),
      decls,
      Seq(),
      Utils.predicate_apply(Utils.thiz, globalInvariant_ref, Seq()),
    )(Utils.origen("FreeRTOSScheduler"))
  }

  private def create_schedulerPerms(
      eventPerms_ref: Ref[N, InstancePredicate[N]],
      priorityPerms_ref: Ref[N, InstancePredicate[N]],
  ): InstancePredicate[N] = {
    // Quantifier variables
    val i1: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i2: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i3: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i4: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i5: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i6: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val j4: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))

    val conds: Seq[Expr[N]] = Seq[Expr[N]](
      Utils.predicate_apply(Utils.thiz, priorityPerms_ref, Seq()),
      Utils.predicate_apply(Utils.thiz, eventPerms_ref, Seq()),
      Perm(Utils.loc_of(taskState), Utils.write)(Utils.origen),
      Eq(Utils.size(taskState), Utils.size(taskPriority))(Utils.origen),
      Utils.single_var_forall(
        i1,
        Utils.int_val(0),
        Utils.size(taskState),
        And(
          GreaterEq(
            Utils.subscript_expr(taskState, Utils.local_of(i1)),
            Utils.int_val(-2),
          )(Utils.origen),
          Less(
            Utils.subscript_expr(taskState, Utils.local_of(i1)),
            Utils.size(eventState),
          )(Utils.origen),
        )(Utils.origen),
      ),
      Perm(Utils.loc_of(taskWaitTime), Utils.write)(Utils.origen),
      Eq(Utils.size(taskWaitTime), Utils.size(taskPriority))(Utils.origen),
      Utils.single_var_forall(
        i2,
        Utils.int_val(0),
        Utils.size(taskWaitTime),
        GreaterEq(
          Utils.subscript_expr(taskWaitTime, Utils.local_of(i2)),
          Utils.int_val(0),
        )(Utils.origen),
      ),
      Perm(Utils.loc_of(runnableQueue), Utils.write)(Utils.origen),
      LessEq(Utils.size(runnableQueue), Utils.size(taskPriority))(Utils.origen),
      Utils.single_var_forall(
        i3,
        Utils.int_val(0),
        Utils.size(runnableQueue),
        And(
          GreaterEq(
            Utils.subscript_expr(runnableQueue, Utils.local_of(i3)),
            Utils.int_val(0),
          )(Utils.origen),
          Less(
            Utils.subscript_expr(runnableQueue, Utils.local_of(i3)),
            Utils.size(taskPriority),
          )(Utils.origen),
        )(Utils.origen),
      ),
      Utils.single_var_forall(
        i4,
        Utils.int_val(0),
        Size(Utils.result)(Utils.origen),
        Forall(
          Seq(j4),
          Seq(),
          Implies(
            And(
              Less(Utils.local_of(i4), Utils.local_of(j4))(Utils.origen),
              Less(Utils.local_of(j4), Size(Utils.result)(Utils.origen))(
                Utils.origen
              ),
            )(Utils.origen),
            Neq(
              SeqSubscript(Utils.result, Utils.local_of(i4))(Utils.origen)(
                Utils.origen
              ),
              SeqSubscript(Utils.result, Utils.local_of(j4))(Utils.origen)(
                Utils.origen
              ),
            )(Utils.origen),
          )(Utils.origen),
        )(Utils.origen),
      ),
      Utils.single_var_forall(
        i5,
        Utils.int_val(0),
        Utils.size(runnableQueue),
        Eq(
          Utils.subscript_expr(
            taskState,
            Utils.subscript_expr(runnableQueue, Utils.local_of(i5)),
          ),
          Utils.int_val(-1),
        )(Utils.origen),
      ),
      Utils.single_var_forall(
        i6,
        Utils.int_val(0),
        Utils.size(taskState),
        Implies(
          Eq(
            Utils.subscript_expr(taskState, Utils.local_of(i6)),
            Utils.int_val(-1),
          )(Utils.origen),
          SeqMember(Utils.local_of(i6), Utils.deref_of(runnableQueue))(
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
      objs: Seq[ObjectInfo[O, N]],
      n_events: Int,
      n_tasks: Int,
  ): PVLConstructor[N] = {
    // Supporting calculations
    val launch_objs: Seq[ObjectInfo[O, N]] = objs.filter(o => o.launch)
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
    val runnableQueueInit: Seq[Int] = taskStateInit.filter(i => i == -1)

    // Constructor contract
    val ensures: Expr[N] = Utils.fold_star(
      (launch_objs.map(o => o.precondition_in_scheduler.getOrElse(tt)) ++
        launch_objs
          .map(o => IdleToken(Utils.deref_of(o.field))(Utils.origen))) :+
        Committed(Utils.thiz)(Utils.origen)(Utils.origen)
    )

    // Constructor body construction
    val statements: Seq[Statement[N]] =
      Seq(
        Assign[N](
          Utils.deref_of(eventState),
          Utils.seq_val(eventStateInit.map(i => Utils.int_val(i))),
        )(Utils.origen)(Utils.origen),
        Assign[N](
          Utils.deref_of(taskState),
          Utils.seq_val(taskStateInit.map(i => Utils.int_val(i))),
        )(Utils.origen)(Utils.origen),
        Assign[N](
          Utils.deref_of(taskPriority),
          Utils.seq_val(taskPriorityInit.map(i => Utils.int_val(i))),
        )(Utils.origen)(Utils.origen),
        Assign[N](
          Utils.deref_of(taskWaitTime),
          Utils.seq_val(taskWaitTimeInit.map(i => Utils.int_val(i))),
        )(Utils.origen)(Utils.origen),
        Assign[N](
          Utils.deref_of(runnableQueue),
          Utils.seq_val(runnableQueueInit.map(i => Utils.int_val(i))),
        )(Utils.origen)(Utils.origen),
      ) ++ runnableQueueInit.zipWithIndex.map(t =>
        Assert[N](Eq(Utils.subscript(runnableQueue, t._2), Utils.int_val(t._1))(
          Utils.origen
        ))(Utils.origen)(Utils.origen)
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
          )(Utils.origen)(Utils.origen),
        )(Utils.origen)(Utils.origen)
      ) :+ Commit(Utils.thiz)(Utils.origen)(Utils.origen)

    val body: Statement[N] = Block(statements)(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(tt, ensures),
      Seq(),
      Seq(),
      Some(body),
    )(Utils.origen)(Utils.origen)
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
      Utils.size(eventState),
      Or(
        Less(
          Utils.subscript_expr(eventState, Utils.local_of(i1)),
          Utils.int_val(0),
        )(Utils.origen),
        LessEq(
          Utils.result,
          Utils.subscript_expr(eventState, Utils.local_of(i1)),
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
          Utils.size(eventState),
          Less(
            Utils.subscript_expr(eventState, Utils.local_of(i2)),
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
          Utils.size(eventState),
          GreaterEq(
            Utils.subscript_expr(eventState, Utils.local_of(i31)),
            Utils.int_val(0),
          )(Utils.origen),
        ),
        Utils.single_var_exists(
          i32,
          Utils.int_val(0),
          Utils.size(eventState),
          And(
            GreaterEq(
              Utils.subscript_expr(eventState, Utils.local_of(i32)),
              Utils.int_val(0),
            )(Utils.origen),
            Eq(
              Utils.result,
              Utils.subscript_expr(eventState, Utils.local_of(i32)),
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
    )(Utils.origen)(Utils.origen("nextEventDelay"))
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
    val ensures1: Expr[N] = Utils.unchanged(Utils.size(eventState))

    // ensures (\forall int i; 0 <= i && i < |eventState| && \old(eventState[i]) < 0 ==> eventState[i] == -1);
    val ensures2: Expr[N] = Utils.single_var_forall(
      i2,
      Utils.int_val(0),
      Utils.size(eventState),
      Implies(
        Less(
          Utils.old(Utils.subscript_expr(eventState, Utils.local_of(i2))),
          Utils.int_val(0),
        )(Utils.origen),
        Eq(
          Utils.subscript_expr(eventState, Utils.local_of(i2)),
          Utils.int_val(-1),
        )(Utils.origen),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < |eventState| && \old(eventState[i]) >= 0 ==> eventState[i] == \old(eventState[i]) - advance);
    val ensures3: Expr[N] = Utils.single_var_forall(
      i3,
      Utils.int_val(0),
      Utils.size(eventState),
      Implies(
        GreaterEq(
          Utils.old(Utils.subscript_expr(eventState, Utils.local_of(i3))),
          Utils.int_val(0),
        )(Utils.origen),
        Eq(
          Utils.subscript_expr(eventState, Utils.local_of(i3)),
          Minus(
            Utils.old(Utils.subscript_expr(eventState, Utils.local_of(i3))),
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
    )(Utils.origen)(Utils.origen("advanceTime"))
  }

  private def create_resumeTasks(
      schedulerPerms_ref: Ref[N, InstancePredicate[N]]
  ): InstanceMethod[N] = {
    // Quantifier variables
    val i3: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i4: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i5: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i6: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val j4: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))

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
      Utils.unchanged(Utils.deref_of(eventState)),
      Utils.unchanged(Utils.deref_of(taskPriority)),
      Utils.unchanged(Utils.deref_of(runnableQueue)),
      Utils.unchanged(Utils.size(taskState)),
      Utils.unchanged(Utils.size(taskWaitTime)),
    ))

    // ensures \old(|runnableQueue|) + |\result| <= |taskPriority|;
    val ensures2: Expr[N] =
      LessEq(
        Plus(
          Utils.old(Utils.size(runnableQueue)),
          Size(Utils.result)(Utils.origen),
        )(Utils.origen),
        Utils.size(taskPriority),
      )(Utils.origen)

    // ensures (\forall int i; 0 <= i && i < |\result| ==> 0 <= \result[i] && \result[i] < |taskState|);
    val ensures3: Expr[N] = Utils.single_var_forall(
      i3,
      Utils.int_val(0),
      Size(Utils.result)(Utils.origen),
      And(
        LessEq(
          Utils.int_val(0),
          SeqSubscript(Utils.result, Utils.local_of(i3))(Utils.origen)(
            Utils.origen
          ),
        )(Utils.origen),
        Less(
          SeqSubscript(Utils.result, Utils.local_of(i3))(Utils.origen)(
            Utils.origen
          ),
          Utils.size(taskState),
        )(Utils.origen),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < |\result| ==> (\forall int j; i < j && j < |\result| ==> \result[i] != \result[j]));
    val ensures4: Expr[N] = Utils.single_var_forall(
      i4,
      Utils.int_val(0),
      Size(Utils.result)(Utils.origen),
      Forall(
        Seq(j4),
        Seq(),
        Implies(
          And(
            Less(Utils.local_of(i4), Utils.local_of(j4))(Utils.origen),
            Less(Utils.local_of(j4), Size(Utils.result)(Utils.origen))(
              Utils.origen
            ),
          )(Utils.origen),
          Neq(
            SeqSubscript(Utils.result, Utils.local_of(i4))(Utils.origen)(
              Utils.origen
            ),
            SeqSubscript(Utils.result, Utils.local_of(j4))(Utils.origen)(
              Utils.origen
            ),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < |taskState| && \old(taskState[i]) >= 0 && \old(eventState[taskState[i]]) == 0
    //                                                      ==> taskState[i] == -1 && i in \result && taskWaitTime[i] == 0);
    val ensures5: Expr[N] = Utils.single_var_forall(
      i5,
      Utils.int_val(0),
      Utils.size(taskState),
      Implies(
        And(
          GreaterEq(
            Utils.old(Utils.subscript_expr(taskState, Utils.local_of(i5))),
            Utils.int_val(0),
          )(Utils.origen),
          Eq(
            Utils.old(Utils.subscript_expr(
              eventState,
              Utils.subscript_expr(taskState, Utils.local_of(i5)),
            )),
            Utils.int_val(0),
          )(Utils.origen),
        )(Utils.origen),
        Utils.fold_and(Seq[Expr[N]](
          Eq(
            Utils.subscript_expr(taskState, Utils.local_of(i5)),
            Utils.int_val(-1),
          )(Utils.origen),
          SeqMember(Utils.local_of(i5), Utils.result)(Utils.origen),
          Eq(
            Utils.subscript_expr(taskWaitTime, Utils.local_of(i5)),
            Utils.int_val(0),
          )(Utils.origen),
        )),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < |taskState| && !(\old(taskState[i]) >= 0 && \old(eventState[taskState[i]]) == 0)
    //                                                      ==> taskState[i] == \old(taskState[i]) && !(i in \result) && taskWaitTime[i] == \old(taskWaitTime[i]));
    val ensures6: Expr[N] = Utils.single_var_forall(
      i6,
      Utils.int_val(0),
      Utils.size(taskState),
      Implies(
        Not(
          And(
            GreaterEq(
              Utils.old(Utils.subscript_expr(taskState, Utils.local_of(i6))),
              Utils.int_val(0),
            )(Utils.origen),
            Eq(
              Utils.old(Utils.subscript_expr(
                eventState,
                Utils.subscript_expr(taskState, Utils.local_of(i6)),
              )),
              Utils.int_val(0),
            )(Utils.origen),
          )(Utils.origen)
        )(Utils.origen),
        Utils.fold_and(Seq[Expr[N]](
          Utils.unchanged(Utils.subscript_expr(taskState, Utils.local_of(i6))),
          Not(SeqMember(Utils.local_of(i6), Utils.result)(Utils.origen))(
            Utils.origen
          ),
          Utils
            .unchanged(Utils.subscript_expr(taskWaitTime, Utils.local_of(i6))),
        )),
      )(Utils.origen),
    )

    new InstanceMethod(
      Utils.tseqint,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq[Expr[N]](
          context,
          ensures1,
          ensures2,
          ensures3,
          ensures4,
          ensures5,
          ensures6,
        )),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("resumeTasks"))
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
    val ensures1: Expr[N] = Utils.unchanged(Utils.size(eventState))

    // ensures (\forall int i; 0 <= i && i < |eventState| && \old(eventState[i]) == 0 ==> eventState[i] == -1);
    val ensures2: Expr[N] = Utils.single_var_forall(
      i2,
      Utils.int_val(0),
      Utils.size(eventState),
      Implies(
        Eq(
          Utils.old(Utils.subscript_expr(eventState, Utils.local_of(i2))),
          Utils.int_val(0),
        )(Utils.origen),
        Eq(
          Utils.subscript_expr(eventState, Utils.local_of(i2)),
          Utils.int_val(-1),
        )(Utils.origen),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < |eventState| && \old(eventState[i]) != 0 ==> eventState[i] == \old(eventState[i]));
    val ensures3: Expr[N] = Utils.single_var_forall(
      i3,
      Utils.int_val(0),
      Utils.size(eventState),
      Implies(
        Neq(
          Utils.old(Utils.subscript_expr(eventState, Utils.local_of(i3))),
          Utils.int_val(0),
        )(Utils.origen),
        Utils.unchanged(Utils.subscript_expr(eventState, Utils.local_of(i3))),
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
    )(Utils.origen)(Utils.origen("resetEvents"))

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
        Less(Utils.result, Utils.size(runnableQueue))(Utils.origen),
      )(Utils.origen)

    // ensures \result == -1 ==> (|runnableQueue| == 0 || (\exists int i; 0 <= i && i < |taskState| && taskState[i] == -2));
    val ensures2: Expr[N] =
      Implies(
        Eq(Utils.result, Utils.int_val(-1))(Utils.origen),
        Or(
          Eq(Utils.size(runnableQueue), Utils.int_val(0))(Utils.origen),
          Utils.single_var_exists(
            i2,
            Utils.int_val(0),
            Utils.size(taskState),
            Eq(
              Utils.subscript_expr(taskState, Utils.local_of(i2)),
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
                taskPriority,
                Utils.subscript_expr(runnableQueue, Utils.local_of(i31)),
              ),
              Utils.subscript_expr(
                taskPriority,
                Utils.subscript_expr(runnableQueue, Utils.result),
              ),
            )(Utils.origen),
          ),
          Forall(
            Seq(i32),
            Seq(),
            Implies(
              And(
                Less(Utils.result, Utils.local_of(i32))(Utils.origen),
                Less(Utils.local_of(i32), Utils.size(runnableQueue))(
                  Utils.origen
                ),
              )(Utils.origen),
              LessEq(
                Utils.subscript_expr(
                  taskPriority,
                  Utils.subscript_expr(runnableQueue, Utils.local_of(i32)),
                ),
                Utils.subscript_expr(
                  taskPriority,
                  Utils.subscript_expr(runnableQueue, Utils.result),
                ),
              )(Utils.origen),
            )(Utils.origen),
          )(Utils.origen),
          Utils.single_var_forall(
            i33,
            Utils.int_val(0),
            Utils.size(taskState),
            Neq(
              Utils.subscript_expr(taskState, Utils.local_of(i33)),
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
    )(Utils.origen)(Utils.origen("selectNextTask"))
  }

  private def create_simulateTimePassing(
      schedulerPerms_ref: Ref[N, InstancePredicate[N]]
  ): InstanceMethod[N] = {
    val delay: Variable[N] = new Variable(Utils.tint)(Utils.origen("delay"))

    // Quantifier variables
    val i2: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i3: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i4: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i5: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val i7: Variable[N] = new Variable(Utils.tint)(Utils.origen("i"))
    val j21: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))
    val j22: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))
    val j23: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))
    val j5: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))
    val j7: Variable[N] = new Variable(Utils.tint)(Utils.origen("j"))

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
      Utils.unchanged(Utils.deref_of(runnableQueue)),
      Utils.unchanged(Utils.deref_of(taskPriority)),
      Utils.unchanged(Utils.size(eventState)),
      Utils.unchanged(Utils.size(taskState)),
      Utils.unchanged(Utils.size(taskWaitTime)),
    ))

    // ensures (\forall int i; 0 <= i && i < |eventState| ==>
    //                 (   (    \old(eventState[i]) <= -1
    //                      ==> (   {: eventState[i] :} == -1
    //                           && (\forall int j; 0 <= j && j < |taskState| && \old(taskState[j]) == i ==>
    //                                      ({: taskState[j] :} == \old(taskState[j]) && !(j in \result) && taskWaitTime[j] == \old(taskWaitTime[j]))
    //                              )))
    //                  && (    (0 <= \old(eventState[i]) && \old(eventState[i]) <= delay)
    //                      ==> (   eventState[i] == -1
    //                           && (\forall int j; 0 <= j && j < |taskState| && \old(taskState[j]) == i ==>
    //                                      ({: taskState[j] :} == -1 && j in \result && taskWaitTime[j] == delay - \old(eventState[i]))
    //                              )))
    //                  && (    \old(eventState[i]) > delay
    //                      ==> (   eventState[i] == \old(eventState[i]) - delay
    //                           && (\forall int j; 0 <= j && j < |taskState| && \old(taskState[j]) == i ==>
    //                                      ({:1: taskState[j] :} == \old(taskState[j]) && !(j in \result) && {:2: taskWaitTime[j] :} == \old(taskWaitTime[j]))
    //                              )))
    //                 )
    //         );
    val ensures2: Expr[N] = Utils.single_var_forall(
      i2,
      Utils.int_val(0),
      Utils.size(eventState),
      Utils.fold_and(Seq[Expr[N]](
        Implies(
          LessEq(
            Utils.old(Utils.subscript_expr(eventState, Utils.local_of(i2))),
            Utils.int_val(-1),
          )(Utils.origen),
          And(
            Eq(
              Utils.subscript_expr(eventState, Utils.local_of(i2)),
              Utils.int_val(-1),
            )(Utils.origen),
            Utils.single_var_forall(
              j21,
              Utils.int_val(0),
              Utils.size(taskState),
              Implies(
                Eq(
                  Utils
                    .old(Utils.subscript_expr(taskState, Utils.local_of(j21))),
                  Utils.local_of(i2),
                )(Utils.origen),
                Utils.fold_and(Seq[Expr[N]](
                  Utils.unchanged(
                    Utils.subscript_expr(taskState, Utils.local_of(j21))
                  ),
                  Not(
                    SeqMember(Utils.local_of(j21), Utils.result)(Utils.origen)
                  )(Utils.origen),
                  Utils.unchanged(
                    Utils.subscript_expr(taskWaitTime, Utils.local_of(j21))
                  ),
                )),
              )(Utils.origen),
            ),
          )(Utils.origen),
        )(Utils.origen),
        Implies(
          And(
            LessEq(
              Utils.int_val(0),
              Utils.old(Utils.subscript_expr(eventState, Utils.local_of(i2))),
            )(Utils.origen),
            LessEq(
              Utils.old(Utils.subscript_expr(eventState, Utils.local_of(i2))),
              Utils.local_of(delay),
            )(Utils.origen),
          )(Utils.origen),
          And(
            Eq(
              Utils.subscript_expr(eventState, Utils.local_of(i2)),
              Utils.int_val(-1),
            )(Utils.origen),
            Utils.single_var_forall(
              j22,
              Utils.int_val(0),
              Utils.size(taskState),
              Implies(
                Eq(
                  Utils
                    .old(Utils.subscript_expr(taskState, Utils.local_of(j22))),
                  Utils.local_of(i2),
                )(Utils.origen),
                Utils.fold_and(Seq[Expr[N]](
                  Eq(
                    Utils.subscript_expr(taskState, Utils.local_of(j22)),
                    Utils.int_val(-1),
                  )(Utils.origen),
                  SeqMember(Utils.local_of(j22), Utils.result)(Utils.origen),
                  Eq(
                    Utils.subscript_expr(taskWaitTime, Utils.local_of(j22)),
                    Minus(
                      Utils.local_of(delay),
                      Utils.old(
                        Utils.subscript_expr(eventState, Utils.local_of(i2))
                      ),
                    )(Utils.origen),
                  )(Utils.origen),
                )),
              )(Utils.origen),
            ),
          )(Utils.origen),
        )(Utils.origen),
        Implies(
          Greater(
            Utils.old(Utils.subscript_expr(eventState, Utils.local_of(i2))),
            Utils.local_of(delay),
          )(Utils.origen),
          And(
            Eq(
              Utils.subscript_expr(eventState, Utils.local_of(i2)),
              Minus(
                Utils.old(Utils.subscript_expr(eventState, Utils.local_of(i2))),
                Utils.local_of(delay),
              )(Utils.origen),
            )(Utils.origen),
            Utils.single_var_forall(
              j23,
              Utils.int_val(0),
              Utils.size(taskState),
              Implies(
                Eq(
                  Utils
                    .old(Utils.subscript_expr(taskState, Utils.local_of(j23))),
                  Utils.local_of(i2),
                )(Utils.origen),
                Utils.fold_and(Seq[Expr[N]](
                  Utils.unchanged(
                    Utils.subscript_expr(taskState, Utils.local_of(j23))
                  ),
                  Not(
                    SeqMember(Utils.local_of(j23), Utils.result)(Utils.origen)
                  )(Utils.origen),
                  Utils.unchanged(
                    Utils.subscript_expr(taskWaitTime, Utils.local_of(j23))
                  ),
                )),
              )(Utils.origen),
            ),
          )(Utils.origen),
        )(Utils.origen),
      )),
    )

    // ensures (\forall int i; 0 <= i && i < |taskState| && \old(taskState[i]) < 0 ==>
    //                (taskState[i] == \old(taskState[i]) && !(i in \result) && taskWaitTime[i] == \old(taskWaitTime[i]) + delay)
    //        );
    val ensures3: Expr[N] = Utils.single_var_forall(
      i3,
      Utils.int_val(0),
      Utils.size(taskState),
      Implies(
        Less(
          Utils.old(Utils.subscript_expr(taskState, Utils.local_of(i3))),
          Utils.int_val(0),
        )(Utils.origen),
        Utils.fold_and(Seq(
          Utils.unchanged(Utils.subscript_expr(taskState, Utils.local_of(i3))),
          Not(SeqMember(Utils.local_of(i3), Utils.result)(Utils.origen))(
            Utils.origen
          ),
          Eq(
            Utils.subscript_expr(taskWaitTime, Utils.local_of(i3)),
            Plus(
              Utils.old(Utils.subscript_expr(taskWaitTime, Utils.local_of(i3))),
              Utils.local_of(delay),
            )(Utils.origen),
          )(Utils.origen),
        )),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < |\result| ==>
    //                 (0 <= \result[i] && \result[i] < |taskState|)
    //         );
    val ensures4: Expr[N] = Utils.single_var_forall(
      i4,
      Utils.int_val(0),
      Size(Utils.result)(Utils.origen),
      And(
        LessEq(
          Utils.int_val(0),
          SeqSubscript(Utils.result, Utils.local_of(i4))(Utils.origen)(
            Utils.origen
          ),
        )(Utils.origen),
        Less(
          SeqSubscript(Utils.result, Utils.local_of(i4))(Utils.origen)(
            Utils.origen
          ),
          Utils.size(taskState),
        )(Utils.origen),
      )(Utils.origen),
    )

    // ensures (\forall int i; 0 <= i && i < |\result| ==> (\forall int j; i < j && j < |\result| ==> \result[i] != \result[j]));
    val ensures5: Expr[N] = Utils.single_var_forall(
      i5,
      Utils.int_val(0),
      Size(Utils.result)(Utils.origen),
      Forall(
        Seq(j5),
        Seq(),
        Implies(
          And(
            Less(Utils.local_of(i5), Utils.local_of(j5))(Utils.origen),
            Less(Utils.local_of(j5), Size(Utils.result)(Utils.origen))(
              Utils.origen
            ),
          )(Utils.origen),
          Neq(
            SeqSubscript(Utils.result, Utils.local_of(i5))(Utils.origen)(
              Utils.origen
            ),
            SeqSubscript(Utils.result, Utils.local_of(j5))(Utils.origen)(
              Utils.origen
            ),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen),
    )

    // ensures \old(|runnableQueue|) + |\result| <= |taskPriority|;
    val ensures6: Expr[N] =
      LessEq(
        Plus(
          Utils.old(Utils.size(runnableQueue)),
          Size(Utils.result)(Utils.origen),
        )(Utils.origen),
        Utils.size(taskPriority),
      )(Utils.origen)

    // ensures (\forall int i; 0 <= i && i < |\result| ==>
    //                 (\forall int j; 0 <= j && j < i ==>
    //                         (\old(eventState[\result[j]]) <= \old(eventState[\result[i]]))
    //                 )
    //         );
    val ensures7: Expr[N] = Utils.single_var_forall(
      i7,
      Utils.int_val(0),
      Size(Utils.result)(Utils.origen),
      Utils.single_var_forall(
        j7,
        Utils.int_val(0),
        Utils.local_of(i7),
        LessEq(
          Utils.old(
            SeqSubscript(
              Utils.deref_of(eventState),
              SeqSubscript(Utils.result, Utils.local_of(j7))(Utils.origen)(
                Utils.origen
              ),
            )(Utils.origen)(Utils.origen)
          ),
          Utils.old(
            SeqSubscript(
              Utils.deref_of(eventState),
              SeqSubscript(Utils.result, Utils.local_of(i7))(Utils.origen)(
                Utils.origen
              ),
            )(Utils.origen)(Utils.origen)
          ),
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
        context,
        Utils.fold_star(Seq(
          context,
          ensures1,
          ensures2,
          ensures3,
          ensures4,
          ensures5,
          ensures6,
          ensures7,
        )),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("simulateTimePassing"))
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
    )(Utils.origen)(Utils.origen("executionTime"))
  }

  private def create_schedule(
      nextEventDelay: Ref[N, InstanceMethod[N]],
      advanceTime: Ref[N, InstanceMethod[N]],
      resumeTasks: Ref[N, InstanceMethod[N]],
      resetEvents: Ref[N, InstanceMethod[N]],
      selectNextTask: Ref[N, InstanceMethod[N]],
      globalInvariant_ref: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    val schedulerDelay: Variable[N] =
      new Variable(Utils.tint)(Utils.origen("schedulerDelay"))
    val schedulerNext: Variable[N] =
      new Variable(Utils.tint)(Utils.origen("schedulerNext"))

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
    //     assert schedulerDelay != -1;
    //     advanceTime(schedulerDelay);
    //     runnableQueue = runnableQueue + resumeTasks();
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
        )(Utils.origen)(Utils.origen),
        Branch(Seq((
          Or(
            Eq(Utils.local_of(schedulerDelay), Utils.int_val(0))(Utils.origen),
            Utils.single_var_forall(
              i,
              Utils.int_val(0),
              Utils.size(taskState),
              GreaterEq(
                Utils.subscript_expr(taskState, Utils.local_of(i)),
                Utils.int_val(0),
              )(Utils.origen),
            ),
          )(Utils.origen),
          Block(Seq(
            Assert(Neq(Utils.local_of(schedulerDelay), Utils.int_val(0))(
              Utils.origen
            ))(Utils.origen)(Utils.origen),
            Utils.stmt_invoke(advanceTime, Seq(Utils.local_of(schedulerDelay))),
            Assign(
              Utils.deref_of(runnableQueue),
              Concat(
                Utils.deref_of(runnableQueue),
                Utils.invoke(resumeTasks, Seq()),
              )(Utils.origen),
            )(Utils.origen)(Utils.origen),
            Utils.stmt_invoke(resetEvents, Seq()),
          ))(Utils.origen),
        )))(Utils.origen),
        Assign(
          Utils.local_of(schedulerNext),
          Utils.invoke(selectNextTask, Seq()),
        )(Utils.origen)(Utils.origen),
        Branch(Seq((
          Neq(Utils.local_of(schedulerNext), Utils.int_val(-1))(Utils.origen),
          Block(Seq(
            Assign(
              Utils.deref_of(taskState),
              SeqUpdate(
                Utils.deref_of(taskState),
                Utils
                  .subscript_expr(runnableQueue, Utils.local_of(schedulerNext)),
                Utils.int_val(-2),
              )(Utils.origen),
            )(Utils.origen)(Utils.origen),
            Assign(
              Utils.deref_of(runnableQueue),
              RemoveAt(
                Utils.deref_of(runnableQueue),
                Utils.local_of(schedulerNext),
              )(Utils.origen),
            )(Utils.origen)(Utils.origen),
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
    )(Utils.origen)(Utils.origen("schedule"))
  }

  private def create_start(
      to_launch: Seq[InstanceField[N]],
      preconditions: Seq[Expr[N]],
      schedule: Ref[N, InstanceMethod[N]],
  ): InstanceMethod[N] = {
    val requires: Expr[N] = Utils.fold_star(
      (preconditions ++
        to_launch.map(f => IdleToken(Utils.deref_of(f))(Utils.origen))) :+
        Committed(Utils.thiz)(Utils.origen)(Utils.origen)
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
      Seq[Statement[N]](Lock(Utils.thiz)(Utils.origen)(Utils.origen)) ++
        to_launch
          .map(f => Fork[N](Utils.deref_of(f))(Utils.origen)(Utils.origen)) ++
        Seq[Statement[N]](
          Unlock(Utils.thiz)(Utils.origen)(Utils.origen),
          Loop(
            Utils.skip,
            tt,
            Utils.skip,
            Utils.to_loop_invariant(tt),
            Block(Seq[Statement[N]](
              Lock(Utils.thiz)(Utils.origen)(Utils.origen),
              Utils.stmt_invoke(schedule, Seq()),
              Unlock(Utils.thiz)(Utils.origen)(Utils.origen),
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
    )(Utils.origen)(Utils.origen("start"))
  }
}
