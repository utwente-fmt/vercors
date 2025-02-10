package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.{ObjectInfo, Transformer, Utils}

sealed trait Semaphore[O, N] extends FreeRTOSConstruct[O, N] {
  def get_decl: Option[CLocal[O]]
  def cls_type: String
  def perms_for_scheduler(field: InstanceField[N]): Seq[Expr[N]]
  def additional_constructor_args: Seq[Expr[N]]
  def function_mapping: Seq[(String, InstanceMethod[N])]

  private def class_name(idx: Int): String =
    get_decl match {
      case Some(l) => cls_type + l.name
      case None => cls_type + "Anonymous" + idx
    }

  private def instance_name(idx: Int): String =
    get_decl match {
      case Some(l) => l.name
      case None => "unknown" + cls_type + idx
    }

  override def convert(
      col_ir: Transformer[O, N],
      idx: Int,
  ): ObjectInfo[O, N] = {
    val available_event: Int = col_ir.reserve_event_id

    val cls: Class[N] = transform(
      new LazyRef(col_ir.get_scheduler),
      new LazyRef(col_ir.get_eventState),
      new LazyRef(col_ir.get_eventPerms),
      new LazyRef(col_ir.get_taskPriority),
      new LazyRef(col_ir.get_priorityPerms),
      available_event,
      class_name(idx),
    )
    val tcls =
      TByReferenceClass(new DirectRef[N, Class[N]](cls), Seq())(Utils.origen)

    val field: InstanceField[N] =
      new InstanceField(tcls, Seq())(Utils.origen(instance_name(idx)))

    if (get_decl.nonEmpty) {
      function_mapping
        .foreach(t => col_ir.add_to_api(get_decl.get, t._1, field, t._2))
    }
    col_ir.add_write_event(field, available_event)

    ObjectInfo(
      get_decl,
      field,
      cls,
      Seq[Expr[N]](Utils.thiz) ++ additional_constructor_args,
      Utils.fold_star(
        Seq[Expr[N]](Perm(Utils.loc_of(field), Utils.read)(Utils.origen)) ++
          perms_for_scheduler(field)
      ),
      None,
      None,
      None,
      None,
      None,
      launch = false,
    )
  }

  def transform(
      scheduler_ref: Ref[N, Class[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      priority_ref: Ref[N, InstanceField[N]],
      priority_perms_ref: Ref[N, InstancePredicate[N]],
      available_event: Int,
      name: String,
  ): Class[N]
}

case class BinarySemaphore[O, N](decl: Option[CLocal[O]], is_mutex: Boolean)
    extends Semaphore[O, N] {
  def get_decl: Option[CLocal[O]] = decl
  def cls_type: String =
    if (is_mutex)
      "Mutex"
    else
      "Semaphore"
  def perms_for_scheduler(field: InstanceField[N]): Seq[Expr[N]] =
    Seq(
      Utils.predicate_apply(
        Utils.deref_of(field),
        new DirectRef[N, InstancePredicate[N]](semaphorePerms),
        Seq(),
      ),
      Eq(Utils.deref_of(s, Some(Utils.deref_of(field))), Utils.thiz)(
        Utils.origen
      ),
      if (is_mutex)
        Utils.deref_of(isMutex, Some(Utils.deref_of(field)))
      else
        Not(Utils.deref_of(isMutex, Some(Utils.deref_of(field))))(Utils.origen),
    )
  def additional_constructor_args: Seq[Expr[N]] =
    Seq(BooleanValue(value = is_mutex)(Utils.origen))
  def function_mapping: Seq[(String, InstanceMethod[N])] =
    Seq(
      ("uxSemaphoreGetCount", uxSemaphoreGetCount),
      ("xSemaphoreGetMutexHolder", xSemaphoreGetMutexHolder),
      ("xSemaphoreGive", xSemaphoreGive),
      ("xSemaphoreTake", xSemaphoreTake),
    )

  private var s: InstanceField[N] = ???
  private var isMutex: InstanceField[N] = ???
  private var semaphorePerms: InstancePredicate[N] = ???
  private var uxSemaphoreGetCount: InstanceMethod[N] = ???
  private var xSemaphoreGetMutexHolder: InstanceMethod[N] = ???
  private var xSemaphoreGive: InstanceMethod[N] = ???
  private var xSemaphoreTake: InstanceMethod[N] = ???

  override def transform(
      scheduler_ref: Ref[N, Class[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      priority_ref: Ref[N, InstanceField[N]],
      priority_perms_ref: Ref[N, InstancePredicate[N]],
      available_event: Int,
      name: String,
  ): Class[N] = {
    s =
      new InstanceField(TByReferenceClass(scheduler_ref, Seq()), Seq())(
        Utils.origen("s")
      )
    isMutex = new InstanceField(Utils.tbool, Seq())(Utils.origen("isMutex"))
    val task: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("task"))
    val originalPriority: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("originalPriority"))

    semaphorePerms =
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(Seq(
          Perm(Utils.loc_of(s), Utils.read)(Utils.origen),
          Neq(Utils.deref_of(s), Utils.nul)(Utils.origen),
          Perm(Utils.loc_of(isMutex), Utils.read)(Utils.origen),
          Perm(Utils.loc_of(task), Utils.write)(Utils.origen),
          Perm(Utils.loc_of(originalPriority), Utils.write)(Utils.origen),
        ))),
        false,
        true,
      )(Utils.origen("semaphorePerms"))

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](semaphorePerms)

    val semaphoreConstructor: PVLConstructor[N] = create_constructor(
      s,
      isMutex,
      task,
      originalPriority,
      perms,
      scheduler_ref,
    )

    uxSemaphoreGetCount = create_uxSemaphoreGetCount(task, perms)
    xSemaphoreGetMutexHolder = create_xSemaphoreGetMutexHolder(task, perms)
    xSemaphoreGive = create_xSemaphoreGive(
      s,
      isMutex,
      task,
      originalPriority,
      perms,
      event_ref,
      event_perms_ref,
      priority_ref,
      priority_perms_ref,
      available_event,
    )
    xSemaphoreTake = create_xSemaphoreTake(
      s,
      isMutex,
      task,
      originalPriority,
      perms,
      priority_ref,
      priority_perms_ref,
    )

    new ByReferenceClass(
      Seq(),
      Seq(
        s,
        isMutex,
        task,
        originalPriority,
        semaphorePerms,
        semaphoreConstructor,
        uxSemaphoreGetCount,
        xSemaphoreGetMutexHolder,
        xSemaphoreGive,
        xSemaphoreTake,
      ),
      Seq(),
      tt,
    )(Utils.origen(name))
  }

  private def create_constructor(
      s: InstanceField[N],
      isMutex: InstanceField[N],
      task: InstanceField[N],
      originalPriority: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      scheduler_ref: Ref[N, Class[N]],
  ): PVLConstructor[N] = {
    val s_param: Variable[N] =
      new Variable(TByReferenceClass(scheduler_ref, Seq())(Utils.origen))(
        Utils.origen("s_param")
      )
    val mutex_param: Variable[N] =
      new Variable(Utils.tint)(Utils.origen("mutex_param"))

    // requires s_param != null;
    val requires: Expr[N] =
      Neq(Utils.local_of(s_param), Utils.nul)(Utils.origen)

    // ensures semaphorePerms() ** s == s_param ** isMutex == mutex_param ** task == -1 && originalPriority == -1;
    val ensures: Expr[N] = Utils.fold_star(Seq(
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Eq(Utils.deref_of(s), Utils.local_of(s_param))(Utils.origen),
      Eq(Utils.deref_of(isMutex), Utils.local_of(mutex_param))(Utils.origen),
      Eq(Utils.deref_of(task), Utils.int_val(-1))(Utils.origen),
      Eq(Utils.deref_of(originalPriority), Utils.int_val(-1))(Utils.origen),
    ))

    // s = s_param; isMutex = mutex_param; task = -1; originalPriority = -1;
    val body: Statement[N] =
      Block(Seq(
        Assign(Utils.deref_of(s), Utils.local_of(s_param))(Utils.origen)(
          Utils.origen
        ),
        Assign(Utils.deref_of(isMutex), Utils.local_of(mutex_param))(
          Utils.origen
        )(Utils.origen),
        Assign(Utils.deref_of(task), Utils.int_val(-1))(Utils.origen)(
          Utils.origen
        ),
        Assign(Utils.deref_of(originalPriority), Utils.int_val(-1))(
          Utils.origen
        )(Utils.origen),
      ))(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(requires, ensures),
      Seq(),
      Seq(s_param, mutex_param),
      Some(body),
    )(Utils.origen)(Utils.origen)
  }

  private def create_uxSemaphoreGetCount(
      task: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires semaphorePerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures task >= 0 ==> \result == 1;
    val ensures1: Expr[N] =
      Implies(
        GreaterEq(Utils.deref_of(task), Utils.int_val(0))(Utils.origen),
        Eq(Utils.result, Utils.int_val(1))(Utils.origen),
      )(Utils.origen)

    // ensures task < 0 ==> \result == 0;
    val ensures2: Expr[N] =
      Implies(
        Less(Utils.deref_of(task), Utils.int_val(0))(Utils.origen),
        Eq(Utils.result, Utils.int_val(0))(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(requires, Star(ensures1, ensures2)(Utils.origen)),
      false,
      true,
    )(Utils.origen)(Utils.origen("uxSemaphoreGetCount"))
  }

  private def create_xSemaphoreGetMutexHolder(
      task: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires semaphorePerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == task
    val ensures: Expr[N] = Eq(Utils.result, Utils.deref_of(task))(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(requires, ensures),
      false,
      true,
    )(Utils.origen)(Utils.origen("xSemaphoreGetMutexHolder"))
  }

  private def create_xSemaphoreGive(
      s: InstanceField[N],
      isMutex: InstanceField[N],
      task: InstanceField[N],
      originalPriority: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      priority_ref: Ref[N, InstanceField[N]],
      priority_perms_ref: Ref[N, InstancePredicate[N]],
      available_event: Int,
  ): InstanceMethod[N] = {
    val taskID: Variable[N] = new Variable(Utils.tint)(Utils.origen("taskID"))

    // requires sBufferPerms() ** s.eventPerms() ** s.priorityPerms();
    // ensures sBufferPerms() ** s.eventPerms() ** s.priorityPerms();
    val context: Expr[N] = Utils.fold_star(Seq(
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Utils.predicate_apply(Utils.deref_of(s), event_perms_ref, Seq()),
      Utils.predicate_apply(Utils.deref_of(s), priority_perms_ref, Seq()),
    ))

    // ensures \result == (\old(task) == taskID);
    val ensures1: Expr[N] =
      Eq(
        Utils.result,
        Eq(Utils.old(Utils.deref_of(task)), Utils.local_of(taskID))(
          Utils.origen
        ),
      )(Utils.origen)

    // ensures \old(task) == taskID ==> (   task == -1
    //                                   && originalPriority == -1
    //                                   && s.eventState == \old(s.eventState.update(???, 0))
    //                                   && (    isMutex
    //                                       ==> s.taskPriority == \old(s.taskPriority.update(task, originalPriority)))
    //                                   && (!isMutex ==> s.taskPriority == \old(s.taskPriority)));
    val ensures2: Expr[N] =
      Implies(
        Eq(Utils.old(Utils.deref_of(task)), Utils.local_of(taskID))(
          Utils.origen
        ),
        Utils.fold_and(Seq[Expr[N]](
          Eq(Utils.deref_of(task), Utils.int_val(-1))(Utils.origen),
          Eq(Utils.deref_of(originalPriority), Utils.int_val(-1))(Utils.origen),
          Eq(
            Utils.deref_ref(event_ref, Utils.deref_of(s)),
            Utils.old(
              SeqUpdate(
                Utils.deref_ref(event_ref, Utils.deref_of(s)),
                Utils.int_val(available_event),
                Utils.int_val(0),
              )(Utils.origen)
            ),
          )(Utils.origen),
          Implies(
            Utils.deref_of(isMutex),
            Eq(
              Utils.deref_ref(priority_ref, Utils.deref_of(s)),
              Utils.old(
                SeqUpdate(
                  Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                  Utils.deref_of(task),
                  Utils.deref_of(originalPriority),
                )(Utils.origen)
              ),
            )(Utils.origen),
          )(Utils.origen),
          Implies(
            Not(Utils.deref_of(isMutex))(Utils.origen),
            Eq(
              Utils.deref_ref(priority_ref, Utils.deref_of(s)),
              Utils.old(Utils.deref_ref(priority_ref, Utils.deref_of(s))),
            )(Utils.origen),
          )(Utils.origen),
        )),
      )(Utils.origen)

    // ensures \old(task) != taskID ==> (   task == \old(task)
    //                                   && originalPriority == \old(originalPriority)
    //                                   && main.eventState == \old(main.eventState)
    //                                   && main.taskPriority == \old(main.taskPriority));
    val ensures3: Expr[N] =
      Implies(
        Neq(Utils.old(Utils.deref_of(task)), Utils.local_of(taskID))(
          Utils.origen
        ),
        Utils.fold_and(Seq[Expr[N]](
          Eq(Utils.deref_of(task), Utils.old(Utils.deref_of(task)))(
            Utils.origen
          ),
          Eq(
            Utils.deref_of(originalPriority),
            Utils.old(Utils.deref_of(originalPriority)),
          )(Utils.origen),
          Eq(
            Utils.deref_ref(event_ref, Utils.deref_of(s)),
            Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
          )(Utils.origen),
          Eq(
            Utils.deref_ref(priority_ref, Utils.deref_of(s)),
            Utils.old(Utils.deref_ref(priority_ref, Utils.deref_of(s))),
          )(Utils.origen),
        )),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(taskID),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3)),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("xSemaphoreGive"))
  }

  private def create_xSemaphoreTake(
      s: InstanceField[N],
      isMutex: InstanceField[N],
      task: InstanceField[N],
      originalPriority: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      priority_ref: Ref[N, InstanceField[N]],
      priority_perms_ref: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    val taskID: Variable[N] = new Variable(Utils.tint)(Utils.origen("taskID"))

    // requires sBufferPerms() ** s.priorityPerms();
    // ensures sBufferPerms() ** s.priorityPerms();
    val context: Expr[N] = Utils.fold_star(Seq(
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Utils.predicate_apply(Utils.deref_of(s), priority_perms_ref, Seq()),
    ))

    // ensures \result == (\old(task) < 0);
    val ensures1: Expr[N] =
      Eq(
        Utils.result,
        Less(Utils.old(Utils.deref_of(task)), Utils.int_val(0))(Utils.origen),
      )(Utils.origen)

    // ensures \old(task) < 0 ==> (   task == taskID
    //                             && originalPriority == \old(s.taskPriority[taskID])
    //                             && s.taskPriority == \old(s.taskPriority));
    val ensures2: Expr[N] =
      Implies(
        Less(Utils.old(Utils.deref_of(task)), Utils.int_val(0))(Utils.origen),
        Utils.fold_and(Seq[Expr[N]](
          Eq(Utils.deref_of(task), Utils.local_of(taskID))(Utils.origen),
          Eq(
            Utils.deref_of(originalPriority),
            Utils.old(
              SeqSubscript(
                Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                Utils.local_of(taskID),
              )(Utils.origen)(Utils.origen)
            ),
          )(Utils.origen),
          Eq(
            Utils.deref_ref(priority_ref, Utils.deref_of(s)),
            Utils.old(Utils.deref_ref(priority_ref, Utils.deref_of(s))),
          )(Utils.origen),
        )),
      )(Utils.origen)

    // ensures \old(task) >= 0 ==> (   task == \old(task)
    //                              && originalPriority == \old(originalPriority)
    //                              && (    (isMutex && \old(s.taskPriority[taskID]) > \old(s.taskPriority[task]))
    //                                  ==> s.taskPriority == \old(s.taskPriority.update(task, s.taskPriority[taskID])))
    //                              && (    (!isMutex || \old(s.taskPriority[taskID]) <= \old(s.taskPriority[task]))
    //                                  ==> s.taskPriority == \old(s.taskPriority)));
    val ensures3: Expr[N] =
      Implies(
        GreaterEq(Utils.old(Utils.deref_of(task)), Utils.int_val(0))(
          Utils.origen
        ),
        Utils.fold_and(Seq[Expr[N]](
          Eq(Utils.deref_of(task), Utils.old(Utils.deref_of(task)))(
            Utils.origen
          ),
          Eq(
            Utils.deref_of(originalPriority),
            Utils.old(Utils.deref_of(originalPriority)),
          )(Utils.origen),
          Implies(
            And(
              Utils.deref_of(isMutex),
              Greater(
                Utils.old(
                  SeqSubscript(
                    Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                    Utils.local_of(taskID),
                  )(Utils.origen)(Utils.origen)
                ),
                Utils.old(
                  SeqSubscript(
                    Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                    Utils.deref_of(task),
                  )(Utils.origen)(Utils.origen)
                ),
              )(Utils.origen),
            )(Utils.origen),
            Eq(
              Utils.deref_ref(priority_ref, Utils.deref_of(s)),
              Utils.old(
                SeqUpdate(
                  Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                  Utils.deref_of(task),
                  SeqSubscript(
                    Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                    Utils.local_of(taskID),
                  )(Utils.origen)(Utils.origen),
                )(Utils.origen)
              ),
            )(Utils.origen),
          )(Utils.origen),
          Implies(
            Or(
              Not(Utils.deref_of(isMutex))(Utils.origen),
              LessEq(
                Utils.old(
                  SeqSubscript(
                    Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                    Utils.local_of(taskID),
                  )(Utils.origen)(Utils.origen)
                ),
                Utils.old(
                  SeqSubscript(
                    Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                    Utils.deref_of(task),
                  )(Utils.origen)(Utils.origen)
                ),
              )(Utils.origen),
            )(Utils.origen),
            Eq(
              Utils.deref_ref(priority_ref, Utils.deref_of(s)),
              Utils.old(Utils.deref_ref(priority_ref, Utils.deref_of(s))),
            )(Utils.origen),
          )(Utils.origen),
        )),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(taskID),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3)),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("xSemaphoreTake"))
  }
}

case class RecursiveMutex[O, N](decl: Option[CLocal[O]])
    extends Semaphore[O, N] {
  def get_decl: Option[CLocal[O]] = decl
  def cls_type: String = "RecursiveMutex"
  def perms_for_scheduler(field: InstanceField[N]): Seq[Expr[N]] =
    Seq(
      Utils.predicate_apply(
        Utils.deref_of(field),
        new DirectRef[N, InstancePredicate[N]](mutexPerms),
        Seq(),
      ),
      Eq(Utils.deref_of(s, Some(Utils.deref_of(field))), Utils.thiz)(
        Utils.origen
      ),
    )
  def additional_constructor_args: Seq[Expr[N]] = Seq()
  def function_mapping: Seq[(String, InstanceMethod[N])] =
    Seq(
      ("uxSemaphoreGetCount", uxSemaphoreGetCount),
      ("xSemaphoreGetMutexHolder", xSemaphoreGetMutexHolder),
      ("xSemaphoreGiveRecursive", xSemaphoreGiveRecursive),
      ("xSemaphoreTakeRecursive", xSemaphoreTakeRecursive),
    )

  private var s: InstanceField[N] = ???
  private var mutexPerms: InstancePredicate[N] = ???
  private var uxSemaphoreGetCount: InstanceMethod[N] = ???
  private var xSemaphoreGetMutexHolder: InstanceMethod[N] = ???
  private var xSemaphoreGiveRecursive: InstanceMethod[N] = ???
  private var xSemaphoreTakeRecursive: InstanceMethod[N] = ???

  override def transform(
      scheduler_ref: Ref[N, Class[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      priority_ref: Ref[N, InstanceField[N]],
      priority_perms_ref: Ref[N, InstancePredicate[N]],
      available_event: Int,
      name: String,
  ): Class[N] = {
    s =
      new InstanceField(TByReferenceClass(scheduler_ref, Seq()), Seq())(
        Utils.origen("s")
      )
    val recursionDepth: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("recursionDepth"))
    val task: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("task"))
    val originalPriority: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("originalPriority"))
    mutexPerms =
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(Seq(
          Perm(Utils.loc_of(s), Utils.read)(Utils.origen),
          Neq(Utils.deref_of(s), Utils.nul)(Utils.origen),
          Perm(Utils.loc_of(recursionDepth), Utils.write)(Utils.origen),
          GreaterEq(Utils.deref_of(recursionDepth), Utils.int_val(0))(
            Utils.origen
          ),
          Perm(Utils.loc_of(task), Utils.write)(Utils.origen),
          Perm(Utils.loc_of(originalPriority), Utils.write)(Utils.origen),
        ))),
        false,
        true,
      )(Utils.origen("mutexPerms"))

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](mutexPerms)

    val mutexConstructor: PVLConstructor[N] = create_constructor(
      s,
      recursionDepth,
      task,
      originalPriority,
      perms,
      scheduler_ref,
    )

    uxSemaphoreGetCount = create_uxSemaphoreGetCount(task, perms)
    xSemaphoreGetMutexHolder = create_xSemaphoreGetMutexHolder(task, perms)
    xSemaphoreGiveRecursive = create_xSemaphoreGiveRecursive(
      s,
      recursionDepth,
      task,
      originalPriority,
      perms,
      event_ref,
      event_perms_ref,
      priority_ref,
      priority_perms_ref,
      available_event,
    )
    xSemaphoreTakeRecursive = create_xSemaphoreTakeRecursive(
      s,
      recursionDepth,
      task,
      originalPriority,
      perms,
      priority_ref,
      priority_perms_ref,
    )

    new ByReferenceClass(
      Seq(),
      Seq(
        s,
        recursionDepth,
        task,
        originalPriority,
        mutexPerms,
        mutexConstructor,
        uxSemaphoreGetCount,
        xSemaphoreGetMutexHolder,
        xSemaphoreGiveRecursive,
        xSemaphoreTakeRecursive,
      ),
      Seq(),
      tt,
    )(Utils.origen(name))
  }

  private def create_constructor(
      s: InstanceField[N],
      recursionDepth: InstanceField[N],
      task: InstanceField[N],
      originalPriority: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      scheduler_ref: Ref[N, Class[N]],
  ): PVLConstructor[N] = {
    val s_param: Variable[N] =
      new Variable(TByReferenceClass(scheduler_ref, Seq())(Utils.origen))(
        Utils.origen("s_param")
      )

    // requires s_param != null;
    val requires: Expr[N] =
      Neq(Utils.local_of(s_param), Utils.nul)(Utils.origen)

    // ensures mutexPerms() ** s == s_param ** task == -1 && originalPriority == -1 ** recursionDepth == 0;
    val ensures: Expr[N] = Utils.fold_star(Seq(
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Eq(Utils.deref_of(s), Utils.local_of(s_param))(Utils.origen),
      Eq(Utils.deref_of(task), Utils.int_val(-1))(Utils.origen),
      Eq(Utils.deref_of(originalPriority), Utils.int_val(-1))(Utils.origen),
      Eq(Utils.deref_of(recursionDepth), Utils.int_val(0))(Utils.origen),
    ))

    // s = s_param; isMutex = mutex_param; task = -1; originalPriority = -1;
    val body: Statement[N] =
      Block(Seq(
        Assign(Utils.deref_of(s), Utils.local_of(s_param))(Utils.blame)(
          Utils.origen
        ),
        Assign(Utils.deref_of(task), Utils.int_val(-1))(Utils.blame)(
          Utils.origen
        ),
        Assign(Utils.deref_of(originalPriority), Utils.int_val(-1))(
          Utils.origen
        )(Utils.origen),
        Assign(Utils.deref_of(recursionDepth), Utils.int_val(0))(Utils.blame)(
          Utils.origen
        ),
      ))(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(requires, ensures),
      Seq(),
      Seq(s_param),
      Some(body),
    )(Utils.blame)(Utils.origen)
  }

  private def create_uxSemaphoreGetCount(
      task: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires semaphorePerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures task >= 0 ==> \result == 1;
    val ensures1: Expr[N] =
      Implies(
        GreaterEq(Utils.deref_of(task), Utils.int_val(0))(Utils.origen),
        Eq(Utils.result, Utils.int_val(1))(Utils.origen),
      )(Utils.origen)

    // ensures task < 0 ==> \result == 0;
    val ensures2: Expr[N] =
      Implies(
        Less(Utils.deref_of(task), Utils.int_val(0))(Utils.origen),
        Eq(Utils.result, Utils.int_val(0))(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(requires, Star(ensures1, ensures2)(Utils.origen)),
      false,
      true,
    )(Utils.blame)(Utils.origen("uxSemaphoreGetCount"))
  }

  private def create_xSemaphoreGetMutexHolder(
      task: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires semaphorePerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == task
    val ensures: Expr[N] = Eq(Utils.result, Utils.deref_of(task))(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(requires, ensures),
      false,
      true,
    )(Utils.blame)(Utils.origen("xSemaphoreGetMutexHolder"))
  }

  private def create_xSemaphoreGiveRecursive(
      s: InstanceField[N],
      recursionDepth: InstanceField[N],
      task: InstanceField[N],
      originalPriority: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      priority_ref: Ref[N, InstanceField[N]],
      priority_perms_ref: Ref[N, InstancePredicate[N]],
      available_event: Int,
  ): InstanceMethod[N] = {
    val taskID: Variable[N] = new Variable(Utils.tint)(Utils.origen("taskID"))

    // requires sBufferPerms() ** s.eventPerms() ** s.priorityPerms();
    // ensures sBufferPerms() ** s.eventPerms() ** s.priorityPerms();
    val context: Expr[N] = Utils.fold_star(Seq(
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Utils.predicate_apply(Utils.deref_of(s), event_perms_ref, Seq()),
      Utils.predicate_apply(Utils.deref_of(s), priority_perms_ref, Seq()),
    ))

    // ensures \result == (\old(task) == taskID);
    val ensures1: Expr[N] =
      Eq(
        Utils.result,
        Eq(Utils.old(Utils.deref_of(task)), Utils.local_of(taskID))(
          Utils.origen
        ),
      )(Utils.origen)

    // ensures \old(task) == taskID ==>    (    recursionDepth == 1
    //                                      ==> (   task == -1
    //                                           && originalPriority == -1
    //                                           && recursionDepth == 0
    //                                           && main.eventState == \old(main.eventState.update(???, 0))
    //                                           && main.taskPriority == \old(main.taskPriority.update(task, originalPriority))))
    //                                  && (    recursionDepth != 1
    //                                      ==> (   task == \old(task)
    //                                           && originalPriority == \old(originalPriority)
    //                                           && recursionDepth == \old(recursionDepth) - 1
    //                                           && main.eventState == \old(main.eventState)
    //                                           && main.taskPriority == \old(main.taskPriority)));
    val ensures2: Expr[N] =
      Implies(
        Eq(Utils.old(Utils.deref_of(task)), Utils.local_of(taskID))(
          Utils.origen
        ),
        And(
          Implies(
            Eq(Utils.deref_of(recursionDepth), Utils.int_val(1))(Utils.origen),
            Utils.fold_and(Seq[Expr[N]](
              Eq(Utils.deref_of(task), Utils.int_val(-1))(Utils.origen),
              Eq(Utils.deref_of(originalPriority), Utils.int_val(-1))(
                Utils.origen
              ),
              Eq(Utils.deref_of(recursionDepth), Utils.int_val(0))(
                Utils.origen
              ),
              Eq(
                Utils.deref_ref(event_ref, Utils.deref_of(s)),
                Utils.old(
                  SeqUpdate(
                    Utils.deref_ref(event_ref, Utils.deref_of(s)),
                    Utils.int_val(available_event),
                    Utils.int_val(0),
                  )(Utils.origen)
                ),
              )(Utils.origen),
              Eq(
                Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                Utils.old(
                  SeqUpdate(
                    Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                    Utils.deref_of(task),
                    Utils.deref_of(originalPriority),
                  )(Utils.origen)
                ),
              )(Utils.origen),
            )),
          )(Utils.origen),
          Implies(
            Neq(Utils.deref_of(recursionDepth), Utils.int_val(1))(Utils.origen),
            Utils.fold_and(Seq[Expr[N]](
              Eq(Utils.deref_of(task), Utils.old(Utils.deref_of(task)))(
                Utils.origen
              ),
              Eq(
                Utils.deref_of(originalPriority),
                Utils.old(Utils.deref_of(originalPriority)),
              )(Utils.origen),
              Eq(
                Utils.deref_of(recursionDepth),
                Minus(
                  Utils.old(Utils.deref_of(recursionDepth)),
                  Utils.int_val(1),
                )(Utils.origen),
              )(Utils.origen),
              Eq(
                Utils.deref_ref(event_ref, Utils.deref_of(s)),
                Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
              )(Utils.origen),
              Eq(
                Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                Utils.old(Utils.deref_ref(priority_ref, Utils.deref_of(s))),
              )(Utils.origen),
            )),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(task) != taskID ==> (   task == \old(task)
    //                                   && originalPriority == \old(originalPriority)
    //                                   && recursionDepth == \old(recursionDepth)
    //                                   && main.eventState == \old(main.eventState)
    //                                   && main.taskPriority == \old(main.taskPriority));
    val ensures3: Expr[N] =
      Implies(
        Neq(Utils.old(Utils.deref_of(task)), Utils.local_of(taskID))(
          Utils.origen
        ),
        Utils.fold_and(Seq[Expr[N]](
          Eq(Utils.deref_of(task), Utils.old(Utils.deref_of(task)))(
            Utils.origen
          ),
          Eq(
            Utils.deref_of(originalPriority),
            Utils.old(Utils.deref_of(originalPriority)),
          )(Utils.origen),
          Eq(
            Utils.deref_of(recursionDepth),
            Utils.old(Utils.deref_of(recursionDepth)),
          )(Utils.origen),
          Eq(
            Utils.deref_ref(event_ref, Utils.deref_of(s)),
            Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
          )(Utils.origen),
          Eq(
            Utils.deref_ref(priority_ref, Utils.deref_of(s)),
            Utils.old(Utils.deref_ref(priority_ref, Utils.deref_of(s))),
          )(Utils.origen),
        )),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(taskID),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3)),
      ),
      false,
      false,
    )(Utils.blame)(Utils.origen("xSemaphoreGiveRecursive"))
  }

  private def create_xSemaphoreTakeRecursive(
      s: InstanceField[N],
      recursionDepth: InstanceField[N],
      task: InstanceField[N],
      originalPriority: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      priority_ref: Ref[N, InstanceField[N]],
      priority_perms_ref: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    val taskID: Variable[N] = new Variable(Utils.tint)(Utils.origen("taskID"))

    // requires sBufferPerms() ** s.priorityPerms();
    // ensures sBufferPerms() ** s.priorityPerms();
    val context: Expr[N] = Utils.fold_star(Seq(
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Utils.predicate_apply(Utils.deref_of(s), priority_perms_ref, Seq()),
    ))

    // ensures \result == (\old(task) < 0);
    val ensures1: Expr[N] =
      Eq(
        Utils.result,
        Less(Utils.old(Utils.deref_of(task)), Utils.int_val(0))(Utils.origen),
      )(Utils.origen)

    // ensures \old(task) < 0 ==> (   task == taskID
    //                                && originalPriority == \old(main.taskPriority[taskID])
    //                                && recursionDepth == 1
    //                                && main.taskPriority == \old(main.taskPriority));
    val ensures2: Expr[N] =
      Implies(
        Less(Utils.old(Utils.deref_of(task)), Utils.int_val(0))(Utils.origen),
        Utils.fold_and(Seq[Expr[N]](
          Eq(Utils.deref_of(task), Utils.local_of(taskID))(Utils.origen),
          Eq(
            Utils.deref_of(originalPriority),
            Utils.old(
              SeqSubscript(
                Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                Utils.local_of(taskID),
              )(Utils.blame)(Utils.origen)
            ),
          )(Utils.origen),
          Eq(Utils.deref_of(recursionDepth), Utils.int_val(1))(Utils.origen),
          Eq(
            Utils.deref_ref(priority_ref, Utils.deref_of(s)),
            Utils.old(Utils.deref_ref(priority_ref, Utils.deref_of(s))),
          )(Utils.origen),
        )),
      )(Utils.origen)

    // ensures \old(task) >= 0 ==>    (    \old(task) == taskID
    //                                 ==> (   task == \old(task)
    //                                      && originalPriority == \old(originalPriority)
    //                                      && recursionDepth == \old(recursionDepth) + 1
    //                                      && main.taskPriority == \old(main.taskPriority)))
    //                             && (    \old(task) != taskID
    //                                 ==> (   task == \old(task)
    //                                      && originalPriority == \old(originalPriority)
    //                                      && recursionDepth == \old(recursionDepth)
    //                                      && (    \old(main.taskPriority[taskID]) > \old(main.taskPriority[task])
    //                                          ==> main.taskPriority == \old(main.taskPriority.update(task, main.taskPriority[taskID])))
    //                                      && (    \old(main.taskPriority[taskID]) <= \old(main.taskPriority[task])
    //                                          ==> main.taskPriority == \old(main.taskPriority))));
    val ensures3: Expr[N] =
      Implies(
        GreaterEq(Utils.old(Utils.deref_of(task)), Utils.int_val(0))(
          Utils.origen
        ),
        And(
          Implies(
            Eq(Utils.old(Utils.deref_of(task)), Utils.local_of(taskID))(
              Utils.origen
            ),
            Utils.fold_and(Seq[Expr[N]](
              Eq(Utils.deref_of(task), Utils.old(Utils.deref_of(task)))(
                Utils.origen
              ),
              Eq(
                Utils.deref_of(originalPriority),
                Utils.old(Utils.deref_of(originalPriority)),
              )(Utils.origen),
              Eq(
                Utils.deref_of(recursionDepth),
                Plus(
                  Utils.old(Utils.deref_of(recursionDepth)),
                  Utils.int_val(1),
                )(Utils.origen),
              )(Utils.origen),
              Eq(
                Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                Utils.old(Utils.deref_ref(priority_ref, Utils.deref_of(s))),
              )(Utils.origen),
            )),
          )(Utils.origen),
          Implies(
            Neq(Utils.old(Utils.deref_of(task)), Utils.local_of(taskID))(
              Utils.origen
            ),
            Utils.fold_and(Seq[Expr[N]](
              Eq(Utils.deref_of(task), Utils.old(Utils.deref_of(task)))(
                Utils.origen
              ),
              Eq(
                Utils.deref_of(originalPriority),
                Utils.old(Utils.deref_of(originalPriority)),
              )(Utils.origen),
              Eq(
                Utils.deref_of(recursionDepth),
                Utils.old(Utils.deref_of(recursionDepth)),
              )(Utils.origen),
              Implies(
                Greater(
                  Utils.old(
                    SeqSubscript(
                      Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                      Utils.local_of(taskID),
                    )(Utils.blame)(Utils.origen)
                  ),
                  Utils.old(
                    SeqSubscript(
                      Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                      Utils.deref_of(task),
                    )(Utils.blame)(Utils.origen)
                  ),
                )(Utils.origen),
                Eq(
                  Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                  Utils.old(
                    SeqUpdate(
                      Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                      Utils.deref_of(task),
                      SeqSubscript(
                        Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                        Utils.local_of(taskID),
                      )(Utils.blame)(Utils.origen),
                    )(Utils.origen)
                  ),
                )(Utils.origen),
              )(Utils.origen),
              Implies(
                LessEq(
                  Utils.old(
                    SeqSubscript(
                      Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                      Utils.local_of(taskID),
                    )(Utils.blame)(Utils.origen)
                  ),
                  Utils.old(
                    SeqSubscript(
                      Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                      Utils.deref_of(task),
                    )(Utils.blame)(Utils.origen)
                  ),
                )(Utils.origen),
                Eq(
                  Utils.deref_ref(priority_ref, Utils.deref_of(s)),
                  Utils.old(Utils.deref_ref(priority_ref, Utils.deref_of(s))),
                )(Utils.origen),
              )(Utils.origen),
            )),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(taskID),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3)),
      ),
      false,
      false,
    )(Utils.blame)(Utils.origen("xSemaphoreTakeRecursive"))
  }
}
