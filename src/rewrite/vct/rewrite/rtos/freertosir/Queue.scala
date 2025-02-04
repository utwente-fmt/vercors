package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, Ref}
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.Utils

case class Queue(capacity: Int) {
  def transform[N](
      scheduler_ref: Ref[N, Class[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      read_event: Int,
      write_event: Int,
      name: String,
  ): Class[N] = {
    // TODO: Store correspondence of methods to their names
    val s: InstanceField[N] =
      new InstanceField(TByReferenceClass(scheduler_ref, Seq()), Seq())(
        Utils.origen("s")
      )
    val vals: InstanceField[N] =
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("vals"))
    val maxSize: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("maxSize"))
    val output: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("output"))

    val queuePerms: InstancePredicate[N] =
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(Seq(
          Perm(Utils.loc_of(s), Utils.read)(Utils.origen),
          Neq(Utils.deref_of(s), Utils.nul)(Utils.origen),
          Perm(Utils.loc_of(maxSize), Utils.read)(Utils.origen),
          Greater(Utils.deref_of(maxSize), Utils.int_val(0))(Utils.origen),
          Perm(Utils.loc_of(vals), Utils.write)(Utils.origen),
          LessEq(Utils.size(vals), Utils.deref_of(maxSize))(Utils.origen),
          Perm(Utils.loc_of(output), Utils.write)(Utils.origen),
        ))),
        false,
        true,
      )(Utils.origen("queuePerms"))

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](queuePerms)

    val queueConstructor: PVLConstructor[N] = create_constructor(
      s,
      vals,
      maxSize,
      perms,
      scheduler_ref,
    )

    val xQueueSendToBack: InstanceMethod[N] = create_xQueueSendToBack(
      s,
      vals,
      maxSize,
      perms,
      event_ref,
      event_perms_ref,
      write_event,
    )
    val xQueueSendToFront: InstanceMethod[N] = create_xQueueSendToFront(
      s,
      vals,
      maxSize,
      perms,
      event_ref,
      event_perms_ref,
      write_event,
    )
    val xQueueOverwrite: InstanceMethod[N] = create_xQueueOverwrite(
      s,
      vals,
      perms,
      event_ref,
      event_perms_ref,
      write_event,
    )
    val xQueueReset: InstanceMethod[N] = create_xQueueReset(vals, perms)
    val xQueueReceive: InstanceMethod[N] = create_xQueueReceive(
      s,
      vals,
      maxSize,
      output,
      perms,
      event_ref,
      event_perms_ref,
      read_event,
    )
    val xQueuePeek: InstanceMethod[N] = create_xQueuePeek(vals, output, perms)
    val uxQueueSpacesAvailable: InstanceMethod[N] =
      create_uxQueueSpacesAvailable(vals, maxSize, perms)
    val uxQueueMessagesWaiting: InstanceMethod[N] =
      create_uxQueueMessagesWaiting(vals, perms)
    val xQueueIsQueueEmptyFromISR: InstanceMethod[N] =
      create_xQueueIsQueueEmptyFromISR(vals, perms)
    val xQueueIsQueueFullFromISR: InstanceMethod[N] =
      create_xQueueIsQueueFullFromISR(vals, maxSize, perms)

    new ByReferenceClass(
      Seq(),
      Seq(
        s,
        vals,
        maxSize,
        output,
        queuePerms,
        queueConstructor,
        xQueueSendToBack,
        xQueueSendToFront,
        xQueueOverwrite,
        xQueueReset,
        xQueueReceive,
        xQueuePeek,
        uxQueueSpacesAvailable,
        uxQueueMessagesWaiting,
        xQueueIsQueueEmptyFromISR,
        xQueueIsQueueFullFromISR,
      ),
      Seq(),
      tt,
    )(Utils.origen(name))
  }

  private def create_constructor[N](
      s: InstanceField[N],
      vals: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      scheduler_ref: Ref[N, Class[N]],
  ): PVLConstructor[N] = {
    val s_param: Variable[N] =
      new Variable(TByReferenceClass(scheduler_ref, Seq()))(
        Utils.origen("s_param")
      )
    val size_param: Variable[N] =
      new Variable(Utils.tint)(Utils.origen("size_param"))

    // requires s_param != null && size_param > 0;
    val requires: Expr[N] =
      And(
        Neq(Utils.local_of(s_param), Utils.nul)(Utils.origen),
        Greater(Utils.local_of(size_param), Utils.int_val(0))(Utils.origen),
      )(Utils.origen)
    // ensures queuePerms() ** s == s_param ** maxSize == size_param ** |vals| == 0;
    val ensures: Expr[N] =
      Star(
        Utils.predicate_apply(Utils.thiz, perms, Seq()),
        Star(
          Eq(Utils.deref_of(s), Utils.local_of(s_param))(Utils.origen),
          Star(
            Eq(Utils.deref_of(maxSize), Utils.local_of(size_param))(
              Utils.origen
            ),
            Eq(Utils.size(vals), Utils.int_val(0))(Utils.origen),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen)

    // s = s_param; maxSize = size_param; vals = seq<int> {};
    val body: Statement[N] =
      Block(Seq(
        Assign(Utils.deref_of(s), Utils.local_of(s_param))(Utils.origen)(
          Utils.origen
        ),
        Assign(Utils.deref_of(maxSize), Utils.local_of(size_param))(
          Utils.origen
        )(Utils.origen),
        Assign(Utils.deref_of(vals), Utils.seq_val(Seq()))(Utils.origen)(
          Utils.origen
        ),
      ))(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(requires, ensures),
      Seq(),
      Seq(s_param, size_param),
      Some(body),
    )(Utils.origen)(Utils.origen)
  }

  private def create_xQueueSendToBack[N](
      s: InstanceField[N],
      vals: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      write_event: Int,
  ): InstanceMethod[N] = {
    val value: Variable[N] = new Variable(Utils.tint)(Utils.origen("value"))

    // requires queuePerms() ** s.eventPerms();
    // ensures queuePerms() ** s.eventPerms();
    val context: Expr[N] =
      Star(
        Utils.predicate_apply(Utils.thiz, perms, Seq()),
        Utils.predicate_apply(Utils.deref_of(s), event_perms_ref, Seq()),
      )(Utils.origen)

    // ensures \old(|vals|) < maxSize ==> (\result && vals == \old(vals) + seq<int> {value});
    val ensures1: Expr[N] =
      Implies(
        Less(Utils.old(Utils.size(vals)), Utils.deref_of(maxSize))(
          Utils.origen
        ),
        And(
          Utils.result,
          Eq(
            Utils.deref_of(vals),
            Concat(
              Utils.old(Utils.deref_of(vals)),
              Utils.seq_val(Seq(Utils.local_of(value))),
            )(Utils.origen),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(|vals|) >= maxSize ==> (!\result && vals == \old(vals));
    val ensures2: Expr[N] =
      Implies(
        GreaterEq(Utils.old(Utils.size(vals)), Utils.deref_of(maxSize))(
          Utils.origen
        ),
        And(
          Not(Utils.result)(Utils.origen),
          Eq(Utils.deref_of(vals), Utils.old(Utils.deref_of(vals)))(
            Utils.origen
          ),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(|vals|) == 0 ==> s.eventState == \old(s.eventState.update(???, 0));
    val ensures3: Expr[N] =
      Implies(
        Eq(Utils.old(Utils.size(vals)), Utils.int_val(0))(Utils.origen),
        Eq(
          Utils.deref_ref(event_ref, Utils.deref_of(s)),
          Utils.old(
            SeqUpdate(
              Utils.deref_ref(event_ref, Utils.deref_of(s)),
              Utils.int_val(write_event),
              Utils.int_val(0),
            )(Utils.origen)
          ),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(|vals|) != 0 ==> s.eventState == \old(s.eventState);
    val ensures4: Expr[N] =
      Implies(
        Neq(Utils.old(Utils.size(vals)), Utils.int_val(0))(Utils.origen),
        Eq(
          Utils.deref_ref(event_ref, Utils.deref_of(s)),
          Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
        )(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(value),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3, ensures4)),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("xQueueSendToBack"))
  }

  private def create_xQueueSendToFront[N](
      s: InstanceField[N],
      vals: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      write_event: Int,
  ): InstanceMethod[N] = {
    val value: Variable[N] = new Variable(Utils.tint)(Utils.origen("value"))

    // requires queuePerms() ** s.eventPerms();
    // ensures queuePerms() ** s.eventPerms();
    val context: Expr[N] =
      Star(
        Utils.predicate_apply(Utils.thiz, perms, Seq()),
        Utils.predicate_apply(Utils.deref_of(s), event_perms_ref, Seq()),
      )(Utils.origen)

    // ensures \old(|vals|) < maxSize ==> (\result && vals == seq<int> {value} + \old(vals));
    val ensures1: Expr[N] =
      Implies(
        Less(Utils.old(Utils.size(vals)), Utils.deref_of(maxSize))(
          Utils.origen
        ),
        And(
          Utils.result,
          Eq(
            Utils.deref_of(vals),
            Concat(
              Utils.seq_val(Seq(Utils.local_of(value))),
              Utils.old(Utils.deref_of(vals)),
            )(Utils.origen),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(|vals|) >= maxSize ==> (!\result && vals == \old(vals));
    val ensures2: Expr[N] =
      Implies(
        GreaterEq(Utils.old(Utils.size(vals)), Utils.deref_of(maxSize))(
          Utils.origen
        ),
        And(
          Not(Utils.result)(Utils.origen),
          Eq(Utils.deref_of(vals), Utils.old(Utils.deref_of(vals)))(
            Utils.origen
          ),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(|vals|) == 0 ==> s.eventState == \old(s.eventState.update(???, 0));
    val ensures3: Expr[N] =
      Implies(
        Eq(Utils.old(Utils.size(vals)), Utils.int_val(0))(Utils.origen),
        Eq(
          Utils.deref_ref(event_ref, Utils.deref_of(s)),
          Utils.old(
            SeqUpdate(
              Utils.deref_ref(event_ref, Utils.deref_of(s)),
              Utils.int_val(write_event),
              Utils.int_val(0),
            )(Utils.origen)
          ),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(|vals|) != 0 ==> s.eventState == \old(s.eventState);
    val ensures4: Expr[N] =
      Implies(
        Neq(Utils.old(Utils.size(vals)), Utils.int_val(0))(Utils.origen),
        Eq(
          Utils.deref_ref(event_ref, Utils.deref_of(s)),
          Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
        )(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(value),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3, ensures4)),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("xQueueSendToFront"))
  }

  private def create_xQueueOverwrite[N](
      s: InstanceField[N],
      vals: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      write_event: Int,
  ): InstanceMethod[N] = {
    val value: Variable[N] = new Variable(Utils.tint)(Utils.origen("value"))

    // requires queuePerms() ** s.eventPerms();
    // ensures queuePerms() ** s.eventPerms();
    val context: Expr[N] =
      Star(
        Utils.predicate_apply(Utils.thiz, perms, Seq()),
        Utils.predicate_apply(Utils.deref_of(s), event_perms_ref, Seq()),
      )(Utils.origen)

    // ensures \result;
    val ensures1: Expr[N] = Utils.result

    // ensures \old(|vals|) > 0 ==> (vals == seq<int> {value} + \old(vals[1 .. |vals|]) && main.eventState == \old(main.eventState));
    val ensures2: Expr[N] =
      Implies(
        Greater(Utils.old(Utils.size(vals)), Utils.int_val(0))(Utils.origen),
        And(
          Eq(
            Utils.deref_of(vals),
            Concat(
              Utils.seq_val(Seq(Utils.local_of(value))),
              Utils.old(
                Slice(Utils.deref_of(vals), Utils.int_val(1), Utils.size(vals))(
                  Utils.origen
                )
              ),
            )(Utils.origen),
          )(Utils.origen),
          Eq(
            Utils.deref_ref(event_ref, Utils.deref_of(s)),
            Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(|vals|) == 0 ==> (vals == seq<int> {value} && main.eventState == \old(main.eventState.update(???, 0)));
    val ensures3: Expr[N] =
      Implies(
        Eq(Utils.old(Utils.size(vals)), Utils.int_val(0))(Utils.origen),
        And(
          Eq(Utils.deref_of(vals), Utils.seq_val(Seq(Utils.local_of(value))))(
            Utils.origen
          ),
          Eq(
            Utils.deref_ref(event_ref, Utils.deref_of(s)),
            Utils.old(
              SeqUpdate(
                Utils.deref_ref(event_ref, Utils.deref_of(s)),
                Utils.int_val(write_event),
                Utils.int_val(0),
              )(Utils.origen)
            ),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(value),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3)),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("xQueueOverwrite"))
  }

  private def create_xQueueReset[N](
      vals: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires queuePerms();
    // ensures queuePerms();
    val context: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result && vals == seq<int> {};
    val ensures: Expr[N] =
      And(
        Utils.result,
        Eq(Utils.deref_of(vals), Utils.seq_val(Seq()))(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(context, Utils.fold_star(Seq(context, ensures))),
      false,
      false,
    )(Utils.origen)(Utils.origen("xQueueReset"))
  }

  private def create_xQueueReceive[N](
      s: InstanceField[N],
      vals: InstanceField[N],
      maxSize: InstanceField[N],
      output: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      read_event: Int,
  ): InstanceMethod[N] = {
    // requires queuePerms() ** s.eventPerms();
    // ensures queuePerms() ** s.eventPerms();
    val context: Expr[N] =
      Star(
        Utils.predicate_apply(Utils.thiz, perms, Seq()),
        Utils.predicate_apply(Utils.deref_of(s), event_perms_ref, Seq()),
      )(Utils.origen)

    // ensures \old(|vals|) > 0 ==> (\result && output == \old(vals[0]) && vals == \old(vals[1 .. |vals|]));
    val ensures1: Expr[N] =
      Implies(
        Greater(Utils.old(Utils.size(vals)), Utils.int_val(0))(Utils.origen),
        Utils.fold_and(Seq(
          Utils.result,
          Eq(Utils.deref_of(output), Utils.old(Utils.subscript(vals, 0)))(
            Utils.origen
          ),
          Eq(
            Utils.deref_of(vals),
            Utils.old(
              Slice(Utils.deref_of(vals), Utils.int_val(1), Utils.size(vals))(
                Utils.origen
              )
            ),
          )(Utils.origen),
        )),
      )(Utils.origen)

    // ensures \old(|vals|) == 0 ==> (!\result && vals == \old(vals) && s.eventState == \old(s.eventState));
    val ensures2: Expr[N] =
      Implies(
        Eq(Utils.old(Utils.size(vals)), Utils.int_val(0))(Utils.origen),
        Utils.fold_and(Seq(
          Not(Utils.result)(Utils.origen),
          Eq(Utils.deref_of(vals), Utils.old(Utils.deref_of(vals)))(
            Utils.origen
          ),
          Eq(
            Utils.deref_ref(event_ref, Utils.deref_of(s)),
            Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
          )(Utils.origen),
        )),
      )(Utils.origen)

    // ensures \old(|vals|) == maxSize ==> s.eventState == \old(s.eventState.update(???, 0));
    val ensures3: Expr[N] =
      Implies(
        Eq(Utils.old(Utils.size(vals)), Utils.deref_of(maxSize))(Utils.origen),
        Eq(
          Utils.deref_ref(event_ref, Utils.deref_of(s)),
          Utils.old(
            SeqUpdate(
              Utils.deref_ref(event_ref, Utils.deref_of(s)),
              Utils.int_val(read_event),
              Utils.int_val(0),
            )(Utils.origen)
          ),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(|vals|) != maxSize ==> s.eventState == \old(s.eventState);
    val ensures4: Expr[N] =
      Implies(
        Neq(Utils.old(Utils.size(vals)), Utils.deref_of(maxSize))(Utils.origen),
        Eq(
          Utils.deref_ref(event_ref, Utils.deref_of(s)),
          Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
        )(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3, ensures4)),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("xQueueReceive"))
  }

  private def create_xQueuePeek[N](
      vals: InstanceField[N],
      output: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires queuePerms();
    // ensures queuePerms();
    val context: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures vals == \old(vals);
    val ensures1: Expr[N] =
      Eq(Utils.deref_of(vals), Utils.old(Utils.deref_of(vals)))(Utils.origen)

    // ensures \old(|vals|) > 0 ==> (\result && output == \old(vals[0]));
    val ensures2: Expr[N] =
      Implies(
        Greater(Utils.old(Utils.size(vals)), Utils.int_val(0))(Utils.origen),
        And(
          Utils.result,
          Eq(Utils.deref_of(output), Utils.old(Utils.subscript(vals, 0)))(
            Utils.origen
          ),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(|vals|) == 0 ==> (!\result && output == \old(output));
    val ensures3: Expr[N] =
      Implies(
        Eq(Utils.old(Utils.size(vals)), Utils.int_val(0))(Utils.origen),
        And(
          Not(Utils.result)(Utils.origen),
          Eq(Utils.deref_of(output), Utils.old(Utils.deref_of(output)))(
            Utils.origen
          ),
        )(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3)),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("xQueuePeek"))
  }

  private def create_uxQueueSpacesAvailable[N](
      vals: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires queuePerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == maxSize - |vals|;
    val ensures: Expr[N] =
      Eq(
        Utils.result,
        Minus(Utils.deref_of(maxSize), Utils.size(vals))(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(requires, ensures),
      false,
      true,
    )(Utils.origen)(Utils.origen("uxQueueSpacesAvailable"))
  }

  private def create_uxQueueMessagesWaiting[N](
      vals: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires queuePerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == |vals|;
    val ensures: Expr[N] = Eq(Utils.result, Utils.size(vals))(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(requires, ensures),
      false,
      true,
    )(Utils.origen)(Utils.origen("uxQueueMessagesWaiting"))
  }

  private def create_xQueueIsQueueEmptyFromISR[N](
      vals: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires queuePerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == (|vals| == 0);
    val ensures: Expr[N] =
      Eq(Utils.result, Eq(Utils.size(vals), Utils.int_val(0))(Utils.origen))(
        Utils.origen
      )

    new InstanceMethod(
      Utils.tbool,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(requires, ensures),
      false,
      true,
    )(Utils.origen)(Utils.origen("xQueueIsQueueEmptyFromISR"))
  }

  private def create_xQueueIsQueueFullFromISR[N](
      vals: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires queuePerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == (|vals| == maxSize);
    val ensures: Expr[N] =
      Eq(
        Utils.result,
        Eq(Utils.size(vals), Utils.deref_of(maxSize))(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tbool,
      Seq(),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(requires, ensures),
      false,
      true,
    )(Utils.origen)(Utils.origen("xQueueIsQueueFullFromISR"))
  }
}
case object Queue {
  def of[O](invocation: CInvocation[O]): Queue = {
    Utils.creation_arg_assert(
      invocation,
      2,
      "Queue creation has wrong number of arguments!",
    )

    val size_arg: Expr[O] = invocation.args.head

    Queue(Utils.resolve_integer(size_arg, "queue size"))
  }
}
