package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, Ref}
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.Utils

case class StreamBuffer(size: Int, trigger_bytes: Int) {
  def transform[N](
      scheduler_ref: Ref[N, Class[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      read_event: Int,
      write_event: Int,
      name: String,
  ): Class[N] = {
    val s: InstanceField[N] =
      new InstanceField(TByReferenceClass(scheduler_ref, Seq()), Seq())(
        Utils.origen("s")
      )
    val buffer: InstanceField[N] =
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("buffer"))
    val maxSize: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("maxSize"))
    val triggerLevel: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("triggerLevel"))
    val output: InstanceField[N] =
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("output"))

    val sBufferPerms: InstancePredicate[N] =
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(Seq(
          Perm(Utils.loc_of(s), Utils.read)(Utils.origen),
          Neq(Utils.deref_of(s), Utils.nul)(Utils.origen),
          Perm(Utils.loc_of(maxSize), Utils.read)(Utils.origen),
          Perm(Utils.loc_of(triggerLevel), Utils.read)(Utils.origen),
          Greater(Utils.deref_of(triggerLevel), Utils.int_val(0))(Utils.origen),
          GreaterEq(Utils.deref_of(maxSize), Utils.deref_of(triggerLevel))(
            Utils.origen
          ),
          Perm(Utils.loc_of(buffer), Utils.write)(Utils.origen),
          Perm(Utils.loc_of(output), Utils.write)(Utils.origen),
        ))),
        false,
        true,
      )(Utils.origen("sBufferPerms"))

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](sBufferPerms)

    val sBufferConstructor: PVLConstructor[N] = create_constructor(
      s,
      buffer,
      maxSize,
      triggerLevel,
      perms,
      scheduler_ref,
    )

    val xStreamBufferIsEmpty: InstanceMethod[N] = create_xStreamBufferIsEmpty(
      buffer,
      perms,
    )
    val xStreamBufferIsFull: InstanceMethod[N] = create_xStreamBufferIsFull(
      buffer,
      maxSize,
      perms,
    )
    val xStreamBufferSpacesAvailable: InstanceMethod[N] =
      create_xStreamBufferSpacesAvailable(buffer, maxSize, perms)
    val xStreamBufferReceive: InstanceMethod[N] = create_xStreamBufferReceive(
      s,
      buffer,
      output,
      perms,
      event_ref,
      event_perms_ref,
      read_event,
    )
    val xStreamBufferSend: InstanceMethod[N] = create_xStreamBufferSend(
      s,
      buffer,
      maxSize,
      triggerLevel,
      perms,
      event_ref,
      event_perms_ref,
      write_event,
    )
    // TODO: val xStreamBufferReset: InstanceMethod[N] = ???

    new ByReferenceClass(
      Seq(),
      Seq(
        s,
        buffer,
        maxSize,
        triggerLevel,
        output,
        sBufferPerms,
        sBufferConstructor,
        xStreamBufferIsEmpty,
        xStreamBufferIsFull,
        xStreamBufferSpacesAvailable,
        xStreamBufferReceive,
        xStreamBufferSend,
        // TODO: xStreamBufferReset,
      ),
      Seq(),
      tt,
    )(Utils.origen(name))
  }

  private def create_constructor[N](
      s: InstanceField[N],
      buffer: InstanceField[N],
      maxSize: InstanceField[N],
      triggerLevel: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      scheduler_ref: Ref[N, Class[N]],
  ): PVLConstructor[N] = {
    val s_param: Variable[N] =
      new Variable(TByReferenceClass(scheduler_ref, Seq())(Utils.origen))(
        Utils.origen("s_param")
      )
    val size_param: Variable[N] =
      new Variable(Utils.tint)(Utils.origen("size_param"))
    val trigger_param: Variable[N] =
      new Variable(Utils.tint)(Utils.origen("trigger_param"))

    // requires s_param != null && trigger_param > 0 && size_param >= trigger_param;
    val requires: Expr[N] = Utils.fold_and(Seq(
      Neq(Utils.local_of(s_param), Utils.nul)(Utils.origen),
      Greater(Utils.local_of(trigger_param), Utils.int_val(0))(Utils.origen),
      GreaterEq(Utils.local_of(size_param), Utils.local_of(trigger_param))(
        Utils.origen
      ),
    ))

    // ensures sBufferPerms() ** s == s_param ** maxSize == size_param ** triggerLevel == trigger_param ** |buffer| == 0;
    val ensures: Expr[N] = Utils.fold_star(Seq(
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Eq(Utils.deref_of(s), Utils.local_of(s_param))(Utils.origen),
      Eq(Utils.deref_of(maxSize), Utils.local_of(size_param))(Utils.origen),
      Eq(Utils.deref_of(triggerLevel), Utils.local_of(trigger_param))(
        Utils.origen
      ),
      Eq(Utils.size(buffer), Utils.int_val(0))(Utils.origen),
    ))

    // s = s_param; maxSize = size_param; triggerLevel = trigger_param; buffer = seq<int>{};
    val body: Statement[N] =
      Block(Seq(
        Assign(Utils.deref_of(s), Utils.local_of(s_param))(Utils.origen)(
          Utils.origen
        ),
        Assign(Utils.deref_of(maxSize), Utils.local_of(size_param))(
          Utils.origen
        )(Utils.origen),
        Assign(Utils.deref_of(triggerLevel), Utils.local_of(trigger_param))(
          Utils.origen
        )(Utils.origen),
        Assign(Utils.deref_of(buffer), Utils.seq_val(Seq()))(Utils.origen)(
          Utils.origen
        ),
      ))(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(requires, ensures),
      Seq(),
      Seq(s_param, size_param, trigger_param),
      Some(body),
    )(Utils.origen)(Utils.origen)
  }

  private def create_xStreamBufferIsEmpty[N](
      buffer: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires mBufferPerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == (|buffer| == 0);
    val ensures: Expr[N] =
      Eq(Utils.result, Eq(Utils.size(buffer), Utils.int_val(0))(Utils.origen))(
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
    )(Utils.origen)(Utils.origen("xStreamBufferIsEmpty"))
  }

  private def create_xStreamBufferIsFull[N](
      buffer: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires mBufferPerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == (|buffer| == maxSize);
    val ensures: Expr[N] =
      Eq(
        Utils.result,
        Eq(Utils.size(buffer), Utils.deref_of(maxSize))(Utils.origen),
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
    )(Utils.origen)(Utils.origen("xStreamBufferIsFull"))
  }

  private def create_xStreamBufferSpacesAvailable[N](
      buffer: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires mBufferPerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == maxSize - |buffer|;
    val ensures: Expr[N] =
      Eq(
        Utils.result,
        Minus(Utils.deref_of(maxSize), Utils.size(buffer))(Utils.origen),
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
    )(Utils.origen)(Utils.origen("xStreamBufferSpacesAvailable"))
  }

  private def create_xStreamBufferReceive[N](
      s: InstanceField[N],
      buffer: InstanceField[N],
      output: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      read_event: Int,
  ): InstanceMethod[N] = {
    val n: Variable[N] = new Variable(Utils.tint)(Utils.origen("n"))

    // requires sBufferPerms() ** s.eventPerms();
    // ensures sBufferPerms() ** s.eventPerms();
    val context: Expr[N] =
      Star(
        Utils.predicate_apply(Utils.thiz, perms, Seq()),
        Utils.predicate_apply(Utils.deref_of(s), event_perms_ref, Seq()),
      )(Utils.origen)

    // ensures \old(|buffer|) >= n ==> (   \result == n
    //                                  && output == \old(buffer[0 .. n])
    //                                  && buffer == \old(buffer[n .. |buffer|]));
    val ensures1: Expr[N] =
      Implies(
        GreaterEq(Utils.old(Utils.size(buffer)), Utils.local_of(n))(
          Utils.origen
        ),
        Utils.fold_and(Seq(
          Eq(Utils.result, Utils.local_of(n))(Utils.origen),
          Eq(
            Utils.deref_of(output),
            Utils.old(
              Slice(
                Utils.deref_of(buffer),
                Utils.int_val(0),
                Utils.local_of(n),
              )(Utils.origen)
            ),
          )(Utils.origen),
          Eq(
            Utils.deref_of(buffer),
            Utils.old(
              Slice(
                Utils.deref_of(buffer),
                Utils.local_of(n),
                Utils.size(buffer),
              )(Utils.origen)
            ),
          )(Utils.origen),
        )),
      )(Utils.origen)

    // ensures \old(|buffer|) < n ==> (   \result == \old(|buffer|)
    //                                 && output == \old(buffer)
    //                                 && |buffer| == 0);
    val ensures2: Expr[N] =
      Implies(
        Less(Utils.old(Utils.size(buffer)), Utils.local_of(n))(Utils.origen),
        Utils.fold_and(Seq(
          Eq(Utils.result, Utils.old(Utils.size(buffer)))(Utils.origen),
          Eq(Utils.deref_of(output), Utils.old(Utils.deref_of(buffer)))(
            Utils.origen
          ),
          Eq(Utils.size(buffer), Utils.int_val(0))(Utils.origen),
        )),
      )(Utils.origen)

    // ensures n > 0 ==> s.eventState == \old(s.eventState.update(???, 0));
    val ensures3: Expr[N] =
      Implies(
        Greater(Utils.local_of(n), Utils.int_val(0))(Utils.origen),
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

    // ensures n == 0 ==> s.eventState == \old(s.eventState);
    val ensures4: Expr[N] =
      Implies(
        Eq(Utils.local_of(n), Utils.int_val(0))(Utils.origen),
        Eq(
          Utils.deref_ref(event_ref, Utils.deref_of(s)),
          Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
        )(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(n),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3, ensures4)),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("xStreamBufferReceive"))
  }

  private def create_xStreamBufferSend[N](
      s: InstanceField[N],
      buffer: InstanceField[N],
      maxSize: InstanceField[N],
      triggerLevel: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      write_event: Int,
  ): InstanceMethod[N] = {
    val data: Variable[N] = new Variable(Utils.tseqint)(Utils.origen("data"))

    // requires sBufferPerms() ** s.eventPerms();
    // ensures sBufferPerms() ** s.eventPerms();
    val context: Expr[N] =
      Star(
        Utils.predicate_apply(Utils.thiz, perms, Seq()),
        Utils.predicate_apply(Utils.deref_of(s), event_perms_ref, Seq()),
      )(Utils.origen)

    // ensures \old(|buffer|) + |data| <= maxSize ==> (   \result == |data|
    //                                                 && buffer == \old(buffer) + data);
    val ensures1: Expr[N] =
      Implies(
        LessEq(
          Plus(
            Utils.old(Utils.size(buffer)),
            Size(Utils.local_of(data))(Utils.origen),
          )(Utils.origen),
          Utils.deref_of(maxSize),
        )(Utils.origen),
        And(
          Eq(Utils.result, Size(Utils.local_of(data))(Utils.origen))(
            Utils.origen
          ),
          Eq(
            Utils.deref_of(buffer),
            Concat(Utils.old(Utils.deref_of(buffer)), Utils.local_of(data))(
              Utils.origen
            ),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen)

    // ensures \old(|buffer|) + |data| > maxSize ==> (   \result == maxSize - |buffer|
    //                                                && buffer == \old(buffer) + data[0 .. maxSize - \old(|buffer|)]);
    val ensures2: Expr[N] =
      Implies(
        Greater(
          Plus(
            Utils.old(Utils.size(buffer)),
            Size(Utils.local_of(data))(Utils.origen),
          )(Utils.origen),
          Utils.deref_of(maxSize),
        )(Utils.origen),
        And(
          Eq(
            Utils.result,
            Minus(
              Utils.deref_of(maxSize),
              Size(Utils.local_of(data))(Utils.origen),
            )(Utils.origen),
          )(Utils.origen),
          Eq(
            Utils.deref_of(buffer),
            Concat(
              Utils.old(Utils.deref_of(buffer)),
              Slice(
                Utils.local_of(data),
                Utils.int_val(0),
                Minus(Utils.deref_of(maxSize), Utils.old(Utils.size(buffer)))(
                  Utils.origen
                ),
              )(Utils.origen),
            )(Utils.origen),
          )(Utils.origen),
        )(Utils.origen),
      )(Utils.origen)

    // ensures |buffer| >= triggerLevel ==> s.eventState == \old(s.eventState.update(???, 0));
    val ensures3: Expr[N] =
      Implies(
        GreaterEq(Utils.size(buffer), Utils.deref_of(triggerLevel))(
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
      )(Utils.origen)

    // ensures |buffer| < triggerLevel ==> s.eventState == \old(s.eventState);
    val ensures4: Expr[N] =
      Implies(
        Less(Utils.size(buffer), Utils.deref_of(triggerLevel))(Utils.origen),
        Eq(
          Utils.deref_ref(event_ref, Utils.deref_of(s)),
          Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
        )(Utils.origen),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(data),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2, ensures3, ensures4)),
      ),
      false,
      false,
    )(Utils.origen)(Utils.origen("xStreamBufferSend"))
  }

  /*
   * TODO: Implement method for xStreamBufferReset
   */
}
case object StreamBuffer {
  def of[O](invocation: CInvocation[O]): StreamBuffer = {
    Utils.creation_arg_assert(
      invocation,
      2,
      "Stream buffer creation has wrong number of arguments!",
    )

    val size_arg: Expr[O] = invocation.args.head
    val trigger_arg: Expr[O] = invocation.args(1)

    StreamBuffer(
      Utils.resolve_integer(size_arg, "stream buffer size"),
      Utils.resolve_integer(trigger_arg, "stream buffer trigger level"),
    )
  }
}
