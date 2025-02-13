package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.{ObjectInfo, COLEncoder, Utils}

case class StreamBuffer[O <: Generation](
    decl: Option[CLocal[O]],
    size: Int,
    trigger_bytes: Int,
) extends FreeRTOSConstruct[O] {
  private var s: Option[InstanceField[N]] = None
  private var buffer: Option[InstanceField[N]] = None
  private var maxSize: Option[InstanceField[N]] = None
  private var triggerLevel: Option[InstanceField[N]] = None
  private var output: Option[InstanceField[N]] = None
  private var sBufferPerms: Option[InstancePredicate[N]] = None
  private var xStreamBufferIsEmpty: Option[InstanceMethod[N]] = None
  private var xStreamBufferIsFull: Option[InstanceMethod[N]] = None
  private var xStreamBufferSpacesAvailable: Option[InstanceMethod[N]] = None
  private var xStreamBufferReceive: Option[InstanceMethod[N]] = None
  private var xStreamBufferSend: Option[InstanceMethod[N]] = None

  private def class_name(idx: Int): String =
    decl match {
      case Some(l) => "SBuffer" + l.name
      case None => "SBufferAnonymous" + idx
    }

  private def instance_name(idx: Int): String =
    decl match {
      case Some(l) => l.name
      case None => "unknownSBuffer" + idx
    }

  override def convert(col_ir: COLEncoder[O], idx: Int): ObjectInfo[O] = {
    val read_event: Int = col_ir.reserve_event_id
    val write_event: Int = col_ir.reserve_event_id

    val cls: Class[N] = transform(
      new LazyRef(col_ir.get_scheduler),
      new LazyRef(col_ir.get_eventState),
      new LazyRef(col_ir.get_eventPerms),
      read_event,
      write_event,
      class_name(idx),
    )
    val tcls =
      TByReferenceClass(new DirectRef[N, Class[N]](cls), Seq())(Utils.origen)

    val field: InstanceField[N] =
      new InstanceField(tcls, Seq())(Utils.origen(instance_name(idx)))

    if (decl.nonEmpty) {
      col_ir.add_to_api(
        decl.get,
        "xStreamBufferIsEmpty",
        field,
        xStreamBufferIsEmpty.get,
      )
      col_ir.add_to_api(
        decl.get,
        "xStreamBufferIsFull",
        field,
        xStreamBufferIsFull.get,
      )
      col_ir.add_to_api(
        decl.get,
        "xStreamBufferSpacesAvailable",
        field,
        xStreamBufferSpacesAvailable.get,
      )
      col_ir.add_to_api(
        decl.get,
        "xStreamBufferReceive",
        field,
        xStreamBufferReceive.get,
      )
      col_ir.add_call_condition(
        xStreamBufferReceive.get,
        exprs => Less(Utils.size(buffer.get), exprs.head)(Utils.origen),
      )
      col_ir
        .add_to_api(decl.get, "xStreamBufferSend", field, xStreamBufferSend.get)
      col_ir.add_call_condition(
        xStreamBufferSend.get,
        exprs =>
          Greater(
            Plus(Utils.size(buffer.get), Size(exprs.head)(Utils.origen))(
              Utils.origen
            ),
            Utils.deref_of(maxSize.get),
          )(Utils.origen),
      )
    }
    col_ir.add_output_field(field, output.get)
    col_ir.add_read_event(field, read_event)
    col_ir.add_write_event(field, write_event)

    ObjectInfo(
      decl,
      field,
      cls,
      Seq[Expr[N]](Utils.thiz, Utils.int_val(size)),
      Utils.fold_star(Seq[Expr[N]](
        Perm(Utils.loc_of(field), Utils.read)(Utils.origen),
        Utils.predicate_apply(
          Utils.deref_of(field),
          new DirectRef[N, InstancePredicate[N]](sBufferPerms.get),
          Seq(),
        ),
        Eq(Utils.deref_of(s.get, Some(Utils.deref_of(field))), Utils.thiz)(
          Utils.origen
        ),
        Eq(
          Utils.deref_of(maxSize.get, Some(Utils.deref_of(field))),
          Utils.int_val(size),
        )(Utils.origen),
        Eq(
          Utils.deref_of(triggerLevel.get, Some(Utils.deref_of(field))),
          Utils.int_val(trigger_bytes),
        )(Utils.origen),
      )),
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
      read_event: Int,
      write_event: Int,
      name: String,
  ): Class[N] = {
    s = Some(new InstanceField(TByReferenceClass(scheduler_ref, Seq()), Seq())(
      Utils.origen("s")
    ))
    buffer = Some(
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("buffer"))
    )
    maxSize = Some(
      new InstanceField(Utils.tint, Seq())(Utils.origen("maxSize"))
    )
    triggerLevel = Some(
      new InstanceField(Utils.tint, Seq())(Utils.origen("triggerLevel"))
    )
    output = Some(
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("output"))
    )

    sBufferPerms = Some(
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(Seq(
          Perm(Utils.loc_of(s.get), Utils.read)(Utils.origen),
          Neq(Utils.deref_of(s.get), Utils.nul)(Utils.origen),
          Perm(Utils.loc_of(maxSize.get), Utils.read)(Utils.origen),
          Perm(Utils.loc_of(triggerLevel.get), Utils.read)(Utils.origen),
          Greater(Utils.deref_of(triggerLevel.get), Utils.int_val(0))(
            Utils.origen
          ),
          GreaterEq(
            Utils.deref_of(maxSize.get),
            Utils.deref_of(triggerLevel.get),
          )(Utils.origen),
          Perm(Utils.loc_of(buffer.get), Utils.write)(Utils.origen),
          Perm(Utils.loc_of(output.get), Utils.write)(Utils.origen),
        ))),
        false,
        true,
      )(Utils.origen("sBufferPerms"))
    )

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](sBufferPerms.get)

    val sBufferConstructor: PVLConstructor[N] = create_constructor(
      s.get,
      buffer.get,
      maxSize.get,
      triggerLevel.get,
      perms,
      scheduler_ref,
    )

    xStreamBufferIsEmpty = Some(create_xStreamBufferIsEmpty(buffer.get, perms))
    xStreamBufferIsFull = Some(
      create_xStreamBufferIsFull(buffer.get, maxSize.get, perms)
    )
    xStreamBufferSpacesAvailable = Some(
      create_xStreamBufferSpacesAvailable(buffer.get, maxSize.get, perms)
    )
    xStreamBufferReceive = Some(create_xStreamBufferReceive(
      s.get,
      buffer.get,
      output.get,
      perms,
      event_ref,
      event_perms_ref,
      read_event,
    ))
    xStreamBufferSend = Some(create_xStreamBufferSend(
      s.get,
      buffer.get,
      maxSize.get,
      triggerLevel.get,
      perms,
      event_ref,
      event_perms_ref,
      write_event,
    ))
    // TODO: val xStreamBufferReset: InstanceMethod[N] = ???

    new ByReferenceClass(
      Seq(),
      Seq(
        s.get,
        buffer.get,
        maxSize.get,
        triggerLevel.get,
        output.get,
        sBufferPerms.get,
        sBufferConstructor,
        xStreamBufferIsEmpty.get,
        xStreamBufferIsFull.get,
        xStreamBufferSpacesAvailable.get,
        xStreamBufferReceive.get,
        xStreamBufferSend.get,
        // TODO: xStreamBufferReset,
      ),
      Seq(),
      tt,
    )(Utils.origen(name))
  }

  private def create_constructor(
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
        Assign(Utils.deref_of(s), Utils.local_of(s_param))(Utils.blame)(
          Utils.origen
        ),
        Assign(Utils.deref_of(maxSize), Utils.local_of(size_param))(
          Utils.origen
        )(Utils.origen),
        Assign(Utils.deref_of(triggerLevel), Utils.local_of(trigger_param))(
          Utils.origen
        )(Utils.origen),
        Assign(Utils.deref_of(buffer), Utils.seq_val(Seq()))(Utils.blame)(
          Utils.origen
        ),
      ))(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(requires, ensures),
      Seq(),
      Seq(s_param, size_param, trigger_param),
      Some(body),
    )(Utils.blame)(Utils.origen)
  }

  private def create_xStreamBufferIsEmpty(
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
    )(Utils.blame)(Utils.origen("xStreamBufferIsEmpty"))
  }

  private def create_xStreamBufferIsFull(
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
    )(Utils.blame)(Utils.origen("xStreamBufferIsFull"))
  }

  private def create_xStreamBufferSpacesAvailable(
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
    )(Utils.blame)(Utils.origen("xStreamBufferSpacesAvailable"))
  }

  private def create_xStreamBufferReceive(
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
    )(Utils.blame)(Utils.origen("xStreamBufferReceive"))
  }

  private def create_xStreamBufferSend(
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
    )(Utils.blame)(Utils.origen("xStreamBufferSend"))
  }

  /*
   * TODO: Implement method for xStreamBufferReset
   */
}
case object StreamBuffer {
  def of[O <: Generation](
      variable: Option[CLocal[O]],
      invocation: CInvocation[O],
  ): StreamBuffer[O] = {
    Utils.creation_arg_assert(
      invocation,
      2,
      "Stream buffer creation has wrong number of arguments!",
    )

    val size_arg: Expr[O] = invocation.args.head
    val trigger_arg: Expr[O] = invocation.args(1)

    StreamBuffer(
      variable,
      Utils.resolve_integer(size_arg, "stream buffer size"),
      Utils.resolve_integer(trigger_arg, "stream buffer trigger level"),
    )
  }
}
