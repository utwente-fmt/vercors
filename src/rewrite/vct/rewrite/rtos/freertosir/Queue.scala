package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.{ObjectInfo, COLEncoder, Utils}

case class Queue[O <: Generation](decl: Option[CLocal[O]], capacity: Int)
    extends FreeRTOSConstruct[O] {

  private var s: Option[InstanceField[N]] = None
  private var vals: Option[InstanceField[N]] = None
  private var maxSize: Option[InstanceField[N]] = None
  private var output: Option[InstanceField[N]] = None
  private var queuePerms: Option[InstancePredicate[N]] = None
  private var xQueueSendToBack: Option[InstanceMethod[N]] = None
  private var xQueueSendToFront: Option[InstanceMethod[N]] = None
  private var xQueueOverwrite: Option[InstanceMethod[N]] = None
  private var xQueueReset: Option[InstanceMethod[N]] = None
  private var xQueueReceive: Option[InstanceMethod[N]] = None
  private var xQueuePeek: Option[InstanceMethod[N]] = None
  private var uxQueueSpacesAvailable: Option[InstanceMethod[N]] = None
  private var uxQueueMessagesWaiting: Option[InstanceMethod[N]] = None
  private var xQueueIsQueueEmptyFromISR: Option[InstanceMethod[N]] = None
  private var xQueueIsQueueFullFromISR: Option[InstanceMethod[N]] = None

  private def class_name(idx: Int): String =
    decl match {
      case Some(l) => "Queue" + l.name
      case None => "QueueAnonymous" + idx
    }

  private def instance_name(idx: Int): String =
    decl match {
      case Some(l) => l.name
      case None => "unknownQueue" + idx
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
      col_ir
        .add_to_api(decl.get, "xQueueSendToBack", field, xQueueSendToBack.get)
      col_ir.add_call_condition(
        xQueueSendToBack.get,
        (scheduler, _) =>
          GreaterEq(
            Utils.size(vals.get, Some(Utils.deref_of(field, Some(scheduler)))),
            Utils.deref_of(maxSize.get),
          )(Utils.origen),
      )
      col_ir
        .add_to_api(decl.get, "xQueueSendToFront", field, xQueueSendToFront.get)
      col_ir.add_call_condition(
        xQueueSendToFront.get,
        (scheduler, _) =>
          GreaterEq(
            Utils.size(vals.get, Some(Utils.deref_of(field, Some(scheduler)))),
            Utils.deref_of(maxSize.get),
          )(Utils.origen),
      )
      col_ir.add_to_api(decl.get, "xQueueOverwrite", field, xQueueOverwrite.get)
      col_ir.add_to_api(decl.get, "xQueueReset", field, xQueueReset.get)
      col_ir.add_to_api(decl.get, "xQueueReceive", field, xQueueReceive.get)
      col_ir.add_call_condition(
        xQueueReceive.get,
        (scheduler, _) =>
          Eq(
            Utils.size(vals.get, Some(Utils.deref_of(field, Some(scheduler)))),
            Utils.int_val(0),
          )(Utils.origen),
      )
      col_ir.add_to_api(decl.get, "xQueuePeek", field, xQueuePeek.get)
      col_ir.add_call_condition(
        xQueuePeek.get,
        (scheduler, _) =>
          Eq(
            Utils.size(vals.get, Some(Utils.deref_of(field, Some(scheduler)))),
            Utils.int_val(0),
          )(Utils.origen),
      )
      col_ir.add_to_api(
        decl.get,
        "uxQueueSpacesAvailable",
        field,
        uxQueueSpacesAvailable.get,
      )
      col_ir.add_to_api(
        decl.get,
        "uxQueueMessagesWaiting",
        field,
        uxQueueMessagesWaiting.get,
      )
      col_ir.add_to_api(
        decl.get,
        "xQueueIsQueueEmptyFromISR",
        field,
        xQueueIsQueueEmptyFromISR.get,
      )
      col_ir.add_to_api(
        decl.get,
        "xQueueIsQueueFullFromISR",
        field,
        xQueueIsQueueFullFromISR.get,
      )
    }
    col_ir.add_output_field(field, output.get)
    col_ir.add_read_event(field, read_event)
    col_ir.add_write_event(field, write_event)

    ObjectInfo(
      decl,
      field,
      None,
      cls,
      Seq[Expr[N]](Utils.thiz, Utils.int_val(capacity)),
      Utils.fold_star(Seq[Expr[N]](
        Perm(Utils.loc_of(field), Utils.read)(Utils.origen),
        Utils.predicate_apply(
          Utils.deref_of(field),
          new DirectRef[N, InstancePredicate[N]](queuePerms.get),
          Seq(),
        ),
        Eq(Utils.deref_of(s.get, Some(Utils.deref_of(field))), Utils.thiz)(
          Utils.origen
        ),
        Eq(
          Utils.deref_of(maxSize.get, Some(Utils.deref_of(field))),
          Utils.int_val(capacity),
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
    vals = Some(new InstanceField(Utils.tseqint, Seq())(Utils.origen("vals")))
    maxSize = Some(
      new InstanceField(Utils.tint, Seq())(Utils.origen("maxSize"))
    )
    output = Some(new InstanceField(Utils.tint, Seq())(Utils.origen("output")))

    queuePerms = Some(
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(Seq(
          Perm(Utils.loc_of(s.get), Utils.read)(Utils.origen),
          Neq(Utils.deref_of(s.get), Utils.nul)(Utils.origen),
          Perm(Utils.loc_of(maxSize.get), Utils.read)(Utils.origen),
          Greater(Utils.deref_of(maxSize.get), Utils.int_val(0))(Utils.origen),
          Perm(Utils.loc_of(vals.get), Utils.write)(Utils.origen),
          LessEq(Utils.size(vals.get), Utils.deref_of(maxSize.get))(
            Utils.origen
          ),
          Perm(Utils.loc_of(output.get), Utils.write)(Utils.origen),
        ))),
        false,
        true,
      )(Utils.origen("queuePerms"))
    )

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](queuePerms.get)

    val queueConstructor: PVLConstructor[N] = create_constructor(
      s.get,
      vals.get,
      maxSize.get,
      perms,
      scheduler_ref,
    )

    xQueueSendToBack = Some(create_xQueueSendToBack(
      s.get,
      vals.get,
      maxSize.get,
      perms,
      event_ref,
      event_perms_ref,
      write_event,
    ))
    xQueueSendToFront = Some(create_xQueueSendToFront(
      s.get,
      vals.get,
      maxSize.get,
      perms,
      event_ref,
      event_perms_ref,
      write_event,
    ))
    xQueueOverwrite = Some(create_xQueueOverwrite(
      s.get,
      vals.get,
      perms,
      event_ref,
      event_perms_ref,
      write_event,
    ))
    xQueueReset = Some(create_xQueueReset(vals.get, perms))
    xQueueReceive = Some(create_xQueueReceive(
      s.get,
      vals.get,
      maxSize.get,
      output.get,
      perms,
      event_ref,
      event_perms_ref,
      read_event,
    ))
    xQueuePeek = Some(create_xQueuePeek(vals.get, output.get, perms))
    uxQueueSpacesAvailable = Some(
      create_uxQueueSpacesAvailable(vals.get, maxSize.get, perms)
    )
    uxQueueMessagesWaiting = Some(
      create_uxQueueMessagesWaiting(vals.get, perms)
    )
    xQueueIsQueueEmptyFromISR = Some(
      create_xQueueIsQueueEmptyFromISR(vals.get, perms)
    )
    xQueueIsQueueFullFromISR = Some(
      create_xQueueIsQueueFullFromISR(vals.get, maxSize.get, perms)
    )

    new ByReferenceClass(
      Seq(),
      Seq(
        s.get,
        vals.get,
        maxSize.get,
        output.get,
        queuePerms.get,
        queueConstructor,
        xQueueSendToBack.get,
        xQueueSendToFront.get,
        xQueueOverwrite.get,
        xQueueReset.get,
        xQueueReceive.get,
        xQueuePeek.get,
        uxQueueSpacesAvailable.get,
        uxQueueMessagesWaiting.get,
        xQueueIsQueueEmptyFromISR.get,
        xQueueIsQueueFullFromISR.get,
      ),
      Seq(),
      tt,
    )(Utils.origen(name))
  }

  private def create_constructor(
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
        Assign(Utils.deref_of(s), Utils.local_of(s_param))(Utils.blame)(
          Utils.origen
        ),
        Assign(Utils.deref_of(maxSize), Utils.local_of(size_param))(
          Utils.origen
        )(Utils.origen),
        Assign(Utils.deref_of(vals), Utils.seq_val(Seq()))(Utils.blame)(
          Utils.origen
        ),
      ))(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(requires, ensures),
      Seq(),
      Seq(s_param, size_param),
      Some(body),
    )(Utils.blame)(Utils.origen)
  }

  private def create_xQueueSendToBack(
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

    // ensures \old(|vals|) < \old(maxSize) ==> (\result && vals == \old(vals) + seq<int> {value});
    val ensures1: Expr[N] =
      Implies(
        Less(Utils.old(Utils.size(vals)), Utils.old(Utils.deref_of(maxSize)))(
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

    // ensures \old(|vals|) >= \old(maxSize) ==> (!\result && vals == \old(vals));
    val ensures2: Expr[N] =
      Implies(
        GreaterEq(
          Utils.old(Utils.size(vals)),
          Utils.old(Utils.deref_of(maxSize)),
        )(Utils.origen),
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
    )(Utils.blame)(Utils.origen("xQueueSendToBack"))
  }

  private def create_xQueueSendToFront(
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

    // ensures \old(|vals|) < \old(maxSize) ==> (\result && vals == seq<int> {value} + \old(vals));
    val ensures1: Expr[N] =
      Implies(
        Less(Utils.old(Utils.size(vals)), Utils.old(Utils.deref_of(maxSize)))(
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

    // ensures \old(|vals|) >= \old(maxSize) ==> (!\result && vals == \old(vals));
    val ensures2: Expr[N] =
      Implies(
        GreaterEq(
          Utils.old(Utils.size(vals)),
          Utils.old(Utils.deref_of(maxSize)),
        )(Utils.origen),
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
    )(Utils.blame)(Utils.origen("xQueueSendToFront"))
  }

  private def create_xQueueOverwrite(
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
    )(Utils.blame)(Utils.origen("xQueueOverwrite"))
  }

  private def create_xQueueReset(
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
    )(Utils.blame)(Utils.origen("xQueueReset"))
  }

  private def create_xQueueReceive(
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
        Utils.fold_and(Seq[Expr[N]](
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
        Utils.fold_and(Seq[Expr[N]](
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

    // ensures \old(|vals|) == \old(maxSize) ==> s.eventState == \old(s.eventState.update(???, 0));
    val ensures3: Expr[N] =
      Implies(
        Eq(Utils.old(Utils.size(vals)), Utils.old(Utils.deref_of(maxSize)))(
          Utils.origen
        ),
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

    // ensures \old(|vals|) != \old(maxSize) ==> s.eventState == \old(s.eventState);
    val ensures4: Expr[N] =
      Implies(
        Neq(Utils.old(Utils.size(vals)), Utils.old(Utils.deref_of(maxSize)))(
          Utils.origen
        ),
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
    )(Utils.blame)(Utils.origen("xQueueReceive"))
  }

  private def create_xQueuePeek(
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
    )(Utils.blame)(Utils.origen("xQueuePeek"))
  }

  private def create_uxQueueSpacesAvailable(
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
    )(Utils.blame)(Utils.origen("uxQueueSpacesAvailable"))
  }

  private def create_uxQueueMessagesWaiting(
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
    )(Utils.blame)(Utils.origen("uxQueueMessagesWaiting"))
  }

  private def create_xQueueIsQueueEmptyFromISR(
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
    )(Utils.blame)(Utils.origen("xQueueIsQueueEmptyFromISR"))
  }

  private def create_xQueueIsQueueFullFromISR(
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
    )(Utils.blame)(Utils.origen("xQueueIsQueueFullFromISR"))
  }
}
case object Queue {
  def of[O <: Generation](
      variable: Option[CLocal[O]],
      invocation: CInvocation[O],
  ): Queue[O] = {
    Utils.creation_arg_assert(
      invocation,
      2,
      "Queue creation has wrong number of arguments!",
    )

    val size_arg: Expr[O] = invocation.args.head

    Queue(variable, Utils.resolve_integer(size_arg, "queue size"))
  }
}
