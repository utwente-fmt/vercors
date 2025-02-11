package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.{ObjectInfo, Transformer, Utils}

case class MessageBuffer[O <: Generation](decl: Option[CLocal[O]], size: Int)
    extends FreeRTOSConstruct[O] {
  private val bit_width: Int = 4

  private var s: Option[InstanceField[N]] = None
  private var buffer: Option[InstanceField[N]] = None
  private var messageSizes: Option[InstanceField[N]] = None
  private var maxSize: Option[InstanceField[N]] = None
  private var output: Option[InstanceField[N]] = None
  private var mBufferPerms: Option[InstancePredicate[N]] = None
  private var xMessageBufferIsEmpty: Option[InstanceMethod[N]] = None
  private var xMessageBufferIsFull: Option[InstanceMethod[N]] = None
  private var xMessageBufferSpacesAvailable: Option[InstanceMethod[N]] = None
  private var xMessageBufferReceive: Option[InstanceMethod[N]] = None
  private var xMessageBufferSend: Option[InstanceMethod[N]] = None

  private def class_name(idx: Int): String =
    decl match {
      case Some(l) => "MBuffer" + l.name
      case None => "MBufferAnonymous" + idx
    }

  private def instance_name(idx: Int): String =
    decl match {
      case Some(l) => l.name
      case None => "unknownMBuffer" + idx
    }

  override def convert(
      col_ir: Transformer[O],
      idx: Int,
  ): ObjectInfo[O] = {
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
        "xMessageBufferIsEmpty",
        field,
        xMessageBufferIsEmpty.get,
      )
      col_ir.add_to_api(
        decl.get,
        "xMessageBufferIsFull",
        field,
        xMessageBufferIsFull.get,
      )
      col_ir.add_to_api(
        decl.get,
        "xMessageBufferSpacesAvailable",
        field,
        xMessageBufferSpacesAvailable.get,
      )
      col_ir.add_to_api(
        decl.get,
        "xMessageBufferReceive",
        field,
        xMessageBufferReceive.get,
      )
      col_ir.add_call_condition(
        xMessageBufferReceive.get,
        _ => Eq(Utils.size(messageSizes.get), Utils.int_val(0))(Utils.origen),
      )
      col_ir
        .add_to_api(decl.get, "xMessageBufferSend", field, xMessageBufferSend.get)
      col_ir.add_call_condition(
        xMessageBufferSend.get,
        args =>
          Greater(
            Plus(
              Utils.size(buffer.get),
              Plus(
                Mult(Utils.int_val(bit_width), Utils.size(messageSizes.get))(
                  Utils.origen
                ),
                Plus(Utils.int_val(bit_width), Size(args.head)(Utils.origen))(
                  Utils.origen
                ),
              )(Utils.origen),
            )(Utils.origen),
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
          new DirectRef[N, InstancePredicate[N]](mBufferPerms.get),
          Seq(),
        ),
        Eq(Utils.deref_of(s.get, Some(Utils.deref_of(field))), Utils.thiz)(
          Utils.origen
        ),
        Eq(
          Utils.deref_of(maxSize.get, Some(Utils.deref_of(field))),
          Utils.int_val(size),
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
    s =
      Some(new InstanceField(TByReferenceClass(scheduler_ref, Seq()), Seq())(
        Utils.origen("s")
      ))
    messageSizes =
      Some(new InstanceField(Utils.tseqint, Seq())(Utils.origen("messageSizes")))
    buffer = Some(new InstanceField(Utils.tseqint, Seq())(Utils.origen("buffer")))
    maxSize = Some(new InstanceField(Utils.tint, Seq())(Utils.origen("maxSize")))
    output = Some(new InstanceField(Utils.tseqint, Seq())(Utils.origen("output")))

    mBufferPerms =
      Some(new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(Seq(
          Perm(Utils.loc_of(s.get), Utils.read)(Utils.origen),
          Neq(Utils.deref_of(s.get), Utils.nul)(Utils.origen),
          Perm(Utils.loc_of(maxSize.get), Utils.read)(Utils.origen),
          Greater(Utils.deref_of(maxSize.get), Utils.int_val(0))(Utils.origen),
          Perm(Utils.loc_of(messageSizes.get), Utils.write)(Utils.origen),
          Perm(Utils.loc_of(buffer.get), Utils.write)(Utils.origen),
          LessEq(
            Plus(
              Utils.size(buffer.get),
              Mult(Utils.int_val(bit_width), Utils.size(messageSizes.get))(
                Utils.origen
              ),
            )(Utils.origen),
            Utils.deref_of(maxSize.get),
          )(Utils.origen),
          Perm(Utils.loc_of(output.get), Utils.write)(Utils.origen),
        ))),
        false,
        true,
      )(Utils.origen("mBufferPerms")))

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](mBufferPerms.get)

    val mBufferConstructor: PVLConstructor[N] = create_constructor(
      s.get,
      messageSizes.get,
      buffer.get,
      maxSize.get,
      perms,
      scheduler_ref,
    )

    xMessageBufferIsEmpty = Some(create_xMessageBufferIsEmpty(messageSizes.get, perms))
    xMessageBufferIsFull = Some(create_xMessageBufferIsFull(
      messageSizes.get,
      buffer.get,
      maxSize.get,
      perms,
    ))
    xMessageBufferSpacesAvailable = Some(create_xMessageBufferSpacesAvailable(
      messageSizes.get,
      buffer.get,
      maxSize.get,
      perms,
    ))
    xMessageBufferReceive = Some(create_xMessageBufferReceive(
      s.get,
      messageSizes.get,
      buffer.get,
      output.get,
      perms,
      event_ref,
      event_perms_ref,
      read_event,
    ))
    xMessageBufferSend = Some(create_xMessageBufferSend(
      s.get,
      messageSizes.get,
      buffer.get,
      maxSize.get,
      perms,
      event_ref,
      event_perms_ref,
      write_event,
    ))
    // TODO: val xMessageBufferReset: InstanceMethod[N] = ???

    new ByReferenceClass(
      Seq(),
      Seq(
        s.get,
        messageSizes.get,
        buffer.get,
        maxSize.get,
        output.get,
        mBufferPerms.get,
        mBufferConstructor,
        xMessageBufferIsEmpty.get,
        xMessageBufferIsFull.get,
        xMessageBufferSpacesAvailable.get,
        xMessageBufferReceive.get,
        xMessageBufferSend.get,
        // TODO: xMessageBufferReset,
      ),
      Seq(),
      tt,
    )(Utils.origen(name))
  }

  private def create_constructor(
      s: InstanceField[N],
      messageSizes: InstanceField[N],
      buffer: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      scheduler_ref: Ref[N, Class[N]],
  ): PVLConstructor[N] = {
    val s_param: Variable[N] =
      new Variable(TByReferenceClass(scheduler_ref, Seq())(Utils.origen))(
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

    // ensures mBufferPerms() ** s == s_param ** maxSize == size_param ** |buffer| == 0 ** |messageSizes| == 0;
    val ensures: Expr[N] = Utils.fold_star(Seq(
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Eq(Utils.deref_of(s), Utils.local_of(s_param))(Utils.origen),
      Eq(Utils.deref_of(maxSize), Utils.local_of(size_param))(Utils.origen),
      Eq(Utils.size(buffer), Utils.int_val(0))(Utils.origen),
      Eq(Utils.size(messageSizes), Utils.int_val(0))(Utils.origen),
    ))

    // s = s_param; maxSize = size_param; buffer = seq<int>{}; messageSizes = seq<int>{};
    val body: Statement[N] =
      Block(Seq(
        Assign(Utils.deref_of(s), Utils.local_of(s_param))(Utils.blame)(
          Utils.origen
        ),
        Assign(Utils.deref_of(maxSize), Utils.local_of(size_param))(
          Utils.origen
        )(Utils.origen),
        Assign(Utils.deref_of(buffer), Utils.seq_val(Seq()))(Utils.blame)(
          Utils.origen
        ),
        Assign(Utils.deref_of(messageSizes), Utils.seq_val(Seq()))(
          Utils.origen
        )(Utils.origen),
      ))(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(requires, ensures),
      Seq(),
      Seq(s_param, size_param),
      Some(body),
    )(Utils.blame)(Utils.origen)
  }

  private def create_xMessageBufferIsEmpty(
      messageSizes: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires mBufferPerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == (|messageSizes| == 0);
    val ensures: Expr[N] =
      Eq(
        Utils.result,
        Eq(Utils.size(messageSizes), Utils.int_val(0))(Utils.origen),
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
    )(Utils.blame)(Utils.origen("xMessageBufferIsEmpty"))
  }

  private def create_xMessageBufferIsFull(
      messageSizes: InstanceField[N],
      buffer: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires mBufferPerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == (|buffer| + 4 * |messageSizes| >= maxSize - 4);
    val ensures: Expr[N] =
      Eq(
        Utils.result,
        GreaterEq(
          Plus(
            Utils.size(buffer),
            Mult(Utils.int_val(bit_width), Utils.size(messageSizes))(
              Utils.origen
            ),
          )(Utils.origen),
          Minus(Utils.deref_of(maxSize), Utils.int_val(bit_width))(Utils.origen),
        )(Utils.origen),
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
    )(Utils.blame)(Utils.origen("xMessageBufferIsFull"))
  }

  private def create_xMessageBufferSpacesAvailable(
      messageSizes: InstanceField[N],
      buffer: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
  ): InstanceMethod[N] = {
    // requires mBufferPerms();
    val requires: Expr[N] = Utils.predicate_apply(Utils.thiz, perms, Seq())

    // ensures \result == maxSize - (|buffer| + 4 * |messageSizes|);
    val ensures: Expr[N] =
      Eq(
        Utils.result,
        Minus(
          Utils.deref_of(maxSize),
          Plus(
            Utils.size(buffer),
            Mult(Utils.int_val(bit_width), Utils.size(messageSizes))(
              Utils.origen
            ),
          )(Utils.origen),
        )(Utils.origen),
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
    )(Utils.blame)(Utils.origen("xMessageBufferSpacesAvailable"))
  }

  private def create_xMessageBufferReceive(
      s: InstanceField[N],
      messageSizes: InstanceField[N],
      buffer: InstanceField[N],
      output: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      read_event: Int,
  ): InstanceMethod[N] = {
    val n: Variable[N] = new Variable(Utils.tint)(Utils.origen("n"))

    // requires mBufferPerms() ** s.eventPerms();
    // ensures mBufferPerms() ** s.eventPerms();
    val context: Expr[N] =
      Star(
        Utils.predicate_apply(Utils.thiz, perms, Seq()),
        Utils.predicate_apply(Utils.deref_of(s), event_perms_ref, Seq()),
      )(Utils.origen)

    // ensures     (\old(|messageSizes|) == 0 || n < \old(messageSizes[0]))
    //         ==> (   \result == 0
    //              && messageSizes == \old(messageSizes)
    //              && buffer == \old(buffer)
    //              && s.eventState == \old(s.eventState));
    val ensures1: Expr[N] =
      Implies(
        Or(
          Eq(Utils.old(Utils.size(messageSizes)), Utils.int_val(0))(
            Utils.origen
          ),
          Less(Utils.local_of(n), Utils.old(Utils.subscript(messageSizes, 0)))(
            Utils.origen
          ),
        )(Utils.origen),
        Utils.fold_and(Seq[Expr[N]](
          Eq(Utils.result, Utils.int_val(0))(Utils.origen),
          Eq(
            Utils.deref_of(messageSizes),
            Utils.old(Utils.deref_of(messageSizes)),
          )(Utils.origen),
          Eq(Utils.deref_of(buffer), Utils.old(Utils.deref_of(buffer)))(
            Utils.origen
          ),
          Eq(
            Utils.deref_ref(event_ref, Utils.deref_of(s)),
            Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
          )(Utils.origen),
        )),
      )(Utils.origen)

    // ensures     (\old(|messageSizes|) > 0 && n >= \old(messageSizes[0]))
    //         ==> (   \result == \old(messageSizes[0])
    //              && messageSizes == \old(messageSizes[1 .. |messageSizes|])
    //              && buffer == \old(buffer[messageSizes[0] .. |buffer|])
    //              && output == \old(buffer[0 .. messageSizes[0]])
    //              && s.eventState == \old(s.eventState.update(???, 0)));
    val ensures2: Expr[N] =
      Implies(
        And(
          Greater(Utils.old(Utils.size(messageSizes)), Utils.int_val(0))(
            Utils.origen
          ),
          GreaterEq(
            Utils.local_of(n),
            Utils.old(Utils.subscript(messageSizes, 0)),
          )(Utils.origen),
        )(Utils.origen),
        Utils.fold_and(Seq(
          Eq(Utils.result, Utils.old(Utils.subscript(messageSizes, 0)))(
            Utils.origen
          ),
          Eq(
            Utils.deref_of(messageSizes),
            Utils.old(
              Slice(
                Utils.deref_of(messageSizes),
                Utils.int_val(1),
                Utils.size(messageSizes),
              )(Utils.origen)
            ),
          )(Utils.origen),
          Eq(
            Utils.deref_of(buffer),
            Utils.old(
              Slice(
                Utils.deref_of(buffer),
                Utils.subscript(messageSizes, 0),
                Utils.size(buffer),
              )(Utils.origen)
            ),
          )(Utils.origen),
          Eq(
            Utils.deref_of(output),
            Utils.old(
              Slice(
                Utils.deref_of(buffer),
                Utils.int_val(0),
                Utils.subscript(messageSizes, 0),
              )(Utils.origen)
            ),
          )(Utils.origen),
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
        )),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(n),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2)),
      ),
      false,
      false,
    )(Utils.blame)(Utils.origen("xMessageBufferReceive"))
  }

  private def create_xMessageBufferSend(
      s: InstanceField[N],
      messageSizes: InstanceField[N],
      buffer: InstanceField[N],
      maxSize: InstanceField[N],
      perms: Ref[N, InstancePredicate[N]],
      event_ref: Ref[N, InstanceField[N]],
      event_perms_ref: Ref[N, InstancePredicate[N]],
      write_event: Int,
  ): InstanceMethod[N] = {
    val data: Variable[N] = new Variable(Utils.tseqint)(Utils.origen("data"))

    // requires mBufferPerms() ** s.eventPerms();
    // ensures mBufferPerms() ** s.eventPerms();
    val context: Expr[N] =
      Star(
        Utils.predicate_apply(Utils.thiz, perms, Seq()),
        Utils.predicate_apply(Utils.deref_of(s), event_perms_ref, Seq()),
      )(Utils.origen)

    // ensures     \old(|buffer|) + 4 * \old(|messageSizes|) + 4 + |data| > maxSize
    //         ==> (   \result == 0
    //              && messageSizes == \old(messageSizes)
    //              && buffer == \old(buffer)
    //              && main.eventState == \old(main.eventState));
    val ensures1: Expr[N] =
      Implies(
        Greater(
          Plus(
            Utils.old(Utils.size(buffer)),
            Plus(
              Mult(
                Utils.int_val(bit_width),
                Utils.old(Utils.size(messageSizes)),
              )(Utils.origen),
              Plus(
                Utils.int_val(bit_width),
                Size(Utils.local_of(data))(Utils.origen),
              )(Utils.origen),
            )(Utils.origen),
          )(Utils.origen),
          Utils.deref_of(maxSize),
        )(Utils.origen),
        Utils.fold_and(Seq[Expr[N]](
          Eq(Utils.result, Utils.int_val(0))(Utils.origen),
          Eq(
            Utils.deref_of(messageSizes),
            Utils.old(Utils.deref_of(messageSizes)),
          )(Utils.origen),
          Eq(Utils.deref_of(buffer), Utils.old(Utils.deref_of(buffer)))(
            Utils.origen
          ),
          Eq(
            Utils.deref_ref(event_ref, Utils.deref_of(s)),
            Utils.old(Utils.deref_ref(event_ref, Utils.deref_of(s))),
          )(Utils.origen),
        )),
      )(Utils.origen)

    // ensures     \old(|buffer|) + 4 * \old(|messageSizes|) + 4 + |data| <= maxSize
    //         ==> (   \result == |data|
    //              && messageSizes == \old(messageSizes) + seq<int> {|data|}
    //              && buffer == \old(buffer) + data
    //              && main.eventState == \old(main.eventState.update(???, 0)));
    val ensures2: Expr[N] =
      Implies(
        LessEq(
          Plus(
            Utils.old(Utils.size(buffer)),
            Plus(
              Mult(
                Utils.int_val(bit_width),
                Utils.old(Utils.size(messageSizes)),
              )(Utils.origen),
              Plus(
                Utils.int_val(bit_width),
                Size(Utils.local_of(data))(Utils.origen),
              )(Utils.origen),
            )(Utils.origen),
          )(Utils.origen),
          Utils.deref_of(maxSize),
        )(Utils.origen),
        Utils.fold_and(Seq(
          Eq(Utils.result, Size(Utils.local_of(data))(Utils.origen))(
            Utils.origen
          ),
          Eq(
            Utils.deref_of(messageSizes),
            Concat(
              Utils.old(Utils.deref_of(messageSizes)),
              Utils.seq_val(Seq(Size(Utils.local_of(data))(Utils.origen))),
            )(Utils.origen),
          )(Utils.origen),
          Eq(
            Utils.deref_of(buffer),
            Concat(Utils.old(Utils.deref_of(buffer)), Utils.local_of(data))(
              Utils.origen
            ),
          )(Utils.origen),
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
        )),
      )(Utils.origen)

    new InstanceMethod(
      Utils.tint,
      Seq(data),
      Seq(),
      Seq(),
      None,
      Utils.to_app_contract(
        context,
        Utils.fold_star(Seq(context, ensures1, ensures2)),
      ),
      false,
      false,
    )(Utils.blame)(Utils.origen("xMessageBufferSend"))
  }

  /*
   * TODO: Implement method for xMessageBufferReset
   */
}
case object MessageBuffer {
  def of[O <: Generation](
      variable: Option[CLocal[O]],
      invocation: CInvocation[O],
  ): MessageBuffer[O] = {
    Utils.creation_arg_assert(
      invocation,
      1,
      "Message buffer has wrong number of arguments!",
    )

    val size_arg: Expr[O] = invocation.args.head

    MessageBuffer(
      variable,
      Utils.resolve_integer(size_arg, "message buffer size"),
    )
  }
}
