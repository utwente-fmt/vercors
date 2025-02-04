package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, Ref}
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.Utils

case class MessageBuffer(size: Int) {
  private val bit_width: Int = 4

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
    val messageSizes: InstanceField[N] =
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("messageSizes"))
    val buffer: InstanceField[N] =
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("buffer"))
    val maxSize: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("maxSize"))
    val output: InstanceField[N] =
      new InstanceField(Utils.tseqint, Seq())(Utils.origen("output"))

    val mBufferPerms: InstancePredicate[N] =
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(Seq(
          Perm(Utils.loc_of(s), Utils.read)(Utils.origen),
          Neq(Utils.deref_of(s), Utils.nul)(Utils.origen),
          Perm(Utils.loc_of(maxSize), Utils.read)(Utils.origen),
          Greater(Utils.deref_of(maxSize), Utils.int_val(0))(Utils.origen),
          Perm(Utils.loc_of(messageSizes), Utils.write)(Utils.origen),
          Perm(Utils.loc_of(buffer), Utils.write)(Utils.origen),
          LessEq(
            Plus(
              Utils.size(buffer),
              Mult(Utils.int_val(bit_width), Utils.size(messageSizes))(
                Utils.origen
              ),
            )(Utils.origen),
            Utils.deref_of(maxSize),
          )(Utils.origen),
          Perm(Utils.loc_of(output), Utils.write)(Utils.origen),
        ))),
        false,
        true,
      )(Utils.origen("mBufferPerms"))

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](mBufferPerms)

    val mBufferConstructor: PVLConstructor[N] = create_constructor(
      s,
      messageSizes,
      buffer,
      maxSize,
      perms,
      scheduler_ref,
    )

    val xMessageBufferIsEmpty: InstanceMethod[N] = create_xMessageBufferIsEmpty(
      messageSizes,
      perms,
    )
    val xMessageBufferIsFull: InstanceMethod[N] = create_xMessageBufferIsFull(
      messageSizes,
      buffer,
      maxSize,
      perms,
    )
    val xMessageBufferSpacesAvailable: InstanceMethod[N] =
      create_xMessageBufferSpacesAvailable(messageSizes, buffer, maxSize, perms)
    val xMessageBufferReceive: InstanceMethod[N] = create_xMessageBufferReceive(
      s,
      messageSizes,
      buffer,
      output,
      perms,
      event_ref,
      event_perms_ref,
      read_event,
    )
    val xMessageBufferSend: InstanceMethod[N] = create_xMessageBufferSend(
      s,
      messageSizes,
      buffer,
      maxSize,
      perms,
      event_ref,
      event_perms_ref,
      write_event,
    )
    // TODO: val xMessageBufferReset: InstanceMethod[N] = ???

    new ByReferenceClass(
      Seq(),
      Seq(
        s,
        messageSizes,
        buffer,
        maxSize,
        output,
        mBufferPerms,
        mBufferConstructor,
        xMessageBufferIsEmpty,
        xMessageBufferIsFull,
        xMessageBufferSpacesAvailable,
        xMessageBufferReceive,
        xMessageBufferSend,
        // TODO: xMessageBufferReset,
      ),
      Seq(),
      tt,
    )(Utils.origen(name))
  }

  private def create_constructor[N](
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
        Assign(Utils.deref_of(s), Utils.local_of(s_param))(Utils.origen)(
          Utils.origen
        ),
        Assign(Utils.deref_of(maxSize), Utils.local_of(size_param))(
          Utils.origen
        )(Utils.origen),
        Assign(Utils.deref_of(buffer), Utils.seq_val(Seq()))(Utils.origen)(
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
    )(Utils.origen)(Utils.origen)
  }

  private def create_xMessageBufferIsEmpty[N](
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
    )(Utils.origen)(Utils.origen("xMessageBufferIsEmpty"))
  }

  private def create_xMessageBufferIsFull[N](
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
    )(Utils.origen)(Utils.origen("xMessageBufferIsFull"))
  }

  private def create_xMessageBufferSpacesAvailable[N](
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
    )(Utils.origen)(Utils.origen("xMessageBufferSpacesAvailable"))
  }

  private def create_xMessageBufferReceive[N](
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
        Utils.fold_and(Seq(
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
    )(Utils.origen)(Utils.origen("xMessageBufferReceive"))
  }

  private def create_xMessageBufferSend[N](
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
        Utils.fold_and(Seq(
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
    )(Utils.origen)(Utils.origen("xMessageBufferSend"))
  }

  /*
   * TODO: Implement method for xMessageBufferReset
   */
}
case object MessageBuffer {
  def of[O](invocation: CInvocation[O]): MessageBuffer = {
    Utils.creation_arg_assert(
      invocation,
      1,
      "Message buffer has wrong number of arguments!",
    )

    val size_arg: Expr[O] = invocation.args.head

    MessageBuffer(Utils.resolve_integer(size_arg, "message buffer size"))
  }
}
