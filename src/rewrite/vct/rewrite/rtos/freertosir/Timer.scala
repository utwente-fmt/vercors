package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.{ObjectInfo, Transformer, COLEncoder, Utils}

case class Timer[O <: Generation](
    decl: Option[CLocal[O]],
    callback: CFunctionDefinition[O],
    period: Int,
    reload: Boolean,
    priority: Int,
) extends FreeRTOSConstruct[O] {
  private var s: Option[InstanceField[N]] = None
  private var timerPerms: Option[InstancePredicate[N]] = None
  private var tid: Int = -1
  private var eid: Int = -1
  private var activated: Boolean = false
  private var cls: Option[Class[N]] = None

  private def get_cls: Class[N] = cls.get

  def activate(): Unit = { activated = true }

  def set_tid(new_tid: Int): Unit = tid = new_tid
  def set_eid(new_eid: Int): Unit = eid = new_eid

  private def class_name(idx: Int): String =
    decl match {
      case Some(l) => "Timer" + l.name.capitalize
      case None => "TimerAnonymous" + idx
    }

  private def instance_name(idx: Int): String =
    decl match {
      case Some(l) => "timer" + l.name.capitalize
      case None => "unknownTimer" + idx
    }

  override def convert(col_ir: COLEncoder[O], idx: Int): ObjectInfo[O] = {
    val tcls: Type[N] =
      TByReferenceClass(new LazyRef[N, Class[N]](get_cls), Seq())(Utils.origen)
    val field: InstanceField[N] =
      new InstanceField(tcls, Seq())(Utils.origen(instance_name(idx)))

    val program_counter: InstanceField[N] =
      new InstanceField(Utils.tint, Seq())(Utils.origen("pc"))

    cls = Some(transform(
      new LazyRef(col_ir.get_scheduler),
      tid,
      col_ir,
      field,
      program_counter,
      class_name(idx),
    ))

    ObjectInfo(
      decl,
      field,
      Some(program_counter),
      cls.get,
      Seq[Expr[N]](Utils.thiz),
      Utils.fold_star(Seq[Expr[N]](
        Perm(Utils.loc_of(field), Utils.read)(Utils.origen),
        Utils.predicate_apply(
          Utils.deref_of(field),
          new DirectRef[N, InstancePredicate[N]](timerPerms.get),
          Seq(),
        ),
        Eq(Utils.deref_of(s.get, Some(Utils.deref_of(field))), Utils.thiz)(
          Utils.origen
        ),
      )),
      Some(Utils.fold_star(Seq[Expr[N]](
        Perm(Utils.loc_of(field), Utils.read)(Utils.origen),
        Neq(Utils.deref_of(field), Utils.nul)(Utils.origen),
        Perm(Utils.loc_of(s.get, Some(Utils.deref_of(field))), Utils.read)(
          Utils.origen
        ),
        Eq(Utils.deref_of(s.get, Some(Utils.deref_of(field))), Utils.thiz)(
          Utils.origen
        ),
      ))),
      Some(tid),
      Some(priority),
      if (activated)
        Some(period)
      else
        None,
      Some(eid),
      launch = true,
    )
  }

  def transform(
      scheduler_ref: Ref[N, Class[N]],
      tid: Int,
      col_ir: COLEncoder[O],
      field: InstanceField[N],
      program_counter: InstanceField[N],
      name: String,
  ): Class[N] = {
    s = Some(new InstanceField(TByValueClass(scheduler_ref, Seq()), Seq())(
      Utils.origen("s")
    ))

    timerPerms = Some(
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(Seq[Expr[N]](
          Perm(Utils.loc_of(s.get), Utils.read)(Utils.origen),
          Neq(Utils.deref_of(s.get), Utils.nul)(Utils.origen),
          Utils.half_perm_of(program_counter),
        ))),
        threadLocal = false,
        inline = true,
      )(Utils.origen("taskPerms"))
    )

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](timerPerms.get)

    val transformer: Transformer[O] =
      new Transformer(col_ir, Some(tid), s, field, Some(program_counter), Seq())

    val taskConstructor: PVLConstructor[N] = create_constructor(
      scheduler_ref,
      perms,
      program_counter,
    )

    val runMethod: RunMethod[N] = create_runMethod(transformer, program_counter)

    new ByReferenceClass(
      Seq(),
      Seq(s.get, program_counter, timerPerms.get, taskConstructor, runMethod) ++
        transformer.get_additional_methods,
      Seq(),
      tt,
    )(Utils.origen(name))
  }

  private def create_constructor(
      scheduler_ref: Ref[N, Class[N]],
      perms: Ref[N, InstancePredicate[N]],
      program_counter: InstanceField[N],
  ): PVLConstructor[N] = {
    val s_param: Variable[N] =
      new Variable(TByReferenceClass(scheduler_ref, Seq()))(
        Utils.origen("s_param")
      )

    val requires: Expr[N] =
      Neq(Utils.local_of(s_param), Utils.nul)(Utils.origen)

    val ensures: Expr[N] = Utils.fold_star(Seq[Expr[N]](
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Eq(Utils.deref_of(s.get), Utils.local_of(s_param))(Utils.origen),
      Utils.half_perm_of(program_counter),
      Eq(Utils.deref_of(program_counter), Utils.int_val(0))(Utils.origen),
      IdleToken(Utils.thiz)(Utils.origen),
    ))

    val body: Statement[N] =
      Block(Seq[Statement[N]](
        Assign(Utils.deref_of(s.get), Utils.local_of(s_param))(Utils.blame)(
          Utils.origen
        ),
        Assign(Utils.deref_of(program_counter), Utils.int_val(0))(Utils.blame)(
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

  private def create_runMethod(
      transformer: Transformer[O],
      program_counter: InstanceField[N],
  ): RunMethod[N] = {
    val cond: Expr[N] = transformer
      .get_default_contract(holding_global_lock = false, runnable = false)
    val requires: Expr[N] =
      Star(
        cond,
        Eq(Utils.deref_of(program_counter), Utils.int_val(0))(Utils.origen),
      )(Utils.origen)

    val wait_loop: Statement[N] = transformer.wait_loop(
      None,
      None,
      subtract_wait_time = false,
      include_execution_time = true,
    )

    val func_body: Statement[N] = transformer.dispatch(callback.body)

    val body: Statement[N] = {
      Block(Seq[Statement[N]](
        Lock(Utils.deref_of(s.get))(Utils.blame)(Utils.origen),
        wait_loop,
        if (reload)
          Loop(
            Utils.skip,
            tt,
            Utils.skip,
            Utils.to_loop_invariant(transformer.get_default_contract(
              holding_global_lock = true,
              runnable = true,
            )),
            Block(Seq[Statement[N]](
              func_body,
              transformer.wait_loop(
                Some(eid),
                Some(Utils.int_val(period)),
                subtract_wait_time = true,
                include_execution_time = true,
              ),
            ))(Utils.origen),
          )(Utils.origen)
        else
          func_body,
        Unlock(Utils.deref_of(s.get))(Utils.blame)(Utils.origen),
      ))(Utils.origen)
    }

    new RunMethod(Some(body), Utils.to_app_contract(requires, cond))(
      Utils.blame
    )(Utils.origen)
  }
}
case object Timer {
  def of[O <: Generation](
      variable: Option[CLocal[O]],
      invocation: CInvocation[O],
      decls: Seq[CFunctionDefinition[O]],
  ): Timer[O] = {
    Utils.creation_arg_assert(
      invocation,
      4,
      "Timer creation has wrong number of arguments!",
    )

    val period_arg: Expr[O] = invocation.args.head
    val reload_arg: Expr[O] = invocation.args(1)
    val priority_arg: Expr[O] = invocation.args(2)
    val call_arg: CInvocation[O] = invocation.args(3)
      .asInstanceOf[CInvocation[O]]

    Timer(
      variable,
      Utils.resolve_function(call_arg, decls, "timer callback function"),
      Utils.resolve_integer(period_arg, "timer period"),
      Utils.resolve_boolean(reload_arg, "timer reload"),
      Utils.resolve_integer(priority_arg, "timer priority"),
    )
  }
}
