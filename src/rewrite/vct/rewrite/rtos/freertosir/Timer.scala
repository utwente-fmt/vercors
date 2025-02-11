package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.{ObjectInfo, StatementTransformer, Transformer, Utils}

case class Timer[O, N](
    decl: Option[CLocal[O]],
    callback: CFunctionDefinition[O],
    period: Int,
    reload: Boolean,
    priority: Int,
) extends FreeRTOSConstruct[O, N] {
  private var s: Option[InstanceField[N]] = None
  private var timerPerms: Option[InstancePredicate[N]] = None

  private var activated: Boolean = false
  private var cls: Option[Class[N]] = None

  private def get_cls: Class[N] = cls.get

  // TODO: Activate if the main method activates the timer
  def activate(): Unit = { activated = true }

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

  override def convert(
      col_ir: Transformer[O, N],
      idx: Int,
  ): ObjectInfo[O, N] = {
    val tcls: Type[N] =
      TByReferenceClass(new LazyRef[N, Class[N]](get_cls), Seq())(Utils.origen)
    val field: InstanceField[N] =
      new InstanceField(tcls, Seq())(Utils.origen(instance_name(idx)))
    val tid: Int = col_ir.reserve_task_id
    val eid: Int = col_ir.reserve_event_id

    cls = Some(transform(
      new LazyRef(col_ir.get_scheduler),
      tid,
      eid,
      col_ir,
      field,
      class_name(idx),
    ))

    ObjectInfo(
      decl,
      field,
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
      assigned_eid: Int,
      col_ir: Transformer[O, N],
      field: InstanceField[N],
      name: String,
  ): Class[N] = {
    s =
      Some(new InstanceField(TByValueClass(scheduler_ref, Seq()), Seq())(
        Utils.origen("s")
      ))

    timerPerms =
      Some(new InstancePredicate(
        Seq(),
        Some(
          Star(
            Perm(Utils.loc_of(s.get), Utils.read)(Utils.origen),
            Neq(Utils.deref_of(s.get), Utils.nul)(Utils.origen),
          )(Utils.origen)
        ),
      )(Utils.origen("taskPerms")))

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](timerPerms.get)

    val transformer: StatementTransformer[O, N] =
      new StatementTransformer(col_ir, Some(tid), s, field)

    val taskConstructor: PVLConstructor[N] = create_constructor(
      scheduler_ref,
      perms,
    )

    // TODO:
    val runMethod: RunMethod[N] = create_runMethod(transformer)

    new ByReferenceClass(
      Seq(),
      Seq(s.get, timerPerms.get, taskConstructor, runMethod),
      Seq(),
      tt,
    )(Utils.origen(name))
  }

  private def create_constructor(
      scheduler_ref: Ref[N, Class[N]],
      perms: Ref[N, InstancePredicate[N]],
  ): PVLConstructor[N] = {
    val s_param: Variable[N] =
      new Variable(TByReferenceClass(scheduler_ref, Seq()))(
        Utils.origen("s_param")
      )

    val requires: Expr[N] = Neq(Utils.deref_of(s.get), Utils.nul)(Utils.origen)

    val ensures: Expr[N] = Utils.fold_star(Seq[Expr[N]](
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Eq(Utils.deref_of(s.get), Utils.local_of(s_param))(Utils.origen),
      IdleToken(Utils.thiz)(Utils.origen),
    ))

    val body: Statement[N] =
      Block(Seq[Statement[N]](
        Assign(Utils.deref_of(s.get), Utils.local_of(s_param))(Utils.blame)(
          Utils.origen
        )
      ))(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(requires, ensures),
      Seq(),
      Seq(s_param),
      Some(body),
    )(Utils.blame)(Utils.origen)
  }

  private def create_runMethod(
      transformer: StatementTransformer[O, N]
  ): RunMethod[N] = ???
}
case object Timer {
  def of[O, N](
      variable: Option[CLocal[O]],
      invocation: CInvocation[O],
      decls: Seq[CFunctionDefinition[O]],
  ): Timer[O, N] = {
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
      Utils.resolve_integer(reload_arg, "timer reload") != 0,
      Utils.resolve_integer(priority_arg, "timer priority"),
    )
  }
}
