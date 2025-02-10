package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.{ObjectInfo, StatementTransformer, Transformer, Utils}

case class Task[O, N](
    decl: Option[CLocal[O]],
    func: CFunctionDefinition[O],
    param: Expr[O],
    priority: Int,
) extends FreeRTOSConstruct[O, N] {
  private var s: InstanceField[N] = ???
  private var taskPerms: InstancePredicate[N] = ???
  private var cls: Class[N] = ???

  private def get_cls: Class[N] = cls

  private def class_name: String =
    "Task" + Utils.get_declarator_name(func.declarator)

  private def instance_name: String =
    "t" + Utils.get_declarator_name(func.declarator)

  override def convert(
      col_ir: Transformer[O, N],
      idx: Int,
  ): ObjectInfo[O, N] = {
    val tcls: Type[N] =
      TByReferenceClass(new LazyRef[N, Class[N]](get_cls), Seq())(Utils.origen)
    val field: InstanceField[N] =
      new InstanceField(tcls, Seq())(Utils.origen(instance_name))

    val tid: Int = col_ir.reserve_task_id

    cls = transform(
      new LazyRef(col_ir.get_scheduler),
      tid,
      col_ir,
      field,
      class_name,
    )

    ObjectInfo(
      decl,
      field,
      cls,
      Seq[Expr[N]](
        Utils.thiz
      ), // TODO: incorporate param or have it be a parameter?
      Utils.fold_star(Seq[Expr[N]](
        Perm(Utils.loc_of(field), Utils.read)(Utils.origen),
        Utils.predicate_apply(
          Utils.deref_of(field),
          new DirectRef[N, InstancePredicate[N]](taskPerms),
          Seq(),
        ),
        Eq(Utils.deref_of(s, Some(Utils.deref_of(field))), Utils.thiz)(
          Utils.origen
        ),
      )),
      Some(Utils.fold_star(Seq[Expr[N]](
        Perm(Utils.loc_of(field), Utils.read)(Utils.origen),
        Neq(Utils.deref_of(field), Utils.nul)(Utils.origen),
        Perm(Utils.loc_of(s, Some(Utils.deref_of(field))), Utils.read)(
          Utils.origen
        ),
        Eq(Utils.deref_of(s, Some(Utils.deref_of(field))), Utils.thiz)(
          Utils.origen
        ),
      ))),
      Some(tid),
      Some(priority),
      None,
      None,
      launch = true,
    )
  }

  def transform(
      scheduler_ref: Ref[N, Class[N]],
      tid: Int,
      col_ir: Transformer[O, N],
      field: InstanceField[N],
      name: String,
  ): Class[N] = {
    s =
      new InstanceField(TByValueClass(scheduler_ref, Seq()), Seq())(
        Utils.origen("s")
      )

    taskPerms =
      new InstancePredicate(
        Seq(),
        Some(
          Star(
            Perm(Utils.loc_of(s), Utils.read)(Utils.origen),
            Neq(Utils.deref_of(s), Utils.nul)(Utils.origen),
          )(Utils.origen)
        ),
      )(Utils.origen("taskPerms"))

    val perms: Ref[N, InstancePredicate[N]] =
      new DirectRef[N, InstancePredicate[N]](taskPerms)

    val transformer: StatementTransformer[O, N] =
      new StatementTransformer(col_ir, Some(tid), Some(s), field)

    val taskConstructor: PVLConstructor[N] = create_constructor(
      scheduler_ref,
      perms,
    )

    // TODO: Handle other generated methods
    val runMethod: RunMethod[N] = create_runMethod(transformer)

    new ByReferenceClass(
      Seq(),
      Seq(s, taskPerms, taskConstructor, runMethod),
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

    val requires: Expr[N] = Neq(Utils.deref_of(s), Utils.nul)(Utils.origen)

    val ensures: Expr[N] = Utils.fold_star(Seq[Expr[N]](
      Utils.predicate_apply(Utils.thiz, perms, Seq()),
      Eq(Utils.deref_of(s), Utils.local_of(s_param))(Utils.origen),
      IdleToken(Utils.thiz)(Utils.origen),
    ))

    val body: Statement[N] =
      Block(Seq[Statement[N]](
        Assign(Utils.deref_of(s), Utils.local_of(s_param))(Utils.blame)(
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
case object Task {
  def of[O, N](
      variable: Option[CLocal[O]],
      invocation: CInvocation[O],
      decls: Seq[CFunctionDefinition[O]],
  ): Task[O, N] = {
    Utils.creation_arg_assert(
      invocation,
      2,
      "Task creation has wrong number of arguments!",
    )

    val call_arg: CInvocation[O] = invocation.args.head
      .asInstanceOf[CInvocation[O]]
    val priority_arg: Expr[O] = invocation.args(1)

    Utils.creation_arg_assert(call_arg, 1, "No task parameters given!")

    Task(
      variable,
      Utils.resolve_function(call_arg, decls, "task method"),
      call_arg.args.head,
      Utils.resolve_integer(priority_arg, "task priority"),
    )
  }
}
