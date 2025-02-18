package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.{DirectRef, LazyRef}
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.rtos.{ObjectInfo, Transformer, COLEncoder, Utils}

case class ISR[O <: Generation](isr: CFunctionDefinition[O])
    extends FreeRTOSConstruct[O] {
  private var cls: Option[Class[N]] = None
  private def get_cls: Class[N] = cls.get

  private def class_name: String =
    "ISR_" + Utils.get_declarator_name(isr.declarator)

  private def instance_name: String =
    "isr_" + Utils.get_declarator_name(isr.declarator)

  override def convert(col_ir: COLEncoder[O], idx: Int): ObjectInfo[O] = {
    val tcls: Type[N] =
      TByReferenceClass(new LazyRef[N, Class[N]](get_cls), Seq())(Utils.origen)
    val field: InstanceField[N] =
      new InstanceField(tcls, Seq())(Utils.origen(instance_name))

    val transformer: Transformer[O] =
      new Transformer(col_ir, None, None, field, Seq())

    cls = Some(transform(transformer, class_name))

    col_ir.add_isr_lock(field)

    ObjectInfo(
      None,
      field,
      cls.get,
      Seq(),
      Utils.fold_star(Seq[Expr[N]](
        Perm(Utils.loc_of(field), Utils.read)(Utils.origen),
        Neq(Utils.deref_of(field), Utils.nul)(Utils.origen),
        Committed(Utils.deref_of(field))(Utils.blame)(Utils.origen),
      )),
      Some(
        Star(
          Perm(Utils.loc_of(field), Utils.read)(Utils.origen),
          Neq(Utils.deref_of(field), Utils.nul)(Utils.origen),
        )(Utils.origen)
      ),
      None,
      None,
      None,
      None,
      launch = true,
    )
  }

  def transform(transformer: Transformer[O], name: String): Class[N] = {
    val runMethod: RunMethod[N] = create_runMethod(transformer)

    val fields: Seq[(InstanceField[N], Option[Expr[N]])] =
      transformer.get_registered_isr_fields

    val isrPermissions: InstancePredicate[N] =
      new InstancePredicate(
        Seq(),
        Some(Utils.fold_star(
          fields.map(t => Perm(Utils.loc_of(t._1), Utils.write)(Utils.origen))
        )),
      )(Utils.origen("isrPermissions"))

    val isrConstructor: PVLConstructor[N] = create_constructor(fields)

    new ByReferenceClass(
      Seq(),
      fields.map(t => t._1) ++ Seq(isrPermissions, isrConstructor, runMethod) ++
        transformer.get_additional_methods,
      Seq(),
      Utils.predicate_apply(
        Utils.thiz,
        new DirectRef[N, InstancePredicate[N]](isrPermissions),
        Seq(),
      ),
    )(Utils.origen(name))
  }

  private def create_constructor(
      fields: Seq[(InstanceField[N], Option[Expr[N]])]
  ): PVLConstructor[N] = {
    val ensures: Expr[N] =
      Star(
        Committed(Utils.thiz)(Utils.blame)(Utils.origen),
        IdleToken(Utils.thiz)(Utils.origen),
      )(Utils.origen)

    val body: Statement[N] =
      Block(
        fields.filter(t => t._2.nonEmpty).map(t =>
          Assign(Utils.deref_of(t._1), t._2.get)(Utils.blame)(Utils.origen)
        ) ++ Seq[Statement[N]](Commit(Utils.thiz)(Utils.blame)(Utils.origen))
      )(Utils.origen)

    new PVLConstructor(
      Utils.to_app_contract(tt, ensures),
      Seq(),
      Seq(),
      Some(body),
    )(Utils.blame)(Utils.origen)
  }

  private def create_runMethod(transformer: Transformer[O]): RunMethod[N] = {
    val cond: Expr[N] = Committed(Utils.thiz)(Utils.blame)(Utils.origen)

    val loop_body: Statement[N] =
      Block(Seq[Statement[N]](
        Lock(Utils.thiz)(Utils.blame)(Utils.origen),
        transformer.dispatch(isr.body),
        Unlock(Utils.thiz)(Utils.blame)(Utils.origen),
      ))(Utils.origen)

    val body: Statement[N] =
      Block(Seq[Statement[N]](
        Loop(
          Utils.skip,
          tt,
          Utils.skip,
          Utils.to_loop_invariant(cond),
          loop_body,
        )(Utils.origen)
      ))(Utils.origen)

    new RunMethod(Some(body), Utils.to_app_contract(cond, cond))(Utils.blame)(
      Utils.origen
    )
  }
}
case object ISR {
  def of[O <: Generation](
      invocation: CInvocation[O],
      decls: Seq[CFunctionDefinition[O]],
  ): ISR[O] = {
    Utils.creation_arg_assert(
      invocation,
      1,
      "ISR creation has wrong number of arguments!",
    )

    val call_arg: CInvocation[O] = invocation.args.head
      .asInstanceOf[CInvocation[O]]

    ISR(Utils.resolve_function(call_arg, decls, "ISR handler method"))
  }
}
