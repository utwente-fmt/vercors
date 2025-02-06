package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.DirectRef
import vct.rewrite.rtos.{ObjectInfo, Transformer, Utils}

case class ISR[O, N](isr: CFunctionDefinition[O]) extends FreeRTOSConstruct[O, N] {

  private def class_name: String = "ISR_" + Utils.get_declarator_name(isr.declarator)

  private def instance_name: String = "isr_" + Utils.get_declarator_name(isr.declarator)

  override def convert(col_ir: Transformer[O, N], idx: Int): ObjectInfo[O, N] = {
    val cls: Class[N] = transform(class_name)
    val tcls: Type[N] = TByReferenceClass(new DirectRef[N, Class[N]](cls), Seq())(Utils.origen)

    val field: InstanceField[N] = new InstanceField(tcls, Seq())(Utils.origen(instance_name))

    ObjectInfo(
      None,
      field,
      cls,
      Seq(),
      Utils.fold_star(Seq[Expr[N]](
        Perm(Utils.loc_of(field), Utils.read)(Utils.origen),
        Neq(Utils.deref_of(field), Utils.nul)(Utils.origen),
        Committed(Utils.deref_of(field))(Utils.origen)(Utils.origen),
      )),
      Some(Star(
        Perm(Utils.loc_of(field), Utils.read)(Utils.origen),
        Neq(Utils.deref_of(field), Utils.nul)(Utils.origen),
      )(Utils.origen)),
      None,
      None,
      None,
      None,
      launch = true,
    )
  }

  def transform(name: String): Class[N] = ???
}
case object ISR {
  def of[O, N](
             invocation: CInvocation[O],
             decls: Seq[CFunctionDefinition[O]],
           ): ISR[O, N] = {
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