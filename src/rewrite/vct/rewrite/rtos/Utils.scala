package vct.rewrite.rtos

import vct.col.ast._
import vct.col.origin.{LabelContext, Origin, PanicBlame, PreferredName}
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers
import vct.col.util.AstBuildHelpers.{ff, tt}
import vct.rewrite.rtos.freertosir.FreeRTOSConstruct

import scala.annotation.tailrec

case object Utils {
  private def try_expr_to_int(expr: Expr[_]): Option[Int] =
    expr match {
      case IntegerValue(i) => Some(i.intValue)
      case CIntegerValue(i) => Some(i.intValue)
      case UMinus(arg) =>
        try_expr_to_int(arg) match {
          case Some(i) => Some(-i)
          case None => None
        }
      case BitNot(arg) =>
        try_expr_to_int(arg) match {
          case Some(i) => Some(~i)
          case None => None
        }
      case Plus(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 + i2)
      case AmbiguousPlus(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 + i2)
      case Minus(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 - i2)
      case AmbiguousMinus(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 - i2)
      case AmbiguousMult(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 * i2)
      case Mult(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 * i2)
      case AmbiguousDiv(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 / i2)
      case AmbiguousTruncDiv(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 / i2)
      case FloorDiv(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 / i2)
      case AmbiguousMod(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 % i2)
      case AmbiguousTruncMod(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 % i2)
      case Mod(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 % i2)
      case Exp(left, right) =>
        resolve_operator(
          left,
          right,
          (i1, i2) => BigDecimal(i1).pow(i2).intValue,
        )
      case AmbiguousComputationalOr(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 | i2)
      case ComputationalOr(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 | i2)
      case AmbiguousComputationalAnd(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 & i2)
      case ComputationalAnd(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 & i2)
      case AmbiguousComputationalXor(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 ^ i2)
      case ComputationalXor(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 ^ i2)
      case BitAnd(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 & i2)
      case BitOr(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 | i2)
      case BitXor(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 ^ i2)
      case BitShl(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 << i2)
      case BitShr(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 >> i2)
      case BitUShr(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 >>> i2)
      case _ => None
    }

  private def resolve_operator(
      left: Expr[_],
      right: Expr[_],
      op: (Int, Int) => Int,
  ): Option[Int] =
    try_expr_to_int(left) match {
      case Some(i1) =>
        try_expr_to_int(right) match {
          case Some(i2) => Some(op(i1, i2))
          case None => None
        }
      case None => None
    }

  def creation_arg_assert[O](
      invocation: CInvocation[O],
      desired_arguments: Int,
      error_message: String,
  ): Unit =
    if (invocation.args.length != desired_arguments)
      throw new IllegalArgumentException(error_message)

  def exclude_isr[N](scheduler: Option[InstanceField[N]]): InstanceField[N] =
    scheduler.getOrElse(
      throw new IllegalArgumentException("ISR must not reference scheduler!")
    )

  def resolve_integer[O](expr: Expr[O], meaning: String): Int =
    try_expr_to_int(expr).getOrElse(
      throw new IllegalArgumentException(
        "Could not resolve " + meaning + " " + expr.toInlineString
      )
    )

  def resolve_function[O](
      invocation: CInvocation[O],
      decls: Seq[CFunctionDefinition[O]],
      meaning: String,
  ): CFunctionDefinition[O] =
    decls.find(f =>
      get_declarator_name(f.declarator)
        .equals(get_applicable_name(invocation.applicable))
    ).getOrElse(
      throw new IllegalArgumentException("Could not find " + meaning + "!")
    )

  def resolve_freertos_constructs[O <: Generation, T <: FreeRTOSConstruct[O]](
      stmts: Seq[Expr[O]],
      func_name: String,
      op: (Option[CLocal[O]], CInvocation[O]) => T,
  ): Seq[T] =
    stmts.collect {
      case PreAssignExpression(target, value) if (value match {
            case CInvocation(applicable, _, _, _) =>
              get_applicable_name(applicable).equals(func_name)
            case _ => false
          }) =>
        target match {
          case t: CLocal[O] => op(Some(t), value.asInstanceOf[CInvocation[O]])
          case _ => op(None, value.asInstanceOf[CInvocation[O]])
        }
      case inv @ CInvocation(applicable, _, _, _)
          if get_applicable_name(applicable).equals(func_name) =>
        op(None, inv)
    }

  @tailrec
  def get_declarator_name(declarator: CDeclarator[_]): String =
    declarator match {
      case CPointerDeclarator(_, inner) => get_declarator_name(inner)
      case CArrayDeclarator(_, _, inner) => get_declarator_name(inner)
      case CTypeExtensionDeclarator(_, inner) => get_declarator_name(inner)
      case CTypedFunctionDeclarator(_, _, inner) => get_declarator_name(inner)
      case CAnonymousFunctionDeclarator(_, inner) => get_declarator_name(inner)
      case CName(name) => name
    }

  def get_applicable_name(applicable: Expr[_]): String =
    applicable match {
      case CLocal(name) => name
      case _ =>
        throw new IllegalArgumentException(
          "Applicable " + applicable.toInlineString +
            " has unexpected node type!"
        )
    }

  def is_pure[O](specs: Seq[CDeclarationSpecifier[O]]): Boolean =
    specs.collectFirst { case p: CPure[O] => p }.nonEmpty

  def is_inline[O](specs: Seq[CDeclarationSpecifier[O]]): Boolean =
    specs.collectFirst { case i: CInline[O] => i }.nonEmpty

  def get_ctype[O <: Generation](
      specs: Seq[CDeclarationSpecifier[O]]
  ): Type[Rewritten[O]] = {
    specs.collectFirst {
      case _: CVoid[O] => tvoid[Rewritten[O]]
      case _: CChar[O] => tint[Rewritten[O]]
      case _: CShort[O] => tint[Rewritten[O]]
      case _: CInt[O] => tint[Rewritten[O]]
      case _: CLong[O] => tint[Rewritten[O]]
      case _: CBool[O] => tbool[Rewritten[O]]
      // TODO: case t: CTypedefName[O] => ???
    }.getOrElse(
      throw new IllegalArgumentException("Unsupported function return type!")
    )
  }

  def args_of[O](d: CDeclarator[O]): Seq[CParam[O]] =
    d match {
      case CTypedFunctionDeclarator(params, _, _) => params
      case CAnonymousFunctionDeclarator(params, _) if params.isEmpty => Seq()
    }

  def thiz[N]: AmbiguousThis[N] = AmbiguousThis()(origen)
  def nul[N]: Null[N] = Null()(origen)
  def read[N]: ReadPerm[N] = ReadPerm()(origen)
  def write[N]: WritePerm[N] = WritePerm()(origen)
  def result[N]: Expr[N] = AmbiguousResult()(origen)
  def seq_val[N](vals: Seq[Expr[N]]): LiteralSeq[N] =
    LiteralSeq(tint, vals)(origen)
  def int_val[N](value: Int): IntegerValue[N] =
    IntegerValue(BigInt(value))(origen)

  def skip[N]: Statement[N] = Block(Seq())(origen)

  def tvoid[N]: TVoid[N] = TVoid()(origen)
  def tint[N]: TInt[N] = TInt()(origen)
  def tbool[N]: TBool[N] = TBool()(origen)
  def tseqint[N]: TSeq[N] = TSeq(TInt()(origen))(origen)

  def fold_star[N](vals: Seq[Expr[N]]): Expr[N] =
    vals.reduce((e1, e2) => Star(e1, e2)(origen))
  def fold_and[N](vals: Seq[Expr[N]]): Expr[N] =
    vals.reduce((e1, e2) => And(e1, e2)(origen))

  def predicate_apply[N](
      obj: Expr[N],
      ref: Ref[N, InstancePredicate[N]],
      args: Seq[Expr[N]],
  ): Expr[N] =
    PredicateApplyExpr(InstancePredicateApply(obj, ref, args)(origen))(origen)

  def old[N](expr: Expr[N]): Old[N] = Old(expr, None)(blame)(origen)

  def size[N](f: InstanceField[N], obj: Option[Expr[N]] = None): Size[N] = Size(deref_of(f, obj))(origen)

  def subscript[N](f: InstanceField[N], index: Int): SeqSubscript[N] =
    SeqSubscript(deref_of(f), int_val(index))(blame)(origen)

  def subscript_expr[N](f: InstanceField[N], index: Expr[N]): SeqSubscript[N] =
    SeqSubscript(deref_of(f), index)(blame)(origen)

  def unchanged[N](expr: Expr[N]): Eq[N] = Eq(expr, old(expr))(origen)

  def single_var_forall[N](
      i: Variable[N],
      lower_incl: Expr[N],
      upper_excl: Expr[N],
      expr: Expr[N],
  ): Forall[N] =
    Forall(
      Seq(i),
      Seq(),
      Implies(
        And(
          LessEq(lower_incl, local_of(i))(origen),
          Less(local_of(i), upper_excl)(origen),
        )(origen),
        expr,
      )(origen),
    )(origen)

  def single_var_exists[N](
      i: Variable[N],
      lower_incl: Expr[N],
      upper_excl: Expr[N],
      expr: Expr[N],
  ): Exists[N] =
    Exists(
      Seq(i),
      Seq(),
      fold_and(Seq[Expr[N]](
        LessEq(lower_incl, local_of(i))(origen),
        Less(local_of(i), upper_excl)(origen),
        expr,
      )),
    )(origen)

  def to_app_contract[N](
      requires: Expr[N],
      ensures: Expr[N],
  ): ApplicableContract[N] =
    ApplicableContract(
      UnitAccountedPredicate(requires)(origen),
      UnitAccountedPredicate(ensures)(origen),
      tt,
      Seq(),
      Seq(),
      Seq(),
      None,
    )(blame)(origen)

  def contract_resolve[O](p: AccountedPredicate[O]): Expr[O] =
    AstBuildHelpers.unfoldPredicate(p).reduce((e1, e2) => Star(e1, e2)(origen))

  def to_loop_invariant[N](expr: Expr[N], decreases: Option[DecreasesClause[N]] = None): LoopContract[N] =
    LoopInvariant(expr, decreases)(blame)(origen)

  def invoke[N](
      method: Ref[N, InstanceMethod[N]],
      args: Seq[Expr[N]],
      obj: Option[Expr[N]] = None,
  ): MethodInvocation[N] =
    obj match {
      case Some(o) =>
        MethodInvocation(o, method, args, Seq(), Seq(), Seq(), Seq())(blame)(
          origen
        )
      case _ =>
        MethodInvocation(thiz, method, args, Seq(), Seq(), Seq(), Seq())(
          origen
        )(origen)
    }

  def stmt_invoke[N](
      method: Ref[N, InstanceMethod[N]],
      args: Seq[Expr[N]],
      obj: Option[Expr[N]] = None,
  ): InvokeMethod[N] =
    obj match {
      case Some(o) =>
        InvokeMethod(o, method, args, Seq(), Seq(), Seq(), Seq())(blame)(origen)
      case _ =>
        InvokeMethod(thiz, method, args, Seq(), Seq(), Seq(), Seq())(blame)(
          origen
        )
    }

  def update_scheduling_variable[N](
      f: => InstanceField[N],
      scheduler: InstanceField[N],
      idx: Expr[N],
      new_val: Expr[N],
  ): Assign[N] = {
    val deref: Deref[N] = deref_ref(new LazyRef(f), deref_of(scheduler))
    Assign(deref, SeqUpdate(deref, idx, new_val)(origen))(blame)(origen)
  }

  def scheduling_variable_entry[N](
      f: => InstanceField[N],
      scheduler: InstanceField[N],
      idx: Expr[N],
  ): SeqSubscript[N] = {
    val deref: Deref[N] = deref_ref(new LazyRef(f), deref_of(scheduler))
    SeqSubscript(deref, idx)(blame)(origen)
  }

  def task_wait[O <: Generation](
      col_ir: COLEncoder[O],
      scheduler: InstanceField[Rewritten[O]],
      invariant: Expr[Rewritten[O]],
      tid: Int,
      eid: Option[Int],
      timeout: Option[Expr[Rewritten[O]]],
  ): Statement[Rewritten[O]] = {
    var block: Seq[Statement[Rewritten[O]]] = Seq(
      Loop(
        skip,
        Neq(
          scheduling_variable_entry(
            col_ir.get_taskState,
            scheduler,
            int_val(tid),
          ),
          int_val(-2),
        )(origen),
        skip,
        to_loop_invariant(invariant),
        Block(Seq(
          Unlock(deref_of(scheduler))(blame)(origen),
          Lock(deref_of(scheduler))(blame)(origen),
        ))(origen),
      )(origen)
    )
    if (eid.nonEmpty) {
      block =
        Seq[Statement[Rewritten[O]]](
          update_scheduling_variable(
            col_ir.get_taskState,
            scheduler,
            int_val(tid),
            int_val(eid.get),
          ),
          update_scheduling_variable(
            col_ir.get_taskWaitTime,
            scheduler,
            int_val(tid),
            int_val(0),
          ),
        ) ++ block
      if (timeout.nonEmpty) {
        block =
          update_scheduling_variable(
            col_ir.get_eventState,
            scheduler,
            int_val(eid.get),
            timeout.get,
          ) +: block
      }
    }
    Block(block)(origen)
  }

  def loc_of[N](
      f: InstanceField[N],
      obj: Option[Expr[N]] = None,
  ): FieldLocation[N] =
    obj match {
      case Some(o) =>
        FieldLocation(o, new DirectRef[N, InstanceField[N]](f))(origen)
      case _ =>
        FieldLocation(thiz, new DirectRef[N, InstanceField[N]](f))(origen)
    }
  def deref_of[N](f: InstanceField[N], obj: Option[Expr[N]] = None): Deref[N] =
    obj match {
      case Some(o) =>
        Deref(o, new DirectRef[N, InstanceField[N]](f))(blame)(origen)
      case _ =>
        Deref(thiz, new DirectRef[N, InstanceField[N]](f))(blame)(origen)
    }

  def deref_ref[N](ref: Ref[N, InstanceField[N]], obj: Expr[N]): Deref[N] =
    Deref(obj, ref)(blame)(origen)

  def local_of[N](v: Variable[N]): Local[N] =
    Local(new DirectRef[N, Variable[N]](v))(origen)

  def origen(name: String): Origin =
    origen.withContent(PreferredName(Seq(name)))
  def origen: Origin = Origin(Seq(LabelContext("FreeRTOS")))
  def blame: PanicBlame = PanicBlame("Error from FreeRTOS encoding output")
}
