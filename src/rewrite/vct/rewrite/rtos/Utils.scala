package vct.rewrite.rtos

import vct.col.ast._
import vct.col.origin.{LabelContext, Origin, PreferredName}
import vct.col.ref.{DirectRef, Ref}
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

  def resolve_integer[O](expr: Expr[O], meaning: String): Int =
    try_expr_to_int(expr).getOrElse(
      throw new IllegalArgumentException(
        "Could not resolve " + meaning + expr.toInlineString
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

  def resolve_freertos_constructs[O, N, T <: FreeRTOSConstruct[O, N]](
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

  private def get_applicable_name(applicable: Expr[_]): String =
    applicable match {
      case CLocal(name) => name
      case _ =>
        throw new IllegalArgumentException(
          "Applicable " + applicable.toInlineString +
            " has unexpected node type!"
        )
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
  def fold_or[N](vals: Seq[Expr[N]]): Expr[N] =
    vals.reduce((e1, e2) => Or(e1, e2)(origen))

  def predicate_apply[N](
      obj: Expr[N],
      ref: Ref[N, InstancePredicate[N]],
      args: Seq[Expr[N]],
  ): Expr[N] =
    PredicateApplyExpr(InstancePredicateApply(obj, ref, args)(origen))(origen)

  def old[N](expr: Expr[N]): Old[N] = Old(expr, None)(origen)(origen)

  def size[N](f: InstanceField[N]): Size[N] = Size(deref_of(f))(origen)

  def subscript[N](f: InstanceField[N], index: Int): SeqSubscript[N] =
    SeqSubscript(deref_of(f), int_val(index))(origen)(origen)

  def subscript_expr[N](f: InstanceField[N], index: Expr[N]): SeqSubscript[N] =
    SeqSubscript(deref_of(f), index)(origen)(origen)

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
    )(origen)(origen)

  def to_loop_invariant[N](expr: Expr[N]): LoopContract[N] =
    LoopInvariant(expr, None)(origen)(origen)

  def invoke[N](
      method: Ref[N, InstanceMethod[N]],
      args: Seq[Expr[N]],
      obj: Option[Expr[N]] = None,
  ): MethodInvocation[N] =
    obj match {
      case Some(o) =>
        MethodInvocation(o, method, args, Seq(), Seq(), Seq(), Seq())(origen)(
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
        InvokeMethod(o, method, args, Seq(), Seq(), Seq(), Seq())(origen)(
          origen
        )
      case _ =>
        InvokeMethod(thiz, method, args, Seq(), Seq(), Seq(), Seq())(origen)(
          origen
        )
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
        Deref(o, new DirectRef[N, InstanceField[N]](f))(origen)(origen)
      case _ =>
        Deref(thiz, new DirectRef[N, InstanceField[N]](f))(origen)(origen)
    }

  def deref_ref[N](ref: Ref[N, InstanceField[N]], obj: Expr[N]): Deref[N] =
    Deref(obj, ref)(origen)(origen)

  def local_of[N](v: Variable[N]): Local[N] =
    Local(new DirectRef[N, Variable[N]](v))(origen)

  def origen(name: String): Origin =
    origen.withContent(PreferredName(Seq(name)))
  def origen: Origin = Origin(Seq(LabelContext("FreeRTOS")))
}
