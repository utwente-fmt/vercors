package vct.col.util

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.Rewriter

import scala.reflect.ClassTag

object AstBuildHelpers {
  implicit class ExprBuildHelpers(left: Expr) {
    def +(right: Expr)(implicit origin: Origin): Plus = Plus(left, right)
    def -(right: Expr)(implicit origin: Origin): Minus = Minus(left, right)
    def *(right: Expr)(implicit origin: Origin): Mult = Mult(left, right)
    def /(right: Expr)(implicit origin: Origin, blame: Blame[DivByZero]): FloorDiv = FloorDiv(left, right)(blame)
    def /:(right: Expr)(implicit origin: Origin, blame: Blame[DivByZero]): Div = Div(left, right)(blame)
    def %(right: Expr)(implicit origin: Origin, blame: Blame[DivByZero]): Mod = Mod(left, right)(blame)

    def ===(right: Expr)(implicit origin: Origin): Eq = Eq(left, right)
    def !==(right: Expr)(implicit origin: Origin): Neq = Neq(left, right)
    def <(right: Expr)(implicit origin: Origin): Less = Less(left, right)
    def >(right: Expr)(implicit origin: Origin): Greater = Greater(left, right)
    def <=(right: Expr)(implicit origin: Origin): LessEq = LessEq(left, right)
    def >=(right: Expr)(implicit origin: Origin): GreaterEq = GreaterEq(left, right)

    def &&(right: Expr)(implicit origin: Origin): And = And(left, right)
    def ||(right: Expr)(implicit origin: Origin): Or = Or(left, right)
    def &*(right: Expr)(implicit origin: Origin): Star = Star(left, right)

    def ==>(right: Expr)(implicit origin: Origin): Implies = Implies(left, right)

    def ~>(field: SilverField)(implicit blame: Blame[InsufficientPermission], origin: Origin): SilverDeref = SilverDeref(left, new DirectRef(field))(blame)

    def @@(index: Expr)(implicit blame: Blame[SeqBoundFailure], origin: Origin): SeqSubscript = SeqSubscript(left, index)(blame)
  }

  implicit class VarBuildHelpers(left: Variable) {
    def get(implicit origin: Origin): Local = Local(new DirectRef(left))
    def <~(right: Expr)(implicit origin: Origin): SilverLocalAssign = SilverLocalAssign(new DirectRef(left), right)
  }

  implicit class FieldBuildHelpers(left: SilverDeref) {
    def <~(right: Expr)(implicit blame: Blame[SilverAssignFailed], origin: Origin): SilverFieldAssign = SilverFieldAssign(left.obj, left.field, right)(blame)
  }

  implicit class ApplicableBuildHelpers(applicable: Applicable)(implicit rewriter: AbstractRewriter) {
    def rewrite(args: Seq[Variable] = rewriter.collectInScope(rewriter.variableScopes) { applicable.args.foreach(rewriter.dispatch) },
               ): Applicable = applicable match {
      case inlineable: InlineableApplicable =>
        new InlineableApplicableBuildHelpers(inlineable).rewrite(args = args)
      case function: ADTFunction =>
        new RewriteADTFunction(function).rewrite(args = args)
      case process: ModelProcess =>
        new RewriteModelProcess(process).rewrite(args = args)
      case action: ModelAction =>
        new RewriteModelAction(action).rewrite(args = args)
    }
  }

  implicit class InlineableApplicableBuildHelpers(inlineable: InlineableApplicable)(implicit rewriter: AbstractRewriter) {
    def rewrite(args: Seq[Variable] = rewriter.collectInScope(rewriter.variableScopes) { inlineable.args.foreach(rewriter.dispatch) },
                inline: Boolean = inlineable.inline,
               ): InlineableApplicable = inlineable match {
      case pred: AbstractPredicate =>
        new PredicateBuildHelpers(pred).rewrite(args = args, inline = inline)
      case contracted: ContractApplicable =>
        new ContractApplicableBuildHelpers(contracted).rewrite(args = args, inline = inline)
    }
  }

  implicit class ContractApplicableBuildHelpers(contracted: ContractApplicable)(implicit rewriter: AbstractRewriter) {
    def rewrite(args: Seq[Variable] = rewriter.collectInScope(rewriter.variableScopes) { contracted.args.foreach(rewriter.dispatch) },
                returnType: Type = rewriter.dispatch(contracted.returnType),
                contract: ApplicableContract = rewriter.dispatch(contracted.contract),
                typeArgs: Seq[Variable] = rewriter.collectInScope(rewriter.variableScopes) { contracted.typeArgs.foreach(rewriter.dispatch) },
                inline: Boolean = contracted.inline,
               ): ContractApplicable = contracted match {
      case function: Function =>
        new RewriteFunction(function).rewrite(args = args, returnType = returnType, inline = inline, contract = contract, typeArgs = typeArgs)
      case function: InstanceFunction =>
        new RewriteInstanceFunction(function).rewrite(args = args, returnType = returnType, inline = inline, contract = contract, typeArgs = typeArgs)
      case method: AbstractMethod =>
        new MethodBuildHelpers(method).rewrite(args = args, returnType = returnType, inline = inline, contract = contract, typeArgs = typeArgs)
    }
  }

  implicit class MethodBuildHelpers(method: AbstractMethod)(implicit rewriter: AbstractRewriter) {
    def rewrite(args: Seq[Variable] = rewriter.collectInScope(rewriter.variableScopes) { method.args.foreach(rewriter.dispatch) },
                returnType: Type = rewriter.dispatch(method.returnType),
                outArgs: Seq[Variable] = rewriter.collectInScope(rewriter.variableScopes) { method.outArgs.foreach(rewriter.dispatch) },
                body: Option[Statement] = method.body.map(rewriter.dispatch),
                contract: ApplicableContract = rewriter.dispatch(method.contract),
                typeArgs: Seq[Variable] = rewriter.collectInScope(rewriter.variableScopes) { method.typeArgs.foreach(rewriter.dispatch) },
                inline: Boolean = method.inline,
                pure: Boolean = method.pure,
               ): AbstractMethod = method match {
      case procedure: Procedure =>
        new RewriteProcedure(procedure).rewrite(args = args, returnType = returnType, body = body, inline = inline, contract = contract, typeArgs = typeArgs, outArgs = outArgs, pure = pure)
      case method: InstanceMethod =>
        new RewriteInstanceMethod(method).rewrite(args = args, returnType = returnType, body = body, inline = inline, contract = contract, typeArgs = typeArgs, outArgs = outArgs, pure = pure)
    }
  }

  implicit class FunctionBuildHelpers(function: AbstractFunction)(implicit rewriter: AbstractRewriter) {
    def rewrite(args: Seq[Variable] = rewriter.collectInScope(rewriter.variableScopes) { function.args.foreach(rewriter.dispatch) },
                returnType: Type = rewriter.dispatch(function.returnType),
                body: Option[Expr] = function.body.map(rewriter.dispatch),
                contract: ApplicableContract = rewriter.dispatch(function.contract),
                typeArgs: Seq[Variable] = rewriter.collectInScope(rewriter.variableScopes) { function.typeArgs.foreach(rewriter.dispatch) },
                inline: Boolean = function.inline,
               ): ContractApplicable = function match {
      case function: Function =>
        new RewriteFunction(function).rewrite(args = args, returnType = returnType, body = body, inline = inline, contract = contract, typeArgs = typeArgs)
      case function: InstanceFunction =>
        new RewriteInstanceFunction(function).rewrite(args = args, returnType = returnType, body = body, inline = inline, contract = contract, typeArgs = typeArgs)
    }
  }

  implicit class PredicateBuildHelpers(predicate: AbstractPredicate)(implicit rewriter: AbstractRewriter) {
    def rewrite(args: Seq[Variable] = rewriter.collectInScope(rewriter.variableScopes) { predicate.args.foreach(rewriter.dispatch) },
                inline: Boolean = predicate.inline,
                threadLocal: Boolean = predicate.threadLocal,
               ): AbstractPredicate = predicate match {
      case predicate: Predicate =>
        new RewritePredicate(predicate).rewrite(args = args, inline = inline, threadLocal = threadLocal)
      case predicate: InstancePredicate =>
        new RewriteInstancePredicate(predicate).rewrite(args = args, inline = inline, threadLocal = threadLocal)
    }
  }

  implicit class ApplyBuildHelpers(apply: Apply)(implicit rewriter: AbstractRewriter) {
    def rewrite(args: Seq[Expr] = apply.args.map(rewriter.dispatch)): Apply = apply match {
      case inv: PredicateApply =>
        new RewritePredicateApply(inv).rewrite(args = args)
      case inv: InstancePredicateApply =>
        new RewriteInstancePredicateApply(inv).rewrite(args = args)
      case inv: ADTFunctionInvocation =>
        new RewriteADTFunctionInvocation(inv).rewrite(args = args)
      case inv: ProcedureInvocation =>
        new RewriteProcedureInvocation(inv).rewrite(args = args)
      case inv: FunctionInvocation =>
        new RewriteFunctionInvocation(inv).rewrite(args = args)
      case inv: MethodInvocation =>
        new RewriteMethodInvocation(inv).rewrite(args = args)
      case inv: InstanceFunctionInvocation =>
        new RewriteInstanceFunctionInvocation(inv).rewrite(args = args)
    }
  }

  private case object ConstOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def messageInContext(message: String): String = s"[At generated constant]: $message"
  }

  val tt: Constant.BooleanValue = Constant.BooleanValue(true)(ConstOrigin)
  val ff: Constant.BooleanValue = Constant.BooleanValue(false)(ConstOrigin)

  def const(i: Int)(implicit o: Origin): Constant.IntegerValue =
    Constant.IntegerValue(i)

  def contract(requires: Expr = tt, ensures: Expr = tt, contextEverywhere: Expr = tt,
               signals: Seq[SignalsClause] = Nil, givenArgs: Seq[Variable] = Nil, yieldsArgs: Seq[Variable] = Nil)
              (implicit o: Origin): ApplicableContract =
    ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs)

  def procedure(blame: Blame[PostconditionFailed],
                returnType: Type = TVoid(),
                args: Seq[Variable] = Nil, outArgs: Seq[Variable] = Nil, typeArgs: Seq[Variable] = Nil,
                body: Option[Statement] = None,
                requires: Expr = tt, ensures: Expr = tt,
                contextEverywhere: Expr = tt,
                signals: Seq[SignalsClause] = Nil,
                givenArgs: Seq[Variable] = Nil, yieldsArgs: Seq[Variable] = Nil,
                inline: Boolean = false, pure: Boolean = false)
               (implicit o: Origin): Procedure =
    new Procedure(returnType, args, outArgs, typeArgs, body,
      ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs),
      inline, pure)(blame)

  def function(blame: Blame[PostconditionFailed],
               returnType: Type = TVoid(),
               args: Seq[Variable] = Nil, typeArgs: Seq[Variable] = Nil,
               body: Option[Expr] = None,
               requires: Expr = tt, ensures: Expr = tt, contextEverywhere: Expr = tt,
               signals: Seq[SignalsClause] = Nil, givenArgs: Seq[Variable] = Nil, yieldsArgs: Seq[Variable] = Nil,
               inline: Boolean = false)(implicit o: Origin): Function =
    new Function(returnType, args, typeArgs, body,
      ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs),
      inline)(blame)

  case object GeneratedQuantifier extends Origin {
    override def preferredName: String = "i"
    override def messageInContext(message: String): String =
      s"[At generated quantifier]: $message"
  }

  def starall(t: Type,
              body: Local => Expr,
              triggers: Local => Seq[Seq[Expr]] = _ => Nil,
             ): Starall = {
    implicit val o: Origin = GeneratedQuantifier
    val i_var = new Variable(TInt())
    val i = Local(i_var.ref)
    Starall(
      bindings = Seq(i_var),
      triggers = triggers(i),
      body = body(i),
    )
  }

  def forall(t: Type,
             body: Local => Expr,
             triggers: Local => Seq[Seq[Expr]] = _ => Nil,
            ): Forall = {
    implicit val o: Origin = GeneratedQuantifier
    val i_var = new Variable(TInt())
    val i = Local(i_var.ref)
    Forall(
      bindings = Seq(i_var),
      triggers = triggers(i),
      body = body(i),
    )
  }

  def assignField(obj: Expr, field: Ref[InstanceField], value: Expr)(implicit o: Origin): Assign =
    Assign(Deref(obj, field)(DerefAssignTarget), value)

  def fieldPerm(obj: Expr, field: Ref[InstanceField], amount: Expr)(implicit o: Origin): Perm =
    Perm(Deref(obj, field)(DerefPerm), amount)

  def arrayPerm(arr: Expr, index: Expr, amount: Expr)(implicit o: Origin): Perm =
    Perm(ArraySubscript(arr, index)(ArrayPerm), amount)
}
