package viper.api

import hre.lang.HREExitException
import hre.lang.System.Warning
import vct.col.{ast => col}
import viper.silver.{ast => silver}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ColToSilver {
  def transform(program: col.Program): silver.Program =
    ColToSilver(program).transform()
}

case class ColToSilver(program: col.Program) {
  val domains: ArrayBuffer[silver.Domain] = ArrayBuffer()
  val fields: mutable.Map[col.SilverField, silver.Field] = mutable.Map()
  val functions: ArrayBuffer[silver.Function] = ArrayBuffer()
  val predicates: ArrayBuffer[silver.Predicate] = ArrayBuffer()
  val methods: ArrayBuffer[silver.Method] = ArrayBuffer()

  val nameStack: mutable.Stack[mutable.Map[col.Declaration, String]] = mutable.Stack()
  var names: mutable.Map[col.Declaration, String] = mutable.Map()

  def ??(node: col.Node): Nothing = {
    Warning("Node not supported: %s", node)
    throw new HREExitException(1)
  }

  def push(): Unit = nameStack.push(names.clone())
  def pop(): Unit = names = nameStack.pop()

  /**
   * Give the declaration a silver-appropriate name that is as close as possible to the preferred name
   */
  def name(decl: col.Declaration): String =
    if(names.contains(decl)) {
      ???
    } else {
      var name = decl.o.preferredName
      while(names.values.exists(_ == name) || silver.utility.Consistency.reservedNames.contains(name)) {
        name += "$"
      }
      names(decl) = name
      name
    }

  /**
   * Evaluate f within a new scope
   */
  def scoped[T](f: => T): T = {
    push()
    val result = f
    pop()
    result
  }

  /**
   * Name decl in the current scope, then evaluate f within a new scope
   */
  def scoped[T](decl: col.Declaration)(f: => T): T = {
    name(decl)
    scoped(f)
  }

  /**
   * Retrieve the name for this reference
   */
  def ref(r: col.Ref[_ <: col.Declaration]): String = ref(r.decl)

  /**
   * Retrieve the name for this declaration
   */
  def ref(decl: col.Declaration): String =
    if(names.contains(decl)) {
      names(decl)
    } else {
      ???
    }

  def transform(): silver.Program = {
    program.declarations.foreach(collect)
    silver.Program(domains.toSeq, fields.values.toSeq, functions.toSeq, predicates.toSeq, methods.toSeq, extensions=Seq())()
  }

  def collect(decl: col.GlobalDeclaration): Unit = decl match {
    case field: col.SilverField =>
      fields(field) = silver.Field(name(field), typ(field.t))(info=NodeInfo(field))
    case rule: col.SimplificationRule =>
      ??(rule)
    case function: col.Function =>
      scoped(function) {
        functions += silver.Function(
          ref(function),
          function.args.map(variable),
          typ(function.returnType),
          Seq(exp(function.contract.requires)),
          Seq(exp(function.contract.ensures)),
          function.body.map(exp),
        )(info=NodeInfo(function))
      }
    case procedure: col.Procedure if procedure.returnType == col.TVoid() =>
      scoped(procedure) {
        val labelDecls = procedure.body.toSeq.flatMap(_.transSubnodes.collect {
          case l: col.LabelDecl => silver.Label(name(l), Seq())(info=NodeInfo(l))
        })
        methods += silver.Method(
          ref(procedure),
          procedure.args.map(variable),
          procedure.outArgs.map(variable),
          Seq(exp(procedure.contract.requires)),
          Seq(exp(procedure.contract.ensures)),
          procedure.body.map(body => silver.Seqn(Seq(block(body)), labelDecls)(info=NodeInfo(body)))
        )(info=NodeInfo(procedure))
      }
    case predicate: col.Predicate =>
      scoped(predicate) {
        predicates += silver.Predicate(
          ref(predicate),
          predicate.args.map(variable),
          predicate.body.map(exp)
        )(info=NodeInfo(predicate))
      }
    case clazz: col.Class =>
      ??(clazz)
  }

  def variable(v: col.Variable): silver.LocalVarDecl =
    silver.LocalVarDecl(name(v), typ(v.t))(info=NodeInfo(v))

  def typ(t: col.Type): silver.Type = t match {
    case col.TBool() => silver.Bool
    case col.TInt() => silver.Int
    case col.TRational() => silver.Perm
    case col.TRef() => silver.Ref
    case col.TSeq(element) => silver.SeqType(typ(element))
    case col.TSet(element) => silver.SetType(typ(element))
    case col.TBag(element) => silver.MultisetType(typ(element))
    case other => ??(other)
  }

  def exp(e: col.Expr): silver.Exp = e match {
    case bool: col.Constant.BooleanValue => silver.BoolLit(bool.value)(info=NodeInfo(e))
    case int: col.Constant.IntegerValue => silver.IntLit(int.value)(info=NodeInfo(e))

    case col.NoPerm() => silver.NoPerm()(info=NodeInfo(e))
    case col.ReadPerm() => silver.WildcardPerm()(info=NodeInfo(e))
    case col.WritePerm() => silver.FullPerm()(info=NodeInfo(e))

    case col.Size(obj) => silver.SeqLength(exp(obj))(info=NodeInfo(e))

    case col.Exists(bindings, triggers, body) =>
      scoped { silver.Exists(bindings.map(variable), triggers.map(trigger), exp(body))(info=NodeInfo(e)) }
    case col.Forall(bindings, triggers, body) =>
      scoped { silver.Forall(bindings.map(variable), triggers.map(trigger), exp(body))(info=NodeInfo(e)) }
    case col.Starall(bindings, triggers, body) =>
      scoped { silver.Forall(bindings.map(variable), triggers.map(trigger), exp(body))(info=NodeInfo(e)) }
    case col.Let(binding, value, main) =>
      scoped { silver.Let(variable(binding), exp(value), exp(main))(info=NodeInfo(e)) }
    case col.Not(arg) => silver.Not(exp(arg))(info=NodeInfo(e))
    case col.And(left, right) => silver.And(exp(left), exp(right))(info=NodeInfo(e))
    case col.Star(left, right) => silver.And(exp(left), exp(right))(info=NodeInfo(e))
    case col.Implies(left, right) => silver.Implies(exp(left), exp(right))(info=NodeInfo(e))
    case col.Or(left, right) => silver.Or(exp(left), exp(right))(info=NodeInfo(e))

    case resource@col.SilverPerm(obj, field, perm) =>
      val permissionValue = exp(perm)
      permissionValue.info.asInstanceOf[NodeInfo[_]].permissionValuePermissionNode = Some(resource)
      silver.FieldAccessPredicate(silver.FieldAccess(exp(obj), fields(field.decl))(info=NodeInfo(e)), permissionValue)(info=NodeInfo(e))
    case resource@col.SilverPredPerm(access) =>
      val silver = pred(access)
      silver.perm.info.asInstanceOf[NodeInfo[_]].permissionValuePermissionNode = Some(resource)
      silver
    case col.SilverCurPredPerm(p, args) => silver.CurrentPerm(silver.PredicateAccess(args.map(exp), ref(p))(info=NodeInfo(e)))(info=NodeInfo(e))
    case col.SilverCurFieldPerm(obj, field) => silver.CurrentPerm(silver.FieldAccess(exp(obj), fields(field.decl))(info=NodeInfo(e)))(info=NodeInfo(e))
    case col.Local(v) => silver.LocalVar(ref(v), typ(v.decl.t))(info=NodeInfo(e))
    case col.SilverDeref(obj, ref) => silver.FieldAccess(exp(obj), fields(ref.decl))(info=NodeInfo(e))
    case col.FunctionInvocation(f, args, Nil) =>
      silver.FuncApp(ref(f), args.map(exp))(silver.NoPosition, silver.NoInfo, typ(f.decl.returnType), silver.NoTrafos)
    case col.SilverUnfolding(p, body) => silver.Unfolding(pred(p), exp(body))(info=NodeInfo(e))
    case col.Select(condition, whenTrue, whenFalse) => silver.CondExp(exp(condition), exp(whenTrue), exp(whenFalse))(info=NodeInfo(e))
    case col.Old(expr, None) => silver.Old(exp(expr))(info=NodeInfo(e))
    case col.Old(expr, Some(lbl)) => silver.LabelledOld(exp(expr), ref(lbl))(info=NodeInfo(e))

    case col.UMinus(arg) => silver.Minus(exp(arg))(info=NodeInfo(e))
    case col.Plus(left, right) => silver.Add(exp(left), exp(right))(info=NodeInfo(e))
    case col.Minus(left, right) => silver.Sub(exp(left), exp(right))(info=NodeInfo(e))
    case col.Mult(left, right) => silver.Mul(exp(left), exp(right))(info=NodeInfo(e))
    case col.Div(left, right) => silver.PermDiv(exp(left), exp(right))(info=NodeInfo(e))
    case col.Mod(left, right) => silver.Mod(exp(left), exp(right))(info=NodeInfo(e))
    case col.FloorDiv(left, right) => silver.Div(exp(left), exp(right))(info=NodeInfo(e))

    case col.Eq(left, right) => silver.EqCmp(exp(left), exp(right))(info=NodeInfo(e))
    case col.Neq(left, right) => silver.NeCmp(exp(left), exp(right))(info=NodeInfo(e))
    case col.Greater(left, right) => silver.GtCmp(exp(left), exp(right))(info=NodeInfo(e))
    case col.Less(left, right) => silver.LtCmp(exp(left), exp(right))(info=NodeInfo(e))
    case col.GreaterEq(left, right) => silver.GeCmp(exp(left), exp(right))(info=NodeInfo(e))
    case col.LessEq(left, right) => silver.LeCmp(exp(left), exp(right))(info=NodeInfo(e))
    case col.SubSet(left, right) => silver.AnySetSubset(exp(left), exp(right))(info=NodeInfo(e))

    case subscript@col.SeqSubscript(seq, index) =>
      val silverIndex = exp(index)
      silverIndex.info.asInstanceOf[NodeInfo[_]].seqIndexSubscriptNode = Some(subscript)
      silver.SeqIndex(exp(seq), silverIndex)(info=NodeInfo(e))
    case col.Range(from, to) => silver.RangeSeq(exp(from), exp(to))(info=NodeInfo(e))
    case col.Drop(xs, count) => silver.SeqDrop(exp(xs), exp(count))(info=NodeInfo(e))
    case col.Take(xs, count) => silver.SeqTake(exp(xs), exp(count))(info=NodeInfo(e))
    case col.SeqUpdate(xs, i, x) => silver.SeqUpdate(exp(xs), exp(i), exp(x))(info=NodeInfo(e))
    case col.Concat(xs, ys) => silver.SeqAppend(exp(xs), exp(ys))(info=NodeInfo(e))
    case col.SetMember(x, xs) => silver.AnySetContains(exp(x), exp(xs))(info=NodeInfo(e))
    case col.SeqMember(x, xs) => silver.SeqContains(exp(x), exp(xs))(info=NodeInfo(e))
    case col.BagMemberCount(x, xs) => silver.AnySetContains(exp(x), exp(xs))(info=NodeInfo(e))
    case other => ??(other)
  }

  def trigger(patterns: Seq[col.Expr]): silver.Trigger =
    silver.Trigger(patterns.map(exp))()

  def pred(p: col.SilverPredicateAccess): silver.PredicateAccessPredicate =
    silver.PredicateAccessPredicate(silver.PredicateAccess(p.args.map(exp), ref(p.ref))(info=NodeInfo(p)), exp(p.perm))(info=NodeInfo(p))

  def stat(s: col.Statement): silver.Stmt = s match {
    case col.Eval(inv@col.ProcedureInvocation(method, args, outArgs, Nil)) =>
      silver.MethodCall(ref(method), args.map(exp), outArgs.map(arg => silver.LocalVar(ref(arg), typ(arg.decl.t))()))(
        silver.NoPosition, NodeInfo(inv), silver.NoTrafos)
    case col.SilverFieldAssign(obj, field, value) =>
      silver.FieldAssign(silver.FieldAccess(exp(obj), fields(field.decl))(info=NodeInfo(s)), exp(value))(info=NodeInfo(s))
    case col.SilverLocalAssign(v, value) =>
      silver.LocalVarAssign(silver.LocalVar(ref(v), typ(v.decl.t))(info=NodeInfo(s)), exp(value))(info=NodeInfo(s))
    case col.Block(statements) => silver.Seqn(statements.map(stat), Seq())(info=NodeInfo(s))
    case col.Scope(locals, body) =>
      val silverLocals = locals.map(variable)
      silver.Seqn(Seq(stat(body)), silverLocals)(info=NodeInfo(s))
    case col.SilverIf(cond, whenTrue, whenFalse) => silver.If(exp(cond), block(whenTrue), block(whenFalse))(info=NodeInfo(s))
    case col.SilverWhile(cond, inv, body) => silver.While(exp(cond), Seq(exp(inv)), block(body))(info=NodeInfo(s))
    case col.Label(decl) => silver.Label(ref(decl), Seq())(info=NodeInfo(s))
    case col.Goto(lbl) => silver.Goto(ref(lbl))(info=NodeInfo(s))
    case col.Exhale(res) => silver.Exhale(exp(res))(info=NodeInfo(s))
    case col.Assert(assn) => silver.Assert(exp(assn))(info=NodeInfo(s))
    case col.Inhale(res) => silver.Inhale(exp(res))(info=NodeInfo(s))
    case col.Assume(assn) => silver.Assume(exp(assn))(info=NodeInfo(s))
    case col.SilverFold(p) => silver.Fold(pred(p))(info=NodeInfo(s))
    case col.SilverUnfold(p) => silver.Unfold(pred(p))(info=NodeInfo(s))
    case col.SilverNewRef(v, fs) => silver.NewStmt(silver.LocalVar(ref(v), typ(v.decl.t))(), fs.map(ref => fields(ref.decl)))(info=NodeInfo(s))
    case other => ??(other)
  }

  def block(s: col.Statement): silver.Seqn = stat(s) match {
    case seqn: silver.Seqn => seqn
    case other => silver.Seqn(Seq(other), Seq())(info=NodeInfo(s))
  }
}
