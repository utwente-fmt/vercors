package viper.api

import vct.col.origin.{Blame, Origin, SourceNameOrigin, VerificationFailure}
import vct.col.ref.UnresolvedRef
import vct.col.util.AstBuildHelpers._
import viper.silver.{ast => silver}
import vct.col.{ast => col}
import viper.silver.ast.{AbstractConcretePerm, AbstractDomainFuncApp, AbstractLocalVar, AccessPredicate, Add, And, AnySetBinExp, AnySetCardinality, AnySetContains, AnySetExp, AnySetIntersection, AnySetMinus, AnySetSubset, AnySetUnExp, AnySetUnion, Applying, BackendFuncApp, BinExp, BoolLit, CondExp, CurrentPerm, Div, DomainBinExp, DomainFuncApp, DomainOpExp, DomainUnExp, EmptyMap, EmptyMultiset, EmptySeq, EmptySet, EpsilonPerm, EqCmp, EqualityCmp, Exists, ExplicitMap, ExplicitMultiset, ExplicitSeq, ExplicitSet, ExtensionExp, FalseLit, FieldAccess, FieldAccessPredicate, ForPerm, Forall, ForbiddenInTrigger, FractionalPerm, FullPerm, FuncApp, FuncLikeApp, GeCmp, GtCmp, Implies, InhaleExhaleExp, IntLit, IntPermMul, LabelledOld, LeCmp, Let, Lhs, Literal, LocalVar, LocationAccess, LtCmp, MagicWand, MapCardinality, MapContains, MapDomain, MapExp, MapLookup, MapRange, MapUpdate, Maplet, Minus, Mod, Mul, MultisetExp, NeCmp, NoPerm, Not, NullLit, Old, OldExp, Or, PermAdd, PermDiv, PermExp, PermGeCmp, PermGtCmp, PermLeCmp, PermLtCmp, PermMinus, PermMul, PermSub, PossibleTrigger, PredicateAccess, PredicateAccessPredicate, QuantifiedExp, RangeSeq, ResourceAccess, Result, SeqAppend, SeqContains, SeqDrop, SeqExp, SeqIndex, SeqLength, SeqTake, SeqUpdate, SetExp, Sub, TrueLit, UnExp, Unfolding, WildcardPerm}

case class SilverToCol[G](program: silver.Program) {
  def blame(node: silver.Positioned): Blame[VerificationFailure] = ???
  def origin(node: silver.Positioned, sourceName: String = ""): Origin = ???
  def ??(node: silver.Node): Nothing = ???

  def transform(): col.Program[G] =
    col.Program(
      declarations =
        program.domains.map(transform) ++
          program.fields.map(transform) ++
          program.functions.map(transform) ++
          program.predicates.map(transform) ++
          program.methods.map(transform),
      rootClass = None,
    )(blame(program))(origin(program))

  def transform(domain: silver.Domain): col.AxiomaticDataType[G] =
    new col.AxiomaticDataType(
      domain.functions.map(transform) ++ domain.axioms.map(transform),
      domain.typVars.map(transform(origin(domain))),
    )(origin(domain))

  def transform(o: Origin)(tVar: silver.TypeVar): col.Variable[G] =
    new col.Variable(col.TType(col.TAny()))(SourceNameOrigin(tVar.name, o))

  def transform(func: silver.DomainFunc): col.ADTFunction[G] =
    new col.ADTFunction(
      returnType = transform(func.typ),
      args = func.formalArgs.map(transform),
    )(origin(func))

  def transform(ax: silver.DomainAxiom): col.ADTAxiom[G] =
    new col.ADTAxiom(transform(ax.exp))(origin(ax))

  def transform(field: silver.Field): col.SilverField[G] =
    new col.SilverField(transform(field.typ))(origin(field))

  def transform(func: silver.Function): col.Function[G] =
    new col.Function(
      returnType = transform(func.typ),
      args = func.formalArgs.map(transform),
      typeArgs = Nil,
      body = func.body.map(transform),
      contract = col.ApplicableContract(
        requires = col.UnitAccountedPredicate(foldStar(func.pres.map(transform))(origin(func)))(origin(func)),
        ensures = col.UnitAccountedPredicate(foldStar(func.posts.map(transform))(origin(func)))(origin(func)),
        contextEverywhere = tt, signals = Nil, givenArgs = Nil, yieldsArgs = Nil,
      )(origin(func)),
      inline = false,
      threadLocal = false,
    )(blame(func))(origin(func))

  def transform(v: silver.AnyLocalVarDecl): col.Variable[G] = v match {
    case silver.LocalVarDecl(_, typ) =>
      new col.Variable(transform(typ))(origin(v))
    case silver.UnnamedLocalVarDecl(typ) =>
      new col.Variable(transform(typ))(origin(v, "dummy"))
    case other => ??(other)
  }

  def transform(pred: silver.Predicate): col.Predicate[G] =
    new col.Predicate(
      args = pred.formalArgs.map(transform),
      body = pred.body.map(transform),
      threadLocal = false,
      inline = false
    )(origin(pred))

  def transform(proc: silver.Method): col.Procedure[G] =
    new col.Procedure(
      returnType = col.TVoid(),
      args = proc.formalArgs.map(transform),
      outArgs = proc.formalReturns.map(transform),
      typeArgs = Nil,
      body = proc.body.map(transform),
      contract = col.ApplicableContract(
        requires = col.UnitAccountedPredicate(foldStar(proc.pres.map(transform))(origin(proc)))(origin(proc)),
        ensures = col.UnitAccountedPredicate(foldStar(proc.posts.map(transform))(origin(proc)))(origin(proc)),
        contextEverywhere = tt, signals = Nil, givenArgs = Nil, yieldsArgs = Nil,
      )(origin(proc)),
      inline = false,
      pure = false,
    )(blame(proc))(origin(proc))

  def transform(s: silver.Stmt): col.Statement[G] = s match {
    case silver.NewStmt(lhs, fields) =>
      col.SilverNewRef(transform(lhs).ref, fields.map(f => new UnresolvedRef[G, col.SilverField[G]](f.name)))(origin(s))
    case silver.LocalVarAssign(lhs, rhs) =>
      assignLocal(transform(lhs), transform(rhs))(origin(s))
    case silver.FieldAssign(lhs, rhs) =>
      col.Assign(transform(lhs), transform(rhs))(blame(s))(origin(s))
    case silver.MethodCall(methodName, args, targets) =>
      col.InvokeProcedure[G](
        ref = new UnresolvedRef(methodName),
        args = args.map(transform),
        outArgs = targets.map(transform).map(_.ref),
        typeArgs = Nil, givenMap = Nil, yields = Nil,
      )(blame(s))(origin(s))
    case silver.Exhale(exp) =>
      col.Exhale(transform(exp))(blame(s))(origin(s))
    case silver.Inhale(exp) =>
      col.Inhale(transform(exp))(origin(s))
    case silver.Assert(exp) =>
      col.Assert(transform(exp))(blame(s))(origin(s))
    case silver.Assume(exp) =>
      col.Assume(transform(exp))(origin(s))
    case silver.Fold(acc) =>
      col.Fold(transform(acc))(blame(s))(origin(s))
    case silver.Unfold(acc) =>
      col.Unfold(transform(acc))(blame(s))(origin(s))
    case silver.Package(wand, proofScript) => ??(s)
    case silver.Apply(exp) => ??(s)
    case silver.Seqn(ss, scopedDecls) =>
      col.Block(ss.map(transform))(origin(s))
    case silver.If(cond, thn, els) =>
      col.Branch(Seq(
        (transform(cond), transform(thn)),
        (tt[G], transform(els)),
      ))(origin(s))
    case silver.While(cond, invs, body) =>
      col.Loop(
        init = col.Block(Nil)(origin(s)),
        cond = transform(cond),
        update = col.Block(Nil)(origin(s)),
        contract = col.LoopInvariant(foldStar(invs.map(transform))(origin(s)))(blame(s))(origin(s)),
        body = transform(body),
      )(origin(s))
    case silver.Label(name, invs) =>
      col.Label[G](new col.LabelDecl()(origin(s, name)), col.Block(Nil)(origin(s)))(origin(s))
    case silver.Goto(target) =>
      col.Goto[G](new UnresolvedRef(target))(origin(s))
    case silver.LocalVarDeclStmt(decl) =>
      col.LocalDecl(transform(decl))(origin(s))

    case stmt: silver.ExtensionStmt => ??(stmt)
  }

  def transform(t: silver.Type): col.Type[G] = t match {
    case silver.Int => col.TInt()
    case silver.Bool => col.TBool()
    case silver.Perm => col.TRational()
    case silver.Ref => col.TRef()
    case silver.Wand => col.TResource()
    case silver.SeqType(elementType) => col.TSeq(transform(elementType))
    case silver.SetType(elementType) => col.TSet(transform(elementType))
    case silver.MultisetType(elementType) => col.TBag(transform(elementType))
    case silver.MapType(keyType, valueType) => col.TMap(transform(keyType), transform(valueType))
    case silver.DomainType(domainName, partialTypVarsMap) => ??(t)
    case silver.TypeVar(name) => col.TVar(new UnresolvedRef(name))

    case silver.InternalType => ??(t)
    case silver.BackendType(boogieName, smtName) => ??(t)
    case extensionType: silver.ExtensionType => ??(extensionType)
  }

  def transform(e: silver.LocalVar): col.Local[G] = col.Local[G](new UnresolvedRef(e.name))(origin(e))

  def transform(e: silver.Exp): col.Expr[G] = {
    implicit val o: Origin = origin(e)
    val f: silver.Exp => col.Expr[G] = transform
    e match {
    case silver.Add(left, right) => col.Plus(f(left), f(right))
    case silver.And(left, right) => col.And(f(left), f(right))
    case silver.AnySetCardinality(s) => col.Size(f(s))
    case silver.AnySetContains(elem, s) =>
      if(s.typ.isInstanceOf[silver.SetType]) col.SetMember(f(elem), f(s))
      else col.BagMemberCount(f(elem), f(s))
    case silver.AnySetIntersection(left, right) => ??(e)
    case silver.AnySetMinus(left, right) => ??(e)
    case silver.AnySetSubset(left, right) => ??(e)
    case silver.AnySetUnion(left, right) => ??(e)
    case silver.CondExp(cond, thn, els) => col.Select(f(cond), f(thn), f(els))
    case silver.CurrentPerm(res) => col.CurPerm(f(res))
    case silver.Div(left, right) => col.FloorDiv(f(left), f(right))(blame(e))
    case silver.DomainFuncApp(funcname, args, typVarMap) => ??(e)
    case silver.EmptyMap(keyType, valueType) => col.LiteralMap(transform(keyType), transform(valueType), Nil)
    case silver.EmptyMultiset(elemTyp) => col.LiteralBag(transform(elemTyp), Nil)
    case silver.EmptySeq(elemTyp) => col.LiteralSeq(transform(elemTyp), Nil)
    case silver.EmptySet(elemTyp) => col.LiteralSet(transform(elemTyp), Nil)
    case silver.EpsilonPerm() => ??(e)
    case silver.EqCmp(left, right) => col.Eq(f(left), f(right))
    case silver.Exists(variables, triggers, exp) => col.Exists(variables.map(transform), triggers.map(transform), f(exp))
    case silver.ExplicitMap(elems) => ??(e)
    case silver.ExplicitMultiset(elems) => ??(e)
    case silver.ExplicitSeq(elems) => ??(e)
    case silver.ExplicitSet(elems) => ??(e)
    case silver.FalseLit() => col.BooleanValue(false)
    case silver.FieldAccess(rcv, field) => col.SilverDeref(f(rcv), new UnresolvedRef(field.name))(blame(e))
    case silver.FieldAccessPredicate(loc, perm) => ???
    case silver.Forall(variables, triggers, exp) => ???
    case silver.ForPerm(variables, resource, body) => ???
    case silver.FractionalPerm(left, right) => ???
    case silver.FullPerm() => ???
    case silver.FuncApp(funcname, args) => ???
    case silver.GeCmp(left, right) => ???
    case silver.GtCmp(left, right) => ???
    case silver.Implies(left, right) => ???
    case silver.InhaleExhaleExp(in, ex) => ???
    case silver.IntLit(i) => ???
    case silver.IntPermMul(left, right) => ???
    case silver.LabelledOld(exp, oldLabel) => ???
    case silver.LeCmp(left, right) => ???
    case silver.Let(variable, exp, body) => ???
    case silver.LocalVar(name, typ) => ???
    case silver.LtCmp(left, right) => ???
    case silver.MagicWand(left, right) => ???
    case silver.MapCardinality(base) => ???
    case silver.MapContains(key, base) => ???
    case silver.MapDomain(base) => ???
    case silver.Maplet(key, value) => ???
    case silver.MapLookup(base, key) => ???
    case silver.MapRange(base) => ???
    case silver.MapUpdate(base, key, value) => ???
    case silver.Minus(exp) => ???
    case silver.Mod(left, right) => ???
    case silver.Mul(left, right) => ???
    case silver.NeCmp(left, right) => ???
    case silver.NoPerm() => ???
    case silver.Not(exp) => ???
    case silver.NullLit() => ???
    case silver.Old(exp) => ???
    case silver.Or(left, right) => ???
    case silver.PermAdd(left, right) => ???
    case silver.PermDiv(left, right) => ???
    case silver.PermGeCmp(left, right) => ???
    case silver.PermGtCmp(left, right) => ???
    case silver.PermLeCmp(left, right) => ???
    case silver.PermLtCmp(left, right) => ???
    case silver.PermMinus(exp) => ???
    case silver.PermMul(left, right) => ???
    case silver.PermSub(left, right) => ???
    case silver.PredicateAccess(args, predicateName) => ???
    case silver.PredicateAccessPredicate(loc, perm) => ???
    case silver.RangeSeq(low, high) => ???
    case silver.Result(typ) => ???
    case silver.SeqAppend(left, right) => ???
    case silver.SeqContains(elem, s) => ???
    case silver.SeqDrop(s, n) => ???
    case silver.SeqIndex(s, idx) => ???
    case silver.SeqLength(s) => ???
    case silver.SeqTake(s, n) => ???
    case silver.SeqUpdate(s, idx, elem) => ???
    case silver.Sub(left, right) => ???
    case silver.TrueLit() => ???
    case silver.Unfolding(acc, body) => ???
    case silver.WildcardPerm() => ???

    case silver.Applying(wand, body) => ??(e)
    case silver.BackendFuncApp(backendFunc, args) => ??(e)
    case exp: ExtensionExp => ??(exp)
  }
  }
}
