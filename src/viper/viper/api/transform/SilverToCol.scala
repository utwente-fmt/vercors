package viper.api.transform

import hre.io.{RWFile, Readable}
import vct.col.origin._
import vct.col.ref.UnresolvedRef
import vct.col.util.AstBuildHelpers._
import vct.col.{ast => col}
import vct.parsers.transform.BlameProvider
import vct.result.VerificationError.UserError
import viper.api.transform.SilverToCol.{
  SilverNodeNotSupported,
  SilverPositionOrigin,
}
import viper.silver.ast.{
  AbstractSourcePosition,
  FilePosition,
  HasIdentifier,
  HasLineColumn,
  IdentifierPosition,
  LineColumnPosition,
  NoPosition,
  SourcePosition,
  TranslatedPosition,
  VirtualPosition,
}
import viper.silver.plugin.standard.termination.{
  DecreasesClause,
  DecreasesStar,
  DecreasesTuple,
  DecreasesWildcard,
}
import viper.silver.verifier.AbstractError
import viper.silver.{ast => silver}

import java.nio.file.{Path, Paths}

case object SilverToCol {
  private def SilverPositionOrigin(node: silver.Positioned): Origin =
    node.pos match {
      case NoPosition => Origin(Nil)
      case position: SourcePosition =>
        val start = position.start
        val end = position.end.getOrElse(start)
        Origin(Seq(
          PositionRange(
            start.line - 1,
            end.line - 1,
            Some((start.column - 1, end.column - 1)),
          ),
          ReadableOrigin(RWFile(position.file)),
        ))
      case FilePosition(file, vline, col) =>
        Origin(Seq(
          PositionRange(vline, vline, Some((col, col))),
          ReadableOrigin(RWFile(file)),
        ))
      case LineColumnPosition(line, column) =>
        Origin(Seq(PositionRange(line, line, Some((column, column)))))
      case VirtualPosition(identifier) => Origin(Seq(LabelContext(identifier)))
      case _ => Origin(Nil)
    }

  case class SilverNodeNotSupported(node: silver.Node) extends UserError {
    override def code: String = "silverNodeNotSupported"
    override def text: String =
      s"This kind of silver node is not supported in VerCors yet: $node" +
        (node match {
          case pos: silver.Positioned => s" (at ${pos.pos})"
          case _ => ""
        })
  }

  case class SilverFrontendParseError(path: Path, errors: Seq[AbstractError])
      extends UserError {
    override def code: String = "silverFrontendError"
    override def text: String =
      s"Could not parse file $path. " +
        (errors match {
          case Nil => "However, viper did not specify an error. (?)"
          case errors => "Viper said:\n" + errors.map(_.toString).mkString("\n")
        })
  }

  def transform[G](
      diagnosticPath: Path,
      in: Either[Seq[AbstractError], silver.Program],
      blameProvider: BlameProvider,
  ): col.Program[G] =
    in match {
      case Right(program) => SilverToCol(program, blameProvider).transform()
      case Left(errors) =>
        throw SilverFrontendParseError(diagnosticPath, errors)
    }

  def parse[G](path: Path, blameProvider: BlameProvider): col.Program[G] =
    transform(path, new SilverParserDummyFrontend().parse(path), blameProvider)

  def parse[G](
      input: String,
      diagnosticPath: Path,
      blameProvider: BlameProvider,
  ): col.Program[G] =
    transform(
      diagnosticPath,
      new SilverParserDummyFrontend().parse(input, diagnosticPath),
      blameProvider,
    )

  def parse[G](
      readable: Readable,
      blameProvider: BlameProvider,
  ): col.Program[G] =
    transform(
      Paths.get(readable.fileName),
      new SilverParserDummyFrontend().parse(readable),
      blameProvider,
    )
}

case class SilverToCol[G](
    program: silver.Program,
    blameProvider: BlameProvider,
) {
  def origin(node: silver.Positioned, sourceName: String = ""): Origin =
    if (sourceName.nonEmpty)
      SilverPositionOrigin(node).sourceName(sourceName)
    else
      node match {
        case node: silver.Declaration =>
          SilverPositionOrigin(node).sourceName(node.name)
        case _ => SilverPositionOrigin(node)
      }

  def blame(node: silver.Positioned): Blame[VerificationFailure] =
    node.pos match {
      case pos: AbstractSourcePosition =>
        blameProvider(
          pos.start.line - 1,
          pos.end.map(_.line).getOrElse(pos.start.line) - 1,
          Some((
            pos.start.column - 1,
            pos.end.map(_.column - 1).getOrElse(pos.start.column),
          )),
        )
      case _ => blameProvider(0, 0, Some((0, 1)))
    }

  def ??(node: silver.Node): Nothing = throw SilverNodeNotSupported(node)

  def transform(): col.Program[G] =
    col.Program(declarations =
      program.domains.map(transform) ++ program.fields.map(transform) ++
        program.functions.map(transform) ++ program.predicates.map(transform) ++
        program.methods.map(transform)
    )(blame(program))(origin(program))

  def transform(domain: silver.Domain): col.AxiomaticDataType[G] =
    new col.AxiomaticDataType(
      domain.functions.map(transform) ++ domain.axioms.map(transform),
      domain.typVars.map(transform(origin(domain))),
    )(origin(domain))

  def transform(o: Origin)(tVar: silver.TypeVar): col.Variable[G] =
    new col.Variable(col.TType(col.TAnyValue()))(o.sourceName(tVar.name))

  def transform(func: silver.DomainFunc): col.ADTFunction[G] =
    new col.ADTFunction(
      returnType = transform(func.typ),
      args = func.formalArgs.map(transform),
    )(origin(func))

  def transform(ax: silver.DomainAxiom): col.ADTAxiom[G] =
    new col.ADTAxiom(transform(ax.exp))(origin(ax))

  def transform(field: silver.Field): col.SilverField[G] =
    new col.SilverField(transform(field.typ))(origin(field))

  def partitionDecreases(
      exps: Seq[silver.Exp]
  ): (Seq[silver.Exp], Seq[DecreasesClause]) =
    exps.partitionMap {
      case decreases: DecreasesClause => Right(decreases)
      case other => Left(other)
    }

  def partitionContract(
      contracted: silver.Contracted
  ): (Seq[silver.Exp], Seq[silver.Exp], Option[DecreasesClause]) = {
    val (pres, decreases1) = partitionDecreases(contracted.pres)
    val (posts, decreases2) = partitionDecreases(contracted.posts)

    val decreases = (decreases1 ++ decreases2) match {
      case Nil => None
      case x +: Nil => Some(x)
      case _ +: x +: _ => ??(x)
    }

    (pres, posts, decreases)
  }

  def transform(clause: DecreasesClause): Option[col.DecreasesClause[G]] =
    clause match {
      case DecreasesTuple(_, Some(cond)) => ??(cond)
      case DecreasesTuple(Nil, None) =>
        Some(col.DecreasesClauseNoRecursion()(origin(clause)))
      case DecreasesTuple(exps, None) =>
        Some(col.DecreasesClauseTuple(exps.map(transform))(origin(clause)))
      case DecreasesWildcard(Some(cond)) => ??(cond)
      case DecreasesWildcard(None) =>
        Some(col.DecreasesClauseAssume()(origin(clause)))
      case DecreasesStar() => None
    }

  def transform(func: silver.Function): col.Function[G] = {
    val (pres, posts, decreases) = partitionContract(func)

    new col.Function(
      returnType = transform(func.typ),
      args = func.formalArgs.map(transform),
      typeArgs = Nil,
      body = func.body.map(transform),
      contract =
        col.ApplicableContract(
          requires =
            col.UnitAccountedPredicate(
              foldStar(pres.map(transform))(origin(func))
            )(origin(func)),
          ensures =
            col.UnitAccountedPredicate(
              foldStar(posts.map(transform))(origin(func))
            )(origin(func)),
          contextEverywhere = tt,
          signals = Nil,
          givenArgs = Nil,
          yieldsArgs = Nil,
          decreases = decreases.flatMap(transform),
        )(blame(func))(origin(func)),
      doInline = false,
      threadLocal = false,
    )(blame(func))(origin(func))
  }

  def transform(v: silver.AnyLocalVarDecl): col.Variable[G] =
    v match {
      case silver.LocalVarDecl(_, typ) =>
        new col.Variable(transform(typ))(origin(v))
      case silver.UnnamedLocalVarDecl(typ) =>
        new col.Variable(transform(typ))(origin(v, "dummy"))
    }

  def transform(pred: silver.Predicate): col.Predicate[G] =
    new col.Predicate(
      args = pred.formalArgs.map(transform),
      body = pred.body.map(transform),
      threadLocal = false,
      doInline = false,
    )(origin(pred))

  def transform(proc: silver.Method): col.Procedure[G] = {
    val (pres, posts, decreases) = partitionContract(proc)

    new col.Procedure(
      returnType = col.TVoid(),
      args = proc.formalArgs.map(transform),
      outArgs = proc.formalReturns.map(transform),
      typeArgs = Nil,
      body = proc.body.map(transform),
      contract =
        col.ApplicableContract(
          requires =
            col.UnitAccountedPredicate(
              foldStar(pres.map(transform))(origin(proc))
            )(origin(proc)),
          ensures =
            col.UnitAccountedPredicate(
              foldStar(posts.map(transform))(origin(proc))
            )(origin(proc)),
          contextEverywhere = tt,
          signals = Nil,
          givenArgs = Nil,
          yieldsArgs = Nil,
          decreases = decreases.flatMap(transform),
        )(blame(proc))(origin(proc)),
      doInline = false,
      pure = false,
    )(blame(proc))(origin(proc))
  }

  def transform(s: silver.Stmt): col.Statement[G] =
    s match {
      case silver.NewStmt(lhs, fields) =>
        col.SilverNewRef(
          transform(lhs).ref,
          fields.map(f => new UnresolvedRef[G, col.SilverField[G]](f.name)),
        )(origin(s))
      case silver.LocalVarAssign(lhs, rhs) =>
        assignLocal(transform(lhs), transform(rhs))(origin(s))
      case silver.FieldAssign(lhs, rhs) =>
        col.Assign(transform(lhs), transform(rhs))(blame(s))(origin(s))
      case silver.MethodCall(methodName, args, targets) =>
        col.InvokeProcedure[G](
          ref = new UnresolvedRef(methodName),
          args = args.map(transform),
          outArgs = targets.map(transform),
          typeArgs = Nil,
          givenMap = Nil,
          yields = Nil,
        )(blame(s))(origin(s))
      case silver.Exhale(exp) => col.Exhale(transform(exp))(blame(s))(origin(s))
      case silver.Inhale(exp) => col.Inhale(transform(exp))(origin(s))
      case silver.Assert(exp) => col.Assert(transform(exp))(blame(s))(origin(s))
      case silver.Assume(exp) => col.Assume(transform(exp))(origin(s))
      case silver.Fold(acc) => col.Fold(transform(acc))(blame(s))(origin(s))
      case silver.Unfold(acc) => col.Unfold(transform(acc))(blame(s))(origin(s))
      case silver.Seqn(ss, scopedDecls) =>
        val vars = scopedDecls.flatMap {
          case decl @ silver.LocalVarDecl(_, typ) =>
            Seq(new col.Variable(transform(typ))(origin(decl)))
          case _: silver.Label => Nil // scoped by the method for us
          case other: silver.Declaration with silver.Node => ??(other)
          case _: silver.Declaration => ??? // unreachable
        }
        col.Scope(vars, col.Block(ss.map(transform))(origin(s)))(origin(s))
      case silver.If(cond, thn, els) =>
        col.Branch(
          Seq((transform(cond), transform(thn)), (tt[G], transform(els)))
        )(origin(s))
      case silver.While(cond, invs, body) =>
        col.Loop(
          init = col.Block(Nil)(origin(s)),
          cond = transform(cond),
          update = col.Block(Nil)(origin(s)),
          contract =
            col.LoopInvariant(foldStar(invs.map(transform))(origin(s)), None)(
              blame(s)
            )(origin(s)),
          body = transform(body),
        )(origin(s))
      case silver.Label(name, invs) =>
        col.Label[G](
          new col.LabelDecl()(origin(s, name)),
          col.Block(Nil)(origin(s)),
        )(origin(s))
      case silver.Goto(target) =>
        col.Goto[G](new UnresolvedRef(target))(origin(s))
      case silver.LocalVarDeclStmt(decl) =>
        col.LocalDecl(transform(decl))(origin(s))

      case silver.Package(wand, proofScript) => ??(s)
      case silver.Apply(exp) => ??(s)
      case silver.Quasihavoc(_, _) => ??(s)
      case silver.Quasihavocall(_, _, _) => ??(s)

      case stmt: silver.ExtensionStmt => ??(stmt)
    }

  def transform(t: silver.Type): col.Type[G] =
    t match {
      case silver.Int => col.TInt()
      case silver.Bool => col.TBool()
      case silver.Perm => col.TRational()
      case silver.Ref => col.TRef()
      case silver.Wand => col.TResource()
      case silver.SeqType(elementType) => col.TSeq(transform(elementType))
      case silver.SetType(elementType) => col.TSet(transform(elementType))
      case silver.MultisetType(elementType) => col.TBag(transform(elementType))
      case silver.MapType(keyType, valueType) =>
        col.TMap(transform(keyType), transform(valueType))
      case silver.DomainType(domainName, partialTypVarsMap) =>
        col.SilverPartialTAxiomatic(
          new UnresolvedRef(domainName),
          partialTypVarsMap.toSeq.map { case (k, v) =>
            (new UnresolvedRef(k.name), transform(v))
          },
        )
      case silver.TypeVar(name) => col.TVar(new UnresolvedRef(name))

      case silver.InternalType => ??(t)
      case silver.BackendType(boogieName, smtName) => ??(t)
      case extensionType: silver.ExtensionType => ??(extensionType)
    }

  def transform(e: silver.LocalVar): col.Local[G] =
    col.Local[G](new UnresolvedRef(e.name))(origin(e))

  def transform(trigger: silver.Trigger): Seq[col.Expr[G]] =
    trigger.exps.map(transform)

  def transform(e: silver.Exp): col.Expr[G] = {
    implicit val o: Origin = origin(e)
    val f: silver.Exp => col.Expr[G] = transform
    e match {
      case silver.Add(left, right) => col.Plus(f(left), f(right))
      case silver.And(left, right) =>
        if (e.isPure)
          col.And(f(left), f(right))
        else
          col.Star(f(left), f(right))
      case silver.AnySetCardinality(s) => col.Size(f(s))
      case silver.AnySetContains(elem, s) =>
        if (s.typ.isInstanceOf[silver.SetType])
          col.SetMember(f(elem), f(s))
        else
          col.BagMemberCount(f(elem), f(s))
      case silver.AnySetIntersection(left, right) =>
        if (left.typ.isInstanceOf[silver.SetType])
          col.SetIntersection(f(left), f(right))
        else
          col.BagLargestCommon(f(left), f(right))
      case silver.AnySetMinus(left, right) =>
        if (left.typ.isInstanceOf[silver.SetType])
          col.SetMinus(f(left), f(right))
        else
          col.BagMinus(f(left), f(right))
      case silver.AnySetSubset(left, right) =>
        if (left.typ.isInstanceOf[silver.SetType])
          col.SubSetEq(f(left), f(right))
        else
          col.SubBagEq(f(left), f(right))
      case silver.AnySetUnion(left, right) =>
        if (left.typ.isInstanceOf[silver.SetType])
          col.SetUnion(f(left), f(right))
        else
          col.BagAdd(f(left), f(right))
      case silver.CondExp(cond, thn, els) => col.Select(f(cond), f(thn), f(els))
      case silver.CurrentPerm(res) =>
        col.CurPerm(col.AmbiguousLocation(f(res))(
          vct.col.origin.PanicBlame("Silver does not have pointers.")
        ))
      case silver.Div(left, right) => col.FloorDiv(f(left), f(right))(blame(e))
      case silver.DomainFuncApp(funcname, args, typVarMap) =>
        col.SilverPartialADTFunctionInvocation(
          funcname,
          args.map(f),
          typVarMap.toSeq.map { case (v, t) =>
            (new UnresolvedRef[G, col.Variable[G]](v.name), transform(t))
          },
        )
      case silver.EmptyMap(keyType, valueType) =>
        col.LiteralMap(transform(keyType), transform(valueType), Nil)
      case silver.EmptyMultiset(elemTyp) =>
        col.LiteralBag(transform(elemTyp), Nil)
      case silver.EmptySeq(elemTyp) => col.LiteralSeq(transform(elemTyp), Nil)
      case silver.EmptySet(elemTyp) => col.LiteralSet(transform(elemTyp), Nil)
      case silver.EqCmp(left, right) => col.Eq(f(left), f(right))
      case silver.Exists(variables, triggers, exp) =>
        col.Exists(variables.map(transform), triggers.map(transform), f(exp))
      case silver.ExplicitMap(elems) =>
        col.SilverUntypedNonemptyLiteralMap(elems.map {
          case silver.Maplet(k, v) => (f(k), f(v))
          case other => ??(other)
        })
      case silver.ExplicitMultiset(elems) => col.UntypedLiteralBag(elems.map(f))
      case silver.ExplicitSeq(elems) => col.UntypedLiteralSeq(elems.map(f))
      case silver.ExplicitSet(elems) => col.UntypedLiteralSet(elems.map(f))
      case silver.FalseLit() => col.BooleanValue(false)
      case silver.FieldAccess(rcv, field) =>
        col.SilverDeref[G](f(rcv), new UnresolvedRef(field.name))(blame(e))
      case silver.FieldAccessPredicate(loc, perm) =>
        col.Perm[G](
          col.SilverFieldLocation[G](
            f(loc.rcv),
            new UnresolvedRef(loc.field.name),
          ),
          f(perm),
        )
      case silver.Forall(variables, triggers, exp) =>
        if (exp.isPure)
          col.Forall(variables.map(transform), triggers.map(transform), f(exp))
        else
          col
            .Starall(variables.map(transform), triggers.map(transform), f(exp))(
              blame(e)
            )
      case silver.FractionalPerm(left, right) =>
        col.RatDiv(f(left), f(right))(blame(e))
      case silver.FullPerm() => col.WritePerm()
      case silver.FuncApp(funcname, args) =>
        col.FunctionInvocation[G](
          new UnresolvedRef(funcname),
          args.map(f),
          Nil,
          Nil,
          Nil,
        )(blame(e))
      case silver.GeCmp(left, right) => col.GreaterEq(f(left), f(right))
      case silver.GtCmp(left, right) => col.Greater(f(left), f(right))
      case silver.Implies(left, right) => col.Implies(f(left), f(right))
      case silver.IntLit(i) => col.IntegerValue(i)
      case silver.IntPermMul(left, right) => col.Mult(f(left), f(right))
      case silver.LabelledOld(exp, oldLabel) =>
        col.Old[G](f(exp), Some(new UnresolvedRef(oldLabel)))(blame(e))
      case silver.LeCmp(left, right) => col.LessEq(f(left), f(right))
      case silver.Let(variable, exp, body) =>
        col.Let(transform(variable), f(exp), f(body))
      case v @ silver.LocalVar(name, typ) => transform(v)
      case silver.LtCmp(left, right) => col.Less(f(left), f(right))
      case silver.MapCardinality(base) => col.Size(f(base))
      case silver.MapContains(key, base) => col.MapMember(f(key), f(base))
      case silver.MapDomain(base) => col.MapKeySet(f(base))
      case silver.Maplet(key, value) =>
        col.SilverUntypedNonemptyLiteralMap(Seq((f(key), f(value))))
      case silver.MapLookup(base, key) => col.MapGet(f(base), f(key))(blame(e))
      case silver.MapRange(base) => col.MapValueSet(f(base))
      case silver.MapUpdate(base, key, value) =>
        col.MapCons(f(base), f(key), f(value))
      case silver.Minus(exp) => col.UMinus(f(exp))
      case silver.Mod(left, right) => col.Mod(f(left), f(right))(blame(e))
      case silver.Mul(left, right) => col.Mult(f(left), f(right))
      case silver.NeCmp(left, right) => col.Neq(f(left), f(right))
      case silver.NoPerm() => col.NoPerm()
      case silver.Not(exp) => col.Not(f(exp))
      case silver.NullLit() => col.Null()
      case silver.Old(exp) => col.Old(f(exp), None)(blame(e))
      case silver.Or(left, right) => col.Or(f(left), f(right))
      case silver.PermAdd(left, right) => col.Plus(f(left), f(right))
      case silver.PermDiv(left, right) =>
        col.RatDiv(f(left), f(right))(blame(e))
      case silver.PermPermDiv(left, right) =>
        col.RatDiv(f(left), f(right))(blame(e))
      case silver.PermGeCmp(left, right) => col.GreaterEq(f(left), f(right))
      case silver.PermGtCmp(left, right) => col.Greater(f(left), f(right))
      case silver.PermLeCmp(left, right) => col.LessEq(f(left), f(right))
      case silver.PermLtCmp(left, right) => col.Less(f(left), f(right))
      case silver.PermMinus(exp) => col.UMinus(f(exp))
      case silver.PermMul(left, right) => col.Mult(f(left), f(right))
      case silver.PermSub(left, right) => col.Minus(f(left), f(right))
      case silver.PredicateAccess(args, predicateName) =>
        col.PredicateApply(
          new UnresolvedRef(predicateName),
          args.map(f),
          col.WritePerm(),
        )
      case silver.PredicateAccessPredicate(
            silver.PredicateAccess(args, predicateName),
            perm,
          ) =>
        col.PredicateApply(
          new UnresolvedRef(predicateName),
          args.map(f),
          f(perm),
        )
      case silver.RangeSeq(low, high) => col.Range(f(low), f(high))
      case silver.Result(typ) => col.AmbiguousResult()
      case silver.SeqAppend(left, right) => col.Concat(f(left), f(right))
      case silver.SeqContains(elem, s) => col.SeqMember(f(elem), f(s))
      case silver.SeqDrop(s, n) => col.Drop(f(s), f(n))
      case silver.SeqIndex(s, idx) => col.SeqSubscript(f(s), f(idx))(blame(e))
      case silver.SeqLength(s) => col.Size(f(s))
      case silver.SeqTake(s, n) => col.Take(f(s), f(n))
      case silver.SeqUpdate(s, idx, elem) =>
        col.SeqUpdate(f(s), f(idx), f(elem))
      case silver.Sub(left, right) => col.Minus(f(left), f(right))
      case silver.TrueLit() => col.BooleanValue(true)
      case silver.Unfolding(acc, body) =>
        col.Unfolding(f(acc), f(body))(blame(e))
      case silver.WildcardPerm() => col.ReadPerm()

      case silver.ForPerm(variables, resource, body) => ??(e)
      case silver.EpsilonPerm() => ??(e)
      case silver.InhaleExhaleExp(in, ex) => ??(e)
      case silver.MagicWand(left, right) => ??(e)
      case silver.Applying(wand, body) => ??(e)
      case silver.BackendFuncApp(backendFunc, args) => ??(e)
      case exp: silver.ExtensionExp => ??(exp)
    }
  }
}
