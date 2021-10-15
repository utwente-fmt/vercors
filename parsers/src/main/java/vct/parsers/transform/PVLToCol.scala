package vct.parsers.transform

import org.antlr.v4.runtime.ParserRuleContext
import vct.col.ast.Origin
import vct.col.ast._
import vct.antlr4.generated.PVLParser._
import vct.antlr4.generated.PVLParserPatterns._
import vct.col.{ast => col}
import vct.antlr4.generated.{PVLParserPatterns => parse}
import vct.col.ast.Constant._
import vct.col.ast.stmt.composite.ParallelRegion
import AstBuildHelpers._
import hre.util.FuncTools

import scala.collection.mutable

case class PVLToCol(override val originProvider: OriginProvider, override val blameProvider: BlameProvider, override val errors: mutable.Map[(Int, Int), String])
  extends ToCol(originProvider, blameProvider, errors) {
  def convert(implicit program: ProgramContext): Seq[GlobalDeclaration] = program match {
    case Program0(decls, _) => decls.flatMap(convert(_))
  }

  def convert(implicit decl: ProgramDeclContext): Seq[GlobalDeclaration] = decl match {
    case ProgramDecl0(cls) => Seq(convert(cls))
    case ProgramDecl1(method) => Seq(convertProcedure(method))
    case ProgramDecl2(valDecl) => convert(valDecl)
  }

  def convertProcedure(implicit method: MethodContext): Procedure = method match {
    case Method0(contract, modifiers, returnType, name, _, args, _, body) =>
      withModifiers(modifiers, mods => withContract(contract, contract => {
        new Procedure(
          convert(returnType),
          args.map(convert(_)).getOrElse(Nil),
          outArgs = Nil,
          convert(body),
          contract.consumeApplicableContract(),
          inline = mods.consume(mods.inline),
          pure = mods.consume(mods.pure),
        )(blame(method))(SourceNameOrigin(convert(name), origin(method)))
      }))
  }

  def convert(implicit cls: DeclClassContext): Class = cls match {
    case DeclClass0(_, name, _, decls, _) =>
      new Class(decls.flatMap(convert(_)), supports = Nil)(SourceNameOrigin(convert(name), origin(cls)))
  }

  def convert(implicit decl: ClassDeclContext): Seq[ClassDeclaration] = decl match {
    case ClassDecl0(inner) => convert(inner)
    case ClassDecl1(inner) => Seq(convert(inner))
    case ClassDecl2(inner) => convert(inner, x => x)
    case ClassDecl3(inner) => convert(inner)
  }

  def convert(implicit method: MethodContext): InstanceMethod = method match {
    case Method0(contract, modifiers, returnType, name, _, args, _, body) =>
      withModifiers(modifiers, mods => withContract(contract, contract => {
        new InstanceMethod(
          convert(returnType),
          args.map(convert(_)).getOrElse(Nil),
          outArgs = Nil,
          convert(body),
          contract.consumeApplicableContract(),
          inline = mods.consume(mods.inline),
          pure = mods.consume(mods.pure),
        )(blame(method))(SourceNameOrigin(convert(name), origin(method)))
      }))
  }

  def convert(implicit body: MethodBodyContext): Option[Statement] = body match {
    case MethodBody0(_) => None
    case MethodBody1(stat) => Some(convert(stat))
  }

  def convert(implicit constructor: ConstructorContext): Seq[ClassDeclaration] = constructor match {
    case Constructor0(contract, _, _, args, _, body) =>
      Seq(withContract(contract, contract =>
        PVLConstructor(contract.consumeApplicableContract(), args.map(convert(_)).getOrElse(Nil), convert(body))))
  }

  def convert(implicit field: FieldContext): Seq[InstanceField] = field match {
    case Field0(t, ids, _) =>
      convert(ids).map(name => new InstanceField(convert(t), Set.empty)(SourceNameOrigin(name, origin(field))))
  }

  def convert(implicit args: ArgsContext): Seq[Variable] = args match {
    case Args0(t, name) => Seq(new Variable(convert(t))(SourceNameOrigin(convert(name), origin(name))))
    case Args1(t, name, _, args) =>
      new Variable(convert(t))(SourceNameOrigin(convert(name), origin(name))) +: convert(args)
  }

  def convert(implicit exprs: ExprListContext): Seq[Expr] = exprs match {
    case ExprList0(e) => Seq(convert(e))
    case ExprList1(e, _, es) => convert(e) +: convert(es)
  }

  def convert(implicit tuple: TupleContext): Seq[Expr] = tuple match {
    case Tuple0(_, exprs, _) => exprs.map(convert(_)).getOrElse(Nil)
  }

  def convert(implicit exprs: NewDimsContext): Seq[Expr] = exprs match {
    case NewDims0(dims) => dims.map(convert(_))
  }

  def convert(implicit exprs: InvariantListContext): Expr = exprs match {
    case InvariantList0(invs) => Star.fold(invs.map(convert(_)))
  }

  def convert(implicit dim: QuantifiedDimContext): Expr = dim match {
    case QuantifiedDim0(_, inner, _) => convert(inner)
  }

  def convert(implicit inv: InvariantContext): Expr = inv match {
    case Invariant0(_, inv, _) => convert(inv)
  }

  def convert(implicit expr: ExprContext): Expr = expr match {
    case Expr0(pre, inner, post) =>
      convertWith(pre, convertThen(post, convert(inner)))
  }

  def convert(implicit expr: UnfoldingExprContext): Expr = expr match {
    case UnfoldingExpr0(_, pred, _, body) => Unfolding(convert(pred), convert(body))
    case UnfoldingExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: IteExprContext): Expr = expr match {
    case IteExpr0(cond, _, whenTrue, _, whenFalse) => Select(convert(cond), convert(whenTrue), convert(whenFalse))
    case IteExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: ImplicationExprContext): Expr = expr match {
    case ImplicationExpr0(left, specOp, right) => convert(specOp, convert(left), convert(right))
    case ImplicationExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: OrExprContext): Expr = expr match {
    case OrExpr0(left, _, right) => AmbiguousOr(convert(left), convert(right))
    case OrExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: AndExprContext): Expr = expr match {
    case AndExpr0(left, _, right) => And(convert(left), convert(right))
    case AndExpr1(left, specOp, right) => convert(specOp, convert(left), convert(right))
    case AndExpr2(inner) => convert(inner)
  }

  def convert(implicit expr: EqExprContext): Expr = expr match {
    case EqExpr0(left, _, right) => Eq(convert(left), convert(right))
    case EqExpr1(left, _, right) => Neq(convert(left), convert(right))
    case EqExpr2(inner) => convert(inner)
  }

  def convert(implicit expr: RelExprContext): Expr = expr match {
    case RelExpr0(left, _, right) => Less(convert(left), convert(right))
    case RelExpr1(left, _, right) => LessEq(convert(left), convert(right))
    case RelExpr2(left, _, right) => GreaterEq(convert(left), convert(right))
    case RelExpr3(left, _, right) => Greater(convert(left), convert(right))
    case RelExpr4(left, specOp, right) => convert(specOp, convert(left), convert(right))
    case RelExpr5(inner) => convert(inner)
  }

  def convert(implicit expr: SetExprContext): Expr = expr match {
    case SetExpr0(x, _, xs) => AmbiguousMember(convert(x), convert(xs))
    case SetExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: AddExprContext): Expr = expr match {
    case AddExpr0(left, _, right) => AmbiguousPlus(convert(left), convert(right))
    case AddExpr1(left, _, right) => Minus(convert(left), convert(right))
    case AddExpr2(inner) => convert(inner)
  }

  def convert(implicit expr: MultExprContext): Expr = expr match {
    case MultExpr0(left, _, right) => AmbiguousMult(convert(left), convert(right))
    case MultExpr1(left, _, right) => Div(convert(left), convert(right))(blame(expr))
    case MultExpr2(left, _, right) => Mod(convert(left), convert(right))(blame(expr))
    case MultExpr3(left, specOp, right) => convert(specOp, convert(left), convert(right))
    case MultExpr4(inner) => convert(inner)
  }

  def convert(implicit expr: PowExprContext): Expr = expr match {
    case PowExpr0(left, _, right) => Exp(convert(left), convert(right))
    case PowExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: SeqAddExprContext): Expr = expr match {
    case SeqAddExpr0(left, _, right) => Cons(convert(left), convert(right))
    case SeqAddExpr1(left, _, right) => ??(expr)
    case SeqAddExpr2(m, _, _, k, _, v, _) => MapCons(convert(m), convert(k), convert(v))
    case SeqAddExpr3(inner) => convert(inner)
  }

  def convert(implicit expr: UnaryExprContext): Expr = expr match {
    case UnaryExpr0(_, inner) => Not(convert(inner))
    case UnaryExpr1(_, inner) => UMinus(convert(inner))
    case UnaryExpr2(inner) => convert(inner)
  }

  def convert(implicit expr: NewExprContext): Expr = expr match {
    case NewExpr0(_, name, Call0(given, args, yields)) => PVLNew(convert(name), convert(args))(blame(expr))
    case NewExpr1(_, t, dims) => NewArray(convert(t), convert(dims), moreDims = 0)
    case NewExpr2(inner) => convert(inner)
  }

  def convert(implicit expr: PostfixExprContext): Expr = expr match {
    case PostfixExpr0(obj, _, field, None) => PVLDeref(convert(obj), convert(field))
    case PostfixExpr0(obj, _, field, Some(Call0(given, args, yields))) =>
      PVLInvocation(Some(convert(obj)), convert(field), convert(args), convertGiven(given), convertYields(yields))(blame(expr))
    case PostfixExpr1(xs, _, i, _) => AmbiguousSubscript(convert(xs), convert(i))
    case PostfixExpr2(obj, specOp) => convert(specOp, convert(obj))
    case PostfixExpr3(inner) => convert(inner)
  }

  def convert(implicit expr: UnitContext): Expr = expr match {
    case Unit0(inner) => convert(inner)
    case Unit1(_) => AmbiguousThis()
    case Unit2(_) => Null()
    case Unit3(n) => Integer.parseInt(n)
    case Unit4(_, inner, _) => convert(inner)
    case Unit5(id, None) => convertExpr(id)
    case Unit5(id, Some(Call0(given, args, yields))) =>
      PVLInvocation(None, convert(id), convert(args), convertGiven(given), convertYields(yields))(blame(expr))
  }

  def convert(implicit stat: StatementContext): Statement = stat match {
    case PvlReturn(_, value, _) => Return(value.map(convert(_)).getOrElse(Void()))
    case PvlLock(_, obj, _) => Lock(convert(obj))
    case PvlUnlock(_, obj, _) => Unlock(convert(obj))
    case PvlWait(_, obj, _) => Wait(convert(obj))
    case PvlNotify(_, obj, _) => Notify(convert(obj))
    case PvlFork(_, obj, _) => Fork(convert(obj))
    case PvlJoin(_, obj, _) => Join(convert(obj))
    case PvlValStatement(inner) => convert(inner)
    case PvlIf(_, _, cond, _, body, None) =>
      Branch(Seq((convert(cond), convert(body))))
    case PvlIf(_, _, cond, _, body, Some(ElseBlock0(_, otherwise))) =>
      Branch(Seq(
        (convert(cond), convert(body)),
        (true, convert(otherwise)),
      ))
    case PvlBarrier(_, _, block, tags, _, body) =>
      val (contract, content) = body match {
        case BarrierBody0(_, contract, _) => (contract, Block(Nil))
        case BarrierBody1(contract, content) => (contract, content)
      }

      withContract(contract, contract => {
        ParBarrier(
        block = new UnresolvedRef[ParBlockDecl](convert(block)),
          invs = tags.map(convert(_)).getOrElse(Nil),
          Star.fold(contract.consume(contract.requires)),
          Star.fold(contract.consume(contract.ensures)),
          Block(Nil),
        )(blame(stat))
      })
    case PvlPar(contract, _, pars) =>
      withContract(contract, contract => {
        ParRegion(
          requires = Star.fold(contract.consume(contract.requires)),
          ensures = Star.fold(contract.consume(contract.ensures)),
          convert(pars),
        )(blame(stat))
      })
    case PvlVec(_, _, iter, _, body) =>
      ??(stat)
    case PvlInvariant(_, name, _, res, _, body) =>
      ParInvariant(
        new ParInvariantDecl()(SourceNameOrigin(convert(name), origin(stat))),
        convert(res), convert(body)
      )(blame(stat))
    case PvlAtomic(_, _, invs, _, body) =>
      ParAtomic(convert(invs).map(new UnresolvedRef[ParInvariantDecl](_)), convert(body))
    case PvlWhile(invs, _, _, cond, _, body) =>
      Scope(Nil, Loop(Block(Nil), convert(cond), Block(Nil), LoopInvariant(convert(invs)), convert(body)))
    case PvlFor(invs, _, _, init, _, cond, _, update, _, body) =>
      Scope(Nil, Loop(
        init.map(convert(_)).getOrElse(Block(Nil)),
        cond.map(convert(_)).getOrElse(true),
        update.map(convert(_)).getOrElse(Block(Nil)),
        LoopInvariant(convert(invs)),
        convert(body)
      ))
    case PvlBlock(inner) => convert(inner)
    case PvlGoto(_, label, _) => Goto(new UnresolvedRef[LabelDecl](convert(label)))
    case PvlLabel(_, label, _) => Label(new LabelDecl()(SourceNameOrigin(convert(label), origin(stat))))
    case PvlForStatement(inner, _) => convert(inner)
  }

  def convert(implicit stat: ForStatementListContext): Statement =
    Block(convertList(stat))

  def convertList(implicit stat: ForStatementListContext): Seq[Statement] = stat match {
    case ForStatementList0(stat) => Seq(convert(stat))
    case ForStatementList1(stat, _, stats) => convert(stat) +: convertList(stats)
  }

  def convert(implicit stat: AllowedForStatementContext): Statement = stat match {
    case PvlLocal(t, decls) =>
      Block(convert(decls, convert(t)))
    case PvlEval(e) => Eval(convert(e))
    case PvlIncDec(name, op) =>
      val target = PVLLocal(convert(name))
      Eval(op match {
        case "++" => PostAssignExpression(target, target + 1)
        case "--" => PostAssignExpression(target, target - 1)
      })
    case PvlAssign(target, _, value) => Assign(convert(target), convert(value))
  }

  def convert(implicit decls: DeclListContext, t: Type): Seq[Statement] = decls match {
    case DeclList0(name, None) =>
      Seq(LocalDecl(new Variable(t)(SourceNameOrigin(convert(name), origin(name)))))
    case DeclList0(name, Some(DeclInit0(_, init))) =>
      val v = new Variable(t)(SourceNameOrigin(convert(name), origin(name)))
      Seq(
        LocalDecl(v),
        Assign(Local(v.ref), convert(init))
      )
    case DeclList1(name, None, _, more) =>
      LocalDecl(new Variable(t)(SourceNameOrigin(convert(name), origin(name)))) +:
        convert(more, t)
    case DeclList1(name, Some(DeclInit0(_, init)), _, more) =>
      val v = new Variable(t)(SourceNameOrigin(convert(name), origin(name)))
      Seq(
        LocalDecl(v),
        Assign(Local(v.ref), convert(init))
      ) ++ convert(more, t)
  }

  def convert(implicit block: BlockContext): Statement = block match {
    case Block0(_, stats, _) => Scope(Nil, Block(stats.map(convert(_))))
  }

  def convert(implicit tags: BarrierTagsContext): Seq[Ref[ParInvariantDecl]] = tags match {
    case BarrierTags0(_, ids) =>
      convert(ids).map(new UnresolvedRef[ParInvariantDecl](_))
  }

  def convert(implicit pars: ParUnitListContext): Seq[ParBlock] = pars match {
    case ParUnitList0(par) => Seq(convert(par))
    case ParUnitList1(par, _, pars) => convert(par) +: convert(pars)
  }

  def convert(implicit par: ParUnitContext): ParBlock = par match {
    case ParUnit0(name, _, iters, wait, _, contract, block) =>
      val decl = name match {
        case None => new ParBlockDecl()
        case Some(name) => new ParBlockDecl()(SourceNameOrigin(convert(name), origin(name)))
      }

      withContract(contract, contract =>
        ParBlock(
          decl,
          wait.map(convert(_)).getOrElse(Nil),
          iters.map(convert(_)).getOrElse(Nil),
          Star.fold(contract.consume(contract.requires)),
          Star.fold(contract.consume(contract.ensures)),
          convert(block),
        ))
    case ParUnit1(contract, block) =>
      withContract(contract, contract =>
        ParBlock(
          new ParBlockDecl(),
          after = Nil,
          iters = Nil,
          Star.fold(contract.consume(contract.requires)),
          Star.fold(contract.consume(contract.ensures)),
          convert(block)
        ))
  }

  def convert(implicit iters: ItersContext): Seq[IterVariable] = iters match {
    case Iters0(iter) => Seq(convert(iter))
    case Iters1(iter, _, iters) => convert(iter) +: convert(iters)
  }

  def convert(implicit iter: IterContext): IterVariable = iter match {
    case Iter0(t, name, _, from, _, to) =>
      IterVariable(
        new Variable(convert(t))(SourceNameOrigin(convert(name), origin(name))),
        convert(from), convert(to)
      )
  }

  def convert(implicit wait: ParWaitListContext): Seq[Ref[ParBlockDecl]] = ??(wait)

  def convert(implicit t: TypeContext): Type = t match {
    case Type0(inner, dims) =>
      FuncTools.repeat(TArray(_), dims.map(convert(_)).getOrElse(0), convert(inner))
  }

  def convert(implicit dims: TypeDimsContext): Int = dims match {
    case TypeDims0(dims) => dims.size
    case TypeDims1(dims) => dims.size
  }

  def convert(implicit t: NonArrayTypeContext): Type = t match {
    case NonArrayType0(inner) => convert(inner)
    case NonArrayType1(name) => name match {
      case "string" => TString()
      case "int" => TInt()
      case "boolean" => TBool()
      case "void" => TVoid()
    }
    case NonArrayType2(inner) => convert(inner)
  }

  def convert(implicit t: ClassTypeContext): Type = t match {
    case ClassType0(name, typeArgs) => PVLNamedType(convert(name))
  }

  def convert(implicit ids: IdentifierListContext): Seq[String] = ids match {
    case IdentifierList0(id) => Seq(convert(id))
    case IdentifierList1(id, _, ids) => convert(id) +: convert(ids)
  }

  def convert(implicit id: IdentifierContext): String = id match {
    case Identifier0(id) => id
    case Identifier1(ValIdEscape(id)) => id.substring(1, id.length - 1)
    case Identifier1(reserved) =>
      fail(reserved, "This identifier is reserved and cannot be declared.")
  }

  def convertExpr(implicit id: IdentifierContext): Expr = id match {
    case Identifier0(name) => PVLLocal(name)
    case Identifier1(reserved) => convert(reserved)
  }

  def withContract[T](node: ContractContext, f: ContractCollector => T): T = node match {
    case Contract0(clauses) =>
      val collector = new ContractCollector()
      clauses.foreach(convert(_, collector))
      withCollector(collector, f)
  }

  def convert(implicit decl: LangGlobalDeclContext): Seq[GlobalDeclaration] = Nil
  def convert(implicit decl: LangClassDeclContext): Seq[ClassDeclaration] = Nil

  def convert(implicit stat: LangStatementContext): Statement = stat match {
    case LangStatement0(block) => convert(block)
  }

  def convert(implicit t: LangTypeContext): Type = t match {
    case LangType0(t) => convert(t)
  }

  def convert(implicit e: LangExprContext): Expr = e match {
    case LangExpr0(e) => convert(e)
  }

  def convert(implicit id: LangIdContext): String = id match {
    case LangId0(id) => convert(id)
  }

  def withCollector[T](collector: ContractCollector, f: ContractCollector => T): T = {
    val result = f(collector)
    collector.nodes.headOption match {
      case Some(node) => fail(node, "This specification clause may not occur here")
      case None => result
    }
  }

  def withContract[T](node: Option[ValEmbedContractContext], f: ContractCollector => T): T = {
    val collector = new ContractCollector()
    node.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withContract[T](node1: Option[ValEmbedContractContext], node2: Option[ValEmbedContractContext], f: ContractCollector => T): T = {
    val collector = new ContractCollector()
    node1.foreach(convert(_, collector))
    node2.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withContract[T](node: Seq[ValContractClauseContext], f: ContractCollector => T): T = {
    val collector = new ContractCollector()
    node.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withCollector[T](collector: ModifierCollector, f: ModifierCollector => T): T = {
    val result = f(collector)
    collector.nodes.headOption match {
      case Some(node) => fail(node, "This modifier cannot be attached to this declaration")
      case None => result
    }
  }

  def withModifiers[T](node: Seq[ValModifierContext], f: ModifierCollector => T): T = {
    val collector = new ModifierCollector()
    node.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withModifiers[T](node: ValEmbedModifierContext, f: ModifierCollector => T): T = {
    val collector = new ModifierCollector()
    convert(node, collector)
    withCollector(collector, f)
  }

  def convert(contract: ValEmbedContractContext, collector: ContractCollector): Unit = contract match {
    case ValEmbedContract0(blocks) => blocks.foreach(convert(_, collector))
  }

  def convert(contract: ValEmbedContractBlockContext, collector: ContractCollector): Unit = contract match {
    case ValEmbedContractBlock0(_, clauses, _) => clauses.foreach(convert(_, collector))
    case ValEmbedContractBlock1(clauses) => clauses.foreach(convert(_, collector))
  }

  def convert(contract: ValContractClauseContext, collector: ContractCollector): Unit = contract match {
    case ValContractClause0(_, ids, _) => collector.modifies ++= convert(ids).map((contract, _))
    case ValContractClause1(_, ids, _) => collector.accessible ++= convert(ids).map((contract, _))
    case ValContractClause2(_, exp, _) => collector.requires += ((contract, convert(exp)))
    case ValContractClause3(_, exp, _) => collector.ensures += ((contract, convert(exp)))
    case ValContractClause4(_, t, id, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(contract)))
      collector.given += ((contract, variable))
    case ValContractClause5(_, t, id, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(contract)))
      collector.yields += ((contract, variable))
    case ValContractClause6(_, exp, _) => collector.context_everywhere += ((contract, convert(exp)))
    case ValContractClause7(_, exp, _) =>
      val specification = (contract, convert(exp))
      collector.requires += specification
      collector.ensures += specification
    case ValContractClause8(_, exp, _) => collector.loop_invariant += ((contract, convert(exp)))
    case ValContractClause9(_, exp, _) => collector.kernel_invariant += ((contract, convert(exp)))
    case ValContractClause10(_, _, t, id, _, exp, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(contract)))
      collector.signals += ((contract, SignalsClause(variable, convert(exp))(originProvider(contract))))
  }

  def convert(mod: ValEmbedModifierContext, collector: ModifierCollector): Unit = mod match {
    case ValEmbedModifier0(_, mod, _) => convert(mod, collector)
    case ValEmbedModifier1(mod) => convert(mod, collector)
  }

  def convert(mod: ValModifierContext, collector: ModifierCollector): Unit = mod match {
    case ValModifier0(name) => name match {
      case "pure" => collector.pure += mod
      case "inline" => collector.inline += mod
      case "thread_local" => collector.threadLocal += mod
    }
    case ValStatic(_) => collector.static += mod
  }

  def convertEmbedWith(implicit whiff: Option[ValEmbedWithContext], inner: Expr): Expr = whiff match {
    case None => inner
    case Some(ValEmbedWith0(_, whiff, _)) => convertWith(whiff, inner)
    case Some(ValEmbedWith1(whiff)) => convertWith(Some(whiff), inner)
  }

  def convertWith(implicit whiff: Option[ValWithContext], inner: Expr): Expr = whiff match {
    case None => inner
    case Some(whiff @ ValWith0(_, stat)) => With(convert(stat), inner)(origin(whiff))
  }

  def convertEmbedThen(implicit den: Option[ValEmbedThenContext], inner: Expr): Expr = den match {
    case None => inner
    case Some(ValEmbedThen0(_, den, _)) => convertThen(den, inner)
    case Some(ValEmbedThen1(den)) => convertThen(Some(den), inner)
  }

  def convertThen(implicit den: Option[ValThenContext], inner: Expr): Expr = den match {
    case None => inner
    case Some(den @ ValThen0(_, stat)) => Then(inner, convert(stat))(origin(den))
  }

  def convert(implicit whiff: ValEmbedWithContext): Statement = whiff match {
    case ValEmbedWith0(_, Some(whiff), _) => convert(whiff)
    case ValEmbedWith0(_, None, _) => Block(Nil)
    case ValEmbedWith1(whiff) => convert(whiff)
  }

  def convert(implicit whiff: ValWithContext): Statement = whiff match {
    case ValWith0(_, stat) => convert(stat)
  }

  def convert(implicit whiff: ValEmbedThenContext): Statement = whiff match {
    case ValEmbedThen0(_, Some(whiff), _) => convert(whiff)
    case ValEmbedThen0(_, None, _) => Block(Nil)
    case ValEmbedThen1(whiff) => convert(whiff)
  }

  def convert(implicit whiff: ValThenContext): Statement = whiff match {
    case ValThen0(_, stat) => convert(stat)
  }

  def convertEmbedGiven(implicit given: Option[ValEmbedGivenContext]): Seq[(String, Expr)] = given match {
    case None => Nil
    case Some(ValEmbedGiven0(_, inner, _)) => convertGiven(inner)
    case Some(ValEmbedGiven1(inner)) => convertGiven(Some(inner))
  }

  def convertGiven(implicit given: Option[ValGivenContext]): Seq[(String, Expr)] = given match {
    case None => Nil
    case Some(ValGiven0(_, _, mappings, _)) => convert(mappings)
  }

  def convert(implicit mappings: ValGivenMappingsContext): Seq[(String, Expr)] = mappings match {
    case ValGivenMappings0(arg, _, v) => Seq((convert(arg), convert(v)))
    case ValGivenMappings1(arg, _, v, _, more) => (convert(arg), convert(v)) +: convert(more)
  }

  def convertEmbedYields(implicit given: Option[ValEmbedYieldsContext]): Seq[(Expr, String)] = given match {
    case None => Nil
    case Some(ValEmbedYields0(_, inner, _)) => convertYields(inner)
    case Some(ValEmbedYields1(inner)) => convertYields(Some(inner))
  }

  def convertYields(implicit given: Option[ValYieldsContext]): Seq[(Expr, String)] = given match {
    case None => Nil
    case Some(ValYields0(_, _, mappings, _)) => convert(mappings)
  }

  def convert(implicit mappings: ValYieldsMappingsContext): Seq[(Expr, String)] = mappings match {
    case ValYieldsMappings0(target, _, res) => Seq((convert(target), convert(res)))
    case ValYieldsMappings1(target, _, res, _, more) => (convert(target), convert(res)) +: convert(more)
  }

  def convert(implicit exprs: ValExpressionListContext): Seq[Expr] = exprs match {
    case ValExpressionList0(expr) => Seq(convert(expr))
    case ValExpressionList1(head, _, tail) => convert(head) +: convert(tail)
  }

  def convert(implicit ids: ValIdListContext): Seq[String] = ids match {
    case ValIdList0(id) => Seq(convert(id))
    case ValIdList1(id, _, ids) => convert(id) +: convert(ids)
  }

  def convert(implicit impOp: ValImpOpContext, left: Expr, right: Expr): Expr = impOp match {
    case ValImpOp0(_) => Wand(left, right)(origin(impOp))
    case ValImpOp1(_) => Implies(left, right)(origin(impOp))
  }

  def convert(implicit andOp: ValAndOpContext, left: Expr, right: Expr): Expr = andOp match {
    case ValAndOp0(_) => col.Star(left, right)(origin(andOp))
  }

  def convert(implicit inOp: ValInOpContext, left: Expr, right: Expr): Expr = inOp match {
    case ValInOp0(_) => AmbiguousMember(left, right)
  }

  def convert(implicit mulOp: ValMulOpContext, left: Expr, right: Expr): Expr = mulOp match {
    case ValMulOp0(_) => col.Div(left, right)(blame(mulOp))
  }

  def convert(implicit prependOp: ValPrependOpContext, left: Expr, right: Expr): Expr = prependOp match {
    case ValPrependOp0(_) => Cons(left, right)
  }

  def convert(implicit postfixOp: ValPostfixContext, xs: Expr): Expr = postfixOp match {
    case ValPostfix0(_, _, to, _) => Take(xs, convert(to))
    case ValPostfix1(_, from, _, None, _) => Drop(xs, convert(from))
    case ValPostfix1(_, from, _, Some(to), _) => Slice(xs, convert(from), convert(to))
    case ValPostfix2(_, idx, _, v, _) => SeqUpdate(xs, convert(idx), convert(v))
  }

  def convert(implicit block: ValEmbedStatementBlockContext): Block = block match {
    case ValEmbedStatementBlock0(_, stats, _) => Block(stats.map(convert(_)))
    case ValEmbedStatementBlock1(stats) => Block(stats.map(convert(_)))
  }

  def convert(implicit stat: ValStatementContext): Statement = stat match {
    case ValCreateWand(_, block) => WandCreate(convert(block))
    case ValQedWand(_, wand, _) => WandQed(convert(wand))
    case ValApplyWand(_, wand, _) => WandApply(convert(wand))
    case ValUseWand(_, wand, _) => WandUse(convert(wand))
    case ValFold(_, predicate, _) =>
      Fold(convert(predicate))
    case ValUnfold(_, predicate, _) =>
      Unfold(convert(predicate))
    case ValOpen(_, _, _) => ??(stat)
    case ValClose(_, _, _) => ??(stat)
    case ValAssert(_, assn, _) => Assert(convert(assn))(blame(stat))
    case ValAssume(_, assn, _) => Assume(convert(assn))
    case ValInhale(_, resource, _) => Inhale(convert(resource))
    case ValExhale(_, resource, _) => Exhale(convert(resource))(blame(stat))
    case ValLabel(_, label, _) =>
      Label(new LabelDecl()(SourceNameOrigin(convert(label), origin(stat))))
    case ValRefute(_, assn, _) => ??(stat)
    case ValWitness(_, _, _) => ??(stat)
    case ValGhost(_, stat) => convert(stat)
    case ValSend(_, resource, _, label, _, offset, _) =>
      Send(convert(resource), new UnresolvedRef[LabelDecl](convert(label)), convert(offset))
    case ValRecv(_, resource, _, label, _, offset, _) =>
      Recv(convert(resource), new UnresolvedRef[LabelDecl](convert(label)), convert(offset))
    case ValTransfer(_, _, _) => ??(stat)
    case ValCslSubject(_, _, _) => ??(stat) // FIXME PB: csl_subject seems to be used
    case ValSpecIgnoreStart(_, _) => SpecIgnoreEnd()
    case ValSpecIgnoreEnd(_, _) => SpecIgnoreStart()
    case ValActionModel(_, _, model, _, perm, _, after, _, action, _, impl) =>
      ModelDo(convert(model), convert(perm), convert(after), convert(action), impl match {
        case ValActionImpl0(_) => Block(Nil)
        case ValActionImpl1(inner) => convert(inner)
      })
    case ValAtomic(_, _, invariant, _, body) =>
      ParAtomic(Seq(new UnresolvedRef[ParInvariantDecl](convert(invariant))), convert(body))
  }

  def convert(implicit block: ValBlockContext): Seq[Statement] = block match {
    case ValBlock0(_, stats, _) => stats.map(convert(_))
  }

  def convert(implicit arg: ValArgContext): Variable = arg match {
    case ValArg0(t, id) => new Variable(convert(t))(SourceNameOrigin(convert(id), origin(arg)))
  }

  def convert(implicit args: ValArgListContext): Seq[Variable] = args match {
    case ValArgList0(arg) => Seq(convert(arg))
    case ValArgList1(arg, _, args) => convert(arg) +: convert(args)
  }

  def convert(implicit decl: ValEmbedGlobalDeclarationBlockContext): Seq[GlobalDeclaration] = decl match {
    case ValEmbedGlobalDeclarationBlock0(_, globals, _) => globals.flatMap(convert(_))
    case ValEmbedGlobalDeclarationBlock1(globals) => globals.flatMap(convert(_))
  }

  def convert(implicit decl: ValGlobalDeclarationContext): Seq[GlobalDeclaration] = decl match {
    case ValAxiom(_, name, _, axiom, _) =>
      Seq(new SimplificationRule(convert(axiom))(SourceNameOrigin(convert(name), origin(decl))))
    case ValPredicate(modifiers, _, name, _, args, _, definition) =>
      withModifiers(modifiers, mods =>
        Seq(new Predicate(args.map(convert(_)).getOrElse(Nil), convert(definition),
          mods.consume(mods.threadLocal), mods.consume(mods.inline))
        (SourceNameOrigin(convert(name), origin(decl)))))
    case ValFunction(contract, modifiers, _, t, name, _, args, _, definition) =>
      Seq(withContract(contract, c =>
        withModifiers(modifiers, m => {
          val namedOrigin = SourceNameOrigin(convert(name), origin(decl))
          new Function(convert(t), args.map(convert(_)).getOrElse(Nil), convert(definition),
            c.consumeApplicableContract(), m.consume(m.inline))(blame(decl))(namedOrigin)
        })
      ))
    case ValModel(_, name, _, decls, _) =>
      Seq(new Model(decls.map(convert(_)))(SourceNameOrigin(convert(name), origin(decl))))
    case ValGhostDecl(_, inner) =>
      convert(inner)
  }

  def convert(implicit decl: ValEmbedClassDeclarationBlockContext): Seq[ClassDeclaration] = decl match {
    case ValEmbedClassDeclarationBlock0(_, decls, _) => decls.flatMap(convert(_, x => x))
    case ValEmbedClassDeclarationBlock1(decls) => decls.flatMap(convert(_, x => x))
  }

  def convert[T](implicit decl: ValClassDeclarationContext, transform: ClassDeclaration => T): Seq[T] = decl match {
    case ValInstancePredicate(modifiers, _, name, _, args, _, definition) =>
      Seq(withModifiers(modifiers, mods => {
        transform(new InstancePredicate(args.map(convert(_)).getOrElse(Nil), convert(definition),
          mods.consume(mods.threadLocal), mods.consume(mods.inline))(
          SourceNameOrigin(convert(name), origin(decl))))
      }))
    case ValInstanceFunction(contract, modifiers, _, t, name, _, args, _, definition) =>
      Seq(withContract(contract, c => {
        withModifiers(modifiers, m => {
          transform(new InstanceFunction(convert(t), args.map(convert(_)).getOrElse(Nil), convert(definition),
            c.consumeApplicableContract(), m.consume(m.inline))(
            blame(decl))(
            SourceNameOrigin(convert(name), origin(decl))))
        })
      }))
    case ValInstanceGhostDecl(_, decl) => convert(decl).map(transform)
  }

  def convert(implicit decl: ValModelDeclarationContext): ModelDeclaration = decl match {
    case ValModelField(t, name, _) =>
      new ModelField(convert(t))(SourceNameOrigin(convert(name), origin(decl)))
    case ValModelProcess(contract, _, name, _, args, _, _, definition, _) =>
      withContract(contract, c => {
        new ModelProcess(args.map(convert(_)).getOrElse(Nil), convert(definition),
          col.And.fold(c.consume(c.requires)), col.And.fold(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef[ModelField](_)), c.consume(c.accessible).map(new UnresolvedRef[ModelField](_)))(
          SourceNameOrigin(convert(name), origin(decl)))
      })
    case ValModelAction(contract, _, name, _, args, _, _) =>
      withContract(contract, c => {
        new ModelAction(args.map(convert(_)).getOrElse(Nil),
          col.And.fold(c.consume(c.requires)), col.And.fold(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef[ModelField](_)), c.consume(c.accessible).map(new UnresolvedRef[ModelField](_)))(
          SourceNameOrigin(convert(name), origin(decl)))
      })
  }

  def convert(implicit definition: ValDefContext): Option[Expr] = definition match {
    case ValAbstractBody(_) => None
    case ValBody(_, expr, _) => Some(convert(expr))
  }

  def convert(implicit t: ValTypeContext): Type = t match {
    case ValPrimaryType(name) => name match {
      case "resource" => TResource()
      case "process" => TProcess()
      case "frac" => TFraction()
      case "zfrac" => TZFraction()
      case "rational" => TRational()
      case "bool" => TBool()
    }
    case ValSeqType(_, _, element, _) => TSeq(convert(element))
    case ValSetType(_, _, element, _) => TSet(convert(element))
    case ValBagType(_, _, element, _) => TBag(convert(element))
    case ValOptionType(_, _, element, _) => TOption(convert(element))
    case ValMapType(_, _, key, _, value, _) => TMap(convert(key), convert(value))
    case ValTupleType(_, _, t1, _, t2, _) => TTuple(Seq(convert(t1), convert(t2)))
    case ValPointerType(_, _, element, _) => TPointer(convert(element))
  }

  def convert(implicit e: ValPrimarySeqContext): Expr = e match {
    case ValCardinality(_, xs, _) => Size(convert(xs))
    case ValArrayValues(_, _, a, _, from, _, to, _) => Values(convert(a), convert(from), convert(to))
  }

  def convert(implicit e: ValPrimaryOptionContext): Expr = e match {
    case ValSome(_, _, v, _) => OptSome(convert(v))
  }

  // valsetcompselectors
  // valMapPairs

  def convert(implicit e: ValPrimaryCollectionConstructorContext): Expr = e match {
    case ValTypedLiteralSeq(_, _, t, _, _, exprs, _) => LiteralSeq(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValTypedLiteralSet(_, _, t, _, _, exprs, _) => LiteralSet(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValSetComprehension(_, _, t, _, _, value, _, selectors, _, something, _) => ??(e)
    case ValTypedLiteralBag(_, _, t, _, _, exprs, _) => LiteralBag(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValTypedLiteralMap(_, _, key, _, value, _, _, pairs, _) => ??(e)
    case ValTypedTuple(_, _, t1, _, t2, _, _, v1, _, v2, _) => ??(e)
    case ValLiteralSeq(_, exprs, _) => UntypedLiteralSeq(convert(exprs))
    case ValLiteralSet(_, exprs, _) => UntypedLiteralSet(convert(exprs))
    case ValLiteralBag(_, exprs, _) => UntypedLiteralBag(convert(exprs))
    case ValEmptySeq(_, t, _) => LiteralSeq(convert(t), Nil)
    case ValEmptySet(_, t, _) => LiteralSet(convert(t), Nil)
    case ValEmptyBag(_, t, _) => LiteralBag(convert(t), Nil)
    case ValRange(_, from, _, to, _) => Range(convert(from), convert(to))
  }

  def convert(implicit e: ValPrimaryPermissionContext): Expr = e match {
    case ValCurPerm(_, _, loc, _) => CurPerm(convert(loc))
    case ValPerm(_, _, loc, _, perm, _) => Perm(convert(loc), convert(perm))
    case ValValue(_, _, loc, _) => Perm(convert(loc), ReadPerm())
    case ValPointsTo(_, _, loc, _, perm, _, v, _) => PointsTo(convert(loc), convert(perm), convert(v))
    case ValHPerm(_, _, loc, _, perm, _) => HPerm(convert(loc), convert(perm))
    case ValAPerm(_, _, loc, _, perm, _) => APerm(convert(loc), convert(perm))
    case ValArrayPerm(_, _, arr, _, i, _, step, _, count, _, perm, _) => ??(e)
    case ValMatrix(_, _, m, _, dim1, _, dim2, _) => ValidMatrix(convert(m), convert(dim1), convert(dim2))
    case ValArray(_, _, arr, _, dim, _) => ValidArray(convert(arr), convert(dim))
    case ValPointer(_, _, ptr, _, n, _, perm, _) => PermPointer(convert(ptr), convert(n), convert(perm))
    case ValPointerIndex(_, _, ptr, _, idx, _, perm, _) => PermPointerIndex(convert(ptr), convert(idx), convert(perm))
  }

  def convert(implicit e: ValPrimaryBinderContext): Expr = e match {
    case ValRangeQuantifier(_, quant, t, id, _, from, _, to, _, body, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id)))
      val cond = SeqMember(Local(variable.ref), Range(convert(from), convert(to)))
      quant match {
        case "\\forall*" => Starall(Seq(variable), Nil, Implies(cond, convert(body)))
        case "\\forall" => Forall(Seq(variable), Nil, Implies(cond, convert(body)))
        case "\\exists" => Exists(Seq(variable), Nil, col.And(cond, convert(body)))
      }
    case ValQuantifier(_, quant, t, id, _, cond, _, body, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id)))
      quant match {
        case "\\forall*" => Starall(Seq(variable), Nil, Implies(convert(cond), convert(body)))
        case "\\forall" => Forall(Seq(variable), Nil, Implies(convert(cond), convert(body)))
        case "\\exists" => Exists(Seq(variable), Nil, col.And(convert(cond), convert(body)))
      }
    case ValLet(_, _, t, id, _, v, _, body, _) =>
      Let(new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id))), convert(v), convert(body))
  }

  def convert(implicit e: ValPrimaryVectorContext): Expr = e match {
    case ValSum(_, _, t, id, _, cond, _, body, _) =>
      val binding = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id)))
      Sum(Seq(binding), convert(cond), convert(body))
    case ValVectorSum(_, _, rng, _, vec, _) => VectorSum(convert(rng), convert(vec))
    case ValVectorCmp(_, _, left, _, right, _) => VectorCompare(convert(left), convert(right))
    case ValVectorRep(_, _, inner, _) => VectorRepeat(convert(inner))
    case ValMatrixSum(_, _, rng, _, mat, _) => MatrixSum(convert(rng), convert(mat))
    case ValMatrixCmp(_, _, left, _, right, _) => MatrixCompare(convert(left), convert(right))
    case ValMatrixRep(_, _, inner, _) => MatrixRepeat(convert(inner))
  }

  def convert(implicit e: ValPrimaryReducibleContext): Expr = ??(e)

  def convert(implicit e: ValPrimaryThreadContext): Expr = e match {
    case ValIdle(_, _, thread, _) => IdleToken(convert(thread))
    case ValRunning(_, _, thread, _) => JoinToken(convert(thread))
  }

  def convert(implicit e: ValPrimaryContext): Expr = e match {
    case ValPrimary0(inner) => convert(inner)
    case ValPrimary1(inner) => convert(inner)
    case ValPrimary2(inner) => convert(inner)
    case ValPrimary3(inner) => convert(inner)
    case ValPrimary4(inner) => convert(inner)
    case ValPrimary5(inner) => convert(inner)
    case ValPrimary6(inner) => convert(inner)
    case ValPrimary7(inner) => convert(inner)
    case ValAny(_) => Any()
    case ValIndependent(_, e, _, name, _) => ??(e)
    case ValScale(_, perm, _, predInvocation) => Scale(convert(perm), convert(predInvocation))
    case ValInlinePattern(_, pattern, _) => InlinePattern(convert(pattern))
    case ValUnfolding(_, predExpr, _, body) => Unfolding(convert(predExpr), convert(body))
    case ValOld(_, _, expr, _) => Old(convert(expr), at = None)(blame(e))
    case ValTypeof(_, _, expr, _) => TypeOf(convert(expr))
    case ValHeld(_, _, obj, _) => Held(convert(obj))
  }

  def convert(implicit res: ValReservedContext): Expr = res match {
    case ValReserved0(name) => fail(res,
      f"This identifier is reserved, and cannot be declared or used in specifications. " +
        f"You might want to escape the identifier with backticks: `$name`")
    case ValIdEscape(id) => Local(new UnresolvedRef[Variable](id.substring(1, id.length-1)))
    case ValResult(_) => AmbiguousResult()
    case ValCurrentThread(_) => CurrentThreadId()
    case ValNonePerm(_) => NoPerm()
    case ValWrite(_) => WritePerm()
    case ValRead(_) => ReadPerm()
    case ValNoneOption(_) => OptNone()
    case ValEmpty(_) => EmptyProcess()
    case ValLtid(_) => ???
    case ValGtid(_) => ???
    case ValTrue(_) => true
    case ValFalse(_) => false
  }
}
