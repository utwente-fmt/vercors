package vct.parsers.transform

import org.antlr.v4.runtime.{ParserRuleContext, Token}
import vct.col.ast._
import vct.col.origin._
import vct.antlr4.generated.PVLParser._
import vct.antlr4.generated.PVLParserPatterns._
import vct.col.{ast => col}
import vct.antlr4.generated.{PVLParserPatterns => parse}
import vct.col.util.AstBuildHelpers._
import hre.util.FuncTools
import vct.col.ref.{Ref, UnresolvedRef}
import vct.col.resolve.lang.PVL
import vct.col.util.AstBuildHelpers

import scala.annotation.nowarn
import scala.collection.mutable

@nowarn("msg=match may not be exhaustive&msg=Some\\(")
case class PVLToCol[G](override val originProvider: OriginProvider, override val blameProvider: BlameProvider, override val errors: Seq[(Token, Token, ExpectedError)])
  extends ToCol[G](originProvider, blameProvider, errors) {
  def convert(implicit program: ProgramContext): Seq[GlobalDeclaration[G]] = program match {
    case Program0(decls, _, _) => decls.flatMap(convert(_))
  }

  def convert(implicit decl: ProgramDeclContext): Seq[GlobalDeclaration[G]] = decl match {
    case ProgramDecl0(valDecl) => convert(valDecl)
    case ProgramDecl1(cls) => Seq(convert(cls))
    case ProgramDecl2(method) => Seq(convertProcedure(method))
  }

  def convertProcedure(implicit method: MethodContext): Procedure[G] = method match {
    case Method0(contract, modifiers, returnType, name, _, args, _, body) =>
      withModifiers(modifiers, mods => withContract(contract, contract => {
        new Procedure(
          convert(returnType),
          args.map(convert(_)).getOrElse(Nil),
          outArgs = Nil,
          typeArgs = Nil,
          convert(body),
          contract.consumeApplicableContract(blame(method)),
          inline = mods.consume(mods.inline),
          pure = mods.consume(mods.pure),
        )(blame(method))(SourceNameOrigin(convert(name), origin(method)))
      }))
  }

  def convert(implicit cls: DeclClassContext): Class[G] = cls match {
    case DeclClass0(contract, _, name, _, decls, _) =>
      withContract(contract, contract => {
        new Class(
          declarations = decls.flatMap(convert(_)),
          supports = Nil,
          intrinsicLockInvariant = AstBuildHelpers.foldStar(contract.consume(contract.lock_invariant)),
        )(SourceNameOrigin(convert(name), origin(cls)))
      })
  }

  def convert(implicit decl: ClassDeclContext): Seq[ClassDeclaration[G]] = decl match {
    case ClassDecl0(inner) => convert(inner, x => x)
    case ClassDecl1(inner) => convert(inner)
    case ClassDecl2(inner) => Seq(convert(inner))
    case ClassDecl3(inner) => convert(inner)
    case ClassDecl4(inner) => convert(inner)
  }

  def convert(implicit method: MethodContext): InstanceMethod[G] = method match {
    case Method0(contract, modifiers, returnType, name, _, args, _, body) =>
      withModifiers(modifiers, mods => withContract(contract, contract => {
        new InstanceMethod(
          convert(returnType),
          args.map(convert(_)).getOrElse(Nil),
          outArgs = Nil,
          typeArgs = Nil,
          convert(body),
          contract.consumeApplicableContract(blame(method)),
          inline = mods.consume(mods.inline),
          pure = mods.consume(mods.pure),
        )(blame(method))(SourceNameOrigin(convert(name), origin(method)))
      }))
  }

  def convert(implicit body: MethodBodyContext): Option[Statement[G]] = body match {
    case MethodBody0(_) => None
    case MethodBody1(stat) => Some(convert(stat))
  }

  def convert(implicit constructor: ConstructorContext): Seq[ClassDeclaration[G]] = constructor match {
    case Constructor0(contract, _, _, args, _, body) =>
      Seq(withContract(contract, contract =>
        new PVLConstructor(contract.consumeApplicableContract(blame(constructor)), args.map(convert(_)).getOrElse(Nil), convert(body))(blame(constructor))))
  }

  def convert(implicit finalFlag: FinalFlagContext): FieldFlag[G] = finalFlag match {
    case FinalFlag0(_) => Final()
  }

  def convert(implicit field: FieldContext): Seq[InstanceField[G]] = field match {
    case Field0(finalFlag, t, ids, _) =>
      convert(ids).map(name => new InstanceField[G](convert(t), finalFlag.map(convert(_)).toSet)(SourceNameOrigin(name, origin(field))))
  }

  def convert(implicit method: RunMethodContext): Seq[RunMethod[G]] = method match {
    case RunMethod0(contract, _, maybeBody) =>
      withContract(contract, contract =>
        Seq(new RunMethod(convert(maybeBody), contract.consumeApplicableContract(blame(method)))(blame(method)))
      )
  }

  def convert(implicit args: ArgsContext): Seq[Variable[G]] = args match {
    case Args0(t, name) => Seq(new Variable(convert(t))(SourceNameOrigin(convert(name), origin(name))))
    case Args1(t, name, _, args) =>
      new Variable(convert(t))(SourceNameOrigin(convert(name), origin(name))) +: convert(args)
  }

  def convert(implicit exprs: ExprListContext): Seq[Expr[G]] = exprs match {
    case ExprList0(e) => Seq(convert(e))
    case ExprList1(e, _, es) => convert(e) +: convert(es)
  }

  def convert(implicit tuple: TupleContext): Seq[Expr[G]] = tuple match {
    case Tuple0(_, exprs, _) => exprs.map(convert(_)).getOrElse(Nil)
  }

  def convert(implicit exprs: NewDimsContext): Seq[Expr[G]] = exprs match {
    case NewDims0(dims) => dims.map(convert(_))
  }

  def convert(implicit exprs: InvariantListContext): Expr[G] = exprs match {
    case InvariantList0(invs) => AstBuildHelpers.foldStar(invs.map(convert(_)))
  }

  def convert(implicit dim: QuantifiedDimContext): Expr[G] = dim match {
    case QuantifiedDim0(_, inner, _) => convert(inner)
  }

  def convert(implicit inv: InvariantContext): Expr[G] = inv match {
    case Invariant0(_, inv, _) => convert(inv)
  }

  def convert(implicit expr: ExprContext): Expr[G] = expr match {
    case Expr0(pre, inner, post) =>
      convertWith(pre, convertThen(post, convert(inner)))
  }

  def convert(implicit expr: UnfoldingExprContext): Expr[G] = expr match {
    case UnfoldingExpr0(_, pred, _, body) => Unfolding(convert(pred), convert(body))(blame(expr))
    case UnfoldingExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: IteExprContext): Expr[G] = expr match {
    case IteExpr0(cond, _, whenTrue, _, whenFalse) => Select(convert(cond), convert(whenTrue), convert(whenFalse))
    case IteExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: ImplicationExprContext): Expr[G] = expr match {
    case ImplicationExpr0(left, specOp, right) => convert(expr, specOp, convert(left), convert(right))
    case ImplicationExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: OrExprContext): Expr[G] = expr match {
    case OrExpr0(left, _, right) => AmbiguousOr(convert(left), convert(right))
    case OrExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: AndExprContext): Expr[G] = expr match {
    case AndExpr0(left, _, right) => And(convert(left), convert(right))
    case AndExpr1(left, specOp, right) => convert(expr, specOp, convert(left), convert(right))
    case AndExpr2(inner) => convert(inner)
  }

  def convert(implicit expr: EqExprContext): Expr[G] = expr match {
    case EqExpr0(left, _, right) => Eq(convert(left), convert(right))
    case EqExpr1(left, _, right) => Neq(convert(left), convert(right))
    case EqExpr2(inner) => convert(inner)
  }

  def convert(implicit expr: RelExprContext): Expr[G] = expr match {
    case RelExpr0(left, _, right) => AmbiguousLess(convert(left), convert(right))
    case RelExpr1(left, _, right) => AmbiguousLessEq(convert(left), convert(right))
    case RelExpr2(left, _, right) => AmbiguousGreaterEq(convert(left), convert(right))
    case RelExpr3(left, _, right) => AmbiguousGreater(convert(left), convert(right))
    case RelExpr4(left, specOp, right) => convert(expr, specOp, convert(left), convert(right))
    case RelExpr5(inner) => convert(inner)
  }

  def convert(implicit expr: SetExprContext): Expr[G] = expr match {
    case SetExpr0(x, _, xs) => AmbiguousMember(convert(x), convert(xs))
    case SetExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: AddExprContext): Expr[G] = expr match {
    case AddExpr0(left, _, right) => AmbiguousPlus(convert(left), convert(right))(blame(expr))
    case AddExpr1(left, _, right) => AmbiguousMinus(convert(left), convert(right))(blame(expr))
    case AddExpr2(inner) => convert(inner)
  }

  def convert(implicit expr: MultExprContext): Expr[G] = expr match {
    case MultExpr0(left, _, right) => AmbiguousMult(convert(left), convert(right))
    case MultExpr1(left, _, right) => FloorDiv(convert(left), convert(right))(blame(expr))
    case MultExpr2(left, _, right) => Mod(convert(left), convert(right))(blame(expr))
    case MultExpr3(left, specOp, right) => convert(expr, specOp, convert(left), convert(right))
    case MultExpr4(inner) => convert(inner)
  }

  def convert(implicit expr: PowExprContext): Expr[G] = expr match {
    case PowExpr0(left, _, right) => Exp(convert(left), convert(right))
    case PowExpr1(inner) => convert(inner)
  }

  def convert(implicit expr: SeqAddExprContext): Expr[G] = expr match {
    case SeqAddExpr0(left, _, right) => Cons(convert(left), convert(right))
    case SeqAddExpr1(left, _, right) => ??(expr)
    case SeqAddExpr2(m, _, _, k, _, v, _) => MapCons(convert(m), convert(k), convert(v))
    case SeqAddExpr3(inner) => convert(inner)
  }

  def convert(implicit expr: UnaryExprContext): Expr[G] = expr match {
    case UnaryExpr0(_, inner) => Not(convert(inner))
    case UnaryExpr1(_, inner) => UMinus(convert(inner))
    case UnaryExpr2(op, inner) => convert(expr, op, convert(inner))
    case UnaryExpr3(inner) => convert(inner)
  }

  def convert(implicit expr: NewExprContext): Expr[G] = expr match {
    case NewExpr0(_, name, Call0(typeArgs, args, given, yields)) =>
      PVLNew(convert(name), convert(args), convertGiven(given), convertYields(yields))(blame(expr))
    case NewExpr1(_, t, dims) => NewArray(convert(t), convert(dims), moreDims = 0)
    case NewExpr2(inner) => convert(inner)
  }

  def convert(implicit expr: PostfixExprContext): Expr[G] = expr match {
    case PostfixExpr0(obj, _, field, None) => PVLDeref(convert(obj), convert(field))(blame(expr))
    case PostfixExpr0(obj, _, field, Some(Call0(typeArgs, args, given, yields))) =>
      PVLInvocation(Some(convert(obj)), convert(field), convert(args), typeArgs.map(convert(_)).getOrElse(Nil),
        convertGiven(given), convertYields(yields))(blame(expr))
    case PostfixExpr1(xs, _, i, _) => AmbiguousSubscript(convert(xs), convert(i))(blame(expr))
    case PostfixExpr2(obj, specOp) => convert(expr, specOp, convert(obj))
    case PostfixExpr3(inner) => convert(inner)
  }

  def convert(implicit expr: UnitContext): Expr[G] = expr match {
    case Unit0(inner) => convert(inner)
    case Unit1(_) => AmbiguousThis()
    case Unit2(_) => Null()
    case Unit3(n) => const(BigInt(n))
    case Unit4(n) => FloatValue(BigDecimal(n), PVL.float64)
    case Unit5(n) => FloatValue(BigDecimal(n.init /* take off final "f" */), PVL.float32)
    case Unit6(_, inner, _) => convert(inner)
    case Unit7(id, None) => local(id, convert(id))
    case Unit7(id, Some(Call0(typeArgs, args, given, yields))) =>
      PVLInvocation(None, convert(id), convert(args), typeArgs.map(convert(_)).getOrElse(Nil),
        convertGiven(given), convertYields(yields))(blame(expr))
    case Unit8(inner) => convert(inner)
  }

  def convert(implicit stat: StatementContext): Statement[G] = stat match {
    case PvlReturn(_, value, _) => Return(value.map(convert(_)).getOrElse(Void()))
    case PvlLock(_, obj, _) => Lock(convert(obj))(blame(stat))
    case PvlUnlock(_, obj, _) => Unlock(convert(obj))(blame(stat))
    case PvlWait(_, obj, _) => Wait(convert(obj))(blame(stat))
    case PvlNotify(_, obj, _) => Notify(convert(obj))(blame(stat))
    case PvlFork(_, obj, _) => Fork(convert(obj))(blame(stat))
    case PvlJoin(_, obj, _) => Join(convert(obj))(blame(stat))
    case PvlValStatement(inner) => convert(inner)
    case PvlIf(_, _, cond, _, body, None) =>
      Branch(Seq((convert(cond), convert(body))))
    case PvlIf(_, _, cond, _, body, Some(ElseBlock0(_, otherwise))) =>
      Branch(Seq(
        (convert(cond), convert(body)),
        (tt, convert(otherwise)),
      ))
    case PvlBarrier(_, _, block, tags, _, body) =>
      val (contract, content: Statement[G]) = body match {
        case BarrierBody0(_, contract, _) => (contract, Block[G](Nil))
        case BarrierBody1(contract, content) => (contract, convert(content))
      }

      withContract(contract, contract => {
        ParBarrier(
        block = new UnresolvedRef[G, ParBlockDecl[G]](convert(block)),
          invs = tags.map(convert(_)).getOrElse(Nil),
          AstBuildHelpers.foldStar(contract.consume(contract.requires)),
          AstBuildHelpers.foldStar(contract.consume(contract.ensures)),
          content,
        )(blame(stat))
      })
    case PvlPar(impl) =>
      ParStatement(convert(impl))
    case PvlVec(_, _, iter, _, body) =>
      ??(stat)
    case PvlInvariant(_, name, _, res, _, body) =>
      ParInvariant(
        new ParInvariantDecl()(SourceNameOrigin(convert(name), origin(stat))),
        convert(res), convert(body)
      )(blame(stat))
    case PvlAtomic(_, _, invs, _, body) =>
      ParAtomic(convert(invs).map(new UnresolvedRef[G, ParInvariantDecl[G]](_)), convert(body))(blame(stat))
    case PvlWhile(invs, _, _, cond, _, body) =>
      Scope(Nil, Loop(Block(Nil), convert(cond), Block(Nil), LoopInvariant(convert(invs), None)(blame(stat)), convert(body)))
    case PvlFor(invs, _, _, init, _, cond, _, update, _, body) =>
      Scope(Nil, Loop(
        init.map(convert(_)).getOrElse(Block(Nil)),
        cond.map(convert(_)).getOrElse(tt),
        update.map(convert(_)).getOrElse(Block(Nil)),
        LoopInvariant(convert(invs), None)(blame(stat)),
        convert(body)
      ))
    case PvlBlock(inner) => convert(inner)
    case PvlGoto(_, label, _) => Goto(new UnresolvedRef[G, LabelDecl[G]](convert(label)))
    case PvlLabel(_, label, _) => Label(new LabelDecl()(SourceNameOrigin(convert(label), origin(stat))), Block(Nil))
    case PvlForStatement(inner, _) => convert(inner)
  }

  def convert(implicit stat: ForStatementListContext): Statement[G] =
    Block(convertList(stat))

  def convertList(implicit stat: ForStatementListContext): Seq[Statement[G]] = stat match {
    case ForStatementList0(stat) => Seq(convert(stat))
    case ForStatementList1(stat, _, stats) => convert(stat) +: convertList(stats)
  }

  def convert(implicit stat: AllowedForStatementContext): Statement[G] = stat match {
    case PvlLocal(t, decls) =>
      Block(convert(decls, convert(t)))
    case PvlEval(e) => Eval(convert(e))
    case PvlIncDec(name, op) =>
      val target = PVLLocal[G](convert(name))(blame(stat))
      Eval(op match {
        case "++" => PostAssignExpression[G](target, target + const(1))(blame(stat))
        case "--" => PostAssignExpression[G](target, target - const(1))(blame(stat))
      })
    case PvlAssign(target, _, value) => Assign(convert(target), convert(value))(blame(stat))
  }

  def convert(implicit region: ParRegionContext): ParRegion[G] = region match {
    case PvlParallel(_, _, regions, _) => ParParallel(regions.map(convert(_)))(blame(region))
    case PvlSequential(_, _, regions, _) => ParSequential(regions.map(convert(_)))(blame(region))
    case PvlParBlock(_, name, iter, contract, impl) =>
      withContract(contract, contract => {
        val decl = name match {
          case None => new ParBlockDecl[G]()
          case Some(name) => new ParBlockDecl[G]()(SourceNameOrigin(convert(name), origin(region)))
        }

        ParBlock(
          decl,
          iter.map(convert(_)).getOrElse(Nil),
          AstBuildHelpers.foldStar(contract.consume(contract.context_everywhere)),
          AstBuildHelpers.foldStar(contract.consume(contract.requires)),
          AstBuildHelpers.foldStar(contract.consume(contract.ensures)),
          convert(impl),
        )(blame(region))
      })
    case PvlOldPar(_, pars) => ParParallel(convert(pars))(blame(region))
  }

  def convert(implicit pars: ParOldUnitListContext): Seq[ParBlock[G]] = pars match {
    case ParOldUnitList0(par) => Seq(convert(par))
    case ParOldUnitList1(par, _, pars) => convert(par) +: convert(pars)
  }

  def convert(implicit par: ParOldUnitContext): ParBlock[G] = par match {
    case PvlOldParUnit(name, iter, contract, impl) =>
      val decl = name match {
        case None => new ParBlockDecl[G]()
        case Some(name) => new ParBlockDecl[G]()(SourceNameOrigin(convert(name), origin(par)))
      }

      withContract(contract, contract => {
        ParBlock(
          decl,
          iter.map(convert(_)).getOrElse(Nil),
          AstBuildHelpers.foldStar(contract.consume(contract.context_everywhere)),
          AstBuildHelpers.foldStar(contract.consume(contract.requires)),
          AstBuildHelpers.foldStar(contract.consume(contract.ensures)),
          convert(impl),
        )(blame(par))
      })
  }

  def convert(implicit iters: ParBlockIterContext): Seq[IterVariable[G]] = iters match {
    case ParBlockIter0(_, iters, _) => convert(iters)
  }

  def convert(implicit decls: DeclListContext, t: Type[G]): Seq[Statement[G]] = decls match {
    case DeclList0(name, None) =>
      Seq(LocalDecl(new Variable(t)(SourceNameOrigin(convert(name), origin(name)))))
    case DeclList0(name, Some(DeclInit0(_, init))) =>
      val v = new Variable(t)(SourceNameOrigin(convert(name), origin(name)))
      Seq(
        LocalDecl(v),
        Assign(Local(v.ref[Variable[G]]), convert(init))(AssignLocalOk)
      )
    case DeclList1(name, None, _, more) =>
      LocalDecl(new Variable(t)(SourceNameOrigin(convert(name), origin(name)))) +:
        convert(more, t)
    case DeclList1(name, Some(DeclInit0(_, init)), _, more) =>
      val v = new Variable[G](t)(SourceNameOrigin(convert(name), origin(name)))
      Seq(
        LocalDecl[G](v),
        Assign[G](Local(v.ref), convert(init))(AssignLocalOk)
      ) ++ convert(more, t)
  }

  def convert(implicit block: BlockContext): Statement[G] = block match {
    case Block0(_, stats, _) => Scope(Nil, Block(stats.map(convert(_))))
  }

  def convert(implicit tags: BarrierTagsContext): Seq[Ref[G, ParInvariantDecl[G]]] = tags match {
    case BarrierTags0(_, ids) =>
      convert(ids).map(new UnresolvedRef[G, ParInvariantDecl[G]](_))
  }

  def convert(implicit iters: ItersContext): Seq[IterVariable[G]] = iters match {
    case Iters0(iter) => Seq(convert(iter))
    case Iters1(iter, _, iters) => convert(iter) +: convert(iters)
  }

  def convert(implicit iter: IterContext): IterVariable[G] = iter match {
    case Iter0(t, name, _, from, _, to) =>
      IterVariable(
        new Variable(convert(t))(SourceNameOrigin(convert(name), origin(name))),
        convert(from), convert(to)
      )
  }

  def convert(implicit wait: ParWaitListContext): Seq[Ref[G, ParBlockDecl[G]]] = ??(wait)

  def convert(implicit t: TypeContext): Type[G] = t match {
    case Type0(inner, dims) =>
      FuncTools.repeat(TArray[G](_), dims.map(convert(_)).getOrElse(0), convert(inner))
  }

  def convert(implicit ts: TypeListContext): Seq[Type[G]] = ts match {
    case TypeList0(t) => Seq(convert(t))
    case TypeList1(t, _, ts) => convert(t) +: convert(ts)
  }

  def convert(implicit dims: TypeDimsContext): Int = dims match {
    case TypeDims0(dims) => dims.size
    case TypeDims1(dims) => dims.size
  }

  def convert(implicit t: NonArrayTypeContext): Type[G] = t match {
    case NonArrayType0(inner) => convert(inner)
    case NonArrayType1(name) => name match {
      case "string" => TString()
      case "int" => TInt()
      case "boolean" => TBool()
      case "void" => TVoid()
      case "float32" => PVL.float32
      case "float64" => PVL.float64
    }
    case NonArrayType2(inner) => convert(inner)
  }

  def convert(implicit t: ClassTypeContext): Type[G] = t match {
    case ClassType0(name, typeArgs) => PVLNamedType(convert(name), typeArgs.map(convert(_)).getOrElse(Nil))
  }

  def convert(implicit ts: TypeArgsContext): Seq[Type[G]] = ts match {
    case TypeArgs0(_, ts, _) => convert(ts)
  }

  def convert(implicit ids: IdentifierListContext): Seq[String] = ids match {
    case IdentifierList0(id) => Seq(convert(id))
    case IdentifierList1(id, _, ids) => convert(id) +: convert(ids)
  }

  def convert(implicit id: IdentifierContext): String = id match {
    case Identifier0(id) => id
    case Identifier1(id) => id.substring(1, id.length - 1)
  }

  def withContract[T](node: ContractContext, f: ContractCollector[G] => T): T = node match {
    case Contract0(clauses) =>
      val collector = new ContractCollector[G]()
      clauses.foreach(convert(_, collector))
      withCollector(collector, f)
  }

  def convert(implicit decl: LangGlobalDeclContext): Seq[GlobalDeclaration[G]] = Nil
  def convert(implicit decl: LangClassDeclContext): Seq[ClassDeclaration[G]] = Nil

  def convert(implicit stat: LangStatementContext): Statement[G] = stat match {
    case LangStatement0(block) => convert(block)
  }

  def convert(implicit t: LangTypeContext): Type[G] = t match {
    case LangType0(t) => convert(t)
  }

  def convert(implicit e: LangExprContext): Expr[G] = e match {
    case LangExpr0(e) => convert(e)
  }

  def convert(implicit id: LangIdContext): String = id match {
    case LangId0(id) => convert(id)
  }

  def convert(implicit n: LangConstIntContext): BigInt = n match {
    case LangConstInt0(string) => BigInt(string)
  }

  def local(ctx: ParserRuleContext, name: String): Expr[G] =
    PVLLocal(name)(blame(ctx))(origin(ctx))

  def withCollector[T](collector: ContractCollector[G], f: ContractCollector[G] => T): T = {
    val result = f(collector)
    collector.nodes.headOption match {
      case Some(node) => fail(node, "This specification clause may not occur here")
      case None => result
    }
  }

  def withContract[T](node: Option[ValEmbedContractContext], f: ContractCollector[G] => T): T = {
    val collector = new ContractCollector[G]()
    node.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withContract[T](node1: Option[ValEmbedContractContext], node2: Option[ValEmbedContractContext], f: ContractCollector[G] => T): T = {
    val collector = new ContractCollector[G]()
    node1.foreach(convert(_, collector))
    node2.foreach(convert(_, collector))
    withCollector(collector, f)
  }

  def withContract[T](node: Seq[ValContractClauseContext], f: ContractCollector[G] => T): T = {
    val collector = new ContractCollector[G]()
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

  def convert(contract: ValEmbedContractContext, collector: ContractCollector[G]): Unit = contract match {
    case ValEmbedContract0(blocks) => blocks.foreach(convert(_, collector))
  }

  def convert(contract: ValEmbedContractBlockContext, collector: ContractCollector[G]): Unit = contract match {
    case ValEmbedContractBlock0(_, clauses, _) => clauses.foreach(convert(_, collector))
    case ValEmbedContractBlock1(clauses) => clauses.foreach(convert(_, collector))
  }

  def convert(implicit contract: ValContractClauseContext, collector: ContractCollector[G]): Unit = contract match {
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
      collector.requires += ((contract, convert(exp)))
      collector.ensures += ((contract, convert(exp)))
    case ValContractClause8(_, exp, _) => collector.loop_invariant += ((contract, convert(exp)))
    case ValContractClause9(_, exp, _) => collector.kernel_invariant += ((contract, convert(exp)))
    case ValContractClause10(_, _, t, id, _, exp, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(contract)))
      collector.signals += ((contract, SignalsClause(variable, convert(exp))(originProvider(contract))))
    case ValContractClause11(_, invariant, _) => collector.lock_invariant += ((contract, convert(invariant)))
    case ValContractClause12(_, None, _) => collector.decreases += ((contract, DecreasesClauseNoRecursion()))
    case ValContractClause12(_, Some(clause), _) => collector.decreases += ((contract, convert(clause)))
  }

  def convert(implicit clause: ValDecreasesMeasureContext): DecreasesClause[G] = clause match {
    case ValDecreasesMeasure0(_) => DecreasesClauseAssume()
    case ValDecreasesMeasure1(exps) => DecreasesClauseTuple(convert(exps))
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

  def convertEmbedWith(implicit whiff: Option[ValEmbedWithContext], inner: Expr[G]): Expr[G] = whiff match {
    case None => inner
    case Some(ValEmbedWith0(_, whiff, _)) => convertWith(whiff, inner)
    case Some(ValEmbedWith1(whiff)) => convertWith(Some(whiff), inner)
  }

  def convertWith(implicit whiff: Option[ValWithContext], inner: Expr[G]): Expr[G] = whiff match {
    case None => inner
    case Some(whiff @ ValWith0(_, stat)) => With(convert(stat), inner)(origin(whiff))
  }

  def convertEmbedThen(implicit den: Option[ValEmbedThenContext], inner: Expr[G]): Expr[G] = den match {
    case None => inner
    case Some(ValEmbedThen0(_, den, _)) => convertThen(den, inner)
    case Some(ValEmbedThen1(den)) => convertThen(Some(den), inner)
  }

  def convertThen(implicit den: Option[ValThenContext], inner: Expr[G]): Expr[G] = den match {
    case None => inner
    case Some(den @ ValThen0(_, stat)) => Then(inner, convert(stat))(origin(den))
  }

  def convert(implicit whiff: ValEmbedWithContext): Statement[G] = whiff match {
    case ValEmbedWith0(_, Some(whiff), _) => convert(whiff)
    case ValEmbedWith0(_, None, _) => Block(Nil)
    case ValEmbedWith1(whiff) => convert(whiff)
  }

  def convert(implicit whiff: ValWithContext): Statement[G] = whiff match {
    case ValWith0(_, stat) => convert(stat)
  }

  def convert(implicit whiff: ValEmbedThenContext): Statement[G] = whiff match {
    case ValEmbedThen0(_, Some(whiff), _) => convert(whiff)
    case ValEmbedThen0(_, None, _) => Block(Nil)
    case ValEmbedThen1(whiff) => convert(whiff)
  }

  def convert(implicit whiff: ValThenContext): Statement[G] = whiff match {
    case ValThen0(_, stat) => convert(stat)
  }

  def convertEmbedGiven(implicit given: Option[ValEmbedGivenContext]): Seq[(Ref[G, Variable[G]], Expr[G])] = given match {
    case None => Nil
    case Some(ValEmbedGiven0(_, inner, _)) => convertGiven(inner)
    case Some(ValEmbedGiven1(inner)) => convertGiven(Some(inner))
  }

  def convertGiven(implicit given: Option[ValGivenContext]): Seq[(Ref[G, Variable[G]], Expr[G])] = given match {
    case None => Nil
    case Some(ValGiven0(_, _, mappings, _)) => convert(mappings)
  }

  def convert(implicit mappings: ValGivenMappingsContext): Seq[(Ref[G, Variable[G]], Expr[G])] = mappings match {
    case ValGivenMappings0(arg, _, v) => Seq((new UnresolvedRef[G, Variable[G]](convert(arg)), convert(v)))
    case ValGivenMappings1(arg, _, v, _, more) => (new UnresolvedRef[G, Variable[G]](convert(arg)), convert(v)) +: convert(more)
  }

  def convertEmbedYields(implicit given: Option[ValEmbedYieldsContext]): Seq[(Expr[G], Ref[G, Variable[G]])] = given match {
    case None => Nil
    case Some(ValEmbedYields0(_, inner, _)) => convertYields(inner)
    case Some(ValEmbedYields1(inner)) => convertYields(Some(inner))
  }

  def convertYields(implicit given: Option[ValYieldsContext]): Seq[(Expr[G], Ref[G, Variable[G]])] = given match {
    case None => Nil
    case Some(ValYields0(_, _, mappings, _)) => convert(mappings)
  }

  def convert(implicit mappings: ValYieldsMappingsContext): Seq[(Expr[G], Ref[G, Variable[G]])] = mappings match {
    case ValYieldsMappings0(target, _, res) => Seq((local(target, convert(target)), new UnresolvedRef[G, Variable[G]](convert(res))))
    case ValYieldsMappings1(target, _, res, _, more) => (local(target, convert(target)), new UnresolvedRef[G, Variable[G]](convert(res))) +: convert(more)
  }

  def convert(implicit exprs: ValExpressionListContext): Seq[Expr[G]] = exprs match {
    case ValExpressionList0(expr) => Seq(convert(expr))
    case ValExpressionList1(head, _, tail) => convert(head) +: convert(tail)
  }

  def convert(implicit ids: ValIdListContext): Seq[String] = ids match {
    case ValIdList0(id) => Seq(convert(id))
    case ValIdList1(id, _, ids) => convert(id) +: convert(ids)
  }

  def convert(implicit ts: ValTypeListContext): Seq[Type[G]] = ts match {
    case ValTypeList0(t) => Seq(convert(t))
    case ValTypeList1(t, _, ts) => convert(t) +: convert(ts)
  }

  def convert(implicit root: ParserRuleContext, impOp: ValImpOpContext, left: Expr[G], right: Expr[G]): Expr[G] = impOp match {
    case ValImpOp0(_) => Wand(left, right)(origin(impOp))
    case ValImpOp1(_) => Implies(left, right)(origin(impOp))
  }

  def convert(implicit root: ParserRuleContext, andOp: ValAndOpContext, left: Expr[G], right: Expr[G]): Expr[G] = andOp match {
    case ValAndOp0(_) => col.Star(left, right)(origin(andOp))
  }

  def convert(implicit root: ParserRuleContext, inOp: ValInOpContext, left: Expr[G], right: Expr[G]): Expr[G] = inOp match {
    case ValInOp0(_) => AmbiguousMember(left, right)
  }

  def convert(implicit root: ParserRuleContext, mulOp: ValMulOpContext, left: Expr[G], right: Expr[G]): Expr[G] = mulOp match {
    case ValMulOp0(_) => col.Div(left, right)(blame(mulOp))
  }

  def convert(implicit root: ParserRuleContext, prependOp: ValPrependOpContext, left: Expr[G], right: Expr[G]): Expr[G] = prependOp match {
    case ValPrependOp0(_) => Cons(left, right)
  }

  def convert(implicit root: ParserRuleContext, postfixOp: ValPostfixContext, xs: Expr[G]): Expr[G] = postfixOp match {
    case ValPostfix0(_, _, to, _) => Take(xs, convert(to))
    case ValPostfix1(_, from, _, None, _) => Drop(xs, convert(from))
    case ValPostfix1(_, from, _, Some(to), _) => Slice(xs, convert(from), convert(to))
    case ValPostfix2(_, idx, _, v, _) => SeqUpdate(xs, convert(idx), convert(v))
    case ValPostfix3(_, name, _, args, _) => CoalesceInstancePredicateApply(xs, new UnresolvedRef[G, InstancePredicate[G]](convert(name)), args.map(convert(_)).getOrElse(Nil), WritePerm())
  }

  def convert(implicit root: ParserRuleContext, prefixOp: ValPrefixContext, xs: Expr[G]): Expr[G] = prefixOp match {
    case ValScale(_, scale, _) => Scale(convert(scale), xs)(blame(prefixOp))
  }

  def convert(implicit block: ValEmbedStatementBlockContext): Block[G] = block match {
    case ValEmbedStatementBlock0(_, stats, _) => Block(stats.map(convert(_)))
    case ValEmbedStatementBlock1(stats) => Block(stats.map(convert(_)))
  }

  def convert(implicit stat: ValStatementContext): Statement[G] = stat match {
    case ValPackage(_, expr, innerStat) => WandPackage(convert(expr), convert(innerStat))(blame(stat))
    case ValApplyWand(_, wand, _) => WandApply(convert(wand))(blame(stat))
    case ValFold(_, predicate, _) =>
      Fold(convert(predicate))(blame(stat))
    case ValUnfold(_, predicate, _) =>
      Unfold(convert(predicate))(blame(stat))
    case ValOpen(_, _, _) => ??(stat)
    case ValClose(_, _, _) => ??(stat)
    case ValAssert(_, assn, _) => Assert(convert(assn))(blame(stat))
    case ValAssume(_, assn, _) => Assume(convert(assn))
    case ValInhale(_, resource, _) => Inhale(convert(resource))
    case ValExhale(_, resource, _) => Exhale(convert(resource))(blame(stat))
    case ValLabel(_, label, _) =>
      Label(new LabelDecl()(SourceNameOrigin(convert(label), origin(stat))), Block(Nil))
    case ValRefute(_, assn, _) => Refute(convert(assn))(blame(stat))
    case ValWitness(_, _, _) => ??(stat)
    case ValGhost(_, stat) => convert(stat)
    case ValSend(_, name, _, delta, _, resource, _) =>
      Send(new SendDecl()(SourceNameOrigin(convert(name), origin(stat))), convert(delta), convert(resource))(blame(stat))
    case ValRecv(_, name, _) =>
      Recv(new UnresolvedRef[G, SendDecl[G]](convert(name)))
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
      ParAtomic(Seq(new UnresolvedRef[G, ParInvariantDecl[G]](convert(invariant))), convert(body))(blame(stat))
    case ValCommit(_, obj, _) =>
      Commit(convert(obj))(blame(stat))
  }

  def convert(implicit block: ValBlockContext): Seq[Statement[G]] = block match {
    case ValBlock0(_, stats, _) => stats.map(convert(_))
  }

  def convert(implicit arg: ValArgContext): Variable[G] = arg match {
    case ValArg0(t, id) => new Variable(convert(t))(SourceNameOrigin(convert(id), origin(arg)))
  }

  def convert(implicit args: ValArgListContext): Seq[Variable[G]] = args match {
    case ValArgList0(arg) => Seq(convert(arg))
    case ValArgList1(arg, _, args) => convert(arg) +: convert(args)
  }

  def convert(implicit decl: ValEmbedGlobalDeclarationBlockContext): Seq[GlobalDeclaration[G]] = decl match {
    case ValEmbedGlobalDeclarationBlock0(_, globals, _) => globals.flatMap(convert(_))
    case ValEmbedGlobalDeclarationBlock1(globals) => globals.flatMap(convert(_))
  }

  def convert(implicit decl: ValGlobalDeclarationContext): Seq[GlobalDeclaration[G]] = decl match {
    case ValAxiom(_, name, _, axiom, _) =>
      Seq(new SimplificationRule(convert(axiom))(SourceNameOrigin(convert(name), origin(decl))))
    case ValPredicate(modifiers, _, name, _, args, _, definition) =>
      withModifiers(modifiers, mods =>
        Seq(new Predicate(args.map(convert(_)).getOrElse(Nil), convert(definition),
          mods.consume(mods.threadLocal), mods.consume(mods.inline))
        (SourceNameOrigin(convert(name), origin(decl)))))
    case ValFunction(contract, modifiers, _, t, name, typeArgs, _, args, _, definition) =>
      Seq(withContract(contract, c =>
        withModifiers(modifiers, m => {
          val namedOrigin = SourceNameOrigin(convert(name), origin(decl))
          new Function(
            convert(t),
            args.map(convert(_)).getOrElse(Nil),
            typeArgs.map(convert(_)).getOrElse(Nil),
            convert(definition),
            c.consumeApplicableContract(blame(decl)),
            m.consume(m.inline))(blame(decl))(namedOrigin)
        })
      ))
    case ValModel(_, name, _, decls, _) =>
      Seq(new Model(decls.flatMap(convert(_)))(SourceNameOrigin(convert(name), origin(decl))))
    case ValGhostDecl(_, inner) =>
      convert(inner)
    case ValAdtDecl(_, name, typeArgs, _, decls, _) =>
      Seq(new AxiomaticDataType(decls.map(convert(_)), typeArgs.map(convert(_)).getOrElse(Nil))(
        SourceNameOrigin(convert(name), origin(decl))))
  }

  def convert(implicit decl: ValEmbedClassDeclarationBlockContext): Seq[ClassDeclaration[G]] = decl match {
    case ValEmbedClassDeclarationBlock0(_, decls, _) => decls.flatMap(convert(_, x => x))
    case ValEmbedClassDeclarationBlock1(decls) => decls.flatMap(convert(_, x => x))
  }

  def convert[T](implicit decl: ValClassDeclarationContext, transform: ClassDeclaration[G] => T): Seq[T] = decl match {
    case ValInstancePredicate(modifiers, _, name, _, args, _, definition) =>
      Seq(withModifiers(modifiers, mods => {
        transform(new InstancePredicate(args.map(convert(_)).getOrElse(Nil), convert(definition),
          mods.consume(mods.threadLocal), mods.consume(mods.inline))(
          SourceNameOrigin(convert(name), origin(decl))))
      }))
    case ValInstanceFunction(contract, modifiers, _, t, name, typeArgs, _, args, _, definition) =>
      Seq(withContract(contract, c => {
        withModifiers(modifiers, m => {
          transform(new InstanceFunction(
            convert(t),
            args.map(convert(_)).getOrElse(Nil),
            typeArgs.map(convert(_)).getOrElse(Nil),
            convert(definition),
            c.consumeApplicableContract(blame(decl)), m.consume(m.inline))(
            blame(decl))(
            SourceNameOrigin(convert(name), origin(decl))))
        })
      }))
    case ValInstanceGhostDecl(_, decl) => convert(decl).map(transform)
  }

  def convert(implicit decl: ValModelDeclarationContext): Seq[ModelDeclaration[G]] = decl match {
    case ValModelField(t, name, _) =>
      convert(name).map(name => {
        new ModelField(convert(t))(SourceNameOrigin(name, origin(decl)))
      })
    case ValModelProcess(contract, _, name, _, args, _, _, definition, _) =>
      Seq(withContract(contract, c => {
        new ModelProcess(args.map(convert(_)).getOrElse(Nil), convert(definition),
          AstBuildHelpers.foldAnd(c.consume(c.requires)), AstBuildHelpers.foldAnd(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef[G, ModelField[G]](_)), c.consume(c.accessible).map(new UnresolvedRef[G, ModelField[G]](_)))(
          blame(decl))(SourceNameOrigin(convert(name), origin(decl)))
      }))
    case ValModelAction(contract, _, name, _, args, _, _) =>
      Seq(withContract(contract, c => {
        new ModelAction(args.map(convert(_)).getOrElse(Nil),
          AstBuildHelpers.foldAnd(c.consume(c.requires)), AstBuildHelpers.foldAnd(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef[G, ModelField[G]](_)), c.consume(c.accessible).map(new UnresolvedRef[G, ModelField[G]](_)))(
          SourceNameOrigin(convert(name), origin(decl)))
      }))
  }

  def convert(implicit ts: ValTypeVarsContext): Seq[Variable[G]] = ts match {
    case ValTypeVars0(_, names, _) =>
      convert(names).map(name => new Variable(TType(TAny()))(SourceNameOrigin(name, origin(ts))))
  }

  def convert(implicit decl: ValAdtDeclarationContext): ADTDeclaration[G] = decl match {
    case ValAdtAxiom(_, ax, _) => new ADTAxiom(convert(ax))
    case ValAdtFunction(_, returnType, name, _, args, _, _) =>
      new ADTFunction(args.map(convert(_)).getOrElse(Nil), convert(returnType))(
        SourceNameOrigin(convert(name), origin(decl)))
  }

  def convert(implicit definition: ValDefContext): Option[Expr[G]] = definition match {
    case ValAbstractBody(_) => None
    case ValBody(_, expr, _) => Some(convert(expr))
  }

  def convert(implicit t: ValTypeContext): Type[G] = t match {
    case ValPrimaryType(name) => name match {
      case "resource" => TResource()
      case "process" => TProcess()
      case "frac" => TFraction()
      case "zfrac" => TZFraction()
      case "rational" => TRational()
      case "bool" => TBool()
      case "ref" => TRef()
      case "any" => TAny()
      case "nothing" => TNothing()
    }
    case ValSeqType(_, _, element, _) => TSeq(convert(element))
    case ValSetType(_, _, element, _) => TSet(convert(element))
    case ValBagType(_, _, element, _) => TBag(convert(element))
    case ValOptionType(_, _, element, _) => TOption(convert(element))
    case ValMapType(_, _, key, _, value, _) => TMap(convert(key), convert(value))
    case ValTupleType(_, _, t1, _, t2, _) => TTuple(Seq(convert(t1), convert(t2)))
    case ValPointerType(_, _, element, _) => TPointer(convert(element))
    case ValTypeType(_, _, element, _) => TType(convert(element))
    case ValEitherType(_, _, left, _, right, _) => TEither(convert(left), convert(right))
  }

  def convert(implicit e: ValPrimarySeqContext): Expr[G] = e match {
    case ValCardinality(_, xs, _) => Size(convert(xs))
    case ValArrayValues(_, _, a, _, from, _, to, _) => Values(convert(a), convert(from), convert(to))(blame(e))
  }

  def convert(implicit e: ValPrimaryOptionContext): Expr[G] = e match {
    case ValSome(_, _, v, _) => OptSome(convert(v))
  }

  def convert(implicit e: ValPrimaryEitherContext): Expr[G] = e match {
    case ValLeft(_, _, inner, _) => EitherLeft(convert(inner))
    case ValRight(_, _, inner, _) => EitherRight(convert(inner))
  }

  // valsetcompselectors
  def convert(implicit exprs: ValMapPairsContext): Seq[(Expr[G], Expr[G])] = exprs match {
    case ValMapPairs0(k, _, v) => Seq((convert(k), convert(v)))
    case ValMapPairs1(k, _, v, _, tail) => (convert(k), convert(v)) +: convert(tail)
  }

  def convert(implicit e: ValPrimaryCollectionConstructorContext): Expr[G] = e match {
    case ValTypedLiteralSeq(_, _, t, _, _, exprs, _) => LiteralSeq(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValTypedLiteralSet(_, _, t, _, _, exprs, _) => LiteralSet(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValSetComprehension(_, _, t, _, _, value, _, selectors, _, something, _) => ??(e)
    case ValTypedLiteralBag(_, _, t, _, _, exprs, _) => LiteralBag(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValTypedLiteralMap(_, _, key, _, value, _, _, pairs, _) => LiteralMap(convert(key), convert(value), pairs.map(convert(_)).getOrElse(Nil))
    case ValTypedTuple(_, _, t1, _, t2, _, _, v1, _, v2, _) =>
      LiteralTuple(Seq(convert(t1), convert(t2)), Seq(convert(v1), convert(v2)))
    case ValLiteralSeq(_, exprs, _) => UntypedLiteralSeq(convert(exprs))
    case ValLiteralSet(_, exprs, _) => UntypedLiteralSet(convert(exprs))
    case ValLiteralBag(_, exprs, _) => UntypedLiteralBag(convert(exprs))
    case ValEmptySeq(_, t, _) => LiteralSeq(convert(t), Nil)
    case ValEmptySet(_, t, _) => LiteralSet(convert(t), Nil)
    case ValEmptyBag(_, t, _) => LiteralBag(convert(t), Nil)
    case ValRange(_, from, _, to, _) => Range(convert(from), convert(to))
  }

  def convert(implicit e: ValPrimaryPermissionContext): Expr[G] = e match {
    case ValCurPerm(_, _, loc, _) => CurPerm(AmbiguousLocation(convert(loc))(blame(e)))
    case ValPerm(_, _, loc, _, perm, _) => Perm(AmbiguousLocation(convert(loc))(blame(e)), convert(perm))
    case ValValue(_, _, loc, _) => Value(AmbiguousLocation(convert(loc))(blame(e)))
    case ValPointsTo(_, _, loc, _, perm, _, v, _) => PointsTo(AmbiguousLocation(convert(loc))(blame(e)), convert(perm), convert(v))
    case ValHPerm(_, _, loc, _, perm, _) => ModelPerm(convert(loc), convert(perm))
    case ValAPerm(_, _, loc, _, perm, _) => ActionPerm(convert(loc), convert(perm))
    case ValArrayPerm(_, _, arr, _, i, _, step, _, count, _, perm, _) => ??(e)
    case ValMatrix(_, _, m, _, dim1, _, dim2, _) => ValidMatrix(convert(m), convert(dim1), convert(dim2))
    case ValArray(_, _, arr, _, dim, _) => ValidArray(convert(arr), convert(dim))
    case ValPointer(_, _, ptr, _, n, _, perm, _) => PermPointer(convert(ptr), convert(n), convert(perm))
    case ValPointerIndex(_, _, ptr, _, idx, _, perm, _) => PermPointerIndex(convert(ptr), convert(idx), convert(perm))
    case ValPointerBlockLength(_, _, ptr, _) => PointerBlockLength(convert(ptr))(blame(e))
    case ValPointerBlockOffset(_, _, ptr, _) => PointerBlockOffset(convert(ptr))(blame(e))
  }

  def convert(implicit v: ValBindingContext): (Variable[G], Seq[Expr[G]]) = v match {
    case ValRangeBinding(t, id, _, from, _, to) =>
      val variable = new Variable[G](convert(t))(SourceNameOrigin(convert(id), origin(id)))
      val cond = SeqMember[G](Local(variable.ref), Range(convert(from), convert(to)))
      (variable, Seq(cond))
    case ValNormalBinding(arg) =>
      (convert(arg), Nil)
  }

  def convert(implicit vs: ValBindingsContext): (Seq[Variable[G]], Seq[Expr[G]]) = vs match {
    case ValBindings0(binding) =>
      val (v, cs) = convert(binding)
      (Seq(v), cs)
    case ValBindings1(binding, _, bindings) =>
      val (v, cs) = convert(binding)
      val (vs, ds) = convert(bindings)
      (v +: vs, cs ++ ds)
  }

  def convert(implicit e: ValPrimaryBinderContext): Expr[G] = e match {
    case ValQuantifier(_, symbol, bindings, _, bodyOrCond, maybeBody, _) =>
      val (variables, bindingConds) = convert(bindings)
      val (bodyConds, body) = maybeBody match {
        case Some(ValBinderCont0(_, body)) => (Seq(convert(bodyOrCond)), convert(body))
        case None => (Nil, convert(bodyOrCond))
      }
      val conds = bindingConds ++ bodyConds
      symbol match {
        case ValForallSymb(_) => Forall(variables, Nil, implies(conds, body))
        case ValStarallSymb(_) => Starall(variables, Nil, implies(conds, body))(blame(e))
        case ValExistsSymb(_) => Exists(variables, Nil, foldAnd(conds :+ body))
      }
    case ValLet(_, _, t, id, _, v, _, body, _) =>
      Let(new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id))), convert(v), convert(body))
  }

  def convert(implicit e: ValPrimaryVectorContext): Expr[G] = e match {
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

  def convert(implicit e: ValPrimaryReducibleContext): Expr[G] = ??(e)

  def convert(implicit e: ValPrimaryThreadContext): Expr[G] = e match {
    case ValIdle(_, _, thread, _) => IdleToken(convert(thread))
    case ValRunning(_, _, thread, _) => JoinToken(convert(thread))
  }

  def convert(implicit e: ValPrimaryContextContext): Expr[G] = e match {
    case ValPrimaryContext0("\\result") => AmbiguousResult()
    case ValPrimaryContext1("\\current_thread") => CurrentThreadId()
    case ValPrimaryContext2("\\ltid") => LocalThreadId()
    case ValPrimaryContext3("\\gtid") => GlobalThreadId()
  }

  def convert(implicit e: ValPrimaryContext): Expr[G] = e match {
    case ValPrimary0(inner) => convert(inner)
    case ValPrimary1(inner) => convert(inner)
    case ValPrimary2(inner) => convert(inner)
    case ValPrimary3(inner) => convert(inner)
    case ValPrimary4(inner) => convert(inner)
    case ValPrimary5(inner) => convert(inner)
    case ValPrimary6(inner) => convert(inner)
    case ValPrimary7(inner) => convert(inner)
    case ValPrimary8(inner) => convert(inner)
    case ValPrimary9(inner) => convert(inner)
    case ValAny(_) => Any()(blame(e))
    case ValFunctionOf(_, inner, _, names, _) => FunctionOf(new UnresolvedRef[G, Variable[G]](convert(inner)), convert(names).map(new UnresolvedRef[G, Variable[G]](_)))
    case ValInlinePattern(open, pattern, _) =>
      val groupText = open.filter(_.isDigit)
      InlinePattern(convert(pattern), open.count(_ == '<'), if(groupText.isEmpty) 0 else groupText.toInt)
    case ValUnfolding(_, predExpr, _, body) => Unfolding(convert(predExpr), convert(body))(blame(e))
    case ValOld(_, _, expr, _) => Old(convert(expr), at = None)(blame(e))
    case ValOldLabeled(_, _, label, _, _, expr, _) => Old(convert(expr), at = Some(new UnresolvedRef[G, LabelDecl[G]](convert(label))))(blame(e))
    case ValTypeof(_, _, expr, _) => TypeOf(convert(expr))
    case ValTypeValue(_, _, t, _) => TypeValue(convert(t))
    case ValHeld(_, _, obj, _) => Held(convert(obj))
    case ValCommitted(_, _, obj, _) => Committed(convert(obj))(blame(e))
    case ValIdEscape(text) => local(e, text.substring(1, text.length-1))
  }

  def convert(implicit e: ValExprContext): Expr[G] = e match {
    case ValExpr0(inner) => convert(inner)
    case ValExpr1(inner) => convert(inner)
  }

  def convert(implicit id: ValIdentifierContext): String = id match {
    case ValIdentifier0(inner) => convertText(inner)
    case ValIdentifier1(ValKeywordNonExpr0(text)) => text
    case ValIdentifier2(text) => text.substring(1, text.length-1)
  }

  def convertText(implicit res: ValKeywordExprContext): String = res match {
    case ValNonePerm(_) => "none"
    case ValWrite(_) => "write"
    case ValRead(_) => "read"
    case ValNoneOption(_) => "None"
    case ValEmpty(_) => "empty"
    case ValTrue(_) => "true"
    case ValFalse(_) => "false"
  }

  def convert(implicit res: ValKeywordExprContext): Expr[G] = res match {
    case ValNonePerm(_) => NoPerm()
    case ValWrite(_) => WritePerm()
    case ValRead(_) => ReadPerm()
    case ValNoneOption(_) => OptNone()
    case ValEmpty(_) => EmptyProcess()
    case ValTrue(_) => tt
    case ValFalse(_) => ff
  }

  def convert(implicit inv: ValGenericAdtInvocationContext): Expr[G] = inv match {
    case ValGenericAdtInvocation0(adt, _, typeArgs, _, _, func, _, args, _) =>
      ADTFunctionInvocation(Some((new UnresolvedRef[G, AxiomaticDataType[G]](convert(adt)), convert(typeArgs))),
        new UnresolvedRef[G, ADTFunction[G]](convert(func)), args.map(convert(_)).getOrElse(Nil))
  }
}