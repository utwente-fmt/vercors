package vct.parsers.transform

import org.antlr.v4.runtime.ParserRuleContext
import vct.antlr4.generated.CParser._
import vct.antlr4.generated.CParserPatterns._
import vct.col.util.AstBuildHelpers._
import vct.col.ast._
import vct.col.{ast => col}
import vct.col.origin._
import vct.col.ref.UnresolvedRef
import vct.col.util.AstBuildHelpers

import scala.annotation.nowarn
import scala.collection.mutable

@nowarn("msg=match may not be exhaustive&msg=Some\\(")
case class CToCol[G](override val originProvider: OriginProvider, override val blameProvider: BlameProvider, override val errors: mutable.Map[(Int, Int), String])
  extends ToCol(originProvider, blameProvider, errors) {
  def convert(unit: CompilationUnitContext): Seq[GlobalDeclaration[G]] = unit match {
    case CompilationUnit0(translationUnit, _) =>
      translationUnit.map(convert).getOrElse(Nil)
  }

  def convert(unit: TranslationUnitContext): Seq[GlobalDeclaration[G]] =
    convertList(TranslationUnit0.unapply, TranslationUnit1.unapply)(unit).flatMap(convert(_))

  def convert(implicit externalDecl: ExternalDeclarationContext): Seq[GlobalDeclaration[G]] = externalDecl match {
    case ExternalDeclaration0(funcDef) => Seq(convert(funcDef))
    case ExternalDeclaration1(decl) => Seq(new CGlobalDeclaration(convert(decl)))
    case ExternalDeclaration2(valDecls) => convert(valDecls)
    case ExternalDeclaration3(";") => Nil
  }

  def convert(implicit funcDef: FunctionDefinitionContext): CFunctionDefinition[G] = {
    funcDef match {
      case FunctionDefinition0(_, _, _, Some(declarationList), _) =>
        ??(declarationList)
      case FunctionDefinition0(maybeContract, declSpecs, declarator, None, body) =>
        new CFunctionDefinition(convert(declSpecs), convert(declarator), convert(body))(blame(funcDef))
    }
  }

  def convert(implicit decl: DeclarationContext): CDeclaration[G] = decl match {
    case Declaration0(maybeContract, declSpecs, maybeInits, _) =>
      withContract(maybeContract, contract =>
        new CDeclaration[G](contract.consumeApplicableContract(), AstBuildHelpers.foldStar[G](contract.consume(contract.kernel_invariant)),
          specs=convert(declSpecs), inits=maybeInits.map(convert(_)) getOrElse Nil))
    case Declaration1(staticAssert) =>
      ??(staticAssert)
  }

  def convert(declSpecs: DeclarationSpecifiersContext): Seq[CDeclarationSpecifier[G]] = declSpecs match {
    case DeclarationSpecifiers0(specs) => specs.map(convert(_))
  }

  def convert(declSpecs: DeclarationSpecifiers2Context): Seq[CDeclarationSpecifier[G]] = declSpecs match {
    case DeclarationSpecifiers20(specs) => specs.map(convert(_))
  }

  def convert(implicit declSpec: DeclarationSpecifierContext): CDeclarationSpecifier[G] = declSpec match {
    case DeclarationSpecifier0(storageClass) => convert(storageClass)
    case DeclarationSpecifier1(typeSpec) => convert(typeSpec)
    case DeclarationSpecifier2(typeQual) => CTypeQualifierDeclarationSpecifier(convert(typeQual))
    case DeclarationSpecifier3(functionSpecifier) => ??(functionSpecifier)
    case DeclarationSpecifier4(alignmentSpecifier) => ??(alignmentSpecifier)
    case DeclarationSpecifier5(kernelSpecifier) => convert(kernelSpecifier)
    case DeclarationSpecifier6(valEmbedModifier) => withModifiers(valEmbedModifier, m => {
      if(m.consume(m.pure))
        CPure[G]()
      else if(m.consume(m.inline))
        CInline[G]()
      else
        fail(m.nodes.head, "This modifier cannot be attached to a declaration in C")
    })
  }

  def convert(implicit storageClass: StorageClassSpecifierContext): CStorageClassSpecifier[G] = storageClass match {
    case StorageClassSpecifier0(_) => CTypedef()
    case StorageClassSpecifier1(_) => CExtern()
    case StorageClassSpecifier2(_) => CStatic()
    case StorageClassSpecifier3(_) => ??(storageClass)
    case StorageClassSpecifier4(_) => ??(storageClass)
    case StorageClassSpecifier5(_) => ??(storageClass)
  }

  def convert(implicit typeSpec: TypeSpecifierContext): CTypeSpecifier[G] = typeSpec match {
    case TypeSpecifier0(name) => name match {
      case "void" => CVoid()
      case "char" => CChar()
      case "short" => CShort()
      case "int" => CInt()
      case "long" => CLong()
      case "float" => CFloat()
      case "double" => CDouble()
      case "signed" => CSigned()
      case "unsigned" => CUnsigned()
      case "_Bool" => CBool()
      case _ => ??(typeSpec)
    }
    case TypeSpecifier1(_, _, _, _) => ??(typeSpec)
    case TypeSpecifier2(valType) => CSpecificationType(convert(valType))
    case TypeSpecifier3(_) => ??(typeSpec)
    case TypeSpecifier4(_) => ??(typeSpec)
    case TypeSpecifier5(_) => ??(typeSpec)
    case TypeSpecifier6(name) => name match {
      case TypedefName0(name) => CTypedefName(convert(name))
    }
    case TypeSpecifier7(_, _, _, _) => ??(typeSpec)
  }

  def convert(implicit quals: TypeQualifierListContext): Seq[CTypeQualifier[G]] =
    convertList(TypeQualifierList0.unapply, TypeQualifierList1.unapply)(quals).map(convert(_))

  def convert(implicit qual: TypeQualifierContext): CTypeQualifier[G] = qual match {
    case TypeQualifier0(_) => CConst()
    case TypeQualifier1(_) => CRestrict()
    case TypeQualifier2(_) => CVolatile()
    case TypeQualifier3(_) => CAtomic()
  }

  def convert(implicit kernel: GpgpuKernelSpecifierContext): CGpgpuKernelSpecifier[G] = kernel match {
    case GpgpuKernelSpecifier0(_) => CKernel()
  }

  def convert(implicit decls: InitDeclaratorListContext): Seq[CInit[G]] = decls match {
    case InitDeclaratorList0(decl) => Seq(convert(decl))
    case InitDeclaratorList1(init, _, last) => convert(init) :+ convert(last)
  }

  def convert(implicit decl: InitDeclaratorContext): CInit[G] = decl match {
    case InitDeclarator0(inner) => CInit(convert(inner), None)
    case InitDeclarator1(inner, _, init) => CInit(convert(inner), Some(convert(init)))
  }

  def convert(implicit decl: DeclaratorContext): CDeclarator[G] = decl match {
    case Declarator0(_, _, extension::_) => ??(extension)
    case Declarator0(Some(ptr), inner, Nil) =>
      val pointers = convert(ptr)
      CPointerDeclarator(pointers, convert(inner))
    case Declarator0(None, inner, Nil) => convert(inner)
  }

  def convert(implicit ptr: PointerContext): Seq[CPointer[G]] = ptr match {
    case Pointer0(_, quals) =>
      Seq(CPointer(quals.map(convert(_)) getOrElse Nil))
    case Pointer1(_, quals, tail) =>
      CPointer(quals.map(convert(_)) getOrElse Nil) +: convert(tail)
    case Pointer2(_, quals) =>
      Seq(CPointer(Nil), CPointer(quals.map(convert(_)) getOrElse Nil))
    case Pointer3(_, quals, tail) =>
      Seq(CPointer[G](Nil), CPointer[G](quals.map(convert(_)) getOrElse Nil)) ++ convert(tail)
    case Pointer4(_, _) => ??(ptr)
    case Pointer5(_, _, _) => ??(ptr)
  }

  def convert(implicit decl: DirectDeclaratorContext): CDeclarator[G] = decl match {
    case DirectDeclarator0(name) => CName(convert(name))
    case DirectDeclarator1(inner, _, quals, dim, _) =>
      CArrayDeclarator(quals.map(convert(_)) getOrElse Nil, dim.map(convert(_)), convert(inner))
    case DirectDeclarator2(_, _, _, _, _, _) => ??(decl)
    case DirectDeclarator3(_, _, _, _, _, _) => ??(decl)
    case DirectDeclarator4(_, _, _, _, _) => ??(decl)
    case DirectDeclarator5(inner, _, paramList, _) =>
      val (params, varargs) = paramList match {
        case ParameterTypeList0(params) => (convert(params), false)
        case ParameterTypeList1(params, _, _) => (convert(params), true)
      }
      CTypedFunctionDeclarator(params, varargs, convert(inner))
    case DirectDeclarator6(inner, _, names, _) =>
      CAnonymousFunctionDeclarator(names.map(convert(_)) getOrElse Nil, convert(inner))
  }

  def convert(implicit params: ParameterListContext): Seq[CParam[G]] = params match {
    case ParameterList0(decl) => Seq(convert(decl))
    case ParameterList1(init, _, last) => convert(init) :+ convert(last)
  }

  def convert(implicit param: ParameterDeclarationContext): CParam[G] = param match {
    case ParameterDeclaration0(declSpecs, declarator) =>
      new CParam(convert(declSpecs), convert(declarator))
    case ParameterDeclaration1(_, _) =>
      ??(param)
  }

  def convert(implicit stat: StatementContext): Statement[G] = stat match {
    case Statement0(stat) => convert(stat)
    case Statement1(stat) => convert(stat)
    case Statement2(stat) => convert(stat)
    case Statement3(stat) => convert(stat)
    case Statement4(stat) => convert(stat)
    case Statement5(stat) => convert(stat)
    case _: Statement6Context => ??(stat)
  }

  def convert(implicit block: CompoundStatementContext): Statement[G] = block match {
    case CompoundStatement0(_, stats, _) =>
      Scope(Nil, Block(stats.map(convert(_)) getOrElse Nil))
    case CompoundStatement1(ompPragma, _, contract, stats, _) =>
      ??(block)
  }

  def convert(implicit stats: BlockItemListContext): Seq[Statement[G]] =
    convertList(BlockItemList0.unapply, BlockItemList1.unapply)(stats).map(convert(_))

  def convert(implicit stat: BlockItemContext): Statement[G] = stat match {
    case BlockItem0(decl) =>
      CDeclarationStatement(convert(decl))
    case BlockItem1(stat) => convert(stat)
    case BlockItem2(embedStats) => convert(embedStats)
    case BlockItem3(embedStat) => convert(embedStat)
    case BlockItem4(GpgpuLocalBarrier0(contract, _, _, _, _)) => withContract(contract, c => {
      GpgpuLocalBarrier(AstBuildHelpers.foldStar[G](c.consume(c.requires)), AstBuildHelpers.foldStar[G](c.consume(c.ensures)))
    })
    case BlockItem5(GpgpuGlobalBarrier0(contract, _, _, _, _)) => withContract(contract, c => {
      GpgpuGlobalBarrier(AstBuildHelpers.foldStar[G](c.consume(c.requires)), AstBuildHelpers.foldStar[G](c.consume(c.ensures)))
    })
    case BlockItem6(GpgpuAtomicBlock0(whiff, _, impl, den)) =>
      GpgpuAtomic(convert(impl), whiff.map(convert(_)).getOrElse(Block(Nil)), den.map(convert(_)).getOrElse(Block(Nil)))
  }

  def convert(implicit stat: LabeledStatementContext): Statement[G] = stat match {
    case LabeledStatement0(label, _, inner) =>
      Label(new LabelDecl()(SourceNameOrigin(convert(label), originProvider(stat))), convert(inner))
    case LabeledStatement1(_, _, _, _) => ??(stat)
    case LabeledStatement2(_, _, _) => ??(stat)
  }

  def convert(implicit stat: ExpressionStatementContext): Statement[G] = stat match {
    case ExpressionStatement0(None, _) => Block(Nil)
    case ExpressionStatement0(Some(expr), _) => Eval(convert(expr))
  }

  def convert(implicit stat: SelectionStatementContext): Statement[G] = stat match {
    case SelectionStatement0(_, _, cond, _, whenTrue, None) =>
      Branch(Seq((convert(cond), convert(whenTrue))))
    case SelectionStatement0(_, _, cond, _, whenTrue, Some(whenFalse)) =>
      Branch(Seq((convert(cond), convert(whenTrue)), (tt, convert(whenFalse))))
    case SelectionStatement1(_, _, _, _, _) => ??(stat)
  }

  def convert(implicit stat: ElseBranchContext): Statement[G] = stat match {
    case ElseBranch0(_, stat) => convert(stat)
  }

  def evalOrNop(implicit expr: Option[ExpressionContext]): Statement[G] = expr match {
    case Some(expr) => Eval(convert(expr))(origin(expr))
    case None =>
      // PB: strictly speaking it would be nice if we can point to the empty range that indicates the absence of a statement here.
      Block(Nil)(DiagnosticOrigin)
  }

  def convert(implicit stat: IterationStatementContext): Statement[G] = stat match {
    case IterationStatement0(contract1, _, _, cond, _, contract2, body) => withContract(contract1, contract2, c => {
      Scope(Nil, Loop[G](Block(Nil), convert(cond), Block(Nil), c.consumeLoopContract(), convert(body))(blame(stat)))
    })
    case IterationStatement1(_, _, _, _, _, _, _) => ??(stat)
    case IterationStatement2(contract1, maybePragma, _, _, init, _, cond, _, update, _, contract2, body) =>
      withContract(contract1, contract2, c => {
        Scope(Nil, Loop[G](evalOrNop(init), cond.map(convert(_)) getOrElse tt, evalOrNop(update), c.consumeLoopContract(), convert(body))(blame(stat)))
      })
    case IterationStatement3(contract1, maybePragma, _, _, init, cond, _, update, _, contract2, body) =>
      withContract(contract1, contract2, c => {
        Scope(Nil, Loop[G](CDeclarationStatement(convert(init)), cond.map(convert(_)) getOrElse tt, evalOrNop(update), c.consumeLoopContract(), convert(body))(blame(stat)))
      })
  }

  def convert(implicit stat: JumpStatementContext): Statement[G] = stat match {
    case JumpStatement0(_, label, _) =>
      CGoto(convert(label))
    case JumpStatement1(_, _) => col.Continue(None)
    case JumpStatement2(_, _) => col.Break(None)
    case JumpStatement3(_, None, _) => col.Return(col.Void())
    case JumpStatement3(_, Some(value), _) => col.Return(convert(value))
    case JumpStatement4(_, _, _) => ??(stat)
  }

  def convert(implicit exprs: ArgumentExpressionListContext): Seq[Expr[G]] = exprs match {
    case ArgumentExpressionList0(argument) => Seq(convert(argument))
    case ArgumentExpressionList1(init, _, last) => convert(init) :+ convert(last)
  }

  def convert(implicit expr: ExpressionContext): Expr[G] = expr match {
    case Expression0(inner) => convert(inner)
    case Expression1(first, _, result) =>
      With(Eval(convert(first)), convert(result))
  }

  def convert(implicit expr: InitializerContext): Expr[G] = expr match {
    case Initializer0(_, _, _) => ??(expr)
    case Initializer1(_, _, _, _) => ??(expr)
    case Initializer2(inner) => convert(inner)
  }

  def convert(implicit expr: AssignmentExpressionContext): Expr[G] = expr match {
    case AssignmentExpression0(pre, inner, post) =>
      convertEmbedWith(pre, convertEmbedThen(post, convert(inner)))
    case AssignmentExpression1(pre, targetNode, AssignmentOperator0(op), valueNode, post) =>
      val target = convert(targetNode)
      val value = convert(valueNode)

      val e = PreAssignExpression(target, op match {
        case "=" => value
        case "*=" => Mult(target, value)
        case "/=" => FloorDiv(target, value)(blame(expr))
        case "%=" => col.Mod(target, value)(blame(expr))
        case "+=" => col.Plus(target, value)
        case "-=" => col.Minus(target, value)
        case "<<=" => BitShl(target, value)
        case ">>=" => BitShr(target, value)
        case "&=" => BitAnd(target, value)
        case "^=" => BitXor(target, value)
        case "|=" => BitOr(target, value)
      })(blame(expr))

      convertEmbedWith(pre, convertEmbedThen(post, e))
  }

  def convert(implicit expr: ConditionalExpressionContext): Expr[G] = expr match {
    case ConditionalExpression0(inner) => convert(inner)
    case ConditionalExpression1(cond, _, whenTrue, _, whenFalse) =>
      Select(convert(cond), convert(whenTrue), convert(whenFalse))
  }

  def convert(implicit expr: ImplicationExpressionContext): Expr[G] = expr match {
    case ImplicationExpression0(left, ImplicationOp0(specOp), right) =>
      convert(specOp, convert(left), convert(right))
    case ImplicationExpression1(inner) => convert(inner)
  }

  def convert(implicit expr: LogicalOrExpressionContext): Expr[G] = expr match {
    case LogicalOrExpression0(inner) => convert(inner)
    case LogicalOrExpression1(left, _, right) => AmbiguousOr(convert(left), convert(right))
  }

  def convert(implicit expr: LogicalAndExpressionContext): Expr[G] = expr match {
    case LogicalAndExpression0(inner) => convert(inner)
    case LogicalAndExpression1(left, op, right) => op match {
      case LogicalAndOp0(_) => col.And(convert(left), convert(right))
      case LogicalAndOp1(valOp) => convert(valOp, convert(left), convert(right))
    }
  }

  def convert(implicit expr: InclusiveOrExpressionContext): Expr[G] = expr match {
    case InclusiveOrExpression0(inner) => convert(inner)
    case InclusiveOrExpression1(left, _, right) => BitOr(convert(left), convert(right))
  }

  def convert(implicit expr: ExclusiveOrExpressionContext): Expr[G] = expr match {
    case ExclusiveOrExpression0(inner) => convert(inner)
    case ExclusiveOrExpression1(left, _, right) => BitXor(convert(left), convert(right))
  }

  def convert(implicit expr: AndExpressionContext): Expr[G] = expr match {
    case AndExpression0(inner) => convert(inner)
    case AndExpression1(left, _, right) => BitAnd(convert(left), convert(right))
  }

  def convert(implicit expr: EqualityExpressionContext): Expr[G] = expr match {
    case EqualityExpression0(inner) => convert(inner)
    case EqualityExpression1(left, _, right) => Eq(convert(left), convert(right))
    case EqualityExpression2(left, _, right) => Neq(convert(left), convert(right))
  }

  def convert(implicit expr: RelationalExpressionContext): Expr[G] = expr match {
    case RelationalExpression0(inner) => convert(inner)
    case RelationalExpression1(left, RelationalOp0(op), right) => op match {
      case "<" => col.Less(convert(left), convert(right))
      case ">" => col.Greater(convert(left), convert(right))
      case "<=" => LessEq(convert(left), convert(right))
      case ">=" => GreaterEq(convert(left), convert(right))
    }
    case RelationalExpression1(left, RelationalOp1(specOp), right) =>
      convert(specOp, convert(left), convert(right))
  }

  def convert(implicit expr: ShiftExpressionContext): Expr[G] = expr match {
    case ShiftExpression0(inner) => convert(inner)
    case ShiftExpression1(left, _, right) => BitShl(convert(left), convert(right))
    case ShiftExpression2(left, _, right) => BitShr(convert(left), convert(right))
  }

  def convert(implicit expr: AdditiveExpressionContext): Expr[G] = expr match {
    case AdditiveExpression0(inner) => convert(inner)
    case AdditiveExpression1(left, _, right) => AmbiguousPlus(convert(left), convert(right))(blame(expr))
    case AdditiveExpression2(left, _, right) => col.Minus(convert(left), convert(right))
  }

  def convert(implicit expr: MultiplicativeExpressionContext): Expr[G] = expr match {
    case MultiplicativeExpression0(inner) => convert(inner)
    case MultiplicativeExpression1(left, op, right) => op match {
      case MultiplicativeOp0(_) => AmbiguousMult(convert(left), convert(right))
      case MultiplicativeOp1(_) => FloorDiv(convert(left), convert(right))(blame(expr))
      case MultiplicativeOp2(_) => col.Mod(convert(left), convert(right))(blame(expr))
      case MultiplicativeOp3(_) => col.Div(convert(left), convert(right))(blame(expr))
    }
  }

  def convert(implicit expr: PrependExpressionContext): Expr[G] = expr match {
    case PrependExpression0(left, PrependOp0(specOp), right) => convert(specOp, convert(left), convert(right))
    case PrependExpression1(inner) => convert(inner)
  }

  def convert(implicit expr: CastExpressionContext): Expr[G] = expr match {
    case CastExpression0(inner) => convert(inner)
    case CastExpression1(_, _, _, _) => ??(expr)
    case CastExpression2(_, _, _, _, _) => ??(expr)
  }

  def convert(implicit expr: UnaryExpressionContext): Expr[G] = expr match {
    case UnaryExpression0(inner) => convert(inner)
    case UnaryExpression1(_, arg) =>
      val target = convert(arg)
      PreAssignExpression(target, col.Plus(target, const(1)))(blame(expr))
    case UnaryExpression2(_, arg) =>
      val target = convert(arg)
      PreAssignExpression(target, col.Minus(target, const(1)))(blame(expr))
    case UnaryExpression3(UnaryOperator0(op), arg) => op match {
      case "&" => AddrOf(convert(arg))
      case "*" => DerefPointer(convert(arg))(blame(expr))
      case "+" => convert(arg)
      case "-" => UMinus(convert(arg))
      case "~" => BitNot(convert(arg))
      case "!" => col.Not(convert(arg))
    }
    case UnaryExpression4(_, _) => ??(expr)
    case UnaryExpression5(_, _, _, _) => ??(expr)
    case UnaryExpression6(_, _, _, _) => ??(expr)
    case UnaryExpression7(_, _) => ??(expr)
  }

  def convert(implicit expr: PostfixExpressionContext): Expr[G] = expr match {
    case PostfixExpression0(inner) => convert(inner)
    case PostfixExpression1(arr, _, idx, _) => AmbiguousSubscript(convert(arr), convert(idx))(blame(expr))
    case PostfixExpression2(f, given, _, args, _, yields) =>
      CInvocation(convert(f), args.map(convert(_)) getOrElse Nil,
        convertEmbedGiven(given), convertEmbedYields(yields))
    case PostfixExpression3(struct, _, field) => CStructAccess(convert(struct), convert(field))(blame(expr))
    case PostfixExpression4(struct, _, field) => CStructDeref(convert(struct), convert(field))
    case PostfixExpression5(targetNode, _) =>
      val target = convert(targetNode)
      PostAssignExpression(target, col.Plus(target, const(1)))(blame(expr))
    case PostfixExpression6(targetNode, _) =>
      val target = convert(targetNode)
      PostAssignExpression(target, col.Minus(target, const(1)))(blame(expr))
    case PostfixExpression7(e, SpecPostfix0(postfix)) => convert(postfix, convert(e))
    case PostfixExpression8(_, _, _, _, _, _) => ??(expr)
    case PostfixExpression9(_, _, _, _, _, _, _) => ??(expr)
    case PostfixExpression10(_, _, _, _, _, _, _) => ??(expr)
    case PostfixExpression11(_, _, _, _, _, _, _, _) => ??(expr)
    case PostfixExpression12(GpgpuCudaKernelInvocation0(given, name, _, blocks, _, threads, _, _, args, _, yields)) =>
      GpgpuCudaKernelInvocation(convert(name), convert(blocks), convert(threads), convert(args),
        convertEmbedGiven(given), convertEmbedYields(yields))
  }

  def convert(implicit expr: AnnotatedPrimaryExpressionContext): Expr[G] = expr match {
    case AnnotatedPrimaryExpression0(pre, inner, post) =>
      convertEmbedWith(pre, convertEmbedThen(post, convert(inner)))
  }

  def convert(implicit expr: PrimaryExpressionContext): Expr[G] = expr match {
    case PrimaryExpression0(inner) => convert(inner)
    case PrimaryExpression1(name) => name match {
      case ClangIdentifier0(specInSpec) => convert(specInSpec)
      case ClangIdentifier1(name) => CLocal(name)
      case ClangIdentifier2(_) => CLocal(convert(name))
    }
    case PrimaryExpression2(const) => IntegerValue(Integer.parseInt(const))
    case PrimaryExpression3(_) => ??(expr)
    case PrimaryExpression4(_, inner, _) => convert(inner)
    case PrimaryExpression5(_) => ??(expr)
    case PrimaryExpression6(_, _, _, _) => ??(expr)
    case PrimaryExpression7(_, _, _, _, _, _) => ??(expr)
    case PrimaryExpression8(_, _, _, _, _, _) => ??(expr)
    case PrimaryExpression9(_) => col.Null()
  }

  def convert(implicit ids: IdentifierListContext): Seq[String] = ids match {
    case IdentifierList0(id) => Seq(convert(id))
    case IdentifierList1(init, _, last) => convert(init) :+ convert(last)
  }

  def convert(id: ClangIdentifierContext): String = id match {
    case ClangIdentifier0(specInSpec) => specInSpec match {
      case ValIdEscape(id) => id.substring(1, id.length-1)
      case other => fail(other,
        f"This identifier is reserved, and cannot be declared or used in specifications. " +
          f"You might want to escape the identifier with backticks: `${other.getText}`")
    }
    case ClangIdentifier1(id) => id
    case ClangIdentifier2(specOutOfSpec) =>
      val text = specOutOfSpec.getText
      if(text.matches("[a-zA-Z_]+")) text
      else fail(specOutOfSpec, f"This identifier is not allowed in C.")
  }

  def convert(expr: LangExprContext): Expr[G] = expr match {
    case LangExpr0(expr) => convert(expr)
  }

  def convert(stat: LangStatementContext): Statement[G] = stat match {
    case LangStatement0(stat) => convert(stat)
  }

  def convert(implicit t: LangTypeContext): Type[G] = t match {
    case LangType0(typeSpec) => CPrimitiveType(Seq(convert(typeSpec)))
  }

  def convert(id: LangIdContext): String = id match {
    case LangId0(id) => convert(id)
  }

  def local(ctx: ParserRuleContext, name: String): Expr[G] =
    CLocal(name)(origin(ctx))

  def convert(decl: LangGlobalDeclContext): Seq[GlobalDeclaration[G]] = decl match {
    case LangGlobalDecl0(decl) => convert(decl)
  }

  def convert(decl: LangClassDeclContext): Seq[ClassDeclaration[G]] = Nil

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

  def convert(contract: ValContractClauseContext, collector: ContractCollector[G]): Unit = contract match {
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

  def convertEmbedGiven(implicit given: Option[ValEmbedGivenContext]): Seq[(String, Expr[G])] = given match {
    case None => Nil
    case Some(ValEmbedGiven0(_, inner, _)) => convertGiven(inner)
    case Some(ValEmbedGiven1(inner)) => convertGiven(Some(inner))
  }

  def convertGiven(implicit given: Option[ValGivenContext]): Seq[(String, Expr[G])] = given match {
    case None => Nil
    case Some(ValGiven0(_, _, mappings, _)) => convert(mappings)
  }

  def convert(implicit mappings: ValGivenMappingsContext): Seq[(String, Expr[G])] = mappings match {
    case ValGivenMappings0(arg, _, v) => Seq((convert(arg), convert(v)))
    case ValGivenMappings1(arg, _, v, _, more) => (convert(arg), convert(v)) +: convert(more)
  }

  def convertEmbedYields(implicit given: Option[ValEmbedYieldsContext]): Seq[(Expr[G], String)] = given match {
    case None => Nil
    case Some(ValEmbedYields0(_, inner, _)) => convertYields(inner)
    case Some(ValEmbedYields1(inner)) => convertYields(Some(inner))
  }

  def convertYields(implicit given: Option[ValYieldsContext]): Seq[(Expr[G], String)] = given match {
    case None => Nil
    case Some(ValYields0(_, _, mappings, _)) => convert(mappings)
  }

  def convert(implicit mappings: ValYieldsMappingsContext): Seq[(Expr[G], String)] = mappings match {
    case ValYieldsMappings0(target, _, res) => Seq((convert(target), convert(res)))
    case ValYieldsMappings1(target, _, res, _, more) => (convert(target), convert(res)) +: convert(more)
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

  def convert(implicit impOp: ValImpOpContext, left: Expr[G], right: Expr[G]): Expr[G] = impOp match {
    case ValImpOp0(_) => Wand(left, right)(origin(impOp))
    case ValImpOp1(_) => Implies(left, right)(origin(impOp))
  }

  def convert(implicit andOp: ValAndOpContext, left: Expr[G], right: Expr[G]): Expr[G] = andOp match {
    case ValAndOp0(_) => col.Star(left, right)(origin(andOp))
  }

  def convert(implicit inOp: ValInOpContext, left: Expr[G], right: Expr[G]): Expr[G] = inOp match {
    case ValInOp0(_) => AmbiguousMember(left, right)
  }

  def convert(implicit mulOp: ValMulOpContext, left: Expr[G], right: Expr[G]): Expr[G] = mulOp match {
    case ValMulOp0(_) => col.Div(left, right)(blame(mulOp))
  }

  def convert(implicit prependOp: ValPrependOpContext, left: Expr[G], right: Expr[G]): Expr[G] = prependOp match {
    case ValPrependOp0(_) => Cons(left, right)
  }

  def convert(implicit postfixOp: ValPostfixContext, xs: Expr[G]): Expr[G] = postfixOp match {
    case ValPostfix0(_, _, to, _) => Take(xs, convert(to))
    case ValPostfix1(_, from, _, None, _) => Drop(xs, convert(from))
    case ValPostfix1(_, from, _, Some(to), _) => Slice(xs, convert(from), convert(to))
    case ValPostfix2(_, idx, _, v, _) => SeqUpdate(xs, convert(idx), convert(v))
  }

  def convert(implicit block: ValEmbedStatementBlockContext): Block[G] = block match {
    case ValEmbedStatementBlock0(_, stats, _) => Block(stats.map(convert(_)))
    case ValEmbedStatementBlock1(stats) => Block(stats.map(convert(_)))
  }

  def convert(implicit stat: ValStatementContext): Statement[G] = stat match {
    case ValCreateWand(_, block) => WandCreate(convert(block))
    case ValQedWand(_, wand, _) => WandQed(convert(wand))
    case ValApplyWand(_, wand, _) => WandApply(convert(wand))
    case ValUseWand(_, wand, _) => WandUse(convert(wand))
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
    case ValRefute(_, assn, _) => Refute(convert(assn))
    case ValWitness(_, _, _) => ??(stat)
    case ValGhost(_, stat) => convert(stat)
    case ValSend(_, resource, _, label, _, offset, _) =>
      Send(convert(resource), new UnresolvedRef[G, LabelDecl[G]](convert(label)), convert(offset))
    case ValRecv(_, resource, _, label, _, offset, _) =>
      Recv(convert(resource), new UnresolvedRef[G, LabelDecl[G]](convert(label)), convert(offset))
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
      ParAtomic(Seq(new UnresolvedRef[G, ParInvariantDecl[G]](convert(invariant))), convert(body))
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
            c.consumeApplicableContract(),
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
          transform(new InstanceFunction[G](
            convert(t),
            args.map(convert(_)).getOrElse(Nil),
            typeArgs.map(convert(_)).getOrElse(Nil),
            convert(definition),
            c.consumeApplicableContract(), m.consume(m.inline))(
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
  // valMapPairs

  def convert(implicit e: ValPrimaryCollectionConstructorContext): Expr[G] = e match {
    case ValTypedLiteralSeq(_, _, t, _, _, exprs, _) => LiteralSeq(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValTypedLiteralSet(_, _, t, _, _, exprs, _) => LiteralSet(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValSetComprehension(_, _, t, _, _, value, _, selectors, _, something, _) => ??(e)
    case ValTypedLiteralBag(_, _, t, _, _, exprs, _) => LiteralBag(convert(t), exprs.map(convert(_)).getOrElse(Nil))
    case ValTypedLiteralMap(_, _, key, _, value, _, _, pairs, _) => ??(e)
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

  def convert(implicit e: ValPrimaryBinderContext): Expr[G] = e match {
    case ValRangeQuantifier(_, quant, t, id, _, from, _, to, _, body, _) =>
      val variable = new Variable[G](convert(t))(SourceNameOrigin(convert(id), origin(id)))
      val cond = SeqMember[G](Local(variable.ref), Range(convert(from), convert(to)))
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
    case ValShortQuantifier(_, quant, bindings, _, body, _) =>
      val variables = convert(bindings)
      quant match {
        case "∀" => Forall(variables, Nil, convert(body))
        case "∀*" => Starall(variables, Nil, convert(body))
        case "∃" => Exists(variables, Nil, convert(body))
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
    case ValAny(_) => Any()
    case ValIndependent(_, e, _, name, _) => ??(e)
    case ValScale(_, perm, _, predInvocation) => Scale(convert(perm), convert(predInvocation))
    case ValInlinePattern(_, pattern, _) => InlinePattern(convert(pattern))
    case ValUnfolding(_, predExpr, _, body) => Unfolding(convert(predExpr), convert(body))
    case ValOld(_, _, expr, _) => Old(convert(expr), at = None)(blame(e))
    case ValTypeof(_, _, expr, _) => TypeOf(convert(expr))
    case ValTypeValue(_, _, t, _) => TypeValue(convert(t))
    case ValHeld(_, _, obj, _) => Held(convert(obj))
  }

  def convert(implicit res: ValReservedContext): Expr[G] = res match {
    case ValReserved0(name) => fail(res,
      f"This identifier is reserved, and cannot be declared or used in specifications. " +
        f"You might want to escape the identifier with backticks: `$name`")
    case ValIdEscape(id) => local(res, id.substring(1, id.length-1))
    case ValResult(_) => AmbiguousResult()
    case ValCurrentThread(_) => CurrentThreadId()
    case ValNonePerm(_) => NoPerm()
    case ValWrite(_) => WritePerm()
    case ValRead(_) => ReadPerm()
    case ValNoneOption(_) => OptNone()
    case ValEmpty(_) => EmptyProcess()
    case ValLtid(_) => ???
    case ValGtid(_) => ???
    case ValTrue(_) => tt
    case ValFalse(_) => ff
  }

  def convert(implicit inv: ValGenericAdtInvocationContext): Expr[G] = inv match {
    case ValGenericAdtInvocation0(adt, _, typeArgs, _, _, func, _, args, _) =>
      ADTFunctionInvocation(Some((new UnresolvedRef[G, AxiomaticDataType[G]](convert(adt)), convert(typeArgs))),
        new UnresolvedRef[G, ADTFunction[G]](convert(func)), args.map(convert(_)).getOrElse(Nil))
  }
}
