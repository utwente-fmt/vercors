package vct.parsers.transform

import org.antlr.v4.runtime.ParserRuleContext
import vct.antlr4.generated.CParser._
import vct.antlr4.generated.CParserPatterns._
import vct.col.ast.Constant._
import vct.col.ast._
import vct.col.{ast => col}


case class CToCol(override val originProvider: OriginProvider, blameProvider: BlameProvider) extends ToCol(originProvider) {
  implicit def origin(implicit ctx: ParserRuleContext): Origin = {
    originProvider(ctx) // FIXME make blames nice
  }

  def convert(unit: CompilationUnitContext): Seq[GlobalDeclaration] = unit match {
    case CompilationUnit0(translationUnit, _) =>
      translationUnit.map(convert).getOrElse(Nil)
  }

  def convert(unit: TranslationUnitContext): Seq[GlobalDeclaration] =
    convertList(TranslationUnit0.unapply, TranslationUnit1.unapply)(unit).flatMap(convert(_))

  def convert(implicit externalDecl: ExternalDeclarationContext): Seq[GlobalDeclaration] = externalDecl match {
    case ExternalDeclaration0(funcDef) => Seq(convert(funcDef))
    case ExternalDeclaration1(decl) => Seq(CGlobalDeclaration(convert(decl)))
    case ExternalDeclaration2(valDecls) => convert(valDecls)
    case ExternalDeclaration3(";") => Nil
  }

  def convert(implicit funcDef: FunctionDefinitionContext): CFunctionDefinition = {
    funcDef match {
      case FunctionDefinition0(_, _, _, Some(declarationList), _) =>
        ??(declarationList)
      case FunctionDefinition0(maybeContract, declSpecs, declarator, None, body) =>
        CFunctionDefinition(convert(declSpecs), convert(declarator), convert(body))
    }
  }

  def convert(implicit decl: DeclarationContext): CDeclaration = decl match {
    case Declaration0(maybeContract, declSpecs, maybeInits, _) =>
      withContract(maybeContract, contract =>
        CDeclaration(contract.consumeApplicableContract(), col.Star.fold(contract.consume(contract.kernel_invariant)),
          specs=convert(declSpecs), inits=maybeInits.map(convert(_)) getOrElse Nil))
    case Declaration1(staticAssert) =>
      ??(staticAssert)
  }

  def convert(declSpecs: DeclarationSpecifiersContext): Seq[CDeclarationSpecifier] = declSpecs match {
    case DeclarationSpecifiers0(specs) => specs.map(convert(_))
  }

  def convert(declSpecs: DeclarationSpecifiers2Context): Seq[CDeclarationSpecifier] = declSpecs match {
    case DeclarationSpecifiers20(specs) => specs.map(convert(_))
  }

  def convert(implicit declSpec: DeclarationSpecifierContext): CDeclarationSpecifier = declSpec match {
    case DeclarationSpecifier0(storageClass) => convert(storageClass)
    case DeclarationSpecifier1(typeSpec) => convert(typeSpec)
    case DeclarationSpecifier2(typeQual) => CTypeQualifierDeclarationSpecifier(convert(typeQual))
    case DeclarationSpecifier3(functionSpecifier) => ??(functionSpecifier)
    case DeclarationSpecifier4(alignmentSpecifier) => ??(alignmentSpecifier)
    case DeclarationSpecifier5(kernelSpecifier) => convert(kernelSpecifier)
    case DeclarationSpecifier6(valEmbedModifier) => withModifiers(valEmbedModifier, m => {
      if(m.consume(m.pure))
        CPure()
      else if(m.consume(m.inline))
        CInline()
      else
        fail(m.nodes.head, "This modifier cannot be attached to a declaration in C")
    })
  }

  def convert(implicit storageClass: StorageClassSpecifierContext): CStorageClassSpecifier = storageClass match {
    case StorageClassSpecifier0(_) => CTypedef()
    case StorageClassSpecifier1(_) => ??(storageClass)
    case StorageClassSpecifier2(_) => CStatic()
    case StorageClassSpecifier3(_) => ??(storageClass)
    case StorageClassSpecifier4(_) => ??(storageClass)
    case StorageClassSpecifier5(_) => ??(storageClass)
  }

  def convert(implicit typeSpec: TypeSpecifierContext): CTypeSpecifier = typeSpec match {
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

  def convert(implicit quals: TypeQualifierListContext): Seq[CTypeQualifier] =
    convertList(TypeQualifierList0.unapply, TypeQualifierList1.unapply)(quals).map(convert(_))

  def convert(implicit qual: TypeQualifierContext): CTypeQualifier = qual match {
    case TypeQualifier0(_) => CConst()
    case TypeQualifier1(_) => CRestrict()
    case TypeQualifier2(_) => CVolatile()
    case TypeQualifier3(_) => CAtomic()
  }

  def convert(implicit kernel: GpgpuKernelSpecifierContext): CGpgpuKernelSpecifier = kernel match {
    case GpgpuKernelSpecifier0(_) => CKernel()
  }

  def convert(implicit decls: InitDeclaratorListContext): Seq[CInit] = decls match {
    case InitDeclaratorList0(decl) => Seq(convert(decl))
    case InitDeclaratorList1(init, _, last) => convert(init) :+ convert(last)
  }

  def convert(implicit decl: InitDeclaratorContext): CInit = decl match {
    case InitDeclarator0(inner) => CInit(convert(inner), None)
    case InitDeclarator1(inner, _, init) => CInit(convert(inner), Some(convert(init)))
  }

  def convert(implicit decl: DeclaratorContext): CDeclarator = decl match {
    case Declarator0(_, _, extension::_) => ??(extension)
    case Declarator0(Some(ptr), inner, Nil) =>
      val pointers = convert(ptr)
      CPointerDeclarator(pointers, convert(inner))
    case Declarator0(None, inner, Nil) => convert(inner)
  }

  def convert(implicit ptr: PointerContext): Seq[CPointer] = ptr match {
    case Pointer0(_, quals) =>
      Seq(CPointer(quals.map(convert(_)) getOrElse Nil))
    case Pointer1(_, quals, tail) =>
      CPointer(quals.map(convert(_)) getOrElse Nil) +: convert(tail)
    case Pointer2(_, quals) =>
      Seq(CPointer(Nil), CPointer(quals.map(convert(_)) getOrElse Nil))
    case Pointer3(_, quals, tail) =>
      Seq(CPointer(Nil), CPointer(quals.map(convert(_)) getOrElse Nil)) ++ convert(tail)
    case Pointer4(_, _) => ??(ptr)
    case Pointer5(_, _, _) => ??(ptr)
  }

  def convert(implicit decl: DirectDeclaratorContext): CDeclarator = decl match {
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

  def convert(implicit params: ParameterListContext): Seq[CParam] = params match {
    case ParameterList0(decl) => Seq(convert(decl))
    case ParameterList1(init, _, last) => convert(init) :+ convert(last)
  }

  def convert(implicit param: ParameterDeclarationContext): CParam = param match {
    case ParameterDeclaration0(declSpecs, declarator) =>
      CParam(convert(declSpecs), convert(declarator))
    case ParameterDeclaration1(_, _) =>
      ??(param)
  }

  def convert(implicit stat: StatementContext): Statement = stat match {
    case Statement0(stat) => convert(stat)
    case Statement1(stat) => convert(stat)
    case Statement2(stat) => convert(stat)
    case Statement3(stat) => convert(stat)
    case Statement4(stat) => convert(stat)
    case Statement5(stat) => convert(stat)
    case _: Statement6Context => ??(stat)
  }

  def convert(implicit block: CompoundStatementContext): Statement = block match {
    case CompoundStatement0(_, stats, _) =>
      Block(stats.map(convert(_)) getOrElse Nil)
    case CompoundStatement1(ompPragma, _, contract, stats, _) =>
      ???
  }

  def convert(implicit stats: BlockItemListContext): Seq[Statement] =
    convertList(BlockItemList0.unapply, BlockItemList1.unapply)(stats).map(convert(_))

  def convert(implicit stat: BlockItemContext): Statement = stat match {
    case BlockItem0(decl) =>
      CDeclarationStatement(convert(decl))
    case BlockItem1(stat) => convert(stat)
    case BlockItem2(embedStats) => convert(embedStats)
    case BlockItem3(embedStat) => convert(embedStat)
    case BlockItem4(GpgpuLocalBarrier0(contract, _, _, _, _)) => withContract(contract, c => {
      GpgpuLocalBarrier(col.Star.fold(c.consume(c.requires)), col.Star.fold(c.consume(c.ensures)))
    })
    case BlockItem5(GpgpuGlobalBarrier0(contract, _, _, _, _)) => withContract(contract, c => {
      GpgpuGlobalBarrier(col.Star.fold(c.consume(c.requires)), col.Star.fold(c.consume(c.ensures)))
    })
    case BlockItem6(GpgpuAtomicBlock0(_, impl, withThen)) =>
      val (before, after) = withThen.map(convert(_)).getOrElse((Nil, Nil))
      GpgpuAtomic(convert(impl), Block(before), Block(after))
  }

  def convert(implicit stat: LabeledStatementContext): Statement = stat match {
    case LabeledStatement0(label, _, inner) =>
      CLabeledStatement(convert(label), convert(inner))
    case LabeledStatement1(_, _, _, _) => ??(stat)
    case LabeledStatement2(_, _, _) => ??(stat)
  }

  def convert(implicit stat: ExpressionStatementContext): Statement = stat match {
    case ExpressionStatement0(None, _) => Block(Nil)
    case ExpressionStatement0(Some(expr), _) => Eval(convert(expr))
  }

  def convert(implicit stat: SelectionStatementContext): Statement = stat match {
    case SelectionStatement0(_, _, cond, _, whenTrue, None) =>
      Branch(Seq((convert(cond), convert(whenTrue))))
    case SelectionStatement0(_, _, cond, _, whenTrue, Some(whenFalse)) =>
      Branch(Seq((convert(cond), convert(whenTrue)), (true, convert(whenFalse))))
    case SelectionStatement1(_, _, _, _, _) => ??(stat)
  }

  def convert(implicit stat: ElseBranchContext): Statement = stat match {
    case ElseBranch0(_, stat) => convert(stat)
  }

  def evalOrNop(implicit expr: Option[ExpressionContext]): Statement = expr match {
    case Some(expr) => Eval(convert(expr))(origin(expr))
    case None =>
      // PB: strictly speaking it would be nice if we can point to the empty range that indicates the absence of a statement here.
      Block(Nil)(DiagnosticOrigin)
  }

  def convert(implicit stat: IterationStatementContext): Statement = stat match {
    case IterationStatement0(contract1, _, _, cond, _, contract2, body) => withContract(contract1, contract2, c => {
      Loop(Block(Nil), convert(cond), Block(Nil), col.Star.fold(c.consume(c.loop_invariant)), convert(body))
    })
    case IterationStatement1(_, _, _, _, _, _, _) => ??(stat)
    case IterationStatement2(contract1, maybePragma, _, _, init, _, cond, _, update, _, contract2, body) =>
      withContract(contract1, contract2, c => {
        Loop(evalOrNop(init), cond.map(convert(_)) getOrElse true, evalOrNop(update), col.Star.fold(c.consume(c.loop_invariant)), convert(body))
      })
    case IterationStatement3(contract1, maybePragma, _, _, init, cond, _, update, _, contract2, body) =>
      withContract(contract1, contract2, c => {
        Loop(CDeclarationStatement(convert(init)), cond.map(convert(_)) getOrElse true, evalOrNop(update), col.Star.fold(c.consume(c.loop_invariant)), convert(body))
      })
  }

  def convert(implicit stat: JumpStatementContext): Statement = stat match {
    case JumpStatement0(_, label, _) =>
      CGoto(convert(label))
    case JumpStatement1(_, _) => col.Continue(None)
    case JumpStatement2(_, _) => col.Break(None)
    case JumpStatement3(_, None, _) => col.Return(col.Void())
    case JumpStatement3(_, Some(value), _) => col.Return(convert(value))
    case JumpStatement4(_, _, _) => ??(stat)
  }

  def convert(implicit exprs: ArgumentExpressionListContext): Seq[Expr] = exprs match {
    case ArgumentExpressionList0(argument) => Seq(convert(argument))
    case ArgumentExpressionList1(init, _, last) => convert(init) :+ convert(last)
  }

  def convert(implicit expr: ExpressionContext): Expr = expr match {
    case Expression0(inner) => convert(inner)
    case Expression1(first, _, result) =>
      With(Eval(convert(first)), convert(result))
  }

  def convert(implicit expr: InitializerContext): Expr = expr match {
    case Initializer0(_, _, _) => ??(expr)
    case Initializer1(_, _, _, _) => ??(expr)
    case Initializer2(inner) => convert(inner)
  }

  def convert(implicit expr: AssignmentExpressionContext): Expr = expr match {
    case AssignmentExpression0(inner) => convert(inner)
    case AssignmentExpression1(targetNode, AssignmentOperator0(op), valueNode) =>
      val target = convert(targetNode)
      val value = convert(valueNode)

      PreAssignExpression(target, op match {
        case "=" => value
        case "*=" => Mult(target, value)
        case "/=" => FloorDiv(target, value)(blameProvider(expr))
        case "%=" => col.Mod(target, value)(blameProvider(expr))
        case "+=" => col.Plus(target, value)
        case "-=" => col.Minus(target, value)
        case "<<=" => BitShl(target, value)
        case ">>=" => BitShr(target, value)
        case "&=" => BitAnd(target, value)
        case "^=" => BitXor(target, value)
        case "|=" => BitOr(target, value)
      })
  }

  def convert(implicit expr: ConditionalExpressionContext): Expr = expr match {
    case ConditionalExpression0(inner) => convert(inner)
    case ConditionalExpression1(cond, _, whenTrue, _, whenFalse) =>
      Select(convert(cond), convert(whenTrue), convert(whenFalse))
  }

  def convert(implicit expr: LogicalOrExpressionContext): Expr = expr match {
    case LogicalOrExpression0(inner) => convert(inner)
    case LogicalOrExpression1(left, op, right) => op match {
      case LogicalOrOp0(_) => col.Or(convert(left), convert(right))
      case LogicalOrOp1(valOp) => convert(valOp, convert(left), convert(right))
    }
  }

  def convert(implicit expr: LogicalAndExpressionContext): Expr = expr match {
    case LogicalAndExpression0(inner) => convert(inner)
    case LogicalAndExpression1(left, op, right) => op match {
      case LogicalAndOp0(_) => col.And(convert(left), convert(right))
      case LogicalAndOp1(valOp) => convert(valOp, convert(left), convert(right))
    }
  }

  def convert(implicit expr: InclusiveOrExpressionContext): Expr = expr match {
    case InclusiveOrExpression0(inner) => convert(inner)
    case InclusiveOrExpression1(left, _, right) => BitOr(convert(left), convert(right))
  }

  def convert(implicit expr: ExclusiveOrExpressionContext): Expr = expr match {
    case ExclusiveOrExpression0(inner) => convert(inner)
    case ExclusiveOrExpression1(left, _, right) => BitXor(convert(left), convert(right))
  }

  def convert(implicit expr: AndExpressionContext): Expr = expr match {
    case AndExpression0(inner) => convert(inner)
    case AndExpression1(left, _, right) => BitAnd(convert(left), convert(right))
  }

  def convert(implicit expr: EqualityExpressionContext): Expr = expr match {
    case EqualityExpression0(inner) => convert(inner)
    case EqualityExpression1(left, _, right) => Eq(convert(left), convert(right))
    case EqualityExpression2(left, _, right) => Neq(convert(left), convert(right))
  }

  def convert(implicit expr: RelationalExpressionContext): Expr = expr match {
    case RelationalExpression0(inner) => convert(inner)
    case RelationalExpression1(left, _, right) => col.Less(convert(left), convert(right))
    case RelationalExpression2(left, _, right) => col.Greater(convert(left), convert(right))
    case RelationalExpression3(left, _, right) => LessEq(convert(left), convert(right))
    case RelationalExpression4(left, _, right) => GreaterEq(convert(left), convert(right))
  }

  def convert(implicit expr: ShiftExpressionContext): Expr = expr match {
    case ShiftExpression0(inner) => convert(inner)
    case ShiftExpression1(left, _, right) => BitShl(convert(left), convert(right))
    case ShiftExpression2(left, _, right) => BitShr(convert(left), convert(right))
  }

  def convert(implicit expr: AdditiveExpressionContext): Expr = expr match {
    case AdditiveExpression0(inner) => convert(inner)
    case AdditiveExpression1(left, _, right) => col.Plus(convert(left), convert(right))
    case AdditiveExpression2(left, _, right) => col.Minus(convert(left), convert(right))
  }

  def convert(implicit expr: MultiplicativeExpressionContext): Expr = expr match {
    case MultiplicativeExpression0(inner) => convert(inner)
    case MultiplicativeExpression1(left, op, right) => op match {
      case MultiplicativeOp0(_) => Mult(convert(left), convert(right))
      case MultiplicativeOp1(_) => FloorDiv(convert(left), convert(right))(blameProvider(expr))
      case MultiplicativeOp2(_) => col.Mod(convert(left), convert(right))(blameProvider(expr))
      case MultiplicativeOp3(_) => col.Div(convert(left), convert(right))(blameProvider(expr))
    }
  }

  def convert(implicit expr: CastExpressionContext): Expr = expr match {
    case CastExpression0(inner) => convert(inner)
    case CastExpression1(_, _, _, _) => ??(expr)
    case CastExpression2(_, _, _, _, _) => ??(expr)
  }

  def convert(implicit expr: UnaryExpressionContext): Expr = expr match {
    case UnaryExpression0(inner) => convert(inner)
    case UnaryExpression1(_, arg) =>
      val target = convert(arg)
      PreAssignExpression(target, col.Plus(target, 1))
    case UnaryExpression2(_, arg) =>
      val target = convert(arg)
      PreAssignExpression(target, col.Minus(target, 1))
    case UnaryExpression3(UnaryOperator0(op), arg) => op match {
      case "&" => AddrOf(convert(arg))
      case "*" => DerefPointer(convert(arg))
      case "+" => UPlus(convert(arg))
      case "-" => UMinus(convert(arg))
      case "~" => BitNot(convert(arg))
      case "!" => col.Not(convert(arg))
    }
    case UnaryExpression4(_, _) => ??(expr)
    case UnaryExpression5(_, _, _, _) => ??(expr)
    case UnaryExpression6(_, _, _, _) => ??(expr)
    case UnaryExpression7(_, _) => ??(expr)
  }

  def convert(implicit expr: PostfixExpressionContext): Expr = expr match {
    case PostfixExpression0(inner) => convert(inner)
    case PostfixExpression1(arr, _, idx, _) => AmbiguousSubscript(convert(arr), convert(idx))
    case PostfixExpression2(f, _, args, _) => CInvocation(convert(f), args.map(convert(_)) getOrElse Nil)
    case PostfixExpression3(struct, _, field) => CStructAccess(convert(struct), convert(field))
    case PostfixExpression4(struct, _, field) => CStructDeref(convert(struct), convert(field))
    case PostfixExpression5(targetNode, _) =>
      val target = convert(targetNode)
      PostAssignExpression(target, col.Plus(target, 1))
    case PostfixExpression6(targetNode, _) =>
      val target = convert(targetNode)
      PostAssignExpression(target, col.Minus(target, 1))
    case PostfixExpression7(_, _, _, _, _, _) => ??(expr)
    case PostfixExpression8(_, _, _, _, _, _, _) => ??(expr)
    case PostfixExpression9(_, _, _, _, _, _, _) => ??(expr)
    case PostfixExpression10(_, _, _, _, _, _, _, _) => ??(expr)
    case PostfixExpression11(GpgpuCudaKernelInvocation0(name, _, blocks, _, threads, _, _, args, _, withThen)) =>
      GpgpuCudaKernelInvocation(convert(name), convert(blocks), convert(threads), convert(args))
  }

  def convert(implicit expr: PrimaryExpressionContext): Expr = expr match {
    case PrimaryExpression0(inner) => convert(inner)
    case PrimaryExpression1(name) => name match {
      case ClangIdentifier0(specInSpec) => convert(specInSpec)
      case ClangIdentifier1(name) => Local(new UnresolvedRef(name))
      case ClangIdentifier2(_) => Local(new UnresolvedRef(convert(name)))
    }
    case PrimaryExpression2(const) => Integer.parseInt(const)
    case PrimaryExpression3(_) => ??(expr)
    case PrimaryExpression4(_, inner, _) => convert(inner)
    case PrimaryExpression5(_) => ??(expr)
    case PrimaryExpression6(_, _, _, _) => ??(expr)
    case PrimaryExpression7(_, _, _, _, _, _) => ??(expr)
    case PrimaryExpression8(_, _, _, _, _, _) => ??(expr)
  }

  def convert(implicit ids: IdentifierListContext): Seq[String] = ids match {
    case IdentifierList0(id) => Seq(convert(id))
    case IdentifierList1(init, _, last) => convert(init) :+ convert(last)
  }

  def convert(id: ClangIdentifierContext): String = id match {
    case ClangIdentifier0(specInSpec) => specInSpec match {
      case ValReserved1(id) => id.substring(1, id.length-1)
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

  def convert(expr: LangExprContext): Expr = expr match {
    case LangExpr0(expr) => convert(expr)
  }

  def convert(stat: LangStatementContext): Statement = stat match {
    case LangStatement0(stat) => convert(stat)
  }

  def convert(implicit t: LangTypeContext): Type = t match {
    case LangType0(typeSpec) => CPrimitiveType(Seq(convert(typeSpec)))
  }

  def convert(id: LangIdContext): String = id match {
    case LangId0(id) => convert(id)
  }

  def convert(decl: LangGlobalDeclContext): Seq[GlobalDeclaration] = decl match {
    case LangGlobalDecl0(decl) => convert(decl)
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
      collector.signals += ((contract, (variable, convert(exp))))
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
    case ValModifier1(_) => collector.static += mod
  }

  private def reduceWithThen(xs: Seq[(Seq[Statement], Seq[Statement])]): (Seq[Statement], Seq[Statement]) =
    (xs.flatMap(_._1), xs.flatMap(_._2))

  def convert(implicit withThen: ValEmbedWithThenContext): (Seq[Statement], Seq[Statement]) = withThen match {
    case ValEmbedWithThen0(blocks) =>
      reduceWithThen(blocks.map(convert(_)))
  }

  def convert(implicit withThen: ValEmbedWithThenBlockContext): (Seq[Statement], Seq[Statement]) = withThen match {
    case ValEmbedWithThenBlock0(_, specs, _) =>
      reduceWithThen(specs.map(convert(_)))
    case ValEmbedWithThenBlock1(specs) =>
      reduceWithThen(specs.map(convert(_)))
  }

  def convert(implicit withThen: ValWithThenContext): (Seq[Statement], Seq[Statement]) = withThen match {
    case ValWithThen0(_, stat) => (convert(stat) +: Nil, Nil)
    case ValWithThen1(_, stat) => (Nil, convert(stat) +: Nil)
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

  def convert(implicit block: ValEmbedStatementBlockContext): Block = block match {
    case ValEmbedStatementBlock0(_, stats, _) => Block(stats.map(convert(_)))
    case ValEmbedStatementBlock1(stats) => Block(stats.map(convert(_)))
  }

  def convert(implicit stat: ValStatementContext): Statement = stat match {
    case ValStatement0(_, block) => WandCreate(convert(block))
    case ValStatement1(_, wand, _) => WandQed(convert(wand))
    case ValStatement2(_, wand, _) => WandApply(convert(wand))
    case ValStatement3(_, wand, _) => WandUse(convert(wand))
    case ValStatement4(_, _, _) => ??(stat)
    case ValStatement5(_, target, _, process, _) => ModelCreate(convert(target), ???, convert(process))
    case ValStatement6(_, _, _, _, _) => ??(stat)
    case ValStatement7(_, model, _) => ModelDestroy(convert(model))
    case ValStatement8(_, model, _, leftPerm, _, leftProcess, _, rightPerm, _, rightProcess, _) =>
      ModelSplitInto(convert(model), convert(leftPerm), convert(leftProcess), convert(rightPerm), convert(rightProcess))
    case ValStatement9(_, model, _, leftPerm, _, leftProcess, _, rightPerm, _, rightProcess, _) =>
      ModelMergeFrom(convert(model), convert(leftPerm), convert(leftProcess), convert(rightPerm), convert(rightProcess))
    case ValStatement10(_, model, _, perm, _, chooseProcess, _, choice, _) =>
      ModelChoose(convert(model), convert(perm), convert(chooseProcess), convert(choice))
    case ValStatement11(_, predicate, _) =>
      Fold(convert(predicate))
    case ValStatement12(_, predicate, _) =>
      Unfold(convert(predicate))
    case ValStatement13(_, _, _) => ??(stat)
    case ValStatement14(_, _, _) => ??(stat)
    case ValStatement15(_, assn, _) => Assert(convert(assn))(blameProvider(stat))
    case ValStatement16(_, assn, _) => Assume(convert(assn))
    case ValStatement17(_, resource, _) => Inhale(convert(resource))
    case ValStatement18(_, resource, _) => Exhale(convert(resource))(blameProvider(stat))
    case ValStatement19(_, label, _) =>
      Label(new LabelDecl()(SourceNameOrigin(convert(label), origin(stat))))
    case ValStatement20(_, assn, _) => ??(stat)
    case ValStatement21(_, _, _) => ??(stat)
    case ValStatement22(_, stat) => convert(stat)
    case ValStatement23(_, resource, _, label, _, idx, _) => ??(stat) // FIXME PB: send/recv should be supported
    case ValStatement24(_, resource, _, label, _, idx, _) => ??(stat)
    case ValStatement25(_, _, _) => ??(stat)
    case ValStatement26(_, _, _) => ??(stat) // FIXME PB: csl_subject seems to be used
    case ValStatement27(_, _) => SpecIgnoreEnd()
    case ValStatement28(_, _) => SpecIgnoreStart()
    case ValStatement29(_, model, _, perm, _, after, _, action, modelHeapMap, _) =>
      modelHeapMap match {
        case Nil =>
        case mapping :: _ =>
          ??(mapping) // FIXME PB: disabled for now, pending investigation moving this to model creation
      }
      ModelDo(convert(model), convert(perm), convert(after), convert(action))
    case ValStatement30(_, _, invariant, _, body) =>
      ParAtomic(new UnresolvedRef(convert(invariant)), convert(body))
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
    case ValGlobalDeclaration0(_, name, _, left, _, right, _) =>
      Seq(new SimplificationRule(convert(left), convert(right))(SourceNameOrigin(convert(name), origin(decl))))
    case ValGlobalDeclaration1(modifiers, _, name, _, args, _, definition) =>
      withModifiers(modifiers, mods =>
        Seq(new Predicate(args.map(convert(_)).getOrElse(Nil), convert(definition),
          mods.consume(mods.threadLocal), mods.consume(mods.inline))
          (SourceNameOrigin(convert(name), origin(decl)))))
    case ValGlobalDeclaration2(contract, modifiers, _, t, name, _, args, _, definition) =>
      Seq(withContract(contract, c =>
        withModifiers(modifiers, m => {
          val namedOrigin = SourceNameOrigin(convert(name), origin(decl))
          new Function(convert(t), args.map(convert(_)).getOrElse(Nil), convert(definition),
            c.consumeApplicableContract(), m.consume(m.inline))(blameProvider(decl))(namedOrigin)
        })
      ))
    case ValGlobalDeclaration3(_, name, _, decls, _) =>
      Seq(new Model(decls.map(convert(_)))(SourceNameOrigin(convert(name), origin(decl))))
    case ValGlobalDeclaration4(_, inner) =>
      convert(inner)
  }

  def convert(implicit decl: ValModelDeclarationContext): ModelDeclaration = decl match {
    case ValModelDeclaration0(t, name, _) =>
      new ModelField(convert(t))(SourceNameOrigin(convert(name), origin(decl)))
    case ValModelDeclaration1(contract, _, name, _, args, _, _, definition, _) =>
      withContract(contract, c => {
        new ModelProcess(args.map(convert(_)).getOrElse(Nil), convert(definition),
          col.Star.fold(c.consume(c.requires)), col.Star.fold(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef(_)), c.consume(c.accessible).map(new UnresolvedRef(_)))(
          SourceNameOrigin(convert(name), origin(decl)))
      })
    case ValModelDeclaration2(contract, _, name, _, args, _, _) =>
      withContract(contract, c => {
        new ModelAction(args.map(convert(_)).getOrElse(Nil),
          col.Star.fold(c.consume(c.requires)), col.Star.fold(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef(_)), c.consume(c.accessible).map(new UnresolvedRef(_)))(
          SourceNameOrigin(convert(name), origin(decl)))
      })
  }

  def convert(implicit definition: ValDefContext): Option[Expr] = definition match {
    case ValDef0(_) => None
    case ValDef1(_, expr, _) => Some(convert(expr))
  }

  def convert(implicit t: ValTypeContext): Type = t match {
    case ValType0(name) => name match {
      case "resource" => TResource()
      case "process" => TProcess()
      case "frac" => TFraction()
      case "zfrac" => TZFraction()
      case "rational" => TRational()
      case "bool" => TBool()
    }
    case ValType1(_, _, element, _) => TSeq(convert(element))
    case ValType2(_, _, element, _) => TSet(convert(element))
    case ValType3(_, _, element, _) => TBag(convert(element))
    case ValType4(_, _, element, _) => TPointer(convert(element))
  }

  def convert(implicit e: ValPrimaryContext): Expr = e match {
    case ValPrimary0(_, _, _, _) => ??(e)
    case ValPrimary1(_, scale, _, resource) => Scale(convert(scale), convert(resource))
    case ValPrimary2(_, xs, _) => Size(convert(xs))
    case ValPrimary3(_, predicate, _, body) => Unfolding(convert(predicate), convert(body))
    case ValPrimary4(_, expr, _, indep, _) => ???
    case ValPrimary5(_, x, _, xs, _) => AmbiguousMember(convert(x), convert(xs))
    case ValPrimary6(_, from, _, to, _) => Range(convert(from), convert(to))
    case ValPrimary7(_) => ???
    case ValPrimary8(_) => ???
    case ValPrimary9(_, binder, t, id, _, from, _, to, _, body, _) =>
      val name = convert(id)
      val variable = new Variable(convert(t))(SourceNameOrigin(name, origin(id)))
      val condition = SeqMember(Local(new DirectRef(variable)), Range(convert(from), convert(to)))
      val main = convert(body)
      binder match {
        case "\\forall*" => Starall(Seq(variable), Seq(), Implies(condition, main))
        case "\\forall" => Forall(Seq(variable), Seq(), Implies(condition, main))
        case "\\exists" => Exists(Seq(variable), Seq(), col.And(condition, main))
      }
    case ValPrimary10(_, binder, t, id, _, cond, _, body, _) =>
      val variable = new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id)))
      binder match {
        case "\\forall*" => Starall(Seq(variable), Seq(), Implies(convert(cond), convert(body)))
        case "\\forall" => Forall(Seq(variable), Seq(), Implies(convert(cond), convert(body)))
        case "\\exists" => Exists(Seq(variable), Seq(), col.And(convert(cond), convert(body)))
      }
    case ValPrimary11(_, _, t, id, _, binding, _, body, _) =>
      Let(new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id))),
        convert(binding), convert(body))
    case ValPrimary12(_, _, t, id, _, cond, _, body, _) =>
      Sum(Seq(new Variable(convert(t))(SourceNameOrigin(convert(id), origin(id)))),
        convert(cond), convert(body))
    case ValPrimary13(_, _, xs, _) => Length(convert(xs))
    case ValPrimary14(_, _, inner, _) => Old(convert(inner), at=None)(blameProvider(e))
    case ValPrimary15(_, _, inner, _) => TypeOf(convert(inner))
    case ValPrimary16(_, _, matrix, _, width, _, height, _) =>
      ValidMatrix(convert(matrix), convert(width), convert(height))
    case ValPrimary17(_, _, array, _, length, _) => ValidArray(convert(array), convert(length))
    case ValPrimary18(_, _, pointer, _, length, _, perm, _) =>
      PermPointer(convert(pointer), convert(length), convert(perm))
    case ValPrimary19(_, _, pointer, _, index, _, perm, _) =>
      PermPointerIndex(convert(pointer), convert(index), convert(perm))
    case ValPrimary20(_, _, array, _, from, _, to, _) =>
      Values(convert(array), convert(from), convert(to))
    case ValPrimary21(_, _, _, _, _, _) => ???
    case ValPrimary22(_, _, _, _, _, _) => ???
    case ValPrimary23(_, _, _, _) => ???
    case ValPrimary24(_, _, _, _, _, _) => ???
    case ValPrimary25(_, _, _, _, _, _) => ???
    case ValPrimary26(_, _, _, _) => ???
    case ValPrimary27(_, _, _) => ??? // FIXME PB: consider just dropping this syntax (used in -*)
    case ValPrimary28(_, trigger, _) => InlinePattern(convert(trigger))
    case ValPrimary29(_, _, expr, _, reducibleOp, _) => ???
    case ValPrimary30(_, _, future, _, state, _) => ???
    case ValPrimary31(_, _, _, _, _, _) => ??? // FIXME PB: unused
    case ValPrimary32(_, _, loc, _, perm, _) => ActionPerm(convert(loc), convert(perm))
    case ValPrimary33(_, _, _, _, _, _, _, _, _, _, _, _) => ??? // FIXME PB: unused
    case ValPrimary34(_, _, map, _, key, _, value, _) => MapCons(convert(map), convert(key), convert(value))
    case ValPrimary35(_, _, map, _) => MapSize(convert(map))
    case ValPrimary36(_, _, _, _, _, _) => ???
    case ValPrimary37(_, _, map1, _, map2, _) => MapDisjoint(convert(map1), convert(map2))
    case ValPrimary38(_, _, map1, _, map2, _) => MapEq(convert(map1), convert(map2))
    case ValPrimary39(_, _, future, _, perm, _, process, _) => ???
    case ValPrimary40(_, _, map, _, key, _) => MapGet(convert(map), convert(key))
    case ValPrimary41(_, _, tup, _) => TupGet(convert(tup), 0)
    case ValPrimary42(_, _, opt, _) => OptGet(convert(opt))
    case ValPrimary43(_, _, tup, _) => TupGet(convert(tup), 1)
    case ValPrimary44(_, _, xs, _) => Head(convert(xs))
    case ValPrimary45(_, _, obj, _) => Held(convert(obj))
    case ValPrimary46(_, _, model, _, perm, _, state, _) => ??? // FIXME PB: remove in favour of Future -> Model?
    case ValPrimary47(_, _, loc, _, perm, _) => ModelPerm(convert(loc), convert(perm))
    case ValPrimary48(_, _, thread, _) => IdleToken(convert(thread))
    case ValPrimary49(_, _, xs, _) => Empty(convert(xs))
    case ValPrimary50(_, _, map, _) => MapItemSet(convert(map))
    case ValPrimary51(_, _, map, _) => MapKeySet(convert(map))
    case ValPrimary52(_, _, loc, _) => CurPerm(convert(loc))
    case ValPrimary53(_, _, loc, _, perm, _) => Perm(convert(loc), convert(perm))
    case ValPrimary54(_, _, loc, _, perm, _, value, _) => PointsTo(convert(loc), convert(perm), convert(value))
    case ValPrimary55(_, _, xs, _, i, _) => RemoveAt(convert(xs), convert(i))
    case ValPrimary56(_, _, map, _, key, _) => MapRemove(convert(map), convert(key))
    case ValPrimary57(_, _, thread, _) => JoinToken(convert(thread))
    case ValPrimary58(_, _, x, _) => OptSome(convert(x))
    case ValPrimary59(_, _, xs, _) => Tail(convert(xs))
    case ValPrimary60(_, _, loc, _) => Perm(convert(loc), ReadPerm())
    case ValPrimary61(_, _, map, _) => MapValueSet(convert(map))
    case ValPrimary62(_, _, elementType, _, _, xs, _) => LiteralSeq(convert(elementType), convert(xs))
    case ValPrimary63(_, _, elementType, _, _, xs, _) => LiteralSet(convert(elementType), convert(xs))
    case ValPrimary64(_, xs, _, _, len, _, _) => Take(convert(xs), convert(len))
    case ValPrimary65(_, xs, _, start, _, maybeEnd, _, _) => maybeEnd match {
      case None => Drop(convert(xs), convert(start))
      case Some(end) => Slice(convert(xs), convert(start), convert(end))
    }
    case ValPrimary66(_, xs, _, idx, _, replacement, _, _) =>
      SeqUpdate(convert(xs), convert(idx), convert(replacement))
    case ValPrimary67(_, x, _, xs, _) => Cons(convert(x), convert(xs))
    case ValPrimary68(_, xs, _, ys, _) => Concat(convert(xs), convert(ys))
    case ValPrimary69(_, x, _, xs, _) => AmbiguousMember(convert(x), convert(xs)) // PB: duplicate?
    case ValPrimary70(_, _, opt, _, alt, _) => OptGetOrElse(convert(opt), convert(alt))
  }

  def convert(implicit res: ValReservedContext): Expr = res match {
    case ValReserved0(name) => fail(res,
      f"This identifier is reserved, and cannot be declared or used in specifications. " +
        f"You might want to escape the identifier with backticks: `$name`")
    case ValReserved1(id) => Local(new UnresolvedRef(id.substring(1, id.length-1)))
    case ValReserved2(_) => ???
    case ValReserved3(_) => ???
    case ValReserved4(_) => NoPerm()
    case ValReserved5(_) => WritePerm()
    case ValReserved6(_) => ReadPerm()
    case ValReserved7(_) => OptNone()
    case ValReserved8(_) => EmptyProcess()
    case ValReserved9(_) => ???
    case ValReserved10(_) => ???
    case ValReserved11(_) => true
    case ValReserved12(_) => false
  }
}
