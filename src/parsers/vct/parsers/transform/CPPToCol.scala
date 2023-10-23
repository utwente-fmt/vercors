package vct.parsers.transform

import org.antlr.v4.runtime.{ParserRuleContext, Token}
import vct.antlr4.generated.CPPParser._
import vct.antlr4.generated.CPPParserPatterns._
import vct.col.ast._
import vct.col.ast.`type`.TFloats
import vct.col.origin._
import vct.col.ref.{Ref, UnresolvedRef}
import vct.col.util.AstBuildHelpers
import vct.col.util.AstBuildHelpers._
import vct.col.{ast => col}

import scala.annotation.nowarn
import scala.collection.mutable
import scala.jdk.CollectionConverters._

@nowarn("msg=match may not be exhaustive&msg=Some\\(")
case class CPPToCol[G](override val baseOrigin: Origin,
                       override val blameProvider: BlameProvider,
                       override val errors: Seq[(Token, Token, ExpectedError)])
  extends ToCol(baseOrigin, blameProvider, errors) {

  private var prependNamespace: Boolean = true
  private val currentNamespacePath: mutable.Stack[String] = mutable.Stack()

  private def getPrependedNameDeclarator(declarator: CPPDeclarator[G])(implicit o: Origin): CPPDeclarator[G] = {
    if (prependNamespace) {
      declarator match {
        case decl: CPPTypedFunctionDeclarator[G] => CPPTypedFunctionDeclarator[G](decl.params, decl.varargs, getPrependedNameDeclarator(decl.inner))
        case CPPName(name) => CPPName[G]((currentNamespacePath.reverse :+ name).mkString("::"))(o)
        case _ => declarator
      }
    } else {
      declarator
    }
  }

  def convert(implicit unit: TranslationUnitContext): Seq[GlobalDeclaration[G]] = unit match {
    case TranslationUnit0(maybeDeclSeq, _) => Seq(new CPPTranslationUnit(maybeDeclSeq.toSeq.flatMap(convert(_))))
  }

  def convert(implicit declSeq: DeclarationseqContext): Seq[GlobalDeclaration[G]] = declSeq match {
    case Declarationseq0(decl, _) => convert(decl)
    case Declarationseq1(declSeq, decl, _) => convert(declSeq) ++ convert(decl)
  }

  // Only support block declarations, function declarations, namespace declarations, empty declarations, and VerCors global declarations
  def convert(implicit decl: DeclarationContext): Seq[GlobalDeclaration[G]] = decl match {
    case Declaration0(funcDecl) => Seq(convert(funcDecl))
    case Declaration1(blockDecl) => Seq(new CPPGlobalDeclaration(convert(blockDecl)))
    case Declaration2(templateDecl) => ??(templateDecl)
    case Declaration3(explicitInst) => ??(explicitInst)
    case Declaration4(explicitSpec) => ??(explicitSpec)
    case Declaration5(linkageSpec) => ??(linkageSpec)
    case Declaration6(namespaceDecl) => convert(namespaceDecl)
    case Declaration7(_) => Seq()
    case Declaration8(attrDecl) => ??(attrDecl)
    case Declaration9(globalSpecDecl) => convert(globalSpecDecl)
  }

  // Do not support inline and unnamed namespaces
  def convert(implicit nsDef: NamespaceDefinitionContext): Seq[GlobalDeclaration[G]] = nsDef match {
    case NamespaceDefinition0(None, _, Some(name), _, maybeBody, _) => {
      val nameStr = convert(name)
      currentNamespacePath.push(nameStr)
      val result = maybeBody.toSeq.flatMap(convert(_))
      currentNamespacePath.pop()
      result
    }
    case NamespaceDefinition0(_, _, _, _, _, _) => ??(nsDef)
  }

  // Not supporting attribute specifiers and virtual specifiers
  def convert(implicit funcDef: FunctionDefinitionContext): CPPFunctionDefinition[G] = funcDef match {
    case FunctionDefinition0(maybeContract, None, maybeDeclSpecs, declarator, None, body) =>
      val convertedDeclarator = getPrependedNameDeclarator(convert(declarator))
      prependNamespace = false
      val converted = withContract(maybeContract, contract =>
        new CPPFunctionDefinition(contract.consumeApplicableContract(blame(funcDef)), maybeDeclSpecs.toSeq.flatMap(convert(_)), convertedDeclarator, convert(body))(blame(funcDef))
      )
      prependNamespace = true
      converted
    case FunctionDefinition0(_, _, _, _, _, _) => ??(funcDef)
  }

  // Not supporting try blocks, '= default;', '= delete;', and constructor initializer ': name1, name2'
  def convert(implicit funcBody: FunctionBodyContext): Statement[G] = funcBody match {
    case FunctionBody0(None, compoundStmnt) => convert(compoundStmnt)
    case FunctionBody0(_, _) => ??(funcBody)
    case FunctionBody1(_) => ??(funcBody)
    case FunctionBody2(_, _, _) => ??(funcBody)
  }

  def convert(implicit compoundStmnt: CompoundStatementContext): Statement[G] = compoundStmnt match {
    case CompoundStatement0(_, None, _) => Scope(Nil, Block(Seq()))
    case CompoundStatement0(_, Some(stmntSeq), _) => Scope(Nil, Block(convert(stmntSeq)))
  }

  def convert(implicit stmntSeq: StatementSeqContext): Seq[Statement[G]] = stmntSeq match {
    case StatementSeq0(stmnts) => stmnts.map(convert(_))
  }

  // Do not support labeled statements and attribute specifiers before them
  def convert(implicit stmnt: StatementContext): Statement[G] = stmnt match {
    case Statement0(None, stmnt2) => convert(stmnt2)
    case Statement1(blockDecl) => CPPDeclarationStatement(new CPPLocalDeclaration(convert(blockDecl)))
    case Statement2(labeledStmnt) => ??(labeledStmnt)
  }

  // Do not support try blocks
  def convert(implicit stmnt: StatementTwoContext): Statement[G] = stmnt match {
    case StatementTwo0(exprStmnt) => convert(exprStmnt)
    case StatementTwo1(compoundStmnt) => convert(compoundStmnt)
    case StatementTwo2(selectionStmnt) => convert(selectionStmnt)
    case StatementTwo3(iterStmnt) => convert(iterStmnt)
    case StatementTwo4(jumpStmnt) => convert(jumpStmnt)
    case StatementTwo5(tryBlock) => ??(tryBlock)
    case StatementTwo6(embedStat) => convert(embedStat)
    case StatementTwo7(valStat) => convert(valStat)
  }

  // Do not support asm definitions, namespace alias definitions, using declarations and directives,
  // static assert declarations, alias declarations, and opaque enum declarations
  def convert(implicit blockDecl: BlockDeclarationContext): CPPDeclaration[G] = blockDecl match {
    case BlockDeclaration0(simpleDecl) => convert(simpleDecl)
    case BlockDeclaration1(decl) => ??(decl)
    case BlockDeclaration2(decl) => ??(decl)
    case BlockDeclaration3(decl) => ??(decl)
    case BlockDeclaration4(decl) => ??(decl)
    case BlockDeclaration5(decl) => ??(decl)
    case BlockDeclaration6(decl) => ??(decl)
    case BlockDeclaration7(decl) => ??(decl)
  }

  // Not supporting attribute specifiers
  def convert(implicit simpleDecl: SimpleDeclarationContext): CPPDeclaration[G] = simpleDecl match {
    case SimpleDeclaration0(maybeContract, Some(declSpecs), maybeInits, _) =>
      var convertedInits: Seq[CPPInit[G]] = Nil
      if (maybeInits.isDefined) {
        convert(maybeInits.get).foreach(init => {
          if (currentNamespacePath.nonEmpty && prependNamespace && !init.decl.isInstanceOf[CPPTypedFunctionDeclarator[G]]) {
            // Only allow method declarations inside a namespace
            ??(simpleDecl)
          }
          convertedInits = convertedInits :+ CPPInit(getPrependedNameDeclarator(init.decl), init.init)
        })
      }
      withContract(maybeContract, contract =>
        new CPPDeclaration[G](contract.consumeApplicableContract(blame(simpleDecl)),
          specs = convert(declSpecs), inits = convertedInits)
      )
    case SimpleDeclaration0(_, _, _, _) => ??(simpleDecl)
    case SimpleDeclaration1(_, _, _, _, _) => ??(simpleDecl)
  }

  def convert(implicit exprStmnt: ExpressionStatementContext): Statement[G] = exprStmnt match {
    case ExpressionStatement0(None, _) => Block(Nil)
    case ExpressionStatement0(Some(expr), _) => Eval(convert(expr))
  }

  def convert(implicit expr: ExpressionContext): Expr[G] = expr match {
    case Expression0(inner) => convert(inner)
    case Expression1(_, _, _) => ??(expr)
  }

  // Do not support switch statement
  def convert(implicit selectionStmnt: SelectionStatementContext): Statement[G] = selectionStmnt match {
    case SelectionStatement0(ifStmnt) => convert(ifStmnt)
    case SelectionStatement1(_) => ??(selectionStmnt)
  }

  def convert(implicit ifStmnt: IfStatementContext): Statement[G] = ifStmnt match {
    case IfStatement0(_, _, cond, _, whenTrue, _, whenFalse) => Branch(Seq((convert(cond), convert(whenTrue)), (tt, convert(whenFalse))))
    case IfStatement1(_, _, cond, _, whenTrue) => Branch(Seq((convert(cond), convert(whenTrue))))
  }

  // Do not support do-while loops, 'for(item : list) {}' and expression instead of declaration in a for-loop
  def convert(implicit iterStmnt: IterationStatementContext): Statement[G] = iterStmnt match {
    case IterationStatement0(contract1, _, _, cond, _, contract2, body) => withContract(contract1, contract2, c => {
      Scope(Nil, Loop[G](Block(Nil), convert(cond), Block(Nil), c.consumeLoopContract(iterStmnt), convert(body)))
    })
    case IterationStatement1(_, _, _, _, _, _, _) => ??(iterStmnt)
    case IterationStatement2(contract1, _, _, ForInitStatement1(simpleDecl), cond, _, update, _, contract2, body) =>
      withContract(contract1, contract2, c => {
        Scope(Nil, Loop[G](CPPDeclarationStatement(new CPPLocalDeclaration(convert(simpleDecl))), cond.map(convert(_)).getOrElse(tt), evalOrNop(update), c.consumeLoopContract(iterStmnt), convert(body)))
      })
    case IterationStatement2(_, _, _, _, _, _, _, _, _, _) => ??(iterStmnt)
    case IterationStatement3(_, _, _, _, _, _, _, _, _) => ??(iterStmnt)
  }

  // Do not support goto or return of a bracedInitList
  def convert(implicit jumpStmnt: JumpStatementContext): Statement[G] = jumpStmnt match {
    case JumpStatement0(_, _) => col.Break(None)
    case JumpStatement1(_, _) => col.Continue(None)
    case JumpStatement2(_, expr, _) => col.Return(convert(expr))
    case JumpStatement3(_, _, _) => ??(jumpStmnt)
    case JumpStatement4(_, _) => col.Return(col.Void())
    case JumpStatement5(_, _, _) => ??(jumpStmnt)
  }

  // Do not support assignments in condition
  def convert(implicit cond: ConditionContext): Expr[G] = cond match {
    case Condition0(expr) => convert(expr)
    case _: Condition1Context => ??(cond)
  }

  // Do not support throw expression
  def convert(implicit expr: AssignmentExpressionContext): Expr[G] = expr match {
    case AssignmentExpression0(pre, inner, post) =>
      convertEmbedWith(pre, convertEmbedThen(post, convert(inner)))
    case AssignmentExpression1(pre, targetNode, op, valueNode, post) =>
      val e = convert(op, targetNode, valueNode, expr)
      convertEmbedWith(pre, convertEmbedThen(post, e))
    case AssignmentExpression2(_) => ??(expr)
  }

  // Do not support bit operators
  def convert(op: AssignmentOperatorContext, targetNode: LogicalOrExpressionContext, valueNode: InitializerClauseContext, expr: AssignmentExpressionContext)(implicit o: Origin): Expr[G] = {
    val target = convert(targetNode)
    val value = convert(valueNode)
    PreAssignExpression(target, op match {
      case AssignmentOperator0(_) => value
      case AssignmentOperator1(_) => AmbiguousMult(target, value)
      case AssignmentOperator2(_) => FloorDiv(target, value)(blame(expr))
      case AssignmentOperator3(_) => col.Mod(target, value)(blame(expr))
      case AssignmentOperator4(_) => col.AmbiguousPlus(target, value)(blame(valueNode))
      case AssignmentOperator5(_) => col.AmbiguousMinus(target, value)(blame(valueNode))
      case _ => ??(op)
    })(blame(expr))
  }

  def convert(implicit expr: ConditionalExpressionContext): Expr[G] = expr match {
    case ConditionalExpression0(inner) => convert(inner)
    case ConditionalExpression1(cond, _, whenTrue, _, whenFalse) =>
      Select(convert(cond), convert(whenTrue), convert(whenFalse))
  }

  def convert(implicit expr: ImplicationExpressionContext): Expr[G] = expr match {
    case ImplicationExpression0(inner) => convert(inner)
    case ImplicationExpression1(left, op, right) => convert(op, convert(left), convert(right))
  }

  def convert(op: ImplicationOpContext, left: Expr[G], right: Expr[G])(implicit o: Origin): Expr[G] = op match {
    case ImplicationOp0(ValImpOp0(_)) => Wand(left, right)
    case ImplicationOp0(ValImpOp1(_)) => Implies(left, right)
  }

  def convert(implicit expr: LogicalOrExpressionContext): Expr[G] = expr match {
    case LogicalOrExpression0(inner) => convert(inner)
    case LogicalOrExpression1(left, _, right) => AmbiguousOr(convert(left), convert(right))
  }

  def convert(implicit expr: LogicalAndExpressionContext): Expr[G] = expr match {
    case LogicalAndExpression0(inner) => convert(inner)
    case LogicalAndExpression1(left, op, right) => op match {
      case LogicalAndOp0(_) => col.And(convert(left), convert(right))
      case LogicalAndOp1(valOp) => convert(expr, valOp, convert(left), convert(right))
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
      case "<" => col.AmbiguousLess(convert(left), convert(right))
      case ">" => col.AmbiguousGreater(convert(left), convert(right))
      case "<=" => AmbiguousLessEq(convert(left), convert(right))
      case ">=" => AmbiguousGreaterEq(convert(left), convert(right))
    }
    case RelationalExpression1(left, RelationalOp1(specOp), right) =>
      convert(expr, specOp, convert(left), convert(right))
  }

  def convert(implicit expr: ShiftExpressionContext): Expr[G] = expr match {
    case ShiftExpression0(inner) => convert(inner)
    case ShiftExpression1(left, _, _, right) => BitShl(convert(left), convert(right))
    case ShiftExpression2(left, _, _, right) => BitShr(convert(left), convert(right))
  }

  def convert(implicit expr: AdditiveExpressionContext): Expr[G] = expr match {
    case AdditiveExpression0(inner) => convert(inner)
    case AdditiveExpression1(left, _, right) => AmbiguousPlus(convert(left), convert(right))(blame(expr))
    case AdditiveExpression2(left, _, right) => col.AmbiguousMinus(convert(left), convert(right))(blame(expr))
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

  // Do not support operators .* and ->*
  def convert(implicit expr: PointerMemberExpressionContext): Expr[G] = expr match {
    case PointerMemberExpression0(inner) => convert(inner)
    case PointerMemberExpression1(_, _, _) => ??(expr)
    case PointerMemberExpression2(_, _, _) => ??(expr)
  }

  def convert(implicit expr: PrependExpressionContext): Expr[G] = expr match {
    case PrependExpression0(inner) => convert(inner)
    case PrependExpression1(left, PrependOp0(specOp), right) => convert(expr, specOp, convert(left), convert(right))
  }

  // Do not support cast expressions
  def convert(expr: CastExpressionContext): Expr[G] = expr match {
    case CastExpression0(inner) => convert(inner)
    case CastExpression1(_, _, _, expr) => ??(expr)
  }

  // Do not support bracedInitList
  def convert(implicit initClause: InitializerClauseContext): Expr[G] = initClause match {
    case InitializerClause0(expr) => ??(expr)
    case InitializerClause1(expr) => convert(expr)
  }

  // Do not support '...'
  def convert(implicit expr: InitializerListContext): Seq[Expr[G]] = expr match {
    case InitializerList0(initClause, None) => Seq(convert(initClause))
    case InitializerList0(_, _) => ??(expr)
    case InitializerList1(initList, _, initClause, None) => convert(initList) :+ convert(initClause)
    case InitializerList1(_, _, _, _) => ??(expr)
  }

  // Do not support sizeof, alignof, noexcept, and delete expressions
  def convert(implicit expr: UnaryExpressionContext): Expr[G] = expr match {
    case UnaryExpression0(inner) => convert(inner)
    case UnaryExpression1(_, arg) =>
      val target = convert(arg)
      PreAssignExpression(target, col.AmbiguousPlus(target, const(1))(blame(expr)))(blame(expr))
    case UnaryExpression2(_, arg) =>
      val target = convert(arg)
      PreAssignExpression(target, col.AmbiguousMinus(target, const(1))(blame(expr)))(blame(expr))
    case UnaryExpression3(UnaryOperator0(_), arg) => AddrOf(convert(arg))
    case UnaryExpression3(UnaryOperator1(_), arg) => DerefPointer(convert(arg))(blame(expr))
    case UnaryExpression3(UnaryOperator2(_), arg) => convert(arg)
    case UnaryExpression3(UnaryOperator3(_), arg) => UMinus(convert(arg))
    case UnaryExpression3(UnaryOperator5(_), arg) => col.Not(convert(arg))
    case UnaryExpression3(UnaryOperator6(_), arg) => col.Not(convert(arg))
    case UnaryExpression4(_, _) => ??(expr)
    case _: UnaryExpression5Context => ??(expr)
    case UnaryExpression6(_, _, _, _) => ??(expr)
    case UnaryExpression7(_) => ??(expr)
    case UnaryExpression8(_) => ??(expr)
    case UnaryExpression9(_) => ??(expr)
    case UnaryExpression10(SpecPrefix0(op), inner) => convert(expr, op, convert(inner))
  }

  def convert(implicit decls: InitDeclaratorListContext): Seq[CPPInit[G]] = decls match {
    case InitDeclaratorList0(decl) => Seq(convert(decl))
    case InitDeclaratorList1(init, _, last) => convert(init) :+ convert(last)
  }

  def convert(implicit decl: InitDeclaratorContext): CPPInit[G] = decl match {
    case InitDeclarator0(inner, None) => CPPInit(convert(inner), None)
    case InitDeclarator0(inner, Some(init)) => CPPInit(convert(inner), Some(convert(init)))
  }

  def convert(implicit expr: InitializerContext): Expr[G] = expr match {
    case Initializer0(expr) => convert(expr)
    case Initializer1(_, _, _) => ??(expr)
  }

  // Do not support bracedInitList
  def convert(implicit expr: BraceOrEqualInitializerContext): Expr[G] = expr match {
    case BraceOrEqualInitializer0(_, expr) => convert(expr)
    case BraceOrEqualInitializer1(expr) => ??(expr)
  }

  // Do not support templactes, typeNameSpecifiers, bracedInitLists,
  def convert(implicit expr: PostfixExpressionContext): Expr[G] = expr match {
    case PostfixExpression0(inner) => convert(inner)
    case PostfixExpression1(arr, _, idx, _) => AmbiguousSubscript(convert(arr), convert(idx))(blame(expr))
    case PostfixExpression2(_, _, _, _) => ??(expr)
    case PostfixExpression3(target, _, args, _, given, yields) =>
      CPPInvocation(convert(target), args.map(convert(_)) getOrElse Nil,
        convertEmbedGiven(given), convertEmbedYields(yields))(blame(expr))
    case PostfixExpression4(classVar, _, None, idExpr) =>
      convert(classVar) match {
        case CPPLocal(className, None) => convert(idExpr) match {
          case CPPTypedefName(name, None) => CPPClassInstanceLocal(className, name)(blame(expr))
          case _ => ??(expr)
        }
        case _ => ??(expr)
      }
    case PostfixExpression4(_, _, _, _) => ??(expr)
    case PostfixExpression5(_, _, _) => ??(expr)
    case PostfixExpression6(_, _, _, _) => ??(expr)
    case PostfixExpression7(_, _, _) => ??(expr)
    case PostfixExpression8(targetNode, _) =>
      val target = convert(targetNode)
      PostAssignExpression(target, col.AmbiguousPlus(target, const(1))(blame(expr)))(blame(expr))
    case PostfixExpression9(targetNode, _) =>
      val target = convert(targetNode)
      PostAssignExpression(target, col.AmbiguousMinus(target, const(1))(blame(expr)))(blame(expr))
    case PostfixExpression10(e, SpecPostfix0(postfix)) => convert(expr, postfix, convert(e))
    case PostfixExpression11(_, _, _, _, _, _) => ??(expr)
    case PostfixExpression12(_, _) => ??(expr)
    case PostfixExpression13(_, _, _, _, _, _) => ??(expr)
    case PostfixExpression14(_, _) => ??(expr)
    case PostfixExpression15(_, _, _, _, _, _, _) => ??(expr)
    case _: PostfixExpression16Context => ??(expr)
  }

  def convert(implicit exprList: ExpressionListContext): Seq[Expr[G]] = exprList match {
    case ExpressionList0(initList) => convert(initList)
  }

  def convert(implicit expr: AnnotatedPrimaryExpressionContext): Expr[G] = expr match {
    case AnnotatedPrimaryExpression0(pre, inner, post) =>
      convertEmbedWith(pre, convertEmbedThen(post, convert(inner)))
  }

  // Dot not more than 1 literal
  def convert(implicit expr: PrimaryExpressionContext): Expr[G] = expr match {
    case PrimaryExpression0(inner) => convert(inner)
    case PrimaryExpression1(literal) => convert(literal)
    case PrimaryExpression2(_) => AmbiguousThis()
    case PrimaryExpression3(_, inner, _) => convert(inner)
    case PrimaryExpression4(inner) => convert(inner) match {
      case CPPTypedefName(name, arg) => CPPLocal(name, arg)(blame(expr))(origin(expr))
      case SYCLClassDefName(name, arg) => CPPLocal("sycl::" + name, arg)(blame(expr))(origin(expr))
      case _ => ??(expr)
    }
    case PrimaryExpression5(lambda) => convert(lambda)
  }

  // Do not support extended chars and strings and user-defined literals
  def convert(implicit literal: LiteralContext): Expr[G] = literal match {
    case Literal0(intLit) => parseInt(intLit).getOrElse(??(literal))
    case Literal1(charLit) => parseChar(charLit).getOrElse(??(literal))
    case Literal2(floatLit) => parseFloat(floatLit).getOrElse(??(literal))
    case Literal3(strLit) => parseString(strLit).getOrElse(??(literal))
    case Literal4(value) => BooleanValue(value == "true")
    case Literal5(_) => Null()
    case Literal6(_) => ??(literal)
  }

  def convert(implicit idExpr: IdExpressionContext): CPPTypeSpecifier[G] = idExpr match {
    case IdExpression0(id) => convert(id)
    case IdExpression1(id) => convert(id)
  }

  def parseFloat(numFlag: String)(implicit o: Origin): Option[Expr[G]] = {
    try {
      Some(numFlag.last match {
        case 'f' | 'F' => FloatValue(BigDecimal(numFlag.init), TFloats.ieee754_32bit)
        case 'l' | 'L' => FloatValue(BigDecimal(numFlag.init), TFloats.ieee754_64bit)
        case _ => FloatValue(BigDecimal(numFlag), TFloats.ieee754_32bit)
      })
    } catch {
      case _: NumberFormatException => None
    }
  }

  def parseInt(i: String)(implicit o: Origin): Option[Expr[G]] =
    try {
      Some(IntegerValue(BigInt(i)))
    } catch {
      case _: NumberFormatException => None
    }

  private def parseChar(value: String)(implicit o: Origin): Option[Expr[G]] = {
    val fixedValue = fixEscapeAndUnicodeChars(value)
    val pattern = "^'(.|\n|\r)'$".r
    fixedValue match {
      case pattern(char, _*) => Some(CharValue(char.codePointAt(0)))
      case _ => None
    }
  }

  private def parseString(value: String)(implicit o: Origin): Option[Expr[G]] = {
    val fixedValue = fixEscapeAndUnicodeChars(value)
    val pattern = "^\"((.|\n|\r)*)\"$".r
    fixedValue match {
      case pattern(str, _*) => Some(StringValue(str))
      case _ => None
    }
  }

  // ANTLR separates escape sequences such into separate characters, so '\n' will be '\' and 'n'
  // This function combines them back into 1 character
  private def fixEscapeAndUnicodeChars(str: String): String = {
    var result = str

    val unicodePattern = "\\\\u([0-9a-fA-F]{4})".r
    var patMatchOpt = unicodePattern.findFirstMatchIn(result)
    while (patMatchOpt.isDefined) {
      val patMatch = patMatchOpt.get
      result = result.substring(0, patMatch.start) + new String(patMatch.group(1).sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte), "UNICODE") + result.substring(patMatch.end)
      patMatchOpt = unicodePattern.findFirstMatchIn(result)
    }

    val escapePattern = "\\\\(['\"\\\\nrt])".r
    val escapeMap: Map[String, String] = Map("'" -> "'", "\"" -> "\"", "\\" -> "\\", "n" -> "\n", "r" -> "\r", "t" -> "\t")
    patMatchOpt = escapePattern.findFirstMatchIn(result)
    while (patMatchOpt.isDefined) {
      val patMatch = patMatchOpt.get
      result = result.substring(0, patMatch.start) + escapeMap(patMatch.group(1)) + result.substring(patMatch.end)
      patMatchOpt = unicodePattern.findFirstMatchIn(result)
    }

    result
  }

  // Not supporting attribute specifiers
  def convert(implicit declSpecSeq: DeclSpecifierSeqContext): Seq[CPPDeclarationSpecifier[G]] = declSpecSeq match {
    case DeclSpecifierSeq0(declSpec, None) => declSpec.flatMap(convert(_))
    case DeclSpecifierSeq0(_, _) => ??(declSpecSeq)
  }

  // Do not support storage specifiers, function specifiers, friends, typedefs, consts.
  def convert(implicit declSpec: DeclSpecifierContext): Seq[CPPDeclarationSpecifier[G]] = declSpec match {
    case DeclSpecifier0(_) => ??(declSpec)
    case DeclSpecifier1(typeSpec) => convert(typeSpec)
    case DeclSpecifier2(_) => ??(declSpec)
    case DeclSpecifier3(_) => ??(declSpec)
    case DeclSpecifier4(_) => ??(declSpec)
    case DeclSpecifier5(_) => ??(declSpec)
    case DeclSpecifier6(valEmbedModifier) => withModifiers(valEmbedModifier, m => {
      if (m.consume(m.pure))
        Seq(new CPPPure[G]())
      else if (m.consume(m.inline))
        Seq(new CPPInline[G]())
      else
        fail(m.nodes.head, "This modifier cannot be attached to a declaration in C++")
    })
  }

  // Do not support enum or class declarations, such as 'class className { members }' as type specifiers
  def convert(implicit typeSpec: TypeSpecifierContext): Seq[CPPTypeSpecifier[G]] = typeSpec match {
    case TypeSpecifier0(trailingTypeSpec) => convert(trailingTypeSpec)
    case TypeSpecifier1(_) => ??(typeSpec)
    case TypeSpecifier2(_) => ??(typeSpec)
  }

  // Do not support elaboratedTypeSpec, typeNameSpec, and cvQualifier
  def convert(implicit typeSpec: TrailingTypeSpecifierContext): Seq[CPPTypeSpecifier[G]] = typeSpec match {
    case TrailingTypeSpecifier0(simpleTypeSpec) => convert(simpleTypeSpec)
    case TrailingTypeSpecifier1(_) => ??(typeSpec)
    case TrailingTypeSpecifier2(_) => ??(typeSpec)
    case TrailingTypeSpecifier3(_) => ??(typeSpec)
  }

  // Do not support types: template, char16_t, char32_t, wchar_t, auto, decltype(), and combination of namespace and typename
  def convert(implicit typeSpec: SimpleTypeSpecifierContext): Seq[CPPTypeSpecifier[G]] = typeSpec match {
    case SimpleTypeSpecifier0(None, typeName) => Seq(convert(typeName))
    case SimpleTypeSpecifier0(Some(nestedNameSpec), typeName) =>
      convert(typeName) match {
        case CPPTypedefName(name, genericArg) => convert(nestedNameSpec).nestedName match {
          case s"sycl::$rest" => Seq(SYCLClassDefName(rest + name, genericArg))
          case other => Seq(CPPTypedefName(other + name, genericArg))
        }
        case _ => ??(typeSpec)
      }
    case SimpleTypeSpecifier1(_, _, _) => ??(typeSpec)
    case SimpleTypeSpecifier2(signedness) => Seq(convert(signedness))
    case SimpleTypeSpecifier3(Some(signedness), typeLengthMods) => Seq(convert(signedness)) ++ typeLengthMods.map(convert(_))
    case SimpleTypeSpecifier3(None, typeLengthMods) => typeLengthMods.map(convert(_))
    case SimpleTypeSpecifier4(Some(signedness), _) => Seq(convert(signedness), new CPPChar[G]())
    case SimpleTypeSpecifier4(None, _) => Seq(new CPPChar[G]())
    case SimpleTypeSpecifier5(_, _) => ??(typeSpec)
    case SimpleTypeSpecifier6(_, _) => ??(typeSpec)
    case SimpleTypeSpecifier7(_, _) => ??(typeSpec)
    case SimpleTypeSpecifier8(_) => Seq(new CPPBool[G]())
    case SimpleTypeSpecifier9(Some(signedness), typeLengthMods, _) => Seq(convert(signedness)) ++ typeLengthMods.map(convert(_)) :+ new CPPInt[G]()
    case SimpleTypeSpecifier9(None, typeLengthMods, _) => typeLengthMods.map(convert(_)) :+ new CPPInt[G]()
    case SimpleTypeSpecifier10(_) => Seq(CPPSpecificationType(TFloats.ieee754_32bit))
    case SimpleTypeSpecifier11(Some(typeLengthMod), _) => Seq(convert(typeLengthMod), CPPSpecificationType(TFloats.ieee754_64bit))
    case SimpleTypeSpecifier11(None, _) => Seq(CPPSpecificationType(TFloats.ieee754_64bit))
    case SimpleTypeSpecifier12(_) => Seq(new CPPVoid[G]())
    case SimpleTypeSpecifier13(_) => ??(typeSpec)
    case SimpleTypeSpecifier14(valType) => Seq(CPPSpecificationType(convert(valType)))
    case SimpleTypeSpecifier15(_) => ??(typeSpec)
  }

  def convert(implicit signedness: SimpleTypeSignednessModifierContext): CPPTypeSpecifier[G] = signedness match {
    case SimpleTypeSignednessModifier0(_) => new CPPUnsigned[G]()
    case SimpleTypeSignednessModifier1(_) => new CPPSigned[G]()
  }

  def convert(implicit simpleTypeLengthMod: SimpleTypeLengthModifierContext): CPPTypeSpecifier[G] = simpleTypeLengthMod match {
    case SimpleTypeLengthModifier0(_) => new CPPShort[G]()
    case SimpleTypeLengthModifier1(_) => new CPPLong[G]()
  }

  // Do not support template or decltypes, or a typename as identifier in the nestedname
  def convert(implicit nestedNameSpec: NestedNameSpecifierContext): CPPTypedefName[G] = nestedNameSpec match {
    case NestedNameSpecifier0(theTypeName, sep) =>
      convert(theTypeName) match {
        case name@CPPTypedefName(_, None) => name.appendName(sep)
        case _ => ??(theTypeName)
      }
    case NestedNameSpecifier1(namespaceName, sep) => convert(namespaceName).appendName(sep)
    case NestedNameSpecifier2(_, _) => ??(nestedNameSpec)
    case NestedNameSpecifier3(sep) => CPPTypedefName(sep, None)
    case NestedNameSpecifier4(inner, id, sep) =>
      convert(inner) match {
        case name@CPPTypedefName(_, None) => name.appendName(convert(id)).appendName(sep)
        case _ => ??(inner)
      }
    case NestedNameSpecifier5(_, _, _, _) => ??(nestedNameSpec)
  }

  def convert(implicit namespaceName: NamespaceNameContext): CPPTypedefName[G] = namespaceName match {
    case NamespaceName0(OriginalNamespaceName0(id)) => CPPTypedefName(convert(id), None)
    case NamespaceName1(NamespaceAlias0(id)) => CPPTypedefName(convert(id), None)
  }

  // Do not support enum-names and typedef-names
  def convert(implicit theTypeName: TheTypeNameContext): CPPTypeSpecifier[G] = theTypeName match {
    case TheTypeName0(simpleTemplateId) => convert(simpleTemplateId)
    case TheTypeName1(className) => convert(className)
    case TheTypeName2(_) => ??(theTypeName)
    case TheTypeName3(_) => ??(theTypeName)
  }

  // Do not support template-names
  def convert(implicit className: ClassNameContext): CPPTypedefName[G] = className match {
    case ClassName0(name) => CPPTypedefName(convert(name), None)
    case ClassName1(_) => ??(className)
  }

  def convert(implicit templateId: TemplateIdContext): CPPTypeSpecifier[G] = templateId match {
    case TemplateId0(simpleTemplateId) => convert(simpleTemplateId)
    case _: TemplateId1Context => ??(templateId)
  }

  // Do not support argument-list with more than one argument
  def convert(implicit simpleTemplateId: SimpleTemplateIdContext): CPPTypeSpecifier[G] = simpleTemplateId match {
    case SimpleTemplateId0(name, _, arg, _) => CPPTypedefName(convert(name), Some(convert(arg)))
    case SimpleTemplateId1(_, _, _, _) => ??(simpleTemplateId)
  }

  // Only support constantExpression
  def convert(implicit templateArgument: TemplateArgumentContext): Int = templateArgument match {
    case TemplateArgument0(_) => ??(templateArgument)
    case TemplateArgument1(constantExpr) => convert(constantExpr) match {
      case IntegerValue(value) => value.intValue
      case _ => ??(constantExpr)
    }
    case TemplateArgument2(_) => ??(templateArgument)
  }

  def convert(implicit templateName: TemplateNameContext): String = templateName match {
    case TemplateName0(identifier) => convert(identifier)
  }

  // Do not support trailing return types
  def convert(implicit declarator: DeclaratorContext): CPPDeclarator[G] = declarator match {
    case Declarator0(pointerDeclarator) => convert(pointerDeclarator)
    case Declarator1(_, _, _) => ??(declarator)
  }

  def convert(implicit pointerDeclarator: PointerDeclaratorContext): CPPDeclarator[G] = pointerDeclarator match {
    case PointerDeclarator0(pointerDeclaratorPrefix, noPointerDeclarator)
      if pointerDeclaratorPrefix.isEmpty => convert(noPointerDeclarator)
    case PointerDeclarator0(pointerDeclaratorPrefix, noPointerDeclarator)
      if pointerDeclaratorPrefix.nonEmpty =>
        val pointers = pointerDeclaratorPrefix.flatMap(convert(_))
        CPPAddressingDeclarator(pointers, convert(noPointerDeclarator))
  }

  // Do not support postfix 'const'
  def convert(implicit pointer: PointerDeclaratorPrefixContext): Seq[CPPAddressing[G]] = pointer match {
    case PointerDeclaratorPrefix0(pointerOp, None) => convert(pointerOp)
    case PointerDeclaratorPrefix0(_, _) => ??(pointer)
  }

  // Do not support nestedNameSpecifier, attributeSpeciefierSeq, and cvQualifierSeq
  def convert(implicit pointerOp: PointerOperatorWithDoubleStarContext): Seq[CPPAddressing[G]] = pointerOp match {
    case PointerOperatorWithDoubleStar0(pointerOp) => convert(pointerOp)
    case PointerOperatorWithDoubleStar1(None, _, None, None) => Seq(CPPPointer(), CPPPointer())
    case PointerOperatorWithDoubleStar1(_, _, _, _) => ??(pointerOp)
  }

  // Do not support '&' and '&&' and nestedNameSpecifier, attributeSpeciefierSeq, and cvQualifierSeq
  def convert(implicit pointerOp: PointerOperatorContext): Seq[CPPAddressing[G]] = pointerOp match {
    case PointerOperator0(_, None) => Seq(CPPReference())
    case PointerOperator0(_, _) => ??(pointerOp)
    case PointerOperator1(_, _) => ??(pointerOp)
    case PointerOperator2(None, _, None, None) => Seq(CPPPointer())
    case PointerOperator2(_, _, _, _) => ??(pointerOp)
  }

  // Do not support attributeSpeciefierSeq
  def convert(noPointerDeclarator: NoPointerDeclaratorContext)(implicit o: Origin): CPPDeclarator[G] = noPointerDeclarator match {
    case NoPointerDeclarator0(declaratorId, None) => convert(declaratorId)
    case NoPointerDeclarator0(_, _) => ??(noPointerDeclarator)
    case NoPointerDeclarator1(innerDeclarator, paramsAndQuals) =>
      val (params, varargs) = convert(paramsAndQuals)
      CPPTypedFunctionDeclarator[G](params, varargs, convert(innerDeclarator))
    case NoPointerDeclarator2(innerDeclarator, _, None, _, None) =>
      convert(innerDeclarator) match {
        case CPPArrayDeclarator(_, _) => ??(noPointerDeclarator) // Do not support > 1 dimensions
        case inner => CPPArrayDeclarator(inner, None)(blame(noPointerDeclarator))
      }
    case NoPointerDeclarator2(innerDeclarator, _, Some(constExpr), _, None) =>
      convert(innerDeclarator) match {
        case CPPArrayDeclarator(_, _) => ??(noPointerDeclarator) // Do not support > 1 dimensions
        case inner => CPPArrayDeclarator(inner, Some(convert(constExpr)))(blame(noPointerDeclarator))
      }
    case NoPointerDeclarator2(_, _, _, _, _) => ??(noPointerDeclarator)
    case NoPointerDeclarator3(_, _, _) => ??(noPointerDeclarator)
  }

  def convert(implicit constExpr: ConstantExpressionContext): Expr[G] = constExpr match {
    case ConstantExpression0(condExpr) => convert(condExpr)
  }

  // Do not support cvQualifier, refqualifier, exceptionSpec and attributeSpec
  def convert(implicit paramsAndQuals: ParametersAndQualifiersContext): (Seq[CPPParam[G]], Boolean) = paramsAndQuals match {
    case ParametersAndQualifiers0(_, None, _, None, None, None, None) => (Seq(), false)
    case ParametersAndQualifiers0(_, Some(paramDeclClause), _, None, None, None, None) => convert(paramDeclClause)
    case ParametersAndQualifiers0(_, _, _, _, _, _, _) => ??(paramsAndQuals)
  }

  def convert(implicit paramDeclClause: ParameterDeclarationClauseContext): (Seq[CPPParam[G]], Boolean) = paramDeclClause match {
    case ParameterDeclarationClause0(paramDeclList, None) => (convert(paramDeclList), false)
    case ParameterDeclarationClause0(paramDeclList, Some(_)) => (convert(paramDeclList), true)
  }

  def convert(implicit paramDeclList: ParameterDeclarationListContext): Seq[CPPParam[G]] = paramDeclList match {
    case value: ParameterDeclarationList0Context => value.parameterDeclaration.asScala.toSeq.map(convert(_))
  }

  // only support params in form of 'declSpecifiers declarator'
  def convert(implicit paramDecl: ParameterDeclarationContext): CPPParam[G] = paramDecl match {
    case ParameterDeclaration0(declSpecs, declarator) => new CPPParam[G](convert(declSpecs), convert(declarator))
    case _: ParameterDeclaration1Context => ??(paramDecl)
  }

  // Do not support if spread operator '...' is used
  def convert(implicit declaratorId: DeclaratoridContext): CPPDeclarator[G] = declaratorId match {
    case Declaratorid0(None, idExpr) => convert(idExpr) match {
      case CPPTypedefName(name, None) => CPPName(name)
      case _ => ??(declaratorId)
    }
    case Declaratorid0(_, _) => ??(declaratorId)
  }

  // Do not support operatorFunctionId, conversionFunctionId, literalOperatorId, and things starting with a tilde
  def convert(implicit unqualifiedId: UnqualifiedIdContext): CPPTypeSpecifier[G] = unqualifiedId match {
    case UnqualifiedId0(templateId) => convert(templateId)
    case UnqualifiedId1(clangppId) => CPPTypedefName(convert(clangppId), None)
    case UnqualifiedId2(_) => ??(unqualifiedId)
    case UnqualifiedId3(_) => ??(unqualifiedId)
    case UnqualifiedId4(_) => ??(unqualifiedId)
  }

  // Do not support template
  def convert(implicit qualifiedId: QualifiedIdContext): CPPTypeSpecifier[G] = qualifiedId match {
    case QualifiedId0(nestedNameSpec, None, id) => convert(id) match {
      case CPPTypedefName(name, genericArg) => convert(nestedNameSpec).nestedName match {
        case s"sycl::$rest" => SYCLClassDefName(rest + name, genericArg)
        case other => CPPTypedefName(other + name, genericArg)
      }
      case _ => ??(qualifiedId)
    }
    case QualifiedId0(_, _, _) => ??(qualifiedId)
  }

  def convert(implicit id: ClangppIdentifierContext): String = id match {
    case ClangppIdentifier0(text) => text
    case ClangppIdentifier1(inner) => convert(inner)
  }

  // Do not support lambdas without a declarator and ignore captures like [=] and [&]
  def convert(implicit lambda: LambdaExpressionContext): CPPLambdaDefinition[G] = lambda match {
    case LambdaExpression0(maybeContract, _, Some(lambdaDecl), compoundStmnt) =>
      withContract(maybeContract, contract =>
        CPPLambdaDefinition(contract.consumeApplicableContract(blame(lambda)), convert(lambdaDecl), convert(compoundStmnt))(blame(lambda))
      )
    case LambdaExpression0(_, _, _, _) => ??(lambda)
  }

  // Do not support Mutable, exceptionSpecification, attributeSpecifierSeq, and trailingReturnType
  // Also do not support declarator with '...'
  def convert(implicit decl: LambdaDeclaratorContext): CPPDeclarator[G] = decl match {
    case LambdaDeclarator0(_, Some(parameterClause), _, None, None, None, None) => convert(parameterClause) match {
      case (x, false) => CPPLambdaDeclarator(x)
      case _ => ??(decl)
    }
    case LambdaDeclarator0(_, None, _, None, None, None, None) => CPPLambdaDeclarator(Seq())
    case LambdaDeclarator0(_, _, _, _, _, _, _) => ??(decl)
  }

  def convert(expr: LangExprContext): Expr[G] = expr match {
    case LangExpr0(expr) => convert(expr)
  }

  def convert(stat: LangStatementContext): Statement[G] = stat match {
    case LangStatement0(stat) => convert(stat)
  }

  def convert(implicit t: LangTypeContext): Type[G] = t match {
    case LangType0(typeSpec) => CPPPrimitiveType(convert(typeSpec))
  }

  def convert(id: LangIdContext): String = id match {
    case LangId0(id) => convert(id)
  }

  def convert(implicit n: LangConstIntContext): BigInt = n match {
    case LangConstInt0(Literal0(string)) => BigInt(string)
    case LangConstInt0(Literal1(string)) => BigInt(string)
    case LangConstInt0(Literal2(string)) => BigInt(string)
    case x => ??(x)
  }

  def local(ctx: ParserRuleContext, name: String): Expr[G] =
    CPPLocal(name, None)(blame(ctx))(origin(ctx))

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

  def convert(implicit contract: ValContractClauseContext, collector: ContractCollector[G]): Unit = contract match {
    case ValContractClause0(_, ids, _) => collector.modifies ++= convert(ids).map((contract, _))
    case ValContractClause1(_, ids, _) => collector.accessible ++= convert(ids).map((contract, _))
    case ValContractClause2(_, exp, _) => collector.requires += ((contract, convert(exp)))
    case ValContractClause3(_, exp, _) => collector.ensures += ((contract, convert(exp)))
    case ValContractClause4(_, t, id, _) =>
      val variable = new Variable(convert(t))(origin(contract).replacePrefName((convert(id))))
      collector.given += ((contract, variable))
    case ValContractClause5(_, t, id, _) =>
      val variable = new Variable(convert(t))(origin(contract).replacePrefName((convert(id))))
      collector.yields += ((contract, variable))
    case ValContractClause6(_, exp, _) => collector.context_everywhere += ((contract, convert(exp)))
    case ValContractClause7(_, exp, _) =>
      collector.requires += ((contract, convert(exp)))
      collector.ensures += ((contract, convert(exp)))
    case ValContractClause8(_, exp, _) => collector.loop_invariant += ((contract, convert(exp)))
    case ValContractClause9(_, exp, _) => collector.kernel_invariant += ((contract, convert(exp)))
    case ValContractClause10(_, _, t, id, _, exp, _) =>
      val variable = new Variable(convert(t))(origin(contract).replacePrefName((convert(id))))
      collector.signals += ((contract, SignalsClause(variable, convert(exp))(OriginProvider(contract))))
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
    case Some(whiff@ValWith0(_, stat)) => With(convert(stat), inner)(origin(whiff))
  }

  def convertEmbedThen(implicit den: Option[ValEmbedThenContext], inner: Expr[G]): Expr[G] = den match {
    case None => inner
    case Some(ValEmbedThen0(_, den, _)) => convertThen(den, inner)
    case Some(ValEmbedThen1(den)) => convertThen(Some(den), inner)
  }

  def convertThen(implicit den: Option[ValThenContext], inner: Expr[G]): Expr[G] = den match {
    case None => inner
    case Some(den@ValThen0(_, stat)) => Then(inner, convert(stat))(origin(den))
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

  def convert(implicit block: ValEmbedStatementBlockContext): Statement[G] = block match {
    case ValEmbedStatementBlock0(_, stats, _) => Block(stats.map(convert(_)))
    case ValEmbedStatementBlock1(stats) => Block(stats.map(convert(_)))
    case ValEmbedStatementBlock2(_, _, _, stat) => Extract(convert(stat))
    case ValEmbedStatementBlock3(_, _, clauses, _, _, body, _, _, _) =>
      withContract(clauses, contract => {
        FramedProof(
          AstBuildHelpers.foldStar(contract.consume(contract.requires)),
          Block(body.map(convert(_))),
          AstBuildHelpers.foldStar(contract.consume(contract.ensures)),
        )(blame(block))
      })
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
      Label(new LabelDecl()(origin(stat).replacePrefName(convert(label))), Block(Nil))
    case ValRefute(_, assn, _) => Refute(convert(assn))(blame(stat))
    case ValWitness(_, _, _) => ??(stat)
    case ValGhost(_, stat) => convert(stat)
    case ValSend(_, name, _, delta, _, resource, _) =>
      Send(new SendDecl()(origin(stat).replacePrefName(convert(name))), convert(delta), convert(resource))(blame(stat))
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
    case ValExtract(_, body) =>
      Extract(convert(body))
    case ValFrame(_, clauses, body) =>
      withContract(clauses, contract => {
        FramedProof(
          AstBuildHelpers.foldStar(contract.consume(contract.requires)),
          convert(body),
          AstBuildHelpers.foldStar(contract.consume(contract.ensures)),
        )(blame(stat))
      })
  }

  def convert(implicit block: ValBlockContext): Seq[Statement[G]] = block match {
    case ValBlock0(_, stats, _) => stats.map(convert(_))
  }

  def convert(implicit arg: ValArgContext): Variable[G] = arg match {
    case ValArg0(t, id) => new Variable(convert(t))(origin(arg).replacePrefName(convert(id)))
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
      Seq(new SimplificationRule(convert(axiom))(origin(decl).replacePrefName(convert(name))))
    case ValPredicate(modifiers, _, name, _, args, _, definition) =>
      withModifiers(modifiers, mods =>
        Seq(new Predicate(args.map(convert(_)).getOrElse(Nil), convert(definition),
          mods.consume(mods.threadLocal), mods.consume(mods.inline))
          (origin(decl).replacePrefName((currentNamespacePath.reverse :+ convert(name)).mkString("::")))))
    case ValFunction(contract, modifiers, _, t, name, typeArgs, _, args, _, definition) =>
      Seq(withContract(contract, c =>
        withModifiers(modifiers, m => {
          val namedOrigin = origin(decl).replacePrefName((currentNamespacePath.reverse :+ convert(name)).mkString("::"))
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
      Seq(new Model(decls.flatMap(convert(_)))(origin(decl).replacePrefName(convert(name))))
    case ValGhostDecl(_, inner) =>
      convert(inner)
    case ValAdtDecl(_, name, typeArgs, _, decls, _) =>
      Seq(new AxiomaticDataType(decls.map(convert(_)), typeArgs.map(convert(_)).getOrElse(Nil))(
        origin(decl).replacePrefName((currentNamespacePath.reverse :+ convert(name)).mkString("::"))
      ))
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
          origin(decl).replacePrefName(convert(name))))
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
            origin(decl).replacePrefName(convert(name))))
        })
      }))
    case ValInstanceGhostDecl(_, decl) => convert(decl).map(transform)
  }

  def convert(implicit decl: ValModelDeclarationContext): Seq[ModelDeclaration[G]] = decl match {
    case ValModelField(t, name, _) =>
      convert(name).map(name => {
        new ModelField(convert(t))(origin(decl).replacePrefName(name))
      })
    case ValModelProcess(contract, _, name, _, args, _, _, definition, _) =>
      Seq(withContract(contract, c => {
        new ModelProcess(args.map(convert(_)).getOrElse(Nil), convert(definition),
          AstBuildHelpers.foldAnd(c.consume(c.requires)), AstBuildHelpers.foldAnd(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef[G, ModelField[G]](_)), c.consume(c.accessible).map(new UnresolvedRef[G, ModelField[G]](_)))(
          blame(decl))(origin(decl).replacePrefName(convert(name)))
      }))
    case ValModelAction(contract, _, name, _, args, _, _) =>
      Seq(withContract(contract, c => {
        new ModelAction(args.map(convert(_)).getOrElse(Nil),
          AstBuildHelpers.foldAnd(c.consume(c.requires)), AstBuildHelpers.foldAnd(c.consume(c.ensures)),
          c.consume(c.modifies).map(new UnresolvedRef[G, ModelField[G]](_)), c.consume(c.accessible).map(new UnresolvedRef[G, ModelField[G]](_)))(
          origin(decl).replacePrefName(convert(name)))
      }))
  }

  def convert(implicit ts: ValTypeVarsContext): Seq[Variable[G]] = ts match {
    case ValTypeVars0(_, names, _) =>
      convert(names).map(name => new Variable(TType(TAnyValue()))(origin(ts).replacePrefName(name)))
  }

  def convert(implicit decl: ValAdtDeclarationContext): ADTDeclaration[G] = decl match {
    case ValAdtAxiom(_, ax, _) => new ADTAxiom(convert(ax))
    case ValAdtFunction(_, returnType, name, _, args, _, _) =>
      new ADTFunction(args.map(convert(_)).getOrElse(Nil), convert(returnType))(
        origin(decl).replacePrefName(convert(name)))
  }

  def convert(implicit definition: ValPureDefContext): Option[Expr[G]] = definition match {
    case ValPureAbstractBody(_) => None
    case ValPureBody(_, expr, _) =>
      prependNamespace = false
      val result = Some(convert(expr))
      prependNamespace = true
      result
  }

  def convert(implicit definition: ValImpureDefContext): Option[Statement[G]] = definition match {
    case ValImpureAbstractBody(_) => None
    case ValImpureBody(statement) => Some(convert(statement))
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
      case "any" => TAnyValue()
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
    case ValPointerLength(_, _, ptr, _) => PointerLength(convert(ptr))(blame(e))
    case ValPolarityDependent(_, _, onInhale, _, onExhale, _) => PolarityDependent(convert(onInhale), convert(onExhale))
  }

  def convert(implicit v: ValBindingContext): (Variable[G], Seq[Expr[G]]) = v match {
    case ValRangeBinding(t, id, _, from, _, to) =>
      val variable = new Variable[G](convert(t))(origin(id).replacePrefName(convert(id)))
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
      Let(new Variable(convert(t))(origin(id).replacePrefName(convert(id))), convert(v), convert(body))
    case ValForPerm(_, _, bindings, _, loc, _, body, _) =>
      ForPerm(convert(bindings), AmbiguousLocation(convert(loc))(blame(loc))(origin(loc)), convert(body))
  }

  def convert(implicit e: ValPrimaryVectorContext): Expr[G] = e match {
    case ValSum(_, _, t, id, _, cond, _, body, _) =>
      val binding = new Variable(convert(t))(origin(id).replacePrefName(convert(id)))
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
      InlinePattern(convert(pattern), open.count(_ == '<'), if (groupText.isEmpty) 0 else groupText.toInt)
    case ValUnfolding(_, predExpr, _, body) => Unfolding(convert(predExpr), convert(body))(blame(e))
    case ValOld(_, _, expr, _) => Old(convert(expr), at = None)(blame(e))
    case ValOldLabeled(_, _, label, _, _, expr, _) => Old(convert(expr), at = Some(new UnresolvedRef[G, LabelDecl[G]](convert(label))))(blame(e))
    case ValTypeof(_, _, expr, _) => TypeOf(convert(expr))
    case ValTypeValue(_, _, t, _) => TypeValue(convert(t))
    case ValHeld(_, _, obj, _) => Held(convert(obj))
    case ValCommitted(_, _, obj, _) => Committed(convert(obj))(blame(e))
    case ValIdEscape(text) => local(e, text.substring(1, text.length - 1))
    case ValSharedMemSize(_, _, ptr, _) => SharedMemSize(convert(ptr))
    case ValNdIndex(_, _, firstIndex, _, firstDim, parsePairs, _) =>
      val pairs = parsePairs.map(convert(_))
      val indices = convert(firstIndex) +: pairs.map(_._1)
      val dims = convert(firstDim) +: pairs.map(_._2)
      NdIndex(indices, dims)
    case ValNdLIndex(_, _, indices, _, dims, _) =>
      val allIndices = convert(indices)
      NdPartialIndex(allIndices.init, allIndices.last, convert(dims))
    case ValNdLength(_, _, dims, _) => NdLength(convert(dims))
  }

  def convert(implicit e: ValExprPairContext): (Expr[G], Expr[G]) = e match {
    case ValExprPair0(_, e1, _, e2) => (convert(e1), convert(e2))
  }

  def convert(implicit e: ValExprContext): Expr[G] = e match {
    case ValExpr0(inner) => convert(inner)
    case ValExpr1(inner) => convert(inner)
  }

  def convert(implicit id: ValIdentifierContext): String = id match {
    case ValIdentifier0(inner) => convertText(inner)
    case ValIdentifier1(ValKeywordNonExpr0(text)) => text
    case ValIdentifier2(text) => text.substring(1, text.length - 1)
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

  def evalOrNop(implicit expr: Option[ExpressionContext]): Statement[G] = expr match {
    case Some(expr) => Eval(convert(expr))(origin(expr))
    case None =>
      // PB: strictly speaking it would be nice if we can point to the empty range that indicates the absence of a statement here.
      Block(Nil)(DiagnosticOrigin)
  }

}