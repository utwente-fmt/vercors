package vct.parsers.transform

import hre.data.BitString
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import vct.antlr4.generated.LLVMSpecParser._
import vct.antlr4.generated.LLVMSpecParserPatterns
import vct.antlr4.generated.LLVMSpecParserPatterns._
import vct.col.ast._
import vct.col.origin.{ExpectedError, Origin}
import vct.col.ref.{Ref, UnresolvedRef}
import vct.col.util.AstBuildHelpers.{ff, foldAnd, implies, tt}

import scala.annotation.nowarn
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable

@nowarn("msg=match may not be exhaustive&msg=Some\\(")
case class LLVMContractToCol[G](override val originProvider: OriginProvider,
                                override val blameProvider: BlameProvider,
                                override val errors: Seq[(Token, Token, ExpectedError)])
  extends ToCol(originProvider, blameProvider, errors) {

  def local(ctx: ParserRuleContext, name: String): Expr[G] =
    LlvmLocal(name)(blame(ctx))(origin(ctx))

  def createVariable(ctx: ParserRuleContext, id: LangIdContext, t: LangTypeContext): Variable[G] = {
    val varId = convert(id)
    val variable = new Variable(convert(t))(origin(ctx).replacePrefName(varId))
    variable
  }

  def convert(implicit contract: ValEmbedContractContext): ApplicableContract[G] = {
    val collector = new ContractCollector[G]()
    contract match {
      case ValEmbedContract0(blocks) => blocks.foreach(convert(_, collector))
    }
    collector.consumeApplicableContract(blame(contract))
  }

  // assume specLevel == 1 (so only cover the 2nd parse rule)
  def convert(contract: ValEmbedContractBlockContext, collector: ContractCollector[G]): Unit = contract match {
    case ValEmbedContractBlock1(clauses) => clauses.foreach(convert(_, collector))
  }

  def convert(implicit contract: ValContractClauseContext, collector: ContractCollector[G]): Unit = contract match {
    //case ValContractClause0(_, ids, _) => collector.modifies ++= convert(ids).map((contract, _))
    //case ValContractClause1(_, ids, _) => collector.accessible ++= convert(ids).map((contract, _))
    case ValContractClause2(_, exp, _) => collector.requires += ((contract, convert(exp)))
    case ValContractClause3(_, exp, _) => collector.ensures += ((contract, convert(exp)))
    case ValContractClause4(_, t, id, _) =>
      val variable = createVariable(contract, id, t)
      collector.given += ((contract, variable))
    case ValContractClause5(_, t, id, _) =>
      val variable = createVariable(contract, id, t)
      collector.yields += ((contract, variable))
    case ValContractClause6(_, exp, _) => collector.context_everywhere += ((contract, convert(exp)))
    case ValContractClause7(_, exp, _) =>
      collector.requires += ((contract, convert(exp)))
      collector.ensures += ((contract, convert(exp)))
    //case ValContractClause8(_, exp, _) => collector.loop_invariant += ((contract, convert(exp)))
    //case ValContractClause9(_, exp, _) => collector.kernel_invariant += ((contract, convert(exp)))
    case ValContractClause10(_, _, t, id, _, exp, _) =>
      val variable = createVariable(contract, id, t)
      collector.signals += ((contract, SignalsClause(variable, convert(exp))(originProvider(contract))))
    //case ValContractClause11(_, invariant, _) => collector.lock_invariant += ((contract, convert(invariant)))
    case ValContractClause12(_, None, _) => collector.decreases += ((contract, DecreasesClauseNoRecursion()))
    case ValContractClause12(_, Some(clause), _) => collector.decreases += ((contract, convert(clause)))
  }

  def convert(implicit t: LangTypeContext): Type[G] = t match {
    case LangType0(t) => convert(t)
  }

  def convert(implicit t: TypeContext): Type[G] = t match {
    case T_intRule(_) => TInt()
    case T_boolRule(_) => TBool()
  }

  def convert(implicit id: LangIdContext): String = id match {
    case LangId0(text) => text
  }

  def convert(implicit exprs: ExpressionListContext): Seq[Expr[G]] = exprs match {
    case ExpressionList0(expr) => Seq(convert(expr))
    case ExpressionList1(expr, _, exprs) => convert(expr) +: convert(exprs)
  }


  def convert(implicit expr: LangExprContext): Expr[G] = expr match {
    case LangExpr0(expr) => convert(expr)
  }

  def convert(implicit expr: ExpressionContext): Expr[G] = expr match {
    case Expression0(instruction) => convert(instruction)
    case Expression1(constant) => convert(constant)
    case Expression2(identifier) => convert(identifier)
    case Expression3(valExpr) => convert(valExpr)
    case Expression4(e1, impOp, e2) => impOp match {
      case ValImpOp0(_) => ???
      case ValImpOp1(_) => Implies(convert(e1), convert(e2))
    }
  }

  def convert(implicit inst: InstructionContext): Expr[G] = inst match {
    case BinOpRule(binOp) => convert(binOp)
    case CmpOpRule(cmpOp) => convert(cmpOp)
    case CallOpRule(callOp) => convert(callOp)
    case BrOpRule(brOp) => convert(brOp)
  }

  def convert(implicit brOp:BranchInstructionContext): Expr[G] = brOp match {
    case BranchInstruction0(_, _, testExpr, _, trueExpr, _, falseExpr, _) =>
      Select(convert(testExpr), convert(trueExpr), convert(falseExpr))
  }

  def convert(implicit callOp: CallInstructionContext): Expr[G] = callOp match {
    case CallInstruction0(_, id, _, exprList, _) =>
      val args: Seq[Expr[G]] = convert(exprList)
      LlvmAmbiguousFunctionInvocation(id, args, Nil, Nil)(blame(callOp))
  }

  def convert(implicit binOp: BinOpInstructionContext): Expr[G] = binOp match {
    case BinOpInstruction0(op, _, lhs, _, rhs, _) => convert(op, lhs, rhs)
  }

  def convert(implicit op: BinOpContext, lhs: ExpressionContext, rhs: ExpressionContext): Expr[G] = {
    implicit val o: Origin = origin(op.getParent)
    val left: Expr[G] = convert(lhs)
    val right: Expr[G] = convert(rhs)
    op match {
      case Add(_) => Plus(left, right)
      case Sub(_) => Minus(left, right)
      case Mul(_) => Mult(left, right)
      case Udiv(_) | Sdiv(_) => FloorDiv(left, right)(blame(op))
      // bitwise/boolean
      case bitOp => left.t match {
        case TBool() => bitOp match {
          case LLVMSpecParserPatterns.And(_) => vct.col.ast.And(left, right)
          case LLVMSpecParserPatterns.Or(_) => vct.col.ast.Or(left, right)
          case Xor(_) => Neq(left, right)
        }
        case TInt() => bitOp match {
          case LLVMSpecParserPatterns.And(_) => BitAnd(left, right)
          case LLVMSpecParserPatterns.Or(_) => BitOr(left, right)
          case Xor(_) => BitXor(left, right)
        }
      }
    }
  }

  def convert(implicit cmpOp: CompareInstructionContext): Expr[G] =
    cmpOp match {
      case CompareInstruction0(_, _, pred, _, lhs, _, rhs, _) => convert(pred, lhs, rhs)
    }

  def convert(implicit pred: CompPredContext, lhs: ExpressionContext, rhs: ExpressionContext): Expr[G] = {
    implicit val o: Origin = origin(pred.getParent)
    val left: Expr[G] = convert(lhs)
    val right: Expr[G] = convert(rhs)
    pred match {
      // Eq name clash
      case LLVMSpecParserPatterns.Eq(_) => vct.col.ast.Eq(left, right)
      case Ne(_) => Neq(left, right)
      case Ugt(_) | Sgt(_) => Greater(left, right)
      case Uge(_) | Sge(_) => GreaterEq(left, right)
      case Ult(_) | Slt(_) => Less(left, right)
      case Ule(_) | Sle(_) => LessEq(left, right)
    }
  }

  def convert(implicit const: ConstantContext): Expr[G] = {
    const match {
      case BoolConstRule(const) => BooleanValue(const match {
        case ConstTrue(_) => true
        case ConstFalse(_) => false
      })(origin(const))
      case IntConstRule(IntegerConstant0(constInt)) => IntegerValue(BigInt(constInt))(origin(const))
    }
  }

  def convert(implicit id: IdentifierContext): Expr[G] = id match {
    case Identifier0(text) => local(id, text)
  }


  def convert(implicit e: ValExprContext): Expr[G] = e match {
    case ValExpr0(inner) => convert(inner)
    case ValExpr1(inner) => convert(inner)
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

  // valsetcompselectors
  def convert(implicit exprs: ValMapPairsContext): Seq[(Expr[G], Expr[G])] = exprs match {
    case ValMapPairs0(k, _, v) => Seq((convert(k), convert(v)))
    case ValMapPairs1(k, _, v, _, tail) => (convert(k), convert(v)) +: convert(tail)
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
      val variable = createVariable(e, id, t)
      Let(variable, convert(v), convert(body))
    case ValForPerm(_, _, bindings, _, loc, _, body, _) =>
      ForPerm(convert(bindings), AmbiguousLocation(convert(loc))(blame(loc))(origin(loc)), convert(body))
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

  def convert(implicit v: ValBindingContext): (Variable[G], Seq[Expr[G]]) = v match {
    case ValRangeBinding(t, id, _, from, _, to) =>
      val variable = createVariable(v, id, t)
      val cond = SeqMember[G](Local(variable.ref), Range(convert(from), convert(to)))
      (variable, Seq(cond))
    case ValNormalBinding(arg) =>
      (convert(arg), Nil)
  }

  def convert(implicit arg: ValArgContext): Variable[G] = arg match {
    case ValArg0(t, id) => createVariable(arg, id, t)
  }

  def convert(implicit args: ValArgListContext): Seq[Variable[G]] = args match {
    case ValArgList0(arg) => Seq(convert(arg))
    case ValArgList1(arg, _, args) => convert(arg) +: convert(args)
  }

  def convert(implicit e: ValPrimaryVectorContext): Expr[G] = e match {
    case ValSum(_, _, t, id, _, cond, _, body, _) =>
      val binding = createVariable(e, id, t)
      Sum(Seq(binding), convert(cond), convert(body))
    case ValVectorSum(_, _, rng, _, vec, _) => VectorSum(convert(rng), convert(vec))
    case ValVectorCmp(_, _, left, _, right, _) => VectorCompare(convert(left), convert(right))
    case ValVectorRep(_, _, inner, _) => VectorRepeat(convert(inner))
    case ValMatrixSum(_, _, rng, _, mat, _) => MatrixSum(convert(rng), convert(mat))
    case ValMatrixCmp(_, _, left, _, right, _) => MatrixCompare(convert(left), convert(right))
    case ValMatrixRep(_, _, inner, _) => MatrixRepeat(convert(inner))
  }

  def convert(implicit clause: ValDecreasesMeasureContext): DecreasesClause[G] = clause match {
    case ValDecreasesMeasure0(_) => DecreasesClauseAssume()
    case ValDecreasesMeasure1(exps) => DecreasesClauseTuple(convert(exps))
  }

  def convert(implicit e: ValPrimaryThreadContext): Expr[G] = e match {
    case ValIdle(_, _, thread, _) => IdleToken(convert(thread))
    case ValRunning(_, _, thread, _) => JoinToken(convert(thread))
  }

  def convert(implicit e: ValPrimaryReducibleContext): Expr[G] = ??(e)

  def convert(implicit e: ValPrimaryContextContext): Expr[G] = e match {
    case ValPrimaryContext0("\\result") => AmbiguousResult()
    case ValPrimaryContext1("\\current_thread") => CurrentThreadId()
    case ValPrimaryContext2("\\ltid") => LocalThreadId()
    case ValPrimaryContext3("\\gtid") => GlobalThreadId()
  }

  def convert(implicit e: ValExprPairContext): (Expr[G], Expr[G]) = e match {
    case ValExprPair0(_, e1, _, e2) => (convert(e1), convert(e2))
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

  def convert(implicit ids: ValIdListContext): Seq[String] = ids match {
    case ValIdList0(id) => Seq(convert(id))
    case ValIdList1(id, _, ids) => convert(id) +: convert(ids)
  }

  def convert(implicit exps: ValExpressionListContext): Seq[Expr[G]] = exps match {
    case ValExpressionList0(expr) => Seq(convert(expr))
    case ValExpressionList1(head, _, tail) => convert(head) +: convert(tail)
  }

  def convert(implicit decl: ValGlobalDeclarationContext): GlobalDeclaration[G] = decl match {
    case ValFunction(contract, modifiers, _, t, name, typeArgs, _, args, _, definition) =>
      val contractCollector = new ContractCollector[G]()
      contract.foreach(convert(_, contractCollector))

      val modifierCollector = new ModifierCollector()
      modifiers.foreach(convert(_, modifierCollector))

      val namedOrigin = origin(decl).replacePrefName(convert(name))
      new LlvmSpecFunction(
        convert(name),
        convert(t),
        args.map(convert(_)).getOrElse(Nil),
        Nil, // TODO implement
        convert(definition),
        contractCollector.consumeApplicableContract(blame(decl)),
        modifierCollector.consume(modifierCollector.inline))(blame(decl)
      )(namedOrigin)
  }

  def convert(implicit definition: ValPureDefContext): Option[Expr[G]] = definition match {
    case ValPureAbstractBody(_) => None
    case ValPureBody(_, expr, _) => Some(convert(expr))
  }

  def convert(mod: ValModifierContext, collector: ModifierCollector): Unit = mod match {
    case ValModifier0(name) => name match {
      case "pure" => collector.pure += mod
      case "inline" => collector.inline += mod
      case "thread_local" => collector.threadLocal += mod
      case "bip_annotation" => collector.bipAnnotation += mod
    }
    case ValStatic(_) => collector.static += mod
  }


}