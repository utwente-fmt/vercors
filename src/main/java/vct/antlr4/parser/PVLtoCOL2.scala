package vct.antlr4.parser

import java.util.{ArrayList => JavaArrayList}

import hre.lang.System._
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
import vct.antlr4.generated.PVFullParser
import vct.antlr4.generated.PVFullParser._
import vct.antlr4.generated.PVFullParserPatterns._
import vct.col.ast.`type`.ASTReserved._
import vct.col.ast.`type`.{PrimitiveSort, Type}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression}
import vct.col.ast.generic.{ASTNode, BeforeAfterAnnotations}
import vct.col.ast.stmt.composite.{BlockStatement, ParallelBlock}
import vct.col.ast.stmt.decl.ASTClass.ClassKind
import vct.col.ast.stmt.decl.Method.Kind
import vct.col.ast.stmt.decl._
import vct.col.ast.util.ContractBuilder

import scala.collection.JavaConverters._

object PVLtoCOL2 {
  def convert(tree: ProgramContext, file_name: String, tokens: CommonTokenStream, parser: PVFullParser): ProgramUnit = {
    PVLtoCOL2(file_name, tokens, parser).convertProgram(tree)
  }
}

case class PVLtoCOL2(fileName: String, tokens: CommonTokenStream, parser: PVFullParser)
  extends ToCOL(fileName, tokens, parser) {
  def convertProgram(tree: ProgramContext): ProgramUnit = {
    val output = new ProgramUnit()

    tree match {
      case Program0(decls, None, _eof) =>
        decls.map(convertDecl).foreach(_.foreach(output.add))
      case Program0(_, Some(block), _eof) =>
        // I think program greedily matches programDecls, which matches block also?(tree)
        ?(tree)
    }

    output
  }

  def convertDecl(tree: ParserRuleContext): Seq[ASTDeclaration] = tree match {
    case ProgramDecl0(claz) => Seq(convertClass(claz))
    case ProgramDecl1(kernel) => Seq(convertKernel(kernel))
    case ProgramDecl2(block) => ?(tree) // What's a block doing at the top level?
    case ProgramDecl3(field) => ?(tree) // This is global state?
    case ProgramDecl4(method_decl) => ?(tree) // Global method?

    case ClazMember0(field) => convertField(field)
    case ClazMember1(method) => Seq(convertMethod(method))
    case ClazMember2(constructor) => Seq(convertConstructor(constructor))

    case KernelMember0(field) => Seq(convertKernelField(field))
    case KernelMember1(method) => Seq(convertMethod(method))
  }

  def convertDeclList(tree: DeclListContext): Seq[(String, Option[ASTNode])] = tree match {
    case DeclList0(name, maybeInit) =>
      Seq((convertID(name), maybeInit.map(expr)))
    case DeclList1(name, maybeInit, _, tail) =>
      (convertID(name), maybeInit.map(expr)) +: convertDeclList(tail)
  }

  def convertClass(tree: ClazContext): ASTClass = origin(tree, tree match {
    case Claz0(contract, "class", name, "{", members, "}") =>
      val result = create.ast_class(convertID(name), ClassKind.Plain, Array(), Array(), Array())
      members.map(convertDecl).foreach(_.foreach(result.add))
      result
  })

  def convertKernel(tree: KernelContext): ASTClass = origin(tree, tree match {
    case Kernel0("kernel", name, "{", members, "}") =>
      val result = create.ast_class(convertID(name), ClassKind.Kernel, Array(), Array(), Array())
      members.map(convertDecl).foreach(_.foreach(result.add))
      result
  })

  def convertField(tree: FieldContext): Seq[ASTDeclaration] = tree match {
    case Field0(t, ids, ";") =>
      val typ = convertType(t)
      convertIDList(ids).map(DeclarationStatement(_, typ, None))
  }

  def convertKernelField(tree: KernelFieldContext): ASTDeclaration = origin(tree, tree match {
    case KernelField0(locality, t, idList, _) =>
      ?(tree)
  })

  def convertMethod(method: MethodDeclContext): Method = origin(method, method match {
    case MethodDecl0(contract, modifiers, returnType, name, "(", maybeArgs, ")", bodyNode) =>
      val returns = convertType(returnType)
      var (kind, body) = convertBody(bodyNode)
      if(returns.isPrimitive(PrimitiveSort.Resource))
        kind = Kind.Predicate

      val result = create method_kind(kind, returns, convertContract(contract),
        convertID(name), maybeArgs.map(convertArgs).getOrElse(Seq()).toArray, body.orNull)

      modifiers match {
        case Modifiers0(modifiers) => modifiers.foreach {
          case "static" => result.setStatic(true)
          case "thread_local" => result.setFlag(ASTFlags.THREAD_LOCAL, true)
          case "inline" => result.setFlag(ASTFlags.INLINE, true)
          case "pure" =>
            Warning("The pure modifier is ignored, as the purity of a function is " +
              "derived from its declaration style (f(){} vs. f() = exp;)")
        }
      }
      result
  })

  def convertConstructor(method: ConstructorContext): Method = origin(method, method match {
    case Constructor0(contract, name, "(", args, ")", bodyNode) =>
      val returns = create primitive_type PrimitiveSort.Void
      val (_, body) = convertBody(bodyNode)
      create method_kind(Kind.Constructor, returns, convertContract(contract),
        convertID(name), args.map(convertArgs).getOrElse(Seq()).toArray, body.orNull)
  })

  def convertArgs(args: ArgsContext): Seq[DeclarationStatement] = args match {
    case Args0(t, name) =>
      Seq(create.field_decl(convertID(name), convertType(t)))
    case Args1(t, name, args) =>
      create.field_decl(convertID(name), convertType(t)) +: convertArgs(args)
  }

  def convertBody(body: ParserRuleContext): (Kind, Option[ASTNode]) = body match {
    case MethodBody0("=", exp, _) =>
      (Kind.Pure, Some(expr(exp)))
    case MethodBody1(inner) => convertBody(inner)

    case ConstructorBody0(_) =>
      (Kind.Plain, None)
    case ConstructorBody1(block) =>
      (Kind.Plain, Some(convertBlock(block)))
  }

  def convertID(thing: Gen_idContext): String = thing match {
    case Gen_id0(id) => convertID(id)
    case Gen_id1(id) => id
  }

  def convertID(identifier: IdentifierContext): String = identifier match {
    case Identifier0(name) => name
    case Identifier1(ValReserved0(_)) => ?(identifier) //forbidden
    case Identifier1(ValReserved1("\\result")) =>
      ?(identifier) // incorrect context for expression?
    case Identifier1(ValReserved2("\\current_thread")) =>
      ?(identifier) // same
  }

  def convertIDList(list: IdentifierListContext): Seq[String] = list match {
    case IdentifierList0(id) => Seq(convertID(id))
    case IdentifierList1(id, ",", ids) => convertID(id) +: convertIDList(ids)
  }

  def convertExpList(args: TupleContext): Seq[ASTNode] = args match {
    case Tuple0("(", maybeExprList, ")") => convertExpList(maybeExprList)
  }

  def convertExpList(args: Option[ExprListContext]): Seq[ASTNode] = args match {
    case Some(args) => convertExpList(args)
    case None => Seq()
  }

  def convertExpList(args: ExprListContext): Seq[ASTNode] = args match {
    case ExprList0(exp) =>
      Seq(expr(exp))
    case ExprList1(exp, ",", expList) =>
      expr(exp) +: convertExpList(expList)
  }

  def convertExpList(args: ValuesContext): Seq[ASTNode] = args match {
    case Values0(_, exps, _) =>
      ?(args)
  }

  def convertValExpList(args: ExpressionListContext): Seq[ASTNode] = args match {
    case ExpressionList0(exp) =>
      Seq(expr(exp))
    case ExpressionList1(exp, ",", expList) =>
      expr(exp) +: convertValExpList(expList)
  }

  // Also needs expression (val)
  def expr(tree: ParserRuleContext): ASTNode = origin(tree, tree match {
    case Expression0(exp) => expr(exp)

    case Expr0(label, ":", exp) =>
      val result = expr(exp)
      result.addLabel(create label convertID(label))
      result
    case Expr1(exp, "with", block) =>
      expr(exp) match {
        case ann: BeforeAfterAnnotations =>
          ann.set_before(convertBlock(block)); ann
        case _ =>
          ?(tree)
      }
    case Expr2(exp, "then", block) =>
      expr(exp) match {
        case ann: BeforeAfterAnnotations =>
          ann.set_before(convertBlock(block)); ann
        case _ =>
          ?(tree)
      }
    case Expr3("unfolding", exp, "in", inExp) =>
      create expression(Unfolding, expr(exp), expr(inExp))
    case Expr4(ite) => expr(ite)
    case IteExpr0(cond, "?", yes, ":", no) =>
      create expression(ITE, expr(cond), expr(yes), expr(no))
    case IteExpr1(impl) => expr(impl)
    case ImplicationExpr0(prop, "==>", concl) =>
      create expression(Implies, expr(prop), expr(concl))
    case ImplicationExpr1(prop, "-*", concl) =>
      create expression(Wand, expr(prop), expr(concl))
    case ImplicationExpr2(andOr) => expr(andOr)
    case AndOrExpr0(p, "&&", q) => create expression(And, expr(p), expr(q))
    case AndOrExpr1(p, "||", q) => create expression(Or, expr(p), expr(q))
    case AndOrExpr2(p, "**", q) => create expression(Star, expr(p), expr(q))
    case AndOrExpr3(eqExp) => expr(eqExp)
    case EqExpr0(left, "==", right) => create expression(EQ, expr(left), expr(right))
    case EqExpr1(left, "!=", right) => create expression(NEQ, expr(left), expr(right))
    case EqExpr2(relExp) => expr(relExp)
    case RelExpr0(left, "<", right) => create expression(LT, expr(left), expr(right))
    case RelExpr1(left, "<=", right) => create expression(LTE, expr(left), expr(right))
    case RelExpr2(left, ">=", right) => create expression(GTE, expr(left), expr(right))
    case RelExpr3(left, ">", right) => create expression(GT, expr(left), expr(right))
    case RelExpr4(setExp) => expr(setExp)
    case SetExpr0(elem, "in", set) => create expression(Member, expr(elem), expr(set))
    case SetExpr1(addExp) => expr(addExp)
    case AddExpr0(left, "+", right) => create expression(Plus, expr(left), expr(right))
    case AddExpr1(left, "-", right) => create expression(Minus, expr(left), expr(right))
    case AddExpr2(multExp) => expr(multExp)
    case MultExpr0(left, "*", right) => create expression(Mult, expr(left), expr(right))
    case MultExpr1(left, "/", right) => create expression(FloorDiv, expr(left), expr(right))
    case MultExpr2(left, "%", right) => create expression(Mod, expr(left), expr(right))
    case MultExpr3(left, "\\", right) => create expression(Div, expr(left), expr(right))
    case MultExpr4(powExp) => expr(powExp)
    case PowExpr0(left, "^^", right) => ?(tree)
    case PowExpr1(seqAddExp) => expr(seqAddExp)
    case SeqAddExpr0(x, "::", xs) => create expression(PrependSingle, expr(x), expr(xs))
    case SeqAddExpr1(xs, "++", ys) => create expression(Append, expr(xs), expr(ys))
    case SeqAddExpr2(unaryExp) => expr(unaryExp)
    case UnaryExpr0("!", exp) => create expression(Not, expr(exp))
    case UnaryExpr1("-", exp) => create expression(UMinus, expr(exp))
    case UnaryExpr2(newExp) => expr(newExp)
    case NewExpr0("new", clsName, args) =>
      create new_object(create class_type(convertID(clsName)), convertExpList(args):_*)
    case NewExpr1("new", t, dims) =>
      val baseType = convertType(t)
      val dimSizes = dims match {
        case NewDims0(qDims) => qDims.map {
          case QuantifiedDim0(_, size, _) => expr(size)
        }.toArray
      }
      var arrayType = baseType
      for(_ <- 0 to dimSizes.size)
        arrayType = create primitive_type(PrimitiveSort.Array, arrayType)
      create expression(NewArray, arrayType, dimSizes)
    case NewExpr2(nonTarget) => expr(nonTarget)
    case NewExpr3(target) => expr(target)

    case Target0(target, ".", prop) =>
      create dereference(expr(target), convertID(prop))
    case Target1(seq, "[", idx, "]") =>
      create expression(Subscript, expr(seq), expr(idx))
    case Target2(nonTarget, ".", prop) =>
      create dereference(expr(nonTarget), convertID(prop))
    case Target3(seq, "[", idx, "]") =>
      create expression(Subscript, expr(seq), expr(idx))
    case Target4(TargetUnit0(id)) => create unresolved_name(convertID(id))

    case NonTarget0(nonTarget, ".", prop) =>
      create dereference(expr(nonTarget), convertID(prop))
    case NonTarget1(obj, args) =>
      expr(obj) match {
        case name: NameExpression =>
          create invokation(null, null, name.getName, convertExpList(args):_*)
        case deref: Dereference =>
          create invokation(deref.obj, null, deref.field, convertExpList(args):_*)
        case other =>
          ?(tree)
      }
    case NonTarget2(seq, "[", "..", to, "]") =>
      create expression(Take, expr(seq), expr(to))
    case NonTarget3(seq, "[", idx, "]") =>
      create expression(Subscript, expr(seq), expr(idx))
    case NonTarget4(seq, "[", fr, "..", Some(to), "]") =>
      create expression(Slice, expr(seq), expr(fr), expr(to))
    case NonTarget4(seq, "[", fr, "..", None, "]") =>
      create expression(Drop, expr(seq), expr(fr))
    case NonTarget5(seq, "[", replIdx, "->", replVal, "]") =>
      create expression(SeqUpdate, expr(seq), expr(replIdx), expr(replVal))
    case NonTarget6(objNode, "->", method, args) =>
      val obj = expr(objNode)
      create expression(Implies,
        create expression(NEQ, obj, create reserved_name(Null)),
        create invokation(obj, null, convertID(method), convertExpList(args):_*))
    case NonTarget7(unit) => expr(unit)

    case NonTargetUnit0("this") => create reserved_name This
    case NonTargetUnit1("null") => create reserved_name Null
    case NonTargetUnit2("true") => create constant true
    case NonTargetUnit3("false") => create constant false
    case NonTargetUnit4("current_thread") => create reserved_name CurrentThread
    case NonTargetUnit5("\\result") => create reserved_name Result
    case NonTargetUnit6(collection) => expr(collection)
    case NonTargetUnit7(method, argsTuple) =>
      val args = convertExpList(argsTuple)
      val methodName = method match { case BuiltinMethod0(name) => name }
      methodName match {
        case "Value" => create expression(Value, args:_*)
        case "HPerm" => create expression(HistoryPerm, args:_*)
        case "Perm" => create expression(Perm, args:_*)
        case "PointsTo" => create expression(PointsTo, args:_*)
        case "Hist" => create expression(History, args:_*)
        case "\\old" => create expression(Old, args:_*)
        case "?" => create expression(BindOutput, args:_*)
        case "idle" => create expression(PVLidleToken, args:_*)
        case "running" => create expression(PVLjoinToken, args:_*)
        case "head" => create expression(Head, args:_*)
        case "tail" => create expression(Tail, args:_*)
        case "held" => create expression(Held, args:_*)
        case "Some" => create expression(OptionSome, args:_*)
      }
    case NonTargetUnit8(_owner, "(", a, ",", b, ",", c, ")") => ?(tree)
    case NonTargetUnit9("id", "(", exp, ")") => expr(exp)
    case NonTargetUnit10("|", seq, "|") => create expression(Size, expr(seq))
    case NonTargetUnit11("?", id) => ?(tree)
    case NonTargetUnit12(num) => create constant Integer.parseInt(num)
    case NonTargetUnit13(seq) => ?(tree)
    case NonTargetUnit14("(", exp, ")") => expr(exp)
    case NonTargetUnit15(id) => create unresolved_name convertID(id)
    case NonTargetUnit16(valPrimary) => expr(valPrimary)
    case DeclInit0("=", exp) => expr(exp)

    case CollectionConstructors0(container, _, t, _, values) =>
      ?(tree)
    case CollectionConstructors1("[", exps, "]") =>
      ?(tree)
    case CollectionConstructors2("[t:", t, "]") =>
      ?(tree)
    case CollectionConstructors3("{", exps, "}") =>
      ?(tree)
    case CollectionConstructors4("b{", exps, "}") =>
      ?(tree)
    case CollectionConstructors5("b{t:", t, "}") =>
      ?(tree)

    case x: ValPrimaryContext => ?(tree)
  })

  def convertType(t: ParserRuleContext): Type = origin(t, t match {
    case Type0(t, dims) =>
      val dimCount = dims match {
        case TypeDims0(dims) => dims.size
        case TypeDims1(dims) => dims.size
      }
      var result = convertType(t)
      for(_ <- 0 until dimCount)
        result = create.primitive_type(PrimitiveSort.Array, result)
      result

    case NonArrayType0(container, "<", innerType, ">") =>
      val kind = container match {
        case "seq" => PrimitiveSort.Sequence
        case "set" => PrimitiveSort.Set
        case "bag" => PrimitiveSort.Bag
      }
      create.primitive_type(kind, convertType(innerType))
    case NonArrayType1("option", "<", t, ">") =>
      create.primitive_type(PrimitiveSort.Option, convertType(t))
    case NonArrayType2(primitive) =>
      create.primitive_type(primitive match {
        case "string" => PrimitiveSort.String
        case "process" => PrimitiveSort.Process
        case "int" => PrimitiveSort.Integer
        case "boolean" => PrimitiveSort.Boolean
        case "zfrac" => PrimitiveSort.ZFraction
        case "frac" => PrimitiveSort.Fraction
        case "resource" => PrimitiveSort.Resource
        case "void" => PrimitiveSort.Void
      })
    case NonArrayType3(ClassType0(name, maybeTypeArgs)) =>
      create class_type(convertID(name), (maybeTypeArgs match {
        case None => Seq()
        case Some(TypeArgs0(_, args, _)) =>
          convertExpList(args)
      }).asJava)
  })

  def convertContract(contract: ParserRuleContext): Contract = origin(contract, contract match {
    case Contract0(clauses) =>
      val contractBuilder = new ContractBuilder
      clauses.foreach(convertClause(_, contractBuilder))
      contractBuilder.getContract(false)
    case InvariantList0(invariants) =>
      val contractBuilder = new ContractBuilder
      invariants.foreach(convertClause(_, contractBuilder))
      contractBuilder.getContract(false)
  })

  def convertClause(clause: ParserRuleContext,
                    builder: ContractBuilder): Unit = clause match {
    case ValContractClause0(_modifies, names, _) =>
      builder.modifies(convertValExpList(names):_*)
    case ValContractClause1(_accessible, names, _) =>
      builder.accesses(convertValExpList(names):_*)
    case ValContractClause2(_requires, exp, _) =>
      builder.requires(expr(exp))
    case ValContractClause3(_ensures, exp, _) =>
      builder.ensures(expr(exp))
    case ValContractClause4(_given, t, name, _) =>
      builder.`given`(create.field_decl(convertID(name), convertType(t)))
    case ValContractClause5(_yields, t, name, _) =>
      builder.yields(create.field_decl(convertID(name), convertType(t)))
    case ValContractClause6(_context_everywhere, exp, _) =>
      builder.appendInvariant(expr(exp))
    case ValContractClause7(_context, exp, _) =>
      builder.context(expr(exp))
    case Invariant0(_loop_invariant, exp, _) =>
      builder.appendInvariant(expr(exp))
  }

  def convertBlock(block: ParserRuleContext): BlockStatement = block match {
    case Block0(_, statements, _) =>
      create block(statements.map(convertStat):_*)
  }

  def convertStat(stat: ParserRuleContext): ASTNode = origin(stat, stat match {
    case Statement0("return", None, _) => create.return_statement()
    case Statement0("return", Some(value), _) => create.return_statement(expr(value))
    case Statement1("lock", exp, _) => create special(ASTSpecial.Kind.Lock, expr(exp))
    case Statement2("unlock", exp, _) => create special(ASTSpecial.Kind.Unlock, expr(exp))
    case Statement3("wait", exp, _) => create special(ASTSpecial.Kind.Wait, expr(exp))
    case Statement4("notify", exp, _) => create special(ASTSpecial.Kind.Notify, expr(exp))
    case Statement5("fork", exp, _) => create special(ASTSpecial.Kind.Fork, expr(exp))
    case Statement6("join", exp, _) => create special(ASTSpecial.Kind.Join, expr(exp))
    case Statement7("action", args, block) => ?(stat)
    case Statement8(valStat) => convertStat(valStat)
    case Statement9("if", "(", cond, ")", thenStat, maybeElseStat) =>
      create ifthenelse(expr(cond), convertStat(thenStat), maybeElseStat.map(convertStat).orNull)
    case ElseBlock0("else", stat) => convertStat(stat)
    case Statement10("barrier", "(", name, maybeTags, ")", bodyNode) =>
      val tags = maybeTags match {
        case Some(BarrierTags0(_, tags)) => convertIDList(tags)
        case None => Seq()
      }
      val (maybeBody, contract) = bodyNode match {
        case BarrierBody0("{", contract, "}") =>
          (None, convertContract(contract))
        case BarrierBody1(contract, body) =>
          (Some(convertBlock(body)), convertContract(contract))
      }
      val tagsJavaList = new JavaArrayList[String](tags.asJava)
      create barrier(convertID(name), contract, tagsJavaList, maybeBody.orNull)
    case Statement11(contract, "par", parUnitList) =>
      val parUnits = convertParUnitList(parUnitList)
      val javaParUnits = new JavaArrayList[ParallelBlock](parUnits.asJava)
      create region(convertContract(contract), javaParUnits)
    case Statement12("vec", "(", iter, ")", block) =>
      create vector_block(convertParIter(iter), convertBlock(block))
    case Statement13("invariant", label, "(", resource, ")", block) =>
      create invariant_block(convertID(label), expr(resource), convertBlock(block))
    case Statement14("atomic", "(", invariants, ")", block) =>
      create parallel_atomic(convertBlock(block), convertIDList(invariants):_*)
    case Statement15(invariants, "while", "(", cond, ")", body) =>
      create while_loop(expr(cond), convertStat(body), convertContract(invariants))
    case Statement16(invariants, "for", "(", maybeInit, ";", maybeCond, ";", maybeUpdate, ")", body) =>
      create for_loop(
        maybeInit.map(convertStat).orNull,
        maybeCond.map(expr).getOrElse(create constant true),
        maybeUpdate.map(convertStat).orNull,
        convertStat(body),
        convertContract(invariants)
      )
    case Statement17(block) => convertBlock(block)
    case Statement18("{*", exp, "*}") => ?(stat)
    case Statement19("goto", label, _) =>
      create special(ASTSpecial.Kind.Goto, create unresolved_name convertID(label))
    case Statement20("label", label, _) =>
      create special(ASTSpecial.Kind.Label, create unresolved_name convertID(label))
    case Statement21(stat, _) => convertStat(stat)
    case ForStatementList0(x) => ?(stat)
    case ForStatementList1(x, ",", xs) => ?(stat)
    case AllowedForStatement0(tNode, decls) =>
      val t = convertType(tNode)
      val result = new VariableDeclaration(t)
      val statements = convertDeclList(decls).foreach{
        case (name, init) =>
          result.add(DeclarationStatement(name, VariableDeclaration.common_type, init))
      }
      result
    case AllowedForStatement1(exp) => expr(exp)
    case AllowedForStatement2(id, "++") => create expression(PostIncr, create unresolved_name convertID(id))
    case AllowedForStatement2(id, "--") => create expression(PostDecr, create unresolved_name convertID(id))
    case AllowedForStatement3(target, "=", exp) =>
      create assignment(expr(target), expr(exp))

    case x: ValStatementContext => ?(x)
  })

  def convertParUnitList(tree: ParUnitListContext): Seq[ParallelBlock] = tree match {
    case ParUnitList0(x) => Seq(convertParUnit(x))
    case ParUnitList1(x, "and", xs) => convertParUnit(x) +: convertParUnitList(xs)
  }

  def convertParUnit(tree: ParUnitContext): ParallelBlock = origin(tree, tree match {
    case ParUnit0(maybeLabel, _, maybeIters, maybeWaitList, _, contract, block) =>
      val label = maybeLabel.map(convertID).getOrElse("")
      val iters = maybeIters.map(convertParIters).getOrElse(Seq())
      val waitList = maybeWaitList.map(convertParWaitList).getOrElse(Seq())
      create parallel_block(label, convertContract(contract), iters.toArray, convertBlock(block), waitList.toArray[ASTNode])
    case ParUnit1(contract, block) =>
      create parallel_block("", convertContract(contract), Array(), convertBlock(block))
  })

  def convertParIters(tree: ItersContext): Seq[DeclarationStatement] = tree match {
    case Iters0(x) => Seq(convertParIter(x))
    case Iters1(x, ",", xs) => convertParIter(x) +: convertParIters(xs)
  }

  def convertParIter(tree: IterContext): DeclarationStatement = origin(tree, tree match {
    case Iter0(t, name, "=", left, "..", right) =>
      val range = create expression(RangeSeq, expr(left), expr(right))
      create field_decl(convertID(name), convertType(t), range)
  })

  def convertParWaitList(tree: ParserRuleContext): Seq[MethodInvokation] = tree match {
    case ParWaitList0(_, list) => convertParWaitList(list)
    case WaitList0(x) => Seq(convertParWait(x))
    case WaitList1(x, ",", xs) => convertParWait(x) +: convertParWaitList(xs)
  }

  def convertParWait(tree: WaitForContext): MethodInvokation = origin(tree, tree match {
    case WaitFor0(name, maybeArgs) =>
      create invokation(null, null, convertID(name), (maybeArgs match {
        case None => Seq()
        case Some(WaitForArgs0(_, args, _)) => convertIdArgs(args)
      }):_*)
  })

  def convertIdArgs(tree: IdArgListContext): Seq[ASTNode] = (tree match {
    case IdArgList0(x) => Seq(x)
    case IdArgList1(x, ",", xs) => x +: convertIdArgs(xs)
  }).map {
    case IdArg0(id) => create unresolved_name(convertID(id))
    case IdArg1("*") => create reserved_name(Any)
  }
}
