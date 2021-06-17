package vct.parsers

import java.util.{ArrayList => JavaArrayList}
import hre.lang.System._
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
import vct.antlr4.generated.PVLParser
import vct.antlr4.generated.PVLParser._
import vct.antlr4.generated.PVLParserPatterns._
import vct.col.ast.`type`.ASTReserved._
import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, Type}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression, NameExpressionKind, StandardOperator}
import vct.col.ast.generic.{ASTNode, BeforeAfterAnnotations}
import vct.col.ast.stmt.composite.{BlockStatement, ParallelBlock}
import vct.col.ast.stmt.decl.ASTClass.ClassKind
import vct.col.ast.stmt.decl.Method.Kind
import vct.col.ast.stmt.decl._
import vct.col.ast.util.ContractBuilder
import vct.parsers.rewrite.InferADTTypes

import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

object PVLtoCOL {
  def convert(tree: ProgramContext, fileName: String, tokens: CommonTokenStream, parser: PVLParser): ProgramUnit = {
    PVLtoCOL(fileName, tokens, parser).convertProgram(tree)
  }
}

// Maybe we can turn this off in the future.
@nowarn("msg=not.*?exhaustive")
case class PVLtoCOL(fileName: String, tokens: CommonTokenStream, parser: PVLParser)
  extends ToCOL(fileName, tokens, parser) {
  def convertProgram(tree: ProgramContext): ProgramUnit = {
    val output = new ProgramUnit()

    tree match {
      case Program0(decls, None, _eof) =>
        decls.map(convertDecl).foreach(_.foreach(output.add))
      case Program0(_, Some(block), _eof) =>
        // I think program greedily matches programDecls, which matches block also
        ??(tree)
    }

    output
  }

  def convertDecl(tree: LangDeclContext): ASTDeclaration = ???

  def convertDecl(tree: ParserRuleContext): Seq[ASTDeclaration] = tree match {
    case ProgramDecl0(claz) => Seq(convertClass(claz))
    case ProgramDecl1(kernel) => Seq(convertKernel(kernel))
    case ProgramDecl2(block) => ??(tree) // What's a block doing at the top level?
    case ProgramDecl3(field) => ??(tree) // This is global state?
    case ProgramDecl4(method_decl) => ??(tree) // Global method?

    case ClazMember0(constructor) => Seq(convertConstructor(constructor))
    case ClazMember1(method) => Seq(convertMethod(method))
    case ClazMember2(field) => convertField(field)

    case KernelMember0(field) => convertKernelField(field)
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
      result.setFlag(ASTFlags.FINAL, true)
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

  def convertKernelField(tree: KernelFieldContext): Seq[ASTDeclaration] = tree match {
    case KernelField0(locality, t, idList, _) =>
      val typ = convertType(t)
      convertIDList(idList).map(id => {
        val decl = create field_decl(id, typ)
        decl.setStatic(locality == "global")
        decl
      })
  }

  def convertMethod(method: MethodDeclContext): Method = origin(method, method match {
    case MethodDecl0(contract, modifiers, returnType, name, "(", maybeArgs, ")", bodyNode) =>
      val returns = convertType(returnType)
      var (kind, body) = convertBody(bodyNode)

      modifiers.foreach {
        case Modifier0("pure") =>
          kind = Kind.Pure
        case _ =>
      }

      if(returns.isPrimitive(PrimitiveSort.Resource))
        kind = Kind.Predicate

      val result = create method_kind(kind, returns, convertContract(contract),
        convertID(name), maybeArgs.map(convertArgs).getOrElse(Seq()).toArray, body.orNull)

      modifiers.map(convertModifier).foreach(mod => {
        /* These flags have special status in InlinePredicatesRewriter and CurrentThreadRewriter. Probably we should
         * have exactly one way of setting the property (although inline may have a language-level and specification-
         * level meaning) */
        if (mod.isReserved(ASTReserved.Inline)) {
          result.setFlag(ASTFlags.INLINE, true);
        } else if(mod.isReserved(ASTReserved.ThreadLocal)) {
          result.setFlag(ASTFlags.THREAD_LOCAL, true)
        } else if(!mod.isReserved(ASTReserved.Pure)) { // already covered by scan above
          result.attach(mod)
        }
      })
      result
  })

  def convertModifier(mod: LangModifierContext): NameExpression =
    mod match { case LangModifier0(mod) => convertModifier(mod) }

  def convertModifier(mod: ModifierContext): NameExpression = origin(mod, mod match {
    case Modifier0("static") => create reserved_name ASTReserved.Static
    case Modifier0("thread_local") => create reserved_name ASTReserved.ThreadLocal
    case Modifier0("inline") => create reserved_name ASTReserved.Inline
    case Modifier0("pure") => create reserved_name ASTReserved.Pure
  })

  def convertConstructor(method: ConstructorContext): Method = origin(method, method match {
    case Constructor0(contract, name, "(", args, ")", bodyNode) =>
      val returns = create primitive_type PrimitiveSort.Void
      val (_, body) = convertBody(bodyNode)
      create method_kind(Kind.Constructor, returns, convertContract(contract),
        convertID(name), args.map(convertArgs).getOrElse(Seq()).toArray, body.orNull)
  })

  def convertContract(contract: ParserRuleContext): Contract = origin(contract, contract match {
    case Contract0(clauses) =>
      getContract(clauses.map(convertValClause):_*)
    case InvariantList0(invariants) =>
      getContract(invariants.map(convertInvariant):_*)
    case valClause: ValContractClauseContext =>
      getContract(convertValClause(valClause))
    case invClause: InvariantContext =>
      getContract(convertInvariant(invClause))
  })

  def convertArgs(args: ArgsContext): Seq[DeclarationStatement] = ???
//    args match {
//    case Args0(t, name) =>
//      Seq(create.field_decl(convertID(name), convertType(t)))
//    case Args1(t, name, _, args) =>
//      create.field_decl(convertID(name), convertType(t)) +: convertArgs(args)
//  }

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
    case Gen_id1(Container0(id)) => id
  }

  def convertID(identifier: IdentifierContext): String = ???
//    identifier match {
//    case Identifier0(name) => name
//    case Identifier1(ValReserved1(s)) => s.substring(1, s.length-1)
//    case Identifier1(reserved) =>
//      fail(reserved, "This identifier is reserved and cannot be declared.")
//  }

  def convertID(identifier: LangIdContext): String = identifier match {
    case LangId0(id) => convertID(id)
  }

  def convertIDName(identifier: IdentifierContext): NameExpression = origin(identifier, identifier match {
    case Identifier0(name) =>
      create unresolved_name name
//    case Identifier1(reserved) =>
//      convertValReserved(reserved)
  })

  def convertIDName(identifier: LangIdContext): NameExpression = identifier match {
    case LangId0(id) => convertIDName(id)
  }

  def convertIDList(list: IdentifierListContext): Seq[String] =  ???
//    list match {
//    case IdentifierList0(id) => Seq(convertID(id))
//    case IdentifierList1(id, ",", ids) => convertID(id) +: convertIDList(ids)
//  }

  def convertExpList(args: TupleContext): Seq[ASTNode] = args match {
    case Tuple0("(", maybeExprList, ")") => convertExpList(maybeExprList)
  }

  def convertMapValues(args: MapValuesContext): Seq[ASTNode] = args match {
    case MapValues0("{", maybeMapPairs,"}") => convertMapPairs(maybeMapPairs)
  }

  def convertMapPairs(args: Option[MapPairsContext]): Seq[ASTNode] = args match {
    case Some(args) => convertMapPairs(args)
    case None => Seq()
  }
  def convertMapPairs(args: MapPairsContext): Seq[ASTNode] = ???
//    args match {
//    case MapPairs0(key, "->", value) => Seq(expr(key), expr(value))
//    case MapPairs1(key, "->", value, ",", pairList) =>
//      Seq(expr(key), expr(value)) ++ convertMapPairs(pairList)
//  }

  def convertExpList(args: Option[ExprListContext]): Seq[ASTNode] = args match {
    case Some(args) => convertExpList(args)
    case None => Seq()
  }

  def convertExpList(args: ExprListContext): Seq[ASTNode] = ???
//    args match {
//    case ExprList0(exp) =>
//      Seq(expr(exp))
//    case ExprList1(exp, ",", expList) =>
//      expr(exp) +: convertExpList(expList)
//  }

  def convertExpList(args: ValuesContext): Seq[ASTNode] = args match {
    case Values0(_, None, _) => Seq()
    case Values0(_, Some(values), _) => convertExpList(values)
  }

  // Also needs expression (val)
  def expr(tree: ParserRuleContext): ASTNode = origin(tree, tree match {
    case LangExpr0(exp) => expr(exp)

//    case Expr0(label, ":", exp) =>
//      val result = expr(exp)
//      result.addLabel(create label convertID(label))
//      result
    case Expr1(exp, "with", block) =>
      expr(exp) match {
        case ann: BeforeAfterAnnotations =>
          ann.set_before(convertBlock(block)); ann
        case _ =>
          fail(tree, "This expression does not allow for with/then annotations.")
      }
//    case Expr2(exp, "then", block) =>
//      expr(exp) match {
//        case ann: BeforeAfterAnnotations =>
//          ann.set_after(convertBlock(block)); ann
//        case _ =>
//          fail(tree, "This expression does not allow for with/then annotations.")
//      }
//    case Expr3("unfolding", exp, "in", inExp) =>
//      create expression(Unfolding, expr(exp), expr(inExp))
//    case Expr4(ite) => expr(ite)
//    case IteExpr0(cond, "?", yes, ":", no) =>
//      create expression(ITE, expr(cond), expr(yes), expr(no))
//    case IteExpr1(impl) => expr(impl)
    case ImplicationExpr0(prop, "==>", concl) =>
      create expression(Implies, expr(prop), expr(concl))
    case ImplicationExpr1(prop, "-*", concl) =>
      create expression(Wand, expr(prop), expr(concl))
    case ImplicationExpr2(andOr) => expr(andOr)
    case AndOrExpr0(p, "&&", q) => create expression(And, expr(p), expr(q))
    case AndOrExpr1(p, "||", q) => create expression(Or, expr(p), expr(q))
    case AndOrExpr2(p, "**", q) => create expression(Star, expr(p), expr(q))
    case AndOrExpr3(eqExp) => expr(eqExp)
//    case EqExpr0(left, "==", right) => create expression(StandardOperator.EQ, expr(left), expr(right))
//    case EqExpr1(left, "!=", right) => create expression(NEQ, expr(left), expr(right))
//    case EqExpr2(relExp) => expr(relExp)
    case RelExpr0(left, "<", right) => create expression(LT, expr(left), expr(right))
    case RelExpr1(left, "<=", right) => create expression(StandardOperator.LTE, expr(left), expr(right))
    case RelExpr2(left, ">=", right) => create expression(StandardOperator.GTE, expr(left), expr(right))
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
    case PowExpr0(left, "^^", right) => ??(tree)
    case PowExpr1(seqAddExp) => expr(seqAddExp)
//    case SeqAddExpr0(x, "::", xs) => create expression(PrependSingle, expr(x), expr(xs))
    case SeqAddExpr1(xs, "++", ys) => create expression(AppendSingle, expr(xs), expr(ys))
    case SeqAddExpr2(xs, "++", "(", a, ",", b, ")") => create expression(MapBuild, expr(xs), expr(a), expr(b))
//    case SeqAddExpr3(unaryExp) => expr(unaryExp)
    case UnaryExpr0("!", exp) => create expression(Not, expr(exp))
    case UnaryExpr1("-", exp) => create expression(UMinus, expr(exp))
    case UnaryExpr2(newExp) => expr(newExp)
    case NewExpr0("new", clsName, args) =>
      create new_object(create class_type(convertID(clsName)), convertExpList(args):_*)

//    case NewExpr1("new", t, dims) =>
//      val baseType = convertType(t)
//      val dimSizes = dims match {
//        case NewDims0(qDims) => qDims.map {
//          case QuantifiedDim0(_, size, _) => expr(size)
//        }.toArray
//      }
//      var arrayType = addDims(baseType, dimSizes.size)
//      create expression(NewArray, arrayType, dimSizes)
//    case NewExpr2(nonTarget) => expr(nonTarget)
//    case NewExpr3(target) => expr(target)

    case Target0(target, ".", prop) =>
      create dereference(expr(target), convertID(prop))
    case Target1(seq, "[", idx, "]") =>
      create expression(Subscript, expr(seq), expr(idx))
    case Target2(nonTarget, ".", prop) =>
      create dereference(expr(nonTarget), convertID(prop))
    case Target3(seq, "[", idx, "]") =>
      create expression(Subscript, expr(seq), expr(idx))
//    case Target4(TargetUnit0(id)) => convertIDName(id)

    case NonTarget0(nonTarget, ".", prop) =>
      create dereference(expr(nonTarget), convertID(prop))
    case NonTarget1(obj, args) =>
      expr(obj) match {
        case name: NameExpression =>
          create invokation(null, null, name.getName, convertExpList(args):_*)
        case deref: Dereference =>
          create invokation(deref.obj, null, deref.field, convertExpList(args):_*)
        case _ =>
          fail(obj, "Cannot apply method invokation to this expression.")
      }
//    case NonTarget2(seq, "[", "..", to, "]") =>
//      create expression(Take, expr(seq), expr(to))
//    case NonTarget3(seq, "[", idx, "]") =>
//      create expression(Subscript, expr(seq), expr(idx))
//    case NonTarget4(seq, "[", fr, "..", Some(to), "]") =>
//      create expression(Slice, expr(seq), expr(fr), expr(to))
//    case NonTarget4(seq, "[", fr, "..", None, "]") =>
//      create expression(Drop, expr(seq), expr(fr))
//    case NonTarget5(seq, "[", replIdx, "->", replVal, "]") =>
//      create expression(SeqUpdate, expr(seq), expr(replIdx), expr(replVal))
//    case NonTarget6(objNode, "->", method, args) =>
//      val obj = expr(objNode)
//      create expression(Implies,
//        create expression(NEQ, obj, create reserved_name(Null)),
//        create invokation(obj, null, convertID(method), convertExpList(args):_*))
//    case NonTarget7(unit) => expr(unit)

//    case NonTargetUnit0(valPrimary) => valExpr(valPrimary)
//    case NonTargetUnit1("this") => create reserved_name This
//    case NonTargetUnit2("null") => create reserved_name Null
//    case NonTargetUnit3("true") => create constant true
//    case NonTargetUnit4("false") => create constant false
//    case NonTargetUnit5("current_thread") => create reserved_name CurrentThread
//    case NonTargetUnit6("\\result") => create reserved_name Result
//    case NonTargetUnit7(collection) => expr(collection)
//    case NonTargetUnit8("map", "<", t1, ",", t2, ">", mapValues) =>
//      create struct_value(create primitive_type(PrimitiveSort.Map, convertType(t1), convertType(t2)), null, convertMapValues(mapValues):_*)
//    case NonTargetUnit9("tuple", "<", t1, ",", t2, ">", values) =>
//      create struct_value(create primitive_type(PrimitiveSort.Tuple, convertType(t1), convertType(t2)), null, convertExpList(values):_*)
//    case NonTargetUnit10(method, argsTuple) =>
//      val args = convertExpList(argsTuple)
//      val methodName = method match { case BuiltinMethod0(name) => name }
//      methodName match {
//        case "Value" => create expression(Value, args:_*)
//        case "HPerm" => create expression(HistoryPerm, args:_*)
//        case "Perm" => create expression(Perm, args:_*)
//        case "PointsTo" => create expression(PointsTo, args:_*)
//        case "Hist" => create expression(History, args:_*)
//        case "\\old" => create expression(Old, args:_*)
//        case "?" => create expression(BindOutput, args:_*)
//        case "idle" => create expression(PVLidleToken, args:_*)
//        case "running" => create expression(PVLjoinToken, args:_*)
//        case "head" => create expression(Head, args:_*)
//        case "tail" => create expression(Tail, args:_*)
//        case "held" => create expression(Held, args:_*)
//        case "Some" => create expression(OptionSome, args:_*)
//      }
//    case NonTargetUnit11(_owner, "(", a, ",", b, ",", c, ")") =>
//      create expression(IterationOwner, expr(a), expr(b), expr(c))
//    case NonTargetUnit12("id", "(", exp, ")") => expr(exp)
//    case NonTargetUnit13("|", seq, "|") => create expression(Size, expr(seq))
//    case NonTargetUnit14("?", id) => create expression(BindOutput, convertIDName(id))
//    case NonTargetUnit15(num) => create constant Integer.parseInt(num)
//    case NonTargetUnit16(seq) => ??(tree)
//    case NonTargetUnit17("(", exp, ")") => expr(exp)
//    case NonTargetUnit18(id) => convertIDName(id)
    case DeclInit0("=", exp) => expr(exp)

//    case CollectionConstructors0(container, _, elemType, _, values) =>
//      create struct_value(
//        create primitive_type(container match {
//          case Container0("seq") => PrimitiveSort.Sequence
//          case Container0("set") => PrimitiveSort.Set
//          case Container0("bag") => PrimitiveSort.Bag
//        }, convertType(elemType)),
//        null,
//        convertExpList(values):_*
//      )
    case CollectionConstructors1("[", exps, "]") =>
      create struct_value(create primitive_type(
        PrimitiveSort.Sequence, InferADTTypes.typeVariable
      ), null, convertExpList(exps):_*)
    case CollectionConstructors2("[t:", t, "]") =>
      create struct_value(create primitive_type(
        PrimitiveSort.Sequence, convertType(t)),
        null)
    case CollectionConstructors3("{", exps, "}") =>
      create struct_value(create primitive_type(
        PrimitiveSort.Set, InferADTTypes.typeVariable
      ), null, convertExpList(exps):_*)
    case CollectionConstructors4("{t:", t, "}") =>
      create struct_value(create primitive_type(
        PrimitiveSort.Set, convertType(t)),
        null)
    case CollectionConstructors5("b{", exps, "}") =>
      create struct_value(create primitive_type(
        PrimitiveSort.Bag, InferADTTypes.typeVariable
      ), null, convertExpList(exps):_*)
    case CollectionConstructors6("b{t:", t, "}") =>
      create struct_value(create primitive_type(
        PrimitiveSort.Bag, convertType(t)),
        null)
    case CollectionConstructors7("set", _, elemType, _, _, main, _, selectors, _, guard, _) => {
      create setComp(
        create primitive_type (PrimitiveSort.Set, convertType(elemType)), // Type
        expr(guard), // The guard expression
        expr(main), // The main/resulting expression
        getVarBounds(selectors).asJava, // Selector
        getVariableDecls(selectors).toArray  // Declaration of variables
      )
    }
  })

  def getVariableDecls(ctx: SetCompSelectorsContext): Seq[DeclarationStatement] = ctx match {
    case SetCompSelectors0(t, id) => Seq(create field_decl(convertID( id), convertType(t)))
    case SetCompSelectors1(t, id, "<-", _) => Seq(create field_decl(convertID( id), convertType(t)))
    case SetCompSelectors2(t, id, "<-", _) => Seq(create field_decl(convertID( id), convertType(t)))
    case SetCompSelectors3(t, id, ",", selectors) => create.field_decl(convertID(id), convertType(t)) +: getVariableDecls(selectors)
    case SetCompSelectors4(t, id, "<-", _, ",", selectors) => create.field_decl(convertID(id), convertType(t)) +: getVariableDecls(selectors)
    case SetCompSelectors5(t, id, "<-", _, ",", selectors) => create.field_decl(convertID(id), convertType(t)) +: getVariableDecls(selectors)
  }

  def getVarBounds(ctx: SetCompSelectorsContext): Map[NameExpression, ASTNode] = ctx match {
    case SetCompSelectors0(t, id) => {
      if (convertType(t).isInstanceOf[ClassType]) {
        fail(id, "Variable %s in set comprehension is not bound. All variables with type class must have bounds.", convertID(id))
      }
      Map.empty
    }
    case SetCompSelectors1(t, id, "<-", collectionId) => Map(convertIDName(id) -> convertIDName(collectionId))
    case SetCompSelectors2(t, id, "<-", collection) => Map(convertIDName(id) -> expr(collection))
    case SetCompSelectors3(t, id, ",", selectors) => {
      if (convertType(t).isInstanceOf[ClassType]) {
        fail(id, "Variable %s in set comprehension is not bound. All variables with type class must have bounds.", convertID(id))
      }
      getVarBounds(selectors)
    }
    case SetCompSelectors4(t, id, "<-", collectionId, ",", selectors) => Map(convertIDName(id) -> convertIDName(collectionId)) ++ getVarBounds(selectors)
    case SetCompSelectors5(t, id, "<-", collection, ",", selectors) => Map(convertIDName(id) -> expr(collection)) ++ getVarBounds(selectors)
  }


  def addDims(t: Type, dimCount: Int): Type = {
    if (dimCount == 0) {
      t
    } else {
      var result = create.primitive_type(PrimitiveSort.Cell, t)
      for(i <- 0 until dimCount) {
        result = create.primitive_type(PrimitiveSort.Array, result)
      }
      create.primitive_type(PrimitiveSort.Option, result)
    }
  }

  def convertType(t: ParserRuleContext): Type = origin(t, t match {
    case LangType0(t) => convertType(t)

    case Type0(t, dims) =>
      val dimCount = dims match {
        case TypeDims0(dims) => dims.size
        case TypeDims1(dims) => dims.size
      }
      var result = addDims(convertType(t), dimCount)
      result

    case NonArrayType0(container, "<", innerType, ">") =>
      val kind = container match {
        case Container0("seq") => PrimitiveSort.Sequence
        case Container0("set") => PrimitiveSort.Set
        case Container0("bag") => PrimitiveSort.Bag
      }
      create.primitive_type(kind, convertType(innerType))
    case NonArrayType1("option", "<", t, ">") =>
      create.primitive_type(PrimitiveSort.Option, convertType(t))
    case NonArrayType2("map", "<", t1, ",", t2, ">") =>
      create.primitive_type(PrimitiveSort.Map, convertType(t1), convertType(t2))
    case NonArrayType2("tuple", "<", t1, ",", t2, ">") =>
      create.primitive_type(PrimitiveSort.Tuple, convertType(t1), convertType(t2))
    case NonArrayType3(primitive) =>
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
//    case NonArrayType4(ClassType0(name, maybeTypeArgs)) =>
//      create class_type(convertID(name), (maybeTypeArgs match {
//        case None => Seq()
//        case Some(TypeArgs0(_, args, _)) =>
//          convertExpList(args)
//      }).asJava)
  })

  def convertInvariant(inv: InvariantContext): (ContractBuilder => Unit) = (builder: ContractBuilder) => inv match {
    case Invariant0(_loop_invariant, exp, _) =>
      builder.appendInvariant(expr(exp))
  }

  def convertBlock(block: ParserRuleContext): BlockStatement = block match {
    case Block0(_, statements, _) =>
      create block(statements.flatMap(convertStat):_*)
  }

  def convertStat(stat: ParserRuleContext): Seq[ASTNode] = origin(stat, Seq[ASTNode](stat match {
    case Statement0("return", None, _) => create.return_statement()
    case Statement0("return", Some(value), _) => create.return_statement(expr(value))
    case Statement1("lock", exp, _) => create special(ASTSpecial.Kind.Lock, expr(exp))
    case Statement2("unlock", exp, _) => create special(ASTSpecial.Kind.Unlock, expr(exp))
    case Statement3("wait", exp, _) => create special(ASTSpecial.Kind.Wait, expr(exp))
    case Statement4("notify", exp, _) => create special(ASTSpecial.Kind.Notify, expr(exp))
    case Statement5("fork", exp, _) => create special(ASTSpecial.Kind.Fork, expr(exp))
    case Statement6("join", exp, _) => create special(ASTSpecial.Kind.Join, expr(exp))
    case Statement7("action", tup, blockNode) =>
      val args = convertExpList(tup)
      val argsOK = args.size >= 4 && args.size % 2 == 0
      if (!argsOK) {
        fail(tup, "action takes four arguments plus any number of pairs, but %d arguments were supplied.", Int.box(args.size))
      }
      val nameMap =
        (4 until args.size by 2).map(i => {
          args(i).asInstanceOf[NameExpression].getName -> args(i+1)
        }).toMap
      val block = convertBlock(blockNode)
      create action_block(args(0), args(1), args(2), args(3), nameMap.asJava, block)
    case Statement8(valStat) => convertValStat(valStat)
//    case Statement9("if", "(", cond, ")", thenStat, maybeElseStat) =>
//      create ifthenelse(expr(cond), flattenIfSingleStatement(convertStat(thenStat)), maybeElseStat.map(convertStat).map(flattenIfSingleStatement).orNull)
//    case ElseBlock0("else", stat) => flattenIfSingleStatement(convertStat(stat))
//    case Statement10("barrier", "(", name, maybeTags, ")", bodyNode) =>
//      val tags = maybeTags match {
//        case Some(BarrierTags0(_, tags)) => convertIDList(tags)
//        case None => Seq()
//      }
//      val (maybeBody, contract) = bodyNode match {
//        case BarrierBody0("{", contract, "}") =>
//          (None, convertContract(contract))
//        case BarrierBody1(contract, body) =>
//          (Some(convertBlock(body)), convertContract(contract))
//      }
//      val tagsJavaList = new JavaArrayList[String](tags.asJava)
//      create barrier(convertID(name), contract, tagsJavaList, maybeBody.orNull)
//    case Statement11(contract, "par", parUnitList) =>
//      val parUnits = convertParUnitList(parUnitList)
//      val javaParUnits = new JavaArrayList[ParallelBlock](parUnits.asJava)
//      create region(convertContract(contract), javaParUnits)
//    case Statement12("vec", "(", iter, ")", block) =>
//      create vector_block(convertParIter(iter), convertBlock(block))
//    case Statement13("invariant", label, "(", resource, ")", block) =>
//      create invariant_block(convertID(label), expr(resource), convertBlock(block))
//    case Statement14("atomic", "(", invariants, ")", block) =>
//      create parallel_atomic(convertBlock(block), convertIDList(invariants):_*)
//    case Statement15(invariants, "while", "(", cond, ")", body) =>
//      create while_loop(expr(cond), flattenIfSingleStatement(convertStat(body)), convertContract(invariants))
//    case Statement16(invariants, "for", "(", maybeInit, ";", maybeCond, ";", maybeUpdate, ")", body) =>
//      create for_loop(
//        maybeInit.map(convertStatList).map(create block(_:_*)).orNull,
//        maybeCond.map(expr).getOrElse(create constant true),
//        maybeUpdate.map(convertStatList).map(create block(_:_*)).orNull,
//        flattenIfSingleStatement(convertStat(body)),
//        convertContract(invariants)
//      )
//    case Statement17(block) => convertBlock(block)
//    case Statement18("{*", exp, "*}") =>
//      create special(ASTSpecial.Kind.HoarePredicate, expr(exp))
//    case Statement19("goto", label, _) =>
//      create special(ASTSpecial.Kind.Goto, convertIDName(label))
//    case Statement20("label", label, _) =>
//      create special(ASTSpecial.Kind.Label, convertIDName(label))
//    case Statement21(stat, _) => flattenIfSingleStatement(convertStat(stat))
    case AllowedForStatement0(tNode, decls) =>
      val t = convertType(tNode)
      val result = new VariableDeclaration(t)
      convertDeclList(decls).foreach{
        case (name, init) =>
          result.add(DeclarationStatement(name, VariableDeclaration.common_type, init))
      }
      result
    case AllowedForStatement1(exp) => expr(exp)
    case AllowedForStatement2(id, "++") => create expression(PostIncr, convertIDName(id))
    case AllowedForStatement2(id, "--") => create expression(PostDecr, convertIDName(id))
    case AllowedForStatement3(target, "=", exp) =>
      create assignment(expr(target), expr(exp))
  }))

  def convertStatList(tree: ForStatementListContext): Seq[ASTNode] = tree match {
    case ForStatementList0(x) => convertStat(x)
    case ForStatementList1(x, ",", xs) => convertStat(x) ++ convertStatList(xs)
  }

  def convertParUnitList(tree: ParUnitListContext): Seq[ParallelBlock] = ???
//    tree match {
//    case ParUnitList0(x) => Seq(convertParUnit(x))
//    case ParUnitList1(x, "and", xs) => convertParUnit(x) +: convertParUnitList(xs)
//  }

  def convertParUnit(tree: ParUnitContext): ParallelBlock = ???
//    origin(tree, tree match {
//    case ParUnit0(maybeLabel, _, maybeIters, maybeWaitList, _, contract, block) =>
//      val label = maybeLabel.map(convertID).getOrElse("")
//      val iters = maybeIters.map(convertParIters).getOrElse(Seq())
//      val waitList = maybeWaitList.map(convertParWaitList).getOrElse(Seq())
//      create parallel_block(label, convertContract(contract), iters.toArray, convertBlock(block), waitList.toArray[ASTNode])
//    case ParUnit1(contract, block) =>
//      create parallel_block("", convertContract(contract), Array(), convertBlock(block))
//  })

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
    case IdArg0(id) => convertIDName(id)
    case IdArg1("*") => create reserved_name(Any)
  }

  /* === Start of duplicated code block ===
   * Below here are the conversion methods for specification constructs. Because they are generated via a language-
   * specific parser, each language has a different set of classes for the ANTLR nodes of specifications. They are
   * however named identically, so we choose to keep this block of code textually the same across the different
   * languages.
   *
   * If you change anything here, please propagate the change to:
   *  - PVLtoCOL.scala
   *  - JavaJMLtoCOL.scala
   *  - CMLtoCOL.scala
   */
  def convertValExpList(args: ValExpressionListContext): Seq[ASTNode] = args match {
    case ValExpressionList0(exp) =>
      Seq(expr(exp))
    case ValExpressionList1(exp, ",", expList) =>
      expr(exp) +: convertValExpList(expList)
  }

  def convertValClause(clause: ValContractClauseContext) = (builder: ContractBuilder) => clause match {
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
    case ValContractClause8(_loop_invariant, exp, _) =>
      builder.appendInvariant(expr(exp))
    case ValContractClause9(_kernel_invariant, exp, _) =>
      builder.appendKernelInvariant(expr(exp))
    case ValContractClause10(_signals, _, signalsType, name, _, condition, _) =>
      builder.signals(origin(clause, new SignalsClause(convertID(name), convertType(signalsType), expr(condition))))
  }

  def convertValBlock(block: ValBlockContext): BlockStatement = origin(block, block match {
    case ValBlock0("{", statements, "}") =>
      create block(statements.map(convertValStat):_*)
  })

  def convertValStat(stat: ValEmbedStatementBlockContext): Seq[ASTNode] = origin(stat, stat match {
    case ValEmbedStatementBlock0(_startSpec, stats, _endSpec) =>
      stats.map(convertValStat)
    case ValEmbedStatementBlock1(stats) =>
      stats.map(convertValStat)
  })

  def convertValStat(stat: LangStatementContext): Seq[ASTNode] = origin(stat, stat match {
    case LangStatement0(stat) =>
      convertStat(stat)
  })

  def convertValStat(stat: ValStatementContext): ASTNode = origin(stat, stat match {
    case ValStatement0(_create, block) =>
      create lemma(convertValBlock(block))
    case ValStatement1(_qed, exp, _) =>
      create special(ASTSpecial.Kind.QED, expr(exp))
    case ValStatement2(_apply, exp, _) =>
      create special(ASTSpecial.Kind.Apply, expr(exp))
    case ValStatement3(_use, exp, _) =>
      create special(ASTSpecial.Kind.Use, expr(exp))
    case ValStatement4(_create, hist, _) =>
      create special(ASTSpecial.Kind.CreateHistory, expr(hist))
    case ValStatement5(_create, fut, _, proc, _) =>
      create special(ASTSpecial.Kind.CreateFuture, expr(fut), expr(proc))
    case ValStatement6(_destroy, hist, _, proc, _) =>
      create special(ASTSpecial.Kind.DestroyHistory, expr(hist), expr(proc))
    case ValStatement7(_destroy, fut, _) =>
      create special(ASTSpecial.Kind.DestroyFuture, expr(fut))
    case ValStatement8(_split, fut, _, perm1, _, proc1, _, perm2, _, proc2, _) =>
      create special(ASTSpecial.Kind.SplitHistory, expr(fut), expr(perm1), expr(proc1), expr(perm2), expr(proc2))
    case ValStatement9(_merge, fut, _, perm1, _, proc1, _, perm2, _, proc2, _) =>
      create special(ASTSpecial.Kind.MergeHistory, expr(fut), expr(perm1), expr(proc1), expr(perm2), expr(proc2))
    case ValStatement10(_choose, fut, _, perm, _, proc1, _, proc2, _) =>
      create special(ASTSpecial.Kind.ChooseHistory, expr(fut), expr(perm), expr(proc1), expr(proc2))
    case ValStatement11(_fold, pred, _) =>
      create special(ASTSpecial.Kind.Fold, expr(pred))
    case ValStatement12(_unfold, pred, _) =>
      create special(ASTSpecial.Kind.Unfold, expr(pred))
    case ValStatement13(_open, pred, _) =>
      create special(ASTSpecial.Kind.Open, expr(pred))
    case ValStatement14(_close, pred, _) =>
      create special(ASTSpecial.Kind.Close, expr(pred))
    case ValStatement15(_assert, assn, _) =>
      create special(ASTSpecial.Kind.Assert, expr(assn))
    case ValStatement16(_assume, assn, _) =>
      create special(ASTSpecial.Kind.Assume, expr(assn))
    case ValStatement17(_inhale, res, _) =>
      create special(ASTSpecial.Kind.Inhale, expr(res))
    case ValStatement18(_exhale, res, _) =>
      create special(ASTSpecial.Kind.Exhale, expr(res))
    case ValStatement19(_label, lbl, _) =>
      create special(ASTSpecial.Kind.Label, convertIDName(lbl))
    case ValStatement20(_refute, assn, _) =>
      create special(ASTSpecial.Kind.Refute, expr(assn))
    case ValStatement21(_witness, pred, _) =>
      create special(ASTSpecial.Kind.Witness, expr(pred))
    case ValStatement22(_ghost, code) =>
      flattenIfSingleStatement(convertValStat(code))
    case ValStatement23(_send, res, _to, lbl, _, thing, _) =>
      create special(ASTSpecial.Kind.Send, expr(res), convertIDName(lbl), expr(thing))
    case ValStatement24(_recv, res, _from, lbl, _, thing, _) =>
      create special(ASTSpecial.Kind.Recv, expr(res), convertIDName(lbl), expr(thing))
    case ValStatement25(_transfer, exp, _) =>
      ??(stat)
    case ValStatement26(_csl_subject, obj, _) =>
      create special(ASTSpecial.Kind.CSLSubject, expr(obj))
    case ValStatement27(_spec_ignore, "}") =>
      create special ASTSpecial.Kind.SpecIgnoreEnd
    case ValStatement28(_spec_ignore, "{") =>
      create special ASTSpecial.Kind.SpecIgnoreStart
    case ValStatement29(_action, arg1, _, arg2, _, arg3, _, arg4, map, _) =>
      if(map.nonEmpty) {
        ??(map.head)
      }
      create special (ASTSpecial.Kind.ActionHeader, expr(arg1), expr(arg2), expr(arg3), expr(arg4))
    case ValStatement30(_atomic, _, resList, _, stat) =>
      create csl_atomic(create block(convertValStat(stat):_*), resList.map(convertValExpList).getOrElse(Seq()).map {
        case name: NameExpression if name.getKind == NameExpressionKind.Unresolved =>
          create label name.getName
        case other => other
      }:_*)
  })

  def valExpr(exp: ValPrimaryContext): ASTNode = origin(exp, exp match {
    case ValPrimary0(t, "{", maybeExps, "}") =>
      val exps = maybeExps.map(convertValExpList).getOrElse(Seq())
      create struct_value(convertType(t), null, exps:_*)
    case ValPrimary1("[", factor, "]", exp) =>
      create expression(Scale, expr(factor), expr(exp))
    case ValPrimary2("|", seq, "|") =>
      create expression(Size, expr(seq))
    case ValPrimary3("\\unfolding", pred, "\\in", exp) =>
      create expression(Unfolding, expr(pred), expr(exp))
    case ValPrimary4("(", exp, "!", indepOf, ")") =>
      create expression(IndependentOf, expr(exp), convertIDName(indepOf))
    case ValPrimary5("(", x, "\\memberof", xs, ")") =>
      create expression(Member, expr(x), expr(xs))
    case ValPrimary6("{", from, "..", to, "}") =>
      create expression(RangeSeq, expr(from), expr(to))
    case ValPrimary7("*") =>
      create reserved_name ASTReserved.Any
    case ValPrimary8("\\current_thread") =>
      create reserved_name ASTReserved.CurrentThread
    case ValPrimary9(_, binderName, t, id, "=", fr, "..", to, _, main, _) =>
      val name = convertID(id)
      val decl = create field_decl(name, convertType(t))
      val guard = create expression(StandardOperator.And,
        create expression(StandardOperator.LTE, expr(fr), create unresolved_name(name)),
        create expression(StandardOperator.LT, create unresolved_name(name), expr(to))
      )
      binderName match {
        case "\\forall*" => create starall(guard, expr(main), decl)
        case "\\forall" => create forall(guard, expr(main), decl)
        case "\\exists" => create exists(guard, expr(main), decl)
      }
    case ValPrimary10(_, binderName, t, id, _, guard, _, main, _) =>
      val decl = create field_decl(convertID(id), convertType(t))
      binderName match {
        case "\\forall*" => create starall(expr(guard), expr(main), decl)
        case "\\forall" => create forall(expr(guard), expr(main), decl)
        case "\\exists" => create exists(expr(guard), expr(main), decl)
      }
    case ValPrimary11(_, "\\let", t, id, "=", exp, _, body, _) =>
      create let_expr(create field_decl(convertID(id), convertType(t), expr(exp)), expr(body))
    case ValPrimary12(_, "\\sum", t, id, _, guard, _, main, _) =>
      create summation(expr(guard), expr(main), create field_decl(convertID(id), convertType(t)))
    case ValPrimary13("\\length", "(", exp, ")") =>
      create expression(Length, expr(exp))
    case ValPrimary14("\\old", "(", exp, ")") =>
      create expression(Old, expr(exp))
    case ValPrimary15("\\typeof", "(", exp, ")") =>
      create expression(TypeOf, expr(exp))
    case ValPrimary16("\\matrix", "(", m, _, size0, _, size1, ")") =>
      create expression(ValidMatrix, expr(m), expr(size0), expr(size1))
    case ValPrimary17("\\array", "(", a, _, size0, ")") =>
      create expression(ValidArray, expr(a), expr(size0))
    case ValPrimary18("\\pointer", "(", p, _, size0, _, perm, ")") =>
      create expression(ValidPointer, expr(p), expr(size0), expr(perm))
    case ValPrimary19("\\pointer_index", "(", p, _, idx, _, perm, ")") =>
      create expression(ValidPointerIndex, expr(p), expr(idx), expr(perm))
    case ValPrimary20("\\values", "(", a, _, fr, _, to, ")") =>
      create expression(Values, expr(a), expr(fr), expr(to))
    case ValPrimary21("\\sum", "(", a, _, b, ")") =>
      create expression(FoldPlus, expr(a), expr(b))
    case ValPrimary22("\\vcmp", "(", a, _, b, ")") =>
      create expression(VectorCompare, expr(a), expr(b))
    case ValPrimary23("\\vrep", "(", v, ")") =>
      create expression(VectorRepeat, expr(v))
    case ValPrimary24("\\msum", "(", a, _, b, ")") =>
      create expression(MatrixSum, expr(a), expr(b))
    case ValPrimary25("\\mcmp", "(", a, _, b, ")") =>
      create expression(MatrixCompare, expr(a), expr(b))
    case ValPrimary26("\\mrep", "(", m, ")") =>
      create expression(MatrixRepeat, expr(m))
    case ValPrimary27(label, _, exp) =>
      val res = expr(exp)
      res.addLabel(create label(convertID(label)))
      res
    case ValPrimary28("{:", pattern, ":}") =>
      create pattern expr(pattern)
//    case ValPrimary29("Reducible", "(", exp, _, opNode, ")") =>
//      val opText = opNode match {
//        case ValReducibleOperator0("+") => "+"
//        case ValReducibleOperator1(id) => convertID(id)
//      }
//      create expression(opText match {
//        case "+" => ReducibleSum
//        case "min" => ReducibleMin
//        case "max" => ReducibleMax
//      }, expr(exp))
//    case ValPrimary30("AbstractState", _, arg1, _, arg2, _) =>
//      create expression(StandardOperator.AbstractState, expr(arg1), expr(arg2))
//    case ValPrimary31("AddsTo", _, arg1, _, arg2, _) =>
//      create expression(StandardOperator.AddsTo, expr(arg1), expr(arg2))
//    case ValPrimary32("APerm", _, loc, _, perm, _) =>
//      create expression(StandardOperator.ActionPerm, expr(loc), expr(perm))
//    case ValPrimary33("ArrayPerm", _, ar, _, fst, _, step, _, cnt, _, perm, _) =>
//      create expression(StandardOperator.ArrayPerm, expr(ar), expr(fst), expr(step), expr(cnt), expr(perm))
//    case ValPrimary34("buildMap", _, map, _, k, _, v, _) =>
//      create expression(StandardOperator.MapBuild, expr(map), expr(k), expr(v))
//    case ValPrimary35("cardMap", _, map, _) =>
//      create expression(StandardOperator.MapCardinality, expr(map))
//    case ValPrimary36("Contribution", _, res, _, con, _) =>
//      create expression(StandardOperator.Contribution, expr(res), expr(con))
//    case ValPrimary37("disjointMap", _, map1, _, map2, _) =>
//      create expression(StandardOperator.MapDisjoint, expr(map1), expr(map2))
//    case ValPrimary38("equalsMap", _, map1, _, map2, _) =>
//      create expression(StandardOperator.MapEquality, expr(map1), expr(map2))
//    case ValPrimary39("Future", _, arg1, _, arg2, _, arg3, _) =>
//      create expression(StandardOperator.Future, expr(arg1), expr(arg2), expr(arg3))
//    case ValPrimary40("getFromMap", _, map, _, k, _) =>
//      create expression(StandardOperator.MapGetByKey, expr(map), expr(k))
//    case ValPrimary41("getFst", _, tup, _) =>
//      create expression(StandardOperator.TupleFst, expr(tup))
//    case ValPrimary42("getOption", _, opt, _) =>
//      create expression(StandardOperator.OptionGet, expr(opt))
//    case ValPrimary43("getSnd", _, tup, _) =>
//      create expression(StandardOperator.TupleSnd, expr(tup))
//    case ValPrimary44("head", _, seq, _) =>
//      create expression(StandardOperator.Head, expr(seq))
//    case ValPrimary45("held", _, lock, _) =>
//      create expression(StandardOperator.Held, expr(lock))
//    case ValPrimary46("Hist", _, arg1, _, arg2, _, arg3, _) =>
//      create expression(StandardOperator.History, expr(arg1), expr(arg2), expr(arg3))
//    case ValPrimary47("HPerm", _, loc, _, perm, _) =>
//      create expression(StandardOperator.HistoryPerm, expr(loc), expr(perm))
//    case ValPrimary48("idle", _, arg, _) =>
//      create expression(StandardOperator.PVLidleToken, expr(arg))
//    case ValPrimary49("isEmpty", _, seq, _) =>
//      create expression(StandardOperator.Empty, expr(seq))
//    case ValPrimary50("itemsMap", _, map, _) =>
//      create expression(StandardOperator.MapItemSet, expr(map))
//    case ValPrimary51("keysMap", _, map, _) =>
//      create expression(StandardOperator.MapKeySet, expr(map))
//    case ValPrimary52("perm", _, loc, _) =>
//      create expression(StandardOperator.CurrentPerm, expr(loc))
//    case ValPrimary53("Perm", _, loc, _, perm, _) =>
//      create expression(StandardOperator.Perm, expr(loc), expr(perm))
//    case ValPrimary54("PointsTo", _, loc, _, perm, _, value, _) =>
//      create expression(StandardOperator.PointsTo, expr(loc), expr(perm), expr(value))
//    case ValPrimary55(_removeAt, _, seq, _, i, _) =>
//      create expression(StandardOperator.RemoveAt, expr(seq), expr(i))
//    case ValPrimary56("removeFromMap", _, map, _, arg, _) =>
//      create expression(StandardOperator.MapRemoveKey, expr(map), expr(arg))
//    case ValPrimary57("running", _, arg, _) =>
//      create expression(StandardOperator.PVLjoinToken, expr(arg))
//    case ValPrimary58("Some", _, arg, _) =>
//      create expression(StandardOperator.OptionSome, expr(arg))
//    case ValPrimary59("tail", _, seq, _) =>
//      create expression(StandardOperator.Tail, expr(seq))
//    case ValPrimary60("Value", _, arg, _) =>
//      create expression(StandardOperator.Value, expr(arg))
//    case ValPrimary61("valuesMap", _, map, _) =>
//      create expression(StandardOperator.MapValueSet, expr(map))
//    case ValPrimary62("seq", "<", t, ">", "{", elems, "}") =>
//      create struct_value(create.primitive_type(PrimitiveSort.Sequence, convertType(t)), null, convertValExpList(elems):_*)
//    case ValPrimary63("set", "<", t, ">", "{", elems, "}") =>
//      create struct_value(create.primitive_type(PrimitiveSort.Set, convertType(t)), null, convertValExpList(elems):_*)
//    case ValPrimary64("(", seq, "[", "..", end, "]", ")") =>
//      create expression(Take, expr(seq), expr(end))
//    case ValPrimary65("(", seq, "[", start, "..", None, "]", ")") =>
//      create expression(Drop, expr(seq), expr(start))
//    case ValPrimary65("(", seq, "[", start, "..", Some(end), "]", ")") =>
//      create expression(Slice, expr(seq), expr(start), expr(end))
//    case ValPrimary66("(", seq, "[", idx, "->", replacement, "]", ")") =>
//      create expression(SeqUpdate, expr(seq), expr(idx), expr(replacement))
//    case ValPrimary67("(", x, "::", xs, ")") =>
//      create expression(PrependSingle, expr(x), expr(xs))
//    case ValPrimary68("(", xs, "++", ys, ")") =>
//      create expression(Concat, expr(xs), expr(ys))
//    case ValPrimary69("(", x, "\\in", xs, ")") =>
//      create expression(Member, expr(x), expr(xs))
//    case ValPrimary70("getOrElseOption", "(", opt, ",", alt, ")") =>
//      create expression(OptionGetOrElse, expr(opt), expr(alt))
  })

  def convertValOp(op: ValImpOpContext): StandardOperator = op match {
    case ValImpOp0("-*") => StandardOperator.Wand
    case ValImpOp1("==>") => StandardOperator.Implies
  }

  def convertValOp(op: ValAndOpContext): StandardOperator = op match {
    case ValAndOp0("**") => StandardOperator.Star
  }

  def convertValOp(op: ValMulOpContext): StandardOperator = op match {
    case ValMulOp0("\\") => StandardOperator.Div
  }

  def convertValReserved(reserved: ValReservedContext): NameExpression = origin(reserved, reserved match {
    case ValReserved0(_) =>
      fail(reserved, "This identifier is reserved and cannot be declared or used.")
    case ValReserved1(s) =>
      create unresolved_name(s.substring(1, s.length-1))
    case ValReserved2("\\result") =>
      create reserved_name ASTReserved.Result
    case ValReserved3("\\current_thread") =>
      create reserved_name ASTReserved.CurrentThread
    case ValReserved4("none") =>
      create reserved_name ASTReserved.NoPerm
    case ValReserved5("write") =>
      create reserved_name ASTReserved.FullPerm
    case ValReserved6("read") =>
      create reserved_name ASTReserved.ReadPerm
    case ValReserved7("None") =>
      create reserved_name ASTReserved.OptionNone
    case ValReserved8("empty") =>
      create reserved_name ASTReserved.EmptyProcess
    case ValReserved9("\\ltid") =>
      create reserved_name ASTReserved.LocalThreadId
    case ValReserved10("\\gtid") =>
      create reserved_name ASTReserved.GlobalThreadId
    case ValReserved11("true") =>
      ??(reserved)
    case ValReserved12("false") =>
      ??(reserved)
  })

  /**
   * This method allows a language grammar to step into the reserved identifiers where they overlap with the underlying
   * language, to allow their use there. They should be forbidden inside specifications.
   * @param reserved the reserved identifier
   * @return the string representation of the identifier
   */
  def convertOverlappingValReservedID(reserved: ValReservedContext): String = reserved match {
    case ValReserved0(s) => s
    case ValReserved1(s) => fail(reserved, "This identifier is invalid in the current language")
    case ValReserved2("\\result") => fail(reserved, "This identifier is invalid in the current language")
    case ValReserved3("\\current_thread") => fail(reserved, "This identifier is invalid in the current language")
    case ValReserved4(s) => s
    case ValReserved5(s) => s
    case ValReserved6(s) => s
    case ValReserved7(s) => s
    case ValReserved8(s) => s
    case ValReserved9("\\ltid") => fail(reserved, "This identifier is invalid in the current language")
    case ValReserved10("\\gtid") => fail(reserved, "This identifier is invalid in the current language")
    case ValReserved11(s) => s
    case ValReserved12(s) => s
  }

  def convertOverlappingValReservedName(reserved: ValReservedContext): NameExpression =
    create unresolved_name convertOverlappingValReservedID(reserved)

  def convertValContract(contract: Option[ValEmbedContractContext]) = (builder: ContractBuilder) => contract match {
    case Some(ValEmbedContract0(blocks)) =>
      for(block <- blocks) {
        convertValContractBlock(block)(builder)
      }
    case None =>
    // nop
  }

  def convertValContractBlock(contract: ValEmbedContractBlockContext) = (builder: ContractBuilder) => contract match {
    case ValEmbedContractBlock0(_startSpec, clauses, _endSpec) =>
      for(clause <- clauses) {
        convertValClause(clause)(builder)
      }
    case ValEmbedContractBlock1(clauses) =>
      for(clause <- clauses) {
        convertValClause(clause)(builder)
      }
  }

  def convertValType(t: ValTypeContext): Type = origin(t, t match {
    case ValType0(s) => s match {
      case "resource" => create primitive_type(PrimitiveSort.Resource)
      case "process" => create primitive_type(PrimitiveSort.Process)
      case "frac" => create primitive_type PrimitiveSort.Fraction
      case "zfrac" => create primitive_type PrimitiveSort.ZFraction
      case "rational" => create primitive_type PrimitiveSort.Rational
      case "bool" => create primitive_type PrimitiveSort.Boolean
    }
    case ValType1("seq", _, subType, _) =>
      create primitive_type(PrimitiveSort.Sequence, convertType(subType))
    case ValType2("set", _, subType, _) =>
      create primitive_type(PrimitiveSort.Set, convertType(subType))
    case ValType3("bag", _, subType, _) =>
      create primitive_type(PrimitiveSort.Bag, convertType(subType))
    case ValType4("loc", _, subType, _) =>
      create primitive_type(PrimitiveSort.Location, convertType(subType))
    case ValType5("pointer", _, subType, _) =>
      create primitive_type(PrimitiveSort.Pointer, convertType(subType))
  })

  def convertValArg(arg: ValArgContext): DeclarationStatement = origin(arg, arg match {
    case ValArg0(t, id) =>
      create field_decl(convertID(id), convertType(t))
  })

  def convertValArgList(argList: ValArgListContext): Seq[DeclarationStatement] = origin(argList, argList match {
    case ValArgList0(arg) => Seq(convertValArg(arg))
    case ValArgList1(arg, _, args) => convertValArg(arg) +: convertValArgList(args)
  })

  def convertValModifier(modifier: ValModifierContext): NameExpression = origin(modifier, modifier match {
    case ValModifier0(s) => s match {
      case "pure" => create reserved_name(ASTReserved.Pure)
      case "inline" => create reserved_name(ASTReserved.Inline)
      case "thread_local" => create reserved_name(ASTReserved.ThreadLocal)
    }
    case ValModifier1(langMod) => convertModifier(langMod)
  })

  def convertValModifiers(modifiers: ValEmbedModifiersContext): Seq[NameExpression] = origin(modifiers, modifiers match {
    case ValEmbedModifiers0(_, mods, _) =>
      mods.map(convertValModifier)
    case ValEmbedModifiers1(mods) =>
      mods.map(convertValModifier)
  })

  def convertValDecl(decl: ValDeclarationContext): ASTDeclaration = origin(decl, decl match {
    case ValDeclaration0(clauses, mods, t, name, _, args, _, body) =>
      val contract = getContract(clauses.map(convertValClause):_*)
      val func = create function_decl(
        convertType(t),
        contract,
        convertID(name),
        args.map(convertValArgList).getOrElse(Seq()).toArray,
        body match {
          case ValPredicateDef0(_) => null
          case ValPredicateDef1("=", exp, _) => expr(exp)
        }
      )
      mods.foreach(mod => func.attach(convertValModifier(mod)))
      func
    case ValDeclaration1("axiom", name, _, left, "==", right, _) =>
      create axiom(convertID(name), create expression(StandardOperator.EQ, expr(left), expr(right)))
    case ValDeclaration2(clauses, "ghost", langDecl) =>
      val decl = convertDecl(langDecl)
      if(clauses.nonEmpty) {
        decl match {
          case method: Method =>
            method.setContract(getContract(clauses.map(convertValClause):_*))
            method
          case _ =>
            fail(langDecl, "This constructor cannot have contract declarations")
        }
      } else {
        decl
      }
  })

  def convertValDecl(decl: ValEmbedDeclarationBlockContext): Seq[ASTDeclaration] = decl match {
    case ValEmbedDeclarationBlock0(_, decls, _) =>
      decls.map((decl) => convertValDecl(decl))
  }

  def convertValWithThen(withThen: ValWithThenContext): ASTNode = withThen match {
    case ValWithThen0("with", stat) =>
      create special(ASTSpecial.Kind.With, flattenIfSingleStatement(convertValStat(stat)))
    case ValWithThen1("then", stat) =>
      create special(ASTSpecial.Kind.Then, flattenIfSingleStatement(convertValStat(stat)))
  }

  def convertValWithThen(withThen: ValEmbedWithThenBlockContext): Seq[ASTNode] = withThen match {
    case ValEmbedWithThenBlock0(_, mappings, _) => mappings.map(convertValWithThen)
    case ValEmbedWithThenBlock1(mappings) => mappings.map(convertValWithThen)
  }

  def convertValWithThen(withThen: ValEmbedWithThenContext): Seq[ASTNode] = withThen match {
    case ValEmbedWithThen0(blocks) => blocks.flatMap(convertValWithThen)
  }
  /* === End of duplicated code block === */
}
