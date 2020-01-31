package vct.antlr4.parser

import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{CommonToken, CommonTokenStream, ParserRuleContext}
import vct.antlr4.generated.Java7JMLParser
import vct.antlr4.generated.Java7JMLParser._
import vct.antlr4.generated.Java7JMLParserPatterns._
import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, Type}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.{MethodInvokation, NameExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.{ASTClass, ASTDeclaration, DeclarationStatement, NameSpace, ProgramUnit}

object JavaJMLtoCOL {
  def convert(tree: CompilationUnitContext, fileName: String, tokens: CommonTokenStream, parser: Java7JMLParser): ProgramUnit = {
    JavaJMLtoCOL(fileName, tokens, parser).convertUnit(tree)
  }
}

case class JavaJMLtoCOL(fileName: String, tokens: CommonTokenStream, parser: Java7JMLParser)
  extends ToCOL(fileName, tokens, parser) {
  def convertUnit(tree: CompilationUnitContext): ProgramUnit = tree match {
    case CompilationUnit0(maybePackage, imports, decls, _) =>
      val result = new ProgramUnit()
      val namespace = maybePackage match {
        case None => create namespace(NameSpace.NONAME)
        case Some(PackageDeclaration0(Seq(annotation, _*), _, _, _)) =>
          ??? // package declaration annotations are not supported.
        case Some(PackageDeclaration0(Seq(), "package", name, _)) =>
          create namespace(convertQualifiedName(name):_*)
      }
      imports.foreach {
        case ImportDeclaration0("import", static, name, importAll, _) =>
          namespace.add_import(static.isDefined, importAll.isDefined, convertQualifiedName(name):_*)
      }
      decls.foreach(convertDecl(_).foreach(namespace.add(_)))
      result.add(namespace)
      result
  }

  def convertQualifiedName(name: QualifiedNameContext): Seq[String] = name match {
    case QualifiedName0(id) => Seq(convertID(id))
    case QualifiedName1(id, _, ids) => convertID(id) +: convertQualifiedName(ids)
  }

  def convertID(id: JavaIdentifierContext): String = id match {
    case JavaIdentifier0(ExtraIdentifier0(ValReserved0(s))) => s match {
      case _ => ???
    }
    case JavaIdentifier1(s) => s
  }

  def convertDecl(decl: ParserRuleContext): Seq[ASTDeclaration] = decl match {
    case TypeDeclaration0(mods, classDecl) =>
      val cls = convertClass(classDecl)
      mods.map(convertModifier).foreach(cls.addLabel(_))
      Seq(cls)
    case ClassBodyDeclaration0(_) => Seq()
    case ClassBodyDeclaration1(maybeStatic, block) =>
      ???
      // This is a block that is executed on class load if "static" is present, otherwise
      // the code block is executed on every instance creation (prior to constructors, I think)
    case ClassBodyDeclaration2(mods, member) =>
      val decl = convertDecl(member)
      decl.foreach(decl => mods.map(convertModifier).foreach(decl.addLabel))
      decl
    case MemberDeclaration0(fwd) => convertDecl(fwd)
    case MemberDeclaration1(fwd) => convertDecl(fwd)
    case MemberDeclaration2(fwd) => convertDecl(fwd)
    case MemberDeclaration3(fwd) => convertDecl(fwd)
    case MemberDeclaration4(fwd) => convertDecl(fwd)
    case MemberDeclaration5(fwd) => convertDecl(fwd)
    case MemberDeclaration6(fwd) => convertDecl(fwd)
    case MemberDeclaration7(fwd) => convertDecl(fwd)
    case MemberDeclaration8(fwd) => convertDecl(fwd)
    case MemberDeclaration9(fwd) => convertDecl(fwd)
    case MethodDeclaration0(retType, name, paramsNode, maybeDims, maybeBody) =>
      val dims = maybeDims match { case None => 0; case Some(Dims0(dims)) => dims.size }
      val returns = convertType(retType, dims)
      val (params, varargs) = convertParams(paramsNode)
      val body = maybeBody match {
        case MethodBodyOrEmpty0(";") => None
        case MethodBodyOrEmpty1(MethodBody0(block)) => Some(convertBlock(block))
      }
      Seq(create method_decl(returns, null, convertID(name), params.toArray, body.orNull))
    case GenericMethodDeclaration0(typeParams, methodDecl) =>
      ??? //generics are unsupported
    case FieldDeclaration0(t, declarators, _) =>
      for((name, dims, init) <- convertDeclarators(declarators))
        yield create field_decl(name, convertType(t, dims), init.orNull)
    case ExtraDeclaration1(AxiomDeclaration0("axiom", name, "{", left, "==", right, "}")) =>
      Seq(create axiom(name, create expression(EQ, expr(left), expr(right))))
  }

  def convertModifier(modifier: ParserRuleContext): NameExpression = modifier match {
    case Modifier0(mod) => convertModifier(mod)
    case Modifier1(ExtraAnnotation0("pure")) =>
      create reserved_name(ASTReserved.Pure)
    case Modifier1(ExtraAnnotation1("inline")) =>
      create reserved_name(ASTReserved.Inline)
    case Modifier1(ExtraAnnotation2("thread_local")) =>
      create reserved_name(ASTReserved.ThreadLocal)
    case Modifier2(mod) => mod match {
      case "native" => ???
      case "synchronized" => create reserved_name(ASTReserved.Synchronized)
      case "transient" => ???
      case "volatile" => create reserved_name(ASTReserved.Volatile)
    }
    case ClassOrInterfaceModifier0(annotation) =>
      ??? // annotations are unsupported
    case ClassOrInterfaceModifier1(attribute) =>
      create reserved_name(attribute match {
        case "public" => ASTReserved.Public
        case "protected" => ASTReserved.Protected
        case "private" => ASTReserved.Private
        case "static" => ASTReserved.Static
        case "abstract" => ASTReserved.Abstract
        case "final" => ASTReserved.Final
        case "strictfp" =>
          ??? // strict floating point math; unsupported.
      })
  }

  def convertClass(decl: ClassDeclarationContext): ASTClass = decl match {
    case ClassDeclaration0(_, _, Some(_typeParams), _, _, _) =>
      ??? // generics are not supported.
    case ClassDeclaration0("class", name, None, maybeExtends, maybeImplements, ClassBody0(_, decls, _)) =>
      val ext = maybeExtends match {
        case None => Seq()
        case Some(Ext0(_, t)) => Seq(convertType(t) match {
          case t: ClassType => t
          case _ => ??? // The ast does not allow bases that are not of ClassType, but the grammar does
        })
      }
      val imp = maybeImplements match {
        case None => Seq()
        case Some(Imp0(_, typeList)) => convertTypeList(typeList).map {
          case t: ClassType => t
          case _ => ??? // see above
        }
      }
      val cls = create ast_class(convertID(name), ASTClass.ClassKind.Plain, Array(), ext.toArray, imp.toArray)
      decls.map(convertDecl).foreach(_.foreach(cls.add))
      cls
  }

  def convertTypeList(tree: ParserRuleContext): Seq[Type] = tree match {
    case _ => ???
  }

  def convertType(tree: ParserRuleContext, extraDims: Int): Type = {
    var t = convertType(tree)
    for(_ <- 0 until extraDims)
      t = create.primitive_type(PrimitiveSort.Array, t)
    t
  }

  def convertType(tree: ParserRuleContext): Type = tree match {
    case Type0(t, dims) =>
      convertType(t, dims match { case None => 0; case Some(Dims0(dims)) => dims.size })
    case Type1(t, dims) =>
      convertType(t, dims match { case None => 0; case Some(Dims0(dims)) => dims.size })
    case Type2(ExtraType0(name)) =>
      create primitive_type(name match {
        case "resource" => PrimitiveSort.Resource
        case "process" => PrimitiveSort.Process
        case "frac" => PrimitiveSort.Fraction
        case "zfrac" => PrimitiveSort.ZFraction
        case "rational" => PrimitiveSort.Rational
      })

    case PrimitiveType0(name) =>
      create primitive_type(name match {
        case "boolean" => PrimitiveSort.Boolean
        case "char" => PrimitiveSort.Char
        case "byte" => PrimitiveSort.Byte
        case "short" => PrimitiveSort.Short
        case "int" => PrimitiveSort.Integer
        case "long" => PrimitiveSort.Long
        case "float" => PrimitiveSort.Float
        case "double" => PrimitiveSort.Double
      })
  }

  def convertParams(params: ParserRuleContext): (Seq[DeclarationStatement], Boolean) = params match {
    case FormalParameters0(_, None, _) => (Seq(), false)
    case FormalParameters0(_, Some(list), _) => convertParams(list)
    case FormalParameterList0(varargsParam) => (Seq(convertParam(varargsParam)), true)
    case FormalParameterList1(list) => convertParams(list)
    case FormalParameterList2(list, _, varargsParam) => (convertParams(list)._1 :+ convertParam(varargsParam), true)
    case InitFormalParameterList0(param) => (Seq(convertParam(param)), false)
    case InitFormalParameterList1(param, _, list) => (convertParam(param) +: convertParams(list)._1, false)
  }

  def convertParam(param: ParserRuleContext): DeclarationStatement = param match {
    case FormalParameter0(Seq(_, _*), _, _) =>
      ??? // modifiers to method arguments are unsupported
    case FormalParameter0(Seq(), t, declaratorName) =>
      val (name, extraDims) = convertDeclaratorName(declaratorName)
      create field_decl(name, convertType(t, extraDims))
  }

  def convertDeclaratorName(decl: VariableDeclaratorIdContext): (String, Int) = decl match {
    case VariableDeclaratorId0(name, None) => (convertID(name), 0)
    case VariableDeclaratorId0(name, Some(Dims0(dims))) => (convertID(name), dims.size)
  }

  def convertDeclarators(decls: VariableDeclaratorsContext): Seq[(String, Int, Option[ASTNode])] = decls match {
    case VariableDeclarators0(x) => Seq(convertDeclarator(x))
    case VariableDeclarators1(x, ",", xs) => convertDeclarator(x) +: convertDeclarators(xs)
  }

  def convertDeclarator(decl: VariableDeclaratorContext): (String, Int, Option[ASTNode]) = decl match {
    case VariableDeclarator0(declId, maybeInit) =>
      val (name, extraDims) = convertDeclaratorName(declId)
      (name, extraDims, maybeInit.map(expr))
  }

  def convertBlock(block: BlockContext): BlockStatement = ???

  def expr(tree: ParserRuleContext): ASTNode = tree match {
    case Expression0(primary) => expr(primary)
    case Expression1(obj, ".", field) =>
      create dereference(expr(obj), convertID(field))
    case Expression2(obj, ".", "this") => ???
    case Expression3(obj, ".", "new", typeArgs, creator) => ???
    case Expression4(obj, ".", "super", suffix) => ???
    case Expression5(obj, ".", invokation) =>
      expr(invokation) match {
        case call: MethodInvokation if call.`object` == null =>
          create invokation(expr(obj), null, call.method, call.getArgs:_*)
        case _ => ???
      }
    case Expression6(seq, "[", idx, "]") =>
      create expression(Subscript, expr(seq), expr(idx))
    case Expression7(_, _, _, _) => ??? //arrow type
    case _: Expression8Context => ???
    case Expression9("new", creator) => ???
    case Expression10("(", t, ")", exp) => ???
    case Expression11(exp, "++") => create expression(PostIncr, expr(exp))
    case Expression11(exp, "--") => create expression(PostDecr, expr(exp))
    case Expression12("+", exp) => expr(exp)
    case Expression12("-", exp) => create expression(UMinus, expr(exp))
    case Expression12("++", exp) => create expression(PreIncr, expr(exp))
    case Expression12("--", exp) => create expression(PreDecr, expr(exp))
    case Expression13("~", exp) => create expression(BitNot, expr(exp))
    case Expression13("!", exp) => create expression(Not, expr(exp))
    case Expression14(left, "*", right) => create expression(Mult, expr(left), expr(right))
    case Expression14(left, "/", right) => create expression(FloorDiv, expr(left), expr(right))
    case Expression14(left, "\\", right) => create expression(Div, expr(left), expr(right))
    case Expression14(left, "%", right) => create expression(Mod, expr(left), expr(right))
    case Expression15(left, "+", right) => create expression(Plus, expr(left), expr(right))
    case Expression15(left, "-", right) => create expression(Minus, expr(left), expr(right))
    case shiftExpr: Expression16Context =>
      val left = shiftExpr.children.get(0).asInstanceOf[ParserRuleContext]
      val right = shiftExpr.children.get(shiftExpr.children.size() - 1).asInstanceOf[ParserRuleContext]
      if(shiftExpr.children.size() == 5) { // >>>
        create expression(UnsignedRightShift, expr(left), expr(right))
      } else if(shiftExpr.children.get(1).asInstanceOf[TerminalNode].getText == "<") { // <<
        create expression(LeftShift, expr(left), expr(right))
      } else {
        create expression(RightShift, expr(left), expr(right))
      }
    case compExpr: Expression17Context =>
      val left = compExpr.children.get(0).asInstanceOf[ParserRuleContext]
      val right = compExpr.children.get(compExpr.children.size() - 1).asInstanceOf[ParserRuleContext]
      if(compExpr.children.size() == 4) {
        if(compExpr.children.get(1).asInstanceOf[TerminalNode].getText == "<") {
          create expression(LTE, expr(left), expr(right))
        } else {
          create expression(GTE, expr(left), expr(right))
        }
      } else {
        if(compExpr.children.get(1).asInstanceOf[TerminalNode].getText == "<") {
          create expression(StandardOperator.LT, expr(left), expr(right))
        } else {
          create expression(StandardOperator.GT, expr(left), expr(right))
        }
      }
    case Expression18(obj, "instanceof", t) =>
      create expression(Instance, expr(obj), convertType(t))
    case Expression19(left, "==", right) =>
      create expression(EQ, expr(left), expr(right))
    case Expression19(left, "!=", right) =>
      create expression(NEQ, expr(left), expr(right))
    case Expression20(left, "&", right) =>
      create expression(BitAnd, expr(left), expr(right))
    case Expression21(left, "^", right) =>
      create expression(BitXor, expr(left), expr(right))
    case Expression22(left, "|", right) =>
      create expression(BitOr, expr(left), expr(right))
    case Expression23(left, "&&", right) =>
      create expression(And, expr(left), expr(right))
    case Expression23(left, "**", right) =>
      create expression(Star, expr(left), expr(right))
    case Expression24(left, "||", right) =>
      create expression(Or, expr(left), expr(right))
    case Expression25(left, "==>", right) =>
      create expression(Implies, expr(left), expr(right))
    case Expression25(left, "-*", right) =>
      create expression(Wand, expr(left), expr(right))
    case Expression26(cond, "?", t, ":", f) =>
      create expression(ITE, expr(cond), expr(t), expr(f))
    case _: Expression27Context => ???

    case Primary0("(", exp, ")") => expr(exp)
    case Primary1("this") => create reserved_name ASTReserved.This
    case Primary2("super") => create reserved_name ASTReserved.Super
    case Primary3(Literal0(s)) => create constant Integer.parseInt(s)
    case Primary3(Literal1(s)) => ??? // float
    case Primary3(Literal2(s)) => ??? // character
    case Primary3(Literal3(s)) => ??? // string
    case Primary3(Literal4(s)) => create constant s.equals("true")
    case Primary4(name) => create unresolved_name convertID(name)
    case Primary5(t, ".", "class") => ???
    case Primary6("void", ".", "class") => ???
    case _: Primary7Context => ??? // generic invocation?
    case Primary8(extra) => expr(extra)

    case ExtraPrimary0(label, ":", exp) =>
      val res = expr(exp)
      res.addLabel(create unresolved_name label)
      res
    case ExtraPrimary1(valPrimary) => expr(valPrimary)


  }
}
