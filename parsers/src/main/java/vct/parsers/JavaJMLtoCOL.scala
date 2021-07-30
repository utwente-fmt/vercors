package vct.parsers

import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{CommonToken, CommonTokenStream, ParserRuleContext}
import vct.antlr4.generated.JavaParser
import vct.antlr4.generated.JavaParser._
import vct.antlr4.generated.JavaParserPatterns._
import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, Type}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression, NameExpressionKind, StandardOperator}
import vct.col.ast.generic.{ASTNode, BeforeAfterAnnotations}
import vct.col.ast.stmt.composite.{BlockStatement, CatchClause, Switch, TryCatchBlock, TryWithResources}
import vct.col.ast.stmt.decl.{ASTClass, ASTDeclaration, ASTSpecial, DeclarationStatement, Method, NameSpace, ProgramUnit, SignalsClause}
import vct.col.ast.util.ContractBuilder
import hre.lang.System.Warning

import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

object JavaJMLtoCOL {
  def convert(tree: CompilationUnitContext, fileName: String, tokens: CommonTokenStream, parser: JavaParser): ProgramUnit = {
    JavaJMLtoCOL(fileName, tokens, parser).convertUnit(tree)
  }
}

// Maybe we can turn this off in the future.
@nowarn("msg=not.*?exhaustive")
case class JavaJMLtoCOL(fileName: String, tokens: CommonTokenStream, parser: JavaParser)
  extends ToCOL(fileName, tokens, parser) {
  def convertUnit(tree: CompilationUnitContext): ProgramUnit = tree match {
    case CompilationUnit0(maybePackage, imports, decls, _) =>
      val result = new ProgramUnit()
      val namespace = origin(tree, maybePackage match {
        case None => create namespace(NameSpace.NONAME)
        case Some(PackageDeclaration0(Seq(annotation, _*), _, _, _)) =>
          ??(annotation) // package declaration annotations are not supported.
        case Some(PackageDeclaration0(Seq(), "package", name, _)) =>
          create namespace(convertQualifiedName(name):_*)
      })
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

  def convertID(id: ParserRuleContext): String = id match {
    case JavaIdentifier0(ValReserved1(s)) => s.substring(1, s.length-1)
    case JavaIdentifier0(reserved) =>
      fail(reserved, "This identifier is reserved, and may not be declared inside specifications.")
    case JavaIdentifier1(s) => s
    case JavaIdentifier2(reserved) =>
      convertOverlappingValReservedID(reserved)
    case LangId0(id) => convertID(id)

    case EnumConstantName0(id) => convertID(id)
  }

  def convertIDName(id: ParserRuleContext): NameExpression = origin(id, id match {
    case JavaIdentifier0(reserved) => convertValReserved(reserved)
    case JavaIdentifier1(s) => create unresolved_name(s)
    case JavaIdentifier2(reserved) => convertOverlappingValReservedName(reserved)
    case LangId0(id) => convertIDName(id)
  })

  def convertDecl(decl: LangDeclContext): ASTDeclaration = decl match {
    case LangDecl0(mods, member) =>
      val decls = convertDecl(member)
      if(decls.size == 1) {
        mods.map(convertModifier).foreach(_.foreach(mod => decls.head.attach(mod)))
        decls.head
      } else {
        ??(decl)
      }
  }

  def convertDecl(decl: ParserRuleContext): Seq[ASTDeclaration] = origin(decl, decl match {
    case TypeDeclaration0(mods, classDecl) =>
      val cls = convertClass(classDecl)
      mods.map(convertModifier).foreach(mod => cls.attach(mod))
      Seq(cls)
    case TypeDeclaration1(mods, enumDecl) =>
      ??(enumDecl)
    case TypeDeclaration2(mods, interfaceDecl) =>
      val cls = convertClass(interfaceDecl)
      mods.map(convertModifier).foreach(mod => cls.attach(mod))
      Seq(cls)
    case TypeDeclaration3(mods, annotationDecl) =>
      ??(annotationDecl)
    case TypeDeclaration4(_extraSemicolon) => Seq()

    case ClassBodyDeclaration0(_extraSemicolon) => Seq()
    case ClassBodyDeclaration1(maybeStatic, block) =>
      // This is a block that is executed on class load if "static" is present, otherwise
      // the code block is executed on every instance creation (prior to constructors, I think)
      ??(decl)
    case ClassBodyDeclaration2(maybeContract, mods, member) =>
      val decls = convertDecl(member)
      val contract = getContract(convertValContract(maybeContract))
      decls.foreach(decl => {
        mods.map(convertModifier).foreach(mods => mods.foreach (mod => decl.attach(mod)))
        decl match {
          case method: Method => method.setContract(contract)
          case other if maybeContract.isDefined =>
            fail(maybeContract.get, "Cannot attach contract to member below")
          case other => // OK
        }
      })
      decls
    case ClassBodyDeclaration3(valEmbed) =>
      convertValDecl(valEmbed)
    case ClassBodyDeclaration4(valDecl) =>
      Seq(convertValDecl(valDecl))
    case InterfaceBodyDeclaration0(mods, member) =>
      val decls = convertDecl(member)
      decls.foreach(decl => mods.map(convertModifier).foreach(mods => mods.foreach (mod => decl.attach(mod))))
      decls

    case MemberDeclaration0(fwd) => convertDecl(fwd)
    case MemberDeclaration1(fwd) => convertDecl(fwd)
    case MemberDeclaration2(fwd) => convertDecl(fwd)
    case MemberDeclaration3(fwd) => convertDecl(fwd)
    case MemberDeclaration4(fwd) => convertDecl(fwd)
    case MemberDeclaration5(fwd) => convertDecl(fwd)
    case MemberDeclaration6(fwd) => convertDecl(fwd)
    case MemberDeclaration7(fwd) => convertDecl(fwd)
    case MemberDeclaration8(fwd) => convertDecl(fwd)

    case MethodDeclaration0(retType, name, paramsNode, maybeDims, maybeThrows, maybeBody) =>
      val dims = maybeDims match { case None => 0; case Some(Dims0(dims)) => dims.size }
      val returns = convertType(retType, dims)
      val signals = convertThrows(maybeThrows)
      val (params, varargs) = convertParams(paramsNode)
      val body = maybeBody match {
        case MethodBodyOrEmpty0(";") => None
        case MethodBodyOrEmpty1(MethodBody0(block)) => Some(convertBlock(block))
      }
      Seq(create method_decl(returns, signals.toArray, null, convertID(name), params.toArray, body.orNull))
    case GenericMethodDeclaration0(typeParams, methodDecl) =>
      ??(typeParams) //generics are unsupported
    case InterfaceMethodDeclaration0(retType, name, params, maybeDims, None, _) =>
      ??(decl) // inheritance is unsupported
    case GenericInterfaceMethodDeclaration0(typeParams, methodDecl) =>
      ??(typeParams) // generics are unsupported
    case ConstructorDeclaration0(clsName, paramsNode, maybeThrows, bodyNode) =>
      val returns = create primitive_type PrimitiveSort.Void
      val signals = convertThrows(maybeThrows)
      val (params, varargs) = convertParams(paramsNode)
      val body = bodyNode match {
        case ConstructorBody0(block) => convertBlock(block)
      }
      Seq(create method_kind(Method.Kind.Constructor, returns, signals.toArray, null, convertID(clsName), params.toArray, varargs, body))
    case GenericConstructorDeclaration0(typeParams, constructorDecl) =>
      ??(typeParams) // generics are unsupported

    case FieldDeclaration0(t, declarators, _) =>
      for((name, dims, init) <- convertDeclarators(declarators))
        yield create field_decl(name, convertType(t, dims), init.orNull)
    case ConstDeclaration0(baseType, decls, _) =>
      for((name, dims, init) <- convertDeclarators(decls))
        yield create field_decl(name, convertType(baseType, dims), init.orNull)
    case LocalVariableDeclaration0(modsNode, t, decls) =>
      val mods = modsNode.map(convertModifier)
      convertDeclarators(decls).map { case (name, dims, init) =>
        val res = create field_decl(name, convertType(t, dims), init.orNull);
        mods.foreach(mod => res.attach(mod))
        res
      }

    case ClassDeclaration0("class", name, typeParameters, superClasses, interfaces, body) =>
      ??(decl)
  })

  def convertThrows(maybeThrows: Option[ThrowyContext]): Seq[Type] = maybeThrows match {
    case None => Seq()
    case Some(Throwy0(_, qualifiedNameList)) => convertTypeList(qualifiedNameList)
  }

  def convertResource(res: ResourceContext): DeclarationStatement = origin(res, res match {
    case Resource0(mods, t, name, "=", init) =>
      val decl = create field_decl(convertID(name), convertType(t), expr(init))
      mods.map(convertModifier).foreach(decl.attach(_))
      decl
  })

  def convertResourceList(res: ResourcesContext): Seq[DeclarationStatement] = origin(res, res match {
    case Resources0(x) => Seq(convertResource(x))
    case Resources1(x, _, xs) => convertResource(x) +: convertResourceList(xs)
  })

  def convertModifier(modifier: ModifierContext): Seq[NameExpression] = origin(modifier, modifier match {
    case Modifier0(mod) => Seq(convertModifier(mod))
    case Modifier1(mod) => Seq(mod match {
      case "native" => ??(modifier)
      case "synchronized" => create reserved_name ASTReserved.Synchronized
      case "transient" => ??(modifier)
      case "volatile" => create reserved_name ASTReserved.Volatile
    })
    case Modifier2(valMods) => convertValModifiers(valMods)
  })

  def convertModifier(modifier: ClassOrInterfaceModifierContext): NameExpression = origin(modifier, modifier match {
    case ClassOrInterfaceModifier0(annotation) =>
      ??(annotation)
    case ClassOrInterfaceModifier1(attribute) =>
      create reserved_name (attribute match {
        case "public" => ASTReserved.Public
        case "protected" => ASTReserved.Protected
        case "private" => ASTReserved.Private
        case "static" => ASTReserved.Static
        case "abstract" => ASTReserved.Abstract
        case "final" => ASTReserved.Final
        case "strictfp" =>
          ??(modifier) // strict floating point math; unsupported.
      })
  })

  def convertModifier(modifier: VariableModifierContext): NameExpression = origin(modifier, modifier match {
    case VariableModifier0("final") =>
      create reserved_name ASTReserved.Final
    case VariableModifier1(annotation) =>
      ??(annotation)
  })

  def convertModifier(langMod: LangModifierContext): NameExpression =
    langMod match { case LangModifier0(mod) => convertModifier(mod) }

  def convertClass(decl: InterfaceDeclarationContext): ASTClass = origin(decl, decl match {
    case InterfaceDeclaration0(_, _, Some(typeParams), _, _) =>
      ??(typeParams)
    case InterfaceDeclaration0("interface", name, None, maybeExtends, InterfaceBody0(_, decls, _)) =>
      val ext = maybeExtends match {
        case None => Seq()
        case Some(IntExt0(_, typeList)) => convertTypeList(typeList).map {
          case t: ClassType => t
          case resolvedType => fail(typeList, "Can only extend from an interface type, but found %s", resolvedType)
        }
      }
      val int = create ast_class(convertID(name), ASTClass.ClassKind.Interface, Array(), ext.toArray, Array())
      decls.map(convertDecl).foreach(_.foreach(int.add))
      int
  })

  def convertClass(decl: ClassDeclarationContext): ASTClass = origin(decl, decl match {
    case ClassDeclaration0(_, _, Some(typeParams), _, _, _) =>
      ??(typeParams) // generics are not supported.
    case ClassDeclaration0("class", name, None, maybeExtends, maybeImplements, ClassBody0(_, decls, _)) =>
      val ext = maybeExtends match {
        case None => Seq(create class_type Array("java", "lang", "Object"))
        case Some(Ext0(_, t)) =>
          Warning("detected inheritance. Inheritance support is incomplete.")
          Seq(convertType(t) match {
            case t: ClassType => t
            case resolvedType => fail(t, "Can only extend from a class type, but found %s", resolvedType)
          })
      }
      val imp = maybeImplements match {
        case None => Seq()
        case Some(Imp0(_, typeList)) => convertTypeList(typeList).map {
          case t: ClassType => t
          case resolvedType => fail(typeList, "Can only implement an interface type, but found %s", resolvedType)
        }
      }
      val cls = create ast_class(convertID(name), ASTClass.ClassKind.Plain, Array(), ext.toArray, imp.toArray)
      decls.map(convertDecl).foreach(_.foreach(cls.add))
      cls
  })

  def convertTypeList(tree: ParserRuleContext): Seq[Type] = tree match {
    case TypeList0(x) => Seq(convertType(x))
    case TypeList1(x, _, xs) => convertType(x) +: convertTypeList(xs)
    case TypeArguments0("<", xs, ">") => convertTypeList(xs)
    case TypeArgumentList0(x) => Seq(convertType(x))
    case TypeArgumentList1(x, _, xs) => convertType(x) +: convertTypeList(xs)
    case CatchType0(x) => Seq(convertType(x))
    case CatchType1(x, "|", xs) => convertType(x) +: convertTypeList(xs)
    case QualifiedNameList0(qualifiedName) => Seq(convertType(qualifiedName))
    case QualifiedNameList1(qualifiedName, ",", qualifiedNames) => Seq(convertType(qualifiedName)) ++ convertTypeList(qualifiedNames)
  }

  def tCell(t: Type) = create.primitive_type(PrimitiveSort.Cell, t)
  def tArray(t: Type) = create.primitive_type(PrimitiveSort.Array, t)
  def tOpt(t: Type) = create.primitive_type(PrimitiveSort.Option, t)

  def addDims(t: Type, dimCount: Int): Type = {
    var result = t
    for(_ <- 0 until dimCount) {
      result = tOpt(tArray(tCell(result)))
    }
    result
  }

  def convertType(tree: ParserRuleContext, extraDims: Int): Type = {
    addDims(convertType(tree), extraDims)
  }

  def convertType(tree: ParserRuleContext): Type = origin(tree, tree match {
    case qualified: QualifiedNameContext =>
      create class_type convertQualifiedName(qualified).toArray

    case LangType0(t) => convertType(t)

    case TypeOrVoid0("void") => create primitive_type PrimitiveSort.Void
    case TypeOrVoid1(t) => convertType(t)

    case Type0(valType) =>
      convertValType(valType)
    case Type1(t, dims) =>
      convertType(t, dims match { case None => 0; case Some(Dims0(dims)) => dims.size })
    case Type2(t, dims) =>
      convertType(t, dims match { case None => 0; case Some(Dims0(dims)) => dims.size })


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

    case ClassOrInterfaceType0(id, generics) =>
      val args = generics.map(convertTypeList).getOrElse(Seq())
      val name = convertID(id)
      if(name.equals("seq")) {
        args match {
          case Seq(subType) => create primitive_type(PrimitiveSort.Sequence, subType)
          case _ => fail(tree, "Built-in type seq takes exactly one type argument")
        }
      } else {
        create class_type(name, args:_*)
      }

    case ClassOrInterfaceType1(baseType, _, innerType, generics) =>
      ??(innerType) // inner classes are unsupported

    case TypeArgument0(t) => convertType(t)
    case wildcard: TypeArgument1Context => ??(wildcard) // generics are unsupported

    case CreatedName0(classType) => create class_type(convertClassType(classType).toArray)
    case CreatedName1(primitiveType) => convertType(primitiveType)
  })

  def convertClassType(tree: ClassTypeDiamondListContext): Seq[String] = tree match {
    case ClassTypeDiamondList0(name, Some(typeArgs)) =>
      ??(typeArgs)
    case ClassTypeDiamondList0(name, None) =>
      Seq(convertID(name))
    case ClassTypeDiamondList1(name, Some(typeArgs), _, names) =>
      ??(typeArgs)
    case ClassTypeDiamondList1(name, None, _, names) =>
      convertID(name) +: convertClassType(names)
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

  def convertParam(param: ParserRuleContext): DeclarationStatement = origin(param, param match {
    case FormalParameter0(Seq(mod, _*), _, _) =>
      ??(mod) // modifiers to method arguments are unsupported
    case FormalParameter0(Seq(), t, declaratorName) =>
      val (name, extraDims) = convertDeclaratorName(declaratorName)
      create field_decl(name, convertType(t, extraDims))
    case VarargsFormalParameter0(Seq(mod, _*), _, _, _) =>
      ??(mod)
    case VarargsFormalParameter0(Seq(), t, "...", declaratorName) =>
      val (name, extraDims) = convertDeclaratorName(declaratorName)
      create field_decl(name, convertType(t, extraDims))
  })

  def convertDeclaratorName(decl: VariableDeclaratorIdContext): (String, Int) = decl match {
    case VariableDeclaratorId0(name, None) => (convertID(name), 0)
    case VariableDeclaratorId0(name, Some(Dims0(dims))) => (convertID(name), dims.size)
  }

  def convertDeclarators(decls: ParserRuleContext): Seq[(String, Int, Option[ASTNode])] = decls match {
    case VariableDeclarators0(x) => Seq(convertDeclarator(x))
    case VariableDeclarators1(x, ",", xs) => convertDeclarator(x) +: convertDeclarators(xs)
    case ConstantDeclaratorList0(x) => Seq(convertDeclarator(x))
    case ConstantDeclaratorList1(x, ",", xs) => convertDeclarator(x) +: convertDeclarators(xs)
  }

  def convertDeclarator(decl: ParserRuleContext): (String, Int, Option[ASTNode]) = decl match {
    case VariableDeclarator0(declId, maybeInit) =>
      val (name, extraDims) = convertDeclaratorName(declId)
      (name, extraDims, maybeInit.map(expr))
    case ConstantDeclarator0(name, maybeDims, _, init) =>
      val extraDims = maybeDims.map { case Dims0(dims) => dims.size }.getOrElse(0)
      (convertID(name), extraDims, Some(expr(init)))
  }

  def convertBlock(block: FinallyBlockContext): BlockStatement = origin(block, block match {
    case FinallyBlock0(_, block) => convertBlock(block)
  })

  def convertBlock(block: BlockContext): BlockStatement = origin(block, block match {
    case Block0(_, stats, _) =>
      create block(stats.flatMap(convertStat):_*)
  })

  def addCatchClauses(tryBlock: TryCatchBlock, catchClauses: Seq[CatchClauseContext]): Unit = {
    catchClauses.foreach {
      case CatchClause0(_, _, Seq(mod, _*), _, _, _, _) =>
        ??(mod)
      case ccCtx@CatchClause0("catch", _, Seq(), types, name, _, block) =>
        val cc = CatchClause(convertID(name), convertTypeList(types), convertBlock(block))
        tryBlock.addCatchClause(origin(ccCtx, cc))
    }
  }

  def addCatchClauses(tryBlock: TryWithResources, catchClauses: Seq[CatchClauseContext]): Unit = {
    catchClauses.foreach {
      case CatchClause0(_, _, Seq(mod, _*), _, _, _, _) =>
        ??(mod)
      case CatchClause0("catch", _, Seq(), types, name, _, block) =>
        tryBlock.addCatchClause(convertID(name), convertTypeList(types), convertBlock(block))
    }
  }

  def convertStat(stat: BlockStatementContext): Seq[ASTNode] = origin(stat, stat match {
    // BlockStatement is a statement that may occur in a block, not a block in itself.
    case BlockStatement0(LocalVariableDeclarationStatement0(varDecl, _)) =>
      convertDecl(varDecl)
    case BlockStatement1(stat) =>
      Seq(convertStat(stat))
    case BlockStatement2(typeDecl) =>
      convertDecl(typeDecl)
    case BlockStatement3(valEmbedBlock) =>
      convertValStat(valEmbedBlock)
  })

  def convertStatWithContract(stat: StatementContext, maybeContracts: Seq[Option[ValEmbedContractContext]]): ASTNode = origin(stat, stat match {
    case Statement3(maybeContract2, "for", "(", ForControl1(maybeInit, _, maybeCond, _, maybeUpdate), ")", maybeContract3, body) =>
      val allContracts = (maybeContracts ++ Seq(maybeContract2, maybeContract3)).map(convertValContract)
      val contract = getContract(allContracts:_*)
      val loop = create for_loop(
        maybeInit.map(convertStat).orNull,
        maybeCond.map(expr).orNull,
        maybeUpdate.map(convertStat).orNull,
        convertStat(body)
      )
      loop.setContract(contract)
      loop
    case Statement4(maybeContract2, "while", cond, maybeContract3, body) =>
      val allContracts = (maybeContracts ++ Seq(maybeContract2, maybeContract3)).map(convertValContract)
      val contract = getContract(allContracts:_*)
      val loop = create while_loop(expr(cond), convertStat(body))
      loop.setContract(contract)
      loop
    case Statement17(maybeContract2, label, ":", stat) =>
      val res = convertStatWithContract(stat, maybeContracts ++ Seq(maybeContract2))
      res.addLabel(create label convertID(label))
      res
    case statement => ??(statement)
  })

  def convertStat(stat: StatementContext): ASTNode = origin(stat, stat match {
    case Statement0(block) =>
      convertBlock(block)
    case Statement1(_assert, exp, _message, _) =>
      create special(ASTSpecial.Kind.Assert, expr(exp))
    case Statement2("if", cond, whenTrue, maybeWhenFalse) =>
      create ifthenelse(expr(cond),
        convertStat(whenTrue),
        maybeWhenFalse.map(convertStat).orNull)
    case Statement3(maybeContract, "for", "(", ForControl0(forEachControl), ")", maybeContract2, body) =>
      ??(forEachControl) // for(a : b) is unsupported
    case s@Statement3(maybeContract2, "for", "(", ForControl1(maybeInit, _, maybeCond, _, maybeUpdate), ")", maybeContract3, body) =>
      convertStatWithContract(s, Seq())
    case s@Statement4(maybeContract2, "while", cond, maybeContract3, body) =>
      convertStatWithContract(s, Seq())
    case Statement5("do", body, "while", cond, _) =>
      ??(stat) // do-while unsupported
    case Statement6("try", block, catchClauses, maybeFinally) =>
      val tryBlock = create try_catch(convertBlock(block), maybeFinally.map(convertBlock).orNull)
      addCatchClauses(tryBlock, catchClauses)
      tryBlock
    case Statement7("try", block, uiteindelijk) =>
      create try_catch(convertBlock(block), convertBlock(uiteindelijk))
    case Statement8("try", ResourceSpecification0(_, res, _, _), block, catchClauses, maybeFinally) =>
      val tryBlock = create try_with_resources(convertBlock(block), maybeFinally.map(convertBlock).orNull)
      convertResourceList(res).foreach(tryBlock.addResource)
      addCatchClauses(tryBlock, catchClauses)
      tryBlock
    case Statement9("switch", cond, "{", caseStatMappings, extraCases, "}") =>
      val cases = caseStatMappings.map(convertCase)
      create switch_statement(
        expr(cond),
        (cases ++ Seq(convertCaseStat(extraCases))).asJava
      )
    case Statement10("synchronized", obj, body) =>
      create syncBlock(expr(obj), convertBlock(body))
    case Statement11("return", maybeValue, _) =>
      maybeValue match {
        case None => create.return_statement()
        case Some(value) => create return_statement(expr(value))
      }
    case Statement12("throw", exc, _) =>
      create special(ASTSpecial.Kind.Throw, expr(exc))
    case Statement13("break", maybeLabel, _) =>
      maybeLabel match {
        case None => create special(ASTSpecial.Kind.Break)
        case Some(lbl) => create special(ASTSpecial.Kind.Break, convertIDName(lbl))
      }
    case Statement14("continue", maybeLabel, _) =>
      maybeLabel match {
        case None => create special(ASTSpecial.Kind.Continue)
        case Some(lbl) => create special(ASTSpecial.Kind.Continue, convertIDName(lbl))
      }
    case Statement15(";") =>
      create.block() // Nop
    case Statement16(exp, _) =>
      expr(exp)
    case Statement17(None, label, ":", stat) =>
      val res = convertStat(stat)
      res.addLabel(create label convertID(label))
      res
    case s@Statement17(_, _, _, _) =>
      convertStatWithContract(s, Seq())
    case Statement18(valStatement) =>
      convertValStat(valStatement)
  })

  def convertStat(stat: ElseBlockContext): ASTNode = origin(stat, stat match {
    case ElseBlock0(_, stat) => convertStat(stat)
  })

  def convertStat(stat: ForInitContext): ASTNode = origin(stat, stat match {
    case ForInit0(varDecl) =>
      // Of course we can't put declarations inside an actual new scope, but these are dealt with explicitly.
      flattenIfSingleStatement(convertDecl(varDecl))
    case ForInit1(exps) =>
      flattenIfSingleStatement(exprList(exps))
  })

  def convertStat(stat: ForUpdateContext): ASTNode = origin(stat, stat match {
    case ForUpdate0(exps) =>
      flattenIfSingleStatement(exprList(exps))
  })

  def convertSwitchLabel(switchLabel: SwitchLabelContext): ASTNode = switchLabel match {
    case SwitchLabel0("case", constantExpr, ":") => expr(constantExpr)
    case SwitchLabel1("case", enumConstantName, ":") => ??(enumConstantName)
    case SwitchLabel2("default", ":") => null
  }

  def convertCase(caseStat: SwitchBlockStatementGroupContext): Switch.Case = caseStat match {
    case SwitchBlockStatementGroup0(labelExprs, stats) =>
      val cases = new Switch.Case()
      cases.cases.addAll(labelExprs.map(convertSwitchLabel).asJavaCollection)
      cases.stats.addAll(stats.flatMap(convertStat).asJavaCollection)
      cases
    case _ => ??(caseStat)
  }

  def convertCaseStat(switchLabels: Seq[SwitchLabelContext]): Switch.Case = {
    val cases = new Switch.Case()
    cases.cases.addAll(switchLabels.map(convertSwitchLabel).asJavaCollection)
    cases
  }

  def exprList(tree: ParserRuleContext): Seq[ASTNode] = tree match {
    case Arguments0(_, None, _) => Seq()
    case Arguments0(_, Some(xs), _) => exprList(xs)
    case ExpressionList0(exp) => Seq(expr(exp))
    case ExpressionList1(x, _, xs) => expr(x) +: exprList(xs)
    case SpecifiedDims0(x) => Seq(expr(x))
    case SpecifiedDims1(x, xs) => expr(x) +: exprList(xs)
  }

  def getArrayInitializerList(initializers: VariableInitializerListContext,
                              dims: Int, baseType: Type): Seq[ASTNode] = initializers match {
    case VariableInitializerList0(initializer) =>
      Seq(getArrayInitializer(initializer, dims, baseType))
    case VariableInitializerList1(initializer, _, initializers) =>
      getArrayInitializer(initializer, dims, baseType) +:
        getArrayInitializerList(initializers, dims, baseType)
  }

  def getArrayInitializer(initializer: ParserRuleContext, dims: Int, baseType: Type): ASTNode = initializer match {
    case ArrayInitializer0("{", "}") =>
      create expression(OptionSome,
        create struct_value(tArray(tCell(addDims(baseType, dims-1))), null))
    case ArrayInitializer1("{", initializers, _, "}") =>
      create expression(OptionSome,
        create struct_value(tArray(tCell(addDims(baseType, dims-1))), null, getArrayInitializerList(initializers, dims-1, baseType):_*))

    case VariableInitializer0(arrayInitializer) => getArrayInitializer(arrayInitializer, dims, baseType)
    case VariableInitializer1(exp) => expr(exp)
  }

  def expr(tree: ParserRuleContext): ASTNode = origin(tree, tree match {
    case ParExpression0("(", exp, ")") => expr(exp)
    case StatementExpression0(exp) => expr(exp)
    case ConstantExpression0(exp) => expr(exp)
    case LangExpr0(exp) => expr(exp)

    case Expression0(valPrimary) => valExpr(valPrimary)
    case Expression1(primary) => expr(primary)
    case Expression2(obj, ".", field) =>
      create dereference(expr(obj), convertID(field))
    case Expression3(obj, ".", "this") =>
      ??(tree)
      /* This is used to refer to a specific "this", within the context of an inner class (which is defined wrt an
      * outer class instance, unless it is defined as static). We should start supporting this if we end up
      * implementing inner classes. */
    case Expression4(obj, ".", "new", typeArgs, creator) =>
      ??(tree)
      /* This is again used in inner classes, where you instantiate an inner class with respect to a specific instance
       * of the outer class. Normally it is instantiated with respect to the current ("this") instance of the outer
       * class. */
    case Expression5(obj, ".", "super", suffix) =>
      ??(tree)
      /*
      Two cases, either suffix = arguments:
        Then this is an invocation of a constructor, whereby classname.super is called a
        qualified superclass constructor
      Or suffix = '.' identifier arguments:
        then classname.super is the qualifying type of the method call <identifier arguments>

      inheritance is unsupported
      */
    case Expression6(objNode, ".", invokation) =>
      ??(tree)
    case Expression7(seq, "[", idx, "]") =>
      create expression(Subscript, expr(seq), expr(idx))
    case Expression8(objNode, "->", predicate, args) =>
      val obj = expr(objNode)
      create expression(Implies,
          create expression(NEQ, obj, create reserved_name ASTReserved.Null),
        create invokation(obj, null, convertID(predicate), exprList(args):_*)
      )
    case Expression9(_, _, _, Some(predicateEntryType), _, _) =>
      // the predicate entry type is set as dispatch of an invokation
      ??(predicateEntryType)
    case Expression9(obj, ".", method, None, args, maybeWithThen) =>
      val res = create invokation(expr(obj), null, convertID(method), exprList(args).asJava)
      maybeWithThen match {
        case None =>
        case Some(block) =>
          res.set_after(create block(convertValWithThen(block):_*))
      }
      res
    case Expression10("new", Creator0(typeArgs, _, _), _) =>
      ??(typeArgs) // generics are unsupported
    case Expression10("new", Creator1(name, creator), maybeWithThen) => (name, creator) match {
      case (CreatedName1(primitiveType), CreatorRest1(_classCreator)) =>
        fail(primitiveType, "This is invalid syntax; it parsed as a constructor call on a primitive type.")
      case (t, CreatorRest0(ArrayCreatorRest0(Dims0(dims), initializer))) =>
        failIfDefined(maybeWithThen, "with/then arguments cannot be applied to an array constructor")
        val baseType = convertType(t)
        getArrayInitializer(initializer, dims.size, baseType)
      case (t, CreatorRest0(ArrayCreatorRest1(specDims, maybeAnonDims))) =>
        failIfDefined(maybeWithThen, "with/then arguments cannot be applied to an array constructor")
        val anonDims = maybeAnonDims match { case None => 0; case Some(Dims0(dims)) => dims.size }
        val knownDims = exprList(specDims)
        create expression(NewArray, convertType(t, knownDims.size + anonDims), knownDims.toArray)
      case (t, CreatorRest1(ClassCreatorRest0(arguments, maybeBody))) =>
        val res = create new_object(convertType(t).asInstanceOf[ClassType], exprList(arguments):_*)
        maybeWithThen match {
          case None =>
          case Some(block) =>
            res.set_after(create block(convertValWithThen(block):_*))
        }
        res
    }

    case Expression11("(", t, ")", exp) => create expression(Cast, convertType(t), expr(exp))
    case Expression12(exp, "++") => create expression(PostIncr, expr(exp))
    case Expression12(exp, "--") => create expression(PostDecr, expr(exp))
    case Expression13("+", exp) => expr(exp)
    case Expression13("-", exp) => create expression(UMinus, expr(exp))
    case Expression13("++", exp) => create expression(PreIncr, expr(exp))
    case Expression13("--", exp) => create expression(PreDecr, expr(exp))
    case Expression14("~", exp) => create expression(BitNot, expr(exp))
    case Expression14("!", exp) => create expression(Not, expr(exp))
    case Expression15(left, MulOp0("*"), right) => create expression(Mult, expr(left), expr(right))
    case Expression15(left, MulOp0("/"), right) => create expression(FloorDiv, expr(left), expr(right))
    case Expression15(left, MulOp0("%"), right) => create expression(Mod, expr(left), expr(right))
    case Expression15(left, MulOp1(valOp), right) => create expression(convertValOp(valOp), expr(left), expr(right))
    case Expression16(left, "+", right) => create expression(Plus, expr(left), expr(right))
    case Expression16(left, "-", right) => create expression(Minus, expr(left), expr(right))
    case shiftExpr: Expression17Context =>
      val left = shiftExpr.children.get(0).asInstanceOf[ParserRuleContext]
      val right = shiftExpr.children.get(shiftExpr.children.size() - 1).asInstanceOf[ParserRuleContext]
      if(shiftExpr.children.size() == 5) { // >>>
        create expression(UnsignedRightShift, expr(left), expr(right))
      } else if(shiftExpr.children.get(1).asInstanceOf[TerminalNode].getText == "<") { // <<
        create expression(LeftShift, expr(left), expr(right))
      } else {
        create expression(RightShift, expr(left), expr(right))
      }
    case Expression18(left, comp, right) =>
      create expression(comp match {
        case "<" => StandardOperator.LT
        case "<=" => LTE
        case ">=" => GTE
        case ">" => StandardOperator.GT
      }, expr(left), expr(right))
    case Expression19(obj, "instanceof", t) =>
      create expression(Instance, expr(obj), convertType(t))
    case Expression20(left, "==", right) =>
      create expression(EQ, expr(left), expr(right))
    case Expression20(left, "!=", right) =>
      create expression(NEQ, expr(left), expr(right))
    case Expression21(left, "&", right) =>
      create expression(AmbiguousAnd, expr(left), expr(right))
    case Expression22(left, "^", right) =>
      create expression(AmbiguousXor, expr(left), expr(right))
    case Expression23(left, "|", right) =>
      create expression(AmbiguousOr, expr(left), expr(right))
    case Expression24(left, AndOp0("&&"), right) =>
      create expression(And, expr(left), expr(right))
    case Expression24(left, AndOp1(valOp), right) =>
      create expression(convertValOp(valOp), expr(left), expr(right))
    case Expression25(left, "||", right) =>
      create expression(Or, expr(left), expr(right))
    case Expression26(left, ImpOp0(valOp), right) =>
      create expression(convertValOp(valOp), expr(left), expr(right))
    case Expression27(cond, "?", t, ":", f) =>
      create expression(ITE, expr(cond), expr(t), expr(f))
    case assignment: Expression28Context => assignment.children.asScala.toSeq match {
      case Seq(left: ExpressionContext, op, right: ExpressionContext) => op.getText match {
        case "=" => create assignment(expr(left), expr(right))
        case "+=" => create expression(AddAssign, expr(left), expr(right))
        case "-=" => create expression(SubAssign, expr(left), expr(right))
        case "*=" => create expression(MulAssign, expr(left), expr(right))
        case "/=" => create expression(FloorDivAssign, expr(left), expr(right))
        case "&=" => create expression(AndAssign, expr(left), expr(right))
        case "|=" => create expression(OrAssign, expr(left), expr(right))
        case "^=" => create expression(XorAssign, expr(left), expr(right))
        case ">>=" => create expression(ShrAssign, expr(left), expr(right))
        case ">>>=" => create expression(SShrAssign, expr(left), expr(right))
        case "<<=" => create expression(ShlAssign, expr(left), expr(right))
        case "%=" => create expression(RemAssign, expr(left), expr(right))
      }
    }

    case Primary0("(", exp, ")") => expr(exp)
    case Primary1("this") => create reserved_name ASTReserved.This
    case Primary2("super") => create reserved_name ASTReserved.Super
    case Primary3(Literal0(s)) => create constant Integer.parseInt(s)
    case Primary3(Literal1(s)) => ??(tree) // float
    case Primary3(Literal2(s)) => ??(tree) // character
    // Pretty sure this completely ignores escape sequences, but we don't support strings anyway...
    // See also CMLtoCOL PrimaryExpression2
    case Primary3(Literal3(s)) => create constant s
    case Primary3(Literal4(s)) => create constant s.equals("true")
    case Primary3(Literal5("null")) => create reserved_name(ASTReserved.Null)
    case Primary4(name) => convertIDName(name)
    case Primary5(_, Some(predicateEntryType), _, _) =>
      ??(predicateEntryType)
    case Primary5(method, None, args, maybeWithThen) =>
      val res = create invokation(null, null, convertID(method), exprList(args).asJava)
      maybeWithThen match {
        case None =>
        case Some(block) =>
          res.set_after(create block(convertValWithThen(block):_*))
      }
      res
    case Primary6(t, ".", "class") =>
      ??(tree) // reflection is unsupported
    case Primary7("void", ".", "class") =>
      ??(tree) // reflection is unsupported
    case _: Primary8Context => ??(tree) // generic invocation?

    case VariableDeclaratorInit0(_, exp) => expr(exp)
    case VariableInitializer0(arr) => ??(arr)
    case VariableInitializer1(exp) => expr(exp)
    case SpecifiedDim0("[", dimSize, "]") => expr(dimSize)
  })

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
        create expression(LTE, expr(fr), create unresolved_name(name)),
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
    case ValPrimary29("Reducible", "(", exp, _, opNode, ")") =>
      val opText = opNode match {
        case ValReducibleOperator0("+") => "+"
        case ValReducibleOperator1(id) => convertID(id)
      }
      create expression(opText match {
        case "+" => ReducibleSum
        case "min" => ReducibleMin
        case "max" => ReducibleMax
      }, expr(exp))
    case ValPrimary30("AbstractState", _, arg1, _, arg2, _) =>
      create expression(StandardOperator.AbstractState, expr(arg1), expr(arg2))
    case ValPrimary31("AddsTo", _, arg1, _, arg2, _) =>
      create expression(StandardOperator.AddsTo, expr(arg1), expr(arg2))
    case ValPrimary32("APerm", _, loc, _, perm, _) =>
      create expression(StandardOperator.ActionPerm, expr(loc), expr(perm))
    case ValPrimary33("ArrayPerm", _, ar, _, fst, _, step, _, cnt, _, perm, _) =>
      create expression(StandardOperator.ArrayPerm, expr(ar), expr(fst), expr(step), expr(cnt), expr(perm))
    case ValPrimary34("buildMap", _, map, _, k, _, v, _) =>
      create expression(StandardOperator.MapBuild, expr(map), expr(k), expr(v))
    case ValPrimary35("cardMap", _, map, _) =>
      create expression(StandardOperator.MapCardinality, expr(map))
    case ValPrimary36("Contribution", _, res, _, con, _) =>
      create expression(StandardOperator.Contribution, expr(res), expr(con))
    case ValPrimary37("disjointMap", _, map1, _, map2, _) =>
      create expression(StandardOperator.MapDisjoint, expr(map1), expr(map2))
    case ValPrimary38("equalsMap", _, map1, _, map2, _) =>
      create expression(StandardOperator.MapEquality, expr(map1), expr(map2))
    case ValPrimary39("Future", _, arg1, _, arg2, _, arg3, _) =>
      create expression(StandardOperator.Future, expr(arg1), expr(arg2), expr(arg3))
    case ValPrimary40("getFromMap", _, map, _, k, _) =>
      create expression(StandardOperator.MapGetByKey, expr(map), expr(k))
    case ValPrimary41("getFst", _, tup, _) =>
      create expression(StandardOperator.TupleFst, expr(tup))
    case ValPrimary42("getOption", _, opt, _) =>
      create expression(StandardOperator.OptionGet, expr(opt))
    case ValPrimary43("getSnd", _, tup, _) =>
      create expression(StandardOperator.TupleSnd, expr(tup))
    case ValPrimary44("head", _, seq, _) =>
      create expression(StandardOperator.Head, expr(seq))
    case ValPrimary45("held", _, lock, _) =>
      create expression(StandardOperator.Held, expr(lock))
    case ValPrimary46("Hist", _, arg1, _, arg2, _, arg3, _) =>
      create expression(StandardOperator.History, expr(arg1), expr(arg2), expr(arg3))
    case ValPrimary47("HPerm", _, loc, _, perm, _) =>
      create expression(StandardOperator.HistoryPerm, expr(loc), expr(perm))
    case ValPrimary48("idle", _, arg, _) =>
      create expression(StandardOperator.PVLidleToken, expr(arg))
    case ValPrimary49("isEmpty", _, seq, _) =>
      create expression(StandardOperator.Empty, expr(seq))
    case ValPrimary50("itemsMap", _, map, _) =>
      create expression(StandardOperator.MapItemSet, expr(map))
    case ValPrimary51("keysMap", _, map, _) =>
      create expression(StandardOperator.MapKeySet, expr(map))
    case ValPrimary52("perm", _, loc, _) =>
      create expression(StandardOperator.CurrentPerm, expr(loc))
    case ValPrimary53("Perm", _, loc, _, perm, _) =>
      create expression(StandardOperator.Perm, expr(loc), expr(perm))
    case ValPrimary54("PointsTo", _, loc, _, perm, _, value, _) =>
      create expression(StandardOperator.PointsTo, expr(loc), expr(perm), expr(value))
    case ValPrimary55(_removeAt, _, seq, _, i, _) =>
      create expression(StandardOperator.RemoveAt, expr(seq), expr(i))
    case ValPrimary56("removeFromMap", _, map, _, arg, _) =>
      create expression(StandardOperator.MapRemoveKey, expr(map), expr(arg))
    case ValPrimary57("running", _, arg, _) =>
      create expression(StandardOperator.PVLjoinToken, expr(arg))
    case ValPrimary58("Some", _, arg, _) =>
      create expression(StandardOperator.OptionSome, expr(arg))
    case ValPrimary59("tail", _, seq, _) =>
      create expression(StandardOperator.Tail, expr(seq))
    case ValPrimary60("Value", _, arg, _) =>
      create expression(StandardOperator.Value, expr(arg))
    case ValPrimary61("valuesMap", _, map, _) =>
      create expression(StandardOperator.MapValueSet, expr(map))
    case ValPrimary62("seq", "<", t, ">", "{", elems, "}") =>
      create struct_value(create.primitive_type(PrimitiveSort.Sequence, convertType(t)), null, convertValExpList(elems):_*)
    case ValPrimary63("set", "<", t, ">", "{", elems, "}") =>
      create struct_value(create.primitive_type(PrimitiveSort.Set, convertType(t)), null, convertValExpList(elems):_*)
    case ValPrimary64("(", seq, "[", "..", end, "]", ")") =>
      create expression(Take, expr(seq), expr(end))
    case ValPrimary65("(", seq, "[", start, "..", None, "]", ")") =>
      create expression(Drop, expr(seq), expr(start))
    case ValPrimary65("(", seq, "[", start, "..", Some(end), "]", ")") =>
      create expression(Slice, expr(seq), expr(start), expr(end))
    case ValPrimary66("(", seq, "[", idx, "->", replacement, "]", ")") =>
      create expression(SeqUpdate, expr(seq), expr(idx), expr(replacement))
    case ValPrimary67("(", x, "::", xs, ")") =>
      create expression(PrependSingle, expr(x), expr(xs))
    case ValPrimary68("(", xs, "++", ys, ")") =>
      create expression(Concat, expr(xs), expr(ys))
    case ValPrimary69("(", x, "\\in", xs, ")") =>
      create expression(Member, expr(x), expr(xs))
    case ValPrimary70("getOrElseOption", "(", opt, ",", alt, ")") =>
      create expression(OptionGetOrElse, expr(opt), expr(alt))
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
      create axiom(convertID(name), create expression(EQ, expr(left), expr(right)))
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
