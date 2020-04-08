package vct.antlr4.parser

import hre.lang.System._
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
import vct.antlr4.generated.CParser
import vct.antlr4.generated.CParser._
import vct.antlr4.generated.CParserPatterns._
import vct.col.ast.`type`.{ASTReserved, FunctionType, PrimitiveSort, Type}
import vct.col.ast.expr.{NameExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.langspecific.c.{CFunctionType, ParamSpec}
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.{ASTDeclaration, ASTSpecial, Contract, DeclarationStatement, ProgramUnit}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.util.ContractBuilder

import scala.collection.immutable.{Bag, HashedBagConfiguration}
import scala.collection.mutable

object CMLtoCOL {
  def convert(tree: CompilationUnitContext, fileName: String, tokens: CommonTokenStream, parser: CParser): ProgramUnit = {
    new CMLtoCOL(fileName, tokens, parser).convertProgram(tree)
  }
}

class CMLtoCOL(fileName: String, tokens: CommonTokenStream, parser: CParser)
  extends ToCOL(fileName, tokens, parser)
{
  def convertProgram(tree: CompilationUnitContext): ProgramUnit = tree match {
    case CompilationUnit0(None, _) =>
      new ProgramUnit()
    case CompilationUnit0(Some(units), _) =>
      val result = new ProgramUnit()
      convertTranslationUnits(units).foreach(result.add)
      result
  }

  def convertTranslationUnits(tree: TranslationUnitContext): Seq[ASTDeclaration] = tree match {
    case TranslationUnit0(decl) => convertDecl(decl)
    case TranslationUnit1(units, decl) => convertTranslationUnits(units) ++ convertDecl(decl)
  }

  def convertDecl(tree: ExternalDeclarationContext): Seq[ASTDeclaration] = tree match {
    case ExternalDeclaration0(funcDecl) => convertDecl(funcDecl)
    case ExternalDeclaration1(decl) => convertDecl(decl)
    case ExternalDeclaration2(";") => Seq()
  }

  def convertDecl(tree: FunctionDefinitionContext): Seq[ASTDeclaration] = origin(tree, tree match {
    case FunctionDefinition0(_, _, _, Some(declList), _) =>
      ??(declList)
    case FunctionDefinition0(_, _, Declarator0(_, _, Seq(extension, _*)), _, _) =>
      ??(extension)
    case FunctionDefinition0(maybeContract, declSpecs, Declarator0(maybePtr, decl, Seq()), None, statement) =>
      val specs = new DeclSpecs
      specs.add(declSpecs)
      val rawT = convertDeclaratorType(decl)(convertPointer(maybePtr)(getOrFail(declSpecs, specs.getType)))

      if(!rawT.isInstanceOf[CFunctionType]) {
        fail(decl, "This declarator specifies something that is not a function at the top level.")
      }

      val t = rawT.asInstanceOf[CFunctionType]
      val name = convertDeclaratorName(decl)
      val body = convertStat(statement)
      val contract = getContract(convertValContract(maybeContract))
      val decls = t.params.map(param => getOrFail(decl, param.asDecl, "Parameter type or name missing"))
      Seq(create method_decl(t.returnType, contract, name, decls.toArray, body))
  })

  def failIfDefined[T <: ParserRuleContext](node: Option[T], format: String, args: Object*): Unit = node match {
    case Some(node) => fail(node, format, args)
    case None => // do nothing
  }

  def convertPointer(ptr: Option[PointerContext]): (Type => Type) = ptr match {
    case None => x => x
    case Some(ptr) => convertPointer(ptr)
  }

  def convertDeclaratorName(decl: DirectDeclaratorContext): String = decl match {
    case DirectDeclarator0(name) => convertID(name)
    case DirectDeclarator1(inner, _, _, _, _) => convertDeclaratorName(inner)
    case DirectDeclarator2(inner, _, _, _, _, _) => convertDeclaratorName(inner)
    case DirectDeclarator3(inner, _, _, _, _, _) => convertDeclaratorName(inner)
    case DirectDeclarator4(inner, _, _, _, _) => convertDeclaratorName(inner)
    case DirectDeclarator5(inner, _, _, _) => convertDeclaratorName(inner)
    case DirectDeclarator6(inner, _, _, _) => convertDeclaratorName(inner)
  }

  def tCell(t: Type) = create.primitive_type(PrimitiveSort.Cell, t)
  def tArray(t: Type) = create.primitive_type(PrimitiveSort.Array, t)
  def tOpt(t: Type) = create.primitive_type(PrimitiveSort.Option, t)

  def addDims(t: Type, dimCount: Int): Type = {
    var result = t

    if(result.isPrimitive(PrimitiveSort.Option)) {
      result = result.firstarg.asInstanceOf[Type]
    }

    for(_ <- 0 until dimCount) {
      result = tArray(tCell(result))
    }

    tOpt(result)
  }

  def convertDeclaratorType(decl: DirectDeclaratorContext): (Type => Type) = decl match {
    case DirectDeclarator0(name) => t => t
    case DirectDeclarator1(inner, "[", quals, _, "]") =>
      failIfDefined(quals, "Qualifiers in array dimensions are unsupported")
      (t => convertDeclaratorType(inner)(addDims(t, 1)))
    case DirectDeclarator2(_, _, _, _, _, _) =>
      ??(decl)
    case DirectDeclarator3(_, _, _, _, _, _) =>
      ??(decl)
    case DirectDeclarator4(_, _, _, _, _) =>
      ??(decl)
    case DirectDeclarator5(inner, "(", params, ")") =>
      t => convertDeclaratorType(inner)(CFunctionType(convertParams(params), t))
    case DirectDeclarator6(inner, "(", None, ")") =>
      t => convertDeclaratorType(inner)(CFunctionType(Seq(), t))
    case DirectDeclarator6(inner, "(", Some(names), ")") =>
      // This is the dual of the declaration list after a function definition; the types of the arguments are missing.
      ??(names)
  }

  def convertParams(tree: ParameterTypeListContext): Seq[ParamSpec] = tree match {
    case ParameterTypeList0(params) =>
      convertParams(params)
    case ParameterTypeList1(params, ",", "...") =>
      convertParams(params) :+
        ParamSpec(Some(create primitive_type PrimitiveSort.CVarArgs), None)
  }

  def convertParams(tree: ParameterListContext): Seq[ParamSpec] = tree match {
    case ParameterList0(param) =>
      Seq(convertParam(param))
    case ParameterList1(params, _, param) =>
      convertParams(params) :+ convertParam(param)
  }

  def convertParam(tree: ParameterDeclarationContext): ParamSpec = tree match {
    case ParameterDeclaration0(declSpecs, declarator) =>
      val specs = new DeclSpecs
      specs.add(declSpecs)
      val baseType = getOrFail(declSpecs, specs.getType)
      declarator match {
        case Declarator0(maybePtr, directDecl, Seq(ext, _*)) =>
          ??(ext)
        case Declarator0(maybePtr, directDecl, Seq()) =>
          val t = convertPointer(maybePtr)(convertDeclaratorType(directDecl)(baseType))
          val name = convertDeclaratorName(directDecl)
          ParamSpec(Some(t), Some(name))
      }
    case ParameterDeclaration1(declSpecs, maybeDeclarator) =>
      ??(declSpecs)
  }

  def convertPointer(ptr: PointerContext): (Type => Type) = ptr match {
    case Pointer0("*", quals) =>
      failIfDefined(quals, "Qualifiers to pointers such as const are not supported")
      (t => create primitive_type(PrimitiveSort.Pointer, t))
    case Pointer1("*", quals, ptr) =>
      failIfDefined(quals, "Qualifiers to pointers such as const are not supported")
      (t => create primitive_type(PrimitiveSort.Pointer,
        // Left pointer is the outermost pointer, which is important if we come to support qualifiers.
        convertPointer(ptr)(t)
      ))
    case Pointer2("**", quals) =>
      failIfDefined(quals, "Qualifiers to pointers such as const are not supported")
      (t => create primitive_type(PrimitiveSort.Pointer, create primitive_type(PrimitiveSort.Pointer, t)))
    case Pointer3("**", quals, ptr) =>
      failIfDefined(quals, "Qualifiers to pointers such as const are not supported")
      (t => create primitive_type(PrimitiveSort.Pointer, create primitive_type(PrimitiveSort.Pointer,
        // Left pointer is the outermost pointer, which is important if we come to support qualifiers.
        convertPointer(ptr)(t)
      )))
    case Pointer4(_, _) => ??(ptr)
    case Pointer5(_, _, _) => ??(ptr)
  }

  def convertDecl(tree: DeclarationContext): Seq[ASTDeclaration] = origin(tree, tree match {
    case Declaration0(maybeContract, declSpecs, maybeInitDecls, _) =>
      val contract = getContract(convertValContract(maybeContract))
      val specs = new DeclSpecs
      specs.add(declSpecs)
      val baseType = getOrFail(declSpecs, specs.getType)
      val decls = maybeInitDecls.map(getDeclList).getOrElse(Seq())
      decls.map(decl => {
        val (initVal, innerDecl) = decl match {
          case InitDeclarator0(decl) => (None, decl)
          case InitDeclarator1(decl, "=", init) => (Some(expr(init)), decl)
        }
        val (direct, ptr) = innerDecl match {
          case Declarator0(maybePtr, decl, _) => (decl, maybePtr)
        }
        val t = convertPointer(ptr)(convertDeclaratorType(direct)(baseType))
        val name = convertDeclaratorName(direct)

        t match {
          case funcT: CFunctionType =>
            val ret = funcT.returnType
            val params = funcT.params.map(param => getOrFail(decl, param.asDecl,
              "Parameter name and types are both required, even in empty forward declarations."))
            create method_decl(ret, contract, name, params.toArray, null)
          case _ =>
            failIfDefined(maybeContract, "Contract not allowed in this place.")
            create field_decl(null, name, t, initVal.orNull)
        }
      })
    case Declaration1(staticAssert) =>
      ??(staticAssert)
  })

  def getDeclList(tree: InitDeclaratorListContext): Seq[InitDeclaratorContext] = tree match {
    case InitDeclaratorList0(x) => Seq(x)
    case InitDeclaratorList1(xs, ",", x) => getDeclList(xs) :+ x
  }

  /**
    * Wrangle the bag of words before a declaration into an intermediate data structure
    */
  class DeclSpecs {
    sealed trait TypeSpec
    case class PrimitiveTypeSpec(primitive: String) extends TypeSpec
    // Unsupported: case class AtomicTypeSpec(obj: Any) extends TypeSpec
    case class StructOrUnionTypeSpec(tree: StructOrUnionSpecifierContext) extends TypeSpec
    // Unsupported: case class EnumTypeSpec(obj: Any) extends TypeSpec
    case class TypedefNameTypeSpec(name: String) extends TypeSpec

    // Scala magic that can be safely ignored: needed to use bags ("multisets")
    private implicit val m1: HashedBagConfiguration[PrimitiveTypeSpec] = Bag.configuration.compact[PrimitiveTypeSpec]
    private implicit val m2: HashedBagConfiguration[TypeSpec] = Bag.configuration.compact[TypeSpec]
    private implicit val m3: mutable.HashedBagConfiguration[TypeSpec] = mutable.Bag.configuration.compact[TypeSpec]

    val primitiveTypeSets: Map[Bag[TypeSpec], PrimitiveSort] = Map(
      Bag[TypeSpec](PrimitiveTypeSpec("void"))
        -> PrimitiveSort.Void,
      Bag[TypeSpec](PrimitiveTypeSpec("char"))
        -> PrimitiveSort.Char,
      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("char"))
        -> PrimitiveSort.Char,
      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("char"))
        -> PrimitiveSort.Char,
      Bag[TypeSpec](PrimitiveTypeSpec("short"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("short"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("short"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("short"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("short"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("short"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("signed"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Bag[TypeSpec](PrimitiveTypeSpec("float"))
        -> PrimitiveSort.Float,
      Bag[TypeSpec](PrimitiveTypeSpec("double"))
        -> PrimitiveSort.Float,
      Bag[TypeSpec](PrimitiveTypeSpec("long"), PrimitiveTypeSpec("double"))
        -> PrimitiveSort.Float,
      Bag[TypeSpec](PrimitiveTypeSpec("_Bool"))
        -> PrimitiveSort.Boolean,
      // Unsupported: complex numbers
      //Set(PrimitiveTypeSpec("float"), PrimitiveTypeSpec("_Complex"))
      //  -> PrimitiveSort.Complex,
      //Set(PrimitiveTypeSpec("double"), PrimitiveTypeSpec("_Complex"))
      //  -> PrimitiveSort.Complex,
      //Set(PrimitiveTypeSpec("long"), PrimitiveTypeSpec("double"), PrimitiveTypeSpec("_Complex"))
      //  -> PrimitiveSort.Complex,
    )

    sealed trait TypeQual
    object ConstTypeQual extends TypeQual
    object RestrictTypeQual extends TypeQual
    object VolatileTypeQual extends TypeQual
    object AtomicTypeQual extends TypeQual

    sealed trait FuncSpec
    object InlineFuncSpec extends FuncSpec
    object NoReturnFuncSpec extends FuncSpec

    sealed trait StorageClass
    object Typedef extends StorageClass
    object ExternSC extends StorageClass
    object Static extends StorageClass
    object ThreadLocal extends StorageClass
    object Auto extends StorageClass
    object Register extends StorageClass

    object ThreadLocalStatic extends StorageClass
    object ThreadLocalExtern extends StorageClass

    private val _typeSpec: mutable.Bag[TypeSpec] = mutable.Bag()
    private val _typeQual: mutable.Set[TypeQual] = mutable.Set()
    private val _funcSpec: mutable.Set[FuncSpec] = mutable.Set()
    private var _storageClass: Option[StorageClass] = None

    def typeSpec: Bag[TypeSpec] = Bag(_typeSpec.toSeq:_*)
    def typeQual: Set[TypeQual] = _typeQual.toSet
    def funcSpec: Set[FuncSpec] = _funcSpec.toSet
    def storageClass: Option[StorageClass] = _storageClass

    def add(tree: DeclarationSpecifiersContext): Unit = tree match {
      case DeclarationSpecifiers0(specs) =>
        specs.foreach(add)
    }

    def add(tree: DeclarationSpecifierContext): Unit = tree match {
      case DeclarationSpecifier0(storageClass) => add(storageClass)
      case DeclarationSpecifier1(typeSpecifier) => add(typeSpecifier)
      case DeclarationSpecifier2(typeQualifier) => add(typeQualifier)
      case DeclarationSpecifier3(functionSpecifier) => add(functionSpecifier)
      case DeclarationSpecifier4(alignmentSpecifier) => ??(alignmentSpecifier)
    }

    def getStorageClass(tree: StorageClassSpecifierContext): StorageClass = tree match {
      case StorageClassSpecifier0("typedef") => Typedef
      case StorageClassSpecifier1("extern") => ExternSC
      case StorageClassSpecifier2("static") => Static
      case StorageClassSpecifier3("_Thread_local") => ThreadLocal
      case StorageClassSpecifier4("auto") => Auto
      case StorageClassSpecifier5("register") => Register
    }

    def add(tree: StorageClassSpecifierContext): Unit = {
      val cls = getStorageClass(tree)
      _storageClass = Some(_storageClass match {
        case None => cls
        case Some(ThreadLocal) if cls == ExternSC => ThreadLocalExtern
        case Some(ThreadLocal) if cls == Static => ThreadLocalStatic
        case Some(ExternSC) if cls == ThreadLocal => ThreadLocalExtern
        case Some(Static) if cls == ThreadLocal => ThreadLocalStatic
        case Some(other) => fail(tree, "Encountered storage class %s before, so cannot also declare as %s", other, cls)
      })
    }

    def add(tree: TypeSpecifierContext): Unit = tree match {
      case TypeSpecifier0(primitive) => _typeSpec += PrimitiveTypeSpec(primitive)
      case TypeSpecifier1("__extension__", _, _, _) => ??(tree)
      case TypeSpecifier2(atomic) => ??(atomic)
      case TypeSpecifier3(structOrUnion) => _typeSpec += StructOrUnionTypeSpec(structOrUnion)
      case TypeSpecifier4(enum) => ??(enum)
      case TypeSpecifier5(TypedefName0(id)) => _typeSpec += TypedefNameTypeSpec(convertID(id))
    }

    def add(tree: TypeQualifierContext): Unit = {
      _typeQual += (tree match {
        case TypeQualifier0("const") => ConstTypeQual
        case TypeQualifier1("restrict") => RestrictTypeQual
        case TypeQualifier2("volatile") => VolatileTypeQual
        case TypeQualifier3("_Atomic") => AtomicTypeQual
      })
    }

    def add(tree: FunctionSpecifierContext): Unit = {
      _funcSpec += (tree match {
        case FunctionSpecifier0(standardSpecifier) => standardSpecifier match {
          case "inline" => InlineFuncSpec
          case "_Noreturn" => NoReturnFuncSpec
          case "__inline__" => ??(tree)
          case "__stdcall" => ??(tree)
          case _ => ???
        }
        case FunctionSpecifier1(gccAttr) => ??(gccAttr)
        case FunctionSpecifier2("__declspec", _, _, _) => ??(tree)
      })
    }

    def getType: Either[String, Type] = {
      if (typeQual.nonEmpty) {
        return Left("Type qualifiers such as const are not supported.")
      }
      if (funcSpec.nonEmpty) {
        return Left("Function specifiers such as inline are not supported.")
      }


      val primitive = primitiveTypeSets.get(typeSpec) match {
        case None =>
          return Left("Type specifiers other than primitive types are not supported")
        case Some(t) =>
          create primitive_type t
      }

      storageClass match {
        case None => Right(primitive)
        case Some(Static) => Right(create.__static(primitive))
        case Some(ExternSC) => Right(create.__extern(primitive))
        case Some(sc) => Left(s"Storage class ${sc.getClass.getSimpleName} not supported")
      }
    }
  }

  def convertID(id: LangIdContext): String = id match {
    case LangId0(id) => convertID(id)
  }

  def convertIDName(id: LangIdContext): NameExpression = id match {
    case LangId0(id) => convertIDName(id)
  }

  def convertID(id: ClangIdentifierContext): String = id match {
    case ClangIdentifier0(reservedInSpec) =>
      fail(reservedInSpec, "This identifier is reserved, and may not be declared inside specifications.")
    case ClangIdentifier1(normalId) =>
      normalId
    case ClangIdentifier2(reservedOutSpec) =>
      convertOverlappingValReservedID(reservedOutSpec)
  }

  def convertIDName(tree: ClangIdentifierContext): NameExpression = origin(tree, tree match {
    case ClangIdentifier0(reservedInSpec) =>
      convertValReserved(reservedInSpec)
    case ClangIdentifier1("NULL") =>
      create reserved_name ASTReserved.Null
    case ClangIdentifier1(normalId) =>
      create unresolved_name normalId
    case ClangIdentifier2(reservedOutSpec) =>
      convertOverlappingValReservedName(reservedOutSpec)
  })

  def convertStat(statement: CompoundStatementContext): BlockStatement = origin(statement, statement match {
    case CompoundStatement0("{", maybeBlock, "}") =>
      create block(maybeBlock.map(convertStatList).getOrElse(Seq()):_*)
  })

  def convertStatList(block: BlockItemListContext): Seq[ASTNode] = block match {
    case BlockItemList0(x) => convertStat(x)
    case BlockItemList1(xs, x) => convertStatList(xs) ++ convertStat(x)
  }

  def convertStat(statement: BlockItemContext): Seq[ASTNode] = statement match {
    case BlockItem0(decl) =>
      convertDecl(decl)
    case BlockItem1(stat) =>
      Seq(convertStat(stat))
    case BlockItem2(valStat) =>
      convertValStat(valStat)
    case BlockItem3(valStat) =>
      Seq(convertValStat(valStat))
  }

  def convertStat(statement: StatementContext): ASTNode = origin(statement, statement match {
    case Statement0(labeled) => convertStat(labeled)
    case Statement1(compound) => convertStat(compound)
    case Statement2(expr) => convertStat(expr)
    case Statement3(selection) => convertStat(selection)
    case Statement4(iteration) => convertStat(iteration)
    case Statement5(jump) => convertStat(jump)
    case ext: Statement6Context => ??(ext)
  })

  def convertStat(labeled: LabeledStatementContext): ASTNode = labeled match {
    case LabeledStatement0(label, ":", statNode) =>
      val stat = convertStat(statNode)
      stat.addLabel(create label convertID(label))
      stat
    case LabeledStatement1("case", _, _, _) =>
      ??(labeled)
    case LabeledStatement2("default", _, _) =>
      ??(labeled)
  }

  def convertStat(exp: ExpressionStatementContext): ASTNode = exp match {
    case ExpressionStatement0(None, _) =>
      create block()
    case ExpressionStatement0(Some(exp), _) =>
      expr(exp)
  }

  def convertStat(selection: SelectionStatementContext): ASTNode = selection match {
    case SelectionStatement0("if", _, cond, _, whenTrue, maybeWhenFalse) =>
      create ifthenelse(expr(cond), convertStat(whenTrue), maybeWhenFalse match {
        case None => null
        case Some(ElseBranch0("else", stat)) => convertStat(stat)
      })
    case SelectionStatement1("switch", _, _, _, _) =>
      ??(selection)
  }

  def convertStat(iteration: IterationStatementContext): ASTNode = iteration match {
    case IterationStatement0(maybeContract1, "while", _, cond, _, maybeContract2, body) =>
      val contract = getContract(convertValContract(maybeContract1), convertValContract(maybeContract2))
      create while_loop(expr(cond), convertStat(body), contract)
    case IterationStatement1("do", _, "while", _, _, _, _) =>
      ??(iteration)
    case IterationStatement2(maybeContract1, "for", _, maybeInit, _, maybeCond, _, maybeUpdate, _, maybeContract2, body) =>
      val contract = getContract(convertValContract(maybeContract1), convertValContract(maybeContract2))
      create for_loop(
        maybeInit.map(expr).orNull,
        maybeCond.map(expr).orNull,
        maybeUpdate.map(expr).orNull,
        convertStat(body),
        contract
      )
    case IterationStatement3(maybeContract1, "for", _, init, maybeCond, _, maybeUpdate, _, maybeContract2, body) =>
      val contract = getContract(convertValContract(maybeContract1), convertValContract(maybeContract2))
      create for_loop(
        create block(convertDecl(init):_*),
        maybeCond.map(expr).orNull,
        maybeUpdate.map(expr).orNull,
        convertStat(body),
        contract
      )
  }

  def convertStat(jump: JumpStatementContext): ASTNode = jump match {
    case JumpStatement0("goto", label, _) =>
      create special(ASTSpecial.Kind.Goto, create label(convertID(label)))
    case JumpStatement1("continue", _) =>
      create special ASTSpecial.Kind.Continue
    case JumpStatement2("break", _) =>
      create special ASTSpecial.Kind.Break
    case JumpStatement3("return", maybeExp, _) =>
      create return_statement(maybeExp.map(expr).toSeq:_*)
    case JumpStatement4("goto", _, _) =>
      ??(jump) // GCC extended goto's unsupported
  }

  def expr(exp: LangExprContext): ASTNode = exp match {
    case LangExpr0(e) => expr(e)
  }

  def expr(exp: InitializerContext): ASTNode = exp match {
    case Initializer0(exp) => expr(exp)
    case Initializer1("{", _, "}") => ??(exp)
    case Initializer2("{", _, _, "}") => ??(exp)
  }

  def expr(exp: ExpressionContext): ASTNode = exp match {
    case Expression0(assign) =>
      expr(assign)
    case Expression1(expr, ",", assign) =>
      ??(exp) // comma operator unsupported
  }

  def expr(exp: AssignmentExpressionContext): ASTNode = origin(exp, exp match {
    case AssignmentExpression0(cond) => expr(cond)
    case AssignmentExpression1(target, ass@AssignmentOperator0(op), assign) =>
      val loc = expr(target)
      val value = expr(assign)
      val operator = op match {
        case "=" => StandardOperator.Assign
        case "*=" => MulAssign
        case "/=" => StandardOperator.DivAssign
        case "%=" => RemAssign
        case "+=" => AddAssign
        case "-=" => SubAssign
        case "<<=" => ShlAssign
        case ">>=" => ShrAssign // TODO: shift of negative (signed) values in C is not well-defined
        case "&=" => return create.assignment(loc, create.expression(BitAnd, loc, value))
        case "^=" => return create.assignment(loc, create.expression(BitXor, loc, value))
        case "|=" => return create.assignment(loc, create.expression(BitOr, loc, value))
      }
      create expression(operator, loc, value)
  })

  def expr(exp: ConditionalExpressionContext): ASTNode = origin(exp, exp match {
    case ConditionalExpression0(orExp) => expr(orExp)
    case ConditionalExpression1(cond, "?", whenTrue, ":", whenFalse) =>
      create expression(ITE, expr(cond), expr(whenTrue), expr(whenFalse))
  })

  def expr(exp: LogicalOrExpressionContext): ASTNode = origin(exp, exp match {
    case LogicalOrExpression0(andExp) => expr(andExp)
    case LogicalOrExpression1(left, "||", right) =>
      create expression(StandardOperator.Or, expr(left), expr(right))
    case LogicalOrExpression2(left, "==>", right) =>
      create expression(StandardOperator.Implies, expr(left), expr(right))
    case LogicalOrExpression3(left, "-*", right) =>
      create expression(Wand, expr(left), expr(right))
  })

  def expr(exp: LogicalAndExpressionContext): ASTNode = origin(exp, exp match {
    case LogicalAndExpression0(orExp) => expr(orExp)
    case LogicalAndExpression1(left, "&&", right) =>
      create expression(StandardOperator.And, expr(left), expr(right))
    case LogicalAndExpression2(left, "**", right) =>
      create expression(StandardOperator.Star, expr(left), expr(right))
  })

  def expr(exp: InclusiveOrExpressionContext): ASTNode = origin(exp, exp match {
    case InclusiveOrExpression0(exOr) => expr(exOr)
    case InclusiveOrExpression1(left, "|", right) =>
      create expression(BitOr, expr(left), expr(right))
  })

  def expr(exp: ExclusiveOrExpressionContext): ASTNode = origin(exp, exp match {
    case ExclusiveOrExpression0(andExp) => expr(andExp)
    case ExclusiveOrExpression1(left, "^", right) =>
      create expression(BitXor, expr(left), expr(right))
  })

  def expr(exp: AndExpressionContext): ASTNode = origin(exp, exp match {
    case AndExpression0(eq) => expr(eq)
    case AndExpression1(left, "&", right) =>
      create expression(BitAnd, expr(left), expr(right))
  })

  def expr(exp: EqualityExpressionContext): ASTNode = origin(exp, exp match {
    case EqualityExpression0(rel) => expr(rel)
    case EqualityExpression1(left, "==", right) =>
      create expression(StandardOperator.EQ, expr(left), expr(right))
    case EqualityExpression2(left, "!=", right) =>
      create expression(StandardOperator.NEQ, expr(left), expr(right))
  })

  def expr(exp: RelationalExpressionContext): ASTNode = origin(exp, exp match {
    case RelationalExpression0(shift) => expr(shift)
    case RelationalExpression1(left, "<", right) =>
      create expression(StandardOperator.LT, expr(left), expr(right))
    case RelationalExpression2(left, ">", right) =>
      create expression(StandardOperator.GT, expr(left), expr(right))
    case RelationalExpression3(left, "<=", right) =>
      create expression(StandardOperator.LTE, expr(left), expr(right))
    case RelationalExpression4(left, ">=", right) =>
      create expression(StandardOperator.GTE, expr(left), expr(right))
  })

  def expr(exp: ShiftExpressionContext): ASTNode = origin(exp, exp match {
    case ShiftExpression0(add) => expr(add)
    case ShiftExpression1(left, "<<", right) =>
      create expression(StandardOperator.LeftShift, expr(left), expr(right))
    case ShiftExpression2(left, ">>", right) =>
      create expression(StandardOperator.RightShift, expr(left), expr(right))
  })

  def expr(exp: AdditiveExpressionContext): ASTNode = origin(exp, exp match {
    case AdditiveExpression0(mult) => expr(mult)
    case AdditiveExpression1(left, "+", right) =>
      create expression(StandardOperator.Plus, expr(left), expr(right))
    case AdditiveExpression2(left, "-", right) =>
      create expression(StandardOperator.Minus, expr(left), expr(right))
  })

  def expr(exp: MultiplicativeExpressionContext): ASTNode = origin(exp, exp match {
    case MultiplicativeExpression0(cast) => expr(cast)
    case MultiplicativeExpression1(left, "*", right) =>
      create expression(StandardOperator.Mult, expr(left), expr(right))
    case MultiplicativeExpression2(left, "/", right) =>
      create expression(StandardOperator.FloorDiv, expr(left), expr(right))
    case MultiplicativeExpression3(left, "%", right) =>
      create expression(StandardOperator.Mod, expr(left), expr(right))
    case MultiplicativeExpression4(left, "\\", right) =>
      create expression(StandardOperator.Div, expr(left), expr(right))
  })

  def expr(exp: CastExpressionContext): ASTNode = origin(exp, exp match {
    case CastExpression0(unary) => expr(unary)
    case CastExpression1(_, _, _, _) =>
      ??(exp)
    case CastExpression2(_, _, _, _, _) =>
      ??(exp)
  })

  def expr(exp: UnaryExpressionContext): ASTNode = origin(exp, exp match {
    case UnaryExpression0(postfix) => expr(postfix)
    case UnaryExpression1("++", exp) =>
      create expression(PreIncr, expr(exp))
    case UnaryExpression2("--", exp) =>
      create expression(PreDecr, expr(exp))
    case UnaryExpression3(uop@UnaryOperator0(op), exp) =>
      val sop = op match {
        case "&" => AddrOf
        case "*" => Indirection
        case "+" => return expr(exp) // Technically incorrect, because now we accept &(+i) as AddrOf(Name("i"))
        case "-" => UMinus
        case "~" => BitNot
        case "!" => StandardOperator.Not
      }
      create expression(sop, expr(exp))
    case UnaryExpression4("sizeof", _) =>
      ??(exp)
    case UnaryExpression5("sizeof", _, _, _) =>
      ??(exp)
    case UnaryExpression6("_Alignof", _, _, _) =>
      ??(exp)
    case UnaryExpression7("&&", _) =>
      ??(exp)
  })

  def expr(exp: PostfixExpressionContext): ASTNode = origin(exp, exp match {
    case PostfixExpression0(primary) => expr(primary)
    case PostfixExpression1(arr, "[", idx, "]") =>
      create expression(Subscript, expr(arr), expr(idx))
    case PostfixExpression2(method, "(", maybeArgs, ")") =>
      val args = maybeArgs.map(exprList).getOrElse(Seq())
      val methodExpr = expr(method)

      if(!methodExpr.isInstanceOf[NameExpression]) {
        ??(method)
      }

      val methodName = methodExpr.asInstanceOf[NameExpression].getName
      create invokation(null, null, methodName, args:_*)
    case PostfixExpression3(obj, ".", field) =>
      create expression(StructSelect, expr(obj), convertIDName(field))
    case PostfixExpression4(obj, "->", field) =>
      create expression(StructDeref, expr(obj), convertIDName(field))
    case PostfixExpression5(exp, "++") =>
      create expression(PostIncr, expr(exp))
    case PostfixExpression6(exp, "--") =>
      create expression(PostDecr, expr(exp))
    case PostfixExpression7(_, _, _, _, _, _) =>
      ??(exp)
    case PostfixExpression8(_, _, _, _, _, _, _) =>
      ??(exp)
    case PostfixExpression9(_, _, _, _, _, _, _) =>
      ??(exp)
    case PostfixExpression10(_, _, _, _, _, _, _, _) =>
      ??(exp)
  })

  def expr(exp: PrimaryExpressionContext): ASTNode = origin(exp, exp match {
    case PrimaryExpression0(id) => convertIDName(id)
    case PrimaryExpression1(const) =>
      // Floats are also tokenized as this const, so we should distinguish here
      create constant const.toInt
    case PrimaryExpression2(strings) =>
      ??(exp)
    case PrimaryExpression3("(", exp, ")") => expr(exp)
    case PrimaryExpression4(genericSelection) =>
      ??(genericSelection)
    case PrimaryExpression5(_, _, _, _) =>
      ??(exp)
    case PrimaryExpression6("__builtin_va_arg", _, _, _, _, _) =>
      ??(exp)
    case PrimaryExpression7("__builtin_offsetof", _, _, _, _, _) =>
      ??(exp)
    case PrimaryExpression8(valPrimary) =>
      valExpr(valPrimary)
  })

  def exprList(tree: ArgumentExpressionListContext): Seq[ASTNode] = tree match {
    case ArgumentExpressionList0(x) => Seq(expr(x))
    case ArgumentExpressionList1(xs, ",", x) => exprList(xs) :+ expr(x)
  }

  def convertType(t: LangTypeContext): Type = t match {
    case LangType0(t) => convertType(t)
    case LangType1(valType) => convertValType(valType)
  }

  def convertType(t: TypeSpecifierContext): Type = {
    val specs = new DeclSpecs
    specs.add(t)
    getOrFail(t, specs.getType)
  }

  def convertModifier(mod: LangModifierContext): NameExpression = ???

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

  def convertValLabelList(args: ValLabelListContext): Seq[ASTNode] = args match {
    case ValLabelList0(label) =>
      Seq(create label(convertID(label)))
    case ValLabelList1(label, _, labels) =>
      (create label convertID(label)) +: convertValLabelList(labels)
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
  }

  def convertValBlock(block: ValBlockContext): BlockStatement = origin(block, block match {
    case ValBlock0("{", statements, "}") =>
      create block(statements.map(convertValStat):_*)
  })

  def convertValStat(stat: ValEmbedStatementBlockContext): Seq[ASTNode] = origin(stat, stat match {
    case ValEmbedStatementBlock0(_startSpec, stats, _endSpec) =>
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
      create special(ASTSpecial.Kind.Send, expr(res), create unresolved_name lbl, expr(thing))
    case ValStatement24(_recv, res, _from, lbl, _, thing, _) =>
      create special(ASTSpecial.Kind.Recv, expr(res), create unresolved_name(lbl), expr(thing))
    case ValStatement25(_transfer, exp, _) =>
      ??(stat)
    case ValStatement26(_csl_subject, obj, _) =>
      create special(ASTSpecial.Kind.CSLSubject, expr(obj))
    case ValStatement27(_spec_ignore, "}") =>
      create special ASTSpecial.Kind.SpecIgnoreEnd
    case ValStatement28(_spec_ignore, "{") =>
      create special ASTSpecial.Kind.SpecIgnoreStart
    case action: ValStatement29Context =>
      ??(action)
    case ValStatement30(_atomic, _, resList, _, stat) =>
      create csl_atomic(create block(convertValStat(stat):_*), resList.map(convertValLabelList).getOrElse(Seq()):_*)
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
      create expression(IndependentOf, expr(exp), create unresolved_name indepOf)
    case ValPrimary5("(", x, "\\memberof", xs, ")") =>
      create expression(Member, expr(x), expr(xs))
    case ValPrimary6("[", from, "..", to, ")") =>
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
    case ValPrimary15("\\id", "(", exp, ")") =>
      create expression(Identity, expr(exp))
    case ValPrimary16("\\typeof", "(", exp, ")") =>
      create expression(TypeOf, expr(exp))
    case ValPrimary17("\\matrix", "(", m, _, size0, _, size1, ")") =>
      create expression(ValidMatrix, expr(m), expr(size0), expr(size1))
    case ValPrimary18("\\array", "(", a, _, size0, ")") =>
      create expression(ValidArray, expr(a), expr(size0))
    case ValPrimary19("\\pointer", "(", p, _, size0, _, perm, ")") =>
      create expression(ValidPointer, expr(p), expr(size0), expr(perm))
    case ValPrimary20("\\pointer_index", "(", p, _, idx, _, perm, ")") =>
      create expression(ValidPointerIndex, expr(p), expr(idx), expr(perm))
    case ValPrimary21("\\values", "(", a, _, fr, _, to, ")") =>
      create expression(Values, expr(a), expr(fr), expr(to))
    case ValPrimary22("\\sum", "(", a, _, b, ")") =>
      create expression(FoldPlus, expr(a), expr(b))
    case ValPrimary23("\\vcmp", "(", a, _, b, ")") =>
      create expression(VectorCompare, expr(a), expr(b))
    case ValPrimary24("\\vrep", "(", v, ")") =>
      create expression(VectorRepeat, expr(v))
    case ValPrimary25("\\msum", "(", a, _, b, ")") =>
      create expression(MatrixSum, expr(a), expr(b))
    case ValPrimary26("\\mcmp", "(", a, _, b, ")") =>
      create expression(MatrixCompare, expr(a), expr(b))
    case ValPrimary27("\\mrep", "(", m, ")") =>
      create expression(MatrixRepeat, expr(m))
    case ValPrimary28("Reducible", "(", exp, _, "+", ")") =>
      create expression(ReducibleSum, expr(exp))
    case ValPrimary28("Reducible", "(", exp, _, "min", ")") =>
      create expression(ReducibleMin, expr(exp))
    case ValPrimary28("Reducible", "(", exp, _, "max", ")") =>
      create expression(ReducibleMax, expr(exp))
    case ValPrimary29(label, _, exp) =>
      val res = expr(exp)
      res.addLabel(create label(convertID(label)))
      res
  })

  def convertValReserved(reserved: ValReservedContext): NameExpression = origin(reserved, reserved match {
    case ValReserved0(_) =>
      fail(reserved, "This identifier is reserved and cannot be declared or used.")
    case ValReserved1("\\result") =>
      create reserved_name ASTReserved.Result
    case ValReserved2("\\current_thread") =>
      create reserved_name ASTReserved.CurrentThread
    case ValReserved3("none") =>
      create reserved_name ASTReserved.NoPerm
    case ValReserved4("write") =>
      create reserved_name ASTReserved.FullPerm
    case ValReserved5("read") =>
      create reserved_name ASTReserved.ReadPerm
    case ValReserved6("None") =>
      create reserved_name ASTReserved.OptionNone
    case ValReserved7("empty") =>
      create reserved_name ASTReserved.EmptyProcess
  })

  /**
   * This method allows a language grammar to step into the reserved identifiers where they overlap with the underlying
   * language, to allow their use there. They should be forbidden inside specifications.
   * @param reserved the reserved identifier
   * @return the string representation of the identifier
   */
  def convertOverlappingValReservedID(reserved: ValReservedContext): String = reserved match {
    case ValReserved0(s) => s
    case ValReserved1("\\result") => fail(reserved, "This identifier is invalid in the current language")
    case ValReserved2("\\current_thread") => fail(reserved, "This identifier is invalid in the current language")
    case ValReserved3(s) => s
    case ValReserved4(s) => s
    case ValReserved5(s) => s
    case ValReserved6(s) => s
    case ValReserved7(s) => s
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
  })

  def convertValDecl(decl: ValEmbedDeclarationBlockContext): Seq[ASTDeclaration] = decl match {
    case ValEmbedDeclarationBlock0(_, decls, _) =>
      decls.map((decl) => convertValDecl(decl))
  }

  def convertValWithThen(withThen: ValWithThenContext): ASTNode = withThen match {
    case ValWithThen0("with", _, mappings, _) =>
      create special(ASTSpecial.Kind.With, create block(mappings.map {
        case ValWithThenMapping0(name, _, exp, _) =>
          create assignment(convertIDName(name), expr(exp))
      }:_*))
    case ValWithThen1("then", _, mappings, _) =>
      create special(ASTSpecial.Kind.Then, create block(mappings.map {
        case ValWithThenMapping0(name, _, exp, _) =>
          create assignment(convertIDName(name), expr(exp))
      }:_*))
  }

  def convertValWithThen(withThen: ValEmbedWithThenBlockContext): Seq[ASTNode] = withThen match {
    case ValEmbedWithThenBlock0(_, mappings, _) => mappings.map(convertValWithThen)
  }

  def convertValWithThen(withThen: ValEmbedWithThenContext): Seq[ASTNode] = withThen match {
    case ValEmbedWithThen0(blocks) => blocks.flatMap(convertValWithThen)
  }
  /* === End of duplicated code block === */
}
