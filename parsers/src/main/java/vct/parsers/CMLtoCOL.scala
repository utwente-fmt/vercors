package vct.parsers

import scala.annotation.nowarn
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
import vct.antlr4.generated.CParser
import vct.antlr4.generated.CParser._
import vct.antlr4.generated.CParserPatterns._
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, Type}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.{NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.langspecific.c._
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl.{ASTDeclaration, ASTSpecial, Contract, DeclarationStatement, Method, ProgramUnit, SignalsClause}
import vct.col.ast.util.ContractBuilder
import vct.col.ast.util.SequenceUtils

import scala.collection.mutable
import java.util

object CMLtoCOL {
  def convert(tree: CompilationUnitContext, fileName: String, tokens: CommonTokenStream, parser: CParser): ProgramUnit = {
    new CMLtoCOL(fileName, tokens, parser).convertProgram(tree)
  }
}

// Maybe we can turn this off in the future.
@nowarn("msg=not.*?exhaustive")
class CMLtoCOL(fileName: String, tokens: CommonTokenStream, parser: CParser)
  extends ToCOL(fileName, tokens, parser)
{
  def convertProgram(tree: CompilationUnitContext): ProgramUnit = tree match {
    case CompilationUnit0(None, _) =>
      new ProgramUnit()
    case CompilationUnit0(Some(units), _) =>
      val result = new ProgramUnit()
      convertTranslationUnits(units).map {
        case field: DeclarationStatement =>
          field.setStatic(true)
          field
        case other => other
      }.foreach(result.add)
      result
  }

  def convertTranslationUnits(tree: TranslationUnitContext): Seq[ASTDeclaration] = tree match {
    case TranslationUnit0(decl) => convertDecl(decl)
    case TranslationUnit1(units, decl) => convertTranslationUnits(units) ++ convertDecl(decl)
  }

  def convertDecl(decl: LangDeclContext): ASTDeclaration = decl match {
    case LangDecl0(funcDecl) =>
      val xs = convertDecl(funcDecl)
      if(xs.size == 1) {
        xs.head
      } else {
        ??(decl)
      }
  }

  def convertDecl(tree: ExternalDeclarationContext): Seq[ASTDeclaration] = tree match {
    case ExternalDeclaration0(funcDecl) => convertDecl(funcDecl)
    case ExternalDeclaration1(decl) => convertDecl(decl)
    case ExternalDeclaration2(valDecls) => convertValDecl(valDecls)
    case ExternalDeclaration3(";") => Seq()
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
      val varargs = decls.nonEmpty && decls.last.`type`.isPrimitive(PrimitiveSort.CVarArgs)
      val res = create method_kind (Method.Kind.Plain, t.returnType, contract, name, decls.toArray, varargs, body)
      res.setStatic(true)
      specs.valModifiers.foreach(res.attach(_))
      Seq(res)
  })

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
        ParamSpec(Some(create primitive_type PrimitiveSort.CVarArgs), Some("..."))
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
          case InitDeclarator1(decl, "=", init) => (Some(init), decl)
        }
        val (direct, ptr) = innerDecl match {
          case Declarator0(maybePtr, decl, _) => (decl, maybePtr)
        }
        val t = convertDeclaratorType(direct)(convertPointer(ptr)(baseType))
        val name = convertDeclaratorName(direct)

        t match {
          case funcT: CFunctionType =>
            val ret = funcT.returnType
            val params = funcT.params.map(param => getOrFail(decl, param.asDecl,
              "Parameter name and types are both required, even in empty forward declarations."))
            val varargs = params.nonEmpty && params.last.`type`.isPrimitive(PrimitiveSort.CVarArgs)
            val res = create method_kind(Method.Kind.Plain, ret, contract, name, params.toArray, varargs, null)
            res.setStatic(true)
            specs.valModifiers.foreach(res.attach(_))
            res
          case _ =>
            failIfDefined(maybeContract, "Contract not allowed in this place.")
            create field_decl(null, name, t, initVal.map(convertInitializer(_, t)).orNull)
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
    case class ValTypeSpec(t: Type) extends TypeSpec

    object TypeSpecOrdering extends Ordering[TypeSpec] {
      override def compare(x: TypeSpec, y: TypeSpec): Int = (x, y) match {
        case (_, _) if x eq y => 0
        // Not supported, so the ordering doesn't matter currently anyway
        case (TypedefNameTypeSpec(l), TypedefNameTypeSpec(r)) => l.compare(r)
        case (PrimitiveTypeSpec(l), PrimitiveTypeSpec(r)) => l.compare(r)
        // Rest of the ordering, randomly chosen: Primitive < Typedef < ValType < StructOrUnion
        case (PrimitiveTypeSpec(_), _) => -1
        case (_, PrimitiveTypeSpec(_)) => 1
        case (TypedefNameTypeSpec(_), _) => -1
        case (_, TypedefNameTypeSpec(_)) => 1
        // Not clear how to even order these for arbitrary COL types or arbitrary structOrUnions
        // But: could be extended for non-anonymous structs
        case (_, _) => 0
      }
    }

    val primitiveTypeSets: Map[Seq[TypeSpec], PrimitiveSort] = Map(
      Seq(PrimitiveTypeSpec("void"))
        -> PrimitiveSort.Void,
      Seq(PrimitiveTypeSpec("char"))
        -> PrimitiveSort.Char,
      Seq(PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("char"))
        -> PrimitiveSort.Char,
      Seq(PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("char"))
        -> PrimitiveSort.Char,
      Seq(PrimitiveTypeSpec("short"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("short"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("short"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("short"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("short"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("short"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("signed"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("unsigned"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
        -> PrimitiveSort.Integer,
      Seq(PrimitiveTypeSpec("float"))
        -> PrimitiveSort.Float,
      Seq(PrimitiveTypeSpec("double"))
        -> PrimitiveSort.Float,
      Seq(PrimitiveTypeSpec("long"), PrimitiveTypeSpec("double"))
        -> PrimitiveSort.Float,
      Seq(PrimitiveTypeSpec("_Bool"))
        -> PrimitiveSort.Boolean,

      // Unsupported: complex numbers
      //Set(PrimitiveTypeSpec("float"), PrimitiveTypeSpec("_Complex"))
      //  -> PrimitiveSort.Complex,
      //Set(PrimitiveTypeSpec("double"), PrimitiveTypeSpec("_Complex"))
      //  -> PrimitiveSort.Complex,
      //Set(PrimitiveTypeSpec("long"), PrimitiveTypeSpec("double"), PrimitiveTypeSpec("_Complex"))
      //  -> PrimitiveSort.Complex,
//    ).map{ case (typespecs, sort) => (typespecs.sortWith{ case (l, r) => l.primitive < r.primitive}, sort) }.toMap
    ).map{ case (typeSpecs, sort) => (typeSpecs.sorted(TypeSpecOrdering), sort) }.toMap

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

    private val _typeSpec: mutable.ArrayBuffer[TypeSpec] = mutable.ArrayBuffer()
    private val _typeQual: mutable.Set[TypeQual] = mutable.Set()
    private val _funcSpec: mutable.Set[FuncSpec] = mutable.Set()
    private var _storageClass: Option[StorageClass] = None
    var valModifiers: mutable.Seq[NameExpression] = mutable.Seq()
    var isKernel: Boolean = false

    def typeSpec: Seq[TypeSpec] = _typeSpec.toSeq
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
      case DeclarationSpecifier5(_) => isKernel = true
      case DeclarationSpecifier6(valModifiersNode) =>
        valModifiers ++= convertValModifiers(valModifiersNode)
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
      case TypeSpecifier2(valType) => _typeSpec += ValTypeSpec(convertValType(valType))
      case TypeSpecifier3(atomic) => ??(atomic)
      case TypeSpecifier4(structOrUnion) => _typeSpec += StructOrUnionTypeSpec(structOrUnion)
      case TypeSpecifier5(enum) => ??(enum)
      case TypeSpecifier6(TypedefName0(id)) => _typeSpec += TypedefNameTypeSpec(convertID(id))
      case TypeSpecifier7("__typeof__", _, _, _) => ??(tree)
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

      if(typeSpec.size == 1) {
        typeSpec.head match {
          case ValTypeSpec(t) => return Right(t)
          case _ =>
        }
      }

      val primitive = primitiveTypeSets.get(typeSpec.sorted(TypeSpecOrdering)) match {
        case None =>
          return Left("Type specifiers other than primitive types are not supported")
        case Some(t) =>
          create primitive_type t
      }

      val sc = storageClass match {
        case None => primitive
        case Some(Static) => create.__static(primitive)
        case Some(ExternSC) => create.__extern(primitive)
        case Some(sc) => return Left(s"Storage class ${sc.getClass.getSimpleName} not supported")
      }

      Right(if(isKernel) {
        create.__kernel(sc)
      } else {
        sc
      })
    }
  }

  def convertID(id: LangIdContext): String = id match {
    case LangId0(id) => convertID(id)
  }

  def convertIDName(id: LangIdContext): ASTNode = id match {
    case LangId0(id) => convertIDName(id)
  }

  def convertID(id: ClangIdentifierContext): String = id match {
    case ClangIdentifier0(ValReserved1(s)) => s.substring(1, s.length-1)
    case ClangIdentifier0(reservedInSpec) =>
      fail(reservedInSpec, "This identifier is reserved, and may not be declared inside specifications.")
    case ClangIdentifier1(normalId) =>
      normalId
    case ClangIdentifier2(reservedOutSpec) =>
      convertOverlappingValReservedID(reservedOutSpec)
  }

  def convertIDName(tree: ClangIdentifierContext): ASTNode = origin(tree, tree match {
    case ClangIdentifier0(reservedInSpec) =>
      convertValReserved(reservedInSpec)
    case ClangIdentifier1("NULL") =>
      create reserved_name ASTReserved.Null
    case ClangIdentifier1(normalId) =>
      create unresolved_name normalId
    case ClangIdentifier2(reservedOutSpec) =>
      convertOverlappingValReservedName(reservedOutSpec)
  })

  def convertStat(statement: CompoundStatementContext): ASTNode = origin(statement, statement match {
    case CompoundStatement0("{", maybeBlock, "}") =>
      create block(maybeBlock.map(convertStatList).getOrElse(Seq()):_*)
    case CompoundStatement1(ompPragma, "{", maybeContract, maybeBlock, "}") =>
      val block = origin(statement, create block(maybeBlock.map(convertStatList).getOrElse(Seq()):_*))
      convertOmpBlock(ompPragma, block, getContract(convertValContract(maybeContract)))
  })

  def convertStatList(block: BlockItemListContext): Seq[ASTNode] = block match {
    case BlockItemList0(x) => convertStat(x)
    case BlockItemList1(xs, x) => convertStatList(xs) ++ convertStat(x)
  }

  def convertStat(statement: BlockItemContext): Seq[ASTNode] = origin(statement, statement match {
    case BlockItem0(decl) =>
      convertDecl(decl)
    case BlockItem1(stat) =>
      Seq(convertStat(stat))
    case BlockItem2(valStat) =>
      convertValStat(valStat)
    case BlockItem3(valStat) =>
      Seq(convertValStat(valStat))
    case BlockItem4(GpgpuLocalBarrier0(maybeContract, _, _, _, _)) =>
      Seq(create.barrier("group_block", getContract(convertValContract(maybeContract)), new util.ArrayList[String](), null))
    case BlockItem5(GpgpuGlobalBarrier0(maybeContract, _, _, _, _)) =>
      Seq(create.barrier("kernel_block", getContract(convertValContract(maybeContract)), new util.ArrayList[String](), null))
    case BlockItem6(GpgpuAtomicBlock0(_, block, maybeWithThen)) =>
      val atomic = create.parallel_atomic(convertStat(block).asInstanceOf[BlockStatement], "__vercors_kernel_invariant__")
      maybeWithThen.map(convertValWithThen).foreach(_.foreach(atomic.get_after.addStatement(_)))
      Seq(atomic)
  })

  def convertStat(statement: StatementContext): ASTNode = origin(statement, statement match {
    case Statement0(labeled) => convertStat(labeled)
    case Statement1(compound) => convertStat(compound)
    case Statement2(expr) => convertStat(expr)
    case Statement3(selection) => convertStat(selection)
    case Statement4(iteration) => convertStat(iteration)
    case Statement5(jump) => convertStat(jump)
    case ext: Statement6Context => ??(ext)
  })

  def convertOmpBlock(pragma: OmpBlockPragmaContext, block: BlockStatement, contract: Contract): ASTNode = origin(pragma, pragma match {
    case OmpBlockPragma0("parallel", options) => OMPParallel(block, options.map(convertOmpOption), contract)
    case OmpBlockPragma1("section") => OMPSection(block)
    case OmpBlockPragma2("sections") => OMPSections(block)
  })

  def convertOmpLoop(pragma: OmpLoopPragmaContext, loop: LoopStatement): ASTNode = origin(pragma, pragma match {
    case OmpLoopPragma0("for", options) => OMPFor(loop, options.map(convertOmpOption))
    case OmpLoopPragma1("parallel", "for", options) => OMPParallelFor(loop, options.map(convertOmpOption))
    case OmpLoopPragma2("for", "simd", options) => OMPForSimd(loop, options.map(convertOmpOption))
  })

  def convertOmpOption(option: OmpOptionContext): OMPOption = option match {
    case OmpOption0("nowait") => OMPNoWait
    case OmpOption1("private", "(", ids, ")") => OMPPrivate(convertOmpIdList(ids))
    case OmpOption2("shared", "(", ids, ")") => OMPShared(convertOmpIdList(ids))
    case OmpOption3("schedule", "(", "static", ")") => OMPSchedule(OMPStatic)
    case OmpOption4("simdlen", "(", len, ")") => OMPSimdLen(Integer.parseInt(len))
    case OmpOption5("num_threads", "(", n, ")") => OMPNumThreads(Integer.parseInt(n))
    case OmpOption6("reduction", "(", reductionOp, ":", ids, ")") => ??(option)
  }

  def convertOmpIdList(tree: OmpIdListContext): Seq[String] = tree match {
    case OmpIdList0(x) => Seq(x)
    case OmpIdList1(x, _, xs) => x +: convertOmpIdList(xs)
  }

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
      create.block()
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
    case IterationStatement2(maybeContract1, maybeOmp, "for", _, maybeInit, _, maybeCond, _, maybeUpdate, _, maybeContract2, body) =>
      val contract = getContract(convertValContract(maybeContract1), convertValContract(maybeContract2))
      val loop = create for_loop(
        maybeInit.map(expr).orNull,
        maybeCond.map(expr).orNull,
        maybeUpdate.map(expr).orNull,
        convertStat(body),
        contract
      )
      maybeOmp match {
        case None => loop
        case Some(pragma) => convertOmpLoop(pragma, loop)
      }
    case IterationStatement3(maybeContract1, maybeOmp, "for", _, init, maybeCond, _, maybeUpdate, _, maybeContract2, body) =>
      val contract = getContract(convertValContract(maybeContract1), convertValContract(maybeContract2))
      val loop = create for_loop(
        create block(convertDecl(init):_*),
        maybeCond.map(expr).orNull,
        maybeUpdate.map(expr).orNull,
        convertStat(body),
        contract
      )
      maybeOmp match {
        case None => loop
        case Some(pragma) => convertOmpLoop(pragma, loop)
      }
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

  def convertInitializer(init: InitializerContext, t: Type): ASTNode = origin(init, init match {
    case Initializer0("{", xs, "}") =>
      convertInitializerList(xs, t)
    case Initializer1("{", xs, _, "}") =>
      convertInitializerList(xs, t)
    case Initializer2(exp) => expr(exp)
  })

  def convertInitializerList(xs: InitializerListContext, t: Type): ASTNode = {
    val seqInfo = SequenceUtils.getTypeInfoOrFail(t, "Array initializer is only applicable to array type")
    val value = create struct_value(seqInfo.getSequenceType, null, convertInitializerListToSeq(xs, seqInfo.getElementType):_*)
    if(seqInfo.isOpt) {
      create expression(StandardOperator.OptionSome, value)
    } else {
      value
    }
  }

  def convertInitializerListToSeq(xs: InitializerListContext, t: Type): Seq[ASTNode] = origin(xs, xs match {
    case InitializerList0(designation, x) =>
      failIfDefined(designation, "This designation is syntactically correct, but not supported by VerCors")
      Seq(convertInitializer(x, t))
    case InitializerList1(xs, _, designation, x) =>
      failIfDefined(designation, "This designation is syntactically correct, but not supported by VerCors")
      convertInitializerListToSeq(xs, t) :+ convertInitializer(x, t)
  })

  def expr(exp: LangExprContext): ASTNode = exp match {
    case LangExpr0(e) => expr(e)
  }

  def expr(exp: InitializerContext): ASTNode = exp match {
    case Initializer0("{", _, "}") => ??(exp)
    case Initializer1("{", _, _, "}") => ??(exp)
    case Initializer2(exp) => expr(exp)
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
        case "/=" => StandardOperator.FloorDivAssign
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
    case LogicalOrExpression1(left, LogicalOrOp0("||"), right) =>
      create expression(StandardOperator.Or, expr(left), expr(right))
    case LogicalOrExpression1(left, LogicalOrOp1(valOp), right) =>
      create expression(convertValOp(valOp), expr(left), expr(right))
  })

  def expr(exp: LogicalAndExpressionContext): ASTNode = origin(exp, exp match {
    case LogicalAndExpression0(orExp) => expr(orExp)
    case LogicalAndExpression1(left, LogicalAndOp0("&&"), right) =>
      create expression(StandardOperator.And, expr(left), expr(right))
    case LogicalAndExpression1(left, LogicalAndOp1(valOp), right) =>
      create expression(convertValOp(valOp), expr(left), expr(right))
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
    case MultiplicativeExpression1(left, MultiplicativeOp0("*"), right) =>
      create expression(StandardOperator.Mult, expr(left), expr(right))
    case MultiplicativeExpression1(left, MultiplicativeOp1("/"), right) =>
      create expression(StandardOperator.FloorDiv, expr(left), expr(right))
    case MultiplicativeExpression1(left, MultiplicativeOp2("%"), right) =>
      create expression(StandardOperator.Mod, expr(left), expr(right))
    case MultiplicativeExpression1(left, MultiplicativeOp3(valOp), right) =>
      create expression(convertValOp(valOp), expr(left), expr(right))
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
    case PostfixExpression11(GpgpuCudaKernelInvocation0(name, _, blockCount, _, threadCount, _, _, arguments, _, maybeWithThen)) =>
      val invocation = create.kernelInvocation(convertID(name), expr(blockCount), expr(threadCount), exprList(arguments):_*)
      maybeWithThen.toSeq.flatMap(convertValWithThen).foreach(invocation.get_after.addStatement(_))
      invocation
  })

  def expr(exp: PrimaryExpressionContext): ASTNode = origin(exp, exp match {
    case PrimaryExpression0(valPrimary) =>
      valExpr(valPrimary)
    case PrimaryExpression1("true") =>
      create constant true
    case PrimaryExpression2("false") =>
      create constant false
    case PrimaryExpression3(id) => convertIDName(id)
    case PrimaryExpression4(const) =>
      // Floats are also tokenized as this const, so we should distinguish here
      create constant const.toInt
    case PrimaryExpression5(strings) =>
      // Pretty sure this completely ignores escape sequences, but we don't support strings anyway...
      // See also JavaJMLtoCOL Literal3
      create constant strings.mkString("")
    case PrimaryExpression6("(", exp, ")") => expr(exp)
    case PrimaryExpression7(genericSelection) =>
      ??(genericSelection)
    case PrimaryExpression8(_, _, _, _) =>
      ??(exp)
    case PrimaryExpression9("__builtin_va_arg", _, _, _, _, _) =>
      ??(exp)
    case PrimaryExpression10("__builtin_offsetof", _, _, _, _, _) =>
      ??(exp)
  })

  def exprList(tree: ArgumentExpressionListContext): Seq[ASTNode] = tree match {
    case ArgumentExpressionList0(x) => Seq(expr(x))
    case ArgumentExpressionList1(xs, ",", x) => exprList(xs) :+ expr(x)
  }

  def convertType(t: LangTypeContext): Type = t match {
    case LangType0(t) => convertType(t)
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
    case ValDeclaration1("axiom", name, _, body, _) =>
      expr(body) match {
        case opExpr: OperatorExpression =>
          if (opExpr.isa(StandardOperator.EQ)) {
            create axiom(convertID(name), create expression(StandardOperator.EQ, opExpr.arg(0), opExpr.arg(1)))
          } else {
            fail(body, "Body of axiom must be of form \"a == b\"")
          }
        case _ =>
          fail(body, "Body of axiom must be of form \"a == b\"")
      }
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
