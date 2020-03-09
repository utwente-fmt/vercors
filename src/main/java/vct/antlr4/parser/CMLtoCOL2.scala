package vct.antlr4.parser

import hre.lang.System._
import org.antlr.v4.runtime.CommonTokenStream
import vct.antlr4.generated.CParser
import vct.antlr4.generated.CParser._
import vct.antlr4.generated.CParserPatterns._
import vct.col.ast.`type`.PrimitiveSort
import vct.col.ast.stmt.decl.{ASTDeclaration, ProgramUnit}

import scala.collection.immutable.{Bag, HashedBagConfiguration}
import scala.collection.mutable

object CMLtoCOL2 {
  def convert(tree: CompilationUnitContext, file_name: String, tokens: CommonTokenStream, parser: CParser): ProgramUnit = {
    new CMLtoCOL2(file_name, tokens, parser).convertProgram(tree)
  }
}

class CMLtoCOL2(fileName: String, tokens: CommonTokenStream, parser: CParser)
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

  def convertDecl(tree: FunctionDefinitionContext): Seq[ASTDeclaration] = tree match {
    case FunctionDefinition0(_, _, Some(declList), _) => ??(declList)
    case FunctionDefinition0(declSpecs, declarator, None, statement) =>
      val specs = new DeclSpecs
      specs.add(declSpecs)
      if (specs.typeQual.nonEmpty) {
        fail(declSpecs, "Type qualifiers such as const are not supported.")
      }
      if (specs.funcSpec.nonEmpty) {
        fail(declSpecs, "Function specifiers such as inline are not supported.")
      }
      if (specs.storageClass.nonEmpty) {
        fail(declSpecs, "Storage class declarations such as static or auto are not supported")
      }

      val baseRetType = specs.primitiveTypeSets.get(specs.typeSpec) match {
        case None =>
          fail(declSpecs, "Type specifiers other than primitive types are not supported")
        case Some(t) =>
          t
      }

      Seq()
  }

  def convertDecl(tree: DeclarationContext): Seq[ASTDeclaration] = ???

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
  }

  def convertID(id: ClangIdentifierContext): String = ???
}
