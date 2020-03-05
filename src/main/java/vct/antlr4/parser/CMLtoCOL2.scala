package vct.antlr4.parser

import org.antlr.v4.runtime.{BufferedTokenStream, Parser}
import org.antlr.v4.runtime.tree.ParseTree
import vct.antlr4.generated.CParser.{StorageClassSpecifierContext, TypeQualifierContext}
import vct.antlr4.generated.CParserPatterns._
import vct.col.ast.`type`.{Type, TypeExpression}
import vct.col.ast.stmt.decl.{DeclarationStatement, ProgramUnit}
import vct.col.ast.`type`.TypeOperator._

object CMLtoCOL2 {
  def convert_pu(tree: ParseTree, file_name: String, tokens: BufferedTokenStream, parser: Parser): ProgramUnit = {
    convert(tree)
    new ProgramUnit()
  }

  def convert(tree: ParseTree): Seq[DeclarationStatement] = {
    tree match {
      case CompilationUnit0(Some(translationUnit), _) =>
        convert(translationUnit)

      case TranslationUnit0(externalDecl) =>
        convertDecl(externalDecl)
      case TranslationUnit1(translationUnit, externalDecl) =>
        convert(translationUnit) ++ convertDecl(externalDecl)
    }
  }

  def convertDecl(tree: ParseTree): Seq[DeclarationStatement] = {
    tree match {
      case ExternalDeclaration0(
        FunctionDefinition0(declSpecs, declarator, maybeDeclList, compoundStmt)) =>
        ???
      case ExternalDeclaration1(Declaration0(declSpecs, maybeInitDecls, _)) =>
        ???
      case ExternalDeclaration1(Declaration1(staticAssertDeclaration)) =>
        ???
      case ExternalDeclaration2(";") => Seq()
    }
  }

  def convertType(tree: ParseTree): Type = tree match {
    case DeclarationSpecifiers0(declSpecs) =>
      val op = declSpecs.init.map(convertTypeOperator).reduce((a, b) => a compose b)
      val t = convertType(declSpecs.last)
      op(t)
    case DeclarationSpecifiers20(declSpecs) =>
      val op = declSpecs.init.map(convertTypeOperator).reduce((a, b) => a compose b)
      val t = convertType(declSpecs.last)
      op(t)
  }

  def convertTypeOperator(tree: ParseTree): (Type => Type) = tree match {
    case DeclarationSpecifier0(storageClass) =>
      convertTypeOperator(storageClass)
    case DeclarationSpecifier1(_) =>
      // typeSpecifier cannot occur in non-last place
      ???
    case DeclarationSpecifier2(typeQualifier) =>
      convertTypeOperator(typeQualifier)
    case DeclarationSpecifier3(functionSpecifier) =>
      convertTypeOperator(functionSpecifier)
    case DeclarationSpecifier4(alignmentSpecifier) =>
      convertTypeOperator(alignmentSpecifier)

    case _: StorageClassSpecifierContext | _: TypeQualifierContext =>
      val operator = tree match {
        case StorageClassSpecifier0("typedef") => ???
        case StorageClassSpecifier1("extern") => Extern
        case StorageClassSpecifier2("static") => Static
        case StorageClassSpecifier3("_Thread_local") => ???
        case StorageClassSpecifier4("auto") => ???
        case StorageClassSpecifier5("register") => ???

        case TypeQualifier0("const") => Const
        case TypeQualifier1("restrict") => ???
        case TypeQualifier2("volatile") => ???
        case TypeQualifier3("_Atomic") => ???
      }
      t => TypeExpression(operator, List(t))
  }
}
