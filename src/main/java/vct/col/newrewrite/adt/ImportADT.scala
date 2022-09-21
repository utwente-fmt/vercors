package vct.col.newrewrite.adt

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteProgram
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{CType, Declaration, GlobalDeclaration, JavaType, PVLType, Program, TAny, TArray, TAxiomatic, TBag, TBool, TBoundedInt, TChar, TClass, TEither, TFloat, TFraction, TInt, TMap, TMatrix, TModel, TNotAValue, TNothing, TNull, TOption, TPointer, TProcess, TRational, TRef, TResource, TSeq, TSet, TString, TTuple, TType, TUnion, TVar, TVoid, TZFraction, Type}
import vct.col.coerce.CoercingRewriter
import vct.col.newrewrite.error.ExtraNode
import vct.col.origin.{Blame, SourceNameOrigin, UnsafeCoercion}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, RewriterBuilderArg}

import scala.reflect.ClassTag

trait ImportADTImporter {
  def loadAdt[G](name: String): Program[G]
}

abstract class ImportADTBuilder(adt: String) extends RewriterBuilderArg[ImportADTImporter] {
  override def key: String = "adt" + adt.toUpperCase
  override def desc: String = s"Import types into vercors that are defined externally, usually via an axiomatic datatype. This pass imports $adt."
}

case object ImportADT {
  def typeText(t: Type[_]): String = t match {
    case _: TNotAValue[_] => throw ExtraNode
    case TVoid() => "void"
    case TBool() => "bool"
    case TFloat() => "float"
    case TChar() => "char"
    case TString() => "string"
    case TRef() => "ref"
    case TArray(element) => "arr_" + typeText(element)
    case TPointer(element) => "ptr_" + typeText(element)
    case TProcess() => "proc"
    case TModel(Ref(model)) => model.o.preferredName
    case TAxiomatic(Ref(adt), args) => args match {
      case Nil => adt.o.preferredName
      case ts => adt.o.preferredName + "$" + ts.map(typeText).mkString("__") + "$"
    }
    case TOption(element) => "opt_" + typeText(element)
    case TTuple(elements) => "tup$" + elements.map(typeText).mkString("__") + "$"
    case TEither(left, right) => "either$" + typeText(left) + "__" + typeText(right) + "$"
    case TSeq(element) => "seq_" + typeText(element)
    case TSet(element) => "set_" + typeText(element)
    case TBag(element) => "bag_" + typeText(element)
    case TMatrix(element) => "mat_" + typeText(element)
    case TType(t) => "typ_" + typeText(t)
    case TAny() => "any"
    case TNothing() => "nothing"
    case TNull() => "null"
    case TResource() => "res"
    case TInt() => "int"
    case TBoundedInt(gte, lt) => "int"
    case TRational() => "rat"
    case TFraction() => "fract"
    case TZFraction() => "zfract"
    case TMap(key, value) => "map$" + typeText(key) + "__" + typeText(value) + "$"
    case TClass(Ref(cls)) => cls.o.preferredName
    case TVar(Ref(v)) => v.o.preferredName
    case TUnion(ts) => "union$" + ts.map(typeText).mkString("__") + "$"
    case _: JavaType[_] => throw ExtraNode
    case _: CType[_] => throw ExtraNode
    case _: PVLType[_] => throw ExtraNode
  }
}

abstract class ImportADT[Pre <: Generation](importer: ImportADTImporter) extends CoercingRewriter[Pre] {
  val globalBlame: ScopedStack[Blame[UnsafeCoercion]] = ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Post] = {
    globalBlame.having(program.blame) {
      program.rewrite(declarations = globalDeclarations.collect {
        program.declarations.foreach(dispatch)
      }._1)
    }
  }

  protected def parse(name: String): Seq[GlobalDeclaration[Post]] = {
    val program = importer.loadAdt[Pre](name)
    program.declarations.foreach(dispatch)
    program.declarations.map(succProvider.computeSucc).map(_.get)
  }

  protected def find[T](decls: Seq[Declaration[Post]], name: String)(implicit tag: ClassTag[T]): T =
    decls.collectFirst {
      case decl: T if decl.o.isInstanceOf[SourceNameOrigin] && decl.o.asInstanceOf[SourceNameOrigin].name == name =>
        decl
    }.get

  protected def find[T](decls: Declarator[Post], name: String)(implicit tag: ClassTag[T]): T =
    find(decls.declarations, name)(tag)
}
