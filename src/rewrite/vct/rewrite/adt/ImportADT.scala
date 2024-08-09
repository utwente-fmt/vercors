package vct.col.rewrite.adt

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteProgram
import vct.col.ast.`type`.typeclass.TFloats
import vct.col.ast.util.Declarator
import vct.col.ast._
import vct.col.typerules.CoercingRewriter
import vct.col.rewrite.error.ExtraNode
import vct.col.origin.{Blame, SourceName, UnsafeCoercion}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, RewriterBuilderArg}

import scala.reflect.ClassTag

trait ImportADTImporter {
  def loadAdt[G](name: String): Program[G]
}

abstract class ImportADTBuilder(adt: String)
    extends RewriterBuilderArg[ImportADTImporter] {
  override def key: String = "adt" + adt.capitalize
  override def desc: String =
    s"Import types into vercors that are defined externally, usually via an axiomatic datatype. This pass imports $adt."
}

case object ImportADT {
  def typeText(t: Type[_]): String =
    t match {
      case _: TNotAValue[_] => throw ExtraNode
      case TVoid() => "void"
      case TBool() => "bool"
      case single: TFloat[_] if single == TFloats.ieee754_32bit => s"float32"
      case double: TFloat[_] if double == TFloats.ieee754_32bit => s"float64"
      case TFloat(exponent, mantissa) => s"float${exponent}m$mantissa"
      case TChar() => "char"
      case TString() => "string"
      case TRef() => "ref"
      case TArray(element) => "arr_" + typeText(element)
      case TPointer(element) => "ptr_" + typeText(element)
      case TUniquePointer(element, unique) => "unique_ptr_" + unique.toString + "_" + typeText(element)
      case TConstPointer(element) => "const_ptr_" + typeText(element)
      case TProcess() => "proc"
      case TModel(Ref(model)) => model.o.getPreferredNameOrElse().camel
      case TAxiomatic(Ref(adt), args) =>
        args match {
          case Nil => adt.o.getPreferredNameOrElse().camel
          case ts =>
            adt.o.getPreferredNameOrElse().camel + "$" + ts.map(typeText)
              .mkString("__") + "$"
        }
      case TOption(element) => "opt_" + typeText(element)
      case TTuple(elements) =>
        "tup$" + elements.map(typeText).mkString("__") + "$"
      case TEither(left, right) =>
        "either$" + typeText(left) + "__" + typeText(right) + "$"
      case TSeq(element) => "seq_" + typeText(element)
      case TSet(element) => "set_" + typeText(element)
      case TVector(size, element) =>
        "vector" + size.toString + "_" + typeText(element)
      case TBag(element) => "bag_" + typeText(element)
      case TMatrix(element) => "mat_" + typeText(element)
      case TType(t) => "typ_" + typeText(t)
      case TAny() => "top"
      case TAnyValue() => "any"
      case TNothing() => "nothing"
      case TNull() => "null"
      case TResource() => "res"
      case TInt() => "int"
      case TBoundedInt(gte, lt) => "int"
      case TRational() => "rat"
      case TFraction() => "fract"
      case TZFraction() => "zfract"
      case TMap(key, value) =>
        "map$" + typeText(key) + "__" + typeText(value) + "$"
      case TClass(Ref(cls), _) => cls.o.getPreferredNameOrElse().camel
      case TVar(Ref(v)) => v.o.getPreferredNameOrElse().camel
      case TUnion(ts) => "union" + ts.map(typeText).mkString("$", "__", "$")
      case SilverPartialTAxiomatic(Ref(adt), _) =>
        adt.o.getPreferredNameOrElse().camel
      case TAnyClass() => "cls"
      case TEnum(Ref(enum)) => enum.o.getPreferredNameOrElse().camel
      case TProverType(Ref(t)) => t.o.getPreferredNameOrElse().camel
      case TChoreography(Ref(prog)) => prog.o.getPreferredNameOrElse().camel
      case TPVLChoreography(Ref(prog)) => prog.o.getPreferredNameOrElse().camel
      case TSmtlibArray(index, value) =>
        "smtarr" + (index :+ value).map(typeText).mkString("$", "__", "$")
      case TSmtlibBitVector(size) => s"bitvec$size"
      case TSmtlibFloatingPoint(e, m) => s"fp_${e}_$m"
      case TSmtlibRegLan() => "reglan"
      case TSmtlibRoundingMode() => "roundingmode"
      case TSmtlibSeq(t) => "smtseq$" + typeText(t) + "$"
      case TSmtlibString() => "smtstr"
      case TVeyMontChannel(t) => "veymontchan$" + t + "$"
      case TEndpoint(Ref(thread)) => thread.o.getPreferredNameOrElse().camel
      case TResourceVal() => "resource"
      case _: JavaType[_] => throw ExtraNode
      case _: CType[_] => throw ExtraNode
      case _: CPPType[_] => throw ExtraNode
      case _: PVLType[_] => throw ExtraNode
      case _: SYCLTClass[_] => throw ExtraNode
    }
}

abstract class ImportADT[Pre <: Generation](importer: ImportADTImporter)
    extends CoercingRewriter[Pre] {
  val globalBlame: ScopedStack[Blame[UnsafeCoercion]] = ScopedStack()

  override def postCoerce(program: Program[Pre]): Program[Post] = {
    globalBlame.having(program.blame) {
      program.rewrite(declarations =
        globalDeclarations.collect { program.declarations.foreach(dispatch) }._1
      )
    }
  }

  protected def parse(name: String): Seq[GlobalDeclaration[Post]] = {
    val program = importer.loadAdt[Pre](name)
    program.declarations.foreach(dispatch)
    program.declarations
      .map(succProvider.globalDeclarationsSuccProvider.computeSucc).map(_.get)
  }

  protected def find[T](decls: Seq[Declaration[Post]], name: String)(
      implicit tag: ClassTag[T]
  ): T =
    decls.collectFirst {
      case decl: T if decl.o.find[SourceName].contains(SourceName(name)) => decl
    }.get

  protected def find[T](decls: Declarator[Post], name: String)(
      implicit tag: ClassTag[T]
  ): T = find(decls.declarations, name)(tag)
}
