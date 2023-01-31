package vct.col.rewrite

import vct.col.ast.{ADTFunction, ADTFunctionInvocation, ApplicableRef, Class, ClassDeclaration, Declaration, Expr, Function, InstanceFunction, InstanceFunctionInvocation, StringClassIntern, Program, StringClass, StringClassConcat, StringClassGetData, StringValue, TClass, TStringClass, ThisObject, ThisStringClass, Type}
import vct.col.origin.{DiagnosticOrigin, Origin, PanicBlame}
import vct.col.ref.Ref
import vct.col.resolve.ctx.{RefInstanceFunction, Referrable}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

case object EncodeStringClass extends RewriterBuilder {
  override def key: String = "encodeStringClass"

  override def desc: String = "Encodes COL String class into interning functions, concat functions and the builtin string type."
}

case class EncodeStringClass[Pre <: Generation]() extends Rewriter[Pre] {
  var program: Program[Pre] = null
  lazy val stringClass: StringClass[Pre] = program.collectFirst {
    case cls: StringClass[Pre] => cls
  }.get
  lazy val intern = stringClass.declarations.flatMap(Referrable.from(_)).collectFirst {
    case ref @ RefInstanceFunction(f) if ref.name == "intern" => f
  }.get
  lazy val concat = stringClass.declarations.flatMap(Referrable.from(_)).collectFirst {
    case ref @ RefInstanceFunction(f) if ref.name == "concat" => f
  }.get
  lazy val data = stringClass.declarations.flatMap(Referrable.from(_)).collectFirst {
    case ref @ RefInstanceFunction(f) if ref.name == "data" => f
  }.get

  val stringClassSuccessor: SuccessionMap[StringClass[Pre], Class[Post]] = SuccessionMap()

  override def dispatch(program: Program[Pre]): Program[Post] = {
    this.program = program

    rewriteDefault(program)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: StringClass[Pre] =>
      stringClassSuccessor(cls) = globalDeclarations.declare(new Class(
        classDeclarations.collect(cls.declarations.map(rewriteDefault(_)))._1, Seq(), tt
      )(cls.o))
    case _ => rewriteDefault(decl)
  }

  def call(panicMsg: String, decl: ClassDeclaration[Pre], obj: Expr[Post], args: Seq[Expr[Post]])(implicit o: Origin): Expr[Post] = decl match {
    case f: InstanceFunction[Pre] =>
      InstanceFunctionInvocation[Post](
        obj = obj,
        ref = succ(f),
        args = args,
        typeArgs = Nil,
        givenMap = Nil,
        yields = Nil,
      )(PanicBlame(panicMsg))
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case i @ StringClassIntern(data) =>
      call("Interning a string cannot fail", intern, dispatch(data), Seq())(i.o)
    case c @ StringClassConcat(left, right) =>
      // TODO (RR): Not true, null!
      call("Concatenating two string class instances cannot fail", concat, dispatch(left), Seq(dispatch(right)))(c.o)
    case StringClassGetData(inner) =>
      // TODO (RR): Not true, null!
      call("Getting data of string class cannot fail", data, dispatch(inner), Seq())(expr.o)
    case t @ ThisStringClass(_) => ThisObject[Post](stringClassSuccessor.ref(stringClass))(t.o)
    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TStringClass() => TClass[Post](stringClassSuccessor.ref(stringClass))
    case _ => rewriteDefault(t)
  }
}
