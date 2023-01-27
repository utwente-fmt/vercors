package vct.col.rewrite

import vct.col.ast.{Class, Declaration, Expr, Function, Intern, Program, StringClass, StringClassConcat, TClass, TStringClass, ThisObject, ThisStringClass, Type}
import vct.col.origin.PanicBlame
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable

case object EncodeStringClass extends RewriterBuilder {
  override def key: String = "encodeStringClass"

  override def desc: String = "Encodes COL String class into interning functions, concat functions and the builtin string type."
}

case class EncodeStringClass[Pre <: Generation]() extends Rewriter[Pre] {
  var program: Program[Pre] = null
  lazy val stringClass: StringClass[Pre] = program.collectFirst {
    case cls: StringClass[Pre] => cls
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

  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case i @ Intern(data) =>
       functionInvocation[Post](PanicBlame("Interning a java string cannot fail"),
         succ(stringClass.intern.decl), Seq(dispatch(data)))(i.o)
    case c @ StringClassConcat(left, right) =>
      functionInvocation[Post](PanicBlame("Concatenating java strings cannot fail"),
        succ(stringClass.concat.decl), Seq(dispatch(left), dispatch(right)))(c.o)
    case t @ ThisStringClass(_) => ThisObject[Post](stringClassSuccessor.ref(stringClass))(t.o)
    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TStringClass() => TClass[Post](stringClassSuccessor.ref(stringClass))
    case _ => rewriteDefault(t)
  }
}
