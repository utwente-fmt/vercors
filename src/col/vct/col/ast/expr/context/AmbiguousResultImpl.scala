package vct.col.ast.expr.context

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{AmbiguousResult, Type}
import vct.col.check.{CheckContext, CheckError, ResultOutsidePostcondition}
import vct.col.err
import vct.col.print._
import vct.col.resolve.ctx._
import vct.col.resolve.lang.{C, CPP}

trait AmbiguousResultImpl[G] extends NodeFamilyImpl[G] {
  this: AmbiguousResult[G] =>
  override lazy val t: Type[G] =
    ref.getOrElse(
      throw err.ContextSensitiveNodeNotResolved(
        this,
        "'\\result' encountered, but its attached method is not resolved.",
      )
    ) match {
      case RefCFunctionDefinition(decl) =>
        C.typeOrReturnTypeFromDeclaration(decl.specs, decl.declarator)
      case RefCGlobalDeclaration(decls, initIdx) =>
        C.typeOrReturnTypeFromDeclaration(
          decls.decl.specs,
          decls.decl.inits(initIdx).decl,
        )
      case RefCPPFunctionDefinition(decl) =>
        CPP.typeOrReturnTypeFromDeclaration(decl.specs, decl.declarator)
      case RefCPPGlobalDeclaration(decls, initIdx) =>
        CPP.typeOrReturnTypeFromDeclaration(
          decls.decl.specs,
          decls.decl.inits(initIdx).decl,
        )
      case RefFunction(decl) => decl.returnType
      case RefProcedure(decl) => decl.returnType
      case RefJavaMethod(decl) => decl.returnType
      case RefJavaAnnotationMethod(decl) => decl.returnType
      case RefLlvmFunctionDefinition(decl) => decl.returnType
      case RefLlvmSpecFunction(decl) => decl.returnType
      case RefInstanceFunction(decl) => decl.returnType
      case RefInstanceMethod(decl) => decl.returnType
      case RefInstanceOperatorMethod(decl) => decl.returnType
      case RefInstanceOperatorFunction(decl) => decl.returnType
    }

  override def check(context: CheckContext[G]): Seq[CheckError] =
    if (context.inPostCondition)
      super.check(context)
    else
      Seq(ResultOutsidePostcondition(this))

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => Text("result")
      case _ => Text("\\result")
    }
}
