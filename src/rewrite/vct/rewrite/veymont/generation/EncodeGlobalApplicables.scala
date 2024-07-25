package vct.rewrite.veymont.generation

import hre.util.ScopedStack
import vct.col.ast.{
  Class,
  Declaration,
  Expr,
  Function,
  FunctionInvocation,
  InstanceFunction,
  InstanceFunctionInvocation,
  InstanceMethod,
  InvokingNode,
  MethodInvocation,
  Null,
  Procedure,
  ProcedureInvocation,
  Program,
  Result,
}
import vct.col.origin.{
  Blame,
  InstanceInvocationFailure,
  InstanceNull,
  InvocationFailure,
  PanicBlame,
  TrueSatisfiable,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{DeclarationBox, SuccessionMap}
import vct.rewrite.veymont.generation.EncodeGlobalApplicables.InstanceInvocationFailureToInvocationFailure

object EncodeGlobalApplicables extends RewriterBuilder {

  override def key: String = "encodeGlobalApplicables"

  override def desc: String =
    "Encode all global applicables into a class, which is created whenever a global applicable is called."

  case class InstanceInvocationFailureToInvocationFailure(inv: InvokingNode[_])
      extends Blame[InstanceInvocationFailure] {
    override def blame(error: InstanceInvocationFailure): Unit =
      error match {
        case InstanceNull(_) =>
          PanicBlame("Invocation target guaranteed not to be null")
        case failure: InvocationFailure => inv.blame.blame(failure)
      }
  }
}

case class EncodeGlobalApplicables[Pre <: Generation]() extends Rewriter[Pre] {
  val procSucc = SuccessionMap[Procedure[Pre], InstanceMethod[Post]]()
  val funcSucc = SuccessionMap[Function[Pre], InstanceFunction[Post]]()

  val inSpec = ScopedStack[Boolean]()
  val globals = DeclarationBox[Post, Class[Post]]()
  val ghostGlobals = DeclarationBox[Post, Function[Post]]()

  def getGlobals: Expr[Post] =
    if (inSpec.topOption.contains(true))
      ???
    else
      ???

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case inv: ProcedureInvocation[Pre] =>
        MethodInvocation(???, ???, ???, ???, ???, ???, ???)(
          InstanceInvocationFailureToInvocationFailure(inv)
        )(inv.o)
      case inv: FunctionInvocation[Pre] =>
        InstanceFunctionInvocation(???, ???, ???, ???, ???, ???)(
          InstanceInvocationFailureToInvocationFailure(inv)
        )(inv.o)
      case _ => expr.rewriteDefault()
    }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    program.rewrite(declarations = {
      globalDeclarations.collect {
        createGlobalClass(program)
        program.declarations.map(dispatch)
      }._1
    })
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      // Functions and procedures are moved into the global statics class
      case _: Function[Pre] | _: Procedure[Pre] =>
      case _ => super.dispatch(decl)
    }

  def createGlobalClass(program: Program[Pre]): Unit = {
    globals.fill(
      new Class(
        decls =
          classDeclarations.collect {
            program.foreach {
              case p: Procedure[Pre] => dispatchProc(p)
              case f: Function[Pre] => dispatchFunc(f)
            }
          }._1,
        typeArgs = Seq(),
        supports = Seq(),
        intrinsicLockInvariant = tt,
      )(program.o.where(name = "G$")).declare()
    )

    implicit val o = program.o
    ghostGlobals.fill(withResult[Post, Function[Post]](result =>
      function(
        blame = PanicBlame("Trivial contract"),
        contractBlame = TrueSatisfiable,
        ensures = (result !== Null()).accounted,
      )(program.o.where(name = "g$")).declare()
    ))
  }

  def dispatchProc(proc: Procedure[Pre]): Unit =
    procSucc(proc) =
      new InstanceMethod(
        returnType = dispatch(proc.returnType),
        args = variables.dispatch(proc.args),
        outArgs = variables.dispatch(proc.outArgs),
        typeArgs = variables.dispatch(proc.typeArgs),
        body = proc.body.map(dispatch),
        contract = dispatch(proc.contract),
        inline = proc.inline,
        pure = proc.pure,
      )(proc.blame)(proc.o).declare()

  def dispatchFunc(func: Function[Pre]): Unit =
    funcSucc(func) =
      new InstanceFunction(
        returnType = dispatch(func.returnType),
        args = variables.dispatch(func.args),
        typeArgs = variables.dispatch(func.typeArgs),
        body = func.body.map(dispatch),
        contract = dispatch(func.contract),
        inline = func.inline,
        threadLocal = func.threadLocal,
      )(func.blame)(func.o).declare()

}
