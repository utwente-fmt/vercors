package vct.rewrite.veymont.generation

import hre.util.ScopedStack
import vct.col.ast.{
  ApplicableContract,
  Block,
  Assert,
  Assume,
  ByReferenceClass,
  Class,
  Constructor,
  Declaration,
  Exhale,
  Expr,
  Function,
  FunctionInvocation,
  Inhale,
  InstanceFunction,
  InstanceFunctionInvocation,
  InstanceMethod,
  InvokingNode,
  LoopContract,
  MethodInvocation,
  NewObject,
  Null,
  Procedure,
  ProcedureInvocation,
  Program,
  Result,
  Statement,
  TByReferenceClass,
}
import vct.col.origin.{
  Blame,
  InstanceInvocationFailure,
  InstanceNull,
  InvocationFailure,
  Origin,
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
  val globalsConstructor = DeclarationBox[Post, Constructor[Post]]()
  val globals = DeclarationBox[Post, Class[Post]]()
  val ghostGlobals = DeclarationBox[Post, Function[Post]]()

  def spec[T](f: => T): T = inSpec.having(true) { f }

  def getGlobals(implicit o: Origin): Expr[Post] =
    if (inSpec.topOption.contains(true))
      functionInvocation(
        ref = ghostGlobals.ref,
        blame = PanicBlame("Trivial contract"),
      )
    else
      constructorInvocation(
        ref = globalsConstructor.ref,
        blame = PanicBlame("Trivial contract"),
      )

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case inv: ProcedureInvocation[Pre] =>
        implicit val o = inv.o
        MethodInvocation[Post](
          getGlobals,
          procSucc.ref(inv.ref.decl),
          inv.args.map(dispatch),
          inv.outArgs.map(dispatch),
          inv.typeArgs.map(dispatch),
          inv.givenMap.map { case (v, e) => (succ(v.decl), dispatch(e)) },
          inv.yields.map { case (e, v) => (dispatch(e), succ(v.decl)) },
        )(InstanceInvocationFailureToInvocationFailure(inv))(inv.o)
      case inv: FunctionInvocation[Pre] =>
        implicit val o = inv.o
        InstanceFunctionInvocation[Post](
          getGlobals,
          funcSucc.ref(inv.ref.decl),
          inv.args.map(dispatch),
          inv.typeArgs.map(dispatch),
          inv.givenMap.map { case (v, e) => (succ(v.decl), dispatch(e)) },
          inv.yields.map { case (e, v) => (dispatch(e), succ(v.decl)) },
        )(InstanceInvocationFailureToInvocationFailure(inv))(inv.o)
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
      case _: Function[Pre] | _: Procedure[Pre] => decl.drop()
      case cls: ByReferenceClass[Pre] =>
        cls.rewrite(intrinsicLockInvariant = spec {
          cls.intrinsicLockInvariant.rewriteDefault()
        }).succeed(cls)
      case _ => super.dispatch(decl)
    }

  override def dispatch(
      contract: ApplicableContract[Pre]
  ): ApplicableContract[Post] = spec { contract.rewriteDefault() }

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] =
    spec { contract.rewriteDefault() }

  override def dispatch(stmt: Statement[Pre]): Statement[Post] =
    stmt match {
      case assert: Assert[Pre] =>
        assert.rewrite(res = spec { assert.res.rewriteDefault() })
      case assume: Assume[Pre] =>
        assume.rewrite(assn = spec { assume.assn.rewriteDefault() })
      case inhale: Inhale[Pre] =>
        inhale.rewrite(res = spec { inhale.res.rewriteDefault() })
      case exhale: Exhale[Pre] =>
        exhale.rewrite(res = spec { exhale.res.rewriteDefault() })
      // Par blocks?
      case _ => stmt.rewriteDefault()
    }

  def createGlobalClass(program: Program[Pre]): Unit = {
    globals.fill(
      new ByReferenceClass(
        decls =
          classDeclarations.collect {
            globalsConstructor.fill(
              constructor(
                cls = globals.ref,
                body = Some(Block[Post](Seq())(program.o)),
                blame = PanicBlame("Trivial contract"),
                contractBlame = PanicBlame("Trivial contract"),
              )(program.o).declare()
            )
            program.declarations.collect {
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
        returnType = TByReferenceClass[Post](globals.ref, Seq()),
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
