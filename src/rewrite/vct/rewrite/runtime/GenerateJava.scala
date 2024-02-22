package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.Util._

object GenerateJava extends RewriterBuilder {
  override def key: String = "generateJava"

  override def desc: String = "Create permissions for items in arrays"
}


case class GenerateJava[Pre <: Generation]() extends Rewriter[Pre] {


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    lazy val newNameSpace = createNewNamespace
   program.rewrite(declarations = globalDeclarations.collectScoped{
     newNameSpace
     program.declarations.foreach(dispatch)
   }._1)
  }

  def createNewNamespace: JavaNamespace[Post] ={
    implicit val origin: Origin = DiagnosticOrigin
    val importUtil = JavaImport[Post](false, JavaName(Seq("java", "util")), true)
//    val importArray = JavaImport(false, JavaName(Seq("java", "lang", "reflect", "Array")), false)
    val importFraction = JavaImport[Post](false, JavaName(Seq("org", "apache", "commons", "math3", "fraction", "Fraction")), false)
    val newNameSpace = new JavaNamespace[Post](
      Some(JavaName[Post](Seq("org", "example"))),
      Seq(importUtil, importFraction),
      Nil,
    )
    globalDeclarations.declare(newNameSpace)
    newNameSpace
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p:Procedure[Pre] => p.drop()
//      case im: InstanceMethod[Pre] if isMethod(im, "run") => im.rewrite(overriding = true, public = true)
      case im: InstanceMethod[Pre] if isMethod(im, "start") || isMethod(im, "join") => im.drop()
      case im: InstanceMethod[Pre] if isMethod(im, "main") => createMainMethod(im)
      case im: InstanceMethod[Pre] if isMethod(im, "run") => makeMethodPublic(im)
      case im: InstanceMethod[Pre] if isMethod(im, "equals") && im.o.getDataObjectClassRuntime.nonEmpty => makeMethodPublic(im)
      case im: InstanceMethod[Pre] if isMethod(im, "hashCode") && im.o.getDataObjectClassRuntime.nonEmpty => makeMethodPublic(im)
      case _ => super.dispatch(decl)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case pe@PreAssignExpression(_, p: ProcedureInvocation[Pre]) => pe.rewrite(value = procedureInvocationToNewObject(p))
      case pe@PostAssignExpression(_, p: ProcedureInvocation[Pre]) => pe.rewrite(value = procedureInvocationToNewObject(p))
      case mi: MethodInvocation[Pre] if isThreadMethod(mi, "start") => generateThreadMethodCall(mi, "start")
      case mi: MethodInvocation[Pre] if isThreadMethod(mi, "join") => generateThreadMethodCall(mi, "join")
      case l: Local[Pre] if variables.freeze.computeSucc(l.ref.decl).isEmpty => JavaLocal[Post](l.ref.decl.o.getPreferredNameOrElse())(null)(l.o)
      //      case mi: MethodInvocation[Pre] if isMethod(mi.ref.decl, "equals") => generateThreadMethodCall(mi, "join")

      case _ => super.dispatch(e)
    }
  }

  def generateThreadMethodCall(mi: MethodInvocation[Pre], name: String): JavaInvocation[Post] = {
    JavaInvocation[Post](
      Some(dispatch(mi.obj)),
      Nil,
      name,
      mi.args.map(dispatch),
      Nil,
      Nil
    )(null)(mi.o)
  }

  def createMainMethod(im: InstanceMethod[Pre]): Unit = {
    implicit val origin: Origin = im.o
    val modifiers: Seq[JavaModifier[Post]] = Seq(JavaPublic[Post](), JavaStatic())
    val returnType: Type[Post] = TVoid[Post]()
    val param: Seq[JavaParam[Post]] = Seq(new JavaParam[Post](Nil, "args", TArray(TString())))
    val newBody: Option[Statement[Post]] = im.body.map(dispatch)
    val contract = dispatch(im.contract)
    val newMethod = new JavaMethod(
      modifiers,
      returnType,
      0,
      "main",
      param,
      Nil,
      Nil,
      newBody,
      contract
    )(null)
    classDeclarations.succeed(im, newMethod)
  }

  def makeMethodPublic(im: InstanceMethod[Pre]): Unit = {
    implicit val origin: Origin = im.o
    val modifiers: Seq[JavaModifier[Post]] = Seq(JavaPublic[Post]())
    val newBody: Option[Statement[Post]] = im.body.map(dispatch)
    val contract = dispatch(im.contract)
    val params: Seq[JavaParam[Post]] = im.args.map(v => {
      new JavaParam[Post](Nil, v.o.getPreferredNameOrElse(), dispatch(v.t))
    })
    val newMethod = new JavaMethod(
      modifiers,
      dispatch(im.returnType),
      0,
      im.o.getPreferredNameOrElse(),
      params,
      Nil,
      Nil,
      newBody,
      contract
    )(null)
    classDeclarations.succeed(im, newMethod)
  }

  override def dispatch(node: Statement[Pre]): Statement[Post] = {
    node match {
      case a@Assign(_, p: ProcedureInvocation[Pre]) => a.rewrite(value = procedureInvocationToNewObject(p))
      case _ => super.dispatch(node)
    }
  }

  private def procedureInvocationToNewObject(p: ProcedureInvocation[Pre]): NewObject[Post]= {
    val classDecl: Class[Pre] = p.ref.decl.returnType.asClass.get.cls.decl
    NewObject[Post](this.anySucc(classDecl))(classDecl.o)
  }

}