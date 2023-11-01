package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers.RewriteClass
import vct.col.ast.{Class, ClassDeclaration, Declaration, GlobalDeclaration, Procedure, Program, TClass, Type}
import vct.col.print.Text
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import vct.rewrite.runtime.util.CreateConstructor

object RefactorGeneratedCode extends RewriterBuilder {
  override def key: String = "refactorGeneratedCode"

  override def desc: String = "Refactors the generated code to have a constructor again and remove implements and extends"
}


case class RefactorGeneratedCode[Pre <: Generation]() extends Rewriter[Pre] {

  private val givenClassConstrSucc: SuccessionMap[Type[Pre], Procedure[Pre]] = SuccessionMap()
  private val givenClassSucc: SuccessionMap[Type[Pre], Class[Post]] = SuccessionMap()


  override def rewriteDefault(node: Program[Pre]): Program[Rewritten[Pre]] = {
    super.rewriteDefault(node)
  }

  def createClassDeclarations(c: Class[Pre], rw: Rewriter[Pre]): Seq[ClassDeclaration[Rewritten[Pre]]] = {
    classDeclarations.collect {
      (givenClassConstrSucc.get(TClass(c.ref)).get +: c.declarations).foreach(d => rw.dispatch(d))
    }._1
  }

  def dispatchGivenClass(c: Class[Pre]): GlobalDeclaration[Rewritten[Pre]] = {
    val rw = CreateConstructor[Pre](this, givenClassSucc)
    val newClass = new RewriteClass[Pre, Post](c)(rw).rewrite(
      declarations = createClassDeclarations(c, rw),
    )
    givenClassSucc.update(TClass(c.ref), newClass)
    newClass
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p: Procedure[Pre] => givenClassConstrSucc.update(p.returnType, p)
      case c: Class[Pre] =>
        val classOrInterface = c.classOrInterfaceDoc()
        classOrInterface match {
          case Text("class") => globalDeclarations.succeed(c, dispatchGivenClass(c))
          case _ =>
            println("Detected interface no creation of constructor")
            rewriteDefault(c)
        }
      case _ => super.rewriteDefault(decl)
    }
  }
}