package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers.RewriteClass
import vct.col.ast.{Block, Class, Declaration, Expr, InstanceMethod, MethodInvocation, Program, Scope, Statement, Type}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap

object CreateBlocksMethods extends RewriterBuilder {
  override def key: String = "CreateBlocksMethods"

  override def desc: String = "Creates internal blocks. In these blocks permissions need to be checked"
}


case class CreateBlocksMethods[Pre <: Generation]() extends Rewriter[Pre] {

  private var seenMethodInvocation: Boolean = false
  private val givenMethodSucc: SuccessionMap[InstanceMethod[Pre], InstanceMethod[Post]] = SuccessionMap()


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val test = super.dispatch(program)
    test
  }

  private def createNewInstanceMethod(im: InstanceMethod[Pre],newBlock: Block[Post], sc: Scope[Pre]): InstanceMethod[Post] = {
    val newLocals = variables.collect(sc.locals.foreach(l => dispatch(l)))._1
    val newScope = Some(Scope(newLocals, newBlock)(sc.o))
    new InstanceMethod[Post](dispatch(im.returnType),
      variables.collect(im.args.foreach(a => dispatch(a)))._1,
      variables.collect(im.outArgs.foreach(a => dispatch(a)))._1,
      variables.collect(im.typeArgs.foreach(a => dispatch(a)))._1,
      newScope,
      dispatch(im.contract),
      im.inline,
      im.pure
    )(im.blame)(im.o)

  }


  private def determineNewMethodStructure(b: Block[Pre]): Block[Post] = {
    val newBlocks = b.statements.foldLeft((Seq.empty[Block[Post]], Block[Post](Seq())(b.o))) {
      case ((blocks, tmpBlock), s) =>
        val newStatement = rewriteDefault(s)
        if (seenMethodInvocation) {
          seenMethodInvocation = false
          (blocks :+ tmpBlock :+ Block[Post](Seq(newStatement))(b.o), Block[Post](Seq())(b.o))
        } else {
          (blocks, Block[Post](tmpBlock.statements :+ newStatement)(b.o))
        }
    }
    val finalBlocks = newBlocks._1 :+ newBlocks._2
    Block[Post](finalBlocks)(b.o)
  }


  private def dispatchGivenMethod(im: InstanceMethod[Pre]): Unit = {
    im.body match {
      case Some(sc: Scope[Pre]) => {
        sc.body match {
          case b: Block[Pre] => {
            val newIm = createNewInstanceMethod(im, determineNewMethodStructure(b), sc)
            classDeclarations.declare(newIm)
            givenMethodSucc.update(im, newIm)
          }
        }
      }
      case _ => rewriteDefault(im)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case mi: MethodInvocation[Pre] => {
        seenMethodInvocation = true
        new MethodInvocation[Post](
          dispatch(mi.obj),
          givenMethodSucc.ref[Post, InstanceMethod[Post]](mi.ref.decl),
          mi.args.map(a => dispatch(a)),
          mi.outArgs.map(a => dispatch(a)),
          mi.typeArgs.map(a => dispatch(a)),
          Seq(),
          Seq()
        )(mi.blame)(mi.o)
      }
      case _ => rewriteDefault(e)
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case im: InstanceMethod[Pre] => dispatchGivenMethod(im)
      case _ => rewriteDefault(decl)
    }
  }
}