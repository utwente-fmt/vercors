package vct.rewrite.runtime

import com.sun.beans.finder.FieldFinder
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteAssign, RewriteEval, RewriteInstanceMethod, RewriteMethodInvocation, RewritePostAssignExpression, RewritePreAssignExpression}
import vct.col.ast.{AnyFunctionInvocation, Assign, Block, Class, CodeStringStatement, ContractApplicable, Declaration, Deref, Eval, Expr, InstanceField, InstanceMethod, Local, MethodInvocation, PostAssignExpression, PreAssignExpression, ProcedureInvocation, Program, Result, Scope, Statement, TClass, ThisObject, Type, Variable}
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.CodeStringDefaults.{assertCheckRead, assertCheckWrite, lookUpThread}
import vct.rewrite.runtime.util.FieldNumber

import scala.collection.immutable.HashMap
import scala.collection.mutable

object CheckPermissionsBlocksMethod extends RewriterBuilder {
  override def key: String = "CheckPermissionsBlocksMethod"

  override def desc: String = "Creates internal method blocks. In these blocks permissions will be checked"
}


case class CheckPermissionsBlocksMethod[Pre <: Generation]() extends Rewriter[Pre] {



  private val givenMethodSucc: SuccessionMap[InstanceMethod[Pre], InstanceMethod[Post]] = SuccessionMap()
  var hasInvocation: Boolean = false
  private val dereferences: ScopedStack[mutable.HashMap[Deref[Pre], Boolean]] = ScopedStack()
  val isTarget: ScopedStack[Boolean] = ScopedStack()




  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val test = super.dispatch(program)
    test
  }

  private def rewriteDefaultMethod(im: InstanceMethod[Pre]): Unit = {
    val newIm = new RewriteInstanceMethod[Pre, Post](im).rewrite()
    givenMethodSucc.update(im, newIm)
    classDeclarations.succeed(im, newIm)
  }




  private def generatePermissionChecksStatements(d: Deref[Pre], write: Boolean) : Option[CodeStringStatement[Post]] = {
    val name: String = d.ref.decl.o.getPreferredNameOrElse()
    d.obj match {
      case to: ThisObject[Pre] => {
        val id: Int = FieldNumber[Pre](to.cls.decl).findNumber(d.ref.decl)
        if (write) {
          Some(CodeStringStatement[Post](assertCheckWrite("this", id, name))(d.o))
        } else {
          Some(CodeStringStatement[Post](assertCheckRead("this", id, name))(d.o))
        }
      }
      case local: Local[Pre] => {
        local.ref.decl.t match {
          case tc: TClass[Pre] => {
            val id: Int = FieldNumber[Pre](tc.cls.decl).findNumber(d.ref.decl)
            val objectName = local.ref.decl.o.getPreferredNameOrElse()
            if (write) {
              Some(CodeStringStatement[Post](assertCheckWrite(objectName, id, name))(d.o))
            } else {
              Some(CodeStringStatement[Post](assertCheckRead(objectName, id, name))(d.o))
            }
          }
        }
      }
      case _ => None
    }
  }


  private def determineNewMethodStructure(b: Block[Pre]): Block[Post] = {
    dereferences.having(new mutable.HashMap[Deref[Pre], Boolean]()) {
      val newBlocks = b.statements.foldLeft((Seq.empty[Block[Post]], Block[Post](Seq())(b.o))) {
        case ((blocks, tmpBlock), statement) => {
          val newStatement = dispatch(statement)
          if (!hasInvocation) {
            (blocks, Block[Post](tmpBlock.statements :+ newStatement)(b.o))
          } else {
            hasInvocation = false
            val newAssertions: Seq[CodeStringStatement[Post]] = dereferences.top.map(pair => generatePermissionChecksStatements(pair._1, pair._2)).filter(o => o.nonEmpty).map(o => o.get).toSeq
            dereferences.top.clear()
            (blocks :+ Block[Post](newAssertions ++ tmpBlock.statements)(b.o) :+ Block[Post](Seq(newStatement))(b.o), Block[Post](Seq())(b.o))
          }
        }
      }
      val newAssertions: Seq[CodeStringStatement[Post]] = dereferences.top.map(pair => generatePermissionChecksStatements(pair._1, pair._2)).filter(o => o.nonEmpty).map(o => o.get).toSeq
      val finalBlock: Block[Post] = Block[Post](newAssertions ++ newBlocks._2.statements)(b.o)
      Block[Post](newBlocks._1 :+ finalBlock)(b.o)
    }
  }


  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case a: Assign[Pre] => {
        new RewriteAssign[Pre, Post](a).rewrite(
          value = isTarget.having(false) {
            dispatch(a.value)
          },
          target = isTarget.having(true) {
            dispatch(a.target)
          }
        )
      }
      case e: Eval[Pre] => {
        new RewriteEval[Pre, Post](e).rewrite(
          expr = dispatch(e.expr)
        )
      }
      case _ => super.dispatch(stat)
    }
  }

  private def dispatchGivenMethod(im: InstanceMethod[Pre]): Unit = {
    im.body match {
      case Some(sc: Scope[Pre]) => {
        sc.body match {
          case b: Block[Pre] => {
            val newLocals = variables.collect(sc.locals.foreach(l => dispatch(l)))._1
            val methodBody = determineNewMethodStructure(b)
            val newIm = new RewriteInstanceMethod[Pre, Post](im).rewrite(
              body = Some(Scope(newLocals, methodBody)(sc.o))
            )
            classDeclarations.declare(newIm)
            givenMethodSucc.update(im, newIm)
          }
          case e => rewriteDefaultMethod(im)
        }
      }
      case _ => rewriteDefaultMethod(im)
    }
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case mi: MethodInvocation[Pre] => {
        val newMI = new RewriteMethodInvocation[Pre, Post](mi).rewrite(
          ref = givenMethodSucc.ref[Post, InstanceMethod[Post]](mi.ref.decl)
        )
        hasInvocation = true
        newMI
      }
      case pi: ProcedureInvocation[Pre] => {
        hasInvocation = true
        super.rewriteDefault(pi)
      }
      case res: Result[Pre] => {
        res.applicable.decl match {
          case im: InstanceMethod[Pre] => {
            Result[Post](givenMethodSucc.ref(im))(res.o)
          }
          case _ => rewriteDefault(res)
        }
      }
      case preExpr: PreAssignExpression[Pre] => {
        new RewritePreAssignExpression[Pre, Post](preExpr).rewrite(
          value = isTarget.having(false) {
            dispatch(preExpr.value)
          },
          target = isTarget.having(true) {
            dispatch(preExpr.target)
          }
        )
      }
      case postExpr: PostAssignExpression[Pre] => {
        new RewritePostAssignExpression[Pre, Post](postExpr).rewrite(
          value = isTarget.having(false) {
            dispatch(postExpr.value)
          },
          target = isTarget.having(true) {
            dispatch(postExpr.target)
          }
        )
      }
      case d: Deref[Pre] => {
        val newD = super.rewriteDefault(d)
        if (!dereferences.isEmpty) {
          if (!isTarget.isEmpty && isTarget.top) {
            dereferences.top += (d -> true)
          } else if (!isTarget.isEmpty && !dereferences.top.contains(d)) {
            dereferences.top += (d -> false)
          }
        }
        newD
      }
      case _ => super.dispatch(e)
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case im: InstanceMethod[Pre] => dispatchGivenMethod(im)
      case cls: Class[Pre] => rewriteDefault(cls)
      case _ => rewriteDefault(decl)
    }
  }


}