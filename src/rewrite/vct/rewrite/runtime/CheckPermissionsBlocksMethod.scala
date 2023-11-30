package vct.rewrite.runtime

import com.sun.beans.finder.FieldFinder
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteAssign, RewriteBranch, RewriteEval, RewriteInstanceMethod, RewriteLoop, RewriteMethodInvocation, RewritePostAssignExpression, RewritePreAssignExpression}
import vct.col.ast.{AnyFunctionInvocation, Assign, Block, Branch, Class, CodeStringStatement, ContractApplicable, Declaration, Deref, Eval, Expr, InstanceField, InstanceMethod, Local, Loop, MethodInvocation, PostAssignExpression, PreAssignExpression, ProcedureInvocation, Program, Result, Scope, Statement, TClass, ThisObject, Type, Variable}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import vct.rewrite.runtime.util.CodeStringDefaults.{assertCheck, assertCheckRead, assertCheckWrite, lookUpThread}
import vct.rewrite.runtime.util.{FieldNumber, FieldObjectString}

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
  val fieldFinder: ScopedStack[FieldNumber[Pre]] = ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    fieldFinder.having(FieldNumber[Pre](program)) {
      isTarget.having(false) {
        val test = super.dispatch(program)
        test
      }
    }
  }

  private def rewriteDefaultMethod(im: InstanceMethod[Pre]): Unit = {
    val newIm = new RewriteInstanceMethod[Pre, Post](im).rewrite()
    givenMethodSucc.update(im, newIm)
    classDeclarations.succeed(im, newIm)
  }


  private def generatePermissionChecksStatements(d: Deref[Pre], write: Boolean): Option[CodeStringStatement[Post]] = {
    val objectLocation: String = FieldObjectString().determineObjectReference(d)
    val id: Int = fieldFinder.top.findNumber(d.ref.decl)
    val name: String = d.ref.decl.o.getPreferredNameOrElse()
    Some(CodeStringStatement[Post](assertCheck(objectLocation, id, name, write))(d.o))
  }


  private def dispatchLoop(loop: Loop[Pre]): Loop[Post] = {
    val newInit = dispatch(loop.init)
    val newCond = dispatch(loop.cond)
    val newUpdate = dispatch(loop.update) //TODO test if this needs to be checked inside of the loop of outside of the loop or both
    val newBody = dereferences.having(new mutable.HashMap[Deref[Pre], Boolean]()) {
      loop.body match {
        case block: Block[Pre] => {
          determineNewBlockStructure(block)
        }
        case _ => ???
      }
    }
    new RewriteLoop[Pre, Post](loop).rewrite(
      init = newInit,
      cond = newCond,
      update = newUpdate,
      body = newBody
    )
  }

  private def dispatchBranch(branch: Branch[Pre]): Branch[Post] = {
    val gatheredConditions = branch.branches.map(b => dispatch(b._1))
    val gatheredBlocks = branch.branches.map(b => {
      b._2 match {
        case block: Block[Pre] => {
          dereferences.having(new mutable.HashMap[Deref[Pre], Boolean]()) {
            determineNewBlockStructure(block)
          }
        }
        case _ => super.dispatch(b._2)
      }
    })
    Branch[Post](gatheredConditions.zip(gatheredBlocks))(branch.o)
  }

  private def retrieveDereferences(): Seq[CodeStringStatement[Post]] = {
    val newAssertions: Seq[CodeStringStatement[Post]] = dereferences.top.map(pair => generatePermissionChecksStatements(pair._1, pair._2)).filter(o => o.nonEmpty).map(o => o.get).toSeq
    dereferences.top.clear()
    newAssertions
  }


  private def defaultStatementNewMethodStructure(blockFold: (Seq[Block[Post]], Block[Post]), statement: Statement[Pre], b: Block[Pre]): (Seq[Block[Post]], Block[Post]) = {
    val newStatement = dispatch(statement)
    if (hasInvocation) {
      hasInvocation = false
      (blockFold._1 :+ Block[Post](retrieveDereferences() ++ blockFold._2.statements)(b.o) :+ Block[Post](Seq(newStatement))(b.o), Block[Post](Seq())(b.o))
    } else {
      (blockFold._1, Block[Post](blockFold._2.statements :+ newStatement)(b.o))
    }
  }


  private def dispatchBranchOrLoop[T[Pre] <: Statement[Pre]](preBlock: Block[Pre], blockFold: (Seq[Block[Post]], Block[Post]), statement: T[Pre], dispatchFunc: T[Pre] => T[Post]): (Seq[Block[Post]], Block[Post]) = {
    val newStat = dispatchFunc(statement)
    (blockFold._1 :+ Block[Post](retrieveDereferences() ++ blockFold._2.statements :+ newStat)(preBlock.o), Block[Post](Seq())(preBlock.o))
  }


  private def determineNewBlockStructure(b: Block[Pre]): Block[Post] = {
    dereferences.having(new mutable.HashMap[Deref[Pre], Boolean]()) {
      val newBlocks = b.statements.foldLeft((Seq.empty[Block[Post]], Block[Post](Seq())(b.o))) {
        case (blockFold, branch: Branch[Pre]) => dispatchBranchOrLoop(b, blockFold, branch, dispatchBranch)
        case (blockFold, loop: Loop[Pre]) => dispatchBranchOrLoop(b, blockFold, loop, dispatchLoop)
        case (blockFold, statement) => defaultStatementNewMethodStructure(blockFold, statement, b)
      }
      val finalBlock: Block[Post] = Block[Post](retrieveDereferences() ++ newBlocks._2.statements)(b.o)
      Block[Post](newBlocks._1 :+ finalBlock)(b.o)
    }
  }


  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case a: Assign[Pre] => {
        new RewriteAssign[Pre, Post](a).rewrite(
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
            val methodBody = determineNewBlockStructure(b)
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
          target = isTarget.having(true) {
            dispatch(preExpr.target)
          }
        )
      }
      case postExpr: PostAssignExpression[Pre] => {
        new RewritePostAssignExpression[Pre, Post](postExpr).rewrite(
          target = isTarget.having(true) {
            dispatch(postExpr.target)
          }
        )
      }
      case d: Deref[Pre] => {
        val newD = super.rewriteDefault(d)
        if(dereferences.isEmpty) return newD
        if (isTarget.top) {
          dereferences.top += (d -> true)
        } else if (!dereferences.top.contains(d)) {
          dereferences.top += (d -> false)
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