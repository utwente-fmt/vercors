package vct.rewrite.runtime

import vct.col.ast.{AccountedPredicate, AmbiguousLocation, ApplicableContract, Block, Branch, Class, CodeStringStatement, Declaration, Deref, Div, Expr, InstanceField, InstanceMethod, IntegerValue, Local, Loop, Perm, Program, ReadPerm, Result, Return, RewriteHelpers, Scope, Statement, TClass, ThisObject, WritePerm}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import RewriteHelpers._
import hre.util.ScopedStack
import vct.col.ref.LazyRef
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.CodeStringDefaults._
import vct.rewrite.runtime.util.FieldNumber


object CreatePrePostConditions extends RewriterBuilder {
  override def key: String = "CreatePrePostConditions"

  override def desc: String = "Create permissions for the pre and post conditions of the methods"
}


case class CreatePrePostConditions[Pre <: Generation]() extends Rewriter[Pre] {
  val permissionExprContract: ScopedStack[Seq[CodeStringStatement[Post]]] = ScopedStack()
  val permDeref: ScopedStack[Deref[Pre]] = ScopedStack()


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val test = super.dispatch(program)
    test
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case im: InstanceMethod[Pre] => dispatchInstanceMethod(im)
      case _ => rewriteDefault(decl)
    }
  }

  def dispatchInstanceMethod(im: InstanceMethod[Pre]): Unit = {
    im.body match {
      case Some(sc: Scope[Pre]) => sc.body match {
        case block: Block[Pre] => classDeclarations.succeed(im, im.rewrite(body = Some(sc.rewrite(body = dispatchMethodBlock(block, im)))))
        case _ => ???
      }
      case _ => super.rewriteDefault(im)
    }
  }

  def dispatchMethodBlock(block: Block[Pre], im: InstanceMethod[Pre]): Block[Post] = {
    val preConditionStatements: Seq[CodeStringStatement[Post]] = dispatchApplicableContractToAssert(im.contract.requires)
    val postConditionStatements: Seq[CodeStringStatement[Post]] = dispatchApplicableContractToAssert(im.contract.ensures)
    val originalStatements: Seq[Statement[Post]] = block.statements.map(dispatch)
    val lastStatement: Option[Statement[Post]] = originalStatements.lastOption
    val insertedPostConditions = addPostConditions(postConditionStatements, originalStatements)
    lastStatement match {
      case Some(_: Return[Post]) => Block[Post](preConditionStatements ++ insertedPostConditions)(block.o)
      case _ => Block[Post](preConditionStatements ++ insertedPostConditions ++ postConditionStatements)(block.o)
    }
  }

  private def addPostConditions(postConditionStatements: Seq[CodeStringStatement[Post]], originalStatements: Seq[Statement[Post]]): Seq[Statement[Post]] = {
    originalStatements.foldLeft[Seq[Statement[Post]]](Seq.empty[Statement[Post]]) {
      case (statements: Seq[Statement[Post]], currentStatement: Return[Post]) => statements ++ postConditionStatements :+ currentStatement
      case (statements: Seq[Statement[Post]], loop: Loop[Post]) => statements :+ addPostConditionsLoop(postConditionStatements, loop)
      case (statements: Seq[Statement[Post]], branch: Branch[Post]) => statements :+ addPostConditionsBranch(postConditionStatements, branch)
      case (statements: Seq[Statement[Post]], currentStatement: Statement[Post]) => statements :+ currentStatement
    }
  }

  private def addPostConditionsLoop(postConditionStatements: Seq[CodeStringStatement[Post]], loop: Loop[Post]): Loop[Post] = {
    loop.body match {
      case block: Block[Post] => {
        val newBlock = Block[Post](addPostConditions(postConditionStatements, block.statements))(block.o)
        Loop[Post](loop.init, loop.cond, loop.update, loop.contract, newBlock)(loop.o)
      }
      case _ => ???
    }
  }

  private def addPostConditionsBranch(postConditionStatements: Seq[CodeStringStatement[Post]], branch: Branch[Post]): Branch[Post] = {
    val updatedBranches = branch.branches.map(b => {
      b._2 match {
        case block: Block[Post] => (b._1, Block[Post](addPostConditions(postConditionStatements, block.statements))(block.o))
        case _ => ???
      }
    })
    Branch[Post](updatedBranches)(branch.o)
  }

  private def dispatchApplicableContractToAssert(ap: AccountedPredicate[Pre]): Seq[CodeStringStatement[Post]] = {
    permissionExprContract.having(Seq.empty) {
      dispatch(ap)
      permissionExprContract.top
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case p: Perm[Pre] => {
        if (!permissionExprContract.isEmpty) {
          permDeref.having(null) {
            val res = super.dispatch(e)
            val newTop = permissionExprContract.top :+ createConditionCode(permDeref.top, p)
            permissionExprContract.pop()
            permissionExprContract.push(newTop)
            res
          }
        } else {
          super.dispatch(e)
        }
      }
      case d: Deref[Pre] => {
        if (!permDeref.isEmpty) {
          permDeref.pop()
          permDeref.push(d)
        }
        super.rewriteDefault(d)

      }
      case _ => super.dispatch(e)
    }
  }


  private def createConditionCode(deref: Deref[Pre], p: Perm[Pre]): CodeStringStatement[Post] = {
    val objectInfo = findObjectInfo(deref).getOrElse(("Unknown", -1))
    p.perm match {
      case iv: IntegerValue[Pre] => {
        if (iv.value > 1) {
          throw Unreachable("Permission cannot be exceeding 1")
        }
        CodeStringStatement(assertPermissionCondition(objectInfo._1, objectInfo._2, p.perm.toString))(p.o)
      }
      case d: Div[Pre] => CodeStringStatement(assertPermissionCondition(objectInfo._1, objectInfo._2, fractionTemplate(d.left.toString, d.right.toString)))(p.o)
      case w: WritePerm[Pre] => CodeStringStatement(assertCheckWrite(objectInfo._1, objectInfo._2, deref.ref.decl.o.getPreferredNameOrElse()))(p.o)
      case r: ReadPerm[Pre] => CodeStringStatement(assertCheckRead(objectInfo._1, objectInfo._2, deref.ref.decl.o.getPreferredNameOrElse()))(p.o)
      case _ => CodeStringStatement(assertPermissionCondition(objectInfo._1, objectInfo._2, p.perm.toString))(p.o)
    }
  }

  private def findObjectInfo(d: Deref[Pre]): Option[(String, Int)] = {
    d.obj match {
      case to: ThisObject[Pre] => Some("this", FieldNumber[Pre](to.cls.decl).findNumber(d.ref.decl))
      case local: Local[Pre] => {
        local.ref.decl.t match {
          case tc: TClass[Pre] => {
            val id: Int = FieldNumber[Pre](tc.cls.decl).findNumber(d.ref.decl)
            val objectName = local.ref.decl.o.getPreferredNameOrElse()
            Some((objectName, id))
          }
          case _ => None
        }
      }
      case _ => None
    }
  }
}