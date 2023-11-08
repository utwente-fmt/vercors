package vct.rewrite.runtime

import vct.col.ast.{AccountedPredicate, AmbiguousLocation, ApplicableContract, Block, Class, CodeStringAssertStatement, Declaration, Deref, Div, Expr, InstanceField, InstanceMethod, IntegerValue, Perm, ReadPerm, Statement, WritePerm}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import hre.util.ScopedStack
import vct.col.ref.LazyRef
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.CodeStringDefaults.{assertCheckRead, assertCheckWrite, assertPermissionCondition, fractionTemplate}
import vct.rewrite.runtime.util.FieldNumber


object CreatePrePostConditions extends RewriterBuilder {
  override def key: String = "CreatePrePostConditions"

  override def desc: String = "Create permissions for the pre and post conditions of the methods"
}


case class CreatePrePostConditions[Pre <: Generation]() extends Rewriter[Pre] {


  val instanceMethods: ScopedStack[InstanceMethod[Pre]] = ScopedStack()
  val permissionExprContract: ScopedStack[Seq[CodeStringAssertStatement[Post]]] = ScopedStack()
  val permInstanceFieldRef: ScopedStack[LazyRef[Pre, InstanceField[Pre]]] = ScopedStack()
  val fieldFinder: ScopedStack[FieldNumber[Pre]] = ScopedStack()


  def dispatchApplicableContractToAssert(ap: AccountedPredicate[Pre]): Seq[CodeStringAssertStatement[Post]] = {
    permissionExprContract.having(Seq.empty) {
      val t = permissionExprContract.top
      t
      dispatch(ap)
      permissionExprContract.push(permissionExprContract.top)
    }
    permissionExprContract.pop()
  }


  def dispatchBlockInMethod(b: Block[Pre], im: InstanceMethod[Pre]): Block[Post] = {
    val preConditionStatements: Seq[CodeStringAssertStatement[Post]] = dispatchApplicableContractToAssert(im.contract.requires)
    val originalStatements: Seq[Statement[Post]] = b.statements.map(dispatch)
    val postConditionStatements: Seq[CodeStringAssertStatement[Post]] = dispatchApplicableContractToAssert(im.contract.ensures)
    Block[Post](preConditionStatements ++ originalStatements ++ postConditionStatements)(b.o)
  }


  private def createConditionCode(ref: LazyRef[Pre, InstanceField[Pre]], p: Perm[Pre]): CodeStringAssertStatement[Post] = {

    p.perm match {
      case iv: IntegerValue[Pre] => {
        if (iv.value > 1) {
          throw Unreachable("Permission cannot be exceeding 1")
        }
        CodeStringAssertStatement(assertPermissionCondition(fieldFinder.top.findNumber(ref.decl), p.perm.toString))(p.o)
      }
      case d: Div[Pre] => {
        CodeStringAssertStatement (assertPermissionCondition (fieldFinder.top.findNumber (ref.decl), fractionTemplate(d.left.toString, d.right.toString)) ) (p.o)
      }
      case w: WritePerm[Pre] => CodeStringAssertStatement(assertCheckWrite(fieldFinder.top.findNumber (ref.decl),ref.decl.o.getPreferredNameOrElse()))(p.o)
      case r: ReadPerm[Pre] => CodeStringAssertStatement(assertCheckRead(fieldFinder.top.findNumber (ref.decl),ref.decl.o.getPreferredNameOrElse()))(p.o)
      case _ => CodeStringAssertStatement(assertPermissionCondition(fieldFinder.top.findNumber(ref.decl), p.perm.toString))(p.o)
    }
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case p: Perm[Pre] => {
        if (!permissionExprContract.isEmpty) {
          permInstanceFieldRef.having(null) {
            super.dispatch(e)
            val ref = permissionExprContract.top
            val newTop = permissionExprContract.top :+ createConditionCode(permInstanceFieldRef.top, p)
            permissionExprContract.pop()
            permissionExprContract.push(newTop)
          }
        }
        super.dispatch(e)
      }
      case d: Deref[Pre] => {
        if (!permInstanceFieldRef.isEmpty) {
          d.ref match {
            case lr: LazyRef[Pre, InstanceField[Pre]] => {
              permInstanceFieldRef.pop()
              permInstanceFieldRef.push(lr)
            }
            case _ =>
          }

        }
        super.rewriteDefault(d)

      }
      case _ => super.dispatch(e)
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    stat match {
      case b: Block[Pre] => {
        if (instanceMethods.isEmpty) {
          super.rewriteDefault(b)
        } else {
          dispatchBlockInMethod(b, instanceMethods.top)
        }
      }
      case _ => super.rewriteDefault(stat)
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case im: InstanceMethod[Pre] => {
        instanceMethods.having(im) {
          rewriteDefault(im)
        }
      }
      case cls: Class[Pre] => {
        fieldFinder.having(FieldNumber[Pre](cls)) {
          super.dispatch(cls)
        }
      }
      case _ => rewriteDefault(decl)
    }

  }
}