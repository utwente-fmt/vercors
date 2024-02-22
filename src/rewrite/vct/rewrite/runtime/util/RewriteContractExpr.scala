package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteDeref
import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.AbstractQuantifierRewriter.LoopBodyContent
import vct.rewrite.runtime.util.LedgerHelper._
import vct.rewrite.runtime.util.Util.permissionToRuntimeValueRewrite
import vct.rewrite.runtime.util.permissionTransfer.PermissionData


case class RewriteContractExpr[Pre <: Generation](pd: PermissionData[Pre])(implicit program: Program[Pre]) extends AbstractQuantifierRewriter[Pre](pd) {
  override val allScopes = pd.outer.allScopes

  override def dispatchLoopBody(loopBodyContent: LoopBodyContent[Pre])(implicit origin: Origin): Block[Post] = {
    inQuantifier.having(true){
      Block[Post](unfoldStar(loopBodyContent.expr).map(dispatchExpr))
    }
  }


  val inQuantifier: ScopedStack[Boolean] = ScopedStack()
  inQuantifier.push(false)
  val ledger: LedgerMethodBuilderHelper[Post] = pd.ledger.get
  var injectivityMap: Variable[Post] = pd.injectivityMap.get

  def createAssertions(expr: Expr[Pre]): Statement[Post] = {
    implicit val origin: Origin = expr.o
    val (initInjectivityMap, checkInjectivtyMap) = createInjectivityMap
    val unfoldedExpr = unfoldStar(expr)
    val assertionChecks = unfoldedExpr.map(dispatchExpr)
    assertionChecks match {
      case Nil => Block[Post](Nil)
      case _ => Block[Post](initInjectivityMap +: assertionChecks :+ checkInjectivtyMap)
    }
  }

  private def createInjectivityMap: (Statement[Post], Statement[Post]) = {
    implicit val origin: Origin = DiagnosticOrigin
    val assignInjectivtyMap = Assign[Post](injectivityMap.get, ledger.injectivityMap.newOuterMap)(null)
    val checkInjectivityMap = Eval[Post](ledger.miCheckForInjectivity(injectivityMap.get).get)
    (assignInjectivtyMap, checkInjectivityMap)
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case d@Deref(t@ThisObject(_), i) => d.rewrite(obj = pd.getOffset(t))
      case _ => super.dispatch(e)
    }
  }

  private def dispatchExpr(e: Expr[Pre]): Statement[Post] = {
    implicit val origin: Origin = e.o
    e match {
      case _: Star[Pre] => createAssertions(e)
      case p: Perm[Pre] => dispatchPermission(p)
      case ipa: InstancePredicateApply[Pre] => dispatchInstancePredicateApply(ipa)
      case _: Starall[Pre] | _: Exists[Pre] | _: Forall[Pre] => {
        super.dispatchQuantifier(e)
      }
      case _ => Assert[Post](super.dispatch(e))(null)
    }
  }

  private def dispatchPermission(p: Perm[Pre])(implicit origin: Origin = p.o): Block[Post] = {
    val cond = permissionToRuntimeValueRewrite(p)
    val dataObject: Expr[Post] = p.loc.asInstanceOf[AmbiguousLocation[Pre]].expr match {
      case d@Deref(o, _) => {
        ledger.pmbh.miCreate(CreateObjectArray[Post](
          Seq(getNewExpr(o),
            dispatch(const[Pre](findNumberInstanceField(program, d.ref.decl).get)))
        )).get
      }
      case AmbiguousSubscript(coll, index) => {
        ledger.pmbh.miCreate(CreateObjectArray[Post](Seq(getNewExpr(coll), dispatch(index)))).get
      }
      case _ => throw Unreachable(s"This type of permissions transfer is not yet supported: ${p}")
    }

    val getPermission = ledger.miGetPermission(dataObject).get
    val injectivityMapFunctionality: Statement[Post] = if(inQuantifier.top){
      val containsInMap = ledger.injectivityMap.contains(injectivityMap.get, dataObject)
      val checkDuplicates = RuntimeAssert[Post](!containsInMap, "Permission cannot be checked twice for the same object in a quantifier")(null)
      val putInjectivity = Eval[Post](ledger.injectivityMap.put(injectivityMap.get, dataObject, cond))
      Block[Post](Seq(checkDuplicates, putInjectivity))
    }else{
      val getOrDefaultPerm = ledger.injectivityMap.getOrDefault(injectivityMap.get, dataObject, RuntimeFractionZero[Post]())
      val putPermissionInjectivity = ledger.injectivityMap.put(injectivityMap.get, dataObject, getOrDefaultPerm r_+ cond)
      Eval[Post](putPermissionInjectivity)
    }

    val check: Expr[Post] = (getPermission r_<=> cond) !== const(-1) // test if the value is equal or bigger than the required permission
    val assertion = RuntimeAssertExpected[Post](check, cond, getPermission, s"Permission is not enough")(null)
    Block[Post](Seq(injectivityMapFunctionality, assertion))
  }

  override def getNewExpr(e: Expr[Pre]): Expr[Post] = {
    e match {
      case d: Deref[Pre] => d.rewrite(obj = getNewExpr(d.obj))
      case t: ThisObject[Pre] => pd.getOffset(t)
      case _ => dispatch(e)
    }
  }

  private def dispatchInstancePredicateApply(ipa: InstancePredicateApply[Pre]): Block[Post] = {
    implicit val origin: Origin = ipa.o
    val allArgs: Seq[Expr[Pre]] = ipa.args :+ ipa.obj :+ StringValue(ipa.ref.decl.o.getPreferredNameOrElse())
    val dispatchedArgs: Seq[Expr[Post]] = allArgs.map(dispatch)
    val newObject = CreateObjectArray[Post](dispatchedArgs)
    val mi: MethodInvocation[Post] = ledger.miHasPredicateCheck(newObject).get
    Block[Post](Seq(Eval[Post](mi)))
  }

}
