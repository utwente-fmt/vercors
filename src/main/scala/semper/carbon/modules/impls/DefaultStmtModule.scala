package semper.carbon.modules.impls

import semper.carbon.modules.StmtModule
import semper.sil.{ast => sil}
import semper.carbon.boogie._
import semper.carbon.verifier.Verifier
import Implicits._

/**
 * The default implementation of a [[semper.carbon.modules.StmtModule]].
 *
 * @author Stefan Heule
 */
class DefaultStmtModule(val verifier: Verifier) extends StmtModule {

  import verifier.expModule._
  import verifier.stateModule._

  def name = "Statement module"
  override def translateStmt(stmt: sil.Stmt): Stmt = {
    var comment = "-- Translation of statement: " + stmt.toString
    val translation = (stmt match {
      case sil.LocalVarAssign(lhs, rhs) =>
        Assign(translateExp(lhs).asInstanceOf[Lhs], translateExp(rhs))
      case sil.FieldAssign(lhs, rhs) =>
        ???
      case sil.Fold(e) =>
        ???
      case sil.Unfold(e) =>
        ???
      case sil.Inhale(e) =>
        ???
      case sil.Exhale(e) =>
        // TODO: use the exhale module
        Assert(translateExp(e))
      case sil.MethodCall(m, rcv, args, targets) =>
        ???
      case sil.Seqn(ss) =>
        // return to avoid adding a comment, and to avoid the extra 'assumeGoodState'
        return Seqn(ss map translateStmt)
      case sil.While(cond, invs, locals, body) =>
        ???
      case sil.If(cond, thn, els) =>
        comment = s"Translation of statement: if ($cond)"
        If(translateExp(cond),
          translateStmt(thn),
          translateStmt(els))
      case sil.Label(name) =>
        ???
      case sil.Goto(target) =>
        ???
      case sil.FreshReadPerm(vars, body) =>
        comment = s"Translation of statement: fresh(${vars.mkString(", ")})"
        ???
      case sil.NewStmt(target) =>
        ???
    }) ::
      assumeGoodState ::
      Nil
    CommentBlock(comment, translation)
  }
}
