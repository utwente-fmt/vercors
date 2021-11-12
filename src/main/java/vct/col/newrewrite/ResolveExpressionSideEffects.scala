package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.util.AstBuildHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.Rewriter

import scala.collection.mutable.ArrayBuffer

case object ResolveExpressionSideEffects {
  case object SideEffectOrigin extends Origin {
    override def preferredName: String = "flatten"
    override def messageInContext(message: String): String =
      s"[At node generated to collect side effects]: $message"
  }
}

case class ResolveExpressionSideEffects() extends Rewriter {
  import ResolveExpressionSideEffects._

  val sideEffectConsumer: ScopedStack[(Statement => Unit, Statement => Unit)] = ScopedStack()

  def evaluate(expr: Expr): (Seq[Statement], Expr, Seq[Statement]) = {
    val before = ArrayBuffer[Statement]()
    val after = ArrayBuffer[Statement]()

    val result = sideEffectConsumer.having((before.append, after.append)) {
      dispatch(expr)
    }

    (before.toSeq, result, after.toSeq)
  }

  def frame(expr: Expr, make: Expr => Statement): Statement = {
    val (before, result, after) = evaluate(expr)
    implicit val o: Origin = SideEffectOrigin

    if(after.isEmpty) {
      if(before.isEmpty) {
        make(result)
      } else {
        Block(before :+ make(result))
      }
    } else {
      val intermediate = new Variable(result.t)
      Scope(Seq(intermediate), Block(
        before ++
          Seq(Assign(intermediate.get, result)) ++
          after ++
          Seq(make(intermediate.get))
      ))
    }
  }

  def frame(expr1: Expr, expr2: Expr, make: (Expr, Expr) => Statement): Statement = {
    val (before1, result1, after1) = evaluate(expr1)
    val (before2, result2, after2) = evaluate(expr2)

    val before = before1 ++ before2
    val after = after1 ++ after2

    implicit val o: Origin = SideEffectOrigin

    if(after.isEmpty) {
      if(before.isEmpty) {
        make(result1, result2)
      } else {
        Block(before :+ make(result1, result2))
      }
    } else {
      val intermediate1 = new Variable(result1.t)
      val intermediate2 = new Variable(result2.t)
      Scope(Seq(intermediate1, intermediate2), Block(
        before ++
          Seq(Assign(intermediate1.get, result1), Assign(intermediate2.get, result2)) ++
          after ++
          Seq(make(intermediate1.get, intermediate2.get))
      ))
    }
  }

  override def dispatch(stat: Statement): Statement = {
    implicit val o: Origin = stat.o
    stat match {
      case Eval(e) => ???
      case LocalDecl(_) => rewriteDefault(stat)
      case Return(result) => frame(result, res => Return(res))
      case Assign(target, value) => ???
      case Block(_) => rewriteDefault(stat)
      case Scope(_, _) => rewriteDefault(stat)
      case Branch(branches) => ???
      case Switch(expr, body) => frame(expr, expr => Switch(expr, dispatch(body)))
      case Loop(init, cond, update, contract, body) =>
      case TryCatchFinally(body, after, catches) =>
      case Synchronized(obj, body) =>
      case ParInvariant(decl, inv, content) =>
      case ParAtomic(inv, content) =>
      case ParBarrier(block, invs, requires, ensures, content) =>
      case ParRegion(requires, ensures, blocks) =>
      case VecBlock(iters, requires, ensures, content) =>
      case Send(resource, label, offset) =>
      case Recv(resource, label, offset) =>
      case DefaultCase() =>
      case Case(pattern) =>
      case Label(decl) =>
      case Goto(lbl) =>
      case Exhale(res) =>
      case Assert(res) =>
      case Refute(assn) =>
      case Inhale(res) =>
      case Assume(assn) =>
      case SpecIgnoreStart() =>
      case SpecIgnoreEnd() =>
      case Throw(obj) =>
      case Wait(obj) =>
      case Notify(obj) =>
      case Fork(obj) =>
      case Join(obj) =>
      case Lock(obj) =>
      case Unlock(obj) =>
      case Fold(res) =>
      case Unfold(res) =>
      case WandCreate(statements) =>
      case WandQed(res) =>
      case WandApply(res) =>
      case WandUse(res) =>
      case ModelDo(model, perm, after, action, impl) =>
      case Havoc(loc) =>
      case Break(label) =>
      case Continue(label) =>
    }
  }
}
