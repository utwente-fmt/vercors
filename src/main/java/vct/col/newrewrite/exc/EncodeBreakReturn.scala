package vct.col.newrewrite.exc

import vct.col.ast._
import RewriteHelpers._
import vct.col.newrewrite.error.ExcludedByPassOrder
import vct.col.origin.Origin
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.Rewriter
import vct.col.util.SuccessionMap

import scala.collection.mutable

case object EncodeBreakReturn {
  case class PostLabeledStatementOrigin(label: LabelDecl) extends Origin {
    override def preferredName: String = "post_" + label.o.preferredName
    override def messageInContext(message: String): String =
      s"[At node generated to jump past a statement]: $message"
  }
}

case class EncodeBreakReturn() extends Rewriter {
  import EncodeBreakReturn._

  def hasFinally(stat: Statement): Boolean =
    stat.transSubnodes.exists {
      case TryCatchFinally(_, Block(Nil), _) => false
      case TryCatchFinally(_, _, _) => true
      case _ => false
    }

  case class BreakReturnToGoto(returnTarget: LabelDecl, resultVariable: Local) extends Rewriter {
    val breakLabels: mutable.Set[LabelDecl] = mutable.Set()
    val postLabeledStatement: SuccessionMap[LabelDecl, LabelDecl] = SuccessionMap()

    override def dispatch(stat: Statement): Statement = {
      implicit val o: Origin = stat.o
      stat match {
        case Label(decl, stat) =>
          val newBody = dispatch(stat)

          if (breakLabels.contains(decl)) {
            postLabeledStatement(decl) = new LabelDecl()(PostLabeledStatementOrigin(decl))
            Block(Seq(
              Label(decl.rewrite(), Block(Nil)),
              newBody,
              Label(postLabeledStatement(decl), Block(Nil)),
            ))
          } else {
            Block(Seq(
              Label(decl.rewrite(), Block(Nil)),
              newBody,
            ))
          }

        case Break(None) =>
          throw ExcludedByPassOrder("Break statements without a label are made explicit by SpecifyImplicitLabels", Some(stat))

        case Break(Some(Ref(label))) =>
          breakLabels += label
          Goto(postLabeledStatement.ref(label))

        case Return(result) =>
          Block(Seq(
            Assign(resultVariable, dispatch(result)),
            Goto(returnTarget.ref),
          ))

        case other => rewriteDefault(other)
      }
    }
  }
}
