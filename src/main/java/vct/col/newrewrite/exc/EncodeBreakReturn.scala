package vct.col.newrewrite.exc

import vct.col.ast._
import RewriteHelpers._
import vct.col.newrewrite.error.ExcludedByPassOrder
import vct.col.origin.{Origin, PanicBlame}
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

  case object ReturnClass extends Origin {
    override def preferredName: String = "Return"
    override def messageInContext(message: String): String =
      s"[At class generated to encode return with an exception]: $message"
  }

  case object ReturnField extends Origin {
    override def preferredName: String = "value"
    override def messageInContext(message: String): String =
      s"[At field generated to encode return with an exception]: $message"
  }

  case object ReturnTarget extends Origin {
    override def preferredName: String = "end"
    override def messageInContext(message: String): String =
      s"[At label generated for the end of the method]: $message"
  }

  case object ReturnVariable extends Origin {
    override def preferredName: String = "return"
    override def messageInContext(message: String): String =
      s"[At variable generated for the result of the method]: $message"
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

  case class BreakReturnToException(returnClass: Class, valueField: InstanceField) extends Rewriter {
    val breakLabelException: SuccessionMap[LabelDecl, Class] = SuccessionMap()

    override def dispatch(stat: Statement): Statement = {
      implicit val o: Origin = stat.o
      stat match {
        case Label(decl, stat) =>
          val newBody = dispatch(stat)

          if(breakLabelException.contains(decl)) {
            TryCatchFinally(
              body = Block(Seq(Label(decl.rewrite(), Block(Nil)), newBody)),
              after = Block(Nil),
              catches = Seq(CatchClause(
                decl = new Variable(TClass(breakLabelException.ref(decl))),
                body = Block(Nil),
              )),
            )
          } else {
            Block(Seq(
              Label(decl.rewrite(), Block(Nil)),
              newBody,
            ))
          }

        case Break(None) =>
          throw ExcludedByPassOrder("Break statements without a label are made explicit by SpecifyImplicitLabels", Some(stat))

        case Break(Some(Ref(label))) =>
          val cls = breakLabelException.getOrElseUpdate(label, {
            val cls = new Class(Nil, Nil, tt)
            cls.declareDefault(this)
            cls
          })
          Throw(NewObject(cls.ref))(PanicBlame("The result of NewObject is never null"))

        case Return(result) =>
          val exc = new Variable(TClass(returnClass.ref))
          Scope(Seq(exc), Block(Seq(
            Assign(exc.get, NewObject(returnClass.ref)),
            assignField(exc.get, valueField.ref, dispatch(result)),
            Throw(exc.get)(PanicBlame("The result of NewObject is never null")),
          )))

        case other => rewriteDefault(other)
      }
    }
  }

  override def dispatch(decl: Declaration): Unit = decl match {
    case method: AbstractMethod =>
      method.body match {
        case None => rewriteDefault(method)
        case Some(body) =>
          val newBody: Statement = if(hasFinally(body)) {
            val returnField = new InstanceField(method.returnType, Set.empty)(ReturnField)
            val returnClass = new Class(Seq(returnField), Nil, tt)(ReturnClass)
            returnClass.declareDefault(this)

            BreakReturnToException(returnClass, returnField).dispatch(body)
          } else {
            val resultTarget = new LabelDecl()(ReturnTarget)
            val resultVar = new Variable(method.returnType)(ReturnVariable)
            val newBody = BreakReturnToGoto(resultTarget, resultVar.get(ReturnVariable)).dispatch(body)
            implicit val o: Origin = ???
            Scope(Seq(resultVar), Block(Seq(
              newBody,
              Label(resultTarget, Block(Nil)),
              Return(resultVar.get),
            )))
          }
          method.rewrite(body = Some(newBody)).succeedDefault(this, method)
      }
  }
}
