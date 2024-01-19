package vct.col.rewrite.exc

import vct.col.ast._
import RewriteHelpers._
import hre.util.ScopedStack
import vct.col.rewrite.error.ExcludedByPassOrder
import vct.col.origin.{LabelContext, Origin, PanicBlame, PreferredName}
import vct.col.ref.{LazyRef, Ref}
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

case object EncodeBreakReturn extends RewriterBuilder {
  override def key: String = "breakReturn"
  override def desc: String = "Encode break and return with goto or with exceptions."

  private def PostLabeledStatementOrigin(label: LabelDecl[_]): Origin =
    label.o.where(prefix = "break", context = "label after")

  private def ReturnClass: Origin =
    Origin(Seq(
      PreferredName(Seq("return")),
      LabelContext("return exception"),
    ))

  private def ReturnField: Origin =
    Origin(Seq(
      PreferredName(Seq("value")),
      LabelContext("return exception"),
    ))

  private def ReturnTarget: Origin =
    Origin(Seq(
      PreferredName(Seq("end")),
      LabelContext("method end"),
    ))

  private def ReturnVariable: Origin =
    Origin(Seq(
      PreferredName(Seq("return")),
      LabelContext("return value"),
    ))

  private def BreakException: Origin =
    Origin(Seq(
      PreferredName(Seq("break")),
      LabelContext("break exception"),
    ))
}

case class EncodeBreakReturn[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeBreakReturn._

  def needBreakReturnExceptions(stat: Statement[Pre]): Boolean =
    stat.transSubnodes.exists {
      case TryCatchFinally(_, Block(Nil), _) => false
      case TryCatchFinally(_, _, _) => true
      case _ => false
    }

  def needReturn(method: AbstractMethod[Pre]): Boolean =
    method match {
      case procedure: Procedure[Pre] => true
      case constructor: Constructor[Pre] => false
      case method: InstanceMethod[Pre] => true
      case method: InstanceOperatorMethod[Pre] => true
    }

  case class BreakReturnToGoto(returnTarget: Option[LabelDecl[Post]], resultVariable: Option[Local[Post]]) extends Rewriter[Pre] {
    val breakLabels: mutable.Set[LabelDecl[Pre]] = mutable.Set()
    val postLabeledStatement: SuccessionMap[LabelDecl[Pre], LabelDecl[Post]] = SuccessionMap()

    override val allScopes: AllScopes[Pre, Post] = EncodeBreakReturn.this.allScopes

    override def dispatch(stat: Statement[Pre]): Statement[Post] = {
      implicit val o: Origin = stat.o
      stat match {
        case Label(decl, stat) =>
          val newBody = dispatch(stat)

          if (breakLabels.contains(decl)) {
            postLabeledStatement(decl) = new LabelDecl()(PostLabeledStatementOrigin(decl))
            Block(Seq(
              Label(labelDecls.dispatch(decl), Block(Nil)),
              newBody,
              Label(postLabeledStatement(decl), Block(Nil)),
            ))
          } else {
            Block(Seq(
              Label(labelDecls.dispatch(decl), Block(Nil)),
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
            assignLocal(resultVariable.get, dispatch(result)),
            Goto(returnTarget.get.ref),
          ))

        case other => rewriteDefault(other)
      }
    }
  }

  case class BreakReturnToException(returnClass: Option[Class[Post]], valueField: Option[InstanceField[Post]]) extends Rewriter[Pre] {
    val breakLabelException: SuccessionMap[LabelDecl[Pre], Class[Post]] = SuccessionMap()

    override val allScopes: AllScopes[Pre, Post] = EncodeBreakReturn.this.allScopes

    override def dispatch(stat: Statement[Pre]): Statement[Post] = {
      implicit val o: Origin = stat.o
      stat match {
        case Label(decl, stat) =>
          val newBody = dispatch(stat)

          if(breakLabelException.contains(decl)) {
            TryCatchFinally(
              body = Block(Seq(Label(labelDecls.dispatch(decl), Block(Nil)), newBody)),
              after = Block(Nil),
              catches = Seq(CatchClause(
                decl = new Variable(TClass(breakLabelException.ref(decl))),
                body = Block(Nil),
              )),
            )
          } else {
            Block(Seq(
              Label(labelDecls.dispatch(decl), Block(Nil)),
              newBody,
            ))
          }

        case Break(None) =>
          throw ExcludedByPassOrder("Break statements without a label are made explicit by SpecifyImplicitLabels", Some(stat))

        case Break(Some(Ref(label))) =>
          val cls = breakLabelException.getOrElseUpdate(label,
            globalDeclarations.declare(new Class[Post](Nil, Nil, tt)(BreakException)))

          Throw(NewObject[Post](cls.ref))(PanicBlame("The result of NewObject is never null"))

        case Return(result) =>
          val exc = new Variable[Post](TClass(returnClass.get.ref))
          Scope(Seq(exc), Block(Seq(
            assignLocal(exc.get, NewObject(returnClass.get.ref)),
            assignField(exc.get, valueField.get.ref, dispatch(result), PanicBlame("Have write permission immediately after NewObject")),
            Throw(exc.get)(PanicBlame("The result of NewObject is never null")),
          )))

        case other => rewriteDefault(other)
      }
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] =>
      method.body match {
        case None => rewriteDefault(method)
        case Some(body) =>
          allScopes.anyDeclare(allScopes.anySucceedOnly(method, method.rewrite(body = Some({
            if (needBreakReturnExceptions(body)) {
              implicit val o: Origin = body.o

              if(needReturn(method)) {
                val returnField = new InstanceField[Post](dispatch(method.returnType), Nil)(ReturnField)
                val returnClass = new Class[Post](Seq(returnField), Nil, tt)(ReturnClass)
                globalDeclarations.declare(returnClass)

                val caughtReturn = new Variable[Post](TClass(returnClass.ref))

                TryCatchFinally(
                  body = BreakReturnToException(Some(returnClass), Some(returnField)).dispatch(body),
                  catches = Seq(CatchClause(caughtReturn,
                    Return(Deref[Post](caughtReturn.get, returnField.ref)(PanicBlame("Permission for the field of a return exception cannot be non-write, as the class is only instantiated at a return site, and caught immediately.")))
                  )),
                  after = Block(Nil)
                )
              } else {
                BreakReturnToException(None, None).dispatch(body)
              }
            } else {
              implicit val o: Origin = body.o

              if(needReturn(method)) {
                val resultTarget = new LabelDecl[Post]()(ReturnTarget)
                val resultVar = new Variable(dispatch(method.returnType))(ReturnVariable)
                val newBody = BreakReturnToGoto(Some(resultTarget), Some(resultVar.get(ReturnVariable))).dispatch(body)

                Scope(Seq(resultVar), Block(Seq(
                  newBody,
                  Label(resultTarget, Block(Nil)),
                  Return(resultVar.get),
                )))
              } else {
                BreakReturnToGoto(None, None).dispatch(body)
              }
            }
          }))))
      }
    case other => rewriteDefault(other)
  }
}
