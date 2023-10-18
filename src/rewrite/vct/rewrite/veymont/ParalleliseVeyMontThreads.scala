package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{
  RewriteApplicableContract,
  RewriteClass,
  RewriteDeref,
  RewriteJavaClass,
  RewriteJavaConstructor,
  RewriteMethodInvocation,
}
import vct.col.ast.{
  AbstractRewriter,
  ApplicableContract,
  Assert,
  Assign,
  Block,
  BooleanValue,
  Branch,
  Class,
  ClassDeclaration,
  Declaration,
  Deref,
  DerefVeyMontThread,
  Eval,
  Expr,
  InstanceField,
  InstanceMethod,
  JavaClass,
  JavaConstructor,
  JavaInvocation,
  JavaLocal,
  JavaMethod,
  JavaNamedType,
  JavaParam,
  JavaPublic,
  JavaTClass,
  Local,
  Loop,
  MethodInvocation,
  NewObject,
  Node,
  Procedure,
  Program,
  RunMethod,
  Scope,
  Statement,
  TClass,
  TVeyMontChannel,
  TVoid,
  ThisObject,
  ThisSeqProg,
  Type,
  UnitAccountedPredicate,
  Variable,
  VeyMontAssignExpression,
  VeyMontCommExpression,
  VeyMontCondition,
  VeyMontSeqProg,
  VeyMontThread,
}
import vct.col.origin.Origin
import vct.col.resolve.ctx.RefJavaMethod
import vct.col.rewrite.{
  Generation,
  Rewriter,
  RewriterBuilder,
  RewriterBuilderArg,
  Rewritten,
}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}
import vct.rewrite.veymont.ParalleliseVeyMontThreads.{
  ChannelFieldOrigin,
  ParalliseVeyMontThreadsError,
  RunMethodOrigin,
  ThreadClassOrigin,
  getChannelClassName,
  getThreadClassName,
  getVarName,
}

import java.lang

object ParalleliseVeyMontThreads extends RewriterBuilderArg[JavaClass[_]] {
  override def key: String = "ParalleliseVeyMontThreads"

  override def desc: String =
    "Generate classes for VeyMont threads in parallel program"

  private val channelClassName = "Channel"
  private val threadClassName = "Thread"

  def getChannelClassName(channelType: Type[_]): String =
    channelType.toString.capitalize + channelClassName

  def getThreadClassName(thread: VeyMontThread[_]): String =
    thread.o.preferredName.capitalize + threadClassName

  def getVarName(v: Variable[_]) = v.o.preferredName

  case class ParalliseVeyMontThreadsError(node: Node[_], msg: String)
      extends UserError {
    override def code: String = "ParalleliseVeyMontThreadsError"

    override def text: String = node.o.messageInContext(msg)
  }

  case class ThreadClassOrigin(thread: VeyMontThread[_]) extends Origin {
    override def preferredName: String = getThreadClassName(thread)

    override def context: String = thread.o.context

    override def inlineContext: String = thread.o.inlineContext

    override def shortPosition: String = thread.o.shortPosition
  }

  case class ChannelFieldOrigin(channelName: String, assign: Statement[_])
      extends Origin {
    override def preferredName: String = channelName

    override def context: String = assign.o.context

    override def inlineContext: String = assign.o.inlineContext

    override def shortPosition: String = assign.o.shortPosition
  }

  case class RunMethodOrigin(runMethod: RunMethod[_]) extends Origin {
    override def preferredName: String = "run"

    override def context: String = runMethod.o.context

    override def inlineContext: String = runMethod.o.inlineContext

    override def shortPosition: String = runMethod.o.shortPosition
  }
}

case class ParalleliseVeyMontThreads[Pre <: Generation](
    channelClass: JavaClass[_]
) extends Rewriter[Pre] {
  outer =>

  private val threadBuildingBlocks: ScopedStack[ThreadBuildingBlocks[Pre]] =
    ScopedStack()
  private val threadClassSucc: SuccessionMap[VeyMontThread[Pre], Class[Post]] =
    SuccessionMap()
  private val threadMethodSucc: SuccessionMap[
    (VeyMontThread[Pre], ClassDeclaration[Pre]),
    InstanceMethod[Post],
  ] = SuccessionMap()
  private val givenClassSucc: SuccessionMap[Type[Pre], Class[Post]] =
    SuccessionMap()
  private val givenClassConstrSucc: SuccessionMap[Type[Pre], Procedure[Pre]] =
    SuccessionMap()

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p: Procedure[Pre] => givenClassConstrSucc.update(p.returnType, p)
      case c: Class[Pre] => globalDeclarations.succeed(c, dispatchGivenClass(c))
      case seqProg: VeyMontSeqProg[Pre] => dispatchThreads(seqProg)
      case thread: VeyMontThread[Pre] => dispatchThread(thread)
      case other => rewriteDefault(other)
    }
  }

  private def dispatchThread(thread: VeyMontThread[Pre]): Unit = {
    if (threadBuildingBlocks.nonEmpty) {
      val threadRes: ThreadBuildingBlocks[Pre] = threadBuildingBlocks.top
      val threadMethods: Seq[ClassDeclaration[Post]] = createThreadMethod(
        thread,
        threadRes,
      )
      createThreadClass(thread, threadRes, threadMethods)
    } else
      rewriteDefault(thread)
  }

  private def dispatchThreads(seqProg: VeyMontSeqProg[Pre]): Unit = {
    val (channelClasses, indexedChannelInfo) = extractChannelInfo(seqProg)
    channelClasses.foreach { case (t, c) => globalDeclarations.declare(c) }
    seqProg.threads.foreach(thread => {
      val threadField =
        new InstanceField[Post](
          TClass(givenClassSucc.ref(thread.threadType)),
          Set.empty,
        )(thread.o)
      val channelFields = getChannelFields(
        thread,
        indexedChannelInfo,
        channelClasses,
      )
      threadBuildingBlocks.having(new ThreadBuildingBlocks(
        seqProg.runMethod,
        seqProg.methods,
        channelFields,
        channelClasses,
        thread,
        threadField,
      )) { dispatch(thread) }
    })
  }

  private def dispatchGivenClass(c: Class[Pre]): Class[Post] = {
    val rw = GivenClassRewriter()
    val gc = new RewriteClass[Pre, Post](c)(rw).rewrite(declarations =
      classDeclarations.collect {
        (givenClassConstrSucc.get(TClass(c.ref)).get +: c.declarations)
          .foreach(d => rw.dispatch(d))
      }._1
    )
    givenClassSucc.update(TClass(c.ref), gc)
    gc
  }

  case class GivenClassRewriter() extends Rewriter[Pre] {
    override val allScopes = outer.allScopes

    val rewritingConstr: ScopedStack[(Seq[Variable[Pre]], TClass[Pre])] =
      ScopedStack()

    override def dispatch(decl: Declaration[Pre]): Unit =
      decl match {
        case p: Procedure[Pre] =>
          p.returnType match {
            case tc: TClass[Pre] =>
              rewritingConstr.having(p.args, tc) {
                classDeclarations.declare(createClassConstructor(p))
              };
            case _ =>
              ??? // ("This procedure is expected to have a class as return type");
          }
        case other => rewriteDefault(other)
      }

    // PB: from what I understand this restores a constructor from a generated procedure, so this should be refactored
    // once we make constructors first class.
    def createClassConstructor(p: Procedure[Pre]): JavaConstructor[Post] =
      new JavaConstructor[Post](
        Seq(JavaPublic[Post]()(p.o)),
        rewritingConstr.top._2.cls.decl.o.preferredName,
        p.args.map(createJavaParam),
        variables.dispatch(p.typeArgs),
        Seq.empty,
        p.body match {
          case Some(s: Scope[Pre]) =>
            s.body match {
              case b: Block[Pre] =>
                dispatch(Block(b.statements.tail.dropRight(1))(p.o))
              case other => dispatch(other)
            }
          case Some(_) =>
            throw Unreachable(
              "The body of a procedure always starts with a Scope."
            )
          case None => Block(Seq.empty)(p.o)
        },
        p.contract.rewrite(ensures =
          UnitAccountedPredicate[Post](BooleanValue(true)(p.o))(p.o)
        ),
      )(null)(p.o)

    def createJavaParam(v: Variable[Pre]): JavaParam[Post] =
      new JavaParam[Post](Seq.empty, getVarName(v), dispatch(v.t))(v.o)

    override def dispatch(e: Expr[Pre]): Expr[Post] =
      e match {
        case l: Local[Pre] =>
          if (
            rewritingConstr.nonEmpty &&
            rewritingConstr.top._1.contains(l.ref.decl)
          )
            JavaLocal[Post](getVarName(l.ref.decl))(null)(e.o)
          else
            rewriteDefault(l)
        case t: ThisObject[Pre] =>
          val thisClassType = TClass(t.cls)
          if (
            rewritingConstr.nonEmpty && rewritingConstr.top._2 == thisClassType
          )
            ThisObject(givenClassSucc.ref[Post, Class[Post]](thisClassType))(
              t.o
            )
          else
            rewriteDefault(t)
        case d: Deref[Pre] =>
          if (rewritingConstr.nonEmpty)
            d.obj match {
              case _: Local[Pre] =>
                d.rewrite(obj =
                  ThisObject(
                    givenClassSucc
                      .ref[Post, Class[Post]](rewritingConstr.top._2)
                  )(d.o)
                )
              case other => rewriteDefault(other)
            }
          else
            rewriteDefault(d)
        case other => rewriteDefault(other)
      }
  }

  private def extractChannelInfo(
      seqProg: VeyMontSeqProg[Pre]
  ): (Map[Type[Pre], JavaClass[Post]], Seq[ChannelInfo[Pre]]) = {
    val channelInfo =
      collectChannelsFromRun(seqProg) ++ collectChannelsFromMethods(seqProg)
    val indexedChannelInfo: Seq[ChannelInfo[Pre]] =
      channelInfo.groupBy(_.channelName).values.flatMap(chanInfoSeq =>
        if (chanInfoSeq.size <= 1)
          chanInfoSeq
        else
          chanInfoSeq.zipWithIndex.map { case (chanInfo, index) =>
            new ChannelInfo(
              chanInfo.comExpr,
              chanInfo.channelType,
              chanInfo.channelName + index,
            )
          }
      ).toSeq
    val channelClasses = generateChannelClasses(indexedChannelInfo)
    (channelClasses, indexedChannelInfo)
  }

  private def createThreadMethod(
      thread: VeyMontThread[Pre],
      threadRes: ThreadBuildingBlocks[Pre],
  ) = {
    threadRes.methods.map { preMethod =>
      val postMethod = getThreadMethodFromDecl(thread)(preMethod)
      threadMethodSucc.update((thread, preMethod), postMethod)
      postMethod
    }
  }

  private def createThreadClass(
      thread: VeyMontThread[Pre],
      threadRes: ThreadBuildingBlocks[Pre],
      threadMethods: Seq[ClassDeclaration[Post]],
  ): Unit = {
    val threadConstr = createThreadClassConstructor(
      thread,
      threadRes.threadField,
    )
    val threadRun = getThreadRunFromDecl(thread, threadRes.runMethod)
    classDeclarations.scope {
      val threadClass =
        new Class[Post](
          (threadRes.threadField +: threadRes.channelFields.values.toSeq) ++
            (threadConstr +: threadRun +: threadMethods),
          Seq(),
          BooleanValue(true)(thread.o),
        )(ThreadClassOrigin(thread))
      globalDeclarations.declare(threadClass)
      threadClassSucc.update(thread, threadClass)
    }
  }

  private def createThreadClassConstructor(
      thread: VeyMontThread[Pre],
      threadField: InstanceField[Post],
  ): JavaConstructor[Post] = {
    val threadConstrArgBlocks = thread.args.map {
      case l: Local[Pre] => (l.ref.decl.o.preferredName, dispatch(l.t))
      case other =>
        throw ParalliseVeyMontThreadsError(
          other,
          "This node is expected to be an argument of seq_prog, and have type Local",
        )
    }
    val threadConstrArgs: Seq[JavaParam[Post]] = threadConstrArgBlocks.map {
      case (a, t) =>
        new JavaParam[Post](Seq.empty, a, t)(ThreadClassOrigin(thread))
    }
    val passedArgs = threadConstrArgs
      .map(a => JavaLocal[Post](a.name)(null)(ThreadClassOrigin(thread)))
    val threadTypeName =
      thread.threadType match { // TODO: replace by using givenClassSucc
        case tc: TClass[Pre] => tc.cls.decl.o.preferredName
        case _ =>
          throw ParalliseVeyMontThreadsError(
            thread.threadType,
            "This type is expected to be a class",
          )
      }
    val threadConstrBody = {
      Assign(
        getThisVeyMontDeref(thread, ThreadClassOrigin(thread), threadField),
        JavaInvocation[Post](
          None,
          Seq.empty,
          "new " + threadTypeName,
          passedArgs,
          Seq.empty,
          Seq.empty,
        )(null)(ThreadClassOrigin(thread)),
      )(null)(ThreadClassOrigin(thread))
    }
    val threadConstrContract =
      new ApplicableContract[Post](
        UnitAccountedPredicate[Post](
          BooleanValue(true)(ThreadClassOrigin(thread))
        )(ThreadClassOrigin(thread)),
        UnitAccountedPredicate[Post](
          BooleanValue(true)(ThreadClassOrigin(thread))
        )(ThreadClassOrigin(thread)),
        BooleanValue(true)(ThreadClassOrigin(thread)),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        None,
      )(null)(ThreadClassOrigin(thread))
    new JavaConstructor[Post](
      Seq(JavaPublic[Post]()(ThreadClassOrigin(thread))),
      getThreadClassName(thread),
      threadConstrArgs,
      Seq.empty,
      Seq.empty,
      threadConstrBody,
      threadConstrContract,
    )(ThreadClassOrigin(thread))(ThreadClassOrigin(thread))
  }

  private def getThreadMethodFromDecl(
      thread: VeyMontThread[Pre]
  )(decl: ClassDeclaration[Pre]): InstanceMethod[Post] =
    decl match {
      case m: InstanceMethod[Pre] => getThreadMethod(m)
      case _ =>
        throw ParalliseVeyMontThreadsError(
          thread,
          "Methods of seq_program need to be of type InstanceMethod",
        )
    }

  private def getThreadRunFromDecl(
      thread: VeyMontThread[Pre],
      decl: ClassDeclaration[Pre],
  ): InstanceMethod[Post] =
    decl match {
      case m: RunMethod[Pre] => getThreadRunMethod(m)
      case _ =>
        throw ParalliseVeyMontThreadsError(
          thread,
          "RunMethod expected in seq_program",
        )
    }

  private def getChannelFields(
      thread: VeyMontThread[Pre],
      channelInfo: Seq[ChannelInfo[Pre]],
      channelClasses: Map[Type[Pre], JavaClass[Post]],
  ): Map[(VeyMontCommExpression[Pre], Origin), InstanceField[Post]] = {
    channelInfo.filter(chanInfo =>
      chanInfo.comExpr.receiver.decl == thread ||
        chanInfo.comExpr.sender.decl == thread
    ).map { chanInfo =>
      val chanFieldOrigin = ChannelFieldOrigin(
        chanInfo.channelName,
        chanInfo.comExpr.assign,
      )
      val chanField =
        new InstanceField[Post](
          TVeyMontChannel(getChannelClassName(chanInfo.channelType)),
          Set.empty,
        )(chanFieldOrigin)
      ((chanInfo.comExpr, chanInfo.comExpr.o), chanField)
    }.toMap
  }

  private def generateChannelClasses(
      channelInfo: Seq[ChannelInfo[Pre]]
  ): Map[Type[Pre], JavaClass[Post]] = {
    val channelTypes = channelInfo.map(_.channelType).toSet
    channelTypes.map(channelType =>
      channelType -> {
        val chanClassPre = channelClass.asInstanceOf[JavaClass[Pre]]
        val rw = ChannelClassGenerator(channelType)
        new RewriteJavaClass[Pre, Post](chanClassPre)(rw).rewrite(
          name = getChannelClassName(channelType),
          modifiers = Seq.empty,
          decls =
            classDeclarations.collect {
              chanClassPre.decls.foreach(d => rw.dispatch(d))
            }._1,
        )
      }
    ).toMap
  }

  case class ChannelClassGenerator(channelType: Type[_]) extends Rewriter[Pre] {
    override val allScopes = outer.allScopes

    override def dispatch(t: Type[Pre]): Type[Post] =
      t match {
        case jnt: JavaNamedType[Pre] =>
          if (jnt.names.head._1 == "MessageType") {
            dispatch(channelType.asInstanceOf[Type[Pre]])
          } else
            rewriteDefault(jnt)
        case _ => rewriteDefault(t)
      }

    override def dispatch(decl: Declaration[Pre]): Unit =
      decl match {
        case jc: JavaConstructor[Pre] =>
          classDeclarations
            .declare(jc.rewrite(name = getChannelClassName(channelType)))
        case other => rewriteDefault(other)
      }
  }

  private def collectChannelsFromRun(seqProg: VeyMontSeqProg[Pre]) =
    seqProg.runMethod match {
      case r: RunMethod[Pre] => getChannelsFromBody(r.body, r)
      case other =>
        throw ParalliseVeyMontThreadsError(
          other,
          "seq_program run method expected",
        )
    }

  private def collectChannelsFromMethods(seqProg: VeyMontSeqProg[Pre]) =
    seqProg.methods.flatMap {
      case m: InstanceMethod[Pre] => getChannelsFromBody(m.body, m)
      case other =>
        throw ParalliseVeyMontThreadsError(other, "seq_program method expected")
    }

  private def getChannelsFromBody(
      body: Option[Statement[Pre]],
      method: ClassDeclaration[Pre],
  ) = {
    body match {
      case None =>
        throw ParalliseVeyMontThreadsError(
          method,
          "Method in seq_program needs to have non-empty body",
        )
      case Some(b) => getChannelNamesAndTypes(b)
    }
  }

  private def getChannelNamesAndTypes(
      s: Statement[Pre]
  ): Seq[ChannelInfo[Pre]] = {
    s.collect {
      case e @ VeyMontCommExpression(recv, sender, chanType, assign) =>
        new ChannelInfo(
          e,
          chanType,
          recv.decl.o.preferredName + sender.decl.o.preferredName + "Channel",
        )
    }
  }

  private def getThreadMethod(
      method: InstanceMethod[Pre]
  ): InstanceMethod[Post] = {
    new InstanceMethod[Post](
      dispatch(method.returnType),
      variables.dispatch(method.args),
      variables.dispatch(method.outArgs),
      variables.dispatch(method.typeArgs),
      method.body.map(dispatch),
      dispatch(method.contract),
    )(method.blame)(method.o)
  }

  private def getThreadRunMethod(run: RunMethod[Pre]): InstanceMethod[Post] = {
    new InstanceMethod[Post](
      TVoid[Post](),
      Seq.empty,
      Seq.empty,
      Seq.empty,
      run.body.map(dispatch),
      dispatch(run.contract),
    )(run.blame)(RunMethodOrigin(run))
  }

  override def dispatch(node: Expr[Pre]): Expr[Post] = {
    if (threadBuildingBlocks.nonEmpty) {
      val thread = threadBuildingBlocks.top.thread
      node match {
        case c: VeyMontCondition[Pre] =>
          paralleliseThreadCondition(node, thread, c)
        case m: MethodInvocation[Pre] => updateThreadRefMethodInvoc(thread, m)
        case d: Deref[Pre] => updateThreadRefInDeref(node, thread, d)
        case t: DerefVeyMontThread[Pre] =>
          updateThreadRefVeyMontDeref(node, thread, t)
        case _ => rewriteDefault(node)
      }
    } else
      rewriteDefault(node)
  }

  private def updateThreadRefVeyMontDeref(
      node: Expr[Pre],
      thread: VeyMontThread[Pre],
      t: DerefVeyMontThread[Pre],
  ) = {
    if (t.ref.decl == thread) {
      getThisVeyMontDeref(thread, t.o, threadBuildingBlocks.top.threadField)
    } else
      rewriteDefault(node)
  }

  private def updateThreadRefInDeref(
      node: Expr[Pre],
      thread: VeyMontThread[Pre],
      d: Deref[Pre],
  ) = {
    d.obj match {
      case t: DerefVeyMontThread[Pre] if t.ref.decl == thread =>
        d.rewrite(obj =
          getThisVeyMontDeref(thread, d.o, threadBuildingBlocks.top.threadField)
        )
      case _ => rewriteDefault(node)
    }
  }

  private def updateThreadRefMethodInvoc(
      thread: VeyMontThread[Pre],
      m: MethodInvocation[Pre],
  ) = {
    m.obj match {
      case threadRef: DerefVeyMontThread[Pre] =>
        m.rewrite(obj = dispatch(threadRef))
      case _ =>
        threadMethodSucc.get((thread, m.ref.decl)) match {
          case Some(postMethod) =>
            m.rewrite(
              obj = dispatch(m.obj),
              ref = postMethod.ref,
              m.args.map(dispatch),
            )
          case None =>
            throw ParalliseVeyMontThreadsError(
              m,
              "No successor for this method found",
            )
        }
    }
  }

  private def paralleliseThreadCondition(
      node: Expr[Pre],
      thread: VeyMontThread[Pre],
      c: VeyMontCondition[Pre],
  ) = {
    c.condition.find { case (threadRef, _) => threadRef.decl == thread } match {
      case Some((_, threadExpr)) => dispatch(threadExpr)
      case _ =>
        throw ParalliseVeyMontThreadsError(
          node,
          "Condition of if statement or while loop must contain an expression for every thread",
        )
    }
  }

  private def getThisVeyMontDeref(
      thread: VeyMontThread[Pre],
      o: Origin,
      threadField: InstanceField[Rewritten[Pre]],
  ) = {
    Deref(
      ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o),
      threadField.ref[InstanceField[Post]],
    )(null)(o)
  }

  override def dispatch(st: Statement[Pre]): Statement[Post] = {
    if (threadBuildingBlocks.nonEmpty) {
      val thread = threadBuildingBlocks.top.thread
      st match {
        case v: VeyMontCommExpression[Pre] =>
          paralleliseVeyMontCommExpr(thread, v, createParComBlocks(thread, v))
        case v @ VeyMontAssignExpression(threadRef, assign) =>
          if (threadRef.decl == thread)
            dispatch(assign)
          else
            Block(Seq.empty)(assign.o)
        case a: Assign[Pre] =>
          Assign(dispatch(a.target), dispatch(a.value))(a.blame)(a.o)
        case Branch(_) => rewriteDefault(st)
        case Loop(_, _, _, _, _) => rewriteDefault(st)
        case Scope(_, _) => rewriteDefault(st)
        case Block(_) => rewriteDefault(st)
        case Eval(expr) => paralleliseMethodInvocation(st, thread, expr)
        case _: Assert[Pre] => Block(Seq.empty)(st.o)
        case _ =>
          throw ParalliseVeyMontThreadsError(
            st,
            "Statement not allowed in seq_program",
          )
      }
    } else
      rewriteDefault(st)
  }

  private def createParComBlocks(
      thread: VeyMontThread[Pre],
      v: VeyMontCommExpression[Pre],
  ): ParallelCommExprBuildingBlocks[Pre] = {
    val channelField = threadBuildingBlocks.top.channelFields((v, v.o))
    val channelClass = threadBuildingBlocks.top.channelClasses(v.chanType)
    val thisChanField =
      Deref(
        ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(thread.o),
        channelField.ref[InstanceField[Post]],
      )(null)(v.assign.o)
    val assignment = v.assign.asInstanceOf[Assign[Pre]]
    new ParallelCommExprBuildingBlocks(
      channelField,
      channelClass,
      thisChanField,
      assignment,
    )
  }

  private def paralleliseMethodInvocation(
      st: Statement[Pre],
      thread: VeyMontThread[Pre],
      expr: Expr[Pre],
  ): Statement[Post] = {
    expr match {
      case m: MethodInvocation[Pre] =>
        m.obj match {
          case _: ThisSeqProg[Pre] =>
            Eval(m.rewrite(
              obj =
                ThisObject(threadClassSucc.ref[Post, Class[Post]](thread))(
                  thread.o
                ),
              ref = threadMethodSucc.ref((thread, m.ref.decl)),
            ))(st.o)
          case d: DerefVeyMontThread[Pre] =>
            if (d.ref.decl == thread)
              Eval(dispatch(expr))(st.o)
            else
              Block(Seq.empty)(st.o)
          case _ =>
            throw ParalliseVeyMontThreadsError(
              st,
              "Statement not allowed in seq_program",
            )
        }
      case _ =>
        throw ParalliseVeyMontThreadsError(
          st,
          "Statement not allowed in seq_program",
        )
    }
  }

  private def paralleliseVeyMontCommExpr(
      thread: VeyMontThread[Pre],
      v: VeyMontCommExpression[Pre],
      blocks: ParallelCommExprBuildingBlocks[Pre],
  ): Statement[Post] = {
    if (v.receiver.decl == thread) {
      val readMethod = findChannelClassMethod(
        v,
        blocks.channelClass,
        "readValue",
      )
      val assignValue =
        JavaInvocation(
          Some(blocks.thisChanField),
          Seq.empty,
          "readValue",
          Seq.empty,
          Seq.empty,
          Seq.empty,
        )(null)(v.o)
      assignValue.ref = Some(RefJavaMethod(readMethod))
      Assign(dispatch(blocks.assign.target), assignValue)(null)(v.o)
    } else if (v.sender.decl == thread) {
      val writeMethod = findChannelClassMethod(
        v,
        blocks.channelClass,
        "writeValue",
      )
      val writeInvoc =
        JavaInvocation(
          Some(blocks.thisChanField),
          Seq.empty,
          "writeValue",
          Seq(dispatch(blocks.assign.value)),
          Seq.empty,
          Seq.empty,
        )(null)(v.o)
      writeInvoc.ref = Some(RefJavaMethod(writeMethod))
      Eval(writeInvoc)(v.o)
    } else
      Block(Seq.empty)(blocks.assign.o)
  }

  private def findChannelClassMethod(
      v: VeyMontCommExpression[Pre],
      channelClass: JavaClass[Post],
      methodName: String,
  ): JavaMethod[Post] = {
    val method = channelClass.decls.find {
      case jm: JavaMethod[Post] => jm.name == methodName
      case _ => false
    }
    method match {
      case Some(m: JavaMethod[Post]) => m
      case _ =>
        throw ParalliseVeyMontThreadsError(
          v,
          "Could not find method `" + methodName + "' for channel class " +
            channelClass.name,
        )
    }
  }

}
