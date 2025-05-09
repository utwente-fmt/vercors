package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.ctx.Referrable
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError

case object EncodeByValueClassUsage extends RewriterBuilder {
  override def key: String = "encodeByValueClassUsage"

  override def desc: String =
    "Initialise ByValueClasses when they are declared and copy them whenever they're read"

  private case class ClassCopyInAssignmentFailed(
      blame: Blame[PointerDerefError],
      assign: Node[_],
      clazz: ByValueClass[_],
      field: InstanceField[_],
  ) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit = {
      if (blame.isInstanceOf[PanicBlame]) {
        assign.o
          .blame(CopyClassFailed(assign, clazz, Referrable.originName(field)))
      } else {
        blame
          .blame(CopyClassFailed(assign, clazz, Referrable.originName(field)))
      }
    }
  }

  private case class ClassCopyInCallFailed(
      blame: Blame[PointerDerefError],
      inv: Invocation[_],
      clazz: ByValueClass[_],
      field: InstanceField[_],
  ) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit = {
      blame.blame(
        CopyClassFailedBeforeCall(inv, clazz, Referrable.originName(field))
      )
    }
  }

  private case class InvocationBlameAdapter(blame: Blame[InvocationFailure])
      extends Blame[PointerDerefError] {
    override def blame(error: PointerDerefError) =
      error match {
        case e @ CopyClassFailed(_, _, _) => blame.blame(e)
        case e @ CopyClassFailedBeforeCall(_, _, _) => blame.blame(e)
        case _ => ???
      }
  }

  case class UnsupportedStructPerm(o: Origin) extends UserError {
    override def code: String = "unsupportedStructPerm"
    override def text: String =
      o.messageInContext(
        "Shorthand for Permissions for structs not possible, since the struct has a cyclic reference"
      )
  }

  private sealed class CopyContext

  private case class InCall(invocation: Invocation[_]) extends CopyContext

  private case class InAssignmentExpression(assignment: AssignExpression[_])
      extends CopyContext

  private case class InAssignmentStatement(assignment: Assign[_])
      extends CopyContext

}

case class EncodeByValueClassUsage[Pre <: Generation]() extends Rewriter[Pre] {

  import EncodeByValueClassUsage._

  private val classCreationMethodsSucc
      : SuccessionMap[TByValueClass[Pre], Procedure[Post]] = SuccessionMap()
  private val heapLocalArgSucc
      : SuccessionMap[Variable[Pre], LocalHeapVariable[Post]] = SuccessionMap()
  private val inContract: ScopedStack[Unit] = ScopedStack()

  // Duplicated in LowerHeapVariables, keep in sync
  def makeClassCreationMethod(t: TByValueClass[Pre]): Procedure[Post] = {
    implicit val o: Origin = t.cls.decl.o

    globalDeclarations.declare(withResult((result: Result[Post]) =>
      procedure[Post](
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = dispatch(t),
        ensures = UnitAccountedPredicate(
          unwrapClassPerm(result, Perm(_, WritePerm()), t)
        ),
        decreases = Some(DecreasesClauseNoRecursion[Post]()),
      )
    ))
  }

  override def dispatch(node: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = node.o
    node match {
      case assign: Assign[Pre] =>
        val target = dispatch(assign.target)
        assign.target.t match {
          case t: TByValueClass[Pre] =>
            doCopy(assign.value, target, t, InAssignmentStatement(assign))
          case _ => assign.rewrite(target = target)
        }
      case _ => node.rewriteDefault()
    }
  }

  private def copyClassValue2(
      obj: Expr[Post],
      t: TByValueClass[Pre],
      target: Expr[Post],
      blame: InstanceField[Pre] => Blame[InsufficientPermission],
  ): Statement[Post] = {
    implicit val o: Origin = obj.o
    val ov = new Variable[Post](dispatch(t))(o.where(name = "original"))
    val children = t.cls.decl.decls.collect { case f: InstanceField[Pre] =>
      f.t match {
        case inner: TByValueClass[Pre] =>
          copyClassValue2(
            Deref[Post](ov.get, succ(f))(blame(f)),
            inner,
            Deref[Post](target, succ(f))(DerefAssignTarget),
            blame,
          )
        case _ =>
          Assign[Post](
            Deref[Post](target, succ(f))(DerefAssignTarget),
            Deref[Post](ov.get, succ(f))(blame(f)),
          )(AssignLocalOk)
      }
    }
    Scope(Seq(ov), Block(assignLocal(ov.get, obj) +: children))
  }

  // Duplicated in LowerHeapVariables, keep in sync
  private def unwrapClassPerm(
      obj: Expr[Post],
      perm: Location[Post] => Expr[Post],
      structType: TByValueClass[Pre],
      visited: Seq[TByValueClass[Pre]] = Seq(),
  ): Expr[Post] = {
    if (visited.contains(structType))
      throw UnsupportedStructPerm(
        obj.o
      ) // We do not allow this notation for recursive structs
    implicit val o: Origin = obj.o
    val blame = PanicBlame("Field permission is framed")
    val fields = structType.cls.decl.decls.collect {
      case f: InstanceField[Pre] => f
    }
    val newFieldPerms = fields.map(member => {
      val loc = FieldLocation[Post](obj, succ(member))
      member.t match {
        case inner: TByValueClass[Pre] =>
          unwrapClassPerm(
            Deref[Post](obj, succ(member))(blame),
            perm,
            inner,
            structType +: visited,
          )
        case _ => perm(loc)
      }
    })

    foldStar(newFieldPerms)
  }

  override def dispatch(node: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = node.o
    node match {
      case NewObject(Ref(cls: ByValueClass[Pre])) =>
        val t = TByValueClass[Pre](cls.ref, Seq())
        procedureInvocation[Post](
          TrueSatisfiable,
          classCreationMethodsSucc
            .getOrElseUpdate(t, makeClassCreationMethod(t)).ref,
        )
      case Perm(ByValueClassLocation(e), p) =>
        val permission = dispatch(p)
        unwrapClassPerm(
          dispatch(e),
          Perm(_, permission),
          e.t.asByValueClass.get,
        )
      case Value(ByValueClassLocation(e)) =>
        unwrapClassPerm(dispatch(e), Value(_), e.t.asByValueClass.get)
      case AutoValue(ByValueClassLocation(e)) =>
        unwrapClassPerm(dispatch(e), AutoValue(_), e.t.asByValueClass.get)
      // Only doing this for TNonNullPointer pointers since those originate from the frontend and users can define heap variables of the normal TPointer pointer type
      case Perm(pl @ PointerLocation(dhv @ DerefHeapVariable(Ref(v))), p)
          if v.t.isInstanceOf[TNonNullPointer[Pre]] ||
            v.t.isInstanceOf[TNonNullConstPointer[Pre]] =>
        val t = v.t.asPointer.get
        if (t.element.asByValueClass.isDefined) {
          val newV: Ref[Post, HeapVariable[Post]] = succ(v)
          val newP = dispatch(p)
          Perm(HeapVariableLocation(newV), newP) &* Perm(
            PointerLocation(DerefHeapVariable(newV)(dhv.blame))(pl.blame),
            newP,
          )
        } else { node.rewriteDefault() }
      case assign: PreAssignExpression[Pre] =>
        val target = dispatch(assign.target)
        assign.t match {
          case t: TByValueClass[Pre] =>
            With(
              doCopy(assign.value, target, t, InAssignmentExpression(assign)),
              target,
            )
          case _ => assign.rewrite(target = target)
        }
      case invocation: Invocation[Pre] =>
        invocation.rewrite(args = invocation.args.map { a =>
          a.t match {
            case t: TByValueClass[Pre] =>
              val v = new Variable(dispatch(t))(a.o.where(name = "copy"))
              ScopedExpr(
                Seq(v),
                With(
                  Assign(
                    v.get(a.o),
                    procedureInvocation[Post](
                      TrueSatisfiable,
                      classCreationMethodsSucc
                        .getOrElseUpdate(t, makeClassCreationMethod(t)).ref,
                    )(a.o),
                  )(AssignLocalOk)(a.o),
                  With(
                    doCopy(a, v.get(a.o), t, InCall(invocation)),
                    v.get(a.o),
                  )(a.o),
                )(a.o),
              )(a.o)
            case _ => dispatch(a)
          }
        })
      case Local(Ref(v))
          if inContract.isEmpty && heapLocalArgSucc.contains(v) =>
        heapLocalArgSucc(v).get(PanicBlame(
          "Missing permission to procedure argument of struct type, no suitable blame available"
        ))
      case _ => node.rewriteDefault()
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      // TODO: AS: This transformation should be moved to LangCToCol to get proper blames
      case proc: Procedure[Pre] =>
        val byValueArgs = proc.args.filter(_.t.asByValueClass.isDefined)
        globalDeclarations.succeed(
          proc,
          proc.rewrite(
            body = proc.body.map(b =>
              Scope(
                Nil,
                localHeapVariables.scope {
                  Block(byValueArgs.flatMap { v =>
                    val newVar =
                      new LocalHeapVariable(
                        TNonNullPointer(dispatch(v.t), None)
                      )
                    heapLocalArgSucc(v) = newVar
                    Seq(
                      HeapLocalDecl(newVar),
                      Assign(
                        newVar.get(PanicBlame(
                          "Should always have access to local variable just after declaration"
                        )),
                        Local[Post](succ(v)),
                      )(AssignLocalOk),
                    )
                  } :+ dispatch(b))
                },
              )
            ),
            contract = inContract.having(()) { dispatch(proc.contract) },
          ),
        )
      case _ => super.dispatch(decl)
    }
  }

  private def doCopy(
      e: Expr[Pre],
      target: Expr[Post],
      t: TByValueClass[Pre],
      context: CopyContext,
  ): Statement[Post] =
    e match {
      case assign: PreAssignExpression[Pre] =>
        val innerTarget = dispatch(assign.target)
        Block(Seq(
          doCopy(assign.value, innerTarget, t, InAssignmentExpression(assign)),
          doCopy(assign.value, target, t, context),
        ))(assign.o)
      case invocation: Invocation[Pre] =>
        val newInvocation = invocation.rewrite(args = invocation.args.map { a =>
          a.t match {
            case t: TByValueClass[Pre] =>
              val v = new Variable(dispatch(t))(a.o.where(name = "copy"))
              ScopedExpr(
                Seq(v),
                With(
                  Assign(
                    v.get(a.o),
                    procedureInvocation[Post](
                      TrueSatisfiable,
                      classCreationMethodsSucc
                        .getOrElseUpdate(t, makeClassCreationMethod(t)).ref,
                    )(a.o),
                  )(AssignLocalOk)(a.o),
                  With(
                    doCopy(a, v.get(a.o), t, InCall(invocation)),
                    v.get(a.o),
                  )(a.o),
                )(a.o),
              )(a.o)
            case _ => dispatch(a)
          }
        })
        rewriteInCopyContext2(
          newInvocation,
          InvocationBlameAdapter(invocation.blame),
          t,
          target,
          context,
        )
      case dp @ DerefPointer(_) =>
        rewriteInCopyContext2(dispatch(dp), dp.blame, t, target, context)
      case ps @ PointerSubscript(_, _) =>
        rewriteInCopyContext2(dispatch(ps), ps.blame, t, target, context)
      case deref @ Deref(_, _) =>
        // TODO: Improve blame message here
        copyClassValue2(deref.rewriteDefault(), t, target, f => deref.blame)
      case Then(value, post) =>
        Block(Seq(doCopy(value, target, t, context), dispatch(post)))(e.o)
      case With(pre, value) =>
        Block(Seq(dispatch(pre), doCopy(value, target, t, context)))(e.o)
      case Local(Ref(v)) if heapLocalArgSucc.contains(v) =>
        rewriteInCopyContext2(
          heapLocalArgSucc(v).get(PanicBlame(
            "Missing permission to procedure argument of struct type, no suitable blame available"
          ))(e.o),
          PanicBlame(
            "Failed to copy struct that originated from a procedure argument, no suitable blame available"
          ),
          t,
          target,
          context,
        )
      case _ =>
        println(e)
        ???
    }

  private def rewriteInCopyContext2(
      e: Expr[Post],
      blame: Blame[PointerDerefError],
      t: TByValueClass[Pre],
      target: Expr[Post],
      context: CopyContext,
  ): Statement[Post] = {
    val cls = t.cls.decl.asInstanceOf[ByValueClass[Pre]]

    context match {
      case InCall(invocation) =>
        copyClassValue2(
          e,
          t,
          target,
          f => ClassCopyInCallFailed(blame, invocation, cls, f),
        )
      case InAssignmentExpression(assignment) =>
        copyClassValue2(
          e,
          t,
          target,
          f => ClassCopyInAssignmentFailed(blame, assignment, cls, f),
        )
      case InAssignmentStatement(assignment) =>
        copyClassValue2(
          e,
          t,
          target,
          f => ClassCopyInAssignmentFailed(blame, assignment, cls, f),
        )
    }
  }
}
