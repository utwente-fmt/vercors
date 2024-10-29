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

  private case class NoCopy() extends CopyContext
}

case class EncodeByValueClassUsage[Pre <: Generation]() extends Rewriter[Pre] {

  import EncodeByValueClassUsage._

  private val inAssignment: ScopedStack[Unit] = ScopedStack()
  private val copyContext: ScopedStack[CopyContext] = ScopedStack()
  copyContext.push(NoCopy())
  private val classCreationMethodsSucc
      : SuccessionMap[TByValueClass[Pre], Procedure[Post]] = SuccessionMap()

  def makeClassCreationMethod(t: TByValueClass[Pre]): Procedure[Post] = {
    implicit val o: Origin = t.cls.decl.o

    globalDeclarations.declare(withResult((result: Result[Post]) =>
      procedure[Post](
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = dispatch(t),
        ensures = UnitAccountedPredicate(
          unwrapClassPerm(result, WritePerm(), t)
        ),
        decreases = Some(DecreasesClauseNoRecursion[Post]()),
      )
    ))
  }

  override def dispatch(node: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = node.o
    node match {
      case HeapLocalDecl(local)
          if local.t.asPointer.get.element.asByValueClass.isDefined => {
        val t = local.t.asPointer.get.element.asByValueClass.get
        val newLocal = localHeapVariables.dispatch(local)
        Block(Seq(
          HeapLocalDecl(newLocal),
//          Assign(
//            HeapLocal[Post](newLocal.ref),
//            NewNonNullPointerArray(t, const(1))(PanicBlame("Size > 0")),
//          )(AssignLocalOk),
          // TODO: Only do this if the first use does not overwrite it again (do something similar to what I implemented in ImportPointer)....
          Assign(
            newLocal.get(DerefAssignTarget),
            procedureInvocation[Post](
              TrueSatisfiable,
              classCreationMethodsSucc
                .getOrElseUpdate(t, makeClassCreationMethod(t)).ref,
            ),
          )(AssignLocalOk),
        ))
      }
      case assign: Assign[Pre] =>
        val target = inAssignment.having(()) { dispatch(assign.target) }
        assign.target.t match {
          case _: TByValueClass[Pre] =>
            copyContext.having(InAssignmentStatement(assign)) {
              assign.rewrite(target = target)
            }
          case _ => assign.rewrite(target = target)
        }
      case _ => node.rewriteDefault()
    }
  }

  private def copyClassValue(
      obj: Expr[Post],
      t: TByValueClass[Pre],
      blame: InstanceField[Pre] => Blame[InsufficientPermission],
  ): Expr[Post] = {
    implicit val o: Origin = obj.o
    val ov = new Variable[Post](obj.t)(o.where(name = "original"))
    val v = new Variable[Post](dispatch(t))(o.where(name = "copy"))
    val children = t.cls.decl.decls.collect { case f: InstanceField[Pre] =>
      Assign[Post](
        Deref[Post](v.get, succ(f))(DerefAssignTarget),
        f.t match {
          case inner: TByValueClass[Pre] =>
            copyClassValue(Deref[Post](ov.get, succ(f))(blame(f)), inner, blame)
          case _ => Deref[Post](ov.get, succ(f))(blame(f))
        },
      )(AssignLocalOk)
    }
    ScopedExpr(
      Seq(ov, v),
      Then(
        With(
          assignLocal(ov.get, obj),
          PreAssignExpression(
            v.get,
            procedureInvocation[Post](
              TrueSatisfiable,
              classCreationMethodsSucc
                .getOrElseUpdate(t, makeClassCreationMethod(t)).ref,
            ),
          )(AssignLocalOk),
        ),
        Block(children),
      ),
    )
  }

  private def unwrapClassPerm(
      obj: Expr[Post],
      perm: Expr[Post],
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
          Perm[Post](loc, perm) &* unwrapClassPerm(
            Deref[Post](obj, succ(member))(blame),
            perm,
            inner,
            structType +: visited,
          )
        case _ => Perm(loc, perm)
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
      case _ if inAssignment.nonEmpty => node.rewriteDefault()
      case Perm(ByValueClassLocation(e), p) =>
        unwrapClassPerm(dispatch(e), dispatch(p), e.t.asByValueClass.get)
      case Perm(pl @ PointerLocation(dhv @ DerefHeapVariable(Ref(v))), p) =>
        assert(
          v.t.isInstanceOf[TNonNullPointer[Pre]],
          "Frontends should ensure that HeapVariables are non-null pointers",
        )
        val t = v.t.asInstanceOf[TNonNullPointer[Pre]]
        if (t.element.asByValueClass.isDefined) {
          val newV: Ref[Post, HeapVariable[Post]] = succ(v)
          val newP = dispatch(p)
          Perm(HeapVariableLocation(newV), newP) &* Perm(
            PointerLocation(DerefHeapVariable(newV)(dhv.blame))(pl.blame),
            newP,
          )
        } else { node.rewriteDefault() }
      case assign: PreAssignExpression[Pre] =>
        val target = inAssignment.having(()) { dispatch(assign.target) }
        if (assign.target.t.asByValueClass.isDefined) {
          copyContext.having(InAssignmentExpression(assign)) {
            assign.rewrite(target = target)
          }
        } else {
          // No need for copy semantics in this context
          copyContext.having(NoCopy()) { assign.rewrite(target = target) }
        }
      case invocation: Invocation[Pre] =>
        invocation.rewrite(args = invocation.args.map { a =>
          if (a.t.asByValueClass.isDefined) {
            copyContext.having(InCall(invocation)) { dispatch(a) }
          } else { copyContext.having(NoCopy()) { dispatch(a) } }
        })
      case dp @ DerefPointer(HeapLocal(Ref(v)))
          if v.t.asPointer.get.element.asByValueClass.isDefined =>
        rewriteInCopyContext(dp, v.t.asPointer.get.element.asByValueClass.get)
      case dp @ DerefPointer(DerefHeapVariable(Ref(v)))
          if v.t.asPointer.get.element.asByValueClass.isDefined =>
        rewriteInCopyContext(dp, v.t.asPointer.get.element.asByValueClass.get)
      case deref @ Deref(_, Ref(f))
          if f.t.asByValueClass.isDefined && copyContext.top != NoCopy() =>
        // TODO: Improve blame message here
        copyClassValue(
          deref.rewriteDefault(),
          f.t.asByValueClass.get,
          f => deref.blame,
        )
      case dp @ DerefPointer(Local(Ref(v)))
          if v.t.asPointer.get.element.asByValueClass.isDefined =>
        // This can happen if the user specifies a local of type pointer to TByValueClass
        rewriteInCopyContext(dp, v.t.asPointer.get.element.asByValueClass.get)
      case _ => node.rewriteDefault()
    }
  }

  private def rewriteInCopyContext(
      dp: DerefPointer[Pre],
      t: TByValueClass[Pre],
  ): Expr[Post] = {
    val cls = t.cls.decl.asInstanceOf[ByValueClass[Pre]]

    copyContext.top match {
      case InCall(invocation) =>
        copyClassValue(
          dp.rewriteDefault(),
          t,
          f => ClassCopyInCallFailed(dp.blame, invocation, cls, f),
        )
      case InAssignmentExpression(assignment) =>
        copyClassValue(
          dp.rewriteDefault(),
          t,
          f => ClassCopyInAssignmentFailed(dp.blame, assignment, cls, f),
        )
      case InAssignmentStatement(assignment) =>
        copyClassValue(
          dp.rewriteDefault(),
          t,
          f => ClassCopyInAssignmentFailed(dp.blame, assignment, cls, f),
        )
      case NoCopy() => dp.rewriteDefault()
    }
  }
}
