package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.ctx.Referrable
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}

// TODO: Think of a better name
case object PrepareByValueClass extends RewriterBuilder {
  override def key: String = "prepareByValueClass"

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

  case class PointerLocationDerefBlame(blame: Blame[PointerLocationError])
      extends Blame[PointerDerefError] {
    override def blame(error: PointerDerefError): Unit = {
      error match {
        case error: PointerLocationError => blame.blame(error)
        case _ =>
          Unreachable(
            "Blame of the respective pointer operation should be used not of DerefPointer"
          )
      }
    }
  }
}

case class PrepareByValueClass[Pre <: Generation]() extends Rewriter[Pre] {

  import PrepareByValueClass._

  private val inAssignment: ScopedStack[Unit] = ScopedStack()
  private val copyContext: ScopedStack[CopyContext] = ScopedStack()

  override def dispatch(node: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = node.o
    node match {
      case HeapLocalDecl(local)
          if local.t.asPointer.get.element.isInstanceOf[TByValueClass[Pre]] => {
        val newLocal = localHeapVariables.dispatch(local)
        val t = newLocal.t.asPointer.get.element
        Block(Seq(
          HeapLocalDecl(newLocal),
//          Assign(
//            HeapLocal[Post](newLocal.ref),
//            NewNonNullPointerArray(t, const(1))(PanicBlame("Size > 0")),
//          )(AssignLocalOk),
          // TODO: Only do this if the first use does not overwrite it again (do something similar to what I implemented in ImportPointer)....
          Assign(
            newLocal.get(DerefAssignTarget),
            NewObject(t.asInstanceOf[TByValueClass[Post]].cls),
          )(AssignLocalOk),
        ))
      }
      case assign: Assign[Pre] => {
        val target = inAssignment.having(()) { dispatch(assign.target) }
        copyContext.having(InAssignmentStatement(assign)) {
          assign.rewrite(target = target)
        }
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
    val ov = new Variable[Post](obj.t)
    val v =
      new Variable[Post](dispatch(t))(o.withContent(TypeName("HelloWorld")))
    val children = t.cls.decl.decls.collect { case f: InstanceField[Pre] =>
      f.t match {
        case inner: TByValueClass[Pre] =>
          Assign[Post](
            DerefPointer(Deref[Post](v.get, succ(f))(DerefAssignTarget))(
              NonNullPointerNull
            ),
            copyClassValue(Deref[Post](ov.get, succ(f))(blame(f)), inner, blame),
          )(AssignLocalOk)
        case _ =>
          Assign[Post](
            DerefPointer(Deref[Post](v.get, succ(f))(DerefAssignTarget))(
              NonNullPointerNull
            ),
            DerefPointer(Deref[Post](ov.get, succ(f))(blame(f)))(
              NonNullPointerNull
            ),
          )(AssignLocalOk)

      }
    }
    ScopedExpr(
      Seq(ov, v),
      Then(
        With(
          assignLocal(ov.get, obj),
          PreAssignExpression(v.get, NewObject[Post](succ(t.cls.decl)))(
            AssignLocalOk
          ),
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
      // TODO: Don't go through regular pointers...
      member.t.asPointer.get.element match {
        case inner: TByValueClass[Pre] =>
          Perm[Post](loc, perm) &* unwrapClassPerm(
            DerefPointer(Deref[Post](obj, succ(member))(blame))(
              NonNullPointerNull
            ),
            perm,
            inner,
            structType +: visited,
          )
        case _ => Perm(loc, perm)
      }
    })

    foldStar(newFieldPerms)
  }

  private def unwrapClassComp(
      comp: (Expr[Post], Expr[Post]) => Expr[Post],
      left: Expr[Post],
      right: Expr[Post],
      structType: TByValueClass[Pre],
      visited: Seq[TByValueClass[Pre]] = Nil,
  )(implicit o: Origin): Expr[Post] = {
    // TODO: Better error
    if (visited.contains(structType))
      throw UnsupportedStructPerm(o)

    val blame = PanicBlame("Struct deref can never fail")
    val fields = structType.cls.decl.decls.collect {
      case f: InstanceField[Pre] => f
    }
    foldAnd(fields.map(member => {
      val l =
        RawDerefPointer(Deref[Post](left, succ(member))(blame))(
          NonNullPointerNull
        )
      val r =
        RawDerefPointer(Deref[Post](right, succ(member))(blame))(
          NonNullPointerNull
        )
      member.t match {
//        case p: TNonNullPointer[Pre] if p.element.isInstanceOf[TByValueClass[Pre]] =>
//          unwrapClassComp(comp, DerefPointer(l)(NonNullPointerNull), r, p.element.asInstanceOf[TByValueClass[Pre]], structType +: visited)
        case _ => comp(l, r)
      }
    }))
  }

  override def dispatch(node: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = node.o
    node match {
      case Eq(left, right)
          if left.t == right.t && left.t.isInstanceOf[TByValueClass[Pre]] =>
        val newLeft = dispatch(left)
        val newRight = dispatch(right)
        return Eq(newLeft, newRight) && unwrapClassComp(
          (l, r) => Eq(l, r),
          newLeft,
          newRight,
          left.t.asInstanceOf[TByValueClass[Pre]],
        )
      case Neq(left, right)
          if left.t == right.t && left.t.isInstanceOf[TByValueClass[Pre]] =>
        val newLeft = dispatch(left)
        val newRight = dispatch(right)
        return Neq(newLeft, newRight) && unwrapClassComp(
          (l, r) => Neq(l, r),
          newLeft,
          newRight,
          left.t.asInstanceOf[TByValueClass[Pre]],
        )
      case _ => {}
    }
    if (inAssignment.nonEmpty)
      node.rewriteDefault()
    else
      node match {
        case Perm(ByValueClassLocation(e), p) =>
          unwrapClassPerm(
            dispatch(e),
            dispatch(p),
            e.t.asInstanceOf[TByValueClass[Pre]],
          )
        case Perm(pl @ PointerLocation(dhv @ DerefHeapVariable(Ref(v))), p)
            if v.t.isInstanceOf[TNonNullPointer[Pre]] =>
          val t = v.t.asInstanceOf[TNonNullPointer[Pre]]
          if (t.element.isInstanceOf[TByValueClass[Pre]]) {
            val newV: Ref[Post, HeapVariable[Post]] = succ(v)
            val newP = dispatch(p)
            Perm(HeapVariableLocation(newV), newP) &* Perm(
              PointerLocation(DerefHeapVariable(newV)(dhv.blame))(pl.blame),
              newP,
            )
          } else { node.rewriteDefault() }
        // What if I get rid of this...
//        case Perm(loc@PointerLocation(e), p) if e.t.asPointer.exists(t => t.element.isInstanceOf[TByValueClass[Pre]])=>
//          unwrapClassPerm(DerefPointer(dispatch(e))(PointerLocationDerefBlame(loc.blame))(loc.o), dispatch(p), e.t.asPointer.get.element.asInstanceOf[TByValueClass[Pre]])
        case assign: PreAssignExpression[Pre] =>
          val target = inAssignment.having(()) { dispatch(assign.target) }
          copyContext.having(InAssignmentExpression(assign)) {
            assign.rewrite(target = target)
          }
        case invocation: Invocation[Pre] => {
          copyContext.having(InCall(invocation)) { invocation.rewriteDefault() }
        }
        // WHOOPSIE WE ALSO MAKE A COPY IF IT WAS A POINTER
        case dp @ DerefPointer(HeapLocal(Ref(v)))
            if v.t.asPointer.get.element.isInstanceOf[TByValueClass[Pre]] =>
          rewriteInCopyContext(
            dp,
            v.t.asPointer.get.element.asInstanceOf[TByValueClass[Pre]],
          )
        case dp @ DerefPointer(DerefHeapVariable(Ref(v)))
            if v.t.asPointer.get.element.isInstanceOf[TByValueClass[Pre]] =>
          rewriteInCopyContext(
            dp,
            v.t.asPointer.get.element.asInstanceOf[TByValueClass[Pre]],
          )
        case dp @ DerefPointer(Deref(_, Ref(f)))
            if f.t.asPointer.get.element.isInstanceOf[TByValueClass[Pre]] =>
          rewriteInCopyContext(
            dp,
            f.t.asPointer.get.element.asInstanceOf[TByValueClass[Pre]],
          )
        case dp @ DerefPointer(Local(Ref(v)))
            if v.t.asPointer.get.element.isInstanceOf[TByValueClass[Pre]] =>
          // This can happen if the user specifies a local of type pointer to TByValueClass
          rewriteInCopyContext(
            dp,
            v.t.asPointer.get.element.asInstanceOf[TByValueClass[Pre]],
          )
        case _ => node.rewriteDefault()
      }
  }

  private def rewriteInCopyContext(
      dp: DerefPointer[Pre],
      t: TByValueClass[Pre],
  ): Expr[Post] = {
    if (copyContext.isEmpty) {
      // If we are in other kinds of expressions like if statements
      return dp.rewriteDefault()
    }
    val clazz = t.cls.decl.asInstanceOf[ByValueClass[Pre]]

    copyContext.top match {
      case InCall(invocation) =>
        copyClassValue(
          dp.rewriteDefault(),
          t,
          f => ClassCopyInCallFailed(dp.blame, invocation, clazz, f),
        )
      case InAssignmentExpression(assignment) =>
        copyClassValue(
          dp.rewriteDefault(),
          t,
          f => ClassCopyInAssignmentFailed(dp.blame, assignment, clazz, f),
        )
      case InAssignmentStatement(assignment) =>
        copyClassValue(
          dp.rewriteDefault(),
          t,
          f => ClassCopyInAssignmentFailed(dp.blame, assignment, clazz, f),
        )
    }
  }
}
