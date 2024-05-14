package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.ctx.Referrable
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

case object EncodeByValueClass extends RewriterBuilder {
  override def key: String = "encodeByValueClass"

  override def desc: String =
    "Initialise ByValueClasses when they are declared and copy them whenever they're read"

  private case class ClassCopyInAssignmentFailed(
      blame: Blame[AssignFailed],
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
      blame: Blame[InvocationFailure],
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
}

case class EncodeByValueClass[Pre <: Generation]() extends Rewriter[Pre] {

  import EncodeByValueClass._

  private val inAssignment: ScopedStack[Unit] = ScopedStack()
  private val copyContext: ScopedStack[CopyContext] = ScopedStack()

  override def dispatch(node: Statement[Pre]): Statement[Post] =
    node match {
      case s: Scope[Pre] =>
        cPPLocalDeclarations.scope {
          cLocalDeclarations.scope {
            variables.scope {
              localHeapVariables.scope {
                val locals = variables.dispatch(s.locals)
                Scope(
                  locals,
                  Block(locals.collect {
                    case v: Variable[Post]
                        if v.t.isInstanceOf[TByValueClass[Post]] =>
                      Assign(
                        v.get(v.o),
                        NewObject(v.t.asInstanceOf[TByValueClass[Post]].cls)(v.o),
                      )(PanicBlame(
                        "Instantiating a ByValueClass should always succeed"
                      ))(v.o)
                  } ++ Seq(s.body.rewriteDefault()))(node.o),
                )(node.o)
              }
            }
          }
        }
      case assign: Assign[Pre] => {
        val target = inAssignment.having(()) { assign.target.rewriteDefault() }
        copyContext.having(InAssignmentStatement(assign)) {
          assign.rewrite(target = target)
        }
      }
      case _ => node.rewriteDefault()
    }

  private def copyClassValue(
      obj: Expr[Post],
      t: TByValueClass[Pre],
      blame: InstanceField[Pre] => Blame[InsufficientPermission],
  ): Expr[Post] = {
    implicit val o: Origin = obj.o
    val v = new Variable[Post](dispatch(t))
    val children = t.cls.decl.decls.collect { case f: InstanceField[Pre] =>
      f.t match {
        case inner: TByValueClass[Pre] =>
          Assign[Post](
            Deref[Post](v.get, succ(f))(DerefAssignTarget),
            copyClassValue(Deref[Post](obj, succ(f))(blame(f)), inner, blame),
          )(AssignLocalOk)
        case _ =>
          Assign[Post](
            Deref[Post](v.get, succ(f))(DerefAssignTarget),
            Deref[Post](obj, succ(f))(blame(f)),
          )(AssignLocalOk)

      }
    }
    ScopedExpr(
      Seq(v),
      Then(
        PreAssignExpression(v.get, NewObject[Post](succ(t.cls.decl)))(
          AssignLocalOk
        ),
        Block(children),
      ),
    )
  }

  // def unwrapClassPerm(
  //     struct: Expr[Post],
  //     perm: Expr[Pre],
  //     structType: TByValueClass[Pre],
  //     origin: Origin,
  //     visited: Seq[TByValueClass[Pre]] = Seq(),
  // ): Expr[Post] = {
  //   if (visited.contains(structType))
  //     throw UnsupportedStructPerm(
  //       origin
  //     ) // We do not allow this notation for recursive structs
  //   implicit val o: Origin = origin
  //   val blame = PanicBlame("Field permission is framed")
  //   val Seq(CStructDeclaration(_, fields)) = structType.ref.decl.decl.specs
  //   val newPerm = dispatch(perm)
  //   val AmbiguousLocation(newExpr) = struct
  //   val newFieldPerms = fields.map(member => {
  //     val loc =
  //       AmbiguousLocation(
  //         Deref[Post](
  //           newExpr,
  //           cStructFieldsSuccessor.ref((structType.ref.decl, member)),
  //         )(blame)
  //       )(struct.blame)
  //     member.specs.collectFirst {
  //       case CSpecificationType(newStruct: CTStruct[Pre]) =>
  //         // We recurse, since a field is another struct
  //         Perm(loc, newPerm) &* unwrapStructPerm(
  //           loc,
  //           perm,
  //           newStruct,
  //           origin,
  //           structType +: visited,
  //         )
  //     }.getOrElse(Perm(loc, newPerm))
  //   })

  //   foldStar(newFieldPerms)
  // }
  override def dispatch(node: Expr[Pre]): Expr[Post] =
    if (inAssignment.nonEmpty)
      node.rewriteDefault()
    else
      node match {
        case Perm(loc, p) => node.rewriteDefault()
        case assign: PreAssignExpression[Pre] =>
          val target =
            inAssignment.having(()) { assign.target.rewriteDefault() }
          copyContext.having(InAssignmentExpression(assign)) {
            assign.rewrite(target = target)
          }
        case invocation: Invocation[Pre] => {
          copyContext.having(InCall(invocation)) { invocation.rewriteDefault() }
        }
        case Local(Ref(v)) if v.t.isInstanceOf[TByValueClass[Pre]] =>
          if (copyContext.isEmpty) {
            return node.rewriteDefault()
          } // If we are in other kinds of expressions like if statements
          val t = v.t.asInstanceOf[TByValueClass[Pre]]
          val clazz = t.cls.decl.asInstanceOf[ByValueClass[Pre]]

          copyContext.top match {
            case InCall(invocation) =>
              copyClassValue(
                node.rewriteDefault(),
                t,
                f =>
                  ClassCopyInCallFailed(invocation.blame, invocation, clazz, f),
              )
            case InAssignmentExpression(assignment: PreAssignExpression[_]) =>
              copyClassValue(
                node.rewriteDefault(),
                t,
                f =>
                  ClassCopyInAssignmentFailed(
                    assignment.blame,
                    assignment,
                    clazz,
                    f,
                  ),
              )
            case InAssignmentExpression(assignment: PostAssignExpression[_]) =>
              copyClassValue(
                node.rewriteDefault(),
                t,
                f =>
                  ClassCopyInAssignmentFailed(
                    assignment.blame,
                    assignment,
                    clazz,
                    f,
                  ),
              )
            case InAssignmentStatement(assignment) =>
              copyClassValue(
                node.rewriteDefault(),
                t,
                f =>
                  ClassCopyInAssignmentFailed(
                    assignment.blame,
                    assignment,
                    clazz,
                    f,
                  ),
              )
          }
        case _ => node.rewriteDefault()
      }
}
