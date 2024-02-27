package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteApplicableContract, RewriteDeref}
import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers.tt
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable

case class CreateConstructor[Pre <: Generation](outer: Rewriter[Pre], val givenClassSucc: SuccessionMap[Type[Pre], Class[Rewritten[Pre]]]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  val rewritingConstr: ScopedStack[(Seq[Variable[Pre]], TClass[Pre])] = ScopedStack()

  /**
   * Dispatches the declarations and converts a procedure with a class return type to a constructor
   * @param decl
   */
  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case p: Procedure[Pre] => p.returnType match {
      case tc: TClass[Pre] => rewritingConstr.having(p.args, tc) {
        classDeclarations.declare(createClassConstructor(p))
      };
      case _ => ??? //("This procedure is expected to have a class as return type");
    }
    case other => rewriteDefault(other)
  }

  /**
   * Creates a constructor from a procedure
   * @param p
   * @return
   */
  def createClassConstructor(p: Procedure[Pre]): JavaConstructor[Post] = {
    implicit val origin: Origin = DiagnosticOrigin
    new JavaConstructor[Post](Seq(JavaPublic[Post]()),
      rewritingConstr.top._2.cls.decl.o.getPreferredNameOrElse(),
      p.args.map(createJavaParam),
      variables.dispatch(p.typeArgs),
      Seq.empty,
      p.body match {
        case Some(s: Scope[Pre]) => s.body match {
          case b: Block[Pre] => dispatch(Block(b.statements.tail.dropRight(1)))
          case other => dispatch(other)
        }
        case Some(_) => throw Unreachable("The body of a procedure always starts with a Scope.")
        case None => Block(Seq.empty)
      },
      p.contract.rewrite(ensures = UnitAccountedPredicate[Post](tt)))(null)
  }

  /**
   * Converts a variable to a Java parameter
   * @param v
   * @return
   */
  def createJavaParam(v: Variable[Pre]): JavaParam[Post] =
    new JavaParam[Post](Seq.empty, v.o.getPreferredNameOrElse(), dispatch(v.t))(v.o)

  /**
   * Converts expressions to Java expressions
   * @param e
   * @return
   */
  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case l: Local[Pre] =>
      if (rewritingConstr.nonEmpty && rewritingConstr.top._1.contains(l.ref.decl))
        JavaLocal[Post](l.ref.decl.o.getPreferredNameOrElse())(null)(e.o)
      else rewriteDefault(l)
    case t: ThisObject[Pre] =>
      val thisClassType = TClass(t.cls)
      if (rewritingConstr.nonEmpty && rewritingConstr.top._2 == thisClassType)
        ThisObject(givenClassSucc.ref[Post, Class[Post]](thisClassType))(t.o)
      else rewriteDefault(t)
    case d: Deref[Pre] =>
      if (rewritingConstr.nonEmpty)
        d.obj match {
          case _: Local[Pre] => d.rewrite(obj = ThisObject(givenClassSucc.ref[Post, Class[Post]](rewritingConstr.top._2))(d.o))
          case other => rewriteDefault(other)
        }
      else rewriteDefault(d)
    case other => super.dispatch(other)
  }

}
