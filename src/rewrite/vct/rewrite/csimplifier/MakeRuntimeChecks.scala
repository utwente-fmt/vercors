package vct.rewrite.csimplifier

import vct.col.ast._
import vct.col.origin.{Origin, PanicBlame}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers.{VarBuildHelpers, tt}

case object MakeRuntimeChecks extends RewriterBuilder {
  override def key: String = "makeRuntimeChecks"

  override def desc: String =
    "Turn VerCors annotations into runtime checks for CPAchecker and similar"
}

case class MakeRuntimeChecks[Pre <: Generation]() extends Rewriter[Pre] {
  private var verifierAssert: Procedure[Post] = null
  private var verifierAssume: Procedure[Post] = null
  private var nondetInt: Procedure[Post] = null

  /** add CPAchecker-specific methods to program */
  override def dispatch(program: Program[Pre]): Program[Post] = {
    implicit val o: Origin = program.o

    // create abstract "abort" and "__verifier_nondet_int" methods
    lazy val abort =
      new Procedure[Post](TVoid(), Nil, Nil, Nil, None, emptyContract())(
        PanicBlame("abort failed")
      )(o.sourceName("abort"))
    nondetInt =
      new Procedure[Post](TCInt[Post](), Nil, Nil, Nil, None, emptyContract())(
        PanicBlame("nondet_int failed")
      )(o.sourceName("__VERIFIER_nondet_int"))

    createAssertMethod(abort)
    createAssumeMethod()

    lazy val other: Seq[GlobalDeclaration[Post]] =
      globalDeclarations.collect { program.declarations.foreach(dispatch) }._1

    program.rewrite(declarations =
      Seq(abort, verifierAssert, verifierAssume, nondetInt) ++ other
    )
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    stat match {
      case a: Assert[Pre] =>
        assert(a.res).getOrElse(Assert[Post](tt)(a.blame)(a.o))
      case a: Assume[Pre] => assume(a.expr).getOrElse(Assume[Post](tt)(a.o))
      case _ => super.dispatch(stat)
    }
  }

  /** remove subexpressions that CPAchecker cannot handle, e.g. quantifiers and
    * permissions. Todo: Only looking at top level, not checking for e.g.
    * quantifiers inside equalities
    */
  def dispatchExpr(node: Expr[Pre]): Option[Expr[Post]] = {
    node match {
      case _: Binder[Pre] => None // quantifiers
      case _: Perm[Pre] => None
      case a: Star[Pre] => // separating conjunct: check both subexpressions
        dispatchExpr(a.left) match {
          case Some(l) =>
            dispatchExpr(a.right) match {
              case Some(r) => Some(And[Post](l, r)(a.o))
              case None => Some(l)
            }
          case None => dispatchExpr(a.right)
        }
      case i: Implies[Pre] =>
        dispatchExpr(i.right) match {
          case Some(e) =>
            dispatchExpr(i.left).map(l => Or(Not[Post](l)(l.o), e)(i.o))
          case None => None
        }
      case _ => Some(super.dispatch(node))
    }
  }

  /** dispatch of expressions rewrites them, so the contract becomes invalid ->
    * replace with empty contract
    */
  private def emptyContract()(implicit o: Origin): ApplicableContract[Post] = {
    ApplicableContract[Post](
      UnitAccountedPredicate(tt),
      UnitAccountedPredicate(tt),
      tt,
      Nil,
      Nil,
      Nil,
      None,
    )(PanicBlame("trivial contract unsatisfiable"))
  }

  /** create "__verifier_assert" method, which calls "abort" if given condition
    * is false
    */
  private def createAssertMethod(
      abort: Procedure[Post]
  )(implicit o: Origin): Unit = {
    val arg = new Variable[Post](TInt())(o.sourceName("cond"))
    val eq = AmbiguousEq(arg.get, CIntegerValue(0), TCInt())
    val thenStmt = Label(
      new LabelDecl[Post]()(o.sourceName("ERROR")),
      InvokeProcedure[Post](abort.ref, Nil, Nil, Nil, Nil, Nil)(PanicBlame(
        "abort failed"
      )),
    )
    val body = Block[Post](Seq(Branch[Post](Seq((eq, thenStmt)))))
    verifierAssert =
      new Procedure[Post](
        TVoid(),
        Seq(arg),
        Nil,
        Nil,
        Some(body),
        emptyContract(),
      )(PanicBlame("abort failed"))(o.sourceName("__VERIFIER_assert"))
  }

  /** create "__verifier_assume" method, which loops forever if given condition
    * is false
    */
  private def createAssumeMethod()(implicit o: Origin): Unit = {
    val arg = new Variable[Post](TInt())(o.sourceName("cond"))
    val eq = AmbiguousEq(arg.get, CIntegerValue(0), TCInt())
    val emptyInv = LoopInvariant[Post](tt, None)(PanicBlame("assume failed"))
    val loop = Loop(Block(Nil), eq, Block(Nil), emptyInv, Block(Nil))
    val body = Block[Post](Seq(loop))
    verifierAssume =
      new Procedure[Post](
        TVoid(),
        Seq(arg),
        Nil,
        Nil,
        Some(body),
        emptyContract(),
      )(PanicBlame("assume failed"))(o.sourceName("__VERIFIER_assume"))
  }

  /** create a CPAchecker assert for given expression (if it is an expression
    * CPAchecker can handle)
    */
  def assert(expr: Expr[Pre]): Option[Statement[Post]] = {
    // TODO: figure out blame
    val rewritten = dispatchExpr(expr)
    rewritten match {
      case Some(e) =>
        if (e != tt[Post]) {
          val tern =
            Select(e, CIntegerValue(1)(expr.o), CIntegerValue(0)(expr.o))(
              expr.o
            )
          Some(
            InvokeProcedure[Post](
              verifierAssert.ref,
              Seq(tern),
              Nil,
              Nil,
              Nil,
              Nil,
            )(PanicBlame("Assert failed"))(expr.o)
          )
        } else { None }
      case None => None
    }
  }

  /** create a CPAchecker assumption (i.e. infinite loop if it does not hold)
    * for given expression (if it is an expression CPAchecker can handle)
    */
  def assume(expr: Expr[Pre]): Option[Statement[Post]] = {
    val rewritten = dispatchExpr(expr)
    rewritten match {
      case Some(e) =>
        if (e != tt[Post]) {
          val tern =
            Select(e, CIntegerValue(1)(expr.o), CIntegerValue(0)(expr.o))(
              expr.o
            )
          Some(
            InvokeProcedure[Post](
              verifierAssume.ref,
              Seq(tern),
              Nil,
              Nil,
              Nil,
              Nil,
            )(PanicBlame("Assume failed"))(expr.o)
          )
        } else { None }
      case None => None
    }
  }

}
