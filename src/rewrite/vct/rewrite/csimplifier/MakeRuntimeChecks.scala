package vct.rewrite.csimplifier

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{Origin, PanicBlame}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers.{VarBuildHelpers, assignLocal, tt}

import scala.collection.mutable

case object MakeRuntimeChecks extends RewriterBuilder {
  override def key: String = "makeRuntimeChecks"

  override def desc: String =
    "Turn VerCors annotations into runtime checks for CPAchecker and similar"
}

case class MakeRuntimeChecks[Pre <: Generation]() extends Rewriter[Pre] {
  private var verifierAssert: Procedure[Post] = null
  private var verifierAssume: Procedure[Post] = null
  private var nondetInt: Procedure[Post] = null

  private val currentMethod: ScopedStack[Procedure[Pre]] = new ScopedStack()
  private val returnVar: ScopedStack[Variable[Post]] = new ScopedStack()
  // currently active mapping of global vars to local vars (used inside \old)
  private val globalVars: ScopedStack[Map[HeapVariable[Pre], Variable[Post]]] =
    new ScopedStack()
  // mapping of global vars to local vars storing cached value (for \old)
  private val olds
      : ScopedStack[mutable.Map[HeapVariable[Pre], Variable[Post]]] =
    new ScopedStack()

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

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case m: Procedure[Pre] => dispatchMethod(m).succeed(m)
      case _ => super.dispatch(decl)
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    stat match {
      case a: Assert[Pre] =>
        assert(a.res).getOrElse(Assert[Post](tt)(a.blame)(a.o))
      case a: Assume[Pre] => assume(a.expr).getOrElse(Assume[Post](tt)(a.o))
      case r: Return[Pre] => rewriteReturn(r)
      case _ => super.dispatch(stat)
    }
  }

  /** rewrite expressions for runtime checking, e.g. replacing "\result" with
    * reference to return value
    */
  override def dispatch(node: Expr[Pre]): Expr[Post] = {
    node match {
      case r: Result[Pre] => returnVar.top.get(r.o)
      case o: Old[Pre] => globalVars.having(olds.top.toMap) { dispatch(o.expr) }
      case h: DerefHeapVariable[Pre] =>
        if (globalVars.isEmpty) { super.dispatch(h) }
        else { globalVars.top(h.ref.decl).get(h.o) }
      case _ => super.dispatch(node)
    }
  }

  /** turn preconditions into assumptions (and postconditions into assertions if
    * void method)
    */
  private def dispatchMethod(meth: Procedure[Pre]): Procedure[Post] = {
    currentMethod
      .having(meth) { // remember current method, so that dispatchStatement can reference it
        // cache old values for all heap locations referenced in "\old"
        // todo: labeled old?
        var oldExprs = meth.contract.ensures.collect {
          case o: Old[Pre] if o.at.isEmpty => o.expr
        }
        if (meth.body.nonEmpty) {
          oldExprs =
            oldExprs ++ meth.body.get.collect {
              case o: Old[Pre] if o.at.isEmpty => o.expr
            }
        }
        val derefs = oldExprs
          .flatMap(e => e.collect { case r: DerefHeapVariable[Pre] => r })
        olds.having(mutable.Map()) {
          for (r <- derefs) {
            if (!olds.top.contains(r.ref.decl)) {
              val newOrigin = r.o.sourceName(
                "__old_" + r.ref.decl.o.getPreferredNameOrElse().camel
              )
              val old = new Variable(dispatch(r.ref.decl.t))(newOrigin)
              olds.top.addOne(r.ref.decl, old)
            }
          }
          val cached = olds.top.map { case (r, old) =>
            implicit val o: Origin = r.o
            assignLocal(
              old.get,
              new DerefHeapVariable[Post](succ(r.ref.decl))(PanicBlame(
                "caching old failed"
              )),
            )
          }

          // assume precondition
          val pres = gatherConditions(meth.contract.requires).flatMap(assume)

          // if non-void, then return statements check postcondition. if void, do it at end of method
          val posts =
            if (meth.returnType == TVoid[Pre]())
              gatherConditions(meth.contract.ensures).flatMap(assert)
            else
              Seq()

          // rewrite method with new body
          val body = meth.body.map {
            case b: Block[Pre] =>
              val newScope =
                Scope[Post](
                  olds.top.values.toSeq,
                  Block[Post](
                    cached.toSeq ++ b.statements.map(dispatch) ++ posts
                  )(b.o),
                )(b.o)
              b.rewrite(statements = pres ++ Seq(newScope))
            case stmt =>
              val newScope =
                Scope[Post](
                  olds.top.values.toSeq,
                  Block[Post](cached.toSeq ++ Seq(dispatch(stmt)) ++ posts)(
                    meth.o
                  ),
                )(meth.o)
              Block[Post](pres ++ Seq(newScope))(meth.o)
          }
          meth.rewrite(body = body, contract = emptyContract()(meth.o))
        }
      }
  }

  /** turn "return expr;" into "<rtype> result = expr; assert <postconditions>;
    * return result;"
    */
  private def rewriteReturn(r: Return[Pre]): Statement[Post] = {
    val meth = currentMethod.top
    val retV =
      new Variable(dispatch(meth.returnType))(
        meth.o.sourceName("__verifier_result")
      )
    returnVar
      .having(retV) { // remember ref to result variable so that dispatchExpr can reference it
        val posts = gatherConditions(meth.contract.ensures).flatMap(assert)
        if (posts.nonEmpty) {
          val d = LocalDecl(retV)(retV.o)
          val asgn = assignLocal(retV.get(r.o), dispatch(r.result))(r.o)
          val ret = Return(retV.get(r.o))(r.o)
          Block[Post](Seq(d, asgn) ++ posts ++ Seq(ret))(r.o)
        } else { super.dispatch(r) }
      }
  }

  /** remove subexpressions that CPAchecker cannot handle, e.g. quantifiers and
    * permissions. Todo: Only looking at top level, not checking for e.g.
    * quantifiers inside equalities
    */
  private def dispatchExpr(node: Expr[Pre]): Option[Expr[Post]] = {
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

  /** create "&#95;&#95;verifier_assert" method, which calls "abort" if given
    * condition is false
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

  /** create "&#95;&#95;verifier_assume" method, which loops forever if given
    * condition is false
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

  /** gather all expressions that this contract clause contains */
  def gatherConditions(pred: AccountedPredicate[Pre]): Seq[Expr[Pre]] = {
    pred match {
      case unit: UnitAccountedPredicate[Pre] => Seq(unit.pred)
      case split: SplitAccountedPredicate[Pre] =>
        gatherConditions(split.left) ++ gatherConditions(split.right)
    }
  }

}
