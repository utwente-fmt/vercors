package vct.rewrite.csimplifier

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{
  AssignFailed,
  Blame,
  CallableFailure,
  ContractedFailure,
  ExceptionNotInSignals,
  Origin,
  PanicBlame,
  SignalsFailed,
}
import vct.col.print.Namer
import vct.col.ref.Ref
import vct.col.rewrite.{
  Generation,
  NonLatchingRewriter,
  Rewriter,
  RewriterBuilder,
  Rewritten,
}
import vct.col.util.AstBuildHelpers.{VarBuildHelpers, assignLocal, tt}
import vct.col.util.SuccessionMap

import scala.collection.mutable

case object MakeRuntimeChecks extends RewriterBuilder {
  override def key: String = "makeRuntimeChecks"

  override def desc: String =
    "Turn VerCors annotations into runtime checks for CPAchecker and similar"

  case class FunctionBlameToProcedureBlame(blame: Blame[ContractedFailure])
      extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit =
      error match {
        case failure: ContractedFailure => blame.blame(failure)
        case SignalsFailed(_, _) => PanicBlame("Function had signals clause")
        case ExceptionNotInSignals(_) =>
          PanicBlame("Function had signals clause")
      }
  }
}

case class MakeRuntimeChecks[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  outer =>
  private val verifierAssertName = "__VERIFIER_assert"
  private val verifierAssumeName = "__VERIFIER_assume"
  private val abortName = "reach_error"
  private val nondetIntName = "__VERIFIER_nondet_int"

  private var verifierAssert: Procedure[Post] = null
  private var verifierAssume: Procedure[Post] = null
  private var nondetInt: Procedure[Post] = null
  private var abort: Procedure[Post] = null

  private val currentMethod: ScopedStack[Procedure[Pre]] = new ScopedStack()
  private val simplifiedMethodRefs
      : SuccessionMap[Procedure[Pre], Procedure[Post]] = SuccessionMap()
  private val returnVar: ScopedStack[Variable[Post]] = new ScopedStack()
  // currently active mapping of global vars to local vars (used inside \old)
  private val globalVars: ScopedStack[Map[HeapVariable[Pre], Variable[Post]]] =
    new ScopedStack()
  // mapping of global vars to local vars storing cached value (for \old)
  private val olds: ScopedStack[mutable.Map[Expr[Pre], Variable[Post]]] =
    new ScopedStack()

  /** add CPAchecker-specific methods to program */
  override def dispatch(program: Program[Pre]): Program[Post] = {
    implicit val o: Origin = program.o

    // create abstract "reach_error" and "__verifier_nondet_int" methods;
    val declNames = Map.from(program.declarations.flatMap(d =>
      d match {
        case p: Procedure[Pre] => Namer.getSrcName(p.o).map(n => (n, p))
        case _ => None
      }
    ))

    abort =
      new Procedure[Post](TVoid(), Nil, Nil, Nil, None, emptyContract())(
        PanicBlame(abortName + " failed")
      )(o.sourceName(abortName))

    nondetInt =
      new Procedure[Post](TCInt[Post](), Nil, Nil, Nil, None, emptyContract())(
        PanicBlame("nondet_int failed")
      )(o.sourceName(nondetIntName))

    verifierAssert = createAssertMethod(abort.ref)

    verifierAssume = createAssumeMethod()

    lazy val decls: Seq[GlobalDeclaration[Post]] = globalDeclarations
      .dispatch(program.declarations)

    // declare above methods as successors to existing methods of that name if possible,
    // otherwise declare as new methods
    val newDecls: Seq[GlobalDeclaration[Post]] = Seq(
      abort,
      nondetInt,
      verifierAssert,
      verifierAssume,
    ).filter(d => !declNames.contains(Namer.getSrcName(d.o).get))

    program.rewrite(declarations = newDecls ++ decls)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case f: Function[Pre] =>
        val p =
          new Procedure[Pre](
            f.returnType,
            f.args,
            Nil,
            f.typeArgs,
            f.body.map(e => Return[Pre](e)(e.o)),
            f.contract,
          )(MakeRuntimeChecks.FunctionBlameToProcedureBlame(f.blame))(f.o)
        rewriteProcedure(p).succeed(f)
      case pred: Predicate[Pre] =>
        val p =
          new Procedure[Pre](
            TCInt[Pre](),
            pred.args,
            Nil,
            Nil,
            pred.body.map(e => Return[Pre](e)(e.o)),
            emptyContract[Pre]()(pred.o),
          )(PanicBlame("predicate failed"))(pred.o)
        rewriteProcedure(p).succeed(pred)
      case m: Procedure[Pre] => rewriteProcedure(m).succeed(m)
      case v: Variable[Pre] => super.dispatch(v)
      case _ => super.dispatch(decl)
    }
  }

  private def rewriteProcedure(m: Procedure[Pre]): GlobalDeclaration[Post] = {
    // declare new method as replacement of existing method of same name
    Namer.getSrcName(m.o) match {
      case Some(name) if name == verifierAssertName => return verifierAssert
      case Some(name) if name == abortName => return abort
      case Some(name) if name == verifierAssumeName => return verifierAssume
      case Some(name) if name == nondetIntName => return nondetInt
      case Some(name) if name == "__vercors_malloc" =>
        // todo: a bit hack-y: use classical malloc by turning this method
        //  into include statement.
        //  This means the include is followed by a ";", though
        implicit val o: Origin = m.o
        val decl =
          new CGlobalDeclaration[Post](CDeclaration(
            m.contract.rewriteDefault(),
            tt,
            Nil,
            Seq(CInit(CName[Post]("#include<stdlib.h>"), None)),
          ))
        return decl
      case _ =>
    }

    val rewritten = dispatchMethod(m, simplify = false)
    if (m.body.isDefined) {
      val simplified = variables.scope { dispatchMethod(m, simplify = true) }
      // add simplified versions for non-abstract methods
      globalDeclarations.declare(simplified)
      simplifiedMethodRefs.update(m, simplified)
    } else { simplifiedMethodRefs.update(m, rewritten) }
    rewritten
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    stat match {
      case a: Assert[Pre] =>
        assert(a.res).getOrElse(Assert[Post](tt)(a.blame)(a.o))
      case a: Assume[Pre] => assume(a.expr).getOrElse(Assume[Post](tt)(a.o))
      case r: Return[Pre] => rewriteReturn(r)
      case l: Loop[Pre] => annotateLoop(l)
      case l: SimplifiedLoop[Pre] => rewriteLoop(l)
      case _: WandApply[Pre] => Block[Post](Nil)(stat.o)
      case _: WandPackage[Pre] => Block[Post](Nil)(stat.o)
      case _: Fold[Pre] => Block[Post](Nil)(stat.o)
      case _: Unfold[Pre] => Block[Post](Nil)(stat.o)
      case _ => super.dispatch(stat)
    }
  }

  /** rewrite expressions for runtime checking, e.g. replacing "\result" with
    * reference to return value
    */
  override def dispatch(node: Expr[Pre]): Expr[Post] = {
    node match {
      case r: Result[Pre] => returnVar.top.get(r.o)
      case o: Old[Pre] =>
        olds.top.get(o.expr) match {
          case Some(v) => Local[Post](v.ref)(v.o)
          case None => dispatch(o.expr) // should not be possible
        }
      case h: DerefHeapVariable[Pre] =>
        if (globalVars.isEmpty) { super.dispatch(h) }
        else { globalVars.top(h.ref.decl).get(h.o) }
      case s: SimplifiedProcedureInvocation[Pre] =>
        ProcedureInvocation[Post](
          simplifiedMethodRefs.ref(s.ref.decl),
          s.args.map(dispatch),
          Nil,
          Nil,
          Nil,
          Nil,
        )(s.blame)(s.o)
      case i: FunctionInvocation[Pre] =>
        ProcedureInvocation[Post](
          succ(i.ref.decl),
          i.args.map(dispatch),
          Nil,
          Nil,
          Nil,
          Nil,
        )(i.blame)(i.o)
      case i: PredicateApply[Pre] =>
        ProcedureInvocation[Post](
          succ(i.ref.decl),
          i.args.map(dispatch),
          Nil,
          Nil,
          Nil,
          Nil,
        )(PanicBlame("failed to apply predicate"))(i.o)
      case r: Scale[Pre] => dispatch(r.res)
      case i: Implies[Pre] =>
        Or(Not[Post](dispatch(i.left))(i.left.o), dispatch(i.right))(i.o)
      case u: Unfolding[Pre] => dispatch(u.body)
      case _ => super.dispatch(node)
    }
  }

  override def dispatch(node: Type[Pre]): Type[Post] = {
    node match {
      case _: TResource[Pre] => TCInt[Post]()
      case _ => super.dispatch(node)
    }
  }

  /** turn preconditions into assumptions (and postconditions into assertions if
    * void method)
    */
  private def dispatchMethod(
      meth: Procedure[Pre],
      simplify: Boolean,
  ): Procedure[Post] = {
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
//        val derefs = oldExprs
//          .flatMap(e => e.collect {
//            case r: DerefHeapVariable[Pre] => r
////            case p: DerefPointer[Pre] => p
////            case s: AmbiguousSubscript[Pre] => s
//          })
        olds.having(mutable.Map()) {
          for (e <- oldExprs) {
            val newOrigin = e.o.sourceName("__old_expr")
            val old = new Variable(dispatch(e.t))(newOrigin)
            olds.top.addOne(e, old)
          }
//          for (r <- derefs) {
//            if (!olds.top.contains(r.ref.decl)) {
//              val newOrigin = r.o.sourceName(
//                "__old_" + r.ref.decl.o.getPreferredNameOrElse().camel
//              )
//              val old = new Variable(dispatch(r.ref.decl.t))(newOrigin)
//              olds.top.addOne(r.ref.decl, old)
//            }
//          }
          val cached = olds.top.map { case (e, old) =>
            implicit val o: Origin = e.o
            assignLocal(old.get, dispatch(e))
          }

          // generate new method body
          if (simplify) {
            // check preconditions
            val pres = gatherConditions(meth.contract.requires).flatMap(assert)

            // havoc global assignment targets
            val havocced =
              meth.body match {
                case Some(b) => havocGlobals(b)
                case None => Seq()
              }

            // assume postcondition
            implicit val o: Origin = meth.o
            val newBody =
              if (meth.returnType == TVoid[Pre]()) {
                val newPost = Scope[Post](
                  olds.top.values.toSeq,
                  Block[Post](cached.toSeq ++ havocced ++ assumePost(meth)),
                )
                Block[Post](pres ++ Seq(newPost))(meth.o)
              } else {
                val retVar =
                  new Variable[Post](meth.returnType.rewriteDefault())(
                    meth.o.sourceName("returnValue")
                  )
                returnVar.having(retVar) {
                  lazy val ret = Return[Post](retVar.get)
                  val newPost = Scope[Post](
                    olds.top.values.toSeq ++ Seq(retVar),
                    Block[Post](
                      cached.toSeq ++ havocced ++ assumePost(meth) ++ Seq(ret)
                    ),
                  )
                  Block[Post](pres ++ Seq(newPost))
                }
              }

            // declare new procedure with the simplified body
            val newOrigin = meth.o.sourceName(
              meth.o.getPreferredNameOrElse().snake + "__simplified"
            )
            val newProc = meth.rewrite(
              body = Some(newBody),
              contract = emptyContract(),
              o = newOrigin,
            )
            newProc
          } else { // i.e. not simplifying
            // assume precondition
            val pres = gatherConditions(meth.contract.requires).flatMap(assert)

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
          val asgn = dispatchExpr(r.result)
            .map(e => assignLocal(retV.get(r.o), e)(r.o))
          val ret = Return(retV.get(r.o))(r.o)
          Block[Post](Seq(d) ++ asgn ++ posts ++ Seq(ret))(r.o)
        } else {
          dispatchExpr(r.result) match {
            case Some(e) => r.rewrite(result = e)
            case _ => Block[Post](Nil)(r.o)
          }
        }
      }
  }

  /** annotate loop with its invariant as assertion
    */
  private def annotateLoop(loop: Loop[Pre]): Statement[Post] = {
    loop.contract match {
      case inv: LoopInvariant[Pre] =>
        val contract = LoopInvariant[Post](tt, None)(inv.blame)(loop.o)
        val ass = assert(inv.invariant)
        ass match {
          case Some(a) =>
            val body =
              Block[Post](Seq(a, loop.body.rewriteDefault()))(loop.body.o)
            loop.rewrite(body = body, contract = contract)
          case _ => loop.rewrite(contract = contract)
        }
      case _ => super.dispatch(loop) // todo? iterationContract, not handled yet
    }
  }

  /** replace loop with its invariant, i.e. "assert inv; havoc
    * assignedVariables; assume inv;"
    */
  private def rewriteLoop(loop: SimplifiedLoop[Pre]): Statement[Post] = {
    loop.contract match {
      case inv: LoopInvariant[Pre] =>
        val decls = loop.body.collect { case d: LocalDecl[Pre] => d }
        val rewrittenDecls = decls.map(dispatch)
        lazy val havocUpdate =
          havoc(gatherAssigns(loop.update)) ++ havocGlobals(loop.update)
        lazy val havocBody =
          havoc(gatherAssigns(loop.body)) ++ havocGlobals(loop.body)
        Block[Post](
          Seq(dispatch(loop.init)) ++ assert(inv.invariant).toSeq ++
            rewrittenDecls ++ havocUpdate ++ assume(inv.invariant).toSeq ++
            assume(loop.cond) ++ havocBody ++ havocUpdate ++
            assume(inv.invariant).toSeq ++
            assume(Not[Pre](loop.cond)(loop.cond.o)).toSeq
        )(loop.o)
      case _ => super.dispatch(loop) // todo? iterationContract, not handled yet
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
      case _: Value[Pre] => None
      case _: PermPointer[Pre] => None
      case _: Wand[Pre] => None
      case a: Star[Pre] => // separating conjunct: check both subexpressions
        dispatchExpr(a.left) match {
          case Some(l) if l != tt[Post] =>
            dispatchExpr(a.right) match {
              case Some(r) if r != tt[Post] => Some(And[Post](l, r)(a.o))
              case _ => Some(l)
            }
          case _ => dispatchExpr(a.right)
        }
      case a: And[Pre] => // conjunction: check both subexpressions
        dispatchExpr(a.left) match {
          case Some(l) if l != tt[Post] =>
            dispatchExpr(a.right) match {
              case Some(r) if r != tt[Post] => Some(And[Post](l, r)(a.o))
              case _ => Some(l)
            }
          case _ => dispatchExpr(a.right)
        }
      case i: Implies[Pre] =>
        dispatchExpr(i.right).flatMap(e =>
          dispatchExpr(i.left).map(l => Or(Not[Post](l)(l.o), e)(i.o))
        )
      case o: Or[Pre] =>
        dispatchExpr(o.left) match {
          case Some(l) if l != tt[Post] =>
            dispatchExpr(o.right) match {
              case Some(r) if r != tt[Post] => Some(Or[Post](l, r)(o.o))
              case _ => Some(l)
            }
          case _ =>
            dispatchExpr(o.right) match {
              case Some(r) if r != tt[Post] => Some(r)
              case _ => None
            }
        }
      // TODO: filter other non-C expressions, e.g. seqs, \array
      case _ => Some(dispatch(node))
    }
  }

  /** dispatch of expressions rewrites them, so the contract becomes invalid ->
    * replace with empty contract
    */
  private def emptyContract[T]()(implicit o: Origin): ApplicableContract[T] = {
    ApplicableContract[T](
      UnitAccountedPredicate(tt),
      UnitAccountedPredicate(tt),
      tt,
      Nil,
      Nil,
      Nil,
      None,
    )(PanicBlame("trivial contract unsatisfiable"))
  }

  /** create "&#95;&#95;verifier_assert" method, which calls "reach_error" if
    * given condition is false
    */
  private def createAssertMethod(
      abort: Ref[Post, Procedure[Post]]
  )(implicit o: Origin): Procedure[Post] = {
    val arg = new Variable[Post](TInt())(o.sourceName("cond"))
    val eq = AmbiguousEq(arg.get, CIntegerValue(0), TCInt())
    val thenStmt = Label(
      new LabelDecl[Post]()(o.sourceName("ERROR")),
      InvokeProcedure[Post](abort, Nil, Nil, Nil, Nil, Nil)(PanicBlame(
        abortName + " failed"
      )),
    )
    val body = Block[Post](Seq(Branch[Post](Seq((eq, thenStmt)))))
    new Procedure[Post](
      TVoid(),
      Seq(arg),
      Nil,
      Nil,
      Some(body),
      emptyContract(),
    )(PanicBlame("calling assert failed"))(o.sourceName(verifierAssertName))
  }

  /** create "&#95;&#95;verifier_assume" method, which loops forever if given
    * condition is false
    */
  private def createAssumeMethod()(implicit o: Origin): Procedure[Post] = {
    val arg = new Variable[Post](TInt())(o.sourceName("cond"))
    val eq = AmbiguousEq(arg.get, CIntegerValue(0), TCInt())
    val emptyInv = LoopInvariant[Post](tt, None)(PanicBlame("assume failed"))
    val loop = Loop(Block(Nil), eq, Block(Nil), emptyInv, Block(Nil))
    val body = Block[Post](Seq(loop))
    new Procedure[Post](
      TVoid(),
      Seq(arg),
      Nil,
      Nil,
      Some(body),
      emptyContract(),
    )(PanicBlame("calling assume failed"))(o.sourceName(verifierAssumeName))
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

  /** turn postcondition into assumption */
  private def assumePost(proc: Procedure[Pre]): Seq[Statement[Post]] = {
    gatherConditions(proc.contract.ensures).flatMap(assume)
  }

  /** havoc given assignment targets
    * @param assigns:
    *   pairs of (assigmentTarget, Blame)
    */
  private def havoc(
      assigns: Iterable[(Expr[Pre], Blame[AssignFailed])]
  ): Seq[Statement[Post]] = {

    /** Rewriter for array subscripts, whose index is also must be havocced. So
      * this rewriter creates an index variable for each array access (not
      * havocced yet)
      */
    class SubscriptHavoccer extends NonLatchingRewriter[Pre, Post] {
      override val allScopes: AllScopes[Pre, Post] = outer.allScopes
      var indices: Seq[Variable[Post]] = Seq()

      override def dispatch(e: Expr[Pre]): Expr[Post] = {
        e match {
          case s @ AmbiguousSubscript(a, i) =>
            val idx = new Variable[Post](TCInt())(i.o.sourceName("idx"))
            indices :+= idx
            AmbiguousSubscript[Post](dispatch(a), Local[Post](idx.ref)(i.o))(
              s.blame
            )(e.o)
          case _ => e.rewriteDefault()
        }
      }
    }

    assigns.flatMap { case (t, b) =>
      val rwrtr = new SubscriptHavoccer()
      val target = rwrtr.dispatch(t)
      // havoc all index variables created by rwrtr
      val indices = rwrtr.indices.flatMap(v =>
        Seq(
          LocalDecl(v)(v.o),
          Assign[Post](
            Local[Post](v.ref)(v.o),
            ProcedureInvocation[Post](nondetInt.ref, Nil, Nil, Nil, Nil, Nil)(
              PanicBlame("havoc failed")
            )(t.o),
          )(b)(t.o),
          // add assumption "idx >= 0 ? 1 : 0"
          InvokeProcedure[Post](
            verifierAssume.ref,
            Seq(
              Select(
                GreaterEq(Local[Post](v.ref)(v.o), CIntegerValue(0)(v.o))(v.o),
                CIntegerValue(1)(v.o),
                CIntegerValue(0)(v.o),
              )(v.o)
            ),
            Nil,
            Nil,
            Nil,
            Nil,
          )(PanicBlame("Assume failed"))(t.o),
        )
      )
      val asgn =
        Assign[Post](
          target,
          ProcedureInvocation[Post](nondetInt.ref, Nil, Nil, Nil, Nil, Nil)(
            PanicBlame("havoc failed")
          )(t.o),
        )(b)(t.o)
      indices ++ Seq(asgn)
    }.toSeq
  }

  /** havoc all global assigments in given statement (incl. by called
    * procedures)
    */
  private def havocGlobals(stmt: Statement[Pre]): Seq[Statement[Post]] = {
    val allCalledBodies = gatherInvocations(stmt)
    val decls = allCalledBodies
      .flatMap(s => s.collect { case l: LocalDecl[Pre] => l.rewriteDefault() })
    val hav = havoc(allCalledBodies.flatMap(gatherGlobalAssigns))
    decls ++ hav
  }

  /** gather all expressions that this contract clause contains */
  private def gatherConditions(
      pred: AccountedPredicate[Pre]
  ): Seq[Expr[Pre]] = {
    pred match {
      case unit: UnitAccountedPredicate[Pre] => Seq(unit.pred)
      case split: SplitAccountedPredicate[Pre] =>
        gatherConditions(split.left) ++ gatherConditions(split.right)
    }
  }

  /** gather assignment expressions and their blames (for havoccing) */
  private def gatherAssigns(
      node: Statement[Pre]
  ): Seq[(Expr[Pre], Blame[AssignFailed])] = {
    node.collect {
      case a: Assign[Pre] => (a.target, a.blame)
      case p: PreAssignExpression[Pre] => (p.target, p.blame)
      case p: PostAssignExpression[Pre] => (p.target, p.blame)
    }
  }

  /** gather assignment expressions to global vars and their blames (for
    * havoccing)
    */
  private def gatherGlobalAssigns(
      node: Statement[Pre]
  ): Seq[(Expr[Pre], Blame[AssignFailed])] = {
    val as = gatherAssigns(node)
    as.filter { case (t, _) =>
      t match {
        case _: DerefHeapVariable[Pre] => true
        case _: DerefPointer[Pre] => true
        case _: AmbiguousSubscript[Pre] => true
        case _ => false
      }
    }
  }

  /** gather bodies of invoked procedures recursively (for havoccing all their
    * global assignments)
    * @param node:
    *   body of outer invoked procedure
    * @param alreadyGathered:
    *   Buffer of Procedures in call stack (to avoid recursion loop)
    */
  private def gatherInvocations(
      node: Statement[Pre],
      alreadyGathered: mutable.Buffer[Procedure[Pre]] = mutable.Buffer(),
  ): Seq[Statement[Pre]] = {
    val procList = node.collect {
      case i: InvokeProcedure[Pre] => i.ref.decl
      case i: ProcedureInvocation[Pre] => i.ref.decl
    }
    val res: mutable.Buffer[Statement[Pre]] = mutable.Buffer(node)
    for (p <- procList) {
      p.body match {
        case Some(b) =>
          if (!res.contains(b)) { res.appendAll(gatherInvocations(b)) }
        case None =>
      }
    }
    res.toSeq
  }

}
