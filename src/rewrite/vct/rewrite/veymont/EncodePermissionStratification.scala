package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{
  Generation,
  Rewriter,
  RewriterBuilder,
  RewriterBuilderArg,
}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}
import EncodeChoreography.{
  AssertFailedToParticipantsNotDistinct,
  AssignFailedToSeqAssignFailure,
}
import vct.col.origin.{
  Blame,
  ChorRunPreconditionFailed,
  ExhaleFailed,
  FoldFailed,
  InsufficientPermission,
  InvocationFailure,
  Name,
  Origin,
  PanicBlame,
  PreconditionFailed,
  UnfoldFailed,
  VerificationFailure,
}
import vct.col.ref.Ref
import vct.rewrite.veymont
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.EncodePermissionStratification.{
  ForwardExhaleFailedToChorRun,
  ForwardInvocationFailureToDeref,
  ForwardUnfoldFailedToDeref,
  NoEndpointContext,
}

import scala.collection.immutable.HashSet
import scala.collection.{mutable => mut}

object EncodePermissionStratification extends RewriterBuilderArg[Boolean] {
  override def key: String = "encodePermissionStratification"
  override def desc: String =
    "Encodes stratification of permissions by wrapping each permission in an opaque predicate, guarding the permission using an endpoint reference."

  case class ForwardExhaleFailedToChorRun(run: ChorRun[_])
      extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      run.blame.blame(ChorRunPreconditionFailed(None, error.failure, run))
  }

  case class ForwardInvocationFailureToDeref(deref: Deref[_])
      extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit =
      deref.blame.blame(InsufficientPermission(deref))
  }

  case class ForwardUnfoldFailedToDeref(deref: Deref[_])
      extends Blame[UnfoldFailed] {
    override def blame(error: UnfoldFailed): Unit =
      deref.blame.blame(InsufficientPermission(deref))
  }

  case class NoEndpointContext(node: Node[_]) extends UserError {
    override def code = "noEndpointContext"
    override def text =
      node.o.messageInContext("There is no endpoint context inferrable here")
  }
}

// TODO (RR): Document here the hack to make \chor work
case class EncodePermissionStratification[Pre <: Generation](
    veymontGeneratePermissions: Boolean
) extends Rewriter[Pre] with VeymontContext[Pre] with LazyLogging {

  val inChor = ScopedStack[Boolean]()

  lazy val specializedApplicables
      : mut.LinkedHashMap[ContractApplicable[Pre], Seq[Endpoint[Pre]]] =
    findInContext {
      case inv: AnyFunctionInvocation[Pre] => inv.ref.decl
      case inv: MethodInvocation[Pre] => inv.ref.decl
    }

  // Given a function f that finds Nodes inside nodes, findInContext keeps applying f
  // to nodes that f itself finds, until no more new nodes are found. The search is started
  // by first applying f to all endpointexprs and endpoint statements. In addition, the
  // endpoint in the endpoint expr that is at the start of the search, is kept, and attached to each
  // node found later.
  def findInContext[T <: Node[Pre]](
      f: PartialFunction[Node[Pre], T]
  ): mut.LinkedHashMap[T, Seq[Endpoint[Pre]]] = {
    val specializations: mut.LinkedHashSet[(Endpoint[Pre], T)] = mut
      .LinkedHashSet.from(
        mappings
          // For each endpoint expr
          .program.collect {
            // Get all T's from endpoint contexts
            case expr: EndpointExpr[Pre] =>
              expr.collect(f).map { t => (expr.endpoint.decl, t) }
            case stmt: EndpointStatement[Pre] =>
              stmt.collect(f).map { t => (stmt.endpoint.get.decl, t) }
          }.flatten
      )

    // Do a transitive closure given the selector function f
    var changes = true
    while (changes) {
      changes = false
      val oldSize = specializations.size
      specializations.flatMap { case (endpoint, t) =>
        t.collect(f).map { newT => (endpoint, newT) }
      }.foreach(specializations.add)
      changes = oldSize != specializations.size
    }

    val map = mut.LinkedHashMap[T, Seq[Endpoint[Pre]]]()
    specializations.foreach { case (endpoint, t) =>
      map.updateWith(t) {
        case None => Some(Seq(endpoint))
        case Some(endpoints) => Some(endpoint +: endpoints)
      }
    }
    map
  }

  val specializedApplicableSucc =
    SuccessionMap[(Endpoint[Pre], ContractApplicable[Pre]), ContractApplicable[
      Post
    ]]()

  // Keeps track of the current anchoring/endpoint context identity expression for the current endpoint context.
  // E.g. within a choreograph, it is EndpointName(succ(endpoint)), within a specialized function it is a local
  // to the endpoint context argument.
  val specializing = ScopedStack[Expr[Post]]()

  type WrapperPredicateKey = (TClass[Pre], Type[Pre], InstanceField[Pre])
  val wrapperPredicates = mut
    .LinkedHashMap[WrapperPredicateKey, Predicate[Post]]()

  def wrapperPredicate(
      endpoint: Endpoint[Pre],
      objT: Type[Pre],
      field: InstanceField[Pre],
  )(implicit o: Origin): Ref[Post, Predicate[Post]] = {
    val k = (endpoint.t, objT, field)
    wrapperPredicates.getOrElseUpdate(
      k, {
        logger.debug(s"Declaring wrapper predicate for $k")
        val endpointArg =
          new Variable(dispatch(endpoint.t))(o.where(name = "endpoint"))
        val objectArg = new Variable(dispatch(objT))(o.where(name = "obj"))
        val body = Perm[Post](
          FieldLocation(objectArg.get, succ(field)),
          WritePerm(),
        )
        new Predicate(Seq(endpointArg, objectArg), Some(body))(
          o.where(indirect =
            Name.names(Name("wrap"), field.o.getPreferredNameOrElse())
          )
        ).declare()
      },
    ).ref
  }

  val readFunctions = mut.LinkedHashMap[WrapperPredicateKey, Function[Post]]()
  def readFunction(
      endpoint: Endpoint[Pre],
      obj: Expr[Pre],
      field: InstanceField[Pre],
  )(implicit o: Origin): Ref[Post, Function[Post]] = {
    val k = (endpoint.t, obj.t, field)
    val pred = wrapperPredicate(endpoint, obj.t, field)
    readFunctions.getOrElseUpdate(
      k, {
        logger.debug(s"Declaring read function for $k")
        val endpointArg =
          new Variable(dispatch(endpoint.t))(o.where(name = "endpoint"))
        val objArg = new Variable(dispatch(obj.t))(o.where(name = "obj"))
        function(
          requires =
            Value(PredicateLocation(pred, Seq(endpointArg.get, objArg.get)))
              .accounted,
          args = Seq(endpointArg, objArg),
          returnType = dispatch(field.t),
          body = Some(
            Unfolding(
              Value(PredicateLocation(pred, Seq(endpointArg.get, objArg.get))),
              Deref[Post](objArg.get, succ(field))(PanicBlame(
                "Permission is guaranteed by the predicate"
              )),
            )(PanicBlame("Predicate is guaranteed to be in the precondition"))
          ),
          blame = PanicBlame("Contract is guaranteed to hold"),
          contractBlame = PanicBlame("Contract is guaranteed to be satisfiable"),
        )(o.where(indirect =
          Name.names(Name("read"), field.o.getPreferredNameOrElse())
        )).declare()
      },
    ).ref
  }

  case class StripPermissionStratification() extends Rewriter[Pre] {
    override val allScopes: AllScopes[Pre, Post] =
      EncodePermissionStratification.this.allScopes

    override def dispatch(expr: Expr[Pre]): Expr[Post] =
      expr match {
        case ChorPerm(_, loc, perm) =>
          Perm(dispatch(loc), dispatch(perm))(expr.o)
        case ChorExpr(inner) => dispatch(inner)
        case EndpointExpr(_, inner) => dispatch(inner)
        case _ => expr.rewriteDefault()
      }
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    mappings.program = program
    super.dispatch(program)
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        implicit val o = chor.o
        val pre = foldStar(
          chor.run.contract.contextEverywhere +:
            unfoldPredicate(chor.run.contract.requires)
        )
        currentChoreography.having(chor) {
          chor.rewrite(preRun =
            Some(Block(
              chor.preRun.map(dispatch).toSeq ++
                Seq(Exhale[Post](StripPermissionStratification().dispatch(pre))(
                  ForwardExhaleFailedToChorRun(chor.run)
                )) :+ Inhale[Post](dispatch(pre))
            ))
          ).succeed(chor)
        }
      case f: Function[Pre] if specializedApplicables.contains(f) =>
        f.rewriteDefault().succeed(f)
        specializeApplicable(f)
      case f: InstanceFunction[Pre] if specializedApplicables.contains(f) =>
        f.rewriteDefault().succeed(f)
        specializeApplicable(f)
      case m: InstanceMethod[Pre] if specializedApplicables.contains(m) =>
        m.rewriteDefault().succeed(m)
        specializeApplicable(m)
      case _ => super.dispatch(decl)
    }

  def specializeApplicable(app: ContractApplicable[Pre]): Unit = {
    assert(app match {
      case _: InstanceMethod[Pre] |
          _: InstanceFunction[Pre] | _: Function[Pre] =>
        true
      case _ => false
    })

    def nameOrigin(
        endpoint: Endpoint[Pre],
        f: ContractApplicable[Pre],
    ): Origin = {
      f.o.where(indirect =
        Name.names(
          f.o.getPreferredNameOrElse(),
          endpoint.o.getPreferredNameOrElse(),
        )
      )
    }

    specializedApplicables(app).foreach { endpoint =>
      // Make sure the unnapply methods InChor/InEndpoint pick this rewrite up
      currentChoreography.having(choreographyOf(endpoint)) {
        currentEndpoint.having(endpoint) {
          variables.scope {
            val endpointCtxVar =
              new Variable(dispatch(endpoint.t))(
                currentChoreography.top.o
                  .where(indirect = Name.strings("endpoint", "ctx"))
              )

            // Make sure plain perms are rewritten into wrapped perms
            specializing.having(endpointCtxVar.get(app.o)) {
              val newF: ContractApplicable[Post] =
                app match {
                  case f: InstanceFunction[Pre] =>
                    val newF = f.rewrite(
                      args = endpointCtxVar +: variables.dispatch(f.args),
                      o = nameOrigin(endpoint, f),
                    )
                    newF.declare()
                  case f: Function[Pre] =>
                    val newF = f.rewrite(
                      args = endpointCtxVar +: variables.dispatch(f.args),
                      o = nameOrigin(endpoint, f),
                    )
                    newF.declare()
                  case m: InstanceMethod[Pre] =>
                    val newM = m.rewrite(
                      args = endpointCtxVar +: variables.dispatch(m.args),
                      body = None,
                      o = nameOrigin(endpoint, m),
                    )
                    newM.declare()
                }
              specializedApplicableSucc((endpoint, app)) = newF
            }
          }
        }
      }
    }
  }

  def makeWrappedPerm(
      endpoint: Endpoint[Pre],
      loc: FieldLocation[Pre],
      perm: Expr[Pre],
      endpointExpr: Expr[Post] = null,
  )(implicit o: Origin): Expr[Post] = {
    // TODO: This branch + parameter use is ugly and unclear
    val expr =
      if (endpointExpr == null)
        EndpointName[Post](succ(endpoint))
      else
        endpointExpr

    if (perm == ReadPerm[Pre]()) {
      Value(PredicateLocation(
        wrapperPredicate(endpoint, loc.obj.t, loc.field.decl),
        Seq(expr, dispatch(loc.obj)),
      ))
    } else {
      PredicateApply(
        wrapperPredicate(endpoint, loc.obj.t, loc.field.decl),
        Seq(expr, dispatch(loc.obj)),
        dispatch(perm),
      )
    }
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case InChor(_, cp: ChorPerm[Pre]) =>
        assert(currentEndpoint.isEmpty)
        currentEndpoint.having(cp.endpoint.decl) {
          specializing
            .having(EndpointName[Post](succ(cp.endpoint.decl))(cp.o)) {
              dispatch(cp)
            }
        }

      case InEndpoint(
            _,
            _,
            ChorPerm(Ref(endpoint), loc: FieldLocation[Pre], perm),
          ) =>
        makeWrappedPerm(endpoint, loc, perm)(expr.o)

      case InEndpoint(_, endpoint, Perm(loc: FieldLocation[Pre], perm)) =>
        makeWrappedPerm(endpoint, loc, perm, specializing.top)(expr.o)

      case InEndpoint(_, endpoint, Value(loc: FieldLocation[Pre])) =>
        makeWrappedPerm(endpoint, loc, ReadPerm()(expr.o), specializing.top)(
          expr.o
        )

      case EndpointExpr(Ref(endpoint), inner) =>
        assert(currentEndpoint.isEmpty)
        currentEndpoint.having(endpoint) {
          specializing.having(EndpointName[Post](succ(endpoint))(expr.o)) {
            dispatch(inner)
          }
        }

      case InEndpoint(_, endpoint, deref @ Deref(obj, Ref(field))) =>
        implicit val o = expr.o
        functionInvocation(
          ref = readFunction(endpoint, obj, field)(expr.o),
          args = Seq(specializing.top, dispatch(obj)),
          blame = ForwardInvocationFailureToDeref(deref),
        )

      case ChorExpr(inner) if veymontGeneratePermissions =>
        implicit val o = expr.o

        def predicates(
            seenClasses: Set[TClass[Pre]],
            endpoint: Endpoint[Pre],
            baseT: TClass[Pre],
            base: Expr[Post],
        ): Seq[PredicateApply[Post]] = {
          val cls = baseT.cls.decl
          // Permission generation makes sure no cycles exist at this point by crashing, but lets make sure anyway
          assert(!seenClasses.contains(baseT))
          val newSeenClasses = seenClasses.incl(baseT)
          val predicatesForClass = cls.fields.map { field =>
            PredicateApply[Post](
              wrapperPredicate(endpoint, baseT, field),
              Seq(EndpointName(succ(endpoint)), base),
              WritePerm(),
            )
          }
          predicatesForClass ++
            (cls.fields.filter { _.t.asClass.nonEmpty }.flatMap { field =>
              val newBase =
                Deref[Post](base, succ(field))(PanicBlame(
                  "Permissions were unfolded earlier"
                ))
              predicates(
                newSeenClasses,
                endpoint,
                baseT.instantiate(field.t).asClass.get,
                newBase,
              )
            })
        }

        val newInner = inChor.having(true) { dispatch(inner) }

        InferEndpointContexts.getEndpoints(inner).flatMap { endpoint =>
          predicates(
            HashSet(),
            endpoint,
            endpoint.t,
            EndpointName[Post](succ(endpoint)),
          )
        }.foldRight[Expr[Post]](newInner) { case (app, inner) =>
          Unfolding[Post](app, inner)(PanicBlame(
            "Generating permissions guarantee permissions are in scope"
          ))
        }

      case ChorExpr(inner) if !veymontGeneratePermissions =>
        // If not generating permissions, we rely on endpoint expressions to indicate the owner
        // of relevant permissions
        inChor.having(true) { dispatch(inner) }

      // Generate an invocation to the unspecialized function version if we're inside a \chor
      // This is safe because \chor unfold all predicates of all endpoints that occur within the expression...
      // ... in the case of permission generation. Otherwise it just does nothing...?
      // The natural successor of the function will be the unspecialized one
      case inv: FunctionInvocation[Pre]
          if inChor.topOption.contains(true) && veymontGeneratePermissions =>
        inv.rewriteDefault()
      case inv: InstanceFunctionInvocation[Pre]
          if inChor.topOption.contains(true) && veymontGeneratePermissions =>
        inv.rewriteDefault()

      case InEndpoint(_, endpoint, inv: FunctionInvocation[Pre]) =>
        val k = (endpoint, inv.ref.decl)
        inv.rewrite(
          ref = specializedApplicableSucc.ref(k),
          args = specializing.top +: inv.args.map(dispatch),
        )
      case InEndpoint(_, endpoint, inv: InstanceFunctionInvocation[Pre]) =>
        val k = (endpoint, inv.ref.decl)
        inv.rewrite(
          ref = specializedApplicableSucc.ref(k),
          args = specializing.top +: inv.args.map(dispatch),
        )
      case InEndpoint(_, endpoint, inv: MethodInvocation[Pre]) =>
        val k = (endpoint, inv.ref.decl)
        inv.rewrite(
          ref = specializedApplicableSucc.ref(k),
          args = specializing.top +: inv.args.map(dispatch),
        )

      case _ => expr.rewriteDefault()
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case EndpointStatement(None, Assign(_, _)) =>
        throw NoEndpointContext(statement)
      case EndpointStatement(
            Some(Ref(endpoint)),
            assign @ Assign(deref @ Deref(obj, Ref(field)), _),
          ) =>
        implicit val o = statement.o
        val apply = {
          val newEndpoint: Ref[Post, Endpoint[Post]] = succ(endpoint)
          val ref = wrapperPredicate(endpoint, obj.t, field)
          PredicateApply[Post](
            ref,
            Seq(
              EndpointName(newEndpoint),
              currentEndpoint.having(endpoint) {
                specializing.having(EndpointName[Post](succ(endpoint))) {
                  dispatch(obj)
                }
              },
            ),
            WritePerm(),
          )
        }
        val intermediate =
          new Variable(dispatch(assign.value.t))(
            assign.o.where(name = "intermediate")
          )
        currentEndpoint.having(endpoint) {
          specializing.having(EndpointName[Post](succ(endpoint))) {
            Scope(
              Seq(intermediate),
              Block(Seq(
                assignLocal(intermediate.get, dispatch(assign.value)),
                Unfold(apply)(ForwardUnfoldFailedToDeref(deref)),
                assign.rewrite(
                  target =
                    Deref[Post](dispatch(obj), succ(field))(PanicBlame(
                      "Unfold succeeded, so assignment is safe"
                    )),
                  value = intermediate.get,
                ),
                Fold(apply)(PanicBlame("Unfold succeeded, so fold is safe")),
              )),
            )
          }
        }
      case EndpointStatement(Some(Ref(endpoint)), assert: Assert[Pre]) =>
        currentEndpoint.having(endpoint) {
          specializing.having(EndpointName[Post](succ(endpoint))(statement.o)) {
            assert.rewriteDefault()
          }
        }
      case EndpointStatement(Some(Ref(endpoint)), eval: Eval[Pre]) =>
        currentEndpoint.having(endpoint) {
          specializing.having(EndpointName[Post](succ(endpoint))(statement.o)) {
            eval.rewriteDefault()
          }
        }
      case _ => statement.rewriteDefault()
    }
}
