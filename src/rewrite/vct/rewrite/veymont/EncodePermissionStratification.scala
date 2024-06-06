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
  CallableFailureToSeqCallableFailure,
}
import vct.col.origin.{Name, Origin, PanicBlame}
import vct.col.ref.Ref
import vct.rewrite.veymont

import scala.collection.immutable.HashSet
import scala.collection.{mutable => mut}

object EncodePermissionStratification extends RewriterBuilderArg[Boolean] {
  override def key: String = "encodePermissionStratification"
  override def desc: String =
    "Encodes stratification of permissions by wrapping each permission in an opaque predicate, guarding the permission using an endpoint reference."
}

// TODO (RR): Document here the hack to make \chor work
case class EncodePermissionStratification[Pre <: Generation](
    veymontGeneratePermissions: Boolean
) extends Rewriter[Pre] with VeymontContext[Pre] with LazyLogging {

  val inChor = ScopedStack[Boolean]()

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
              Deref[Post](objArg.get, succ(field))(PanicBlame("???")),
            )(PanicBlame("???"))
          ),
          blame = PanicBlame("???"),
          contractBlame = PanicBlame("???"),
        )(o.where(indirect =
          Name.names(Name("read"), field.o.getPreferredNameOrElse())
        )).declare()
      },
    ).ref
  }

  case class StripChorPerm() extends Rewriter[Pre] {
    override val allScopes: AllScopes[Pre, Post] =
      EncodePermissionStratification.this.allScopes

    override def dispatch(expr: Expr[Pre]): Expr[Post] =
      expr match {
        case ChorPerm(_, loc, perm) =>
          Perm(dispatch(loc), dispatch(perm))(expr.o)
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
        currentChoreography.having(chor) {
          chor.rewrite(preRun =
            Some(Block(
              chor.preRun.map(dispatch).toSeq ++
                (Seq(
                  Exhale[Post](StripChorPerm().dispatch(foldStar(
                    unfoldPredicate(chor.run.contract.requires)
                  )))(PanicBlame(
                    "Exhaling non-stratified part of precondition failed"
                  ))
                )) ++
                (unfoldPredicate(chor.run.contract.requires)
                  .map(e => Inhale[Post](dispatch(e))))
            ))
          ).succeed(chor)
        }
      case _ => super.dispatch(decl)
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case InChor(_, cp: ChorPerm[Pre]) =>
        assert(currentEndpoint.isEmpty)
        currentEndpoint.having(cp.endpoint.decl) { dispatch(cp) }

      case InEndpoint(
            _,
            _,
            ChorPerm(Ref(endpoint), loc: FieldLocation[Pre], perm),
          ) =>
        implicit val o = expr.o
        PredicateApply(
          wrapperPredicate(endpoint, loc.obj.t, loc.field.decl)(expr.o),
          Seq(EndpointName(succ(endpoint)), dispatch(loc.obj)),
          dispatch(perm),
        )

      case EndpointExpr(Ref(endpoint), inner) =>
        assert(currentEndpoint.isEmpty)
        currentEndpoint.having(endpoint) { dispatch(inner) }

      case InEndpoint(_, endpoint, Deref(obj, Ref(field))) =>
        implicit val o = expr.o
        functionInvocation(
          ref = readFunction(endpoint, obj, field)(expr.o),
          args = Seq(EndpointName(succ(endpoint)), dispatch(obj)),
          blame = PanicBlame("???"),
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
            "Generating permissions should be good"
          ))
        }

      case _ => expr.rewriteDefault()
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case EndpointStatement(
            Some(Ref(endpoint)),
            assign @ Assign(target @ Deref(obj, Ref(field)), _),
          ) =>
        implicit val o = statement.o
        val apply = {
          val newEndpoint: Ref[Post, Endpoint[Post]] = succ(endpoint)
          val ref = wrapperPredicate(endpoint, obj.t, field)
          PredicateApply[Post](
            ref,
            Seq(EndpointName(newEndpoint), dispatch(obj)),
            WritePerm(),
          )
        }
        val intermediate =
          new Variable(dispatch(assign.value.t))(
            assign.o.where(name = "intermediate")
          )
        Scope(
          Seq(intermediate),
          Block(Seq(
            currentEndpoint.having(endpoint) {
              assignLocal(intermediate.get, dispatch(assign.value))
            },
            Unfold(apply)(PanicBlame("TODO: Use blame on endpoint")),
            assign.rewrite(value = intermediate.get),
            Fold(apply)(PanicBlame("TODO: Use blame on endpointstatement")),
          )),
        )
      case EndpointStatement(Some(Ref(endpoint)), assert: Assert[Pre]) =>
        currentEndpoint.having(endpoint) { assert.rewriteDefault() }
      case EndpointStatement(_, eval: Eval[Pre]) => Block(Seq())(statement.o)
      // TODO (RR): Implement this
//        throw new Exception(statement.o.messageInContext(
//          "Eval with permission stratification not yet supported"
//        ))
      case _ => statement.rewriteDefault()
    }
}
