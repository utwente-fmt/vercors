package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
  Block,
  ByReferenceClass,
  Choreography,
  Declaration,
  Endpoint,
  EndpointName,
  Expr,
  InstanceField,
  Local,
  Program,
  Variable,
}
import vct.col.origin.{Name, PanicBlame}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.SuccessionMap
import vct.col.util.AstBuildHelpers._

object EncodeChoreographyParameters extends RewriterBuilder {
  override def key: String = "encodeChoreographyParameters"
  override def desc: String =
    "Encode choreography parameters as fields on all endpoint types."
}

/** This is a rewrite that encodes choreography parameters as endpoint fields in
  * a separate class. It was abandoned because the connection between parameters
  * and the respective fields in each endpoint class is lost between this pass
  * and the generateImplementation pass. For that reason I decided to move this
  * concern into the GenerateImplementation pass, as all information necessary
  * is available there, at the cost of additional complexity in the rewrite.
  *
  * Can probably be deleted once the veymont artefact is in a finished state.
  */
case class EncodeChoreographyParameters[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  val currentSeqProg = ScopedStack[Choreography[Pre]]()

  var program: Program[Pre] = null
  lazy val choreographies = program.declarations.collect {
    case p: Choreography[Pre] => p
  }
  lazy val allEndpoints = choreographies.flatMap { _.endpoints }
  lazy val endpointOfClass: Map[ByReferenceClass[Pre], Endpoint[Pre]] =
    allEndpoints.map { endpoint =>
      (endpoint.cls.decl.asInstanceOf[ByReferenceClass[Pre]], endpoint)
    }.toMap
  lazy val choreographyOfEndpoint: Map[Endpoint[Pre], Choreography[Pre]] =
    choreographies.flatMap { chor => chor.endpoints.map { ep => (ep, chor) } }
      .toMap

  // For each endpoint and input variable, there is a unique instance field (on the class of the endpoint)
  val endpointParamFields =
    SuccessionMap[(Endpoint[Pre], Variable[Pre]), InstanceField[Post]]()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    program = p
    super.dispatch(p)
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case p: Choreography[Pre] =>
        implicit val o = p.o
        currentSeqProg.having(p) {
          allScopes.anySucceed(
            p,
            p.rewrite(preRun = {
              val paramAssigns = p.endpoints.flatMap { endpoint =>
                p.params.map { param =>
                  assignField[Post](
                    EndpointName[Post](succ(endpoint)),
                    endpointParamFields.ref((endpoint, param)),
                    Local(succ(param)),
                    blame = PanicBlame("Should be safe"),
                  )
                }
              }

              val preRun = p.preRun.map(s => Seq(dispatch(s))).getOrElse(Seq())
              Some(Block(preRun ++ paramAssigns))
            }),
          )
        }
      case cls: ByReferenceClass[Pre] if endpointOfClass.contains(cls) =>
        val endpoint = endpointOfClass(cls)
        val chor = choreographyOfEndpoint(endpoint)
        implicit val o = chor.o
        val additionFields = chor.params.map { param =>
          val f =
            new InstanceField(dispatch(param.t), Seq())(param.o.where(indirect =
              Name.names(
                chor.o.getPreferredNameOrElse(),
                Name("p"),
                param.o.getPreferredNameOrElse(),
              )
            ))
          endpointParamFields((endpoint, param)) = f
          f
        }
        allScopes.anySucceed(
          cls,
          cls.rewrite(decls =
            classDeclarations.dispatch(cls.decls) ++ additionFields
          ),
        )
      case _ => super.dispatch(decl)
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = ???
}
