package vct.rewrite.veymont.verification

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{
  AllScopes,
  Block,
  CommTargetIndex,
  CommTargetRange,
  CtExpr,
  Deref,
  EndpointExpr,
  EndpointFamilyExpr,
  EndpointStatement,
  Eval,
  Expr,
  Inhale,
  IterVariable,
  Label,
  LabelDecl,
  Local,
  MethodInvocation,
  Old,
  ParBlock,
  ParBlockDecl,
  ParStatement,
  RangeBinder,
  SeqSubscript,
  Statement,
  TInt,
  ThisObject,
  Variable,
}
import vct.col.origin.PanicBlame
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

object EncodeParameterizedEndpointStatements extends RewriterBuilder {
  override def key: String = "encodeParameterizedEndpointStatements"
  override def desc: String =
    "Encodes parameterized endpoint statements as par blocks"
}

case class EncodeParameterizedEndpointStatements[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging() {

  case class ContractExtractor(
      thisObj: Expr[Post],
      oldLabel: LabelDecl[Post],
      args: Seq[(Variable[Pre], Expr[Post])],
  ) extends Rewriter[Pre] {
    override val allScopes: AllScopes[Pre, Post] =
      EncodeParameterizedEndpointStatements.this.allScopes

    val argMap: Map[Variable[Pre], Expr[Post]] = Map.from(args)

    override def dispatch(expr: Expr[Pre]): Expr[Post] =
      expr match {
        case ThisObject(_) => thisObj
        case old @ Old(_, None) => old.rewrite(at = Some(oldLabel.ref))
        case Local(Ref(v)) if argMap.contains(v) => argMap(v)
        case _ => expr.rewriteDefault()
      }
  }

  override def dispatch(stmt: Statement[Pre]): Statement[Post] =
    stmt match {
      case EndpointStatement(
            Some(CommTargetRange(Ref(f), RangeBinder(v, l, h))),
            Eval(
              MethodInvocation(
                SeqSubscript(EndpointFamilyExpr(Ref(otherF)), Local(Ref(i))),
                Ref(m),
                args,
                Seq(),
                Seq(),
                Seq(),
                Seq(),
              )
            ),
          ) =>
        assert(f == otherF)
        assert(v == i)
        implicit val o = stmt.o

        val label = new LabelDecl[Post]()(o.where(name = "pre"))
        val parV = new Variable[Post](TInt())(o.where(name = "i"))
        variables.succeedOnly(v, parV)
        val target = CommTargetIndex[Post](succ(f), Local[Post](parV.ref))
        val endpoint = CtExpr[Post](target)
        val extractor = ContractExtractor(
          endpoint,
          label,
          m.args.zip(args.map(dispatch)),
        )
        val par =
          ParBlock(
            new ParBlockDecl()(o.where(name = "s")),
            Seq(IterVariable(parV, dispatch(l), dispatch(h))),
            tt,
            EndpointExpr(
              target,
              unaccount(extractor.dispatch(m.contract.requires)),
            ),
            EndpointExpr(
              target,
              unaccount(extractor.dispatch(m.contract.ensures)),
            ),
            Inhale(ff), // Statement
          )(PanicBlame("Unexpected error from par block"))

        Label(label, ParStatement(par))
      case EndpointStatement(Some(CommTargetRange(_, _)), Eval(_)) => ???

      case _ => stmt.rewriteDefault()
    }
}
