package vct.col.ast.statement.composite

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.print
import vct.col.print._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

object LoopImpl {
  case class InvalidLoopFormatForIterationContract(loop: Loop[_], message: String) extends UserError {
    override def code: String = "invalidIterationLoop"
    override def text: String =
      loop.o.messageInContext(s"This loop has an iteration contract, but $message.")
  }

  case class IterationContractData[G](v: Variable[G], lowerBound: Expr[G], upperBound: Expr[G])
}

trait LoopImpl[G] { this: Loop[G] =>
  import LoopImpl._

  def getVariableAndLowerBound(implicit o: Origin): Option[(Variable[G], Expr[G])] =
    init match {
      case Block(Seq(Assign(Local(Ref(v)), low, _))) =>
        Some((v, low))
      case _ => None
    }

  def getExclusiveUpperBound(v: Variable[G])(implicit o: Origin): Option[Expr[G]] =
    cond match {
      case Less(Local(Ref(`v`)), high) => Some(high)
      case LessEq(Local(Ref(`v`)), high) => Some(high + const(1))
      case Greater(high, Local(Ref(`v`))) => Some(high)
      case GreaterEq(high, Local(Ref(`v`))) => Some(high + const(1))
      case _ => None
    }

  def doesIncrement(v: Variable[G], update: Statement[G] = update): Boolean =
    update match {
      case Block(Seq(s)) => doesIncrement(v, s)
      case Assign(Local(Ref(`v`)), Plus(Local(Ref(`v`)), IntegerValue(ONE)), _) => true
      case Eval(PostAssignExpression(Local(Ref(`v`)), Plus(Local(Ref(`v`)), IntegerValue(ONE)), _)) => true
      case _ => false
    }

  def getIterationContractData(implicit o: Origin): Either[InvalidLoopFormatForIterationContract, IterationContractData[G]] = {
    val (v, low) = getVariableAndLowerBound.getOrElse(
      return Left(InvalidLoopFormatForIterationContract(this,
      "we could not derive the iteration variable or its lower bound from the initialization portion of the loop")))

    val high = getExclusiveUpperBound(v).getOrElse(
      return Left(InvalidLoopFormatForIterationContract(this,
      "we could not derive an upper bound for the iteration variable from the condition")))

    if(!doesIncrement(v))
      return Left(InvalidLoopFormatForIterationContract(this,
        "we could not ascertain that the iteration variable is incremented by one each iteration"))

    Right(IterationContractData(v, low, high))
  }

  def layoutSilver(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Group(Text("while") <+> "(" <> Doc.arg(cond) <> ")"),
      contract,
    )) <+> body.layoutAsBlock

  def layoutGeneric(implicit ctx: Ctx): Doc =
    if(init == Block[G](Nil) && update == Block[G](Nil)) layoutGenericWhile
    else layoutGenericFor

  def layoutGenericWhile(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Text("while") <+> "(" <> Doc.arg(cond) <> ")") <+> body.layoutAsBlock
    ))

  def layoutControlElement(e: Show)(implicit ctx: Ctx): Option[Show] = Some(e match {
    case Eval(e) => e.show
    case a: Assign[G] => a.layoutAsExpr
    case e: VeyMontAssignExpression[G] => return layoutControlElement(e.assign)
    case e: VeyMontCommExpression[G] => return layoutControlElement(e.assign)
    case LocalDecl(local) => local.show
    case JavaLocalDeclarationStatement(local) => local.show

    case _ => return None
  })

  def layoutControl(stat: Statement[G])(implicit ctx: Ctx): Doc =
    Group(Doc.args(stat.blockElementsForLayout.map(layoutControlElement(_).getOrElse(return stat.show))))

  def layoutGenericFor(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Text("for") <+> "(" <> Nest(NonWsLine <>
        (if(init == Block[G](Nil)) Text(";") else layoutControl(init) <> ";" <+/> print.Empty) <>
          cond <> ";" <>
          (if(update == Block[G](Nil)) print.Empty else print.Empty <+/> layoutControl(update))
      ) </> ")") <+> body.layoutAsBlock
    ))

  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => layoutSilver
    case _ => layoutGeneric
  }
}