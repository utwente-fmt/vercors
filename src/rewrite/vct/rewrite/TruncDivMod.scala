package vct.col.rewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

case object TruncDivMod extends RewriterBuilder {
  override def key: String = "truncDivmod"
  override def desc: String = "Encode truncated division and modulo using euclidean division and modulo."
}

// In most languages division and modulo are defined as `truncated`, whilst viper uses the `euclidean` definition
// So here we rewrite the truncated versions towards versions that use the euclidean definition
// See the paper Division and Modulus for Computer Scientists by DAAN LEIJEN for details
// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
case class TruncDivMod[Pre <: Generation]() extends Rewriter[Pre] {

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case mod: TruncMod[Pre] => truncMod(mod)
    case mod: TruncDiv[Pre] => truncDiv(mod)
    case other => rewriteDefault(other)
  }


  val truncModFunctions: mutable.Map[Unit, Function[Post]] = mutable.Map()
  val truncDivFunctions: mutable.Map[Unit, Function[Post]] = mutable.Map()

  def truncMod(mod: TruncMod[Pre]): Expr[Post] = {
    val truncModFunc = truncModFunctions.getOrElseUpdate((), makeTruncModFunction())
    // TODO: How to get blame right? It is a DivByZero blame, but that is exactly why a function invocation might fail.
    FunctionInvocation[Post](truncModFunc.ref, Seq(dispatch(mod.left), dispatch(mod.right)), Nil, Nil, Nil)(PanicBlame("TODO"))(mod.o)
  }

  def truncDiv(div: TruncDiv[Pre]): Expr[Post] = {
    div.t match {
      case _: TFloat[Pre] => return FloatDiv[Post](dispatch(div.left), dispatch(div.right))(div.blame)(div.o)
      case _ =>
    }

    val truncDiv_func = truncDivFunctions.getOrElseUpdate((), makeTruncDivFunction())
    FunctionInvocation[Post](truncDiv_func.ref, Seq(dispatch(div.left), dispatch(div.right)), Nil, Nil, Nil)(PanicBlame("TODO"))(div.o)
  }

  def truncFunctionOrigin(operator: String) = Origin(Seq(LabelContext("generated at `\" + operator + \"` operator\"")))

  /* Make a truncated modulo function.
     It should be equivalent to
      truncMod(a,b) = let mod == (a % b) in (a >= 0 || mod == 0) ? mod : mod - abs(b)
     where / and % are euclidean division and modulo, which Viper uses as default.
    */
  def makeTruncModFunction(): Function[Post] = {
    implicit val o: Origin = truncFunctionOrigin("%")
    val new_t = TInt[Post]()
    val a_var = new Variable[Post](new_t)(truncFunctionOrigin("%").where(name="a"))
    val b_var = new Variable[Post](new_t)(truncFunctionOrigin("%").where(name="b"))

    val a = Local[Post](a_var.ref)
    val b = Local[Post](b_var.ref)
    val absb = Select(b > const(0), b, UMinus(b))

    globalDeclarations.declare(function[Post](
      blame = AbstractApplicable,
      contractBlame = PanicBlame("TODO: Integer division should not have zero second parameter"),
      returnType = new_t,
      args = Seq(a_var, b_var),
      requires = UnitAccountedPredicate(b !== const(0)),
      body = Some(let(new_t, a % b, (mod: Local[Post]) => Select(a >= const(0) || mod === const(0), mod, mod - absb)))
    )(truncFunctionOrigin("%").where(name="truncMod")))
  }

  /* Make a truncated division function.
   It should be equivalent to
    truncDiv(a,b) = let div == (a / b) in let mod == (a % b) in (a >= 0 || mod == 0) ? div : div + (b > 0 ? 1 : -1)
   where / and % are euclidean division and modulo, which Viper uses as default.
  */
  def makeTruncDivFunction(): Function[Post] = {
    implicit val o: Origin = truncFunctionOrigin("/")
    val new_t = TInt[Post]()
    val a_var = new Variable[Post](new_t)(truncFunctionOrigin("/").where(name="a"))
    val b_var = new Variable[Post](new_t)(truncFunctionOrigin("/").where(name="b"))

    val a = Local[Post](a_var.ref)
    val b = Local[Post](b_var.ref)
    val one = Select(b > const(0), const(1), const(-1))

    globalDeclarations.declare(function[Post](
      blame = AbstractApplicable,
      contractBlame = PanicBlame("TODO"),
      returnType = new_t,
      args = Seq(a_var, b_var),
      requires = UnitAccountedPredicate(b !== const(0)),
      body = Some(let(new_t, a / b, (div: Local[Post]) =>
        let(new_t, a % b, (mod: Local[Post]) =>
          Select(a >= const(0) || mod === const(0), div, div + one))))
    )(truncFunctionOrigin("/").where(name="truncDiv")))
  }

}