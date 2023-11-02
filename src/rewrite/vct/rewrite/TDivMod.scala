package vct.rewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

case object TDivMod extends RewriterBuilder {
  override def key: String = "tdivmod"
  override def desc: String = "Encode truncated division and modulo using euclidean division and modulo."
}

// In most languages division and modulo are defined as `truncated`, whilst viper uses the `euclidean` definition
// So here we rewrite the truncated versions towards versions that use the euclidean definition
// See the paper Division and Modulus for Computer Scientists by DAAN LEIJEN for details
// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
case class TDivMod[Pre <: Generation]() extends Rewriter[Pre] {

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case mod: TMod[Pre] => tmod(mod)
    case mod: TDiv[Pre] => tdiv(mod)
    case other => rewriteDefault(other)
  }


  val tmodFunctions: mutable.Map[Unit, Function[Post]] = mutable.Map()
  val tdivFunctions: mutable.Map[Unit, Function[Post]] = mutable.Map()

  def tmod(mod: TMod[Pre]): Expr[Post] = {
    val tmod_func = tmodFunctions.getOrElseUpdate((), makeTModFunction())
    // TODO: How to get blame right? It is a DivByZero blame, but that is exactly why a function invocation might fail.
    FunctionInvocation[Post](tmod_func.ref, Seq(dispatch(mod.left), dispatch(mod.right)), Nil, Nil, Nil)(PanicBlame("TODO"))(mod.o)
  }

  def tdiv(div: TDiv[Pre]): Expr[Post] = {
    div.left.t match {
      case _: TFloat[Pre] => return FloorDiv[Post](dispatch(div.left), dispatch(div.right))(div.blame)(div.o)
      case _ =>
    }

    val tdiv_func = tdivFunctions.getOrElseUpdate((), makeTDivFunction())
    FunctionInvocation[Post](tdiv_func.ref, Seq(dispatch(div.left), dispatch(div.right)), Nil, Nil, Nil)(PanicBlame("TODO"))(div.o)
  }

  case class TFunctionOrigin(operator: String, preferredName: String) extends Origin {
    override def shortPosition: String = "generated"

    override def context: String = "[At node generated for `" + operator + "` operator]"

    override def inlineContext: String = "[At node generated for `" + operator + "` operator]"
  }

  /* Make a truncated modulo function.
     It should be equivalent to
      tmod(a,b) = let mod == (a % b) in (a >= 0 || mod == 0) ? mod : mod - abs(b)
     where / and % are euclidean division and modulo, which Viper uses as default.
    */
  def makeTModFunction(): Function[Post] = {
    implicit val o: Origin = TFunctionOrigin("%", "unknown")
    val new_t = TInt[Post]()
    val a_var = new Variable[Post](new_t)(TFunctionOrigin("%", "a"))
    val b_var = new Variable[Post](new_t)(TFunctionOrigin("%", "b"))

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
    )(TFunctionOrigin("%", "tmod")))
  }

  /* Make a truncated division function.
   It should be equivalent to
    tdiv(a,b) = let div == (a / b) in let mod == (a % b) in (a >= 0 || mod == 0) ? div : div + (b > 0 ? 1 : -1)
   where / and % are euclidean division and modulo, which Viper uses as default.
  */
  def makeTDivFunction(): Function[Post] = {
    implicit val o: Origin = TFunctionOrigin("/", "unknown")
    val new_t = TInt[Post]()
    val a_var = new Variable[Post](new_t)(TFunctionOrigin("/", "a"))
    val b_var = new Variable[Post](new_t)(TFunctionOrigin("/", "b"))

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
    )(TFunctionOrigin("/", "tdiv")))
  }

}