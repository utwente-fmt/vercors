package vct.col.rewrite
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{Origin, PanicBlame}
import vct.col.rewrite.EncodeForPermWithValue.ForPermWithValueVar
import vct.col.util.AstBuildHelpers._
import vct.col.util.Substitute

case object EncodeForPermWithValue extends RewriterBuilder {
  override def key: String = "encodeForPermWithValue"
  override def desc: String = "Expand \\forperm_with_value over all declared fields"

  case class ForPermWithValueVar(preferredName: String) extends Origin {
    override def context: String = "At: [Node for ForPermWithValue]"
    override def inlineContext: String = "[Node for ForPermWithValue]"
    override def shortPosition: String = "generated"
  }
}

case class EncodeForPermWithValue[Pre <: Generation]() extends Rewriter[Pre] {
  val currentFields: ScopedStack[Seq[SilverField[Pre]]] = ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Post] =
    currentFields.having(program.declarations.collect { case field: SilverField[Pre] => field }) {
      rewriteDefault(program)
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case ForPermWithValue(binding, body) =>
      foldAnd(currentFields.top.map { field =>
        implicit val o: Origin = e.o
        val obj = new Variable[Pre](TRef())(ForPermWithValueVar("obj"))
        val newBody = Substitute[Pre](Map(binding.get -> SilverDeref[Pre](obj.get, field.ref)(PanicBlame("schematic")))).dispatch(body)
        dispatch(ForPerm(Seq(obj), SilverFieldLocation[Pre](obj.get, field.ref), newBody))
      })(e.o)

    case other => rewriteDefault(other)
  }
}
