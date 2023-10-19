package vct.col.rewrite
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{Context, InlineContext, Origin, PanicBlame, PreferredName, ShortPosition}
import vct.col.rewrite.EncodeForPermWithValue.ForPermWithValueVar
import vct.col.util.AstBuildHelpers._
import vct.col.util.Substitute

case object EncodeForPermWithValue extends RewriterBuilder {
  override def key: String = "encodeForPermWithValue"
  override def desc: String = "Expand \\forperm_with_value over all declared fields"

  private def ForPermWithValueVar(preferredName: String): Origin = Origin(
    Seq(
      PreferredName(preferredName),
      Context("At: [Node for ForPermWithValue]"),
      InlineContext("[Node for ForPermWithValue]"),
      ShortPosition("generated"),
    )
  )
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
