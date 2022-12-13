import scala.meta._

object ColHelperUtil {
  def NonemptyMatch(context: String, expr: Term, cases: List[Case]): Term.Match = {
    if(cases.nonEmpty) Term.Match(expr, cases)
    else {
      fail("We tried to generate a match statement, but the list of cases is empty.", context=context)
    }
  }

  def fail(message: String, context: String = "", node: Option[scala.meta.Tree] = None): Nothing = {
    println("[error] [ColHelper] failed!")
    println("[error] [ColHelper] " + message)
    if(context.nonEmpty) {
      println(s"[error] [ColHelper] The context is: $context")
    }
    node match {
      case None =>
      case Some(node) => println(s"[error] [ColHelper] At ${node.pos.input.asInstanceOf[Input.VirtualFile].path}:${node.pos.startLine+1}:${node.pos.startColumn+1}")
    }
    ???
  }

  def substituteTypeName(name: String, replacement: Type.Name)(subject: Type): Type = {
    val recurse = substituteTypeName(name, replacement)(_)
    subject match {
      case Type.Apply(t, ts) => Type.Apply(recurse(t), ts.map(recurse))
      case Type.Name(value) => if(value == name) replacement else Type.Name(value)
      case Type.Tuple(ts) => Type.Tuple(ts.map(recurse))
      case _ => fail("I don't know how to recurse into this kind of type:", node = Some(subject))
    }
  }
}
