import scala.meta._

object MetaUtil {
  def NonemptyMatch(context: String, expr: Term, cases: List[Case]): Term.Match = {
    if(cases.nonEmpty) Term.Match(expr, cases)
    else {
      fail("We tried to generate a match statement, but the list of cases is empty.", context=context)
    }
  }

  def fail(message: String, context: String = "", node: Option[scala.meta.Tree] = None): Nothing = {
    println("ColHelpers has failed!")
    println(message)
    if(context.nonEmpty) {
      println(s"The context is: $context")
    }
    node match {
      case None =>
      case Some(node) => println(s"At ${node.pos.input.asInstanceOf[Input.VirtualFile].path}:${node.pos.startLine+1}:${node.pos.startColumn+1}")
    }
    ???
  }
}
