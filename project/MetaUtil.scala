import scala.meta._

object MetaUtil {
  def NonemptyMatch(context: String, expr: Term, cases: List[Case]): Term.Match = {
    if(cases.nonEmpty) Term.Match(expr, cases)
    else {
      println("ColHelpers has failed!")
      println("We tried to generate a match statement, but the list of cases is empty.")
      println(s"The context is: $context")
      ???
    }
  }

}
