import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import vct.antlr4.generated.{testLexer, testParser}

case class Asdf(x: String, y: Int)



object Main {
  val PROGRAM: String =
    "return;\n" +
      "/* doc */ return; \n" +
      "/*@ assert; */ return;\n" +
      "/*doc @ assert;*/ return;"

  def method(a: Asdf) = a match {
    case Asdf(z, 5) => println(z)
    case _ =>
  }

  def main(args: Array[String]) = {
    method(Asdf("try 5", 5))
    method(Asdf("try 4", 4))

    val lexer = new testLexer(CharStreams.fromString(PROGRAM))
    val parser = new testParser(new CommonTokenStream(lexer));
    val ctx = parser.start
    println(ctx.toStringTree(parser))
  }
}