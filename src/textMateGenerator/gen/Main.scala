package gen

import hre.io.{RWFile, Readable}
import upickle.default.{macroRW, ReadWriter => RW}
import upickle._
import java.io.File
import org.antlr.v4.runtime
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import vct.antlr4.generated.ANTLRv4Parser.Rules0Context
import vct.antlr4.generated.ANTLRv4ParserPatterns.{GrammarSpec0, RuleSpec0, Rules0}
import vct.antlr4.generated.{ANTLRv4Lexer, ANTLRv4Parser}

import java.io.FileNotFoundException

case object Main {

  def parse() = {
    val readable: Readable = RWFile(new File("C:\\Development\\Vercors-Dev\\vercors\\src\\parsers\\antlr4\\LangPVLLexer.g4"))
    try {
      readable.read { reader =>
        val stream: runtime.CharStream = CharStreams.fromReader(reader, readable.fileName)
        val lexer = new ANTLRv4Lexer(stream)
        val tokens = new CommonTokenStream(lexer)
        val parser = new ANTLRv4Parser(tokens)
        parser.grammarSpec()
      }
    } catch {
      case f: FileNotFoundException => throw f
    }
  }

  def main(args: Array[String]): Unit = {
    var textMateGrammar = CGL(args(0), args(1), Nil)
    val tree = parse()
    tree match {
      case GrammarSpec0(grammarDecl, prequelConstructs, rules, modeSpec, _) =>
        rules match {
          case Rules0(specRules) =>
            for (ruleText <- specRules.map(rule => rule.getText))
              if (ruleText.matches(".*\\{.*\\}.*"))
              textMateGrammar = textMateGrammar.addPattern(
                MatchPattern(ruleText.substring(ruleText.indexOf('{') + 2, ruleText.indexOf("}") - 2),
                "\\b(?:" + ruleText.split('\'')(1) + ")\\b")
              )
        }
    }
    print(upickle.default.write(textMateGrammar, indent = 2))
  }

}

