package gen

import hre.io.{RWFile, Readable}
import upickle.default.{macroRW, ReadWriter => RW}
import upickle._

import java.nio.file.Paths
import org.antlr.v4.runtime
import org.antlr.v4.runtime.misc.IntervalSet
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import vct.antlr4.generated.ANTLRv4Parser.{LexerAltContext, LexerRuleSpecContext, Rules0Context}
import vct.antlr4.generated.ANTLRv4ParserPatterns._
import vct.antlr4.generated.{ANTLRv4Lexer, ANTLRv4Parser}

import java.io.FileNotFoundException
import scala.jdk.CollectionConverters.CollectionHasAsScala

case object Main {

  def parse(path: String) = {
    val readable: Readable = RWFile(Paths.get(path))
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
    val tree = parse(args(2))
    val GrammarSpec0(
      GrammarDecl0(_, grammarName, _),
      _,
      Rules0(modelessRules),
      modedRules,
      _
    ) = tree

    val rules: Seq[LexerRuleSpecContext] =
      modelessRules.map { case RuleSpec0(rule) => rule } ++
        modedRules.flatMap { case ModeSpec0(_, _, _, rules) => rules }

    rules.foreach(processRule)

    // print(upickle.default.write(textMateGrammar, indent = 2))
  }

  def processRule(rule: LexerRuleSpecContext): Unit = {
    val LexerRuleSpec0(_, name, _, _, LexerRuleBlock0(alts), _) = rule
    val alts = altsRule.children.asScala.collect { case alt: LexerAltContext => alt }
    val set = new IntervalSet()
    set.addAll(set)
    println(name)
  }

  sealed trait RegLang
  case class CharInSet(chars: IntervalSet)
  case class Alts(langs: Seq[RegLang]) extends RegLang
  case class Seqn(langs: Seq[RegLang]) extends RegLang
  case class Star(lang: RegLang, greedy: Boolean) extends RegLang
  case class Plus(lang: RegLang, greedy: Boolean) extends RegLang
  case class QMark(lang: RegLAng, greedy: Boolean) extends RegLang

  def asRegLang(alts: LexerAltListContext): RegLang =


  def escapeAndEncloseWords(text: String): String = {
    val stringWithEscapedSymbols = text.replaceAll("([^\\w\\\\])", "\\\\$1")
    val wordsWithBoundaries = stringWithEscapedSymbols.replaceAll("\\b(\\w+)\\b", "\\\\b(?:$1)\\\\b")
    val finalString = wordsWithBoundaries.replaceAll("""\\\\\\\\\\""", """\\\\\\""")

    finalString
  }

}

