package vct.parsers

import hre.lang.System.Failure
import org.antlr.v4.runtime
import org.antlr.v4.runtime.atn.PredictionMode
import org.antlr.v4.runtime.misc.ParseCancellationException
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{ANTLRErrorListener, BailErrorStrategy, DefaultErrorStrategy}
import vct.col.ast.stmt.decl.ProgramUnit

import java.io.{File, FileInputStream, FileNotFoundException, InputStream}

abstract class Parser {
  def parse(stream: runtime.CharStream, name: String): ProgramUnit

  def parse(stream: InputStream, name: String): ProgramUnit =
    parse(runtime.CharStreams.fromStream(stream), name)

  def parse(f: File): ProgramUnit = {
    val name = f.toString
    try {
      parse(new FileInputStream(f), name)
    } catch {
      case _: FileNotFoundException =>
        throw Failure("Could not find file: %s", name)
    }
  }

  protected def errorCounter(parser: runtime.Parser, lexer: runtime.Lexer, name: String): ErrorCounter = {
    parser.removeErrorListeners()
    lexer.removeErrorListeners()
    val ec = new ErrorCounter(name)
    parser.addErrorListener(ec)
    lexer.addErrorListener(ec)
    ec
  }

  /**
    * Performs a parse, first in SLL mode because it can sometimes be faster. If that fails
    * because of a false positive, the parse is retried in slightly slower LL mode, which only fails if there
    * is an actual syntax error.
    *
    * @param customErrorListener A custom error listener. Required, but will only be used on LL mode.
    * @param parser
    * @param parseCommand Performs the actual parse. If there are any variables that might need to be reset on second parse, this command should do that.
    * @param profile Whether or not to do profiling.
    * @return A possibly incomplete parse tree. Inspect the custom error listener to know if there were errors.
    */
  def parseLLSLL(customErrorListener: ANTLRErrorListener,
                 parser: org.antlr.v4.runtime.Parser,
                 parseCommand: () => ParseTree,
                 profile: Boolean): ParseTree = {
    if (profile) {
      parser.setProfile(true)
    }

    try {
      parser.getInterpreter.setPredictionMode(PredictionMode.SLL);
      parser.removeErrorListeners()
      // SLL might fail with a false positive. Therefore, the BailErrorStrategy is used,
      // to ensure that as soon as parsing fails, the parser can switch to LL, which has
      // no false positives.
      parser.setErrorHandler(new BailErrorStrategy)

      parseCommand()
    } catch {
      case _: ParseCancellationException =>
        parser.reset()
        parser.addErrorListener(customErrorListener)
        parser.setErrorHandler(new DefaultErrorStrategy)
        parser.getInterpreter.setPredictionMode(PredictionMode.LL)

        parseCommand()
    }
  }

  def getDebugInfo(parser: org.antlr.v4.runtime.Parser): String = {
    val decisionInfo = parser.getParseInfo().getDecisionInfo()
    decisionInfo
      .filter(_.timeInPrediction > 100)
      .sortWith((a, b) => b.timeInPrediction < a.timeInPrediction) // Reversed
      .map(decision =>
        "Time: %d in %d calls - SLL_Lookaheads: %d SLL_Max k: %d LL_Lookaheads: %d LL_Max k: %d Ambiguities: %d Errors: %d Rule: %s".format(
          decision.timeInPrediction / 1000000,
          decision.invocations,
          decision.SLL_TotalLook,
          decision.SLL_MaxLook,
          decision.LL_TotalLook,
          decision.LL_MaxLook,
          decision.ambiguities.size(),
          decision.errors.size(),
          parser.getRuleNames()(parser.getATN.getDecisionState(decision.decision).ruleIndex)
        ))
        .fold("")(_ + _)
  }
}
