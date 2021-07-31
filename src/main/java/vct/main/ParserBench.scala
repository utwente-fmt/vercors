package vct.main

import hre.config.{BooleanSetting, Configuration, OptionParser}
import hre.lang.HREExitException
import hre.lang.System.{Output, Warning}
import org.antlr.v4.runtime.{ANTLRErrorListener, BailErrorStrategy, CharStream, CommonTokenStream, DefaultErrorStrategy, Lexer, ParserRuleContext, RecognitionException, Recognizer, TokenStream}
import vct.antlr4.generated.{JavaParser, LangJavaLexer, LangPVLLexer, PVLParser}
import org.antlr.v4.runtime
import org.antlr.v4.runtime.atn.{ATNConfigSet, PredictionMode}
import org.antlr.v4.runtime.dfa.DFA
import org.antlr.v4.runtime.misc.ParseCancellationException
import vct.parsers.ErrorCounter
import vct.test.CommandLineTesting.{caseFilters, testDirs, vercors}
import vct.test.{CommandLineTesting, RecursiveFileVisitor, Task}

import java.io.FileInputStream
import java.nio.file.{FileVisitOption, Files, Path, Paths}
import java.time.{Duration, Instant}
import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object ParserBench {
  def main(args: Array[String]): Unit = System.exit(new ParserBench().run(args))
}

trait Parser {
  def customSetupAndParse(): ParserRuleContext

  def createLexer(cs: CharStream): org.antlr.v4.runtime.Lexer

  def setNewParser(): Unit

  def getParser(): org.antlr.v4.runtime.Parser

  def setInput(path: Path): Unit = {
    setInput(Files.readString(path))
  }

  var contents: String = null
  def setInput(contents: String): Unit = {
    this.contents = contents
  }

  def parse(): ParserRuleContext = {
    var parserRuleContext: ParserRuleContext = null

    try {
      // Stage 1: High-speed parsing for correct documents
      val p = getParser()
      p.reset()

      p.setErrorHandler(new BailErrorStrategy())
      p.getInterpreter().setPredictionMode(PredictionMode.SLL)

      val cs = runtime.CharStreams.fromString(contents)
      val lexer = createLexer(cs)
      val tokens = new CommonTokenStream(lexer)
      p.setInputStream(tokens)

      encounteredError = false
      parserRuleContext = customSetupAndParse()
      if (encounteredError) {
        Output("ERROR DURING PARSING")
      }
    } catch {
      case e: ParseCancellationException => {
        val p = getParser()
        p.reset()

        // Reset input stream
        val cs = runtime.CharStreams.fromString(contents)
        val lexer = createLexer(cs)
        val tokens = new CommonTokenStream(lexer)
        p.setInputStream(tokens)

        // Stage 2: High-accuracy fallback parsing for complex and/or erroneous documents
        p.setErrorHandler(new DefaultErrorStrategy())
        p.getInterpreter().setPredictionMode(PredictionMode.LL)

        encounteredError = false
        parserRuleContext = customSetupAndParse()
        if (encounteredError) {
          Output("ERROR DURING PARSING")
        }
      }
    }

    parserRuleContext
  }

  def resetParser(): Unit = {
    setNewParser()
    installErrorListener(getParser())
  }

  var encounteredError = false;

  def installErrorListener(p: org.antlr.v4.runtime.Parser): Unit = {
    p.removeErrorListeners()
    p.addErrorListener(new ANTLRErrorListener {
      override def syntaxError(recognizer: Recognizer[_, _], o: Any, i: Int, i1: Int, s: String, e: RecognitionException): Unit = {
        encounteredError = true
      }

      override def reportAmbiguity(parser: runtime.Parser, dfa: DFA, i: Int, i1: Int, b: Boolean, bitSet: util.BitSet, atnConfigSet: ATNConfigSet): Unit = {}

      override def reportAttemptingFullContext(parser: runtime.Parser, dfa: DFA, i: Int, i1: Int, bitSet: util.BitSet, atnConfigSet: ATNConfigSet): Unit = {}

      override def reportContextSensitivity(parser: runtime.Parser, dfa: DFA, i: Int, i1: Int, i2: Int, atnConfigSet: ATNConfigSet): Unit = {}
    })
  }

  def enableProfiling(): Unit = {
    getParser().setProfile(true)
    getParser().getInterpreter().setPredictionMode(PredictionMode.SLL)
  }

  def debugSOByTime(): Unit = {
    Output("--- BY TIME ---")
    val decisionInfo = getParser().getParseInfo().getDecisionInfo()
    decisionInfo
      .filter(_.timeInPrediction > 100)
      .sortWith((a, b) => b.timeInPrediction < a.timeInPrediction) // Reversed
      .foreach(decision => {
        Output("Time: %d in %d calls - LL_Lookaheads: %d Max k: %d Ambiguities: %d Errors: %d Rule: %s",
          decision.timeInPrediction / 1000000,
          decision.invocations,
          decision.SLL_TotalLook,
          decision.SLL_MaxLook,
          decision.ambiguities.size(),
          decision.errors.size(),
          getParser().getRuleNames()(getParser().getATN.getDecisionState(decision.decision).ruleIndex)
        )
//        Output("%s", decision.SLL_MaxLookEvent.input.getText)
//        for (a <- decision.ambiguities.asScala) {
//          Output("-------------------------")
//          for (ambigAlt <- a.ambigAlts.stream().toArray) {
//            Output("Alt: %s", getParser().getRuleNames()(ambigAlt))
//          }
//          val sb = new StringBuilder()
//          for (i <- 0 until a.startIndex) {
//            sb.append(decision.SLL_MaxLookEvent.input.get(i).getText)
//            sb.append(' ')
//          }
//          sb.append("\n|||\n")
//          for (i <- a.startIndex until a.stopIndex) {
//            sb.append(decision.SLL_MaxLookEvent.input.get(i).getText)
//            sb.append(' ')
//          }
//          Output("%s", sb)
//        }
      })
  }

  def debugSOByMaxLookahead(): Unit = {
    Output("--- BY LOOKAHEAD ---")
    // filter(decision -> decision.SLL_MaxLook > 50).sorted((d1, d2) -> Long.compare(d2.SLL_MaxLook, d1.SLL_MaxLook))
    val decisionInfo = getParser().getParseInfo().getDecisionInfo()
    decisionInfo
      .filter(_.SLL_MaxLook > 50)
      .sortWith((a, b) => b.SLL_MaxLook < a.SLL_MaxLook) // Reversed
      .foreach(decision => {
        Output("Time: %d in %d calls - LL_Lookaheads: %d Max k: %d Ambiguities: %d Errors: %d Rule: %s",
          decision.timeInPrediction / 1000000,
          decision.invocations,
          decision.SLL_TotalLook,
          decision.SLL_MaxLook,
          decision.ambiguities.size(),
          decision.errors.size(),
          getParser().getRuleNames()(getParser().getATN.getDecisionState(decision.decision).ruleIndex)
        )
//        Output("%s", decision.SLL_MaxLookEvent.input.getText)
//        val mle = decision.SLL_MaxLookEvent
//        Output("%s", decision.SLL_MaxLookEvent.input.getText.substring(0, mle.startIndex))
//        Output("|||")
//        Output("%s", decision.SLL_MaxLookEvent.input.getText.substring(mle.startIndex, mle.stopIndex))
      })
  }
}

class MyJavaParser extends Parser {
  var parser: JavaParser = null
  setNewParser()

  override def customSetupAndParse(): ParserRuleContext = {
    parser.specLevel = 0
    parser.compilationUnit()
  }

  override def setNewParser(): Unit = {
    parser = new JavaParser(null)
  }

  override def getParser(): org.antlr.v4.runtime.Parser = parser

  override def createLexer(cs: CharStream): Lexer = new LangJavaLexer(cs)
}

class MyPVLParser extends Parser {
  var parser: PVLParser = null
  setNewParser()

  override def customSetupAndParse(): ParserRuleContext = parser.program()

  override def setNewParser(): Unit = {
    parser = new PVLParser(null)
  }

  override def getParser(): org.antlr.v4.runtime.Parser = parser

  override def createLexer(cs: CharStream): Lexer = new LangPVLLexer(cs)
}

class ParserBench {
  private val separateRuns = new BooleanSetting(false)
  private val combinedRuns = new BooleanSetting(false)
  private val profile = new BooleanSetting(false)
  private val reuseParser = new BooleanSetting(false)

  private val parsers = Map("java" -> new MyJavaParser(), "pvl" -> new MyPVLParser())

  private lazy val vercors = Configuration.getThisVerCors(Seq().asJava)

  def getExtension(p: Path): Option[String] = {
    val s = p.toString
    if (s.contains(".")) {
      Some(s.split('.')(1))
    } else {
      None
    }
  }

  def run(args: Array[String]): Int = {
    hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info)
    hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info)

    val clops = new OptionParser
    clops.add(separateRuns.getEnable(""), "separate")
    clops.add(combinedRuns.getEnable(""), "combined")
    clops.add(profile.getEnable(""), "profile")
    clops.add(reuseParser.getEnable(""), "reuse-parser")

    clops.parse(args)

    val dir = "examples"
    val visitor = new RecursiveFileVisitor
    Files.walkFileTree(Paths.get(dir), Set(FileVisitOption.FOLLOW_LINKS).asJava, Integer.MAX_VALUE, visitor)
    if (visitor.delayedFail) {
      Output("Because of warnings above, the test suite will not run.")
      throw new HREExitException(1)
    }

    val cases = visitor.testsuite.asScala

    if (separateRuns.get()) {
      Output("Separate")

      val m : mutable.Map[String, Duration] = mutable.Map()
      for ((name, kees) <- cases) {
        val d = parse(kees.files.asScala.toSeq.map(_.toAbsolutePath.toString).filter(_.contains(".java")))
        Output("%s Took: %dms", name, d.toMillis)
        m.put(name, d)
      }

      Output("name,num_files,bytes,separate_run_millis")
      for ((name, d) <- m) {
        Output("%s,%d,%d,%d",
          name,
          cases.get(name).get.files.size(),
          cases.get(name).get.files.asScala.map(countBytes(_)).sum,
          d.toMillis)
      }

      0
    } else if (combinedRuns.get()) {
      Output("Combined")

      val files = cases.flatMap(_._2.files.asScala.toSeq
        .map(_.toAbsolutePath.toString)
      ).toSeq
      Output("%s", files)

      val d = parse(files)
      Output("Parsing all took: %dms", d.toMillis)

      0
    } else if (profile.get()) {
      val m : mutable.Map[String, Duration] = mutable.Map()

      val start = Instant.now()

      parsers("pvl").enableProfiling()

      for ((name, kees) <- cases) {
        if (kees.tools.contains("silicon") || kees.tools.contains("carbon")) {
          val selectedFiles = kees.files.asScala.toSeq
            .map(_.toAbsolutePath.toString)
//            .filter(f => f.contains(".java") || f.contains(".pvl"))
            .filter(f => f.contains(".pvl"))
            .map(Path.of(_))

          if (selectedFiles.length == 0) {
            //          Output("----- Skipping: %s", name)
          } else {
            for (file <- selectedFiles) {
              val d = myParse(file)
              Output("%s Took: %dms", name, d.toMillis)
              m.put(name, d)
            }
          }
        }
      }

      parsers("pvl").debugSOByTime()
      parsers("pvl").debugSOByMaxLookahead()

      val end = Instant.now()
      Output("Total parsing time taken: %sms", Duration.between(start, end).toMillis)
      // 39 secs for pvl before commenting out stuff
      // DONE: 8 secs when commenting the "target" out in "newExpr"
      // NOT APPLIED: seqAddExpr is a bit messy?
      // - Not sure what I meant by that. Contains several operators but looks fine besides that.
      // LATER REFACTOR: Values overlaps with collectionConstructors, but it doesn't seem to matter
      // - This is an opportunity for refactoring the collections section in the parser, though.
      // DONE: typeDims can be refactored by having both alternatives use +, and adding one "empty" alternative. Only saves 63ms though
      // DONE: | expr | is both in nonTargetUnit and valPrimary, saves about 200 ambiguities
      // DONE: Shouldn't use both recursive and non-recursive form for parUnitList (see: https://stackoverflow.com/questions/42093553/antlr-does-not-automatically-do-lookahead-matching)
      // - Seems to reduce the parsing time about half a second
      // DONE: Same for iteExpr, saves about 2 secs!
      // Commenting out Reducible until "set < langtype >" in SpecParser shaves off about 400 ms
      //    (initial: 1400ms, after: 943)
      //    So seems that overlapping with function calls is problematic
      //    Though this one seems hard to actually resolve - we _need_ to overlap with the host language in this case...
      //    So I am leaving this one in for now
      //    Maybe if you assume that:
      //    - Each lang has function calls
      //    - generics can be turned on/off when needed
      //    Then there doesn't have to be overlap
      /* The following block also has about 150 ambiguities:
            | collectionConstructors // About 100 ambiguities
            | 'map' '<' type ',' type '>' mapValues // -5 ambiguities?
            | 'tuple' '<' type ',' type '>' values // 0 ambiguities
            | builtinMethod tuple // 49 ambiguities
         But it causes quite a few parse errors so it's hard to turn off...?
       */
      /*
        args, exprList, mapPairs also should not be wrongly/duplicatingly tail recursive. Not sure if they matter though
       */
      /* rule above seqAddExpr (powExpr) should refer to seqAddExpr. Also, seqAddExpr also has duplicating tail. */
      /* nonTargetUnit: "'?' identifier" is also in builtinMethod! */
      /* clazMember is probably slow because it partially overlaps with method (i.e. int a looks similar to int a())
         refactor by merging field, constructor, and method, and specifying in COL which one you want by not allowing them (i.e. not having an argument list and no contract means a field)
       */
      /*
        Cannot figure out what is the problem with eqExpr. Even refactoring to relExpr ((== | !=) eqExpr)? doesn't help.
        Maybe we should just switch over to how antlr handles binary grammars, and just enter associativity/precedence?
       */
      /* builtinMethod completely overlaps with Spec! */
      /* action in statement is duplicate */
      /* constructor and method overlap: from 1200 to 90 */
      /* contract & invariantList overlap, reduces from 1100 to 309 */
      /* 'action' overlap between valStatement and statement saves about 100ms */
      /* 'atomic' overlap between valStatement and statement saves another 100ms */
      /* label appears in both statement and valStatement, but doesn't matter for parsing */
      /*
        For nonTargetUnit:
        - langID : langExpr is not a problem (in valPrimary)
        - removing true, false, and \result shaves off 300ms
       */
      0
    } else {
      Output("No mode specified")
      1
    }
  }

  def countBytes(file: Path): Int = {
    Files.readAllBytes(file).length
  }

  def parse(files: Seq[String]): Duration = {
    val v = vercors.withArgs(Seq("--parse-only", "--silicon", "--debug", "vct.main.Main") ++ files : _*)
    v.setWorkingDirectory(Paths.get("").toAbsolutePath)
    val t = Task(v, Seq())
    val start = Instant.now()
    t.call()
    val end = Instant.now()
    for (m <- t.log) {
      Output("%s", m.getFormattedMessage)
    }

    Duration.between(start, end)
  }

  def myParse(file: Path): Duration = {
    Output("%s", getExtension(file).get)
    Output("File: %s", file)

    val parser: Parser = parsers(getExtension(file).get)

    if (!reuseParser.get()) {
      parser.resetParser()
    }

    parser.setInput(file)

    val start = Instant.now()

    val tree = parser.parse()

    val end = Instant.now()

    Duration.between(start, end)
  }

  protected def silenceParser(parser: runtime.Parser, name: String): ErrorCounter = {
    parser.removeErrorListeners()
    val ec = new ErrorCounter(name)
    parser.addErrorListener(ec)
    ec
  }

  protected def silenceLexer(lexer: runtime.Lexer, name: String): ErrorCounter = {
    lexer.removeErrorListeners()
    val ec = new ErrorCounter(name)
    lexer.addErrorListener(ec)
    ec
  }
}
