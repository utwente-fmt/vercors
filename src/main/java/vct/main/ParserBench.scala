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

  def debugSOByMaxLookahead(maxK: Int): Unit = {
    Output("--- BY LOOKAHEAD ---")
    // filter(decision -> decision.SLL_MaxLook > 50).sorted((d1, d2) -> Long.compare(d2.SLL_MaxLook, d1.SLL_MaxLook))
    val decisionInfo = getParser().getParseInfo().getDecisionInfo()
    decisionInfo
      .filter(_.SLL_MaxLook > maxK)
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

class MyJavaParser(enableSpec: Boolean = false) extends Parser {
  var parser: JavaParser = null
  setNewParser()

  override def customSetupAndParse(): ParserRuleContext = {
    parser.specLevel = if (enableSpec) 1 else 0
    parser.compilationUnit()
  }

  override def setNewParser(): Unit = {
    parser = new JavaParser(null)
  }

  override def getParser(): org.antlr.v4.runtime.Parser = parser

  override def createLexer(cs: CharStream): Lexer = new LangJavaLexer(cs)
}

class MyJspecParser extends MyJavaParser(true) { }

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
  private val parsePvl = new BooleanSetting(false)
  private val parseJspec = new BooleanSetting(false)

  private val parsers = Map("java" -> new MyJavaParser(), "pvl" -> new MyPVLParser(), "jspec" -> new MyJspecParser())

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
    clops.add(parsePvl.getEnable(""), "parse-pvl")
    clops.add(parseJspec.getEnable(""), "parse-jspec")

    clops.parse(args)

    if (profile.get()) {
      val m : mutable.Map[String, Duration] = mutable.Map()

      val start = Instant.now()

      if (parsePvl.get()) {
        parsers("pvl").enableProfiling()

        val dir = "examples"
        val visitor = new RecursiveFileVisitor
        Files.walkFileTree(Paths.get(dir), Set(FileVisitOption.FOLLOW_LINKS).asJava, Integer.MAX_VALUE, visitor)
        if (visitor.delayedFail) {
          Output("Because of warnings above, the test suite will not run.")
          throw new HREExitException(1)
        }
        val cases = visitor.testsuite.asScala

        for ((name, kees) <- cases) {
          if (kees.tools.contains("silicon") || kees.tools.contains("carbon")) {
            val selectedFiles = kees.files.asScala.toSeq
              .map(_.toAbsolutePath.toString)
              .filter(f => f.contains(".pvl"))
              .map(Path.of(_))

            for (file <- selectedFiles) {
              val d = myParse(file)
              Output("%s Took: %dms", name, d.toMillis)
              m.put(name, d)
            }
          }
        }

        parsers("pvl").debugSOByTime()
        parsers("pvl").debugSOByMaxLookahead(50)

        val end = Instant.now()
        Output("Total parsing time taken: %sms", Duration.between(start, end).toMillis)
        0
      } else if (parseJspec.get()) {
        val base = "src/main/universal/res/config"
        val files = Seq(
          "silver_optimize.jspec",
          "simplify_expr.jspec",
          "simplify_quant_pass1.jspec",
          "simplify_quant_pass2.jspec",
          "summation.jspec"
        )

        parsers("jspec").enableProfiling()

        for (jspecFile <- files) {
          val path = Paths.get(base, jspecFile)
          val d = myParse(path)
          Output("%s Took: %dms", jspecFile, d.toMillis)
          m.put(jspecFile, d)
        }

        parsers("jspec").debugSOByTime()
        parsers("jspec").debugSOByMaxLookahead(0)

        val end = Instant.now()
        Output("Total parsing time taken: %sms", Duration.between(start, end).toMillis)
        0
      } else {
        Output("No filetype specified")
        1
      }
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
