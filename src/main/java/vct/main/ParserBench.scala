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

  private lazy val vercors = Configuration.getThisVerCors

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
    var will_fail = visitor.delayed_fail
    if (will_fail) {
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
      // 8 secs when commenting the "target" out in "newExpr"
      // seqAddExpr is a bit messy?
      // Values overlaps with collectionConstructors, but it doesn't seem to matter
      // typeDims can be refactored by having both alternatives use +, and adding one "empty" alternative. Only saves 63ms though
      // | expr | is both in nonTargetUnit and valPrimary, saves about 200 ambiguities

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
