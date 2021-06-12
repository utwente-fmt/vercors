package vct.main

import hre.config.{BooleanSetting, Configuration, OptionParser}
import hre.lang.HREExitException
import hre.lang.System.{Output, Warning}
import org.antlr.v4.runtime.{ANTLRErrorListener, CommonTokenStream, Lexer, ParserRuleContext, RecognitionException, Recognizer, TokenStream}
import vct.antlr4.generated.{JavaParser, LangJavaLexer, LangPVLLexer, PVLParser}
import org.antlr.v4.runtime
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
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
  def reset()

  def setInput(p: Path)

  def S(): ParserRuleContext

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
}

class MyJavaParser extends Parser {
  var l: LangJavaLexer
  var parser: JavaParser = null
  reset()

  override def setInput(path: Path): Unit = {
    val fis = new FileInputStream(path.toString)
    val cs = runtime.CharStreams.fromStream(fis)
    val lexer = new LangJavaLexer(cs)
    val tokens = new CommonTokenStream(lexer)
    parser.setTokenStream(tokens)
  }

  override def S(): ParserRuleContext = {
    parser.specLevel = 0
    encounteredError = false

    val t = parser.compilationUnit()

    if (encounteredError) {
      Output("ENCOUNTERED ERROR")
    }

    t
  }

  override def reset(): Unit = {
    parser = new JavaParser(null)
    installErrorListener(parser)
  }
}

class MyPVLParser extends Parser {
  var parser: PVLParser = null
  reset()

  override def setInput(path: Path): Unit = {
    val fis = new FileInputStream(path.toString)
    val cs = runtime.CharStreams.fromStream(fis)
    val lexer = new LangPVLLexer(cs)
    val tokens = new CommonTokenStream(lexer)
    parser.setTokenStream(tokens)
  }

  override def S(): ParserRuleContext = {
    encounteredError = false
    val t = parser.program()
    if (encounteredError) {
      Output("ENCOUNTERED ERROR")
    }
    t
  }

  override def reset(): Unit = {
    parser = new PVLParser(null)
    installErrorListener(parser)
  }
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

      for ((name, kees) <- cases) {
        if (kees.tools.contains("silicon") || kees.tools.contains("carbon")) {
          val selectedFiles = kees.files.asScala.toSeq
            .map(_.toAbsolutePath.toString)
            .filter(f => f.contains(".java") || f.contains(".pvl"))
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

  var reusedParser: JavaParser = null

  def myParse(file: Path): Duration = {
    val start = Instant.now()

    val fis = new FileInputStream(file.toAbsolutePath.toString)
    val cs = runtime.CharStreams.fromStream(fis)
    val lexer = new LangJavaLexer(cs)
    val tokens = new CommonTokenStream(lexer)

    Output("%s", getExtension(file).get)

    val parser: Parser = parsers(getExtension(file).get)

    if (reuseParser.get()) {
      parser.reset()
    }

    parser.setTokenStream(tokens)

    val tree = parser.S()

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
