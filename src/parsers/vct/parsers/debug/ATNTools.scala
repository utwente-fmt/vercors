package vct.parsers.debug

import org.antlr.v4.runtime.{Parser, TokenStream, Recognizer => ANTLRRecognizer}
import org.antlr.v4.runtime.atn._

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Using

object ATNTools {
  type Recognizer = ANTLRRecognizer[_, _ <: ATNSimulator]

  def vertexNameAttrs(recognizer: Recognizer, state: ATNState): (String, String) =
    state match {
      case start: RuleStartState =>
        val name = recognizer.getRuleNames()(start.ruleIndex)
        (name, "[color=green]")
      case end: RuleStopState =>
        val name = recognizer.getRuleNames()(end.ruleIndex)
        (s"end${name.capitalize}", "[color=red]")
      case start: BlockStartState =>
        (s"${start.endState.stateNumber}#{", "")
      case end: BlockEndState =>
        (s"${end.stateNumber}#}", "")
      case other =>
        (s"#${other.stateNumber}", "")
    }

  def outputGraph(recognizer: Recognizer, edges: Seq[(ATNState, RegLang, ATNState)], out: Appendable): Unit = {
    out.append("digraph {\n")

    val vertices = edges.flatMap(e => Seq(e._1, e._3)).distinct

    for(vertex <- vertices) {
      val (name, attrs) = vertexNameAttrs(recognizer, vertex)
      out.append(s"  \"$name\" $attrs\n")
    }

    out.append("  \n")

    for(edge <- edges) {
      val start = vertexNameAttrs(recognizer, edge._1)._1
      val end = vertexNameAttrs(recognizer, edge._3)._1
      out.append(s"  \"$start\" -> \"$end\" [label=\"${edge._2}\"]\n")
    }

    out.append("}\n")
  }

  def transitionLanguage(recognizer: Recognizer, trans: Transition): RegLang =
    trans match {
      case _: EpsilonTransition => Seqn()
      case other => Antlr(recognizer, other)
    }

  def getEdges(recognizer: Recognizer, from: ATNState, expandRules: Set[Int] = Set.empty): Seq[(ATNState, RegLang, ATNState)] = {
    val toExplore = mutable.Set(from)
    val explored = mutable.Set[ATNState]()
    val edges = mutable.ArrayBuffer[(ATNState, RegLang, ATNState)]()

    while(toExplore.nonEmpty) {
      val s = toExplore.head
      toExplore.remove(s)
      explored += s

      val follow = s match {
        case r: RuleStopState if !expandRules.contains(r.ruleIndex) => false
        case _ => true
      }

      if (follow) {
        for (trans <- s.getTransitions) {
          val target = trans match {
            case rule: RuleTransition if !expandRules.contains(rule.ruleIndex) =>
              rule.followState
            case other =>
              other.target
          }

          edges += ((s, transitionLanguage(recognizer, trans), target))

          if (!explored.contains(target)) {
            toExplore += target
          }
        }
      }
    }

    edges.toSeq
  }

  class LanguageGraph(val recognizer: Recognizer, val s0: ATNState, val accept: ATNState, edges: Iterable[(ATNState, RegLang, ATNState)]) {
    // only read or removed from
    private final val EMPTY_MAP = mutable.Map[ATNState, RegLang]()

    private val inEdge: mutable.Map[ATNState, mutable.Map[ATNState, RegLang]] = mutable.Map()
    private val outEdge: mutable.Map[ATNState, mutable.Map[ATNState, RegLang]] = mutable.Map()

    for((start, lang, end) <- edges) {
      addEdge(start, lang, end)
    }

    def addEdge(start: ATNState, lang: RegLang, end: ATNState): Unit = {
      val startOut = outEdge.getOrElseUpdate(start, mutable.Map())
      val endIn = inEdge.getOrElseUpdate(end, mutable.Map())

      startOut(end) = Alts(lang, startOut.getOrElse(end, Alts())).simplify
      endIn(start) = Alts(lang, endIn.getOrElse(start, Alts())).simplify
    }

    def delete(state: ATNState): Unit = {
      require(s0 != state && accept != state)

      val (selfLoops, inEdges) = inEdge.remove(state).getOrElse(EMPTY_MAP).toSeq.partition(_._1 == state)
      val outEdges = outEdge.remove(state).getOrElse(EMPTY_MAP).toSeq.filterNot(_._1 == state)

      for((start, _) <- inEdges) outEdge.getOrElse(start, EMPTY_MAP).remove(state)
      for((end, _) <- outEdges) inEdge.getOrElse(end, EMPTY_MAP).remove(state)

      val selfLoopLang = Star(Alts(Seqn() +: selfLoops.map(_._2): _*))

      for((start, inLang) <- inEdges; (end, outLang) <- outEdges) {
        addEdge(start, Seqn(inLang, selfLoopLang, outLang), end)
      }
    }

    def output(path: Path): Unit = {
      Using(Files.newBufferedWriter(path)) { w =>
        outputGraph(recognizer, outEdge.toSeq.flatMap {
          case (start, ends) =>
            ends.map {
              case (end, lang) =>
                (start, lang, end)
            }
        }, w)
      }
    }

    def compact(): Unit = {
      output(Paths.get(s"tmp/${recognizer.getRuleNames()(s0.ruleIndex)}-0.dot"))

      for((state, i) <- inEdge.keys.toSeq.zipWithIndex) {
        if(state != s0 && state != accept) {
          delete(state)
        }

        output(Paths.get(s"tmp/${recognizer.getRuleNames()(s0.ruleIndex)}-${i+1}.dot"))
      }
    }

    def getLang(start: ATNState, end: ATNState): RegLang =
      outEdge.getOrElse(start, EMPTY_MAP).getOrElse(end, Alts())

    def asRegLang(): RegLang = {
      compact()
      val stayReject = getLang(s0, s0)
      val stayAccept = getLang(accept, accept)
      val goAccept = getLang(s0, accept)
      val goReject = getLang(accept, s0)

      val endAtReject = Star(Alts(stayReject, Seqn(goAccept, Star(stayAccept), goReject)))
      Seqn(endAtReject, goAccept, Star(stayAccept)).simplify
    }
  }

  def language(recognizer: Recognizer, from: ATNState, accept: ATNState): RegLang =
    new LanguageGraph(recognizer, from, accept, getEdges(recognizer, from)).asRegLang()

  /**
   * Reconstructs an ATN graph as a dot file for a given parse rule
   * Argument 1: class of the parser to analyze (e.g. vct.antlr4.generated.CParser)
   * Argument 2: parse rule to derive the ATN of (e.g. initializerList)
   * Argument 3: output file in DOT/graphviz format (e.g. initializerList.dot)
   */
  def main(args: Array[String]): Unit = {
    val parserClass = getClass.getClassLoader.loadClass(args(0))
    val parser = parserClass.getConstructor(classOf[TokenStream]).newInstance(null).asInstanceOf[Recognizer]
    val ruleIndex = parser.getRuleIndexMap.get(args(1))
    val state = parser.getATN.ruleToStartState(ruleIndex)
    val edges = getEdges(parser, state)
    Using(Files.newBufferedWriter(Paths.get(args(2)))) { w =>
      outputGraph(parser, edges, w)
    }
  }
}
