package vct.parsers.debug

import org.antlr.v4.runtime.Recognizer
import org.antlr.v4.runtime.atn._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait RenderExp {
  def render: (String, Int)

  def bind(minPrecedence: Int): String = {
    val (text, precedence) = render

    if(precedence < minPrecedence)
      s"($text)"
    else
      text
  }

  override def toString: String =
    render._1
}

sealed trait RegLang extends RenderExp {
  private def collectSeqn(buf: ArrayBuffer[RegLang]): Unit =
    this match {
      case Seqn(langs @ _*) => langs.foreach(_.simplify.collectSeqn(buf))
      case other => buf += other
    }

  private def collectAlts(buf: mutable.Set[RegLang]): Unit =
    this match {
      case Alts(langs @ _*) => langs.foreach(_.simplify.collectAlts(buf))
      case other => buf += other
    }

  def flatSeqn: Seq[RegLang] = {
    val buf = ArrayBuffer[RegLang]()
    collectSeqn(buf)
    buf.toSeq
  }

  def flatAlts: Seq[RegLang] = {
    val buf = mutable.Set[RegLang]()
    collectAlts(buf)
    buf.toSeq
  }

  def simplify: RegLang = this match {
    case tok: Antlr => tok
    case Seqn(_ @ _*) => flatSeqn match {
      case Nil => Seqn()
      case one :: Nil => one
      case more if more.contains(Alts()) => Alts()
      case more => Seqn(more: _*)
    }
    case Alts(_ @ _*) => flatAlts match {
      case Nil => Alts()
      case one :: Nil => one
      case more => Alts(more: _*)
    }
    case Star(lang) => lang.simplify match {
      case Seqn() => Seqn()
      case Alts(alts @ _*) => alts.filter(_ != Seqn()) match {
        case Nil => Seqn()
        case more => Star(Alts(more: _*))
      }
      case other => Star(other)
    }
  }
}

object Antlr {
  def apply(recognizer: Recognizer[_, _ <: ATNSimulator], transition: Transition): Antlr =
    transition match {
      case _: EpsilonTransition => new Antlr("ε", 100)
      case t: RangeTransition => new Antlr(t.toString, 70)
      case t: RuleTransition => new Antlr(recognizer.getRuleNames()(t.ruleIndex), 100)
      case t: PredicateTransition => new Antlr(s"{parser.sempred(_, ${t.ruleIndex}, ${t.predIndex})}?", 70)
      case t: AtomTransition => new Antlr(recognizer.getVocabulary.getDisplayName(t.label), 100)
      case t: ActionTransition => new Antlr(s"{parser.action(_, ${t.ruleIndex}, ${t.actionIndex})}", 70)
      case t: SetTransition => new Antlr(t.toString, 100)
      case t: NotSetTransition => new Antlr(t.toString, 70)
      case t: WildcardTransition => new Antlr(t.toString, 100)
      case t: PrecedencePredicateTransition => new Antlr(s"{$t}?", 70)
      case other => new Antlr(other.toString, 0)
    }
}

case class Antlr(text: String, precedence: Int) extends RegLang {
  override def render: (String, Int) = text -> precedence
}

case class Seqn(langs: RegLang*) extends RegLang {
  override def render: (String, Int) =
    langs match {
      case Nil => "()" -> 100
      case langs => langs.map(_.bind(50)).mkString(" ") -> 50
    }
}

case class Alts(langs: RegLang*) extends RegLang {
  override def render: (String, Int) = langs match {
    case Nil => "{false}?" -> 100
    case lang :: Nil => lang.render
    case Seqn() :: lang :: Nil => s"${lang.bind(80)}?" -> 80
    case lang :: Seqn() :: Nil => s"${lang.bind(80)}?" -> 80
    case langs => langs.mkString(" | ") -> 0
  }
}

case class Star(lang: RegLang) extends RegLang {
  override def render: (String, Int) = s"${lang.bind(80)}*" -> 80
}