package vct.parsers.debug

import org.antlr.v4.runtime.Recognizer
import org.antlr.v4.runtime.atn._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.runtime.ScalaRunTime

trait RenderExp {
  def render(sb: Appendable): Unit
  def precedence: Int

  def bind(sb: Appendable, minPrecedence: Int): Unit = {
    if (precedence < minPrecedence) {
      sb.append("(")
      render(sb)
      sb.append(")")
    } else
      render(sb)
  }

  override def toString: String = {
    val sb = new StringBuilder()
    render(new Appendable {
      override def append(charSequence: CharSequence): Appendable = {
        sb.append(charSequence); this
      }
      override def append(
          charSequence: CharSequence,
          i: Int,
          i1: Int,
      ): Appendable = { sb.append(charSequence.subSequence(i, i1)); this }
      override def append(c: Char): Appendable = { sb.append(c); this }
    })
    sb.toString()
  }
}

/** Data structure to represent regular expressions with added node to represent
  * arbitrary (potentially non-regular) antlr expressions.
  */
sealed trait RegLang extends RenderExp {
  private[debug] def collectSeqn(buf: ArrayBuffer[RegLang]): Unit =
    this match {
      case Seqn(langs @ _*) => langs.foreach(_.collectSeqn(buf))
      case other => buf += other
    }

  private[debug] def collectAlts(buf: mutable.Set[RegLang]): Unit =
    this match {
      case Alts(langs @ _*) => langs.foreach(_.collectAlts(buf))
      case other => buf += other
    }
}

object Antlr {
  def apply(
      recognizer: Recognizer[_, _ <: ATNSimulator],
      transition: Transition,
  ): Antlr =
    transition match {
      case _: EpsilonTransition => new Antlr("ε", 100)
      case t: RangeTransition => new Antlr(t.toString, 70)
      case t: RuleTransition =>
        new Antlr(recognizer.getRuleNames()(t.ruleIndex), 100)
      case t: PredicateTransition =>
        new Antlr(s"{parser.sempred(_, ${t.ruleIndex}, ${t.predIndex})}?", 70)
      case t: AtomTransition =>
        new Antlr(recognizer.getVocabulary.getDisplayName(t.label), 100)
      case t: ActionTransition =>
        new Antlr(s"{parser.action(_, ${t.ruleIndex}, ${t.actionIndex})}", 70)
      case t: NotSetTransition =>
        new Antlr(
          t.label().toList.asScala
            .map(recognizer.getVocabulary.getDisplayName(_))
            .mkString("~(", "|", ")"),
          70,
        )
      case t: SetTransition =>
        new Antlr(
          t.label().toList.asScala
            .map(recognizer.getVocabulary.getDisplayName(_))
            .mkString("(", "|", ")"),
          70,
        )
      case t: WildcardTransition => new Antlr(t.toString, 100)
      case t: PrecedencePredicateTransition => new Antlr(s"{$t}?", 70)
      case other => new Antlr(other.toString, 0)
    }
}

case class Antlr(text: String, precedence: Int) extends RegLang {
  override def render(sb: Appendable): Unit = sb.append(text)
}

object Seqn {
  def apply(langs: RegLang*): RegLang = {
    val buf = ArrayBuffer[RegLang]()
    langs.foreach(_.collectSeqn(buf))
    buf.toSeq match {
      case Nil => new Seqn()
      case one +: Nil => one
      case more if more.contains(new Alts()) => new Alts()
      case more => new Seqn(more: _*)
    }
  }
}

case class Seqn(langs: RegLang*) extends RegLang {
  override lazy val hashCode: Int = ScalaRunTime._hashCode(this)

  @tailrec
  private def renderSeqnWithPlus(
      sb: Appendable,
      langs: Seq[RegLang],
      safe: Boolean,
  ): Unit =
    langs match {
      case Nil =>
      case Star(x) +: y +: tail if x == y =>
        if (!safe)
          sb.append(' ')
        x.bind(sb, 50)
        sb.append("+")
        renderSeqnWithPlus(sb, tail, false)
      case Star(x @ Seqn(xs @ _*)) +: tail if tail.startsWith(xs) =>
        if (!safe)
          sb.append(' ')
        x.bind(sb, 50)
        sb.append("+")
        renderSeqnWithPlus(sb, tail.drop(xs.size), false)
      case x +: tail =>
        if (!safe)
          sb.append(' ')
        x.bind(sb, 50)
        renderSeqnWithPlus(sb, tail, false)
    }

  override def render(sb: Appendable): Unit =
    langs match {
      case Nil => sb.append("()")
      case langs => renderSeqnWithPlus(sb, langs, true)
    }

  override def precedence: Int =
    langs match {
      case Nil => 100
      case _ => 50
    }
}

object Alts {
  def apply(langs: RegLang*): RegLang = {
    val buf = mutable.Set[RegLang]()
    langs.foreach(_.collectAlts(buf))
    buf.toSeq match {
      case Nil => new Alts()
      case one +: Nil => one
      case more => new Alts(more: _*)
    }
  }
}

case class Alts(langs: RegLang*) extends RegLang {
  override lazy val hashCode: Int = ScalaRunTime._hashCode(this)

  override def render(sb: Appendable): Unit =
    langs match {
      case Nil => sb.append("{false}?")
      case lang +: Nil => lang.render(sb)
      case langs if langs.contains(Seqn()) =>
        langs.filterNot(_ == Seqn()) match {
          case Nil => sb.append("()")
          case lang +: Nil => lang.bind(sb, 80)
          case langs =>
            sb.append("(")
            langs.head.bind(sb, 0)
            for (lang <- langs.tail) {
              sb.append(" | ")
              lang.bind(sb, 0)
            }
            sb.append(")?")
        }
      case langs =>
        langs.head.bind(sb, 0)
        for (lang <- langs.tail) {
          sb.append(" | ")
          lang.bind(sb, 0)
        }
    }

  override def precedence: Int =
    langs match {
      case Nil => 100
      case lang +: Nil => lang.precedence
      case langs if langs.contains(Seqn()) =>
        langs.filterNot(_ == Seqn()) match {
          case Nil => 100
          case _ +: Nil => 80
          case _ => 0
        }
      case _ => 0
    }
}

object Star {
  def apply(lang: RegLang): RegLang =
    lang match {
      case Seqn() => new Seqn()
      case Alts(alts @ _*) =>
        alts.filter(_ != new Seqn()) match {
          case Nil => new Seqn()
          case more => new Star(new Alts(more: _*))
        }
      case other => new Star(other)
    }
}

case class Star(lang: RegLang) extends RegLang {
  override lazy val hashCode: Int = ScalaRunTime._hashCode(this)

  override def render(sb: Appendable): Unit = {
    lang.bind(sb, 80)
    sb.append("*")
  }

  override def precedence: Int = 80
}
