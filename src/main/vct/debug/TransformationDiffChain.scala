package vct.debug

import vct.col.ast.{Node, Verification}
import vct.col.origin.Origin
import vct.col.print
import vct.col.print.{Cons, Ctx, Doc, EEnd, ELine, EStart, EText, Elem, Empty, Group, InfoDoc, Line, Nest, NonWsLine, Text}
import vct.main.stages.Transformation
import vct.options.types.PathOrStd

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object TransformationDiffChain {
  import vct.col.ast

  implicit val o: Origin = Origin(Nil)

  private val zeroNode = ast.IntegerValue(0)
  private val oneNode = ast.IntegerValue(1)
  private val twoNode = ast.IntegerValue(2)
  private val threeNode = ast.IntegerValue(3)
  private val fourNode = ast.IntegerValue(4)

  private val zero = InfoDoc(Text("0"))(zeroNode)
  private val one = InfoDoc(Text("1"))(oneNode)
  private val two = InfoDoc(Text("2"))(twoNode)
  private val three = InfoDoc(Text("3"))(threeNode)
  private val four = InfoDoc(Text("4"))(fourNode)

  def main(args: Array[String]): Unit = {
    val chain = new TransformationDiffChain(null)
    println(chain.infosEditDistance(Seq(
      zero,
      two,
      three,
    ), Seq(
      one,
      two,
      four,
    )))
  }
}

class TransformationDiffChain(out: PathOrStd) extends Transformation.Log {
  import TransformationDiffChain._

  private val baseCtx = Ctx(width = 60, tabWidth = 1)

  var log: ArrayBuffer[(String, Doc)] = ArrayBuffer()

  def getInfos(doc: Doc): Seq[InfoDoc] =
    doc match {
      case Empty => Nil
      case NonWsLine => Nil
      case Line => Nil
      case Cons(left, right) => getInfos(left) ++ getInfos(right)
      case Text(_) => Nil
      case Nest(doc) => getInfos(doc)
      case Group(doc) => getInfos(doc)
      case node @ InfoDoc(_) => Seq(node)
    }

  def getTexts(doc: Doc): Seq[String] =
    doc match {
      case Empty => Nil
      case NonWsLine => Nil
      case Line => Nil
      case Cons(left, right) => getTexts(left) ++ getTexts(right)
      case Text(text) => Seq(text)
      case Nest(doc) => getTexts(doc)
      case Group(doc) => getTexts(doc)
      case InfoDoc(doc) => getTexts(doc)
    }

  sealed trait Action
  case class Delete(left: InfoDoc) extends Action
  case class Insert(right: InfoDoc) extends Action
  case class Substitute(left: InfoDoc, right: InfoDoc) extends Action

  @tailrec
  final def textEditDistance(left: Seq[String], right: Seq[String]): Int =
    if(left.isEmpty) right.size
    else if(right.isEmpty) left.size
    else if(left.head == right.head) textEditDistance(left.tail, right.tail)
    else if(left.last == right.last) textEditDistance(left.init, right.init)
    else {
      val distance = Array.fill[Int](left.size+1, right.size+1)(-1000)
      for (i <- (0 to left.size).reverse) {
        for (j <- (0 to right.size).reverse) {
          (left.drop(i), right.drop(j)) match {
            case Nil -> Nil =>
              distance(i)(j) = 0
            case Nil -> right =>
              distance(i)(j) = right.size
            case left -> Nil =>
              distance(i)(j) = left.size
            case (l +: left) -> (r +: right) if l == r =>
              distance(i)(j) = distance(i+1)(j+1)
            case (l +: left) -> (r +: right) =>
              distance(i)(j) = Seq(distance(i+1)(j), distance(i)(j+1)).min
          }
        }
      }

      distance(0)(0)
    }

  def textDistance(left: Doc, right: Doc): Double = {
    val leftTexts = getTexts(left)
    val rightTexts = getTexts(right)

    if(leftTexts.isEmpty && rightTexts.isEmpty) 0.0
    else ((leftTexts.toSet -- rightTexts.toSet) ++ (rightTexts.toSet -- leftTexts.toSet)).size.toDouble / Seq(leftTexts.size, rightTexts.size).max
  }

  def infosEditDistance(left: Seq[InfoDoc], right: Seq[InfoDoc]): (Double, Seq[Action]) = {
    if(left.isEmpty && right.isEmpty) return (0.0, Nil)

    val action = Array.fill[Action](left.size+1, right.size+1)(null)
    val distance = Array.fill[Double](left.size+1, right.size+1)(-1000.0)

    for(i <- (0 to left.size).reverse) {
      for(j <- (0 to right.size).reverse) {
        (left.drop(i), right.drop(j)) match {
          case Nil -> Nil =>
            distance(i)(j) = 0.0
          case Nil -> right =>
            action(i)(j) = Insert(right.head)
            distance(i)(j) = right.size
          case left -> Nil =>
            action(i)(j) = Delete(left.head)
            distance(i)(j) = left.size
          case (l +: left) -> (r +: right) =>
            val insertCost = 1
            val deleteCost = 1
            val substitutePenalty = (insertCost + deleteCost) * 1.2

            val subDistance = distance(i+1)(j+1)
            val delDistance = distance(i+1)(j)
            val insDistance = distance(i)(j+1)

            val resultSubstitute = textDistance(l.doc, r.doc) * substitutePenalty + subDistance
            val resultDelete = deleteCost + delDistance
            val resultInsert = insertCost + insDistance

            val best = Seq(resultSubstitute, resultDelete, resultInsert).min

            val bestAction =
              if (best == resultSubstitute) Substitute(l, r)
              else if (best == resultDelete) Delete(l)
              else if (best == resultInsert) Insert(r)
              else ???

            action(i)(j) = bestAction
            distance(i)(j) = best
        }
      }
    }

    var path = Seq.empty[Action]

    var (i, j) = (0, 0)

    while((i, j) != (left.size, right.size)) {
      path :+= action(i)(j)
      path.last match {
        case Delete(_) => i += 1
        case Insert(_) => j += 1
        case Substitute(_, _) => i += 1; j += 1
      }
    }

    (distance(0)(0) / Seq(left.size, right.size).max, path)
  }

  def docEditDistance(left: Doc, right: Doc): (Double, Seq[Action]) = {
    val leftDocs = getInfos(left)
    val rightDocs = getInfos(right)
    if(leftDocs.isEmpty || rightDocs.isEmpty)
      (if(left == right) 0.0 else 1.0, Nil)
    else
      infosEditDistance(leftDocs, rightDocs)
  }

  def flatInfoSyncs(left: Doc, right: Doc): Seq[(InfoDoc, InfoDoc)] =
    docEditDistance(left, right)._2.flatMap {
      case Delete(_) => Nil
      case Insert(_) => Nil
      case Substitute(left, right) =>
        (left, right) +: flatInfoSyncs(left.doc, right.doc)
    }

  private var _uniqueId: Int = 0
  def uniqueId(): Int = { _uniqueId += 1; _uniqueId }

  override def accept(afterPass: String, verification: Verification[_]): Unit = {
    implicit val ctx: Ctx = baseCtx.namesIn(verification)
    log += ((afterPass, verification.show.rewrite(rec => {
      case info @ InfoDoc(doc) if info.info.isInstanceOf[Node[_]] =>
        InfoDoc(rec(doc))(uniqueId())
    })))
  }

  override def finish(): Unit = {
    out.write(out => {
      val rsynced = mutable.Map[Any, Any]()
      val lsynced = mutable.Map[Any, Any]()

      for (((_, left), (_, right)) <- log.zip(log.tail)) {
        for ((leftNode, rightNode) <- flatInfoSyncs(left, right)) {
          rsynced += leftNode.info -> rightNode.info
          lsynced += rightNode.info -> leftNode.info
        }
      }

      val lines = log.map(_._2.lines(baseCtx).map(_.toIndexedSeq).toIndexedSeq)

      val positions = Array.fill(log.size)(0)
      val printed = mutable.Set[SyncSpec]()

      out.write(log.map(_._1.padTo(62, ' ')).mkString("") + "\n")

      // Should we print this line?
      sealed trait State
      case object Print extends State // yes
      case object DoNotPrint extends State // no
      case object DependLeft extends State // only if my left prints
      case object DependRight extends State // only if my right prints
      case object DependLeftRight extends State // only if my left *and* right print
      val state = Array.fill[State](log.size)(DoNotPrint)

      sealed trait SyncSpec {
        def data: Any
        def copy(data: Any = data): SyncSpec
      }
      case class SyncStart(data: Any) extends SyncSpec {
        override def copy(data: Any): SyncSpec = SyncStart(data)
      }
      case class SyncEnd(data: Any) extends SyncSpec {
        override def copy(data: Any): SyncSpec = SyncEnd(data)
      }

      while(positions.zipWithIndex.exists { case (pos, idx) => pos < lines(idx).size }) {
        for(i <- lines.indices) {
          if(positions(i) < lines(i).size) {
            lines(i)(positions(i)).collectFirst {
              case EStart(data) => SyncStart(data)
              case EEnd(data) => SyncEnd(data)
            } match {
              case None =>
                state(i) = Print // no nodes -> always print
              case Some(syncSpec) =>
                // are we syncing our first node?
                // if it's already printed anyway it doesn't matter.
                val leftSyncNode = lsynced.get(syncSpec.data).flatMap(lData => if (printed.contains(syncSpec.copy(lData))) None else Some(syncSpec.copy(lData)))
                val rightSyncNode = rsynced.get(syncSpec.data).flatMap(rData => if (printed.contains(syncSpec.copy(rData))) None else Some(syncSpec.copy(rData)))

                def syncs(elems: Seq[Elem], sync: SyncSpec): Boolean =
                  elems.collectFirst {
                    case EStart(data) if sync == SyncStart(data) => ()
                    case EEnd(data) if sync == SyncEnd(data) => ()
                  }.isDefined

                state(i) = (leftSyncNode, rightSyncNode) match {
                  case (None, None) => Print // no nodes to sync
                  // if the next line to be printed on our left/right doesn't contain our synced node, we cannot print.
                  case (Some(syncNode), _) if !syncs(lines(i - 1)(positions(i - 1)), syncNode) => DoNotPrint
                  case (_, Some(syncNode)) if !syncs(lines(i + 1)(positions(i + 1)), syncNode) => DoNotPrint
                  // if they are contained we just have to depend on them being printed
                  case (Some(_), Some(_)) => DependLeftRight
                  case (Some(_), None) => DependLeft
                  case (None, Some(_)) => DependRight
                }
            }
          } else {
            state(i) = DoNotPrint
          }
        }

        var changed = true
        while(changed) {
          changed = false

          for(i <- state.indices) {
            state(i) match {
              case DependLeft | DependLeftRight if state(i-1) == DoNotPrint =>
                state(i) = DoNotPrint
                changed = true
              case DependRight | DependLeftRight if state(i+1) == DoNotPrint =>
                state(i) = DoNotPrint
                changed = true
              case _ =>
            }
          }
        }

        if(state.forall(_ == DoNotPrint))
          ???

        var lineOut = ""

        for((decision, i) <- state.zipWithIndex) {
          if(decision == DoNotPrint) lineOut += " ".repeat(62)
          else {
            var lineText = ""

            for (elem <- lines(i)(positions(i))) {
              elem match {
                case EText(text) => lineText += text
                case EStart(data) => printed += SyncStart(data)
                case EEnd(data) => printed += SyncEnd(data)
                case ELine(_) => ???
              }
            }

            if(lineText.length > 60)
              lineText = ".." + lineText.takeRight(58)
            else if(lineText.length < 60)
              lineText = lineText + " ".repeat(60 - lineText.length)

            lineOut += lineText + "  "

            positions(i) += 1
          }
        }

        out.write(lineOut + "\n")
      }
    })
  }
}
