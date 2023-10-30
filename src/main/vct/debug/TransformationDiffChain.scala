package vct.debug

import vct.col.ast.{Node, Verification}
import vct.col.origin.Origin
import vct.col.print
import vct.col.print.{Cons, Ctx, Doc, EEnd, ELine, EStart, EText, Elem, Empty, Group, Line, Nest, NodeDoc, NonWsLine, Text}
import vct.main.stages.Transformation
import vct.options.types.PathOrStd

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

  private val zero = NodeDoc(Text("0"))(zeroNode)
  private val one = NodeDoc(Text("1"))(oneNode)
  private val two = NodeDoc(Text("2"))(twoNode)
  private val three = NodeDoc(Text("3"))(threeNode)
  private val four = NodeDoc(Text("4"))(fourNode)

  def main(args: Array[String]): Unit = {
    val chain = new TransformationDiffChain(null)
    println(chain.nodeSeqDistance(Seq(
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

  private val baseCtx = Ctx(width = 60)

  var log: ArrayBuffer[(String, Doc)] = ArrayBuffer()

  def getNodeDocs(doc: Doc): Seq[NodeDoc] =
    doc match {
      case Empty => Nil
      case NonWsLine => Nil
      case Line => Nil
      case Cons(left, right) => getNodeDocs(left) ++ getNodeDocs(right)
      case Text(_) => Nil
      case Nest(doc) => getNodeDocs(doc)
      case Group(doc) => getNodeDocs(doc)
      case node @ NodeDoc(_) => Seq(node)
    }

  sealed trait Action
  case class Delete(left: NodeDoc) extends Action
  case class Insert(right: NodeDoc) extends Action
  case class Substitute(left: NodeDoc, right: NodeDoc) extends Action

  def nodeSeqDistance(left: Seq[NodeDoc], right: Seq[NodeDoc]): (Double, Seq[Action]) = {
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
            val substitutePenalty = (insertCost + deleteCost) * 3.0

            val subDistance = distance(i+1)(j+1)
            val delDistance = distance(i+1)(j)
            val insDistance = distance(i)(j+1)

            val resultSubstitute = docDistance(l.doc, r.doc) * substitutePenalty + subDistance
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

  case class DocRef(doc: Doc) {
    override def hashCode: Int = System.identityHashCode(doc)

    override def equals(obj: Any): Boolean = obj match {
      case DocRef(other) => doc eq other
      case _ => false
    }
  }

  val nodesDistance: mutable.Map[(DocRef, DocRef), (Double, Seq[Action])] = mutable.Map()

  def getNodesDistance(left: Doc, right: Doc): (Double, Seq[Action]) =
    nodesDistance.getOrElseUpdate((DocRef(left), DocRef(right)), {
      val leftDocs = getNodeDocs(left)
      val rightDocs = getNodeDocs(right)
      if(leftDocs.isEmpty || rightDocs.isEmpty)
        (if(left == right) 0.0 else 1.0, Nil)
      else
        nodeSeqDistance(leftDocs, rightDocs)
    })

  def docDistance(left: Doc, right: Doc): Double =
    getNodesDistance(left, right)._1

  def flatNodeSynchronizations(left: Doc, right: Doc): Seq[(Node[_], Node[_])] =
    getNodesDistance(left, right)._2.flatMap {
      case Delete(_) => Nil
      case Insert(_) => Nil
      case Substitute(left, right) =>
        (left.node, right.node) +: flatNodeSynchronizations(left.doc, right.doc)
    }

  override def accept(afterPass: String, verification: Verification[_]): Unit = {
    implicit val ctx: Ctx = baseCtx.namesIn(verification)
    log += ((afterPass, verification.show))
  }

  case class NodeRef(node: Node[_]) {
    override def hashCode(): Int = System.identityHashCode(node)
    override def equals(obj: Any): Boolean = obj match {
      case NodeRef(otherNode) => node eq otherNode
      case _ => false
    }
  }

  override def finish(): Unit = {
    val rsynced = mutable.Map[NodeRef, NodeRef]()
    val lsynced = mutable.Map[NodeRef, NodeRef]()

    for(((_, left), (_, right)) <- log.zip(log.tail)) {
      for((leftNode, rightNode) <- flatNodeSynchronizations(left, right)) {
        rsynced += NodeRef(leftNode) -> NodeRef(rightNode)
        lsynced += NodeRef(rightNode) -> NodeRef(leftNode)
      }
    }

    val lines = log.map(_._2.lines(baseCtx).map(_.toIndexedSeq).toIndexedSeq)
    val positions = Array.fill(log.size)(0)
    val printed = mutable.Set[NodeRef]()

    println(log.map(_._1.padTo(62, ' ')).mkString(""))

    // Should we print this line?
    sealed trait State
    case object Print extends State // yes
    case object DoNotPrint extends State // no
    case object DependLeft extends State // only if my left prints
    case object DependRight extends State // only if my right prints
    case object DependLeftRight extends State // only if my left *and* right print
    val state = Array.fill[State](log.size)(DoNotPrint)

    while(positions.zipWithIndex.exists { case (pos, idx) => pos < lines(idx).size }) {
      for(i <- lines.indices) {
        if(positions(i) < lines(i).size) {
          lines(i)(positions(i)).collectFirst {
            case EStart(node) => NodeRef(node)
            case EEnd(node) => NodeRef(node)
          } match {
            case None =>
              state(i) = Print // no nodes -> always print
            case Some(node) =>
              // are we syncing our first node?
              // if it's already printed anyway it doesn't matter.
              val leftSyncNode = lsynced.get(node).flatMap(node => if (printed.contains(node)) None else Some(node))
              val rightSyncNode = rsynced.get(node).flatMap(node => if (printed.contains(node)) None else Some(node))

              def containsNode(elems: Seq[Elem], node: NodeRef): Boolean =
                elems.collectFirst {
                  case EStart(n) if NodeRef(n) == node => ()
                  case EEnd(n) if NodeRef(n) == node => ()
                }.isDefined

              state(i) = (leftSyncNode, rightSyncNode) match {
                case (None, None) => Print // no nodes to sync
                // if the next line to be printed on our left/right doesn't contain our synced node, we cannot print.
                case (Some(syncNode), _) if !containsNode(lines(i - 1)(positions(i - 1)), syncNode) => DoNotPrint
                case (_, Some(syncNode)) if !containsNode(lines(i + 1)(positions(i + 1)), syncNode) => DoNotPrint
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

      var out = ""

      for((decision, i) <- state.zipWithIndex) {
        if(decision == DoNotPrint) out += " ".repeat(62)
        else {
          var lineText = ""

          for (elem <- lines(i)(positions(i))) {
            elem match {
              case EText(text) => lineText += text
              case EStart(node) => printed += NodeRef(node)
              case EEnd(node) => printed += NodeRef(node)
              case ELine(_) => ???
            }
          }

          if(lineText.length > 60)
            lineText = ".." + lineText.takeRight(58)
          else if(lineText.length < 60)
            lineText = lineText + " ".repeat(60 - lineText.length)

          out += lineText + "  "

          positions(i) += 1
        }
      }

      println(out)
    }
  }
}
