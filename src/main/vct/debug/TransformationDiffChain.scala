package vct.debug

import vct.col.ast.Verification
import vct.col.print
import vct.col.print.{Cons, Ctx, Doc, Empty, Group, Line, Nest, NodeDoc, NonWsLine, Text}
import vct.main.stages.Transformation
import vct.options.types.PathOrStd

import scala.collection.mutable.ArrayBuffer

object TransformationDiffChain {

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
  case object Delete extends Action
  case object Insert extends Action
  case object Substitute extends Action

  // 0 <= result <= max(left.size, right.size)
  def editDistance(left: Seq[NodeDoc], right: Seq[NodeDoc]): (Double, Seq[Action]) =
    (left, right) match {
      case Nil -> right => (right.size.toDouble, Seq.fill(right.size)(Insert))
      case left -> Nil => (left.size.toDouble, Seq.fill(left.size)(Delete))
      case (l +: left) -> (r +: right) =>
        val insertCost = 1
        val deleteCost = 1
        val substitutePenalty = (insertCost + deleteCost) * 1.5

        val (subDistance, subPath) = editDistance(left, right)
        val (delDistance, delPath) = editDistance(left, r +: right)
        val (insDistance, insPath) = editDistance(l +: left, right)

        val resultSubstitute = distance(l, r) * substitutePenalty + subDistance
        val resultDelete = deleteCost * delDistance
        val resultInsert = insertCost * insDistance

        val best = Seq(resultSubstitute, resultDelete, resultInsert).min

        val path =
          if (best == resultSubstitute) Substitute +: subPath
          else if(best == resultDelete) Delete +: delPath
          else if(best == resultInsert) Insert +: insPath
          else ???

        (best, path)
    }

  // 0 <= result <= scale
  def distance(left: Doc, right: Doc, scale: Double = 1.0): Double = {
    lazy val leftNodes = getNodeDocs(left)
    lazy val rightNodes = getNodeDocs(right)

    if (scale < 0.01 || leftNodes.isEmpty || rightNodes.isEmpty)
      if(left == right) 0.0 else scale
    else
      editDistance(leftNodes, rightNodes)._1 / Seq(leftNodes.size, rightNodes.size).max * scale
  }

  override def accept(afterPass: String, verification: Verification[_]): Unit = {
    implicit val ctx: Ctx = baseCtx.namesIn(verification)
    log += ((afterPass, verification.show))
  }

  override def finish(): Unit = {
    for((key, doc) <- log) {
      println(key)
      println(doc.toStringWithContext(baseCtx))
    }
  }
}
