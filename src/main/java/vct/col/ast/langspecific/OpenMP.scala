package vct.col.ast.langspecific

import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl.Contract
import vct.col.ast.util.ASTVisitor
import vct.col.util.{ASTMapping, ASTMapping1}

sealed trait OMPOption
case class OMPPrivate(names: Seq[String]) extends OMPOption
case class OMPShared(names: Seq[String]) extends OMPOption
case object OMPNoWait extends OMPOption
case class OMPSimdLen(len: Int) extends OMPOption
case class OMPNumThreads(len: Int) extends OMPOption
case class OMPSchedule(schedule: OMPScheduleChoice) extends OMPOption

sealed trait OMPScheduleChoice
case object OMPStatic extends OMPScheduleChoice

sealed abstract class OMPBlock(block: BlockStatement) extends ASTNode {
  override def accept_simple[T](visitor: ASTVisitor[T]): Unit = this match {
    case me: OMPParallel => visitor.visit(me)
    case me: OMPSection => visitor.visit(me)
    case me: OMPSections => visitor.visit(me)
  }
  override def accept_simple[T](map: ASTMapping[T]): T = this match {
    case me: OMPParallel => map.map(me)
    case me: OMPSection => map.map(me)
    case me: OMPSections => map.map(me)
  }
  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A): R = this match {
    case me: OMPParallel => map.map(me, arg)
    case me: OMPSection => map.map(me, arg)
    case me: OMPSections => map.map(me, arg)
  }

  override def debugTreeChildrenFields: Iterable[String] = Seq("block")
  override def debugTreePropertyFields: Iterable[String] = Seq()
}

case class OMPParallel(block: BlockStatement, options: Seq[OMPOption], contract: Contract) extends OMPBlock(block)
case class OMPSection(block: BlockStatement) extends OMPBlock(block)
case class OMPSections(block: BlockStatement) extends OMPBlock(block)

sealed abstract class OMPLoop(val loop: LoopStatement, val options: Seq[OMPOption]) extends ASTNode {
  override def accept_simple[T](visitor: ASTVisitor[T]): Unit = this match {
    case me: OMPFor => visitor.visit(me)
    case me: OMPParallelFor => visitor.visit(me)
    case me: OMPForSimd => visitor.visit(me)
  }
  override def accept_simple[T](map: ASTMapping[T]): T = this match {
    case me: OMPFor => map.map(me)
    case me: OMPParallelFor => map.map(me)
    case me: OMPForSimd => map.map(me)
  }
  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A): R = this match {
    case me: OMPFor => map.map(me, arg)
    case me: OMPParallelFor => map.map(me, arg)
    case me: OMPForSimd => map.map(me, arg)
  }

  override def debugTreeChildrenFields: Iterable[String] = Seq("loop")
  override def debugTreePropertyFields: Iterable[String] = Seq()
}

case class OMPFor(override val loop: LoopStatement, override val options: Seq[OMPOption]) extends OMPLoop(loop, options)
case class OMPParallelFor(override val loop: LoopStatement, override val options: Seq[OMPOption]) extends OMPLoop(loop, options)
case class OMPForSimd(override val loop: LoopStatement, override val options: Seq[OMPOption]) extends OMPLoop(loop, options)