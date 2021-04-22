package vct.col.rewrite

import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.{ASTSpecial, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

import scala.jdk.CollectionConverters._

case class ActionHeaderToBlock(override val source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(block: BlockStatement): Unit =
    block.asScala.toSeq match {
      case Seq(s: ASTSpecial, tail@_*) if s.kind == ASTSpecial.Kind.ActionHeader =>
        val history = rewrite(s.args(0))
        val fraction = rewrite(s.args(1))
        val process = rewrite(s.args(2))
        val action = rewrite(s.args(3))
        val map = s.args.toSeq.drop(4).grouped(2).map {
          case Seq(k, v) =>
            k.toString -> rewrite(v)
        }.toMap
        val block = create.block(tail.map(rewrite(_)):_*)
        result = create action_block(history, fraction, process, action, map.asJava, block)
      case _ =>
        super.visit(block)
    }

  override def visit(spec: ASTSpecial): Unit = {
    if(spec.isSpecial(ASTSpecial.Kind.ActionHeader)) {
      spec.getOrigin.report("error", "Action blocks denoted with a header may only have the header at the start of a block")
      throw new Error()
    } else {
      super.visit(spec)
    }
  }
}
