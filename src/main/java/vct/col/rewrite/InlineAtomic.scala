package vct.col.rewrite

import scala.collection.JavaConverters._
import vct.col.ast.`type`.ClassType
import vct.col.ast.expr.{NameExpression, NameExpressionKind}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, ParallelAtomic, ParallelInvariant}
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, ProgramUnit}
import vct.col.ast.util.AbstractRewriter
import vct.logging.ErrorMapping
import vct.logging.VerCorsError.ErrorCode

import scala.collection.mutable

class InlineAtomic(arg: ProgramUnit, map: ErrorMapping) extends AbstractRewriter(arg) {

  val LEAVE_ATOMIC = "leave_atomic"
  map.add(LEAVE_ATOMIC, ErrorCode.ExhaleFailed, ErrorCode.InvariantBroken)

  // Stack is not deprecated anymore in 2.13
  val invBlocks: mutable.Stack[ParallelInvariant] = mutable.Stack()

  /**
    * atomic(a, b, c) {S}
    * ==> inhale ?; S; exhale ?
    *
    * if a is one of the invariant labels on the stack, do inhale invariant / exhale invariant
    * otherwise, find a predicate ending with csl_invariant in the class denoted by the type of a.
    * then do inhale pred; unfold pred // fold pred; exhale pred
    * arguments from the predicate presumably must be in local scope, perhaps just for this?
    */
  override def visit(pa: ParallelAtomic): Unit = {
    val block: BlockStatement =
      rewrite(pa.block) match {
        case statement: BlockStatement =>
          statement
        case atomicStat =>
          create.block(atomicStat)
      }

    for (node <- pa.synclist) {
      node match {
        case name: NameExpression if name.getKind == NameExpressionKind.Label =>
          invBlocks.find(_.label == name.getName) match {
            case Some(invBlock) =>
              block.prepend(create.special(ASTSpecial.Kind.Inhale, invBlock.inv))
              block.append(create.special(ASTSpecial.Kind.Exhale, invBlock.inv).set_branch(LEAVE_ATOMIC))
            case None =>
              Fail("Could not find an invariant labeled %s", name)
          }
        case _ =>
          val ct: ClassType = node.getType.asInstanceOf[ClassType]
          val cl: ASTClass = source.find(ct)

          val (name, args: Seq[ASTNode]) = cl.dynamicMethods()
            .asScala
            .find(m => m.name.endsWith("csl_invariant")) match {
              case Some(m) => (m.name, m.getArgs.toSeq.map(a => create.local_name(a.name)))
              case None => ("csl_invariant", Seq())
            }

          block.prepend(create.special(ASTSpecial.Kind.Unfold, create.invokation(rewrite(node), null, name, args:_*)))
          block.prepend(create.special(ASTSpecial.Kind.Inhale, create.invokation(rewrite(node), null, name, args:_*)))
          block.append(create.special(ASTSpecial.Kind.Fold, create.invokation(rewrite(node), null, name, args:_*)))
          block.append(create.special(ASTSpecial.Kind.Exhale, create.invokation(rewrite(node), null, name, args:_*)))
      }
    }

    result = block
  }

  override def visit(inv: ParallelInvariant): Unit = {
    invBlocks.push(inv)
    super.visit(inv)
    invBlocks.pop
  }
}
