package vct.col.rewrite

import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{MethodInvokation, NameExpression}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, TryCatchBlock}
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.ReturnStatement
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.veymont.Util
import vct.col.veymont.Util.{getBlockOrThrow, isParClassName, javaForkMethodName, javaNotifyAllMethodName, javaNotifyMethodName, javaRunMethodName, javaThreadInvoke, javaWaitMethodName, localMainClassName, parClassName}

import scala.jdk.CollectionConverters._

class UndoJavaParallelEncoding(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  val parClasses: Iterable[ASTClass] = source.get().asScala.collect {
    case c: ASTClass if Util.isParClassName(c.name) => c
  }

  override def visit(c : ASTClass) : Unit = {
    if(!isParClassName(c.name)) {
     super.visit(c)
   }
  }

  override def visit(b: BlockStatement): Unit = {
    var blockStats = List.empty[ASTNode]
    var parRegionMethods = List.empty[Method]
    var inRegion = false
    b.getStatements.foreach(stat => {
      getIfParInvokation(stat) match {
        case Some(parCompMethod) => parRegionMethods = parRegionMethods :+ parCompMethod
        case None => {
          if (inRegion) { //end of region detected
            val parBlocks = parRegionMethods.map(comp => create.parallel_block(null,copy_rw.rewrite(comp.getContract),Array.empty,
              copy_rw.rewrite(getBlockOrThrow(comp.getBody,"Fail: BlockStatement expected in method " + comp.name + " at "+ stat.getOrigin))))
            blockStats = blockStats :+ create.region(ContractBuilder.emptyContract(),parBlocks:_*)
          }
          inRegion = false
          parRegionMethods = List.empty
          blockStats = blockStats :+ rewrite(stat)
        }
      }
    })
    result = create.block(blockStats:_*)
  }

  private def getIfParInvokation(node: ASTNode): Option[Method] = node match {
    case m: MethodInvokation => {
      if (m.method == javaThreadInvoke)
        m.`object` match {
          case p: MethodInvokation =>
            if (p.method == Method.JavaConstructor && isParClassName(p.dispatch.getName)) {
              parClasses.find(_.name == p.dispatch.getName) match {
                case Some(parClass) => parClass.methods().asScala.find(_.name == javaRunMethodName)
                case None => None
              }
            } else None
          case _ => None
        }
      else None
    }
    case _ => None
  }

  override def visit(m : MethodInvokation) : Unit = {
    if(m.method == javaForkMethodName) {
      result = create.special(ASTSpecial.Kind.Fork,m.`object`)
    } else if(m.method == javaWaitMethodName) {
      result = create.special(ASTSpecial.Kind.Wait,create.reserved_name(ASTReserved.This))
    } else if(m.method == javaNotifyAllMethodName) {
      result = create.special(ASTSpecial.Kind.NotifyAll)
    } else if(m.method == javaNotifyMethodName) {
      result = create.special(ASTSpecial.Kind.Notify)
    } else super.visit(m)
  }

  override def visit(m : Method) : Unit = {
    if(m.isSynchronized) {
      if(m.getParent != null && (m.getParent match {
        case c : ASTClass => c.methods().asScala.exists(_.name == Method.LockInvariant)
        case _ => false
      })) {
        val body = getBlockOrThrow(m.getBody,"Fail: BlockStatement expected in synchronized method " + m.name)
        val lockthis = create.special(ASTSpecial.Kind.Lock,create.reserved_name(ASTReserved.This))
        val unlockthis = create.special(ASTSpecial.Kind.Unlock,create.reserved_name(ASTReserved.This))
        val rewStats = body.getStatements.map(rewrite(_))
        val newBody = if(rewStats.last.isInstanceOf[ReturnStatement])
          lockthis +: rewStats.dropRight(1) :+ unlockthis :+ rewStats.last
        else lockthis +: rewStats :+ unlockthis
        result = create.method_kind(m.kind,m.getReturnType,m.getContract,m.name,m.getArgs,create.block(newBody:_*))
      }
      else super.visit(m)
    } else super.visit(m)
  }

  override def visit(tc : TryCatchBlock) : Unit = {
    result = rewrite(tc.main)
  }

}
