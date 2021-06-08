package vct.col.veymont

import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{NameExpression, StandardOperator}
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelBlock}
import vct.col.ast.stmt.decl.{ASTClass, Contract, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.veymont.Util.{chanWriteMethodName, getChansFromBlockStateMent, isChanName, isThreadClassName}

class ChannelPerms(override val source : ProgramUnit)  extends AbstractRewriter(null, true) {

  override def visit(m : Method) = {
    m.getParent match {
      case c : ASTClass => {
        if(isThreadClassName(c.name) && m.kind != Method.Kind.Pure && m.kind != Method.Kind.Predicate) {
          val chans : Set[ChannelRepr] = m.getBody match {
            case b : BlockStatement => getChans(b)
            case _ => throw Failure("VeyMont Fail: Body of method %s in class %s is not a BlockStatement",m.name,c.name)
          }
          if(chans.nonEmpty) {
            result = create.method_decl(m.getReturnType, extendContract(chans, m.getContract(), false), m.name, m.getArgs, rewrite(m.getBody))
          } else {
            super.visit(m)
          }
        } else {
          super.visit(m)
        }
      }
      case _ => super.visit(m)
    }
  }

  override def visit(pb : ParallelBlock) = {
    val chans = getChans(pb.block)
    if(chans.nonEmpty) {
      result = create.parallel_block(pb.label,extendContract(chans,rewrite(pb.contract), false), pb.itersJava, rewrite(pb.block),pb.deps)
    } else {
      super.visit(pb)
    }
  }

  override def visit(l : LoopStatement) = {
    val chans = l.getBody match {
      case b : BlockStatement => getChans(b)
      case _ => throw Failure("VeyMont Fail: Body of LoopStatement is not a BlockStatement\n" + l.getBody.getOrigin)
    }
    if(chans.nonEmpty) {
      if(l.getExitGuard != null) {
        Fail("VeyMont Fail: LoopStatement has an exit guard")
      } else {
        result = create.for_loop(l.getInitBlock,l.getEntryGuard,l.getUpdateBlock,rewrite(l.getBody),extendContract(chans,l.getContract,true))
      }
    } else {
      super.visit(l)
    }
  }

  private def extendContract(chans : Set[ChannelRepr], c : Contract, isLoop : Boolean) : Contract = {
    val cb = new ContractBuilder()
    chans.foreach(getChanPerms(_,isLoop,cb))
    super.rewrite(c,cb)
    cb.getContract()
  }

  private def getChans(b : BlockStatement): Set[ChannelRepr] = {
    getChansFromBlockStateMent(b).flatMap(m => m.`object` match {
      case n: NameExpression => if (isChanName(n.name)) Set(new ChannelRepr(n.name, m.method == chanWriteMethodName,null)) else Set.empty[ChannelRepr]
      case _ => Set.empty[ChannelRepr]
    })
  }

  private def getChanPerms(c : ChannelRepr, isLoop : Boolean, contract : ContractBuilder) : ContractBuilder = {
    val chanNotNull =
      create.expression(StandardOperator.NEQ,
        create.field_name(c.channel),
        create.reserved_name(ASTReserved.Null))
    val chanPerm =
      create.expression(StandardOperator.Perm,
        create.field_name(c.channel),
        create.reserved_name(ASTReserved.ReadPerm))
    if(isLoop) {
      contract.appendInvariant(chanPerm)
      contract.appendInvariant(chanNotNull)
    //  contract.appendInvariant(c.getChanFieldPerm(create))
    } else {
      contract.context(chanPerm)
      contract.context(chanNotNull)
    //  contract.context(c.getChanFieldPerm(create))
    }
    contract
  }

}
