package vct.col.rewrite;

import java.util.ArrayList;
import java.util.Stack;

import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;

public class PropagateInvariants extends AbstractRewriter {

  public PropagateInvariants(ProgramUnit source) {
    super(source);
  }
  
  private Stack<ASTNode> invariants=new Stack<ASTNode>();
  
  @Override
  public void visit(Method m){
    Contract c=m.getContract();
    if (c!=null){
      invariants.push(c.invariant);
    }
    super.visit(m);
    if(c!=null){
      invariants.pop();
    }
  }
  
  @Override
  public void visit(LoopStatement l){
    super.visit(l);
    LoopStatement res=(LoopStatement)result;
    if (l.getContract()!=null && !l.getContract().isEmpty()){
      for(ASTNode inv:invariants){
        res.prependInvariant(inv);
      }
    }
    result=res;
  }
  
  
  @Override
  public void visit(ParallelRegion region){
    if (region.contract() !=null && !region.contract().isEmpty()) {
      ContractBuilder cb = new ContractBuilder();
      for(ASTNode inv:invariants) { cb.prependInvariant(inv); }
      rewrite(region.contract(), cb);
      invariants.push(region.contract().invariant);
      ParallelBlock blocks[] = rewrite(region.blocksJava()).toArray(new ParallelBlock[0]);
      invariants.pop();
      result=create.region(cb.getContract(),blocks);
    } else {
      super.visit(region);
    }
  }
  
  @Override
  public void visit(ParallelBarrier pb){
    ContractBuilder cb=new ContractBuilder();
    for (ASTNode inv:invariants) { cb.prependInvariant(inv); }
    rewrite(pb.contract(), cb);
    result=create.barrier(pb.label(), cb.getContract(), pb.invs(), rewrite(pb.body()));
  }
  
  @Override
  public void visit(ParallelInvariant inv){
    ArrayList <ASTNode> invs = new ArrayList<ASTNode>();
    for (ASTNode n : invariants) { invs.add(rewrite(n)); }
    invs.add(rewrite(inv.inv()));
    result = create.invariant_block(inv.label(), create.fold(StandardOperator.Star,invs), rewrite(inv.block()));
  }
  @Override
  public void visit(ParallelBlock pb){
    ContractBuilder cb=new ContractBuilder();
    for(ASTNode inv:invariants) { cb.prependInvariant(inv); }
    rewrite(pb.contract(), cb);
    ParallelBlock res=create.parallel_block(
        pb.label(),
        // Make sure the contract remains null if the original contract is null,
        // and non-null if the original contract is
        cb.getContract(pb.contract() == null),
        rewrite(pb.itersJava()),
        rewrite(pb.block()),
        rewrite(pb.deps())
    );
    result=res;
  }
}
