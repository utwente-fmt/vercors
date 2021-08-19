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
    if(m.getContract() == null) {
      super.visit(m);
    } else {
      Contract c = m.getContract();
      ContractBuilder builder = new ContractBuilder();
      if(c.given != null) builder.given(rewrite(c.given));
      if(c.yields != null) builder.yields(rewrite(c.yields));
      if(c.modifies != null) builder.modifies(rewrite(c.modifies));
      if(c.accesses != null) builder.accesses(rewrite(c.accesses));
      builder.context(rewrite(c.invariant));
      builder.requires(rewrite(c.pre_condition));
      builder.ensures(rewrite(c.post_condition));
      builder.signals(rewrite(c.signals));

      invariants.push(c.invariant);

      result = create.method_kind(
              m.kind,
              rewrite(m.getReturnType()),
              rewrite(m.signals),
              builder.getContract(),
              m.name(),
              rewrite(m.getArgs()),
              m.usesVarArgs(),
              rewrite(m.getBody())
      );

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
      result=create.region(null, cb.getContract(),blocks);
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
