// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.stmt.composite;

import hre.ast.MessageOrigin;
import hre.lang.HREError;
import hre.util.ScalaHelper;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.decl.GPUOpt;
import vct.col.ast.util.ASTMapping;
import vct.col.ast.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.BeforeAfterAnnotations;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.util.ASTVisitor;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.util.ASTUtils;

import static hre.lang.System.Debug;

public class LoopStatement extends ASTNode implements BeforeAfterAnnotations {

  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    return map.map(this,arg);
  }

  private ContractBuilder cb=new ContractBuilder();
  private GPUOpt gpuopt;
  private Contract contract;
  private ASTNode body;
  private ASTNode entry_guard;
  private ASTNode exit_guard;
  private ASTNode init_block;
  private ASTNode update_block;

  public void fixate(){
    if (contract!=null && cb==null) return;
    if (cb==null){
      throw new HREError("loop contract has already been fixated");
    }
    contract=cb.getContract(false);
  }
  
  public void setContract(Contract contract){
    if (cb==null){
      throw new HREError("loop contract has already been fixated");
    }
    cb=null;
    this.contract=contract;
  }

  public void setGpuopt(GPUOpt gpuopt){
    this.gpuopt = gpuopt;
  }

  public GPUOpt getGpuopt(){
    return this.gpuopt;
  }

  public Contract getContract(){
    if (contract==null){
      throw new HREError("contract has not been fixated");
    }
    return contract;
  }

  public void setInitBlock(ASTNode init_block){
    this.init_block=init_block;
  }
  public ASTNode getInitBlock(){
    return init_block;
  }

  public void setUpdateBlock(ASTNode update_block){
    this.update_block=update_block;
  }
  public ASTNode getUpdateBlock(){
    return update_block;
  }

  public void setBody(ASTNode body){
    this.body=body;
  }
  public ASTNode getBody() {
    return this.body;
  }

  public void setEntryGuard(ASTNode entry_guard){
    this.entry_guard=entry_guard;
  }
  public ASTNode getEntryGuard(){
    return entry_guard;
  }
  
  public void setExitGuard(ASTNode exit_guard){
    this.exit_guard=exit_guard;
  }
  public ASTNode getExitGuard(){
    return exit_guard;
  }

  
  @Override
  public <T> void accept_simple(ASTVisitor<T> visitor){
    try {
      visitor.visit(this);
    } catch (Throwable t){
      if (thrown.get()!=t){
        Debug("Triggered by %s:",getOrigin());
        thrown.set(t);
     }
      throw t;
    }
  }
  
  @Override
  public <T> T accept_simple(ASTMapping<T> map){
    try {
      return map.map(this);
    } catch (Throwable t){
      if (thrown.get()!=t){
        Debug("Triggered by %s:",getOrigin());
        thrown.set(t);
     }
      throw t;
    }
  }
 

  public void prependInvariant(ASTNode inv){
    cb.prependInvariant(inv);
  }
  
  public void appendInvariant(ASTNode inv){
    cb.appendInvariant(inv);
  }
  
  public Iterable<ASTNode> getInvariants(){
    return ASTUtils.conjuncts(contract.invariant, StandardOperator.Star);
  }

  /** Block of proof hints to be executed just before
   *  evaluating the expression represented by this AST node.
   *  But after any argument has been evaluated.
   */
  private BlockStatement before;
  /** Block of proof hints to be executed after the
   *  current node has been evaluated.
   */
  private BlockStatement after;
  public LoopStatement set_before(BlockStatement block){
    before=block;
    return this;
  }
  public BlockStatement get_before(){
    if (before==null) {
      before=new BlockStatement();
      before.setOrigin(new MessageOrigin("before block"));
    }
    return before;
  }
  public LoopStatement set_after(BlockStatement block){
    after=block;
    return this;
  }
  public BlockStatement get_after(){
    if (after==null) {
      after=new BlockStatement();
      after.setOrigin(new MessageOrigin("after block"));
    }
    return after;
  }

  public void appendContract(Contract contract) {
    cb.appendInvariant(contract.invariant);
    cb.requires(contract.pre_condition);
    cb.ensures(contract.post_condition);
  }

  @Override
  public scala.collection.Iterable<String> debugTreeChildrenFields() {
    return ScalaHelper.toIterable("contract", "init_block", "entry_guard", "body", "exit_guard", "update_block");
  }

  @Override
  public scala.collection.Iterable<String> debugTreePropertyFields() {
    return ScalaHelper.toIterable();
  }
}

