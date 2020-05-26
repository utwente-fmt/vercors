package vct.col.ast.stmt.composite;

import hre.ast.MessageOrigin;
import scala.collection.Iterable;
import scala.collection.JavaConverters;
import vct.col.ast.util.ASTMapping;
import vct.col.ast.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.BeforeAfterAnnotations;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.util.ASTVisitor;

import java.util.Arrays;
import java.util.Collections;

import static hre.lang.System.Debug;

public class ForEachLoop extends ASTNode implements BeforeAfterAnnotations {

  public final DeclarationStatement decls[];
  public final ASTNode guard;
  public final ASTNode body;
  
  public ForEachLoop(DeclarationStatement decls[],ASTNode guard,ASTNode body){
    this.decls=decls;
    this.guard=guard;
    this.body=body;
  }
  
  private Contract contract;
  
  public Contract getContract(){
    return contract;
  }
  
  public ForEachLoop setContract(Contract contract){
    this.contract=contract;
    return this;
  }
  
  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    return map.map(this,arg);
  }

  @Override
  public Iterable<String> debugTreeChildrenFields() {
    return JavaConverters.iterableAsScalaIterable(Arrays.asList("decls", "guard", "body"));
  }

  @Override
  public Iterable<String> debugTreePropertyFields() {
    return JavaConverters.iterableAsScalaIterable(Collections.emptyList());
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
 
  /** Block of proof hints to be executed just before
   *  evaluating the expression represented by this AST node.
   *  But after any argument has been evaluated.
   */
  private BlockStatement before;
  /** Block of proof hints to be executed after the
   *  current node has been evaluated.
   */
  private BlockStatement after;
  public ForEachLoop set_before(BlockStatement block){
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
  public ForEachLoop set_after(BlockStatement block){
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

}
