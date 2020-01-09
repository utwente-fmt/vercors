package vct.antlr4.parser;


import hre.ast.FileOrigin;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.util.ContractBuilder;
import vct.col.rewrite.AbstractRewriter;
import vct.col.syntax.Syntax;

import static hre.lang.System.Output;

/**
 * Rewrite an AST with specifications in the form of comments
 * to an AST with specifications in the from of ASTs. 
 */
public class SpecificationCollector extends AbstractRewriter {

  /**
   * Keep track of the region in which contract parts (requires, ensures, etc.) were encountered
   */
  private FileOrigin contractStart, previousContractOrigin;

  private Syntax syntax;
  
  public SpecificationCollector(Syntax syntax,ProgramUnit source) {
    super(source);
    this.syntax=syntax;
    currentContractBuilder=new ContractBuilder();
  }
  
  public SpecificationCollector(ProgramUnit source) {
    super(source);
    this.syntax=new Syntax("EmptySyntax");
    currentContractBuilder=new ContractBuilder();
  }
  
  @Override
  public void visit(ASTSpecial s){
    switch(s.kind){
    case Given:
    case Yields:
    case Requires:
    case Ensures:
    case RequiresAndEnsures:
    case Invariant:
    case Modifies:
    case Accessible:
      if (currentContractBuilder != null) break;
    default:
      super.visit(s);
      return;
    }

    // If we have not been recording the contract region, mark the start
    if (contractStart == null) {
      contractStart = (FileOrigin) s.getOrigin();
    }
    // Update the previously seen origin, so we can see where the last place was
    // where we saw a contract part
    previousContractOrigin = (FileOrigin) s.getOrigin();

    switch(s.kind){
    case Modifies:{
      currentContractBuilder.modifies(rewrite(s.args));
      break;
    }
    case Accessible:{
      currentContractBuilder.accesses(rewrite(s.args));
      break;
    }
    case Given:
    {
      ASTNode input[];
      if (s.args.length==1 && s.args[0] instanceof BlockStatement){
        BlockStatement block=(BlockStatement)s.args[0];
        input=block.getStatements();
      } else {
        input=s.args;
      }
      for(int i=0;i<input.length;i++){
        currentContractBuilder.given((DeclarationStatement)rewrite(input[i]));
      }
      break;
    }
    case Yields:
    {
      ASTNode input[];
      if (s.args.length==1 && s.args[0] instanceof BlockStatement){
        BlockStatement block=(BlockStatement)s.args[0];
        input=block.getStatements();
      } else {
        input=s.args;
      }
      for(int i=0;i<input.length;i++){
        currentContractBuilder.yields((DeclarationStatement)rewrite(input[i]));
      }
      break;
    }
    case Requires:
      currentContractBuilder.requires(rewrite(s.args[0]));
      break;
    case Ensures:
      currentContractBuilder.ensures(rewrite(s.args[0]));
      break;
    case RequiresAndEnsures:
      currentContractBuilder.requires(rewrite(s.args[0]));
      currentContractBuilder.ensures(rewrite(s.args[0]));
      break;
    case Invariant:
      currentContractBuilder.appendInvariant(rewrite(s.args[0]));
      break;
    default:
      Abort("Missing case %s",s.kind);
    }
  }

  @Override
  public void visit(ASTClass c){

    String name=c.getName();
    if (name==null) {
      Abort("illegal class without name");
    } else {
      Debug("rewriting class "+name);
      ASTClass res=new ASTClass(name,c.kind,rewrite(c.parameters),rewrite(c.super_classes),rewrite(c.implemented_classes));
      res.setOrigin(c.getOrigin());
      currentTargetClass=res;
      Contract contract=c.getContract();
      if (currentContractBuilder==null){
        currentContractBuilder=new ContractBuilder();
      }
      if (contract!=null){
        rewrite(contract,currentContractBuilder);
      }
      res.setContract(currentContractBuilder.getContract());
      currentContractBuilder=new ContractBuilder();
      for(ASTNode item:c){
        res.add(rewrite(item));
      }
      result=res;
      currentTargetClass=null;
    }

    if (currentContractBuilder!=null && !currentContractBuilder.isEmpty()){
      Abort("class contains unattached contract clauses");
    }
  }

  @Override
  public void visit(Method m){
    super.visit(m);

    // Whenever a contract has no origin but we've been gathering contract components, add the recorded
    // region of contract components to the contract
    Contract newContract = ((Method) result).getContract();
    if (newContract != null && newContract.getOrigin() == null && contractStart != null) {
      newContract.setOrigin(contractStart.merge(previousContractOrigin));
      contractStart = null;
      previousContractOrigin = null;
    } else {
      // Otherwise, erase the previously recorded region. Either the contract already had an origin (which
      // means we should discard the recorded origin), or there wasn't a contract at all, which means the
      // recorded should not be used if present. Or there wasn't a recorded region, in which case this doesn't
      // affect anything.
      contractStart = null;
      previousContractOrigin = null;
    }

    currentContractBuilder=new ContractBuilder();
  }
  @Override
  public void visit(LoopStatement s){
    BlockStatement new_before=create.block();
    BlockStatement new_after=create.block();
    BlockStatement old_before=s.get_before();
    BlockStatement old_after=s.get_after();
    
    LoopStatement res=new LoopStatement();
    ASTNode tmp;
    tmp=s.getInitBlock();
    if (tmp!=null) res.setInitBlock(tmp.apply(this));
    tmp=s.getUpdateBlock();
    if (tmp!=null) res.setUpdateBlock(tmp.apply(this));
    tmp=s.getEntryGuard();
    if (tmp!=null) res.setEntryGuard(tmp.apply(this));
    tmp=s.getExitGuard();
    if (tmp!=null) res.setExitGuard(tmp.apply(this));
    if (currentContractBuilder==null) currentContractBuilder=new ContractBuilder();
    rewrite(s.getContract(),currentContractBuilder);
    filter_with_then(new_before, new_after, new_before, old_before);
    filter_with_then(new_before, new_after, new_after, old_after);
    res.setContract(currentContractBuilder.getContract(false));
    currentContractBuilder=null;
    res.set_before(new_before);
    res.set_after(new_after);
    res.setBody(rewrite(s.getBody()));
    res.setOrigin(s);
    result=res;
  }

  private void filter_with_then(BlockStatement new_before,
      BlockStatement new_after, BlockStatement new_current, BlockStatement old_current) {
    for(ASTNode n:old_current){
      if (n instanceof ASTSpecial){
        ASTSpecial sp=(ASTSpecial)n; 
        switch(sp.kind){
        case Requires:{
          currentContractBuilder.requires(rewrite(sp.args[0]));
          continue;
        }
        case Ensures:{
          currentContractBuilder.ensures(rewrite(sp.args[0]));
          continue;
        }
        default:
          break;
        }
      } else if (n instanceof ASTSpecial){
        ASTSpecial sp=(ASTSpecial)n; 
        switch(sp.kind){
        case With:{
          for(ASTNode s:(BlockStatement)sp.args[0]){
            new_before.addStatement(rewrite(s));
          }
          continue;
        }
        case Then:{
          for(ASTNode s:(BlockStatement)sp.args[0]){
            new_after.addStatement(rewrite(s));
          }
          continue;
        }
        default:
          break;
        }
      }
      new_current.addStatement(rewrite(n));
    }
  }
  
  @Override
  public void visit(BlockStatement block){
    BlockStatement tmp=currentBlock;
    currentBlock=create.block();
    
    int N=block.getLength();
    for(int i=0;i<N;i++){
      if (block.get(i) instanceof ASTSpecial && currentContractBuilder==null){
        int j;
        for(j=i+1;j<N && (block.get(j) instanceof ASTSpecial);j++){
          ASTSpecial S=(ASTSpecial)block.get(j);
          switch(S.kind){
          case Requires:
          case Invariant:
          case Ensures:
          case Comment:
          case RequiresAndEnsures:
            continue;
          default:
            break;
          }
          break;
        }
        if (j<N && block.get(j) instanceof LoopStatement) {
          currentContractBuilder=new ContractBuilder();
        } else {
          j--;
          for(;i<j;i++){
            currentBlock.add(rewrite(block.get(i)));
          }
        }
      }
      currentBlock.add(rewrite(block.get(i)));
      //if (block.get(i) instanceof LoopStatement){
      //  
      //}
    }
    
    if (currentBlock.size()==0 && block.getParent()!=null){
      result=null;
    } else {
      result=currentBlock;
    }
    currentBlock=tmp;
  }
  
  @Override
  public void visit(MethodInvokation m){
    StandardOperator op=syntax.parseFunction(m.method);
    if (op!=null && op.arity()==m.getArity()){
      result=create.expression(op,rewrite(m.getArgs()));
    } else {
      super.visit(m);
    }
  }
  
  @Override
  public void visit(DeclarationStatement d){
    /* We correct for the fact that for parsers the following two
     * patterns are indistinguishable in many cases:
      
      special argument ;
      type var ;
      
      TODO
      
      special a1 , a2 ;
      type v1 , v2 ;
      
     */
    Kind kind=syntax.get_annotation(d.getType().toString(),1);
    if (kind==null){
      super.visit(d);
    } else {
      Warning("fixing special %s",kind);
      result = create.special(kind, create.unresolved_name(d.name()));
    }
  }
}

