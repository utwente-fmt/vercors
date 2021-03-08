package vct.col.rewrite;

import java.util.HashMap;
import java.util.HashSet;

import vct.col.ast.expr.NameExpressionKind;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;

/**
 * This rewriter converts all method argument to final arguments.
 * @author Stefan Blom
 *
 */
public class FinalizeArguments extends AbstractRewriter {

  public FinalizeArguments(ProgramUnit source) {
    super(source);
  }

  private HashSet<String> currentArgumentLocals = new HashSet<>();

  public void visit(Method m) {
    switch(m.kind){
      case Constructor:
      case Plain:
      {
        HashMap<NameExpression,ASTNode> subst=new HashMap<NameExpression, ASTNode>();
        ASTNode body=m.getBody();
        if (body==null){
          result=m.apply(copy_rw);
          return;
        }
        String name=m.getName();
        int N=m.getArity();
        DeclarationStatement old_decls[]=m.getArgs();
        DeclarationStatement args[]=new DeclarationStatement[N];
        BlockStatement block=new BlockStatement();
        block.setOrigin(body);
        create.enter();
        for(int i=0;i<N;i++){
          String old_name=m.getArgument(i);
          String new_name="__"+m.getArgument(i);
          currentArgumentLocals.add(old_name);
          args[i]=create(old_decls[i].getOrigin()).field_decl(new_name,rewrite(m.getArgType(i)));
          if (old_decls[i].isValidFlag(ASTFlags.GHOST)){
            args[i].setGhost(old_decls[i].isGhost());
          }
          Type arg_type=rewrite(m.getArgType(i));
          block.addStatement(create.field_decl(old_name,arg_type));
          block.addStatement(create.assignment(create.local_name(old_name),create.local_name(new_name)));
          subst.put(create.local_name(old_name),create.local_name(new_name));
        }
        create.leave();
        Method.Kind kind=m.kind;
        Contract mc=m.getContract();
        Contract c=null;
        if (mc!=null){
          Substitution sigma=new Substitution(source(),subst);
          ASTNode inv=mc.invariant.apply(sigma);
          ASTNode pre=mc.pre_condition.apply(sigma);
          ASTNode post=mc.post_condition.apply(sigma);
          c=new Contract(rewrite(mc.given),rewrite(mc.yields),rewrite(mc.modifies),rewrite(mc.accesses),inv,pre,post);
          if (mc.getOrigin()!=null){
            c.setOrigin(mc);
          }
        }
        { // this flattening could also be done by a generic pass.
          BlockStatement orig=(BlockStatement)body;
          for(int i=0;i<orig.getLength();i++){
            block.addStatement(orig.getStatement(i).apply(this));
          }
        }
        result=create.method_kind(kind, rewrite(m.getReturnType()),c, name, args, block);
        currentArgumentLocals.clear();
        break;
      }
      default:
        result=m.apply(copy_rw);
        break;
    }
  }
  
  @Override
  public void visit(OperatorExpression e){
    switch(e.operator()){
      case Old:{
        ASTNode arg=e.arg(0);
        if (arg instanceof NameExpression){
          NameExpression name=(NameExpression)arg;
          if (name.getKind()== NameExpressionKind.Argument){
            result=create.argument_name("__"+name.getName());
            break;
          }
        }
        super.visit(e);
        break; 
      }
      default:
        super.visit(e);
        break;
    }
  }

  @Override
  public void visit(NameExpression name) {
    switch(name.kind()) {
      case Argument:
        if(currentArgumentLocals.contains(name.name())) {
          result = create.local_name(name.name());
        } else {
          super.visit(name);
        }
        break;
      default:
        super.visit(name);
    }
  }
}
