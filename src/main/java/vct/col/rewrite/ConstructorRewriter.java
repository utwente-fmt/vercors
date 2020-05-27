package vct.col.rewrite;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.*;

public class ConstructorRewriter extends AbstractRewriter {

  public ConstructorRewriter(ProgramUnit source) {
    super(source);
  }

  public void visit(MethodInvokation e){
    if (e.getDefinition()==null){
      Warning("method invokation (%s) without definition",e.method());
    } else if (e.getDefinition().kind== Method.Kind.Constructor) {
      Fail("%s cannot deal with instantiation that is not an assignment at %s",getClass(),e.getOrigin());
    }
    super.visit(e);
  }
  
  public void visit(AssignmentStatement e){
    if ((e.expression() instanceof StructValue)
       && (((StructValue)e.expression()).type() instanceof ClassType)){
      Abort("illegal use of structvalue for constructor call");
    }
    if (e.expression() instanceof MethodInvokation){
      MethodInvokation i=(MethodInvokation)e.expression();
      if (i.getDefinition().kind==Method.Kind.Constructor) {
        ASTNode s1=create.assignment(rewrite(e.location()),create.expression(StandardOperator.New,rewrite(i.getType())));
        
        String method_name;
        if (i.method().equals(Method.JavaConstructor)){
          method_name=i.dispatch().toString();
        } else {
          method_name=i.method();
        }
        method_name+="_init";
        MethodInvokation s2=create.invokation(rewrite(e.location()),null,method_name,rewrite(i.getArgs()));
        if (i.get_before().size()>0) {
          s2.set_before(rewrite(i.get_before()));
        }
        if (i.get_after().size()>0) {
          s2.set_after(rewrite(i.get_after()));
        }
        copy_labels(s2,e.expression());
        result=create.block(s1,s2);
        return;
      }
    }
    super.visit(e);
  }
  
  public void visit(Method m){
    if (m.kind==Method.Kind.Constructor){
      String name=m.getName()+"_init";
      DeclarationStatement args[]=rewrite(m.getArgs());
      ASTNode body=rewrite(m.getBody());
      ContractBuilder cb=new ContractBuilder();
      Contract c=m.getContract();
      if (c!=null){
        rewrite(c,cb);
      }
      for(DeclarationStatement field:((ASTClass)m.getParent()).dynamicFields()){
        cb.requires(create.expression(
            StandardOperator.Perm,
            create.field_name(field.name()),
            create.constant(100)
        ));
      }
      result=create.method_decl(create.primitive_type(PrimitiveSort.Void), cb.getContract(), name, args, body);
    } else {
      super.visit(m);
    }
  }
}
