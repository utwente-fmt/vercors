package vct.col.rewrite;

import vct.col.ast.ASTClass;
import vct.col.ast.ASTNode;
import vct.col.ast.ASTReserved;
import vct.col.ast.ASTSpecial;
import vct.col.ast.ConstantExpression;
import vct.col.ast.ContractBuilder;
import vct.col.ast.DeclarationStatement;
import vct.col.ast.Dereference;
import vct.col.ast.IfStatement;
import vct.col.ast.IntegerValue;
import vct.col.ast.Method;
import vct.col.ast.MethodInvokation;
import vct.col.ast.NameExpression.Kind;
import vct.col.ast.OperatorExpression;
import vct.col.ast.PrimitiveType;
import vct.col.ast.PrimitiveType.Sort;
import vct.col.ast.ProgramUnit;
import vct.col.ast.StandardOperator;
import vct.col.ast.Type;
import hre.ast.MessageOrigin;

import java.util.Hashtable;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

public class ChalicePreProcess extends AbstractRewriter {

  private Hashtable<Type,String>cell_types=new Hashtable<Type, String>();
  
  public ChalicePreProcess(ProgramUnit source) {
    super(source);
  }
  
  private int count;
  
  @Override
  public void visit(ASTSpecial s){
    boolean exhale=false;
    switch(s.kind){
    case Exhale:
      exhale=true;
    case Inhale:{
      count++;
      String name;
      ContractBuilder cb=new ContractBuilder();
      if (exhale){
        name="exhale_"+count;
        cb.requires(rewrite(s.args[0]));
      } else {
        name="inhale_"+count;
        cb.ensures(rewrite(s.args[0]));
      }
      Hashtable<String,Type> vars=free_vars(s.args[0]);
      currentTargetClass.add(create.method_decl(
          create.primitive_type(Sort.Void),
          cb.getContract(),
          name,
          gen_pars(vars),
          create.block(create.special(ASTSpecial.Kind.Assume, create.constant(false)))
      ));
      result=gen_call(name,vars);
      break;
    }
    default:
      super.visit(s);
    }
  }
  
  @Override
  public void visit(ConstantExpression e){
    if (e.getType().isPrimitive(Sort.Fraction)){
      int v = ((IntegerValue)(e.value())).value();
      if (v==1){
        result=create.reserved_name(ASTReserved.FullPerm);
        return;
      }
    }
    super.visit(e);
  }
  
  @Override
  public ProgramUnit rewriteAll(){
    ProgramUnit res=super.rewriteAll();
    for(Entry<Type,String> entry : cell_types.entrySet()){
      create.setOrigin(new MessageOrigin("added by ChalicePreProcess"));
      ASTClass cl=create.ast_class(entry.getValue(), ASTClass.ClassKind.Plain ,null, null,null);
      cl.add_dynamic(create.field_decl("item",entry.getKey()));
      res.add(cl);
    }
    return res;
  }
  
  @Override
  public void visit(MethodInvokation e){
    if (e.method.equals("length") && e.object.getType().isPrimitive(PrimitiveType.Sort.Sequence)){
      result=create.expression(StandardOperator.Size,rewrite(e.object));
    } else {
      super.visit(e);
    }
  }
  
  @Override
  public void visit(Dereference e){
    if (e.field().equals("length") && e.object().getType().isPrimitive(PrimitiveType.Sort.Sequence)){
      result=create.expression(StandardOperator.Size,rewrite(e.object()));
    } else {
      super.visit(e);
    }    
  }
  
  @Override
  public void visit(PrimitiveType t){
    if (t.sort==PrimitiveType.Sort.Cell){
      String sort="cell_of_"+t.getArg(0);
      cell_types.put((Type)t.getArg(0),sort);
      result=create.class_type(sort);
    } else {
      super.visit(t);
    }
  }
  
  @Override
  public void visit(OperatorExpression e){
    switch(e.operator()){
      case Minus:{
        super.visit(e);
        if (e.arg(0).getType().isPrimitive(Sort.Fraction) ||
            e.arg(0).getType().isPrimitive(Sort.ZFraction) ||
            e.arg(1).getType().isPrimitive(Sort.Fraction) ||
            e.arg(1).getType().isPrimitive(Sort.ZFraction) )
        {
          ASTNode temp=result;
          result=create.expression(StandardOperator.ITE,
              create.expression(StandardOperator.LT, rewrite(e.arg(0)), rewrite(e.arg(1))),
              create.constant(0),
              temp
          );
          result.setType(new PrimitiveType(Sort.ZFraction));
        }
        break;
      }
      case Plus:{
        if (e.getType().isPrimitive(Sort.Sequence)){
          result = create.expression(StandardOperator.Append, rewrite(e.args()));
        } else {
          super.visit(e);
        }
        break;
      }
      default:
        super.visit(e);
        break;
    }
  }
  
  private AtomicInteger if_any_count;
  private Method if_any_method;
  
  @Override
  public void visit(IfStatement s){
    int N=s.getCount();
    IfStatement res=new IfStatement();
    for(int i=0;i<N;i++){
      ASTNode guard;
      if (s.getGuard(i).isReserved(ASTReserved.Any)){
        int id=if_any_count.incrementAndGet();
        currentBlock.add(create.field_decl("if_any_bool"+id,create.primitive_type(Sort.Boolean)));
        ASTNode name=create.name(Kind.Local,null,"if_any_bool"+id);
        MethodInvokation rnd=create.invokation(create.reserved_name(ASTReserved.This),null,"if_any_random",name);
        rnd.setDefinition(if_any_method);
        currentBlock.add(rnd);
        guard=name;
      } else if (s.getGuard(i)==IfStatement.elseGuard()) {
        guard=IfStatement.elseGuard();
      } else {
        guard=rewrite(s.getGuard(i));
      }
      res.addClause(guard, rewrite(s.getStatement(i)));
    }
    res.setOrigin(s);
    result=res;
  }
  
  @Override
  public void visit(ASTClass cl){
    if_any_count=new AtomicInteger(0);
    DeclarationStatement args[]=new DeclarationStatement[1];
    args[0]=create.field_decl("random_bool", create.primitive_type(Sort.Boolean));    
    if_any_method=create.method_decl(create.primitive_type(Sort.Void), null, "if_any_random", args, null);
    super.visit(cl);
    if (if_any_count.get()>0){
      cl=(ASTClass)result;
      cl.add(if_any_method);
      result=cl;
    }
  }
}
