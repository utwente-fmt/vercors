package vct.col.rewrite;

import hre.ast.MessageOrigin;

import java.util.ArrayList;
import java.util.HashSet;
import vct.col.ast.*;
import vct.col.ast.PrimitiveType.Sort;
import static vct.col.rewrite.VectorEncode.Type.*;
import static vct.col.rewrite.VectorEncode.Op.*;

public class VectorEncode extends AbstractRewriter {
  
  protected static enum Type {Int,Float};
  protected static enum Op {AssignConst,Add};
  private static class Pair<E1,E2>{
    public final E1 e1;
    public final E2 e2;
    public Pair(E1 e1,E2 e2){
      this.e1=e1;
      this.e2=e2;
    }
    @Override
    public int hashCode(){
      return e1.hashCode() ^ e2.hashCode();
    }
    @Override
    public boolean equals(Object o){
      if (o instanceof Pair){
        Pair<?,?> p=(Pair<?,?>)o;
        return e1.equals(p.e1) && e2.equals(p.e2);
      } else {
        return false;
      }
    }
  }

  private static String name(Pair<Op,Type> p){
    return "Vector_"+p.e1.toString()+"_"+p.e2.toString();
  }
  
  private HashSet<Pair<Op,Type>> ops=new HashSet<Pair<Op, Type>>();
  private String ivar;
  private HashSet<String> locals;
  
  //static ProgramUnit vector_lib;
  //static {
  //  File file=new File(new File(Configuration.getHome().toFile(),"config"),"vectorlib.pvl");
  //  vector_lib=Parsers.getParser("pvl").parse(file);
  //}

  public VectorEncode(ProgramUnit source) {
    super(source);
  }
  
  @Override
  public ProgramUnit rewriteAll(){
    create.enter();
    create.setOrigin(new MessageOrigin("Vector Library"));
    ProgramUnit res=super.rewriteAll();
    ASTClass vector_lib=create.new_class("VectorLib",null,null);
    res.add(vector_lib);
    for(Pair<Op,Type> op:ops){
      ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
      ContractBuilder cb=new ContractBuilder();
      vct.col.ast.Type t=null;
      switch(op.e2){
      case Int:
        t=create.primitive_type(Sort.Integer);
        break;
      case Float:
        t=create.primitive_type(Sort.Float);
        break;
      }
      args.add(create.field_decl("ar", create.primitive_type(Sort.Array,t)));
      args.add(create.field_decl("from", create.primitive_type(Sort.Integer)));
      args.add(create.field_decl("upto", create.primitive_type(Sort.Integer)));
      cb.context(neq(create.local_name("ar"),create.reserved_name(ASTReserved.Null)));
      cb.context(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("from")));
      cb.context(create.expression(StandardOperator.LTE,create.local_name("from"),create.local_name("upto")));
      cb.context(create.expression(StandardOperator.LTE,create.local_name("upto"),
          create.dereference(create.local_name("ar"), "length")));
      ASTNode range=create.expression(StandardOperator.And,
          create.expression(StandardOperator.LTE,create.local_name("from"),create.local_name("i")),
          create.expression(StandardOperator.LT,create.local_name("i"),create.local_name("upto"))
      );
      DeclarationStatement decl=create.field_decl("i",create.primitive_type(Sort.Integer));
      ASTNode ari=create.expression(StandardOperator.Subscript,
          create.local_name("ar"),create.local_name("i"));
      cb.context(create.starall(range,create.expression(StandardOperator.Perm,ari,create.constant(1)), decl));
      switch(op.e1){
      case AssignConst:
        args.add(create.field_decl("c",t));
        cb.ensures(create.forall(range,
            create.expression(StandardOperator.EQ,ari,create.local_name("c")), decl));
        break;
      case Add:
        args.add(create.field_decl("e1",create.primitive_type(Sort.Sequence,t)));
        args.add(create.field_decl("e2",create.primitive_type(Sort.Sequence,t)));
        ASTNode len=create.expression(StandardOperator.Minus,create.local_name("upto"),create.local_name("from"));
        cb.context(create.expression(StandardOperator.EQ,
            create.expression(StandardOperator.Size,create.local_name("e1")),len));
        cb.context(create.expression(StandardOperator.EQ,
            create.expression(StandardOperator.Size,create.local_name("e2")),len));
        ASTNode idx=create.expression(StandardOperator.Minus,create.local_name("i"),create.local_name("from"));
        ASTNode e1i=create.expression(StandardOperator.Subscript,
            create.local_name("e1"),idx);
        ASTNode e2i=create.expression(StandardOperator.Subscript,
            create.local_name("e2"),idx);
        cb.ensures(create.forall(range,
            create.expression(StandardOperator.EQ,ari,create.expression(StandardOperator.Plus,e1i,e2i)), decl));
        break;
      }
      Method header=create.method_decl(
          create.primitive_type(Sort.Void), cb.getContract(), name(op), args, null);
      vector_lib.add_static(header);
    }
    create.leave();
    return res;
  }

  @Override
  public void visit(VectorBlock v){
    OperatorExpression init = (OperatorExpression)v.iter().getInit();
    ASTNode from=rewrite(init.arg(0));
    ASTNode upto=rewrite(init.arg(1));
    ivar = v.iter().getName();
    BlockStatement res=create.block();
    locals = new HashSet<String>();
    for(ASTNode S:v.block()){
      // Turn locally declared variables into arrays.
      if (S instanceof DeclarationStatement){
        DeclarationStatement D=(DeclarationStatement)S;
        detect(D.getType()); // check for valid type.
        DeclarationStatement decl=create.field_decl(
            D.getName(),
            create.primitive_type(Sort.Array,D.getType()),
            create.new_array(D.getType(), upto));
        res.add(decl);
        locals.add(D.getName());
        continue;
      }
      
      // Process assignments.
      if (S instanceof AssignmentStatement){
        AssignmentStatement A=(AssignmentStatement)S;
        ASTNode loc = A.location();
        ASTNode expr = A.expression();

        // check types.
        Type t1=detect(loc.getType());
        Type t2=detect(expr.getType());
        if (t1 != t2){
          Fail("types differ %s and %s",t1,t2);
        }
        
        // Detect array assigned to.
        String array=detect_array(loc);
        
        // detect operation.
        ArrayList<ASTNode> args=new ArrayList<ASTNode>();
        Op op=null;
        if (expr instanceof ConstantExpression){
          op=AssignConst;
          args.add(expr);
        }
        if (expr.isa(StandardOperator.Plus)){
          OperatorExpression rhs=(OperatorExpression)expr;
          String e1=detect_array(rhs.arg(0));
          String e2=detect_array(rhs.arg(1));
          op=Add;
          args.add(create.expression(StandardOperator.Values,create.local_name(e1),from,upto));
          args.add(create.expression(StandardOperator.Values,create.local_name(e2),from,upto));
        }
        if (op==null){
          Fail("unsupported RHS: %s",expr);
        }
        
        Pair<Op,Type> kind=new Pair<Op,Type>(op,t1);
        ops.add(kind);

        args.add(0,create.unresolved_name(array));
        args.add(1,from);
        args.add(2,upto);
        res.add(create.invokation(create.class_type("VectorLib"),null,name(kind),args));
        continue;
      }
      Fail("could not convert vector statement %s",S);
    }
    result=res;
  }

  private String detect_array(ASTNode loc){
    String array=null;
    if (loc instanceof NameExpression){
      NameExpression name=(NameExpression)loc;
      if (locals.contains(name.getName())){
        array=name.getName();
      }
    } else if (loc.isa(StandardOperator.Subscript)) {
      OperatorExpression deref=(OperatorExpression)loc;
      ASTNode ar=deref.arg(0);
      ASTNode idx=deref.arg(1);
      if (ar instanceof NameExpression && idx.isName(ivar)){
        array=((NameExpression)ar).getName();
      }
    }
    if (array==null){
      Fail("unsupported LHS: %s",loc);
    }
    return array;
  }
  
  private Type detect(vct.col.ast.Type type) {
    if (type.isInteger()) return Int;
    if (type.isPrimitive(Sort.Float)) return Float;
    Fail("type %s is invalid in vector blocks",type);
    return null;
  }
  
}
