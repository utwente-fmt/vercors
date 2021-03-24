package vct.col.rewrite;

import hre.ast.MessageOrigin;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Objects;

import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.VectorBlock;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.util.SequenceUtils;

public class VectorEncode extends AbstractRewriter {
  protected enum Op {AssignConst, Add}
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

  private static String name(Pair<Op, Type> p){
    return "Vector_"+p.e1.toString()+"_"+p.e2.toString();
  }
  
  private HashSet<Pair<Op,Type>> ops=new HashSet<>();
  private HashMap<String, Type> locals;
  
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
      ArrayList<DeclarationStatement> args=new ArrayList<>();
      ContractBuilder cb=new ContractBuilder();
      SequenceUtils.SequenceInfo t = SequenceUtils.expectArrayType(op.e2, "Expected array type here, but got %s");

      if(!t.isAssignable()) {
        Fail("Target array is not assignable");
      }

      args.add(create.field_decl("ar", op.e2));
      args.add(create.field_decl("from", create.primitive_type(PrimitiveSort.Integer)));
      args.add(create.field_decl("upto", create.primitive_type(PrimitiveSort.Integer)));
      SequenceUtils.validSequenceUsingType(create, cb::context, t.getCompleteType(), create.local_name("ar"));
      cb.context(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("from")));
      cb.context(create.expression(StandardOperator.LTE,create.local_name("from"),create.local_name("upto")));
      cb.context(create.expression(StandardOperator.LTE,create.local_name("upto"),
          create.array_length_dereference(create.local_name("ar"))));
      ASTNode range=create.expression(StandardOperator.And,
          create.expression(StandardOperator.LTE,create.local_name("from"),create.local_name("i")),
          create.expression(StandardOperator.LT,create.local_name("i"),create.local_name("upto"))
      );
      DeclarationStatement decl=create.field_decl("i",create.primitive_type(PrimitiveSort.Integer));
      ASTNode ari = SequenceUtils.accessUsingType(create, t.getCompleteType(),
          create.local_name("ar"),create.local_name("i"));
      cb.context(create.starall(range,create.expression(StandardOperator.Perm,ari,create.reserved_name(ASTReserved.FullPerm)), decl));
      switch(op.e1){
      case AssignConst:
        args.add(create.field_decl("c",t.getElementType()));
        cb.ensures(create.forall(range,
            create.expression(StandardOperator.EQ,ari,create.local_name("c")), decl));
        break;
      case Add:
        Type seqType = create.primitive_type(PrimitiveSort.Sequence, t.getElementType());
        args.add(create.field_decl("e1", seqType));
        args.add(create.field_decl("e2", seqType));
        ASTNode len=create.expression(StandardOperator.Minus,create.local_name("upto"),create.local_name("from"));
        cb.context(create.expression(StandardOperator.EQ,
            create.expression(StandardOperator.Size,create.local_name("e1")),len));
        cb.context(create.expression(StandardOperator.EQ,
            create.expression(StandardOperator.Size,create.local_name("e2")),len));
        ASTNode idx=create.expression(StandardOperator.Minus,create.local_name("i"),create.local_name("from"));
        ASTNode e1i = SequenceUtils.accessUsingType(create, seqType, create.local_name("e1"), idx);
        ASTNode e2i = SequenceUtils.accessUsingType(create, seqType, create.local_name("e2"), idx);
        cb.ensures(create.forall(range,
            create.expression(StandardOperator.EQ,ari,create.expression(StandardOperator.Plus,e1i,e2i)), decl));
        break;
      }
      Method header=create.method_decl(
          create.primitive_type(PrimitiveSort.Void), cb.getContract(), name(op), args, null);
      vector_lib.add_static(header);
    }
    create.leave();
    return res;
  }

  @Override
  public void visit(VectorBlock v){
    OperatorExpression init = (OperatorExpression)v.iter().initJava();
    ASTNode from=rewrite(init.arg(0));
    ASTNode upto=rewrite(init.arg(1));
    String iterVarName = v.iter().name();
    BlockStatement res=create.block();
    locals = new HashMap<>();
    for(ASTNode S:v.block()){
      // Turn locally declared variables into arrays.
      if (S instanceof DeclarationStatement){
        DeclarationStatement D=(DeclarationStatement)S;
        PrimitiveType elementType = expectAllowedElementType(D.getType());
        PrimitiveType arrayType = create.primitive_type(PrimitiveSort.Array,
                create.primitive_type(PrimitiveSort.Cell, elementType));
        DeclarationStatement decl=create.field_decl(D.name(), arrayType, create.new_array(arrayType, upto));
        res.add(decl);
        locals.put(D.name(), arrayType);
        continue;
      }
      
      // Process assignments.
      if (S instanceof AssignmentStatement){
        AssignmentStatement A=(AssignmentStatement)S;
        ASTNode loc = A.location();
        ASTNode expr = A.expression();

        // check types.
        PrimitiveType t1 = expectAllowedElementType(loc.getType());
        PrimitiveType t2 = expectAllowedElementType(expr.getType());
        if (!t1.equals(t2)) {
          Fail("types differ %s and %s",t1,t2);
        }
        
        // Detect array assigned to.
        Pair<String, Type> array = detectArray(loc, iterVarName);
        
        // detect operation.
        ArrayList<ASTNode> args=new ArrayList<ASTNode>();
        Op op=null;
        if (expr instanceof ConstantExpression){
          op=Op.AssignConst;
          args.add(expr);
        }
        if (expr.isa(StandardOperator.Plus)){
          OperatorExpression rhs=(OperatorExpression)expr;
          Pair<String, Type> left = detectArray(rhs.arg(0), iterVarName);
          Pair<String, Type> right = detectArray(rhs.arg(1), iterVarName);
          op=Op.Add;
          args.add(create.expression(StandardOperator.Values,create.local_name(left.e1),from,upto));
          args.add(create.expression(StandardOperator.Values,create.local_name(right.e1),from,upto));
        }
        if (op==null){
          Fail("unsupported RHS: %s",expr);
        }
        
        Pair<Op,Type> kind = new Pair<>(op, array.e2);
        ops.add(kind);

        args.add(0,create.unresolved_name(array.e1));
        args.add(1,from);
        args.add(2,upto);
        res.add(create.invokation(create.class_type("VectorLib"),null,name(kind),args));
        continue;
      }
      Fail("could not convert vector statement %s",S);
    }
    result=res;
  }

  private Pair<String, Type> detectArray(ASTNode expr, String indexName){
    Pair<String, Type> result = null;

    if (expr.isa(StandardOperator.Subscript)) {
      OperatorExpression deref = (OperatorExpression)expr;

      ASTNode array = deref.arg(0);
      ASTNode index = deref.arg(1);

      if (array instanceof NameExpression && index.isName(indexName)) {
        result = new Pair<>(((NameExpression) array).getName(), array.getType());
      }
    } else if(expr instanceof NameExpression) {
      NameExpression name = (NameExpression) expr;
      if(locals.containsKey(name.getName())) {
        result = new Pair<>(name.getName(), locals.get(name.getName()));
      }
    }

    Objects.requireNonNull(result, () -> String.format("unsupported LHS: %s", expr));

    return result;
  }

  private PrimitiveType expectAllowedElementType(Type type) {
    if(type.isPrimitive(PrimitiveSort.Float) || type.isPrimitive(PrimitiveSort.Integer)) {
      return (PrimitiveType) type;
    } else {
      Fail("Type %s is not allowed as an array element type in a vector block", type);
      throw new IllegalArgumentException();
    }
  }
}
