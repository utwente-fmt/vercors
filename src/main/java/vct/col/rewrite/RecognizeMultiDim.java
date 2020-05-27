package vct.col.rewrite;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import vct.col.ast.expr.BindingExpression;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;

public class RecognizeMultiDim extends AbstractRewriter {

  public RecognizeMultiDim(ProgramUnit source) {
    super(source);
  }

  private static Type get_basetype(Type t){
    while(t.isPrimitive(PrimitiveSort.Array)){
      PrimitiveType tt=(PrimitiveType)t;
      t=(Type)tt.firstarg();
    }
    return t;
  }

  private static ASTNode[] get_dimensions(Type t){
    ArrayList<ASTNode> list=new ArrayList<ASTNode>();
    while(t.isPrimitive(PrimitiveSort.Array)){
      PrimitiveType tt=(PrimitiveType)t;
      if (tt.nrOfArguments()==2){
        list.add(tt.secondarg());
        t=(Type)tt.firstarg();
      } else {
        // dimension unknown.
        return null;
      }
    }
    if (list.size()>0){ 
      return list.toArray(new ASTNode[0]);
    } else {
      return null;
    }
  }
  
  private HashMap<String,ASTNode[]> dimension=new HashMap<String,ASTNode[]>();
  
  @Override
  public void visit(Method m){
    String name=m.getName();
    int N=m.getArity();
    if (currentContractBuilder==null) currentContractBuilder=new ContractBuilder();
    DeclarationStatement args[]=new DeclarationStatement[N];
    for(int i=0;i<N;i++){
      String arg=m.getArgument(i);
      Type t=m.getArgType(i);
      ASTNode dims[]=get_dimensions(t);
      if (dims!=null){
        Debug("Looks like %s is a multi-dimensional array",arg);
        args[i]=create.field_decl(arg,create.primitive_type(
        		PrimitiveSort.Array,
            rewrite(get_basetype(t)),
            create.fold(StandardOperator.Mult,dims)
        ));
        dimension.put(arg,dims);
      } else {
        args[i]=create.field_decl(arg,rewrite(t));
      }
    }
    Contract mc=m.getContract();
    if (mc!=null){
      rewrite(mc,currentContractBuilder);
    }
    Method.Kind kind=m.kind;
    Type rt=rewrite(m.getReturnType());
    Contract c=currentContractBuilder.getContract();
    currentContractBuilder=null;
    ASTNode body=rewrite(m.getBody());
    dimension.clear();
    result=create.method_kind(kind, rt, c, name, args, m.usesVarArgs(), body);
  }

  @Override
  public void visit(OperatorExpression e){
    if (e.isa(StandardOperator.Subscript)){
      ArrayList<ASTNode> args=new ArrayList<ASTNode>();
      ASTNode tmp=try_multidim(e,args);
      if(tmp!=null){
        result=tmp;
        return;
      }
    }
    super.visit(e);
  }

  
  @Override
  public void visit(ASTClass cl){
    multidimset=new HashSet<Integer>();
    super.visit(cl);
    ASTClass res=(ASTClass)result;
    for(int N:multidimset){
      ContractBuilder cb=new ContractBuilder();
      DeclarationStatement args[]=new DeclarationStatement[N+N];
      for(int i=0;i<N;i++){
        cb.requires(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("i"+i)));
        cb.requires(create.expression(StandardOperator.LT,create.local_name("i"+i),create.local_name("N"+i)));
        cb.requires(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("N"+i)));
        args[i]=create.field_decl("N"+i,create.primitive_type(PrimitiveSort.Integer));
        args[N+i]=create.field_decl("i"+i,create.primitive_type(PrimitiveSort.Integer));
      }
      ASTNode result=create.local_name("i0");
      ASTNode max=create.local_name("N0");
      for(int i=1;i<N;i++){
        result=create.expression(StandardOperator.Plus,
            create.expression(StandardOperator.Mult,result,create.local_name("N"+i)),
            create.local_name("i"+i)
        );
        max=create.expression(StandardOperator.Mult,max,create.local_name("N"+i));
      }
      cb.ensures(create.expression(StandardOperator.LTE,create.constant(0),create.reserved_name(ASTReserved.Result)));
      cb.ensures(create.expression(StandardOperator.LT,create.reserved_name(ASTReserved.Result),max));
      cb.ensures(create.expression(StandardOperator.EQ,create.reserved_name(ASTReserved.Result),result));
      if (N==2){
        cb.ensures(create.expression(StandardOperator.EQ,
            create.expression(StandardOperator.Mod,create.reserved_name(ASTReserved.Result),create.local_name("N1")),
            create.local_name("i1")
        ));
        // Disabled because it caused an inconsistency.
        //cb.ensures(create.expression(StandardOperator.LT,
        //    create.expression(StandardOperator.Mod,create.reserved_name(ASTReserved.Result),create.local_name("N1")),
        //    create.local_name("N0")
        //));
      }
      Method idx=create.function_decl(
          create.primitive_type(PrimitiveSort.Integer),
          cb.getContract(),
          "multidim_index_"+N ,
          args,
          null
      );
      res.add_static(idx);
    }
    result=res;
  }
  
  public void visit(BindingExpression e){
    boolean rw_needed;
    switch(e.binder()){
    case Star:
    case Sum:
      rw_needed=true;
      rw_binders++;
      break;
    default:
      rw_needed=false;
      break;
    }
    super.visit(e);
    if(rw_needed) rw_binders--;
  }
  
  private HashSet<Integer> multidimset;
  
  private int rw_binders=0;
  
  private ASTNode try_multidim(ASTNode node, ArrayList<ASTNode> args) {
    if (node.isa(StandardOperator.Subscript)){
      OperatorExpression e=(OperatorExpression)node;
      args.add(rewrite(e.arg(1)));
      return try_multidim(e.arg(0),args);
    }
    if (node instanceof NameExpression){
      NameExpression n=(NameExpression)node;
      String name=n.getName();
      ASTNode dims[]=dimension.get(name);
      if(dims==null) return null;
      if(dims.length!=args.size()) return null;
      if(dims.length==1) return null;
      ASTNode idx;
      if (rw_binders==0){
        int N=dims.length;
        ASTNode idx_args[]=new ASTNode[N+N];
        for(int i=0;i<N;i++){
          idx_args[i]=dims[N-i-1];
          idx_args[N+i]=args.get(N-i-1);
        }
        multidimset.add(N);
        idx=create.expression(StandardOperator.Identity,create.invokation(null,null,"multidim_index_"+N,idx_args));
      } else {
        int i=dims.length-1;
        idx=args.get(i);
        if (idx.isReserved(ASTReserved.Any)){
          while(i>0){
            i--;
            if (!args.get(i).isReserved(ASTReserved.Any)) return null;
          }
        } else {
          while(i>0){
            i--;
            idx=create.expression(StandardOperator.Mult,idx,dims[i]);
            idx=create.expression(StandardOperator.Plus,idx,args.get(i));
          }
        }
      }
      return create.expression(StandardOperator.Subscript,rewrite(node),idx);
    }
    return null;
  }
}
