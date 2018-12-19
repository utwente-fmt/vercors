package vct.col.rewrite;

import java.util.ArrayList;
import java.util.HashSet;

import vct.col.ast.ASTClass;
import vct.col.ast.ASTNode;
import vct.col.ast.ASTReserved;
import vct.col.ast.AssignmentStatement;
import vct.col.ast.ClassType;
import vct.col.ast.ContractBuilder;
import vct.col.ast.DeclarationStatement;
import vct.col.ast.Dereference;
import vct.col.ast.OperatorExpression;
import vct.col.ast.PrimitiveType;
import vct.col.ast.Type;
import vct.col.ast.PrimitiveSort;
import vct.col.ast.ProgramUnit;
import vct.col.ast.StandardOperator;

public class RewriteArrayRef extends AbstractRewriter {

	public RewriteArrayRef(ProgramUnit source) {
	  super(source);
  }
	
  private HashSet<Type> new_array;
  private HashSet<Type> array_values;

  
  private ASTNode injective(ASTNode seq){
    ArrayList<ASTNode> conds=new ArrayList<ASTNode>();
    ASTNode max=create.expression(StandardOperator.Size, seq );
    conds.add(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("i")));
    conds.add(create.expression(StandardOperator.LT,create.local_name("i"),max));
    conds.add(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("j")));
    conds.add(create.expression(StandardOperator.LT,create.local_name("j"),max));
    conds.add(create.expression(StandardOperator.EQ
        ,create.expression(StandardOperator.Subscript,seq,create.local_name("i"))
        ,create.expression(StandardOperator.Subscript,seq,create.local_name("j"))
    ));
    ASTNode guard=create.fold(StandardOperator.And, conds);
    ASTNode claim=create.expression(StandardOperator.EQ,
        create.local_name("i"),create.local_name("j"));
    return create.forall(guard,claim
        ,create.field_decl("i", create.primitive_type(PrimitiveSort.Integer))
        ,create.field_decl("j", create.primitive_type(PrimitiveSort.Integer))
    );
  }
  
  @Override
	public void visit(OperatorExpression e){
		switch (e.operator()){
    case ValidArray:{
      ASTNode M=rewrite(e.arg(0));
      ASTNode MD=create.expression(StandardOperator.OptionGet,M);
      ASTNode sz=rewrite(e.arg(1));
      result=create.expression(StandardOperator.And
        ,create.expression(StandardOperator.NEQ,M,create.reserved_name(ASTReserved.OptionNone))
        ,and(create.expression(StandardOperator.EQ,create.expression(StandardOperator.Size,MD),sz),
             injective(MD)
            )
      );
      break;
    }
    case ValidMatrix:{
      ASTNode M=rewrite(e.arg(0));
      ASTNode MD=create.expression(StandardOperator.OptionGet,M);
      ASTNode sz=create.expression(StandardOperator.Mult,
          rewrite(e.arg(1)),rewrite(e.arg(2)));
      result=create.expression(StandardOperator.And
        ,create.expression(StandardOperator.NEQ,M,create.reserved_name(ASTReserved.OptionNone))
        ,and(create.expression(StandardOperator.EQ,create.expression(StandardOperator.Size,MD),sz),
             injective(MD)
            )
      );
      break;
    }
		  case EQ:
		  case NEQ:
		  {
        ASTNode e0=e.arg(0);
        ASTNode e1=e.arg(1);
        ASTNode array=null;
        if (e0.isReserved(ASTReserved.Null) && e1.getType().isPrimitive(PrimitiveSort.Array)){
          array=e1;
        }
        if (e1.isReserved(ASTReserved.Null) && e0.getType().isPrimitive(PrimitiveSort.Array)){
          array=e0;
        }
        if(array==null){
          super.visit(e);
        } else {
          result=create.expression(e.operator(),
              create.reserved_name(ASTReserved.OptionNone),
              rewrite(array));
        }
		    break;
		  }
		  case Subscript:
			  if (e.arg(0).getType().isPrimitive(PrimitiveSort.Array)){
			    
          ASTNode res=create.expression(StandardOperator.OptionGet,rewrite(e.arg(0)));
          res=create.expression(StandardOperator.Subscript,res,rewrite(e.arg(1)));
          result=create.dereference(res,"item");
			  } else {
			    super.visit(e);
			  }
			  break;
		  case Length:{
		    ASTNode res=create.expression(StandardOperator.OptionGet,rewrite(e.arg(0)));
		    result=create.expression(StandardOperator.Size,res);
		    break;
		  }
		  case Values:{
        Type t=(Type)((PrimitiveType)e.getType()).firstarg();
        array_values.add(t);
        result=create.invokation(null,null,"array_values_"+t,rewrite(e.argsJava()));
        break;
		  }
		  case NewArray:{
        Type t=(Type)e.arg(0);
        new_array.add(t);
        result=create.invokation(null,null,"new_array_some_"+t,rewrite(e.arg(1)));
		    break;
		  }
		  case PointsTo: {
		    // If an Array field should have a Null value, change it to OptionNone.
		    if(e.first().getType().isPrimitive(PrimitiveSort.Array) && e.third().isReserved(ASTReserved.Null)) {
		      ASTNode res = rewrite(e.first());
		      result = star(create.expression(StandardOperator.Perm, res, rewrite(e.second())),
		          create.expression(StandardOperator.EQ, res, create.reserved_name(ASTReserved.OptionNone)));
		    } else {
		      super.visit(e);
		    }
		    break;
		  }
			default:
				super.visit(e);
		}
	}
	
  @Override
  public void visit(Dereference e){
    if (e.field().equals("length")){
      ASTNode res=rewrite(e.obj());
      if (e.obj().getType().isPrimitive(PrimitiveSort.Array)){
        res=create.expression(StandardOperator.OptionGet,res);
      }
      result=create.expression(StandardOperator.Size,res);
      return;
    }
    // Change Dereference type if Class field is of Array type. 
    Type object_type=e.obj().getType();
    if(object_type instanceof ClassType) {
      ASTClass cl=source().find(((ClassType)object_type).getNameFull());
      DeclarationStatement decl=cl.find_field(e.field(),true);
      if(decl != null && decl.getType().isPrimitive(PrimitiveSort.Array)) {
        e.setType(create.primitive_type(PrimitiveSort.Option, create.primitive_type(PrimitiveSort.Sequence, create.primitive_type(PrimitiveSort.Cell, decl.getType().firstarg()))));
      }
    super.visit(e);
  }
}
	
	@Override
	public void visit(PrimitiveType t){
		switch(t.sort){
		case Array:
			result=
			  create.primitive_type(PrimitiveSort.Option,
			    create.primitive_type(PrimitiveSort.Sequence,
		          create.primitive_type(PrimitiveSort.Cell,
			        rewrite(t.firstarg()))));
			break;
		  default:
		  	super.visit(t);
		}
	}
	
  @Override
  public ProgramUnit rewriteAll(){
    ProgramUnit res=super.rewriteAll();
    // TODO: move newarray generation here.
    return res;
  }

  @Override
  public void visit(ASTClass cl){
    new_array=new HashSet<Type>();
    array_values=new HashSet<Type>();
    super.visit(cl);
    ASTClass res=(ASTClass)result;
    for(Type t:new_array){
      Type result_type;
      result_type=create.primitive_type(PrimitiveSort.Sequence,create.primitive_type(PrimitiveSort.Cell,rewrite(t)));
      ContractBuilder cb=new ContractBuilder();
      
      cb.ensures(create.expression(StandardOperator.EQ,
          create.expression(StandardOperator.Size,create.reserved_name(ASTReserved.Result)),
          create.local_name("len")));
      DeclarationStatement decl=create.field_decl("i",create.primitive_type(PrimitiveSort.Integer));
      ASTNode guard=and(lte(constant(0),create.local_name("i")),
          less(create.local_name("i"),create.expression(StandardOperator.Size,create.reserved_name(ASTReserved.Result))));
      ASTNode base=create.expression(StandardOperator.Subscript,create.reserved_name(ASTReserved.Result),create.local_name("i"));
      ASTNode claim;
      claim=create.expression(StandardOperator.Perm,
            create.dereference(base,"item")
            ,create.reserved_name(ASTReserved.FullPerm));
      cb.ensures(create.starall(guard, claim, decl));
      DeclarationStatement args[]=new DeclarationStatement[]{create.field_decl("len",create.primitive_type(PrimitiveSort.Integer))};
      res.add_dynamic(create.method_decl(result_type,cb.getContract(), "new_array_"+t, args,null));
    }
    for(Type t:new_array){
      Type result_type;
      result_type=create.primitive_type(PrimitiveSort.Option,
          create.primitive_type(PrimitiveSort.Sequence,
              create.primitive_type(PrimitiveSort.Cell,rewrite(t))));
      ContractBuilder cb=new ContractBuilder();
      cb.ensures(create.expression(StandardOperator.NEQ,
          create.reserved_name(ASTReserved.Result),
          create.reserved_name(ASTReserved.OptionNone)
          ));
      ASTNode Result=create.expression(StandardOperator.OptionGet,create.reserved_name(ASTReserved.Result));
      cb.ensures(create.expression(StandardOperator.EQ,
          create.expression(StandardOperator.Size,Result),
          create.local_name("len")));
      DeclarationStatement decl=create.field_decl("i",create.primitive_type(PrimitiveSort.Integer));
      ASTNode guard=and(lte(constant(0),create.local_name("i")),
          less(create.local_name("i"),create.expression(StandardOperator.Size,Result)));
      ASTNode base=create.expression(StandardOperator.Subscript,Result,create.local_name("i"));
      ASTNode claim;
      claim=create.expression(StandardOperator.Perm,
            create.dereference(base,"item")
            ,create.reserved_name(ASTReserved.FullPerm));
      cb.ensures(create.starall(guard, claim, decl));
      DeclarationStatement args[]=new DeclarationStatement[]{create.field_decl("len",create.primitive_type(PrimitiveSort.Integer))};
      res.add_dynamic(create.method_decl(result_type,cb.getContract(), "new_array_some_"+t, args,null));
    }
    for(Type t:array_values){
      Type result_type=create.primitive_type(PrimitiveSort.Sequence,t);
      Type type0=create.primitive_type(PrimitiveSort.Option,
          create.primitive_type(PrimitiveSort.Sequence,
              create.primitive_type(PrimitiveSort.Cell,rewrite(t))));
      ContractBuilder cb=new ContractBuilder();
      ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
      args.add(create.field_decl("ar", type0));
      args.add(create.field_decl("from",create.primitive_type(PrimitiveSort.Integer)));
      args.add(create.field_decl("upto",create.primitive_type(PrimitiveSort.Integer)));
      cb.requires(neq(create.local_name("ar"),create.reserved_name(ASTReserved.OptionNone)));
      ASTNode deref=create.expression(StandardOperator.OptionGet,create.local_name("ar"));
      
      cb.requires(create.expression(StandardOperator.LTE,create.constant(0),create.local_name("from")));
      cb.requires(create.expression(StandardOperator.LTE,create.local_name("from"),create.local_name("upto")));
      cb.requires(create.expression(StandardOperator.LTE,create.local_name("upto"),
          create.expression(StandardOperator.Size,deref)));
      ASTNode range=create.expression(StandardOperator.And,
          create.expression(StandardOperator.LTE,create.local_name("from"),create.local_name("i")),
          create.expression(StandardOperator.LT,create.local_name("i"),create.local_name("upto"))
      );
      DeclarationStatement decl=create.field_decl("i",create.primitive_type(PrimitiveSort.Integer));
      ASTNode ari=create.dereference(create.expression(StandardOperator.Subscript,
          deref,create.local_name("i")),"item");
      // TODO: use ASTNode perm=create.reserved_name(ASTReserved.ReadPerm);
      ASTNode perm=create.expression(StandardOperator.Div,create.constant(1),create.constant(100));
      cb.requires(create.starall(range,create.expression(StandardOperator.Perm,ari,perm), decl));
      ASTNode Resi=create.expression(StandardOperator.Subscript,create.reserved_name(ASTReserved.Result),
          create.expression(StandardOperator.Minus,create.local_name("i"),create.local_name("from")));
      cb.ensures(create.expression(StandardOperator.EQ,
          create.expression(StandardOperator.Size,create.reserved_name(ASTReserved.Result)),
          create.expression(StandardOperator.Minus,create.local_name("upto"),create.local_name("from"))
      ));
      cb.ensures(create.forall(range,create.expression(StandardOperator.EQ,ari,Resi),decl));
      
      
      ASTNode len=create.expression(StandardOperator.Minus,create.local_name("upto"),create.local_name("from"));
      range=create.expression(StandardOperator.And,
          create.expression(StandardOperator.LTE,create.constant(0),create.local_name("i")),
          create.expression(StandardOperator.LT,create.local_name("i"),len));
      ari=create.dereference(create.expression(StandardOperator.Subscript,
          deref,create.expression(StandardOperator.Plus,create.local_name("i"),create.local_name("from"))),"item");
      Resi=create.expression(StandardOperator.Subscript,create.reserved_name(ASTReserved.Result),
          create.local_name("i"));
      cb.ensures(create.forall(range,create.expression(StandardOperator.EQ,ari,Resi),decl));
      
      res.add_static(create.function_decl(result_type,cb.getContract(),"array_values_"+t, args, null));
    }
    new_array=null;
    array_values=null;
    result=res;
  }
  
  @Override
  public void visit(AssignmentStatement as) {
    // Assign OptionNone if array is assigned null.
    if(as.location().getType().isPrimitive(PrimitiveSort.Array) && as.expression().isReserved(ASTReserved.Null)) {
      result = create.assignment(rewrite(as.location()), create.reserved_name(ASTReserved.OptionNone));
    }else {
      super.visit(as);
    }
  }

}

