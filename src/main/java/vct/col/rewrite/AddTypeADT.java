package vct.col.rewrite;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import hre.ast.MessageOrigin;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ContractBuilder;

public class AddTypeADT extends AbstractRewriter {

  public static final String type_adt="TYPE";

  private AxiomaticDataType adt;
  
  private HashSet<String> rootclasses=new HashSet<String>();
  @SuppressWarnings("unused")
  private HashMap<String,Set<String>> subclasses=new HashMap<String, Set<String>>();
  
  public AddTypeADT(ProgramUnit source) {
    super(source);
    create.enter();
    create.setOrigin(new MessageOrigin("Generated type system ADT"));
    adt=create.adt(type_adt);
    ClassType adt_type=create.class_type(type_adt);
    adt.add_cons(create.function_decl(create.class_type(type_adt),null,"class_Object",new DeclarationStatement[0],null));
    adt.add_map(create.function_decl(
        create.primitive_type(PrimitiveSort.Boolean),
        null,
        "instanceof",
        new DeclarationStatement[]{
          create.field_decl("t1",create.class_type(type_adt)),
          create.field_decl("t2",create.class_type(type_adt))
        },
        null
    ));
    adt.add_axiom(create.axiom("object_top",create.forall(
        create.constant(true),
        create.invokation(adt_type, null,"instanceof",create.invokation(adt_type,null,"class_Object"),create.local_name("t")),
        new DeclarationStatement[]{create.field_decl("t",create.class_type(type_adt))}
    )));
    adt.add_axiom(create.axiom("object_eq",create.forall(
        create.constant(true),
        create.invokation(adt_type, null,"instanceof",create.local_name("t"),create.local_name("t")),
        new DeclarationStatement[]{create.field_decl("t",create.class_type(type_adt))}
    )));
    create.leave();
    target().add(adt);
  }

  @Override
  public void visit(Method m){
    if (m.getKind()==Method.Kind.Constructor){
      ASTClass cls=(ASTClass)m.getParent();
      currentContractBuilder=new ContractBuilder();
      currentContractBuilder.ensures(create.expression(StandardOperator.EQ,
          create.expression(StandardOperator.TypeOf,create.reserved_name(ASTReserved.Result)),
          create.invokation(create.class_type(type_adt),null,"class_"+cls.name())
      ));
    }
    //else if (!m.isStatic()) {
    //  String name=((ASTClass)m.getParent()).name;
    //  currentContractBuilder=new ContractBuilder();
    //  currentContractBuilder.ensures(create.invokation(null, null,"instanceof",
    //      create.expression(StandardOperator.TypeOf,create.reserved_name(ASTReserved.This)),
    //      create.invokation(create.class_type(type_adt),null,"class_"+name)
    //    ));
    //}
    super.visit(m);
    if (m.getKind()==Method.Kind.Constructor){
      Method c=(Method)result;
      if (c!=null && c.getBody()!=null){
        ASTClass cls=(ASTClass)m.getParent();
        ((BlockStatement)c.getBody()).prepend(create.special(ASTSpecial.Kind.Inhale,create.expression(StandardOperator.EQ,
            create.expression(StandardOperator.TypeOf,create.reserved_name(ASTReserved.This)),
            create.invokation(create.class_type(type_adt),null,"class_"+cls.name())
        )));
      }
      result=c;
    } else if (!m.isStatic()) {
      
    }
  }

  public void visit(ASTClass cl){
    super.visit(cl);
    ASTClass res=(ASTClass)result;
    adt.add_cons(create.function_decl(create.class_type(type_adt),null, "class_"+cl.name(), new DeclarationStatement[0],null));
    if (cl.super_classes.length==0){
      for(String other:rootclasses){
        adt.add_axiom(create.axiom("different_"+other+"_" + cl.name(),
            create.expression(StandardOperator.Not,
               create.invokation(create.class_type(type_adt), null,"instanceof",
                   create.invokation(create.class_type(type_adt),null,"class_"+other),
                   create.invokation(create.class_type(type_adt),null,"class_"+cl.name())))
        ));
        adt.add_axiom(create.axiom("different_"+cl.name()+"_"+other,
            create.expression(StandardOperator.Not,
               create.invokation(create.class_type(type_adt), null,"instanceof",
                   create.invokation(create.class_type(type_adt),null,"class_"+cl.name()),
                   create.invokation(create.class_type(type_adt),null,"class_"+other)))
        ));
      }
      rootclasses.add(cl.name());
    } else {
      // TODO
    }
    result=res;
  }
  
  public void visit(OperatorExpression e){
    switch(e.operator()){
    case EQ:
      if (e.arg(0).isa(StandardOperator.TypeOf)
        && e.arg(1) instanceof ClassType){
        result=create.expression(StandardOperator.EQ,rewrite(e.arg(0)),
               create.invokation(create.class_type(type_adt),null,"class_"+e.arg(1)));
      } else if(e.arg(1).isa(StandardOperator.TypeOf)
          && e.arg(0) instanceof ClassType) {
        result=create.expression(StandardOperator.EQ,rewrite(e.arg(1)),
            create.invokation(create.class_type(type_adt),null,"class_"+e.arg(0)));       
      } else {
        super.visit(e);
      }
      break;
    case Instance:
      result=create.expression(StandardOperator.And,
          create.expression(StandardOperator.NEQ,
              rewrite(e.arg(0)),
              create.reserved_name(ASTReserved.Null)
          ),
          create.invokation(create.class_type(type_adt), null,"instanceof",
            create.expression(StandardOperator.TypeOf,rewrite(e.arg(0))),
            create.invokation(create.class_type(type_adt),null,"class_"+e.arg(1))
          )
      );
      break;
    default:
      super.visit(e);
      break;
    }
  }
  
}
