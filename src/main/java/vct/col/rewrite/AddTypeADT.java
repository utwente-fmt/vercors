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
  public static final String objectName = "java_DOT_lang_DOT_Object";
  public static final String objectCons = "class_" + objectName;

  private AxiomaticDataType adt;
  
  private HashSet<String> rootclasses=new HashSet<String>();
  @SuppressWarnings("unused")
  private HashMap<String,Set<String>> subclasses=new HashMap<String, Set<String>>();
  
  public AddTypeADT(ProgramUnit source) {
    super(source);
    create.enter();
    create.setOrigin(new MessageOrigin("Generated type system ADT"));
    adt=create.adt(type_adt);
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
    addTypeConstructor(cl);
    if (cl.super_classes.length==0) {
      addDirectSuperclassAxiom(cl);
    } else {
      // TODO
    }
    result=res;
  }

  private void addTypeConstructor(ASTClass cl) {
    adt.add_unique_cons(create.function_decl(
            create.class_type(type_adt),
            null,
            "class_"+cl.name(),
            new DeclarationStatement[0],
            null
            ));
  }

  private void addDirectSuperclassAxiom(ASTClass cl) {
    String cl_adt_constructor = "class_" + cl.name();
    String type_var = "t";
    // Axiom: forall t: TYPE :: (t == Object || t == cl) ? instanceof(cl, t) : !instanceof(cl, t)
    // In other words: cl is only an instance of object and cl, and nothing else
    // This will need to be significantly enhanced to allow for a type system with a partial order/inheritance
    adt.add_axiom(create.axiom(cl.name() + "_direct_superclass",
            create.forall(
                    create.constant(true),
                    create.expression(StandardOperator.ITE,
                            create.expression(StandardOperator.Or,
                                    create.expression(StandardOperator.EQ,
                                            create.local_name(type_var),
                                            create.domain_call(type_adt, objectCons)
                                            ),
                                    create.expression(StandardOperator.EQ,
                                            create.local_name(type_var),
                                            create.domain_call(type_adt, cl_adt_constructor)
                                            )
                            ),
                            create.domain_call(type_adt, "instanceof",
                                    create.domain_call(type_adt, cl_adt_constructor),
                                    create.local_name(type_var)
                                    ),
                            create.expression(StandardOperator.Not,
                                    create.domain_call(type_adt, "instanceof",
                                            create.domain_call(type_adt, cl_adt_constructor),
                                            create.local_name(type_var)
                                            )
                                    )
                            ),
                    create.field_decl(type_var, create.class_type(type_adt))
                    )
            ));
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
