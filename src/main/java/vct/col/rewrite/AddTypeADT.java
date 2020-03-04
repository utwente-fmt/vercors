package vct.col.rewrite;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import hre.ast.MessageOrigin;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ContractBuilder;
import vct.col.util.ASTFactory;

public class AddTypeADT extends AbstractRewriter {

  public static final String type_adt="TYPE";
  public static final String javaObjectName = "java_DOT_lang_DOT_Object";

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
    adt.add_map(create.function_decl(
            create.class_type(type_adt),
            null,
            "direct_superclass",
            new DeclarationStatement[] {
                    create.field_decl("t", create.class_type(type_adt))
            },
            null
    ));
    addInstanceOfAxiom();
    create.leave();
    target().add(adt);
  }

  public void addInstanceOfAxiom() {
    adt.add_axiom(create.axiom("instanceof_superclass", create.forall(
            create.constant(true),
            create.expression(StandardOperator.EQ,
                    create.domain_call(type_adt, "instanceof", create.local_name("t"), create.local_name("u")),
                    create.expression(StandardOperator.Or,
                            create.expression(StandardOperator.EQ, create.local_name("t"), create.local_name("u")),
                            create.expression(StandardOperator.EQ,
                                    create.domain_call(type_adt, "direct_superclass", create.local_name("t")),
                                    create.local_name("u")
                                    )
                            )
                    ),
            create.field_decl("t", create.class_type(type_adt)),
            create.field_decl("u", create.class_type(type_adt))
    )));
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
    // Assume classes extend Object by default
    if (cl.super_classes.length==0) {
      addDirectSuperclassAxiom(new ClassType(cl.getName()), new ClassType(javaObjectName));
    } else if (cl.super_classes.length == 1 && cl.super_classes[0].getName().equals(javaObjectName)){
      // And otherwise a class can only extend Object
      addDirectSuperclassAxiom(new ClassType(cl.getName()), cl.super_classes[0]);
    } else {
      // TODO
      Abort("Cannot extend more than one class or extend something else than Object");
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

  private void addDirectSuperclassAxiom(ClassType child, ClassType parent) {
    String child_adt_constructor = "class_" + child.getName();
    String parent_adt_constructor = "class_" + parent.getName();
    adt.add_axiom(create.axiom(child.getName() + "_direct_superclass",
            create.expression(StandardOperator.EQ,
              create.domain_call(type_adt, "direct_superclass", create.domain_call(type_adt, child_adt_constructor)),
              create.domain_call(type_adt, parent_adt_constructor)
            )));
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

  /**
   * Encodes an instanceof operation as encoded by the AddTypeADT phase. Intended to be reused if any instanceof
   * checks need to be added later on.
   * @param create ASTFactory used at the call site
   * @param expr The expression the typeof operator will be applied to. Will be copied into the resulting expression
   * @param type Currently assumed to be a class type. TODO: but if extended to pvl this could be any type? Need to consider nullness...
   * @return Condition that is true if expr instanceof type holds
   */
  public static ASTNode expr_instanceof(ASTFactory<?> create, AbstractRewriter copy_rw, ASTNode expr, ClassType type) {
    return create.expression(StandardOperator.And,
            create.expression(StandardOperator.NEQ,
              copy_rw.rewrite(expr),
              create.reserved_name(ASTReserved.Null)
              ),
            create.invokation(create.class_type(type_adt), null,"instanceof",
              create.expression(StandardOperator.TypeOf, copy_rw.rewrite(expr)),
              create.invokation(create.class_type(type_adt),null,"class_" + type
              )
            ));
  }
}
