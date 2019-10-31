package vct.col.rewrite;

import java.util.ArrayList;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.expr.constant.BooleanValue;
import vct.col.ast.type.ClassType;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.expr.Dereference;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.Type;
import vct.util.Configuration;
import static vct.col.ast.type.ASTReserved.*;

/**
 * Explicit permission encoding.
 * 
 * @author Stefan Blom
 *
 */
public class ExplicitPermissionEncoding extends AbstractRewriter {
 
  public ExplicitPermissionEncoding(ProgramUnit source) {
    super(source);
  }

  public AbstractRewriter copy_rw=new AbstractRewriter(source()){
    public void visit(NameExpression name){
      if (name.getKind()==NameExpression.Kind.Label){
        result=new NameExpression(name.getName());
        result.setOrigin(create.getOrigin());
      } else {
        super.visit(name);
      }
    }
  };
  
  public void visit(DeclarationStatement s) {
    Type t=s.getType();
    ASTNode tmp;
    if (t instanceof ClassType) {
      ClassType ct=(ClassType)t;
      String name[]=ct.getNameFull();
      switch(name.length){
      case 2 : tmp=create.class_type(name[0]+"_"+name[1]); break;
      default : tmp=create.class_type(name); break;
      }
    } else {
      tmp=t.apply(copy_rw);
    }
    t=(Type)tmp;
    String name = s.name();
    ASTNode init = s.initJava();
    if (init!=null) init=init.apply(this);
    DeclarationStatement res=new DeclarationStatement(name,t,init);
    res.setOrigin(s.getOrigin());
    result=res; return ;
  }

  
  public void visit(Method m){
     if (m.kind==Method.Kind.Predicate && !m.getName().equals(WandEncoder.VALID)){
      // Witnesses must be generated for every predicate because
      // a predicate without argument could invoke one that uses them,
      // which requires using a witness.
      ASTClass pred_class=(ASTClass)(new PredicateClassGenerator(source())).rewrite((ASTNode)m);
      Contract c=((ASTClass)m.getParent()).getContract();
      if (c!=null){
        pred_class.setContract(copy_rw.rewrite(c));
      }
      target().add(pred_class);
      result=null;
    } else {
      Contract c=m.getContract();
      if (c==null) c=ContractBuilder.emptyContract();
      final ContractBuilder cb=new ContractBuilder();
      cb.given(rewrite(c.given));
      cb.yields(rewrite(c.yields));
      final ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
      for(DeclarationStatement arg:m.getArgs()){
        args.add(rewrite(arg));
      }
      ClauseEncoding clause_rw;
      clause_rw=new ClauseEncoding(source()){
        public void visit(MethodInvokation i){
          if (i.labels()==1){
            NameExpression lbl=i.getLabel(0);
            String pred_name=lbl.getName();
            Type t=create.class_type(i.object.getType()+"_"+i.method);
            DeclarationStatement decl=new DeclarationStatement(pred_name,t,create.reserved_name(Null));
            decl.setOrigin(i);
            cb.given(decl);
          }
          super.visit(i);
        }        
      };
      cb.requires(c.pre_condition.apply(clause_rw));
      clause_rw=new ClauseEncoding(source()){
        public void visit(MethodInvokation i){
          if (i.labels()==1){
            NameExpression lbl=i.getLabel(0);
            String pred_name=lbl.getName();
            Type t=create.class_type(i.object.getType()+"_"+i.method);
            DeclarationStatement decl=new DeclarationStatement(pred_name,t,create.reserved_name(Null));
            decl.setOrigin(i);
            cb.yields(decl);
          }
          super.visit(i);
        }
      };
      cb.ensures(c.post_condition.apply(clause_rw));
      ASTNode body=rewrite(m.getBody());
      result=create.method_kind(
          m.kind,
          rewrite(m.getReturnType()),
          cb.getContract(),
          m.getName(),
          args.toArray(new DeclarationStatement[0]),
          m.usesVarArgs(),
          body);
    }
  }
  
  public void visit(final LoopStatement s){
    final BlockStatement block=create.block();
    AbstractRewriter clause_rw=new ClauseEncoding(source()){
      public void visit(MethodInvokation i){
        if (i.labels()==1){
          NameExpression lbl=i.getLabel(0);
          String label_name=lbl.getName();
          String pred_name=i.method;
          String class_name=((ASTClass)i.getDefinition().getParent()).getName();
          Type t=create.class_type(class_name+"_"+pred_name);
          DeclarationStatement decl=new DeclarationStatement(label_name,t);
          decl.setOrigin(s);
          decl.setFlag(ASTFlags.GHOST, true);
          block.addStatement(decl);
        }
        super.visit(i);
      }        
    };
    LoopStatement res=new LoopStatement();
    ASTNode tmp;
    tmp=s.getInitBlock();
    if (tmp!=null) res.setInitBlock(tmp.apply(this));
    tmp=s.getUpdateBlock();
    if (tmp!=null) res.setUpdateBlock(tmp.apply(this));
    tmp=s.getEntryGuard();
    if (tmp!=null) res.setEntryGuard(tmp.apply(this));
    tmp=s.getExitGuard();
    if (tmp!=null) res.setExitGuard(tmp.apply(this));
    for(ASTNode inv:s.getInvariants()){
      res.appendInvariant(inv.apply(clause_rw));
    }
    res.fixate();
    tmp=s.getBody();
    res.setBody(tmp.apply(this));
    res.setOrigin(s.getOrigin());
    res.set_before(copy_rw.rewrite(s.get_before()));
    res.set_after(rewrite(s.get_after()));
    block.addStatement(res);
    result=block;
  }
   
  public void visit(NameExpression name){
    if (name.getKind()==NameExpression.Kind.Label){
      result=new NameExpression(name.getName());
      result.setOrigin(create.getOrigin());
      return;
    }
    super.visit(name);
  }
  
  private AbstractRewriter clause_rw=new ClauseEncoding(source());
  
  @Override
  public void visit(ASTSpecial s){
    ASTSpecial e=s;
    switch(s.kind){
    case Assert:
    case Inhale:
    case Exhale:
      result=clause_rw.rewrite(s);
      break;
    case Witness:{
      ASTNode arg1=e.getArg(0);
      if (arg1.labels()!=1){
        Fail("Witness must have precisely one label.");
      }
      String lbl=arg1.getLabel(0).getName();
      if (arg1 instanceof MethodInvokation){
        // instantiate predicate witnesses only
        MethodInvokation pred=(MethodInvokation)arg1;
        String pred_name=pred.method;
        String class_name=((ASTClass)pred.getDefinition().getParent()).getName();
        Type t;
        if (pred.dispatch==null){
          t=create.class_type(class_name+"_"+pred_name);
        } else {
          t=create.class_type(class_name+"_"+pred_name+"_at_"+pred.dispatch);
        }
        result=create.field_decl(lbl, t);
      } else {
        super.visit(e);
      }
      break;
    }
    case Unfold:{
      ASTNode arg1=e.getArg(0);
      if (arg1.labels()==1){
        for(NameExpression lbl:arg1.getLabels()){
          result=create.block(
              create.special(ASTSpecial.Kind.Assert,clause_rw.rewrite(arg1)),
              create.special(ASTSpecial.Kind.Unfold,
              create.invokation(create.local_name(lbl.getName()), null, "valid")
              )
          );
          return;
        }
      } else {
        super.visit(e);
      }
      break;
    }
    case Fold:{
      ASTNode arg1=e.getArg(0);
      if (arg1.labels()==1){
        NameExpression lbl=arg1.getLabel(0);
        MethodInvokation pred=(MethodInvokation)arg1;
        String pred_name=pred.method;
        String class_name=((ASTClass)pred.getDefinition().getParent()).getName();
        Type t=create.class_type(class_name+"_"+pred_name);
        ArrayList<ASTNode> args=new ArrayList<ASTNode>();
        ArrayList<ASTNode> cons_args=new ArrayList<ASTNode>();
        args.add(pred.object.apply(copy_rw));
        cons_args.add(pred.object.apply(copy_rw));
        BlockStatement block=create.block(
            create.assignment(create.local_name(lbl.getName()),create.new_object((ClassType)t)),
            create.assignment(
                create.dereference(create.local_name(lbl.getName()),"ref"),
                rewrite(pred.object)
            )
        );
        DeclarationStatement decls[]=pred.getDefinition().getArgs();
        for (int i=0;i<decls.length;i++){
          block.addStatement(create.assignment(
              create.dereference(create.local_name(lbl.getName()),decls[i].name()),
              rewrite(pred.getArg(i))
          ));
          args.add(pred.getArg(i).apply(copy_rw));
          cons_args.add(pred.getArg(i).apply(copy_rw));
        }
        ASTNode pred_args[]=pred.getArgs();
        for(int i=decls.length;i<pred_args.length;i++){
          block.addStatement(create.assignment(
              create.dereference(create.local_name(lbl.getName()),pred_args[i].getLabel(0).getName()),
              rewrite(pred_args[i])
          ));
          cons_args.add(rewrite(pred_args[i]));
        }
        block.addStatement(
            create.special(e.kind,
                create.invokation(create.local_name(lbl.getName()), null, "valid"))
        );
        block.addStatement(
            create.special(ASTSpecial.Kind.Assert,
                create.invokation(create.local_name(lbl.getName()), null, "check",args.toArray(new ASTNode[0])))
        );
        if (Configuration.witness_constructors.get()){
          result=create.assignment(create.local_name(lbl.getName()),create.new_object((ClassType)t,cons_args.toArray(new ASTNode[0])));
        } else {
          result=block;
          result.setGhost(true);
        }
        return;
      } else {
        super.visit(e);
      }
      break;
    }

    default:
      super.visit(s);
    }
  }
  
}

class ClauseEncoding extends AbstractRewriter {

  public ClauseEncoding(ProgramUnit source) {
    super(source);
  }

  public AbstractRewriter copy_rw=new AbstractRewriter(source()){
    public void visit(NameExpression name){
      if (name.getKind()==NameExpression.Kind.Label){
        result=new NameExpression(name.getName());
        result.setOrigin(create.getOrigin());
      } else {
        super.visit(name);
      }
    }
  };

  public void visit(MethodInvokation i) {
    if (i.getDefinition()==null){
      //Abort("Missing definition of %s",i.method);
      Warning("Ignoring missing definition of %s",i.method);
      result=copy_rw.rewrite(i);
    } else if (i.getDefinition().getKind()!=Method.Kind.Predicate){
      result=copy_rw.rewrite(i);
    } else {
      if (i.labels()==0){
        if (i.method.equals(WandEncoder.VALID)){
          result=copy_rw.rewrite(i);
          return;
        }
        Abort("At %s: every predicate invokation with must be labeled.",i.getOrigin());
      }
      NameExpression lbl=i.getLabel(0);
      ASTNode body=neq(create.unresolved_name(lbl.getName()),create.reserved_name(Null));
      body=star(body,invoke(create.unresolved_name(lbl.getName()),"valid"));
      ASTNode args[];
      if (i.getDefinition().isStatic()){
        args=rewrite(i.getArgs());
      } else {
        args=rewrite(i.object,i.getArgs());
      }
      body=star(body,invoke(
              create.unresolved_name(lbl.getName()),
              ("check"),
              args
          ));
      
      result=body;
      auto_labels=false;
    }
  }
}

class PredicateClassGenerator extends AbstractRewriter {
  public AbstractRewriter copy_rw=new AbstractRewriter(source()){};
  private String class_name;
  private String pred_name;
  private ASTClass pred_class;
  private boolean in_use=false;
  private int condition_level=0;
  
  public PredicateClassGenerator(ProgramUnit source){
    super(source);
  }
  
  public void visit(Method m){
    if (in_use) {
      Abort("Predicate class generator already in use.");
    }
    in_use=true;
    
    class_name=((ASTClass)m.getParent()).getName();
    pred_name=m.getName();
    String pred_class_name=class_name+"_"+pred_name;
    pred_class=create.ast_class(pred_class_name,ClassKind.Plain,null,null,null);
    String tmp[]=((ASTClass)m.getParent()).getFullName();
    Type class_type=create.class_type(tmp);
    tmp[tmp.length-1]=pred_class_name;
    
    // Start with ref field:
    if (!m.isStatic()) pred_class.add_dynamic(create.field_decl("ref",class_type));
    DeclarationStatement args[]=m.getArgs();
    ArrayList<ASTNode> cons_req=new ArrayList<ASTNode>();
    if (!m.isStatic()) cons_req.add(create.expression(StandardOperator.NEQ,create.unresolved_name("ref"),create.reserved_name(Null)));
    // Note that permission for fields will be added later.
    
    // Add arguments as fields:
    for (int i=0;i<args.length;i++){
      pred_class.add_dynamic(args[i].apply(copy_rw));
      if (args[i].getType().isPrimitive(PrimitiveSort.Fraction)){
        cons_req.add(less(constant(0),name(args[i])));
        cons_req.add(lte(name(args[i]),constant(100)));
      }
      if (args[i].getType().isPrimitive(PrimitiveSort.ZFraction)){
        cons_req.add(lte(constant(0),name(args[i])));
        cons_req.add(lte(name(args[i]),constant(100)));
      }
    }
    
    // Rewrite the body, which will cause fields to be created.
    ArrayList<ASTNode> valid_body=new ArrayList<ASTNode>();
    if (m.getBody()==null){
      pred_class.add_dynamic(create.predicate("abstract_valid", null , new DeclarationStatement[0] ));
      valid_body.add(create.invokation(null,null, ("abstract_valid"), new DeclarationStatement[0]));
    } else {
      ASTNode body=m.getBody();
      boolean non_true=true;
      if (body instanceof ConstantExpression){
        ConstantExpression e= (ConstantExpression)body;
        if (e.value() instanceof BooleanValue){
          BooleanValue b=(BooleanValue)e.value();
          non_true = !b.value();
        }
      }
      if (non_true) valid_body.add(rewrite(m.getBody()));
    }
    valid_body.addAll(cons_req);
    // Add permissions to render all fields immutable.
    for(DeclarationStatement field:pred_class.dynamicFields()){
      //valid_body.add(0,create.expression(StandardOperator.Perm,create.field_name(field.getName()),create.constant(100)));
      valid_body.add(0,create.expression(StandardOperator.Value,create.field_name(field.name())));
    }
    // Add valid predicate;
    pred_class.add_dynamic(create.predicate("valid", create.fold(StandardOperator.Star, valid_body) , new DeclarationStatement[0] ));
    
    // Prepare check function;
    ContractBuilder cb=new ContractBuilder();
    cb.requires(create.invokation(null,null, ("valid"), new DeclarationStatement[0]));
    ASTNode check_body;
    if (m.isStatic()){
      check_body=create.constant(true);
    } else {
      check_body=create.expression(StandardOperator.EQ,create.field_name("ref"),create.local_name("object"));
    }
    for (DeclarationStatement decl:m.getArgs()){
      ASTNode field=create.dereference(create.reserved_name(This),decl.name());
      check_body=create.expression(StandardOperator.And,check_body,
          create.expression(StandardOperator.EQ,field,create.local_name(decl.name())));
    }
    DeclarationStatement check_decls[];
    if (m.isStatic()){
      check_decls=rewrite(m.getArgs());
    } else {
      check_decls=rewrite(create.field_decl("object",class_type),m.getArgs());
    }
    // Add check function;
    pred_class.add_dynamic(create.function_decl(
        create.primitive_type(PrimitiveSort.Boolean),
        cb.getContract(),
        "check",
        check_decls,
        check_body
    ));
    
    // Prepare constructor
    final BlockStatement cons_body=create.block();
    final ArrayList<DeclarationStatement> cons_decls=new ArrayList<DeclarationStatement>();
    cb=new ContractBuilder();
    cb.ensures(create.invokation(null,null, ("valid"), new ASTNode[0]));
    int N=m.getArity();
    ASTNode check_args[];
    int ofs;
    if (m.isStatic()) {
      check_args=new ASTNode[N];
      ofs=0;
    } else {
      check_args=new ASTNode[N+1];
      ofs=1;
      cons_decls.add(create.field_decl("ref",class_type));
      ASTNode field=create.dereference(create.reserved_name(This),"ref");
      check_args[0]=create.local_name("ref");
      cons_body.addStatement(create.assignment(field, check_args[0]));
    }
    for(int i=0;i<N;i++){
      cons_decls.add(create.field_decl(m.getArgument(i),m.getArgType(i)));
      ASTNode field=create.dereference(create.reserved_name(This),m.getArgument(i));
      check_args[i+ofs]=create.local_name(m.getArgument(i));
      cons_body.addStatement(create.assignment(field, check_args[i+ofs]));
    }
    cb.requires(cons_req);
    if (m.getBody()!=null) {
      cb.requires(m.getBody().apply(new ClauseEncoding(source()){
        public void visit(NameExpression e){
          if (e.isReserved(This)){
            result=create.local_name("ref");
          } else {
            super.visit(e);
          }
        }
        public void visit(MethodInvokation i){
          super.visit(i);
          if (i.labels()==1){
            String name=i.getLabel(0).getName();
            ClassType type=(ClassType)i.object.getType();
            String type_name[]=type.getNameFull();
            type_name[type_name.length-1]+="_"+i.method;
            type=create.class_type(type_name);
            DeclarationStatement decl=create.field_decl(name,type);
            decl.setGhost(true);
            cons_decls.add(decl);
            ASTNode field=create.dereference(create.reserved_name(This),name);
            cons_body.addStatement(create.assignment(field, create.local_name(name)));
          }
        }
      }));
    } else {
      cons_body.addStatement(create.special(ASTSpecial.Kind.Assume,
          create.invokation(create.reserved_name(This),null, ("abstract_valid"), new ASTNode[0])));
    }
    cb.ensures(create.invokation(null,null, ("check"), check_args));
    cons_body.addStatement(create.special(ASTSpecial.Kind.Fold,
        create.invokation(create.reserved_name(This),null, ("valid"), new ASTNode[0])));
    if (Configuration.witness_constructors.get()){
      pred_class.add_dynamic(create.method_kind(Method.Kind.Constructor,
          create.primitive_type(PrimitiveSort.Void),
          cb.getContract(),
          pred_class_name,
          cons_decls.toArray(new DeclarationStatement[0]),
          cons_body
      ));
    } else {
      // Add default constructor.
      create.addZeroConstructor(pred_class);
    }
    
    // Add getters;
    ArrayList<Method> getters=new ArrayList<Method>();
    for(DeclarationStatement field:pred_class.dynamicFields()){
      cb=new ContractBuilder();
      cb.requires(create.invokation(null,null, ("valid"), new DeclarationStatement[0]));
      getters.add(create.function_decl(
          field.getType(),
          cb.getContract(),
          "get_"+field.name(),
          new DeclarationStatement[0],
          create.field_name(field.name())));
    }
    for(Method decl:getters){
      pred_class.add_dynamic(decl);
    }
    result=pred_class;
  }

  public void visit(MethodInvokation call){
    String field_name=null;
    if (call.object instanceof Dereference) {
      field_name=((Dereference)call.object).field();
    } else if (call.object instanceof NameExpression){
      field_name=((NameExpression)call.object).getName();
      if (!field_name.equals("This")) {
        Abort("unexpected field name %s",field_name);
      }
    } else {
      Abort("could not get field name at %s",call.object.getOrigin());
    }
    if (call.labels()==0){
      Fail("unlabeled invokation of %s",call.method);
    }
    String label_name=call.getLabel(0).getName();
    if (pred_class.find_field(label_name)!=null){
      Abort("label %s declared twice",label_name);
    }
    String tmp[]=((ClassType)call.object.getType()).getNameFull();
    
    tmp[tmp.length-1]=tmp[tmp.length-1]+"_"+call.method;
    Type pred_type=create.class_type(tmp);

    pred_class.add_dynamic(create.field_decl(label_name,pred_type));
    

    ASTNode exists=create.expression(StandardOperator.NEQ,create.field_name(label_name),create.reserved_name(Null)); 
    ASTNode valid=create.invokation(create.field_name(label_name), null, ("valid"));
    ASTNode check=create.invokation(create.field_name(label_name), null, ("check"),
        rewrite(call.object,call.getArgs()));
    auto_labels=false;
    result=create.expression(StandardOperator.Star,create.expression(StandardOperator.Star,exists,valid),check);
  }

  public void visit(NameExpression e){
    NameExpression.Kind kind=e.getKind();
    String name=e.getName();
    switch(kind){
      case Local:
      case Argument:
        result=create.unresolved_name(name);
        break;
      case Reserved:{
        if (e.reserved()==This){
          result=create.dereference(create.reserved_name(This),"ref");
          break;
        }
      }
      default:
        super.visit(e);
        break;
    }
  }
  
  public void visit(OperatorExpression e){
    switch(e.operator()){
      case Perm:
      case PointsTo:
        if (condition_level==0){
          ASTNode tmp=e.arg(0);
          if (tmp instanceof Dereference){
            Dereference field=(Dereference)tmp;
            tmp=field.obj();
            /*
            if (tmp instanceof NameExpression && ((NameExpression)tmp).getName().equals("this")){
              String name=field.field;
              Debug("adding getter %s_get_%s",pred_name,name);
              ContractBuilder cb=new ContractBuilder();
              cb.given(copy_rw.rewrite(pred_decl.getArgs()));
              cb.given(create.field_decl("req",create.class_type(class_name+"_"+pred_name)));
              cb.requires(create.expression(StandardOperator.NEQ,
                  create.local_name("req"),create.reserved_name(Null)));
              cb.requires(
                  create.invokation(
                      create.local_name("req"),null,
                      ("valid"),new ASTNode[0]
                  )
              );
              int N=pred_decl.getArity();
              ASTNode args[]=new ASTNode[N+1];
              args[0]=create.reserved_name(This);
              for(int i=0;i<N;i++){
                args[i+1]=create.local_name(pred_decl.getArgument(i));
              }
              cb.requires(
                  create.invokation(
                      create.local_name("req"),null,
                      ("check"),
                      args
                  )
              );
              protected_fields.add(name);
              Method getter=create.function_decl(
                  field.getType(),
                  cb.getContract(),
                  pred_name+"_get_"+name,
                  new DeclarationStatement[0],
                  create.block(
                      create.expression(StandardOperator.Unfold,
                          create.invokation(
                              create.local_name("req"),null,
                              ("valid"),new ASTNode[0]
                          )
                      ),
                      create.return_statement(copy_rw.rewrite(field))
                  )
              );
              master.add_dynamic(getter);
            }
            */
          }
        }
        super.visit(e);
        return;
      case Implies:
        condition_level++;
        super.visit(e);
        condition_level--;
        return;        
      default:
        super.visit(e);
        return;
    }
  }
}
