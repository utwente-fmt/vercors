// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.boogie;

import java.util.HashSet;

import hre.ast.Origin;
import hre.ast.TrackingOutput;
import vct.col.ast.*;
import vct.col.ast.PrimitiveType.Sort;
import vct.col.util.ASTUtils;

/**
 * This class contains the pretty printer for Chalice code, which is
 * based on the abstract Boogie pretty printer.
 * 
 * @author Stefan Blom
 *
 */
public class ChalicePrinter extends AbstractBoogiePrinter {
  
  boolean in_class=false;
  boolean uses_refute=false;
  int refute_count=0;
  
  public HashSet<Origin> refutes=new HashSet<Origin>();
  
  HashSet<String> classParameters=new HashSet<String>();
  
  public ChalicePrinter(TrackingOutput out){
    super(BoogieSyntax.getChalice(),out,false);
  }

  @Override
  public void visit(NameExpression n){
    if(n.getKind()==NameExpression.Kind.Reserved){
      switch(n.reserved()){
        case FullPerm:
          out.print("100");
          return;
      default:
        break;
      }
    }
    super.visit(n);
  }

  public void visit(ClassType t){
    String name[]=t.getNameFull();
    ASTNode args[]=t.getArgs();
    if (name.length>1){
      Abort("cannot deal with class name of length %d",name.length);
    }
    if (args.length > 1) {
      Abort("more than one type parameter is illegal");
    }
    out.print(t.getName());
    if (args.length==1){
      out.print("<");
      args[0].accept(this);
      out.print(">");
    }
  }
  public void visit(PrimitiveType t){
    switch (t.sort){
    case Sequence:
      out.print("seq<");
      t.getArg(0).accept(this);
      out.print(">");
      break;
    case ZFraction:
    case Fraction:
      out.printf("int");
      break;
    default:
      super.visit(t);
    }
  }

  public void visit(ASTClass c){
    String class_name=c.getName();
    if (class_name.equals("LockSet")){
      Warning("skipping LockSet class");
      return;
    }
    if (in_class) throw new Error("illegal nested class "+class_name);
    if (c.getStaticCount()>0) throw new Error("illegal static entries in "+class_name);
    Contract contract=c.getContract();
    if (contract!=null){
      for(DeclarationStatement decl:contract.given){
        if (decl.getType().isPrimitive(Sort.Class)){
          if (!classParameters.contains(decl.getName())){
            classParameters.add(decl.getName());
            out.lnprintf("class %s { }",decl.getName());
          }
        }
      }
    }
    in_class=true;
    String long_name[]=c.getFullName();
    out.printf("class %s",long_name[0]);
    for(int i=1;i<long_name.length;i++){
      out.printf("__%s",long_name[i]);
    }
    out.lnprintf(" {");
    out.incrIndent();
    int N=c.getDynamicCount();
    for(int i=0;i<N;i++) c.getDynamic(i).accept(this);
    out.decrIndent();
    if (uses_refute){
      out.println("method refute_random_bool() returns (b:bool) { }");
      uses_refute=false;
    }
    out.lnprintf("}//end class %s",class_name);
    in_class=false;
  }

  private void print_arguments(Method m,boolean function){
    DeclarationStatement args[]=m.getArgs();
    Contract c=m.getContract();
    Type result_type=m.getReturnType();
    out.printf("(");
    String next="";
    for(int i=0;i<args.length;i++){
      if (args[i].isValidFlag(ASTFlags.OUT_ARG)&&args[i].getFlag(ASTFlags.OUT_ARG)) continue;
      out.printf("%s%s: ",next,args[i].getName());
        if (i==args.length-1 && m.usesVarArgs() && function){
          out.print("seq<");
        }
      args[i].getType().accept(this);
        if (i==args.length-1 && m.usesVarArgs() && function){
          out.print(">");
        }
      next=",";
    }
    if (c!=null){
      for(int i=0;i<c.given.length;i++){
        out.printf("%s%s: ",next,c.given[i].getName());
        c.given[i].getType().accept(this);
        next=",";
      }      
    }
    if (function) {
      out.printf("):");
      result_type.accept(this);
      out.lnprintf("");
    } else {
      out.printf(") returns (");
      if (result_type.equals(Sort.Void)){
        next="";
      } else {
        out.printf("__result: ");
        result_type.accept(this);
        next=",";
      }
      for(int i=0;i<args.length;i++){
        if (args[i].isValidFlag(ASTFlags.OUT_ARG)&&args[i].getFlag(ASTFlags.OUT_ARG)) {
          out.printf("%s%s: ",next,args[i].getName());
          args[i].getType().accept(this);
          next=",";
        }
      }
      if (c!=null){
        for(int i=0;i<c.yields.length;i++){
          out.printf("%s%s: ",next,c.yields[i].getName());
          c.yields[i].getType().accept(this);
          next=",";          
        }
      }
      out.lnprintf(")");
    }
  }
  public void visit(Method m){
    String name=m.getName();
    Method.Kind kind=m.getKind();
    int N=m.getArity();
    Contract contract=m.getContract();
    if (contract!=null) post_condition=contract.post_condition;
    ASTNode body=m.getBody();
    Debug("method %s of kind %s", name, kind);
    switch(kind){
      case Constructor:{
        out.printf("method %s",name);
        print_arguments(m,false);
        out.lnprintf("  requires true; // should be access to variables");
        if (contract!=null) visit(contract,false);
        body.accept(this);        
        break;
      }
      case Predicate:{
        if (N>0){
          Fail("predicate %s (%s) has arguments",name,m.getOrigin());
        }
        if (body==null) {
          out.lnprintf("function %s_abstract():bool",name);
          out.lnprintf("predicate %s { %s_abstract() }",name,name);
        } else {
          out.lnprintf("predicate %s",name);
          // no arguments allowed for predicate!
          out.lnprintf("{");
          out.incrIndent();
          in_clause=true;
          nextExpr();
          body.accept(this);
          out.decrIndent();
          out.lnprintf("");
          out.lnprintf("}");
          in_clause=false;
        }
        break;
      }
      case Pure:{
        out.printf("function %s",name);
        print_arguments(m,true);
        if (contract!=null) visit(contract,false);
        if (body !=null) {
          if (body instanceof BlockStatement){
            Fail("function bodies cannot be blocks");
          } else {
            out.lnprintf("{");
            out.incrIndent();
            in_clause=true;
            if (contract!=null && !contract.pre_condition.equals(Contract.default_true)
                && !body.isa(StandardOperator.Unfolding)) {
              // this is an unsafe trick!
              for(ASTNode part:ASTUtils.conjuncts(contract.pre_condition,StandardOperator.Star)){
                if (!(part instanceof MethodInvokation)) continue;
                MethodInvokation mi=(MethodInvokation)part;
                if (mi.getDefinition().kind != Method.Kind.Predicate) continue;
                out.printf("unfolding ");
                nextExpr();
                current_precedence=0;
                part.accept(this);
                out.printf(" in ");
              }
            }
            nextExpr();
            body.accept(this);
            out.decrIndent();
            out.lnprintf("");
            out.lnprintf("}");
            in_clause=false;
          }
        } else {
          out.lnprintf("// abstract! ");
        }
        break;
      }
      case Plain:{
        out.printf("method %s",name);
        print_arguments(m,false);
        if (contract!=null) visit(contract,false);
        if (body==null){
          out.lnprintf("{ assume false ; /* skip body check for abstract method. */ }");
        } else {
          body.accept(this);
        }
        break;
      }
    }
  }
  
  public void visit(Dereference e){
    if (e.object() instanceof NameExpression){
      NameExpression arg1=(NameExpression)e.object();
      if (arg1.getKind()==NameExpression.Kind.Unresolved){
        Abort("unresolved name %s",arg1.getName());
      }
      if (arg1.getKind()==NameExpression.Kind.Label){
        arg1.accept(this);
        out.printf("_%s", e.field());
        return;
      }
    }
    current_precedence=0;
    super.visit(e);    
  }
    /*
    case Select:{
      // label.field should be printed as label_field, for now. 
      if (e.getArg(0) instanceof NameExpression){
        NameExpression arg1=(NameExpression)e.getArg(0);
        Debug("kind of %s is %s at %s",arg1.getName(),arg1.getKind(),arg1.getOrigin());
        if (arg1.getKind()==NameExpression.Kind.Label){
          arg1.accept(this);
          out.printf("_");
          e.getArg(1).accept(this);
          break;
        }
      }
      super.visit(e);
      break;
    }
      */
  
  public void visit(OperatorExpression e){
    switch(e.operator()){
    case Subscript:{
      e.arg(0).apply(this);
      out.print("[");
      e.arg(1).apply(this);
      out.print("]");
      break;
    }
      case Perm:{
        assert in_expr;
        ASTNode a0=e.arg(0);
        ASTNode a1=e.arg(1);
        //Warning("perm with %s",a1.getType());
        if (a1.getType().isPrimitive(Sort.ZFraction)){
          out.print("(((");
          a1.accept(this);
          out.print(")>0)==>acc(");
          a0.accept(this);
          out.print(",");
          a1.accept(this);
          out.print("))");
       } else {
          out.print("acc(");
          current_precedence=0;
          a0.accept(this);
          out.print(",");
          current_precedence=0;
          a1.accept(this);
          out.print(")");
        }
        break;
      }
      case PointsTo:{
        assert in_expr;
        ASTNode a0=e.arg(0);
        ASTNode a1=e.arg(1);
        ASTNode a2=e.arg(2);
        out.print("(acc(");
        current_precedence=0;
        a0.accept(this);
        out.print(",");
        current_precedence=0;
        a1.accept(this);
        out.print(") && ");
        current_precedence=syntax.getPrecedence(StandardOperator.EQ);
        a0.accept(this);
        out.print("==");
        current_precedence=syntax.getPrecedence(StandardOperator.EQ);
        a2.accept(this);
        out.print(")");
        break;
      }
      case Value:{
        assert in_expr;
        ASTNode a0=e.arg(0);
        out.print("rd*(");
        //out.print("rd(");
        a0.accept(this);
        out.print(")");
        //out.print(",*)");
        break;
      }
      case New:{
        assert in_expr;
        out.print("new ");
        e.arg(0).accept(this);
        break;
      }
      case Head:{
        e.arg(0).accept(this);
        out.print("[0]");
        break;
      }
      case Tail:{
        e.arg(0).accept(this);
        out.print("[1..]");
        break;
      }
      case Member:{
        if (e.arg(1).isa(StandardOperator.RangeSeq)){
          OperatorExpression range=(OperatorExpression)e.arg(1);
          out.print("(");
          range.arg(0).accept(this);
          out.print(" <= ");
          e.arg(0).accept(this);
          out.print(" && ");
          e.arg(0).accept(this);
          out.print(" < ");
          range.arg(1).accept(this);
          out.print(")");
        } else {
          super.visit(e);
        }
        break;
      }
      case Unfolding:{
        out.print("unfolding ");
        e.arg(0).accept(this);
        out.print(" in ");
        e.arg(1).accept(this);
        break;
      }
      default:{
        super.visit(e);
      }
    }
  }

  @Override
  public void visit(StructValue v) {
    if (v.type() instanceof PrimitiveType) {
      PrimitiveType t = (PrimitiveType)v.type();
      if (t.sort==Sort.Sequence){
        if (v.values().length==0) {
          out.print("nil<");
          t.getArg(0).accept(this);
          out.print(">");
        } else {
          String sep="[";
          for(int i=0;i<v.values().length;i++){
            out.print(sep);
            sep=",";
            v.value(i).accept(this);
          }
          out.print(" ]");
        }
      } else {
        Abort("illegal struct value type %s",t);
      }
    } else {
      Abort("illegal struct value type");
    }
  }
  
  /*
  public void visit(NameExpression e){
    String s=e.toString();
    if (s.equals("\\result")){
      if (current_method().kind==Method.Kind.Pure)
        out.print("result");
      else
        out.print("__result");
    } else if (s.equals("\\old")){
      out.print("old");
    } else if (s.equals("nil")) {
      out.print("nil<");
      Type t=e.getType();
      if (t==null) Abort("untyped occurrence of nil");
      t=(Type)((PrimitiveType)t).getArg(0);
      t.accept(this);
      out.print(">");
    } else {
      out.print(s);
    }
  }
*/
  @Override
  public void visit(ASTSpecial s){
    switch(s.kind){
    case Refute:{
      uses_refute=true;
      refutes.add(s.getOrigin());
      int no=refute_count++;
      out.println("call b"+no+":=refute_random_bool()");
      out.println("if (b"+no+"){");
      out.incrIndent();
      in_clause=true;
      out.printf("assert ");
      current_precedence=0;
      setExpr();
      ASTNode prop=s.args[0];
      prop.accept(this);
      out.lnprintf(";");
      in_clause=false;    
      out.decrIndent();
      out.println("}");
      break;
    }
    case Fold:{
      if (in_expr) throw new Error("fold is a statement");
      in_clause=true;
      out.printf("fold ");
      current_precedence=0;
      setExpr();
      ASTNode prop=s.args[0];
      prop.accept(this);
      out.lnprintf(";");
      in_clause=false;
      break;
    }
    case Unfold:{
      if (in_expr) throw new Error("unfold is a statement");
      in_clause=true;
      out.printf("unfold ");
      current_precedence=0;
      setExpr();
      ASTNode prop=s.args[0];
      prop.accept(this);
      out.lnprintf(";");
      in_clause=false;
      break;
    }
      case Assume:{
        if (s.args[0] instanceof MethodInvokation) {
          MethodInvokation i=(MethodInvokation)s.args[0];
          Method m=i.getDefinition();
          if (m!=null && m.kind==Method.Kind.Predicate && m.getBody()==null){
            setExpr();
            out.printf("assume ");
            i.object.accept(this);
            out.lnprintf(".%s_abstract();",i.method);
            out.printf("fold ");
            i.object.accept(this);
            out.lnprintf(".%s ;",i.method);            
            break;
          }
        }
        super.visit(s);
        break;
      }
    case Exhale:
      Abort("chalice cannot support exhale");
      break;
    case Inhale:
      Warning("chalice cannot support inhale");
      break;
    case Expression:
      if (s.args[0] instanceof MethodInvokation){
        out.print("call ");
      }
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;
    case Assert:
      Debug("assertion origin is %s",s.getOrigin());
      out.print("assert ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;
    case Fork:{
      assert !in_expr;
      ASTNode thread=s.args[0];
      String name;
      if (thread instanceof NameExpression){
        name=((NameExpression)thread).toString();
      } else {
        throw new Error("fork/join are limited to name expressions");
      }
      //TODO: fix assumed global argument!
      //possibly fork label:expression ... join label.
      //out.lnprintf("fork tok_%s := %s.run(global);",name,name);
      //Warning("first arg of method: %s",current_method().getArgType(0).toString());
      if (current_method().getArity()>0 && current_method().getArgType(0).toString().equals("Global")){
        out.lnprintf("fork tok_%s := %s.run(global);",name,name);
      } else {
        out.lnprintf("fork tok_%s := %s.run();",name,name);
      }
      break;
    }
      case Join:{
        assert !in_expr;
        ASTNode thread=s.args[0];
        String name;
        if (thread instanceof NameExpression){
          name=((NameExpression)thread).toString();
        } else {
          throw new Error("fork/join are limited to name expressions");
        }
        out.lnprintf("join tok_%s;",name);
        break;
      }

    default:
      super.visit(s);
      break;
    }
  }
  
  public void visit(MethodInvokation e){
    Method m=e.getDefinition();
    if (m==null){
      Abort("at %s: definition of invoked method unavailable",e.getOrigin());
    }
    int N=e.getArity();
    if (m.kind==Method.Kind.Predicate){
      if (N>0) Abort("predicates cannot have arguments in Chalice");
      if (e.object!=null) {
        e.object.accept(this);
        out.print(".");
      }
      out.print(e.method);
    } else if (m.kind==Method.Kind.Pure && m.usesVarArgs()) {
      if (e.object!=null) {
        e.object.accept(this);
        out.print(".");
      }
      out.print(e.method);
      out.print("(");
      String sep="";
      if (m.usesVarArgs()){
        N=m.getArity()-1;
      }
      for(int i=0;i<N;i++){
        out.print(sep);
        sep=",";
        e.getArg(i).accept(this);
      }
      if (m.usesVarArgs()){
        out.printf("%s",sep);
        sep="";
        int i=N;
        N=e.getArity();
        if (N>i){
          out.print("[");
          for(;i<N;i++){
            out.print(sep);
            sep=",";
            e.getArg(i).accept(this);
          }
          out.print("]");
        } else {
          out.print("nil<");
          m.getArgType(N).accept(this);
          out.print(">");
        }
      }
      out.print(")");
    } else {
      super.visit(e);
    }
  }

}
