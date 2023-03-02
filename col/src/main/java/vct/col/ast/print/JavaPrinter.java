// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.print;

import hre.ast.TrackingOutput;
import hre.lang.HREError;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringEscapeUtils;

import vct.col.ast.langspecific.c.*;
import vct.col.ast.stmt.composite.Switch.Case;
import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StringValue;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.BeforeAfterAnnotations;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;
import vct.col.ast.syntax.JavaDialect;
import vct.col.ast.syntax.JavaSyntax;
import vct.col.ast.util.ClassName;
import hre.util.LambdaHelper;

/** 
 * This class contains a pretty printer for Java code.
 * 
 * @author Stefan Blom
 * 
 */
public class JavaPrinter extends AbstractPrinter {
  
  public final JavaDialect dialect;
  public JavaPrinter(TrackingOutput out,JavaDialect dialect){
    super(JavaSyntax.getJava(dialect),out);
    this.dialect=dialect;
  }

  public void visit(TypeVariable v){
    out.print(v.name());
  }
  
  public void pre_visit(ASTNode node){
    super.pre_visit(node);
    for(NameExpression lbl:node.getLabels()){
      nextExpr();
      lbl.accept(this);
      out.printf(":");
    }
    printAnnotations(node,false);
  }

  private void printAnnotations(ASTNode node, boolean printJavaModifier) {
    if (node.annotated()) for(ASTNode ann:node.annotations()) {
      if (ann==null){
        out.printf(" <null annotation> ");
      } else {
        if(printJavaModifier && isJavModifier(ann) || !printJavaModifier && !isJavModifier(ann)) {
          nextExpr();
          ann.accept(this);
          out.printf(" ");
        }
      }
    }
  }

  private boolean isJavModifier(ASTNode a) {
    if(a instanceof NameExpression) {
      NameExpression n = (NameExpression) a;
      return n.isReserved(ASTReserved.Private) || n.isReserved(ASTReserved.Protected) || n.isReserved(ASTReserved.Public) || n.isReserved(ASTReserved.Synchronized);
    } else return false;
  }
  
  @Override
  public void visit(TryCatchBlock tcb){
    out.print("try");
    tcb.main().accept(this);
    for (CatchClause cb : tcb.catchesJava()) {
      cb.accept(this);
    }
    if (tcb.after() != null){
      out.print(" finally ");
      tcb.after().accept(this);
    }
    out.println("");
  }
  
  @Override
  public void visit(NameSpace ns){
    if (!ns.name().equals(NameSpace.NONAME)) {
      out.printf("package %s;",ns.getDeclName().toString("."));
      out.println("");
    }
    for(NameSpace.Import i:ns.imports){
      out.printf("import %s%s",i.static_import?"static ":"",new ClassName(i.name).toString("."));
      if(i.all){
        out.println(".*;");
      } else {
        out.println(";");
      }
    }
    for(ASTNode n:ns){
      n.accept(this);
    }
  }
  
  @Override
  public void visit(ActionBlock ab){
    out.printf("action(");
    nextExpr(); ab.history().accept(this);
    out.printf(",");
    nextExpr(); ab.fraction().accept(this);
    out.printf(",");
    nextExpr(); ab.process().accept(this);
    out.printf(",");
    nextExpr(); ab.action().accept(this);
    
    // visit all (key/value) entries in `ab.map` (via a lambda)
    ab.foreach(LambdaHelper.fun((key,val) -> {
      out.printf(", %s, ", key);
      nextExpr();
      val.accept(this);
    }));
    
    out.printf(")");
    ab.block().accept(this);
  }
  
  public void post_visit(ASTNode node){
    if (node instanceof BeforeAfterAnnotations && !(node instanceof LoopStatement)){
      BeforeAfterAnnotations baa=(BeforeAfterAnnotations)node;
      if (baa.get_before()!=null && baa.get_before().size()>0 || baa.get_after()!=null && baa.get_after().size()>0){
        out.printf("/*@ ");
        BlockStatement tmp=baa.get_before();
        if (tmp!=null && tmp.size()>0) {
          out.printf("with ");
          tmp.accept(this);
        }
        tmp=baa.get_after();
        if (tmp!=null && tmp.size()>0) {
          out.printf("then ");
          tmp.accept(this);      
        }
        out.printf(" */");
      }
    }
    super.post_visit(node);
  }
   
  @Override 
  public void visit(Axiom axiom){
    out.printf("axioms %s: ", axiom.name());
    axiom.rule().accept(this);
    out.println(";");
  }

  @Override
  public void visit(AxiomaticDataType adt){
    out.printf("ADT %s [", adt.name());
    String sep="";
    for (DeclarationStatement d : adt.parametersJava()) {
      out.printf("%s%s", sep, d.name());
      sep=", ";
    }
    out.println("] {");
    out.incrIndent();
    out.println("//constructors");
    for (Method f : adt.constructorsJava()) {
      f.accept(this);
    }
    out.println("//mappings");
    for(Method f:adt.mappingsJava()){
      f.accept(this);
    }
    out.println("//axioms");
    for(Axiom ax:adt.axiomsJava()){
      ax.accept(this);
    }
    out.decrIndent();
    out.println("}");
  }
  @Override
  public void visit(ASTSpecial s){
    ASTSpecial e=s;
    switch(s.kind){
    case Refute:{
      out.printf("refute ");
      current_precedence=0;
      setExpr();
      ASTNode prop=e.getArg(0);
      prop.accept(this);
      break;
    }
    case Assume:{
      out.printf("assume ");
      current_precedence=0;
      setExpr();
      ASTNode prop=e.getArg(0);
      prop.accept(this);
      break;
    }
    case HoarePredicate:{
        out.printf("/*{ ");
        current_precedence=0;
        setExpr();
        ASTNode prop=e.getArg(0);
        prop.accept(this);
        out.lnprintf(" }*/");
        break;        
    }
    case Unfold:{
      out.printf("//@ unfold ");
      current_precedence=0;
      setExpr();
      ASTNode prop=e.getArg(0);
      prop.accept(this);
      out.println(";");
      break;
    }
    case Fold:{
        out.printf("//@ fold ");
        current_precedence=0;
        setExpr();
        ASTNode prop=e.getArg(0);
        prop.accept(this);
        out.println(";");
        break;
      }
    case Use:{
      out.printf("//@ use ");
      current_precedence=0;
      setExpr();
      ASTNode prop=e.getArg(0);
      prop.accept(this);
      break;
    }
    case Witness:{
      out.printf("//@ witness ");
      current_precedence=0;
      setExpr();
      ASTNode prop=e.getArg(0);
      prop.accept(this);
      break;        
    }
    case Apply:{
        out.printf("//@ apply ");
        current_precedence=0;
        setExpr();
        ASTNode prop=e.getArg(0);
        prop.accept(this);
        break;
      }
    case QED:{
        out.printf("//@ qed ");
        current_precedence=0;
        setExpr();
        ASTNode prop=e.getArg(0);
        prop.accept(this);
        break;
      }
    case Open:{
      out.printf("//@ open ");
      current_precedence=0;
      setExpr();
      ASTNode prop=e.getArg(0);
      prop.accept(this);
      break;
    }
    case Close:{
      out.printf("//@ close ");
      current_precedence=0;
      setExpr();
      ASTNode prop=e.getArg(0);
      prop.accept(this);
      break;
    }
    case Fork:{
      setExpr();
      if(s.args.length > 0) {
        s.args[0].accept(this);
        out.print(".");
      }
      out.printf("fork()");
      out.println(";");
      current_precedence=0;
      break;
    }
    case Join:{
      setExpr();
      if(s.args.length > 0) {
        s.args[0].accept(this);
        out.print(".");
      }
      out.printf("join()");
      out.println(";");
      current_precedence=0;
      break;
    }
    case ThreadPoolExecutor:{
      setExpr();
      out.printf("ExecutorService EXECUTOR_SERVICE = Executors.newFixedThreadPool(");
      s.args[0].accept(this);
      out.printf(");");
      break;
    }
    case ThreadExecute:{
      setExpr();
      out.printf("EXECUTOR_SERVICE.execute(() -> ");
      s.args[0].accept(this);
      out.printf(".compute());");
      break;
    }
    case ThreadPoolExecutorShutDown:{
      out.printf("EXECUTOR_SERVICE.shutdown();\n    EXECUTOR_SERVICE.awaitTermination(120L, TimeUnit.SECONDS);");
      break;
    }
    case Goto:
      out.print("goto ");
      s.args[0].accept(this);
      break;
    case Label:
      out.print("label ");
      s.args[0].accept(this);
      break;
    case With:
      out.print("WITH");
      s.args[0].accept(this);
      break;
    case Then:
      out.print("THEN");
      s.args[0].accept(this);
      break;
    case Expression:
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;
    case Assert:
      out.print("//@ assert ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;
    case Lock:
      out.print("lock ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;
    case Unlock:
      out.print("unlock ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;
    case Wait:
      setExpr();
      if(s.args.length > 0) {
        s.args[0].accept(this);
        out.print(".");
      }
      out.print("wait()");
      out.println(";");
      break;
    case Notify:
      setExpr();
      if(s.args.length > 0) {
        s.args[0].accept(this);
        out.print(".");
      }
      out.print("notify()");
      out.println(";");
      break;
    case Import:
      out.print("import ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;    
    case Throw:
      out.print("throw ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;
    case Exhale:
      out.print("exhale ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;    
    case Inhale:
      out.print("inhale ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;    
    case CreateHistory:
      out.print("create ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;    
    case CreateFuture:
      out.print("create ");
      setExpr();
      s.args[0].accept(this);
      out.printf(",");
      s.args[1].accept(this);
      out.println(";");
      break;    
    case DestroyHistory:
      out.print("destroy ");
      setExpr();
      s.args[0].accept(this);
      out.printf(",");
      s.args[1].accept(this);
      out.println(";");
      break;    
    case DestroyFuture:
      out.print("destroy ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;    
    case SplitHistory:
      out.print("split ");
      setExpr();
      s.args[0].accept(this);
      out.printf(",");
      s.args[1].accept(this);
      out.printf(",");
      s.args[2].accept(this);
      out.printf(",");
      s.args[3].accept(this);
      out.printf(",");
      s.args[4].accept(this);
      out.println(";");
      break;    
    case MergeHistory:
      out.print("merge ");
      setExpr();
      s.args[0].accept(this);
      out.printf(",");
      s.args[1].accept(this);
      out.printf(",");
      s.args[2].accept(this);
      out.printf(",");
      s.args[3].accept(this);
      out.printf(",");
      s.args[4].accept(this);
      out.println(";");
      break;    
    case CSLSubject:
      out.print("csl_subject ");
      setExpr();
      s.args[0].accept(this);
      out.println(";");
      break;    
    case SpecIgnoreStart:
      out.println("spec_ignore {");
      break;
    case SpecIgnoreEnd:
      out.println("} spec_ignore");
      break;
    default:
      super.visit(s);
      break;
    }
  }
  
  @Override
  public void visit(ClassType t){
    out.print(t.getFullName());
    if (t.hasArguments()) {
      setExpr();
      out.print("<");
      ASTNode args[] = t.argsJava().toArray(new ASTNode[0]);
			if (args.length>=1) {
				args[0].accept(this);
				for(int i = 1; i < args.length; i++){
					out.print(",");
					args[i].accept(this);
				}
      }
      out.print(">");
    }
  }
  
  public void visit(FunctionType t){
	Type[] types = t.paramsJava().toArray(new Type[0]);
	  
    for (int i=0;i<types.length-1;i++) {
    	types[i].accept(this);
      out.print(",");
    }
    types[types.length].accept(this);
    
    out.print("->");
    t.result().accept(this);
  }

  public void visit(BindingExpression e){
    String binder=null;
    switch(e.binder()){
      case Forall:
        binder="\\forall";
        break;
      case Exists:
        binder="\\exists";
        break;
      case Star:
        binder="\\forall*";
        break;
      case Let:
        binder="\\let";
        break;
      case Sum:
        binder="\\sum";
        break;
      case SetComp:
        //TODO Have a correct way of outputting this.
        out.printf("setcomp");
        break;
      default:
        Abort("binder %s unimplemented",e.binder());
    }
    setExpr();
    out.printf("(%s ",binder);
    int N=e.getDeclCount();
    for(int i=0;i<N;i++){
      if (i>0) out.printf(",");
      DeclarationStatement decl=e.getDeclaration(i);
      decl.getType().accept(this);
      out.printf(" %s", decl.name());
      if (decl.initJava() != null) {
        out.printf("= ");
        decl.initJava().accept(this);
      }
    }
    if (e.triggers()!=null){
      for(ASTNode trigger[]:e.javaTriggers()){
        out.printf("{");
        trigger[0].accept(this);
        for(int i=1;i<trigger.length;i++){
          out.printf(",");
          trigger[i].accept(this);
        }
        out.printf("}");
      }
    }
    out.printf(";");
    if (e.select()!=null){
      e.select().accept(this);
      out.printf(";");
    }
    e.main().accept(this);
    out.printf(")");
  }

  public void visit(BlockStatement block){
    out.lnprintf("{");
    out.incrIndent();
    int N=block.getLength();
    for(int i=0;i<N;i++){
      ASTNode statement=block.getStatement(i);
      if (statement.isValidFlag(ASTFlags.GHOST) && statement.isGhost()){
        out.enterGhost();
      }
      statement.accept(this);
      if (self_terminating(statement)){
        out.clearline();
      } else {
        out.lnprintf(";");
      }
      if (statement.isValidFlag(ASTFlags.GHOST) && statement.isGhost()){
        out.leaveGhost();
      }     
    }
    out.decrIndent();
    out.printf("}");
  }

  public void visit(ASTClass cl){
    visit(cl.getContract());
    if(cl.isValidFlag(ASTFlags.FINAL) && cl.getFlag(ASTFlags.FINAL))
      out.printf("final ");
    switch(cl.kind){
    case Plain:
      out.printf("class %s",cl.getName());
      break;
    case Abstract:
      out.printf("abstract class %s",cl.getName());
      break;
    case Interface:
      out.printf("interface %s",cl.getName());
      break;
    case Kernel:
      out.printf("kernel %s",cl.getName());
      break;
    case Record:
      out.printf("record %s",cl.getName());
      break;
    default:
      Abort("unexpected class kind %s",cl.kind);
    }
    if (cl.parameters.length>0){
      String sep="<";
      for(DeclarationStatement d:cl.parameters){
        out.print(sep);
        if(d.getType().isPrimitive(PrimitiveSort.Class)){
          out.print(d.name());
        } else {
          d.accept(this);
        }
        sep=",";
      }
      out.print(">");
    }
    List<ClassType> sup = Arrays.stream(cl.super_classes).filter(classEl -> !classEl.getName().equals("Object")).collect(Collectors.toList());
    if (sup.size()>0) {
      out.printf("  extends %s",cl.super_classes[0]);
      for(int i=1;i<cl.super_classes.length;i++){
        out.printf(", %s",sup.get(i));
      }
      out.lnprintf("");
    }
    if (cl.implemented_classes.length>0) {
      out.printf("  implements %s",cl.implemented_classes[0]);
      for(int i=1;i<cl.implemented_classes.length;i++){
        out.printf(", %s",cl.implemented_classes[i]);
      }
      out.lnprintf("");
    }
    out.lnprintf("{");
    out.incrIndent();
    for(ASTNode item:cl){
      if (item.isStatic()){
        if (item instanceof DeclarationStatement) out.printf("static ");
      }
      item.accept(this);
      out.println("");
    }
    out.decrIndent();
    out.lnprintf("}");
    out.println("");
  }

  @Override
  public void visit(Contract contract) {
    if (contract!=null && !contract.isEmpty()){
      out.lnprintf("/*@");
      out.incrIndent();
      super.visit(contract);
      out.decrIndent();
      out.lnprintf("@*/");
    }
  }

  public void visit(SignalsClause sc) {
    out.printf("signals (");
    sc.type().accept(this);
    out.printf(" %s) ",sc.name());
    nextExpr();
    sc.condition().accept(this);
    out.lnprintf(";");
  }

  public void visit(DeclarationStatement s){
    ASTNode expr = s.initJava();
    nextExpr();
    s.getType().accept(this);
    out.printf(" %s", s.name());
    if (expr!=null){
      out.printf(" = ");
      nextExpr();
      expr.accept(this);
    }
    if (!in_expr) out.printf(";");
  }

  public void visit(Method m){
    int N=m.getArity();
    Type result_type=m.getReturnType();
    String name=m.getName();
    Contract contract=m.getContract();
    boolean predicate=m.getKind()==Method.Kind.Predicate;
    boolean resource = result_type instanceof PrimitiveType && ((PrimitiveType) result_type).sort == PrimitiveSort.Resource;
    if (predicate && !resource){
      if (contract!=null) {
        out.lnprintf("//ignoring contract of predicate");
        Debug("ignoring contract of predicate");
      }
      out.lnprintf("/*@");
      out.incrIndent();
      out.print("predicate ");
    }
    if (contract!=null && dialect!=JavaDialect.JavaVeriFast && !predicate && !contract.isEmpty()){
      visit(contract);
    }
    if(resource) {
      out.printf("/*@ ");
    }
    printAnnotations(m,true);
    for(int i=1;i<0xF000;i<<=1){
      if (m.isValidFlag(i)){
        if (m.getFlag(i)){
          switch(i){
          case ASTFlags.STATIC:
          case ASTFlags.FINAL:
            break;
          case ASTFlags.INLINE:
            out.printf("inline ");
          case ASTFlags.PUBLIC:
            out.printf("public ");
            break;
          case ASTFlags.THREAD_LOCAL:
            out.printf("/*@ thread_local @*/ ");
            break;
          case ASTFlags.EXTERN:
            out.printf("/*@ extern @*/ ");
            break;
          case ASTFlags.UNIQUE:
            out.printf("unique ");
            break;
          default:
            throw new HREError("unknown flag %04x",i);
          }
        }
      }
    }
    if (!m.isValidFlag(ASTFlags.STATIC)) {
      out.printf("static?? ");
    } else if (m.isStatic()) out.printf("static ");
    if (m.isValidFlag(ASTFlags.FINAL) && m.getFlag(ASTFlags.FINAL)){
      out.printf("final ");
    }
    if (m.getKind()==Method.Kind.Pure && !resource){
      out.printf("/*@ pure */");
    }
    if (m.getKind()==Method.Kind.Constructor){
    } else {
      result_type.accept(this);
      out.printf(" ");
    }
    out.printf("%s(",name);
    if (N>0) {
      DeclarationStatement args[]=m.getArgs();
      if (args[0].isValidFlag(ASTNode.GHOST) && args[0].isGhost()){ out.printf("/*@ ghost */"); }
      if (args[0].isValidFlag(ASTFlags.OUT_ARG) && args[0].getFlag(ASTFlags.OUT_ARG)){ out.printf("/*@ out */"); }
      m.getArgType(0).accept(this);
      if (N==1 && m.usesVarArgs()){
        out.print(" ...");
      }
      out.printf(" %s",m.getArgument(0));
      for(int i=1;i<N;i++){
        out.printf(",");
        if (args[i].isValidFlag(ASTNode.GHOST) && args[i].isGhost()){ out.printf("/*@ ghost */"); }
        if (args[i].isValidFlag(ASTFlags.OUT_ARG) && args[i].getFlag(ASTFlags.OUT_ARG)){ out.printf("/*@ out */"); }
        m.getArgType(i).accept(this);
        if (i==N-1 && m.usesVarArgs()){
          out.print(" ...");
        }
        out.printf(" %s",m.getArgument(i));
      }
    }
    out.printf(")");
    if (contract!=null && dialect==JavaDialect.JavaVeriFast && !predicate){
      visitVeriFast(contract);
    }
    if (m.signals.length > 0) {
      out.printf(" throws ");
      m.signals[0].accept(this);
      if (m.signals.length > 1) {
        for (int i = 1; i < m.signals.length; i++) {
          Type t = m.signals[i];
          out.printf(", ");
          t.accept(this);
        }
      }
    }
    ASTNode body=m.getBody();
    if (body==null) {
      out.lnprintf("{ \nthrow new UnsupportedOperationException(\"Method %s has not been implemented\");\n}",m.getName());
    } else if (body instanceof BlockStatement) {
      body.accept(this);
      out.lnprintf("");
    } else if(predicate || resource) {
      out.printf("=");
      body.accept(this);
      out.println(";");
      out.decrIndent();
      out.lnprintf("*/");
    } else {
      out.println("{");
      if(body instanceof OperatorExpression || body instanceof ConstantExpression) {
        out.println("return ");
      }
      nextExpr();
      body.accept(this);
      out.println(";");
      out.println("}");
    }
  }

  private void visitVeriFast(Contract contract) {
    out.printf("//@ requires ");
    nextExpr();
    contract.pre_condition.accept(this);
    out.lnprintf(";");
    out.printf("//@ ensures ");
    nextExpr();
    contract.post_condition.accept(this);
    out.lnprintf(";");
  }

  public void visit(IfStatement s){
      int N=s.getCount();
      out.printf("if (");
      nextExpr();
      s.getGuard(0).accept(this);
      out.print(") ");
      s.getStatement(0).accept(this);
      if (!self_terminating(s.getStatement(0))){
        out.printf(";");
      }
      for(int i=1;i<N;i++){
        if (self_terminating(s.getStatement(i-1))){
          out.printf(" ");
        }
        if (i==N-1 && s.getGuard(i)==IfStatement.elseGuard()){
          out.printf("else ");
        } else {
          out.printf(" else if (");
          nextExpr();
          s.getGuard(i).accept(this);
          out.lnprintf(") ");
        }
        s.getStatement(i).accept(this);
        if (!self_terminating(s.getStatement(i))){
          out.lnprintf(";");
        }        
      }
  }

  private boolean self_terminating(ASTNode s) {
    return (s instanceof BlockStatement)
        || (s instanceof IfStatement)
        || (s instanceof LoopStatement)
        || (s instanceof ASTSpecial)
        || (s instanceof DeclarationStatement)
        || (s instanceof ParallelRegion);
  }

  public void visit(AssignmentStatement s){
    setExpr();
    s.location().accept(this);
    out.printf(" = ");
    s.expression().accept(this);
  }

  public void visit(ReturnStatement s){
    ASTNode expr=s.getExpression();
    if (expr==null){
      out.lnprintf("return");
    } else {
      out.printf("return ");
      setExpr();
      expr.accept(this);
    }
    if (s.get_after()!=null && s.get_after().size() > 0){
      out.printf("/*@ ");
      out.printf("then ");
      s.get_after().accept(this);
      out.printf(" */");
    }
  }

  public void visit(Lemma lemma){
      out.printf("/*@ lemma ");
      lemma.block().accept(this);
      out.lnprintf(" */");
  }
  
  public void visit(OperatorExpression e){
    switch(dialect){
    case JavaVerCors:
      visitVerCors(e);
      break;
    case JavaVeriFast:
      visitVeriFast(e);
      break;
      default:
        super.visit(e);
    }
  }
  private void visitVeriFast(OperatorExpression e){
    switch(e.operator()){
    case PointsTo:{
      if (!(e.arg(1) instanceof ConstantExpression)||
              !(e.arg(1)).equals(1)) {
        out.printf("[");
        e.arg(1).accept(this);
        out.printf("]");
      }
      e.arg(0).accept(this);
      out.printf(" |-> ");
      e.arg(2).accept(this);
      break;
    }
    default:{
      super.visit(e);
    }}
  }
  private void visitVerCors(OperatorExpression e){
    switch(e.operator()){
      case NewSilver:{
        out.print("new ");
        // no break on purpose!
      }
      case Wrap:{
        out.print("(");
        String sep="";
        for (ASTNode arg : e.argsJava()) {
          out.print(sep);
          sep=",";
          arg.accept(this);
        }
        out.print(")");
        break;
      }
      case New:{
        out.printf("new ");
        e.arg(0).accept(this);
        out.printf("()");
        break;
      }
      case ITE:{
        out.printf("(");
        nextExpr();
        super.visit(e);
        out.printf(")");
        break;
      }
      default:{
        super.visit(e);
      }
    }
  }
  
  @Override
  public void visit(TypeExpression t){
    switch (t.operator()) {
    case Extends:
      out.printf("? extends ");
      t.firstType().accept(this);
      return;
    case Super:
      out.printf("? super ");
      t.firstType().accept(this);
      return;
    default:
      super.visit(t);
    }
  }
  
  @Override
  public void visit(Switch s){
    out.printf("switch (");
    nextExpr();
    s.expr.accept(this);
    out.println("){");
    for(Case c:s.cases){
      for(ASTNode n:c.cases){
        if (n == null) {
          out.println("default: ");
        } else {
          out.printf("case ");
          nextExpr();
          n.accept(this);
          out.println(":");
        }
      }
      out.incrIndent();
      for(ASTNode n:c.stats){
        n.accept(this);
        out.println("");
      }
      out.decrIndent();
    }
    out.println("}");
  }
  
  @Override
  public void visit(StructValue v) {
    setExpr();
    if (v.type() != null) {
      v.type().accept(this);
    }
    out.print("{");
    String sep="";
    for (int i = 0; i < v.valuesLength(); i++) {
      out.print(sep);
      sep=",";
      v.value(i).accept(this);
    }
    out.print("}");
  }
  
  public void visit(ForEachLoop s){
    visit(s.getContract());
    out.printf("for(");
    for(DeclarationStatement decl:s.decls){
      nextExpr();
      decl.apply(this);
      out.printf(";");
    }
    nextExpr();
    s.guard.apply(this);
    out.printf(")");
    s.body.apply(this);
  }

  private void visitForStatementList(BlockStatement s) {
    boolean first = true;
    for(ASTNode n : s){
      if(!first) {
        out.printf(", ");
      }

      nextExpr();
      n.accept(this);

      first = false;
    }
  }

  public void visit(LoopStatement s){
    visit(s.getContract());
    ASTNode tmp;
    if (s.getInitBlock()!=null || s.getUpdateBlock()!=null){
      out.printf("for(");

      if(s.getInitBlock() != null) {
        if (s.getInitBlock() instanceof BlockStatement) {
          visitForStatementList((BlockStatement) s.getInitBlock());
        } else {
          nextExpr();
          s.getInitBlock().accept(this);
        }
      }
      out.printf(";");

      nextExpr();
      s.getEntryGuard().accept(this);
      out.printf(";");
      
      if((s.getUpdateBlock())!=null) {
        if(s.getUpdateBlock() instanceof BlockStatement){
          visitForStatementList((BlockStatement) s.getUpdateBlock());
        } else {
          nextExpr();
          s.getUpdateBlock().accept(this);
        }
      }
      out.printf(")");
    } else if ((tmp=s.getEntryGuard())!=null) {
      out.printf("while(");
      nextExpr();
      tmp.accept(this);
      out.printf(")");      
    } else {
      out.printf("do");
    }
    if (s.get_before()!=null && s.get_before().size()>0 || s.get_after()!=null  && s.get_after().size()>0){
      out.println("");
      out.println("/*@");
      out.incrIndent();
    }
    if (s.get_before()!=null && s.get_before().size()>0){
      out.printf("with ");
      s.get_before().accept(this);
      out.println("");
    }
    if (s.get_after()!=null  && s.get_after().size()>0){
      out.printf("then ");
      s.get_after().accept(this);
      out.println("");
    }
    if (s.get_before()!=null && s.get_before().size()>0 || s.get_after()!=null  && s.get_after().size()>0){
      out.decrIndent();
      out.println("@*/");
    }
    tmp=s.getBody();
    if (!(tmp instanceof BlockStatement)) { out.printf(" "); }
    tmp.accept(this);
    tmp=s.getExitGuard();
    if (tmp!=null){
      out.printf("while(");
      nextExpr();
      tmp.accept(this);
      out.lnprintf(")");      
    }
  }
  
  private void print_tuple(ASTNode ... args){
    out.print("(");
    String sep="";
    for(ASTNode n:args){
      out.print(sep);
      nextExpr();
      n.accept(this);
      sep=",";
    }
    out.print(")");
  }
  
  public void visit(MethodInvokation s){
    if (s.method().equals(Method.JavaConstructor)){
      setExpr();
      out.print("new ");
      s.dispatch().accept(this);
      print_tuple(s.getArgs());
    } else {
      super.visit(s);
    }
  }

  public void visit(Dereference e){
    e.obj().accept(this);
    out.printf(".%s", e.field());
  }
  
  public void visit(PrimitiveType t){
	int nrofargs = t.nrOfArguments();
	  
    switch(t.sort){
      case Pointer:{
        t.firstarg().accept(this);
        out.printf("*");
        break;
      }
      case Array:
        t.firstarg().accept(this);
        switch(nrofargs){
        case 1:
            out.printf("[]");
            return;
        case 2:
          out.printf("[/*");
          t.secondarg().accept(this);
          out.printf("*/]");
          return;
        default:
            Fail("Array type constructor with %d arguments instead of 1 or 2",nrofargs);
        }
      case Cell:
        if (nrofargs==2){
          out.printf("cell<");
          t.firstarg().accept(this);
          out.printf(">[");
          t.secondarg().accept(this);
          out.printf("]");
          break;
        }
        if (nrofargs!=1){
          Fail("Cell type constructor with %d arguments instead of 1",nrofargs);
        }
        t.firstarg().accept(this);
        break;
      case Option:
        if (nrofargs!=1){
          Fail("Option type constructor with %d arguments instead of 1",nrofargs);
        }
        t.firstarg().accept(this);
        break;
      case Map:
        if (nrofargs!=2){
          Fail("Map type constructor with %d arguments instead of 2",nrofargs);
        }
        out.printf("Map<");
        if(t.firstarg() instanceof PrimitiveType && ((PrimitiveType) t.firstarg()).sort == PrimitiveSort.Integer)
          out.printf(((PrimitiveType)t.firstarg()).sort.toString());
        else t.firstarg().accept(this);
        out.printf(",");
        if(t.secondarg() instanceof PrimitiveType && ((PrimitiveType) t.secondarg()).sort == PrimitiveSort.Integer)
          out.printf(((PrimitiveType)t.secondarg()).sort.toString());
        else t.secondarg().accept(this);
        out.printf(">");
        break;
      case Tuple:
        if (nrofargs!=2){
          Fail("Tuple type constructor with %d arguments instead of 2",nrofargs);
        }
        out.printf("tuple<");
        t.firstarg().accept(this);
        out.printf(",");
        t.secondarg().accept(this);
        out.printf(">");
        break;
      case Sequence:
        if (nrofargs!=1){
          Fail("Sequence type constructor with %d arguments instead of 1",nrofargs);
        }
        out.printf("List<");
        if(t.firstarg() instanceof PrimitiveType && ((PrimitiveType) t.firstarg()).sort == PrimitiveSort.Integer)
          out.printf(((PrimitiveType)t.firstarg()).sort.toString());
        else t.firstarg().accept(this);
        out.printf(">");
        break;
      case Set:
        if (nrofargs!=1){
          Fail("Set type constructor with %d arguments instead of 1",nrofargs);
        }
        out.printf("set<");
        t.firstarg().accept(this);
        out.printf(">");
        break;
      case Bag:
        if (nrofargs!=1){
          Fail("Bag type constructor with %d arguments instead of 1",nrofargs);
        }
        out.printf("bag<");
        t.firstarg().accept(this);
        out.printf(">");
        break;
      case CVarArgs:
        out.printf("...");
        break;
      default:
        super.visit(t);
    }
  }
  
  @Override
  public void visit(ParallelAtomic pa){
    out.printf("atomic (");
    String sep="";
    
    for (ASTNode item : pa.synclistJava()) {
      out.printf("%s", sep);
      sep = ",";
      nextExpr();
      item.apply(this);
    }
    
    out.printf(")");
    pa.block().accept(this);
  }

  @Override
  public void visit(ParallelBlock pb){
    int j = 0;
    for (DeclarationStatement iter : pb.itersJava()) {
      iter.accept(this);
      if (j > 0) out.printf(",");
      j++;
    }
    
    if (pb.depslength() > 0){
      out.printf(";");
      pb.dependency(0).accept(this);
      for (int i = 1; i < pb.depslength(); i++) {
        out.printf(",");
        pb.dependency(i).accept(this);
      }
    }
    if (pb.contract() != null) {
      visit(pb.contract());
    }
    pb.block().accept(this);
  }

  @Override
  public void visit(ParallelBarrier pb){
    if (pb.contract() == null) {
      Fail("parallel barrier with null contract!");
    } else {
      out.printf("barrier(%s;%s){", pb.label(), pb.invs());
      out.println("");
      out.incrIndent();
      visit(pb.contract());
      out.decrIndent();
      if (pb.body() == null ) {
        out.println("{ }");
      } else {
        pb.body().accept(this);
      }
      
    }
  }
  @Override
  public void visit(ParallelInvariant pb) {
    out.printf("invariants %s (", pb.label());
    nextExpr();
    pb.inv().accept(this);
    out.printf(")");
    pb.block().accept(this);
  }
  @Override
  public void visit(ParallelRegion region){
    if (region.contract() != null) {
      out.println("parallel");
      region.contract().accept(this);
      out.println("{");
    } else {
      out.println("parallel {");
    }
    for (ParallelBlock pb : region.blocksJava()) {
      out.incrIndent();
      pb.accept(this);
      out.println("");
      out.decrIndent();
    }
    out.println("}");
  }
  
  public void visit(ConstantExpression ce){
    if (ce.value() instanceof StringValue){
      out.print("\""+StringEscapeUtils.escapeJava(ce.toString())+"\"");
    } else {
      out.print(ce.toString());
    }
  }

  @Override
  public void visit(VariableDeclaration decl){
    decl.basetype.accept(this);
    String sep=" ";
    for(ASTDeclaration dd:decl.get()){
      out.print(sep);
      sep=",";
      if (dd instanceof DeclarationStatement){
        DeclarationStatement d = (DeclarationStatement)dd;
        out.print(d.name());
        ASTNode init = d.initJava();
        if (init!=null){
          out.print(" = ");
          setExpr();
          init.accept(this);
        }
      } else {
        out.print("TODO");
      }
    }
  }
  
  @Override
  public void visit(FieldAccess a) {
    setExpr();
    if (a.value() == null) {
      out.printf("((");
    }
    if (a.object() != null) {
      a.object().apply(this);
      out.printf(".");
    }
    out.printf("%s", a.name());
    if (a.value() != null) {
      out.printf(" := ");
      a.value().apply(this);
    } else {
      out.printf("))");
    }
  }
  
  @Override
  public void visit(VectorBlock v){
    out.print("vec(");
    nextExpr();
    v.iter().accept(this);
    out.println(")");
    v.block().apply(this);
  }
  
  @Override
  public void visit(Constraining c){
    out.print("constraining(");
    String sep = "";
    
    for (NameExpression n : c.varsJava()) {
      out.print(sep);
      nextExpr();
      n.accept(this);
      sep=",";
    }
    
    out.print(")");
    c.block().accept(this);
  }

  private void visitNames(List<String> names) {
    boolean first = true;
    for(String name : names) {
      if(!first) out.print(", ");
      first = false;
      out.print(name);
    }
  }

  private void visitOmpOptions(List<OMPOption> options) {
    for(OMPOption option : options) {
      out.print(" ");
      if(option instanceof OMPNoWait$) {
        out.print("nowait");
      } else if(option instanceof OMPPrivate) {
        out.print("private(");
        visitNames(((OMPPrivate) option).namesJava());
        out.print(")");
      } else if(option instanceof OMPShared) {
        out.print("shared(");
        visitNames(((OMPShared) option).namesJava());
        out.print(")");
      } else if(option instanceof OMPSimdLen) {
        out.printf("simdlen(%d)", ((OMPSimdLen) option).len());
      } else if(option instanceof OMPNumThreads) {
        out.printf("num_threads(%d)", ((OMPNumThreads) option).len());
      } else if(option instanceof OMPSchedule) {
        out.print("schedule(");
        OMPScheduleChoice choice = ((OMPSchedule) option).schedule();
        if(choice instanceof OMPStatic$) {
          out.print("static");
        } else {
          out.print("??");
        }
        out.print(")");
      } else {
        out.print("??");
      }
    }
  }

  @Override
  public void visit(OMPParallel parallel) {
    out.print("#pragma omp parallel");
    visitOmpOptions(parallel.optionsJava());
    out.newline();
    parallel.block().accept(this);
  }

  @Override
  public void visit(OMPSection section) {
    out.println("#pragma omp section");
    section.block().accept(this);
  }

  @Override
  public void visit(OMPSections sections) {
    out.println("#pragma omp sections");
    sections.block().accept(this);
  }

  @Override
  public void visit(OMPFor loop) {
    out.print("#pragma omp for");
    visitOmpOptions(loop.optionsJava());
    out.newline();
    loop.loop().accept(this);
  }

  @Override
  public void visit(OMPParallelFor loop) {
    out.print("#pragma omp parallel for");
    visitOmpOptions(loop.optionsJava());
    out.newline();
    loop.loop().accept(this);
  }

  @Override
  public void visit(OMPForSimd loop) {
    out.print("#pragma omp for simd");
    visitOmpOptions(loop.optionsJava());
    out.newline();
    loop.loop().accept(this);
  }

  @Override
  public void visit(InlineQuantifierPattern pattern) {
    out.print("{:");
    pattern.inner().apply(this);
    out.print(":}");
  }

  public void visit(Synchronized sync) {
    out.print("synchronized (");
    nextExpr();
    sync.expr().accept(this);
    out.lnprintf(")");
    sync.statement().accept(this);
  }

  @Override
  public void visit(CatchClause cc) {
    out.print("catch (");
    nextExpr();
    boolean first = true;
    for(Type t : cc.javaCatchTypes()) {
      if(!first) out.print(" | ");
      t.accept(this);
      first = false;
    }
    out.print(" ");
    out.print(cc.name());
    out.print(")");
    cc.block().accept(this);
  }
}
