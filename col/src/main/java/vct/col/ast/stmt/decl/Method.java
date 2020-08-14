// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.stmt.decl;

import java.util.*;

import hre.ast.Origin;
import scala.Option;
import scala.collection.Iterable;
import scala.collection.JavaConverters;
import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.generic.ASTList;
import vct.col.ast.util.ASTMapping;
import vct.col.ast.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.type.*;
import vct.col.ast.util.ASTVisitor;
import vct.col.ast.util.MultiSubstitution;
import vct.col.ast.util.ClassName;
import static hre.lang.System.Abort;
import static hre.lang.System.Debug;

/**
 * Method Declaration.
 * @author sccblom
 *
 */
public class Method extends ASTDeclaration {

  public static final String JavaConstructor = "<<constructor>>";
  public final Type[] signals;

  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    return map.map(this,arg);
  }

  @Override
  public Iterable<String> debugTreeChildrenFields() {
    return JavaConverters.iterableAsScalaIterable(Arrays.asList("return_type", "args", "spec", "body"));
  }

  @Override
  public Iterable<String> debugTreePropertyFields() {
    return JavaConverters.iterableAsScalaIterable(Arrays.asList("var_args", "kind"));
  }

  /** Enumeration of kinds of methods. */
  public static enum Kind{
    Constructor,
    Predicate,
    Pure,
    Plain
  };
 
  private final Type return_type;
  private DeclarationStatement[] args;
  private final boolean var_args;
  private Hashtable<String, Contract> spec=new Hashtable<String,Contract>();
  private ASTNode body;
  public final Kind kind;
  
  public boolean usesVarArgs(){
    return var_args;
  }
  
  public Method(String name, Type return_type, Type[] signals, Contract contract, DeclarationStatement args[], boolean varArgs, ASTNode body){
    this(Kind.Plain,name,return_type, signals,contract,args,varArgs,body);
  }
  
  public Method(Kind kind,
                String name,
                Type return_type,
                Type[] signals,
                Contract contract,
                DeclarationStatement[] args,
                boolean varArgs,
                ASTNode body)
  {
    super(name);
    this.return_type=return_type;
    this.signals = signals;
    this.args=Arrays.copyOf(args,args.length);
    this.var_args=varArgs;
    for(int i=0;i<args.length;i++){
      if (this.args[i].getParent()==null) this.args[i].setParent(this);
    }
    this.body=body;
    this.kind=kind;
    setContract(contract);
  }

  public Kind getKind(){ return kind; }
    
  public String getName(){ return name(); }

  public int getArity(){ return args.length; }

  public String getArgument(int i){ return args[i].name(); }

  public Type getArgType(int i){ return args[i].getType(); }

  public void setContract(Contract contract){
    setContract("this",contract);
  }
  
  public void setContract(String tag,Contract contract){
    if (contract==null) {
      spec.remove(tag);
      return;
    }
    spec.put(tag,contract);
    for(DeclarationStatement d:contract.given){
      d.setParent(this);
    }
    for(DeclarationStatement d:contract.yields){
      d.setParent(this);
    }
  }
  
  public Contract getContract(String tag){
    return spec.get(tag);
  }
  
  public Contract getContract(){
    return spec.get("this");
  }
  
  public void setBody(ASTNode body){
    this.body=body;
  }
  public ASTNode getBody(){
    return body;
  }
  public DeclarationStatement[] getArgs() {
    return args;
  }
  public Type getReturnType() {
    return return_type;
  }

  public Type[] getArgType() {
    Type res[]=new Type[args.length];
    for(int i=0;i<args.length;i++){
      res[i]=args[i].getType();
    }
    return res;
  }

  public MultiSubstitution getSubstitution(ClassType object_type) {
    Map<String,Type> map=new HashMap<String,Type>();
    MultiSubstitution sigma=new MultiSubstitution(null,map);
    if (object_type==null){
      Debug("missing object type");
      return sigma;      
    }
    if (!object_type.hasArguments()){
      Debug("object type has no arguments");
      return sigma;
    }
    ASTNode parent=getParent();
    if (parent==null){
      Debug("missing parent");
      return sigma;
    }
    if (parent instanceof ASTClass){
      Contract c=((ASTClass)parent).getContract();
      if (c==null) {
        Debug("missing contract");
        return sigma;
      }
      Debug("building map...");
      
      int i = 0;
      for (ASTNode arg : object_type.argsJava()) {
    	if (i >= c.given.length) break;
    	if (c.given[i].getType().isPrimitive(PrimitiveSort.Class)){
            Debug("%s = %s", c.given[i].name(), arg);
            map.put(c.given[i].name(), (Type)arg);
          } else {
            Debug("skipping %s", c.given[i].name());
          }
        i++;
      }
      
    } else if(parent instanceof AxiomaticDataType) {
      AxiomaticDataType adt=(AxiomaticDataType)parent;
      Debug("%s: computing substitution (%s)...",object_type.getOrigin(), adt.name());

      int i = 0;
      ASTNode args[] = object_type.argsJava().toArray(new ASTNode[0]);
      for (DeclarationStatement decl : adt.parametersJava()) {
    	if (i < args.length) {
          Debug("%s -> %s",decl.name(), (Type)args[i]);
          map.put(decl.name(), (Type)args[i]);          
        }
    	i++;
      }
      
    }
    return sigma;
  }

  @Override
  public ClassName getDeclName() {
    ASTDeclaration parent=((ASTDeclaration)getParent());
    if (parent ==null || parent instanceof AxiomaticDataType){
      return new ClassName(name());
    } else {
      return new ClassName(parent.getDeclName(), name());
    }
  }

  
  @Override
  public <T> void accept_simple(ASTVisitor<T> visitor){
    try {
      visitor.visit(this);
    } catch (Throwable t){
      if (thrown.get()!=t){
        Debug("Triggered by %s:",getOrigin());
        thrown.set(t);
     }
      throw t;
    }
  }
  
  @Override
  public <T> T accept_simple(ASTMapping<T> map){
    try {
      return map.map(this);
    } catch (Throwable t){
      if (thrown.get()!=t){
        Debug("Triggered by %s:",getOrigin());
        thrown.set(t);
      }
      throw t;
    }
  }
 

  public boolean isRecursive() {
    if (this.body==null) return true;
    HashSet<Method> scanned=new HashSet<Method>();
    boolean res=find(this,scanned,body);
    if (res){
      Debug("function %s is recursive", name());
    }
    return res;
  }

  private boolean find(Method target,HashSet<Method> scanned){
    if (this==target) return true;
    if (scanned.contains(this)) return false;
    scanned.add(this);
    if (this.body==null) return false;
    return find(target,scanned,this.body);
  }
  
  private static boolean find(Method target,HashSet<Method> scanned,ASTNode node){
    if (node instanceof NameExpression) return false;
    if (node instanceof ConstantExpression) return false;
    if (node instanceof OperatorExpression){
      OperatorExpression expr=(OperatorExpression)node;
      for (ASTNode child : expr.argsJava()) {
        if (find(target,scanned,child)) return true;
      }
      return false;
    }
    if (node instanceof MethodInvokation){
      MethodInvokation s=(MethodInvokation)node;
      if (s.getDefinition()==target) return true;
      if (find(target,scanned,s.object())) return true;
      for(ASTNode child:s.getArgs()){
        if (find(target,scanned,child)) return true;
      }
      return s.getDefinition().find(target, scanned);
    }
    if (node instanceof Dereference){
      Dereference expr = (Dereference)node;
      return find(target,scanned, expr.obj());
    }
    if (node instanceof BindingExpression){
      BindingExpression abs=(BindingExpression)node;
      if (find(target,scanned,abs.main())) return true;
      return find(target,scanned,abs.select());
    }
    if (node instanceof PrimitiveType){
      return false;
    }
    if (node instanceof BlockStatement){
      //TODO this breaks is resources uses blocks!
      return false;
    }
    Abort("missing case in isRecursive: %s",node.getClass());
    return true;
  }

  public boolean isOverloaded() {
    ASTClass cl=(ASTClass)getParent();
    return cl.isOverloaded(name());
  }

  /**
   * Method is synchronized if one of the annotations is the Synchronized keyword.
   */
  public boolean isSynchronized() {
    ASTList annotations = annotations();

    for (int i = 0; i < annotations.size(); i++) {
      ASTNode annotation = annotations.get(i);
      if (annotation instanceof NameExpression) {
        NameExpression modifier = (NameExpression) annotation;
        if (modifier.isReserved(ASTReserved.Synchronized)) {
          return true;
        }
      }
    }

    return false;
  }

  public void prependArg(Origin o, String name, Type type, boolean outArg) {
    DeclarationStatement newArg = new DeclarationStatement(name, type, Option.empty());
    newArg.setOrigin(o);
    if (outArg) {
      newArg.setFlag(ASTFlags.OUT_ARG, true);
    }

    ArrayList<DeclarationStatement> argList = new ArrayList<>(Arrays.asList(args));
    argList.add(0, newArg);
    args = argList.toArray(new DeclarationStatement[argList.size()]);
  }

  /**
   * True if the method "can throw" as defined in the Java spec, which means it must have exceptions
   * in the throws attribute.
   * See: https://docs.oracle.com/javase/specs/jls/se7/html/jls-11.html#jls-11.2
   */
  public boolean canThrow() {
    return signals.length > 0;
  }

  /**
   * True if the method is specified to throw, i.e. has signals clauses, or can throw according
   * to the java spec.
   *
   * Note that a signals clause does not guarantee an exception is thrown.
   * Example: "signals (Exception e) false;" guarantees an "Exception" will never be thrown.
   */
  public boolean canThrowSpec() {
    return getContract().signals.length > 0 || canThrow();
  }
}


