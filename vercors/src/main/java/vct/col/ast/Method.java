// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast;

import hre.ast.MessageOrigin;

import java.util.*;

import vct.col.ast.PrimitiveType.Sort;
import vct.col.rewrite.MultiSubstitution;
import vct.util.ClassName;
import static hre.lang.System.Abort;
import static hre.lang.System.Debug;

/**
 * Method Declaration.
 * @author sccblom
 *
 */
public class Method extends ASTDeclaration {

  public static final String JavaConstructor = "<<constructor>>";

  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map,A arg){
    return map.map(this,arg);
  }

  /** Enumeration of kinds of methods. */
  public static enum Kind{
    Constructor,
    Predicate,
    Pure,
    Plain
  };
 
  private final Type return_type;
  private final DeclarationStatement args [];
  private final boolean var_args;
  private Hashtable<String,Contract> spec=new Hashtable<String,Contract>();
  private ASTNode body;
  public final Kind kind;
  
  public boolean usesVarArgs(){
    return var_args;
  }
  
  public Method(String name,Type return_type,Contract contract,DeclarationStatement args[],boolean varArgs,ASTNode body){
    this(Kind.Plain,name,return_type,contract,args,varArgs,body);
  }

  public Method(Kind kind, String name,String args[],boolean many,FunctionType t){
    super(name);
    this.return_type=t.getResult();
    this.args=new DeclarationStatement[args.length];
    this.var_args=many;
    for(int i=0;i<args.length;i++){
      this.args[i]=new DeclarationStatement(args[i],t.getArgument(i));
      this.args[i].setParent(this);
      this.args[i].setOrigin(new MessageOrigin("dummy origin for argument "+i));
    }
    this.kind=kind;
  }
  
  public Method(Kind kind, String name,Type return_type,Contract contract,DeclarationStatement args[],boolean varArgs,ASTNode body){
    super(name);
    this.return_type=return_type;
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
    
  public String getName(){ return name; }

  public int getArity(){ return args.length; }

  public String getArgument(int i){ return args[i].getName(); }

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
    if (object_type.args==null){
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
      for(int i=0;i<c.given.length&&i<object_type.args.length;i++){
        if (c.given[i].getType().isPrimitive(Sort.Class)){
          Debug("%s = %s",c.given[i].getName(),object_type.args[i]);
          map.put(c.given[i].getName(),(Type)object_type.args[i]);
        } else {
          Debug("skipping %s",c.given[i].getName());
        }
      }
    } else if(parent instanceof AxiomaticDataType) {
      AxiomaticDataType adt=(AxiomaticDataType)parent;
      Debug("%s: computing substitution (%s)...",object_type.getOrigin(),adt.name);
      DeclarationStatement decl[] = adt.parameters();
      for(int i=0;i<decl.length;i++){
        if (i<object_type.args.length){
          Debug("%s -> %s",decl[i].name,(Type)object_type.args[i]);
          map.put(decl[i].name,(Type)object_type.args[i]);          
        }
      }
    }
    return sigma;
  }

  @Override
  public ClassName getDeclName() {
    ASTDeclaration parent=((ASTDeclaration)getParent());
    if (parent ==null || parent instanceof AxiomaticDataType){
      return new ClassName(name);
    } else {
      return new ClassName(parent.getDeclName(),name);
    }
  }

  
  @Override
  public <T> void accept_simple(ASTVisitor<T> visitor){
    try {
      visitor.visit(this);
    } catch (Throwable t){
      if (thrown.get()!=t){
        System.err.printf("Triggered by %s:%n",getOrigin());
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
        System.err.printf("Triggered by %s:%n",getOrigin());
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
      Debug("function %s is recursive",name);
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
      for(ASTNode child:expr.args()){
        if (find(target,scanned,child)) return true;
      }
      return false;
    }
    if (node instanceof MethodInvokation){
      MethodInvokation s=(MethodInvokation)node;
      if (s.getDefinition()==target) return true;
      if (find(target,scanned,s.object)) return true;
      for(ASTNode child:s.getArgs()){
        if (find(target,scanned,child)) return true;
      }
      return s.getDefinition().find(target, scanned);
    }
    if (node instanceof Dereference){
      Dereference expr = (Dereference)node;
      return find(target,scanned, expr.object());
    }
    if (node instanceof BindingExpression){
      BindingExpression abs=(BindingExpression)node;
      if (find(target,scanned,abs.main)) return true;
      return find(target,scanned,abs.select);
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
}


