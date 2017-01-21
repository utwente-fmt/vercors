package vct.col.rewrite;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Map;
import java.util.Map.Entry;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import vct.col.ast.*;
import vct.col.ast.ASTSpecial.Kind;
import vct.col.ast.Switch.Case;
import vct.col.util.ASTFactory;
import vct.col.util.ASTUtils;
import vct.col.util.NameScanner;

/**
 * This abstract rewriter copies the AST it is applied to.
 * 
 * @author Stefan Blom
 */ 
public class AbstractRewriter extends AbstractVisitor<ASTNode> {

  private static ThreadLocal<AbstractRewriter> tl=new ThreadLocal<AbstractRewriter>();

  public static Hashtable<String,Type> free_vars(ASTNode ... nodes) {
    Hashtable<String,Type> vars=new Hashtable<String,Type>();
    NameScanner scanner=new NameScanner(vars);
    for(ASTNode n:nodes){
      n.accept(scanner);
    }
    return vars;
  }

  public final AbstractRewriter copy_rw;
  
  private AbstractRewriter(Thread t){
    copy_rw=null;
    create=new ASTFactory<Object>(null);    
  }
  
  public AbstractRewriter(ASTFrame<ASTNode> shared){
    super(shared);
    AbstractRewriter tmp=tl.get();
    if(tmp==null){
      tmp=new AbstractRewriter(Thread.currentThread());
      tl.set(tmp);
    }
    copy_rw=tmp;
    create=new ASTFactory<Object>(copy_rw);
  }

  public AbstractRewriter(ProgramUnit source,ProgramUnit target,boolean do_scope){
    super(source,target,do_scope);
    AbstractRewriter tmp=tl.get();
    if(tmp==null){
      tmp=new AbstractRewriter(Thread.currentThread());
      tl.set(tmp);
    }
    copy_rw=tmp;
    create=new ASTFactory<Object>(copy_rw);    
  }
  public AbstractRewriter(ProgramUnit source,ProgramUnit target){
    this(source,target,false);
  }

  /**
   * Refers to the resulting class of the current class being rewritten.
   */
  protected ASTClass currentTargetClass=null;
  
  /**
   * Refers to the block that is the result of rewriting the current block.
   */
  protected BlockStatement currentBlock=null;
  
  
  protected ASTSequence<?> current_sequence(){
    if (currentBlock!=null) return currentBlock;
    if (currentTargetClass!=null) return currentTargetClass;
    return target();
  }
  
  /**
   * Refer to the contract builder, used for the contract of the current method. 
   */
  protected ContractBuilder currentContractBuilder=null;
  
  /**
   * Prevent automatic copying of labels.
   */
  protected boolean auto_labels=true;
  
  /**
   * This variable references an AST factory, whose Origin is set to
   * the origin of the current node being rewritten.
   */
  public final ASTFactory<?> create;
  
  public final ASTFactory<?> create(Origin origin){
    create.setOrigin(origin);
    return create;
  }
  public final ASTFactory<?> create(ASTNode node){
    create.setOrigin(node.getOrigin());
    return create;
  }
  
  public AbstractRewriter(ProgramUnit source){
    this(source,new ProgramUnit(),false);
  }
  
  public AbstractRewriter(ProgramUnit source,boolean do_scope){
    this(source,new ProgramUnit(),do_scope);
  }
  
  public void pre_visit(ASTNode n){
    super.pre_visit(n);
    for(NameExpression lbl:n.getLabels()){
      Debug("enter %s with label %s",n.getClass(),lbl);
    }
    auto_labels=true;
    create.enter();
    create.setOrigin(n.getOrigin());
    result=null;
    if (n.isSpecial(Kind.Fold)||n.isSpecial(Kind.Unfold)){
      fold_unfold=true;
    }
  }
  public void copy_labels(ASTNode dest,ASTNode source){
    for(NameExpression lbl:source.getLabels()){
      NameExpression copy=new NameExpression(lbl.getName());
      copy.setKind(NameExpression.Kind.Label);
      copy.setOrigin(lbl.getOrigin());
      dest.addLabel(copy);
    }    
  }
  public void post_visit(ASTNode n){
    if (n.isSpecial(Kind.Fold)||n.isSpecial(Kind.Unfold)){
      fold_unfold=false;
    }
    if (result==n) Debug("rewriter linked instead of making a copy"); 
    if (result!=null && result!=n) {
      if (auto_labels){
        ASTNode tmp=result;
        copy_labels(tmp,n);
        result=tmp;
      }
      result.copyMissingFlags(n);
      if (n.annotated() && !result.annotated()){
        ASTNode tmp=result;
        for(ASTNode ann:n.annotations()){
          tmp.attach(rewrite(ann));
        }
        result=tmp;
      }
    }
    auto_labels=true;
    if (result!=null && result instanceof LoopStatement){
      ((LoopStatement)result).fixate();
    }
    create.leave();
    super.post_visit(n);
  }

  protected boolean fold_unfold=false;
  protected boolean in_invariant=false;
  protected boolean in_requires=false;
  protected boolean in_ensures=false;
  
  /** Rewrite contract while adding to a contract builder. */
  public void rewrite(Contract c,ContractBuilder cb){
    if (c==null) return;
    cb.given(rewrite(c.given));
    cb.yields(rewrite(c.yields));
    if (c.modifies!=null) cb.modifies(rewrite(c.modifies)); 
    if (c.accesses!=null) cb.accesses(rewrite(c.accesses)); 
    in_invariant=true;
    for(ASTNode clause:ASTUtils.conjuncts(c.invariant,StandardOperator.Star)){
      cb.appendInvariant(rewrite(clause));
    }
    in_invariant=false;
    in_requires=true;
    for(ASTNode clause:ASTUtils.conjuncts(c.pre_condition,StandardOperator.Star)){
      cb.requires(rewrite(clause));
    }
    in_requires=false;
    in_ensures=true;
    for(ASTNode clause:ASTUtils.conjuncts(c.post_condition,StandardOperator.Star)){
      cb.ensures(rewrite(clause));
    }
    in_ensures=false;
    if (c.signals!=null) for(DeclarationStatement decl:c.signals){
      cb.signals((ClassType)rewrite(decl.getType()),decl.getName(),rewrite(decl.getInit()));      
    }
    //cb.requires(rewrite(c.pre_condition));
    //cb.ensures(rewrite(c.post_condition));
  }
  public Contract rewrite(Contract c){
    if (c==null) return null;
    ContractBuilder cb=new ContractBuilder();
    rewrite(c,cb);
    return cb.getContract();
  }

  @SuppressWarnings("unchecked")
  public <E extends ASTNode> E rewrite(E node){
    if (node==null) return null;
    ASTNode tmp=node.apply(this);
    try {
      return (E)tmp;
    } catch (ClassCastException e) {
     throw new Error("Expected "+node.getClass()+ " got " + tmp.getClass()); 
    }
  }
  
  @SafeVarargs
  private final <E extends ASTNode> E[] glue(E... args){
    return Arrays.copyOf(args,args.length);
  }
  public <E extends ASTNode> E[] rewrite(E head,E[] tail){
    E[] res;
    if (tail==null) {
      res=glue(head);
    } else {
      res=Arrays.copyOf(tail, tail.length+1);
    }
    res[0]=rewrite(head);
    for(int i=0;i<tail.length;i++){
      res[i+1]=rewrite(tail[i]);
    }
    return res;
  }
  
  public <E extends ASTNode> E[][] rewrite(E node[][]){
    if (node==null) return null;
    E res[][]=Arrays.copyOf(node, node.length);
    for(int i=0;i<res.length;i++){
      res[i]=rewrite(res[i]);
    }
    return res;
  }

  /**
   * Rewrite an array.
   * If the given array is null then this function will return null.
   * If any of the elements of the array is null, the corresponding element will
   * also be null.
   * @param <E> The type of the array elements. 
   * @param node The array to be rewritten.
   * @return A new array with rewritten elements.
   */
  public <E extends ASTNode> E[] rewrite(E node[]){
    if (node==null) return null;
    E res[]=Arrays.copyOf(node, node.length);
    for(int i=0;i<res.length;i++){
      res[i]=rewrite(res[i]);
    }
    return res;
  }
  @Override
  public void visit(MethodInvokation e) {
    ASTNode object=rewrite(e.object);
    int N=e.getArity();
    ASTNode args[]=new ASTNode[N];
    for(int i=0;i<N;i++){
      args[i]=e.getArg(i).apply(this);
    }
    MethodInvokation res=create.invokation(object,rewrite(e.dispatch),e.method,args);
    res.set_before(rewrite(e.get_before()));
    res.set_after(rewrite(e.get_after()));
    result=res;
  }

  @Override
  public void visit(AssignmentStatement s) {
    ASTNode loc=s.getLocation().apply(this);
    ASTNode val=s.getExpression().apply(this);
    result=create.assignment(loc,val);
  }

  @Override
  public void visit(ASTClass c) {
    //checkPermission(c);
    String name=c.getName();
    if (name==null) {
      Abort("illegal class without name");
    } else {
      Debug("rewriting class "+name);
      ASTClass res=new ASTClass(name,c.kind,rewrite(c.parameters),rewrite(c.super_classes),rewrite(c.implemented_classes));
      res.setOrigin(c.getOrigin());
      currentTargetClass=res;
      Contract contract=c.getContract();
      if (currentContractBuilder==null){
        currentContractBuilder=new ContractBuilder();
      }
      if (contract!=null){
        rewrite(contract,currentContractBuilder);
      }
      res.setContract(currentContractBuilder.getContract());
      currentContractBuilder=null;
      for(ASTNode item:c){
        res.add(rewrite(item));
      }
      result=res;
      currentTargetClass=null;
    }
  }

  @Override
  public void visit(BlockStatement s) {
    //checkPermission(s);
    Debug("rewriting block");
    BlockStatement tmp=currentBlock;
    currentBlock=new BlockStatement();
    currentBlock.setOrigin(s.getOrigin());
    int N=s.getLength();
    for (int i=0;i<N;i++){
      ASTNode n=s.getStatement(i).apply(this);
      if (n==null) {
        Debug("Got null rewriting %s at %s",s.getStatement(i).getClass(),s.getStatement(i).getOrigin());
      } else {
        Debug("adding %s",n.getClass());
        currentBlock.add_statement(n);
      }
    }
    result=currentBlock;
    currentBlock=tmp;
  }

  public void visit(ClassType t){
    //checkPermission(t);
    ClassType res=new ClassType(t.getNameFull(),rewrite(t.getArgs()));
    res.setOrigin(t.getOrigin());
    result=res; return ;    
  }
  
  @Override
  public void visit(Contract c){
    result=rewrite(c);
  }
  
  @Override
  public void visit(ConstantExpression e) {
    //checkPermission(e);
    result=new ConstantExpression(e.value(),e.getType(),e.getOrigin());
  }
  
  @Override
  public void visit(DeclarationStatement s) {
    //checkPermission(s);
    Type t=s.getType();
    ASTNode tmp=t.apply(this);
    if (tmp instanceof Type){
      t=(Type)tmp;
    } else {
      Abort("Type %s rewrote to non-type %s",t.getClass(),tmp==null ? "null":tmp.getClass());
      throw new Error("type AST rewrote to non-type AST");
    }
    String name=s.getName();
    ASTNode init=s.getInit();
    if (init!=null) init=init.apply(this);
    DeclarationStatement res=new DeclarationStatement(name,t,init);
    res.setOrigin(s.getOrigin());
    result=res; return ;
  }

  public void visit(FunctionType t){
    //checkPermission(t);
    int N=t.getArity();
    Type args[]=new Type[N];
    for(int i=0;i<N;i++){
      args[i]=(Type)t.getArgument(i).apply(this);
    }
    Type result_type=(Type)t.getResult().apply(this);
    ASTNode res=new FunctionType(args,result_type);
    if (t.getOrigin()!=null) res.setOrigin(t);
    result=res;
  }
  


  @Override
  public void visit(IfStatement s) {
    //checkPermission(s);
    IfStatement res=new IfStatement();
    res.setOrigin(s.getOrigin());
    int N=s.getCount();
    for(int i=0;i<N;i++){
      ASTNode guard=s.getGuard(i);
      if (guard!=IfStatement.elseGuard()) guard=guard.apply(this);
      ASTNode body=s.getStatement(i);
      body=body.apply(this);
      res.addClause(guard,body);
    }
    result=res; return ;
  }

  @Override
  public void visit(ForEachLoop s){
    ForEachLoop res=create.foreach(rewrite(s.decls),rewrite(s.guard),rewrite(s.body));
    res.setContract(rewrite(s.getContract()));
    res.set_before(rewrite(s.get_before()));
    res.set_after(rewrite(s.get_after()));
    result=res;
  }
  
  @Override
  public void visit(LoopStatement s) {
    //checkPermission(s);
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
    res.appendContract(rewrite(s.getContract()));
    tmp=s.getBody();
    res.setBody(tmp.apply(this));
    res.set_before(rewrite(s.get_before()));
    res.set_after(rewrite(s.get_after()));
    res.setOrigin(s.getOrigin());
    result=res; return ;
  }

  @Override
  public void visit(Method m) {
    //checkPermission(m);
    String name=m.getName();
    if (currentContractBuilder==null) currentContractBuilder=new ContractBuilder();
    DeclarationStatement args[]=rewrite(m.getArgs());
    Contract mc=m.getContract();
    if (mc!=null){
      rewrite(mc,currentContractBuilder);
    }
    Method.Kind kind=m.kind;
    Type rt=rewrite(m.getReturnType());
    Contract c=currentContractBuilder.getContract();
    currentContractBuilder=null;
    ASTNode body=rewrite(m.getBody());
    result=create.method_kind(kind, rt, c, name, args, m.usesVarArgs(), body);
  }

  @Override
  public void visit(NameExpression e) {
    //checkPermission(e);
    NameExpression res=new NameExpression(e.getKind(),e.reserved(),e.getName());
    res.setOrigin(e.getOrigin());
    result=res;
  }

  @Override
  public void visit(OperatorExpression e) {
    //checkPermission(e);
    StandardOperator op=e.operator();
    int N=op.arity();
    if(N<0) N=e.args().length;
    ASTNode args[]=new ASTNode[N];
    for(int i=0;i<N;i++){
      args[i]=e.arg(i).apply(this);
    }
    OperatorExpression res = OperatorExpression.construct(op, args);
    res.set_before(rewrite(e.get_before()));
    res.set_after(rewrite(e.get_after()));
    res.setOrigin(e.getOrigin());
    result=res;
  }

  public void visit(PrimitiveType t){
    //checkPermission(t);
    PrimitiveType res=new PrimitiveType(t.sort,rewrite(t.getArgs()));
    if (t.getOrigin()!=null){
      res.setOrigin(t);
    } else {
      res.setOrigin(new MessageOrigin("fix problem??"));
    }
    result=res;
  }

  public void visit(RecordType t){
    //checkPermission(t);
    throw new Error("missing case in rewriter: "+t.getClass());
  }

  @Override
  public void visit(ReturnStatement s) {
    //checkPermission(s);
    ASTNode val=s.getExpression();
    if(val!=null) val=val.apply(this);
    ReturnStatement res=new ReturnStatement(val);
    res.setOrigin(s.getOrigin());
    res.set_after(rewrite(s.get_after()));
    result=res;
  }
  @Override
  public void visit(StandardProcedure p) {
    //checkPermission(p);
    StandardProcedure res = new StandardProcedure(p.operator());
    res.setOrigin(p.getOrigin());
    result=res;
  }

  public ProgramUnit rewriteAll() {
    for(ASTDeclaration n:source().get()){
        ASTNode tmp=rewrite(n);
        if (tmp!=null){
          target().add(tmp);
        }
    }
    target().index_classes();
    return target();
  }

  private void rewriteOrdered(HashSet<ASTClass> done,ASTClass cl){
    if (!done.contains(cl)){
      done.add(cl);
      if (cl.implemented_classes.length>0){
        Fail("interfaces are not supported");
      }
      for(ClassType parent:cl.super_classes){
        rewriteOrdered(done,source().find(parent));
      }
      Debug("rewriting %s",cl.getName());
      ASTClass tmp=rewrite(cl);
      if (tmp!=null){
        target().add(tmp);
      }
    }
  }

  public ProgramUnit rewriteOrdered() {
    HashSet<ASTClass> done=new HashSet<ASTClass>();
    for(ASTNode n:source().get()){
        if (n instanceof ASTClass) {
          rewriteOrdered(done,(ASTClass)n);
        } else {
          ASTNode tmp=rewrite(n);
          if (tmp!=null){
            target().add(tmp);
          }
        }
    }
    return target();
  }

  @Override
  public void visit(Dereference e) {
    result = create.dereference(e.object().apply(this), e.field());
  }
  
  @Override
  public void visit(BindingExpression e){
    result=create.binder(e.binder,rewrite(e.result_type),rewrite(e.getDeclarations()),rewrite(e.triggers), rewrite(e.select), rewrite(e.main));
  }
  
  @Override
  public void visit(Lemma l) {
    result = create.lemma(rewrite(l.block()));
  }
  
  @Override
  public void visit(ParallelAtomic pa){
    result = create.csl_atomic(rewrite(pa.block()), rewrite(pa.synclist().toArray(new ASTNode[0])));
  }
  
  @Override
  public void visit(ParallelInvariant inv){
    result = create.invariant_block(inv.label(), rewrite(inv.inv()), rewrite(inv.block()));
  }
  
  @Override
  public void visit(ParallelBlock pb){
    ParallelBlock res=create.parallel_block(
        pb.label,
        rewrite(pb.contract),
        rewrite(pb.iters),
        rewrite(pb.block),
        rewrite(pb.deps)
    );
    result=res;
  }
  
  @Override
  public void visit(ParallelRegion region){
    result = create.region(rewrite(region.contract()), rewrite(region.blocks()));
  }
  
  @Override
  public void visit(ParallelBarrier pb) {
    result = create.barrier(pb.label(), rewrite(pb.contract()), pb.invs(), rewrite(pb.body()));
  }

  @Override
  public void visit(ASTSpecial special) {
    result=create.special(special.kind,rewrite(special.args));
  }
    
  @Override
  public void visit(VariableDeclaration decl) {
    VariableDeclaration res=create.variable_decl(decl.basetype);
    for(ASTDeclaration d:decl.get()){
      res.add(rewrite(d));
    }
    result=res;
  }
  
  @Override
  public void visit(AxiomaticDataType adt){
    AxiomaticDataType res = create.adt(adt.name, rewrite(adt.parameters()));
    for (Method c : adt.constructors()) {
      res.add_cons(rewrite(c));
    }
    for(Method m:adt.mappings()){
      res.add_cons(rewrite(m));
    }
    for(Axiom ax:adt.axioms()){
      res.add_axiom(rewrite(ax));
    }
    result=res;
  }
  
  public void visit(Axiom axiom){
    result = create.axiom(axiom.name, rewrite(axiom.rule()));
  }
  
  /*
   * The following functions make generating code easier...
   */
  
  public ASTNode constant(int c){
  	return create.constant(c);
  }
  public ASTNode name(String name){
  	return create.unresolved_name(name);
  }
  public ASTNode name(DeclarationStatement decl){
  	create.enter();
  	ASTNode res=create(decl.getOrigin()).unresolved_name(decl.getName());
  	create.leave();
  	return res;
  }
  
  public ASTNode and(ASTNode e1,ASTNode e2){
    return create.expression(StandardOperator.And,e1,e2);
  }
  public ASTNode plus(ASTNode e1,ASTNode e2){
    return create.expression(StandardOperator.Plus,e1,e2);
  }
  public ASTNode mult(ASTNode e1,ASTNode e2){
    return create.expression(StandardOperator.Mult,e1,e2);
  }
  public ASTNode less(ASTNode e1,ASTNode e2){
  	return create.expression(StandardOperator.LT,e1,e2);
  }
  public ASTNode lte(ASTNode e1,ASTNode e2){
  	return create.expression(StandardOperator.LTE,e1,e2);
  }
  public ASTNode neq(ASTNode e1,ASTNode e2){
  	return create.expression(StandardOperator.NEQ,e1,e2);
  }
  public ASTNode star(ASTNode e1,ASTNode e2){
  	return create.expression(StandardOperator.Star,e1,e2);
  }
  public ASTNode invoke(ASTNode object,String method,ASTNode ... args){
  	return create.invokation(object, null, method, args);
  }

  @Override
  public void visit(ActionBlock ab) {
    Map<String,ASTNode> map = new HashMap<String,ASTNode>();
    
    for (Entry<String, ASTNode> e : ab.mapAsJava().entrySet()) {
      map.put(e.getKey(), rewrite(e.getValue()));
    }
    
    result=create.action_block(
        rewrite(ab.history()),
        rewrite(ab.fraction()),
        rewrite(ab.process()),
        rewrite(ab.action()),
        map,
        rewrite(ab.block())
    );
  }
  
  @Override
  public void visit(Hole hole){
    result=hole;
  }

  protected DeclarationStatement[] gen_pars(Hashtable<String, Type> vars) {
    DeclarationStatement decls[]=new DeclarationStatement[vars.size()];
    int i=0;
    for(String name:vars.keySet()){
      decls[i]=create.field_decl(name, vars.get(name));
      i++;
    }
    return decls;
  }

  protected MethodInvokation gen_call(String method, Hashtable<String, Type> vars) {
    ASTNode args[]=new ASTNode[vars.size()];
    int i=0;
    for(String name:vars.keySet()){
      args[i]=create.unresolved_name(name);
      i++;
    }
    return create.invokation(null,null, method, args);
  }

  @Override
  public void visit(NameSpace ns) {
    NameSpace res=create.namespace(ns.getDeclName().name);
    for(ASTNode n:ns){
      res.add(rewrite(n));
    }
    for(NameSpace.Import i:ns.imports){
      res.add_import(i.static_import,i.all,i.name);
    }
    result=res;
  }
  @Override
  public void visit(TryCatchBlock tcb){
    TryCatchBlock res=create.try_catch(rewrite(tcb.main),rewrite(tcb.after));
    for(CatchClause cc:tcb.catches){
      pre_visit(cc.block());
      BlockStatement tmp=currentBlock;
      currentBlock=new BlockStatement();
      currentBlock.setOrigin(cc.block().getOrigin());
      DeclarationStatement d=rewrite(cc.decl());
      for(ASTNode S:cc.block()){
        currentBlock.add(rewrite(S));
      }
      BlockStatement block=currentBlock;
      currentBlock=tmp;
      post_visit(cc.block());
      res.catch_clause(d,block);
    }
    result=res;
  }
  
  @Override
  public void visit(TypeExpression te){
    result = create.type_expression(te.operator(), rewrite(te.types()));
  }
  
  @Override
  public void visit(TypeVariable t){
    result=create.type_variable(t.name());
  }
  
  @Override
  public void visit(FieldAccess a){
    result=create.set_field(a.classname(), rewrite(a.object()), a.name(), rewrite(a.value()));
  }

  public ASTNode inline_call(MethodInvokation e){
    return inline_call(e,e.getDefinition());
  }
  public ASTNode inline_call(MethodInvokation e, Method def) {
    int N=def.getArity();
    HashMap<NameExpression,ASTNode> map=new HashMap<NameExpression, ASTNode>();
    Substitution sigma=new Substitution(source(),map);
    map.put(create.reserved_name(ASTReserved.This), rewrite(e.object));
    for(int i=0;i<N;i++){
      map.put(create.unresolved_name(def.getArgument(i)),rewrite(e.getArg(i)));
    }
    ASTNode body=rewrite(def.getBody());
    InlineMarking marker=new InlineMarking(source(),e.getOrigin());
    body.accept(marker);
    return sigma.rewrite(body);
  }
  
  @Override
  public void visit(StructValue v) {
    result = create.struct_value(rewrite(v.type()), v.map(), rewrite(v.values()));
  }
  
  @Override
  public void visit(VectorBlock v){
    result = create.vector_block(rewrite(v.iter()), rewrite(v.block()));
  }
  
  @Override
  public void visit(Constraining c){
    result = create.constraining(rewrite(c.block()) ,rewrite(c.vars()));
  }
  
  @Override
  public void visit(Switch s){
    ASTNode expr=rewrite(s.expr);
    ArrayList<Case> case_list=new ArrayList();
    for (Case c:s.cases){
      Case rwc=new Case();
      for(ASTNode n:c.cases) rwc.cases.add(rewrite(n));
      for(ASTNode n:c.stats) rwc.stats.add(rewrite(n));
      case_list.add(rwc);
    }
    result = create.switch_statement(expr, case_list);
  }
}
