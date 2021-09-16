package vct.col.rewrite;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;

import vct.col.ast.expr.Dereference;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.Method.Kind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.util.ASTFactory;
import vct.col.ast.util.ASTUtils;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.Configuration;
import vct.col.ast.util.ContractBuilder;
import vct.col.util.AstToId;

/**
 * This class checks if a specified process algebra is correct.
 * 
 * @author sccblom
 *
 */
public class CheckProcessAlgebra extends AbstractRewriter {

  public CheckProcessAlgebra(ProgramUnit source) {
    super(source);
  }

  private Hashtable<String,String> composite_map;
  private Hashtable<String, Method> process_map;

  public static final String FRESH_ZFRAC_NAME = "freshZfrac";

  int counter = 0;

  private static ArrayList<ArrayList<String>> permutations(ArrayList<String> list) {
    if (list.size() == 0) {
    	ArrayList<ArrayList<String>> result = new ArrayList<ArrayList<String>>();
      result.add(new ArrayList<String>());
      return result;
    }

    ArrayList<ArrayList<String>> returnMe = new ArrayList<ArrayList<String>>();
    String firstElement = list.remove(0);

    ArrayList<ArrayList<String>> recursiveReturn = permutations(list);
    for (ArrayList<String> li : recursiveReturn) {
        for (int index = 0; index <= li.size(); index++) {
        	ArrayList<String> temp = new ArrayList<String>(li);
          temp.add(index, firstElement);
          returnMe.add(temp);
        }
    }
    return returnMe;
}
  
  @Override
  public void visit(ASTClass cl){
    composite_map=new Hashtable<String,String>();
    process_map=new Hashtable<String,Method>();
//    HashSet<NameExpression> hist_set=new HashSet<NameExpression>();
    for(Method m:cl.dynamicMethods()){
      // Register all processes in this class by name
      if (!m.getReturnType().isPrimitive(PrimitiveSort.Process)) continue;
      ASTNode body=m.getBody();
      process_map.put(m.name(), m);

//      ASTNode[] modifies = m.getContract().modifies;
//      if (modifies == null) {
//      	modifies = new ASTNode[0];
//      }

      // Since hist_set is not used, we can ignore this part
//      for(ASTNode n : modifies){
//        if (n instanceof NameExpression){
//          hist_set.add((NameExpression)n);
//        } else if (n instanceof Dereference) {
//          Dereference d=(Dereference)n;
//          hist_set.add(create.field_name(d.field()));
//        } else {
//          Fail("unexpected entry in modifies clause");
//        }
//      }
      if (body==null) continue;
      // If body is of form P() || Q() || ... || Z():
      // Save all permutations of P, Q, .., Z to composite map
      // E.g., PQZ => methodName
      //       QPZ => methodName
      //       etc.
      // Otherwise, error, cannot do any complex expressions in parallel composition sequences
      if(body.isa(StandardOperator.Or)){
        ArrayList<String> compounds = new ArrayList<String>();
        for(ASTNode p:ASTUtils.conjuncts(body, StandardOperator.Or)){
          if (p instanceof MethodInvokation) {
          	compounds.add(((MethodInvokation)p).method());
          } else {
            Fail("misformed parallel composition");
          }
        }
        
        ArrayList<ArrayList<String>> allcompounds = permutations(compounds);
        
        for (ArrayList<String> part : allcompounds) {
        	String composite = ":";
        	for (String itm : part) {
        		composite += itm + ":";
        	}
        	composite_map.put(composite, m.name());
        	Warning("mapping %s to %s", composite, m.name());
        }

        // This is obviously a semantics question
        // TODO: check if arguments are passed in-order.
        // That is p(a,b)=q(a)||q(b) is allowed, while p(a,b)=q(b)||q(a) is forbidden.
      }
    }
    // Also copy the class literally, and trigger the visit function for lower methods and such
    super.visit(cl);

    /* this block creates _old vars and begin/end methods which are nto needed for checking histories.
    ASTClass res=(ASTClass)result;
    for(NameExpression n:hist_set){
      String name=n.getName();
      VariableInfo info=variables.lookup(name);
      Type t=((DeclarationStatement)info.reference).getType();
      res.add(create.field_decl(n.getName()+"_old",copy_rw.rewrite(t)));
    }
    for(Method m:cl.dynamicMethods()){
      if (!m.getReturnType().isPrimitive(Sort.Process)) continue;
      if (m.getBody()!=null) continue;
      // m defines an action.
      create.enter();
      create.setOrigin(m.getOrigin());
      ContractBuilder begin_cb=new ContractBuilder();
      ArrayList<DeclarationStatement> begin_args=new ArrayList();
      ContractBuilder commit_cb=new ContractBuilder();
      ArrayList<DeclarationStatement> commit_args=new ArrayList();
      res.add(create.method_decl(
          create.primitive_type(Sort.Void),
          begin_cb.getContract(),
          m.name+"_begin",
          begin_args.toArray(new DeclarationStatement[0]),
          null
      ));
      res.add(create.method_decl(
          create.primitive_type(Sort.Void),
          commit_cb.getContract(),
          m.name+"_commit",
          commit_args.toArray(new DeclarationStatement[0]),
          null
      ));
      create.leave();
    }
    result=res;
    */
  }
  
  @Override
  public void visit(MethodInvokation e){
    Method m=e.getDefinition();
    if (m.getReturnType().isPrimitive(PrimitiveSort.Process)){
      result=create.invokation(null,null, "p_"+e.method(),rewrite(e.getArgs()));
    } else {
      super.visit(e);
    }
  }

  @Override
  public void visit(Method m){
    // Process methods are handled specially
    // Pure methods are kept as is, the rest is discarded
    if (m.getReturnType().isPrimitive(PrimitiveSort.Process)){
      // Create a new contract for the new method
      Contract c=m.getContract();
      ContractBuilder cb = new ContractBuilder();
      
      ASTNode[] modifies = c.modifies == null ? new ASTNode[0] : c.modifies;

      // Create a write perm for each modifies
      for (ASTNode v : modifies){
        create.enter();
        create.setOrigin(v.getOrigin());
        cb.requires(create.expression(StandardOperator.Perm,rewrite(v),create.reserved_name(ASTReserved.FullPerm)));
        cb.ensures(create.expression(StandardOperator.Perm,rewrite(v),create.reserved_name(ASTReserved.FullPerm)));
        create.leave();
      }

      // Create a 0 < p < 1 permission for each accessible
      ArrayList<DeclarationStatement> accessArgs = new ArrayList<>();
      if (c.accesses != null) {
        for (ASTNode v : c.accesses) {
          create.enter(v);

          String argV = "f_" + AstToId.toId(v);
          cb.requires(create.fold(StandardOperator.Star,
                  create.expression(StandardOperator.LT,  create.constant(0), create.local_name(argV)),
                  create.expression(StandardOperator.LT,  create.local_name(argV), create.constant(1)),
                  create.expression(StandardOperator.Perm, rewrite(v), create.local_name(argV))
                  ));
          cb.ensures(create.expression(StandardOperator.Perm,rewrite(v),create.local_name(argV)));

          accessArgs.add(create.field_decl(argV, create.primitive_type(PrimitiveSort.ZFraction)));

          create.leave();
        }
      }

      // Copy the pre and posts over
      if (c.pre_condition!=null) cb.requires(rewrite(c.pre_condition));
      if (c.post_condition!=null) cb.ensures(rewrite(c.post_condition));

      // Add all original arguments after the accessible arguments
      DeclarationStatement args[]=rewrite(m.getArgs());
      for (DeclarationStatement arg : args) {
        accessArgs.add(0, arg);
      }
      args = accessArgs.toArray(DeclarationStatement[]::new);

      // Transform the process body in a tricky way
      BlockStatement body=null;
      ASTNode m_body=m.getBody();
      if (m_body!=null){
        create.enter();
        body=create(m_body).block();
        m_body=normalize_body(m_body);
        // Create_body turns a process algebra expression into a "regular" viper program
        // E.g. + becomes if, sequential composition becomes newline, etc.
        // At this point all left merge operators and || must be transformed away
        create_body(body,m_body);
        create.leave();
      }

      // The result is plain method
      result=create.method_decl(create.primitive_type(PrimitiveSort.Void), cb.getContract(), m.name(), args, body);
    }
    else { 	
    	if (m.kind == Kind.Pure) {
    		super.visit(m);
    	}
    	else {
    		result = null;
    	}
    }
  }


  private ASTNode normalize_body(ASTNode m_body) {
    PrintWriter out = hre.lang.System.getLogLevelErrorWriter(hre.lang.System.LogLevel.Debug);

    out.print("normalize input: ");
    Configuration.getDiagSyntax().print(out,m_body);
    out.println();
    m_body=expand_unguarded(m_body);
    out.print("guarded rewrite: ");
    Configuration.getDiagSyntax().print(out,m_body);
    out.println();

    out.close();
    return m_body;
  }

  @Override
  public void visit(PrimitiveType t){
    if (t.sort==PrimitiveSort.Process){
      Fail("illegal use of process type");
    } else {
      super.visit(t);
    }
  }

  private ASTNode expand_unguarded(ASTNode m_body) {
    /*
    Expansion works as follows:
    - MethodInvokations with a definition are inlined, and then further expanded
    - Empty processes are kept
    - Parllel composition: left and right are expanded. Then a non-determinstic choice must happen:
      - Either an action is taken from the left expansion
      - Or an action is taken from the right expansion
    - Choice: either side is just expanded
    - Mult: only the first argument is expanded
    - Ite: both branches must be expanded

    Through this, expansion tries to ensure m_body eventually can always do an action (which is modelled as an abstract process)
    This fails if there is recursion between processes on the left hand side of a parallel composition.
     */

    PrintWriter out = hre.lang.System.getLogLevelErrorWriter(hre.lang.System.LogLevel.Debug);
    out.print("expanding: ");
    Configuration.getDiagSyntax().print(out,m_body);
    out.println();

    if (m_body instanceof MethodInvokation){
      MethodInvokation p=(MethodInvokation)m_body;
      Method def=process_map.get(p.method());
      if (def.getBody()==null){
        return m_body;
      } else {
        Hashtable<NameExpression,ASTNode> map=new Hashtable<NameExpression, ASTNode>();
        int N=def.getArity();
        for(int i=0;i<N;i++){
          map.put(create.unresolved_name(def.getArgument(i)),copy_rw.rewrite(p.getArg(i)));
        }
        Substitution sigma=new Substitution(source(),map);
        ASTNode tmp=sigma.rewrite(def.getBody());
        return expand_unguarded(tmp);
      }
    } else if (m_body.isReserved(ASTReserved.EmptyProcess)) {
      return m_body;
    } else if (m_body instanceof OperatorExpression){
      OperatorExpression p=(OperatorExpression)m_body;
      switch(p.operator()){
      case Or:{
        ASTNode p0=p.arg(0);
        ASTNode g0=expand_unguarded(p0);
        ASTNode p1=p.arg(1);
        ASTNode g1=expand_unguarded(p1);
        return create.expression(StandardOperator.Plus,
            left_merge(g0,p1),
            left_merge(g1,p0)
        );
      }
      case Plus:{
        ASTNode p0=p.arg(0);
        ASTNode g0=expand_unguarded(p0);
        ASTNode p1=p.arg(1);
        ASTNode g1=expand_unguarded(p1);
        return create.expression(StandardOperator.Plus,g0,g1);
      }
      case Mult:{
        ASTNode p0=p.arg(0);
        ASTNode g0=expand_unguarded(p0);
        ASTNode p1=p.arg(1);
        return create.expression(StandardOperator.Mult,g0,p1); 
      }
      case ITE:{
        ASTNode b=p.arg(0);
        ASTNode p0=p.arg(1);
        ASTNode g0=expand_unguarded(p0);
        ASTNode p1=p.arg(2);
        ASTNode g1=expand_unguarded(p1);
        return create.expression(StandardOperator.ITE,b,g0,g1);
      }
      default:
        // No problem
      }
    }
    Fail("illegal process expression");
    return null;
  }

  /*
   a L b is identical to a || b, except that the first action performed must be done by a
   Hence, if a is an atomic action:
   a L b = a . b
   Hence, left merging is a way to possibly get rid of parallel compositions.
   It is a linearization strategy that works in the absence of recursion, at least in this implementation
   (See also: The Importance of the Left Merge Operator in Process Algebra by Faron Moller)

   Key laws about left merge:
   a || b = a L b + b L a         (parallel composition can be encoded as left merge)
   (a + b) L c = a L c + b L c    (choice on the left hand side can be distributed across left merge)
   Note that:
   (a + b) || c =/= a || c + b || c

   */
  private ASTNode left_merge(ASTNode m_body, ASTNode other) {
    /*
      When:
      - Method invocation: assume it is an atomic action, and hence sequentially compose m_body and other
      - Empty process: we can just continue with other (though in pure process algebra, I'd say this case is not defined!)
        (Also not present in Faron Moller's work, I think)
      - Choice: Choice of left merge of either side of the plus with other. This is effectively the distributive property at work,
        and hence tries to ensure each branch begins with a concrete action
      - Sequential composition:
        - So in this case it is: (P * Q) L R
        - In this case, P and Q must be atomic actions
        - By left-merge's definition, rewrite this to P * (Q || R)
        - We then try to reduce this back to an actual top-level parallel composition by seeing if this Q || R is defined in composite_map
          - There is an inherent assumption here that all processes that are parallel compositions are unique compositions of other processes
        - If so, we return P * S, for which S := Q || R, or maybe S := R || Q
        - Else, we can't handle this, so we bail
      - Ite: left merge both branches with other. Here again the transformation ensures each branch begins with a concrete action
     */
    if (m_body instanceof MethodInvokation){
      return create.expression(StandardOperator.Mult,m_body,other);
    } else if(m_body.isReserved(ASTReserved.EmptyProcess)){
      return other;
    } else if (m_body instanceof OperatorExpression){
      OperatorExpression p=(OperatorExpression)m_body;
      switch(p.operator()){
      case Plus:{
        ASTNode p0=p.arg(0);
        ASTNode g0=left_merge(p0,other);
        ASTNode p1=p.arg(1);
        ASTNode g1=left_merge(p1,other);
        return create.expression(StandardOperator.Plus,g0,g1);
      }
      case Mult:{
        // P * Q L R
        ASTNode p0=p.arg(0);
        ASTNode p1=p.arg(1);
        if (!(p0 instanceof MethodInvokation)) break;
        if (!(p1 instanceof MethodInvokation)) break;
        MethodInvokation m0=(MethodInvokation)p1;
        if (!(other instanceof MethodInvokation)) break;
        MethodInvokation m1=(MethodInvokation)other;
        ArrayList<ASTNode> args=new ArrayList<ASTNode>();
        String key=":"+m0.method()+":"+m1.method()+":";
        String merged=composite_map.get(key);
        if (merged==null){
          Abort("missing key %s",key);
        }
        for(ASTNode n:m0.getArgs()) args.add(n);
        for(ASTNode n:m1.getArgs()) args.add(n);
        ASTNode guess=create.invokation(create.reserved_name(ASTReserved.This),null,merged,args.toArray(new ASTNode[0]));
        return create.expression(StandardOperator.Mult,p0,guess); 
      }
      case ITE:{
        ASTNode b=p.arg(0);
        ASTNode p0=p.arg(1);
        ASTNode g0=left_merge(p0,other);
        ASTNode p1=p.arg(2);
        ASTNode g1=left_merge(p1,other);
        return create.expression(StandardOperator.ITE,b,g0,g1);
      }
      default:
        break;
      }
    }
    Fail("illegal process expression in left_merge");
    return null;   
  }

  protected void create_body(BlockStatement body, ASTNode m_body) {
    create.enter();
    create.setOrigin(m_body.getOrigin());
    if (m_body instanceof OperatorExpression) {
      OperatorExpression e=(OperatorExpression)m_body;
      switch(e.operator()){
      case ITE:{
        BlockStatement lhs=create.block();
        create_body(lhs,e.arg(1));
        BlockStatement rhs=create.block();
        create_body(rhs,e.arg(2));
        body.add(create.ifthenelse(copy_rw.rewrite(e.arg(0)),lhs,rhs));
        break;
      }
      case Mult:{
        create_body(body,e.arg(0));
        create_body(body,e.arg(1));
        break;
      }
      case Plus:{
        BlockStatement lhs=create.block();
        create_body(lhs,e.arg(0));
        BlockStatement rhs=create.block();
        create_body(rhs,e.arg(1));
        body.add(create.ifthenelse(create.reserved_name(ASTReserved.Any),lhs,rhs));    
        break;
      }
      case Or:{
        Abort("cannot generate body for parallel composition");
      }
      default:
        Abort("skipping unknown process operator %s", e.operator());
      }
    } else if (m_body instanceof MethodInvokation) {
      MethodInvokation mi = copy_rw.rewrite((MethodInvokation) m_body);
      Method def = process_map.get(mi.method());
      if (def.getContract().accesses != null && def.getContract().accesses.length > 0) {
        ensureFreshZfracPresent(source(), target(), create);
        addAccessibleArgs(mi, body);
      }
      counter++;
      body.add(mi);
    } else if (!m_body.isReserved(ASTReserved.EmptyProcess)){
      Abort("unknown process %s",m_body.getClass());
    }
    create.leave();
  }

  public static void ensureFreshZfracPresent(ProgramUnit source, ProgramUnit target, ASTFactory<?> create) {
    if (source.find_procedure(FRESH_ZFRAC_NAME) != null || target.find_procedure(FRESH_ZFRAC_NAME) != null) {
      // Already present in source or target
      return;
    }

    Method m = create.method_decl(
            create.primitive_type(PrimitiveSort.ZFraction),
            null,
            FRESH_ZFRAC_NAME,
            new DeclarationStatement[0],
            null
    );

    m.setFlag(ASTFlags.STATIC, true);

    target.add(m);
  }

  private void addAccessibleArgs(MethodInvokation mi, BlockStatement body) {
    for (ASTNode v : process_map.get(mi.method()).getContract().accesses) {
      String vId = "f_" + AstToId.toId(v) + ("_" + counter++);

      body.add(create.field_decl(vId, create.primitive_type(PrimitiveSort.ZFraction)));
      body.add(create.assignment(create.local_name(vId), create.invokation(null, null, FRESH_ZFRAC_NAME)));

      body.add(create.special(ASTSpecial.Kind.Assert, create.expression(StandardOperator.LT,
              create.constant(0),
              create.expression(StandardOperator.CurrentPerm, copy_rw.rewrite(v))
      )));

      body.add(create.special(ASTSpecial.Kind.Inhale, create.fold(StandardOperator.Star,
              create.expression(StandardOperator.LT,
                      create.constant(0),
                      create.local_name(vId)
              ),
              create.expression(StandardOperator.LT,
                      create.local_name(vId),
                      create.expression(StandardOperator.CurrentPerm, copy_rw.rewrite(v))
              )
      )));

      mi.addArg(create.local_name(vId));
    }
  }

  @Override
  public void visit(OperatorExpression e){
    switch(e.operator()){
      case Or:
      if(e.getType().isPrimitive(PrimitiveSort.Process)){
        result=create.invokation(null, null, "p_merge", rewrite(e.argsJava()));
        return;
      }
      break;
    case Mult:
      if(e.getType().isPrimitive(PrimitiveSort.Process)){
        result=create.invokation(null, null, "p_seq", rewrite(e.argsJava()));
        return;
      }
      break;
      default:
        break;
    }
    super.visit(e);
  }
}
