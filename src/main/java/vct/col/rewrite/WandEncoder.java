package vct.col.rewrite;

import hre.ast.MessageOrigin;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.concurrent.atomic.AtomicInteger;

import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.composite.Lemma;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.Type;
import vct.col.ast.util.ASTUtils;
import vct.logging.ErrorMapping;
import vct.logging.VerCorsError.ErrorCode;

public class WandEncoder extends AbstractRewriter {
  
  public final static String VALID="valid";
  
  private HashSet<String> wands=new HashSet<String>();
  
  private class WandUtil {

    ASTNode args[];
    
    private String get_wand_type(OperatorExpression e){
      String type_name="Wand";
      ArrayList<ASTNode> args=new ArrayList<ASTNode>();      
      ArrayList<DeclarationStatement> decls=new ArrayList<DeclarationStatement>();
      for(ASTNode n:ASTUtils.conjuncts(e.arg(0),StandardOperator.Star)){
        if (n instanceof MethodInvokation){
          MethodInvokation m=(MethodInvokation)n;
          type_name+="_"+m.method;
          Method def=m.getDefinition();
          if(!def.isStatic()){
            args.add(m.object);
            Type t=create.class_type(((ASTClass)def.getParent()).getFullName());
            decls.add(create.field_decl("x_"+decls.size(),t));
          }
          for(ASTNode a:m.getArgs()){
            args.add(a);
          }
          for(Type t:m.getDefinition().getArgType()){
            decls.add(create.field_decl("x_"+decls.size(), t));
          }
        } else {
          Abort("unexpected clause in magic wand");
        }
      }
      type_name+="_for";
      for(ASTNode n:ASTUtils.conjuncts(e.arg(1),StandardOperator.Star)){
        if (n instanceof MethodInvokation){
          MethodInvokation m=(MethodInvokation)n;
          type_name+="_"+m.method;
          Method def=m.getDefinition();
          if(!def.isStatic()){
            args.add(m.object);
            Type t=create.class_type(((ASTClass)def.getParent()).getFullName());
            decls.add(create.field_decl("x_"+decls.size(),t));
          }
          for(ASTNode a:m.getArgs()){
            args.add(a);
          }
          for(Type t:m.getDefinition().getArgType()){
            decls.add(create.field_decl("x_"+decls.size(), t));
          }
        } else {
          Abort("unexpected clause in magic wand");
        }
      }
      this.args=args.toArray(new ASTNode[args.size()]);
      if (!wands.contains(type_name)){
        wands.add(type_name);
        wand_class.add_static(create.predicate(
            type_name,null,decls.toArray(new DeclarationStatement[decls.size()])));
      }
      return type_name;
    }

    ASTNode pre;
    ASTNode post;
    
    private String name;
    
    private String label=null;
    
    public WandUtil(OperatorExpression e){
      if (e.labels()>0){
        label=e.getLabel(0).getName();
      }
      name=get_wand_type(e);
      pre=e.arg(0);
      post=e.arg(1);
    }
    
    public ASTNode wand(boolean lbl){
      ASTNode res=create.invokation(create.class_type("MagicWands"),null, name,rewrite(args));
      if (lbl && label!=null) res.addLabel(create.label(label));
      return res;
    }
    
    public ASTNode pre(){
      return rewrite(pre);
    }
    public ASTNode post(){
      return rewrite(post);
    }

    public ASTNode generate_lemma(BlockStatement block) {
      int no=count.incrementAndGet();
      String name="lemma_"+no;
      ArrayList<ASTNode> uses=new ArrayList<ASTNode>();
      ContractBuilder cb=new ContractBuilder();
      BlockStatement body=create.block();
      BlockStatement tmp=currentBlock;
      currentBlock=body;
      BlockStatement intro=create.block();
      int N=block.size()-1;
      for(int i=0;i<N;i++){
        ASTNode S=block.get(i);
        if (S.isSpecial(Kind.Use)){
          uses.add(rewrite(((ASTSpecial)S).getArg(0)));
          cb.requires(rewrite(((ASTSpecial)S).getArg(0)));
        } else {
          body.add(rewrite(S));
        }
      }
      intro.add(create.special(Kind.Exhale,create.fold(StandardOperator.Star, uses)));
      intro.add(create.special(Kind.Inhale,wand(true)));
      cb.requires(pre());
      cb.ensures(post());
      Hashtable<String,Type> vars=free_vars(block);
      currentTargetClass.add(create.method_decl(
          create.primitive_type(PrimitiveSort.Void),
          cb.getContract(),
          name,
          gen_pars(vars),
          body
      ));
      currentBlock=tmp;
      return intro;
    }
  }
  
  
  private static String WAND_FORMULA="magic wand";
  private static String WAND_REQUIRED="magic wand requirement";
  
  
	public WandEncoder(ProgramUnit source,ErrorMapping map) {
		super(source);
    map.add(WAND_FORMULA,ErrorCode.ExhaleFailed,ErrorCode.MagicWandUnproven);
    map.add(WAND_REQUIRED,ErrorCode.ExhaleFailed,ErrorCode.MagicWandPreCondition);
		
	}
	
	
	@Override
	public void visit(ASTSpecial e){
    switch (e.kind){
    case Apply:{
      ASTNode arg=e.getArg(0);
      if (!arg.isa(StandardOperator.Wand)){
        Fail("illegal argument to apply.");
      }
      WandUtil wand=new WandUtil((OperatorExpression)arg);
      currentBlock.add(create.special(Kind.Exhale,wand.wand(true)).set_branch(WAND_FORMULA));
      currentBlock.add(create.special(Kind.Exhale,wand.pre()).set_branch(WAND_REQUIRED));
      currentBlock.add(create.special(Kind.Inhale,wand.post()));
      break;
    }
    default:
      super.visit(e);
    }  
	}
	
	@Override
	public void visit(OperatorExpression e){
    switch (e.operator()){
		    case Wand:{
		      WandUtil wand=new WandUtil(e);
		      result=wand.wand(false);
		      break;
		    }
		    default:
		      super.visit(e);
		    }
    }
	  
	AtomicInteger count=new AtomicInteger();
	  	
	public void visit(Lemma lemma) {
	  int N = lemma.block().size();
	  WandUtil wand=null;
	  if (lemma.block().get(N-1).isSpecial(Kind.QED)) {
	    ASTNode tmp=((ASTSpecial)lemma.block().get(N-1)).getArg(0);
	    if (tmp.isa(StandardOperator.Wand)){
	      wand=new WandUtil((OperatorExpression)tmp);
	    } else {
	      Fail("argument of qed is not a magic wand %s",tmp);
	    }
	  } else {
	    Fail("lemma does not end in qed.");
	  }
	  result=wand.generate_lemma(lemma.block());
	}
	
	private ASTClass wand_class;
	
	@Override
	public ProgramUnit rewriteAll(){
	  create.enter();
	  create.setOrigin(new MessageOrigin("collected magic wand predicates"));
	  wand_class=create.ast_class("MagicWands",ClassKind.Plain,null,null,null);
	  create.leave();
	  ProgramUnit res=super.rewriteAll();
	  res.add(wand_class);
	  return res;
	}
}

