package vct.col.rewrite;

import hre.lang.HREError;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicInteger;

import vct.antlr4.parser.OMPParser;
import vct.antlr4.parser.OMPoption;
import vct.antlr4.parser.OMPpragma;

import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.ParallelBlock;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ContractBuilder;
import vct.col.util.ASTUtils;
import vct.col.util.FeatureScanner;

import static hre.lang.System.Debug;

interface PPLProgram {

  boolean static_schedule();

  boolean nowait();

  boolean is_section();//$

  void set_section(); //$
}

class PPLCompose implements PPLProgram {
  
  public final PPLProgram P1;
  public final PPLOperator op;
  public final PPLProgram P2;
  private boolean isSection;//$
  
  public PPLCompose(PPLProgram P1,PPLOperator op,PPLProgram P2){
    this.P1=P1;
    this.op=op;
    this.P2=P2;
    if(op==PPLOperator.Fuse){
      PPLProgram Pred=P1;
      while(Pred instanceof PPLCompose){
        Pred=((PPLCompose)Pred).P2;
      }
      ((PPLParallel)P2).fused=((PPLParallel)Pred).number;
    }
    if (op==PPLOperator.Sequential){
      Set<Integer> all=get_nums(P1);
      add_nums(P2,all);
    }
  }

  private void add_nums(PPLProgram P, Set<Integer> all) {
    if (P instanceof PPLCompose){
      PPLCompose C=(PPLCompose)P;
      add_nums(C.P1,all);
      add_nums(C.P2,all);
      return;
    }
    if (P instanceof PPLParallel){
      PPLParallel par=(PPLParallel)P;
      par.preds.addAll(all);
      return;
    }
    throw new HREError("missing case %s",P.getClass());
  }

  private Set<Integer> get_nums(PPLProgram P) {
    if (P instanceof PPLCompose){
      PPLCompose C=(PPLCompose)P;
      Set<Integer> res=get_nums(C.P1);
      res.addAll(get_nums(C.P2));
      return res;
    }
    if (P instanceof PPLParallel){
      PPLParallel par=(PPLParallel)P;
      Set<Integer> res=new HashSet<Integer>();
      res.add(par.number);
      return res;
    }
    throw new HREError("missing case %s",P.getClass());
  }

  @Override
  public boolean static_schedule() {
    return P1.static_schedule() && P2.static_schedule();
  }

  @Override
  public boolean nowait() {
    return P1.nowait() && P2.nowait();
  }

 @Override
 public boolean is_section() {//$
    return isSection;
  }
   @Override
 public void set_section() { //$
    Debug("------------SAeed: set section in PPLCOmpose ");
    isSection=true;
  }
}

class PPLParallel implements PPLProgram {
  private static final AtomicInteger count=new AtomicInteger();
  
  public final OMPoption[] options;
  public final Contract contract;
  public final DeclarationStatement[] decls;
  public final BlockStatement body;
  public final int number;
  private boolean isSection;//$

  
  public int fused=0;
  public Set<Integer> preds=new HashSet<Integer>();
  
  public PPLParallel(OMPoption[] options,Contract contract,BlockStatement body,DeclarationStatement ... decls){
    this.options=options;
    this.decls=decls;
    this.body=body;
    this.contract=contract;
    number=count.incrementAndGet();
    isSection=false;//$
  }

  @Override
  public boolean static_schedule() {
    return OMPoption.static_schedule(options);
  }

  @Override
  public boolean nowait() {
    return OMPoption.nowait(options);
  }

@Override
 public boolean is_section() {//$$
    return isSection;
  }
@Override
  public void set_section() {//$$
    isSection=true;
  }

}

enum PPLOperator { Sequential, Fuse, Parallel }

public class OpenMPtoPVL extends AbstractRewriter {

  private DeclarationStatement tryIter(LoopStatement loop){
    ASTNode tmp=loop.getInitBlock();
    if (tmp instanceof BlockStatement){
      BlockStatement block=(BlockStatement)tmp;
      if (block.getLength()==1){
        tmp=block.get(0);
      }
    }
    if (tmp instanceof DeclarationStatement) {
      Debug("declaration found");
      DeclarationStatement decl=(DeclarationStatement)tmp;
      tmp=loop.getUpdateBlock();
      if (tmp instanceof BlockStatement){
        BlockStatement block=(BlockStatement)tmp;
        if (block.getLength()==1){
          tmp=block.get(0);
        }
      }
      if (tmp.isa(StandardOperator.PostIncr)||tmp.isa(StandardOperator.PreIncr)){
        Debug("increment found");
        tmp=((OperatorExpression)tmp).arg(0);
        if (tmp.isName(decl.name())){
          Debug("match");
          ASTNode upper=((OperatorExpression)loop.getEntryGuard()).arg(1);
          return create.field_decl(
              decl.name(),
              rewrite(decl.getType()),
              create.expression(StandardOperator.RangeSeq,
                  rewrite(decl.initJava()),
                  rewrite(upper)
              )
          );
        }
      }
    }
    return null;
  }
  
  @Override
  public void visit(LoopStatement loop){
    int level=0;
    ArrayList<DeclarationStatement> decls=new ArrayList<DeclarationStatement>();
    LoopStatement tmp=loop;
    for(;;){
      DeclarationStatement d=tryIter(tmp);
      if (d==null){
        level=0;
        break;
      }
      decls.add(d);
      if(FeatureScanner.isIterationContract(tmp.getContract())){
        level++;
        break;
      }
      if (tmp.getContract()==null || tmp.getContract().isEmpty()){
        if (tmp.getBody() instanceof BlockStatement){
          BlockStatement block=(BlockStatement)tmp.getBody();
          if (block.size()==1 && block.get(0) instanceof LoopStatement){
            tmp=(LoopStatement)block.get(0);
            level++;
            continue;
          }
        }
        if (tmp.getBody() instanceof LoopStatement){
          tmp=(LoopStatement)tmp.getBody();
          level++;
          continue;
        }
      }
      level=0;
      break;
    }
    if (level>0){
      ParallelBlock block=create.parallel_block(
          "auto",tmp.getContract(),
          decls.toArray(new DeclarationStatement[decls.size()]),
          rewrite((BlockStatement)tmp.getBody()));
      result=create.region(null,block);
    } else {
      super.visit(loop);
    }
  }
  
  public OpenMPtoPVL(ProgramUnit source) {
    super(source);
  }

  public void visit(BlockStatement block){
    enterCurrentBlock(create.block());
    do_block(block.getStatements(),0,block.getLength());
    result=leaveCurrentBlock();
  }
  
  @Override
  public void visit(Method m){
    switch (m.name()) {
    case "omp_get_thread_num":
    case "omp_get_num_threads":
      Warning("removing method %s", m.name());
      return;
    default:
      super.visit(m);
      return;
    }
  }
    
  public void do_block(ASTNode statements[],int from,int upto){
    for(int i=from;i<upto;i++){
      ASTNode s=statements[i];
      if(s.isDeclaration(ASTSpecial.Kind.Pragma)){
        String pragma=((ASTSpecial)s).args[0].toString();
        Debug("pragma [%s]",pragma);
        if (pragma.startsWith("omp")){
          pragma=pragma.substring(3).trim();
          OMPpragma command=OMPParser.parse(pragma);
          Debug("type is %s",command.kind);
          switch(command.kind){
          case Parallel:
            if (statements[i+1] instanceof BlockStatement){
              ASTNode src_block[]=((BlockStatement)statements[i+1]).getStatements();
              i=i+1;
//ContractBuilder
              ContractBuilder cb=new ContractBuilder();
              int lo=0;int hi=src_block.length;
              while(lo<hi && (src_block[lo] instanceof ASTSpecial)){
                ASTSpecial d=(ASTSpecial)src_block[lo];
                switch(d.kind){
                case Requires:
                  cb.requires(rewrite(d.args[0]));
                  lo++;
                  continue;
                case RequiresAndEnsures:
                  cb.requires(rewrite(d.args[0]));
                  cb.ensures(rewrite(d.args[0]));
                  lo++;
                  continue;
                case Ensures:
                  cb.ensures(rewrite(d.args[0]));
                  lo++;
                  continue;
                case Comment:
                  currentBlock.add(src_block[lo]);
                  lo++;
                  continue;
                case Pragma:{
                  String pragma2=((ASTSpecial)s).args[0].toString();
                  if (!pragma2.startsWith("omp")){
                    Abort("unexpected pragma %s",pragma2);
                  }
                  break;
                }
                default:
                  Abort("unexpected special %s",d.kind);
                }
                break;
              }
              while(hi > lo && (src_block[hi-1] instanceof ASTSpecial)){
                ASTSpecial d=(ASTSpecial)src_block[hi-1];
                switch(d.kind){
                case Ensures:
                  cb.ensures(rewrite(d.args[0]));
                  hi--;
                  continue;
                default:
                  break;
                }
                break;
              }
//ContractBuilder
              ArrayList<PPLProgram> parts=do_ppl(src_block,lo,hi);
              PPLProgram ppl_program= do_operator(parts);//$$
	      
              currentBlock.add(create.region(cb.getContract(),ppl_to_ordered(ppl_program)));
              continue;
            } else {
              create.enter(s);
              Fail("illegally placed pragma: omp %s",pragma);
            }
          case ParallelFor:{
            PPLProgram ppl_program=do_for(command.options,statements[i+1]);
            i=i+1;
            currentBlock.add(create.region(null,ppl_to_ordered(ppl_program)));
            continue;
          }
          case For:
            Fail("pragma omp for is not allowed at the top level");
          default:
            Abort("Cannot translate pragma: omp %s",pragma);
          }
        } else {
          currentBlock.add(rewrite(s));
        }
      } else {
        currentBlock.add(rewrite(s));
      }
    }
  }

  private PPLProgram do_for(OMPoption[] options, ASTNode S) {
    boolean simd=false;
    int simd_len=-1;
    for(OMPoption o:options){
      if (o.kind==OMPoption.Kind.SimdLen){
        simd_len=o.len;
      }
      if (o.kind==OMPoption.Kind.Simd){
        simd=true;
      }
    }
    if (!(S instanceof LoopStatement)){
      Fail("omp for only applies to loops");
    }
    if (simd && simd_len < 0){
      Fail("cannot do sinmd without simdlen");
    }
    LoopStatement loop=(LoopStatement)S;
    String var_name="iii";
    ASTNode lo=create.constant(0);
    ASTNode hi=create.constant(481);
    
    ASTNode init=loop.getInitBlock();
    if (init.isa(StandardOperator.Assign)){
      OperatorExpression tmp=(OperatorExpression)init;
      var_name=tmp.arg(0).toString();
      lo=rewrite(tmp.arg(1));
    } else if (init instanceof DeclarationStatement){
      DeclarationStatement tmp=(DeclarationStatement)init;
      var_name = tmp.name();
      lo=tmp.initJava();
    } else {
      Fail("missing case in for initialisation");
    }
    if (!loop.getEntryGuard().isa(StandardOperator.LT)){
      Fail("loop guard must be . < . ");
    }
    OperatorExpression cond=(OperatorExpression)loop.getEntryGuard();
    hi=rewrite(cond.arg(1));
    
    if (simd){
      lo=create.expression(StandardOperator.FloorDiv,lo,create.constant(simd_len));
      hi=create.expression(StandardOperator.FloorDiv,hi,create.constant(simd_len));
      String par_var_name="par_"+var_name;
      DeclarationStatement range=create.field_decl(par_var_name,
          create.primitive_type(PrimitiveSort.Integer),
          create.expression(StandardOperator.RangeSeq,lo,hi));
      ASTNode pv=create.local_name(par_var_name);
      ASTNode pv1=create.expression(StandardOperator.Plus,pv,create.constant(1));
      ASTNode len=create.constant(simd_len);
      DeclarationStatement vec_range=create.field_decl(var_name,
          create.primitive_type(PrimitiveSort.Integer),
          create.expression(StandardOperator.RangeSeq
              ,create.expression(StandardOperator.Mult,pv,len)
              ,create.expression(StandardOperator.Mult,pv1,len)
          ));
      BlockStatement vec_body=(BlockStatement)rewrite(loop.getBody());
      BlockStatement body=create.block();
      body.add(create.special(Kind.Assume,create.expression(StandardOperator.LTE
          ,create.expression(StandardOperator.Mult,pv1,len)
          ,create.expression(StandardOperator.Mult,hi,len)
      )));
      body.add(create.vector_block(vec_range,vec_body));
      ContractBuilder cb=new ContractBuilder();
      for(ASTNode clause:ASTUtils.conjuncts(loop.getContract().pre_condition,StandardOperator.Star)){
        cb.requires(starall(vec_range,clause));
      }
      for(ASTNode clause:ASTUtils.conjuncts(loop.getContract().post_condition,StandardOperator.Star)){
        cb.ensures(starall(vec_range,clause));
      }
      return new PPLParallel(options,cb.getContract(),body,range);
    } else {
      DeclarationStatement range=create.field_decl(var_name,
          create.primitive_type(PrimitiveSort.Integer),
          create.expression(StandardOperator.RangeSeq,lo,hi));
      BlockStatement body=(BlockStatement)rewrite(loop.getBody());
      return new PPLParallel(options,rewrite(loop.getContract()),body,range);
    }
  }

  private ASTNode starall(DeclarationStatement range,ASTNode clause){
    DeclarationStatement decl=create.field_decl(range.name(), range.getType());
    ASTNode guard=create.expression(StandardOperator.Member,
        create.local_name(range.name()), range.initJava());
    return create.starall(guard,clause,decl);
  }

  private OMPpragma ParseOMP(String pragma){
        Debug("pragma [%s]",pragma);
        if (!pragma.startsWith("omp"))
          return null;
        pragma=pragma.substring(3).trim();
        return OMPParser.parse(pragma);
 }


  private ArrayList<PPLProgram> do_ppl(ASTNode[] src_block, int lo, int hi) {
   ArrayList<PPLProgram> parts=new ArrayList<PPLProgram>();
    for(int i=lo;i<hi;i++){
      if (src_block[i].isDeclaration(ASTSpecial.Kind.Pragma)){
        String pragma=((ASTSpecial)src_block[i]).args[0].toString();
        OMPpragma command = ParseOMP(pragma);
        if(command ==null) {Warning("ignoring statement %d",i);continue;}        
        Debug("type is %s",command.kind);
        switch(command.kind){
        case Simd:
          Fail("simd only supported as for simd");
          continue;
        case For:{
          while(i+1<hi && src_block[i+1].isDeclaration(Kind.Comment)){
            i=i+1;
          }
          if (!(i+1<hi)){
            Fail("pragma without subsequent statement");
          }
          parts.add(do_for(command.options,src_block[i+1]));
          i=i+1;
          continue;
        }
        case ParallelFor:
        case Parallel:
          //Fail("pragma omp parallel is not allowed at the task list level");
	  ASTNode src_block_par[]=((BlockStatement)src_block[i+1]).getStatements();  //$$
          ArrayList<PPLProgram> parparts= do_ppl(src_block_par,0,src_block_par.length);//$$
	  PPLProgram ParPPLProg= do_operator(parparts);//$$
	  parts.add(ParPPLProg);//$$
          continue;
	case Sections: //$  //
          ASTNode src_block_sec[]=((BlockStatement)src_block[i+1]).getStatements(); 
          for(int j=0;j<src_block_sec.length;j++){
	     if (src_block_sec[j].isDeclaration(ASTSpecial.Kind.Pragma)){
		String pragma_sec=((ASTSpecial)src_block_sec[j]).args[0].toString();
                if (!pragma_sec.startsWith("omp")){
	          Warning("ignoring statement %d",i);
        	  continue;
        	}
              pragma_sec=pragma_sec.substring(3).trim();
	      OMPpragma cmd=OMPParser.parse(pragma_sec);
	      switch(cmd.kind) {
		 case Section:     		 
	         ContractBuilder cb=new ContractBuilder();
                 j=j+1;
                 while(j<src_block_sec.length && (src_block_sec[j] instanceof ASTSpecial)){
		       ASTSpecial d=(ASTSpecial)src_block_sec[j];
                       switch(d.kind){ //contract builder
			case Requires:
			  cb.requires(rewrite(d.args[0]));
			  j++;
			  continue;
			case RequiresAndEnsures:
			  cb.requires(rewrite(d.args[0]));
			  cb.ensures(rewrite(d.args[0]));
			  j++;
			  continue;
			case Ensures:
			  cb.ensures(rewrite(d.args[0]));
			  j++;
			  continue;
			case Comment:
			  currentBlock.add(src_block_sec[j]);
			  j++;
			  continue;
			case Pragma:
			  String temp=((ASTSpecial)src_block_sec[j]).args[0].toString();
			  if (!temp.startsWith("omp"))
	    			Abort("unexpected pragma %s",temp);
			  break;				
			default:
			  Abort("unexpected special %s",d.kind);
		        }//swtich contract
		        break;
	           }//while	
		   ASTNode sec_block[]=((BlockStatement)src_block_sec[j]).getStatements();			 
		   //if section includes a parallel block
		   if(sec_block[0].isDeclaration(ASTSpecial.Kind.Pragma)) //$$
		   {
			ArrayList<PPLProgram> secparts= do_ppl(sec_block,0,sec_block.length);
			PPLProgram SecPPLProg= do_operator(secparts);
			SecPPLProg.set_section();		
	                parts.add(SecPPLProg);
		   }                   
   		   else{//if section includes a sequential block 
		   	PPLParallel PPLProg = new PPLParallel(cmd.options,rewrite(cb.getContract()),(BlockStatement)src_block_sec[j]);
 		        PPLProg.set_section();		
	      	        parts.add(PPLProg);		   
		   }
                   continue;
              default:
                Fail("%s is not allowed inside sections",cmd.kind);
                break;
	         }//switch
               }//if 
	    }//for loop      	  
	  continue;
          case Section: 
	       Abort("Orphan section!");
	       continue;
          default:
               Abort("Cannot translate pragma: omp %s",pragma);
        }
      } else {
        Warning("ignoring statement %d",i);
      }
    }
return parts;
}
 private PPLProgram do_operator(ArrayList<PPLProgram> parts){//$$
    if (parts.size() == 1) {return parts.get(0);} //$$

    PPLOperator op[]=new PPLOperator[parts.size()];
    for(int i=0;i+1<parts.size();i++){
      PPLProgram P1=parts.get(i);
      PPLProgram P2=parts.get(i+1);
      if (P1.is_section() && P2.is_section()){op[i]=PPLOperator.Parallel;} //$
      else if (P1.static_schedule() && P1.nowait() && P2.static_schedule()){
        op[i]=PPLOperator.Fuse;
      } else if (P1.nowait()){
        op[i]=PPLOperator.Parallel;
      } else {
        op[i]=PPLOperator.Sequential;
      }
    }
    PPLProgram res=null;
    int i=0;
    int N=parts.size()-1;
    op[N]=PPLOperator.Sequential;
    do {
      if(res!=null) i=i+1;
      PPLProgram par=null;
      do {
        if (par!=null) i=i+1;
        PPLProgram fuse=parts.get(i);
        while(op[i]==PPLOperator.Fuse){
          i=i+1;
          fuse=new PPLCompose(fuse,PPLOperator.Fuse,parts.get(i));
        }
        if (par==null){
          par=fuse;
        } else {
          par=new PPLCompose(par,PPLOperator.Parallel,fuse);
        }
      } while(i<N && op[i]==PPLOperator.Parallel);
      if (res==null){
        res=par;
      } else {
        res=new PPLCompose(res,PPLOperator.Sequential,par);
      }
    } while(i<N);
    return res;
  }

  private ParallelBlock[] ppl_to_ordered(PPLProgram ppl_program) {
    ArrayList<ParallelBlock> blocks=new ArrayList<ParallelBlock>();
    scan(blocks,ppl_program);
    return blocks.toArray(new ParallelBlock[0]);
  }

  private void scan(ArrayList<ParallelBlock> blocks, PPLProgram ppl_program) {
    if (ppl_program instanceof PPLParallel){
      PPLParallel par=(PPLParallel)ppl_program;
      ArrayList<ASTNode> dep_list=new ArrayList<ASTNode>();
      if(par.fused>0){
        dep_list.add(create.invokation(null,null,"omp_"+par.fused,
            create.unresolved_name(par.decls[0].name())));
      } else {
        for(int no:par.preds){
          dep_list.add(create.unresolved_name("omp_"+no));
        }
      }
      ASTNode deps[]=dep_list.toArray(new ASTNode[0]);
      blocks.add(create.parallel_block("omp_"+par.number,par.contract,par.decls,par.body,deps));
      return;
    }
    if (ppl_program instanceof PPLCompose){
      PPLCompose par=(PPLCompose)ppl_program;
      scan(blocks,par.P1);
      scan(blocks,par.P2);
      return;
    }
    throw new HREError("missing case %s",ppl_program.getClass());
  }

  private Stack<BlockStatement> currentBlockStack=new Stack<BlockStatement>();
  
  private BlockStatement leaveCurrentBlock() {
    BlockStatement res=currentBlock;
    currentBlock=currentBlockStack.pop();
    return res;
  }

  private void enterCurrentBlock(BlockStatement block) {
    currentBlockStack.push(currentBlock);
    currentBlock=block;
  }
  
}
