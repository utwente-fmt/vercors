package vct.antlr4.parser;

import hre.lang.HREError;

import java.util.ArrayList;
import java.util.HashMap;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import vct.antlr4.generated.PVFullLexer;
import vct.antlr4.generated.PVFullParser;
import vct.antlr4.generated.PVFullParser.*;
import vct.antlr4.generated.PVFullVisitor;

import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.generic.ASTSequence;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.generic.BeforeAfterAnnotations;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.expr.Dereference;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.composite.ParallelBlock;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.Type;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.stmt.decl.Method.Kind;
import vct.col.ast.stmt.decl.VariableDeclaration;
import vct.col.rewrite.InferADTTypes;
import vct.col.syntax.PVLSyntax;
import vct.col.syntax.Syntax;

import static hre.lang.System.*;
import static vct.col.ast.type.ASTReserved.*;

/**
 * Convert ANTLR parse trees for PVL to COL.
 * 
 * @author <a href="mailto:s.c.c.blom@utwente.nl">Stefan Blom</a>
*/
public class PVLtoCOL extends ANTLRtoCOL implements PVFullVisitor<ASTNode> {

  public static ProgramUnit convert(ParseTree tree, String file_name,BufferedTokenStream tokens,org.antlr.v4.runtime.Parser parser) {
    ProgramUnit unit=new ProgramUnit();
    PVLtoCOL visitor=new PVLtoCOL(unit,PVLSyntax.get(),file_name,tokens,parser);
    visitor.scan_to(unit,tree);
    return unit;
  }
  public PVLtoCOL(ASTSequence<?> unit,Syntax syntax, String filename, BufferedTokenStream tokens,org.antlr.v4.runtime.Parser parser) {
    super(unit, false, syntax, filename, tokens,parser,PVFullLexer.Identifier,PVFullLexer.class);
  }

  @Override
  public ASTNode visitClaz(ClazContext ctx) {
    if (match(ctx,"ValDeclaration")){
      return convert(ctx,1);
    }
    Contract c;
    if (((ParserRuleContext)ctx.getChild(0)).children==null){
      c=null;
    } else {
      c=(Contract)convert(ctx,0);
    }
    String name=getIdentifier(ctx,2);
    ASTClass cl=create.ast_class(name,ClassKind.Plain,null,null,null);
    for(int i=4;i<ctx.children.size()-1;i++){
      ASTNode node=convert(ctx.children.get(i));
      if (node.isValidFlag(ASTNode.STATIC) && node.isStatic()){
        cl.add_static(node);
      } else {
        cl.add_dynamic(node);
      } 
    }
    cl.setContract(c);
    cl.setFlag(ASTFlags.FINAL, true);
    return cl;
  }

  @Override
  public ASTNode visitContract(ContractContext ctx) {
    ContractBuilder cb=new ContractBuilder();
    Debug("contract %s",ctx.toStringTree());
    int N=ctx.getChildCount();
    for(int i=0;i<N;i++){
      ParserRuleContext ctx2=(ParserRuleContext)ctx.getChild(i);
      if (match(ctx2,"accessible",null,";")){
        cb.accesses(convert(ctx2,1));
        continue;
      }
      if (match(ctx2,"modifies",null,";")){
        cb.modifies(convert(ctx2,1));
        continue;
      }
      if (match(ctx2,"context",null,";")){
        ASTNode expr=convert(ctx2,1);
        cb.requires(expr);
        cb.ensures(expr);
        continue;
      }
      if (match(ctx2,"requires",null,";")){
        cb.requires(convert(ctx2,1));
        continue;
      }
      if (match(ctx2,"ensures",null,";")){
        cb.ensures(convert(ctx2,1));
        continue;
      }
      if (match(ctx2,"context_everywhere",null,";")){
        cb.appendInvariant(convert(ctx2,1));
        continue;
      }
      if (match(ctx2,"given",null,null,";")){
        cb.given(create.field_decl(getIdentifier(ctx2, 2),checkType(convert(ctx2,1))));
        continue;
      }
      if (match(ctx2,"yields",null,null,";")){
        cb.yields(create.field_decl(getIdentifier(ctx2, 2),checkType(convert(ctx2,1))));
        continue;
      }
      throw new HREError("missing case %s",ctx2.toStringTree(parser));
    }
    Contract contract = cb.getContract(false);
    contract.setOrigin(origin(ctx.start,ctx.stop));
    return contract;
  }

  @Override
  public ASTNode visitArgs(ArgsContext ctx) {
    throw hre.lang.System.Failure("illegal call to visitArgs");
  }
  
  private DeclarationStatement[] convertArgs(ArgsContext ctx){
    ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
    DeclarationStatement empty[]=new DeclarationStatement[0];
    if (ctx.children==null) return empty;
    int N=ctx.children.size();
    for(int i=0;i<N;i+=3){
      Type type=(Type)convert(ctx.children.get(i));
      String name=getIdentifier(ctx, i+1);
      args.add(create.field_decl(name, type));
    }
    return args.toArray(empty);
  }

  @Override
  public ASTNode visitTuple(TupleContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitFence_list(Fence_listContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitBlock(BlockContext ctx) {
    BlockStatement block=create.block();
    int N=ctx.children.size()-1;
    for(int i=1;i<N;i++){
      block.addStatement(convert(ctx.children.get(i)));
    }
    return block;
  }

  @Override
  public ASTNode visitExpr(ExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitIteExpr(IteExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitImplicationExpr(ImplicationExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitAndOrExpr(AndOrExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitEqExpr(EqExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitRelExpr(RelExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitSetExpr(SetExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitAddExpr(AddExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitMultExpr(MultExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitPowExpr(PowExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitSeqAddExpr(SeqAddExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitUnaryExpr(UnaryExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitNewExpr(NewExprContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitTarget(TargetContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitNonTarget(NonTargetContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitNonTargetUnit(NonTargetUnitContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitTargetUnit(TargetUnitContext ctx) {
    return doExpr(ctx);
  }

  @Override
  public ASTNode visitBuiltinMethod(BuiltinMethodContext ctx) {
    return new NameExpression(ctx.getChild(0).toString());
  }

  public ASTNode add_new_dims(Type type, New_dimsContext ctx) {
    type = create.primitive_type(PrimitiveSort.Cell, type);
    ASTNode[] newArrayArgs = new ASTNode[ctx.getChildCount() / 3];

    for(int i = 0; i < ctx.getChildCount();) {
      if(match(i, true, ctx, "[", "Expr", "]")) {
        // The lengths are included in a bare type, but that is just a marker in order to generate the zero value of
        // declared variables. The length has no bearing on the actual type.
        type = create.primitive_type(PrimitiveSort.Array, type);
        newArrayArgs[i / 3] = convert(ctx, 1);
        i += 3;
      } else {
        Fail("Unimplemented: has the grammar of PVL related to arrays changed?");
      }
    }

    type = create.primitive_type(PrimitiveSort.Option, type);
    return create.expression(StandardOperator.NewArray, type, newArrayArgs);
  }
  
  public ASTNode doExpr(ParserRuleContext ctx){
    if (ctx.children.size()==1){
      ASTNode res=try_specials(ctx.children.get(0).toString());
      if (res!=null) return res;
      return convert(ctx, 0);
    }
    if (ctx.children.get(0) instanceof TerminalNode){
      String ops=ctx.children.get(0).toString();
      StandardOperator op=syntax.parseFunction(ops);
      if (op!=null){
        ASTNode args[]=getTuple((ParserRuleContext)ctx.children.get(1));
        return create.expression(op, args);
      }
    }
    if (match(ctx,"(","\\forall*",null,null,";",null,";",null,")")){
      return create.starall(convert(ctx,5),convert(ctx,7),create.field_decl(getIdentifier(ctx,3),(Type)convert(ctx,2)));
    }
    if (match(ctx,"(","\\forall",null,null,";",null,";",null,")")){
      return create.forall(convert(ctx,5),convert(ctx,7),create.field_decl(getIdentifier(ctx,3),(Type)convert(ctx,2)));
    }
    if (match(ctx,"(","\\exists",null,null,";",null,";",null,")")){
      return create.exists(convert(ctx,5),convert(ctx,7),create.field_decl(getIdentifier(ctx,3),(Type)convert(ctx,2)));
    }
    if (match(ctx,"(","\\sum",null,null,";",null,";",null,")")){
      return create.summation(convert(ctx,5),convert(ctx,7),create.field_decl(getIdentifier(ctx,3),(Type)convert(ctx,2)));
    }
    if (match(0,true,ctx,"[",null,"]",null)){
      return create.expression(StandardOperator.Scale,convert(ctx,1),convert(ctx,3)); 
    }
    if (match(ctx,"!",null)){
      return create.expression(StandardOperator.Not,convert(ctx,1));
    }
    if (match(ctx,null,"?",null,":",null)){
      return create.expression(StandardOperator.ITE,convert(ctx,0),convert(ctx,2),convert(ctx,4));
    }
    if (match(ctx,"(",null,")")){
      return convert(ctx,1);
    }
    if (match(ctx,null,tuple)){
      ASTNode args[] = getTuple((ParserRuleContext) ctx.getChild(1));

      ASTNode left = convert(ctx, 0);

      if(left instanceof NameExpression) {
        String name = ((NameExpression) left).getName();

        StandardOperator op;
        if((op = syntax.parseFunction(name)) != null) {
          return create.expression(op, args);
        } else {
          return create.invokation(null, null, name, args);
        }
      } else if(left instanceof Dereference){
        return create.invokation(((Dereference) left).obj(), null, ((Dereference) left).field(), args);
      } else {
        Abort("Method call on node of type %s not supported", left.getClass());
      }
    }
    if (match(ctx,"new",null,tuple)){
      ASTNode args[]=getTuple((ParserRuleContext)ctx.getChild(2));
      String name=getIdentifier(ctx,1);
      return create.new_object(create.class_type(name), args);
    }
    if (match(ctx,"new",null,"New_dims")){
      Type t = checkType(convert(ctx,1));
      return add_new_dims(t, (New_dimsContext) ctx.getChild(2));
    }
    if (match(ctx,null,":",null)){
      ASTNode res=convert(ctx,2);
      String name=getIdentifier(ctx,0);
      res.addLabel(create.label(name));
      return res;
    }
    if (match(ctx,null,"with",null)){
      ASTNode tmp=convert(ctx,0);
      if (tmp instanceof BeforeAfterAnnotations){
        BeforeAfterAnnotations res=(BeforeAfterAnnotations)tmp;
        BlockStatement block=(BlockStatement)convert(ctx,2);
        res.set_before(block);
        return tmp;
      } else {
        Fail("%s: with block not allowed here",create.getOrigin());
      }
    }
    if (match(ctx,null,"then",null)){
      ASTNode tmp=convert(ctx,0);
      if (tmp instanceof BeforeAfterAnnotations){
        BeforeAfterAnnotations res=(BeforeAfterAnnotations)tmp;
        BlockStatement block=(BlockStatement)convert(ctx,2);
        res.set_after(block);
        return tmp;
      } else {
        Fail("%s: then block not allowed here",create.getOrigin());
      }
    }
    if(match(ctx,null,"->",null,tuple)){
      ASTNode args[]=getTuple((ParserRuleContext)ctx.getChild(3));
      String method=getIdentifier(ctx,2);
      ASTNode object=convert(ctx,0);
      return create.expression(StandardOperator.Implies,
        create.expression(StandardOperator.NEQ,object,create.reserved_name(ASTReserved.Null)),
        create.invokation(object,null, method, args)
      );
    }

    // matching a dereference operation (for example `o.f`)
    if (match(ctx,null, ".", null)) {
      return create.dereference(convert(ctx, 0), getGeneralizedIdentifier(ctx,2));
    }

    // matching an array indexing (for example `a[1]`)
    if(match(ctx,null, "[", null, "]")) {
      return create.expression(StandardOperator.Subscript, convert(ctx, 0), convert(ctx,2));
    }

    // matching a take operation (for example `xs[..2]`)
    if(match(ctx, null, "[", "..", null, "]")) {
      return create.expression(StandardOperator.Take, convert(ctx, 0), convert(ctx,3));
    }

    // matching a drop operation (for example `xs[4..]`)
    if(match(ctx, null, "[", null, "..", "]")) {
      return create.expression(StandardOperator.Drop, convert(ctx, 0), convert(ctx,2));
    }

    // matching a slicing operation (for example `xs[2..4]`)
    if(match(ctx, null, "[", null, "..", null, "]")) {
      return create.expression(StandardOperator.Slice, convert(ctx, 0), convert(ctx,2), convert(ctx,4));
    }

    // matching a sequence updating operation (for example `xs[2 -> 43]`)
    if(match(ctx, null, "[", null, "->", null, "]")) {
      return create.expression(StandardOperator.SeqUpdate, convert(ctx, 0), convert(ctx,2), convert(ctx,4));
    }

    // Initializing a map
    if(match(ctx, "map", "<", null, ",", null, ">", "{","}")) {
      Type t1 = checkType(convert(ctx,2));
      Type t2 = checkType(convert(ctx,4));
      return create.struct_value(create.primitive_type(PrimitiveSort.Map, t1, t2), null);
    }
    return visit(ctx);
  }

  @Override
  public ASTNode visitCollectionConstructors(CollectionConstructorsContext ctx) {
    // Sequence Constructors
    if (match(ctx,"seq","<",null,">",null)){
      Type t=checkType(convert(ctx,2));
      ASTNode args[]=convert_list((ParserRuleContext)ctx.getChild(4),"{",",","}");
      return create.struct_value(create.primitive_type(PrimitiveSort.Sequence,t),null,args);
    }
    if (match(ctx, "[", null, "]")) {
      ASTNode[] args = getValues((ParserRuleContext)ctx.children.get(1));
      return create.struct_value(create.primitive_type(PrimitiveSort.Sequence, create.type_variable(InferADTTypes.typeVariableName())),null,args);
    }
    if (match(ctx, "[t:", type_expr, "]")) {
      Type t=checkType(convert(ctx,1));
      return create.struct_value(create.primitive_type(PrimitiveSort.Sequence, t), null);
    }

    // Set Constructors
    if (match(ctx,"set","<",null,">",null)){
      Type t=checkType(convert(ctx,2));
      ASTNode args[]=convert_list((ParserRuleContext)ctx.getChild(4),"{",",","}");
      return create.struct_value(create.primitive_type(PrimitiveSort.Set,t),null,args);
    }
    if (match(ctx, "{", null, "}")) {
      ASTNode[] args = getValues((ParserRuleContext)ctx.children.get(1));
      return create.struct_value(create.primitive_type(PrimitiveSort.Set, create.type_variable(InferADTTypes.typeVariableName())),null,args);
    }
    if (match(ctx, "{t:", type_expr, "}")) {
      Type t=checkType(convert(ctx,1));
      return create.struct_value(create.primitive_type(PrimitiveSort.Set, t), null);
    }

    // Bag Constructors
    if (match(ctx,"bag","<",null,">",null)){
      Type t=checkType(convert(ctx,2));
      ASTNode args[]=convert_list((ParserRuleContext)ctx.getChild(4),"{",",","}");
      return create.struct_value(create.primitive_type(PrimitiveSort.Bag,t),null,args);
    }
    if (match(ctx, "b{", null, "}")) {
      ASTNode[] args = getValues((ParserRuleContext)ctx.children.get(1));
      return create.struct_value(create.primitive_type(PrimitiveSort.Bag, create.type_variable(InferADTTypes.typeVariableName())),null,args);
    }
    if (match(ctx, "b{t:", type_expr, "}")) {
      Type t=checkType(convert(ctx,1));
      return create.struct_value(create.primitive_type(PrimitiveSort.Bag, t), null);
    }

    return visit(ctx);
  }

  private ASTNode[] getValues(ParserRuleContext ctx) {
    if (ctx.children == null) {
      return new ASTNode[0];
    }
    int N=(ctx.children.size()+1)/2;
    ASTNode res[]=new ASTNode[N];
    for(int i=0;i<N;i++){
      res[i]=convert(ctx,2*i);
    }
    return res;
  }

  @Override
  public ASTNode visitArguments(ArgumentsContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitNew_dims(New_dimsContext ctx) {
    return null;
  }

  private ASTNode add_dims(ASTNode type, Type_dimsContext ctx) {
    if(ctx.getChildCount() == 0) {
      return type;
    }

    type = create.primitive_type(PrimitiveSort.Cell, type);

    for(int i = 0; i < ctx.getChildCount();) {
      if(match(i, true, ctx, "[", "Expr", "]")) {
        type = create.primitive_type(PrimitiveSort.Array, type);
        i += 3;
      } else if(match(i, true, ctx, "[", "]")) {
        type = create.primitive_type(PrimitiveSort.Array, type);
        i += 2;
      } else {
        Fail("Unimplemented: has the grammar of PVL related to arrays changed?");
      }
    }

    type = create.primitive_type(PrimitiveSort.Option, type);
    return type;
  }

  @Override
  public ASTNode visitNon_array_type(Non_array_typeContext ctx) {
    ASTNode res=null;
    if (match(ctx,"option","<",null,">")){
      Type t=checkType(convert(ctx,2));
      return create.primitive_type(PrimitiveSort.Option,t);
    }
    if (match(ctx,"seq","<",null,">")){
      Type t=checkType(convert(ctx,2));
      return create.primitive_type(PrimitiveSort.Sequence,t);
    }
    if (match(ctx,"set","<",null,">")){
      Type t=checkType(convert(ctx,2));
      return create.primitive_type(PrimitiveSort.Set,t);
    }
    if (match(ctx,"bag","<",null,">")){
      Type t=checkType(convert(ctx,2));
      return create.primitive_type(PrimitiveSort.Bag,t);
    }
    if (match(ctx,"map","<",null,",", null, ">")){
      Type t1=checkType(convert(ctx,2));
      Type t2=checkType(convert(ctx,4));
      return create.primitive_type(PrimitiveSort.Map,t1, t2);
    }
    if (match(ctx,null,"<",null,">")) {
      String name=getIdentifier(ctx,0);
      ASTNode arg=convert(ctx,2);
      return create.class_type(name,arg);
    }
    if (match(0,true,ctx,"TerminalNode")){
      String type=ctx.children.get(0).toString();
      PrimitiveSort S=null;
      for (PrimitiveSort s : PrimitiveSort.values()){
        if (type.equals(syntax.getPrimitiveType(s))){
          S=s;
          break;
        }
      }
      if (S==null){
        res=create.class_type(type);
      } else {
        res=create.primitive_type(S);
      }
    } else if (match(0,true,ctx,"ClassType")) {
      res=checkType(convert(ctx,0));
    } else {
      Fail("unknown type %s",ctx.toStringTree(parser));
    }

    return res;
  }

  @Override
  public ASTNode visitType(TypeContext ctx) {
    return add_dims(convert(ctx, 0), (Type_dimsContext) ctx.getChild(1));
  }

  @Override
  public ASTNode visitType_dims(Type_dimsContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitKernel(KernelContext ctx) {
    String name=getIdentifier(ctx,1);
    ASTClass cl=create.ast_class(name,ClassKind.Kernel,null,null,null);
    for(int i=3;i<ctx.children.size()-1;i++){
      ASTNode tmp=convert(ctx.children.get(i));
      if (tmp.isStatic()){
        cl.add_static(tmp);
      } else {
        cl.add_dynamic(tmp);
      }
    }
    return cl;
  }

  @Override
  public ASTNode visitMethod_decl(Method_declContext ctx) {
    Contract c=(Contract) convert(ctx.children.get(0));
    Type returns=(Type)convert(ctx.children.get(2));
    String name=getIdentifier(ctx,3);
    DeclarationStatement args[]=convertArgs((ArgsContext) ctx.children.get(5));
    Kind kind=Kind.Plain;
    ASTNode body;
    if (match(7,false,ctx,";")){
      body=null;
      if (returns.isPrimitive(PrimitiveSort.Resource)) {
        kind=Kind.Predicate;
      }
    } else if (match(7,false,ctx,"=",null,";")){
      body=convert(ctx,8);
      if (returns.isPrimitive(PrimitiveSort.Resource)) {
        kind=Kind.Predicate;
      } else {
        kind=Kind.Pure;
      }
    } else {
      body=convert(ctx,7);
    }
    ParserRuleContext flags=(ParserRuleContext)ctx.getChild(1);
    for(int i=0;i<flags.getChildCount();i++){
      if (match(i,true,flags,"pure")){
        kind=Kind.Pure;
      }
    }
    ASTNode res=create.method_kind(kind,returns,c, name, args ,body);
    scan_modifiers(flags, res);
    return res;
  }
  
  
  private boolean scan_pure(ParserRuleContext ctx,int i) {
    return scan_pure((ParserRuleContext)ctx.getChild(i));
  }
  private void scan_modifiers(ParserRuleContext ctx,int i,ASTNode res) {
    scan_modifiers((ParserRuleContext)ctx.getChild(i),res);
  }
  
  private boolean scan_pure(ParserRuleContext flags) {
    for(int i=0;i<flags.getChildCount();i++){
      if (match(i,true,flags,"pure")){
        return true;
      }
    }
    return false;
  }
  
  private void scan_modifiers(ParserRuleContext flags,ASTNode res) {
    res.setStatic(false);
    for(int i=0;i<flags.getChildCount();i++){
      if (match(i,true,flags,"pure")){
        // already accounted for
      } else if (match(i,true,flags,"static")){
         res.setStatic(true);
      } else if (match(i,true,flags,"thread_local")){
        res.setFlag(ASTFlags.THREAD_LOCAL,true);
      } else if (match(i,true,flags,"inline")){
        res.setFlag(ASTFlags.INLINE,true);
      } else {
        Fail("Unknown modifier %s",flags.getChild(i).toStringTree());
      }
    }
  }
  
  private String type_expr="TypeContext";
  private String tuple="TupleContext";

  @Override
  public ASTNode visitPar_unit(Par_unitContext ctx) {
    if (match(ctx,"Contract","Block")){
      Contract c=(Contract)convert(ctx, 0);
      BlockStatement block=(BlockStatement)convert(ctx, 1);
      return create.parallel_block("", c, new DeclarationStatement[0], block);
    }
    if(match(ctx,"(",null,")","Contract","Block")){
      DeclarationStatement iters[]=checkDecls(convert_list((ParserRuleContext)ctx.getChild(1), ","));
      Contract c=(Contract)convert(ctx, 3);
      BlockStatement block=(BlockStatement)convert(ctx, 4);
      return create.parallel_block("", c, iters, block);      
    }
    if (match(ctx,null,"(",null,")",null,null)){
      String label=getIdentifier(ctx, 0);
      DeclarationStatement iters[]=checkDecls(convert_list((ParserRuleContext)ctx.getChild(2), ","));
      Contract c=(Contract)convert(ctx, 4);
      BlockStatement block=(BlockStatement)convert(ctx, 5);
      return create.parallel_block(label, c, iters, block);
    }
    if (match(ctx,null,"(",null,";",null,")",null,null)){
      String label=getIdentifier(ctx, 0);
      DeclarationStatement iters[]=checkDecls(convert_list((ParserRuleContext)ctx.getChild(2), ","));
      ASTNode deps[]=convert_list((ParserRuleContext)ctx.getChild(4), ",");
      Contract c=(Contract)convert(ctx, 6);
      BlockStatement block=(BlockStatement)convert(ctx, 7);
      return create.parallel_block(label, c, iters, block, deps);
    }
    return null;
  }


  @Override
  public ASTNode visitValStatement(ValStatementContext ctx) {
    return getValStatement(ctx);
  }
 
  
  @Override
  public ASTNode visitStatement(StatementContext ctx) {
    if (match(ctx,"vec","(",null,")",null)){
      DeclarationStatement iter=(DeclarationStatement)convert(ctx,2);
      BlockStatement block=(BlockStatement)convert(ctx,4);
      return create.vector_block(iter,block);
    }
    if (match(0,true,ctx,"Contract","par","Par_unit")){
      Contract c;
      if (((ContractContext)ctx.getChild(0)).getChildCount()>0){
        c=(Contract)convert(ctx,0);
      } else {
        c=null;
      }
      int offset=1;
      ArrayList<ParallelBlock> res=new ArrayList<ParallelBlock>();
      do {
        ParallelBlock blk=(ParallelBlock)convert(ctx,offset+1);
        res.add(blk);
        offset+=2;
      } while (match(offset,true,ctx,"and","Par_unit"));
      if (offset == ctx.getChildCount()){
        return create.region(c,res);
      }
      Warning("incomplete match of parallel region");
    }
    if (match(ctx,"invariant",null,"(",null,")",null)){
      String label=getIdentifier(ctx, 1);
      ASTNode inv=convert(ctx, 3);
      ASTNode block=convert(ctx, 5);
      return create.invariant_block(label,inv,(BlockStatement)block);
    }
    if (match(ctx,"atomic","(",null,")",null)){
      return create.parallel_atomic(
          (BlockStatement)convert(ctx,4),
          getIdentifierList((ParserRuleContext)ctx.getChild(2), ","));
    }
    if (match(ctx,"return",";")){
      return create.return_statement();
    }
    if (match(ctx,"return",null,";")){
      return create.return_statement(convert(ctx,1));
    }
    if (match(ctx,"if","(",null,")",null)){
      return create.ifthenelse(convert(ctx,2),convert(ctx,4));
    }
    if (match(ctx,"if","(",null,")",null,"else",null)){
      return create.ifthenelse(convert(ctx,2),convert(ctx,4),convert(ctx,6));
    }
    if (match(ctx,null,"while","(",null,")",null)){
      PVFullParser.InvariantContext inv_ctx=(PVFullParser.InvariantContext)ctx.children.get(0);
      int N = (inv_ctx.children==null) ? 0 : inv_ctx.children.size()/3;
      ASTNode invs[]=new ASTNode[N];
      for(int i=0;i<N;i++){
        invs[i]=convert(inv_ctx,3*i+1);
      }
      return create.while_loop(convert(ctx,3),convert(ctx,5),invs);
    }
    if (match(ctx, null, "for", "(", null, ";", null, ";", null, ")", null)) {
      PVFullParser.InvariantContext invariantContext = (PVFullParser.InvariantContext)ctx.children.get(0);
      int invariantCount = (invariantContext.children == null) ? 0 : invariantContext.children.size() / 3;
      ASTNode invariants[] = new ASTNode[invariantCount];

      for(int i = 0; i < invariantCount; i++) {
        invariants[i] = convert(invariantContext, 3*i + 1);
      }

      ASTNode init = convert(ctx.children.get(3));
      ASTNode condition = convert(ctx.children.get(5));
      ASTNode action = convert(ctx.children.get(7));
      ASTNode body = convert(ctx.children.get(9));

      return create.for_loop(init, condition, action, body, invariants);
    }
    
    if (match(ctx,"action",tuple,null)){
      ASTNode args[] = getTuple((ParserRuleContext)ctx.getChild(1));
      ASTNode block=convert(ctx,2);
      HashMap<String,ASTNode> map=new HashMap<String,ASTNode>();
      if (args.length < 4){
        throw new HREError("missing arguments in action");
      }
      for(int i=4;i<args.length;i+=2){
        if(!(args[i] instanceof NameExpression)){
          throw new HREError("argument %d of action is not a name",i);
        }
        String name=((NameExpression)args[i]).getName();
        if (i+1==args.length){
          throw new HREError("argument %d of action is missing",i+1);
        }
        map.put(name,args[i+1]);
      }
      return create.action_block(args[0],args[1],args[2],args[3], map, block);
    }

    if (match(ctx,"lock",null,";")){
      return create.special(ASTSpecial.Kind.Lock,convert(ctx,1));
    }
    if (match(ctx,"unlock",null,";")){
      return create.special(ASTSpecial.Kind.Unlock,convert(ctx,1));
    }
    if (match(ctx,"wait",null,";")){
      return create.special(ASTSpecial.Kind.Wait,convert(ctx,1));
    }
    if (match(ctx,"notify",null,";")){
      return create.special(ASTSpecial.Kind.Notify,convert(ctx,1));
    }
    if (match(ctx,"fork",null,";")){
      return create.special(ASTSpecial.Kind.Fork,convert(ctx,1));
    }
    if (match(ctx,"join",null,";")){
      return create.special(ASTSpecial.Kind.Join,convert(ctx,1));
    }
    if (match(ctx,"goto",null,";")){
      return create.special(ASTSpecial.Kind.Goto,convert(ctx,1));
    }
    if (match(0,true,ctx,"barrier","(",null)){
      String name=getIdentifier(ctx, 2);
      ArrayList<String> invs=new ArrayList<String>();
      int offset;
      if (match(0,true,ctx,"barrier","(",null,";",null,")")){
        ASTNode tags[]=convert_list((ParserRuleContext)ctx.children.get(4),",");
        for (ASTNode tag:tags){
          invs.add(tag.toString());
        }
        offset=6;
      } else {
        offset=4;
      }
      Contract c=null;
      BlockStatement body=null;
      if (match(offset,false,ctx,null,null)){
        c=(Contract)convert(ctx,offset);
        body=(BlockStatement)convert(ctx,offset+1);
        return create.barrier(name,c,invs,body);
      }
      if (match(offset,false,ctx,"{",null,"}")){
        c=(Contract)convert(ctx,offset+1);
        return create.barrier(name,c,invs,body);
      }
    }
    if (match(ctx, "AllowedForStatementContext", ";")) {
      return convert(ctx, 0);
    }
    return visit(ctx);
  }

  @Override
  public ASTNode visitForStatementList(ForStatementListContext ctx) {
    ArrayList<ASTNode> statementList = new ArrayList<ASTNode>();

    for(int i = 0; i < ctx.children.size(); i += 2) {
      statementList.add(convert(ctx, i));
    }

    return create.block(statementList.toArray(new ASTNode[0]));
  }

  @Override
  public ASTNode visitAllowedForStatement(AllowedForStatementContext ctx) {
    if (match(ctx,type_expr,null)){
      return create.field_decl(getIdentifier(ctx,1),(Type)convert(ctx,0));
    }
    if (match(ctx,type_expr,null,"=",null)){
      return create.field_decl(getIdentifier(ctx,1),(Type)convert(ctx,0),convert(ctx,3));
    }
    if (match(ctx,"ExprContext")){
      return convert(ctx,0);
    }
    // the (postfix) incremental statement
    if (match(ctx, null, "++")) {
      return create.postfix_increment(getIdentifier(ctx, 0));
    }
    // the (postfix) decremental statement
    if (match(ctx, null, "--")) {
      return create.postfix_decrement(getIdentifier(ctx, 0));
    }
    if (match(ctx,null,"=",null)){
      return create.assignment(convert(ctx,0),convert(ctx,2));
    }

    return visit(ctx);
  }

  private DeclarationStatement[] checkDecls(ASTNode[] list) {
    DeclarationStatement res[]=new DeclarationStatement[list.length];
    for(int i=0;i<list.length;i++){
      if(!(list[i] instanceof DeclarationStatement)){
        Fail("not a declaration...");
      }
      res[i]=(DeclarationStatement)list[i];
    }
    return res;
  }

  @Override
  public ASTNode visitKernel_field(Kernel_fieldContext ctx) {
    ASTNode res;
    if (ctx.children.size()==4){
      res=create.field_decl(getIdentifier(ctx,2),(Type)convert(ctx.children.get(1)));
    } else {
      VariableDeclaration decl=new VariableDeclaration((Type)convert(ctx.children.get(1)));
      int N=ctx.children.size();
      for(int i=2;i<N;i+=2){
        String name=getIdentifier(ctx,i);
        Type t=create.class_type(name);
        decl.add(create.field_decl(name,t));
      }
      decl.setOrigin(create.getOrigin());
      res=decl;
    }
    String keyword=ctx.children.get(0).toString();
    switch(keyword){
    case "global":
      res.setStatic(true);
      break;
    case "local":
      res.setStatic(false);
      break;
    default:
      Fail("bad variable class %s",keyword);
    }
    return res;
  }

  @Override
  public ASTNode visitField(FieldContext ctx) {
    if (ctx.children.size()==3){
      return create.field_decl(getIdentifier(ctx,1),(Type)convert(ctx.children.get(0)));
    } else {
      VariableDeclaration decl=new VariableDeclaration((Type)convert(ctx.children.get(0)));
      int N=ctx.children.size();
      for(int i=1;i<N;i+=2){
        String name=getIdentifier(ctx,i);
        Type t=create.class_type(name);
        decl.add(create.field_decl(name,t));
      }
      decl.setOrigin(create.getOrigin());
      return decl;
    }
  }

  @Override
  public ASTNode visitInvariant(InvariantContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitProgram(ProgramContext ctx) {
    return null;
  }

  private ASTNode[] getTuple(ParserRuleContext ctx){
    int N=(ctx.children.size()-1)/2;
    ASTNode res[]=new ASTNode[N];
    for(int i=0;i<N;i++){
      res[i]=convert(ctx,2*i+1);
    }
    return res;
  }

  private ASTNode try_specials(String text){
    ASTReserved res=syntax.reserved(text);
    if (res!=null){
      return create.reserved_name(res);
    }
    switch(text){
    case "tcount":
    case "gsize":
    case "tid":
    case "gid":
    case "lid":
      return create.unresolved_name(text);
    case "this":
      return create.reserved_name(This);
    case "\\result": return create.reserved_name(Result);
    case "null": return create.reserved_name(Null);
    case "true": return create.constant(true);
    case "false": return create.constant(false);
    }
    return null;
  }
  
  @Override
  public ASTNode visitTerminal(TerminalNode node){
    Token tok=node.getSymbol();
    ASTNode res=try_specials(tok.getText());
    if (res!=null) return res;
    switch(tok.getType()){
    case PVFullLexer.Identifier:
      return create.unresolved_name(tok.getText());
    case PVFullLexer.NUMBER:
      return create.constant(Integer.parseInt(tok.getText()));
    case PVFullLexer.EOF:
      return null;
    }
    Fail("At %s: unimplemented terminal node",create.getOrigin());
    return visit(node);
  }
  @Override
  public ASTNode visitConstructor(ConstructorContext ctx) {
    Contract c=(Contract) convert(ctx.children.get(0));
    String name=getIdentifier(ctx,1);
    DeclarationStatement args[]=convertArgs((ArgsContext) ctx.children.get(3));
    ASTNode body=convert(ctx.children.get(5));
    Type returns=create.primitive_type(PrimitiveSort.Void);
    ASTNode res=create.method_kind(Kind.Constructor,returns,c, name, args ,body);
    res.setStatic(false);
    return res;
  }
  
  @Override
  public ASTNode visitTypeArgs(TypeArgsContext ctx) {
    
    return null;
  }
  @Override
  public ASTNode visitClassType(ClassTypeContext ctx) {
    String name=getIdentifier(ctx,0);
    ASTNode args[];
    if (ctx.children.size() >1){
      args=convert_list((ParserRuleContext)ctx.getChild(1),"<",",",">");
    } else {
      args=new ASTNode[0];
    }
    return create.class_type(name,args);
  }
  @Override
  public ASTNode visitValues(ValuesContext ctx) {
    if (match(0,true,ctx,"{",null)){
      ASTNode args[]=convert_list(ctx,"{",",","}");
      Type t=create.primitive_type(PrimitiveSort.Set,create.primitive_type(PrimitiveSort.Location));
      return create.struct_value(t,null,args); 
    }
    return null;
  }

  @Override
  public ASTNode visitIters(ItersContext ctx) {
    
    return null;
  }
  @Override
  public ASTNode visitIter(IterContext ctx) {
    return create.field_decl(getIdentifier(ctx,1),checkType(convert(ctx,0)),
        create.expression(StandardOperator.RangeSeq,convert(ctx,3),convert(ctx,5)));
  }
  @Override
  public ASTNode visitDecls(DeclsContext ctx) {
    
    return null;
  }
  @Override
  public ASTNode visitDecl(DeclContext ctx) {
    if (match(ctx,null,null,"=",null)){
      return create.field_decl(getIdentifier(ctx,1),checkType(convert(ctx,0)),convert(ctx,3));
    } else {
      return create.field_decl(getIdentifier(ctx,1),checkType(convert(ctx,0)));
    }
  }
  @Override
  public ASTNode visitWith_then(With_thenContext ctx) {
    return null;
  }
  @Override
  public ASTNode visitId_list(Id_listContext ctx) {
    return null;
  }
  @Override
  public ASTNode visitGen_id(Gen_idContext ctx) {
    return null;
  }
  
  @Override
  public ASTNode visitModifiers(ModifiersContext ctx) {
    int N=ctx.getChildCount();
    BlockStatement res=create.block();
    for(int i=0;i<N;i++){
      String mod=getIdentifier(ctx,i);
      switch(mod){
      case "static":
        res.add(create.special(ASTSpecial.Kind.StaticEntry));
        continue;
      case "inline":
        res.add(create.special(ASTSpecial.Kind.InlineEntry));
        continue;
      default:
        throw new HREError("unmatched modifier %s",mod);
      }
    }
    return res;
  }
  
  @Override
  public ASTNode visitWait_list(Wait_listContext ctx) {
    return null;
  }
  @Override
  public ASTNode visitWait_for(Wait_forContext ctx) {
    if (match(ctx,null,"(",null,")")){
      String name=getIdentifier(ctx,0);
      ASTNode args[]=convert_list((ParserRuleContext)ctx.getChild(2), ",");
      return create.invokation(null, null, name, args);
    }
    return null;
  }

  @Override
  public ASTNode visitId_arg_list(Id_arg_listContext ctx) {
    return null;
  }
  
  @Override
  public ASTNode visitId_arg(Id_argContext ctx) {
    return null;
  }
  @Override
  public ASTNode visitIdentifier(IdentifierContext ctx) {
    
    return null;
  }
 
  @Override
  public ASTNode visitValReserved(ValReservedContext ctx) {
    
    return null;
  }
  @Override
  public ASTNode visitValContractClause(ValContractClauseContext ctx) {
    if (match(ctx,"accessible",null,";")){
      return create.special(ASTSpecial.Kind.Accessible,convert(ctx,1));
    }
    if (match(ctx,"modifies",null,";")){
      return create.special(ASTSpecial.Kind.Modifies,convert(ctx,1));
    }
    if (match(ctx,"requires",null,";")){
      return create.special(ASTSpecial.Kind.Requires,convert(ctx,1));
    }
    if (match(ctx,"ensures",null,";")){
      return create.special(ASTSpecial.Kind.Ensures,convert(ctx,1));
    }
    if (match(ctx,"given",null,null,";")){
      return create.special(ASTSpecial.Kind.Given,create.field_decl(getIdentifier(ctx, 2),checkType(convert(ctx,1))));
    }
    if (match(ctx,"yields",null,null,";")){
      return create.special(ASTSpecial.Kind.Yields,create.field_decl(getIdentifier(ctx, 2),checkType(convert(ctx,1))));
    }
    if (match(0,true,ctx,"Modifiers","resource")||match(0,true,ctx,"Modifiers","predicate")){
      String name=getIdentifier(ctx, 2);
      DeclarationStatement args[]=checkDecls(convert_list(ctx.getChild(3),"(",",",")"));
      ASTNode body;
      if (match(4,true,ctx,"=")){
        body=convert(ctx,5);
      } else {
        body=null;
      }
      ASTNode res=create.predicate(name, body, args);
      scan_modifiers(ctx,0, res);
      return res;
    }
    if (match(0,true,ctx,"Modifiers","Type")
      ||match(0,true,ctx,"Modifiers","boolean")){
      Type returns;
      if (match(1,true,ctx,"Type")){
        returns=checkType(convert(ctx,1));
      } else {
        returns=create.primitive_type(PrimitiveSort.Boolean);
      }
      String name=getIdentifier(ctx, 2);
      DeclarationStatement args[]=checkDecls(convert_list(ctx.getChild(3),"(",",",")"));
      ASTNode body;
      Method.Kind kind;
      if (match(4,true,ctx,"=")){
        body=convert(ctx,5);
        kind=Kind.Pure;
      } else {
        body=null;
        if (scan_pure(ctx,0)){
          kind=Kind.Pure;
        } else {
          kind=Kind.Plain;
        }
      }
      ASTNode res=create.method_kind(kind, returns, null, name, args, body);
      scan_modifiers(ctx,0, res);
      return res;
    }
    throw new HREError("Missing case: %s",ctx.toStringTree(parser));
  }

  @Override
  public ASTNode visitExpression(ExpressionContext ctx) {
    return convert(ctx, 0);
  }

  @Override
  public ASTNode visitValPrimary(ValPrimaryContext ctx) {
    return getValPrimary(ctx);
  }
}
