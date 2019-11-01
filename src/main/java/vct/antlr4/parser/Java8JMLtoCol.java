package vct.antlr4.parser;

import hre.lang.HREError;

import java.util.ArrayList;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicBoolean;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.antlr.v4.runtime.Parser;
import org.apache.commons.lang3.StringEscapeUtils;

import vct.col.ast.expr.Dereference;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.generic.ASTList;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.ASTSequence;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;
import vct.col.ast.util.ContractBuilder;
import vct.col.syntax.JavaDialect;
import vct.col.syntax.JavaSyntax;
import vct.col.syntax.Syntax;
import vct.antlr4.generated.Java8JMLParser.*;
import vct.antlr4.generated.*;
import vct.util.Configuration;

import static hre.lang.System.*;

/**
 * Convert JML parse trees to COL.
 *
 * @author <a href="mailto:s.c.c.blom@utwente.nl">Stefan Blom</a>
*/
public class Java8JMLtoCol extends ANTLRtoCOL implements Java8JMLVisitor<ASTNode> {

  private static <E extends ASTSequence<?>> E convert(E unit, ParseTree tree, String file_name, BufferedTokenStream tokens, Parser parser){
    ANTLRtoCOL visitor=new Java8JMLtoCol(unit,JavaSyntax.getJava(JavaDialect.JavaVerCors),file_name,tokens,parser);
    visitor.scan_to(unit,tree);
    return unit;
  }
  public static TempSequence convert_seq(ParseTree tree, String file_name,BufferedTokenStream tokens,Parser parser) {
    return convert(new TempSequence(),tree,file_name,tokens,parser);
  }

  public static ProgramUnit convert_tree(ParseTree tree, String file_name,BufferedTokenStream tokens,Parser parser) {
    return convert(new ProgramUnit(),tree,file_name,tokens,parser);
  }
  
  private final int IntegerLiteral;
  
  private final int StringLiteral;

  private Stack<ASTNode> primarystack=new Stack<ASTNode>();

 
  public Java8JMLtoCol(ASTSequence<?> unit,Syntax syntax, String filename, BufferedTokenStream tokens, Parser parser) {
    super(unit,true,syntax, filename, tokens, parser, Java8JMLLexer.Identifier,Java8JMLLexer.class);
    IntegerLiteral=getStaticInt(lexer_class,"IntegerLiteral");
    StringLiteral=getStaticInt(lexer_class,"StringLiteral");
  }

  private int get_dim_count(DimsContext dims) {
    int dimCount = 0;

    for(int offset = 0; offset < dims.getChildCount(); offset += 2) {
      if(!match(offset, true, dims, "[", "]")) {
        throw Failure("Annotated array dimensions are not yet supported by VerCors.");
      }

      dimCount++;
    }

    return dimCount;
  }

  private Type add_dims(Type type, int dims) {
    for(int i = 0; i < dims; i++) {
      type = create.primitive_type(PrimitiveSort.Option,
              create.primitive_type(PrimitiveSort.Array,
                      create.primitive_type(PrimitiveSort.Cell, type)));
    }

    return type;
  }

  private Type add_dims(Type type, DimsContext dims) {
    return add_dims(type, get_dim_count(dims));
  }

  private ASTNode convert_annotated(ParserRuleContext ctx) {
    ASTList list=new ASTList();
    int N=ctx.children.size()-1;
    for(int i=0;i<N;i++){
      list.add(convert(ctx,i));
      scan_comments_after(list, ctx.getChild(i));
    }
    ASTNode res=convert(ctx,N);
    res.attach();
    for (ASTNode n:list){
      Debug("adding %s annotation",n.getClass());
      res.attach(n);
    }
    return res;
  }

  private ASTNode doNew(int ofs,ParserRuleContext ctx){
    if (match(ofs,false,ctx,"new",null,"(",")")){
      MethodInvokation res=create.new_object(create.class_type(getIdentifier(ctx,ofs+1)));
      scan_comments_after(res.get_after(),ctx.getChild(ofs+3));
      return res;
    }
    if (match(ofs,false,ctx,"new",null,"(",null,")")){
      MethodInvokation res=create.new_object(create.class_type(getIdentifier(ctx,ofs+1)),
          convert_list((ParserRuleContext)ctx.getChild(ofs+3),","));
      scan_comments_after(res.get_after(),ctx.getChild(ofs+4));
      return res;
    }    
    return null;
  }

  private DeclarationStatement doParameter(ContractBuilder cb, ParseTree tree) {
    DeclarationStatement decl=null;
    enter(tree);
    Debug("converting type parameter %s",tree.toStringTree(parser));
    if (tree instanceof ParserRuleContext) {
      ParserRuleContext ctx=(ParserRuleContext)tree;
      if (instance(ctx,"TypeParameter")){
        decl=create.field_decl(getIdentifier(ctx,0),create.primitive_type(PrimitiveSort.Class));
        decl.setGhost(false);
      //} else if (match(ctx,"")){
        
      } else {
        Abort("missing case %s",ctx.toStringTree(parser));
      }
    } else {
      Abort("missing case");
    }
    leave(tree,null);
    return decl;
  }

  public ASTNode getArrayInitializer(VariableInitializerContext ctx, Type baseType, int dimension) {
    if(dimension <= 0) {
      throw new HREError("Array initializer has too many levels", ctx);
    }

    ArrayList<ASTNode> elements = new ArrayList<>();

    for(int offset = 0; offset < ctx.getChildCount(); offset++) {
      if(match(offset, true, ctx, ",")) {
        continue;
      } else if(match((VariableInitializerContext) ctx.getChild(offset), "Expression")) {
        elements.add(convert(ctx, offset));
      } else if(match((VariableInitializerContext) ctx.getChild(offset), "ArrayInitializer")) {
        elements.add(getArrayInitializer((ArrayInitializerContext) ctx.getChild(offset), baseType, dimension-1));
      } else {
        Abort("missing case");
      }
    }

    return create.expression(StandardOperator.OptionSome,
            create.struct_value(create.primitive_type(PrimitiveSort.Array,
                    create.primitive_type(PrimitiveSort.Cell,
                            add_dims(baseType, dimension-1)
                    )
            ), null, elements)
    );
  }

  public ASTNode getArrayInitializer(ArrayInitializerContext ctx, Type baseType, int dimension) {
    if(dimension <= 0) {
      throw new HREError("Array initializer has too many levels", ctx);
    }

    if(ctx.getChild(1) instanceof VariableInitializerContext) {
      return getArrayInitializer((VariableInitializerContext) ctx.getChild(1), baseType, dimension);
    } else {
      // Empty array
      return create.expression(StandardOperator.OptionSome,
              create.struct_value(create.primitive_type(PrimitiveSort.Array,
                      create.primitive_type(PrimitiveSort.Cell,
                              add_dims(baseType, dimension-1)
                      )
              ), null));
    }
  }

  private ASTNode getBasicFor(ParserRuleContext ctx){
    int ptr=2;
    ASTNode init;
    if (match(ptr,true,ctx,";")){
      ptr+=1;
      init=null;
    } else {
      init=convert(ctx,ptr);
      // It is probably a bug that the following line is needed.
      init=create.block(init.getOrigin(),init);
      ptr+=2;
    }
    ASTNode test;
    if (match(ptr,true,ctx,";")){
      ptr+=1;
      test=null;
    } else {
      test=convert(ctx,ptr);
      ptr+=2;
    }
    ASTNode update;
    if (match(ptr,true,ctx,")")){
      update=null;
      ptr+=1;
    } else {
      update=convert(ctx,ptr);
      ptr+=2;
    }    
    ASTList lst=new ASTList();
    scan_comments_before(lst,ctx.getChild(ptr));
    ASTNode body=convert(ctx,ptr);
    LoopStatement loop=create.for_loop(init, test, update, body);
    for(ASTNode n:lst) loop.get_after().add(n);
    return loop;
  }

  public BlockStatement getBlock(ParserRuleContext ctx) {
    BlockStatement res=create.block();
    if (match(ctx,"{",null,"}")){
      ParserRuleContext body_ctx=(ParserRuleContext)ctx.getChild(1);
      int N=body_ctx.getChildCount();
      for(int i=0;i<N;i++){
        scan_comments_before(res,body_ctx.getChild(i));
        if (match(i,true,body_ctx,";")) {
          continue;
        }
        res.add(convert(body_ctx.getChild(i)));
      }
      scan_comments_before(res,ctx.getChild(2));
    } else if (match (ctx,"{","}")) {
      scan_comments_before(res,ctx.getChild(1));
    } else {
      throw Failure("unexpected kind of block");
    }
    return res;
  }

  public ASTNode getClassBodyDeclaration(ParserRuleContext ctx) {
    if (match(ctx,"static","BlockContext")){
      ASTNode res=convert(ctx,1);
      res.setStatic(true);
      return res;
    }
    if (match(ctx,"BlockContext")){
      return convert(ctx,0);
    }
    return convert_annotated(ctx);
  }

  public ASTClass getClassDeclaration(ParserRuleContext ctx) {
    int base=0;
    int N=ctx.getChildCount();
    while(!match(base,true,ctx,"class")){
      base++;
    }
    String name=getIdentifier(ctx, base+1);
    int ptr=base+2;
    ClassType bases[]=null;
    while(ptr<N-1){
      if (match(ptr,true,ctx,"Superclass")){
        ParserRuleContext tmp=(ParserRuleContext)ctx.getChild(ptr);
        ASTNode t=convert(tmp,1);
        bases=new ClassType[]{forceClassType(t)};
        ptr++;
      } else {
        Debug("missing case ???");
        throw new Error("missing case");
      }
    }
    ASTClass cl=create.ast_class(name,ASTClass.ClassKind.Plain,null,bases,null);
    try {
      scan_body(cl, (ParserRuleContext)ctx.getChild(N-1));
    } catch (Throwable t) {
      DebugException(t);
      throw t;
    }
    for(int i=0;i<base;i++){
      cl.attach(convert(ctx,i));
    }
    return cl;
  }

  public ASTNode getClassOrInterfaceType(ParserRuleContext ctx) {
    if (match(ctx,null,".",null)){
      String name[]=new String[2];
      name[0]=getIdentifier(ctx,0);
      name[1]=getIdentifier(ctx,2);
      return create.class_type(name);
    }
    if (match(ctx,null,"TypeArgumentsContext")){
      String name=getIdentifier(ctx,0);
      ParserRuleContext arg_ctx=(ParserRuleContext)ctx.getChild(1);
      ASTNode args[]=convert_list(arg_ctx,1,arg_ctx.getChildCount()-1,",");
      return create.class_type(name, args);
    }
    ASTNode names[]=convert_list(ctx, ".");
    if (names!=null){
      String name[]=new String[names.length];
      for(int i=0;i<name.length;i++){
        name[i]=names[i].toString();
      }
      return create.class_type(name);
    }
    return null;
  }

  public ASTNode getCreatedName(ParserRuleContext ctx) {
    if (match(ctx,(String)null)){
      String name=getIdentifier(ctx,0);
      return create.class_type(name);
    } else if (match(ctx,(String)null,"TypeArgumentsOrDiamond")) {
      String name=getIdentifier(ctx,0);
      ASTNode args[]=convert_list((ParserRuleContext)(((ParserRuleContext)ctx.getChild(1)).getChild(0)), "<", ",", ">");
      return create.class_type(name, args);
    } else {
      throw MissingCase(ctx);
    }
  }

  public ASTNode getExpression(ParserRuleContext ctx) {
    if (match(ctx,null,":",null)){
      String label=getIdentifier(ctx,0);
      ASTNode res=convert(ctx,2);
      res.labeled(label);
      return res;
    }
    if (match(ctx,null,".",null)){
      return create.dereference(convert(ctx,0),getIdentifier(ctx,2));
    }
    if (match(ctx,null,"(",")")||match(ctx,null,"(",null,")")){
      ASTList before=new ASTList();
      scan_comments_before(before,ctx.getChild(1));
      ASTList after=new ASTList();
      scan_comments_after(after,ctx);
      ASTNode om=convert(ctx,0);
      ASTNode args[];
      if (match(ctx,null,"(",")")){
        args=new ASTNode[0];
      } else {
        args=convert_list((ParserRuleContext)ctx.getChild(2),",");
      }
      ASTNode object=null;
      String method;
      if (om instanceof NameExpression){
        object=null;
        method=((NameExpression)om).getName();
      } else if (om instanceof Dereference){
        object=((Dereference)om).obj();
        method=((Dereference)om).field();
      } else {
        throw hre.lang.System.Failure("could not convert %s to object/method at %s",om.getClass(),om.getOrigin());
      }
      MethodInvokation res= create.invokation(object, null, method, args);
      for(ASTNode n:before){
        res.get_before().add(n);
      }
      for(ASTNode n:after){
        res.get_after().add(n);
      }
      return res;
    }
    if (match(ctx,"new",null)){
      return convert(ctx,1); 
    }
    if (match(ctx,null,"?",null,":",null)){
      return create.expression(StandardOperator.ITE,convert(ctx,0),convert(ctx,2),convert(ctx,4));
    }
    if (match(ctx,null,"instanceof",null)){
      return create.expression(StandardOperator.Instance,convert(ctx,0),convert(ctx,2));
    }
    if (match(ctx,"(",null,")",null)) {
      return create.expression(StandardOperator.Cast,convert(ctx,1),convert(ctx,3));
    }
    return null;    
  }

  public DeclarationStatement getFormalParameter(ParserRuleContext ctx) {
    if (match(ctx,null,null)){
      VariableDeclaration decl=create.variable_decl(checkType(convert(ctx,0)));
      DeclarationStatement var=getVariableDeclaratorId((ParserRuleContext)ctx.getChild(1));
      decl.add(var);
      DeclarationStatement vars[]=decl.flatten();
      if (vars.length==1) return vars[0];
    }
    return null;
  }

  protected DeclarationStatement[] getFormalParameters(ParseTree tree, AtomicBoolean varargs) {
    DeclarationStatement args[];
    ParserRuleContext args_ctx=(ParserRuleContext)tree;
    ASTNode tmp[]=convert_smart_list(args_ctx,",");
    switch(tmp.length){
    case 0:
      break;
    case 1:
      varargs.set(!match((ParserRuleContext)args_ctx.getChild(0),"FormalParameter"));
      break;
    case 2:
      varargs.set(!match((ParserRuleContext)args_ctx.getChild(2),"FormalParameter"));
      break;
    }
    args=new DeclarationStatement[tmp.length];
    for(int i=0;i<tmp.length;i++){
      args[i]=(DeclarationStatement)tmp[i];
    }
    return args;
  }

  public ASTNode getImportDeclaration(ParserRuleContext ctx) {
    if (match(ctx,"import",null,";")){
      return create.special(Kind.Import,convert(ctx,1));
    }
    return null;
  }

  public DeclarationStatement getLastFormalParameter(ParserRuleContext ctx) {
    if (match(ctx,null,"...",null)){
      VariableDeclaration decl=create.variable_decl(checkType(convert(ctx,0)));
      DeclarationStatement var=getVariableDeclaratorId((ParserRuleContext)ctx.getChild(2));
      decl.add(var);
      DeclarationStatement vars[]=decl.flatten();
      if (vars.length==1) return vars[0];
    }
    return null;
  }

  public ASTNode getLiteral(ParserRuleContext ctx) {
    Token tok=((TerminalNode)ctx.getChild(0)).getSymbol();
    int t=tok.getType();
    if (t==IntegerLiteral){
      return create.constant(Integer.parseInt(tok.getText()));
    }
    if (t==StringLiteral){
      String text=tok.getText();
      return create.constant(StringEscapeUtils.unescapeJava(text.substring(1,text.length()-1)));
    }
    if (match(ctx,"true")) return create.constant(true);
    if (match(ctx,"false")) return create.constant(false);
    return null;
  }

  public Method getMethodDeclaration(ParserRuleContext ctx) {
    int N=ctx.getChildCount();
    // get the header
    Method header=(Method)convert(ctx,N-2);
    ASTNode body;
    // add the body if it has one
    if (match(N-1,true,ctx,";")){
      body=null; 
    } else {
      body=convert(ctx,N-1);
    }
    header.setBody(body);
    // add modifiers and annotations
    for(int i=0;i<N-2;i++){
      ASTNode mod=convert(ctx,i);
      header.attach(mod);
      scan_comments_after(header.annotations(),ctx.getChild(i));
    }
    return header;
  }

 
  public Method getMethodHeader(ParserRuleContext ctx) {
    int N=ctx.getChildCount();
    if (match(N-1,false,ctx,"Throws_")){
      Warning("exceptions are not supported yet.");
      N=N-1;
    }
    Type t;
    if (match(N-2,true,ctx,"void")){
      t=create.primitive_type(PrimitiveSort.Void);
    } else {
      t=checkType(convert(ctx,N-2));
    }
    ParserRuleContext temp_ctx=(ParserRuleContext)ctx.getChild(N-1);
    String name=getIdentifier(temp_ctx,0);
    AtomicBoolean varargs=new AtomicBoolean(false);
    DeclarationStatement args[];
    if (match(2,true,temp_ctx,"FormalParameterList")){
      args=getFormalParameters(temp_ctx.getChild(2),varargs);
    } else {
      args=new DeclarationStatement[0];
    }
    Method res=create.method_kind(Method.Kind.Plain,t,null, name, args, varargs.get(), null);
    for(int i=0;i<N-2;i++){
      res.attach(convert(ctx,i));
    }
    return res;    
  }

  private ASTNode getMethodInvocation(ParserRuleContext ctx) {
    if (match(0,true,ctx,"TypeName",".")||match(0,true,ctx,"Primary",".")){
      if (match(2,true,ctx,"super",".")||match(2,true,ctx,"TypeArguments")){
        throw new Error("missing case");
      }
      ASTNode object=convert(ctx,0);
      String method=getIdentifier(ctx,2);
      ASTNode args[];
      int close;
      if (match(4,true,ctx,"ArgumentList")){
        args=convert_list((ParserRuleContext)ctx.getChild(4),",");
        close=5;
      } else {
        args=new ASTNode[0];
        close=4;
      }
      MethodInvokation res=create.invokation(object,null, method, args);
      scan_comments_before(res.get_before(),ctx.getChild(3));
      scan_comments_after(res.get_after(),ctx.getChild(close));
      return res;
    }
    return getExpression(ctx);
  }
  
  public ASTNode getQualifiedName(ParserRuleContext ctx) {
    ASTNode n[]=convert_list(ctx,".");
    ASTNode res=n[0];
    for(int i=1;i<n.length;i++){
      if (!(n[i] instanceof NameExpression)) return null;
      String field=((NameExpression)n[i]).getName();
      res=create.dereference(res, field);
    }
    return res;
  }

  
  public ASTNode getStatement(ParserRuleContext ctx) {
    if (match(ctx,"if",null,null)){
      return create.ifthenelse(convert(ctx,1),convert(ctx,2));
    }
    if (match(ctx,"if",null,null,"else",null)){
      return create.ifthenelse(convert(ctx,1),convert(ctx,2),convert(ctx,4));
    }
    if (match(ctx,"return",";")){
      return create.return_statement();
    }
    if (match(ctx,"return",null,";")){
      ReturnStatement res=create.return_statement(convert(ctx,1));
      scan_comments_after(res.get_after(),ctx.getChild(1));
      return res;
    }
    if (match(ctx,"Expression",";")){
      return create.special(ASTSpecial.Kind.Expression,convert(ctx,0));
    }
    if (match(ctx,"StatementExpression",";")){
      return create.special(ASTSpecial.Kind.Expression,convert((ParserRuleContext)ctx.getChild(0),0)); 
    }
    if (match(ctx,"for","(",null,")",null)){
      ParserRuleContext control=(ParserRuleContext)ctx.getChild(2);
      if (match(control,null,";",null,";",null)){
        ASTNode init=convert(control,0);
        init=create.block(init.getOrigin(),init);
        ASTNode test=convert(control,2);
        ASTNode update=convert(control,4);
        ASTNode body=convert(ctx,4);
        LoopStatement res=create.for_loop(init, test, update, body);
        scan_comments_after(res.get_after(), ctx.getChild(3));
        return res;
      }
    }
    if (match(ctx,"assert",null,";")){
      return create.special(ASTSpecial.Kind.Assert,convert(ctx,1));
    }
    if (match(ctx,"throw",null,";")){
      return create.special(ASTSpecial.Kind.Throw,convert(ctx,1));
    }
    if (match(0,true,ctx,"try","Block")){
      BlockStatement main=(BlockStatement)convert(ctx,1);
      int N=ctx.getChildCount();
      BlockStatement after;
      if (match(N-1,true,ctx,"FinallyBlock")){
        after=(BlockStatement)convert((ParserRuleContext)ctx.getChild(N-1),1);
        N--;
      } else {
        after=null;
      }
      TryCatchBlock res=create.try_catch(main,after);
      for(int i=2;i<N;i++){
        ParserRuleContext clause=(ParserRuleContext)ctx.getChild(i);
        if (match(clause,"catch","(",null,null,")","Block")){
          Type type=checkType(convert(clause,2));
          String name=getIdentifier(clause, 3);
          BlockStatement block=(BlockStatement)convert(clause,5);
          DeclarationStatement decl=create.field_decl(name, type);
          res.addCatchClause(decl, block);
        } else {
          return null;
        }
      }
      return res;
    }
    if (match(ctx,null,":",null)){
      ASTNode res=convert(ctx,2);
      String label=getIdentifier(ctx,0);
      res.labeled(label);
      return res;
    }
    return null;
  }

  private void getTuple(ArrayList<Type> types, ParserRuleContext ctx) {
    ParseTree left=ctx.getChild(0);
    if (left instanceof ParserRuleContext && match((ParserRuleContext)left,null,",",null)){
      getTuple(types,(ParserRuleContext)left);
    } else {
      types.add(checkType(convert(ctx,0)));
    }
    types.add(checkType(convert(ctx,2)));
  }

  public ASTNode getType(ParserRuleContext ctx) {
    if (match(ctx,"seq","<",null,">")){
      return create.primitive_type(PrimitiveSort.Sequence, checkType(convert(ctx,0)));
    }
    if (match(ctx,"bag","<",null,">")){
      return create.primitive_type(PrimitiveSort.Bag, checkType(convert(ctx,0)));
    }
    if (match(ctx,"set","<",null,">")){
      return create.primitive_type(PrimitiveSort.Set, checkType(convert(ctx,0)));
    }
  
    if (match(ctx,null,"Dims")){
      return add_dims(checkType(convert(ctx, 0)), (DimsContext) ctx.getChild(1));
    }
    if (match(ctx,null,"->",null)){
      Type left=checkType(convert(ctx,0));
      if(left instanceof TupleType){
        return create.arrow_type(((TupleType)left).typesJava(),checkType(convert(ctx,2)));
      } else {
        return create.arrow_type(left,checkType(convert(ctx,2)));
      }
    }
    if (match(ctx,null,",",null)){
      ArrayList<Type> types=new ArrayList<Type>();
      getTuple(types,ctx);
      return create.tuple_type(types.toArray(new Type[0]));
    }
    return null;
  }

  private ASTNode getVariableDeclaration(ParserRuleContext ctx) {
    int base=0;
    while(!match(base,true,ctx,"UnannType")){
      base++;
    }
    Type t=checkType(convert(ctx,base));
    ASTNode vars[]=convert_list((ParserRuleContext)ctx.getChild(base+1),",");
    VariableDeclaration decl=create.variable_decl(t);
    for(int i=0;i<vars.length;i++){
      DeclarationStatement tmp;
      if (vars[i] instanceof NameExpression){
        String name=((NameExpression)vars[i]).getName();
        tmp=create.field_decl(name,create.class_type(name));
      } else if (vars[i] instanceof DeclarationStatement) {
        DeclarationStatement d=(DeclarationStatement)vars[i];
        tmp=create.field_decl(d.name(), d.getType(), d.initJava());
      } else {
        throw new HREError("unexpected %s in variable list at %s",vars[i].getClass(),create.getOrigin());
      }
      decl.add(tmp);
    }
    for(int i=0;i<base;i++){
      decl.attach(convert(ctx,i));
    }
    return decl;
  }

  public DeclarationStatement getVariableDeclarator(ParserRuleContext ctx) {
    if (match(ctx,null,"=",null)){
      DeclarationStatement decl = (DeclarationStatement)convert(ctx,0);
      ASTNode expr=convert(ctx,2);
      return create.field_decl(decl.name(), decl.getType(), expr);
    }
    return null;
  }

  public DeclarationStatement getVariableDeclaratorId(ParserRuleContext ctx) {
    String name=getIdentifier(ctx,0);
    Type t=create.class_type(name);
    if (match(ctx,null,"Dims")){
      t = add_dims(t, (DimsContext) ctx.getChild(1));
    }
    return create.field_decl(name, t);
  }

  public ASTClass oldGetClassDeclaration(ParserRuleContext ctx) {
    int N=ctx.getChildCount()-1;
    ClassType[]bases=null;
    ClassType[]supports=null;
    ContractBuilder cb=new ContractBuilder();
    DeclarationStatement parameters[]=null;
    //Warning("class decl %s",ctx.toStringTree(parser));
    for(int i=2;i<N;i++){
      //Warning("i==%d",i);
      if (match(i,true,ctx,"extends",null)){
        if (match(i+1,true,ctx,"TypeList")){
          bases=forceClassType(convert_list(((ParserRuleContext)ctx.getChild(i+1)), ","));
        } else {
          bases=new ClassType[]{forceClassType(convert(ctx,i+1))};
        }
        i+=1;
      } else if (match(i,true,ctx,"implements",null)){
        supports=forceClassType(convert_list(((ParserRuleContext)ctx.getChild(i+1)), ","));
        i+=1;
      } else if (match(i,true,ctx,"TypeParametersContext")){
        ParserRuleContext pars=(ParserRuleContext)ctx.getChild(i);
        int K=pars.getChildCount();
        parameters=new DeclarationStatement[K/2];
        for(int k=1;k<K;k+=2){
          parameters[k/2]=doParameter(cb,pars.getChild(k));
        }
      } else {
        return null;
      }
    }
    ASTClass.ClassKind kind;
    if (match(0,true,ctx,"class")){
      kind=ASTClass.ClassKind.Plain;
    } else if(match(0,true,ctx,"interface")){
      kind=ASTClass.ClassKind.Interface;
    } else {
      return null;
    }
    ASTClass cl=create.ast_class(getIdentifier(ctx,1), kind ,parameters, bases , supports );
    scan_body(cl,(ParserRuleContext)ctx.getChild(N));
    cl.setContract(cb.getContract());
    return cl;
  }

  public ASTNode PreviousvisitFunctionDeclaration(FunctionDeclarationContext ctx) {
    Contract contract=null;
    int i=0;
    if (match(0,true,ctx,"ContractContext")){
      contract=(Contract)convert(ctx,0);
      i=1;
    }
    int i0=i;
    while(match(i,true,ctx,"ModifierContext")){
      // skip now convert later.
      i++;
    }
    Type returns=checkType(convert(ctx,i));
    String name=getIdentifier(ctx,i+1);
    hre.lang.System.Debug("function %s, contract %s",name,contract);
    AtomicBoolean varargs=new AtomicBoolean();
    DeclarationStatement args[]=getFormalParameters(ctx.getChild(i+2),varargs);
    if (varargs.get()){
      hre.lang.System.Fail("functions with varargs not supported yet.");
    }
    ASTNode body=null;
    if (match(i+3,false,ctx,"=",null,";")){
      body=convert(ctx,i+4);
    }
    Method res=create.function_decl(returns, contract, name, args, body);
    hre.lang.System.Debug("function %s, contract %s", res.name(), res.getContract());
    while(i0<i){
      //add modifiers as annotations.
      ASTNode mod=convert(ctx,i0);
      res.attach(mod);
      i0++;
    }
    return res;
  }

  public void scan_body(ASTSequence<?> cl, ParserRuleContext ctx) {
    int N=ctx.getChildCount()-1;
    for(int i=1;i<N;i++){
      if (match(i,true,ctx,";")) {
        scan_comments_before(cl,ctx.getChild(i));
        continue;
      }
      ASTNode tmp=convert(ctx,i);
      scan_comments_before(cl,ctx.getChild(i));
      cl.add(tmp);
    }
    scan_comments_before(cl,ctx.getChild(N));
  }

  private String[] to_name(ASTNode pkg) {
    ArrayList<String> list=new ArrayList<String>();
    while(pkg instanceof Dereference){
      Dereference d=(Dereference)pkg;
      list.add(0,d.field());
      pkg=d.obj();
    }
    list.add(0,pkg.toString());
    return list.toArray(new String[0]);
  }

  @Override
  public ASTNode visitAdditionalBound(AdditionalBoundContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAdditiveExpression(AdditiveExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAmbiguousName(AmbiguousNameContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAndExpression(AndExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAnnotation(AnnotationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAnnotationTypeBody(AnnotationTypeBodyContext ctx) {
    
    return null;
  }
  
  @Override
  public ASTNode visitAnnotationTypeDeclaration(
      AnnotationTypeDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAnnotationTypeElementDeclaration(
      AnnotationTypeElementDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAnnotationTypeElementModifier(
      AnnotationTypeElementModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAnnotationTypeMemberDeclaration(
      AnnotationTypeMemberDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitArgumentList(ArgumentListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitArrayAccess(ArrayAccessContext ctx) {
    ASTNode result = convert(ctx, 0);

    for(int i = 1; i < ctx.getChildCount() - 2;) {
      if(match(i, true, ctx, "[", null, "]")) {
        result = create.expression(StandardOperator.Subscript, result, convert(ctx, i+1));
        i += 3;
      } else {
        i++;
      }
    }

    return result;
  }

  @Override
  public ASTNode visitArrayAccess_lf_primary(ArrayAccess_lf_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitArrayAccess_lfno_primary(
      ArrayAccess_lfno_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitArrayCreationExpression(ArrayCreationExpressionContext ctx) {
    // A new array statement may have some number of dimensions, some of which may have a defined length
    // If no dimensions have a defined length, a value may be explicitly specified.
    // format: new T definedDimensions undefinedDimensions?
    //       | new T undefinedDimensions initializer
    Type baseType = checkType(convert(ctx,1));

    if(ctx.getChild(2) instanceof DimExprsContext) {
      // We have defined dimensions, so no initializer
      ASTNode[] knownDims = convert_all((DimExprsContext) ctx.getChild(2));

      // If there are no undefined dimensions, we add no further dimensions to the base type
      // e.g. int[3][3] -> the type for which zeroes are generated is int (so 0)
      //      int[3][] -> the type for which zeroes are generated is int[] (so null)
      Type zeroType = ctx.getChildCount() == 3 ? baseType : add_dims(baseType, (DimsContext) ctx.getChild(3));

      return create.expression(StandardOperator.NewArray, zeroType, knownDims);
    } else {
      // We have no defined dimensions, so there must be an initializer

      return getArrayInitializer((ArrayInitializerContext) ctx.getChild(3),
              baseType,
              get_dim_count((DimsContext) ctx.getChild(2)));
    }
  }

  @Override
  public ASTNode visitArrayInitializer(ArrayInitializerContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitArrayType(ArrayTypeContext ctx) {
    return add_dims(checkType(convert(ctx,0)), (DimsContext) ctx.getChild(1));
  }

  @Override
  public ASTNode visitAssertStatement(AssertStatementContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitAssignment(AssignmentContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAssignmentExpression(AssignmentExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAssignmentOperator(AssignmentOperatorContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitAxiomDeclaration(AxiomDeclarationContext ctx) {
    if (match(ctx,"axiom",null,"{",null,"==",null,"}")){
      return create.axiom(getIdentifier(ctx,1),create.expression(StandardOperator.EQ,convert(ctx,3),convert(ctx,5)));
    }
    return null;
  }

  @Override
  public ASTNode visitBasicForStatement(BasicForStatementContext ctx) {
    return getBasicFor(ctx);
  }

  @Override
  public ASTNode visitBasicForStatementNoShortIf(
      BasicForStatementNoShortIfContext ctx) {
    return getBasicFor(ctx);
  }

  @Override
  public ASTNode visitBlock(BlockContext ctx) {
    return getBlock(ctx);
  }

  @Override
  public ASTNode visitBlockStatement(BlockStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitBlockStatements(BlockStatementsContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitBreakStatement(BreakStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitCastExpression(CastExpressionContext ctx) {
    ASTNode t=convert(ctx,1);
    ASTNode e=convert(ctx,3);
    return create.expression(StandardOperator.Cast, t ,e);
  }

  @Override
  public ASTNode visitCatchClause(CatchClauseContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitCatches(CatchesContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitCatchFormalParameter(CatchFormalParameterContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitCatchType(CatchTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitClassBody(ClassBodyContext ctx) {
    
    return null;
  }
  
  @Override
  public ASTNode visitClassBodyDeclaration(ClassBodyDeclarationContext ctx) {
    return getClassBodyDeclaration(ctx);
  }

  @Override
  public ASTNode visitClassDeclaration(ClassDeclarationContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitClassInstanceCreationExpression(
      ClassInstanceCreationExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitClassInstanceCreationExpression_lf_primary(
      ClassInstanceCreationExpression_lf_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitClassInstanceCreationExpression_lfno_primary(
      ClassInstanceCreationExpression_lfno_primaryContext ctx) {
    if (match(0,true,ctx,"new")) {
      return doNew(0,ctx);
    }
    return null;
  }

  @Override
  public ASTNode visitClassMemberDeclaration(ClassMemberDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitClassModifier(ClassModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitClassOrInterfaceType(ClassOrInterfaceTypeContext ctx) {
    return getClassOrInterfaceType(ctx);
  }

  @Override
  public ASTNode visitClassType(ClassTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitClassType_lf_classOrInterfaceType(
      ClassType_lf_classOrInterfaceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitClassType_lfno_classOrInterfaceType(
      ClassType_lfno_classOrInterfaceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitCompilationUnit(CompilationUnitContext ctx) {
    NameSpace ns;
    int ptr=0;
    if (match(0,true,ctx,"PackageDeclaration")) {
      hre.lang.System.Debug("has package");
      ASTNode pkg=convert((ParserRuleContext)ctx.getChild(0),1);
      Debug("pkg %s (%s)",Configuration.getDiagSyntax().print(pkg),pkg.getClass());
      ptr++;
      ns=create.namespace(to_name(pkg));
    } else {
      hre.lang.System.Debug("does not have package");
      ns=create.namespace(NameSpace.NONAME);
    }
    while(match(ptr,true,ctx,"ImportDeclaration")){
      ParserRuleContext imp=(ParserRuleContext)ctx.getChild(ptr);
      if (match(imp,"import",null,";")){
        ASTNode name=convert(imp,1);
        ns.add_import(false,false,to_name(name));
      } else if (match(imp,"import",null,".","*",";")){
        ASTNode name=convert(imp,1);
        ns.add_import(false,true,to_name(name));
      } else if (match(imp,"import","static",null,";")){
        ASTNode name=convert(imp,2);
        ns.add_import(true,false,to_name(name));
      } else if (match(imp,"import","static",null,".","*",";")){
        ASTNode name=convert(imp,2);
        ns.add_import(true,true,to_name(name));
      } else {
        hre.lang.System.Abort("unimplemented import type");
      }
      ptr++;
    }
    scan_to(ns, ctx, ptr, ctx.getChildCount());
    return ns;
  }

  @Override
  public ASTNode visitConditionalAndExpression(
      ConditionalAndExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitConditionalExpression(ConditionalExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitConditionalOrExpression(ConditionalOrExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitConstantDeclaration(ConstantDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitConstantExpression(ConstantExpressionContext ctx) {
    return getExpression(ctx);
  }

  @Override
  public ASTNode visitConstantModifier(ConstantModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitConstructorBody(ConstructorBodyContext ctx) {
    return getBlock(ctx);
  }

  @Override
  public ASTNode visitConstructorDeclaration(ConstructorDeclarationContext ctx) {
    int base=0;
    int N=ctx.getChildCount();
    while(!match(base,true,ctx,"ConstructorDeclarator")){
      base++;
    }
    ParserRuleContext cons_ctx=(ParserRuleContext)ctx.getChild(base);
    if (base!=N-2){
      throw new Error("no exceptions yet");
    }
    String name=getIdentifier(cons_ctx,0);
    DeclarationStatement[] args;
    if (match(cons_ctx,null,"(",")")){
      args=new DeclarationStatement[0];
    } else {
      args=getFormalParameters(cons_ctx.getChild(2),new AtomicBoolean());
    }
    ASTNode body=convert(ctx,N-1);
    Type returns=create.primitive_type(PrimitiveSort.Void);
    Method res=create.method_kind(Method.Kind.Constructor, returns ,null, name, args, body);
    for(int i=0;i<base;i++){
      res.attach(convert(ctx,i));
    }
    return res;
  }

  @Override
  public ASTNode visitConstructorDeclarator(ConstructorDeclaratorContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitConstructorModifier(ConstructorModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitContinueStatement(ContinueStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitDefaultValue(DefaultValueContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitDimExpr(DimExprContext ctx) {
    return convert(ctx.getChild(ctx.getChildCount() - 2));
  }

  @Override
  public ASTNode visitDimExprs(DimExprsContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitDims(DimsContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitDoStatement(DoStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitElementValue(ElementValueContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitElementValueArrayInitializer(
      ElementValueArrayInitializerContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitElementValueList(ElementValueListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitElementValuePair(ElementValuePairContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitElementValuePairList(ElementValuePairListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitEmptyStatement(EmptyStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitEnhancedForStatement(EnhancedForStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitEnhancedForStatementNoShortIf(
      EnhancedForStatementNoShortIfContext ctx) {
    
    return null;
  }

  
  @Override
  public ASTNode visitEnumBody(EnumBodyContext ctx) {
    
    return null;
  }
  
  @Override
  public ASTNode visitEnumBodyDeclarations(EnumBodyDeclarationsContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitEnumConstant(EnumConstantContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitEnumConstantList(EnumConstantListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitEnumConstantModifier(EnumConstantModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitEnumConstantName(EnumConstantNameContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitEnumDeclaration(EnumDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitEqualityExpression(EqualityExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExceptionType(ExceptionTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExceptionTypeList(ExceptionTypeListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExclusiveOrExpression(ExclusiveOrExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExplicitConstructorInvocation(
      ExplicitConstructorInvocationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExpression(ExpressionContext ctx) {
    return getExpression(ctx);
  }

  @Override
  public ASTNode visitExpressionList(ExpressionListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExpressionName(ExpressionNameContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExpressionStatement(ExpressionStatementContext ctx) {
    return convert(ctx,0);
  }

  @Override
  public ASTNode visitExtendsInterfaces(ExtendsInterfacesContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExtraAnnotation(ExtraAnnotationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExtraDeclaration(ExtraDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExtraIdentifier(ExtraIdentifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExtraPrimary(ExtraPrimaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExtraStatement(ExtraStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitExtraType(ExtraTypeContext ctx) {
    if (match(ctx,null,"TypeArgs")){
      String type=getIdentifier(ctx,0);
      ASTNode args[]=convert_list(ctx.getChild(1),"<",",",">");
      return create.class_type(type, args);
    }
    return null;
  }

  @Override
  public ASTNode visitFieldAccess(FieldAccessContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitFieldAccess_lf_primary(FieldAccess_lf_primaryContext ctx) {
    return create.dereference(primarystack.pop(),getIdentifier(ctx,1));
  }

  @Override
  public ASTNode visitFieldAccess_lfno_primary(
      FieldAccess_lfno_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitFieldDeclaration(FieldDeclarationContext ctx) {
    return getVariableDeclaration(ctx);
  }

  @Override
  public ASTNode visitFieldModifier(FieldModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitFinally_(Finally_Context ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitFloatingPointType(FloatingPointTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitForInit(ForInitContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitFormalParameter(FormalParameterContext ctx) {
    return getFormalParameter(ctx);
  }

  @Override
  public ASTNode visitFormalParameterList(FormalParameterListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitFormalParameters(FormalParametersContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitForStatement(ForStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitForStatementNoShortIf(ForStatementNoShortIfContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitForUpdate(ForUpdateContext ctx) {
    
    return null;
  }
  
  @Override
  public ASTNode visitFunctionDeclaration(FunctionDeclarationContext ctx) {
    int N=ctx.getChildCount();
    Method m=(Method)convert(ctx,N-4);
    m.setBody(convert(ctx,N-2));
    for(int i=0;i<N-4;i++){
      m.attach(convert(ctx,i));
    }
    m.attach(create.reserved_name(ASTReserved.Pure));
    return m;
  }

  @Override
  public ASTNode visitIdentifier(IdentifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitIfThenElseStatement(IfThenElseStatementContext ctx) {
    return create.ifthenelse(convert(ctx,2),convert(ctx,4),convert(ctx,6));
  }

  @Override
  public ASTNode visitIfThenElseStatementNoShortIf(
      IfThenElseStatementNoShortIfContext ctx) {
    return create.ifthenelse(convert(ctx,2),convert(ctx,4),convert(ctx,6));
  }

  @Override
  public ASTNode visitIfThenStatement(IfThenStatementContext ctx) {
    return create.ifthenelse(convert(ctx,2),convert(ctx,4));
  }

  @Override
  public ASTNode visitImportDeclaration(ImportDeclarationContext ctx) {
    return getImportDeclaration(ctx);
  }

  @Override
  public ASTNode visitInclusiveOrExpression(InclusiveOrExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitInferredFormalParameterList(
      InferredFormalParameterListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitInstanceInitializer(InstanceInitializerContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitIntegralType(IntegralTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitInterfaceBody(InterfaceBodyContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitInterfaceDeclaration(InterfaceDeclarationContext ctx) {
    return getClassDeclaration(ctx);
  }

  @Override
  public ASTNode visitInterfaceMemberDeclaration(
      InterfaceMemberDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitInterfaceMethodDeclaration(InterfaceMethodDeclarationContext ctx) {
    return getMethodDeclaration(ctx);
  }

  @Override
  public ASTNode visitInterfaceMethodModifier(InterfaceMethodModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitInterfaceModifier(InterfaceModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitInterfaceType(InterfaceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitInterfaceType_lf_classOrInterfaceType(
      InterfaceType_lf_classOrInterfaceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitInterfaceType_lfno_classOrInterfaceType(
      InterfaceType_lfno_classOrInterfaceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitInterfaceTypeList(InterfaceTypeListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitJavaIdentifier(JavaIdentifierContext ctx) {
    return create.unresolved_name(ctx.getChild(0).getText());
  }

  @Override
  public ASTNode visitLabeledExpression(LabeledExpressionContext ctx) {
    return getExpression(ctx);
  }

  @Override
  public ASTNode visitLabeledStatement(LabeledStatementContext ctx) {
    ASTNode S=convert(ctx,2);
    S.addLabel(create.label(getIdentifier(ctx, 0)));
    return S;
  }

  @Override
  public ASTNode visitLabeledStatementNoShortIf(
      LabeledStatementNoShortIfContext ctx) {
    ASTNode S=convert(ctx,2);
    S.addLabel(create.label(getIdentifier(ctx, 0)));
    return S;
  }

  
  
  @Override
  public ASTNode visitLambdaBody(LambdaBodyContext ctx) {
    
    return null;
  }
  
  @Override
  public ASTNode visitLambdaExpression(LambdaExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitLambdaParameters(LambdaParametersContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitLastFormalParameter(LastFormalParameterContext ctx) {
    return getLastFormalParameter(ctx);
  }

  @Override
  public ASTNode visitLeftHandSide(LeftHandSideContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitLiteral(LiteralContext ctx) {
    return getLiteral(ctx);
  }

  @Override
  public ASTNode visitLocalVariableDeclaration(LocalVariableDeclarationContext ctx) {
    return getVariableDeclaration(ctx);
  }

  @Override
  public ASTNode visitLocalVariableDeclarationStatement(
      LocalVariableDeclarationStatementContext ctx) {
    return convert(ctx,0);
  }

  @Override
  public ASTNode visitMarkerAnnotation(MarkerAnnotationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitMethodBody(MethodBodyContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitMethodDeclaration(MethodDeclarationContext ctx) {
    return getMethodDeclaration(ctx);
  }

  @Override
  public ASTNode visitMethodDeclarator(MethodDeclaratorContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitMethodHeader(MethodHeaderContext ctx) {
    return getMethodHeader(ctx);
  }

  @Override
  public ASTNode visitMethodInvocation(MethodInvocationContext ctx) {
    return getMethodInvocation(ctx);
  }

  @Override
  public ASTNode visitMethodInvocation_lf_primary(
      MethodInvocation_lf_primaryContext ctx) {
    int N=ctx.getChildCount();
    String method=getIdentifier(ctx,1);
    ASTNode args[];
    if (match(3,true,ctx,"ArgumentList")){
      args=convert_list((ParserRuleContext)ctx.getChild(3),",");
    } else {
      args=new ASTNode[0];
    }
    MethodInvokation res=create.invokation(primarystack.pop(), null, method, args);
    scan_comments_before(res.get_before(),ctx.getChild(2));
    scan_comments_after(res.get_after(),ctx.getChild(N-1));
    return res;
  }

  @Override
  public ASTNode visitMethodInvocation_lfno_primary(MethodInvocation_lfno_primaryContext ctx) {
    return getMethodInvocation(ctx);
  }

  @Override
  public ASTNode visitMethodModifier(MethodModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitMethodName(MethodNameContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitMethodReference(MethodReferenceContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitMethodReference_lf_primary(
      MethodReference_lf_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitMethodReference_lfno_primary(
      MethodReference_lfno_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitMultiplicativeExpression(
      MultiplicativeExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitNormalAnnotation(NormalAnnotationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitNormalClassDeclaration(NormalClassDeclarationContext ctx) {
    return getClassDeclaration(ctx);
  }

  @Override
  public ASTNode visitNormalInterfaceDeclaration(
      NormalInterfaceDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitNumericType(NumericTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPackageDeclaration(PackageDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPackageModifier(PackageModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPackageName(PackageNameContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPackageOrTypeName(PackageOrTypeNameContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPostDecrementExpression(PostDecrementExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPostDecrementExpression_lf_postfixExpression(
      PostDecrementExpression_lf_postfixExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPostfixExpression(PostfixExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPostIncrementExpression(PostIncrementExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPostIncrementExpression_lf_postfixExpression(
      PostIncrementExpression_lf_postfixExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPreDecrementExpression(PreDecrementExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPreIncrementExpression(PreIncrementExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPrimary(PrimaryContext ctx) {
    ASTNode res=convert(ctx,0);
    int N=ctx.getChildCount();
    for(int i=1;i<N;i++){
      int chk=primarystack.size();
      primarystack.push(res);
      res=convert(ctx,i);
      if (chk!=primarystack.size()){
        throw new HREError("stack problem %d expected %d found",chk,primarystack.size());
      }
    }
    return res;
  }

  @Override
  public ASTNode visitPrimaryNoNewArray(PrimaryNoNewArrayContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPrimaryNoNewArray_lf_arrayAccess(
      PrimaryNoNewArray_lf_arrayAccessContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPrimaryNoNewArray_lf_primary(
      PrimaryNoNewArray_lf_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPrimaryNoNewArray_lf_primary_lf_arrayAccess_lf_primary(
      PrimaryNoNewArray_lf_primary_lf_arrayAccess_lf_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPrimaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary(
      PrimaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPrimaryNoNewArray_lfno_arrayAccess(
      PrimaryNoNewArray_lfno_arrayAccessContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPrimaryNoNewArray_lfno_primary(
      PrimaryNoNewArray_lfno_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPrimaryNoNewArray_lfno_primary_lf_arrayAccess_lfno_primary(
      PrimaryNoNewArray_lfno_primary_lf_arrayAccess_lfno_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPrimaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary(
      PrimaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primaryContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitPrimitiveType(PrimitiveTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitReceiverParameter(ReceiverParameterContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitReferenceType(ReferenceTypeContext ctx) {
    return convert(ctx, 0);
  }

  @Override
  public ASTNode visitRelationalExpression(RelationalExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitResource(ResourceContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitResourceList(ResourceListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitResourceSpecification(ResourceSpecificationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitResult(ResultContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitReturnStatement(ReturnStatementContext ctx) {
    if (ctx.getChildCount()==3){
      return create.return_statement(convert(ctx,1));
    } else {
      return create.return_statement();
    }
  }

  @Override
  public ASTNode visitShiftExpression(ShiftExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSimpleTypeName(SimpleTypeNameContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSingleElementAnnotation(SingleElementAnnotationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSingleStaticImportDeclaration(
      SingleStaticImportDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSingleTypeImportDeclaration(
      SingleTypeImportDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSpecificationSequence(SpecificationSequenceContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitStatement(StatementContext ctx) {
    return getStatement(ctx);
  }

  @Override
  public ASTNode visitStatementExpression(StatementExpressionContext ctx) {
    return getExpression(ctx);
  }

  @Override
  public ASTNode visitStatementExpressionList(StatementExpressionListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitStatementNoShortIf(StatementNoShortIfContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitStatementWithoutTrailingSubstatement(
      StatementWithoutTrailingSubstatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitStaticImportOnDemandDeclaration(
      StaticImportOnDemandDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitStaticInitializer(StaticInitializerContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSuperclass(SuperclassContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSuperinterfaces(SuperinterfacesContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSwitchBlock(SwitchBlockContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSwitchBlockStatementGroup(
      SwitchBlockStatementGroupContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSwitchLabel(SwitchLabelContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSwitchLabels(SwitchLabelsContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSwitchStatement(SwitchStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitSynchronizedStatement(SynchronizedStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitThrows_(Throws_Context ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitThrowStatement(ThrowStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTryStatement(TryStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTryWithResourcesStatement(
      TryWithResourcesStatementContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitType(TypeContext ctx) {
    return getType(ctx);
  }

  @Override
  public ASTNode visitTypeArgs(TypeArgsContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeArgument(TypeArgumentContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeArgumentList(TypeArgumentListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeArguments(TypeArgumentsContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeArgumentsOrDiamond(TypeArgumentsOrDiamondContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeBound(TypeBoundContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeDeclaration(TypeDeclarationContext ctx) {
    return convert_annotated(ctx);
  }

  @Override
  public ASTNode visitTypeImportOnDemandDeclaration(
      TypeImportOnDemandDeclarationContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeName(TypeNameContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeParameter(TypeParameterContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeParameterList(TypeParameterListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeParameterModifier(TypeParameterModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeParameters(TypeParametersContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeVariable(TypeVariableContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnannArrayType(UnannArrayTypeContext ctx) {
    return add_dims(checkType(convert(ctx,0)), (DimsContext) ctx.getChild(1));
  }

  @Override
  public ASTNode visitUnannClassOrInterfaceType(
      UnannClassOrInterfaceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnannClassType(UnannClassTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnannClassType_lf_unannClassOrInterfaceType(
      UnannClassType_lf_unannClassOrInterfaceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnannClassType_lfno_unannClassOrInterfaceType(
      UnannClassType_lfno_unannClassOrInterfaceTypeContext ctx) {
    String name=getIdentifier(ctx,0);
    ASTNode args[];
    if (ctx.getChildCount()==1){
      args=new ASTNode[0];
    } else {
      args=convert_list((ParserRuleContext)ctx.getChild(1),"<",",",">");
    }
    return create.class_type(name, args);
  }

  @Override
  public ASTNode visitUnannInterfaceType(UnannInterfaceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnannInterfaceType_lf_unannClassOrInterfaceType(
      UnannInterfaceType_lf_unannClassOrInterfaceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnannInterfaceType_lfno_unannClassOrInterfaceType(
      UnannInterfaceType_lfno_unannClassOrInterfaceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnannPrimitiveType(UnannPrimitiveTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnannReferenceType(UnannReferenceTypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnannType(UnannTypeContext ctx) {
    return getType(ctx);
  }

  @Override
  public ASTNode visitUnannTypeVariable(UnannTypeVariableContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnaryExpression(UnaryExpressionContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitUnaryExpressionNotPlusMinus(
      UnaryExpressionNotPlusMinusContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitValContractClause(ValContractClauseContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitValPrimary(ValPrimaryContext ctx) {
    return getValPrimary(ctx);
  }

  @Override
  public ASTNode visitValReserved(ValReservedContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitValStatement(ValStatementContext ctx) {
    return getValStatement(ctx);
  }

  @Override
  public ASTNode visitVariableDeclarator(VariableDeclaratorContext ctx) {
    return getVariableDeclarator(ctx);
  }

  @Override
  public ASTNode visitVariableDeclaratorId(VariableDeclaratorIdContext ctx) {
    return getVariableDeclaratorId(ctx);
  }

  @Override
  public ASTNode visitVariableDeclaratorList(VariableDeclaratorListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitVariableInitializer(VariableInitializerContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitVariableInitializerList(VariableInitializerListContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitVariableModifier(VariableModifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitWhileStatement(WhileStatementContext ctx) {
    assert match(ctx,"while","(",null,")",null);
    LoopStatement res=create.while_loop(convert(ctx,2),convert(ctx,4));
    scan_comments_after(res.get_after(), ctx.getChild(3));
    return res;
  }

  @Override
  public ASTNode visitWhileStatementNoShortIf(WhileStatementNoShortIfContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitWildcard(WildcardContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitWildcardBounds(WildcardBoundsContext ctx) {
    
    return null;
  }

}
