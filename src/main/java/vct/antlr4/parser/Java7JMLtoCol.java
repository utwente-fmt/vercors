package vct.antlr4.parser;

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicBoolean;

import hre.lang.HREError;
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
import vct.col.ast.generic.ASTList;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.ASTSequence;
import vct.col.ast.generic.BeforeAfterAnnotations;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.composite.Switch.Case;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;
import vct.col.ast.util.ContractBuilder;
import vct.col.syntax.JavaDialect;
import vct.col.syntax.JavaSyntax;
import vct.col.syntax.Syntax;
import vct.antlr4.generated.Java7JMLParser.*;
import vct.antlr4.generated.*;
import vct.util.Configuration;
import viper.silver.cfg.Block;

import static hre.lang.System.*;
import static vct.col.ast.type.ASTReserved.Synchronized;

/**
 * Convert JML parse trees to COL.
 *
 * @author <a href="mailto:s.c.c.blom@utwente.nl">Stefan Blom</a>
*/
public class Java7JMLtoCol extends ANTLRtoCOL implements Java7JMLVisitor<ASTNode> {

  private static <E extends ASTSequence<?>> E convert(E unit, ParseTree tree, String file_name, BufferedTokenStream tokens, Parser parser){
    Java7JMLtoCol visitor=new Java7JMLtoCol(unit,JavaSyntax.getJava(JavaDialect.JavaVerCors),file_name,tokens,parser);
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

  private final int CharacterLiteral;

  private final int FloatingPointLiteral;

  public Java7JMLtoCol(ASTSequence<?> unit,Syntax syntax, String filename, BufferedTokenStream tokens, Parser parser) {
    super(unit,true,syntax, filename, tokens, parser, Java7JMLLexer.Identifier,Java7JMLLexer.class);
    IntegerLiteral=getStaticInt(lexer_class,"IntegerLiteral");
    StringLiteral=getStaticInt(lexer_class,"StringLiteral");
    CharacterLiteral=getStaticInt(lexer_class,"CharacterLiteral");
    FloatingPointLiteral=getStaticInt(lexer_class,"FloatingPointLiteral");
  }

  protected ASTNode convert_annotated(ParserRuleContext ctx) {
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

  public BlockStatement getBlock(ParserRuleContext ctx) {
    BlockStatement res=create.block();
    scan_body(res,ctx);
    return res;
  }

  public ASTNode getClassOrInterfaceBodyDeclaration(ParserRuleContext ctx) {
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

  public ASTClass getClassOrIntefaceDeclaration(ParserRuleContext ctx) {
    int N=ctx.getChildCount()-1;
    // https://docs.oracle.com/javase/specs/jls/se7/html/jls-4.html#jls-4.10 4.10.2
    ClassType[] bases = new ClassType[]{create.class_type(new String[]{"java", "lang", "Object"})};
    ClassType[]supports=null;
    ContractBuilder cb=new ContractBuilder();
    DeclarationStatement parameters[]=null;
    //Warning("class decl %s",ctx.toStringTree(parser));
    for(int i=2;i<N;i++){
      //Warning("i==%d",i);
      if (match(i,true,ctx,"extends",null)){
        if (match(i+1,true,ctx,"TypeList")){
          ASTNode[] convert = convert_list(((ParserRuleContext)ctx.getChild(i+1)), ",");
          ClassType[] res=new ClassType[convert.length];
          for(int i1=0;i1<convert.length;i1++){
            res[i1]=forceClassType(convert[i1]);
          }
          bases=res;
        } else {
          bases=new ClassType[]{forceClassType(convert(ctx,i+1))};
        }
        i+=1;
      } else if (match(i,true,ctx,"implements",null)){
        ASTNode[] convert = convert_list(((ParserRuleContext)ctx.getChild(i+1)), ",");
        ClassType[] res=new ClassType[convert.length];
        for(int i1=0;i1<convert.length;i1++){
          res[i1]=forceClassType(convert[i1]);
        }
        supports=res;
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
    if (match(ctx,null,"->",null,"Arguments")){
      ASTNode object=convert(ctx,0);
      String method=getIdentifier(ctx,2);
      ASTNode args[]=convert_list(ctx.getChild(3),"(",",",")");
      return create.expression(StandardOperator.Implies,
          create.expression(StandardOperator.NEQ,object,create.reserved_name(ASTReserved.Null)),
          create.invokation(object, null, method, args)
      );
    }
    boolean static_dispatch=false;
    if (match(ctx,null,"Arguments")||(static_dispatch=match(ctx,null,"@",null,"Arguments"))){
      ASTList before=new ASTList();
      if (static_dispatch){
        scan_comments_before(before,ctx.getChild(3));
      } else {
        scan_comments_before(before,ctx.getChild(1));
      }
      ASTList after=new ASTList();
      // scan with and then annotations:
      scan_comments_after(after,ctx);
      ASTNode om=convert(ctx,0);
      ASTNode args[]=convert_list(ctx.getChild(static_dispatch?3:1),"(",",",")");
      ASTNode object=null;
      String method;
      if (om instanceof NameExpression){
        object=null;
        NameExpression name=((NameExpression)om);
        if (name.getKind()==NameExpression.Kind.Reserved){
          method=syntax.getSyntax(name.reserved());
        } else {
          method=((NameExpression)om).getName();
        }
      } else if (om instanceof Dereference){
        object=((Dereference)om).obj();
        method=((Dereference)om).field();
      } else {
        throw hre.lang.System.Failure("could not convert %s to object/method at %s",om.getClass(),om.getOrigin());
      }
      ClassType dispatch=null;
      if (static_dispatch){
        dispatch=create.class_type(getIdentifier(ctx,2));
      }
      MethodInvokation res= create.invokation(object, dispatch, method, args);
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

  protected DeclarationStatement[] getFormalParameters(ParseTree tree, AtomicBoolean varargs) {
    DeclarationStatement args[];
    ParserRuleContext arg_ctx=(ParserRuleContext)tree;
    if (match(arg_ctx,"(",")")) {
      varargs.set(false);
      args=new DeclarationStatement[0];
    } else {
      ParserRuleContext args_ctx=(ParserRuleContext)arg_ctx.getChild(1);
      ASTNode tmp[]=convert_list(args_ctx,",");
      varargs.set(match(tmp.length*2-2,true,args_ctx,"LastFormalParameter"));
      args=new DeclarationStatement[tmp.length];
      for(int i=0;i<tmp.length;i++){
        args[i]=(DeclarationStatement)tmp[i];
      }
    }
    return args;
  }

  public Method getMethodDeclaration(ParserRuleContext ctx) {
    int N=ctx.getChildCount();
    Type t;
    if (match(0,true,ctx,"void")){
      t=create.primitive_type(PrimitiveSort.Void);
    } else {
      t=checkType(convert(ctx,0));
    }
    String name=getIdentifier(ctx,1);
    AtomicBoolean varargs=new AtomicBoolean();
    DeclarationStatement args[]=getFormalParameters(ctx.getChild(2),varargs);
    ASTNode body;
    if (match(N-1,true,ctx,";")){
      body=null;
    } else {
      body=convert(ctx,N-1);
    }
    Method res=create.method_kind(Method.Kind.Plain,t,null, name, args, varargs.get(), body);
    res.setStatic(false);
    return res;
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
    if (match(ctx,null,"Dims")){
      return add_dims(checkType(convert(ctx,0)), (DimsContext) ctx.getChild(1));
    }
    if (match(ctx,null,"->",null)){
      Type left=checkType(convert(ctx,0));
      if(left instanceof TupleType){
        return create.arrow_type(((TupleType)left).typesJava(), checkType(convert(ctx,2)));
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

  public ASTNode getVariableDeclaration(ParserRuleContext var_ctx, ASTNode ... list) {
    int N=list.length-1;
    ASTNode vars[]=convert_list(var_ctx,",");
    VariableDeclaration decl=create.variable_decl(checkType(list[N]));
    for(int i=0;i<vars.length;i++){
      DeclarationStatement tmp;
      if (vars[i] instanceof NameExpression){
        String name=((NameExpression)vars[i]).getName();
        tmp=create.field_decl(name,create.class_type(name));
      } else if (vars[i] instanceof DeclarationStatement) {
        DeclarationStatement d=(DeclarationStatement)vars[i];
        tmp = create.field_decl(d.name(), d.getType(), d.initJava());
      } else {
        throw hre.lang.System.Failure("unexpected %s in variable list at %s",vars[i].getClass(),create.getOrigin());
      }
      decl.add(tmp);
    }
    for(int i=0;i<N;i++){
      decl.attach(list[i]);
    }
    return decl;
  }

  public DeclarationStatement getVariableDeclaratorId(ParserRuleContext ctx) {
    String name=getIdentifier(ctx,0);
    Type t=create.class_type(name);
    if (match(ctx,null,"Dims")){
      t = add_dims(t, (DimsContext) ctx.getChild(1));
    }
    return create.field_decl(name, t);
  }

  public ASTNode getArrayInitializer(ArrayInitializerContext ctx, Type baseType, int dimension) {
    if(dimension <= 0) {
      throw new HREError("Array initializer has too many levels", ctx);
    }

    ArrayList<ASTNode> elements = new ArrayList<>();

    for(int offset = 1; offset < ctx.getChildCount() - 1; offset++) {
      if(match(offset, true, ctx, ",")) {
        continue;
      }

      VariableInitializerContext varCtx = (VariableInitializerContext) ctx.getChild(offset);

      if(match(varCtx, "ArrayInitializer")) {
        elements.add(getArrayInitializer((ArrayInitializerContext) varCtx.getChild(0), baseType, dimension - 1));
      } else if(match(varCtx, "Expression")) {
        elements.add(convert(varCtx, 0));
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
      pkg = d.obj();
    }
    list.add(0,pkg.toString());
    return list.toArray(new String[0]);
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
    int dimCount = 0;

    for(int offset = 0; offset < dims.getChildCount(); offset += 2) {
      if(!match(offset, true, dims, "[", "]")) {
        throw Failure("Unimplemented: has the Java7 grammar related to arrays changed?");
      }

      dimCount++;
    }

    return add_dims(type, dimCount);
  }

  @Override
  public ASTNode visitAnnotation(AnnotationContext ctx) {
    if (match(ctx,"@",null)){
      return convert(ctx, 1);
    }
    if (match(ctx,"@",null,"(",null,")")){
      return create.invokation(null, null,getIdentifier(ctx,1),convert(ctx,3));
    }
    return null;
  }

  @Override
  public ASTNode visitAnnotationConstantRest(AnnotationConstantRestContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitAnnotationMethodOrConstantRest(
      AnnotationMethodOrConstantRestContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitAnnotationMethodRest(AnnotationMethodRestContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitAnnotationName(AnnotationNameContext ctx) {

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
  public ASTNode visitAnnotationTypeElementRest(
      AnnotationTypeElementRestContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitArguments(ArgumentsContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitArrayCreatorRest(ArrayCreatorRestContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitArrayInitializer(ArrayInitializerContext ctx) {
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
  public ASTNode visitBlock(BlockContext ctx) {
    return getBlock(ctx);
  }

  @Override
  public ASTNode visitBlockStatement(BlockStatementContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitCatchClause(CatchClauseContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitCatchType(CatchTypeContext ctx) {
    if (ctx.children.size() > 1) {
      Abort("Catch type union not supported yet");
    }
    return null;
  }

  @Override
  public ASTNode visitClassBody(ClassBodyContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitClassBodyDeclaration(ClassBodyDeclarationContext ctx) {
    return getClassOrInterfaceBodyDeclaration(ctx);
  }

  @Override
  public ASTNode visitClassCreatorRest(ClassCreatorRestContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitClassDeclaration(ClassDeclarationContext ctx) {
    return getClassOrIntefaceDeclaration(ctx);
  }

  @Override
  public ASTNode visitClassOrInterfaceModifier(ClassOrInterfaceModifierContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitClassOrInterfaceType(ClassOrInterfaceTypeContext ctx) {
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

  @Override
  public ASTNode visitCompilationUnit(CompilationUnitContext ctx) {
    NameSpace ns;
    int ptr=0;
    if (match(0,true,ctx,"PackageDeclaration")) {
      Debug("has package");
      ASTNode pkg=convert((ParserRuleContext)ctx.getChild(0),1);
      Debug("pkg %s (%s)",Configuration.getDiagSyntax().print(pkg),pkg.getClass());
      ptr++;
      ns=create.namespace(to_name(pkg));
    } else {
      Debug("does not have package");
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
  public ASTNode visitConstantDeclarator(ConstantDeclaratorContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitConstantExpression(ConstantExpressionContext ctx) {
    return getExpression(ctx);
  }

  @Override
  public ASTNode visitConstDeclaration(ConstDeclarationContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitConstructorBody(ConstructorBodyContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitConstructorDeclaration(ConstructorDeclarationContext ctx) {
    String name=getIdentifier(ctx,0);
    AtomicBoolean varargs=new AtomicBoolean();
    DeclarationStatement args[]=getFormalParameters(ctx.getChild(1),varargs);
    Type returns=create.primitive_type(PrimitiveSort.Void);
    ASTNode body;
    if (ctx.getChildCount()==3){
      body=convert(ctx,2);
    } else {
      hre.lang.System.Warning("ignoring exceptiojns in contructor declaration");
      body=convert(ctx,4);
    }
    return create.method_kind(Method.Kind.Constructor, returns, null, name, args,varargs.get(),body);
  }

  @Override
  public ASTNode visitCreatedName(CreatedNameContext ctx) {
    if(match(ctx, "PrimitiveType")) {
      return convert(ctx, 0);
    } else if (match(ctx,(String)null)){
      String name=getIdentifier(ctx,0);
      return create.class_type(name);
    } else if (match(ctx,(String)null,"TypeArgumentsOrDiamond")) {
      String name = getIdentifier(ctx, 0);
      ASTNode args[] = convert_list((ParserRuleContext) (((ParserRuleContext) ctx.getChild(1)).getChild(0)), "<", ",", ">");
      return create.class_type(name, args);
    } else if (match(0, true, ctx, null, ".")) {
      ASTNode[] nodes = convert_list(ctx, ".");
      String[] names = new String[nodes.length];
      for (int i = 0; i < nodes.length; i++) {
        names[i] = ((NameExpression) nodes[i]).getName();
      }
      return create.class_type(names);
    } else {
      throw MissingCase(ctx);
    }
  }

  @Override
  public ASTNode visitCreator(CreatorContext ctx) {
    if (match(ctx,null,"ClassCreatorRestContext")){
      ParserRuleContext rest_ctx=(ParserRuleContext)ctx.getChild(1);
      Type type=checkType(convert(ctx,0));
      if (match(rest_ctx,"ArgumentsContext")){
        ASTNode args[]=convert_list(rest_ctx.getChild(0),"(",",",")");
        BeforeAfterAnnotations res=create.new_object((ClassType)type/*create.class_type(name)*/, args);
        scan_comments_after(res.get_before(),ctx.getChild(0));
        scan_comments_after(res.get_after(),ctx);
        return (ASTNode)res;
      }
      if (match(rest_ctx,"ArgumentsContext","ClassBody")){
        ASTNode args[]=convert_list(rest_ctx.getChild(0),"(",",",")");
        BeforeAfterAnnotations res=create.new_object((ClassType)type, args);
        scan_comments_after(res.get_before(),ctx.getChild(0));
        scan_comments_after(res.get_after(),ctx);

        ClassType bases[]=new ClassType[]{(ClassType)type};
        ASTClass cl=create.ast_class("__inline_ext", ClassKind.Plain ,null, bases , null );
        scan_body(cl,(ParserRuleContext)rest_ctx.getChild(1));
        Debug("cannot attach inline class %s", cl);
        return (ASTNode)res;
      }
      Debug("no arguments");
    }
    if (match(ctx,null,"ArrayCreatorRest")) {
      // A new array statement may have some number of dimensions, some of which may have a defined length
      // If no dimensions have a defined length, a value may be explicitly specified.
      Type baseType = checkType(convert(ctx,0));

      ArrayCreatorRestContext arrayCreator = (ArrayCreatorRestContext) ctx.getChild(1);
      int offset = 0;

      ArrayList<ASTNode> knownDims = new ArrayList<>();
      int dimCount = 0;

      while(offset < arrayCreator.getChildCount()) {
        if(match(offset, true, arrayCreator, "[", "]")) {
          dimCount++;
          offset += 2;
        } else if(match(offset, true, arrayCreator, "[", "Expression", "]")) {
          knownDims.add(convert(arrayCreator, offset+1));
          dimCount++;
          offset += 3;
        } else {
          break;
        }
      }

      if(offset >= arrayCreator.getChildCount() && !knownDims.isEmpty()) {
        // No initializer, so pass the intended type of the whole array
        // and the expressions for the known dimensions to generate the array.
        return create.expression(StandardOperator.NewArray, add_dims(baseType, dimCount), knownDims);
      } else if(offset == arrayCreator.getChildCount() - 1 &&
              match(offset, true, arrayCreator, "ArrayInitializer") &&
              knownDims.isEmpty()) {
        return getArrayInitializer((ArrayInitializerContext) arrayCreator.getChild(offset), baseType, dimCount);
      } else {
        // Children left and no valid initializer
        throw Failure("Invalid array creator format");
      }
    }
    Debug("no class creator");
    return null;
  }

  @Override
  public ASTNode visitDefaultValue(DefaultValueContext ctx) {

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
  public ASTNode visitElementValuePair(ElementValuePairContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitElementValuePairs(ElementValuePairsContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitEnhancedForControl(EnhancedForControlContext ctx) {

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
  public ASTNode visitEnumConstantName(EnumConstantNameContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitEnumConstants(EnumConstantsContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitEnumDeclaration(EnumDeclarationContext ctx) {
    String name=getIdentifier(ctx,1);
    ASTNode elements[]=null;
    if (match(ctx,"enum",null,"{",null,"}")){
      elements=convert_list((ParserRuleContext)ctx.getChild(3), ",");
    }
    ASTClass res=create.ast_class(name, ClassKind.Plain, null, null, null);
    Type t=create.primitive_type(PrimitiveSort.Sequence, create.class_type(name));
    ASTNode vals=create.struct_value(t, null, elements);
    res.add(create.field_decl(name,t,vals));
    return res;
  }

  @Override
  public ASTNode visitExplicitGenericInvocation(
      ExplicitGenericInvocationContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitExplicitGenericInvocationSuffix(
      ExplicitGenericInvocationSuffixContext ctx) {

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
    return getSpecificationPrimary(ctx);
  }

  @Override
  public ASTNode visitExtraStatement(ExtraStatementContext ctx) {
    ASTNode res=null;
    if (match(ctx,"loop_invariant",null,";")){
      res=create.special_decl(ASTSpecial.Kind.Invariant,convert(ctx,1));
    } else if (match(ctx,"set",null,"=",null,";")){
      res=create.assignment(convert(ctx,1),convert(ctx,3));
    } else if (match(ctx,"fold",null,";")){
      res=create.special(ASTSpecial.Kind.Fold,convert(ctx,1));
    } else if (match(ctx,"unfold",null,";")){
      res=create.special(ASTSpecial.Kind.Unfold,convert(ctx,1));
    } else if (match(ctx,"refute",null,";")){
      res=create.special(ASTSpecial.Kind.Refute,convert(ctx,1));
    } else if (match(ctx,"assert",null,";")){
      res=create.special(ASTSpecial.Kind.Assert,convert(ctx,1));
    } else if (match(ctx,"check",null,";")){
      res=create.special(ASTSpecial.Kind.Assert,convert(ctx,1));
    } else if (match(ctx,"spec_ignore","{")){
      res=create.special(ASTSpecial.Kind.SpecIgnoreStart);
    } else if (match(ctx,"}","spec_ignore")){
      res=create.special(ASTSpecial.Kind.SpecIgnoreEnd);
    } else if (match(ctx,"inhale",null,";")){
      res=create.special(ASTSpecial.Kind.Inhale,convert(ctx,1));
    } else if (match(ctx,"exhale",null,";")){
      res=create.special(ASTSpecial.Kind.Exhale,convert(ctx,1));
    } else if (match(ctx,"send",null,"to",null,",",null,";")){//DRB
      res=create.special(ASTSpecial.Kind.Send,convert(ctx,1),convert(ctx,3),convert(ctx,5));
      res.setGhost(true);
    } else if (match(ctx,"recv",null,"from",null,",",null,";")){//DRB
      res=create.special(ASTSpecial.Kind.Recv,convert(ctx,1),convert(ctx,3),convert(ctx,5));
      res.setGhost(true);
    } else if (match(ctx,"assume",null,";")){
      res=create.special(ASTSpecial.Kind.Assume,convert(ctx,1));
    }
    if (match(ctx,"create",null,";")){
      return create.special(ASTSpecial.Kind.CreateHistory,convert(ctx,1));
    }
    if (match(ctx,"create", null , "," , null , ";")){
      return create.special(ASTSpecial.Kind.CreateFuture,convert(ctx,1),convert(ctx,3));
    }
    if (match(ctx,"destroy",null,",",null,";")){
      return create.special(ASTSpecial.Kind.DestroyHistory,convert(ctx,1),convert(ctx,3));
    }
    if (match(ctx,"destroy",null,";")){
      return create.special(ASTSpecial.Kind.DestroyFuture,convert(ctx,1));
    }
    if (match(ctx,"split",null,",",null,",",null,",",null,",",null,";")){
      return create.special(ASTSpecial.Kind.SplitHistory,
          convert(ctx,1),convert(ctx,3),convert(ctx,5),convert(ctx,7),convert(ctx,9));
    }
    if (match(ctx,"merge",null,",",null,",",null,",",null,",",null,";")){
      return create.special(ASTSpecial.Kind.MergeHistory,
          convert(ctx,1),convert(ctx,3),convert(ctx,5),convert(ctx,7),convert(ctx,9));
    }
    if (match(ctx,"open",null,";")){
      return create.special(ASTSpecial.Kind.Open,convert(ctx,1));
    }
    if (match(ctx,"close",null,";")){
      return create.special(ASTSpecial.Kind.Close,convert(ctx,1));
    }
    if (match(ctx,"csl_subject",null,";")){
      return create.special(ASTSpecial.Kind.CSLSubject,convert(ctx,1));
    }
    if (match(ctx,"label",null)){
        return create.special(ASTSpecial.Kind.Label,convert(ctx,1));
      }
    if (match(ctx,"proof",null)){
      return create.special(ASTSpecial.Kind.Proof,convert(ctx,1));
    }
    if (match(ctx,"create","BlockContext")){
        BlockStatement block=getBlock((ParserRuleContext)ctx.getChild(1));
        return create.lemma(block);
      }
    if (match(ctx,"qed",null,";")){
        return create.special(ASTSpecial.Kind.QED,convert(ctx,1));
      }
    if (match(ctx,"apply",null,";")){
      return create.special(ASTSpecial.Kind.Apply,convert(ctx,1));
    }
    if (match(ctx,"use",null,";")){
      return create.special(ASTSpecial.Kind.Use,convert(ctx,1));
    }
    if (match(ctx,"witness",null,";")){
      res=create.special(ASTSpecial.Kind.Witness,convert(ctx,1));
    }
    if (match(ctx,"atomic","(",null,")",null)){
      ASTNode args[]=convert_list((ParserRuleContext)ctx.getChild(2),",");
      BlockStatement block=(BlockStatement)convert(ctx,4);
      res=create.csl_atomic(block,args);
    }
    if (res!=null){
      res.setGhost(true);
    }
    return res;
  }

  @Override
  public ASTNode visitExtraType(ExtraTypeContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitFieldDeclaration(FieldDeclarationContext ctx) {
    return getVariableDeclaration((ParserRuleContext)ctx.getChild(1),convert(ctx,0));
  }

  @Override
  public ASTNode visitFinallyBlock(FinallyBlockContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitForControl(ForControlContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitForInit(ForInitContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitFormalParameter(FormalParameterContext ctx) {
    if (match(ctx,null,null)){
      VariableDeclaration decl=create.variable_decl(checkType(convert(ctx,0)));
      DeclarationStatement var=getVariableDeclaratorId((ParserRuleContext)ctx.getChild(1));
      decl.add(var);
      DeclarationStatement vars[]=decl.flatten();
      if (vars.length==1) return vars[0];
    }
    if (match(ctx,"final",null,null)){
      VariableDeclaration decl=create.variable_decl(checkType(convert(ctx,1)));
      DeclarationStatement var=getVariableDeclaratorId((ParserRuleContext)ctx.getChild(2));
      decl.add(var);
      DeclarationStatement vars[]=decl.flatten();
      if (vars.length==1) return vars[0];
    }
    return null;
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
  public ASTNode visitForUpdate(ForUpdateContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitFunctionDeclaration(FunctionDeclarationContext ctx) {
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

  @Override
  public ASTNode visitGenericConstructorDeclaration(
      GenericConstructorDeclarationContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitGenericInterfaceMethodDeclaration(
      GenericInterfaceMethodDeclarationContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitGenericMethodDeclaration(
      GenericMethodDeclarationContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitIdentifier(IdentifierContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitImportDeclaration(ImportDeclarationContext ctx) {
    if (match(ctx,"import",null,";")){
      return create.special(Kind.Import,convert(ctx,1));
    }
    return null;
  }

  @Override
  public ASTNode visitInnerCreator(InnerCreatorContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitInterfaceBody(InterfaceBodyContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitInterfaceBodyDeclaration(
      InterfaceBodyDeclarationContext ctx) {
    return getClassOrInterfaceBodyDeclaration(ctx);
  }

  @Override
  public ASTNode visitInterfaceDeclaration(InterfaceDeclarationContext ctx) {
    return getClassOrIntefaceDeclaration(ctx);
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
  public ASTNode visitJavaDeclarations(JavaDeclarationsContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitJavaIdentifier(JavaIdentifierContext ctx) {
    String ident=getGeneralizedIdentifier(ctx, 0);
    if (syntax.is_reserved(ident)){
      return create.reserved_name(syntax.reserved(ident));
    } else {
      return create.unresolved_name(ident);
    }
  }

  @Override
  public ASTNode visitJavaStatements(JavaStatementsContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitLastFormalParameter(LastFormalParameterContext ctx) {
    if (match(ctx,null,"...",null)){
      VariableDeclaration decl=create.variable_decl(checkType(convert(ctx,0)));
      DeclarationStatement var=getVariableDeclaratorId((ParserRuleContext)ctx.getChild(2));
      decl.add(var);
      DeclarationStatement vars[]=decl.flatten();
      if (vars.length==1) return vars[0];
    }
    return null;
  }

  @Override
  public ASTNode visitLiteral(LiteralContext ctx) {
    Token tok=((TerminalNode)ctx.getChild(0)).getSymbol();
    int t=tok.getType();
    if (t==IntegerLiteral){
      String val=tok.getText();
      if (val.endsWith("L")){
        return create.constant(Long.parseLong(val.substring(0,val.length()-1)));

      } else {
        return create.constant(Integer.parseInt(tok.getText()));
      }
    }
    if (t==FloatingPointLiteral){
      return create.constant(Double.parseDouble(tok.getText()));
    }
    if (t==StringLiteral){
      String text=tok.getText();
      return create.constant(StringEscapeUtils.unescapeJava(text.substring(1,text.length()-1)));
    }
    if (t==CharacterLiteral){
      String text=tok.getText();
      String innerString = StringEscapeUtils.unescapeJava(text.substring(1,text.length()-1));
      if (innerString.length() != 1) {
        Abort("Character literal can only contain 1 character");
      }
      return create.constant(innerString.charAt(0));
    }
    if (match(ctx,"true")) return create.constant(true);
    if (match(ctx,"false")) return create.constant(false);
    return null;
  }

  @Override
  public ASTNode visitLocalVariableDeclaration(
      LocalVariableDeclarationContext ctx) {
    // Format is modifier* type declarators

    int N=ctx.getChildCount();

    // convert modifier* type
    ASTNode list[]=convert_range(ctx,0,N-1);

    // Pass declarators, and modifier* type as a list
    return getVariableDeclaration((ParserRuleContext)ctx.getChild(N-1),list);
  }

  @Override
  public ASTNode visitLocalVariableDeclarationStatement(
      LocalVariableDeclarationStatementContext ctx) {
    return convert(ctx,0);
  }

  @Override
  public ASTNode visitMemberDeclaration(MemberDeclarationContext ctx) {

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
  public ASTNode visitModifier(ModifierContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitNonWildcardTypeArguments(
      NonWildcardTypeArgumentsContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitNonWildcardTypeArgumentsOrDiamond(
      NonWildcardTypeArgumentsOrDiamondContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitPackageDeclaration(PackageDeclarationContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitParExpression(ParExpressionContext ctx) {
    return convert(ctx,1);
  }

  @Override
  public ASTNode visitPrimary(PrimaryContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitPrimitiveType(PrimitiveTypeContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitQualifiedName(QualifiedNameContext ctx) {
    // Imports are handled at the top level. So a qualified name is always a type since it is only used by throws and catch
    ASTNode[] nodes = convert_list(ctx, ".");
    String[] names = new String[nodes.length];
    for (int i = 0; i < nodes.length; i++) {
      names[i] = ((NameExpression) nodes[i]).getName();
    }
    return create.class_type(names);
  }

  @Override
  public ASTNode visitQualifiedNameList(QualifiedNameListContext ctx) {
    ASTNode types[] = convert_list(ctx, ",");
    // Would like to return a Type[] here, but since that's not currently possible we return a special.
    // Also, since this clause is only used by "throws", we use the kind Throw here.
    return create.special(Kind.Throw, types);
  }

  @Override
  public ASTNode visitResource(ResourceContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitResources(ResourcesContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitResourceSpecification(ResourceSpecificationContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitSpecificationSequence(SpecificationSequenceContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitStatement(StatementContext ctx) {
    if (match(ctx,"if",null,null)){
      return create.ifthenelse(convert(ctx,1),convert(ctx,2));
    }
    if (match(ctx,"if",null,null,"else",null)){
      return create.ifthenelse(convert(ctx,1),convert(ctx,2),convert(ctx,4));
    }
    if (match(ctx,";")){
      return create.comment(";");
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
    if (match(ctx,"while",null,null)){
      LoopStatement res=create.while_loop(convert(ctx,1),convert(ctx,2));
      scan_comments_after(res.get_after(), ctx.getChild(1));
      return res;
    }
    if (match(ctx,"for","(",null,")",null)){
      ParserRuleContext control=(ParserRuleContext)ctx.getChild(2);
      if (match(control,"EnhancedForControl")){
        control=(ParserRuleContext)control.getChild(0);
        if (match(control,null,null,":",null)){
          Type t=checkType(convert(control,0));
          String var=getIdentifier(control,1);
          ASTNode collection=convert(control,3);
          ASTNode body=convert(ctx,4);
          DeclarationStatement decls[]=new DeclarationStatement[]{create.field_decl(var, t,collection)};
          ForEachLoop res=create.foreach(decls, create.constant(true), body);
          scan_comments_after(res.get_after(), ctx.getChild(3));
          Debug("%s",res);
          return res;
        }
      } else {
        int ofs=0;
        ASTNode init;
        if (match(ofs,true,control,";")){
          init=null;
          ofs++;
        } else {
          init=convert(control,ofs);
          init=create.block(init.getOrigin(),init);
          ofs+=2;
        }
        ASTNode test;
        if (match(ofs,true,control,";")){
          test=null;
          ofs++;
        } else {
          test=convert(control,ofs);
          ofs+=2;
        }
        ASTNode update;
        if(ofs<control.getChildCount()){
          update=convert(control,ofs);
        } else {
          update=null;
        }
        ASTNode body=convert(ctx,4);
        LoopStatement res=create.for_loop(init, test, update, body);
        scan_comments_after(res.get_after(), ctx.getChild(3));
        return res;
      }
    }
    if (match(0,true, ctx,"switch",null,"{")){
      ASTNode expr=convert(ctx,1);
      ArrayList<Case> case_list=new ArrayList<Case>();
      Case c=new Case();
      int G=ctx.getChildCount()-1;
      for(int i=3;i<G;i++){
        if (match(i,true,ctx,"SwitchBlockStatementGroup")){
          ParserRuleContext group=(ParserRuleContext)ctx.getChild(i);
          int k=0;
          int N=group.getChildCount();
          while (k<N && match(k,true,group,"SwitchLabel")){
            if (match((ParserRuleContext)group.getChild(k),"default",":")){
              c.cases.add(null);
            } else {
              c.cases.add(convert((ParserRuleContext)group.getChild(k),1));
            }
            k++;
          }
          while(k < N){
            c.stats.add(convert(group,k));
            k++;
          }
          case_list.add(c);
          c=new Case();
        } else if (match(i,true,ctx,"SwitchLabel")){
          if (match((ParserRuleContext)ctx.getChild(i),"default",":")){
            c.cases.add(create.reserved_name(ASTReserved.Default));
          } else {
            c.cases.add(convert((ParserRuleContext)ctx.getChild(i),1));
          }
        }
      }
      if (c.cases.size()>0){
        case_list.add(c);
      }
      return create.switch_statement(expr,case_list);
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
    if (match(ctx, "synchronized", null, null)) {
      return create.synchronized_block(convert(ctx, 1), (BlockStatement) convert(ctx, 2));
    }
    return null;
  }

  @Override
  public ASTNode visitStatementExpression(StatementExpressionContext ctx) {
    return getExpression(ctx);
  }

  @Override
  public ASTNode visitSuperSuffix(SuperSuffixContext ctx) {

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
  public ASTNode visitType(TypeContext ctx) {
    return getType(ctx);
  }

  @Override
  public ASTNode visitDims(DimsContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitTypeArgument(TypeArgumentContext ctx) {
    if (match(ctx,"?","extends",null)){
      return create.type_expression(TypeOperator.Extends,checkType(convert(ctx,2)));
    }
    if (match(ctx,"?","super",null)){
      return create.type_expression(TypeOperator.Super,checkType(convert(ctx,2)));
    }
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
    int N=ctx.getChildCount();
    ASTNode res=convert(ctx.getChild(N-1));
    for(int i=0;i<N-1;i++){
      res.attach(convert(ctx,i));
    }
    return res;
  }

  @Override
  public ASTNode visitTypeList(TypeListContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitTypeParameter(TypeParameterContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitTypeParameters(TypeParametersContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitValContractClause(ValContractClauseContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitValInvocationAnnotation(ValInvocationAnnotationContext ctx) {
    if (match(0, true, ctx,"with") ||
            match(0, true, ctx,"then")){
      BlockStatement block = create.block();

      int offset = 2;
      while(!match(offset, true, ctx, "}")) {
        String givenName = getIdentifier(ctx, offset);
        ASTNode givenValue = convert(ctx, offset+2);
        block.add(create.assignment(create.unresolved_name(givenName), givenValue));
        offset += 4;
      }

      ASTSpecial.Kind kind = match(0, true, ctx, "with") ? Kind.With : Kind.Then;
      return create.special(kind, block);
    }

    return null;

//     : 'with' '{' (identifier '=' expression ';')* '}' ';'
//     | 'then' '{' (identifier '=' expression ';')* '}' ';'
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
    if (match(ctx,null,"=",null)){
      DeclarationStatement decl = (DeclarationStatement)convert(ctx,0);
      ASTNode expr=convert(ctx,2);
      return create.field_decl(decl.name(), decl.getType(), expr);
    }
    return null;
  }

  @Override
  public ASTNode visitVariableDeclaratorId(VariableDeclaratorIdContext ctx) {
    return getVariableDeclaratorId(ctx);
  }

  @Override
  public ASTNode visitVariableDeclarators(VariableDeclaratorsContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitVariableInitializer(VariableInitializerContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitVariableModifier(VariableModifierContext ctx) {

    return null;
  }

}
