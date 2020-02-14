package vct.antlr4.parser;

import hre.lang.Failure;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import vct.antlr4.generated.CMLLexer;
import vct.antlr4.generated.CMLParser.*;
import vct.antlr4.generated.CMLVisitor;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTList;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.ASTSequence;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.type.TypeOperator;
import vct.col.syntax.CSyntax;
import vct.col.syntax.Syntax;
import vct.col.util.SequenceUtils;
import vct.util.Configuration;

import java.util.ArrayList;

import static hre.lang.System.Debug;

/**
 * Convert CML (C Modeling Language) parse trees to COL.
 *
 * This class contains the conversions for parse tree nodes,
 * which are unique to CML or have to be handled differently
 * from the same node for C.
 * 
 * @author <a href="mailto:s.c.c.blom@utwente.nl">Stefan Blom</a>
*/
public class CMLtoCOL extends ANTLRtoCOL implements CMLVisitor<ASTNode> {

  private static <E extends ASTSequence<?>> E convert(E unit, ParseTree tree, String file_name, BufferedTokenStream tokens, org.antlr.v4.runtime.Parser parser) {
    ANTLRtoCOL visitor=new CMLtoCOL(unit, CSyntax.getCML(),file_name,tokens,parser,CMLLexer.Identifier,CMLLexer.CH_COMMENT);
    visitor.scan_to(unit,tree);
    return unit;
  }
  
  public static ProgramUnit convert_pu(ParseTree tree, String file_name, BufferedTokenStream tokens, org.antlr.v4.runtime.Parser parser) {
    return convert(new ProgramUnit(),tree,file_name,tokens,parser);
  }
  
  public static TempSequence convert_seq(ParseTree tree, String file_name,BufferedTokenStream tokens,org.antlr.v4.runtime.Parser parser) {
    return convert(new TempSequence(),tree,file_name,tokens,parser);
  }

  protected java.lang.Class<? extends ASTNode> expect;

  private int struct_no=0;

  public CMLtoCOL(ASTSequence<?> unit,Syntax syntax, String filename, BufferedTokenStream tokens,
      Parser parser, int identifier, int comment
  ) {
    super(unit,true, syntax, filename, tokens, parser, identifier, CMLLexer.class);
  }

  protected void convert_parameters(ArrayList<DeclarationStatement> args, ParserRuleContext arg_ctx) throws Failure {
    if (match(arg_ctx,null,",","...")){
      args.add(create.field_decl("va_args", create.primitive_type(PrimitiveSort.CVarArgs)));
    }
    arg_ctx=(ParserRuleContext)arg_ctx.getChild(0);
    while(match(arg_ctx,null,",",null)){
      args.add(0,(DeclarationStatement)convert(arg_ctx,2));
      arg_ctx=(ParserRuleContext)arg_ctx.getChild(0);
    }
    ASTNode n=convert(arg_ctx);
    if (n instanceof DeclarationStatement){
      args.add(0,(DeclarationStatement)n);
    } else {
      args.add(0,create.field_decl("__x_"+args.size(),(Type)n));
    }
      
  }

  private void doblock(BlockStatement block, ParserRuleContext ctx) {
    if (match(ctx,"BlockItemContext")){
        //hre.System.Warning("%s",ctx.getChild(0).toStringTree(parser));
        ASTNode temp = convert(ctx,0);
        scan_comments_before(block,ctx.getChild(0)); //DRB    
        block.addStatement(temp);
        scan_comments_after(block,ctx.getChild(0));//DRB 
    } else if (match(ctx,"BlockItemListContext","BlockItemContext")){                       
         doblock(block,(ParserRuleContext)ctx.getChild(0));
         //hre.System.Warning("%s",ctx.getChild(1).toStringTree(parser));
         ASTNode temp = convert(ctx,1);              
         block.addStatement(temp);
         scan_comments_after(block,ctx.getChild(1)); //DRB
    } else {      
      throw hre.lang.System.Failure("unknown BlockItemList");
    }
  }

  private ASTDeclaration getPointer(ASTDeclaration decl, ParserRuleContext ctx){
    if(match(ctx,"*",null)){
      Debug("pointer");
      decl = getPointer(decl,(ParserRuleContext)ctx.getChild(1));
    } else if(match(ctx, "**", null)) {
      // This must be a separate rule, as ** is tokenized separately for separating conjunction.
      Debug("pointer pointer");
      decl = getPointer(decl,(ParserRuleContext)ctx.getChild(1));
      decl = getPointer(decl,(ParserRuleContext)ctx.getChild(1));
    }

    if(decl instanceof DeclarationStatement) {
      DeclarationStatement result = new DeclarationStatement(
              decl.name(),
              create.primitive_type(PrimitiveSort.Pointer, ((DeclarationStatement) decl).type()),
              ((DeclarationStatement) decl).init()
      );

      result.setOrigin(decl.getOrigin());
      return result;
    } else if(decl instanceof Method) {
      Method result = new Method(
              ((Method) decl).getKind(),
              ((Method) decl).getName(),
              create.primitive_type(PrimitiveSort.Pointer, ((Method) decl).getReturnType()),
              ((Method) decl).getContract(),
              ((Method) decl).getArgs(),
              ((Method) decl).usesVarArgs(),
              ((Method) decl).getBody()
      );

      result.setOrigin(decl.getOrigin());
      return result;
    }

    return null;
  }

  @Override
  public ASTNode visitAbstractDeclarator(AbstractDeclaratorContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitAdditiveExpression(AdditiveExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitAlignmentSpecifier(AlignmentSpecifierContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitAndExpression(AndExpressionContext ctx) {
    return null;
  }
  
  @Override
  public ASTNode visitArgumentExpressionList(ArgumentExpressionListContext ctx) {
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
  public ASTNode visitAtomicTypeSpecifier(AtomicTypeSpecifierContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitBlock(BlockContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitBlockItem(BlockItemContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitBlockItemList(BlockItemListContext ctx) {
    return null;
  }
  
  @Override
  public ASTNode visitCastExpression(CastExpressionContext ctx) {
    if (match(ctx,"(",null,")",null)){
      Type t=checkType(convert(ctx,1));
      ASTNode e=convert(ctx,3);
      return create.expression(StandardOperator.Cast,t,e);
    }
    return null;
  }

  @Override
  public ASTNode visitClangIdentifier(ClangIdentifierContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitCompilationUnit(CompilationUnitContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitCompoundStatement(CompoundStatementContext ctx) {
    BlockStatement block=create.block();
    if (match(ctx,"{","}")) {
      scan_comments_after(block,ctx.getChild(0));
      return block;
    }
    if (!match(ctx,"{","BlockItemListContext","}")) return null;    
    doblock(block,(ParserRuleContext)ctx.getChild(1)); 
    return block;
  }

  @Override
  public ASTNode visitConditionalExpression(ConditionalExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitConstantExpression(ConstantExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitDeclaration(DeclarationContext ctx) {
    if (match(ctx,null,";")){
      ASTNode res;
      expect=DeclarationStatement.class;
      res=convert(ctx,0);
      expect=null;
      return res;
    } else if (match(ctx,null,null,";")){
      VariableDeclaration res=create.variable_decl(checkType(convert(ctx,0)));
      ParserRuleContext list=(ParserRuleContext)ctx.getChild(1);
      ASTNode decls[]=convert_linked_list(list,",");
      for(int i=0;i<decls.length;i++){
        if (decls[i] instanceof ASTDeclaration){
          res.add((ASTDeclaration)decls[i]);
        } else if (decls[i] instanceof OperatorExpression){
          OperatorExpression e=(OperatorExpression)decls[i];
          DeclarationStatement d=(DeclarationStatement)e.arg(0);
          res.add(create.field_decl(d.name(),d.getType(),e.arg(1)));
        } else {
          return null;
        }
      }
      return res;
    }
    return null;
  }

  @Override
  public ASTNode visitDeclarationList(DeclarationListContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitDeclarationSpecifier(DeclarationSpecifierContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitDeclarationSpecifiers(DeclarationSpecifiersContext ctx) {
    Debug("\"decl specs\" %s",ctx.toStringTree(parser));
    int i=ctx.getChildCount()-1;
    ParserRuleContext tmp=(ParserRuleContext)((ParserRuleContext)ctx.getChild(i)).getChild(0);
    Debug("\"last:\" %s",tmp.toStringTree(parser));
    String name=null;
    if (expect!=null && expect==DeclarationStatement.class){
      if (match(tmp,"TypedefName")){
        name=getIdentifier(tmp, 0);
        Debug("\"name:\" %s",name);
        i=i-1;
        tmp=(ParserRuleContext)((ParserRuleContext)ctx.getChild(i)).getChild(0);
      } else {
        return null;
      }
    }
    if (match(tmp,"TypedefName")){
      tmp=(ParserRuleContext)((ParserRuleContext)tmp).getChild(0);
    } 
    Debug("\"type:\" %s",tmp.toStringTree(parser));
    expect=Type.class;
    ASTNode t=convert(tmp);
    Type type=null;
    if (t instanceof Type){
      type=(Type)t;
    } else if (t instanceof NameExpression){
      type=create.class_type(((NameExpression)t).getName());
    } else {
      hre.lang.System.Abort("cannot convert %s/%s to type",tmp.toStringTree(parser),t.getClass());
    }
    i=i-1;
    while(i>=0){
      if (i==0 && match((ParserRuleContext)ctx.getChild(0),"StorageClassSpecifier")){
          Debug("\"class:\" %s",ctx.getChild(0).toStringTree(parser));
          String sclass=((ParserRuleContext)((ParserRuleContext)ctx.getChild(0))).getText();
          Debug("\"class:\" %s",sclass);
          switch(sclass){
          case "typedef":
            return create.field_decl(name,create.primitive_type(PrimitiveSort.Class) ,type);
          case "extern":
            type=create.__extern(type);
          case "static":
            type=create.__static(type);
            break;
          default:
            hre.lang.System.Abort("missing case");
          }
      } else if (match((ParserRuleContext)ctx.getChild(i),"TypeQualifier")){
        Debug("\"tspec:\" %s",ctx.getChild(i).toStringTree(parser));
        String modifier=((ParserRuleContext)((ParserRuleContext)ctx.getChild(i))).getText();
        switch(modifier){
        case "const":
          type=create.__const(type);
          break;
        case "short":
          type=create.__short(type);
          break;
        case "signed":
          type=create.__signed(type);
          break;
        case "unsigned":
          type=create.__unsigned(type);
          break;
        case "long":
          type=create.__long(type);
          break;
        default:
          hre.lang.System.Abort("unknown type modifier: %s",modifier);
        }
      } else  if (match((ParserRuleContext)ctx.getChild(i),"TypeSpecifier")){
        Debug("\"tspec:\" %s",ctx.getChild(i).toStringTree(parser));
        String modifier=((ParserRuleContext)((ParserRuleContext)ctx.getChild(i))).getText();
        switch(modifier){
        case "const":
          type=create.__const(type);
          break;
        case "short":
          type=create.__short(type);
          break;
        case "signed":
          type=create.__signed(type);
          break;
        case "unsigned":
          type=create.__unsigned(type);
          break;
        case "long":
          type=create.__long(type);
          break;
        case "__kernel":
          type=create.__kernel(type);
          break;
        case "__global":
          type=create.__global(type);
          break;
        case "__local":
          type=create.__local(type);
          break;
        default:
          hre.lang.System.Abort("unknown type modifier: %s",modifier);
        }
      } else {
        ASTNode n=convert(ctx,i);
        if (n instanceof NameExpression){
          NameExpression l=(NameExpression)n;
          if (l.getKind()==NameExpression.Kind.Unresolved){
            l.setKind(NameExpression.Kind.Label);
          }
          type.addLabel(l);
        } else {
          hre.lang.System.Abort("cannot handle modifier %s at %s",ctx.getChild(i).toStringTree(parser),n.getOrigin());
        }
      }
      i=i-1;
    }
    if (name==null){
      return type;
    } else {
      return create.field_decl(name,type);
    }
  }

  @Override
  public ASTNode visitDeclarationSpecifiers2(DeclarationSpecifiers2Context ctx) {
    return null;
  }

  @Override
  public ASTNode visitDeclarator(DeclaratorContext ctx) {
    if (match(ctx,"Pointer",null)){
      ASTDeclaration innerDeclarator = (ASTDeclaration) convert(ctx,1);
      return getPointer(innerDeclarator, (ParserRuleContext)ctx.getChild(0));
    } else if(match(ctx, "DirectDeclarator")) {
      return convert(ctx, 0);
    }
    return null;
  }

  @Override
  public ASTNode visitDesignation(DesignationContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitDesignator(DesignatorContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitDesignatorList(DesignatorListContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitDirectAbstractDeclarator(
      DirectAbstractDeclaratorContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitDirectDeclarator(DirectDeclaratorContext ctx) {
    if (match(ctx,(String)null)){
      String name=getIdentifier(ctx,0);
      return create.field_decl(name, VariableDeclaration.common_type);
    }
    boolean pars=false;
    if (match(ctx,null,"(",")")||(pars=match(ctx,null,"(","ParameterTypeList",")"))){
      String name=getIdentifier(ctx, 0);
      ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
      if (pars){  
        convert_parameters(args,(ParserRuleContext)ctx.getChild(2));
      }
      boolean varargs=false;
      if (args.size()>0){
        Type t=args.get(args.size()-1).getType();
        varargs=t.isPrimitive(PrimitiveSort.CVarArgs);
      }
      return create.method_kind(Method.Kind.Plain, VariableDeclaration.common_type, null, name, args, varargs, null);
    }
    if (match(ctx,null,"[","]")){
      Debug("unspecified array dimension");
      DeclarationStatement d=(DeclarationStatement)convert(ctx,0);
      // Multi-dimensional arrays should not be Option inbetween dimensions, so if the subtype is an array dimension, we
      // strip the option type from the subtype.
      Type t=d.getType();
      SequenceUtils.SequenceInfo info = SequenceUtils.getTypeInfo(t);
      if(info != null && info.isOpt() && info.isCell() && info.getSequenceSort() == PrimitiveSort.Array) {
        t = SequenceUtils.optArrayCell(create, info.getSequenceType());
      } else {
        t = SequenceUtils.optArrayCell(create, t);
      }
      return create.field_decl(d.name(), t);
    }
    int N=ctx.getChildCount();
    if (match(0,true,ctx,null,"[") && match(N-1,false,ctx,"]")){
      Debug("specified array dimension %s", convert(ctx, 2));
      DeclarationStatement d=(DeclarationStatement)convert(ctx,0);
      Type t=d.getType();
      if (N>4) {
        hre.lang.System.Warning("ignoring %d modifiers in declaration",N-4);
      }
      SequenceUtils.SequenceInfo info = SequenceUtils.getTypeInfo(t);
      if(info != null && info.isOpt() && info.isCell() && info.getSequenceSort() == PrimitiveSort.Array) {
        t = SequenceUtils.optArrayCell(create, info.getSequenceType());
      } else {
        t = SequenceUtils.optArrayCell(create, t);
      }
      return create.field_decl(d.name(), t);
    }
    return null;
  }

  @Override
  public ASTNode visitEnumerationConstant(EnumerationConstantContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitEnumerator(EnumeratorContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitEnumeratorList(EnumeratorListContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitEnumSpecifier(EnumSpecifierContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitEqualityExpression(EqualityExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitExclusiveOrExpression(ExclusiveOrExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitExpression(ExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitExpressionStatement(ExpressionStatementContext ctx) {
    if (match(ctx,"Expression",";")){
      return convert(ctx,0);
    } 
    if (match(ctx,";")){
      return create.block();
    }
    return null;
  }

  @Override
  public ASTNode visitExternalDeclaration(ExternalDeclarationContext ctx) {
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
    if (match(ctx,"spec_ignore","{")) {
      return create.special(Kind.SpecIgnoreStart);
    }
    if (match(ctx,"spec_ignore","}")) {
      return create.special(Kind.SpecIgnoreEnd);
    }
    return null;
  }

  @Override
  public ASTNode visitExtraType(ExtraTypeContext ctx) {
    return getValType(ctx);
  }

  @Override
  public ASTNode visitFunctionDefinition(FunctionDefinitionContext ctx) {
    if(!match(ctx, "DeclarationSpecifiers", "Declarator", "CompoundStatement")) {
      throw new Failure("Declaring parameters types after the method declaration is unsupported.");
    }

    Type declSpec = (Type)convert(ctx,0);
    Method declaration = (Method) convert(ctx, 1);
    declaration.setBody(convert(ctx, 2));

    VariableDeclaration result = create.variable_decl(declSpec);
    result.add(declaration);
    return result;
  }

  @Override
  public ASTNode visitFunctionSpecifier(FunctionSpecifierContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitGccAttribute(GccAttributeContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitGccAttributeList(GccAttributeListContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitGccAttributeSpecifier(GccAttributeSpecifierContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitGccDeclaratorExtension(GccDeclaratorExtensionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitGenericAssociation(GenericAssociationContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitGenericAssocList(GenericAssocListContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitGenericSelection(GenericSelectionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitIdentifier(IdentifierContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitIdentifierList(IdentifierListContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitInclusiveOrExpression(InclusiveOrExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitInitDeclarator(InitDeclaratorContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitInitDeclaratorList(InitDeclaratorListContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitInitializer(InitializerContext ctx) {
    if (match(ctx,"{","InitializerList","}")){
      ASTNode values[]=convert_linked_list((ParserRuleContext)ctx.getChild(1),",");
      Type t=SequenceUtils.arrayCell(create, create.primitive_type(PrimitiveSort.Integer));
      return create.struct_value(t,null,values);
    }
    return null;
  }

  @Override
  public ASTNode visitInitializerList(InitializerListContext ctx) {
    return null;
  }
  
  @Override
  public ASTNode visitIterationStatement(IterationStatementContext ctx) {
    if (match(ctx,"while","(",null,")",null)){
      LoopStatement res=(LoopStatement)create.while_loop(convert(ctx,2),convert(ctx,4));
      // scan for comments between loop header and loop body.
      scan_comments_after(res.get_after(), ctx.getChild(3));        
      return res;
    } else if (match(0,true,ctx,"for","(")){
      int ofs=1;
      ASTNode init;
      ASTNode test=null;
      if (match(ofs,true,ctx,"(",";")){
        ofs++;
        init=null;
      } else if (match(ofs,true,ctx,"(",null,";")) {
        init=convert(ctx,ofs+1);
        ofs+=2;
      } else if (match(ofs,true,ctx,"(",null,null,";")){
        init=convert(ctx,ofs+1);
        init=((VariableDeclaration)init).flatten()[0];
        test=convert(ctx,ofs+2);
        ofs+=3;
      } else {
        return null;
      }
      if (match(ofs,true,ctx,";",";")){
        ofs++;
        test=create.constant(true);
      } else if (match(ofs,true,ctx,";",null,";")) {
        test=convert(ctx,ofs+1);
        ofs+=2;
      }
      ASTNode update;
      if (match(ofs,true,ctx,";",")")){
        ofs++;
        update=create.constant(true);
      } else if (match(ofs,true,ctx,";",null,")")) {
        update=convert(ctx,ofs+1);
        ofs+=2;
      } else {
        return null;
      }
      ASTNode body;
      if (match(ofs,false,ctx,")",null)){
        body=convert(ctx,ofs+1);
      } else {
        return null;
      }
      LoopStatement res=create.for_loop(init,test,update,body);
      // scan for comments in between loop header and loop body.
      // Those probably should go in before() rather than in after.
      scan_comments_after(res.get_after(), ctx.getChild(ofs));
      return res;
    } else {
      return null;
    }
  }

  @Override
  public ASTNode visitJumpStatement(JumpStatementContext ctx) {
    if (match(ctx,"return",null,";")){
      return create.return_statement(convert(ctx,1));
    } else if (match(ctx,"return",";")){
      return create.return_statement();
    }
    return null;
  }

  @Override
  public ASTNode visitLabeledStatement(LabeledStatementContext ctx) {
    if (match(ctx, null, ":", null)) {
      ASTNode res = convert(ctx, 2);
      res.addLabel(create.label(ctx.getChild(0).getText()));
      return res;
    }
    return null;
  }

  @Override
  public ASTNode visitLogicalAndExpression(LogicalAndExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitLogicalOrExpression(LogicalOrExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitMultiplicativeExpression(
      MultiplicativeExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitNestedParenthesesBlock(NestedParenthesesBlockContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitParameterDeclaration(ParameterDeclarationContext ctx) {
    if (match(ctx,null,null)){
      Type t=(Type)convert(ctx,0);
      ParseTree var=ctx.getChild(1);
      if (var instanceof ParserRuleContext){
        ASTNode v=convert(ctx,1);
        if (v instanceof DeclarationStatement){
          VariableDeclaration decl=create.variable_decl(t);
          decl.add((DeclarationStatement)v);
          ASTDeclaration vars[]=decl.flatten();
          if (vars.length==1) return vars[0];
        } else if (v instanceof NameExpression){
          String name=((NameExpression)v).getName();
          return create.field_decl(name,t);
        }
      } else {
        String name=getIdentifier(ctx,1);
        return create.field_decl(name,t);
      }
    }
    return null;
  }

  @Override
  public ASTNode visitParameterList(ParameterListContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitParameterTypeList(ParameterTypeListContext ctx) {
    return null;
  }
  
  @Override
  public ASTNode visitPointer(PointerContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitPostfixExpression(PostfixExpressionContext ctx) {
    return visitPrimaryExpression((ParserRuleContext)ctx);
  }

  public ASTNode visitPrimaryExpression(ParserRuleContext ctx) {
    ASTNode res=visit(ctx);
    if (res!=null) return res;
    if (match(ctx,null,"(",null,")")){
      String name=getIdentifier(ctx,0);
      ASTNode args[]=convert_linked_list((ParserRuleContext)ctx.getChild(2),",");
      ASTList after = new ASTList();
      scan_comments_after(after, ctx);
      MethodInvokation invokation = create.invokation(null,null,name, args);
      for (ASTNode node : after) {
        invokation.get_after().add(node);
      }
      return invokation;
    }
    if (match(ctx,null,"(",")")){
      String name=getIdentifier(ctx,0);
      ASTList after = new ASTList();
      scan_comments_after(after, ctx);
      MethodInvokation invokation = create.invokation(null,null,name,new ASTNode[0]);
      for (ASTNode node : after) {
        invokation.get_after().add(node);
      }
      return invokation;
    }
    ParseTree t=ctx.getChild(0);
    if (t instanceof TerminalNode){
      TerminalNode tn=(TerminalNode)t;
      Token tok=tn.getSymbol();
      Vocabulary voc=parser.getVocabulary();
      String name=voc.getSymbolicName(tok.getType());
      String text=tok.getText();
      switch(name){
        case "Identifier":
          return convert(ctx,0);
        case "Constant":
          if (text.matches("[0-9]*")){
            return create.constant(Integer.parseInt(text));
          } else if (text.matches("0x[0-9]*")) {
            return create.constant(Integer.parseInt(text.substring(2),16));
          } else if (text.matches("[0-9]*[.][0-9]*")) {
            return create.constant(Float.parseFloat(text));
          } else if (text.matches("['].*[']")) {
            return create.constant(text);
          } else {
            throw new hre.lang.HREError("could not match constant: %s",text);
          }
        case "StringLiteral":
          String str=text;
          int i=1;
          while(i<ctx.getChildCount()){
            tn=(TerminalNode)ctx.getChild(i);
            tok=tn.getSymbol();
            str=str+tok.getText();
            i++;
          }
          return create.constant(str);
        default:
          throw new hre.lang.HREError("missing case in visitPrimaryExpression: %s",name);
      }
    }
    return null;
  }

  @Override
  public ASTNode visitPrimaryExpression(PrimaryExpressionContext ctx) {
    return visitPrimaryExpression((ParserRuleContext)ctx);
  }

  @Override
  public ASTNode visitPureFunctionDeclaration(PureFunctionDeclarationContext ctx) {
    Type t=checkType(convert(ctx,0));
    ASTNode expr=convert(ctx,3);
    int ofs=1;
    String name=null;
    ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
    if (match((DeclaratorContext)ctx.getChild(ofs),"DirectDeclaratorContext")){
      DirectDeclaratorContext decl_ctx=(DirectDeclaratorContext)((DeclaratorContext)ctx.getChild(ofs)).getChild(0);
      if (match(decl_ctx,null,"(","ParameterTypeListContext",")")){
        enter(decl_ctx);
        name=getIdentifier(decl_ctx, 0);
        ParserRuleContext arg_ctx=(ParserRuleContext)decl_ctx.getChild(2);
        convert_parameters(args, arg_ctx);
        leave(decl_ctx,null);
      } else {
        return null;
      }
    } else {
      throw hre.lang.System.Failure("unknown declarator%ntree: %s",ctx.getChild(ofs).toStringTree(parser));
    }
    hre.lang.System.Warning("? %s (?) = %s",name,Configuration.getDiagSyntax().print(expr));
    return create.function_decl(t, null, name, args.toArray(new DeclarationStatement[0]), expr);
  }

  @Override
  public ASTNode visitRelationalExpression(RelationalExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitSelectionStatement(SelectionStatementContext ctx) {
    if (match(ctx,"if","(","ExpressionContext",")",null)){  //DRB --Added  
        return create.ifthenelse(convert(ctx,2),convert(ctx,4));
    }
    else if (match(ctx,"if","(","ExpressionContext",")",null,"else",null)){ //DRB --Added     
        return create.ifthenelse(convert(ctx,2),convert(ctx,4),convert(ctx,6));
    }
    return null;
  }

  @Override
  public ASTNode visitShiftExpression(ShiftExpressionContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitSpecificationDeclaration(
      SpecificationDeclarationContext ctx) {
    return null;
  }

  @Override  
  public ASTNode visitSpecificationSequence(SpecificationSequenceContext ctx) {
	  return null;
  }

  @Override
  public ASTNode visitSpecifierQualifierList(SpecifierQualifierListContext ctx) {
    if (match(ctx,"unsigned",null)){
      ASTNode tmp=convert(ctx,1);
      if (tmp instanceof DeclarationStatement){
        DeclarationStatement decl=(DeclarationStatement)convert(ctx,1);
        return create.field_decl(decl.name(), create.type_expression(TypeOperator.Unsigned,decl.getType()));
      } else {
        return create.type_expression(TypeOperator.Unsigned,(Type)tmp);
      }
    }
    if (match(ctx,"const",null)){
      ASTNode tmp=convert(ctx,1);
      if (tmp instanceof DeclarationStatement){
        DeclarationStatement decl=(DeclarationStatement)convert(ctx,1);
        return create.field_decl(decl.name(), create.type_expression(TypeOperator.Const,decl.getType()));
      } else {
        return create.type_expression(TypeOperator.Const,(Type)tmp);
      }
    }
    if (match(ctx,null,null)){
      Type t=checkType(convert(ctx,0));
      String name=getIdentifier(ctx,1);
      return create.field_decl(name, t);
    }
    return null;
  }

  @Override
  public ASTNode visitStatement(StatementContext ctx) {
	  return null;
  }

  @Override
  public ASTNode visitStaticAssertDeclaration(StaticAssertDeclarationContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitStorageClassSpecifier(StorageClassSpecifierContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitStructDeclaration(StructDeclarationContext ctx) {
    if (match(ctx,null,null,";")){
      Type t=checkType(convert(ctx,0));
      VariableDeclaration decl=create.variable_decl(t);
      ASTNode n = convert(ctx,1);
      decl.add((DeclarationStatement)n);
      return decl;
    }
    if (match(ctx,null,";")){
      return convert(ctx,0);
    }
    return null;
  }

  @Override
  public ASTNode visitStructDeclarationList(StructDeclarationListContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitStructDeclarator(StructDeclaratorContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitStructDeclaratorList(StructDeclaratorListContext ctx) {
    return null;
  }

  @Override
  public ASTNode visitStructOrUnion(StructOrUnionContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitStructOrUnionSpecifier(StructOrUnionSpecifierContext ctx) {
    if (match(ctx,"struct",null)){
      String name=getIdentifier(ctx,1);
      return create.class_type(name);
    }
    String name=null;
    int ofs;
    if (match(0,true,ctx,"struct",null,"{")){
      name=getIdentifier(ctx,1);
      ofs=3;
    } else if (match(0,true,ctx,"struct","{")){
      name="struct_"+(++struct_no);
      ofs=2;
    } else {
      return null;
    }
    ASTClass res=new ASTClass(name,ClassKind.Record);
    res.setOrigin(create.getOrigin());
    if (!match(ofs,true,ctx,null,"}")){
      return null;
    }
    scan_to(res,ctx,ofs,ofs+1);
    ofs+=2;
    int N=ctx.getChildCount();
    if (N==ofs){
      if (expect!=null && expect==Type.class){
        unit.add(res);
        return create.class_type(name); 
      } else {
        return res;
      }
    } else {
      return null;
    }
  }

  @Override
  public ASTNode visitTranslationUnit(TranslationUnitContext ctx) {

    return null;
  }

  @Override
  public ASTNode visitType(TypeContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeArgs(TypeArgsContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypedefName(TypedefNameContext ctx) {
    String name=getIdentifier(ctx,0);
    return create.unresolved_name(name);
  }

  @Override
  public ASTNode visitTypeName(TypeNameContext ctx) {
    if (match(ctx,null,"AbstractDeclarator")){
      // TODO: check that the second part is *!
      return create.type_expression(TypeOperator.PointerTo,checkType(convert(ctx,0)));
    }
    return null;
  }

  @Override
  public ASTNode visitTypeQualifier(TypeQualifierContext ctx) {
    
    return null;
  }

  @Override
  public ASTNode visitTypeQualifierList(TypeQualifierListContext ctx) {
    if(match(ctx,"volatile")){
      return create.label("volatile");
    }
    return null;
  }

  @Override
  public ASTNode visitTypeSpecifier(TypeSpecifierContext ctx) {
    if(match(ctx,"signed")){
      return create.label("signed");
    }
    return null;
  }

  @Override
  public ASTNode visitUnaryExpression(UnaryExpressionContext ctx) {
    if(match(ctx,"sizeof",null)){
      return create.expression(StandardOperator.SizeOf,convert(ctx,1));
    }
    return null;
  }

  @Override
  public ASTNode visitUnaryOperator(UnaryOperatorContext ctx) {
    
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


}
