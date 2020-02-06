package vct.antlr4.parser;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import hre.ast.FileOrigin;
import hre.ast.Origin;
import hre.lang.Failure;
import hre.lang.HREError;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.generic.ASTSequence;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.type.ClassType;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.Type;
import vct.col.ast.stmt.decl.VariableDeclaration;
import vct.col.syntax.Syntax;
import vct.col.util.ASTFactory;
import static hre.lang.System.*;
import static vct.col.ast.stmt.decl.ASTSpecial.Kind.*;
import static vct.col.ast.expr.StandardOperator.*;

/**
 * Convert common parts of all ANTLR parse trees to COL.
 * 
 * This class implements functionality that all parse tree converters need.
 *
 * @author <a href="mailto:s.c.c.blom@utwente.nl">Stefan Blom</a>
*/
public class ANTLRtoCOL implements ParseTreeVisitor<ASTNode> {

  /** How to translate the atomic block. */
  public final boolean csl_atomic;
  
  /** Syntax of the language being parsed. */ 
  protected final Syntax syntax;
  /** Factory for COL AST nodes. */
  protected final ASTFactory<ParseTree> create=new ASTFactory<ParseTree>();
  /** Reference to the token stream, needed to access comments and otehr hidden tokens. */
  protected final BufferedTokenStream tokens;

  /** Name of the file that was parsed. */
  protected final String filename;
  /** File from which the current position was read. */
  protected String current_filename;
  /** Keep track of the difference between input line numbers and file line numbers. */
  protected int line_offset;
  
  /** Reference to the parser, used for debugging messages. */
  protected final org.antlr.v4.runtime.Parser parser;
  
  /** Number of the token for identifiers. */
  protected final int id_token;
  
  /** The number of the channel used for comments. */
  protected final int ch_comment;
  /** The number of the channel used for line directives.
   * 
   *  When a file is passed through the C Pre Processor, line directives
   *  are added in order to be able to tell from which file the following lines
   *  were included. We also use this features to get the correct line numbers
   *  for specification comments. */ 
  protected final int ch_line_direction;
  
  /**
   * Keeps track of the (specification) comments that have already been attached to the AST. 
   */
  private HashSet<Integer> attached_comments=new HashSet<Integer>();
 
  /**
   * Even though ANTLR4 grammars can share large parts, their parsers
   * do not share their internal classes. Thus we need to use reflection
   * to map the shared names for rules in the grammar to classes.
   */
  protected HashMap<String,Class<?>> context=new HashMap<String,Class<?>>();

  
  public final ASTSequence<?> unit;
  
  public final Class<?> lexer_class;
  
  /**
   * Create a new parse tree converter.
   * 
   * @param syntax Syntax for the common types and operations of the language the is being converted.
   * @param filename The name of the main file that was parsed to generate the parse tree.
   * @param tokens The token stream from which the par tree was built.
   * @param parser The parser for the language that is being converted.
   * @param identifier The number of the token that represents identifiers.
   * @param lexer_class The class of the lexer for the language.
   */
  public ANTLRtoCOL(ASTSequence<?>  unit,boolean csl_atomic,Syntax syntax,String filename,BufferedTokenStream tokens,
      org.antlr.v4.runtime.Parser parser, int identifier, Class<?> lexer_class){
    this.csl_atomic=csl_atomic;
    this.unit=unit;
    this.lexer_class=lexer_class;
    this.syntax=syntax;
    this.filename=filename;
    current_filename=filename;
    this.tokens=tokens;
    this.parser=parser;
    this.id_token=identifier;
    ch_comment=getStaticInt(lexer_class,"CH_COMMENT");
    ch_line_direction=getStaticInt(lexer_class,"CH_LINEDIRECTION");
    Class<?> parser_classes[]=parser.getClass().getDeclaredClasses();
    for(Class<?> cl:parser_classes){
      String name=cl.getName();
      int pos=name.lastIndexOf('$');
      name=name.substring(pos+1);
      //Warning("putting %s",name);
      context.put(name,cl);
    }
    context.put("TerminalNode", TerminalNode.class);
  }

  
  /**
   * Process line directives to generate the origin of a range of tokens.
   * 
   * @param tok1 First token in the range.
   * @param tok2 Last token in the range.
   * @return Origin of the range [tok1,tok2].
   */
  public Origin origin(Token tok1,Token tok2){
    List<Token> direction=tokens.getHiddenTokensToLeft(tok1.getTokenIndex(),ch_line_direction);
    if (direction!=null) {
      Token tok=direction.get(direction.size()-1);
      //for(Token tok:direction){
        //int id=tok.getTokenIndex();
        //if (interpreted_directions.contains(id)) continue;
        //interpreted_directions.add(id);
        String line[]=tok.getText().split("([ \t])+");
        Debug("line %d maps to line %s of file %s",tok.getLine(),line[1],line[2]);
        line_offset=Integer.parseInt(line[1])-tok.getLine()-1;
        current_filename=line[2].substring(1,line[2].length()-1);
        
      //}
      Debug("line offset in %s is %d",current_filename,line_offset);
    }
    if (line_offset+tok1.getLine()<0){
      throw new HREError("line %d while offset is %d",tok1.getLine(),line_offset);
    }
    return new FileOrigin(current_filename,line_offset+tok1.getLine(),tok1.getCharPositionInLine()+1,
        line_offset+tok2.getLine(),tok2.getCharPositionInLine()+tok2.getStopIndex()-tok2.getStartIndex()+1);
    
  }

  /** Get static field by reflection. */
  public static boolean hasStaticInt(Class<?> cl,String field){
    try {
      @SuppressWarnings("unused")
      Field f=cl.getDeclaredField(field);
      return true;
    } catch (NoSuchFieldException e) {
      return false;
    } catch (SecurityException e) {
      DebugException(e);
    } catch (IllegalArgumentException e) {
      DebugException(e);
    }
    throw hre.lang.System.Failure("reflection failed to work for field %s",field);
  }
  /** Get static field by reflection. */
  public static int getStaticInt(Class<?> cl,String field){
    try {
      Field f=cl.getDeclaredField(field);
      return f.getInt(null);
    } catch (NoSuchFieldException e) {
      throw hre.lang.System.Failure("class has no static field %s",field);
    } catch (SecurityException e) {
      DebugException(e);
    } catch (IllegalArgumentException e) {
      DebugException(e);
    } catch (IllegalAccessException e) {
      DebugException(e);
    }
    throw hre.lang.System.Failure("reflection failed to work for field %s",field);
  }
  
  /** Enter a new context for processing parse trees by setting the current origin
   *  in the AST factory.
   */
  public void enter(ParseTree node){
	 
    create.enter();
    
    Origin origin;
    if (node instanceof ParserRuleContext){    	
    	ParserRuleContext ctx=(ParserRuleContext)node;
    	if (ctx.stop==null) {
    	  origin=origin(ctx.start,ctx.start);
    	} else {
      	origin=origin(ctx.start,ctx.stop);
    	}
    } else  if (node instanceof TerminalNode) {    	
      Token tok=((TerminalNode)node).getSymbol();
      origin=origin(tok,tok);
    } else {
      throw Failure("unknown parse tree node: %s",node.getClass());
    }
    
    create.setOrigin(origin);
  }
  
  /** Leave context after parse tree has been converted. */ 
  public void leave(ParseTree node, ASTNode res) {
    /* no longer needed because specifications are gathered in
     * a follow up pass.
    if (//res instanceof vct.col.ast.expr.MethodInvokation ||
        res instanceof vct.col.ast.stmt.composite.LoopStatement){
      BeforeAfterAnnotations loc=(BeforeAfterAnnotations)res;
      BlockStatement block;
      block=loc.get_before();
      scan_loop_comments_before(block,node);
      //comments following a loop are collected in a later stage.
      //block=loc.get_after();
      //scan_comments_after(block,node);
    }
    */
    create.leave();
  }
  
  public void scan_to(ASTSequence<?> unit,ParseTree tree){
    try {
      scan_to_rec(unit,tree);
    } catch(MissingCase mc) {
      Warning("in tree %s: %s",tree.toStringTree(parser),mc);
      throw mc;
    }
  }
  public void scan_to(ASTSequence<?> unit,ParserRuleContext ctx,int from,int upto){
    for(int i=from;i<upto;i++){
      try {
        BlockStatement temp=new BlockStatement();
        scan_to_rec(temp,ctx.getChild(i));
        scan_comments_before(unit,ctx.getChild(i));
        for(ASTNode n:temp){
          n.clearParent();
          unit.add(n);
        }
      } catch(MissingCase mc) {
        Warning("in tree %s: %s",ctx.getChild(i).toStringTree(parser),mc);
        throw mc;
      }
    }
    if (upto>from){
      scan_comments_after(unit,ctx.getChild(upto-1));
    }
  }
  public void scan_loop_comments_before(ASTSequence<?> unit,ParseTree tree){
    Token tok;
    if (tree instanceof TerminalNode){
      tok=((TerminalNode)tree).getSymbol();
    } else {
      tok=((ParserRuleContext)tree).start;
    }
    List<Token> comments=tokens.getHiddenTokensToLeft(tok.getTokenIndex(),ch_comment);
    boolean take=false;
    if (comments!=null) for(Token tk:comments){
      int id=tk.getTokenIndex();
      if (!attached_comments.contains(id)){
        if (!take){
          take=tk.getText().contains("loop_invariant");
        }
        if (take) {
          attached_comments.add(id);
          Debug("attaching %s",tk.getText());
          unit.add(comment(tk));
        } else {
          Debug("skipping %s",tk.getText());
        }
      } else {
        Debug("skipping %s",tk.getText());
      }
    }    
  }
  public void scan_comments_before(ASTSequence<?> unit,ParseTree tree){
    Token tok;
    if (tree instanceof TerminalNode){
      tok=((TerminalNode)tree).getSymbol();
    } else {
      tok=((ParserRuleContext)tree).start;
    }
    List<Token> comments=tokens.getHiddenTokensToLeft(tok.getTokenIndex(),ch_comment);
    if (comments!=null) for(Token tk:comments){
      int id=tk.getTokenIndex();
      if (!attached_comments.contains(id)){
        attached_comments.add(id);
        Debug("attaching %s",tk.getText());
        unit.add(comment(tk));
      } else {
        Debug("skipping %s",tk.getText());
      }
    }    
  }
  public void scan_comments_after(ASTSequence<?> unit,ParseTree tree){
    Token tok;
    if (tree instanceof TerminalNode){
      tok=((TerminalNode)tree).getSymbol();
      if (tok.getType()<0) return; // EOF??
    } else {
      tok=((ParserRuleContext)tree).stop;
    }
    List<Token> comments=tokens.getHiddenTokensToRight(tok.getTokenIndex(),ch_comment);
    if (comments!=null) for(Token tk:comments){
      int id=tk.getTokenIndex();
      if (!attached_comments.contains(id)){
        attached_comments.add(id);
        Debug("attaching %s",tk.getText());
        unit.add(comment(tk));
      } else {
        Debug("skipping %s",tk.getText());
      }
    }    
  }
  
  private void scan_to_rec(ASTSequence<?> unit,ParserRuleContext ctx,int from,int upto){
    for(int i=from;i<upto;i++){
      BlockStatement temp=new BlockStatement();
      scan_to_rec(temp,ctx.getChild(i));
      scan_comments_before(unit,ctx.getChild(i));
      for(ASTNode n:temp){
        n.clearParent();
        unit.add(n);
      }
    }
    if (upto>from) scan_comments_after(unit,ctx.getChild(upto-1));
  }
  private void scan_to_rec(ASTSequence<?> unit,ParseTree tree){
	
	enter(tree);
    
    ASTNode res=tree.accept(this);
    if (res==null){
      res=visit(tree);
    }
    leave(tree,res);
    
    scan_comments_before(unit,tree);
    if (res==null){
      if (tree instanceof ParserRuleContext){
        ParserRuleContext ctx=(ParserRuleContext)tree;
        scan_to_rec(unit,ctx,0,ctx.getChildCount());
      } else if (tree instanceof TerminalNode){
        TerminalNode n=(TerminalNode)tree;
        if (n.getSymbol().getType()!=Recognizer.EOF){
          throw new MissingCase("missing case in %s: %s%ntree: %s%nat: %s",
              this.getClass(),tree.getClass(),tree.toStringTree(parser),
              create.getOrigin());
        }
      }
    } else {
      unit.add(res);
    }
  }
  
  public ASTNode comment(Token tk){
    create.enter();
    Debug("comment at line %d",tk.getLine());
    create.setOrigin(origin(tk,tk));
    ASTNode res=create.comment(tk.getText());
    create.leave();    
    return res;
  }
  
  public ASTNode convert(ParseTree arg0){
    enter(arg0);
    ASTNode res=arg0.accept(this);
    if (res==null){
      res=visit(arg0);
    }
    if (res==null){
      throw new MissingCase("missing case in %s: %s%ntree: %s%nat %s",
          this.getClass(),arg0.getClass(),arg0.toStringTree(parser),create.getOrigin());
    }
    leave(arg0,res);
    return res;
  }
    
  @Override
  public ASTNode visit(ParseTree arg0) {
    if (arg0 instanceof ParserRuleContext){
      Debug("Scanning using Syntax");
      ParserRuleContext ctx=(ParserRuleContext)arg0;
      if (ctx.children != null && ctx.children.size()==1){
        ParseTree tmp=ctx.getChild(0);
        if (tmp instanceof ParserRuleContext) {
          return convert(tmp);
        } else {
          for(PrimitiveSort sort : PrimitiveSort.values()){
            String text=syntax.getPrimitiveType(sort);
            if (text!=null && match(ctx,text)){
              return create.primitive_type(sort);
            }
          }
        }
        if (tmp instanceof TerminalNode){
          Token tok=((TerminalNode)tmp).getSymbol();
          /* By testing for reserved names first, we do not need
           * to enumerate all reserved names in the grammar.
           */
          if(syntax.is_reserved(tok.getText())){
            return create.reserved_name(syntax.reserved(tok.getText()));
          }
          if (tok.getType()==id_token) {            
            return create.unresolved_name(tok.getText());
          }
        }
        return null;
        //throw Failure("missing case in %s: %s%ntree: %s",this.getClass(),arg0.getClass(),arg0.toStringTree(parser));
      }
      for(StandardOperator op:StandardOperator.values()){
        String pat[]=syntax.getPattern(op);
        if (pat!=null){
          Debug("Scanning for %s",op);
          if (match(ctx,pat)){
            int N=op.arity();
            ASTNode args[]=new ASTNode[N];
            int i=0;
            int j=0;
            while(j<N){
              if (pat[i]==null){
                args[j]=convert(arg0.getChild(i));
                j++;
              }
              i++;
            }
            return create.expression(op,args);
          }
        }
      }
      if (match(ctx,"(",null,")")){
        return convert(ctx,1);
      } else if (match(ctx,"modifies",null,";")){
        ASTNode list[]=convert_list((ParserRuleContext) ctx.getChild(1), ",");
        return create.special_decl(ASTSpecial.Kind.Modifies,list);
      } else if (match(ctx,"accessible",null,";")){
        ASTNode list[]=convert_list((ParserRuleContext) ctx.getChild(1), ",");
        return create.special_decl(ASTSpecial.Kind.Accessible,list);
      } else if (match(ctx,"requires",null,";")){                     
        return create.special_decl(ASTSpecial.Kind.Requires,convert(ctx,1));
      } else if (match(ctx,"ensures",null,";")){
        return create.special_decl(ASTSpecial.Kind.Ensures,convert(ctx,1));
      } else if (match(ctx,"given",null,";")){
        return create.special_decl(ASTSpecial.Kind.Given,create.block(convert(ctx,1)));
      } else if (match(ctx,"given",null,null,";")){
        DeclarationStatement decl=create.field_decl(getIdentifier(ctx,2),checkType(convert(ctx,1)));
        return create.special_decl(ASTSpecial.Kind.Given,create.block(decl));
      } else if (match(ctx,"yields",null,";")){
        return create.special_decl(ASTSpecial.Kind.Yields,create.block(convert(ctx,1)));
      }
      ParseTree tmp=ctx.getChild(0);
      int N=ctx.getChildCount();
      if(tmp instanceof TerminalNode){
        Token tok=((TerminalNode)tmp).getSymbol();
        if (syntax.is_annotation(tok.getText())){
          if (!match(N-1,false,ctx,";")){
            throw new HREError("missing ; at end of special");
          }
          ASTNode list[]=convert_list(ctx, 1, N-1, ",");
          ASTSpecial.Kind kind=syntax.get_annotation(tok.getText(),list.length);
          if (kind==null){
            Abort("arity of special %s is not %d",tok.getText(),list.length);
          }
          return create.special(kind,list);
        }
      }
    } else if (arg0 instanceof TerminalNode){
      Token tok=((TerminalNode)arg0).getSymbol();
      if (tok.getType()==id_token) {
        return create.unresolved_name(tok.getText());
      } else if (syntax.is_reserved(tok.getText())){
        return create.reserved_name(syntax.reserved(tok.getText()));
      }
    }
    return null;
    //throw Failure("missing case in %s: %s%ntree: %s",this.getClass(),arg0.getClass(),arg0.toStringTree(parser));
  }

  @Override
  public ASTNode visitChildren(RuleNode arg0) {
    throw Failure("illegal call to %s.visitChildren",this.getClass());
  }

  @Override
  public ASTNode visitErrorNode(ErrorNode arg0) {
    return visit(arg0);
  }

  @Override
  public ASTNode visitTerminal(TerminalNode arg0) {
    return visit(arg0);
  }

  protected ASTNode convert(ParserRuleContext ctx,int i){
    return convert(ctx.children.get(i));
  }
  protected ASTNode[] convert_all(ParserRuleContext ctx){
    int N;
    if (ctx.children==null) N=0; else N=ctx.children.size();
    ASTNode[] res=new ASTNode [N];
    for(int i=0;i<N;i++){
      res[i]=convert(ctx,i);
    }
    return res;
  }
  protected ASTNode[] convert_range(ParserRuleContext ctx,int from,int upto){
    int N=upto-from;
    ASTNode[] res=new ASTNode [N];
    for(int i=0;i<N;i++){
      res[i]=convert(ctx,from+i);
    }
    return res;
  }
  
  private void convert_linked(ArrayList<ASTNode> res,ParserRuleContext ctx,String sep){
    if (match(ctx,null,sep,null)){
      convert_linked(res,(ParserRuleContext)ctx.getChild(0),sep);
      res.add(convert(ctx,2));
    } else {
      res.add(convert(ctx));
    }
  }
  protected ASTNode[] convert_linked_list(ParserRuleContext ctx,String sep){
    ArrayList<ASTNode> res=new ArrayList<ASTNode>();
    convert_linked(res,ctx,sep);
    return res.toArray(new ASTNode[0]);
  }
  
  private void convert_smart(ArrayList<ASTNode> res,ParseTree tree,String sep){
    if (tree instanceof ParserRuleContext){
      ParserRuleContext ctx=(ParserRuleContext)tree;
      if (match(0,true,ctx,null,sep,null)){
        convert_smart(res,ctx.getChild(0),sep);
        int N=ctx.getChildCount();
        int ofs=1;
        while(ofs+1 < N && match(ofs,true,ctx,sep,null)){
          convert_smart(res,ctx.getChild(ofs+1),sep);
          ofs+=2;
        }
      } else {
        res.add(convert(tree));
      }
    } else {
      res.add(convert(tree));
    }
  }
  protected ASTNode[] convert_smart_list(ParserRuleContext ctx,String sep){
    ArrayList<ASTNode> res=new ArrayList<ASTNode>();
    convert_smart(res,ctx,sep);
    return res.toArray(new ASTNode[0]);
  }
  
  protected ASTNode[] convert_list(ParseTree tree,String open,String sep,String close){
    ParserRuleContext ctx=(ParserRuleContext)tree;
    int N=ctx.getChildCount();
    if (match(0,true,ctx,open)&&match(N-1,true,ctx,close)){
      return convert_list(ctx,1,N-1,sep);
    }
    return null;
  }

  
  protected ASTNode[] convert_list(ParserRuleContext ctx,String sep){
    if (ctx==null || ctx.children==null) {
      return new ASTNode[0];
    } else {
      return convert_list(ctx,0,ctx.getChildCount(),sep);
    }
  }
  protected ASTNode[] convert_list(ParserRuleContext ctx,int from,int upto,String sep){
    int N=(upto-from+1)/2;
    ASTNode[] res=new ASTNode [N];
    for(int i=0;i<N;i++){
      res[i]=convert(ctx,from+2*i);
      if (i+1<N && !match(from+2*i+1,true,ctx,sep)){
        Debug("bad separator");
        return null;
      }
    }
    return res;
  }

  protected ASTNode[] convert_pairs(ParseTree tree,String open,String sep, String pairSep, String close){
    ParserRuleContext ctx=(ParserRuleContext)tree;
    int N=ctx.getChildCount();
    if (match(0,true,ctx,open)&&match(N-1,true,ctx,close)){
      return convert_pairs(ctx,1,N-1,sep, pairSep);
    }
    return null;
  }
  protected ASTNode[] convert_pairs(ParserRuleContext ctx,String sep, String pairSep){
    if (ctx==null || ctx.children==null) {
      return new ASTNode[0];
    } else {
      return convert_pairs(ctx,0,ctx.getChildCount(), sep, pairSep);
    }
  }
  protected ASTNode[] convert_pairs(ParserRuleContext ctx,int from,int upto,String sep, String pairSep){
    int N=(upto-from+1)/4;
    ASTNode[] res=new ASTNode [N*2];
    for(int i=0;i<N;i++){
      res[2*i]=convert(ctx,from+4*i);
      res[2*i+1]=convert(ctx,from+4*i+2);
      if (i+3<N && !match(from+4*i+1,true,ctx,pairSep) && !match(from+4*i+3,true,ctx,sep)){
        Debug("bad separator");
        return null;
      }
    }
    return res;
  }

  protected boolean instance(Object item,String pattern){
    Class<?> cls=context.get(pattern);
    if (cls==null){
      cls=context.get(pattern+"Context");
    }
    if (cls!=null){
      return cls.isInstance(item);
    } else {
      return false;
    }
  }
  /**
   * Check if the children of an ANTLT parse tree node match a given pattern.
   * 
   * The pattern matching has three cases:
   * <ul>
   * <li> A string which matches the name of a parse tree node matches only nodes of that type. </li>
   * <li> A non-null string matches a token with the same contents. </li>
   * <li> A null string matches any node.</li>
   * </ul>
   * @param ctx The node that has to be matched.
   * @param pattern The pattern that has to be matched.
   * @return true in case of a match and false otherwise.
   */
  protected boolean match(ParserRuleContext ctx,String ... pattern){
    return match(0,false,ctx,pattern);
  }
  /**
   * Check if a sub-range of the children of an ANTLT parse tree node match a given pattern.
   * 
   * The pattern matching has three cases:
   * <ul>
   * <li> A string which matches the name of a parse tree node matches only nodes of that type. </li>
   * <li> A non-null string matches a token with the same contents. </li>
   * <li> A null string matches any node.</li>
   * </ul>
   * 
   * @param ofs First node to match.
   * @param prefix Accept match if there are more children.
   * @param ctx The node that has to be matched.
   * @param pattern The pattern that has to be matched.
   * @return true in case of a match and false otherwise.
   */
  protected boolean match(int ofs,boolean prefix,ParserRuleContext ctx,String ... pattern){
    if (ctx.children==null) return pattern.length==0 && ofs==0;
    if (ctx.children.size()<ofs+pattern.length) return false;
    if (!prefix && ctx.children.size()>ofs+pattern.length) return false;
    for(int i=0;i<pattern.length;i++){
      if (pattern[i]==null) continue;
      ParseTree item=ctx.children.get(ofs+i);
      if (item instanceof TerminalNode){
        if(hasStaticInt(lexer_class, pattern[i])) {
          int id=getStaticInt(lexer_class, pattern[i]);
          TerminalNode tn=(TerminalNode)item;
          Token tok=tn.getSymbol();
          if (tok.getType() == id) continue;
        }
      }
      Class<?> cls=context.get(pattern[i]);
      if (cls==null){
        cls=context.get(pattern[i]+"Context");
      }
      if (cls!=null){
        if (cls.isInstance(item)) continue;
        return false;
      } else {
        if (item.toString().equals(pattern[i])) continue;
        if (item instanceof ParserRuleContext)
        {//BLM - DRB --added
        ParserRuleContext item_ctx = (ParserRuleContext) item;
        if(item_ctx.children !=null && item_ctx.children.size() ==1 )            
        	if (item_ctx.children.get(0).toString().equals(pattern[i])) continue;
        }
        return false;
      }
    }
    return true;
  }
  
  protected String[] getIdentifierList(ParserRuleContext ctx,String sep){
    int N=(ctx.getChildCount()+1)/2;
    String res[]=new String[N];
    if (N>0){
      res[0]=getIdentifier(ctx,0);
    }
    for(int i=1;i<N;i++){
      if (!match(2*i-1,true,ctx,sep)){
        Abort("bad separator");
      }
      res[i]=getIdentifier(ctx,2*i);
    }
    return res;
  }
  
  protected String getIdentifier(ParserRuleContext ctx, int i) {
    return getIdentifier(ctx,i,true);
  }

  protected String getGeneralizedIdentifier(ParserRuleContext ctx, int i) {
    return getIdentifier(ctx,i,false);
  }
  
  protected String getIdentifier(ParserRuleContext ctx, int i, boolean strict) {
    ParseTree node=ctx.children.get(i);
    if (node==null) Abort("child %d does not exist",i);
    while(node instanceof ParserRuleContext){
      ParserRuleContext tmp=(ParserRuleContext)node;
      if (tmp.children.size()==1){
        node=tmp.getChild(0);
      } else {
        Abort("not a nested identifier: %s",node.toStringTree(parser));
      }
    }
    if (node instanceof TerminalNode){
      Token tok=((TerminalNode)node).getSymbol();
      //if ((!strict) || tok.getType()==id_token) {
        return tok.getText();
      //}
    }
    Abort("child %d (%s) is not an identifier",i,node.toStringTree(parser));
    return null;
  }
  
  public Contract getContract(ParserRuleContext ctx){
    ContractBuilder cb=new ContractBuilder();
    for(ParseTree t:ctx.children){      
      if (t instanceof ParserRuleContext){
        ParserRuleContext clause=(ParserRuleContext)t;        
        enter(clause);        
        if (match(clause,"requires",null,";")){                   	
          cb.requires(convert(clause,1));
        } else if (match(clause,"ensures",null,";")){
          cb.ensures(convert(clause,1));
        } else if (match(clause,"given",null,";")){
          ASTNode decl=convert(clause,1);
          decl.setGhost(true);
          if (decl instanceof DeclarationStatement){
            cb.given((DeclarationStatement)decl);
          } else if (decl instanceof VariableDeclaration){
            cb.given((VariableDeclaration)convert(clause,1));
          }
        } else if (match(clause,"given",null,null,";")){
          DeclarationStatement decl=create.field_decl(getIdentifier(clause,2),checkType(convert(clause,1)));
          decl.setGhost(true);
          cb.given(decl);
        } else if (match(clause,"yields",null,";")){
          ASTNode decl=convert(clause,1);
          if (decl instanceof DeclarationStatement){
            cb.yields((DeclarationStatement)convert(clause,1));
          } else if (decl instanceof VariableDeclaration){
            cb.yields((VariableDeclaration)convert(clause,1));
          }          
        }  else {
          throw hre.lang.System.Failure("bad clause %s",t);
        }
        leave(clause,null);
      } else {
        throw hre.lang.System.Failure("bad clause %s",t);
      }
    }
    Contract res=cb.getContract(false);
    res.setOrigin(origin(ctx.start,ctx.stop));
    return res;    
  }
  public Type checkType(ASTNode n){
    if (n instanceof Type) {
      return (Type)n;
    }
    if (n instanceof NameExpression){
      return create.class_type(n.getOrigin(),((NameExpression)n).getName());
    }
    throw hre.lang.System.Failure("%s node at %s is not a type",n.getClass(),n.getOrigin());
  }

  protected HREError MissingCase(ParserRuleContext ctx){
    return new HREError("missing case: %s",ctx.toStringTree(parser));
  }
  
  public ASTNode getSpecificationPrimary(ParserRuleContext ctx) {
    if (match(ctx,null,":",null)){
      ASTNode res=convert(ctx,2);
      res.addLabel(create.label(getIdentifier(ctx,0)));
      return res;
    }
    if (match(ctx,"TypeContext","{","}")){
      return create.struct_value(checkType(convert(ctx,0)),null);
    }
    if (match(ctx,"TypeContext","{","ExpressionListContext","}")){
      ASTNode args[]=convert_list((ParserRuleContext)ctx.getChild(2),",");
      Type t=checkType(convert(ctx,0));
      return create.struct_value(t,null,args);
    }
    if (match(ctx,"*")){
      return create.reserved_name(ASTReserved.Any);
    }
    if (match(ctx,"(","\\forall*",null,null,";",null,";",null,")")){
      return create.starall(convert(ctx,5),convert(ctx,7),
          create.field_decl(getIdentifier(ctx,3),checkType(convert(ctx,2))));
    }
    if (match(ctx,"(","\\forall",null,null,";",null,";",null,")")){
      return create.forall(convert(ctx,5),convert(ctx,7),
          create.field_decl(getIdentifier(ctx,3),checkType(convert(ctx,2))));
    }
    if (match(ctx,"(","\\sum",null,null,";",null,";",null,")")){
      return create.summation(convert(ctx,5),convert(ctx,7),
          create.field_decl(getIdentifier(ctx,3),checkType(convert(ctx,2))));
    }
    if (match(ctx,"(","\\exists",null,null,";",null,";",null,")")){
      return create.exists(convert(ctx,5),convert(ctx,7),
          create.field_decl(getIdentifier(ctx,3),checkType(convert(ctx,2))));
    }
    if (match(ctx,"(","\\let",null,null,"=",null,";",null,")")){
      return create.let_expr(create.field_decl(getIdentifier(ctx,3),checkType(convert(ctx,2)),convert(ctx,5)),convert(ctx,7));
    }
    if (match(ctx,"\\unfolding",null,"\\in",null)){
      return create.expression(StandardOperator.Unfolding,convert(ctx,1),convert(ctx,3));
    }
    return null;
  }

  public ASTNode getResourceExpression(ParserRuleContext ctx) {
    if (match(ctx,"(","\\forall*",null,null,";",null,";",null,")")){
      return create.starall(convert(ctx,5),convert(ctx,7),
          create.field_decl(getIdentifier(ctx,3),checkType(convert(ctx,2))));
    }
    if (match(ctx,"Reducible","(",null,",",null,")")){
      String op=ctx.getChild(4).getText();
      switch(op){
        case "+":
          return create.expression(StandardOperator.ReducibleSum,convert(ctx,2));
        
        default:
          throw new HREError("unknown reduction operator %s",op);
      }
    }
    return null;
  }

  public ASTNode getValStatement(ParserRuleContext ctx){
    if (match(ctx,"spec_ignore","{")){
      return create.special(SpecIgnoreStart);
    }
    if (match(ctx,"spec_ignore","}")){
      return create.special(SpecIgnoreEnd);
    }
    if (match(ctx,"assert",null,";")){
      return create.special(Assert,convert(ctx,1));
    }
    if (match(ctx,"assume",null,";")){
      return create.special(Assume,convert(ctx,1));
    }
    if (match(ctx,"refute",null,";")){
      return create.special(Refute,convert(ctx,1));
    }
    if (match(ctx,"fold",null,";")){
      return create.special(Fold,convert(ctx,1));
    }
    if (match(ctx,"unfold",null,";")){
      return create.special(Unfold,convert(ctx,1));
    }
    if (match(ctx,"witness",null,";")){
      return create.special(Witness,convert(ctx,1));
    }
    if (match(ctx,"create","BlockContext")){
        BlockStatement block=checkBlock(convert(ctx,1));
        return create.lemma(block);
      }
    if (match(ctx,"qed",null,";")){
        return create.special(QED,convert(ctx,1));
      }
    if (match(ctx,"apply",null,";")){
      return create.special(Apply,convert(ctx,1));
    }
    if (match(ctx,"use",null,";")){
      return create.special(Use,convert(ctx,1));
    }
    if (match(ctx,"send",null,"to",null,",",null,";")){
      return create.special(Send,convert(ctx,1),convert(ctx,3),convert(ctx,5));   
    }
    if (match(ctx,"recv",null,"from",null,",",null,";")){
      return create.special(Recv,convert(ctx,1),convert(ctx,3),convert(ctx,5));
    }

    int N=ctx.getChildCount();
    if (N>=3 && match(0,true,ctx,"atomic","(") && match(N-2,true,ctx,")", "Block")){
      BlockStatement block=checkBlock(convert(ctx,N-1));
      ASTNode args[]=convert_list(ctx,2,N-2,",");
      if (csl_atomic){
        return create.csl_atomic(block, args);
      } else {
        String strings[]=new String[args.length];
        for(int i=0;i<args.length;i++){
          strings[i]=args[i].toString();
        }
        return create.parallel_atomic(block, strings);
      }
    }
    
    String keyword=getIdentifier(ctx,0);
    if (!syntax.is_annotation(keyword)){
      throw new HREError("%s is not a specification command",keyword);
    }    
    ArrayList<ASTNode> args=new ArrayList<ASTNode>();
    for(int i=1;i<N;i++){
      if(match(i,true,ctx,"Identifier")||match(i,true,ctx,"Expression")||match(i,true,ctx,"Block")){
        args.add(convert(ctx,i));
      }
    }
    int argc=args.size();
    Kind kind=syntax.get_annotation(keyword, argc);
    if (kind==null){
      throw new HREError("Incorrect number of arguments for %s: %d",keyword,argc);
    }
    return create.special(kind, args);
  }

  
  private BlockStatement checkBlock(ASTNode convert) {
    if (convert instanceof BlockStatement){
      return (BlockStatement) convert;
    }
    throw new HREError("found %s instead of block",convert.getClass());
  }

  public Type getValType(ParserRuleContext ctx){
    if (match(ctx,"Identifier","TypeArgs")){
      String id=getIdentifier(ctx,0);
      ASTNode args[]=convert_list(ctx.getChild(1),"<",",",">");
      switch(id){
        case "seq": return create.primitive_type(PrimitiveSort.Sequence, args);
        default: break;
      }
    }
    Debug("NO MATCH: %s",ctx.toStringTree(parser));
    return null;
  }

  public ASTNode getValPrimary(ParserRuleContext ctx){
    int N=ctx.getChildCount();
    if (match(ctx,"Reducible","(",null,",",null,")")){
      String op=ctx.getChild(4).getText();
      switch(op){
        case "+":
          return create.expression(StandardOperator.ReducibleSum,convert(ctx,2));
        case "min":
          return create.expression(StandardOperator.ReducibleMin,convert(ctx,2));
        case "max":
          return create.expression(StandardOperator.ReducibleMax,convert(ctx,2));
        default:
          throw new HREError("unknown reduction operator %s",op);
      }
    }
    if (N>=3 && match(1,true,ctx,"(") && match(N-1,true,ctx,")")){
      String functionName = getIdentifier(ctx,0);
      StandardOperator op=syntax.parseFunction(functionName);
      if (op!=null){
        ASTNode args[]=convert_list(ctx,2,N-1,",");
        return create.expression(op, args);
      } else {
        Abort("No such function: %s", functionName);
      }
    }
    if (N>=3 && match(0,true,ctx,"Type","{") && match(N-1,true,ctx,"}")){
      Type t = checkType(convert(ctx,0));
      ASTNode args[]=convert_list(ctx,2,N-1,",");
      return create.struct_value(t,null,args);
    }
    if (match(ctx,"(","\\forall*",null,null,";",null,";",null,")")){
      return create.starall(convert(ctx,5),convert(ctx,7),
          create.field_decl(getIdentifier(ctx,3),checkType(convert(ctx,2))));
    }
    if (match(ctx,"(","\\forall*",null,null,"=",null,"..",null,";",null,")")){
      String name=getIdentifier(ctx,3);
      ASTNode guard=create.expression(And,
          create.expression(LTE,convert(ctx,5),create.local_name(name)),
          create.expression(LT,create.local_name(name),convert(ctx,7))
      );
      return create.starall(guard,convert(ctx,9),
          create.field_decl(name,checkType(convert(ctx,2))));
    }
    if (match(ctx,"(","\\forall",null,null,"=",null,"..",null,";",null,")")){
      String name=getIdentifier(ctx,3);
      ASTNode guard=create.expression(And,
          create.expression(LTE,convert(ctx,5),create.local_name(name)),
          create.expression(LT,create.local_name(name),convert(ctx,7))
      );
      return create.forall(guard,convert(ctx,9),
          create.field_decl(name,checkType(convert(ctx,2))));
    }
    if (match(ctx,"(","\\forall",null,null,";",null,";",null,")")){
      return create.forall(convert(ctx,5),convert(ctx,7),
          create.field_decl(getIdentifier(ctx,3),checkType(convert(ctx,2))));
    }
    if (match(ctx,"(","\\exists",null,null,";",null,";",null,")")){
      return create.exists(convert(ctx,5),convert(ctx,7),create.field_decl(getIdentifier(ctx,3),(Type)convert(ctx,2)));
    }
    if (match(ctx,"(","\\sum",null,null,";",null,";",null,")")){
      return create.summation(convert(ctx,5),convert(ctx,7),create.field_decl(getIdentifier(ctx,3),(Type)convert(ctx,2)));
    }
    if (match(ctx,"(","\\let",null,null,"=",null,";",null,")")){
      DeclarationStatement decl=create.field_decl(
          getIdentifier(ctx,3),
          checkType(convert(ctx,2)),
          convert(ctx,5));
      return create.let_expr(decl, convert(ctx,7));
    }
    if (match(ctx,"[",null,"]",null)){
      return create.expression(Scale,convert(ctx,1),convert(ctx,3));
    }
    if (match(ctx,"(",null,"\\memberof",null,")")){
      return create.expression(Member,convert(ctx,1),convert(ctx,3));
    }
    if (match(ctx,"|",null,"|")){
      return create.expression(Size,convert(ctx,1));
    }
    if (match(ctx,"(",null,"!",null,")")){
      return create.expression(IndependentOf,convert(ctx,1),convert(ctx,3));
    }
    if (match(ctx,"[",null,"..",null,")")){
      return create.expression(RangeSeq,convert(ctx,1),convert(ctx,3));
    }
    if (match(ctx,"\\unfolding",null,"\\in",null)){
      return create.expression(Unfolding,convert(ctx,1),convert(ctx,3));
    }
    if (match(ctx,"spec_ignore","{")){
      return create.special(SpecIgnoreStart);
    }
    if (match(ctx,"spec_ignore","}")){
      return create.special(SpecIgnoreEnd);
    }
    if (match(ctx,"*")){
      return create.reserved_name(ASTReserved.Any);
    }
    if(match(ctx, "\\pointer", "(", null, ",", null, ",", null, ")")) {
      return create.expression(StandardOperator.ValidPointer, convert(ctx, 2), convert(ctx, 4), convert(ctx, 6));
    }
    if(match(ctx, "\\pointer_index", "(", null, ",", null, ",", null, ")")) {
      return create.expression(StandardOperator.ValidPointerIndex, convert(ctx, 2), convert(ctx, 4), convert(ctx, 6));
    }
    throw new HREError("missing case for VAL primary expression: %s",ctx.toStringTree(parser));
  }


  public ClassType forceClassType(ASTNode convert) {
    if (convert instanceof ClassType) return (ClassType)convert;
    if (convert instanceof NameExpression) return create.class_type(convert.toString());
    throw hre.lang.System.Failure("cannot convert %s to ClassType",convert.getClass());
  }


  public ClassType[] forceClassType(ASTNode convert[]) {
    ClassType[] res=new ClassType[convert.length];
    for(int i=0;i<convert.length;i++){
      res[i]=forceClassType(convert[i]);
    }
    return res;
  }
}
