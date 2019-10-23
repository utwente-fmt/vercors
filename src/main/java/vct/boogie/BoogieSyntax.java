// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.boogie;

import hre.ast.TrackingOutput;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.rewrite.Parenthesize;
import vct.col.rewrite.Standardize;
import vct.col.syntax.Syntax;
import vct.col.util.SimpleTypeCheck;
import static vct.col.ast.expr.StandardOperator.*;
import static vct.col.ast.type.ASTReserved.*;

/**
 * Create Syntax objects for Boogie and Chalice.
 * 
 * @author Stefan Blom
 */
public class BoogieSyntax extends Syntax {
  
  public static enum Variant{Boogie, Chalice, Dafny };
  
  public final Variant variant;
  public BoogieSyntax(String language,Variant variant) {
    super(language);
    this.variant=variant;
  }

  private static BoogieSyntax boogie;
  private static BoogieSyntax chalice;
  
  private static BoogieSyntax dafny;
  
  private static void setCommon(Syntax syntax){
    
    syntax.addPrefix(Not,"!",130);
    syntax.addPrefix(UMinus,"-",130);
    syntax.addPrefix(UPlus,"+",130);
    syntax.addLeftFix(Mult,"*",120);
    syntax.addLeftFix(Div,"/",120);
    syntax.addLeftFix(Mod,"%",120);
    syntax.addLeftFix(Plus,"+",110);
    syntax.addLeftFix(Minus,"-",110);
    syntax.addInfix(LT,"<",90);
    syntax.addInfix(LTE,"<=",90);
    syntax.addInfix(GT,">",90);
    syntax.addInfix(GTE,">=",90);
    syntax.addInfix(EQ,"==",80);
    syntax.addInfix(NEQ,"!=",80);
    syntax.addLeftFix(Star,"&&",40); // 40??
    syntax.addLeftFix(And,"&&",40); // 40??
    syntax.addLeftFix(Or,"||",30); // 30??
    syntax.addLeftFix(Implies, "==>", 30); 
    syntax.addLeftFix(IFF, "<==>", 30);
    syntax.addRightFix(Assign,"=",10);
    syntax.addFunction(Old,"old");
    syntax.addOperator(ITE,20,"","?",":","");
    syntax.addOperator(Size,0,"|","|");
    
    syntax.addReserved(Result,"result");
    syntax.addOperator(Wrap,0,"(",")");
  }

  public static synchronized Syntax getBoogie(){
    if(boogie==null){
      boogie=new BoogieSyntax("Boogie",Variant.Boogie);
      setCommon(boogie);
    }
    return boogie;
  }
  
  public static synchronized Syntax getDafny(){
    if(dafny==null){
      dafny=new BoogieSyntax("Dafny",Variant.Dafny);
      setCommon(dafny);
      dafny.addReserved(This,"this");
      dafny.addReserved(Null,"null");
    }
    return dafny;
  }

  public static synchronized BoogieSyntax getChalice(){
    if(chalice==null){
      chalice=new BoogieSyntax("Chalice",Variant.Chalice);
      setCommon(chalice);
      chalice.addFunction(Perm,"acc");
      chalice.addOperator(Cons,0,"([","]++(","))");
      chalice.addOperator(Subscript,0,"(",")[","]");
      chalice.addLeftFix(Append,"++",100);
      
      chalice.addOperator(Member,45,"","in","");
      chalice.addReserved(This,"this");
      chalice.addReserved(Null,"null");
      chalice.addReserved(Any,"*");
    }
    return chalice;
  }

  @Override
  public AbstractBoogiePrinter print(TrackingOutput out,ASTNode n){
    AbstractBoogiePrinter p;
    switch(this.variant){
    case Boogie:
      p=new BoogiePrinter(out);
      break;
    case Chalice:
      p=new ChalicePrinter(out);
      break;
    default:
      throw new hre.lang.HREError("cannot print boogie language family member %s", variant);
    }
    if (n!=null){
      ASTNode nn=new Parenthesize(this).rewrite(n);
      nn.accept(p);
    }
    return p;
  }
  @Override
  public AbstractBoogiePrinter print(TrackingOutput out,ProgramUnit pu){
    AbstractBoogiePrinter p;
    switch(this.variant){
    case Boogie:
      p=new BoogiePrinter(out);
      break;
    case Chalice:
      p=new ChalicePrinter(out);
      break;
    case Dafny:
    	p=new DafnyPrinter(out);
    	break;
    default:
      throw new hre.lang.HREError("cannot print boogie language family member %s", variant);
    }
    if (pu!=null){
      ProgramUnit nn=new Parenthesize(this,pu).rewriteAll();
      nn=new Standardize(nn).rewriteAll();
      new SimpleTypeCheck(nn).check();
      nn.accept(p);
    }
    return p;
  }

}


/*
Java Operators 	Precedence
14 postfix 	expr++ expr--
13 unary 	++expr --expr +expr -expr ~ !
12 multiplicative 	* / %
11 additive 	+ -
10 shift 	<< >> >>>
 9 relational 	< > <= >= instanceof
 8 equality 	== !=
 7 bitwise AND 	&
 6 bitwise exclusive OR 	^
 5 bitwise inclusive OR 	|
 4 logical AND 	&&
 3 logical OR 	||
 2 ternary 	? :
 1 assignment 	= += -= *= /= %= &= ^= |= <<= >>= >>>=
*/

