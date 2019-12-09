// -*- tab-width:2 ; indent-tabs-mode:nil -*-

package vct.col.syntax;

import hre.ast.TrackingOutput;
import hre.lang.HREError;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.type.PrimitiveSort;
import vct.col.print.JavaPrinter;
import vct.col.rewrite.Parenthesize;
import static vct.col.ast.expr.StandardOperator.*;
import static vct.col.ast.type.ASTReserved.*;

/**
 * Create a Syntax object for Java.
 */
public class JavaSyntax extends Syntax {

  public final JavaDialect dialect;
  public JavaSyntax(String language,JavaDialect dialect) {
    super(language);
    this.dialect=dialect;
  }

  private static Syntax JavaSyntax;
  
  
  public synchronized static Syntax getJava(){
    if (JavaSyntax==null){
      Syntax syntax=new JavaSyntax("Java",null);
      setCommon(syntax);
      JavaSyntax=syntax;
    }
    return JavaSyntax;
  }
  
  private static JavaSyntax JavaVerCorsSyntax;
  private static JavaSyntax JavaVeriFastSyntax;
  
  public synchronized static JavaSyntax getJava(JavaDialect dialect){
    switch(dialect){
    case JavaVerCors:
      if (JavaVerCorsSyntax==null){
        JavaSyntax syntax=new JavaSyntax("Java + JML",dialect);
        setCommon(syntax);
        VerCorsSyntax.add(syntax);
        syntax.addLeftFix(Exp,"^^",125);
        syntax.addLeftFix(StructSelect,".",-1);
        syntax.addLeftFix(LeftMerge,"||_",30);

        syntax.addOperator(Member,-1,"(","\\memberof",")");
        syntax.addFunction(TypeOf,"\\typeof");
        syntax.addFunction(CurrentPerm,"perm");
        syntax.addFunction(HistoryPerm,"HPerm");
        syntax.addOperator(Scale,130,"[","]","");
        syntax.addFunction(Drop,"drop");
        syntax.addFunction(Take,"take");
        syntax.addFunction(History,"Hist");
        syntax.addFunction(Future,"Future");
        syntax.addFunction(AbstractState,"AbstractState");
        syntax.addFunction(Contribution,"Contribution");
        syntax.addFunction(Held,"held");
        syntax.addFunction(Identity,"\\id");
        syntax.addFunction(SizeOf,"\\sizeof");
        syntax.addFunction(AddrOf,"\\addrof");
        syntax.addFunction(Indirection,"\\indirect");
        syntax.addFunction(StructDeref,"\\structderef");
        syntax.addFunction(IterationOwner,"\\owner");
        syntax.addFunction(Empty,"empty");
        syntax.addFunction(RemoveAt, "remove");
        syntax.addFunction(RangeFromSeq, "range");
        syntax.addRightFix(PrependSingle, "::", 110);
        syntax.addLeftFix(AppendSingle, "++", 110);

        syntax.addFunction(Values,"\\values");

        syntax.addOperator(Unfolding,140,"\\unfolding","\\in","");
        syntax.addOperator(IndependentOf, -1 , "(" ,"!",")");
        syntax.addOperator(ReducibleSum,-1,"Reducible(",",+)");
        syntax.addReserved(EmptyProcess, "empty");
        
        syntax.addReserved(Inline,"inline");
        syntax.addReserved(ThreadLocal,"thread_local");
        syntax.addReserved(Pure,"pure");
        syntax.addReserved(CurrentThread,"\\current_thread");
        syntax.addFunction(OptionGet,"getOption");
        
        syntax.addFunction(ValidArray,"\\array");
        syntax.addFunction(ValidMatrix,"\\matrix");
        syntax.addFunction(ValidPointer,"\\pointer");
        syntax.addFunction(ValidPointerIndex, "\\pointer_index");
        
        JavaVerCorsSyntax=syntax;
        
      }
      return JavaVerCorsSyntax;
    case JavaVeriFast:
      if (JavaVeriFastSyntax==null){
        JavaSyntax syntax=new JavaSyntax("Java + VeriFast",dialect);
        setCommon(syntax);
        syntax.addLeftFix(Star,"&*&",40);
        syntax.addPrefix(BindOutput,"?",666);
        syntax.addReserved(ASTReserved.FullPerm, "1");
        JavaVeriFastSyntax=syntax;  
      }
      return JavaVeriFastSyntax;
    default:
      throw new HREError("Java specification language dialect %s not supported",dialect);
    }
  }
  
  private static  void setCommon(Syntax syntax){
    syntax.addOperator(NewArray,-1,"new ","[","]");
    syntax.addOperator(Subscript,145,"","[","]"); // TODO: check if relative order to Select is OK!
    syntax.addOperator(Cast,145,"((",")",")");
    syntax.addOperator(Slice, 145, "[","","..","","]");

    // Java Operators  Precedence
    // 14 postfix  expr++ expr--
    syntax.addPostfix(PostIncr,"++",140);
    syntax.addPostfix(PostDecr,"--",140);
    // 13 unary   ++expr --expr +expr -expr ~ !
    syntax.addPrefix(BitNot, "~", 130);
    syntax.addPrefix(Not, "!", 130);
    syntax.addPrefix(UMinus, "-", 130);
    syntax.addPrefix(UPlus, "+", 130);
    syntax.addPrefix(PreIncr, "++", 130);
    syntax.addPrefix(PreDecr, "--", 130);
    // 12 multiplicative  * / %
    syntax.addLeftFix(Mult,"*",120);
    syntax.addLeftFix(FloorDiv,"/",120);
    syntax.addLeftFix(Div,"\\",120);
    syntax.addLeftFix(Mod,"%",120);
    // 11 additive  + -
    syntax.addLeftFix(Plus,"+",110);
    syntax.addLeftFix(Minus,"-",110);
    // 10 shift   << >> >>>
    syntax.addInfix(LeftShift,"<<", 100);
    syntax.addInfix(RightShift,">>", 100);
    syntax.addInfix(UnsignedRightShift,">>", 100);
    //  9 relational  < > <= >= instanceof
    syntax.addInfix(LT,"<",90);
    syntax.addInfix(LTE,"<=",90);
    syntax.addInfix(GT,">",90);
    syntax.addInfix(GTE,">=",90);
    syntax.addInfix(Instance,"instanceof",90);
    //  8 equality  == !=
    syntax.addInfix(EQ,"==",80);
    syntax.addInfix(NEQ,"!=",80);
    //  7 bitwise AND   &
    syntax.addInfix(BitAnd,"&",70);
    //  6 bitwise exclusive OR  ^
    syntax.addInfix(BitXor,"^",60);
    //  5 bitwise inclusive OR  |
    syntax.addInfix(BitOr,"|",50);
    //  4 logical AND   &&
    syntax.addLeftFix(And,"&&",40);
    //  3 logical OR  ||
    syntax.addLeftFix(Or,"||",30);
    //  2 ternary   ? :    
    syntax.addOperator(ITE,20,"","?",":","");
    //  1 assignment  = += -= *= /= %= &= ^= |= <<= >>= >>>=

    syntax.addRightFix(Assign,"=",10);
    syntax.addRightFix(AddAssign,"+=",10);
    syntax.addRightFix(SubAssign,"-=",10);
    syntax.addRightFix(MulAssign,"*= ",10);
    syntax.addRightFix(DivAssign,"/=",10);
    syntax.addRightFix(RemAssign,"%=",10);
    syntax.addRightFix(AndAssign,"&=",10);
    syntax.addRightFix(XorAssign,"^=",10);
    syntax.addRightFix(OrAssign,"|=",10);
    syntax.addRightFix(ShlAssign,"<<=",10);
    syntax.addRightFix(ShrAssign,">>=",10);
    syntax.addRightFix(SShrAssign,">>>=",10);
    
    syntax.addPrimitiveType(PrimitiveSort.Double,"double");
    syntax.addPrimitiveType(PrimitiveSort.Integer,"int");
    syntax.addPrimitiveType(PrimitiveSort.Long,"long");
    syntax.addPrimitiveType(PrimitiveSort.Void,"void");
    syntax.addPrimitiveType(PrimitiveSort.Boolean,"boolean");
    syntax.addPrimitiveType(PrimitiveSort.Char,"char");
    syntax.addPrimitiveType(PrimitiveSort.Byte,"byte");
    syntax.addPrimitiveType(PrimitiveSort.Float,"float");
    syntax.addPrimitiveType(PrimitiveSort.UInteger,"/*unsigned*/ int");
    syntax.addPrimitiveType(PrimitiveSort.ULong,"/*unsigned*/ long");
    syntax.addPrimitiveType(PrimitiveSort.UShort,"/*unsigned*/ short");
    syntax.addPrimitiveType(PrimitiveSort.Short,"short");
    syntax.addPrimitiveType(PrimitiveSort.Process,"process");
    syntax.addPrimitiveType(PrimitiveSort.String,"String");
    syntax.addPrimitiveType(PrimitiveSort.Rational, "rational");
    

    syntax.addReserved(Public,"public");
    syntax.addReserved(Private,"private");
    syntax.addReserved(Static,"static");
    syntax.addReserved(Volatile,"volatile");
    syntax.addReserved(Synchronized,"synchronized");
    syntax.addReserved(Protected,"protected");
    syntax.addReserved(Abstract,"abstract");
    syntax.addReserved(This,"this");
    syntax.addReserved(Null,"null");
    syntax.addReserved(Super,"super");
    syntax.addReserved(GetClass,"class");
    syntax.addReserved(Final,"final");
    syntax.addReserved(Any,"?");
    
    syntax.addReserved(Default,"default");
    syntax.add_annotation(ASTSpecial.Kind.Continue,"continue");
    syntax.add_annotation(ASTSpecial.Kind.Break,"break");
  }

  @Override
  public JavaPrinter print(TrackingOutput out, ASTNode n) {
    JavaPrinter p=new JavaPrinter(out,dialect);
    if (n!=null) {
      ASTNode nn=new Parenthesize(this).rewrite(n);
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

