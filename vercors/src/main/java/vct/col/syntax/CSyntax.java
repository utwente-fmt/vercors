package vct.col.syntax;

import hre.ast.TrackingOutput;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.print.CPrinter;
import static vct.col.ast.expr.StandardOperator.*;

public class CSyntax extends Syntax{
  private static Syntax c_syntax;
  private static Syntax cml_syntax;
  
  public CSyntax(String dialect) {
    super(dialect);
  }

  public static void setCommon(Syntax syntax){
    syntax.addOperator(Subscript,160,"","[","]");
    syntax.addLeftFix(StructSelect,".",160);
    syntax.addLeftFix(StructDeref,"->",160);
    
    syntax.addPostfix(PostIncr, "++", 160);
    syntax.addPostfix(PostDecr, "--", 160);
    syntax.addPrefix(PreIncr, "++", 160);
    syntax.addPrefix(PreDecr, "--", 160);
    
    syntax.addPrefix(UMinus, "-", 150);
    syntax.addPrefix(UPlus, "+", 150);
    syntax.addPrefix(Not,"!",150);
    syntax.addPrefix(AddrOf,"&",150);
    syntax.addPrefix(Indirection,"*",150);

    syntax.addLeftFix(Mult,"*",130);
    syntax.addLeftFix(FloorDiv,"/",130);
    syntax.addLeftFix(Div,"f/",130);
    syntax.addLeftFix(Mod,"%",130);
    
    syntax.addLeftFix(Plus,"+",120);
    syntax.addLeftFix(Minus,"-",120);
    
    syntax.addInfix(LeftShift,"<<", 110);
    syntax.addInfix(RightShift,">>", 110);

    syntax.addInfix(LT,"<",100);
    syntax.addInfix(LTE,"<=",100);
    syntax.addInfix(GT,">",100);
    syntax.addInfix(GTE,">=",100);
    
    syntax.addInfix(EQ,"==",90);
    syntax.addInfix(NEQ,"!=",90);
    
    syntax.addInfix(BitAnd,"&",80);
    
    syntax.addInfix(BitXor,"^",70);

    syntax.addInfix(BitOr,"|",60);

    syntax.addLeftFix(And, "&&", 50);
    
    syntax.addLeftFix(Or, "||", 40);
    
    syntax.addOperator(ITE,30,"","?",":","");
    syntax.addRightFix(Assign,"=",30);
    syntax.addRightFix(AddAssign,"+=",30);
    syntax.addRightFix(SubAssign,"-=",30);
    syntax.addRightFix(MulAssign,"*=",30);
    syntax.addRightFix(DivAssign,"/=",30);
    syntax.addRightFix(RemAssign,"%=",30);
    syntax.addRightFix(AndAssign,"&=",30);
    syntax.addRightFix(XorAssign,"^=",30);
    syntax.addRightFix(OrAssign,"|=",30);
    syntax.addRightFix(ShlAssign,"<<=",30);
    syntax.addRightFix(ShrAssign,">>=",30);

    syntax.addFunction(SizeOf, "sizeof");
    syntax.addFunction(ValidPointer, "\\pointer");
    syntax.addFunction(ValidPointerIndex, "\\pointer_index");
    syntax.addFunction(Values, "\\values");
    
    syntax.addPrimitiveType(PrimitiveSort.Double,"double");
    syntax.addPrimitiveType(PrimitiveSort.Integer,"int");
    //syntax.addPrimitiveType(Fraction,"frac");
    syntax.addPrimitiveType(PrimitiveSort.Long,"long");
    syntax.addPrimitiveType(PrimitiveSort.Void,"void");
    //syntax.addPrimitiveType(Resource,"resource");
    syntax.addPrimitiveType(PrimitiveSort.Boolean,"bool");
    //syntax.addPrimitiveType(Class,"classtype");
    syntax.addPrimitiveType(PrimitiveSort.Char,"char");
    syntax.addPrimitiveType(PrimitiveSort.Float,"float");
    
    syntax.addReserved(ASTReserved.Null, "NULL");
    
  }

  public static Syntax getC() {
    if (c_syntax==null){
      c_syntax=new CSyntax("C");
      setCommon(c_syntax);
    }
    return c_syntax;
  }
  
  public static Syntax getCML() {
    if (cml_syntax==null){
      cml_syntax=new CSyntax("C + CML");
      setCommon(cml_syntax);
      VerCorsSyntax.add(cml_syntax);
    }
    return cml_syntax;
  }

  @Override
  public CPrinter print(TrackingOutput out, ASTNode n) {
    CPrinter p=new CPrinter(out);
    if (n!=null) n.accept(p);
    return p;
  } 

}

/*

17  ::  Scope resolution  Left-to-right
16  ++   --   Suffix/postfix increment and decrement
    ()  Function call
    []  Array subscripting
    .   Element selection by reference
    −>  Element selection through pointer
15  ++   --   Prefix increment and decrement  Right-to-left
    +   −   Unary plus and minus
    !   ~   Logical NOT and bitwise NOT
    (type)  Type cast
    *   Indirection (dereference)
    &   Address-of
    sizeof  Size-of
    new, new[]  Dynamic memory allocation
    delete, delete[]  Dynamic memory deallocation
14  .*   ->*  Pointer to member   Left-to-right
13   *   /   %   Multiplication, division, and remainder
12   +   −   Addition and subtraction
11   <<   >>   Bitwise left shift and right shift
10   <   <=  For relational operators < and ≤ respectively
    >   >=  For relational operators > and ≥ respectively
 9  ==   !=   For relational = and ≠ respectively
 8  &   Bitwise AND
 7  ^   Bitwise XOR (exclusive or)
 6  |   Bitwise OR (inclusive or)
 5  &&  Logical AND
 4  ||  Logical OR
 3  ?:  Ternary conditional   Right-to-left
    =   Direct assignment (provided by default for C++ classes)
    +=   −=   Assignment by sum and difference
   *=   /=   %=  Assignment by product, quotient, and remainder
    <<=   >>=   Assignment by bitwise left shift and right shift
    &=   ^=   |=  Assignment by bitwise AND, XOR, and OR
 2 throw   Throw operator (for exceptions)
 1 ,   Comma   Left-to-right 

*/
