package vct.col.syntax;


import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;

import static vct.col.ast.expr.StandardOperator.*;
import static vct.col.ast.type.ASTReserved.FullPerm;
import static vct.col.ast.type.ASTReserved.NoPerm;
import static vct.col.ast.type.ASTReserved.ReadPerm;
import static vct.col.ast.type.ASTReserved.EmptyProcess;
import static vct.col.ast.type.ASTReserved.CurrentThread;

/**
 * Defines the syntax of common types and operations of  
 * the Program Verification Language (PVL).
 * 
 * @see Syntax
 * 
 */
public class PVLSyntax {

  private static Syntax syntax;
  
  public static Syntax get(){
    if(syntax==null){
      syntax=new Syntax("PVL");

      VerCorsSyntax.add(syntax);
      
      //syntax.addInfix(SubType,"<:",90);
      //syntax.addInfix(SuperType,":>",90);
      syntax.addInfix(Implies,"==>",30);
      //syntax.addInfix(IFF,"<==>",30);
      syntax.addLeftFix(Wand,"-*",30);
      syntax.addFunction(Perm,"Perm");
      syntax.addFunction(HistoryPerm,"HPerm");
      syntax.addFunction(Future,"Future");
      syntax.addFunction(Head,"head");
      syntax.addFunction(Tail,"tail");
      syntax.addFunction(Empty,"empty");
      syntax.addFunction(RemoveAt, "remove");
      syntax.addFunction(RangeFromSeq, "range");
      syntax.addFunction(Take, "take");
      syntax.addFunction(Drop, "drop");
      //syntax.addFunction(Head,"head");
      //syntax.addFunction(Tail,"tail");
      syntax.addFunction(Value,"Value");

      syntax.addFunction(PointsTo,"PointsTo");
      syntax.addFunction(Identity,"id");
      syntax.addFunction(IterationOwner,"\\owner");
      //syntax.addFunction(ArrayPerm,"ArrayPerm");
      syntax.addFunction(Old,"\\old");

      syntax.addFunction(OptionSome, "Some");

      syntax.addOperator(Size,-1,"|","|");
      syntax.addOperator(Member,45,"","in","");
      syntax.addOperator(Slice, 10, "[","","..","","]");

      syntax.addPrefix(Not, "!", 130);
      syntax.addPrefix(UMinus, "-", 130);
 
      syntax.addLeftFix(Exp,"^^",125);
      // 12 multiplicative  * / %
      syntax.addLeftFix(Mult,"*",120);
      syntax.addLeftFix(FloorDiv,"/",120);
      syntax.addLeftFix(Div, "\\", 120);
      syntax.addLeftFix(Mod,"%",120);
      // 11 additive  + -
      syntax.addRightFix(PrependSingle, "::", 110);
      syntax.addLeftFix(AppendSingle, "++", 110);
      syntax.addLeftFix(Plus,"+",110);
      syntax.addLeftFix(Minus,"-",110);

/*
      // 10 shift   << >> >>>
      syntax.addInfix(LeftShift,"<<", 100);
      syntax.addInfix(RightShift,">>", 100);
      syntax.addInfix(UnsignedRightShift,">>", 100);
      //  9 relational  < > <= >= instanceof
       */
      syntax.addInfix(LT,"<",90);
      syntax.addInfix(LTE,"<=",90);
      syntax.addInfix(GT,">",90);
      syntax.addInfix(GTE,">=",90);
      /*
      syntax.addInfix(Instance," instanceof ",90);
      //  8 equality  == !=
       * */
      syntax.addInfix(EQ,"==",80);
      syntax.addInfix(NEQ,"!=",80);
      /*
      //  7 bitwise AND   &
      syntax.addInfix(BitAnd,"&",70);
      //  6 bitwise exclusive OR  ^
      syntax.addInfix(BitXor,"^",60);
      //  5 bitwise inclusive OR  |
      syntax.addInfix(BitOr,"|",50);
      //  4 logical AND   &&
       */
      syntax.addLeftFix(And,"&&",40);
      syntax.addLeftFix(Star,"**",40);
      //  3 logical OR  ||
      syntax.addLeftFix(Or,"||",30);
      //  2 ternary   ? :
      syntax.addInfix(Implies,"==>",30);
      syntax.addOperator(ITE,20,"","?",":","");
      syntax.addPrefix(BindOutput,"?",666);
      /*
      //  1 assignment  = += -= *= /= %= &= ^= |= <<= >>= >>>=
       */
      syntax.addRightFix(Assign,"=",10);

      /*
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
      
      syntax.addPrimitiveType(Double,"double");
      */
      syntax.addPrimitiveType(PrimitiveSort.Integer,"int");
      syntax.addPrimitiveType(PrimitiveSort.ZFraction,"zfrac");
      syntax.addPrimitiveType(PrimitiveSort.Fraction,"frac");
      //syntax.addPrimitiveType(Long,"long");
      syntax.addPrimitiveType(PrimitiveSort.Void,"void");
      syntax.addPrimitiveType(PrimitiveSort.Resource,"resource");
      syntax.addPrimitiveType(PrimitiveSort.Boolean,"boolean");
      syntax.addPrimitiveType(PrimitiveSort.Process,"process");
      syntax.addPrimitiveType(PrimitiveSort.String,"string");
      
      /*
      syntax.addPrimitiveType(Class,"classtype");
      syntax.addPrimitiveType(Char,"char");
      syntax.addPrimitiveType(Float,"float");
      */
      //syntax.addPrimitiveType(UInteger,"/*unsigned*/ int");
      //syntax.addPrimitiveType(ULong,"/*unsigned*/ long");
      //syntax.addPrimitiveType(UShort,"/*unsigned*/ short");
      //syntax.addPrimitiveType(Short,"short");
      
      syntax.addReserved(FullPerm,"write");
      syntax.addReserved(ReadPerm,"read");
      syntax.addReserved(NoPerm,"none");
      syntax.addReserved(EmptyProcess,"empty");
      syntax.addReserved(CurrentThread,"current_thread");
      
      syntax.addOperator(Unfolding,140,"unfolding","in","");
      
      syntax.addFunction(PVLidleToken,"idle");
      syntax.addFunction(PVLjoinToken,"running");
      syntax.addFunction(Held, "held");

      syntax.addReserved(ASTReserved.Any, "*");

      syntax.addFunction(ValidArray,"\\array");
      syntax.addFunction(ValidMatrix,"\\matrix");
    }
    return syntax;
  }

}
