package vct.col.ast.syntax;


import hre.ast.TrackingOutput;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.print.JavaPrinter;
import vct.col.ast.print.PVLPrinter;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.Parenthesize;

import java.io.PrintWriter;

import static vct.col.ast.expr.StandardOperator.*;
import static vct.col.ast.type.ASTReserved.*;

/**
 * Defines the syntax of common types and operations of  
 * the Program Verification Language (PVL).
 * 
 * @see Syntax
 * 
 */
public class PVLSyntax extends Syntax {

  private static PVLSyntax syntax;

  public PVLSyntax(String language) {
    super(language);
  }

  public static PVLSyntax get(){
    if(syntax==null){
      syntax=new PVLSyntax("PVL");

      VerCorsSyntax.add(syntax);

      //TODO revise the whole syntax to see what is missing.
      syntax.addReserved(Null,"null");
      syntax.addOperator(Subscript,145,"","[","]"); // TODO: check if relative order to Select is OK!
      syntax.addPostfix(PostIncr,"++",140);
      syntax.addPostfix(PostDecr,"--",140);
      syntax.addOperator(NewArray,-1,"new ","[","]");
      syntax.addOperator(IndependentOf, -1 , "(" ,"!",")");

      syntax.addLeftFix(Exp,"^^",125);
      syntax.addLeftFix(StructSelect,".",-1);
      syntax.addLeftFix(LeftMerge,"||_",30);

      syntax.addOperator(Member,-1,"(","\\memberof",")");
      syntax.addFunction(TypeOf,"\\typeof");
      syntax.addFunction(CurrentPerm,"perm");
      syntax.addFunction(HistoryPerm,"HPerm");
      syntax.addOperator(Scale,130,"[","]","");
      syntax.addFunction(History,"Hist");
      syntax.addFunction(Future,"Future");
      syntax.addFunction(AbstractState,"AbstractState");
      syntax.addFunction(Contribution,"Contribution");
      syntax.addFunction(Held,"held");
      syntax.addFunction(SizeOf,"\\sizeof");
      syntax.addFunction(AddrOf,"\\addrof");
      syntax.addFunction(Indirection,"\\indirect");
      syntax.addFunction(StructDeref,"\\structderef");
      syntax.addFunction(IterationOwner,"\\owner");
      syntax.addFunction(RemoveAt, "remove");
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

      syntax.addFunction(MapBuild, "buildMap");
      syntax.addFunction(MapEquality, "equalsMap");
      syntax.addFunction(MapDisjoint, "disjointMap");
      syntax.addFunction(MapKeySet, "keysMap");
      syntax.addFunction(MapCardinality, "cardMap");
      syntax.addFunction(MapValueSet, "valuesMap");
      syntax.addFunction(MapGetByKey, "getFromMap");
      syntax.addFunction(MapRemoveKey, "removeFromMap");
      syntax.addFunction(MapItemSet, "itemsMap");




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
      syntax.addFunction(Empty,"isEmpty");
      syntax.addFunction(RemoveAt, "removeAt");
      //syntax.addFunction(Head,"head");
      //syntax.addFunction(Tail,"tail");
      syntax.addFunction(Value,"Value");

      syntax.addFunction(PointsTo,"PointsTo");
      syntax.addFunction(IterationOwner,"\\owner");
      //syntax.addFunction(ArrayPerm,"ArrayPerm");
      syntax.addFunction(Old,"\\old");

      syntax.addFunction(OptionSome, "Some");
                                                                 
      syntax.addFunction(MapBuild, "buildMap");           
      syntax.addFunction(MapEquality, "equalsMap");       
      syntax.addFunction(MapDisjoint, "disjointMap");     
      syntax.addFunction(MapKeySet, "keysMap");           
      syntax.addFunction(MapCardinality, "cardMap");      
      syntax.addFunction(MapValueSet, "valuesMap");       
      syntax.addFunction(MapGetByKey, "getFromMap");      
      syntax.addFunction(MapRemoveKey, "removeFromMap"); 
      syntax.addFunction(MapItemSet, "itemsMap");


      syntax.addFunction(TupleFst, "getFst");
      syntax.addFunction(TupleSnd, "getSnd");


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
      syntax.addPrimitiveType(PrimitiveSort.Map,"map");
      
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

  @Override
  public PVLPrinter print(TrackingOutput out, ASTNode n) {
    PVLPrinter p=new PVLPrinter(out);
    if (n!=null) {
      ASTNode nn=new Parenthesize(this).rewrite(n);
      nn.accept(p);
    }
    return p;
  }
}
