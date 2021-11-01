package vct.col.ast.syntax;


import hre.ast.TrackingOutput;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.print.PVLPrinter;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.Parenthesize;

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
      syntax.addPostfix(PostIncr,"++",140);
      syntax.addPostfix(PostDecr,"--",140);
      syntax.addOperator(NewArray,-1,"new ","[","]");
      syntax.addOperator(Subscript,145,"","[","]"); // TODO: check if relative order to Select is OK!
        syntax.addInfix(Implies,"==>",30);
      syntax.addLeftFix(Wand,"-*",30);
      syntax.addFunction(Perm,"Perm");
      syntax.addFunction(HistoryPerm,"HPerm");
      syntax.addFunction(Future,"Future");
      syntax.addFunction(Head,"head");
      syntax.addFunction(Tail,"tail");
      syntax.addFunction(Empty,"isEmpty");
      syntax.addFunction(RemoveAt, "removeAt");
      syntax.addFunction(SeqPermutation, "permutationOf");
        syntax.addFunction(Value,"Value");

      syntax.addFunction(PointsTo,"PointsTo");
      syntax.addFunction(IterationOwner,"\\owner");
      syntax.addFunction(Old,"\\old");

      syntax.addFunction(OptionSome, "Some");
      syntax.addFunction(OptionGet, "getOption");
      syntax.addFunction(OptionGetOrElse, "getOrElseOption");

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

        syntax.addInfix(LT,"<",90);
      syntax.addInfix(LTE,"<=",90);
      syntax.addInfix(GT,">",90);
      syntax.addInfix(GTE,">=",90);
      syntax.addInfix(EQ,"==",80);
      syntax.addInfix(NEQ,"!=",80);
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

        syntax.addPrimitiveType(PrimitiveSort.Integer,"int");
      syntax.addPrimitiveType(PrimitiveSort.ZFraction,"zfrac");
      syntax.addPrimitiveType(PrimitiveSort.Fraction,"frac");
      syntax.addPrimitiveType(PrimitiveSort.Void,"void");
      syntax.addPrimitiveType(PrimitiveSort.Resource,"resource");
      syntax.addPrimitiveType(PrimitiveSort.Boolean,"boolean");
      syntax.addPrimitiveType(PrimitiveSort.Process,"process");
      syntax.addPrimitiveType(PrimitiveSort.String,"string");
      syntax.addPrimitiveType(PrimitiveSort.Map,"map");

        syntax.addReserved(FullPerm,"write");
      syntax.addReserved(ReadPerm,"read");
      syntax.addReserved(NoPerm,"none");
      syntax.addReserved(EmptyProcess,"empty");
      syntax.addReserved(CurrentThread,"current_thread");
      syntax.addReserved(Null,"null");
      syntax.addReserved(This,"this");
      syntax.addReserved(Protected,"");
      
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
