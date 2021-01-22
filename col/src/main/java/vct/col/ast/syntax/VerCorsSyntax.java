package vct.col.ast.syntax;

import vct.col.ast.type.ASTReserved;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.PrimitiveSort;

import static vct.col.ast.expr.StandardOperator.*;
import static vct.col.ast.type.ASTReserved.*;

public class VerCorsSyntax {

  public static void add(Syntax syntax) {
    //TODO: find out what the proper priorities are!
    syntax.addInfix(SubType,"<:",9);
    syntax.addInfix(SuperType,":>",9);
    syntax.addInfix(Implies,"==>",3);
    syntax.addInfix(IFF,"<==>",3);
    syntax.addLeftFix(Wand,"-*",3);
    syntax.addFunction(Perm,"Perm");
    syntax.addFunction(HistoryPerm,"HPerm");
    syntax.addFunction(ActionPerm,"APerm");
    syntax.addFunction(Contribution,"Contribution");
    syntax.addFunction(Head,"head");
    syntax.addFunction(Tail,"tail");
    syntax.addFunction(Value,"Value");
    syntax.addFunction(PointsTo,"PointsTo");
    syntax.addFunction(ArrayPerm,"ArrayPerm");
    syntax.addFunction(AddsTo,"AddsTo");
    syntax.addFunction(Old,"\\old");
    syntax.addFunction(Length,"\\length");
    syntax.addFunction(Get,"get?");
    syntax.addFunction(Set,"set!");
    syntax.addOperator(Size,999,"|","|");
    syntax.addOperator(RangeSeq,-1,"{","..","}");
    syntax.addLeftFix(Concat,"+++",5);
    syntax.addLeftFix(Star,"**",4);
    
    syntax.addPrimitiveType(PrimitiveSort.ZFraction,"zfrac");
    syntax.addPrimitiveType(PrimitiveSort.Fraction,"frac");
    syntax.addPrimitiveType(PrimitiveSort.Resource,"resource");
    syntax.addPrimitiveType(PrimitiveSort.Class,"classtype");
    syntax.addPrimitiveType(PrimitiveSort.Location,"loc");
    
    syntax.addReserved(Result,"\\result");
    syntax.addReserved(GlobalThreadId, "\\gtid");
    syntax.addReserved(LocalThreadId, "\\ltid");
    syntax.addReserved(Pure,"pure");
    syntax.addReserved(Any,"*");
    syntax.addPrefix(BindOutput,"?",666);
    
    syntax.addReserved(FullPerm,"write");
    syntax.addReserved(ReadPerm,"read");
    syntax.addReserved(NoPerm,"none");
    syntax.addReserved(ASTReserved.OptionNone,"None");
    syntax.addFunction(StandardOperator.OptionSome,"Some");

    syntax.addFunction(StandardOperator.Future,"Future");
    syntax.addFunction(StandardOperator.History,"Hist");
    syntax.addFunction(StandardOperator.AbstractState,"AbstractState");

    
    
    syntax.add_annotation(ASTSpecial.Kind.Inhale, "inhale");
    syntax.add_annotation(ASTSpecial.Kind.Exhale, "exhale");
    
    
    syntax.add_annotation(ASTSpecial.Kind.ActionHeader, "action");
    syntax.add_annotation(ASTSpecial.Kind.ChooseHistory, "choose");
    syntax.add_annotation(ASTSpecial.Kind.CreateHistory, "create");
    syntax.add_annotation(ASTSpecial.Kind.CreateFuture, "create");
    syntax.add_annotation(ASTSpecial.Kind.SplitHistory, "split");
    syntax.add_annotation(ASTSpecial.Kind.MergeHistory, "merge");
    syntax.add_annotation(ASTSpecial.Kind.DestroyHistory, "destroy");
    syntax.add_annotation(ASTSpecial.Kind.DestroyFuture, "destroy");
    
    syntax.add_annotation(ASTSpecial.Kind.Fold, "fold");
    syntax.add_annotation(ASTSpecial.Kind.Unfold, "unfold");
    syntax.add_annotation(ASTSpecial.Kind.Open, "open");
    syntax.add_annotation(ASTSpecial.Kind.Close, "close");
    
    syntax.add_annotation(ASTSpecial.Kind.Assert, "assert");
    syntax.add_annotation(ASTSpecial.Kind.Refute, "refute");
    syntax.add_annotation(ASTSpecial.Kind.Assume, "assume");
    
    syntax.add_annotation(ASTSpecial.Kind.Invariant, "loop_invariant");
    syntax.add_annotation(ASTSpecial.Kind.Invariant, "context_everywhere");
    syntax.add_annotation(ASTSpecial.Kind.RequiresAndEnsures, "context");
    syntax.add_annotation(ASTSpecial.Kind.Label, "label");
    syntax.add_annotation(ASTSpecial.Kind.CSLSubject, "csl_subject");
    
    syntax.addFunction(StandardOperator.ValidArray,"\\array");
    syntax.addFunction(StandardOperator.ValidMatrix,"\\matrix");
    
    syntax.addFunction(StandardOperator.FoldPlus, "\\sum");
    syntax.addFunction(StandardOperator.VectorRepeat, "\\vrep");
    syntax.addFunction(StandardOperator.VectorCompare, "\\vcmp");
    
    syntax.addFunction(StandardOperator.MatrixSum, "\\msum");
    syntax.addFunction(StandardOperator.MatrixRepeat, "\\mrep");
    syntax.addFunction(StandardOperator.MatrixCompare, "\\mcmp");
  }

}
