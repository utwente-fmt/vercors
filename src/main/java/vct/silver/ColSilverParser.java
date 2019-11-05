package vct.silver;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.lang.HREError;

import java.io.File;

import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.error.VerificationError;
import viper.api.ViperAPI;

public class ColSilverParser implements vct.col.util.Parser {

  @Override
  public ProgramUnit parse(File file) {
    return run_test(file);
  }
  
  public static <T,E,S,Decl,DFunc,DAxiom,Program>
  ProgramUnit run_test(File f){
    ViperAPI<Origin,VerificationError,T,E,S,DFunc,DAxiom,Program> viper=
        SilverBackend.getVerifier("parser");
    Program program=viper.prog.parse_program(f.toString());
    if (program==null){
      throw new HREError("parsing %s failed",f);
    }
    VerCorsViperAPI vercors=VerCorsViperAPI.get();
    ProgramUnit tmp=viper.prog.convert(vercors, program);
    ProgramUnit res=new ProgramUnit();
    ASTClass ref=new ASTClass("Ref",ASTClass.ClassKind.Record);
    ref.setOrigin(new MessageOrigin("implicit Ref for %s",f));
    res.add(ref);
    for(ASTNode d:tmp){
      if (d instanceof DeclarationStatement){
        ref.add_dynamic(d);
      } else {
        res.add(d);
      }
    }
    return res;
  }
  
}
