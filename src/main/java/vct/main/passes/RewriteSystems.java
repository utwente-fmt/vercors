package vct.main.passes;

import java.io.File;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import hre.util.FileHelper;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.rewrite.JavaResolver;
import vct.col.rewrite.RewriteSystem;
import vct.col.util.AbstractTypeCheck;

public class RewriteSystems {

  static Map<File,ProgramUnit> systems= new ConcurrentHashMap<>();
  
  public static RewriteSystem getRewriteSystem(String name){
    File f=new File(name+".jspec");
    if (!f.exists()){
      f= FileHelper.getConfigFile(name + ".jspec");
    }
    ProgramUnit unit=systems.get(f);
    if (unit==null) synchronized(systems){
      unit=systems.get(f);
      if (unit==null){
        unit= Parsers.getParser("jspec").parse(f);
        unit=new JavaResolver(unit).rewriteAll();
        new AbstractTypeCheck(null, unit).check();
        systems.put(f, unit);
      }
    }
    return new RewriteSystem(unit,name);
  }

}
