package vct.main;

import java.io.File;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.rewrite.RewriteSystem;
import vct.col.util.AbstractTypeCheck;
import hre.config.Configuration;

public class RewriteSystems {

  static Map<File,ProgramUnit> systems=new ConcurrentHashMap<File,ProgramUnit>();
  
  public static RewriteSystem getRewriteSystem(String name){
    File f=new File(name+".jspec");
    if (!f.exists()){
      f=Configuration.getConfigFile(name + ".jspec");
    }
    ProgramUnit unit=systems.get(f);
    if (unit==null) synchronized(systems){
      unit=systems.get(f);
      if (unit==null){
        unit=Parsers.getParser("jspec").parse(f);
        new AbstractTypeCheck(null, unit).check();
        systems.put(f, unit);
      }
    }
    return new RewriteSystem(unit,name);
  }

}
