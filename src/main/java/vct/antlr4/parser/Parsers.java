package vct.antlr4.parser;

import hre.config.IntegerSetting;

import java.io.File;

import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.util.Parser;
import vct.silver.ColSilverParser;
import static hre.lang.System.Fail;
import static hre.lang.System.Progress;

public class Parsers {
  
  public static IntegerSetting java_version=new IntegerSetting(7);
  
  public static Parser getParser(String extension){
    switch(extension){
    case "cl":
    case "c":
    case "cu":
      return new ColCParser();
    case "i":return new ColIParser();
    case "java7":return new ColJavaParser(7,true);
    case "java8":return new ColJavaParser(8,true);
    case "java": return new ColJavaParser(java_version.get(),true);
    case "jspec": return new ColJavaParser(7,false);
    case "pvl":return new ColPVLParser();
    case "sil":return new ColSilverParser();
    }
    Fail("no parser for %s is known",extension);
    return null;
    
  }
  
  public static ProgramUnit parseFile(String name){
    int dot=name.lastIndexOf('.');
    if (dot<0) {
      Fail("cannot deduce language of %s",name);
    }
    String lang=name.substring(dot+1);
    Progress("Parsing %s file %s",lang,name);
    ProgramUnit unit=Parsers.getParser(lang).parse(new File(name));
    Progress("Read %s succesfully",name);
    return unit;
  }

}
