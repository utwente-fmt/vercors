package vct.parsers;

import static hre.lang.System.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;

import hre.config.Configuration;
import vct.col.ast.stmt.decl.ProgramUnit;

/**
 * Parse specified code and convert the contents to COL. 
 */
public class ColCParser extends ColIParser {
  
  
  
  @Override
  public ProgramUnit parse(File file) {
    String file_name=file.toString();
       try {
        Runtime runtime=Runtime.getRuntime();
                    	
      	String command=Configuration.cpp_command.get();
      	command+=" -nostdinc -nocudainc -nocudalib --cuda-host-only -isystem " + Configuration.getCIncludePath().getAbsolutePath();
        for(String p:Configuration.cpp_include_path){
          command+=" -I"+p;
        }
        for(String p:Configuration.cpp_defines){
          command+=" -D"+p;
        }
      	command+=" "+file_name;
      	
      	Progress("pre-processing command line: %s",command);
      	final Process process=runtime.exec(command);
        Thread t=new Thread(){
          public void run(){
            BufferedReader err=new BufferedReader(new InputStreamReader(process.getErrorStream()));
            boolean err_found=false;
            String s;
            try {
              while((s=err.readLine())!=null){
                Output("%s", s);
                if (s.matches(".*error.*")) err_found=true;
              }
            } catch (IOException e) {
              DebugException(e);
              err_found=true;
            }
            if (err_found){
              System.exit(1);
            }
          }
        };
        t.setDaemon(true);
        t.start();
      	return parse(file_name,process.getInputStream());
      } catch (FileNotFoundException e) {
        Fail("File %s has not been found",file_name);
      } catch (Exception e) {
        DebugException(e);
        Abort("Exception %s while parsing %s",e.getClass(),file_name);
      }
    return null;
  }

}

