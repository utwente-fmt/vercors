package vct.main;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import static hre.lang.System.Output;
import static hre.lang.System.Warning;

public class TestcaseVisitor extends SimpleFileVisitor<Path>{
  
  public final HashMap<String,Set<Path>> files_by_name=new HashMap<String,Set<Path>>();
  
  public final HashMap<String,Testcase> testsuite=new HashMap<String, Testcase>();
  
  public HashSet<Path> unmarked=new HashSet<Path>();
  
  public boolean delayed_fail=false;
  
  public static String extension(Path path){
    Path file=path.getFileName();
    String name=file.toString();
    int dot=name.lastIndexOf('.');
    if (dot<0) {
      return "";
    } else {
      return name.substring(dot+1);
    }
  }

  @Override
  public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
      throws IOException
  {
      String name=file.getFileName().toString().toLowerCase();
      String ext=extension(file);
      switch(ext){
      case "c":
      case "java":
      case "pvl":
      case "sil":
        {
          Set<Path> set=files_by_name.get(name);
          if (set==null){
            set=new HashSet<Path>();
            files_by_name.put(name,set);
          }
          set.add(file);          
          BufferedReader is=new BufferedReader(new InputStreamReader(new FileInputStream(file.toFile())));
          String line;
          HashSet<String> cases=new HashSet<String>();
          while((line=is.readLine())!=null){
            line=line.trim();
            if (line.startsWith("//::")){
              String cmds[]=line.substring(4).trim().split("[ ]+");
              switch(cmds[0]){
              case "case":
              case "cases":
                cases.clear();
                for(int i=1;i<cmds.length;i++) {
                  cases.add(cmds[i]);
                  Testcase test=testsuite.get(cmds[i]);
                  if (test==null){
                    test=new Testcase();
                    testsuite.put(cmds[i],test);
                  }
                  test.files.add(file);
                }
                break;
              case "tool":
              case "tools":
                for(String test:cases){
                  Testcase tc=testsuite.get(test);
                  if (!tc.tools.isEmpty()){
                    Output("%s: tools for test %s already set.",file,test);
                    delayed_fail=true;
                  }
                  for(int i=1;i<cmds.length;i++) {  
                    tc.tools.add(cmds[i]);
                  }
                }
                break;
              case "verdict":
                for(String test:cases){
                  Testcase tc=testsuite.get(test);
                  tc.verdict=cmds[1];
                }                
                break;
              case "suites":
              case "suite":
                for(String test:cases){
                  Testcase tc=testsuite.get(test);
                  for(int i=1;i<cmds.length;i++) {  
                    tc.suites.add(cmds[i]);
                  }
                }                
                break;
              case "option":
              case "options":
                for(int i=1;i<cmds.length;i++) {
                  for(String test:cases){
                    Testcase tc=testsuite.get(test);
                    tc.options.add(cmds[i]);
                  }
                }
                break;
              case "pass":
                for(int i=1;i<cmds.length;i++) {
                  for(String test:cases){
                    Testcase tc=testsuite.get(test);
                    tc.pass_methods.add(cmds[i]);
                  }                  
                }
                break;
              case "fail":
                for(int i=1;i<cmds.length;i++) {
                  for(String test:cases){
                    Testcase tc=testsuite.get(test);
                    tc.fail_methods.add(cmds[i]);
                    tc.verdict="Fail";
                  }                  
                }
                break;
              default:
                Warning("ignoring %s in %s: %s",cmds[0],file,line);
              }
            } else {
              continue;
            }
          }
          is.close();
          if (cases.isEmpty()){
            unmarked.add(file);
          }
        }                       
      }
      return FileVisitResult.CONTINUE;
  }
  @Override
  public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
      throws IOException
  {
          return FileVisitResult.CONTINUE;
  }
  @Override
  public FileVisitResult postVisitDirectory(Path dir, IOException e)
      throws IOException
  {
      if (e == null) {
          return FileVisitResult.CONTINUE;
      } else {
          // directory iteration failed
          throw e;
      }
  }


}
