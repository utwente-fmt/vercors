// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package hre.ast;

import hre.lang.HREError;
import static hre.lang.System.*;

import java.io.PrintStream;
import java.util.Hashtable;

/**
 * Origin that denotes a range of characters in a File.
 * @author sccblom
 *
 */
public class FileOrigin extends Origin {

  public int linesBefore=2;
  public int linesAfter=2;
  

  private void do_mark(String result) {
    String file=getName();
    FileContext fc=data.get(file);
    if (fc==null) return;
    fc.mark(this,result);
  }

  private static Hashtable<String,FileContext> data=new Hashtable<String,FileContext>();
  
  public void printContext(int before,int after){
    String file=getName();
    FileContext fc=data.get(file);
    if (fc==null){
      Output("=========================================");
      Output("error at %s: ",this);
    } else {
      Output("=== %-30s ===",file);
      fc.printContext(this,before,after);
      Output("-----------------------------------------");
    }
  }
  
  public static void add(String file,boolean gui){
    data.put(file,new FileContext(file,gui));
  }
  public synchronized void report(String level, Iterable<String> message) {
    printContext(linesBefore,linesAfter);
    for(String line:message){
      Output("  %s",line);
    }
    Output("=========================================");
  }
  
  public synchronized void report(String level, String ... message) {
    if (level.equals("mark")){
      do_mark(message[0]);
      return;
    }
    if (level.equals("result")){
      switch(message[0]){
      case "pass":
        do_mark("green");
        break;
      case "fail":
        do_mark("red");
        break;
      }
      return;
    }
    printContext(linesBefore,linesAfter);
    for(String line:message){
      Output("  %s",line);
    }
    Output("=========================================");
  }

    private String file_name;
    private int first_line, first_col, last_line, last_col;
    public FileOrigin(String file_name,int first_line, int first_col, int last_line, int last_col){
        if (first_line <0) throw new HREError("bad first line : %d",first_line);
        this.file_name=file_name;
        this.first_line=first_line;
        this.first_col=first_col;
        this.last_line=last_line;
        this.last_col=last_col;
        if (file_name==null) throw new Error("null file name");
    }
    
    public String toString(){
      if (last_line>=0) {
        return String.format(
            "file %s from line %d column %d until line %d column %d",
            file_name,first_line, first_col, last_line, last_col
        );
      } else {
        return String.format(
            "file %s at line %d column %d",
            file_name,first_line, first_col
        );       
      }
     
    }

    public FileOrigin merge(FileOrigin origin){
      return new FileOrigin(file_name,first_line,first_col,origin.last_line,origin.last_col);
    }

    public FileOrigin(String file_name,int first_line, int first_col){
      if (first_line <0) throw new HREError("bad first line : %d",first_line);
      this.file_name=file_name;
      this.first_line=first_line;
      this.first_col=first_col;
      this.last_line=-1;
      this.last_col=-1;
      if (file_name==null) throw new Error("null file name");      
    }
    
    public String getName(){
      return file_name;
    }
    
    public int getFirstLine(){
      return first_line;
    }
    public int getFirstColumn(){
      return first_col;
    }
    public int getLastLine(){
      return last_line;
    }
    public int getLastColumn(){
      return last_col;
    }
}

