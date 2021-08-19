// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package hre.ast;

import hre.config.Configuration;
import hre.lang.HREError;
import static hre.lang.System.*;

import java.nio.file.Path;
import java.util.Hashtable;

/**
 * Origin that denotes a range of characters in a File.
 * @author sccblom
 *
 */
public class FileOrigin extends Origin {


  public int linesBefore=2;
  public int linesAfter=2;

  private Path filePath;
  private int first_line, first_col, last_line, last_col;

  private void do_mark(String result) {
    Path filePath = getPath();
    FileContext fc = Configuration.currentConfiguration().fileContexts().get(filePath);
    if (fc == null) return;
    fc.mark(this, result);
  }

  public void printContext(int before,int after){
    Path filePath = getPath();
    FileContext fc = Configuration.currentConfiguration().fileContexts().get(filePath);
    if (fc == null){
      Output("=========================================");
      Output("error at %s: ", this);
    } else {
      Output("=== %-30s ===",filePath);
      fc.printContext(this, before, after);
      Output("-----------------------------------------");
    }
  }
  
  public static void add(Path filePath, boolean gui) {
    Configuration.currentConfiguration().fileContexts().put(filePath, new FileContext(filePath, gui));
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


  public FileOrigin(Path filePath, int first_line, int first_col, int last_line, int last_col){
    if (first_line <0) throw new HREError("bad first line : %d",first_line);
    this.filePath = filePath;
    this.first_line = first_line;
    this.first_col = first_col;
    this.last_line = last_line;
    this.last_col = last_col;
    if (filePath == null) throw new Error("null file name");
  }
    
  public String toString(){
    if (last_line>=0) {
      return String.format(
          "file %s from line %d column %d until line %d column %d",
              filePath,first_line, first_col, last_line, last_col
      );
    } else {
      return String.format(
          "file %s at line %d column %d",
              filePath,first_line, first_col
      );
    }

  }

  public FileOrigin merge(FileOrigin origin){
    return new FileOrigin(filePath,first_line,first_col,origin.last_line,origin.last_col);
  }

  /**
   * Merges this and origin into the combined area they represent.
   * ensures \return == FileOrigin(Math.min(this, origin), Math.max(this, origin))
   * @param origin
   * @return
   */
  public FileOrigin maximumMerge(FileOrigin origin) {
    assert filePath == origin.filePath;

    int new_first_line, new_first_col, new_last_line, new_last_col;

    if (first_line < origin.first_line) {
      new_first_line = first_line;
      new_first_col = first_col;
    } else if (first_line == origin.first_line) {
      new_first_line = first_line;
      new_first_col = Math.min(first_col, origin.first_col);
    } else {
      new_first_line = origin.first_line;
      new_first_col = origin.first_col;
    }

    if (last_line < origin.last_line) {
      new_last_line = origin.last_line;
      new_last_col = origin.last_col;
    } else if (last_line == origin.last_line) {
      new_last_line = last_line;
      new_last_col = Math.max(last_col, origin.last_col);
    } else {
      new_last_line = last_line;
      new_last_col = last_col;
    }

    return new FileOrigin(filePath, new_first_line, new_first_col, new_last_line, new_last_col);
  }

  public FileOrigin(Path filePath, int first_line, int first_col){
    if (first_line <0) throw new HREError("bad first line : %d",first_line);
    this.filePath = filePath;
    this.first_line = first_line;
    this.first_col = first_col;
    this.last_line = -1;
    this.last_col = -1;
    if (filePath == null) throw new Error("null file name");
  }
    
  public Path getPath(){
    return filePath;
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

