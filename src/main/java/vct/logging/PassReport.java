package vct.logging;

import java.util.ArrayList;
import java.util.HashSet;
import vct.col.ast.stmt.decl.ProgramUnit;

import static hre.lang.System.Output;

public class PassReport {
  
  private ArrayList<Message> entries=new ArrayList<Message>();
  
  public void add(Message m){
    entries.add(m);
    if (m.isFatal()) fatal++;
    for(MessageVisitor v:visitors){
      m.accept(v);
    }
  }
 
  private ProgramUnit input;
  private ProgramUnit output;
  
  private int fatal=0;
  
  public PassReport(ProgramUnit in){
    input=in;
  }
  
  public ProgramUnit setOutput(ProgramUnit pu){
    return output=pu;
  }
  
  public ProgramUnit getOutput(){
    return output;
  }
  
  public ProgramUnit getInput(){
    return input;
  }
  
  public int getFatal(){
    return fatal;
  }
  
  public void fatal(String format,Object ... args){
    Output(format, args);
    fatal++;
  }
  
  private HashSet<MessageVisitor> visitors=new HashSet<MessageVisitor>();
  
  public void add(MessageVisitor visitor){
    visitors.add(visitor);
  }
  public void remove(MessageVisitor visitor){
    visitors.remove(visitor);
  }

}
