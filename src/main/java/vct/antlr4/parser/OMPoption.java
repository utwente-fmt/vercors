package vct.antlr4.parser;

public class OMPoption implements OMPelement {

  public enum Kind { Private, NoWait, Schedule, Simd, SimdLen }
  
  public enum Schedule { Static, Dynamic }
  
  public final Kind kind;
  
  public final Schedule schedule;
  
  public final int len;
  
  public OMPoption(Kind kind,int len){
    this.kind=kind;
    this.len=len;
    this.schedule=null;
  }
  
  public OMPoption(Kind kind){
    this.kind=kind;
    this.schedule=null;
    this.len=-1;
  }
  
  public OMPoption(Kind kind,Schedule schedule){
    this.kind=kind;
    this.schedule=schedule;
    this.len=-1;
  }

  public static boolean nowait(OMPoption[] options) {
    for(OMPoption o:options){
      if (o==null) continue;
      if (o.kind==Kind.NoWait){
        return true;
      }
    }
    return false;
  }

  public static boolean static_schedule(OMPoption[] options) {
    for(OMPoption o:options){
      if (o==null) continue;
      if (o.kind==Kind.Schedule){
        return o.schedule==Schedule.Static;
      }
    }
    return false;
  }

}
