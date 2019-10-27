package vct.antlr4.parser;

class TimeKeeper {
  private long time=System.currentTimeMillis();
  
  public long show(){
    long tmp=time;
    time=System.currentTimeMillis();
    return time-tmp; 
  }
}