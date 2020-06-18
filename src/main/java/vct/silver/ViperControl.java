package vct.silver;

import java.util.HashSet;
import java.util.Hashtable;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import hre.ast.Origin;
import vct.logging.MessageFactory;
import hre.config.Configuration;
import viper.api.VerificationControl;

import static hre.lang.System.Progress;

public class ViperControl implements VerificationControl<Origin> {

  private final ScheduledExecutorService scheduler;

  private String old_task;
  private Origin old_origin;
  private int count;
  private String current_task;
  private Origin current_origin;
  
  private ScheduledFuture<?> future;
  private Runnable task=new Runnable(){
    public void run(){
      if (old_task==current_task && old_origin==current_origin && old_task!=null){
        int tmp=count+1;
        if (count>0 && (tmp&count)==0){
          Progress("Verifying %s at %s is taking %d+ ms",
            current_task,current_origin,count*Configuration.profiling.get());
        }
        count=tmp;
      } else {
        old_task=current_task;
        old_origin=current_origin;
        count=0;
      }
      //future.cancel(false);
    }
  };

  public HashSet<String> verified_methods=new HashSet<String>();
  public HashSet<String> failed_methods=new HashSet<String>();
  
  private Hashtable<Origin,String> origin2method=new Hashtable<Origin, String>();
  
  private MessageFactory report;
  
  public ViperControl(MessageFactory report){
    this.report=report;
    if (Configuration.profiling_option.used()){
      scheduler = Executors.newScheduledThreadPool(1);
      int N=Configuration.profiling.get();
      future=scheduler.scheduleAtFixedRate(task, N, N, TimeUnit.MILLISECONDS);
    } else {
      scheduler=null;
    }
  }
  
  @Override
  public boolean function(Origin origin, String name) {
    // TODO log this event
    return true;
  }

  @Override
  public boolean predicate(Origin origin, String name) {
    // TODO log this event
    return true;
  }

  @Override
  public boolean method(Origin origin, String name) {
    origin2method.put(origin, name);
    for(String suffix:Configuration.skip){
      if (name.endsWith(suffix)) return false;
    }
    // TODO log this event
    return true;
  }

  @Override
  public void pass(Origin origin) {
    report.result(true,origin);
    String m=origin2method.get(origin);
    if (m!=null){
      origin.report("result","pass");
      verified_methods.add(m);
    } else {
      hre.lang.System.Warning("failed to map origin %s to method",origin);
    }
  }

  @Override
  public void fail(Origin origin) {
    report.result(false,origin);
    String m=origin2method.get(origin);
    if (m!=null){
      origin.report("result","fail");
      failed_methods.add(m);
    } else {
      hre.lang.System.Warning("failed to map origin %s to method",origin);
    }
  }

  @Override
  public void progress(String fmt, Object... args) {
    Progress(fmt, args);
  }
  
  @Override
  public void profile(Origin o,String task){
    if (Configuration.profiling_option.used()){
      //hre.System.Progress("verifying %s at %s",task,o);
      current_origin=o;
      current_task=task;
    }
  }

  public void done(){
    if (scheduler !=null){
      future.cancel(false);
      scheduler.shutdown();
    }
  }
  
  public boolean detail(){
    return Configuration.detailed_errors.get();
  }
}
