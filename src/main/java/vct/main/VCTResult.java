package vct.main;

import hre.io.Message;
import hre.util.TestReport.Verdict;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;

class VCTResult {
  public Verdict verdict=null;
  public final List<Message> log=new ArrayList<Message>();
  public final LinkedHashMap<String,Integer> times=new LinkedHashMap<String, Integer>();
  public void mustSay(String string) {
    for(Message msg:log){
      if (msg.getFormat().equals("stdout: %s")||msg.getFormat().equals("stderr: %s")){
        if (((String)msg.getArg(0)).indexOf(string)>=0) return;
      }
    }
    verdict=Verdict.Fail;
    //fail("expected output "+string+" not found");
  };
  public final HashSet<String> pass_methods=new HashSet<String>();
  public final HashSet<String> fail_methods=new HashSet<String>();
}
