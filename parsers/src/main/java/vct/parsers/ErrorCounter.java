package vct.parsers;

import static hre.lang.System.Fail;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.concurrent.atomic.AtomicInteger;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;

public final class ErrorCounter implements ANTLRErrorListener {
  public  final AtomicInteger count =new AtomicInteger();

  
  private final ArrayList<Integer> ofs=new ArrayList<Integer>();
  private final ArrayList<String> file=new ArrayList<String>();
  private final ArrayList<Integer> src=new ArrayList<Integer>();
  
  public final String main_file;
  public ErrorCounter(String main){
    main_file=main;
    ofs.add(0);
    file.add(main);
    src.add(0);
  }

  @Override
  public void syntaxError(Recognizer<?, ?> arg0, Object arg1, int arg2,
      int arg3, String arg4, RecognitionException arg5) {
    int i=ofs.size()-1;
    while(arg2<ofs.get(i)){
      i--;
    }
    String fname=file.get(i);
    int fofs=arg2-ofs.get(i)+src.get(i);
    hre.lang.System.Warning("%s, %d:%d %s",fname,fofs,arg3,arg4);
    count.incrementAndGet();
  }

  @Override
  public void reportContextSensitivity(Parser arg0, DFA arg1, int arg2,
      int arg3, int arg4, ATNConfigSet arg5) {
  }

  @Override
  public void reportAttemptingFullContext(Parser arg0, DFA arg1, int arg2,
      int arg3, BitSet arg4, ATNConfigSet arg5) {
  }

  @Override
  public void reportAmbiguity(Parser arg0, DFA arg1, int arg2, int arg3,
      boolean arg4, BitSet arg5, ATNConfigSet arg6) {
  }

  public void report() {
    if (count.get()>0){
      Fail("encountered %d syntax error(s) in %s",count.get(),main_file);
    }
  }
}