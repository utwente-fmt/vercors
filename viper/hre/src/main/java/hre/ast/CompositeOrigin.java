// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package hre.ast;


import java.util.Arrays;

/**
 * Origin that encapsulates other origins.
 * @author sccblom
 *
 */
public class CompositeOrigin extends Origin {

    private Origin origins[];
    public CompositeOrigin(Origin ... origins){
        this.origins=Arrays.copyOf(origins,origins.length);
    }
    public String toString(){
      String result=origins[0].toString();
      for(int i=1;i<origins.length;i++){
        result+=" and "+origins[i].toString();
      }
      return result;
    }
    @Override
    public void report(String level, Iterable<String> message) {
      // TODO Auto-generated method stub
      origins[0].report(level,message);
    }
    @Override
    public void report(String level, String... message) {
      // TODO Auto-generated method stub
      origins[0].report(level,message);      
    }

}

