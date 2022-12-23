//:: cases BoogieTestFail
//:: tools silicon
//:: verdict Fail

public class BoogieTest {

  /*[/expect postFailed:false]*/
  /*@ 
    requires true;
    ensures \result == x+1;
  @*/
  public static int bad_incr_1(int x){
    return x++;
  }
  /*[/end]*/
}

