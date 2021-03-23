// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases BoogieTest
//:: tools silicon
//:: verdict Pass BoogieTest.abs BoogieTest.good_incr_1 BoogieTest.good_incr_2
//:: verdict Fail BoogieTest.bad_incr_1 BoogieTest.bad_incr_2

/* vct --silicon BoogieTest.java

produces

  method abs: Pass
  method bad_incr_1: Fail (assertion violation)
  method bad_incr_2: Fail (assertion violation)
  method good_incr_1: Pass
  method good_incr_2: Pass
  The final verdict is Fail

*/

public class BoogieTest {

  /*@ 
    requires true;
    ensures \result >= 0 && (\result == x || \result == -x);
  @*/
  public static int abs(int x){
    if(x>0) return x;
    return -x;
  }
  
  /*@ 
    requires true;
    ensures \result == x+1;
  @*/
  public static int bad_incr_1(int x){
    return x++;
  }

  /*@ 
    requires true;
    ensures \result == x+1;
  @*/
  public static int bad_incr_2(int x){
    return ++x;
  }

  /*@ 
    requires true;
    ensures \result == x+1;
  @*/
  public static int good_incr_1(int x){
    int res=x+1;
    return res;
  }

  /*@ 
    requires true;
    ensures \result == x+1;
  @*/
  public static int good_incr_2(int x){
    int res=good_incr_1(x);
    return res;
  }

}

