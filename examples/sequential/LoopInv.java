// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases LoopInvBoogie
//:: tools boogie
//:: verdict Fail
/* vct --boogie LoopInv.java

produces

  method f_ok: Pass
  method f_bad: Fail (loop invariant maintenance violation)
  The final verdict is Fail

*/
public class LoopInv {
  /*@ requires n>0 ; */
  /*@ ensures \result==n*n; */
	public static int f_ok(int n){
	  int res,i;
		res=0;
		i=0;
		//@ loop_invariant res==i*n;
		//@ loop_invariant i <= n;
		while(i<n) {
			res=res+n;
			//i++;
			i=i+1;
		}
		return res;
	}

  /*@ requires n>0 ; */
  /*@ ensures \result==n*n; */
	public static int f_bad(int n){
	  int res,i;
		res=0;
		i=0;
		//@ loop_invariant res==i*n;
		//@ loop_invariant i < n;
		while(i<n) {
			res=res+n;
			i=i+1;
		}
		return res;
	}
}
