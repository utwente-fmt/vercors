// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ZeroArrayIC
//:: tools silicon
//:: verdict Pass

public class ZeroArray {
    /*@ 
        context_everywhere ar != null;
        requires Perm(ar[*],1);
        ensures Perm(ar[*],1) ** (\forall int k ; 0 <= k && k < ar.length ; ar[k]==0 ) ;
     */
    public void zero_array(int ar[]){
        //int N=ar.length;
        for(int i=0;i<ar.length;i++)
        /*@  requires Perm(ar[i],1);
             ensures  Perm(ar[i],1) ** ar[i]==0;
        @*/
        {
            ar[i]=0;
        }
    }
}

