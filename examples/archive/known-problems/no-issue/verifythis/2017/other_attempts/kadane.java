// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases Kadane
//:: tools silicon
//:: verdict Pass

// Challenge 2 of https://formal.iti.kit.edu/ulbrich/verifythis2017

class Kadane {

    /*@ 
        requires a != null;
        requires Perm(a[*], read);
        ensures |\result| == a.length;
        ensures (\forall int i; 0<=i && i<a.length; \result[i] == a[i]);
    static seq<int> arr2seq(int[] a);
    @*/

    /*@
        yields int max_start;
        yields int max_end;
        context_everywhere a != null;
        context_everywhere Perm(a[*], 1\2);
        context_everywhere size == a.length;
        ensures \result == (\sum int j; max_start<=j && j<max_end; arr2seq(a)[j]);
        ensures (\forall int k; 0<=k && k<=size; (\forall int m; k<=m && m<=size;
                        \result >= (\sum int j; k<=j && j<m; arr2seq(a)[j])));
    @*/
    static int maxSubArraySum(int[] a, int size){
        int max_so_far = 0, max_ending_here = 0;
        
        /*@ ghost int start = 0; 
            ghost max_start = 0;
            ghost max_end = 0;
            ghost seq<int> as_seq = arr2seq(a);
        @*/
        
        /*@
            loop_invariant 0<=i && i<=size;
            loop_invariant 0<=start && start<=i;
            loop_invariant as_seq == arr2seq(a);
            loop_invariant max_ending_here == (\sum int j; start<=j && j<i; as_seq[j]);
            loop_invariant (\forall int k; 0<=k && k<=i; 
                        max_ending_here >= (\sum int j; k<=j && j<i; as_seq[j]));
            loop_invariant (\forall int k; 0<=k && k<=i; (\forall int m; k<=m && m<=i;
                        max_so_far >= (\sum int j; k<=j && j<m; as_seq[j])));
            loop_invariant max_so_far == (\sum int j; max_start<=j && j<max_end; as_seq[j]);
        @*/
        for (int i = 0; i < size; i++)
        {
            max_ending_here = max_ending_here + a[i];
            if (max_ending_here < 0) {
                max_ending_here = 0;
                /*@ ghost start = i+1; @*/
            } else if (max_so_far < max_ending_here) {
                max_so_far = max_ending_here;
                /*@ ghost max_start = start;
                    ghost max_end = i+1; @*/
            }
        }
        return max_so_far;
    }
    
}
