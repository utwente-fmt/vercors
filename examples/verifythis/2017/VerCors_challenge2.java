// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases Kadane
//:: tools silicon
//:: verdict Pass

// Challenge 2 of https://formal.iti.kit.edu/ulbrich/verifythis2017
//
/*@
requires 0 <= start && start <= |xs|;
requires 0 <= end && end <= |xs|;
ensures start >= end ==> \result == 0;
ensures end - start == 1 ==> \result == xs[start];
pure int sum(seq<int> xs, int start, int end) = start < end ? xs[start] + sum(xs, start + 1, end) : 0;
*/


class Kadane {
    /*@ ghost
    requires 0 <= start && start < |xs|;
    requires 0 <= end && end < |xs|;
    requires start <= end;
    ensures sum(xs, start, end + 1) == sum(xs, start, end) + xs[end];
    decreases |xs| - start;
    static void sumAddRight(seq<int> xs, int start, int end) {
        if (start < end) {
            sumAddRight(xs, start + 1, end);
        }
    }
    */

    /*@ ghost
    requires 0 <= end && end < |xs|;
    ensures (\let int e1 = end + 1; (\forall int i = 0 .. end + 1; {:sum(xs, i, e1):} == sum(xs, i, end) + xs[end]));
    decreases;
    static void sumAddAllRight(seq<int> xs, int end) {
        loop_invariant 0 <= i && i <= end + 1;
        loop_invariant (\let int e1 = end + 1; (\forall int j = 0 .. i; {:sum(xs, j, e1):} == sum(xs, j, end) + xs[end]));
        decreases end - i;
        for (int i = 0; i <= end; i++) {
            sumAddRight(xs, i, end);
        }
    }
    */

    /*@ ghost
        requires a != null;
        requires Perm(a[*], read);
        ensures |\result| == a.length;
        ensures (\forall int i; 0<=i && i<a.length; {:\result[i]:} == a[i]);
    pure static seq<int> arr2seq(int[] a);
    @*/

    /*@
        yields int max_start;
        yields int max_end;
        context_everywhere a != null;
        context_everywhere Perm(a[*], 1\2);
        context_everywhere size == a.length;
        ensures 0 <= max_start && max_start <= size;
        ensures 0 <= max_end && max_end <= size;
        ensures \result == sum(arr2seq(a), max_start, max_end);
        ensures (\forall int k = 0 .. size + 1, int m = k .. size + 1;
                        \result >= {:sum(arr2seq(a), k, m):});
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
            loop_invariant 0 <= max_start && max_start <= size;
            loop_invariant 0 <= max_end && max_end <= size;
            loop_invariant max_ending_here == sum(as_seq, start, i);
            loop_invariant (\forall int k = 0 .. i + 1;
                        max_ending_here >= {:sum(as_seq, k, i):});
            loop_invariant (\forall int k = 0 .. i + 1, int m = k .. i + 1;
                        max_so_far >= {:sum(as_seq, k, m):});
            loop_invariant max_so_far == sum(as_seq, max_start, max_end);
            decreases size - i;
        @*/
        for (int i = 0; i < size; i++)
        {
            max_ending_here = max_ending_here + a[i];
            //@ ghost sumAddAllRight(as_seq, i);
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
