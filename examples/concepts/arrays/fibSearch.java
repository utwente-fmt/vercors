// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases FibonacciSearch
//:: tools silicon
//:: verdict Pass

// the following code is based on https://en.wikipedia.org/wiki/Fibonacci_search_technique 

// Java program for Fibonacci Search

class fibSearch {

    // calculate minimum of two given values
    public /*@ pure @*/ static int min(int x, int y)
    {
        return (x <= y) ? x : y;
    }

    /*@
        context arr != null;
        context Perm(arr[*], read);
        context arr.length > 0;
        context (\forall int i = 0 .. arr.length, int j = i+1 .. arr.length; {:arr[i]:} <= {:arr[j]:});
        ensures \result == -1 ==> (\forall int j; 0 <= j && j < arr.length; arr[j] != x); 
        ensures \result != -1 ==> \result >= 0 && \result < arr.length;
        ensures \result != -1 ==> arr[\result] == x;
        ensures (\forall int j; 0<=j && j<arr.length; arr[j] == \old(arr[j]));
    @*/
    public static int fibonacciSearch(int[] arr, int x) {
        
        // current Fibonacci number F_k, and the predecessing numbers F_{k-1} and F_{k-2}
        int fk2 = 0;
        int fk1 = 1;
        int fk = fk1 + fk2;
        //@ ghost seq<int> fibs = seq<int>{fk, fk1, fk2};
    

        /* initialise the Fibonacci numbers until F_{k+1} > arr.length */
        /*@
            loop_invariant arr != null;
            loop_invariant fk <= arr.length;
            
            loop_invariant |fibs| >= 3;
            loop_invariant fk == fibs[0] && fk1 == fibs[1] && fk2 == fibs[2];
            loop_invariant fibs[|fibs|-1] == 0 && fibs[|fibs|-2] == 1;
            loop_invariant (\forall int j; 0<=j && j<|fibs|-1; fibs[j] > 0);
            loop_invariant (\forall int j; 0<=j && j<|fibs|-2; fibs[j] == fibs[j+1] + fibs[j+2]);
        @*/
        while (fk+fk1 <= arr.length) {
            fk2 = fk1;
            fk1 = fk;
            fk = fk2 + fk1;
            //@ ghost fibs = seq<int>{fk} + fibs;
        }

        
        /*  idx is the position where we split, with F_{k-1} elements still to be searched below 
            and F_k elements above.
            Note: wikipedia splits s.t. the lower part is larger than the upper, we do the inverse.
                  Therefore we use fk1 to initialise idx, rather than F_k as in wikipedia
            Note: the wikipedia version uses 1-based indexing, we use 0-based, therefore subtract 1
        */
        int idx = fk1 - 1;
        
        
        /*@
              loop_invariant arr != null;
              loop_invariant Perm(arr[*], read);
              loop_invariant (\forall int i; 0<=i && i<arr.length; 
                                (\forall int j; i<j && j<arr.length; arr[i] <= arr[j]));
              loop_invariant (\forall int j; 0<=j && j<arr.length; arr[j] == \old(arr[j]));

              loop_invariant 0<=idx && idx < arr.length;
              loop_invariant idx >= fk1 - 1;

              loop_invariant |fibs| >= 3;
              loop_invariant fk == fibs[0] && fk1 == fibs[1] && fk2 == fibs[2];
              loop_invariant fibs[|fibs|-1] == 0 && fibs[|fibs|-2] == 1;
              loop_invariant (\forall int j; 0<=j && j<|fibs|-1; fibs[j] > 0);
              loop_invariant (\forall int j; 0<=j && j<|fibs|-2; fibs[j] == fibs[j+1] + fibs[j+2]);
              
              loop_invariant (\forall int j; 0<=j && j<=idx-fk1; arr[j] < x);
              loop_invariant (\forall int j; idx+fk <= j && j<arr.length; x < arr[j]);
        @*/
        while (true) {
            if (x < arr[idx]) {
                if (fk1 == 1) {
                    return -1;
                } else {
                    // shift Fibonacci sequence down two steps (always possible if F_{k-1} != 1)
                    fk = fk2;
                    fk1 = fk1 - fk2;
                    fk2 = fk - fk1;
                    //@ ghost fibs = fibs.tail.tail;
                    
                    // update idx s.t. again F_k elements to be searched above and F_{k-1} below
                    idx = idx - fk;
                }
            } else if (x > arr[idx]) {
                if (fk2 == 0) {
                    return -1;
                } else {
                    // shift Fibonacci sequence down one step (always possible if F_{k-2} != 0)
                    fk = fk1;
                    fk1 = fk2;
                    fk2 = fk - fk1;
                    //@ ghost fibs = fibs.tail;
                    
                    // update idx s.t. again F_{k-1} elements to be searched below and F_k above 
                    idx = min(idx + fk1, arr.length-1);
                }
            } else {
                // match found
                return idx;
            }
        }

    }
}