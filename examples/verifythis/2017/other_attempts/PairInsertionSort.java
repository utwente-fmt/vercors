// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases PairInsertionSort
//:: tools silicon
//:: verdict Pass

// Challenge 1 of https://formal.iti.kit.edu/ulbrich/verifythis2017

class PairInsertionSort {

    /*@
        context_everywhere A != null;
        context_everywhere Perm(A[*], write);
        ensures (\forall int m; 0<=m && m<A.length; 
                    (\forall int k; m<k && k<A.length; A[m] <= A[k]));
    @*/
    public void sort(int[] A) {
        int i = 0;                          /// i is running index (inc by 2 every iteration)
        
        /*@ 
            loop_invariant 0<=i && i<=A.length;
            loop_invariant (\forall int m; 0<=m && m<i; 
                                (\forall int k; m<k && k<i; A[m] <= A[k]));
        @*/
        while (i < A.length-1) {
            int x = A[i];                   /// let x and y hold the next two elements in A
            int y = A[i+1];
            if (x < y) {
                /// ensure that x is not smaller than y
                int temp = x;
                x = y;
                y = temp;
            }
                
            int j = i - 1;                  /// j is the index used to find the insertion point
            /*@ 
                loop_invariant 0<=i && i<A.length-1;
                loop_invariant -1<=j && j<i;
                loop_invariant y <= x;
                // loop_invariant (\forall int k; j<k && k<i; x<A[k+2]);
                // same as above, but better for triggers
                loop_invariant (\forall int k; j+2<k && k<i+2; x<A[k]);
                loop_invariant (\forall int m; 0<=m && m<j; 
                                    (\forall int k; m<k && k<=j; A[m] <= A[k]));
                loop_invariant (\forall int m; j+3<=m && m<i+1; 
                                    (\forall int k; m<k && k<=i+1; A[m] <= A[k]));
                loop_invariant 0<=j && j<i-1 ==> A[j] <= A[j+3];
            @*/
            while (j >= 0 && A[j] > x) {    /// find the insertion point for x
               A[j+2] = A[j];               /// shift existing content by 2
               j = j - 1;
            }
            A[j+2] = x;                     /// store x at its insertion place
                                            /// A[j+1] is an available space now
                                            
            /*@ 
                loop_invariant 0<=i && i<A.length-1;
                loop_invariant -1<=j && j<i;
                loop_invariant y <= A[j+2];
                // loop_invariant (\forall int k; j<k && k<i; y<=A[k+1]);
                // same as above, but better for triggers
                loop_invariant (\forall int k; j+1<k && k<i+1; y<=A[k]);
                loop_invariant (\forall int m; 0<=m && m<j; 
                                    (\forall int k; m<k && k<=j; A[m] <= A[k]));
                loop_invariant (\forall int m; j+2<=m && m<i+1; 
                                    (\forall int k; m<k && k<=i+1; A[m] <= A[k]));
                loop_invariant 0<=j && j<i ==> A[j] <= A[j+2];
            @*/
            while (j >= 0 && A[j] > y) {    /// find the insertion point for y
                A[j+1] = A[j];              /// shift existing content by 1
                j = j - 1;
            }
            A[j+1] = y;                     /// store y at its insertion place
            i = i+2;
        }
        
        
        if (i == A.length-1) {               /// if A.length is odd, an extra
            int y = A[i];                    /// single insertion is needed for
            int j = i - 1;                   /// the last element
            /*@ 
                loop_invariant -1<=j && j<A.length-1;
                // loop_invariant (\forall int k; j<k && k<i; y<=A[k+1]);
                // same as above, but better for triggers
                loop_invariant (\forall int k; j+1<k && k<i+1; y<=A[k]);
                loop_invariant (\forall int m; 0<=m && m<j; 
                                    (\forall int k; m<k && k<=j; A[m] <= A[k]));
                loop_invariant (\forall int m; j+2<=m && m<A.length; 
                                    (\forall int k; m<k && k<A.length; A[m] <= A[k]));
                loop_invariant 0<=j && j<A.length-2 ==> A[j] <= A[j+2];
            @*/
            while (j >= 0 && A[j] > y) {
                A[j+1] = A[j];
                j = j - 1;
            }
            A[j+1] = y;
        }
    }
}