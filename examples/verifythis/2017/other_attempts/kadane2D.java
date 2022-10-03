// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases Kadane
//:: verdict Pass

// two-dimentional bonus for Challenge 2 of https://formal.iti.kit.edu/ulbrich/verifythis2017
// uses the 1D case of kadane.java
// Note commented out loop invariants are slow, despite the "assume". To be investigated

class Kadane2D {

    /*@ 
        requires a != null;
        requires a.length > 0;
        requires (\forall* int i; 0<=i && i<a.length; Perm(a[i], read));
        requires (\forall int i; 0<=i && i<a.length; a[i] != null);
        requires (\forall int i; 0<=i && i<a.length; {: a[i].length :} == a[0].length);
        requires (\forall* int i; 0<=i && i<a.length; Perm(a[i][*], read));
        ensures |\result| == a[0].length;
        ensures (\forall int i; 0<=i && i<\result.length; |\result[i]| == a.length);
        ensures (\forall int i; 0<=i && i<a.length; 
                    (\forall int j; 0<=j && j<a[i].length; \result[j][i] == a[i][j]));
    static seq<seq<int>> arr2d2seq(int[][] a);
    @*/
        
    
    /*@
        yields int row_start;
        yields int col_start;
        yields int row_end;
        yields int col_end;
        context_everywhere a != null;
        context_everywhere (\forall* int i; 0<=i && i<m; Perm(a[i], 1\2));
        context_everywhere m == a.length;
        context_everywhere (\forall int i; 0<=i && i<m; {: a[i] :} != null);
        context_everywhere (\forall int i; 0<=i && i<m; {: a[i].length :} == n);
        context_everywhere (\forall* int i; 0<=i && i<m; 
                    (\forall* int j; 0<=j && j<n; Perm(a[i][j], 1\2)));
    @*/
    static int maxSubMatrixSum(int[][] a, int m, int n) {
        int[][][] acc_sums = new int[m][][];
        int total_max = 0;
        
        if (m==0 || n==0) {
            return 0;
        }
        
        /*@ ghost row_start = 0;
            ghost row_end = 0;
            ghost col_start = 0;
            ghost col_end = 0;
            ghost seq<seq<int>> as_seq = arr2d2seq(a);
        @*/
        
        /*@ loop_invariant m>0 && n>0;
            loop_invariant 0<=i && i<=m;
            // loop_invariant as_seq == arr2d2seq(a);
            loop_invariant (\forall* int ii; 0<=ii && ii<m; Perm(acc_sums[ii], write));
        @*/
        for (int i=0; i<m; i++) {
            acc_sums[i] = new int[m-i][];
            /*@ loop_invariant m>0 && n>0;
                loop_invariant 0<=i && i<=m;
                loop_invariant i<=j && j<=m;
                // loop_invariant as_seq == arr2d2seq(a);
                loop_invariant acc_sums != null;
                loop_invariant acc_sums.length == m;
                loop_invariant Perm(acc_sums[i], write);
                loop_invariant acc_sums[i] != null;
                loop_invariant acc_sums[i].length == m-i;
                loop_invariant (\forall* int jj; 0<=jj && jj<m-i; Perm(acc_sums[i][jj], write));
            @*/
            for (int j=i; j<m; j++) {
                acc_sums[i][j-i] = new int[n];
                /*@ loop_invariant m>0 && n>0;
                    loop_invariant 0<=i && i<=m;
                    loop_invariant i<=j && j<=m;
                    loop_invariant 0<=k && k<=n;
                    // loop_invariant as_seq == arr2d2seq(a);
                    loop_invariant Perm(acc_sums[i], 1\2);
                    loop_invariant acc_sums[i] != null;
                    loop_invariant acc_sums[i].length == m-i;
                    loop_invariant Perm(acc_sums[i][j-i], 1\2);
                    loop_invariant acc_sums[i][j-i] != null;
                    loop_invariant acc_sums[i][j-i].length == n;
                    loop_invariant (\let int[] arr = acc_sums[i][j-i]; 
                                        (\forall* int kk; 0<=kk && kk<n; Perm(arr[kk], write)));
                    // loop_invariant (\forall int kk; 0<=kk && kk<k; 
                                // acc_sums[i][j-i][kk] == (\sum int ll; i<=ll && ll<j; as_seq[kk][ll]));
                @*/
                for (int k=0; k<n; k++) {
                    /*@ loop_invariant m>0 && n>0;
                        loop_invariant 0<=i && i<=m;
                        loop_invariant i<=j && j<=m;
                        loop_invariant 0<=k && k<=n;
                        loop_invariant i<=l && l<=j;
                        // loop_invariant as_seq == arr2d2seq(a);
                        loop_invariant Perm(acc_sums[i], 1\2);
                        loop_invariant acc_sums[i] != null;
                        loop_invariant acc_sums[i].length == m-i;
                        loop_invariant Perm(acc_sums[i][j-i], 1\2);
                        loop_invariant acc_sums[i][j-i] != null;
                        loop_invariant acc_sums[i][j-i].length == n;
                        loop_invariant (\let int[] arr = acc_sums[i][j-i]; 
                                            (\forall* int kk; 0<=kk && kk<n; Perm(arr[kk], write)));
                        // loop_invariant acc_sums[i][j-i][k] == (\sum int ll; i<=ll && ll<l; as_seq[k][ll]);
                    @*/
                    for (int l=i; l<j; l++) {
                        acc_sums[i][j-i][k] = acc_sums[i][j-i][k] + a[l][k];
                        //@ assume as_seq == arr2d2seq(a);
                    }
                    //@ assume as_seq == arr2d2seq(a);
                }
                /*@ ghost int col_min;
                    ghost int col_max; @*/
                int max_i_j = Kadane.maxSubArraySum(acc_sums[i][j-i], n) 
                                        /*@ then { col_min=max_start; 
                                                   col_max=max_end; } @*/;
                if (max_i_j > total_max) {
                    total_max = max_i_j;
                    /*@ ghost row_start = i;
                        ghost row_end = j;
                        ghost col_start = col_min;
                        ghost col_end = col_max;
                    @*/
                }
                //@ assume as_seq == arr2d2seq(a);
            }
            //@ assume as_seq == arr2d2seq(a);
        }
        return total_max;
    }
}
