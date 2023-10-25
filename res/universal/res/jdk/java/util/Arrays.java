package java.util;

public class Arrays {
    /*@ requires arr != null;
        requires 0<=l && l<=r && r<=arr.length;
        context (\forall* int i=l .. r; Perm(arr[i], 1\2));
        ensures \result != null && \result.length == r-l;
        ensures (\forall* int i=0 .. r-l; Perm(\result[i], 1));
        ensures (\forall* int i=0 .. r-l; \result[i] == arr[i+l]);
    */
    public static int[] copyOfRange(int [] arr, int l, int r);
}
