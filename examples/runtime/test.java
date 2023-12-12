class Counter {
    /*@
        requires a.length > 0;
        requires \forall* int i; 0 <= i < a.length; Perm(a[i], write);
        requires \forall int i; 0 <= i < a.length; a[i] > 0;
        requires \forall int i; 0 <= i < a.length; \forall int j; 0 <= j < i; a[j] <= a[i];
        ensures (\exists int i; 0 <= i < a.length; a[i] == b) ==> \result >= 0;
        ensures \forall* int i; 0 <= i < a.length; Perm(a[i], write);
     */
    public int indexOf() {
        return 1;
    }
}
