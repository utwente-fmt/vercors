class Counter {
    int count;
    int[] countList;

    //@ resource state(int val, int bal) = Perm(count, write) ** count == val;

    /*@
        requires n > 0;
        requires Perm(count, write);
        requires (\forall int i; 0 < i && i < countList.length; countList[i] <= count);
        requires (\forall* int i; 0 < i && i < countList.length; Perm(countList[i], write));
        ensures (\forall int i, int j; 0 < i && i < countList.length && 0 < j && j < i; countList[i] == countList[j]);
     */
    void increment(int n) {
        count += n;
    };
}
