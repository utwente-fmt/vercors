class Counter {
    int count;
    int[] countList;


    //@ resource state(int n) = Perm(this.count, write) ** this.count == n;


    /*@
        requires (\forall* int i; 0 < i && i < countList.length; Perm(countList[i], 1));
        requires Perm(count, 0.5) ** (count < 0 || count >= 2);
     */
    void increment(int n) {
        count += n;
    };
}
