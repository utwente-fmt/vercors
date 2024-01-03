class Counter {
    int count;
    int[] countList;


    //@ resource state(int n) = Perm(this.count, write) ** this.count == n;


    /*@
        requires (\forall int i; 0 < i && i < countList.length; (\forall int j; 0 < j && j < i; countList[j] < countList[i]));
     */
    void increment(int n) {
        count += n;
    };
}
