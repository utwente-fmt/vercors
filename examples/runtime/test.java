class Counter {
    int count;
    int[] countList;

    /*@
        requires (\forall int i; 0 < i && i < countList.length; (\forall int j; 0 < j && j < i; countList[j] < countList[i]));
     */
    void increment(int n) {
        count += n;
    };
}
