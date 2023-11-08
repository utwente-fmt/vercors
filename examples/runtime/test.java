class Test{
    private int x;
    private int y;

    public Test() {

    }

    /*@
        requires Perm(x, 1);
        requires Perm(y, 1\2);
        requires Perm(x, write);
        requires Perm(y, read);
        ensures \result == x + y;
     */
    public int sum () {
        return x + y;
    }
}