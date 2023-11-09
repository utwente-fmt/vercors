class C{

}

class Test{
    private C[] x;
    private int y;

    public Test() {

    }

    /*@
        requires Perm(x, 1);
        requires Perm(y, 1\2);
        requires Perm(x, write);
        requires Perm(y, read);
     */
    public int sum () {
        return y + 1;
    }
}