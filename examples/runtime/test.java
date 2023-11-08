class Test{
    private int x;
    private int y;

    public Test() {

    }


    public int sum () {
        return x + y;
    }

    public int returnSum() {
        int z = x + y;
        int b = 0;

        int a = sum();

        z = a + z;
        x = z;
        y++;

        return z;
    }
}