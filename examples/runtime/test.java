//@ lock_invariant Perm(x, 1);
class Counter {
    int x;

    int y;
    int z;


    //@requires committed(this);
    public void increment () {
        synchronized(this){
            // Assume lock
            x += 2;
        }
    }
}