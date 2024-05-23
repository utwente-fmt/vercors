//@ lock_invariant Perm(this.i, write);
class Source {
    int i;

    public void test(){
        int x = this.i;
        synchronized (this) {
            this.i = 0;
        }
    }

    public void main() {
        Source source = new Source();
        source.test();
    }
}