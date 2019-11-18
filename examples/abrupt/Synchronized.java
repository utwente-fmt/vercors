final class MyClass {
    resource lock_invariant() = Perm(this.x, 1);

    int x;

    MyClass() {
        x = 0;
    }

    void increment() {
        synchronized(this) {
            x += 1;
        }
    }

    synchronized void other_increment() {
        x += 1;
    }
}

