class MyClass {
    resource lock_invariant() = Perm(this.x, 1);

    int x;

    void increment() {
        synchronized(this) {
            x += 1;
        }
    }
}

