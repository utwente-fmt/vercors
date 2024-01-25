class Counter {
    int[] a;
    int b;
    Counter d;

    int increment(Counter c) {
        c.b += b;
        a[0] = a[0] + c.a[1];
        increment(d);
        b = a[0] + c.b;

        for (int i = 0; i < a.length; i++) {
            if(b < c.b) {
                int n = b + c.b;
            } else if(c.increment(c) < b) {
                int z = b;
            }
        }
    };
}