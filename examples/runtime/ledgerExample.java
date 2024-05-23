package test;

import java.lang.reflect.Array;
import java.util.concurrent.ConcurrentHashMap;


class Ledger {

    public static ConcurrentHashMap<Long, ConcurrentHashMap<Object, Double>> __runtime__ = new ConcurrentHashMap<Long, ConcurrentHashMap<Object, Double>>();
    public static ConcurrentHashMap<Object, ConcurrentHashMap<Integer, Object>> __array_locations = new ConcurrentHashMap<Object, ConcurrentHashMap<Integer, Object>>();


    public static void createHashMap() {
        if (!__runtime__.containsKey(Thread.currentThread().getId())) {
            __runtime__.put(Thread.currentThread().getId(), new ConcurrentHashMap<Object, Double>());
        }
    }

    public static Double getPermission(Object input) {
        createHashMap();
        return __runtime__.get(Thread.currentThread().getId()).getOrDefault(input, 0.0);
    }

    public static Double getPermission(Object input, int location) {
        createHashMap();
        Object permLoc = __array_locations.get(input).get(location);
        return getPermission(permLoc);
    }

    public static void setPermission(Object input, Double value) {
        assert (value >= 0 && value <= 1) : "value is not between bounds 0 and 1: " + value;
        createHashMap();
        __runtime__.get(Thread.currentThread().getId()).put(input, value);
    }

    public static void setPermission(Object input, int location, Double value) {
        createHashMap();
        assert (input.getClass().isArray());
        Object permLoc = __array_locations.get(input).get(location);
        setPermission(permLoc, value);
    }

    public static void initiatePermission(Object input) {
        createHashMap();
        setPermission(input, 1.0);
        if (input.getClass().isArray()) {
            initiatePermission(input, Array.getLength(input));
        }
    }

    public static void initiatePermission(Object input, int size) {
        createHashMap();
        setPermission(input, 1.0);
        __array_locations.put(input, new ConcurrentHashMap<>());
        for (int i = 0; i < size; i++) {
            Object[] permLoc = {input, i};
            __array_locations.get(input).put(i, permLoc);
            setPermission(permLoc, 1.0);
        }

    }
}


class Test extends Thread{
    int[] a;
    int b;

    public Test(int[] a) {
        this.a = a;
    }

    /*@
        requires Perm(this.a, write);
     */
    @Override
    public void run() {
        Ledger.setPermission(this.a, 1.0);
        System.out.println("External thread: " + Ledger.getPermission(this.a));
        super.run();
    }
}


class Main {

    public static void main(String[] args) {
        int[] a = new int[]{1, 2, 3, 4};
        Ledger.initiatePermission(a);
        Test test = new Test(a);
        Ledger.setPermission(test.a, Ledger.getPermission(test.a) - 1.0);
        test.start();
        Ledger.initiatePermission(test, 1);
        System.out.println(Ledger.getPermission(test.a));
        System.out.println(Ledger.getPermission(test, 0));
    }
}

