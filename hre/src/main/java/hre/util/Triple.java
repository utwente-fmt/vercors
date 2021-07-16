package hre.util;

public class Triple<T1, T2, T3> {

    public final T1 v1;
    public final T2 v2;
    public final T3 v3;

    public Triple(T1 v1, T2 v2, T3 v3) {
        this.v1 = v1;
        this.v2 = v2;
        this.v3 = v3;
    }

}
