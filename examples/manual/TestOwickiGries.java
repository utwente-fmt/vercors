// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases OwickiGriesMain
//:: tools
class TestOwickiGries {
    public static void main(String args[]) {
        OwickiGries og = new OwickiGries();
        System.err.printf("og.x == %d%n", og.x);
        og.main();
        System.err.printf("og.x == %d%n", og.x);
    }
}
