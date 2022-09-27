// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases ArrayAsObject
//:: tools silicon
//:: verdict Error
// Unsupported: Arrays are not treated as a subtype of Object.
public class ArrayAsObject {
    public void test() {
        Object x = new Object[1];
    }
}
