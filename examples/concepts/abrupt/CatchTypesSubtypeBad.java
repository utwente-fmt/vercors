// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases CatchTypesSubtypeBad
//:: tools silicon
//:: verdict Error

import java.io.*;

class MyClass {
    void foo() {
        try {
            throw new Exception();
        } catch (Exception e) {

        } catch (IOException f) {

        }
    }
}