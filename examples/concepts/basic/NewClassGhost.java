//:: cases NewClassGhost
//:: tools silicon
//:: verdict Pass

class NewClassGhost {
    /*@ given int g;
        requires g == 5;
     */
    public NewClassGhost() {

    }

    public void test() {
        new NewClassGhost() /*@ given { g = 5 } */;
    }
}