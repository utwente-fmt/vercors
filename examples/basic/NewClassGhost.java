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
        /*@ given { g = 5 } */ new NewClassGhost();
    }
}