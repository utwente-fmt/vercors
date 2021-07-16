//:: cases CatchEmpty
//:: tools silicon
//:: verdict Pass

class CatchEmpty {
    void m() {
        try {
            throw new Exception();
        } catch (Exception e) {

        }
    }
}

