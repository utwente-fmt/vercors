//:: cases CatchStackTrace
//:: tools silicon
//:: verdict Pass

class CatchStackTrace {
    void m () {
        try {
            throw new Exception();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

