//:: cases MethodCallCatchStackTrace
//:: tools silicon
//:: verdict Pass

class MethodCallCatchStackTrace {
    void m () {
        try {
            throw new Exception();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

