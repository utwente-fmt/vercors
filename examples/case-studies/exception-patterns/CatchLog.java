//:: cases MethodCallCatchLog
//:: tools silicon
//:: verdict Pass

class MethodCallCatchLog {
    void log(String msg);

    void m() {
        try {
            throw new Exception();
        } catch (Exception e) {
            log(e.getMessage());
        }
    }
}

