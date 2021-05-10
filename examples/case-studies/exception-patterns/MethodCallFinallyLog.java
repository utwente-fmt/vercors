//:: cases MethodCallFinallyLog
//:: tools silicon
//:: verdict Pass

class MethodCallFinallyLog {
    void log(String msg);
    String getMessage();

    void m () {
        try {

        } finally {
            log(getMessage());
        }
    }
}


