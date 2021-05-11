//:: cases FinallyLog
//:: tools silicon
//:: verdict Pass

class FinallyLog {
    void log(String msg);
    String getMessage();

    void m () {
        try {

        } finally {
            log(getMessage());
        }
    }
}


