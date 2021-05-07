//:: cases EmptyCatch
//:: tools silicon
//:: verdict Pass

class EmptyCatch {
    void m () {
        try {
            throw new Exception();
        } catch (Exception e) {
            
        }
    }
}

