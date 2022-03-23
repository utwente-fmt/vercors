//:: cases NoResultInSignals
//:: tools silicon
//:: verdict Error

class C {
    //@ signals (RuntimeException e) \result;
    boolean e();
}
