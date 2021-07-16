//:: cases InvokationInGuard
//:: tools silicon
//:: verdict Pass

class InvokationInGuard {
    int bar() {
        return 0;
    }

    int foo() {
        return 0;
    }

    void action(int i) {
        return;
    }

    void test() {
        if (bar() == bar()) {
            action(0);
        } else if (bar() == foo()) {
            action(1);
        } else if (foo() == bar()) {
            action(2);
        } else {
            action(3);
        }
    }
}
