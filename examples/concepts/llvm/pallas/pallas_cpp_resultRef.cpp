// Test of the _resultRef construct which returns a reference to the
// return-value of a function. This allows referring to the result of a
// function that returns a type with a non-trivial destructor.

/*@
declare using namespace pallasSpec;
@*/

class AClass {
    public:
    int a;
    int b;

    /*@
    contract for generated src \~AClass;
    requires _sep(&*this != nullptr,
                  _Perm(this, _write));
    @*/
    ~AClass()
     {
        a = 0;
        b = 0;
    }
};

/*@
ensures _resultRef<AClass>().a == 1;
ensures _resultRef<AClass>().b == 2;
@*/
AClass make_init_class() {
    AClass c;
    c.a = 1;
    c.b = 2;
    return c;
}
