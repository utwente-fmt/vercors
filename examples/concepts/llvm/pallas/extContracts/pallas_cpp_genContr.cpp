// Check that generated C++ contracts work as expected.

/*@
declare using namespace pallasSpec;
@*/

class ClassyClass {
  public:
    int i = 1;
    int j = 2;

    // Contract for the generated constructor
    /*@
    contract for generated src ClassyClass;
    requires &*this != nullptr;
    requires _sep(_Perm(&this->i, _write), _Perm(&this->j, _write));
    ensures  _sep(_Perm(&this->i, _write), _Perm(&this->j, _write));
    ensures this->i == 1;
    ensures this->j == 2;
    @*/
};

/*@
ensures _result<int>() == 3;
@*/
int foo() {
    ClassyClass c;
    int res = c.i + c.j;
    return res;
}
