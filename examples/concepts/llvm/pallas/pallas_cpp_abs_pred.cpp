// Example for the use of abstract predicates to ´mock-up´ external libraries.
// Contains dummy-implementations of the external functions as VerCors does not support
// ghost arguments in contracts that are attached to function declarations yet.

/*@
declare using namespace pallasSpec;
@*/

class Buffer {
    private:
    int *data;
    int size;

    public:

    /*@
    abstract predicate pBuffValid(seq<int> s);
    @*/

    /*@
    assumed contract for external ir _ZN6Buffer3getEi;
    args Buffer* _this, int idx;
    returns int;
    given seq<int> s;
    requires _this->pBuffValid(s);
    requires _inRange(0, idx, _seqSize(s));
    ensures _this->pBuffValid(s);
    ensures _result<int>() == _seqGet(s, idx);
    @*/
    int get(int idx) {
        // DUMMY IMPLEMENTATION
        return 42;
    }

    /*@
    assumed contract for external ir _ZN6Buffer3setEii;
    args Buffer* _this, int idx, int val;
    returns void;
    given  seq<int> s;
    yields seq<int> sNew;
    requires _this->pBuffValid(s);
    requires _inRange(0, idx, _seqSize(s));
    ensures _this->pBuffValid(sNew);
    ensures _seqSize(s) == _seqSize(sNew);
    ensures _seqGet(sNew, idx) == val;
    ensures _forall(_and(_inRange(0, _bv<int>("i"), _seqSize(sNew)),
                         _bv<int>("i") != idx),
                    _seqGet(sNew, _bv<int>("i")) == _seqGet(s, _bv<int>("i")));
    @*/
    void set(int idx, int val) {
        // DUMMY IMPLEMENTATION
    }

    /*@
    assumed contract for external ir _ZN6Buffer11make_bufferEi;
    args int n;
    returns Buffer*;
    yields seq<int> sNew;
    requires n > 0;
    ensures _result<Buffer*>() != nullptr;
    ensures _seqSize(sNew) == n;
    ensures _forall(_inRange(0, _bv<int>("i"), _seqSize(sNew)),
                    _seqGet(sNew, _bv<int>("i")) == 0);
    ensures _result<Buffer*>()->pBuffValid(sNew);
    @*/
    static Buffer *make_buffer(int n) {
        // DUMMY IMPLEMENTATION
        return nullptr;
    }
};


// The ensures-clause is trivial, but needed to determine the type of ´data´.
/*@
yields seq<int> data;
ensures _seqSize(data) > 0;
@*/
int main() {
    auto *b = Buffer::make_buffer /*@ yields data = sNew; @*/ (42);
    b->set /*@ given s = data; @*/ /*@ yields data = sNew; @*/ (0, 16);
    int res = b->get /*@ given s = data; @*/ (0);
    /*@
    assert res == 16;
    @*/
    return 0;
}