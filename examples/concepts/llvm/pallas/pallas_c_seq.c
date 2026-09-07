// Example of a simple linked list to test that sequences in Pallas 
// specificiations work correctly. 
// (Assumes write-perm everywhere even though read would be sufficient)
// This includes handling sret- and byval-arguments on the LLVM-IR level.

#include <stddef.h>

typedef struct IntListT {
    int value;
    struct IntListT *next;
} IntList;

/*@
declare DEF_RESULT(int);
declare DEF_SEQ(int);
declare DEF_RESULT_(IntList *,ListPtr);
declare DEF_UNFOLDING(bool);
@*/

/*@
predicate listWrite(IntList *l, SEQ(int) s) :=
    _sep(
        _imply(l == NULL, _seqSize(int)(s) == 0),
        _imply(l != NULL, _sep(_seqSize(int)(s) > 0,
                          _sep(_Perm(l, _write),
                          _sep(l->value == _seqAt(int)(s, 0),
                          listWrite(l->next, _seqTail(int, s))
                          )))
        )
    );
@*/

/*@
given SEQ(int) s;
requires listWrite(l, s);
ensures  listWrite(l, s);
ensures  _result(int) == _seqSize(int)(s);
@*/
int list_size(IntList *l) {
    if (l == NULL) {
        /*@
        assert _unfolding(bool)(listWrite(l, s) , 
                                _seqSize(int)(s) == 0);
        @*/
        return 0; 
    } else {
        /*@
        unfold listWrite(l, s);
        @*/
        int len =  1 + list_size /*@ given s = _seqTail(int, s); @*/ (l->next);
        /*@
        fold listWrite(l, s);
        @*/
        return len;
    } 
}


/*@
given  SEQ(int) s;
requires _seqSize(int)(s) > 0;
requires listWrite(l, s);
ensures  listWrite(l, s);
ensures _result(int) == _seqHead(int, s);
@*/
int get_head(IntList *l) {
    /*@ unfold listWrite(l, s); @*/
    int res = l->value;
    /*@ fold   listWrite(l, s); @*/
    return res;
}

// We need to assume this function because alloca with a void-ptr 
// is not yet supported.
/*@
assumed contract for external ir allocIntList;
args ;
returns IntList*;
ensures _result(ListPtr) != NULL;
ensures _Perm(_result(ListPtr), _write);
ensures _result(ListPtr)->next == NULL;
ensures _result(ListPtr)->value == 0;
@*/
IntList *allocIntList();



/*@
given  SEQ(int) s;
yields SEQ(int) sNew;
requires listWrite(l, s);
ensures  listWrite(_result(ListPtr), sNew);
ensures  _seqSize(int)(sNew) == _seqSize(int)(s) + 1; 
ensures  _seqEq(int)(s, _seqTail(int, sNew));
ensures  _seqHead(int, sNew) == elem;
@*/
IntList *prepend(IntList *l, int elem) {
    IntList *newHead = allocIntList();
    newHead->value = elem;
    newHead->next = l;
    /*@
    ghost assign sNew = _seqPrepend(int)(elem, s);
    fold listWrite(newHead, sNew);
    @*/
    return newHead;
}