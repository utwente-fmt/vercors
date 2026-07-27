/**
 * Implementation for the second problem of the VSTTE'10 competition
 * https://www.macs.hw.ac.uk/vstte10/Competition.html
 * https://www.macs.hw.ac.uk/vstte10/Competition_files/Competition.pdf
 * 
 * Transform with -mem2reg option
 */

 /*@
 declare using namespace pallasSpec;
 @*/

#define _j _bv<int>("j")
#define _k _bv<int>("k")
#define _l _bv<int>("l")
#define _m _bv<int>("m")

/*@
pure;
ensures _result<bool>() == true;
@*/
bool trig(int v) { 
    return true; 
}

/*@
requires N >= 0;
requires A != nullptr && B != nullptr;
requires _ptrLength(A) == N && _ptrLength(B) == N;
requires _forallS(_inRange(0, _j, N), _Perm(&A[_j], _fracOf(1, 2)));
requires _forallS(_inRange(0, _j, N), _Perm(&B[_j], _write));
requires _forall (_inRange(0, _j, N), _inRange(0, A[_j], N));
requires _forall (_and(_inRange(0, _j, N), _inRange(0, _k, N)),  // Injectivity of A
                  _imply(_j != _k, A[_j] != A[_k]));
requires _forall (_inRange(0, _j, N), _imply(trig(_j),           // Surjectivity of A
                  _exists(_inRange(0, _k, N), A[_k] == _j)));
ensures  _forallS(_inRange(0, _j, N), _Perm(&A[_j], _fracOf(1, 2)));
ensures  _forallS(_inRange(0, _j, N), _Perm(&B[_j], _write));
ensures  _forall (_inRange(0, _j, N), A[_j] == _old<int>(A[_j]));
ensures  _forall (_inRange(0, _j, N), B[A[_j]] == _j);
ensures  _forall (_and(_inRange(0, _j, N), _inRange(0, _k, N)),  // Injectivity of B
                  _imply(_j != _k, B[_j] != B[_k]));
@*/
void invert(int *A, int *B, int N) {

    /*@
    loop_invariant 0 <= i && i <= N;
    loop_invariant _forallS(_inRange(0, _j, N), _Perm(&A[_j], _fracOf(1, 4)));
    loop_invariant _forallS(_inRange(0, _j, N), _Perm(&B[_j], _write));
    loop_invariant _forall (_inRange(0, _j, N), A[_j] == _old<int>(A[_j]));
    loop_invariant _forall (_inRange(0, _j, i), B[A[_j]] == _j);
    @*/
    for (int i = 0; i < N; ++i) {
        B[A[i]] = i;
    }

    // The assert is required to get the correct triggers for the injectivity condition
    /*@
    assert _forall (_and(_inRange(0, _j, N), _inRange(0, _k, N)),  // Injectivity of B
                    _imply(_and(trig(_j), trig(_k)),
                    _imply(_j != _k, B[_j] != B[_k])));
    @*/

    return;
}