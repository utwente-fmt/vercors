/**
 * Implementation for the first problem of the VSTTE'10 competition
 * https://www.macs.hw.ac.uk/vstte10/Competition.html
 * https://www.macs.hw.ac.uk/vstte10/Competition_files/Competition.pdf
 * 
 * Simplified version in which the result-struct is large enough so that 
 * it gets passed and returned as a pointer. 
 * 
 * Transform with -mem2reg option
 * Verify with VerCors-flag --prover-config:smt.arith.solver=6
 */
#include <stdint.h>

/*@
declare using namespace pallasSpec;
@*/

typedef struct {
    int sum;
    int max;
    int64_t dummy1;
    int64_t dummy2;
} SumMaxRes;

// Since we do not support ghost functions yet, this is required as a normal function. 
/*@
pure;
requires arr != nullptr;
requires 0 <= n && n <= _ptrLength(arr);
requires _forallS(_inRange(0, _bv<int>("j"), n), 
                  _Perm(&arr[_bv<int>("j")], _fracOf(1, 100)));
@*/
int arrSum(int *arr, int n) {
  return n == 0 ? 0 : arrSum(arr, n-1) + arr[n-1];  
}

/*@
requires arr != nullptr;
requires 0 <= n && n == _ptrLength(arr);
requires _forallS(_inRange(0, _bv<int>("j"), n),                  // Memory valid
                  _Perm(&arr[_bv<int>("j")], _fracOf(1, 2)));
requires _forall (_inRange(0, _bv<int>("j"), n),                  // Input range
                  arr[_bv<int>("j")] >= 0);
ensures _forallS(_inRange(0, _bv<int>("j"), n),                   // Memory valid
                 _Perm(&arr[_bv<int>("j")], _fracOf(1, 2)));
ensures _forall(_inRange(0, _bv<int>("j"), n),                    // Max & sum are correct
                arr[_bv<int>("j")] <= _result<SumMaxRes>().max);
ensures _imply(n > 0, 
               _exists(_inRange(0, _bv<int>("j"), n), 
                       arr[_bv<int>("j")] == _result<SumMaxRes>().max));
ensures _result<SumMaxRes>().sum == arrSum(arr, n); 
ensures _result<SumMaxRes>().sum <= _result<SumMaxRes>().max * n; 
@*/
SumMaxRes getSumMax(int *arr, int n) {
    int sum = 0;
    int max = 0;

    /*@
    loop_invariant 0 <= i && i <= n;
    loop_invariant _forallS(_inRange(0, _bv<int>("j"), n),
                            _Perm(&arr[_bv<int>("j")], _fracOf(1, 4)));
    loop_invariant _forall (_inRange(0, _bv<int>("j"), n),                // Input range
                            arr[_bv<int>("j")] >= 0);
    loop_invariant _forall (_inRange(0, _bv<int>("j"), n),                // Array not modified
                            arr[_bv<int>("j")] >= _old<int>(arr[_bv<int>("j")]));
    loop_invariant _imply(i == 0, max == 0);
    loop_invariant _imply(_and(i == 1, n > 0), max == arr[0]);
    loop_invariant _forall (_inRange(0, _bv<int>("j"), i),                // Max & sum are valid
                            arr[_bv<int>("j")] <= max);
    loop_invariant _imply(i > 0, _exists(_inRange(0, _bv<int>("j"), i), 
                                         arr[_bv<int>("j")] == max));
    loop_invariant sum == arrSum(arr, i);
    loop_invariant sum <= i * max;
    @*/
    for (int i = 0; i < n; ++i) {
        int e = arr[i];
        if (e > max) {
            max = e;
        }

        sum += e;
    }

    SumMaxRes res = {sum, max, 0, 0};
    return res;
}


