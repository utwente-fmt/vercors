// Transform with mem2reg-option
// Verify with VerCors-flag --pallas-sroa

/*@
requires 1 <= m1 &&& m1 <= 12;
requires 1 <= d1 &&& d1 <= 31;
requires 1 <= m2 &&& m2 <= 12;
requires 1 <= d2 &&& d2 <= 31;
ensures y1 > y2 ==> _result() == true;
ensures (y1 == y2 &&& m1 == m2) ==> (_result() == (d1 > d2));
@*/
func later(_ y1: Int, _ m1: Int, _ d1: Int,
           _ y2: Int, _ m2: Int, _ d2: Int) -> Bool {
    if (y1 != y2) {
        return y1 > y2
    } else if (m1 != m2) {
        return m1 > m2
    } else {
        return d1 > d2
    }
}

func test() -> Int {
    _ = later(2023, 03, 07,
              2023, 01, 01)
    _ = later(01, 01, 2023,
              15, 03, 2023)
    return 0;
}