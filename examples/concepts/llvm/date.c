#include "stdbool.h"

bool isLater(
	int y1, int m1, int d1,
	int y2, int m2, int d2)
{
	if (y1 != y2) {
		return y1 > y2;
	} else if (m1 != m2) {
		return m1 > m2;
	} else {
		return d1 > d2;
	}
}

int test() {
	isLater(
		2023, 03, 07,
		2020, 01, 01
	);
	isLater(
		01, 01, 2023,
		15, 03, 2023
	);
	return 0;
}