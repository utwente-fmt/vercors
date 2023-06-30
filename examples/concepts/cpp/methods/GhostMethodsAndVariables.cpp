// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases GhostMethodsAndVariables
//:: tools silicon
//:: verdict Pass

/*@ ghost
	ensures \result == (current && a >= 0);
	bool isPositive(bool current, int a) {
		return current && a >= 0;
	}
*/

//@ context_everywhere \pointer(a, size, read);
int sum(int a[], int size) {
	//@ ghost bool positive = true;
	int result = 0;
	//@ loop_invariant i >= 0 && i <= size;
	//@ loop_invariant positive ==> (\forall int j = 0 .. i; a[j] >= 0);
	//@ loop_invariant (\forall int j = 0 .. i; a[j] >= 0) ==> result >= 0;
	for(int i = 0; i < size; i++) {
		//@ ghost positive = isPositive(positive, a[i]);
		result += a[i];
	}
	//@ assert positive ==> result >= 0;
	return result;
}
