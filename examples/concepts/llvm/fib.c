int fibonacci(int n) {
	if(n > 2) {
		return fibonacci(n - 1) + fibonacci(n - 2);
	}
	return 1;
}