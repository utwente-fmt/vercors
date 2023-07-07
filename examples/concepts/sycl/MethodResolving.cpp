#include <sycl/sycl.hpp>

void test2() {
	sycl::queue queue;
	queue.submit(5);
	queue.submit(33);
	int x = queue.test2();
}