#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	// There must be a kernel inside a command group
	[&](sycl::handler& cgh) {});
}