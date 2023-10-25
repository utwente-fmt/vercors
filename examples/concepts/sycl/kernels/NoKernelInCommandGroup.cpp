#include <sycl/sycl.hpp>

void main() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	// There must be a kernel inside a command group
	[&](sycl::handler& cgh) {});
}