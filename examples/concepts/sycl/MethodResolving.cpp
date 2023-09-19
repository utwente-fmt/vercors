#include <sycl/sycl.hpp>

void test2() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {
		cgh.parallel_for(sycl::range<3>(6,4,2), // global range
		/*@ requires it.get_id(0) > -1;
				ensures 4 + 5 == 9;
		*/
		[=] (sycl::item<3> it) {
		//[kernel code]
			int a = it.get_id(0) + it.get_id(1) + it.get_id(2) + 3;
			int b = a + 45;
			int c = it.get_range(0);
			int d = it.get_linear_id();
		});
	});
	//@ assert (\forall int x, int y; x != y || x == y);
	sycl::event mew = myEvent;
	mew.wait();

	myEvent = myQueue.submit(
  	[&](sycl::handler& cgh) {
  		cgh.parallel_for(sycl::range<3>(6,4,2), // global range
  		/*@ requires it.get_id(0) > -1;
  				ensures it.get_id(0) > -1;
  		*/
  		[=] (sycl::item<3> it) {
  		//[kernel code]
  			int a = it.get_id(0) + it.get_id(1) + it.get_id(2) + 3;
  			int b = a + 45;
  			int c = it.get_range(0);
  			int d = it.get_linear_id();
  		});
  	});

	myEvent.wait();
	mew.wait();



}