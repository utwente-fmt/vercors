#include <sycl/sycl.hpp>


void test() {
	sycl::queue myQueue;

	myQueue.submit(
  	[&](sycl::handler& cgh) {
      sycl::local_accessor<bool, 1> a_local_acc = sycl::local_accessor<bool>(sycl::range<1>(10), cgh);
      sycl::local_accessor<int, 2> b_local_acc = sycl::local_accessor<int>(sycl::range<2>(2, 5), cgh);
      sycl::local_accessor<long, 3> c_local_acc = sycl::local_accessor<long>(sycl::range<3>(2, 5, 1), cgh);
      sycl::local_accessor<double, 1> d_local_acc = sycl::local_accessor<double>(sycl::range<1>(10), cgh);
      sycl::local_accessor<float, 1> e_local_acc = sycl::local_accessor<float>(sycl::range<1>(10), cgh);
      sycl::local_accessor<char, 1> f_local_acc = sycl::local_accessor<char>(sycl::range<1>(10), cgh);

      cgh.parallel_for(sycl::range<1>(1), [=] (sycl::item<1> it) {});
  	}
  );
}