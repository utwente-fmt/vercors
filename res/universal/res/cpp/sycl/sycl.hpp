namespace sycl {

	namespace event {
  	void wait();
  }


	namespace queue {
		sycl::event submit(VERCORS::LAMBDA lambda_method);
		void wait();
	}

	namespace range {
		sycl::range<1> constructor_1(int dim0);
		sycl::range<2> constructor_2(int dim0, int dim1);
		sycl::range<3> constructor_3(int dim0, int dim1, int dim2);
	}

	namespace handler {
		void parallel_for(sycl::range<1> numWorkItems, VERCORS::LAMBDA lambda_method);
		void parallel_for(sycl::range<2> numWorkItems, VERCORS::LAMBDA lambda_method);
		void parallel_for(sycl::range<3> numWorkItems, VERCORS::LAMBDA lambda_method);
  }

  namespace item {
  	/*@ pure @*/ int get_id(int dimension);
  }

}