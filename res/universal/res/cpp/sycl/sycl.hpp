namespace sycl {

	namespace event {
  	void wait();
  }

	namespace queue {
		sycl::event submit(VERCORS::LAMBDA lambda_method);
	}

	namespace range {
		sycl::range<1> constructor(int dim0);
		sycl::range<2> constructor(int dim0, int dim1);
		sycl::range<3> constructor(int dim0, int dim1, int dim2);
	}

	namespace nd_range {
		sycl::nd_range<1> constructor(sycl::range<1> globalSize, sycl::range<1> localSize);
		sycl::nd_range<2> constructor(sycl::range<2> globalSize, sycl::range<2> localSize);
		sycl::nd_range<3> constructor(sycl::range<3> globalSize, sycl::range<3> localSize);
	}

	namespace handler {
		void parallel_for(sycl::range<1> numWorkItems, VERCORS::LAMBDA lambda_method);
		void parallel_for(sycl::range<2> numWorkItems, VERCORS::LAMBDA lambda_method);
		void parallel_for(sycl::range<3> numWorkItems, VERCORS::LAMBDA lambda_method);

		void parallel_for(sycl::nd_range<1> numWorkItems, VERCORS::LAMBDA lambda_method);
    void parallel_for(sycl::nd_range<2> numWorkItems, VERCORS::LAMBDA lambda_method);
    void parallel_for(sycl::nd_range<3> numWorkItems, VERCORS::LAMBDA lambda_method);
  }

  namespace item {
  	/*@ pure @*/ int get_id(int dimension);
  	/*@ pure @*/ int get_linear_id();
  	/*@ pure @*/ int get_range(int dimension);
  }

  namespace nd_item {
		/*@ pure @*/ int get_global_id(int dimension);
		/*@ pure @*/ int get_global_linear_id();
		/*@ pure @*/ int get_global_range(int dimension);

		/*@ pure @*/ int get_local_id(int dimension);
		/*@ pure @*/ int get_local_linear_id();
		/*@ pure @*/ int get_local_range(int dimension);

		/*@ pure @*/ int get_group_id(int dimension);
		/*@ pure @*/ int get_group_linear_id();
		/*@ pure @*/ int get_group_range(int dimension);
  }

}