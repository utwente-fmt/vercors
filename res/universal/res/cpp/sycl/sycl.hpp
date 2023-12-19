namespace sycl {

	namespace event {
  	void wait();
  }

	namespace queue {
		sycl::event submit(VERCORS::LAMBDA lambda_method);
	}

	namespace handler {
		void parallel_for(sycl::range<1> numWorkItems, VERCORS::LAMBDA lambda_method);
		void parallel_for(sycl::range<2> numWorkItems, VERCORS::LAMBDA lambda_method);
		void parallel_for(sycl::range<3> numWorkItems, VERCORS::LAMBDA lambda_method);

		void parallel_for(sycl::nd_range<1> numWorkItems, VERCORS::LAMBDA lambda_method);
    void parallel_for(sycl::nd_range<2> numWorkItems, VERCORS::LAMBDA lambda_method);
    void parallel_for(sycl::nd_range<3> numWorkItems, VERCORS::LAMBDA lambda_method);
  }

  namespace range {
    int get(int dimension);
  }

  // See SYCL spec 3.11.1 for the linearization formulas
  /*@
    requires id0 >= 0 && id0 < r0 && id1 >= 0 && id1 < r1;
    requires r0 >= 0 && r1 >= 0;
    ensures \result == id1 + (id0 * r1);
    ensures \result >= 0 && \result < r0 * r1;
    pure int linearize2(int id0, int id1, int r0, int r1);

    requires id0 >= 0 && id0 < r0 && id1 >= 0 && id1 < r1 && id2 >= 0 && id2 < r2;
    requires r0 >= 0 && r1 >= 0 && r2 >= 0;
    ensures \result == id2 + (id1 * r2) + (id0 * r1 * r2);
    ensures \result >= 0 && \result < r0 * r1 * r2;
    pure int linearize3(int id0, int id1, int id2, int r0, int r1, int r2);

    pure bool linearize2_is_injective(int r0, int r1) = (\forall
      int ida0, int idb0, int ida1, int idb1;
      ida0 >= 0 && ida0 < r0 && idb0 >= 0 && idb0 < r0 &&
      ida1 >= 0 && ida1 < r1 && idb1 >= 0 && idb1 < r1 &&
      (ida0 != idb0 || ida1 != idb1) ==>
      {: sycl::linearize2(ida0, ida1, r0, r1) :} != {: sycl::linearize2(idb0, idb1, r0, r1) :}
    );

    pure bool linearize3_is_injective(int r0, int r1, int r2) = (\forall
      int ida0, int idb0, int ida1, int idb1, int ida2, int idb2;
      ida0 >= 0 && ida0 < r0 && idb0 >= 0 && idb0 < r0 &&
      ida1 >= 0 && ida1 < r1 && idb1 >= 0 && idb1 < r1 &&
      ida2 >= 0 && ida2 < r2 && idb2 >= 0 && idb2 < r2 &&
      (ida0 != idb0 || ida1 != idb1 || ida2 != idb2) ==>
      {: sycl::linearize3(ida0, ida1, ida2, r0, r1, r2) :} != {: sycl::linearize3(idb0, idb1, idb2, r0, r1, r2) :}
    );
  */

  namespace item {
		int get_id(int dimension);

  	int get_range(int dimension);

  	int get_linear_id();
  }

  namespace nd_item {
		int get_local_id(int dimension);

		int get_local_range(int dimension);

		int get_local_linear_id();

		int get_group_id(int dimension);

		int get_group_range(int dimension);

		int get_group_linear_id();

		int get_global_id(int dimension);

		//@ pure int get_global_range(int localRange, int globalRange) = localRange * globalRange;

		int get_global_range(int dimension);

		int get_global_linear_id();
  }
  
  namespace accessor {
    sycl::range<1> get_range();
  }

  namespace local_accessor {
    sycl::range<1> get_range();
  }

}