namespace sycl {

  namespace h {
    /*@
      ensures (a>0 && b>0) ==> \result > 0;
      pure int mul(int a, int b) = a*b;

      ensures (a>0 && b>0) ==> \result > 0;
      pure int add(int a, int b) = a+b;

      requires 0 <= p;
      requires n > 0;
      ensures 0 < p  ==> \result == n * sycl::h::exp(n, p - 1);
      ensures !(0<p) ==> \result == 1;
      ensures \result > 0;
      decreases p;
      pure int exp(int n, int p) = 0 < p ? n * sycl::h::exp(n, p - 1) : 1;

      ensures \result >= 0;
      ensures sycl::h::exp(2,\result) == N;
      pure int logTwo(int N);

      ensures \result > 2;
      pure int wrpsz_pow();

      ensures \result >= 8 && \result == sycl::h::exp(2,sycl::h::wrpsz_pow()) && sycl::h::logTwo(\result) == sycl::h::wrpsz_pow();
      pure int warp_sizes();

    */
  }

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
    requires r0 > 0 && r1 > 0;
    ensures \result == sycl::linearize2formula(id0, id1, r1);
    ensures \result >= 0 && \result < sycl::h::mul(r0,r1) && \result < r0*r1;
    ensures (\forall int ida0, int ida1;
      ida0 >= 0 && ida0 < r0 &&
      ida1 >= 0 && ida1 < r1 &&
      (ida0 != id0 || ida1 != id1) ==>
      \result != {: sycl::linearize2formula(ida0, ida1, r1) :}
    );
    pure int linearize2(int id0, int id1, int r0, int r1);

    ensures \result == id1+id0*r1;
    pure int linearize2formula(int id0, int id1, int r1) = sycl::h::add(id1, sycl::h::mul(id0, r1));

    requires id0 >= 0 && id0 < r0 && id1 >= 0 && id1 < r1 && id2 >= 0 && id2 < r2;
    requires r0 > 0 && r1 > 0 && r2 > 0;
    ensures \result == sycl::linearize3formula(id0, id1, id2, r1, r2);
    ensures \result >= 0 && \result < r0 * r1 * r2;
    ensures (\forall int ida0, int ida1, int ida2;
      ida0 >= 0 && ida0 < r0 &&
      ida1 >= 0 && ida1 < r1 &&
      ida2 >= 0 && ida2 < r2 &&
      (ida0 != id0 || ida1 != id1 || ida2 != id2) ==>
      \result != {: sycl::linearize3formula(ida0, ida1, ida2, r1, r2) :}
    );
    pure int linearize3(int id0, int id1, int id2, int r0, int r1, int r2);

    pure int linearize3formula(int id0, int id1, int id2, int r1, int r2) = id2 + (id1 * r2) + (id0 * r1 * r2);
  */

  namespace item {
		int get_id(int dimension);

  	int get_range(int dimension);

  	int get_linear_id();
  }

  namespace sub_group {
    int get_local_id();   // lane within subgroup

    int get_local_range(int dimension);

    int get_group_id();   // subgroup index within work-group

    int get_group_range();
  }

  namespace nd_item {
		int get_local_id(int dimension);

		int get_local_range(int dimension);

		int get_local_linear_id();

		int get_group(int dimension);

		int get_group_range(int dimension);

		int get_group_linear_id();

		int get_global_id(int dimension);

		//@ pure int get_global_range(int localRange, int groupRange) = localRange * groupRange;

		int get_global_range(int dimension);

		int get_global_linear_id();

        sycl::sub_group get_sub_group();
  }

  namespace accessor {
    sycl::range<1> get_range();
  }

  namespace local_accessor {
    sycl::range<1> get_range();
  }

  int shift_group_left(sycl::sub_group g, int valueToSend, int d);

  int shift_group_right(sycl::sub_group g, int x, int d);

  int group_broadcast(sycl::sub_group g, int x, int id);

}