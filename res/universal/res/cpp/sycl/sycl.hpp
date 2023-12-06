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
    ghost
    requires id0 >= 0 && id0 < r0 && id1 >= 0 && id1 < r1;
    requires r0 >= 0 && r1 >= 0;
    ensures \result == id1 + (id0 * r1);
    ensures \result >= 0 && \result < r0 * r1;
    pure int linearize2(int id0, int id1, int r0, int r1);

    ghost
    requires id0 >= 0 && id0 < r0 && id1 >= 0 && id1 < r1 && id2 >= 0 && id2 < r2;
    requires r0 >= 0 && r1 >= 0 && r2 >= 0;
    ensures \result == id2 + (id1 * r2) + (id0 * r1 * r2);
    ensures \result >= 0 && \result < r0 * r1 * r2;
    pure int linearize3(int id0, int id1, int id2, int r0, int r1, int r2);

    ghost
    requires |ids| == |ranges|;
    requires (\forall int i; i >= 0 && i < |ids|; ids[i] >= 0 && ids[i] < ranges[i]);
    ensures |ids| == 1 ==> \result == ids[0];
    ensures |ids| == 2 ==> \result == sycl::linearize2(ids[0], ids[1], ranges[0], ranges[1]);
    ensures |ids| == 3 ==> \result == sycl::linearize3(ids[0], ids[1], ids[2], ranges[0], ranges[1], ranges[2]);
    pure int get_linear_id(seq<int> ids, seq<int> ranges);
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

		/*@
			given seq<int> groupIds;
			given seq<int> localIds;
			given seq<int> groupRanges;
			given seq<int> localRanges;
			requires |groupIds| == |localIds| && |localIds| == |groupRanges| && |groupRanges| == |localRanges|;
			requires dimension >= 0 && dimension < |groupIds|;
			requires groupRanges[dimension] > 0;
			ensures \result == groupIds[dimension] + (localIds[dimension] * groupRanges[dimension]);
			ensures \result >= 0 && \result < sycl::nd_item::get_global_range(dimension) given{groupRanges = groupRanges, localRanges = localRanges};
		@*/
		/*@ pure @*/ int get_global_id(int dimension);

		/*@
			given seq<int> groupRanges;
			given seq<int> localRanges;
			requires |groupRanges| == |localRanges|;
			requires dimension >= 0 && dimension < |groupRanges|;
			ensures \result == groupRanges[dimension] * localRanges[dimension];
		@*/
		/*@ pure @*/ int get_global_range(int dimension);

		/*@
			given seq<int> groupIds;
			given seq<int> localIds;
			given seq<int> groupRanges;
			given seq<int> localRanges;
			requires |groupIds| == |localIds| && |localIds| == |groupRanges| && |groupRanges| == |localRanges|;
			requires (\forall int i; i >= 0 && i < |groupIds|;
			  sycl::nd_item::get_global_id(i) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges, localRanges = localRanges} >= 0 &&
			  sycl::nd_item::get_global_id(i) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges, localRanges = localRanges} < sycl::nd_item::get_global_range(i) given{groupRanges = groupRanges, localRanges = localRanges} &&
			  sycl::nd_item::get_global_range(i) given{groupRanges = groupRanges, localRanges = localRanges} >= 0);
			ensures |groupIds| == 1 ==> \result == sycl::nd_item::get_global_id(0) given {groupIds = groupIds, localIds = localIds, groupRanges = groupRanges, localRanges = localRanges};
			ensures |groupIds| == 2 ==> \result == sycl::linearize2(
				sycl::nd_item::get_global_id(0) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges, localRanges = localRanges},
				sycl::nd_item::get_global_id(1) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges, localRanges = localRanges},
				sycl::nd_item::get_global_range(0) given{groupRanges = groupRanges, localRanges = localRanges},
				sycl::nd_item::get_global_range(1) given{groupRanges = groupRanges, localRanges = localRanges});
			ensures |groupIds| == 3 ==> \result == sycl::linearize3(
				sycl::nd_item::get_global_id(0) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges, localRanges = localRanges},
				sycl::nd_item::get_global_id(1) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges, localRanges = localRanges},
				sycl::nd_item::get_global_id(2) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges, localRanges = localRanges},
				sycl::nd_item::get_global_range(0) given{groupRanges = groupRanges, localRanges = localRanges},
				sycl::nd_item::get_global_range(1) given{groupRanges = groupRanges, localRanges = localRanges},
				sycl::nd_item::get_global_range(2) given{groupRanges = groupRanges, localRanges = localRanges});
		@*/
		/*@ pure @*/ int get_global_linear_id();
  }
  
  namespace accessor {
    sycl::range<1> get_range();
  }

  namespace local_accessor {
    sycl::range<1> get_range();
  }

}