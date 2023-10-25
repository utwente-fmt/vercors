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

	// See SYCL spec 3.11.1 for the linearization formulas
  //@ pure int linearize2(int id0, int id1, int r1) = id1 + (id0 * r1);
  //@ pure int linearize3(int id0, int id1, int id2, int r1, int r2) = id2 + (id1 * r2) + (id0 * r1 * r2);

  namespace item {
		/*@
			given seq<int> ids;
			requires dimension >= 0;
			requires dimension < |ids|;
			ensures \result == ids[dimension];
  	@*/
  	/*@ pure @*/ int get_id(int dimension);

  	/*@
			given seq<int> ranges;
			requires dimension >= 0;
			requires dimension < |ranges|;
			ensures \result == ranges[dimension];
		@*/
		/*@ pure @*/ int get_range(int dimension);

  	/*@
			given seq<int> ids;
			given seq<int> ranges;
			requires |ids| == |ranges|;
			ensures |ids| == 1 ==> \result == ids[0];
			ensures |ids| == 2 ==> \result == sycl::linearize2(
				sycl::item::get_id(0) given{ids = ids},
				sycl::item::get_id(1) given{ids = ids},
				sycl::item::get_range(1) given{ranges = ranges});
			ensures |ids| == 3 ==> \result == sycl::linearize3(
				sycl::item::get_id(0) given{ids = ids},
				sycl::item::get_id(1) given{ids = ids},
				sycl::item::get_id(2) given{ids = ids},
				sycl::item::get_range(1) given{ranges = ranges},
				sycl::item::get_range(2) given{ranges = ranges});
		@*/
  	/*@ pure @*/ int get_linear_id();
  }

  namespace nd_item {

		/*@
			given seq<int> ids;
			requires dimension >= 0;
			requires dimension < |ids|;
			ensures \result == ids[dimension];
		@*/
		/*@ pure @*/ int get_local_id(int dimension);

		/*@
			given seq<int> ranges;
			requires dimension >= 0;
			requires dimension < |ranges|;
			ensures \result == ranges[dimension];
		@*/
		/*@ pure @*/ int get_local_range(int dimension);

		/*@
			given seq<int> ids;
			given seq<int> ranges;
			requires |ids| == |ranges|;
			ensures |ids| == 1 ==> \result == ids[0];
			ensures |ids| == 2 ==> \result == sycl::linearize2(
				sycl::nd_item::get_local_id(0) given{ids = ids},
				sycl::nd_item::get_local_id(1) given{ids = ids},
				sycl::nd_item::get_local_range(1) given{ranges = ranges});
			ensures |ids| == 3 ==> \result == sycl::linearize3(
				sycl::nd_item::get_local_id(0) given{ids = ids},
				sycl::nd_item::get_local_id(1) given{ids = ids},
				sycl::nd_item::get_local_id(2) given{ids = ids},
				sycl::nd_item::get_local_range(1) given{ranges = ranges},
				sycl::nd_item::get_local_range(2) given{ranges = ranges});
		@*/
		/*@ pure @*/ int get_local_linear_id();

		/*@
			given seq<int> ids;
			requires dimension >= 0;
			requires dimension < |ids|;
			ensures \result == ids[dimension];
		@*/
		/*@ pure @*/ int get_group_id(int dimension);

		/*@
			given seq<int> ranges;
			requires dimension >= 0;
			requires dimension < |ranges|;
			ensures \result == ranges[dimension];
		@*/
		/*@ pure @*/ int get_group_range(int dimension);

		/*@
			given seq<int> ids;
			given seq<int> ranges;
			requires |ids| == |ranges|;
			ensures |ids| == 1 ==> \result == ids[0];
			ensures |ids| == 2 ==> \result == sycl::linearize2(
				sycl::nd_item::get_group_id(0) given{ids = ids},
				sycl::nd_item::get_group_id(1) given{ids = ids},
				sycl::nd_item::get_group_range(1) given{ranges = ranges});
			ensures |ids| == 3 ==> \result == sycl::linearize3(
				sycl::nd_item::get_group_id(0) given{ids = ids},
				sycl::nd_item::get_group_id(1) given{ids = ids},
				sycl::nd_item::get_group_id(2) given{ids = ids},
				sycl::nd_item::get_group_range(1) given{ranges = ranges},
				sycl::nd_item::get_group_range(2) given{ranges = ranges});
		@*/
		/*@ pure @*/ int get_group_linear_id();


		/*@
			given seq<int> groupIds;
			given seq<int> localIds;
			given seq<int> groupRanges;
			requires |groupIds| == |localIds| && |localIds| == |groupRanges|;
			requires dimension >= 0;
			requires dimension < |groupIds|;
			ensures \result == groupIds[dimension] + (localIds[dimension] * groupRanges[dimension]);
		@*/
		/*@ pure @*/ int get_global_id(int dimension);

		/*@
			given seq<int> groupRanges;
			given seq<int> localRanges;
			requires |groupRanges| == |localRanges|;
			requires dimension >= 0;
			requires dimension < |groupRanges|;
			ensures \result == groupRanges[dimension] * localRanges[dimension];
		@*/
		/*@ pure @*/ int get_global_range(int dimension);

		/*@
			given seq<int> groupIds;
			given seq<int> localIds;
			given seq<int> groupRanges;
			given seq<int> localRanges;
			requires |groupIds| == |localIds| && |localIds| == |groupRanges| && |groupRanges| == |localRanges|;
			ensures |groupIds| == 1 ==> \result == sycl::nd_item::get_global_id(0) given {groupIds = groupIds, localIds = localIds, groupRanges = groupRanges};
			ensures |groupIds| == 2 ==> \result == sycl::linearize2(
				sycl::nd_item::get_global_id(0) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges},
				sycl::nd_item::get_global_id(1) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges},
				sycl::nd_item::get_global_range(1) given{groupRanges = groupRanges, localRanges = localRanges});
			ensures |groupIds| == 3 ==> \result == sycl::linearize3(
				sycl::nd_item::get_global_id(0) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges},
				sycl::nd_item::get_global_id(1) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges},
				sycl::nd_item::get_global_id(2) given{groupIds = groupIds, localIds = localIds, groupRanges = groupRanges},
				sycl::nd_item::get_global_range(1) given{groupRanges = groupRanges, localRanges = localRanges},
				sycl::nd_item::get_global_range(2) given{groupRanges = groupRanges, localRanges = localRanges});
		@*/
		/*@ pure @*/ int get_global_linear_id();
  }

}