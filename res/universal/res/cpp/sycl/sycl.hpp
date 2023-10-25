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

  namespace buffer {
    /*@
      ghost
      requires id0 >= 0 && id0 < r0 && id1 >= 0 && id1 < r1;
      requires r0 >= 0 && r1 >= 0;
      ensures \result == id1 + (id0 * r1);
      ensures \result >= 0 && \result < r0 * r1;
      pure int buffer_linearize2(int id0, int id1, int r0, int r1);

      ghost
      requires id0 >= 0 && id0 < r0 && id1 >= 0 && id1 < r1 && id2 >= 0 && id2 < r2;
      requires r0 >= 0 && r1 >= 0 && r2 >= 0;
      ensures \result == id2 + (id1 * r2) + (id0 * r1 * r2);
      ensures \result >= 0 && \result < r0 * r1 * r2;
      pure int buffer_linearize3(int id0, int id1, int id2, int r0, int r1, int r2);
    */

    sycl::buffer<bool, 1> constructor(bool* hostData, sycl::range<1>& bufferRange);
    sycl::buffer<bool, 2> constructor(bool* hostData, sycl::range<2>& bufferRange);
    sycl::buffer<bool, 3> constructor(bool* hostData, sycl::range<3>& bufferRange);

    /*@
      resource exclusive_buffer_access(bool* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires r0 >= 0;
      context \pointer(hostData, r0, write);
      ensures |\result| == r0;
      ensures (\forall int i; i >= 0 && i < r0; {: \result[i] :} == hostData[i]);
      seq<bool> copy_hostdata_to_buffer(bool* hostData, int r0);

      ghost
      requires r0 >= 0 && r1 >= 0;
      context \pointer(hostData, r0 * r1, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1);
      ensures (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: \result[i][j] :} == hostData[sycl::buffer::buffer_linearize2(i, j, r0, r1)]);
      seq<seq<bool>> copy_hostdata_to_buffer(bool* hostData, int r0, int r1);

      ghost
      requires r0 >= 0 && r1 >= 0 && r2 >= 0;
      context \pointer(hostData, r0 * r1 * r2, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1) && (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: |\result[i][j]| :} == r2);
      ensures (\forall int i, int j, int k; i >= 0 && i < r0 && j >= 0 && j < r1 && k >= 0 && k < r2; {: \result[i][j][k] :} == hostData[sycl::buffer::buffer_linearize3(i, j, k, r0, r1, r2)]);
      seq<seq<seq<bool>>> copy_hostdata_to_buffer(bool* hostData, int r0, int r1, int r2);

      ghost
      context \pointer(hostData, |buffer|, write);
      ensures (\forall int i; i >= 0 && i < |buffer|; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(bool* hostData, seq<bool> buffer);

      ghost
      requires |buffer| > 0 ==> (\forall int i; i > 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]|);
      context |buffer| > 0 ==> \pointer(hostData, (|buffer|) * |buffer[0]|, write);
      ensures |buffer| > 0 ==> (\forall int i, int j; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]|; {: hostData[sycl::buffer::buffer_linearize2(i, j, |buffer|, |buffer[0]|)] :} == buffer[i][j]);
      void copy_buffer_to_hostdata(bool* hostData, seq<seq<bool>> buffer);

      ghost
      requires |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i; i >= 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]| && (\forall int j; j >= 0 && j < |buffer[0]|; {: |buffer[i][j]| :} == |buffer[0][0]|));
      context |buffer| > 0 && |buffer[0]| > 0 ==> \pointer(hostData, (|buffer|) * (|buffer[0]|) * |buffer[0][0]|, write);
      ensures |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i, int j, int k; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]| && k >= 0 && k < |buffer[0][0]|;
        {: hostData[sycl::buffer::buffer_linearize3(i, j, k, |buffer|, |buffer[0]|, |buffer[0][0]|)] :} == buffer[i][j][k]);
      void copy_buffer_to_hostdata(bool* hostData, seq<seq<seq<bool>>> buffer);
    */

    sycl::buffer<int, 1> constructor(int* hostData, sycl::range<1>& bufferRange);
    sycl::buffer<int, 2> constructor(int* hostData, sycl::range<2>& bufferRange);
    sycl::buffer<int, 3> constructor(int* hostData, sycl::range<3>& bufferRange);
    /*@
      resource exclusive_buffer_access(int* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires r0 >= 0;
      context \pointer(hostData, r0, write);
      ensures |\result| == r0;
      ensures (\forall int i; i >= 0 && i < r0; {: \result[i] :} == hostData[i]);
      seq<int> copy_hostdata_to_buffer(int* hostData, int r0);

      ghost
      requires r0 >= 0 && r1 >= 0;
      context \pointer(hostData, r0 * r1, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1);
      ensures (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: \result[i][j] :} == hostData[sycl::buffer::buffer_linearize2(i, j, r0, r1)]);
      seq<seq<int>> copy_hostdata_to_buffer(int* hostData, int r0, int r1);

      ghost
      requires r0 >= 0 && r1 >= 0 && r2 >= 0;
      context \pointer(hostData, r0 * r1 * r2, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1) && (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: |\result[i][j]| :} == r2);
      ensures (\forall int i, int j, int k; i >= 0 && i < r0 && j >= 0 && j < r1 && k >= 0 && k < r2; {: \result[i][j][k] :} == hostData[sycl::buffer::buffer_linearize3(i, j, k, r0, r1, r2)]);
      seq<seq<seq<int>>> copy_hostdata_to_buffer(int* hostData, int r0, int r1, int r2);

      ghost
      context \pointer(hostData, |buffer|, write);
      ensures (\forall int i; i >= 0 && i < |buffer|; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(int* hostData, seq<int> buffer);

      ghost
      requires |buffer| > 0 ==> (\forall int i; i > 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]|);
      context |buffer| > 0 ==> \pointer(hostData, (|buffer|) * |buffer[0]|, write);
      ensures |buffer| > 0 ==> (\forall int i, int j; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]|; {: hostData[sycl::buffer::buffer_linearize2(i, j, |buffer|, |buffer[0]|)] :} == buffer[i][j]);
      void copy_buffer_to_hostdata(int* hostData, seq<seq<int>> buffer);

      ghost
      requires |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i; i >= 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]| && (\forall int j; j >= 0 && j < |buffer[0]|; {: |buffer[i][j]| :} == |buffer[0][0]|));
      context |buffer| > 0 && |buffer[0]| > 0 ==> \pointer(hostData, (|buffer|) * (|buffer[0]|) * |buffer[0][0]|, write);
      ensures |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i, int j, int k; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]| && k >= 0 && k < |buffer[0][0]|;
        {: hostData[sycl::buffer::buffer_linearize3(i, j, k, |buffer|, |buffer[0]|, |buffer[0][0]|)] :} == buffer[i][j][k]);
      void copy_buffer_to_hostdata(int* hostData, seq<seq<seq<int>>> buffer);
    */

    sycl::buffer<long, 1> constructor(long* hostData, sycl::range<1>& bufferRange);
    sycl::buffer<long, 2> constructor(long* hostData, sycl::range<2>& bufferRange);
    sycl::buffer<long, 3> constructor(long* hostData, sycl::range<3>& bufferRange);
    /*@
      resource exclusive_buffer_access(long* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires r0 >= 0;
      context \pointer(hostData, r0, write);
      ensures |\result| == r0;
      ensures (\forall int i; i >= 0 && i < r0; {: \result[i] :} == hostData[i]);
      seq<long> copy_hostdata_to_buffer(long* hostData, int r0);

      ghost
      requires r0 >= 0 && r1 >= 0;
      context \pointer(hostData, r0 * r1, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1);
      ensures (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: \result[i][j] :} == hostData[sycl::buffer::buffer_linearize2(i, j, r0, r1)]);
      seq<seq<long>> copy_hostdata_to_buffer(long* hostData, int r0, int r1);

      ghost
      requires r0 >= 0 && r1 >= 0 && r2 >= 0;
      context \pointer(hostData, r0 * r1 * r2, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1) && (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: |\result[i][j]| :} == r2);
      ensures (\forall int i, int j, int k; i >= 0 && i < r0 && j >= 0 && j < r1 && k >= 0 && k < r2; {: \result[i][j][k] :} == hostData[sycl::buffer::buffer_linearize3(i, j, k, r0, r1, r2)]);
      seq<seq<seq<long>>> copy_hostdata_to_buffer(long* hostData, int r0, int r1, int r2);

      ghost
      context \pointer(hostData, |buffer|, write);
      ensures (\forall int i; i >= 0 && i < |buffer|; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(long* hostData, seq<long> buffer);

      ghost
      requires |buffer| > 0 ==> (\forall int i; i > 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]|);
      context |buffer| > 0 ==> \pointer(hostData, (|buffer|) * |buffer[0]|, write);
      ensures |buffer| > 0 ==> (\forall int i, int j; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]|; {: hostData[sycl::buffer::buffer_linearize2(i, j, |buffer|, |buffer[0]|)] :} == buffer[i][j]);
      void copy_buffer_to_hostdata(long* hostData, seq<seq<long>> buffer);

      ghost
      requires |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i; i >= 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]| && (\forall int j; j >= 0 && j < |buffer[0]|; {: |buffer[i][j]| :} == |buffer[0][0]|));
      context |buffer| > 0 && |buffer[0]| > 0 ==> \pointer(hostData, (|buffer|) * (|buffer[0]|) * |buffer[0][0]|, write);
      ensures |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i, int j, int k; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]| && k >= 0 && k < |buffer[0][0]|;
        {: hostData[sycl::buffer::buffer_linearize3(i, j, k, |buffer|, |buffer[0]|, |buffer[0][0]|)] :} == buffer[i][j][k]);
      void copy_buffer_to_hostdata(long* hostData, seq<seq<seq<long>>> buffer);
    */

    sycl::buffer<double, 1> constructor(double* hostData, sycl::range<1>& bufferRange);
    sycl::buffer<double, 2> constructor(double* hostData, sycl::range<2>& bufferRange);
    sycl::buffer<double, 3> constructor(double* hostData, sycl::range<3>& bufferRange);
    /*@
      resource exclusive_buffer_access(double* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires r0 >= 0;
      context \pointer(hostData, r0, write);
      ensures |\result| == r0;
      ensures (\forall int i; i >= 0 && i < r0; {: \result[i] :} == hostData[i]);
      seq<double> copy_hostdata_to_buffer(double* hostData, int r0);

      ghost
      requires r0 >= 0 && r1 >= 0;
      context \pointer(hostData, r0 * r1, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1);
      ensures (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: \result[i][j] :} == hostData[sycl::buffer::buffer_linearize2(i, j, r0, r1)]);
      seq<seq<double>> copy_hostdata_to_buffer(double* hostData, int r0, int r1);

      ghost
      requires r0 >= 0 && r1 >= 0 && r2 >= 0;
      context \pointer(hostData, r0 * r1 * r2, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1) && (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: |\result[i][j]| :} == r2);
      ensures (\forall int i, int j, int k; i >= 0 && i < r0 && j >= 0 && j < r1 && k >= 0 && k < r2; {: \result[i][j][k] :} == hostData[sycl::buffer::buffer_linearize3(i, j, k, r0, r1, r2)]);
      seq<seq<seq<double>>> copy_hostdata_to_buffer(double* hostData, int r0, int r1, int r2);

      ghost
      context \pointer(hostData, |buffer|, write);
      ensures (\forall int i; i >= 0 && i < |buffer|; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(double* hostData, seq<double> buffer);

      ghost
      requires |buffer| > 0 ==> (\forall int i; i > 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]|);
      context |buffer| > 0 ==> \pointer(hostData, (|buffer|) * |buffer[0]|, write);
      ensures |buffer| > 0 ==> (\forall int i, int j; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]|; {: hostData[sycl::buffer::buffer_linearize2(i, j, |buffer|, |buffer[0]|)] :} == buffer[i][j]);
      void copy_buffer_to_hostdata(double* hostData, seq<seq<double>> buffer);

      ghost
      requires |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i; i >= 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]| && (\forall int j; j >= 0 && j < |buffer[0]|; {: |buffer[i][j]| :} == |buffer[0][0]|));
      context |buffer| > 0 && |buffer[0]| > 0 ==> \pointer(hostData, (|buffer|) * (|buffer[0]|) * |buffer[0][0]|, write);
      ensures |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i, int j, int k; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]| && k >= 0 && k < |buffer[0][0]|;
        {: hostData[sycl::buffer::buffer_linearize3(i, j, k, |buffer|, |buffer[0]|, |buffer[0][0]|)] :} == buffer[i][j][k]);
      void copy_buffer_to_hostdata(double* hostData, seq<seq<seq<double>>> buffer);
    */

    sycl::buffer<float, 1> constructor(float* hostData, sycl::range<1>& bufferRange);
    sycl::buffer<float, 2> constructor(float* hostData, sycl::range<2>& bufferRange);
    sycl::buffer<float, 3> constructor(float* hostData, sycl::range<3>& bufferRange);
    /*@
      resource exclusive_buffer_access(float* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires r0 >= 0;
      context \pointer(hostData, r0, write);
      ensures |\result| == r0;
      ensures (\forall int i; i >= 0 && i < r0; {: \result[i] :} == hostData[i]);
      seq<float> copy_hostdata_to_buffer(float* hostData, int r0);

      ghost
      requires r0 >= 0 && r1 >= 0;
      context \pointer(hostData, r0 * r1, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1);
      ensures (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: \result[i][j] :} == hostData[sycl::buffer::buffer_linearize2(i, j, r0, r1)]);
      seq<seq<float>> copy_hostdata_to_buffer(float* hostData, int r0, int r1);

      ghost
      requires r0 >= 0 && r1 >= 0 && r2 >= 0;
      context \pointer(hostData, r0 * r1 * r2, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1) && (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: |\result[i][j]| :} == r2);
      ensures (\forall int i, int j, int k; i >= 0 && i < r0 && j >= 0 && j < r1 && k >= 0 && k < r2; {: \result[i][j][k] :} == hostData[sycl::buffer::buffer_linearize3(i, j, k, r0, r1, r2)]);
      seq<seq<seq<float>>> copy_hostdata_to_buffer(float* hostData, int r0, int r1, int r2);

      ghost
      context \pointer(hostData, |buffer|, write);
      ensures (\forall int i; i >= 0 && i < |buffer|; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(float* hostData, seq<float> buffer);

      ghost
      requires |buffer| > 0 ==> (\forall int i; i > 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]|);
      context |buffer| > 0 ==> \pointer(hostData, (|buffer|) * |buffer[0]|, write);
      ensures |buffer| > 0 ==> (\forall int i, int j; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]|; {: hostData[sycl::buffer::buffer_linearize2(i, j, |buffer|, |buffer[0]|)] :} == buffer[i][j]);
      void copy_buffer_to_hostdata(float* hostData, seq<seq<float>> buffer);

      ghost
      requires |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i; i >= 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]| && (\forall int j; j >= 0 && j < |buffer[0]|; {: |buffer[i][j]| :} == |buffer[0][0]|));
      context |buffer| > 0 && |buffer[0]| > 0 ==> \pointer(hostData, (|buffer|) * (|buffer[0]|) * |buffer[0][0]|, write);
      ensures |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i, int j, int k; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]| && k >= 0 && k < |buffer[0][0]|;
        {: hostData[sycl::buffer::buffer_linearize3(i, j, k, |buffer|, |buffer[0]|, |buffer[0][0]|)] :} == buffer[i][j][k]);
      void copy_buffer_to_hostdata(float* hostData, seq<seq<seq<float>>> buffer);
    */

    sycl::buffer<char, 1> constructor(char* hostData, sycl::range<1>& bufferRange);
    sycl::buffer<char, 2> constructor(char* hostData, sycl::range<2>& bufferRange);
    sycl::buffer<char, 3> constructor(char* hostData, sycl::range<3>& bufferRange);
    /*@
      resource exclusive_buffer_access(char* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires r0 >= 0;
      context \pointer(hostData, r0, write);
      ensures |\result| == r0;
      ensures (\forall int i; i >= 0 && i < r0; {: \result[i] :} == hostData[i]);
      seq<char> copy_hostdata_to_buffer(char* hostData, int r0);

      ghost
      requires r0 >= 0 && r1 >= 0;
      context \pointer(hostData, r0 * r1, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1);
      ensures (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: \result[i][j] :} == hostData[sycl::buffer::buffer_linearize2(i, j, r0, r1)]);
      seq<seq<char>> copy_hostdata_to_buffer(char* hostData, int r0, int r1);

      ghost
      requires r0 >= 0 && r1 >= 0 && r2 >= 0;
      context \pointer(hostData, r0 * r1 * r2, write);
      ensures |\result| == r0 && (\forall int i; i >= 0 && i < r0; {: |\result[i]| :} == r1) && (\forall int i, int j; i >= 0 && i < r0 && j >= 0 && j < r1; {: |\result[i][j]| :} == r2);
      ensures (\forall int i, int j, int k; i >= 0 && i < r0 && j >= 0 && j < r1 && k >= 0 && k < r2; {: \result[i][j][k] :} == hostData[sycl::buffer::buffer_linearize3(i, j, k, r0, r1, r2)]);
      seq<seq<seq<char>>> copy_hostdata_to_buffer(char* hostData, int r0, int r1, int r2);

      ghost
      context \pointer(hostData, |buffer|, write);
      ensures (\forall int i; i >= 0 && i < |buffer|; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(char* hostData, seq<char> buffer);

      ghost
      requires |buffer| > 0 ==> (\forall int i; i > 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]|);
      context |buffer| > 0 ==> \pointer(hostData, (|buffer|) * |buffer[0]|, write);
      ensures |buffer| > 0 ==> (\forall int i, int j; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]|; {: hostData[sycl::buffer::buffer_linearize2(i, j, |buffer|, |buffer[0]|)] :} == buffer[i][j]);
      void copy_buffer_to_hostdata(char* hostData, seq<seq<char>> buffer);

      ghost
      requires |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i; i >= 0 && i < |buffer|; {: |buffer[i]| :} == |buffer[0]| && (\forall int j; j >= 0 && j < |buffer[0]|; {: |buffer[i][j]| :} == |buffer[0][0]|));
      context |buffer| > 0 && |buffer[0]| > 0 ==> \pointer(hostData, (|buffer|) * (|buffer[0]|) * |buffer[0][0]|, write);
      ensures |buffer| > 0 && |buffer[0]| > 0 ==> (\forall int i, int j, int k; i >= 0 && i < |buffer| && j >= 0 && j < |buffer[0]| && k >= 0 && k < |buffer[0][0]|;
        {: hostData[sycl::buffer::buffer_linearize3(i, j, k, |buffer|, |buffer[0]|, |buffer[0][0]|)] :} == buffer[i][j][k]);
      void copy_buffer_to_hostdata(char* hostData, seq<seq<seq<char>>> buffer);
    */


  }
  
//  namespace accessor {
//    sycl::accessor<bool, 1> constructor(sycl::buffer<bool, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<bool, 2> constructor(sycl::buffer<bool, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<bool, 3> constructor(sycl::buffer<bool, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//
//    sycl::accessor<int, 1> constructor(sycl::buffer<int, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<int, 2> constructor(sycl::buffer<int, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<int, 3> constructor(sycl::buffer<int, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//
//    sycl::accessor<long, 1> constructor(sycl::buffer<long, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<long, 2> constructor(sycl::buffer<long, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<long, 3> constructor(sycl::buffer<long, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//
//    sycl::accessor<double, 1> constructor(sycl::buffer<double, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<double, 2> constructor(sycl::buffer<double, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<double, 3> constructor(sycl::buffer<double, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//
//    sycl::accessor<float, 1> constructor(sycl::buffer<float, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<float, 2> constructor(sycl::buffer<float, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<float, 3> constructor(sycl::buffer<float, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//
//    sycl::accessor<char, 1> constructor(sycl::buffer<char, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<char, 2> constructor(sycl::buffer<char, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//    sycl::accessor<char, 3> constructor(sycl::buffer<char, 1> bufferRef, sycl::handler& commandGroupHandlerRef, VERCORS::ACCESS_MODE);
//  }

}