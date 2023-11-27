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
      resource exclusive_hostData_access(bool* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires size >= 0;
      context \pointer(hostData, size, write);
      ensures \array(\result, size) ** Perm(\result[*], write);
      ensures (\forall int i; i >= 0 && i < size; {: \result[i] :} == hostData[i]);
      VERCORS::ARRAY<bool, 1> copy_hostdata_to_buffer(bool* hostData, int size);

      ghost
      context \array(buffer, buffer.length) ** Perm(buffer[*], read);
      context sycl::buffer::exclusive_hostData_access(hostData, buffer.length);
      ensures \unfolding sycl::buffer::exclusive_hostData_access(hostData, buffer.length) \in (\forall int i; i >= 0 && i < buffer.length; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(bool* hostData, VERCORS::ARRAY<bool, 1> buffer);
    */

    /*@
      resource exclusive_hostData_access(int* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires size >= 0;
      context \pointer(hostData, size, write);
      ensures \array(\result, size) ** Perm(\result[*], write);
      ensures (\forall int i; i >= 0 && i < size; {: \result[i] :} == hostData[i]);
      VERCORS::ARRAY<int, 1> copy_hostdata_to_buffer(int* hostData, int size);

      ghost
      context \array(buffer, buffer.length) ** Perm(buffer[*], read);
      context sycl::buffer::exclusive_hostData_access(hostData, buffer.length);
      ensures \unfolding sycl::buffer::exclusive_hostData_access(hostData, buffer.length) \in (\forall int i; i >= 0 && i < buffer.length; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(int* hostData, VERCORS::ARRAY<int, 1> buffer);
    */

    /*@
      resource exclusive_hostData_access(long* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires size >= 0;
      context \pointer(hostData, size, write);
      ensures \array(\result, size) ** Perm(\result[*], write);
      ensures (\forall int i; i >= 0 && i < size; {: \result[i] :} == hostData[i]);
      VERCORS::ARRAY<long, 1> copy_hostdata_to_buffer(long* hostData, int size);

      ghost
      context \array(buffer, buffer.length) ** Perm(buffer[*], read);
      context sycl::buffer::exclusive_hostData_access(hostData, buffer.length);
      ensures \unfolding sycl::buffer::exclusive_hostData_access(hostData, buffer.length) \in (\forall int i; i >= 0 && i < buffer.length; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(long* hostData, VERCORS::ARRAY<long, 1> buffer);
    */

    /*@
      resource exclusive_hostData_access(double* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires size >= 0;
      context \pointer(hostData, size, write);
      ensures \array(\result, size) ** Perm(\result[*], write);
      ensures (\forall int i; i >= 0 && i < size; {: \result[i] :} == hostData[i]);
      VERCORS::ARRAY<double, 1> copy_hostdata_to_buffer(double* hostData, int size);

      ghost
      context \array(buffer, buffer.length) ** Perm(buffer[*], read);
      context sycl::buffer::exclusive_hostData_access(hostData, buffer.length);
      ensures \unfolding sycl::buffer::exclusive_hostData_access(hostData, buffer.length) \in (\forall int i; i >= 0 && i < buffer.length; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(double* hostData, VERCORS::ARRAY<double, 1> buffer);
    */

    /*@
      resource exclusive_hostData_access(float* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires size >= 0;
      context \pointer(hostData, size, write);
      ensures \array(\result, size) ** Perm(\result[*], write);
      ensures (\forall int i; i >= 0 && i < size; {: \result[i] :} == hostData[i]);
      VERCORS::ARRAY<float, 1> copy_hostdata_to_buffer(float* hostData, int size);

      ghost
      context \array(buffer, buffer.length) ** Perm(buffer[*], read);
      context sycl::buffer::exclusive_hostData_access(hostData, buffer.length);
      ensures \unfolding sycl::buffer::exclusive_hostData_access(hostData, buffer.length) \in (\forall int i; i >= 0 && i < buffer.length; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(float* hostData, VERCORS::ARRAY<float, 1> buffer);
    */

    /*@
      resource exclusive_hostData_access(char* hostData, int range) = \pointer(hostData, range, write);

      ghost
      requires size >= 0;
      context \pointer(hostData, size, write);
      ensures \array(\result, size) ** Perm(\result[*], write);
      ensures (\forall int i; i >= 0 && i < size; {: \result[i] :} == hostData[i]);
      VERCORS::ARRAY<char, 1> copy_hostdata_to_buffer(char* hostData, int size);

      ghost
      context \array(buffer, buffer.length) ** Perm(buffer[*], read);
      context sycl::buffer::exclusive_hostData_access(hostData, buffer.length);
      ensures \unfolding sycl::buffer::exclusive_hostData_access(hostData, buffer.length) \in (\forall int i; i >= 0 && i < buffer.length; {: hostData[i] :} == buffer[i]);
      void copy_buffer_to_hostdata(char* hostData, VERCORS::ARRAY<char, 1> buffer);
    */
  }
  
  namespace accessor {
    /*@
      ghost
      requires id0 >= 0 && id0 < r0 && id1 >= 0 && id1 < r1;
      requires r0 >= 0 && r1 >= 0;
      ensures \result == id1 + (id0 * r1);
      ensures \result >= 0 && \result < r0 * r1;
      pure int linearize_2_indices(int id0, int id1, int r0, int r1);

      ghost
      requires id0 >= 0 && id0 < r0 && id1 >= 0 && id1 < r1 && id2 >= 0 && id2 < r2;
      requires r0 >= 0 && r1 >= 0 && r2 >= 0;
      ensures \result == id2 + (id1 * r2) + (id0 * r1 * r2);
      ensures \result >= 0 && \result < r0 * r1 * r2;
      pure int linearize_3_indices(int id0, int id1, int id2, int r0, int r1, int r2);
    */

    sycl::range<1> get_range();
    sycl::range<2> get_range();
    sycl::range<3> get_range();
  }

}