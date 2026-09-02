#include <sycl/sycl.hpp>
/*@ pure */ float f(float value);
/*@ context rows >= 0 && cols > 0;
	context (rows * cols) % cols == 0;
	context \pointer(matrix, rows * cols, write);
	context \pointer(result, cols * rows, write);
	ensures (\forall int r, int c; r >= 0 && r < rows && c >= 0 && c < cols;result[sycl::linearize2(c, r, cols, rows)] ==f(matrix[sycl::linearize2(r, c, rows, cols)]));*/
void transpose_with_f(float matrix[], int rows, int cols, float* result) {
	sycl::queue myQueue;
    {
	sycl::buffer<float, 2> matrix_buffer = sycl::buffer(matrix, sycl::range<2>(rows, cols));
	float temp[rows * cols];
	sycl::buffer<float, 1> temp_buffer = sycl::buffer(temp, sycl::range<1>(rows * cols));
	sycl::event cpKernel = myQueue.submit(
		[&](sycl::handler& cgh) {
			sycl::accessor<float, 2, sycl::access_mode::read> matrix_acc = sycl::accessor(matrix_buffer, cgh, sycl::read_only);
			sycl::accessor<float, 1> temp_acc = sycl::accessor(temp_buffer, cgh, sycl::read_write);
			cgh.parallel_for(sycl::range<2>(rows, cols),
				/*@ context Perm(temp_acc[it.get_linear_id()], write);
					ensures temp_acc[it.get_linear_id()] == {:f(matrix_acc[it.get_id(0)][it.get_id(1)]):}; */
				[=] (sycl::item<2> it) {
					temp_acc[it.get_linear_id()] = f(matrix_acc[it.get_id(0)][it.get_id(1)]);
				}
			);
		}
	);
    cpKernel.wait();
	bool done = false;
		sycl::buffer<float, 2> result_buffer = sycl::buffer(result, sycl::range<2>(cols, rows));
		sycl::event transpose_event = myQueue.submit(
			[&](sycl::handler& cgh) {
				sycl::accessor<float, 1, sycl::access_mode::read> temp_acc = sycl::accessor(temp_buffer, cgh, sycl::read_only);
				sycl::accessor<float, 2> result_acc = sycl::accessor(result_buffer, cgh, sycl::read_write);
				sycl::local_accessor<float, 1> temp_local_acc = sycl::local_accessor<float, 1>(sycl::range<1>(result_acc.get_range().get(0)), cgh);
				cgh.parallel_for(sycl::nd_range(sycl::range(rows*cols), sycl::range(cols)),
					/*@ context Perm(result_acc[it.get_local_id(0)][it.get_group(0)], write);
                        context Perm(temp_local_acc[it.get_local_linear_id()], write);
                        ensures {:result_acc[it.get_local_id(0)][it.get_group(0)]:} == temp_acc[it.get_global_id(0)]; */
					[=] (sycl::nd_item<1> it) {
						int ll_id = it.get_local_linear_id();
						temp_local_acc[ll_id] = temp_acc[it.get_global_id(0)];
						result_acc[it.get_local_id(0)][it.get_group(0)] = temp_local_acc[ll_id];
					}
				);
			}
		);
		transpose_event.wait();
		done = true;
	}
	//@ assert rows > 0 ==> result[sycl::linearize2(0, 0, cols, rows)] == f(matrix[sycl::linearize2(0, 0, rows, cols)]);
}