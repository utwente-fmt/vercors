#include <sycl/sycl.hpp>

// Helper function which calculates a linear index out a row and column
/*@
	requires row >= 0 && row < rows && col >= 0 && col < cols;
	ensures \result == col + (row * cols);
	ensures \result >= 0 && \result < rows * cols;
	pure int calc_index(int row, int col, int rows, int cols);
*/

// The function to apply to every element of matrix
/*@ pure */ float f(float value);

/*@
	context rows >= 0 && cols > 0;
	context (rows * cols) % cols == 0;
	context \pointer(matrix, rows * cols, write);
	context \pointer(result, cols * rows, write);
	ensures (\forall int r, int c;
		r >= 0 && r < rows && c >= 0 && c < cols;
		result[calc_index(c, r, cols, rows)] ==
		f(matrix[calc_index(r, c, rows, cols)])
	);
*/
void transpose_with_f(float matrix[], int rows, int cols, float* result) {
	sycl::queue myQueue;

	sycl::buffer<float, 2> matrix_buffer = sycl::buffer(matrix, sycl::range<2>(rows, cols));
	// Create a buffer to share results of the first kernel with the second one
	float temp[rows * cols];
	sycl::buffer<float, 1> temp_buffer = sycl::buffer(temp, sycl::range<1>(rows * cols));

	// Apply f to all elements of matrix and store results in temp_buffer
	myQueue.submit(
		[&](sycl::handler& cgh) {
			sycl::accessor<float, 2, sycl::access_mode::read> matrix_acc = sycl::accessor(matrix_buffer, cgh, sycl::read_only);
			sycl::accessor<float, 1> temp_acc = sycl::accessor(temp_buffer, cgh, sycl::read_write);

			cgh.parallel_for(sycl::range<2>(rows, cols),
				/*@
					context it.get_id(0) < matrix_acc.get_range().get(0);
					context it.get_id(1) < matrix_acc.get_range().get(1);
					context Perm(matrix_acc[it.get_id(0)][it.get_id(1)], read);
					context it.get_linear_id() < temp_acc.get_range().get(0);
					context Perm(temp_acc[it.get_linear_id()], write);
					ensures temp_acc[it.get_linear_id()] == f(matrix_acc[it.get_id(0)][it.get_id(1)]);
				*/
				[=] (sycl::item<2> it) {
					temp_acc[it.get_linear_id()] = f(matrix_acc[it.get_id(0)][it.get_id(1)]);
				}
			);
		}
	);

	bool done = false;
	{
		sycl::buffer<float, 2> result_buffer =
			sycl::buffer(result, sycl::range<2>(cols, rows));

		// Transpose the results of the first kernel and store it in result_buffer
		// Implicitly waits on previous kernel as that kernel writes to temp_buffer
		//	and this kernel needs read access to temp_buffer
		sycl::event transpose_event = myQueue.submit(
			[&](sycl::handler& cgh) {
				sycl::accessor<float, 1, sycl::access_mode::read> temp_acc = sycl::accessor(temp_buffer, cgh, sycl::read_only);
				sycl::accessor<float, 2> result_acc = sycl::accessor(result_buffer, cgh, sycl::read_write);
				// Cannot use cols variable here, so use result_acc.get_range().get(0)
				sycl::local_accessor<float, 1> temp_local_acc = sycl::local_accessor<float, 1>(sycl::range<1>(result_acc.get_range().get(0)), cgh);

				cgh.parallel_for(sycl::nd_range(sycl::range(rows*cols), sycl::range(cols)),
					/*@
					  context it.get_global_id(0) < temp_acc.get_range().get(0);
            context Perm(temp_acc[it.get_global_id(0)], read);
            context it.get_local_id(0) < result_acc.get_range().get(0);
            context it.get_group(0) < result_acc.get_range().get(1);
            context Perm(result_acc[it.get_local_id(0)][it.get_group(0)], write);
            context it.get_local_linear_id() < temp_local_acc.get_range().get(0);
            context Perm(temp_local_acc[it.get_local_linear_id()], write);
            ensures result_acc[it.get_local_id(0)][it.get_group(0)] == temp_acc[it.get_global_id(0)];
					*/
					[=] (sycl::nd_item<1> it) {
						int ll_id = it.get_local_linear_id();
						temp_local_acc[ll_id] = temp_acc[it.get_global_id(0)];
						result_acc[it.get_local_id(0)][it.get_group(0)] = temp_local_acc[ll_id];
					}
				);
			}
		);
		// Explicitly wait till the whole matrix has been transposed
		transpose_event.wait();
		done = true;
	}
	// result_buffer has been destroyed so result is accessible again
	//@ assert rows > 0 ==> result[calc_index(0, 0, cols, rows)] == f(matrix[calc_index(0, 0, rows, cols)]);
}