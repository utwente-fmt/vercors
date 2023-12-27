#include <sycl/sycl.hpp>

// helper function to be used in verification statements
/*@
  requires id0 >= 0 && id0 < r0 && id1 >= 0 && id1 < r1;
  requires r0 >= 0 && r1 >= 0;
  ensures \result == id1 + (id0 * r1);
  ensures \result >= 0 && \result < r0 * r1;
  pure int linearize(int id0, int id1, int r0, int r1);
*/

//@ pure
float f(float value);

/*@
  context row_size >= 0 && col_size > 0;
  context (row_size * col_size) % col_size == 0;
  context \pointer(matrix, row_size * col_size, write);
  context \pointer(result, col_size * row_size, write);
  ensures (\forall int r, int c; r >= 0 && r < row_size && c >= 0 && c < col_size;
    result[linearize(c, r, col_size, row_size)] == f(matrix[linearize(r, c, row_size, col_size)])
  );
*/
void transpose_with_f(float matrix[], int row_size, int col_size, float* result) {
	sycl::queue myQueue;

	float temp[row_size * col_size];

  sycl::buffer<float, 2> matrix_buffer = sycl::buffer(matrix, sycl::range<2>(row_size, col_size));
  sycl::buffer<float, 1> temp_buffer = sycl::buffer(temp, sycl::range<1>(row_size * col_size));

  // Apply f to all elements of matrix and store in temp
  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::accessor<float, 2, sycl::access_mode::read> matrix_acc = sycl::accessor(matrix_buffer, cgh, sycl::read_only);
      sycl::accessor<float, 1> temp_acc = sycl::accessor(temp_buffer, cgh, sycl::read_write);

      cgh.parallel_for(sycl::range<2>(row_size, col_size),
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
    sycl::buffer<float, 2> result_buffer = sycl::buffer(result, sycl::range<2>(col_size, row_size));
    // Transpose matrix and store in result
    // Implicitly waits on previous kernel as that kernel writes to temp_buffer and this kernel needs read access to temp_buffer
    sycl::event transpose_event = myQueue.submit(
      [&](sycl::handler& cgh) {
        sycl::accessor<float, 1, sycl::access_mode::read> temp_acc = sycl::accessor(temp_buffer, cgh, sycl::read_only);
        sycl::accessor<float, 2> result_acc = sycl::accessor(result_buffer, cgh, sycl::read_write);


        // result_acc.get_range().get(0) == col_size, cannot use col_size itself here
        sycl::local_accessor<float, 1> temp_local_acc = sycl::local_accessor<float, 1>(sycl::range<1>(result_acc.get_range().get(0)), cgh);

        cgh.parallel_for(sycl::nd_range<1>(sycl::range<1>(row_size * col_size), sycl::range<1>(col_size)),
          /*@
            context it.get_global_id(0) < temp_acc.get_range().get(0);
            context Perm(temp_acc[it.get_global_id(0)], read);
            context it.get_local_id(0) < result_acc.get_range().get(0);
            context it.get_group(0) < result_acc.get_range().get(1);
            context Perm(result_acc[it.get_local_id(0)][it.get_group(0)], write);
            context Perm(temp_local_acc[it.get_local_linear_id()], write);
            ensures result_acc[it.get_local_id(0)][it.get_group(0)] == temp_acc[it.get_global_id(0)];
          */
          [=] (sycl::nd_item<1> it) {
            temp_local_acc[it.get_local_linear_id()] = temp_acc[it.get_global_id(0)];
            result_acc[it.get_local_id(0)][it.get_group(0)] = temp_local_acc[it.get_local_linear_id()];
          }
        );
      }
    );
    transpose_event.wait(); // Explicitly wait till f has been applied to all elements of matrix
    done = true;
  }
  // result_buffer has been destroyed so result is accessible again
  //@ assert row_size > 0 ==> result[linearize(0, 0, col_size, row_size)] == f(matrix[linearize(0, 0, row_size, col_size)]);
}

