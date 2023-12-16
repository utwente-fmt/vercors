//==============================================================
// Vector Add is the equivalent of a Hello, World! sample for data parallel
// programs. Building and running the sample verifies that your development
// environment is setup correctly and demonstrates the use of the core features
// of SYCL. This sample runs on both CPU and GPU (or FPGA). When run, it
// computes on both the CPU and offload device, then compares results. If the
// code executes on both CPU and offload device, the device name and a success
// message are displayed. And, your development environment is setup correctly!
//
// For comprehensive instructions regarding SYCL Programming, go to
// https://software.intel.com/en-us/oneapi-programming-guide and search based on
// relevant terms noted in the comments.
//
// SYCL material used in the code sample:
// •	A one dimensional array of data.
// •	A device queue, buffer, accessor, and kernel.
//==============================================================
// Copyright © Intel Corporation
//
// SPDX-License-Identifier: MIT
// =============================================================

// Altered to be verifiable by VerCors

#include <sycl/sycl.hpp>
//#include <vector>
//#include <iostream>
//#include <string>
//#if FPGA_HARDWARE || FPGA_EMULATOR || FPGA_SIMULATOR
//#include <sycl/ext/intel/fpga_extensions.hpp>
//#endif

//using namespace sycl;

// num_repetitions: How many times to repeat the kernel invocation
//size_t num_repetitions = 1;
int num_repetitions = 1;
// Vector type and data size for this example.
//size_t vector_size = 10000;
int vector_size = 10000;
//typedef std::vector<int> IntVector;

// Create an exception handler for asynchronous SYCL exceptions
//static auto exception_handler = [](sycl::exception_list e_list) {
//  for (std::exception_ptr const &e : e_list) {
//    try {
//      std::rethrow_exception(e);
//    }
//    catch (std::exception const &e) {
//#if _DEBUG
//      std::cout << "Failure" << std::endl;
//#endif
//      std::terminate();
//    }
//  }
//};

//************************************
// Vector add in SYCL on device: returns sum in 4th parameter "sum_parallel".
//************************************
//void VectorAdd(queue &q, const IntVector &a_vector, const IntVector &b_vector,
//               IntVector &sum_parallel) {
/*@
  context size >= 0;
  context \pointer(a_vector, size, write);
  context \pointer(b_vector, size, write);
  context \pointer(sum_parallel, size, write);
  context_everywhere Perm(num_repetitions, write) ** num_repetitions >= 0;
*/
void VectorAdd(sycl::queue &q, int* a_vector, int* b_vector, int* sum_parallel, int size) {
  // Create the range object for the vectors managed by the buffer.
//  range<1> num_items{a_vector.size()};
  sycl::range<1> num_items = sycl::range<1>(size);

  // Create buffers that hold the data shared between the host and the devices.
  // The buffer destructor is responsible to copy the data back to host when it
  // goes out of scope.
//  buffer a_buf(a_vector);
//  buffer b_buf(b_vector);
//  buffer sum_buf(sum_parallel.data(), num_items);
  sycl::buffer<int, 1> a_buf = sycl::buffer(a_vector, sycl::range<1>(size));
  sycl::buffer<int, 1> b_buf = sycl::buffer(b_vector, sycl::range<1>(size));
  sycl::buffer<int, 1> sum_buf = sycl::buffer(sum_parallel, num_items);

  for (int i = 0; i < num_repetitions; i++) {

    // Submit a command group to the queue by a lambda function that contains the
    // data access permission and device computation (kernel).
//    q.submit([&](sycl::handler &h) {
    sycl::event e = q.submit([&](sycl::handler &h) {
      // Create an accessor for each buffer with access permission: read, write or
      // read/write. The accessor is a mean to access the memory in the buffer.
//      accessor a(a_buf, h, read_only);
//      accessor b(b_buf, h, read_only);
      sycl::accessor<int, 1> a = sycl::accessor(a_buf, h, sycl::read_only);
      sycl::accessor<int, 1> b = sycl::accessor(b_buf, h, sycl::read_only);

      // The sum_accessor is used to store (with write permission) the sum data.
//      accessor sum(sum_buf, h, write_only, no_init);
      sycl::accessor<int, 1> sum = sycl::accessor(sum_buf, h, sycl::read_write);

      // Use parallel_for to run vector addition in parallel on device. This
      // executes the kernel.
      //    1st parameter is the number of work items.
      //    2nd parameter is the kernel, a lambda that specifies what to do per
      //    work item. The parameter of the lambda is the work item id.
      // SYCL supports unnamed lambda kernel by default.
//      h.parallel_for(num_items, [=](auto i) { sum[i] = a[i] + b[i]; });
      h.parallel_for(num_items,
        /*@
          context i.get_linear_id() < a.get_range().get(0);
          context i.get_linear_id() < b.get_range().get(0);
          context i.get_linear_id() < sum.get_range().get(0);
          context Perm(a[i.get_linear_id()], read);
          context Perm(b[i.get_linear_id()], read);
          context Perm(sum[i.get_linear_id()], write);
          ensures sum[i.get_linear_id()] == a[i.get_linear_id()] + b[i.get_linear_id()];
        */
        [=](sycl::item<1> i) { sum[i.get_linear_id()] = a[i.get_linear_id()] + b[i.get_linear_id()]; }
      );
    });
    e.wait(); // EW TODO: not accurate, implement q.wait()? Then we do need to differentiate between different queues
  };
  // Wait until compute tasks on GPU done
//  q.wait();
}

//************************************
// Initialize the vector from 0 to vector_size - 1
//************************************
//void InitializeVector(IntVector &a) {
//  for (size_t i = 0; i < a.size(); i++) a.at(i) = i;
//}
/*@
  context_everywhere size >= 0;
  context_everywhere \pointer(a, size, write);
  ensures (\forall int i; i >= 0 && i < size; a[i] == i);
*/
int* InitializeVector(int* a, int size) {
  //@ loop_invariant i >= 0 && i <= size;
	//@ loop_invariant (\forall int j; j >= 0 && j < i; a[j] == j);
  for (int i = 0; i < size; i++) a[i] = i;
  return a;
}

//************************************
// Demonstrate vector add both in sequential on CPU and in parallel on device.
//************************************
//int main(int argc, char* argv[]) {
/*@
  requires argc >= 0;
  requires \pointer(argv, argc + 1, write);
  context Perm(vector_size, write) ** vector_size >= 0;
  requires \pointer(a, argc > 1 ? argv[1] : vector_size, write);
  requires \pointer(b, argc > 1 ? argv[1] : vector_size, write);
  requires \pointer(sum_sequential, argc > 1 ? argv[1] : vector_size, write);
  requires \pointer(sum_parallel, argc > 1 ? argv[1] : vector_size, write);
  context_everywhere Perm(num_repetitions, write) ** num_repetitions >= 0;
  ensures \pointer(a, vector_size, write);
  ensures \pointer(b, vector_size, write);
  ensures \pointer(sum_sequential, vector_size, write);
  ensures \pointer(sum_parallel, vector_size, write);
*/
int main(int argc, int* argv, int* a, int* b, int* sum_sequential, int* sum_parallel) {
  // Change num_repetitions if it was passed as argument
//  if (argc > 2) num_repetitions = std::stoi(argv[2]);
  if (argc > 2) num_repetitions = argv[2];
  // Change vector_size if it was passed as argument
  if (argc > 1) vector_size = argv[1];
  // Create device selector for the device of your interest.
//#if FPGA_EMULATOR
//  // Intel extension: FPGA emulator selector on systems without FPGA card.
//  auto selector = sycl::ext::intel::fpga_emulator_selector_v;
//#elif FPGA_SIMULATOR
//  // Intel extension: FPGA simulator selector on systems without FPGA card.
//  auto selector = sycl::ext::intel::fpga_simulator_selector_v;
//#elif FPGA_HARDWARE
//  // Intel extension: FPGA selector on systems with FPGA card.
//  auto selector = sycl::ext::intel::fpga_selector_v;
//#else
//  // The default device selector will select the most performant device.
//  auto selector = default_selector_v;
//#endif

  // Create vector objects with "vector_size" to store the input and output data.
//  IntVector a, b, sum_sequential, sum_parallel;
//  a.resize(vector_size);
//  b.resize(vector_size);
//  sum_sequential.resize(vector_size);
//  sum_parallel.resize(vector_size);

  // Initialize input vectors with values from 0 to vector_size - 1
  //@ assert vector_size >= 0;
  a = InitializeVector(a, vector_size);
  a = InitializeVector(b, vector_size);

//  try {
//    queue q(selector, exception_handler);
    sycl::queue q;

    // Print out the device information used for the kernel code.
//    std::cout << "Running on device: "
//              << q.get_device().get_info<info::device::name>() << "\n";
//    std::cout << "Vector size: " << a.size() << "\n";

    // Vector addition in SYCL
    VectorAdd(q, a, b, sum_parallel, vector_size);
//  } catch (exception const &e) {
//    std::cout << "An exception is caught for vector add.\n";
//    std::terminate();
//  }

  // Compute the sum of two vectors in sequential for validation.
//  for (size_t i = 0; i < sum_sequential.size(); i++)
//    sum_sequential.at(i) = a.at(i) + b.at(i);
  for (int i = 0; i < vector_size; i++)
    sum_sequential[i] = a[i] + b[i];

  // Verify that the two vectors are equal.
//  for (size_t i = 0; i < sum_sequential.size(); i++) {
//    if (sum_parallel.at(i) != sum_sequential.at(i)) {
//      std::cout << "Vector add failed on device.\n";
//      return -1;
//    }
//  }
  for (int i = 0; i < vector_size; i++) {
    if (sum_parallel[i] != sum_sequential[i]) {
      return -1;
    }
  }
  // EW TODO assert here that we did not fail

//  int indices[]{0, 1, 2, (static_cast<int>(a.size()) - 1)};
//  constexpr size_t indices_size = sizeof(indices) / sizeof(int);

  // Print out the result of vector add.
//  for (int i = 0; i < indices_size; i++) {
//    int j = indices[i];
//    if (i == indices_size - 1) std::cout << "...\n";
//    std::cout << "[" << j << "]: " << a[j] << " + " << b[j] << " = "
//              << sum_parallel[j] << "\n";
//  }
//
//  a.clear();
//  b.clear();
//  sum_sequential.clear();
//  sum_parallel.clear();
//
//  std::cout << "Vector add successfully completed on device.\n";
  return 0;
}
