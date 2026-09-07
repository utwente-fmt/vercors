#include <sycl/sycl.hpp>

#include <array>
//#include <iostream>

//constexpr sycl::access::mode sycl_read = sycl::access::mode::read;
//constexpr sycl::access::mode sycl_write = sycl::access::mode::write;

//class SimpleVadd;

int main() {
  /*const size_t*/ int N = 5;
//  std::array<int, N>
   int A[] = {1, 2, 3, 4, 5};
   int B[] = {2, 3, 4, 5, 6};
   int C[] = {0, 0, 0, 0, 0};
  // Removed N, Template support ˆ N must be a class, struct or namespace...
  // The generics support in VerCors seems to be very much focussed on Java generics.
  sycl::queue deviceQueue;
  sycl::range<1> numOfItems = {N};
  {
      sycl::buffer<int, 1> bufferA = sycl::buffer<int, 1>(A, sycl::range<1>(N));
      sycl::buffer<int, 1> bufferB = sycl::buffer<int, 1>(B, sycl::range<1>(N));
      sycl::buffer<int, 1> bufferC = sycl::buffer<int, 1>(C, sycl::range<1>(N));

//    sycl::buffer<int, 1> bufferA(A.data(), numOfItems);
//    sycl::buffer<int, 1> bufferB(B.data(), numOfItems);
//    sycl::buffer<int, 1> bufferC(C.data(), numOfItems);
//    Requires proper postfix syntax support.

    sycl::event e = deviceQueue
        .submit([&](sycl::handler& cgh) {
          sycl::accessor<int, 1, sycl::access_mode::read> accessorA = bufferA.get_access<sycl::access::mode::read>(cgh);
//          auto accessorB = bufferB.get_access<sycl::access::mode::read>(cgh);
//          auto accessorC = bufferC.get_access<sycl::access::mode::write>(cgh);
// Requires templates
//          sycl::accessor<int, 1, sycl::access_mode::read> accessorA = sycl::accessor(bufferA, cgh, sycl::read_only);
          sycl::accessor<int, 1, sycl::access_mode::read> accessorB = sycl::accessor(bufferB, cgh, sycl::read_only);
          sycl::accessor<int, 1, sycl::access_mode::read_write> accessorC = sycl::accessor(bufferC, cgh, sycl::read_write);
// What is the type of a lambda expression?
//          auto kern = [=](sycl::id<1> wiID) {
//            accessorC[wiID] = accessorA[wiID] + accessorB[wiID];
//          };
//          cgh.parallel_for/*<class SimpleVadd>*/(numOfItems, kern);
          cgh.parallel_for/*<class SimpleVadd>*/(sycl::range<1>(N),
            /*@
                context Perm(accessorA[wiID.get_id(0)], read);
                context Perm(accessorB[wiID.get_id(0)], read);
                context Perm(accessorC[wiID.get_id(0)], write);
                ensures accessorC[wiID.get_id(0)] == accessorA[wiID.get_id(0)] + accessorB[wiID.get_id(0)];
            @*/
            [=](sycl::item<1> wiID) {
                 accessorC[wiID.get_id(0)] = accessorA[wiID.get_id(0)] + accessorB[wiID.get_id(0)];
               }
           );
        });
        e.wait();
  }

  //@ assert (\forall int i=0 .. N; C[i] == A[i] + B[i]);

    /*
        loop_invariant 0 <= i && i <= N;
        loop_invariant \pointer(A, N, 1\2);
        loop_invariant \pointer(B, N, 1\2);
        loop_invariant \pointer(C, N, 1\2);
    */
  for (unsigned int i = 0; i < N; i++) {
//    std::cout << "C[" << i << "] = " << C[i] << "\n";
    if (C[i] != A[i] + B[i]) {
//      std::cout << "The results are incorrect (element " << i << " is " << C[i]
//                << "!\n";
      return 1;
    }
  }
//  std::cout << "The results are correct!\n";
  return 0;
}
