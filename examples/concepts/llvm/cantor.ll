define dso_local i32 @nthTriangular(i32 %0)
!VC.pure !{i1 true}
{
	%2 = add nsw i32 %0, 1
	%3 = mul nsw i32 %0, %2
	%4 = sdiv i32 %3, 2
	ret i32 %4
}
define dso_local i32 @cantorPair(i32 %0, i32 %1)
!VC.pure !{i1 true}
!VC.contract !{
!"ensures icmp(eq, %1, 0) ==> icmp(eq, call @nthTriangular(%0), \result);"
}
{
	%3 = mul nsw i32 %0, %0
	%4 = add nsw i32 %3, %0
	%5 = mul nsw i32 2, %0
	%6 = mul nsw i32 %5, %1
	%7 = add nsw i32 %4, %6
	%8 = mul nsw i32 3, %1
	%9 = add nsw i32 %7, %8
	%10 = mul nsw i32 %1, %1
	%11 = add nsw i32 %9, %10
	%12 = sdiv i32 %11, 2
	ret i32 %12
}