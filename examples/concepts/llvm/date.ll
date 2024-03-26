define i1 @isLater(i32 %0, i32 %1, i32 %2, i32 %3, i32 %4, i32 %5)
!VC.contract !{
!"requires and(icmp(sge, %1, 1), icmp(sle, %1, 12));",
!"requires and(icmp(sge, %2, 1), icmp(sle, %2, 31));",
!"requires and(icmp(sge, %4, 1), icmp(sle, %4, 12));",
!"requires and(icmp(sge, %5, 1), icmp(sle, %5, 31));",

!"ensures icmp(sgt, %0, %3) ==> \result;",
!"ensures and(icmp(eq, %0, %3), icmp(eq, %1, %4)) ==> icmp(eq, \result, icmp(sgt, %2, %5));"
}
{
	%7 = icmp ne i32 %0, %3
	br i1 %7, label %8, label %10
  8:
	%9 = icmp sgt i32 %0, %3
	br label %16
  10:
	%11 = icmp ne i32 %1, %4
	br i1 %11, label %12, label %14
  12:
	%13 = icmp sgt i32 %1, %4
	br label %16
  14:
	%15 = icmp sgt i32 %2, %5
	br label %16
  16:
	%.0 = phi i1 [ %9, %8 ], [ %13, %12 ], [ %15, %14 ]
	ret i1 %.0
}


define i32 @test() {
	%1 = call i1 @isLater(i32 2023, i32 3, i32 7, i32 2020, i32 1, i32 1)
	%2 = call i1 @isLater(i32 1, i32 1, i32 2023, i32 15, i32 3, i32 2023)
	ret i32 0
}