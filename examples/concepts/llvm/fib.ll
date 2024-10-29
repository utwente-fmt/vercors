!VC.global = !{!0}
!0 = !{
!"pure i32 @fib(i32 %n) =
	br(icmp(sgt, %n, 2),
		add(call @fib(sub(%n, 1)), call @fib(sub(%n, 2))),
		1);"
}
define dso_local i32 @fibonacci(i32 noundef %0)
!VC.contract !{
!"requires icmp(sge, %0, 1);",
!"ensures icmp(eq, \result, call @fib(%0));"
}
{
	%2 = icmp sgt i32 %0, 2
	br i1 %2, label %3, label %9
  3:
	%4 = sub nsw i32 %0, 1
	%5 = call i32 @fibonacci(i32 noundef %4)
	%6 = sub nsw i32 %0, 2
	%7 = call i32 @fibonacci(i32 noundef %6)
	%8 = add nsw i32 %5, %7
	br label %10
  9:
	br label %10
  10:
	%.0 = phi i32 [ %8, %3 ], [ 1, %9 ]
	ret i32 %.0
}