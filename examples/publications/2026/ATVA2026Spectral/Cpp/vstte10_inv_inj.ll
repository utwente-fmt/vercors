; ModuleID = 'tmp_spectral/tmp_ir_source0.ll'
source_filename = "examples/publications/2026/ATVA2026Spectral/Cpp/vstte10_inv_inj.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.compiler.used = appending global [20 x ptr] [ptr @_Z13PALLAS_SPEC_0i, ptr @_Z13PALLAS_SPEC_1PiS_i, ptr @_Z13PALLAS_SPEC_2PiS_i, ptr @_Z13PALLAS_SPEC_3PiS_i, ptr @_Z13PALLAS_SPEC_4PiS_i, ptr @_Z13PALLAS_SPEC_5PiS_i, ptr @_Z13PALLAS_SPEC_6PiS_i, ptr @_Z13PALLAS_SPEC_7PiS_i, ptr @_Z13PALLAS_SPEC_8PiS_i, ptr @_Z13PALLAS_SPEC_9PiS_i, ptr @_Z14PALLAS_SPEC_10PiS_i, ptr @_Z14PALLAS_SPEC_11PiS_i, ptr @_Z14PALLAS_SPEC_12PiS_i, ptr @_Z14PALLAS_SPEC_13PiS_i, ptr @_Z14PALLAS_SPEC_15PiS_ii, ptr @_Z14PALLAS_SPEC_14PiS_ii, ptr @_Z14PALLAS_SPEC_17PiS_ii, ptr @_Z14PALLAS_SPEC_18PiS_ii, ptr @_Z14PALLAS_SPEC_16PiS_ii, ptr @_Z14PALLAS_SPEC_19PiS_i], section "llvm.metadata"
@llvm.used = appending global [20 x ptr] [ptr @_Z13PALLAS_SPEC_0i, ptr @_Z13PALLAS_SPEC_1PiS_i, ptr @_Z13PALLAS_SPEC_2PiS_i, ptr @_Z13PALLAS_SPEC_3PiS_i, ptr @_Z13PALLAS_SPEC_4PiS_i, ptr @_Z13PALLAS_SPEC_5PiS_i, ptr @_Z13PALLAS_SPEC_6PiS_i, ptr @_Z13PALLAS_SPEC_7PiS_i, ptr @_Z13PALLAS_SPEC_8PiS_i, ptr @_Z13PALLAS_SPEC_9PiS_i, ptr @_Z14PALLAS_SPEC_10PiS_i, ptr @_Z14PALLAS_SPEC_11PiS_i, ptr @_Z14PALLAS_SPEC_12PiS_i, ptr @_Z14PALLAS_SPEC_13PiS_i, ptr @_Z14PALLAS_SPEC_15PiS_ii, ptr @_Z14PALLAS_SPEC_14PiS_ii, ptr @_Z14PALLAS_SPEC_17PiS_ii, ptr @_Z14PALLAS_SPEC_18PiS_ii, ptr @_Z14PALLAS_SPEC_16PiS_ii, ptr @_Z14PALLAS_SPEC_19PiS_i], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"j\00", align 1, !dbg !0
@.str.1 = private unnamed_addr constant [2 x i8] c"k\00", align 1, !dbg !8

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z4trigi(i32 noundef %0) #0 !dbg !119 !pallas.fcontract !124 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !131, metadata !DIExpression()), !dbg !134
  ret i1 true, !dbg !135
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local void @_Z6invertPiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #0 !dbg !136 !pallas.fcontract !140 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !146, metadata !DIExpression()), !dbg !277
  call void @llvm.dbg.value(metadata ptr %1, metadata !152, metadata !DIExpression()), !dbg !277
  call void @llvm.dbg.value(metadata i32 %2, metadata !155, metadata !DIExpression()), !dbg !277
  call void @llvm.dbg.value(metadata i32 0, metadata !278, metadata !DIExpression()), !dbg !280
  br label %4, !dbg !281

4:                                                ; preds = %12, %3
  %.0 = phi i32 [ 0, %3 ], [ %13, %12 ], !dbg !282
  call void @llvm.dbg.value(metadata i32 %.0, metadata !278, metadata !DIExpression()), !dbg !280
  %5 = icmp slt i32 %.0, %2, !dbg !283
  br i1 %5, label %6, label %14, !dbg !285

6:                                                ; preds = %4
  %7 = sext i32 %.0 to i64, !dbg !286
  %8 = getelementptr inbounds i32, ptr %0, i64 %7, !dbg !286
  %9 = load i32, ptr %8, align 4, !dbg !286
  %10 = sext i32 %9 to i64, !dbg !288
  %11 = getelementptr inbounds i32, ptr %1, i64 %10, !dbg !288
  store i32 %.0, ptr %11, align 4, !dbg !289
  br label %12, !dbg !290

12:                                               ; preds = %6
  %13 = add nsw i32 %.0, 1, !dbg !291
  call void @llvm.dbg.value(metadata i32 %13, metadata !278, metadata !DIExpression()), !dbg !280
  br label %4, !dbg !292, !llvm.loop !293

14:                                               ; preds = %4
  ret void, !dbg !360, !pallas.stmntBlock !361
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_0i(i32 noundef %0) #2 !dbg !133 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !132, metadata !DIExpression()), !dbg !374
  %2 = call noundef zeroext i1 @"pallas.result noundef zeroext i1"(), !dbg !375
  %3 = zext i1 %2 to i32, !dbg !375
  %4 = icmp eq i32 %3, 1, !dbg !376
  ret i1 %4, !dbg !374
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_1PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #0 !dbg !148 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !147, metadata !DIExpression()), !dbg !377
  call void @llvm.dbg.value(metadata ptr %1, metadata !153, metadata !DIExpression()), !dbg !377
  call void @llvm.dbg.value(metadata i32 %2, metadata !156, metadata !DIExpression()), !dbg !377
  %4 = icmp sge i32 %2, 0, !dbg !378
  ret i1 %4, !dbg !377
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_2PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #0 !dbg !162 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !161, metadata !DIExpression()), !dbg !379
  call void @llvm.dbg.value(metadata ptr %1, metadata !164, metadata !DIExpression()), !dbg !379
  call void @llvm.dbg.value(metadata i32 %2, metadata !166, metadata !DIExpression()), !dbg !379
  %4 = icmp ne ptr %0, null, !dbg !380
  br i1 %4, label %5, label %7, !dbg !381

5:                                                ; preds = %3
  %6 = icmp ne ptr %1, null, !dbg !382
  br label %7

7:                                                ; preds = %5, %3
  %8 = phi i1 [ false, %3 ], [ %6, %5 ], !dbg !379
  ret i1 %8, !dbg !379
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_3PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !172 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !171, metadata !DIExpression()), !dbg !383
  call void @llvm.dbg.value(metadata ptr %1, metadata !174, metadata !DIExpression()), !dbg !383
  call void @llvm.dbg.value(metadata i32 %2, metadata !176, metadata !DIExpression()), !dbg !383
  %4 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !384
  %5 = sext i32 %2 to i64, !dbg !385
  %6 = icmp eq i64 %4, %5, !dbg !386
  br i1 %6, label %7, label %11, !dbg !387

7:                                                ; preds = %3
  %8 = call i64 @pallas.ptrLength(ptr noundef %1), !dbg !388
  %9 = sext i32 %2 to i64, !dbg !389
  %10 = icmp eq i64 %8, %9, !dbg !390
  br label %11

11:                                               ; preds = %7, %3
  %12 = phi i1 [ false, %3 ], [ %10, %7 ], !dbg !383
  ret i1 %12, !dbg !383
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_4PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !182 !pallas.exprWrapper !373 {
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !181, metadata !DIExpression()), !dbg !391
  call void @llvm.dbg.value(metadata ptr %1, metadata !184, metadata !DIExpression()), !dbg !391
  call void @llvm.dbg.value(metadata i32 %2, metadata !186, metadata !DIExpression()), !dbg !391
  %5 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !392
  %6 = icmp sle i32 0, %5, !dbg !392
  %7 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !392
  %8 = icmp slt i32 %7, %2, !dbg !392
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !392
  %10 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !393
  %11 = sext i32 %10 to i64, !dbg !394
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !394
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 2), !dbg !395
  %13 = call i1 @pallas.perm(ptr noundef %12, ptr noundef byval(%pallas.fracT) %4), !dbg !396
  %14 = call i1 @pallas.forallSep(i1 %9, i1 %13), !dbg !397
  ret i1 %14, !dbg !391
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_5PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !192 !pallas.exprWrapper !373 {
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !191, metadata !DIExpression()), !dbg !398
  call void @llvm.dbg.value(metadata ptr %1, metadata !194, metadata !DIExpression()), !dbg !398
  call void @llvm.dbg.value(metadata i32 %2, metadata !196, metadata !DIExpression()), !dbg !398
  %5 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !399
  %6 = icmp sle i32 0, %5, !dbg !399
  %7 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !399
  %8 = icmp slt i32 %7, %2, !dbg !399
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !399
  %10 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !400
  %11 = sext i32 %10 to i64, !dbg !401
  %12 = getelementptr inbounds i32, ptr %1, i64 %11, !dbg !401
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 1), !dbg !402
  %13 = call i1 @pallas.perm(ptr noundef %12, ptr noundef byval(%pallas.fracT) %4), !dbg !403
  %14 = call i1 @pallas.forallSep(i1 %9, i1 %13), !dbg !404
  ret i1 %14, !dbg !398
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_6PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !202 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !201, metadata !DIExpression()), !dbg !405
  call void @llvm.dbg.value(metadata ptr %1, metadata !204, metadata !DIExpression()), !dbg !405
  call void @llvm.dbg.value(metadata i32 %2, metadata !206, metadata !DIExpression()), !dbg !405
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !406
  %5 = icmp sle i32 0, %4, !dbg !406
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !406
  %7 = icmp slt i32 %6, %2, !dbg !406
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !406
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !407
  %10 = sext i32 %9 to i64, !dbg !407
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !407
  %12 = load i32, ptr %11, align 4, !dbg !407
  %13 = icmp sle i32 0, %12, !dbg !407
  %14 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !407
  %15 = sext i32 %14 to i64, !dbg !407
  %16 = getelementptr inbounds i32, ptr %0, i64 %15, !dbg !407
  %17 = load i32, ptr %16, align 4, !dbg !407
  %18 = icmp slt i32 %17, %2, !dbg !407
  %19 = call i1 @pallas.scAnd(i1 %13, i1 %18), !dbg !407
  %20 = call i1 @pallas.forall(i1 %8, i1 %19), !dbg !408
  ret i1 %20, !dbg !405
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_7PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !212 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !211, metadata !DIExpression()), !dbg !409
  call void @llvm.dbg.value(metadata ptr %1, metadata !214, metadata !DIExpression()), !dbg !409
  call void @llvm.dbg.value(metadata i32 %2, metadata !216, metadata !DIExpression()), !dbg !409
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !410
  %5 = icmp sle i32 0, %4, !dbg !410
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !410
  %7 = icmp slt i32 %6, %2, !dbg !410
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !410
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !411
  %10 = icmp sle i32 0, %9, !dbg !411
  %11 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !411
  %12 = icmp slt i32 %11, %2, !dbg !411
  %13 = call i1 @pallas.scAnd(i1 %10, i1 %12), !dbg !411
  %14 = call i1 @pallas.scAnd(i1 %8, i1 %13), !dbg !412
  %15 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !413
  %16 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !414
  %17 = icmp ne i32 %15, %16, !dbg !415
  %18 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !416
  %19 = sext i32 %18 to i64, !dbg !417
  %20 = getelementptr inbounds i32, ptr %0, i64 %19, !dbg !417
  %21 = load i32, ptr %20, align 4, !dbg !417
  %22 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !418
  %23 = sext i32 %22 to i64, !dbg !419
  %24 = getelementptr inbounds i32, ptr %0, i64 %23, !dbg !419
  %25 = load i32, ptr %24, align 4, !dbg !419
  %26 = icmp ne i32 %21, %25, !dbg !420
  %27 = call i1 @pallas.imply(i1 %17, i1 %26), !dbg !421
  %28 = call i1 @pallas.forall(i1 %14, i1 %27), !dbg !422
  ret i1 %28, !dbg !409
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_8PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !222 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !221, metadata !DIExpression()), !dbg !423
  call void @llvm.dbg.value(metadata ptr %1, metadata !224, metadata !DIExpression()), !dbg !423
  call void @llvm.dbg.value(metadata i32 %2, metadata !226, metadata !DIExpression()), !dbg !423
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !424
  %5 = icmp sle i32 0, %4, !dbg !424
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !424
  %7 = icmp slt i32 %6, %2, !dbg !424
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !424
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !425
  %10 = call noundef zeroext i1 @_Z4trigi(i32 noundef %9), !dbg !426
  %11 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !427
  %12 = icmp sle i32 0, %11, !dbg !427
  %13 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !427
  %14 = icmp slt i32 %13, %2, !dbg !427
  %15 = call i1 @pallas.scAnd(i1 %12, i1 %14), !dbg !427
  %16 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !428
  %17 = sext i32 %16 to i64, !dbg !429
  %18 = getelementptr inbounds i32, ptr %0, i64 %17, !dbg !429
  %19 = load i32, ptr %18, align 4, !dbg !429
  %20 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !430
  %21 = icmp eq i32 %19, %20, !dbg !431
  %22 = call i1 @pallas.exists(i1 %15, i1 %21), !dbg !432
  %23 = call i1 @pallas.imply(i1 %10, i1 %22), !dbg !433
  %24 = call i1 @pallas.forall(i1 %8, i1 %23), !dbg !434
  ret i1 %24, !dbg !423
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_9PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !232 !pallas.exprWrapper !373 {
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !231, metadata !DIExpression()), !dbg !435
  call void @llvm.dbg.value(metadata ptr %1, metadata !234, metadata !DIExpression()), !dbg !435
  call void @llvm.dbg.value(metadata i32 %2, metadata !236, metadata !DIExpression()), !dbg !435
  %5 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !436
  %6 = icmp sle i32 0, %5, !dbg !436
  %7 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !436
  %8 = icmp slt i32 %7, %2, !dbg !436
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !436
  %10 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !437
  %11 = sext i32 %10 to i64, !dbg !438
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !438
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 2), !dbg !439
  %13 = call i1 @pallas.perm(ptr noundef %12, ptr noundef byval(%pallas.fracT) %4), !dbg !440
  %14 = call i1 @pallas.forallSep(i1 %9, i1 %13), !dbg !441
  ret i1 %14, !dbg !435
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_10PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !242 !pallas.exprWrapper !373 {
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !241, metadata !DIExpression()), !dbg !442
  call void @llvm.dbg.value(metadata ptr %1, metadata !244, metadata !DIExpression()), !dbg !442
  call void @llvm.dbg.value(metadata i32 %2, metadata !246, metadata !DIExpression()), !dbg !442
  %5 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !443
  %6 = icmp sle i32 0, %5, !dbg !443
  %7 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !443
  %8 = icmp slt i32 %7, %2, !dbg !443
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !443
  %10 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !444
  %11 = sext i32 %10 to i64, !dbg !445
  %12 = getelementptr inbounds i32, ptr %1, i64 %11, !dbg !445
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 1), !dbg !446
  %13 = call i1 @pallas.perm(ptr noundef %12, ptr noundef byval(%pallas.fracT) %4), !dbg !447
  %14 = call i1 @pallas.forallSep(i1 %9, i1 %13), !dbg !448
  ret i1 %14, !dbg !442
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_11PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !252 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !251, metadata !DIExpression()), !dbg !449
  call void @llvm.dbg.value(metadata ptr %1, metadata !254, metadata !DIExpression()), !dbg !449
  call void @llvm.dbg.value(metadata i32 %2, metadata !256, metadata !DIExpression()), !dbg !449
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !450
  %5 = icmp sle i32 0, %4, !dbg !450
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !450
  %7 = icmp slt i32 %6, %2, !dbg !450
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !450
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !451
  %10 = sext i32 %9 to i64, !dbg !452
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !452
  %12 = load i32, ptr %11, align 4, !dbg !452
  %13 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !453
  %14 = sext i32 %13 to i64, !dbg !454
  %15 = getelementptr inbounds i32, ptr %0, i64 %14, !dbg !454
  %16 = load i32, ptr %15, align 4, !dbg !454
  %17 = call noundef i32 @"pallas.old noundef i32_noundef i32"(i32 noundef %16), !dbg !455
  %18 = icmp eq i32 %12, %17, !dbg !456
  %19 = call i1 @pallas.forall(i1 %8, i1 %18), !dbg !457
  ret i1 %19, !dbg !449
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_12PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !262 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !261, metadata !DIExpression()), !dbg !458
  call void @llvm.dbg.value(metadata ptr %1, metadata !264, metadata !DIExpression()), !dbg !458
  call void @llvm.dbg.value(metadata i32 %2, metadata !266, metadata !DIExpression()), !dbg !458
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !459
  %5 = icmp sle i32 0, %4, !dbg !459
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !459
  %7 = icmp slt i32 %6, %2, !dbg !459
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !459
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !460
  %10 = sext i32 %9 to i64, !dbg !461
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !461
  %12 = load i32, ptr %11, align 4, !dbg !461
  %13 = sext i32 %12 to i64, !dbg !462
  %14 = getelementptr inbounds i32, ptr %1, i64 %13, !dbg !462
  %15 = load i32, ptr %14, align 4, !dbg !462
  %16 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !463
  %17 = icmp eq i32 %15, %16, !dbg !464
  %18 = call i1 @pallas.forall(i1 %8, i1 %17), !dbg !465
  ret i1 %18, !dbg !458
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_13PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !272 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !271, metadata !DIExpression()), !dbg !466
  call void @llvm.dbg.value(metadata ptr %1, metadata !274, metadata !DIExpression()), !dbg !466
  call void @llvm.dbg.value(metadata i32 %2, metadata !276, metadata !DIExpression()), !dbg !466
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !467
  %5 = icmp sle i32 0, %4, !dbg !467
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !467
  %7 = icmp slt i32 %6, %2, !dbg !467
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !467
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !468
  %10 = icmp sle i32 0, %9, !dbg !468
  %11 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !468
  %12 = icmp slt i32 %11, %2, !dbg !468
  %13 = call i1 @pallas.scAnd(i1 %10, i1 %12), !dbg !468
  %14 = call i1 @pallas.scAnd(i1 %8, i1 %13), !dbg !469
  %15 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !470
  %16 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !471
  %17 = icmp ne i32 %15, %16, !dbg !472
  %18 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !473
  %19 = sext i32 %18 to i64, !dbg !474
  %20 = getelementptr inbounds i32, ptr %1, i64 %19, !dbg !474
  %21 = load i32, ptr %20, align 4, !dbg !474
  %22 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !475
  %23 = sext i32 %22 to i64, !dbg !476
  %24 = getelementptr inbounds i32, ptr %1, i64 %23, !dbg !476
  %25 = load i32, ptr %24, align 4, !dbg !476
  %26 = icmp ne i32 %21, %25, !dbg !477
  %27 = call i1 @pallas.imply(i1 %17, i1 %26), !dbg !478
  %28 = call i1 @pallas.forall(i1 %14, i1 %27), !dbg !479
  ret i1 %28, !dbg !466
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_15PiS_ii(ptr noundef %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #2 !dbg !317 !pallas.exprWrapper !373 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !316, metadata !DIExpression()), !dbg !480
  call void @llvm.dbg.value(metadata ptr %1, metadata !319, metadata !DIExpression()), !dbg !480
  call void @llvm.dbg.value(metadata i32 %2, metadata !321, metadata !DIExpression()), !dbg !480
  call void @llvm.dbg.value(metadata i32 %3, metadata !323, metadata !DIExpression()), !dbg !480
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !481
  %7 = icmp sle i32 0, %6, !dbg !481
  %8 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !481
  %9 = icmp slt i32 %8, %2, !dbg !481
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !481
  %11 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !482
  %12 = sext i32 %11 to i64, !dbg !483
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !483
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 4), !dbg !484
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %5), !dbg !485
  %15 = call i1 @pallas.forallSep(i1 %10, i1 %14), !dbg !486
  ret i1 %15, !dbg !480
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_14PiS_ii(ptr noundef %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !303 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !302, metadata !DIExpression()), !dbg !487
  call void @llvm.dbg.value(metadata ptr %1, metadata !307, metadata !DIExpression()), !dbg !487
  call void @llvm.dbg.value(metadata i32 %2, metadata !309, metadata !DIExpression()), !dbg !487
  call void @llvm.dbg.value(metadata i32 %3, metadata !311, metadata !DIExpression()), !dbg !487
  %5 = icmp sle i32 0, %3, !dbg !488
  br i1 %5, label %6, label %8, !dbg !489

6:                                                ; preds = %4
  %7 = icmp sle i32 %3, %2, !dbg !490
  br label %8

8:                                                ; preds = %6, %4
  %9 = phi i1 [ false, %4 ], [ %7, %6 ], !dbg !487
  ret i1 %9, !dbg !487
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_17PiS_ii(ptr noundef %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #2 !dbg !341 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !340, metadata !DIExpression()), !dbg !491
  call void @llvm.dbg.value(metadata ptr %1, metadata !343, metadata !DIExpression()), !dbg !491
  call void @llvm.dbg.value(metadata i32 %2, metadata !345, metadata !DIExpression()), !dbg !491
  call void @llvm.dbg.value(metadata i32 %3, metadata !347, metadata !DIExpression()), !dbg !491
  %5 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !492
  %6 = icmp sle i32 0, %5, !dbg !492
  %7 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !492
  %8 = icmp slt i32 %7, %2, !dbg !492
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !492
  %10 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !493
  %11 = sext i32 %10 to i64, !dbg !494
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !494
  %13 = load i32, ptr %12, align 4, !dbg !494
  %14 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !495
  %15 = sext i32 %14 to i64, !dbg !496
  %16 = getelementptr inbounds i32, ptr %0, i64 %15, !dbg !496
  %17 = load i32, ptr %16, align 4, !dbg !496
  %18 = call noundef i32 @"pallas.old noundef i32_noundef i32"(i32 noundef %17), !dbg !497
  %19 = icmp eq i32 %13, %18, !dbg !498
  %20 = call i1 @pallas.forall(i1 %9, i1 %19), !dbg !499
  ret i1 %20, !dbg !491
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_18PiS_ii(ptr noundef %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #2 !dbg !353 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !352, metadata !DIExpression()), !dbg !500
  call void @llvm.dbg.value(metadata ptr %1, metadata !355, metadata !DIExpression()), !dbg !500
  call void @llvm.dbg.value(metadata i32 %2, metadata !357, metadata !DIExpression()), !dbg !500
  call void @llvm.dbg.value(metadata i32 %3, metadata !359, metadata !DIExpression()), !dbg !500
  %5 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !501
  %6 = icmp sle i32 0, %5, !dbg !501
  %7 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !501
  %8 = icmp slt i32 %7, %3, !dbg !501
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !501
  %10 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !502
  %11 = sext i32 %10 to i64, !dbg !503
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !503
  %13 = load i32, ptr %12, align 4, !dbg !503
  %14 = sext i32 %13 to i64, !dbg !504
  %15 = getelementptr inbounds i32, ptr %1, i64 %14, !dbg !504
  %16 = load i32, ptr %15, align 4, !dbg !504
  %17 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !505
  %18 = icmp eq i32 %16, %17, !dbg !506
  %19 = call i1 @pallas.forall(i1 %9, i1 %18), !dbg !507
  ret i1 %19, !dbg !500
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_16PiS_ii(ptr noundef %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #2 !dbg !329 !pallas.exprWrapper !373 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !328, metadata !DIExpression()), !dbg !508
  call void @llvm.dbg.value(metadata ptr %1, metadata !331, metadata !DIExpression()), !dbg !508
  call void @llvm.dbg.value(metadata i32 %2, metadata !333, metadata !DIExpression()), !dbg !508
  call void @llvm.dbg.value(metadata i32 %3, metadata !335, metadata !DIExpression()), !dbg !508
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !509
  %7 = icmp sle i32 0, %6, !dbg !509
  %8 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !509
  %9 = icmp slt i32 %8, %2, !dbg !509
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !509
  %11 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !510
  %12 = sext i32 %11 to i64, !dbg !511
  %13 = getelementptr inbounds i32, ptr %1, i64 %12, !dbg !511
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 1), !dbg !512
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %5), !dbg !513
  %15 = call i1 @pallas.forallSep(i1 %10, i1 %14), !dbg !514
  ret i1 %15, !dbg !508
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_19PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !368 !pallas.exprWrapper !373 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !367, metadata !DIExpression()), !dbg !515
  call void @llvm.dbg.value(metadata ptr %1, metadata !370, metadata !DIExpression()), !dbg !515
  call void @llvm.dbg.value(metadata i32 %2, metadata !372, metadata !DIExpression()), !dbg !515
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !516
  %5 = icmp sle i32 0, %4, !dbg !516
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !516
  %7 = icmp slt i32 %6, %2, !dbg !516
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !516
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !517
  %10 = icmp sle i32 0, %9, !dbg !517
  %11 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !517
  %12 = icmp slt i32 %11, %2, !dbg !517
  %13 = call i1 @pallas.scAnd(i1 %10, i1 %12), !dbg !517
  %14 = call i1 @pallas.scAnd(i1 %8, i1 %13), !dbg !518
  %15 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !519
  %16 = call noundef zeroext i1 @_Z4trigi(i32 noundef %15), !dbg !520
  %17 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !521
  %18 = call noundef zeroext i1 @_Z4trigi(i32 noundef %17), !dbg !522
  %19 = call i1 @pallas.scAnd(i1 %16, i1 %18), !dbg !523
  %20 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !524
  %21 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !525
  %22 = icmp ne i32 %20, %21, !dbg !526
  %23 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !527
  %24 = sext i32 %23 to i64, !dbg !528
  %25 = getelementptr inbounds i32, ptr %1, i64 %24, !dbg !528
  %26 = load i32, ptr %25, align 4, !dbg !528
  %27 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str.1), !dbg !529
  %28 = sext i32 %27 to i64, !dbg !530
  %29 = getelementptr inbounds i32, ptr %1, i64 %28, !dbg !530
  %30 = load i32, ptr %29, align 4, !dbg !530
  %31 = icmp ne i32 %26, %30, !dbg !531
  %32 = call i1 @pallas.imply(i1 %22, i1 %31), !dbg !532
  %33 = call i1 @pallas.imply(i1 %19, i1 %32), !dbg !533
  %34 = call i1 @pallas.forall(i1 %14, i1 %33), !dbg !534
  ret i1 %34, !dbg !515
}

declare !pallas.specLib !535 noundef zeroext i1 @"pallas.result noundef zeroext i1"()

declare !pallas.specLib !536 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !537 i1 @pallas.exists(i1, i1)

declare !pallas.specLib !538 noundef i32 @"pallas.old noundef i32_noundef i32"(i32 noundef)

declare !pallas.specLib !539 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !540 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !541 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !542 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !543 i1 @pallas.imply(i1, i1)

declare !pallas.specLib !544 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !545 noundef i32 @"pallas.boundVar noundef i32"(ptr)

attributes #0 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { mustprogress noinline uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!10, !12}
!llvm.module.flags = !{!111, !112, !113, !114, !115, !116, !117}
!llvm.ident = !{!118, !118}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 75, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp_spectral/source_wrappers.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "b1ca14213f567c9f73209189699ab8df")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !6)
!4 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !5)
!5 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!6 = !{!7}
!7 = !DISubrange(count: 2)
!8 = !DIGlobalVariableExpression(var: !9, expr: !DIExpression())
!9 = distinct !DIGlobalVariable(scope: null, file: !2, line: 105, type: !3, isLocal: true, isDefinition: true)
!10 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !11, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!11 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/Cpp/vstte10_inv_inj.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "136ee566b8a54a1013cd8a1c4ff22e53")
!12 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !2, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, globals: !13, imports: !14, splitDebugInlining: false, nameTableKind: None)
!13 = !{!0, !8}
!14 = !{!15, !23, !27, !31, !35, !38, !40, !42, !44, !48, !51, !54, !57, !60, !62, !67, !71, !75, !79, !81, !83, !85, !87, !90, !93, !96, !99, !102, !104, !109}
!15 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !17, file: !22, line: 51)
!16 = !DINamespace(name: "std", scope: null)
!17 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !18, line: 24, baseType: !19)
!18 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!19 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int8_t", file: !20, line: 37, baseType: !21)
!20 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!21 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!22 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstdint", directory: "")
!23 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !24, file: !22, line: 52)
!24 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !18, line: 25, baseType: !25)
!25 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int16_t", file: !20, line: 39, baseType: !26)
!26 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!27 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !28, file: !22, line: 53)
!28 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !18, line: 26, baseType: !29)
!29 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !20, line: 41, baseType: !30)
!30 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!31 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !32, file: !22, line: 54)
!32 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !18, line: 27, baseType: !33)
!33 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !20, line: 44, baseType: !34)
!34 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!35 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !36, file: !22, line: 56)
!36 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !37, line: 47, baseType: !21)
!37 = !DIFile(filename: "/usr/include/stdint.h", directory: "", checksumkind: CSK_MD5, checksum: "bfb03fa9c46a839e35c32b929fbdbb8e")
!38 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !39, file: !22, line: 57)
!39 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !37, line: 49, baseType: !34)
!40 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !41, file: !22, line: 58)
!41 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !37, line: 50, baseType: !34)
!42 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !43, file: !22, line: 59)
!43 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !37, line: 51, baseType: !34)
!44 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !45, file: !22, line: 61)
!45 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !46, line: 25, baseType: !47)
!46 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-least.h", directory: "", checksumkind: CSK_MD5, checksum: "a866be81c480920b0293bd5f6336a0a3")
!47 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least8_t", file: !20, line: 52, baseType: !19)
!48 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !49, file: !22, line: 62)
!49 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !46, line: 26, baseType: !50)
!50 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least16_t", file: !20, line: 54, baseType: !25)
!51 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !52, file: !22, line: 63)
!52 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !46, line: 27, baseType: !53)
!53 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least32_t", file: !20, line: 56, baseType: !29)
!54 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !55, file: !22, line: 64)
!55 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !46, line: 28, baseType: !56)
!56 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least64_t", file: !20, line: 58, baseType: !33)
!57 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !58, file: !22, line: 66)
!58 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !37, line: 90, baseType: !59)
!59 = !DIDerivedType(tag: DW_TAG_typedef, name: "__intmax_t", file: !20, line: 72, baseType: !34)
!60 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !61, file: !22, line: 67)
!61 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !37, line: 76, baseType: !34)
!62 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !63, file: !22, line: 69)
!63 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !64, line: 24, baseType: !65)
!64 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-uintn.h", directory: "", checksumkind: CSK_MD5, checksum: "256fcabbefa27ca8cf5e6d37525e6e16")
!65 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !20, line: 38, baseType: !66)
!66 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!67 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !68, file: !22, line: 70)
!68 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !64, line: 25, baseType: !69)
!69 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !20, line: 40, baseType: !70)
!70 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!71 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !72, file: !22, line: 71)
!72 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !64, line: 26, baseType: !73)
!73 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint32_t", file: !20, line: 42, baseType: !74)
!74 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!75 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !76, file: !22, line: 72)
!76 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !64, line: 27, baseType: !77)
!77 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint64_t", file: !20, line: 45, baseType: !78)
!78 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!79 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !80, file: !22, line: 74)
!80 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !37, line: 60, baseType: !66)
!81 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !82, file: !22, line: 75)
!82 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !37, line: 62, baseType: !78)
!83 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !84, file: !22, line: 76)
!84 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !37, line: 63, baseType: !78)
!85 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !86, file: !22, line: 77)
!86 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !37, line: 64, baseType: !78)
!87 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !88, file: !22, line: 79)
!88 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !46, line: 31, baseType: !89)
!89 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least8_t", file: !20, line: 53, baseType: !65)
!90 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !91, file: !22, line: 80)
!91 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !46, line: 32, baseType: !92)
!92 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least16_t", file: !20, line: 55, baseType: !69)
!93 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !94, file: !22, line: 81)
!94 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !46, line: 33, baseType: !95)
!95 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least32_t", file: !20, line: 57, baseType: !73)
!96 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !97, file: !22, line: 82)
!97 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !46, line: 34, baseType: !98)
!98 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least64_t", file: !20, line: 59, baseType: !77)
!99 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !100, file: !22, line: 84)
!100 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !37, line: 91, baseType: !101)
!101 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uintmax_t", file: !20, line: 73, baseType: !78)
!102 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !103, file: !22, line: 85)
!103 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !37, line: 79, baseType: !78)
!104 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !16, entity: !105, file: !108, line: 58)
!105 = !DIDerivedType(tag: DW_TAG_typedef, name: "max_align_t", file: !106, line: 24, baseType: !107)
!106 = !DIFile(filename: "/usr/lib/llvm-17/lib/clang/17/include/__stddef_max_align_t.h", directory: "", checksumkind: CSK_MD5, checksum: "48e8e2456f77e6cda35d245130fa7259")
!107 = !DICompositeType(tag: DW_TAG_structure_type, file: !106, line: 19, size: 256, flags: DIFlagFwdDecl, identifier: "_ZTS11max_align_t")
!108 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstddef", directory: "")
!109 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !12, entity: !110, file: !2, line: 10)
!110 = !DINamespace(name: "pallasSpec", scope: null)
!111 = !{i32 7, !"Dwarf Version", i32 5}
!112 = !{i32 2, !"Debug Info Version", i32 3}
!113 = !{i32 1, !"wchar_size", i32 4}
!114 = !{i32 8, !"PIC Level", i32 2}
!115 = !{i32 7, !"PIE Level", i32 2}
!116 = !{i32 7, !"uwtable", i32 2}
!117 = !{i32 7, !"frame-pointer", i32 2}
!118 = !{!"Ubuntu clang version 17.0.6 (9ubuntu1)"}
!119 = distinct !DISubprogram(name: "trig", linkageName: "_Z4trigi", scope: !11, file: !11, line: 22, type: !120, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!120 = !DISubroutineType(types: !121)
!121 = !{!122, !30}
!122 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!123 = !{}
!124 = !{!125, i1 true, i1 false, !123, !123, !127}
!125 = !{!"pallas.srcLoc", i64 18, i64 1, i64 21, i64 1, !126}
!126 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/Cpp/vstte10_inv_inj.cpp", directory: "", checksumkind: CSK_MD5, checksum: "136ee566b8a54a1013cd8a1c4ff22e53")
!127 = !{!"pallas.ensures", !128, ptr @_Z13PALLAS_SPEC_0i, !123, !123, !129}
!128 = !{!"pallas.srcLoc", i64 20, i64 1, i64 20, i64 32, !126}
!129 = !{!130}
!130 = !{!131, !132}
!131 = !DILocalVariable(name: "v", arg: 1, scope: !119, file: !11, line: 22, type: !30)
!132 = !DILocalVariable(name: "v", arg: 1, scope: !133, file: !11, line: 20, type: !30)
!133 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_Z13PALLAS_SPEC_0i", scope: !11, file: !11, line: 20, type: !120, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!134 = !DILocation(line: 0, scope: !119)
!135 = !DILocation(line: 23, column: 5, scope: !119)
!136 = distinct !DISubprogram(name: "invert", linkageName: "_Z6invertPiS_i", scope: !11, file: !11, line: 44, type: !137, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!137 = !DISubroutineType(types: !138)
!138 = !{null, !139, !139, !30}
!139 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !30, size: 64)
!140 = !{!141, i1 false, i1 false, !123, !123, !142, !157, !167, !177, !187, !197, !207, !217, !227, !237, !247, !257, !267}
!141 = !{!"pallas.srcLoc", i64 26, i64 1, i64 43, i64 1, !126}
!142 = !{!"pallas.requires", !143, ptr @_Z13PALLAS_SPEC_1PiS_i, !123, !123, !144}
!143 = !{!"pallas.srcLoc", i64 27, i64 1, i64 27, i64 16, !126}
!144 = !{!145, !151, !154}
!145 = !{!146, !147}
!146 = !DILocalVariable(name: "A", arg: 1, scope: !136, file: !11, line: 44, type: !139)
!147 = !DILocalVariable(name: "A", arg: 1, scope: !148, file: !11, line: 27, type: !139)
!148 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_Z13PALLAS_SPEC_1PiS_i", scope: !11, file: !11, line: 27, type: !149, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!149 = !DISubroutineType(types: !150)
!150 = !{!122, !139, !139, !30}
!151 = !{!152, !153}
!152 = !DILocalVariable(name: "B", arg: 2, scope: !136, file: !11, line: 44, type: !139)
!153 = !DILocalVariable(name: "B", arg: 2, scope: !148, file: !11, line: 27, type: !139)
!154 = !{!155, !156}
!155 = !DILocalVariable(name: "N", arg: 3, scope: !136, file: !11, line: 44, type: !30)
!156 = !DILocalVariable(name: "N", arg: 3, scope: !148, file: !11, line: 27, type: !30)
!157 = !{!"pallas.requires", !158, ptr @_Z13PALLAS_SPEC_2PiS_i, !123, !123, !159}
!158 = !{!"pallas.srcLoc", i64 28, i64 1, i64 28, i64 38, !126}
!159 = !{!160, !163, !165}
!160 = !{!146, !161}
!161 = !DILocalVariable(name: "A", arg: 1, scope: !162, file: !11, line: 28, type: !139)
!162 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_Z13PALLAS_SPEC_2PiS_i", scope: !11, file: !11, line: 28, type: !149, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!163 = !{!152, !164}
!164 = !DILocalVariable(name: "B", arg: 2, scope: !162, file: !11, line: 28, type: !139)
!165 = !{!155, !166}
!166 = !DILocalVariable(name: "N", arg: 3, scope: !162, file: !11, line: 28, type: !30)
!167 = !{!"pallas.requires", !168, ptr @_Z13PALLAS_SPEC_3PiS_i, !123, !123, !169}
!168 = !{!"pallas.srcLoc", i64 29, i64 1, i64 29, i64 50, !126}
!169 = !{!170, !173, !175}
!170 = !{!146, !171}
!171 = !DILocalVariable(name: "A", arg: 1, scope: !172, file: !11, line: 29, type: !139)
!172 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_Z13PALLAS_SPEC_3PiS_i", scope: !11, file: !11, line: 29, type: !149, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!173 = !{!152, !174}
!174 = !DILocalVariable(name: "B", arg: 2, scope: !172, file: !11, line: 29, type: !139)
!175 = !{!155, !176}
!176 = !DILocalVariable(name: "N", arg: 3, scope: !172, file: !11, line: 29, type: !30)
!177 = !{!"pallas.requires", !178, ptr @_Z13PALLAS_SPEC_4PiS_i, !123, !123, !179}
!178 = !{!"pallas.srcLoc", i64 30, i64 1, i64 30, i64 68, !126}
!179 = !{!180, !183, !185}
!180 = !{!146, !181}
!181 = !DILocalVariable(name: "A", arg: 1, scope: !182, file: !11, line: 30, type: !139)
!182 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_Z13PALLAS_SPEC_4PiS_i", scope: !11, file: !11, line: 30, type: !149, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!183 = !{!152, !184}
!184 = !DILocalVariable(name: "B", arg: 2, scope: !182, file: !11, line: 30, type: !139)
!185 = !{!155, !186}
!186 = !DILocalVariable(name: "N", arg: 3, scope: !182, file: !11, line: 30, type: !30)
!187 = !{!"pallas.requires", !188, ptr @_Z13PALLAS_SPEC_5PiS_i, !123, !123, !189}
!188 = !{!"pallas.srcLoc", i64 31, i64 1, i64 31, i64 61, !126}
!189 = !{!190, !193, !195}
!190 = !{!146, !191}
!191 = !DILocalVariable(name: "A", arg: 1, scope: !192, file: !11, line: 31, type: !139)
!192 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_Z13PALLAS_SPEC_5PiS_i", scope: !11, file: !11, line: 31, type: !149, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!193 = !{!152, !194}
!194 = !DILocalVariable(name: "B", arg: 2, scope: !192, file: !11, line: 31, type: !139)
!195 = !{!155, !196}
!196 = !DILocalVariable(name: "N", arg: 3, scope: !192, file: !11, line: 31, type: !30)
!197 = !{!"pallas.requires", !198, ptr @_Z13PALLAS_SPEC_6PiS_i, !123, !123, !199}
!198 = !{!"pallas.srcLoc", i64 32, i64 1, i64 32, i64 61, !126}
!199 = !{!200, !203, !205}
!200 = !{!146, !201}
!201 = !DILocalVariable(name: "A", arg: 1, scope: !202, file: !11, line: 32, type: !139)
!202 = distinct !DISubprogram(name: "PALLAS_SPEC_6", linkageName: "_Z13PALLAS_SPEC_6PiS_i", scope: !11, file: !11, line: 32, type: !149, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!203 = !{!152, !204}
!204 = !DILocalVariable(name: "B", arg: 2, scope: !202, file: !11, line: 32, type: !139)
!205 = !{!155, !206}
!206 = !DILocalVariable(name: "N", arg: 3, scope: !202, file: !11, line: 32, type: !30)
!207 = !{!"pallas.requires", !208, ptr @_Z13PALLAS_SPEC_7PiS_i, !123, !123, !209}
!208 = !{!"pallas.srcLoc", i64 33, i64 1, i64 34, i64 52, !126}
!209 = !{!210, !213, !215}
!210 = !{!146, !211}
!211 = !DILocalVariable(name: "A", arg: 1, scope: !212, file: !11, line: 33, type: !139)
!212 = distinct !DISubprogram(name: "PALLAS_SPEC_7", linkageName: "_Z13PALLAS_SPEC_7PiS_i", scope: !11, file: !11, line: 33, type: !149, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!213 = !{!152, !214}
!214 = !DILocalVariable(name: "B", arg: 2, scope: !212, file: !11, line: 33, type: !139)
!215 = !{!155, !216}
!216 = !DILocalVariable(name: "N", arg: 3, scope: !212, file: !11, line: 33, type: !30)
!217 = !{!"pallas.requires", !218, ptr @_Z13PALLAS_SPEC_8PiS_i, !123, !123, !219}
!218 = !{!"pallas.srcLoc", i64 35, i64 1, i64 36, i64 61, !126}
!219 = !{!220, !223, !225}
!220 = !{!146, !221}
!221 = !DILocalVariable(name: "A", arg: 1, scope: !222, file: !11, line: 35, type: !139)
!222 = distinct !DISubprogram(name: "PALLAS_SPEC_8", linkageName: "_Z13PALLAS_SPEC_8PiS_i", scope: !11, file: !11, line: 35, type: !149, scopeLine: 35, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!223 = !{!152, !224}
!224 = !DILocalVariable(name: "B", arg: 2, scope: !222, file: !11, line: 35, type: !139)
!225 = !{!155, !226}
!226 = !DILocalVariable(name: "N", arg: 3, scope: !222, file: !11, line: 35, type: !30)
!227 = !{!"pallas.ensures", !228, ptr @_Z13PALLAS_SPEC_9PiS_i, !123, !123, !229}
!228 = !{!"pallas.srcLoc", i64 37, i64 1, i64 37, i64 68, !126}
!229 = !{!230, !233, !235}
!230 = !{!146, !231}
!231 = !DILocalVariable(name: "A", arg: 1, scope: !232, file: !11, line: 37, type: !139)
!232 = distinct !DISubprogram(name: "PALLAS_SPEC_9", linkageName: "_Z13PALLAS_SPEC_9PiS_i", scope: !11, file: !11, line: 37, type: !149, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!233 = !{!152, !234}
!234 = !DILocalVariable(name: "B", arg: 2, scope: !232, file: !11, line: 37, type: !139)
!235 = !{!155, !236}
!236 = !DILocalVariable(name: "N", arg: 3, scope: !232, file: !11, line: 37, type: !30)
!237 = !{!"pallas.ensures", !238, ptr @_Z14PALLAS_SPEC_10PiS_i, !123, !123, !239}
!238 = !{!"pallas.srcLoc", i64 38, i64 1, i64 38, i64 61, !126}
!239 = !{!240, !243, !245}
!240 = !{!146, !241}
!241 = !DILocalVariable(name: "A", arg: 1, scope: !242, file: !11, line: 38, type: !139)
!242 = distinct !DISubprogram(name: "PALLAS_SPEC_10", linkageName: "_Z14PALLAS_SPEC_10PiS_i", scope: !11, file: !11, line: 38, type: !149, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!243 = !{!152, !244}
!244 = !DILocalVariable(name: "B", arg: 2, scope: !242, file: !11, line: 38, type: !139)
!245 = !{!155, !246}
!246 = !DILocalVariable(name: "N", arg: 3, scope: !242, file: !11, line: 38, type: !30)
!247 = !{!"pallas.ensures", !248, ptr @_Z14PALLAS_SPEC_11PiS_i, !123, !123, !249}
!248 = !{!"pallas.srcLoc", i64 39, i64 1, i64 39, i64 65, !126}
!249 = !{!250, !253, !255}
!250 = !{!146, !251}
!251 = !DILocalVariable(name: "A", arg: 1, scope: !252, file: !11, line: 39, type: !139)
!252 = distinct !DISubprogram(name: "PALLAS_SPEC_11", linkageName: "_Z14PALLAS_SPEC_11PiS_i", scope: !11, file: !11, line: 39, type: !149, scopeLine: 39, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!253 = !{!152, !254}
!254 = !DILocalVariable(name: "B", arg: 2, scope: !252, file: !11, line: 39, type: !139)
!255 = !{!155, !256}
!256 = !DILocalVariable(name: "N", arg: 3, scope: !252, file: !11, line: 39, type: !30)
!257 = !{!"pallas.ensures", !258, ptr @_Z14PALLAS_SPEC_12PiS_i, !123, !123, !259}
!258 = !{!"pallas.srcLoc", i64 40, i64 1, i64 40, i64 54, !126}
!259 = !{!260, !263, !265}
!260 = !{!146, !261}
!261 = !DILocalVariable(name: "A", arg: 1, scope: !262, file: !11, line: 40, type: !139)
!262 = distinct !DISubprogram(name: "PALLAS_SPEC_12", linkageName: "_Z14PALLAS_SPEC_12PiS_i", scope: !11, file: !11, line: 40, type: !149, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!263 = !{!152, !264}
!264 = !DILocalVariable(name: "B", arg: 2, scope: !262, file: !11, line: 40, type: !139)
!265 = !{!155, !266}
!266 = !DILocalVariable(name: "N", arg: 3, scope: !262, file: !11, line: 40, type: !30)
!267 = !{!"pallas.ensures", !268, ptr @_Z14PALLAS_SPEC_13PiS_i, !123, !123, !269}
!268 = !{!"pallas.srcLoc", i64 41, i64 1, i64 42, i64 52, !126}
!269 = !{!270, !273, !275}
!270 = !{!146, !271}
!271 = !DILocalVariable(name: "A", arg: 1, scope: !272, file: !11, line: 41, type: !139)
!272 = distinct !DISubprogram(name: "PALLAS_SPEC_13", linkageName: "_Z14PALLAS_SPEC_13PiS_i", scope: !11, file: !11, line: 41, type: !149, scopeLine: 41, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!273 = !{!152, !274}
!274 = !DILocalVariable(name: "B", arg: 2, scope: !272, file: !11, line: 41, type: !139)
!275 = !{!155, !276}
!276 = !DILocalVariable(name: "N", arg: 3, scope: !272, file: !11, line: 41, type: !30)
!277 = !DILocation(line: 0, scope: !136)
!278 = !DILocalVariable(name: "i", scope: !279, file: !11, line: 53, type: !30)
!279 = distinct !DILexicalBlock(scope: !136, file: !11, line: 53, column: 5)
!280 = !DILocation(line: 0, scope: !279)
!281 = !DILocation(line: 53, column: 10, scope: !279)
!282 = !DILocation(line: 53, scope: !279)
!283 = !DILocation(line: 53, column: 23, scope: !284)
!284 = distinct !DILexicalBlock(scope: !279, file: !11, line: 53, column: 5)
!285 = !DILocation(line: 53, column: 5, scope: !279)
!286 = !DILocation(line: 54, column: 11, scope: !287)
!287 = distinct !DILexicalBlock(scope: !284, file: !11, line: 53, column: 33)
!288 = !DILocation(line: 54, column: 9, scope: !287)
!289 = !DILocation(line: 54, column: 17, scope: !287)
!290 = !DILocation(line: 55, column: 5, scope: !287)
!291 = !DILocation(line: 53, column: 28, scope: !284)
!292 = !DILocation(line: 53, column: 5, scope: !284)
!293 = distinct !{!293, !285, !294, !295, !296}
!294 = !DILocation(line: 55, column: 5, scope: !279)
!295 = !{!"llvm.loop.mustprogress"}
!296 = !{!"pallas.loopInvBlock", !297, !298, !312, !324, !336, !348}
!297 = !{!"pallas.srcLoc", i64 46, i64 5, i64 52, i64 5, !126}
!298 = !{!"pallas.loopInv", !299, ptr @_Z14PALLAS_SPEC_14PiS_ii, !123, !123, !300}
!299 = !{!"pallas.srcLoc", i64 47, i64 5, i64 47, i64 36, !126}
!300 = !{!301, !306, !308, !310}
!301 = !{!146, !302}
!302 = !DILocalVariable(name: "A", arg: 1, scope: !303, file: !11, line: 47, type: !139)
!303 = distinct !DISubprogram(name: "PALLAS_SPEC_14", linkageName: "_Z14PALLAS_SPEC_14PiS_ii", scope: !11, file: !11, line: 47, type: !304, scopeLine: 47, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!304 = !DISubroutineType(types: !305)
!305 = !{!122, !139, !139, !30, !30}
!306 = !{!152, !307}
!307 = !DILocalVariable(name: "B", arg: 2, scope: !303, file: !11, line: 47, type: !139)
!308 = !{!155, !309}
!309 = !DILocalVariable(name: "N", arg: 3, scope: !303, file: !11, line: 47, type: !30)
!310 = !{!278, !311}
!311 = !DILocalVariable(name: "i", arg: 4, scope: !303, file: !11, line: 47, type: !30)
!312 = !{!"pallas.loopInv", !313, ptr @_Z14PALLAS_SPEC_15PiS_ii, !123, !123, !314}
!313 = !{!"pallas.srcLoc", i64 48, i64 5, i64 48, i64 78, !126}
!314 = !{!315, !318, !320, !322}
!315 = !{!146, !316}
!316 = !DILocalVariable(name: "A", arg: 1, scope: !317, file: !11, line: 48, type: !139)
!317 = distinct !DISubprogram(name: "PALLAS_SPEC_15", linkageName: "_Z14PALLAS_SPEC_15PiS_ii", scope: !11, file: !11, line: 48, type: !304, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!318 = !{!152, !319}
!319 = !DILocalVariable(name: "B", arg: 2, scope: !317, file: !11, line: 48, type: !139)
!320 = !{!155, !321}
!321 = !DILocalVariable(name: "N", arg: 3, scope: !317, file: !11, line: 48, type: !30)
!322 = !{!278, !323}
!323 = !DILocalVariable(name: "i", arg: 4, scope: !317, file: !11, line: 48, type: !30)
!324 = !{!"pallas.loopInv", !325, ptr @_Z14PALLAS_SPEC_16PiS_ii, !123, !123, !326}
!325 = !{!"pallas.srcLoc", i64 49, i64 5, i64 49, i64 71, !126}
!326 = !{!327, !330, !332, !334}
!327 = !{!146, !328}
!328 = !DILocalVariable(name: "A", arg: 1, scope: !329, file: !11, line: 49, type: !139)
!329 = distinct !DISubprogram(name: "PALLAS_SPEC_16", linkageName: "_Z14PALLAS_SPEC_16PiS_ii", scope: !11, file: !11, line: 49, type: !304, scopeLine: 49, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!330 = !{!152, !331}
!331 = !DILocalVariable(name: "B", arg: 2, scope: !329, file: !11, line: 49, type: !139)
!332 = !{!155, !333}
!333 = !DILocalVariable(name: "N", arg: 3, scope: !329, file: !11, line: 49, type: !30)
!334 = !{!278, !335}
!335 = !DILocalVariable(name: "i", arg: 4, scope: !329, file: !11, line: 49, type: !30)
!336 = !{!"pallas.loopInv", !337, ptr @_Z14PALLAS_SPEC_17PiS_ii, !123, !123, !338}
!337 = !{!"pallas.srcLoc", i64 50, i64 5, i64 50, i64 75, !126}
!338 = !{!339, !342, !344, !346}
!339 = !{!146, !340}
!340 = !DILocalVariable(name: "A", arg: 1, scope: !341, file: !11, line: 50, type: !139)
!341 = distinct !DISubprogram(name: "PALLAS_SPEC_17", linkageName: "_Z14PALLAS_SPEC_17PiS_ii", scope: !11, file: !11, line: 50, type: !304, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!342 = !{!152, !343}
!343 = !DILocalVariable(name: "B", arg: 2, scope: !341, file: !11, line: 50, type: !139)
!344 = !{!155, !345}
!345 = !DILocalVariable(name: "N", arg: 3, scope: !341, file: !11, line: 50, type: !30)
!346 = !{!278, !347}
!347 = !DILocalVariable(name: "i", arg: 4, scope: !341, file: !11, line: 50, type: !30)
!348 = !{!"pallas.loopInv", !349, ptr @_Z14PALLAS_SPEC_18PiS_ii, !123, !123, !350}
!349 = !{!"pallas.srcLoc", i64 51, i64 5, i64 51, i64 64, !126}
!350 = !{!351, !354, !356, !358}
!351 = !{!146, !352}
!352 = !DILocalVariable(name: "A", arg: 1, scope: !353, file: !11, line: 51, type: !139)
!353 = distinct !DISubprogram(name: "PALLAS_SPEC_18", linkageName: "_Z14PALLAS_SPEC_18PiS_ii", scope: !11, file: !11, line: 51, type: !304, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!354 = !{!152, !355}
!355 = !DILocalVariable(name: "B", arg: 2, scope: !353, file: !11, line: 51, type: !139)
!356 = !{!155, !357}
!357 = !DILocalVariable(name: "N", arg: 3, scope: !353, file: !11, line: 51, type: !30)
!358 = !{!278, !359}
!359 = !DILocalVariable(name: "i", arg: 4, scope: !353, file: !11, line: 51, type: !30)
!360 = !DILocation(line: 64, column: 5, scope: !136)
!361 = !{!362, !363}
!362 = !{!"pallas.srcLoc", i64 58, i64 5, i64 62, i64 5, !126}
!363 = !{!"pallas.assert", !364, ptr @_Z14PALLAS_SPEC_19PiS_i, !123, !123, !365}
!364 = !{!"pallas.srcLoc", i64 59, i64 5, i64 61, i64 55, !126}
!365 = !{!366, !369, !371}
!366 = !{!146, !367}
!367 = !DILocalVariable(name: "A", arg: 1, scope: !368, file: !11, line: 59, type: !139)
!368 = distinct !DISubprogram(name: "PALLAS_SPEC_19", linkageName: "_Z14PALLAS_SPEC_19PiS_i", scope: !11, file: !11, line: 59, type: !149, scopeLine: 59, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !10, retainedNodes: !123)
!369 = !{!152, !370}
!370 = !DILocalVariable(name: "B", arg: 2, scope: !368, file: !11, line: 59, type: !139)
!371 = !{!155, !372}
!372 = !DILocalVariable(name: "N", arg: 3, scope: !368, file: !11, line: 59, type: !30)
!373 = !{!""}
!374 = !DILocation(line: 0, scope: !133)
!375 = !DILocation(line: 20, column: 9, scope: !133)
!376 = !DILocation(line: 20, column: 25, scope: !133)
!377 = !DILocation(line: 0, scope: !148)
!378 = !DILocation(line: 27, column: 12, scope: !148)
!379 = !DILocation(line: 0, scope: !162)
!380 = !DILocation(line: 28, column: 12, scope: !162)
!381 = !DILocation(line: 28, column: 23, scope: !162)
!382 = !DILocation(line: 28, column: 28, scope: !162)
!383 = !DILocation(line: 0, scope: !172)
!384 = !DILocation(line: 29, column: 10, scope: !172)
!385 = !DILocation(line: 29, column: 27, scope: !172)
!386 = !DILocation(line: 29, column: 24, scope: !172)
!387 = !DILocation(line: 29, column: 29, scope: !172)
!388 = !DILocation(line: 29, column: 32, scope: !172)
!389 = !DILocation(line: 29, column: 49, scope: !172)
!390 = !DILocation(line: 29, column: 46, scope: !172)
!391 = !DILocation(line: 0, scope: !182)
!392 = !DILocation(line: 30, column: 19, scope: !182)
!393 = !DILocation(line: 30, column: 48, scope: !182)
!394 = !DILocation(line: 30, column: 46, scope: !182)
!395 = !DILocation(line: 30, column: 53, scope: !182)
!396 = !DILocation(line: 30, column: 39, scope: !182)
!397 = !DILocation(line: 30, column: 10, scope: !182)
!398 = !DILocation(line: 0, scope: !192)
!399 = !DILocation(line: 31, column: 19, scope: !192)
!400 = !DILocation(line: 31, column: 48, scope: !192)
!401 = !DILocation(line: 31, column: 46, scope: !192)
!402 = !DILocation(line: 31, column: 53, scope: !192)
!403 = !DILocation(line: 31, column: 39, scope: !192)
!404 = !DILocation(line: 31, column: 10, scope: !192)
!405 = !DILocation(line: 0, scope: !202)
!406 = !DILocation(line: 32, column: 19, scope: !202)
!407 = !DILocation(line: 32, column: 39, scope: !202)
!408 = !DILocation(line: 32, column: 10, scope: !202)
!409 = !DILocation(line: 0, scope: !212)
!410 = !DILocation(line: 33, column: 24, scope: !212)
!411 = !DILocation(line: 33, column: 44, scope: !212)
!412 = !DILocation(line: 33, column: 19, scope: !212)
!413 = !DILocation(line: 34, column: 26, scope: !212)
!414 = !DILocation(line: 34, column: 32, scope: !212)
!415 = !DILocation(line: 34, column: 29, scope: !212)
!416 = !DILocation(line: 34, column: 38, scope: !212)
!417 = !DILocation(line: 34, column: 36, scope: !212)
!418 = !DILocation(line: 34, column: 47, scope: !212)
!419 = !DILocation(line: 34, column: 45, scope: !212)
!420 = !DILocation(line: 34, column: 42, scope: !212)
!421 = !DILocation(line: 34, column: 19, scope: !212)
!422 = !DILocation(line: 33, column: 10, scope: !212)
!423 = !DILocation(line: 0, scope: !222)
!424 = !DILocation(line: 35, column: 19, scope: !222)
!425 = !DILocation(line: 35, column: 51, scope: !222)
!426 = !DILocation(line: 35, column: 46, scope: !222)
!427 = !DILocation(line: 36, column: 27, scope: !222)
!428 = !DILocation(line: 36, column: 49, scope: !222)
!429 = !DILocation(line: 36, column: 47, scope: !222)
!430 = !DILocation(line: 36, column: 56, scope: !222)
!431 = !DILocation(line: 36, column: 53, scope: !222)
!432 = !DILocation(line: 36, column: 19, scope: !222)
!433 = !DILocation(line: 35, column: 39, scope: !222)
!434 = !DILocation(line: 35, column: 10, scope: !222)
!435 = !DILocation(line: 0, scope: !232)
!436 = !DILocation(line: 37, column: 19, scope: !232)
!437 = !DILocation(line: 37, column: 48, scope: !232)
!438 = !DILocation(line: 37, column: 46, scope: !232)
!439 = !DILocation(line: 37, column: 53, scope: !232)
!440 = !DILocation(line: 37, column: 39, scope: !232)
!441 = !DILocation(line: 37, column: 10, scope: !232)
!442 = !DILocation(line: 0, scope: !242)
!443 = !DILocation(line: 38, column: 19, scope: !242)
!444 = !DILocation(line: 38, column: 48, scope: !242)
!445 = !DILocation(line: 38, column: 46, scope: !242)
!446 = !DILocation(line: 38, column: 53, scope: !242)
!447 = !DILocation(line: 38, column: 39, scope: !242)
!448 = !DILocation(line: 38, column: 10, scope: !242)
!449 = !DILocation(line: 0, scope: !252)
!450 = !DILocation(line: 39, column: 19, scope: !252)
!451 = !DILocation(line: 39, column: 41, scope: !252)
!452 = !DILocation(line: 39, column: 39, scope: !252)
!453 = !DILocation(line: 39, column: 60, scope: !252)
!454 = !DILocation(line: 39, column: 58, scope: !252)
!455 = !DILocation(line: 39, column: 48, scope: !252)
!456 = !DILocation(line: 39, column: 45, scope: !252)
!457 = !DILocation(line: 39, column: 10, scope: !252)
!458 = !DILocation(line: 0, scope: !262)
!459 = !DILocation(line: 40, column: 19, scope: !262)
!460 = !DILocation(line: 40, column: 43, scope: !262)
!461 = !DILocation(line: 40, column: 41, scope: !262)
!462 = !DILocation(line: 40, column: 39, scope: !262)
!463 = !DILocation(line: 40, column: 51, scope: !262)
!464 = !DILocation(line: 40, column: 48, scope: !262)
!465 = !DILocation(line: 40, column: 10, scope: !262)
!466 = !DILocation(line: 0, scope: !272)
!467 = !DILocation(line: 41, column: 24, scope: !272)
!468 = !DILocation(line: 41, column: 44, scope: !272)
!469 = !DILocation(line: 41, column: 19, scope: !272)
!470 = !DILocation(line: 42, column: 26, scope: !272)
!471 = !DILocation(line: 42, column: 32, scope: !272)
!472 = !DILocation(line: 42, column: 29, scope: !272)
!473 = !DILocation(line: 42, column: 38, scope: !272)
!474 = !DILocation(line: 42, column: 36, scope: !272)
!475 = !DILocation(line: 42, column: 47, scope: !272)
!476 = !DILocation(line: 42, column: 45, scope: !272)
!477 = !DILocation(line: 42, column: 42, scope: !272)
!478 = !DILocation(line: 42, column: 19, scope: !272)
!479 = !DILocation(line: 41, column: 10, scope: !272)
!480 = !DILocation(line: 0, scope: !317)
!481 = !DILocation(line: 48, column: 29, scope: !317)
!482 = !DILocation(line: 48, column: 58, scope: !317)
!483 = !DILocation(line: 48, column: 56, scope: !317)
!484 = !DILocation(line: 48, column: 63, scope: !317)
!485 = !DILocation(line: 48, column: 49, scope: !317)
!486 = !DILocation(line: 48, column: 20, scope: !317)
!487 = !DILocation(line: 0, scope: !303)
!488 = !DILocation(line: 47, column: 22, scope: !303)
!489 = !DILocation(line: 47, column: 27, scope: !303)
!490 = !DILocation(line: 47, column: 32, scope: !303)
!491 = !DILocation(line: 0, scope: !341)
!492 = !DILocation(line: 50, column: 29, scope: !341)
!493 = !DILocation(line: 50, column: 51, scope: !341)
!494 = !DILocation(line: 50, column: 49, scope: !341)
!495 = !DILocation(line: 50, column: 70, scope: !341)
!496 = !DILocation(line: 50, column: 68, scope: !341)
!497 = !DILocation(line: 50, column: 58, scope: !341)
!498 = !DILocation(line: 50, column: 55, scope: !341)
!499 = !DILocation(line: 50, column: 20, scope: !341)
!500 = !DILocation(line: 0, scope: !353)
!501 = !DILocation(line: 51, column: 29, scope: !353)
!502 = !DILocation(line: 51, column: 53, scope: !353)
!503 = !DILocation(line: 51, column: 51, scope: !353)
!504 = !DILocation(line: 51, column: 49, scope: !353)
!505 = !DILocation(line: 51, column: 61, scope: !353)
!506 = !DILocation(line: 51, column: 58, scope: !353)
!507 = !DILocation(line: 51, column: 20, scope: !353)
!508 = !DILocation(line: 0, scope: !329)
!509 = !DILocation(line: 49, column: 29, scope: !329)
!510 = !DILocation(line: 49, column: 58, scope: !329)
!511 = !DILocation(line: 49, column: 56, scope: !329)
!512 = !DILocation(line: 49, column: 63, scope: !329)
!513 = !DILocation(line: 49, column: 49, scope: !329)
!514 = !DILocation(line: 49, column: 20, scope: !329)
!515 = !DILocation(line: 0, scope: !368)
!516 = !DILocation(line: 59, column: 26, scope: !368)
!517 = !DILocation(line: 59, column: 46, scope: !368)
!518 = !DILocation(line: 59, column: 21, scope: !368)
!519 = !DILocation(line: 60, column: 38, scope: !368)
!520 = !DILocation(line: 60, column: 33, scope: !368)
!521 = !DILocation(line: 60, column: 48, scope: !368)
!522 = !DILocation(line: 60, column: 43, scope: !368)
!523 = !DILocation(line: 60, column: 28, scope: !368)
!524 = !DILocation(line: 61, column: 28, scope: !368)
!525 = !DILocation(line: 61, column: 34, scope: !368)
!526 = !DILocation(line: 61, column: 31, scope: !368)
!527 = !DILocation(line: 61, column: 40, scope: !368)
!528 = !DILocation(line: 61, column: 38, scope: !368)
!529 = !DILocation(line: 61, column: 49, scope: !368)
!530 = !DILocation(line: 61, column: 47, scope: !368)
!531 = !DILocation(line: 61, column: 44, scope: !368)
!532 = !DILocation(line: 61, column: 21, scope: !368)
!533 = !DILocation(line: 60, column: 21, scope: !368)
!534 = !DILocation(line: 59, column: 12, scope: !368)
!535 = !{!"pallas.result"}
!536 = !{!"pallas.ptrLength"}
!537 = !{!"pallas.exists"}
!538 = !{!"pallas.old"}
!539 = !{!"pallas.forallSep"}
!540 = !{!"pallas.perm"}
!541 = !{!"pallas.fracOf"}
!542 = !{!"pallas.forall"}
!543 = !{!"pallas.imply"}
!544 = !{!"pallas.scAnd"}
!545 = !{!"pallas.boundVar"}
