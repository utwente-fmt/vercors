; ModuleID = 'tmp_spectral/tmp_ir_source0.ll'
source_filename = "examples/publications/2026/ATVA2026Spectral/Cpp/vstte10_sum_max.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.SumMaxRes = type { i32, i32, i64, i64 }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.compiler.used = appending global [22 x ptr] [ptr @_Z13PALLAS_SPEC_0Pii, ptr @_Z13PALLAS_SPEC_1Pii, ptr @_Z13PALLAS_SPEC_2Pii, ptr @_Z13PALLAS_SPEC_3Pii, ptr @_Z13PALLAS_SPEC_4Pii, ptr @_Z13PALLAS_SPEC_5Pii, ptr @_Z13PALLAS_SPEC_6Pii, ptr @_Z13PALLAS_SPEC_7Pii, ptr @_Z13PALLAS_SPEC_8Pii, ptr @_Z13PALLAS_SPEC_9Pii, ptr @_Z14PALLAS_SPEC_10Pii, ptr @_Z14PALLAS_SPEC_11Pii, ptr @_Z14PALLAS_SPEC_13Piiiii, ptr @_Z14PALLAS_SPEC_12Piiiii, ptr @_Z14PALLAS_SPEC_15Piiiii, ptr @_Z14PALLAS_SPEC_16Piiiii, ptr @_Z14PALLAS_SPEC_17Piiiii, ptr @_Z14PALLAS_SPEC_18Piiiii, ptr @_Z14PALLAS_SPEC_14Piiiii, ptr @_Z14PALLAS_SPEC_19Piiiii, ptr @_Z14PALLAS_SPEC_20Piiiii, ptr @_Z14PALLAS_SPEC_21Piiiii], section "llvm.metadata"
@llvm.used = appending global [22 x ptr] [ptr @_Z13PALLAS_SPEC_0Pii, ptr @_Z13PALLAS_SPEC_1Pii, ptr @_Z13PALLAS_SPEC_2Pii, ptr @_Z13PALLAS_SPEC_3Pii, ptr @_Z13PALLAS_SPEC_4Pii, ptr @_Z13PALLAS_SPEC_5Pii, ptr @_Z13PALLAS_SPEC_6Pii, ptr @_Z13PALLAS_SPEC_7Pii, ptr @_Z13PALLAS_SPEC_8Pii, ptr @_Z13PALLAS_SPEC_9Pii, ptr @_Z14PALLAS_SPEC_10Pii, ptr @_Z14PALLAS_SPEC_11Pii, ptr @_Z14PALLAS_SPEC_13Piiiii, ptr @_Z14PALLAS_SPEC_12Piiiii, ptr @_Z14PALLAS_SPEC_15Piiiii, ptr @_Z14PALLAS_SPEC_16Piiiii, ptr @_Z14PALLAS_SPEC_17Piiiii, ptr @_Z14PALLAS_SPEC_18Piiiii, ptr @_Z14PALLAS_SPEC_14Piiiii, ptr @_Z14PALLAS_SPEC_19Piiiii, ptr @_Z14PALLAS_SPEC_20Piiiii, ptr @_Z14PALLAS_SPEC_21Piiiii], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"j\00", align 1, !dbg !0

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef i32 @_Z6arrSumPii(ptr noundef %0, i32 noundef %1) #0 !dbg !117 !pallas.fcontract !122 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !129, metadata !DIExpression()), !dbg !154
  call void @llvm.dbg.value(metadata i32 %1, metadata !136, metadata !DIExpression()), !dbg !154
  %3 = icmp eq i32 %1, 0, !dbg !155
  br i1 %3, label %4, label %5, !dbg !156

4:                                                ; preds = %2
  br label %13, !dbg !156

5:                                                ; preds = %2
  %6 = sub nsw i32 %1, 1, !dbg !157
  %7 = call noundef i32 @_Z6arrSumPii(ptr noundef %0, i32 noundef %6), !dbg !158
  %8 = sub nsw i32 %1, 1, !dbg !159
  %9 = sext i32 %8 to i64, !dbg !160
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !160
  %11 = load i32, ptr %10, align 4, !dbg !160
  %12 = add nsw i32 %7, %11, !dbg !161
  br label %13, !dbg !156

13:                                               ; preds = %5, %4
  %14 = phi i32 [ 0, %4 ], [ %12, %5 ], !dbg !156
  ret i32 %14, !dbg !162
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local void @_Z9getSumMaxPii(ptr noalias sret(%struct.SumMaxRes) align 8 %0, ptr noundef %1, i32 noundef %2) #2 !dbg !163 !pallas.fcontract !173 {
  call void @llvm.dbg.value(metadata ptr %1, metadata !179, metadata !DIExpression()), !dbg !249
  call void @llvm.dbg.value(metadata i32 %2, metadata !183, metadata !DIExpression()), !dbg !249
  call void @llvm.dbg.value(metadata i32 0, metadata !250, metadata !DIExpression()), !dbg !249
  call void @llvm.dbg.value(metadata i32 0, metadata !251, metadata !DIExpression()), !dbg !249
  call void @llvm.dbg.value(metadata i32 0, metadata !252, metadata !DIExpression()), !dbg !254
  br label %4, !dbg !255

4:                                                ; preds = %14, %3
  %.02 = phi i32 [ 0, %3 ], [ %13, %14 ], !dbg !249
  %.01 = phi i32 [ 0, %3 ], [ %.1, %14 ], !dbg !249
  %.0 = phi i32 [ 0, %3 ], [ %15, %14 ], !dbg !256
  call void @llvm.dbg.value(metadata i32 %.0, metadata !252, metadata !DIExpression()), !dbg !254
  call void @llvm.dbg.value(metadata i32 %.01, metadata !251, metadata !DIExpression()), !dbg !249
  call void @llvm.dbg.value(metadata i32 %.02, metadata !250, metadata !DIExpression()), !dbg !249
  %5 = icmp slt i32 %.0, %2, !dbg !257
  br i1 %5, label %6, label %16, !dbg !259

6:                                                ; preds = %4
  %7 = sext i32 %.0 to i64, !dbg !260
  %8 = getelementptr inbounds i32, ptr %1, i64 %7, !dbg !260
  %9 = load i32, ptr %8, align 4, !dbg !260
  call void @llvm.dbg.value(metadata i32 %9, metadata !262, metadata !DIExpression()), !dbg !263
  %10 = icmp sgt i32 %9, %.01, !dbg !264
  br i1 %10, label %11, label %12, !dbg !266

11:                                               ; preds = %6
  call void @llvm.dbg.value(metadata i32 %9, metadata !251, metadata !DIExpression()), !dbg !249
  br label %12, !dbg !267

12:                                               ; preds = %11, %6
  %.1 = phi i32 [ %9, %11 ], [ %.01, %6 ], !dbg !249
  call void @llvm.dbg.value(metadata i32 %.1, metadata !251, metadata !DIExpression()), !dbg !249
  %13 = add nsw i32 %.02, %9, !dbg !269
  call void @llvm.dbg.value(metadata i32 %13, metadata !250, metadata !DIExpression()), !dbg !249
  br label %14, !dbg !270

14:                                               ; preds = %12
  %15 = add nsw i32 %.0, 1, !dbg !271
  call void @llvm.dbg.value(metadata i32 %15, metadata !252, metadata !DIExpression()), !dbg !254
  br label %4, !dbg !272, !llvm.loop !273

16:                                               ; preds = %4
  call void @llvm.dbg.declare(metadata ptr %0, metadata !420, metadata !DIExpression()), !dbg !421
  %17 = getelementptr inbounds %struct.SumMaxRes, ptr %0, i32 0, i32 0, !dbg !422
  store i32 %.02, ptr %17, align 8, !dbg !422
  %18 = getelementptr inbounds %struct.SumMaxRes, ptr %0, i32 0, i32 1, !dbg !422
  store i32 %.01, ptr %18, align 4, !dbg !422
  %19 = getelementptr inbounds %struct.SumMaxRes, ptr %0, i32 0, i32 2, !dbg !422
  store i64 0, ptr %19, align 8, !dbg !422
  %20 = getelementptr inbounds %struct.SumMaxRes, ptr %0, i32 0, i32 3, !dbg !422
  store i64 0, ptr %20, align 8, !dbg !422
  ret void, !dbg !423
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_0Pii(ptr noundef %0, i32 noundef %1) #2 !dbg !131 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !130, metadata !DIExpression()), !dbg !425
  call void @llvm.dbg.value(metadata i32 %1, metadata !137, metadata !DIExpression()), !dbg !425
  %3 = icmp ne ptr %0, null, !dbg !426
  ret i1 %3, !dbg !425
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_1Pii(ptr noundef %0, i32 noundef %1) #0 !dbg !143 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !142, metadata !DIExpression()), !dbg !427
  call void @llvm.dbg.value(metadata i32 %1, metadata !145, metadata !DIExpression()), !dbg !427
  %3 = icmp sle i32 0, %1, !dbg !428
  br i1 %3, label %4, label %8, !dbg !429

4:                                                ; preds = %2
  %5 = sext i32 %1 to i64, !dbg !430
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !431
  %7 = icmp ule i64 %5, %6, !dbg !432
  br label %8

8:                                                ; preds = %4, %2
  %9 = phi i1 [ false, %2 ], [ %7, %4 ], !dbg !427
  ret i1 %9, !dbg !427
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_2Pii(ptr noundef %0, i32 noundef %1) #0 !dbg !151 !pallas.exprWrapper !424 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !150, metadata !DIExpression()), !dbg !433
  call void @llvm.dbg.value(metadata i32 %1, metadata !153, metadata !DIExpression()), !dbg !433
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !434
  %5 = icmp sle i32 0, %4, !dbg !434
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !434
  %7 = icmp slt i32 %6, %1, !dbg !434
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !434
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !435
  %10 = sext i32 %9 to i64, !dbg !436
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !436
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 100), !dbg !437
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !438
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !439
  ret i1 %13, !dbg !433
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_3Pii(ptr noundef %0, i32 noundef %1) #2 !dbg !181 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !180, metadata !DIExpression()), !dbg !440
  call void @llvm.dbg.value(metadata i32 %1, metadata !184, metadata !DIExpression()), !dbg !440
  %3 = icmp ne ptr %0, null, !dbg !441
  ret i1 %3, !dbg !440
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_4Pii(ptr noundef %0, i32 noundef %1) #0 !dbg !190 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !189, metadata !DIExpression()), !dbg !442
  call void @llvm.dbg.value(metadata i32 %1, metadata !192, metadata !DIExpression()), !dbg !442
  %3 = icmp sle i32 0, %1, !dbg !443
  br i1 %3, label %4, label %8, !dbg !444

4:                                                ; preds = %2
  %5 = sext i32 %1 to i64, !dbg !445
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !446
  %7 = icmp eq i64 %5, %6, !dbg !447
  br label %8

8:                                                ; preds = %4, %2
  %9 = phi i1 [ false, %2 ], [ %7, %4 ], !dbg !442
  ret i1 %9, !dbg !442
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_5Pii(ptr noundef %0, i32 noundef %1) #0 !dbg !198 !pallas.exprWrapper !424 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !197, metadata !DIExpression()), !dbg !448
  call void @llvm.dbg.value(metadata i32 %1, metadata !200, metadata !DIExpression()), !dbg !448
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !449
  %5 = icmp sle i32 0, %4, !dbg !449
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !449
  %7 = icmp slt i32 %6, %1, !dbg !449
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !449
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !450
  %10 = sext i32 %9 to i64, !dbg !451
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !451
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !452
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !453
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !454
  ret i1 %13, !dbg !448
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_6Pii(ptr noundef %0, i32 noundef %1) #0 !dbg !206 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !205, metadata !DIExpression()), !dbg !455
  call void @llvm.dbg.value(metadata i32 %1, metadata !208, metadata !DIExpression()), !dbg !455
  %3 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !456
  %4 = icmp sle i32 0, %3, !dbg !456
  %5 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !456
  %6 = icmp slt i32 %5, %1, !dbg !456
  %7 = call i1 @pallas.scAnd(i1 %4, i1 %6), !dbg !456
  %8 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !457
  %9 = sext i32 %8 to i64, !dbg !458
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !458
  %11 = load i32, ptr %10, align 4, !dbg !458
  %12 = icmp sge i32 %11, 0, !dbg !459
  %13 = call i1 @pallas.forall(i1 %7, i1 %12), !dbg !460
  ret i1 %13, !dbg !455
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_7Pii(ptr noundef %0, i32 noundef %1) #0 !dbg !214 !pallas.exprWrapper !424 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !213, metadata !DIExpression()), !dbg !461
  call void @llvm.dbg.value(metadata i32 %1, metadata !216, metadata !DIExpression()), !dbg !461
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !462
  %5 = icmp sle i32 0, %4, !dbg !462
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !462
  %7 = icmp slt i32 %6, %1, !dbg !462
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !462
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !463
  %10 = sext i32 %9 to i64, !dbg !464
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !464
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !465
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !466
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !467
  ret i1 %13, !dbg !461
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_8Pii(ptr noundef %0, i32 noundef %1) #0 !dbg !222 !pallas.exprWrapper !424 {
  %3 = alloca %struct.SumMaxRes, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !221, metadata !DIExpression()), !dbg !468
  call void @llvm.dbg.value(metadata i32 %1, metadata !224, metadata !DIExpression()), !dbg !468
  %4 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !469
  %5 = icmp sle i32 0, %4, !dbg !469
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !469
  %7 = icmp slt i32 %6, %1, !dbg !469
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !469
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !470
  %10 = sext i32 %9 to i64, !dbg !471
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !471
  %12 = load i32, ptr %11, align 4, !dbg !471
  call void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8 %3), !dbg !472
  %13 = getelementptr inbounds %struct.SumMaxRes, ptr %3, i32 0, i32 1, !dbg !473
  %14 = load i32, ptr %13, align 4, !dbg !473
  %15 = icmp sle i32 %12, %14, !dbg !474
  %16 = call i1 @pallas.forall(i1 %8, i1 %15), !dbg !475
  ret i1 %16, !dbg !468
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_9Pii(ptr noundef %0, i32 noundef %1) #0 !dbg !230 !pallas.exprWrapper !424 {
  %3 = alloca %struct.SumMaxRes, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !229, metadata !DIExpression()), !dbg !476
  call void @llvm.dbg.value(metadata i32 %1, metadata !232, metadata !DIExpression()), !dbg !476
  %4 = icmp sgt i32 %1, 0, !dbg !477
  %5 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !478
  %6 = icmp sle i32 0, %5, !dbg !478
  %7 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !478
  %8 = icmp slt i32 %7, %1, !dbg !478
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !478
  %10 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !479
  %11 = sext i32 %10 to i64, !dbg !480
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !480
  %13 = load i32, ptr %12, align 4, !dbg !480
  call void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8 %3), !dbg !481
  %14 = getelementptr inbounds %struct.SumMaxRes, ptr %3, i32 0, i32 1, !dbg !482
  %15 = load i32, ptr %14, align 4, !dbg !482
  %16 = icmp eq i32 %13, %15, !dbg !483
  %17 = call i1 @pallas.exists(i1 %9, i1 %16), !dbg !484
  %18 = call i1 @pallas.imply(i1 %4, i1 %17), !dbg !485
  ret i1 %18, !dbg !476
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_10Pii(ptr noundef %0, i32 noundef %1) #0 !dbg !238 !pallas.exprWrapper !424 {
  %3 = alloca %struct.SumMaxRes, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !237, metadata !DIExpression()), !dbg !486
  call void @llvm.dbg.value(metadata i32 %1, metadata !240, metadata !DIExpression()), !dbg !486
  call void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8 %3), !dbg !487
  %4 = getelementptr inbounds %struct.SumMaxRes, ptr %3, i32 0, i32 0, !dbg !488
  %5 = load i32, ptr %4, align 8, !dbg !488
  %6 = call noundef i32 @_Z6arrSumPii(ptr noundef %0, i32 noundef %1), !dbg !489
  %7 = icmp eq i32 %5, %6, !dbg !490
  ret i1 %7, !dbg !486
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_11Pii(ptr noundef %0, i32 noundef %1) #0 !dbg !246 !pallas.exprWrapper !424 {
  %3 = alloca %struct.SumMaxRes, align 8
  %4 = alloca %struct.SumMaxRes, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !245, metadata !DIExpression()), !dbg !491
  call void @llvm.dbg.value(metadata i32 %1, metadata !248, metadata !DIExpression()), !dbg !491
  call void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8 %3), !dbg !492
  %5 = getelementptr inbounds %struct.SumMaxRes, ptr %3, i32 0, i32 0, !dbg !493
  %6 = load i32, ptr %5, align 8, !dbg !493
  call void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8 %4), !dbg !494
  %7 = getelementptr inbounds %struct.SumMaxRes, ptr %4, i32 0, i32 1, !dbg !495
  %8 = load i32, ptr %7, align 4, !dbg !495
  %9 = mul nsw i32 %8, %1, !dbg !496
  %10 = icmp sle i32 %6, %9, !dbg !497
  ret i1 %10, !dbg !491
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_13Piiiii(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !299 !pallas.exprWrapper !424 {
  %6 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !298, metadata !DIExpression()), !dbg !498
  call void @llvm.dbg.value(metadata i32 %1, metadata !301, metadata !DIExpression()), !dbg !498
  call void @llvm.dbg.value(metadata i32 %2, metadata !303, metadata !DIExpression()), !dbg !498
  call void @llvm.dbg.value(metadata i32 %3, metadata !305, metadata !DIExpression()), !dbg !498
  call void @llvm.dbg.value(metadata i32 %4, metadata !307, metadata !DIExpression()), !dbg !498
  %7 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !499
  %8 = icmp sle i32 0, %7, !dbg !499
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !499
  %10 = icmp slt i32 %9, %1, !dbg !499
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !499
  %12 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !500
  %13 = sext i32 %12 to i64, !dbg !501
  %14 = getelementptr inbounds i32, ptr %0, i64 %13, !dbg !501
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %6, i32 noundef 1, i32 noundef 4), !dbg !502
  %15 = call i1 @pallas.perm(ptr noundef %14, ptr noundef byval(%pallas.fracT) %6), !dbg !503
  %16 = call i1 @pallas.forallSep(i1 %11, i1 %15), !dbg !504
  ret i1 %16, !dbg !498
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_12Piiiii(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #2 !dbg !283 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !282, metadata !DIExpression()), !dbg !505
  call void @llvm.dbg.value(metadata i32 %1, metadata !287, metadata !DIExpression()), !dbg !505
  call void @llvm.dbg.value(metadata i32 %2, metadata !289, metadata !DIExpression()), !dbg !505
  call void @llvm.dbg.value(metadata i32 %3, metadata !291, metadata !DIExpression()), !dbg !505
  call void @llvm.dbg.value(metadata i32 %4, metadata !293, metadata !DIExpression()), !dbg !505
  %6 = icmp sle i32 0, %4, !dbg !506
  br i1 %6, label %7, label %9, !dbg !507

7:                                                ; preds = %5
  %8 = icmp sle i32 %4, %1, !dbg !508
  br label %9

9:                                                ; preds = %7, %5
  %10 = phi i1 [ false, %5 ], [ %8, %7 ], !dbg !505
  ret i1 %10, !dbg !505
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_15Piiiii(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !327 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !326, metadata !DIExpression()), !dbg !509
  call void @llvm.dbg.value(metadata i32 %1, metadata !329, metadata !DIExpression()), !dbg !509
  call void @llvm.dbg.value(metadata i32 %2, metadata !331, metadata !DIExpression()), !dbg !509
  call void @llvm.dbg.value(metadata i32 %3, metadata !333, metadata !DIExpression()), !dbg !509
  call void @llvm.dbg.value(metadata i32 %4, metadata !335, metadata !DIExpression()), !dbg !509
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !510
  %7 = icmp sle i32 0, %6, !dbg !510
  %8 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !510
  %9 = icmp slt i32 %8, %1, !dbg !510
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !510
  %11 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !511
  %12 = sext i32 %11 to i64, !dbg !512
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !512
  %14 = load i32, ptr %13, align 4, !dbg !512
  %15 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !513
  %16 = sext i32 %15 to i64, !dbg !514
  %17 = getelementptr inbounds i32, ptr %0, i64 %16, !dbg !514
  %18 = load i32, ptr %17, align 4, !dbg !514
  %19 = call noundef i32 @"pallas.old noundef i32_noundef i32"(i32 noundef %18), !dbg !515
  %20 = icmp sge i32 %14, %19, !dbg !516
  %21 = call i1 @pallas.forall(i1 %10, i1 %20), !dbg !517
  ret i1 %21, !dbg !509
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_16Piiiii(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !341 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !340, metadata !DIExpression()), !dbg !518
  call void @llvm.dbg.value(metadata i32 %1, metadata !343, metadata !DIExpression()), !dbg !518
  call void @llvm.dbg.value(metadata i32 %2, metadata !345, metadata !DIExpression()), !dbg !518
  call void @llvm.dbg.value(metadata i32 %3, metadata !347, metadata !DIExpression()), !dbg !518
  call void @llvm.dbg.value(metadata i32 %4, metadata !349, metadata !DIExpression()), !dbg !518
  %6 = icmp eq i32 %4, 0, !dbg !519
  %7 = icmp eq i32 %3, 0, !dbg !520
  %8 = call i1 @pallas.imply(i1 %6, i1 %7), !dbg !521
  ret i1 %8, !dbg !518
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_17Piiiii(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !355 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !354, metadata !DIExpression()), !dbg !522
  call void @llvm.dbg.value(metadata i32 %1, metadata !357, metadata !DIExpression()), !dbg !522
  call void @llvm.dbg.value(metadata i32 %2, metadata !359, metadata !DIExpression()), !dbg !522
  call void @llvm.dbg.value(metadata i32 %3, metadata !361, metadata !DIExpression()), !dbg !522
  call void @llvm.dbg.value(metadata i32 %4, metadata !363, metadata !DIExpression()), !dbg !522
  %6 = icmp eq i32 %4, 1, !dbg !523
  %7 = icmp sgt i32 %1, 0, !dbg !524
  %8 = call i1 @pallas.scAnd(i1 %6, i1 %7), !dbg !525
  %9 = getelementptr inbounds i32, ptr %0, i64 0, !dbg !526
  %10 = load i32, ptr %9, align 4, !dbg !526
  %11 = icmp eq i32 %3, %10, !dbg !527
  %12 = call i1 @pallas.imply(i1 %8, i1 %11), !dbg !528
  ret i1 %12, !dbg !522
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_18Piiiii(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !369 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !368, metadata !DIExpression()), !dbg !529
  call void @llvm.dbg.value(metadata i32 %1, metadata !371, metadata !DIExpression()), !dbg !529
  call void @llvm.dbg.value(metadata i32 %2, metadata !373, metadata !DIExpression()), !dbg !529
  call void @llvm.dbg.value(metadata i32 %3, metadata !375, metadata !DIExpression()), !dbg !529
  call void @llvm.dbg.value(metadata i32 %4, metadata !377, metadata !DIExpression()), !dbg !529
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !530
  %7 = icmp sle i32 0, %6, !dbg !530
  %8 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !530
  %9 = icmp slt i32 %8, %4, !dbg !530
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !530
  %11 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !531
  %12 = sext i32 %11 to i64, !dbg !532
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !532
  %14 = load i32, ptr %13, align 4, !dbg !532
  %15 = icmp sle i32 %14, %3, !dbg !533
  %16 = call i1 @pallas.forall(i1 %10, i1 %15), !dbg !534
  ret i1 %16, !dbg !529
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_14Piiiii(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !313 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !312, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.value(metadata i32 %1, metadata !315, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.value(metadata i32 %2, metadata !317, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.value(metadata i32 %3, metadata !319, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.value(metadata i32 %4, metadata !321, metadata !DIExpression()), !dbg !535
  %6 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !536
  %7 = icmp sle i32 0, %6, !dbg !536
  %8 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !536
  %9 = icmp slt i32 %8, %1, !dbg !536
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !536
  %11 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !537
  %12 = sext i32 %11 to i64, !dbg !538
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !538
  %14 = load i32, ptr %13, align 4, !dbg !538
  %15 = icmp sge i32 %14, 0, !dbg !539
  %16 = call i1 @pallas.forall(i1 %10, i1 %15), !dbg !540
  ret i1 %16, !dbg !535
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_19Piiiii(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !383 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !382, metadata !DIExpression()), !dbg !541
  call void @llvm.dbg.value(metadata i32 %1, metadata !385, metadata !DIExpression()), !dbg !541
  call void @llvm.dbg.value(metadata i32 %2, metadata !387, metadata !DIExpression()), !dbg !541
  call void @llvm.dbg.value(metadata i32 %3, metadata !389, metadata !DIExpression()), !dbg !541
  call void @llvm.dbg.value(metadata i32 %4, metadata !391, metadata !DIExpression()), !dbg !541
  %6 = icmp sgt i32 %4, 0, !dbg !542
  %7 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !543
  %8 = icmp sle i32 0, %7, !dbg !543
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !543
  %10 = icmp slt i32 %9, %4, !dbg !543
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !543
  %12 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !544
  %13 = sext i32 %12 to i64, !dbg !545
  %14 = getelementptr inbounds i32, ptr %0, i64 %13, !dbg !545
  %15 = load i32, ptr %14, align 4, !dbg !545
  %16 = icmp eq i32 %15, %3, !dbg !546
  %17 = call i1 @pallas.exists(i1 %11, i1 %16), !dbg !547
  %18 = call i1 @pallas.imply(i1 %6, i1 %17), !dbg !548
  ret i1 %18, !dbg !541
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_20Piiiii(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !397 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !396, metadata !DIExpression()), !dbg !549
  call void @llvm.dbg.value(metadata i32 %1, metadata !399, metadata !DIExpression()), !dbg !549
  call void @llvm.dbg.value(metadata i32 %2, metadata !401, metadata !DIExpression()), !dbg !549
  call void @llvm.dbg.value(metadata i32 %3, metadata !403, metadata !DIExpression()), !dbg !549
  call void @llvm.dbg.value(metadata i32 %4, metadata !405, metadata !DIExpression()), !dbg !549
  %6 = call noundef i32 @_Z6arrSumPii(ptr noundef %0, i32 noundef %4), !dbg !550
  %7 = icmp eq i32 %2, %6, !dbg !551
  ret i1 %7, !dbg !549
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_21Piiiii(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #2 !dbg !411 !pallas.exprWrapper !424 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !410, metadata !DIExpression()), !dbg !552
  call void @llvm.dbg.value(metadata i32 %1, metadata !413, metadata !DIExpression()), !dbg !552
  call void @llvm.dbg.value(metadata i32 %2, metadata !415, metadata !DIExpression()), !dbg !552
  call void @llvm.dbg.value(metadata i32 %3, metadata !417, metadata !DIExpression()), !dbg !552
  call void @llvm.dbg.value(metadata i32 %4, metadata !419, metadata !DIExpression()), !dbg !552
  %6 = mul nsw i32 %4, %3, !dbg !553
  %7 = icmp sle i32 %2, %6, !dbg !554
  ret i1 %7, !dbg !552
}

declare !pallas.specLib !555 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !556 void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8)

declare !pallas.specLib !557 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !558 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !559 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !560 noundef i32 @"pallas.old noundef i32_noundef i32"(i32 noundef)

declare !pallas.specLib !561 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !562 i1 @pallas.imply(i1, i1)

declare !pallas.specLib !563 i1 @pallas.exists(i1, i1)

declare !pallas.specLib !564 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !565 noundef i32 @"pallas.boundVar noundef i32"(ptr)

attributes #0 = { mustprogress noinline uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!8, !10}
!llvm.module.flags = !{!109, !110, !111, !112, !113, !114, !115}
!llvm.ident = !{!116, !116}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 54, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp_spectral/source_wrappers.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "fce8ea3ddc6a9e1a1f4e7dc4e93c2f53")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !6)
!4 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !5)
!5 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!6 = !{!7}
!7 = !DISubrange(count: 2)
!8 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !9, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!9 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/Cpp/vstte10_sum_max.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "5a4c51d195bd0eab2dab7e38d498a902")
!10 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !2, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, globals: !11, imports: !12, splitDebugInlining: false, nameTableKind: None)
!11 = !{!0}
!12 = !{!13, !21, !25, !29, !33, !36, !38, !40, !42, !46, !49, !52, !55, !58, !60, !65, !69, !73, !77, !79, !81, !83, !85, !88, !91, !94, !97, !100, !102, !107}
!13 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !15, file: !20, line: 51)
!14 = !DINamespace(name: "std", scope: null)
!15 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !16, line: 24, baseType: !17)
!16 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!17 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int8_t", file: !18, line: 37, baseType: !19)
!18 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!19 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!20 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstdint", directory: "")
!21 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !22, file: !20, line: 52)
!22 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !16, line: 25, baseType: !23)
!23 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int16_t", file: !18, line: 39, baseType: !24)
!24 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!25 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !26, file: !20, line: 53)
!26 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !16, line: 26, baseType: !27)
!27 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !18, line: 41, baseType: !28)
!28 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!29 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !30, file: !20, line: 54)
!30 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !16, line: 27, baseType: !31)
!31 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !18, line: 44, baseType: !32)
!32 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!33 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !34, file: !20, line: 56)
!34 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !35, line: 47, baseType: !19)
!35 = !DIFile(filename: "/usr/include/stdint.h", directory: "", checksumkind: CSK_MD5, checksum: "bfb03fa9c46a839e35c32b929fbdbb8e")
!36 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !37, file: !20, line: 57)
!37 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !35, line: 49, baseType: !32)
!38 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !39, file: !20, line: 58)
!39 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !35, line: 50, baseType: !32)
!40 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !41, file: !20, line: 59)
!41 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !35, line: 51, baseType: !32)
!42 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !43, file: !20, line: 61)
!43 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !44, line: 25, baseType: !45)
!44 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-least.h", directory: "", checksumkind: CSK_MD5, checksum: "a866be81c480920b0293bd5f6336a0a3")
!45 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least8_t", file: !18, line: 52, baseType: !17)
!46 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !47, file: !20, line: 62)
!47 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !44, line: 26, baseType: !48)
!48 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least16_t", file: !18, line: 54, baseType: !23)
!49 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !50, file: !20, line: 63)
!50 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !44, line: 27, baseType: !51)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least32_t", file: !18, line: 56, baseType: !27)
!52 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !53, file: !20, line: 64)
!53 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !44, line: 28, baseType: !54)
!54 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least64_t", file: !18, line: 58, baseType: !31)
!55 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !56, file: !20, line: 66)
!56 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !35, line: 90, baseType: !57)
!57 = !DIDerivedType(tag: DW_TAG_typedef, name: "__intmax_t", file: !18, line: 72, baseType: !32)
!58 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !59, file: !20, line: 67)
!59 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !35, line: 76, baseType: !32)
!60 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !61, file: !20, line: 69)
!61 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !62, line: 24, baseType: !63)
!62 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-uintn.h", directory: "", checksumkind: CSK_MD5, checksum: "256fcabbefa27ca8cf5e6d37525e6e16")
!63 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !18, line: 38, baseType: !64)
!64 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!65 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !66, file: !20, line: 70)
!66 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !62, line: 25, baseType: !67)
!67 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !18, line: 40, baseType: !68)
!68 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!69 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !70, file: !20, line: 71)
!70 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !62, line: 26, baseType: !71)
!71 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint32_t", file: !18, line: 42, baseType: !72)
!72 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!73 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !74, file: !20, line: 72)
!74 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !62, line: 27, baseType: !75)
!75 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint64_t", file: !18, line: 45, baseType: !76)
!76 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!77 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !78, file: !20, line: 74)
!78 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !35, line: 60, baseType: !64)
!79 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !80, file: !20, line: 75)
!80 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !35, line: 62, baseType: !76)
!81 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !82, file: !20, line: 76)
!82 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !35, line: 63, baseType: !76)
!83 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !84, file: !20, line: 77)
!84 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !35, line: 64, baseType: !76)
!85 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !86, file: !20, line: 79)
!86 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !44, line: 31, baseType: !87)
!87 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least8_t", file: !18, line: 53, baseType: !63)
!88 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !89, file: !20, line: 80)
!89 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !44, line: 32, baseType: !90)
!90 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least16_t", file: !18, line: 55, baseType: !67)
!91 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !92, file: !20, line: 81)
!92 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !44, line: 33, baseType: !93)
!93 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least32_t", file: !18, line: 57, baseType: !71)
!94 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !95, file: !20, line: 82)
!95 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !44, line: 34, baseType: !96)
!96 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least64_t", file: !18, line: 59, baseType: !75)
!97 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !98, file: !20, line: 84)
!98 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !35, line: 91, baseType: !99)
!99 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uintmax_t", file: !18, line: 73, baseType: !76)
!100 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !101, file: !20, line: 85)
!101 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !35, line: 79, baseType: !76)
!102 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !103, file: !106, line: 58)
!103 = !DIDerivedType(tag: DW_TAG_typedef, name: "max_align_t", file: !104, line: 24, baseType: !105)
!104 = !DIFile(filename: "/usr/lib/llvm-17/lib/clang/17/include/__stddef_max_align_t.h", directory: "", checksumkind: CSK_MD5, checksum: "48e8e2456f77e6cda35d245130fa7259")
!105 = !DICompositeType(tag: DW_TAG_structure_type, file: !104, line: 19, size: 256, flags: DIFlagFwdDecl, identifier: "_ZTS11max_align_t")
!106 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstddef", directory: "")
!107 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !10, entity: !108, file: !2, line: 15)
!108 = !DINamespace(name: "pallasSpec", scope: null)
!109 = !{i32 7, !"Dwarf Version", i32 5}
!110 = !{i32 2, !"Debug Info Version", i32 3}
!111 = !{i32 1, !"wchar_size", i32 4}
!112 = !{i32 8, !"PIC Level", i32 2}
!113 = !{i32 7, !"PIE Level", i32 2}
!114 = !{i32 7, !"uwtable", i32 2}
!115 = !{i32 7, !"frame-pointer", i32 2}
!116 = !{!"Ubuntu clang version 17.0.6 (9ubuntu1)"}
!117 = distinct !DISubprogram(name: "arrSum", linkageName: "_Z6arrSumPii", scope: !9, file: !9, line: 33, type: !118, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!118 = !DISubroutineType(types: !119)
!119 = !{!28, !120, !28}
!120 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !28, size: 64)
!121 = !{}
!122 = !{!123, i1 true, i1 false, !121, !121, !125, !138, !146}
!123 = !{!"pallas.srcLoc", i64 26, i64 1, i64 32, i64 1, !124}
!124 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/Cpp/vstte10_sum_max.cpp", directory: "", checksumkind: CSK_MD5, checksum: "5a4c51d195bd0eab2dab7e38d498a902")
!125 = !{!"pallas.requires", !126, ptr @_Z13PALLAS_SPEC_0Pii, !121, !121, !127}
!126 = !{!"pallas.srcLoc", i64 28, i64 1, i64 28, i64 24, !124}
!127 = !{!128, !135}
!128 = !{!129, !130}
!129 = !DILocalVariable(name: "arr", arg: 1, scope: !117, file: !9, line: 33, type: !120)
!130 = !DILocalVariable(name: "arr", arg: 1, scope: !131, file: !9, line: 28, type: !120)
!131 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_Z13PALLAS_SPEC_0Pii", scope: !9, file: !9, line: 28, type: !132, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!132 = !DISubroutineType(types: !133)
!133 = !{!134, !120, !28}
!134 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!135 = !{!136, !137}
!136 = !DILocalVariable(name: "n", arg: 2, scope: !117, file: !9, line: 33, type: !28)
!137 = !DILocalVariable(name: "n", arg: 2, scope: !131, file: !9, line: 28, type: !28)
!138 = !{!"pallas.requires", !139, ptr @_Z13PALLAS_SPEC_1Pii, !121, !121, !140}
!139 = !{!"pallas.srcLoc", i64 29, i64 1, i64 29, i64 40, !124}
!140 = !{!141, !144}
!141 = !{!129, !142}
!142 = !DILocalVariable(name: "arr", arg: 1, scope: !143, file: !9, line: 29, type: !120)
!143 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_Z13PALLAS_SPEC_1Pii", scope: !9, file: !9, line: 29, type: !132, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!144 = !{!136, !145}
!145 = !DILocalVariable(name: "n", arg: 2, scope: !143, file: !9, line: 29, type: !28)
!146 = !{!"pallas.requires", !147, ptr @_Z13PALLAS_SPEC_2Pii, !121, !121, !148}
!147 = !{!"pallas.srcLoc", i64 30, i64 1, i64 31, i64 63, !124}
!148 = !{!149, !152}
!149 = !{!129, !150}
!150 = !DILocalVariable(name: "arr", arg: 1, scope: !151, file: !9, line: 30, type: !120)
!151 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_Z13PALLAS_SPEC_2Pii", scope: !9, file: !9, line: 30, type: !132, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!152 = !{!136, !153}
!153 = !DILocalVariable(name: "n", arg: 2, scope: !151, file: !9, line: 30, type: !28)
!154 = !DILocation(line: 0, scope: !117)
!155 = !DILocation(line: 34, column: 12, scope: !117)
!156 = !DILocation(line: 34, column: 10, scope: !117)
!157 = !DILocation(line: 34, column: 36, scope: !117)
!158 = !DILocation(line: 34, column: 23, scope: !117)
!159 = !DILocation(line: 34, column: 47, scope: !117)
!160 = !DILocation(line: 34, column: 42, scope: !117)
!161 = !DILocation(line: 34, column: 40, scope: !117)
!162 = !DILocation(line: 34, column: 3, scope: !117)
!163 = distinct !DISubprogram(name: "getSumMax", linkageName: "_Z9getSumMaxPii", scope: !9, file: !9, line: 54, type: !164, scopeLine: 54, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!164 = !DISubroutineType(types: !165)
!165 = !{!166, !120, !28}
!166 = !DIDerivedType(tag: DW_TAG_typedef, name: "SumMaxRes", file: !9, line: 23, baseType: !167)
!167 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !9, line: 18, size: 192, flags: DIFlagTypePassByValue, elements: !168, identifier: "_ZTS9SumMaxRes")
!168 = !{!169, !170, !171, !172}
!169 = !DIDerivedType(tag: DW_TAG_member, name: "sum", scope: !167, file: !9, line: 19, baseType: !28, size: 32)
!170 = !DIDerivedType(tag: DW_TAG_member, name: "max", scope: !167, file: !9, line: 20, baseType: !28, size: 32, offset: 32)
!171 = !DIDerivedType(tag: DW_TAG_member, name: "dummy1", scope: !167, file: !9, line: 21, baseType: !30, size: 64, offset: 64)
!172 = !DIDerivedType(tag: DW_TAG_member, name: "dummy2", scope: !167, file: !9, line: 22, baseType: !30, size: 64, offset: 128)
!173 = !{!174, i1 false, i1 false, !121, !121, !175, !185, !193, !201, !209, !217, !225, !233, !241}
!174 = !{!"pallas.srcLoc", i64 37, i64 1, i64 53, i64 1, !124}
!175 = !{!"pallas.requires", !176, ptr @_Z13PALLAS_SPEC_3Pii, !121, !121, !177}
!176 = !{!"pallas.srcLoc", i64 38, i64 1, i64 38, i64 24, !124}
!177 = !{!178, !182}
!178 = !{!179, !180}
!179 = !DILocalVariable(name: "arr", arg: 1, scope: !163, file: !9, line: 54, type: !120)
!180 = !DILocalVariable(name: "arr", arg: 1, scope: !181, file: !9, line: 38, type: !120)
!181 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_Z13PALLAS_SPEC_3Pii", scope: !9, file: !9, line: 38, type: !132, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!182 = !{!183, !184}
!183 = !DILocalVariable(name: "n", arg: 2, scope: !163, file: !9, line: 54, type: !28)
!184 = !DILocalVariable(name: "n", arg: 2, scope: !181, file: !9, line: 38, type: !28)
!185 = !{!"pallas.requires", !186, ptr @_Z13PALLAS_SPEC_4Pii, !121, !121, !187}
!186 = !{!"pallas.srcLoc", i64 39, i64 1, i64 39, i64 40, !124}
!187 = !{!188, !191}
!188 = !{!179, !189}
!189 = !DILocalVariable(name: "arr", arg: 1, scope: !190, file: !9, line: 39, type: !120)
!190 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_Z13PALLAS_SPEC_4Pii", scope: !9, file: !9, line: 39, type: !132, scopeLine: 39, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!191 = !{!183, !192}
!192 = !DILocalVariable(name: "n", arg: 2, scope: !190, file: !9, line: 39, type: !28)
!193 = !{!"pallas.requires", !194, ptr @_Z13PALLAS_SPEC_5Pii, !121, !121, !195}
!194 = !{!"pallas.srcLoc", i64 40, i64 1, i64 41, i64 61, !124}
!195 = !{!196, !199}
!196 = !{!179, !197}
!197 = !DILocalVariable(name: "arr", arg: 1, scope: !198, file: !9, line: 40, type: !120)
!198 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_Z13PALLAS_SPEC_5Pii", scope: !9, file: !9, line: 40, type: !132, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!199 = !{!183, !200}
!200 = !DILocalVariable(name: "n", arg: 2, scope: !198, file: !9, line: 40, type: !28)
!201 = !{!"pallas.requires", !202, ptr @_Z13PALLAS_SPEC_6Pii, !121, !121, !203}
!202 = !{!"pallas.srcLoc", i64 42, i64 1, i64 43, i64 43, !124}
!203 = !{!204, !207}
!204 = !{!179, !205}
!205 = !DILocalVariable(name: "arr", arg: 1, scope: !206, file: !9, line: 42, type: !120)
!206 = distinct !DISubprogram(name: "PALLAS_SPEC_6", linkageName: "_Z13PALLAS_SPEC_6Pii", scope: !9, file: !9, line: 42, type: !132, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!207 = !{!183, !208}
!208 = !DILocalVariable(name: "n", arg: 2, scope: !206, file: !9, line: 42, type: !28)
!209 = !{!"pallas.ensures", !210, ptr @_Z13PALLAS_SPEC_7Pii, !121, !121, !211}
!210 = !{!"pallas.srcLoc", i64 44, i64 1, i64 45, i64 60, !124}
!211 = !{!212, !215}
!212 = !{!179, !213}
!213 = !DILocalVariable(name: "arr", arg: 1, scope: !214, file: !9, line: 44, type: !120)
!214 = distinct !DISubprogram(name: "PALLAS_SPEC_7", linkageName: "_Z13PALLAS_SPEC_7Pii", scope: !9, file: !9, line: 44, type: !132, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!215 = !{!183, !216}
!216 = !DILocalVariable(name: "n", arg: 2, scope: !214, file: !9, line: 44, type: !28)
!217 = !{!"pallas.ensures", !218, ptr @_Z13PALLAS_SPEC_8Pii, !121, !121, !219}
!218 = !{!"pallas.srcLoc", i64 46, i64 1, i64 47, i64 64, !124}
!219 = !{!220, !223}
!220 = !{!179, !221}
!221 = !DILocalVariable(name: "arr", arg: 1, scope: !222, file: !9, line: 46, type: !120)
!222 = distinct !DISubprogram(name: "PALLAS_SPEC_8", linkageName: "_Z13PALLAS_SPEC_8Pii", scope: !9, file: !9, line: 46, type: !132, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!223 = !{!183, !224}
!224 = !DILocalVariable(name: "n", arg: 2, scope: !222, file: !9, line: 46, type: !28)
!225 = !{!"pallas.ensures", !226, ptr @_Z13PALLAS_SPEC_9Pii, !121, !121, !227}
!226 = !{!"pallas.srcLoc", i64 48, i64 1, i64 50, i64 72, !124}
!227 = !{!228, !231}
!228 = !{!179, !229}
!229 = !DILocalVariable(name: "arr", arg: 1, scope: !230, file: !9, line: 48, type: !120)
!230 = distinct !DISubprogram(name: "PALLAS_SPEC_9", linkageName: "_Z13PALLAS_SPEC_9Pii", scope: !9, file: !9, line: 48, type: !132, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!231 = !{!183, !232}
!232 = !DILocalVariable(name: "n", arg: 2, scope: !230, file: !9, line: 48, type: !28)
!233 = !{!"pallas.ensures", !234, ptr @_Z14PALLAS_SPEC_10Pii, !121, !121, !235}
!234 = !{!"pallas.srcLoc", i64 51, i64 1, i64 51, i64 51, !124}
!235 = !{!236, !239}
!236 = !{!179, !237}
!237 = !DILocalVariable(name: "arr", arg: 1, scope: !238, file: !9, line: 51, type: !120)
!238 = distinct !DISubprogram(name: "PALLAS_SPEC_10", linkageName: "_Z14PALLAS_SPEC_10Pii", scope: !9, file: !9, line: 51, type: !132, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!239 = !{!183, !240}
!240 = !DILocalVariable(name: "n", arg: 2, scope: !238, file: !9, line: 51, type: !28)
!241 = !{!"pallas.ensures", !242, ptr @_Z14PALLAS_SPEC_11Pii, !121, !121, !243}
!242 = !{!"pallas.srcLoc", i64 52, i64 1, i64 52, i64 65, !124}
!243 = !{!244, !247}
!244 = !{!179, !245}
!245 = !DILocalVariable(name: "arr", arg: 1, scope: !246, file: !9, line: 52, type: !120)
!246 = distinct !DISubprogram(name: "PALLAS_SPEC_11", linkageName: "_Z14PALLAS_SPEC_11Pii", scope: !9, file: !9, line: 52, type: !132, scopeLine: 52, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!247 = !{!183, !248}
!248 = !DILocalVariable(name: "n", arg: 2, scope: !246, file: !9, line: 52, type: !28)
!249 = !DILocation(line: 0, scope: !163)
!250 = !DILocalVariable(name: "sum", scope: !163, file: !9, line: 55, type: !28)
!251 = !DILocalVariable(name: "max", scope: !163, file: !9, line: 56, type: !28)
!252 = !DILocalVariable(name: "i", scope: !253, file: !9, line: 75, type: !28)
!253 = distinct !DILexicalBlock(scope: !163, file: !9, line: 75, column: 5)
!254 = !DILocation(line: 0, scope: !253)
!255 = !DILocation(line: 75, column: 10, scope: !253)
!256 = !DILocation(line: 75, scope: !253)
!257 = !DILocation(line: 75, column: 23, scope: !258)
!258 = distinct !DILexicalBlock(scope: !253, file: !9, line: 75, column: 5)
!259 = !DILocation(line: 75, column: 5, scope: !253)
!260 = !DILocation(line: 76, column: 17, scope: !261)
!261 = distinct !DILexicalBlock(scope: !258, file: !9, line: 75, column: 33)
!262 = !DILocalVariable(name: "e", scope: !261, file: !9, line: 76, type: !28)
!263 = !DILocation(line: 0, scope: !261)
!264 = !DILocation(line: 77, column: 15, scope: !265)
!265 = distinct !DILexicalBlock(scope: !261, file: !9, line: 77, column: 13)
!266 = !DILocation(line: 77, column: 13, scope: !261)
!267 = !DILocation(line: 79, column: 9, scope: !268)
!268 = distinct !DILexicalBlock(scope: !265, file: !9, line: 77, column: 22)
!269 = !DILocation(line: 81, column: 13, scope: !261)
!270 = !DILocation(line: 82, column: 5, scope: !261)
!271 = !DILocation(line: 75, column: 28, scope: !258)
!272 = !DILocation(line: 75, column: 5, scope: !258)
!273 = distinct !{!273, !259, !274, !275, !276}
!274 = !DILocation(line: 82, column: 5, scope: !253)
!275 = !{!"llvm.loop.mustprogress"}
!276 = !{!"pallas.loopInvBlock", !277, !278, !294, !308, !322, !336, !350, !364, !378, !392, !406}
!277 = !{!"pallas.srcLoc", i64 58, i64 5, i64 74, i64 5, !124}
!278 = !{!"pallas.loopInv", !279, ptr @_Z14PALLAS_SPEC_12Piiiii, !121, !121, !280}
!279 = !{!"pallas.srcLoc", i64 59, i64 5, i64 59, i64 36, !124}
!280 = !{!281, !286, !288, !290, !292}
!281 = !{!179, !282}
!282 = !DILocalVariable(name: "arr", arg: 1, scope: !283, file: !9, line: 59, type: !120)
!283 = distinct !DISubprogram(name: "PALLAS_SPEC_12", linkageName: "_Z14PALLAS_SPEC_12Piiiii", scope: !9, file: !9, line: 59, type: !284, scopeLine: 59, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!284 = !DISubroutineType(types: !285)
!285 = !{!134, !120, !28, !28, !28, !28}
!286 = !{!183, !287}
!287 = !DILocalVariable(name: "n", arg: 2, scope: !283, file: !9, line: 59, type: !28)
!288 = !{!250, !289}
!289 = !DILocalVariable(name: "sum", arg: 3, scope: !283, file: !9, line: 59, type: !28)
!290 = !{!251, !291}
!291 = !DILocalVariable(name: "max", arg: 4, scope: !283, file: !9, line: 59, type: !28)
!292 = !{!252, !293}
!293 = !DILocalVariable(name: "i", arg: 5, scope: !283, file: !9, line: 59, type: !28)
!294 = !{!"pallas.loopInv", !295, ptr @_Z14PALLAS_SPEC_13Piiiii, !121, !121, !296}
!295 = !{!"pallas.srcLoc", i64 60, i64 5, i64 61, i64 71, !124}
!296 = !{!297, !300, !302, !304, !306}
!297 = !{!179, !298}
!298 = !DILocalVariable(name: "arr", arg: 1, scope: !299, file: !9, line: 60, type: !120)
!299 = distinct !DISubprogram(name: "PALLAS_SPEC_13", linkageName: "_Z14PALLAS_SPEC_13Piiiii", scope: !9, file: !9, line: 60, type: !284, scopeLine: 60, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!300 = !{!183, !301}
!301 = !DILocalVariable(name: "n", arg: 2, scope: !299, file: !9, line: 60, type: !28)
!302 = !{!250, !303}
!303 = !DILocalVariable(name: "sum", arg: 3, scope: !299, file: !9, line: 60, type: !28)
!304 = !{!251, !305}
!305 = !DILocalVariable(name: "max", arg: 4, scope: !299, file: !9, line: 60, type: !28)
!306 = !{!252, !307}
!307 = !DILocalVariable(name: "i", arg: 5, scope: !299, file: !9, line: 60, type: !28)
!308 = !{!"pallas.loopInv", !309, ptr @_Z14PALLAS_SPEC_14Piiiii, !121, !121, !310}
!309 = !{!"pallas.srcLoc", i64 62, i64 5, i64 63, i64 53, !124}
!310 = !{!311, !314, !316, !318, !320}
!311 = !{!179, !312}
!312 = !DILocalVariable(name: "arr", arg: 1, scope: !313, file: !9, line: 62, type: !120)
!313 = distinct !DISubprogram(name: "PALLAS_SPEC_14", linkageName: "_Z14PALLAS_SPEC_14Piiiii", scope: !9, file: !9, line: 62, type: !284, scopeLine: 62, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!314 = !{!183, !315}
!315 = !DILocalVariable(name: "n", arg: 2, scope: !313, file: !9, line: 62, type: !28)
!316 = !{!250, !317}
!317 = !DILocalVariable(name: "sum", arg: 3, scope: !313, file: !9, line: 62, type: !28)
!318 = !{!251, !319}
!319 = !DILocalVariable(name: "max", arg: 4, scope: !313, file: !9, line: 62, type: !28)
!320 = !{!252, !321}
!321 = !DILocalVariable(name: "i", arg: 5, scope: !313, file: !9, line: 62, type: !28)
!322 = !{!"pallas.loopInv", !323, ptr @_Z14PALLAS_SPEC_15Piiiii, !121, !121, !324}
!323 = !{!"pallas.srcLoc", i64 64, i64 5, i64 65, i64 81, !124}
!324 = !{!325, !328, !330, !332, !334}
!325 = !{!179, !326}
!326 = !DILocalVariable(name: "arr", arg: 1, scope: !327, file: !9, line: 64, type: !120)
!327 = distinct !DISubprogram(name: "PALLAS_SPEC_15", linkageName: "_Z14PALLAS_SPEC_15Piiiii", scope: !9, file: !9, line: 64, type: !284, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!328 = !{!183, !329}
!329 = !DILocalVariable(name: "n", arg: 2, scope: !327, file: !9, line: 64, type: !28)
!330 = !{!250, !331}
!331 = !DILocalVariable(name: "sum", arg: 3, scope: !327, file: !9, line: 64, type: !28)
!332 = !{!251, !333}
!333 = !DILocalVariable(name: "max", arg: 4, scope: !327, file: !9, line: 64, type: !28)
!334 = !{!252, !335}
!335 = !DILocalVariable(name: "i", arg: 5, scope: !327, file: !9, line: 64, type: !28)
!336 = !{!"pallas.loopInv", !337, ptr @_Z14PALLAS_SPEC_16Piiiii, !121, !121, !338}
!337 = !{!"pallas.srcLoc", i64 66, i64 5, i64 66, i64 44, !124}
!338 = !{!339, !342, !344, !346, !348}
!339 = !{!179, !340}
!340 = !DILocalVariable(name: "arr", arg: 1, scope: !341, file: !9, line: 66, type: !120)
!341 = distinct !DISubprogram(name: "PALLAS_SPEC_16", linkageName: "_Z14PALLAS_SPEC_16Piiiii", scope: !9, file: !9, line: 66, type: !284, scopeLine: 66, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!342 = !{!183, !343}
!343 = !DILocalVariable(name: "n", arg: 2, scope: !341, file: !9, line: 66, type: !28)
!344 = !{!250, !345}
!345 = !DILocalVariable(name: "sum", arg: 3, scope: !341, file: !9, line: 66, type: !28)
!346 = !{!251, !347}
!347 = !DILocalVariable(name: "max", arg: 4, scope: !341, file: !9, line: 66, type: !28)
!348 = !{!252, !349}
!349 = !DILocalVariable(name: "i", arg: 5, scope: !341, file: !9, line: 66, type: !28)
!350 = !{!"pallas.loopInv", !351, ptr @_Z14PALLAS_SPEC_17Piiiii, !121, !121, !352}
!351 = !{!"pallas.srcLoc", i64 67, i64 5, i64 67, i64 62, !124}
!352 = !{!353, !356, !358, !360, !362}
!353 = !{!179, !354}
!354 = !DILocalVariable(name: "arr", arg: 1, scope: !355, file: !9, line: 67, type: !120)
!355 = distinct !DISubprogram(name: "PALLAS_SPEC_17", linkageName: "_Z14PALLAS_SPEC_17Piiiii", scope: !9, file: !9, line: 67, type: !284, scopeLine: 67, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!356 = !{!183, !357}
!357 = !DILocalVariable(name: "n", arg: 2, scope: !355, file: !9, line: 67, type: !28)
!358 = !{!250, !359}
!359 = !DILocalVariable(name: "sum", arg: 3, scope: !355, file: !9, line: 67, type: !28)
!360 = !{!251, !361}
!361 = !DILocalVariable(name: "max", arg: 4, scope: !355, file: !9, line: 67, type: !28)
!362 = !{!252, !363}
!363 = !DILocalVariable(name: "i", arg: 5, scope: !355, file: !9, line: 67, type: !28)
!364 = !{!"pallas.loopInv", !365, ptr @_Z14PALLAS_SPEC_18Piiiii, !121, !121, !366}
!365 = !{!"pallas.srcLoc", i64 68, i64 5, i64 69, i64 55, !124}
!366 = !{!367, !370, !372, !374, !376}
!367 = !{!179, !368}
!368 = !DILocalVariable(name: "arr", arg: 1, scope: !369, file: !9, line: 68, type: !120)
!369 = distinct !DISubprogram(name: "PALLAS_SPEC_18", linkageName: "_Z14PALLAS_SPEC_18Piiiii", scope: !9, file: !9, line: 68, type: !284, scopeLine: 68, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!370 = !{!183, !371}
!371 = !DILocalVariable(name: "n", arg: 2, scope: !369, file: !9, line: 68, type: !28)
!372 = !{!250, !373}
!373 = !DILocalVariable(name: "sum", arg: 3, scope: !369, file: !9, line: 68, type: !28)
!374 = !{!251, !375}
!375 = !DILocalVariable(name: "max", arg: 4, scope: !369, file: !9, line: 68, type: !28)
!376 = !{!252, !377}
!377 = !DILocalVariable(name: "i", arg: 5, scope: !369, file: !9, line: 68, type: !28)
!378 = !{!"pallas.loopInv", !379, ptr @_Z14PALLAS_SPEC_19Piiiii, !121, !121, !380}
!379 = !{!"pallas.srcLoc", i64 70, i64 5, i64 71, i64 69, !124}
!380 = !{!381, !384, !386, !388, !390}
!381 = !{!179, !382}
!382 = !DILocalVariable(name: "arr", arg: 1, scope: !383, file: !9, line: 70, type: !120)
!383 = distinct !DISubprogram(name: "PALLAS_SPEC_19", linkageName: "_Z14PALLAS_SPEC_19Piiiii", scope: !9, file: !9, line: 70, type: !284, scopeLine: 70, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!384 = !{!183, !385}
!385 = !DILocalVariable(name: "n", arg: 2, scope: !383, file: !9, line: 70, type: !28)
!386 = !{!250, !387}
!387 = !DILocalVariable(name: "sum", arg: 3, scope: !383, file: !9, line: 70, type: !28)
!388 = !{!251, !389}
!389 = !DILocalVariable(name: "max", arg: 4, scope: !383, file: !9, line: 70, type: !28)
!390 = !{!252, !391}
!391 = !DILocalVariable(name: "i", arg: 5, scope: !383, file: !9, line: 70, type: !28)
!392 = !{!"pallas.loopInv", !393, ptr @_Z14PALLAS_SPEC_20Piiiii, !121, !121, !394}
!393 = !{!"pallas.srcLoc", i64 72, i64 5, i64 72, i64 41, !124}
!394 = !{!395, !398, !400, !402, !404}
!395 = !{!179, !396}
!396 = !DILocalVariable(name: "arr", arg: 1, scope: !397, file: !9, line: 72, type: !120)
!397 = distinct !DISubprogram(name: "PALLAS_SPEC_20", linkageName: "_Z14PALLAS_SPEC_20Piiiii", scope: !9, file: !9, line: 72, type: !284, scopeLine: 72, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!398 = !{!183, !399}
!399 = !DILocalVariable(name: "n", arg: 2, scope: !397, file: !9, line: 72, type: !28)
!400 = !{!250, !401}
!401 = !DILocalVariable(name: "sum", arg: 3, scope: !397, file: !9, line: 72, type: !28)
!402 = !{!251, !403}
!403 = !DILocalVariable(name: "max", arg: 4, scope: !397, file: !9, line: 72, type: !28)
!404 = !{!252, !405}
!405 = !DILocalVariable(name: "i", arg: 5, scope: !397, file: !9, line: 72, type: !28)
!406 = !{!"pallas.loopInv", !407, ptr @_Z14PALLAS_SPEC_21Piiiii, !121, !121, !408}
!407 = !{!"pallas.srcLoc", i64 73, i64 5, i64 73, i64 34, !124}
!408 = !{!409, !412, !414, !416, !418}
!409 = !{!179, !410}
!410 = !DILocalVariable(name: "arr", arg: 1, scope: !411, file: !9, line: 73, type: !120)
!411 = distinct !DISubprogram(name: "PALLAS_SPEC_21", linkageName: "_Z14PALLAS_SPEC_21Piiiii", scope: !9, file: !9, line: 73, type: !284, scopeLine: 73, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !121)
!412 = !{!183, !413}
!413 = !DILocalVariable(name: "n", arg: 2, scope: !411, file: !9, line: 73, type: !28)
!414 = !{!250, !415}
!415 = !DILocalVariable(name: "sum", arg: 3, scope: !411, file: !9, line: 73, type: !28)
!416 = !{!251, !417}
!417 = !DILocalVariable(name: "max", arg: 4, scope: !411, file: !9, line: 73, type: !28)
!418 = !{!252, !419}
!419 = !DILocalVariable(name: "i", arg: 5, scope: !411, file: !9, line: 73, type: !28)
!420 = !DILocalVariable(name: "res", scope: !163, file: !9, line: 84, type: !166)
!421 = !DILocation(line: 84, column: 15, scope: !163)
!422 = !DILocation(line: 84, column: 21, scope: !163)
!423 = !DILocation(line: 85, column: 5, scope: !163)
!424 = !{!""}
!425 = !DILocation(line: 0, scope: !131)
!426 = !DILocation(line: 28, column: 14, scope: !131)
!427 = !DILocation(line: 0, scope: !143)
!428 = !DILocation(line: 29, column: 12, scope: !143)
!429 = !DILocation(line: 29, column: 17, scope: !143)
!430 = !DILocation(line: 29, column: 20, scope: !143)
!431 = !DILocation(line: 29, column: 25, scope: !143)
!432 = !DILocation(line: 29, column: 22, scope: !143)
!433 = !DILocation(line: 0, scope: !151)
!434 = !DILocation(line: 30, column: 19, scope: !151)
!435 = !DILocation(line: 31, column: 30, scope: !151)
!436 = !DILocation(line: 31, column: 26, scope: !151)
!437 = !DILocation(line: 31, column: 46, scope: !151)
!438 = !DILocation(line: 31, column: 19, scope: !151)
!439 = !DILocation(line: 30, column: 10, scope: !151)
!440 = !DILocation(line: 0, scope: !181)
!441 = !DILocation(line: 38, column: 14, scope: !181)
!442 = !DILocation(line: 0, scope: !190)
!443 = !DILocation(line: 39, column: 12, scope: !190)
!444 = !DILocation(line: 39, column: 17, scope: !190)
!445 = !DILocation(line: 39, column: 20, scope: !190)
!446 = !DILocation(line: 39, column: 25, scope: !190)
!447 = !DILocation(line: 39, column: 22, scope: !190)
!448 = !DILocation(line: 0, scope: !198)
!449 = !DILocation(line: 40, column: 19, scope: !198)
!450 = !DILocation(line: 41, column: 30, scope: !198)
!451 = !DILocation(line: 41, column: 26, scope: !198)
!452 = !DILocation(line: 41, column: 46, scope: !198)
!453 = !DILocation(line: 41, column: 19, scope: !198)
!454 = !DILocation(line: 40, column: 10, scope: !198)
!455 = !DILocation(line: 0, scope: !206)
!456 = !DILocation(line: 42, column: 19, scope: !206)
!457 = !DILocation(line: 43, column: 23, scope: !206)
!458 = !DILocation(line: 43, column: 19, scope: !206)
!459 = !DILocation(line: 43, column: 38, scope: !206)
!460 = !DILocation(line: 42, column: 10, scope: !206)
!461 = !DILocation(line: 0, scope: !214)
!462 = !DILocation(line: 44, column: 18, scope: !214)
!463 = !DILocation(line: 45, column: 29, scope: !214)
!464 = !DILocation(line: 45, column: 25, scope: !214)
!465 = !DILocation(line: 45, column: 45, scope: !214)
!466 = !DILocation(line: 45, column: 18, scope: !214)
!467 = !DILocation(line: 44, column: 9, scope: !214)
!468 = !DILocation(line: 0, scope: !222)
!469 = !DILocation(line: 46, column: 17, scope: !222)
!470 = !DILocation(line: 47, column: 21, scope: !222)
!471 = !DILocation(line: 47, column: 17, scope: !222)
!472 = !DILocation(line: 47, column: 39, scope: !222)
!473 = !DILocation(line: 47, column: 60, scope: !222)
!474 = !DILocation(line: 47, column: 36, scope: !222)
!475 = !DILocation(line: 46, column: 9, scope: !222)
!476 = !DILocation(line: 0, scope: !230)
!477 = !DILocation(line: 48, column: 18, scope: !230)
!478 = !DILocation(line: 49, column: 24, scope: !230)
!479 = !DILocation(line: 50, column: 28, scope: !230)
!480 = !DILocation(line: 50, column: 24, scope: !230)
!481 = !DILocation(line: 50, column: 46, scope: !230)
!482 = !DILocation(line: 50, column: 67, scope: !230)
!483 = !DILocation(line: 50, column: 43, scope: !230)
!484 = !DILocation(line: 49, column: 16, scope: !230)
!485 = !DILocation(line: 48, column: 9, scope: !230)
!486 = !DILocation(line: 0, scope: !238)
!487 = !DILocation(line: 51, column: 9, scope: !238)
!488 = !DILocation(line: 51, column: 30, scope: !238)
!489 = !DILocation(line: 51, column: 37, scope: !238)
!490 = !DILocation(line: 51, column: 34, scope: !238)
!491 = !DILocation(line: 0, scope: !246)
!492 = !DILocation(line: 52, column: 9, scope: !246)
!493 = !DILocation(line: 52, column: 30, scope: !246)
!494 = !DILocation(line: 52, column: 37, scope: !246)
!495 = !DILocation(line: 52, column: 58, scope: !246)
!496 = !DILocation(line: 52, column: 62, scope: !246)
!497 = !DILocation(line: 52, column: 34, scope: !246)
!498 = !DILocation(line: 0, scope: !299)
!499 = !DILocation(line: 60, column: 29, scope: !299)
!500 = !DILocation(line: 61, column: 40, scope: !299)
!501 = !DILocation(line: 61, column: 36, scope: !299)
!502 = !DILocation(line: 61, column: 56, scope: !299)
!503 = !DILocation(line: 61, column: 29, scope: !299)
!504 = !DILocation(line: 60, column: 20, scope: !299)
!505 = !DILocation(line: 0, scope: !283)
!506 = !DILocation(line: 59, column: 22, scope: !283)
!507 = !DILocation(line: 59, column: 27, scope: !283)
!508 = !DILocation(line: 59, column: 32, scope: !283)
!509 = !DILocation(line: 0, scope: !327)
!510 = !DILocation(line: 64, column: 29, scope: !327)
!511 = !DILocation(line: 65, column: 33, scope: !327)
!512 = !DILocation(line: 65, column: 29, scope: !327)
!513 = !DILocation(line: 65, column: 65, scope: !327)
!514 = !DILocation(line: 65, column: 61, scope: !327)
!515 = !DILocation(line: 65, column: 51, scope: !327)
!516 = !DILocation(line: 65, column: 48, scope: !327)
!517 = !DILocation(line: 64, column: 20, scope: !327)
!518 = !DILocation(line: 0, scope: !341)
!519 = !DILocation(line: 66, column: 29, scope: !341)
!520 = !DILocation(line: 66, column: 39, scope: !341)
!521 = !DILocation(line: 66, column: 20, scope: !341)
!522 = !DILocation(line: 0, scope: !355)
!523 = !DILocation(line: 67, column: 34, scope: !355)
!524 = !DILocation(line: 67, column: 42, scope: !355)
!525 = !DILocation(line: 67, column: 27, scope: !355)
!526 = !DILocation(line: 67, column: 55, scope: !355)
!527 = !DILocation(line: 67, column: 52, scope: !355)
!528 = !DILocation(line: 67, column: 20, scope: !355)
!529 = !DILocation(line: 0, scope: !369)
!530 = !DILocation(line: 68, column: 29, scope: !369)
!531 = !DILocation(line: 69, column: 33, scope: !369)
!532 = !DILocation(line: 69, column: 29, scope: !369)
!533 = !DILocation(line: 69, column: 48, scope: !369)
!534 = !DILocation(line: 68, column: 20, scope: !369)
!535 = !DILocation(line: 0, scope: !313)
!536 = !DILocation(line: 62, column: 29, scope: !313)
!537 = !DILocation(line: 63, column: 33, scope: !313)
!538 = !DILocation(line: 63, column: 29, scope: !313)
!539 = !DILocation(line: 63, column: 48, scope: !313)
!540 = !DILocation(line: 62, column: 20, scope: !313)
!541 = !DILocation(line: 0, scope: !383)
!542 = !DILocation(line: 70, column: 29, scope: !383)
!543 = !DILocation(line: 70, column: 42, scope: !383)
!544 = !DILocation(line: 71, column: 46, scope: !383)
!545 = !DILocation(line: 71, column: 42, scope: !383)
!546 = !DILocation(line: 71, column: 61, scope: !383)
!547 = !DILocation(line: 70, column: 34, scope: !383)
!548 = !DILocation(line: 70, column: 20, scope: !383)
!549 = !DILocation(line: 0, scope: !397)
!550 = !DILocation(line: 72, column: 27, scope: !397)
!551 = !DILocation(line: 72, column: 24, scope: !397)
!552 = !DILocation(line: 0, scope: !411)
!553 = !DILocation(line: 73, column: 29, scope: !411)
!554 = !DILocation(line: 73, column: 24, scope: !411)
!555 = !{!"pallas.ptrLength"}
!556 = !{!"pallas.result"}
!557 = !{!"pallas.forallSep"}
!558 = !{!"pallas.perm"}
!559 = !{!"pallas.fracOf"}
!560 = !{!"pallas.old"}
!561 = !{!"pallas.forall"}
!562 = !{!"pallas.imply"}
!563 = !{!"pallas.exists"}
!564 = !{!"pallas.scAnd"}
!565 = !{!"pallas.boundVar"}
