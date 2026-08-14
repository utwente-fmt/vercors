; ModuleID = 'tmp_spectral/tmp_ir_source0.ll'
source_filename = "examples/publications/2026/ATVA2026Spectral/C/vstte10_sum_max.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.SumMaxRes = type { i32, i32, i64, i64 }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [22 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_15, ptr @PALLAS_SPEC_16, ptr @PALLAS_SPEC_17, ptr @PALLAS_SPEC_18, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_19, ptr @PALLAS_SPEC_20, ptr @PALLAS_SPEC_21], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"j\00", align 1, !dbg !0

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @arrSum(ptr noundef %0, i32 noundef %1) #0 !dbg !21 !pallas.fcontract !27 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !34, metadata !DIExpression()), !dbg !59
  call void @llvm.dbg.value(metadata i32 %1, metadata !41, metadata !DIExpression()), !dbg !59
  %3 = icmp eq i32 %1, 0, !dbg !60
  br i1 %3, label %4, label %5, !dbg !61

4:                                                ; preds = %2
  br label %13, !dbg !61

5:                                                ; preds = %2
  %6 = sub nsw i32 %1, 1, !dbg !62
  %7 = call i32 @arrSum(ptr noundef %0, i32 noundef %6), !dbg !63
  %8 = sub nsw i32 %1, 1, !dbg !64
  %9 = sext i32 %8 to i64, !dbg !65
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !65
  %11 = load i32, ptr %10, align 4, !dbg !65
  %12 = add nsw i32 %7, %11, !dbg !66
  br label %13, !dbg !61

13:                                               ; preds = %5, %4
  %14 = phi i32 [ 0, %4 ], [ %12, %5 ], !dbg !61
  ret i32 %14, !dbg !67
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @getSumMax(ptr noalias sret(%struct.SumMaxRes) align 8 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !68 !pallas.fcontract !83 {
  call void @llvm.dbg.value(metadata ptr %1, metadata !89, metadata !DIExpression()), !dbg !159
  call void @llvm.dbg.value(metadata i32 %2, metadata !93, metadata !DIExpression()), !dbg !159
  call void @llvm.dbg.value(metadata i32 0, metadata !160, metadata !DIExpression()), !dbg !159
  call void @llvm.dbg.value(metadata i32 0, metadata !161, metadata !DIExpression()), !dbg !159
  call void @llvm.dbg.value(metadata i32 0, metadata !162, metadata !DIExpression()), !dbg !164
  br label %4, !dbg !165

4:                                                ; preds = %14, %3
  %.02 = phi i32 [ 0, %3 ], [ %13, %14 ], !dbg !159
  %.01 = phi i32 [ 0, %3 ], [ %.1, %14 ], !dbg !159
  %.0 = phi i32 [ 0, %3 ], [ %15, %14 ], !dbg !166
  call void @llvm.dbg.value(metadata i32 %.0, metadata !162, metadata !DIExpression()), !dbg !164
  call void @llvm.dbg.value(metadata i32 %.01, metadata !161, metadata !DIExpression()), !dbg !159
  call void @llvm.dbg.value(metadata i32 %.02, metadata !160, metadata !DIExpression()), !dbg !159
  %5 = icmp slt i32 %.0, %2, !dbg !167
  br i1 %5, label %6, label %16, !dbg !169

6:                                                ; preds = %4
  %7 = sext i32 %.0 to i64, !dbg !170
  %8 = getelementptr inbounds i32, ptr %1, i64 %7, !dbg !170
  %9 = load i32, ptr %8, align 4, !dbg !170
  call void @llvm.dbg.value(metadata i32 %9, metadata !172, metadata !DIExpression()), !dbg !173
  %10 = icmp sgt i32 %9, %.01, !dbg !174
  br i1 %10, label %11, label %12, !dbg !176

11:                                               ; preds = %6
  call void @llvm.dbg.value(metadata i32 %9, metadata !161, metadata !DIExpression()), !dbg !159
  br label %12, !dbg !177

12:                                               ; preds = %11, %6
  %.1 = phi i32 [ %9, %11 ], [ %.01, %6 ], !dbg !159
  call void @llvm.dbg.value(metadata i32 %.1, metadata !161, metadata !DIExpression()), !dbg !159
  %13 = add nsw i32 %.02, %9, !dbg !179
  call void @llvm.dbg.value(metadata i32 %13, metadata !160, metadata !DIExpression()), !dbg !159
  br label %14, !dbg !180

14:                                               ; preds = %12
  %15 = add nsw i32 %.0, 1, !dbg !181
  call void @llvm.dbg.value(metadata i32 %15, metadata !162, metadata !DIExpression()), !dbg !164
  br label %4, !dbg !182, !llvm.loop !183

16:                                               ; preds = %4
  call void @llvm.dbg.declare(metadata ptr %0, metadata !330, metadata !DIExpression()), !dbg !331
  %17 = getelementptr inbounds %struct.SumMaxRes, ptr %0, i32 0, i32 0, !dbg !332
  store i32 %.02, ptr %17, align 8, !dbg !332
  %18 = getelementptr inbounds %struct.SumMaxRes, ptr %0, i32 0, i32 1, !dbg !332
  store i32 %.01, ptr %18, align 4, !dbg !332
  %19 = getelementptr inbounds %struct.SumMaxRes, ptr %0, i32 0, i32 2, !dbg !332
  store i64 0, ptr %19, align 8, !dbg !332
  %20 = getelementptr inbounds %struct.SumMaxRes, ptr %0, i32 0, i32 3, !dbg !332
  store i64 0, ptr %20, align 8, !dbg !332
  ret void, !dbg !333
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1) #0 !dbg !36 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !35, metadata !DIExpression()), !dbg !335
  call void @llvm.dbg.value(metadata i32 %1, metadata !42, metadata !DIExpression()), !dbg !335
  %3 = icmp ne ptr %0, null, !dbg !336
  ret i1 %3, !dbg !335
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1) #0 !dbg !48 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !47, metadata !DIExpression()), !dbg !337
  call void @llvm.dbg.value(metadata i32 %1, metadata !50, metadata !DIExpression()), !dbg !337
  %3 = icmp sle i32 0, %1, !dbg !338
  br i1 %3, label %4, label %8, !dbg !339

4:                                                ; preds = %2
  %5 = sext i32 %1 to i64, !dbg !340
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !341
  %7 = icmp sle i64 %5, %6, !dbg !342
  br label %8

8:                                                ; preds = %4, %2
  %9 = phi i1 [ false, %2 ], [ %7, %4 ], !dbg !337
  ret i1 %9, !dbg !337
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1) #0 !dbg !56 !pallas.exprWrapper !334 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !55, metadata !DIExpression()), !dbg !343
  call void @llvm.dbg.value(metadata i32 %1, metadata !58, metadata !DIExpression()), !dbg !343
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !344
  %5 = icmp sle i32 0, %4, !dbg !344
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !344
  %7 = icmp slt i32 %6, %1, !dbg !344
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !344
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !345
  %10 = sext i32 %9 to i64, !dbg !346
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !346
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 100), !dbg !347
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !348
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !349
  ret i1 %13, !dbg !343
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1) #0 !dbg !91 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !90, metadata !DIExpression()), !dbg !350
  call void @llvm.dbg.value(metadata i32 %1, metadata !94, metadata !DIExpression()), !dbg !350
  %3 = icmp ne ptr %0, null, !dbg !351
  ret i1 %3, !dbg !350
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1) #0 !dbg !100 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !99, metadata !DIExpression()), !dbg !352
  call void @llvm.dbg.value(metadata i32 %1, metadata !102, metadata !DIExpression()), !dbg !352
  %3 = icmp sle i32 0, %1, !dbg !353
  br i1 %3, label %4, label %8, !dbg !354

4:                                                ; preds = %2
  %5 = sext i32 %1 to i64, !dbg !355
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !356
  %7 = icmp eq i64 %5, %6, !dbg !357
  br label %8

8:                                                ; preds = %4, %2
  %9 = phi i1 [ false, %2 ], [ %7, %4 ], !dbg !352
  ret i1 %9, !dbg !352
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1) #0 !dbg !108 !pallas.exprWrapper !334 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !107, metadata !DIExpression()), !dbg !358
  call void @llvm.dbg.value(metadata i32 %1, metadata !110, metadata !DIExpression()), !dbg !358
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !359
  %5 = icmp sle i32 0, %4, !dbg !359
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !359
  %7 = icmp slt i32 %6, %1, !dbg !359
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !359
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !360
  %10 = sext i32 %9 to i64, !dbg !361
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !361
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !362
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !363
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !364
  ret i1 %13, !dbg !358
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1) #0 !dbg !116 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !115, metadata !DIExpression()), !dbg !365
  call void @llvm.dbg.value(metadata i32 %1, metadata !118, metadata !DIExpression()), !dbg !365
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !366
  %4 = icmp sle i32 0, %3, !dbg !366
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !366
  %6 = icmp slt i32 %5, %1, !dbg !366
  %7 = call i1 @pallas.scAnd(i1 %4, i1 %6), !dbg !366
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !367
  %9 = sext i32 %8 to i64, !dbg !368
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !368
  %11 = load i32, ptr %10, align 4, !dbg !368
  %12 = icmp sge i32 %11, 0, !dbg !369
  %13 = call i1 @pallas.forall(i1 %7, i1 %12), !dbg !370
  ret i1 %13, !dbg !365
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1) #0 !dbg !124 !pallas.exprWrapper !334 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !123, metadata !DIExpression()), !dbg !371
  call void @llvm.dbg.value(metadata i32 %1, metadata !126, metadata !DIExpression()), !dbg !371
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !372
  %5 = icmp sle i32 0, %4, !dbg !372
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !372
  %7 = icmp slt i32 %6, %1, !dbg !372
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !372
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !373
  %10 = sext i32 %9 to i64, !dbg !374
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !374
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !375
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !376
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !377
  ret i1 %13, !dbg !371
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0, i32 noundef %1) #0 !dbg !132 !pallas.exprWrapper !334 {
  %3 = alloca %struct.SumMaxRes, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !131, metadata !DIExpression()), !dbg !378
  call void @llvm.dbg.value(metadata i32 %1, metadata !134, metadata !DIExpression()), !dbg !378
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !379
  %5 = icmp sle i32 0, %4, !dbg !379
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !379
  %7 = icmp slt i32 %6, %1, !dbg !379
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !379
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !380
  %10 = sext i32 %9 to i64, !dbg !381
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !381
  %12 = load i32, ptr %11, align 4, !dbg !381
  call void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8 %3), !dbg !382
  %13 = getelementptr inbounds %struct.SumMaxRes, ptr %3, i32 0, i32 1, !dbg !383
  %14 = load i32, ptr %13, align 4, !dbg !383
  %15 = icmp sle i32 %12, %14, !dbg !384
  %16 = call i1 @pallas.forall(i1 %8, i1 %15), !dbg !385
  ret i1 %16, !dbg !378
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0, i32 noundef %1) #0 !dbg !140 !pallas.exprWrapper !334 {
  %3 = alloca %struct.SumMaxRes, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !139, metadata !DIExpression()), !dbg !386
  call void @llvm.dbg.value(metadata i32 %1, metadata !142, metadata !DIExpression()), !dbg !386
  %4 = icmp sgt i32 %1, 0, !dbg !387
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !388
  %6 = icmp sle i32 0, %5, !dbg !388
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !388
  %8 = icmp slt i32 %7, %1, !dbg !388
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !388
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !389
  %11 = sext i32 %10 to i64, !dbg !390
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !390
  %13 = load i32, ptr %12, align 4, !dbg !390
  call void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8 %3), !dbg !391
  %14 = getelementptr inbounds %struct.SumMaxRes, ptr %3, i32 0, i32 1, !dbg !392
  %15 = load i32, ptr %14, align 4, !dbg !392
  %16 = icmp eq i32 %13, %15, !dbg !393
  %17 = call i1 @pallas.exists(i1 %9, i1 %16), !dbg !394
  %18 = call i1 @pallas.imply(i1 %4, i1 %17), !dbg !395
  ret i1 %18, !dbg !386
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0, i32 noundef %1) #0 !dbg !148 !pallas.exprWrapper !334 {
  %3 = alloca %struct.SumMaxRes, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !147, metadata !DIExpression()), !dbg !396
  call void @llvm.dbg.value(metadata i32 %1, metadata !150, metadata !DIExpression()), !dbg !396
  call void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8 %3), !dbg !397
  %4 = getelementptr inbounds %struct.SumMaxRes, ptr %3, i32 0, i32 0, !dbg !398
  %5 = load i32, ptr %4, align 8, !dbg !398
  %6 = call i32 @arrSum(ptr noundef %0, i32 noundef %1), !dbg !399
  %7 = icmp eq i32 %5, %6, !dbg !400
  ret i1 %7, !dbg !396
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0, i32 noundef %1) #0 !dbg !156 !pallas.exprWrapper !334 {
  %3 = alloca %struct.SumMaxRes, align 8
  %4 = alloca %struct.SumMaxRes, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !155, metadata !DIExpression()), !dbg !401
  call void @llvm.dbg.value(metadata i32 %1, metadata !158, metadata !DIExpression()), !dbg !401
  call void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8 %3), !dbg !402
  %5 = getelementptr inbounds %struct.SumMaxRes, ptr %3, i32 0, i32 0, !dbg !403
  %6 = load i32, ptr %5, align 8, !dbg !403
  call void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8 %4), !dbg !404
  %7 = getelementptr inbounds %struct.SumMaxRes, ptr %4, i32 0, i32 1, !dbg !405
  %8 = load i32, ptr %7, align 4, !dbg !405
  %9 = mul nsw i32 %8, %1, !dbg !406
  %10 = icmp sle i32 %6, %9, !dbg !407
  ret i1 %10, !dbg !401
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !209 !pallas.exprWrapper !334 {
  %6 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !208, metadata !DIExpression()), !dbg !408
  call void @llvm.dbg.value(metadata i32 %1, metadata !211, metadata !DIExpression()), !dbg !408
  call void @llvm.dbg.value(metadata i32 %2, metadata !213, metadata !DIExpression()), !dbg !408
  call void @llvm.dbg.value(metadata i32 %3, metadata !215, metadata !DIExpression()), !dbg !408
  call void @llvm.dbg.value(metadata i32 %4, metadata !217, metadata !DIExpression()), !dbg !408
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !409
  %8 = icmp sle i32 0, %7, !dbg !409
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !409
  %10 = icmp slt i32 %9, %1, !dbg !409
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !409
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !410
  %13 = sext i32 %12 to i64, !dbg !411
  %14 = getelementptr inbounds i32, ptr %0, i64 %13, !dbg !411
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %6, i32 noundef 1, i32 noundef 4), !dbg !412
  %15 = call i1 @pallas.perm(ptr noundef %14, ptr noundef byval(%pallas.fracT) %6), !dbg !413
  %16 = call i1 @pallas.forallSep(i1 %11, i1 %15), !dbg !414
  ret i1 %16, !dbg !408
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !193 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !192, metadata !DIExpression()), !dbg !415
  call void @llvm.dbg.value(metadata i32 %1, metadata !197, metadata !DIExpression()), !dbg !415
  call void @llvm.dbg.value(metadata i32 %2, metadata !199, metadata !DIExpression()), !dbg !415
  call void @llvm.dbg.value(metadata i32 %3, metadata !201, metadata !DIExpression()), !dbg !415
  call void @llvm.dbg.value(metadata i32 %4, metadata !203, metadata !DIExpression()), !dbg !415
  %6 = icmp sle i32 0, %4, !dbg !416
  br i1 %6, label %7, label %9, !dbg !417

7:                                                ; preds = %5
  %8 = icmp sle i32 %4, %1, !dbg !418
  br label %9

9:                                                ; preds = %7, %5
  %10 = phi i1 [ false, %5 ], [ %8, %7 ], !dbg !415
  ret i1 %10, !dbg !415
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !237 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !236, metadata !DIExpression()), !dbg !419
  call void @llvm.dbg.value(metadata i32 %1, metadata !239, metadata !DIExpression()), !dbg !419
  call void @llvm.dbg.value(metadata i32 %2, metadata !241, metadata !DIExpression()), !dbg !419
  call void @llvm.dbg.value(metadata i32 %3, metadata !243, metadata !DIExpression()), !dbg !419
  call void @llvm.dbg.value(metadata i32 %4, metadata !245, metadata !DIExpression()), !dbg !419
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !420
  %7 = icmp sle i32 0, %6, !dbg !420
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !420
  %9 = icmp slt i32 %8, %1, !dbg !420
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !420
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !421
  %12 = sext i32 %11 to i64, !dbg !422
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !422
  %14 = load i32, ptr %13, align 4, !dbg !422
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !423
  %16 = sext i32 %15 to i64, !dbg !424
  %17 = getelementptr inbounds i32, ptr %0, i64 %16, !dbg !424
  %18 = load i32, ptr %17, align 4, !dbg !424
  %19 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %18), !dbg !425
  %20 = icmp sge i32 %14, %19, !dbg !426
  %21 = call i1 @pallas.forall(i1 %10, i1 %20), !dbg !427
  ret i1 %21, !dbg !419
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !251 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !250, metadata !DIExpression()), !dbg !428
  call void @llvm.dbg.value(metadata i32 %1, metadata !253, metadata !DIExpression()), !dbg !428
  call void @llvm.dbg.value(metadata i32 %2, metadata !255, metadata !DIExpression()), !dbg !428
  call void @llvm.dbg.value(metadata i32 %3, metadata !257, metadata !DIExpression()), !dbg !428
  call void @llvm.dbg.value(metadata i32 %4, metadata !259, metadata !DIExpression()), !dbg !428
  %6 = icmp eq i32 %4, 0, !dbg !429
  %7 = icmp eq i32 %3, 0, !dbg !430
  %8 = call i1 @pallas.imply(i1 %6, i1 %7), !dbg !431
  ret i1 %8, !dbg !428
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !265 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !264, metadata !DIExpression()), !dbg !432
  call void @llvm.dbg.value(metadata i32 %1, metadata !267, metadata !DIExpression()), !dbg !432
  call void @llvm.dbg.value(metadata i32 %2, metadata !269, metadata !DIExpression()), !dbg !432
  call void @llvm.dbg.value(metadata i32 %3, metadata !271, metadata !DIExpression()), !dbg !432
  call void @llvm.dbg.value(metadata i32 %4, metadata !273, metadata !DIExpression()), !dbg !432
  %6 = icmp eq i32 %4, 1, !dbg !433
  %7 = icmp sgt i32 %1, 0, !dbg !434
  %8 = call i1 @pallas.scAnd(i1 %6, i1 %7), !dbg !435
  %9 = getelementptr inbounds i32, ptr %0, i64 0, !dbg !436
  %10 = load i32, ptr %9, align 4, !dbg !436
  %11 = icmp eq i32 %3, %10, !dbg !437
  %12 = call i1 @pallas.imply(i1 %8, i1 %11), !dbg !438
  ret i1 %12, !dbg !432
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !279 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !278, metadata !DIExpression()), !dbg !439
  call void @llvm.dbg.value(metadata i32 %1, metadata !281, metadata !DIExpression()), !dbg !439
  call void @llvm.dbg.value(metadata i32 %2, metadata !283, metadata !DIExpression()), !dbg !439
  call void @llvm.dbg.value(metadata i32 %3, metadata !285, metadata !DIExpression()), !dbg !439
  call void @llvm.dbg.value(metadata i32 %4, metadata !287, metadata !DIExpression()), !dbg !439
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !440
  %7 = icmp sle i32 0, %6, !dbg !440
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !440
  %9 = icmp slt i32 %8, %4, !dbg !440
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !440
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !441
  %12 = sext i32 %11 to i64, !dbg !442
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !442
  %14 = load i32, ptr %13, align 4, !dbg !442
  %15 = icmp sle i32 %14, %3, !dbg !443
  %16 = call i1 @pallas.forall(i1 %10, i1 %15), !dbg !444
  ret i1 %16, !dbg !439
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !223 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !222, metadata !DIExpression()), !dbg !445
  call void @llvm.dbg.value(metadata i32 %1, metadata !225, metadata !DIExpression()), !dbg !445
  call void @llvm.dbg.value(metadata i32 %2, metadata !227, metadata !DIExpression()), !dbg !445
  call void @llvm.dbg.value(metadata i32 %3, metadata !229, metadata !DIExpression()), !dbg !445
  call void @llvm.dbg.value(metadata i32 %4, metadata !231, metadata !DIExpression()), !dbg !445
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !446
  %7 = icmp sle i32 0, %6, !dbg !446
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !446
  %9 = icmp slt i32 %8, %1, !dbg !446
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !446
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !447
  %12 = sext i32 %11 to i64, !dbg !448
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !448
  %14 = load i32, ptr %13, align 4, !dbg !448
  %15 = icmp sge i32 %14, 0, !dbg !449
  %16 = call i1 @pallas.forall(i1 %10, i1 %15), !dbg !450
  ret i1 %16, !dbg !445
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_19(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !293 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !292, metadata !DIExpression()), !dbg !451
  call void @llvm.dbg.value(metadata i32 %1, metadata !295, metadata !DIExpression()), !dbg !451
  call void @llvm.dbg.value(metadata i32 %2, metadata !297, metadata !DIExpression()), !dbg !451
  call void @llvm.dbg.value(metadata i32 %3, metadata !299, metadata !DIExpression()), !dbg !451
  call void @llvm.dbg.value(metadata i32 %4, metadata !301, metadata !DIExpression()), !dbg !451
  %6 = icmp sgt i32 %4, 0, !dbg !452
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !453
  %8 = icmp sle i32 0, %7, !dbg !453
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !453
  %10 = icmp slt i32 %9, %4, !dbg !453
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !453
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !454
  %13 = sext i32 %12 to i64, !dbg !455
  %14 = getelementptr inbounds i32, ptr %0, i64 %13, !dbg !455
  %15 = load i32, ptr %14, align 4, !dbg !455
  %16 = icmp eq i32 %15, %3, !dbg !456
  %17 = call i1 @pallas.exists(i1 %11, i1 %16), !dbg !457
  %18 = call i1 @pallas.imply(i1 %6, i1 %17), !dbg !458
  ret i1 %18, !dbg !451
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_20(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !307 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !306, metadata !DIExpression()), !dbg !459
  call void @llvm.dbg.value(metadata i32 %1, metadata !309, metadata !DIExpression()), !dbg !459
  call void @llvm.dbg.value(metadata i32 %2, metadata !311, metadata !DIExpression()), !dbg !459
  call void @llvm.dbg.value(metadata i32 %3, metadata !313, metadata !DIExpression()), !dbg !459
  call void @llvm.dbg.value(metadata i32 %4, metadata !315, metadata !DIExpression()), !dbg !459
  %6 = call i32 @arrSum(ptr noundef %0, i32 noundef %4), !dbg !460
  %7 = icmp eq i32 %2, %6, !dbg !461
  ret i1 %7, !dbg !459
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_21(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !321 !pallas.exprWrapper !334 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !320, metadata !DIExpression()), !dbg !462
  call void @llvm.dbg.value(metadata i32 %1, metadata !323, metadata !DIExpression()), !dbg !462
  call void @llvm.dbg.value(metadata i32 %2, metadata !325, metadata !DIExpression()), !dbg !462
  call void @llvm.dbg.value(metadata i32 %3, metadata !327, metadata !DIExpression()), !dbg !462
  call void @llvm.dbg.value(metadata i32 %4, metadata !329, metadata !DIExpression()), !dbg !462
  %6 = mul nsw i32 %4, %3, !dbg !463
  %7 = icmp sle i32 %2, %6, !dbg !464
  ret i1 %7, !dbg !462
}

declare !pallas.specLib !465 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !466 void @"pallas.result sret(%struct.SumMaxRes) align 8 void"(ptr sret(%struct.SumMaxRes) align 8)

declare !pallas.specLib !467 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !468 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !469 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !470 i32 @"pallas.old i32_noundef i32"(i32 noundef)

declare !pallas.specLib !471 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !472 i1 @pallas.imply(i1, i1)

declare !pallas.specLib !473 i1 @pallas.exists(i1, i1)

declare !pallas.specLib !474 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !475 i32 @"pallas.boundVar i32"(ptr)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!7, !9}
!llvm.module.flags = !{!13, !14, !15, !16, !17, !18, !19}
!llvm.ident = !{!20, !20}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 58, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp_spectral/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "b0c7ce2d53d9eba5bf2e0cdff8cf1349")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = distinct !DICompileUnit(language: DW_LANG_C11, file: !8, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!8 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/C/vstte10_sum_max.c", directory: ".", checksumkind: CSK_MD5, checksum: "76c1e8f4fe9a30e6faf9eb65b3236849")
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !2, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !10, globals: !12, splitDebugInlining: false, nameTableKind: None)
!10 = !{!11}
!11 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!12 = !{!0}
!13 = !{i32 7, !"Dwarf Version", i32 5}
!14 = !{i32 2, !"Debug Info Version", i32 3}
!15 = !{i32 1, !"wchar_size", i32 4}
!16 = !{i32 8, !"PIC Level", i32 2}
!17 = !{i32 7, !"PIE Level", i32 2}
!18 = !{i32 7, !"uwtable", i32 2}
!19 = !{i32 7, !"frame-pointer", i32 2}
!20 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!21 = distinct !DISubprogram(name: "arrSum", scope: !8, file: !8, line: 37, type: !22, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!22 = !DISubroutineType(types: !23)
!23 = !{!24, !25, !24}
!24 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!25 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !24, size: 64)
!26 = !{}
!27 = !{!28, i1 true, i1 false, !26, !26, !30, !43, !51}
!28 = !{!"pallas.srcLoc", i64 30, i64 1, i64 36, i64 1, !29}
!29 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/C/vstte10_sum_max.c", directory: "", checksumkind: CSK_MD5, checksum: "76c1e8f4fe9a30e6faf9eb65b3236849")
!30 = !{!"pallas.requires", !31, ptr @PALLAS_SPEC_0, !26, !26, !32}
!31 = !{!"pallas.srcLoc", i64 32, i64 1, i64 32, i64 21, !29}
!32 = !{!33, !40}
!33 = !{!34, !35}
!34 = !DILocalVariable(name: "arr", arg: 1, scope: !21, file: !8, line: 37, type: !25)
!35 = !DILocalVariable(name: "arr", arg: 1, scope: !36, file: !8, line: 32, type: !25)
!36 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !8, file: !8, line: 32, type: !37, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!37 = !DISubroutineType(types: !38)
!38 = !{!39, !25, !24}
!39 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!40 = !{!41, !42}
!41 = !DILocalVariable(name: "n", arg: 2, scope: !21, file: !8, line: 37, type: !24)
!42 = !DILocalVariable(name: "n", arg: 2, scope: !36, file: !8, line: 32, type: !24)
!43 = !{!"pallas.requires", !44, ptr @PALLAS_SPEC_1, !26, !26, !45}
!44 = !{!"pallas.srcLoc", i64 33, i64 1, i64 33, i64 41, !29}
!45 = !{!46, !49}
!46 = !{!34, !47}
!47 = !DILocalVariable(name: "arr", arg: 1, scope: !48, file: !8, line: 33, type: !25)
!48 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !8, file: !8, line: 33, type: !37, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!49 = !{!41, !50}
!50 = !DILocalVariable(name: "n", arg: 2, scope: !48, file: !8, line: 33, type: !24)
!51 = !{!"pallas.requires", !52, ptr @PALLAS_SPEC_2, !26, !26, !53}
!52 = !{!"pallas.srcLoc", i64 34, i64 1, i64 35, i64 61, !29}
!53 = !{!54, !57}
!54 = !{!34, !55}
!55 = !DILocalVariable(name: "arr", arg: 1, scope: !56, file: !8, line: 34, type: !25)
!56 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !8, file: !8, line: 34, type: !37, scopeLine: 34, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!57 = !{!41, !58}
!58 = !DILocalVariable(name: "n", arg: 2, scope: !56, file: !8, line: 34, type: !24)
!59 = !DILocation(line: 0, scope: !21)
!60 = !DILocation(line: 38, column: 12, scope: !21)
!61 = !DILocation(line: 38, column: 10, scope: !21)
!62 = !DILocation(line: 38, column: 36, scope: !21)
!63 = !DILocation(line: 38, column: 23, scope: !21)
!64 = !DILocation(line: 38, column: 47, scope: !21)
!65 = !DILocation(line: 38, column: 42, scope: !21)
!66 = !DILocation(line: 38, column: 40, scope: !21)
!67 = !DILocation(line: 38, column: 3, scope: !21)
!68 = distinct !DISubprogram(name: "getSumMax", scope: !8, file: !8, line: 58, type: !69, scopeLine: 58, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!69 = !DISubroutineType(types: !70)
!70 = !{!71, !25, !24}
!71 = !DIDerivedType(tag: DW_TAG_typedef, name: "SumMaxRes", file: !8, line: 20, baseType: !72)
!72 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !8, line: 15, size: 192, elements: !73)
!73 = !{!74, !75, !76, !82}
!74 = !DIDerivedType(tag: DW_TAG_member, name: "sum", scope: !72, file: !8, line: 16, baseType: !24, size: 32)
!75 = !DIDerivedType(tag: DW_TAG_member, name: "max", scope: !72, file: !8, line: 17, baseType: !24, size: 32, offset: 32)
!76 = !DIDerivedType(tag: DW_TAG_member, name: "dummy1", scope: !72, file: !8, line: 18, baseType: !77, size: 64, offset: 64)
!77 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !78, line: 27, baseType: !79)
!78 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!79 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !80, line: 44, baseType: !81)
!80 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!81 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!82 = !DIDerivedType(tag: DW_TAG_member, name: "dummy2", scope: !72, file: !8, line: 19, baseType: !77, size: 64, offset: 128)
!83 = !{!84, i1 false, i1 false, !26, !26, !85, !95, !103, !111, !119, !127, !135, !143, !151}
!84 = !{!"pallas.srcLoc", i64 41, i64 1, i64 57, i64 1, !29}
!85 = !{!"pallas.requires", !86, ptr @PALLAS_SPEC_3, !26, !26, !87}
!86 = !{!"pallas.srcLoc", i64 42, i64 1, i64 42, i64 21, !29}
!87 = !{!88, !92}
!88 = !{!89, !90}
!89 = !DILocalVariable(name: "arr", arg: 1, scope: !68, file: !8, line: 58, type: !25)
!90 = !DILocalVariable(name: "arr", arg: 1, scope: !91, file: !8, line: 42, type: !25)
!91 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !8, file: !8, line: 42, type: !37, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!92 = !{!93, !94}
!93 = !DILocalVariable(name: "n", arg: 2, scope: !68, file: !8, line: 58, type: !24)
!94 = !DILocalVariable(name: "n", arg: 2, scope: !91, file: !8, line: 42, type: !24)
!95 = !{!"pallas.requires", !96, ptr @PALLAS_SPEC_4, !26, !26, !97}
!96 = !{!"pallas.srcLoc", i64 43, i64 1, i64 43, i64 41, !29}
!97 = !{!98, !101}
!98 = !{!89, !99}
!99 = !DILocalVariable(name: "arr", arg: 1, scope: !100, file: !8, line: 43, type: !25)
!100 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !8, file: !8, line: 43, type: !37, scopeLine: 43, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!101 = !{!93, !102}
!102 = !DILocalVariable(name: "n", arg: 2, scope: !100, file: !8, line: 43, type: !24)
!103 = !{!"pallas.requires", !104, ptr @PALLAS_SPEC_5, !26, !26, !105}
!104 = !{!"pallas.srcLoc", i64 44, i64 1, i64 45, i64 59, !29}
!105 = !{!106, !109}
!106 = !{!89, !107}
!107 = !DILocalVariable(name: "arr", arg: 1, scope: !108, file: !8, line: 44, type: !25)
!108 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !8, file: !8, line: 44, type: !37, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!109 = !{!93, !110}
!110 = !DILocalVariable(name: "n", arg: 2, scope: !108, file: !8, line: 44, type: !24)
!111 = !{!"pallas.requires", !112, ptr @PALLAS_SPEC_6, !26, !26, !113}
!112 = !{!"pallas.srcLoc", i64 46, i64 1, i64 47, i64 41, !29}
!113 = !{!114, !117}
!114 = !{!89, !115}
!115 = !DILocalVariable(name: "arr", arg: 1, scope: !116, file: !8, line: 46, type: !25)
!116 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !8, file: !8, line: 46, type: !37, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!117 = !{!93, !118}
!118 = !DILocalVariable(name: "n", arg: 2, scope: !116, file: !8, line: 46, type: !24)
!119 = !{!"pallas.ensures", !120, ptr @PALLAS_SPEC_7, !26, !26, !121}
!120 = !{!"pallas.srcLoc", i64 48, i64 1, i64 49, i64 58, !29}
!121 = !{!122, !125}
!122 = !{!89, !123}
!123 = !DILocalVariable(name: "arr", arg: 1, scope: !124, file: !8, line: 48, type: !25)
!124 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !8, file: !8, line: 48, type: !37, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!125 = !{!93, !126}
!126 = !DILocalVariable(name: "n", arg: 2, scope: !124, file: !8, line: 48, type: !24)
!127 = !{!"pallas.ensures", !128, ptr @PALLAS_SPEC_8, !26, !26, !129}
!128 = !{!"pallas.srcLoc", i64 50, i64 1, i64 51, i64 60, !29}
!129 = !{!130, !133}
!130 = !{!89, !131}
!131 = !DILocalVariable(name: "arr", arg: 1, scope: !132, file: !8, line: 50, type: !25)
!132 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !8, file: !8, line: 50, type: !37, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!133 = !{!93, !134}
!134 = !DILocalVariable(name: "n", arg: 2, scope: !132, file: !8, line: 50, type: !24)
!135 = !{!"pallas.ensures", !136, ptr @PALLAS_SPEC_9, !26, !26, !137}
!136 = !{!"pallas.srcLoc", i64 52, i64 1, i64 54, i64 68, !29}
!137 = !{!138, !141}
!138 = !{!89, !139}
!139 = !DILocalVariable(name: "arr", arg: 1, scope: !140, file: !8, line: 52, type: !25)
!140 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !8, file: !8, line: 52, type: !37, scopeLine: 52, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!141 = !{!93, !142}
!142 = !DILocalVariable(name: "n", arg: 2, scope: !140, file: !8, line: 52, type: !24)
!143 = !{!"pallas.ensures", !144, ptr @PALLAS_SPEC_10, !26, !26, !145}
!144 = !{!"pallas.srcLoc", i64 55, i64 1, i64 55, i64 49, !29}
!145 = !{!146, !149}
!146 = !{!89, !147}
!147 = !DILocalVariable(name: "arr", arg: 1, scope: !148, file: !8, line: 55, type: !25)
!148 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !8, file: !8, line: 55, type: !37, scopeLine: 55, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!149 = !{!93, !150}
!150 = !DILocalVariable(name: "n", arg: 2, scope: !148, file: !8, line: 55, type: !24)
!151 = !{!"pallas.ensures", !152, ptr @PALLAS_SPEC_11, !26, !26, !153}
!152 = !{!"pallas.srcLoc", i64 56, i64 1, i64 56, i64 61, !29}
!153 = !{!154, !157}
!154 = !{!89, !155}
!155 = !DILocalVariable(name: "arr", arg: 1, scope: !156, file: !8, line: 56, type: !25)
!156 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !8, file: !8, line: 56, type: !37, scopeLine: 56, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!157 = !{!93, !158}
!158 = !DILocalVariable(name: "n", arg: 2, scope: !156, file: !8, line: 56, type: !24)
!159 = !DILocation(line: 0, scope: !68)
!160 = !DILocalVariable(name: "sum", scope: !68, file: !8, line: 59, type: !24)
!161 = !DILocalVariable(name: "max", scope: !68, file: !8, line: 60, type: !24)
!162 = !DILocalVariable(name: "i", scope: !163, file: !8, line: 79, type: !24)
!163 = distinct !DILexicalBlock(scope: !68, file: !8, line: 79, column: 5)
!164 = !DILocation(line: 0, scope: !163)
!165 = !DILocation(line: 79, column: 10, scope: !163)
!166 = !DILocation(line: 79, scope: !163)
!167 = !DILocation(line: 79, column: 23, scope: !168)
!168 = distinct !DILexicalBlock(scope: !163, file: !8, line: 79, column: 5)
!169 = !DILocation(line: 79, column: 5, scope: !163)
!170 = !DILocation(line: 80, column: 17, scope: !171)
!171 = distinct !DILexicalBlock(scope: !168, file: !8, line: 79, column: 33)
!172 = !DILocalVariable(name: "e", scope: !171, file: !8, line: 80, type: !24)
!173 = !DILocation(line: 0, scope: !171)
!174 = !DILocation(line: 81, column: 15, scope: !175)
!175 = distinct !DILexicalBlock(scope: !171, file: !8, line: 81, column: 13)
!176 = !DILocation(line: 81, column: 13, scope: !171)
!177 = !DILocation(line: 83, column: 9, scope: !178)
!178 = distinct !DILexicalBlock(scope: !175, file: !8, line: 81, column: 22)
!179 = !DILocation(line: 85, column: 13, scope: !171)
!180 = !DILocation(line: 86, column: 5, scope: !171)
!181 = !DILocation(line: 79, column: 28, scope: !168)
!182 = !DILocation(line: 79, column: 5, scope: !168)
!183 = distinct !{!183, !169, !184, !185, !186}
!184 = !DILocation(line: 86, column: 5, scope: !163)
!185 = !{!"llvm.loop.mustprogress"}
!186 = !{!"pallas.loopInvBlock", !187, !188, !204, !218, !232, !246, !260, !274, !288, !302, !316}
!187 = !{!"pallas.srcLoc", i64 62, i64 5, i64 78, i64 5, !29}
!188 = !{!"pallas.loopInv", !189, ptr @PALLAS_SPEC_12, !26, !26, !190}
!189 = !{!"pallas.srcLoc", i64 63, i64 5, i64 63, i64 36, !29}
!190 = !{!191, !196, !198, !200, !202}
!191 = !{!89, !192}
!192 = !DILocalVariable(name: "arr", arg: 1, scope: !193, file: !8, line: 63, type: !25)
!193 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !8, file: !8, line: 63, type: !194, scopeLine: 63, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!194 = !DISubroutineType(types: !195)
!195 = !{!39, !25, !24, !24, !24, !24}
!196 = !{!93, !197}
!197 = !DILocalVariable(name: "n", arg: 2, scope: !193, file: !8, line: 63, type: !24)
!198 = !{!160, !199}
!199 = !DILocalVariable(name: "sum", arg: 3, scope: !193, file: !8, line: 63, type: !24)
!200 = !{!161, !201}
!201 = !DILocalVariable(name: "max", arg: 4, scope: !193, file: !8, line: 63, type: !24)
!202 = !{!162, !203}
!203 = !DILocalVariable(name: "i", arg: 5, scope: !193, file: !8, line: 63, type: !24)
!204 = !{!"pallas.loopInv", !205, ptr @PALLAS_SPEC_13, !26, !26, !206}
!205 = !{!"pallas.srcLoc", i64 64, i64 5, i64 65, i64 69, !29}
!206 = !{!207, !210, !212, !214, !216}
!207 = !{!89, !208}
!208 = !DILocalVariable(name: "arr", arg: 1, scope: !209, file: !8, line: 64, type: !25)
!209 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !8, file: !8, line: 64, type: !194, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!210 = !{!93, !211}
!211 = !DILocalVariable(name: "n", arg: 2, scope: !209, file: !8, line: 64, type: !24)
!212 = !{!160, !213}
!213 = !DILocalVariable(name: "sum", arg: 3, scope: !209, file: !8, line: 64, type: !24)
!214 = !{!161, !215}
!215 = !DILocalVariable(name: "max", arg: 4, scope: !209, file: !8, line: 64, type: !24)
!216 = !{!162, !217}
!217 = !DILocalVariable(name: "i", arg: 5, scope: !209, file: !8, line: 64, type: !24)
!218 = !{!"pallas.loopInv", !219, ptr @PALLAS_SPEC_14, !26, !26, !220}
!219 = !{!"pallas.srcLoc", i64 66, i64 5, i64 67, i64 51, !29}
!220 = !{!221, !224, !226, !228, !230}
!221 = !{!89, !222}
!222 = !DILocalVariable(name: "arr", arg: 1, scope: !223, file: !8, line: 66, type: !25)
!223 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !8, file: !8, line: 66, type: !194, scopeLine: 66, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!224 = !{!93, !225}
!225 = !DILocalVariable(name: "n", arg: 2, scope: !223, file: !8, line: 66, type: !24)
!226 = !{!160, !227}
!227 = !DILocalVariable(name: "sum", arg: 3, scope: !223, file: !8, line: 66, type: !24)
!228 = !{!161, !229}
!229 = !DILocalVariable(name: "max", arg: 4, scope: !223, file: !8, line: 66, type: !24)
!230 = !{!162, !231}
!231 = !DILocalVariable(name: "i", arg: 5, scope: !223, file: !8, line: 66, type: !24)
!232 = !{!"pallas.loopInv", !233, ptr @PALLAS_SPEC_15, !26, !26, !234}
!233 = !{!"pallas.srcLoc", i64 68, i64 5, i64 69, i64 77, !29}
!234 = !{!235, !238, !240, !242, !244}
!235 = !{!89, !236}
!236 = !DILocalVariable(name: "arr", arg: 1, scope: !237, file: !8, line: 68, type: !25)
!237 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !8, file: !8, line: 68, type: !194, scopeLine: 68, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!238 = !{!93, !239}
!239 = !DILocalVariable(name: "n", arg: 2, scope: !237, file: !8, line: 68, type: !24)
!240 = !{!160, !241}
!241 = !DILocalVariable(name: "sum", arg: 3, scope: !237, file: !8, line: 68, type: !24)
!242 = !{!161, !243}
!243 = !DILocalVariable(name: "max", arg: 4, scope: !237, file: !8, line: 68, type: !24)
!244 = !{!162, !245}
!245 = !DILocalVariable(name: "i", arg: 5, scope: !237, file: !8, line: 68, type: !24)
!246 = !{!"pallas.loopInv", !247, ptr @PALLAS_SPEC_16, !26, !26, !248}
!247 = !{!"pallas.srcLoc", i64 70, i64 5, i64 70, i64 44, !29}
!248 = !{!249, !252, !254, !256, !258}
!249 = !{!89, !250}
!250 = !DILocalVariable(name: "arr", arg: 1, scope: !251, file: !8, line: 70, type: !25)
!251 = distinct !DISubprogram(name: "PALLAS_SPEC_16", scope: !8, file: !8, line: 70, type: !194, scopeLine: 70, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!252 = !{!93, !253}
!253 = !DILocalVariable(name: "n", arg: 2, scope: !251, file: !8, line: 70, type: !24)
!254 = !{!160, !255}
!255 = !DILocalVariable(name: "sum", arg: 3, scope: !251, file: !8, line: 70, type: !24)
!256 = !{!161, !257}
!257 = !DILocalVariable(name: "max", arg: 4, scope: !251, file: !8, line: 70, type: !24)
!258 = !{!162, !259}
!259 = !DILocalVariable(name: "i", arg: 5, scope: !251, file: !8, line: 70, type: !24)
!260 = !{!"pallas.loopInv", !261, ptr @PALLAS_SPEC_17, !26, !26, !262}
!261 = !{!"pallas.srcLoc", i64 71, i64 5, i64 71, i64 62, !29}
!262 = !{!263, !266, !268, !270, !272}
!263 = !{!89, !264}
!264 = !DILocalVariable(name: "arr", arg: 1, scope: !265, file: !8, line: 71, type: !25)
!265 = distinct !DISubprogram(name: "PALLAS_SPEC_17", scope: !8, file: !8, line: 71, type: !194, scopeLine: 71, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!266 = !{!93, !267}
!267 = !DILocalVariable(name: "n", arg: 2, scope: !265, file: !8, line: 71, type: !24)
!268 = !{!160, !269}
!269 = !DILocalVariable(name: "sum", arg: 3, scope: !265, file: !8, line: 71, type: !24)
!270 = !{!161, !271}
!271 = !DILocalVariable(name: "max", arg: 4, scope: !265, file: !8, line: 71, type: !24)
!272 = !{!162, !273}
!273 = !DILocalVariable(name: "i", arg: 5, scope: !265, file: !8, line: 71, type: !24)
!274 = !{!"pallas.loopInv", !275, ptr @PALLAS_SPEC_18, !26, !26, !276}
!275 = !{!"pallas.srcLoc", i64 72, i64 5, i64 73, i64 53, !29}
!276 = !{!277, !280, !282, !284, !286}
!277 = !{!89, !278}
!278 = !DILocalVariable(name: "arr", arg: 1, scope: !279, file: !8, line: 72, type: !25)
!279 = distinct !DISubprogram(name: "PALLAS_SPEC_18", scope: !8, file: !8, line: 72, type: !194, scopeLine: 72, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!280 = !{!93, !281}
!281 = !DILocalVariable(name: "n", arg: 2, scope: !279, file: !8, line: 72, type: !24)
!282 = !{!160, !283}
!283 = !DILocalVariable(name: "sum", arg: 3, scope: !279, file: !8, line: 72, type: !24)
!284 = !{!161, !285}
!285 = !DILocalVariable(name: "max", arg: 4, scope: !279, file: !8, line: 72, type: !24)
!286 = !{!162, !287}
!287 = !DILocalVariable(name: "i", arg: 5, scope: !279, file: !8, line: 72, type: !24)
!288 = !{!"pallas.loopInv", !289, ptr @PALLAS_SPEC_19, !26, !26, !290}
!289 = !{!"pallas.srcLoc", i64 74, i64 5, i64 75, i64 67, !29}
!290 = !{!291, !294, !296, !298, !300}
!291 = !{!89, !292}
!292 = !DILocalVariable(name: "arr", arg: 1, scope: !293, file: !8, line: 74, type: !25)
!293 = distinct !DISubprogram(name: "PALLAS_SPEC_19", scope: !8, file: !8, line: 74, type: !194, scopeLine: 74, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!294 = !{!93, !295}
!295 = !DILocalVariable(name: "n", arg: 2, scope: !293, file: !8, line: 74, type: !24)
!296 = !{!160, !297}
!297 = !DILocalVariable(name: "sum", arg: 3, scope: !293, file: !8, line: 74, type: !24)
!298 = !{!161, !299}
!299 = !DILocalVariable(name: "max", arg: 4, scope: !293, file: !8, line: 74, type: !24)
!300 = !{!162, !301}
!301 = !DILocalVariable(name: "i", arg: 5, scope: !293, file: !8, line: 74, type: !24)
!302 = !{!"pallas.loopInv", !303, ptr @PALLAS_SPEC_20, !26, !26, !304}
!303 = !{!"pallas.srcLoc", i64 76, i64 5, i64 76, i64 41, !29}
!304 = !{!305, !308, !310, !312, !314}
!305 = !{!89, !306}
!306 = !DILocalVariable(name: "arr", arg: 1, scope: !307, file: !8, line: 76, type: !25)
!307 = distinct !DISubprogram(name: "PALLAS_SPEC_20", scope: !8, file: !8, line: 76, type: !194, scopeLine: 76, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!308 = !{!93, !309}
!309 = !DILocalVariable(name: "n", arg: 2, scope: !307, file: !8, line: 76, type: !24)
!310 = !{!160, !311}
!311 = !DILocalVariable(name: "sum", arg: 3, scope: !307, file: !8, line: 76, type: !24)
!312 = !{!161, !313}
!313 = !DILocalVariable(name: "max", arg: 4, scope: !307, file: !8, line: 76, type: !24)
!314 = !{!162, !315}
!315 = !DILocalVariable(name: "i", arg: 5, scope: !307, file: !8, line: 76, type: !24)
!316 = !{!"pallas.loopInv", !317, ptr @PALLAS_SPEC_21, !26, !26, !318}
!317 = !{!"pallas.srcLoc", i64 77, i64 5, i64 77, i64 34, !29}
!318 = !{!319, !322, !324, !326, !328}
!319 = !{!89, !320}
!320 = !DILocalVariable(name: "arr", arg: 1, scope: !321, file: !8, line: 77, type: !25)
!321 = distinct !DISubprogram(name: "PALLAS_SPEC_21", scope: !8, file: !8, line: 77, type: !194, scopeLine: 77, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!322 = !{!93, !323}
!323 = !DILocalVariable(name: "n", arg: 2, scope: !321, file: !8, line: 77, type: !24)
!324 = !{!160, !325}
!325 = !DILocalVariable(name: "sum", arg: 3, scope: !321, file: !8, line: 77, type: !24)
!326 = !{!161, !327}
!327 = !DILocalVariable(name: "max", arg: 4, scope: !321, file: !8, line: 77, type: !24)
!328 = !{!162, !329}
!329 = !DILocalVariable(name: "i", arg: 5, scope: !321, file: !8, line: 77, type: !24)
!330 = !DILocalVariable(name: "res", scope: !68, file: !8, line: 88, type: !71)
!331 = !DILocation(line: 88, column: 15, scope: !68)
!332 = !DILocation(line: 88, column: 21, scope: !68)
!333 = !DILocation(line: 89, column: 5, scope: !68)
!334 = !{!""}
!335 = !DILocation(line: 0, scope: !36)
!336 = !DILocation(line: 32, column: 14, scope: !36)
!337 = !DILocation(line: 0, scope: !48)
!338 = !DILocation(line: 33, column: 12, scope: !48)
!339 = !DILocation(line: 33, column: 17, scope: !48)
!340 = !DILocation(line: 33, column: 20, scope: !48)
!341 = !DILocation(line: 33, column: 25, scope: !48)
!342 = !DILocation(line: 33, column: 22, scope: !48)
!343 = !DILocation(line: 0, scope: !56)
!344 = !DILocation(line: 34, column: 19, scope: !56)
!345 = !DILocation(line: 35, column: 30, scope: !56)
!346 = !DILocation(line: 35, column: 26, scope: !56)
!347 = !DILocation(line: 35, column: 44, scope: !56)
!348 = !DILocation(line: 35, column: 19, scope: !56)
!349 = !DILocation(line: 34, column: 10, scope: !56)
!350 = !DILocation(line: 0, scope: !91)
!351 = !DILocation(line: 42, column: 14, scope: !91)
!352 = !DILocation(line: 0, scope: !100)
!353 = !DILocation(line: 43, column: 12, scope: !100)
!354 = !DILocation(line: 43, column: 17, scope: !100)
!355 = !DILocation(line: 43, column: 20, scope: !100)
!356 = !DILocation(line: 43, column: 25, scope: !100)
!357 = !DILocation(line: 43, column: 22, scope: !100)
!358 = !DILocation(line: 0, scope: !108)
!359 = !DILocation(line: 44, column: 19, scope: !108)
!360 = !DILocation(line: 45, column: 30, scope: !108)
!361 = !DILocation(line: 45, column: 26, scope: !108)
!362 = !DILocation(line: 45, column: 44, scope: !108)
!363 = !DILocation(line: 45, column: 19, scope: !108)
!364 = !DILocation(line: 44, column: 10, scope: !108)
!365 = !DILocation(line: 0, scope: !116)
!366 = !DILocation(line: 46, column: 19, scope: !116)
!367 = !DILocation(line: 47, column: 23, scope: !116)
!368 = !DILocation(line: 47, column: 19, scope: !116)
!369 = !DILocation(line: 47, column: 36, scope: !116)
!370 = !DILocation(line: 46, column: 10, scope: !116)
!371 = !DILocation(line: 0, scope: !124)
!372 = !DILocation(line: 48, column: 18, scope: !124)
!373 = !DILocation(line: 49, column: 29, scope: !124)
!374 = !DILocation(line: 49, column: 25, scope: !124)
!375 = !DILocation(line: 49, column: 43, scope: !124)
!376 = !DILocation(line: 49, column: 18, scope: !124)
!377 = !DILocation(line: 48, column: 9, scope: !124)
!378 = !DILocation(line: 0, scope: !132)
!379 = !DILocation(line: 50, column: 17, scope: !132)
!380 = !DILocation(line: 51, column: 21, scope: !132)
!381 = !DILocation(line: 51, column: 17, scope: !132)
!382 = !DILocation(line: 51, column: 37, scope: !132)
!383 = !DILocation(line: 51, column: 56, scope: !132)
!384 = !DILocation(line: 51, column: 34, scope: !132)
!385 = !DILocation(line: 50, column: 9, scope: !132)
!386 = !DILocation(line: 0, scope: !140)
!387 = !DILocation(line: 52, column: 18, scope: !140)
!388 = !DILocation(line: 53, column: 24, scope: !140)
!389 = !DILocation(line: 54, column: 28, scope: !140)
!390 = !DILocation(line: 54, column: 24, scope: !140)
!391 = !DILocation(line: 54, column: 44, scope: !140)
!392 = !DILocation(line: 54, column: 63, scope: !140)
!393 = !DILocation(line: 54, column: 41, scope: !140)
!394 = !DILocation(line: 53, column: 16, scope: !140)
!395 = !DILocation(line: 52, column: 9, scope: !140)
!396 = !DILocation(line: 0, scope: !148)
!397 = !DILocation(line: 55, column: 9, scope: !148)
!398 = !DILocation(line: 55, column: 28, scope: !148)
!399 = !DILocation(line: 55, column: 35, scope: !148)
!400 = !DILocation(line: 55, column: 32, scope: !148)
!401 = !DILocation(line: 0, scope: !156)
!402 = !DILocation(line: 56, column: 9, scope: !156)
!403 = !DILocation(line: 56, column: 28, scope: !156)
!404 = !DILocation(line: 56, column: 35, scope: !156)
!405 = !DILocation(line: 56, column: 54, scope: !156)
!406 = !DILocation(line: 56, column: 58, scope: !156)
!407 = !DILocation(line: 56, column: 32, scope: !156)
!408 = !DILocation(line: 0, scope: !209)
!409 = !DILocation(line: 64, column: 29, scope: !209)
!410 = !DILocation(line: 65, column: 40, scope: !209)
!411 = !DILocation(line: 65, column: 36, scope: !209)
!412 = !DILocation(line: 65, column: 54, scope: !209)
!413 = !DILocation(line: 65, column: 29, scope: !209)
!414 = !DILocation(line: 64, column: 20, scope: !209)
!415 = !DILocation(line: 0, scope: !193)
!416 = !DILocation(line: 63, column: 22, scope: !193)
!417 = !DILocation(line: 63, column: 27, scope: !193)
!418 = !DILocation(line: 63, column: 32, scope: !193)
!419 = !DILocation(line: 0, scope: !237)
!420 = !DILocation(line: 68, column: 29, scope: !237)
!421 = !DILocation(line: 69, column: 33, scope: !237)
!422 = !DILocation(line: 69, column: 29, scope: !237)
!423 = !DILocation(line: 69, column: 63, scope: !237)
!424 = !DILocation(line: 69, column: 59, scope: !237)
!425 = !DILocation(line: 69, column: 49, scope: !237)
!426 = !DILocation(line: 69, column: 46, scope: !237)
!427 = !DILocation(line: 68, column: 20, scope: !237)
!428 = !DILocation(line: 0, scope: !251)
!429 = !DILocation(line: 70, column: 29, scope: !251)
!430 = !DILocation(line: 70, column: 39, scope: !251)
!431 = !DILocation(line: 70, column: 20, scope: !251)
!432 = !DILocation(line: 0, scope: !265)
!433 = !DILocation(line: 71, column: 34, scope: !265)
!434 = !DILocation(line: 71, column: 42, scope: !265)
!435 = !DILocation(line: 71, column: 27, scope: !265)
!436 = !DILocation(line: 71, column: 55, scope: !265)
!437 = !DILocation(line: 71, column: 52, scope: !265)
!438 = !DILocation(line: 71, column: 20, scope: !265)
!439 = !DILocation(line: 0, scope: !279)
!440 = !DILocation(line: 72, column: 29, scope: !279)
!441 = !DILocation(line: 73, column: 33, scope: !279)
!442 = !DILocation(line: 73, column: 29, scope: !279)
!443 = !DILocation(line: 73, column: 46, scope: !279)
!444 = !DILocation(line: 72, column: 20, scope: !279)
!445 = !DILocation(line: 0, scope: !223)
!446 = !DILocation(line: 66, column: 29, scope: !223)
!447 = !DILocation(line: 67, column: 33, scope: !223)
!448 = !DILocation(line: 67, column: 29, scope: !223)
!449 = !DILocation(line: 67, column: 46, scope: !223)
!450 = !DILocation(line: 66, column: 20, scope: !223)
!451 = !DILocation(line: 0, scope: !293)
!452 = !DILocation(line: 74, column: 29, scope: !293)
!453 = !DILocation(line: 74, column: 42, scope: !293)
!454 = !DILocation(line: 75, column: 46, scope: !293)
!455 = !DILocation(line: 75, column: 42, scope: !293)
!456 = !DILocation(line: 75, column: 59, scope: !293)
!457 = !DILocation(line: 74, column: 34, scope: !293)
!458 = !DILocation(line: 74, column: 20, scope: !293)
!459 = !DILocation(line: 0, scope: !307)
!460 = !DILocation(line: 76, column: 27, scope: !307)
!461 = !DILocation(line: 76, column: 24, scope: !307)
!462 = !DILocation(line: 0, scope: !321)
!463 = !DILocation(line: 77, column: 29, scope: !321)
!464 = !DILocation(line: 77, column: 24, scope: !321)
!465 = !{!"pallas.ptrLength"}
!466 = !{!"pallas.result"}
!467 = !{!"pallas.forallSep"}
!468 = !{!"pallas.perm"}
!469 = !{!"pallas.fracOf"}
!470 = !{!"pallas.old"}
!471 = !{!"pallas.forall"}
!472 = !{!"pallas.imply"}
!473 = !{!"pallas.exists"}
!474 = !{!"pallas.scAnd"}
!475 = !{!"pallas.boundVar"}
