; ModuleID = 'tmp_spectral/tmp_ir_source0.ll'
source_filename = "examples/publications/2026/ATVA2026Spectral/C/sort.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [28 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_15, ptr @PALLAS_SPEC_16, ptr @PALLAS_SPEC_17, ptr @PALLAS_SPEC_18, ptr @PALLAS_SPEC_19, ptr @PALLAS_SPEC_21, ptr @PALLAS_SPEC_20, ptr @PALLAS_SPEC_23, ptr @PALLAS_SPEC_24, ptr @PALLAS_SPEC_25, ptr @PALLAS_SPEC_26, ptr @PALLAS_SPEC_22, ptr @PALLAS_SPEC_27], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0
@.str.1 = private unnamed_addr constant [2 x i8] c"j\00", align 1, !dbg !7

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @getMinIdx(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !23 !pallas.fcontract !29 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !36, metadata !DIExpression()), !dbg !98
  call void @llvm.dbg.value(metadata i32 %1, metadata !43, metadata !DIExpression()), !dbg !98
  call void @llvm.dbg.value(metadata i32 %2, metadata !46, metadata !DIExpression()), !dbg !98
  call void @llvm.dbg.value(metadata i32 %1, metadata !99, metadata !DIExpression()), !dbg !98
  %4 = add nsw i32 %1, 1, !dbg !100
  call void @llvm.dbg.value(metadata i32 %4, metadata !102, metadata !DIExpression()), !dbg !103
  br label %5, !dbg !104

5:                                                ; preds = %17, %3
  %.01 = phi i32 [ %1, %3 ], [ %.1, %17 ], !dbg !98
  %.0 = phi i32 [ %4, %3 ], [ %18, %17 ], !dbg !105
  call void @llvm.dbg.value(metadata i32 %.0, metadata !102, metadata !DIExpression()), !dbg !103
  call void @llvm.dbg.value(metadata i32 %.01, metadata !99, metadata !DIExpression()), !dbg !98
  %6 = icmp slt i32 %.0, %2, !dbg !106
  br i1 %6, label %7, label %19, !dbg !108

7:                                                ; preds = %5
  %8 = sext i32 %.0 to i64, !dbg !109
  %9 = getelementptr inbounds i32, ptr %0, i64 %8, !dbg !109
  %10 = load i32, ptr %9, align 4, !dbg !109
  %11 = sext i32 %.01 to i64, !dbg !112
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !112
  %13 = load i32, ptr %12, align 4, !dbg !112
  %14 = icmp slt i32 %10, %13, !dbg !113
  br i1 %14, label %15, label %16, !dbg !114

15:                                               ; preds = %7
  call void @llvm.dbg.value(metadata i32 %.0, metadata !99, metadata !DIExpression()), !dbg !98
  br label %16, !dbg !115

16:                                               ; preds = %15, %7
  %.1 = phi i32 [ %.0, %15 ], [ %.01, %7 ], !dbg !98
  call void @llvm.dbg.value(metadata i32 %.1, metadata !99, metadata !DIExpression()), !dbg !98
  br label %17, !dbg !117

17:                                               ; preds = %16
  %18 = add nsw i32 %.0, 1, !dbg !118
  call void @llvm.dbg.value(metadata i32 %18, metadata !102, metadata !DIExpression()), !dbg !103
  br label %5, !dbg !119, !llvm.loop !120

19:                                               ; preds = %5
  ret i32 %.01, !dbg !183
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @swap(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !184 !pallas.fcontract !187 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !193, metadata !DIExpression()), !dbg !272
  call void @llvm.dbg.value(metadata i32 %1, metadata !197, metadata !DIExpression()), !dbg !272
  call void @llvm.dbg.value(metadata i32 %2, metadata !200, metadata !DIExpression()), !dbg !272
  %4 = sext i32 %1 to i64, !dbg !273
  %5 = getelementptr inbounds i32, ptr %0, i64 %4, !dbg !273
  %6 = load i32, ptr %5, align 4, !dbg !273
  call void @llvm.dbg.value(metadata i32 %6, metadata !274, metadata !DIExpression()), !dbg !272
  %7 = sext i32 %2 to i64, !dbg !275
  %8 = getelementptr inbounds i32, ptr %0, i64 %7, !dbg !275
  %9 = load i32, ptr %8, align 4, !dbg !275
  %10 = sext i32 %1 to i64, !dbg !276
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !276
  store i32 %9, ptr %11, align 4, !dbg !277
  %12 = sext i32 %2 to i64, !dbg !278
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !278
  store i32 %6, ptr %13, align 4, !dbg !279
  ret void, !dbg !280
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @selectSort(ptr noundef %0, i32 noundef %1) #0 !dbg !281 !pallas.fcontract !284 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !290, metadata !DIExpression()), !dbg !338
  call void @llvm.dbg.value(metadata i32 %1, metadata !296, metadata !DIExpression()), !dbg !338
  %3 = getelementptr inbounds i32, ptr %0, i64 0, !dbg !339
  %4 = load i32, ptr %3, align 4, !dbg !339
  call void @llvm.dbg.value(metadata i32 %4, metadata !340, metadata !DIExpression()), !dbg !338
  call void @llvm.dbg.value(metadata i32 0, metadata !341, metadata !DIExpression()), !dbg !343
  br label %5, !dbg !344

5:                                                ; preds = %13, %2
  %.0 = phi i32 [ 0, %2 ], [ %14, %13 ], !dbg !345
  call void @llvm.dbg.value(metadata i32 %.0, metadata !341, metadata !DIExpression()), !dbg !343
  %6 = sub nsw i32 %1, 1, !dbg !346
  %7 = icmp slt i32 %.0, %6, !dbg !348
  br i1 %7, label %8, label %15, !dbg !349

8:                                                ; preds = %5
  %9 = call i32 @getMinIdx(ptr noundef %0, i32 noundef %.0, i32 noundef %1), !dbg !350
  call void @llvm.dbg.value(metadata i32 %9, metadata !352, metadata !DIExpression()), !dbg !353
  %10 = icmp ne i32 %9, %.0, !dbg !354
  br i1 %10, label %11, label %12, !dbg !356

11:                                               ; preds = %8
  call void @swap(ptr noundef %0, i32 noundef %.0, i32 noundef %9), !dbg !357
  br label %12, !dbg !359

12:                                               ; preds = %11, %8
  br label %13, !dbg !360

13:                                               ; preds = %12
  %14 = add nsw i32 %.0, 1, !dbg !361
  call void @llvm.dbg.value(metadata i32 %14, metadata !341, metadata !DIExpression()), !dbg !343
  br label %5, !dbg !362, !llvm.loop !363

15:                                               ; preds = %5
  ret void, !dbg !417
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !38 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !37, metadata !DIExpression()), !dbg !419
  call void @llvm.dbg.value(metadata i32 %1, metadata !44, metadata !DIExpression()), !dbg !419
  call void @llvm.dbg.value(metadata i32 %2, metadata !47, metadata !DIExpression()), !dbg !419
  %4 = icmp ne ptr %0, null, !dbg !420
  ret i1 %4, !dbg !419
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !53 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !52, metadata !DIExpression()), !dbg !421
  call void @llvm.dbg.value(metadata i32 %1, metadata !55, metadata !DIExpression()), !dbg !421
  call void @llvm.dbg.value(metadata i32 %2, metadata !57, metadata !DIExpression()), !dbg !421
  %4 = icmp sle i32 0, %1, !dbg !422
  br i1 %4, label %5, label %11, !dbg !423

5:                                                ; preds = %3
  %6 = icmp slt i32 %1, %2, !dbg !424
  br i1 %6, label %7, label %11, !dbg !425

7:                                                ; preds = %5
  %8 = sext i32 %2 to i64, !dbg !426
  %9 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !427
  %10 = icmp sle i64 %8, %9, !dbg !428
  br label %11

11:                                               ; preds = %7, %5, %3
  %12 = phi i1 [ false, %5 ], [ false, %3 ], [ %10, %7 ], !dbg !421
  ret i1 %12, !dbg !421
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !63 !pallas.exprWrapper !418 {
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !62, metadata !DIExpression()), !dbg !429
  call void @llvm.dbg.value(metadata i32 %1, metadata !65, metadata !DIExpression()), !dbg !429
  call void @llvm.dbg.value(metadata i32 %2, metadata !67, metadata !DIExpression()), !dbg !429
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !430
  %6 = icmp sle i32 %1, %5, !dbg !430
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !430
  %8 = icmp slt i32 %7, %2, !dbg !430
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !430
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !431
  %11 = sext i32 %10 to i64, !dbg !432
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !432
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 2), !dbg !433
  %13 = call i1 @pallas.perm(ptr noundef %12, ptr noundef byval(%pallas.fracT) %4), !dbg !434
  %14 = call i1 @pallas.forallSep(i1 %9, i1 %13), !dbg !435
  ret i1 %14, !dbg !429
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !73 !pallas.exprWrapper !418 {
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !72, metadata !DIExpression()), !dbg !436
  call void @llvm.dbg.value(metadata i32 %1, metadata !75, metadata !DIExpression()), !dbg !436
  call void @llvm.dbg.value(metadata i32 %2, metadata !77, metadata !DIExpression()), !dbg !436
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !437
  %6 = icmp sle i32 %1, %5, !dbg !437
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !437
  %8 = icmp slt i32 %7, %2, !dbg !437
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !437
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !438
  %11 = sext i32 %10 to i64, !dbg !439
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !439
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 2), !dbg !440
  %13 = call i1 @pallas.perm(ptr noundef %12, ptr noundef byval(%pallas.fracT) %4), !dbg !441
  %14 = call i1 @pallas.forallSep(i1 %9, i1 %13), !dbg !442
  ret i1 %14, !dbg !436
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !83 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !82, metadata !DIExpression()), !dbg !443
  call void @llvm.dbg.value(metadata i32 %1, metadata !85, metadata !DIExpression()), !dbg !443
  call void @llvm.dbg.value(metadata i32 %2, metadata !87, metadata !DIExpression()), !dbg !443
  %4 = call i32 @"pallas.result i32"(), !dbg !444
  %5 = icmp sle i32 %1, %4, !dbg !444
  %6 = call i32 @"pallas.result i32"(), !dbg !444
  %7 = icmp slt i32 %6, %2, !dbg !444
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !444
  ret i1 %8, !dbg !443
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !93 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !92, metadata !DIExpression()), !dbg !445
  call void @llvm.dbg.value(metadata i32 %1, metadata !95, metadata !DIExpression()), !dbg !445
  call void @llvm.dbg.value(metadata i32 %2, metadata !97, metadata !DIExpression()), !dbg !445
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !446
  %5 = icmp sle i32 %1, %4, !dbg !446
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !446
  %7 = icmp slt i32 %6, %2, !dbg !446
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !446
  %9 = call i32 @"pallas.result i32"(), !dbg !447
  %10 = sext i32 %9 to i64, !dbg !448
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !448
  %12 = load i32, ptr %11, align 4, !dbg !448
  %13 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !449
  %14 = sext i32 %13 to i64, !dbg !450
  %15 = getelementptr inbounds i32, ptr %0, i64 %14, !dbg !450
  %16 = load i32, ptr %15, align 4, !dbg !450
  %17 = icmp sle i32 %12, %16, !dbg !451
  %18 = call i1 @pallas.forall(i1 %8, i1 %17), !dbg !452
  ret i1 %18, !dbg !445
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !195 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !194, metadata !DIExpression()), !dbg !453
  call void @llvm.dbg.value(metadata i32 %1, metadata !198, metadata !DIExpression()), !dbg !453
  call void @llvm.dbg.value(metadata i32 %2, metadata !201, metadata !DIExpression()), !dbg !453
  %4 = icmp ne ptr %0, null, !dbg !454
  ret i1 %4, !dbg !453
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !207 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !206, metadata !DIExpression()), !dbg !455
  call void @llvm.dbg.value(metadata i32 %1, metadata !209, metadata !DIExpression()), !dbg !455
  call void @llvm.dbg.value(metadata i32 %2, metadata !211, metadata !DIExpression()), !dbg !455
  %4 = icmp sle i32 0, %1, !dbg !456
  %5 = sext i32 %1 to i64, !dbg !456
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !456
  %7 = icmp slt i64 %5, %6, !dbg !456
  %8 = call i1 @pallas.scAnd(i1 %4, i1 %7), !dbg !456
  ret i1 %8, !dbg !455
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !217 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !216, metadata !DIExpression()), !dbg !457
  call void @llvm.dbg.value(metadata i32 %1, metadata !219, metadata !DIExpression()), !dbg !457
  call void @llvm.dbg.value(metadata i32 %2, metadata !221, metadata !DIExpression()), !dbg !457
  %4 = icmp sle i32 0, %2, !dbg !458
  %5 = sext i32 %2 to i64, !dbg !458
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !458
  %7 = icmp slt i64 %5, %6, !dbg !458
  %8 = call i1 @pallas.scAnd(i1 %4, i1 %7), !dbg !458
  ret i1 %8, !dbg !457
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !227 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !226, metadata !DIExpression()), !dbg !459
  call void @llvm.dbg.value(metadata i32 %1, metadata !229, metadata !DIExpression()), !dbg !459
  call void @llvm.dbg.value(metadata i32 %2, metadata !231, metadata !DIExpression()), !dbg !459
  %4 = icmp ne i32 %1, %2, !dbg !460
  ret i1 %4, !dbg !459
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !237 !pallas.exprWrapper !418 {
  %4 = alloca %pallas.fracT, align 8
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !236, metadata !DIExpression()), !dbg !461
  call void @llvm.dbg.value(metadata i32 %1, metadata !239, metadata !DIExpression()), !dbg !461
  call void @llvm.dbg.value(metadata i32 %2, metadata !241, metadata !DIExpression()), !dbg !461
  %6 = sext i32 %1 to i64, !dbg !462
  %7 = getelementptr inbounds i32, ptr %0, i64 %6, !dbg !462
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 1), !dbg !463
  %8 = call i1 @pallas.perm(ptr noundef %7, ptr noundef byval(%pallas.fracT) %4), !dbg !464
  %9 = sext i32 %2 to i64, !dbg !465
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !465
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 1), !dbg !466
  %11 = call i1 @pallas.perm(ptr noundef %10, ptr noundef byval(%pallas.fracT) %5), !dbg !467
  %12 = call i1 @pallas.sepConj(i1 %8, i1 %11), !dbg !468
  ret i1 %12, !dbg !461
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !247 !pallas.exprWrapper !418 {
  %4 = alloca %pallas.fracT, align 8
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !246, metadata !DIExpression()), !dbg !469
  call void @llvm.dbg.value(metadata i32 %1, metadata !249, metadata !DIExpression()), !dbg !469
  call void @llvm.dbg.value(metadata i32 %2, metadata !251, metadata !DIExpression()), !dbg !469
  %6 = sext i32 %1 to i64, !dbg !470
  %7 = getelementptr inbounds i32, ptr %0, i64 %6, !dbg !470
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 1), !dbg !471
  %8 = call i1 @pallas.perm(ptr noundef %7, ptr noundef byval(%pallas.fracT) %4), !dbg !472
  %9 = sext i32 %2 to i64, !dbg !473
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !473
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 1), !dbg !474
  %11 = call i1 @pallas.perm(ptr noundef %10, ptr noundef byval(%pallas.fracT) %5), !dbg !475
  %12 = call i1 @pallas.sepConj(i1 %8, i1 %11), !dbg !476
  ret i1 %12, !dbg !469
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !257 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !256, metadata !DIExpression()), !dbg !477
  call void @llvm.dbg.value(metadata i32 %1, metadata !259, metadata !DIExpression()), !dbg !477
  call void @llvm.dbg.value(metadata i32 %2, metadata !261, metadata !DIExpression()), !dbg !477
  %4 = sext i32 %1 to i64, !dbg !478
  %5 = getelementptr inbounds i32, ptr %0, i64 %4, !dbg !478
  %6 = load i32, ptr %5, align 4, !dbg !478
  %7 = sext i32 %2 to i64, !dbg !479
  %8 = getelementptr inbounds i32, ptr %0, i64 %7, !dbg !479
  %9 = load i32, ptr %8, align 4, !dbg !479
  %10 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %9), !dbg !480
  %11 = icmp eq i32 %6, %10, !dbg !481
  ret i1 %11, !dbg !477
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !267 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !266, metadata !DIExpression()), !dbg !482
  call void @llvm.dbg.value(metadata i32 %1, metadata !269, metadata !DIExpression()), !dbg !482
  call void @llvm.dbg.value(metadata i32 %2, metadata !271, metadata !DIExpression()), !dbg !482
  %4 = sext i32 %2 to i64, !dbg !483
  %5 = getelementptr inbounds i32, ptr %0, i64 %4, !dbg !483
  %6 = load i32, ptr %5, align 4, !dbg !483
  %7 = sext i32 %1 to i64, !dbg !484
  %8 = getelementptr inbounds i32, ptr %0, i64 %7, !dbg !484
  %9 = load i32, ptr %8, align 4, !dbg !484
  %10 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %9), !dbg !485
  %11 = icmp eq i32 %6, %10, !dbg !486
  ret i1 %11, !dbg !482
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0, i32 noundef %1) #0 !dbg !292 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !291, metadata !DIExpression()), !dbg !487
  call void @llvm.dbg.value(metadata i32 %1, metadata !297, metadata !DIExpression()), !dbg !487
  %3 = icmp ne ptr %0, null, !dbg !488
  ret i1 %3, !dbg !487
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0, i32 noundef %1) #0 !dbg !303 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !302, metadata !DIExpression()), !dbg !489
  call void @llvm.dbg.value(metadata i32 %1, metadata !305, metadata !DIExpression()), !dbg !489
  %3 = icmp sgt i32 %1, 0, !dbg !490
  ret i1 %3, !dbg !489
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(ptr noundef %0, i32 noundef %1) #0 !dbg !311 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !310, metadata !DIExpression()), !dbg !491
  call void @llvm.dbg.value(metadata i32 %1, metadata !313, metadata !DIExpression()), !dbg !491
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !492
  %4 = sext i32 %1 to i64, !dbg !493
  %5 = icmp eq i64 %3, %4, !dbg !494
  ret i1 %5, !dbg !491
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(ptr noundef %0, i32 noundef %1) #0 !dbg !319 !pallas.exprWrapper !418 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !318, metadata !DIExpression()), !dbg !495
  call void @llvm.dbg.value(metadata i32 %1, metadata !321, metadata !DIExpression()), !dbg !495
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !496
  %5 = icmp sle i32 0, %4, !dbg !496
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !496
  %7 = icmp slt i32 %6, %1, !dbg !496
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !496
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !497
  %10 = sext i32 %9 to i64, !dbg !498
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !498
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !499
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !500
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !501
  ret i1 %13, !dbg !495
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(ptr noundef %0, i32 noundef %1) #0 !dbg !327 !pallas.exprWrapper !418 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !326, metadata !DIExpression()), !dbg !502
  call void @llvm.dbg.value(metadata i32 %1, metadata !329, metadata !DIExpression()), !dbg !502
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !503
  %5 = icmp sle i32 0, %4, !dbg !503
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !503
  %7 = icmp slt i32 %6, %1, !dbg !503
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !503
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !504
  %10 = sext i32 %9 to i64, !dbg !505
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !505
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !506
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !507
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !508
  ret i1 %13, !dbg !502
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_19(ptr noundef %0, i32 noundef %1) #0 !dbg !335 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !334, metadata !DIExpression()), !dbg !509
  call void @llvm.dbg.value(metadata i32 %1, metadata !337, metadata !DIExpression()), !dbg !509
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !510
  %4 = icmp sle i32 0, %3, !dbg !510
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !510
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !510
  %7 = icmp slt i32 %5, %6, !dbg !510
  %8 = call i1 @pallas.scAnd(i1 %4, i1 %7), !dbg !510
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !511
  %10 = icmp slt i32 %9, %1, !dbg !512
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !513
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !514
  %13 = sext i32 %12 to i64, !dbg !515
  %14 = getelementptr inbounds i32, ptr %0, i64 %13, !dbg !515
  %15 = load i32, ptr %14, align 4, !dbg !515
  %16 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !516
  %17 = sext i32 %16 to i64, !dbg !517
  %18 = getelementptr inbounds i32, ptr %0, i64 %17, !dbg !517
  %19 = load i32, ptr %18, align 4, !dbg !517
  %20 = icmp sle i32 %15, %19, !dbg !518
  %21 = call i1 @pallas.forall(i1 %11, i1 %20), !dbg !519
  ret i1 %21, !dbg !509
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_21(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !146 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !145, metadata !DIExpression()), !dbg !520
  call void @llvm.dbg.value(metadata i32 %1, metadata !148, metadata !DIExpression()), !dbg !520
  call void @llvm.dbg.value(metadata i32 %2, metadata !150, metadata !DIExpression()), !dbg !520
  call void @llvm.dbg.value(metadata i32 %3, metadata !152, metadata !DIExpression()), !dbg !520
  call void @llvm.dbg.value(metadata i32 %4, metadata !154, metadata !DIExpression()), !dbg !520
  %6 = icmp sle i32 %1, %3, !dbg !521
  %7 = icmp slt i32 %3, %2, !dbg !521
  %8 = call i1 @pallas.scAnd(i1 %6, i1 %7), !dbg !521
  ret i1 %8, !dbg !520
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_20(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !130 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !129, metadata !DIExpression()), !dbg !522
  call void @llvm.dbg.value(metadata i32 %1, metadata !134, metadata !DIExpression()), !dbg !522
  call void @llvm.dbg.value(metadata i32 %2, metadata !136, metadata !DIExpression()), !dbg !522
  call void @llvm.dbg.value(metadata i32 %3, metadata !138, metadata !DIExpression()), !dbg !522
  call void @llvm.dbg.value(metadata i32 %4, metadata !140, metadata !DIExpression()), !dbg !522
  %6 = icmp sle i32 %1, %4, !dbg !523
  %7 = add nsw i32 %2, 1, !dbg !523
  %8 = icmp slt i32 %4, %7, !dbg !523
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !523
  ret i1 %9, !dbg !522
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_23(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !174 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !173, metadata !DIExpression()), !dbg !524
  call void @llvm.dbg.value(metadata i32 %1, metadata !176, metadata !DIExpression()), !dbg !524
  call void @llvm.dbg.value(metadata i32 %2, metadata !178, metadata !DIExpression()), !dbg !524
  call void @llvm.dbg.value(metadata i32 %3, metadata !180, metadata !DIExpression()), !dbg !524
  call void @llvm.dbg.value(metadata i32 %4, metadata !182, metadata !DIExpression()), !dbg !524
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !525
  %7 = icmp sle i32 %1, %6, !dbg !525
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !525
  %9 = icmp slt i32 %8, %4, !dbg !525
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !525
  %11 = sext i32 %3 to i64, !dbg !526
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !526
  %13 = load i32, ptr %12, align 4, !dbg !526
  %14 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !527
  %15 = sext i32 %14 to i64, !dbg !528
  %16 = getelementptr inbounds i32, ptr %0, i64 %15, !dbg !528
  %17 = load i32, ptr %16, align 4, !dbg !528
  %18 = icmp sle i32 %13, %17, !dbg !529
  %19 = call i1 @pallas.forall(i1 %10, i1 %18), !dbg !530
  ret i1 %19, !dbg !524
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_24(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !372 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !371, metadata !DIExpression()), !dbg !531
  call void @llvm.dbg.value(metadata i32 %1, metadata !376, metadata !DIExpression()), !dbg !531
  call void @llvm.dbg.value(metadata i32 %2, metadata !378, metadata !DIExpression()), !dbg !531
  call void @llvm.dbg.value(metadata i32 %3, metadata !380, metadata !DIExpression()), !dbg !531
  %5 = icmp sle i32 0, %3, !dbg !532
  br i1 %5, label %6, label %8, !dbg !533

6:                                                ; preds = %4
  %7 = icmp slt i32 %3, %1, !dbg !534
  br label %8

8:                                                ; preds = %6, %4
  %9 = phi i1 [ false, %4 ], [ %7, %6 ], !dbg !531
  ret i1 %9, !dbg !531
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_25(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !386 !pallas.exprWrapper !418 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !385, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.value(metadata i32 %1, metadata !388, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.value(metadata i32 %2, metadata !390, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.value(metadata i32 %3, metadata !392, metadata !DIExpression()), !dbg !535
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !536
  %7 = icmp sle i32 0, %6, !dbg !536
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !536
  %9 = icmp slt i32 %8, %1, !dbg !536
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !536
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !537
  %12 = sext i32 %11 to i64, !dbg !538
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !538
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 1), !dbg !539
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %5), !dbg !540
  %15 = call i1 @pallas.forallSep(i1 %10, i1 %14), !dbg !541
  ret i1 %15, !dbg !535
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_26(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !398 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !397, metadata !DIExpression()), !dbg !542
  call void @llvm.dbg.value(metadata i32 %1, metadata !400, metadata !DIExpression()), !dbg !542
  call void @llvm.dbg.value(metadata i32 %2, metadata !402, metadata !DIExpression()), !dbg !542
  call void @llvm.dbg.value(metadata i32 %3, metadata !404, metadata !DIExpression()), !dbg !542
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !543
  %6 = icmp sle i32 0, %5, !dbg !543
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !543
  %8 = icmp slt i32 %7, %3, !dbg !543
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !543
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !544
  %11 = icmp sle i32 %3, %10, !dbg !544
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !544
  %13 = icmp slt i32 %12, %1, !dbg !544
  %14 = call i1 @pallas.scAnd(i1 %11, i1 %13), !dbg !544
  %15 = call i1 @pallas.scAnd(i1 %9, i1 %14), !dbg !545
  %16 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !546
  %17 = sext i32 %16 to i64, !dbg !547
  %18 = getelementptr inbounds i32, ptr %0, i64 %17, !dbg !547
  %19 = load i32, ptr %18, align 4, !dbg !547
  %20 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !548
  %21 = sext i32 %20 to i64, !dbg !549
  %22 = getelementptr inbounds i32, ptr %0, i64 %21, !dbg !549
  %23 = load i32, ptr %22, align 4, !dbg !549
  %24 = icmp sle i32 %19, %23, !dbg !550
  %25 = call i1 @pallas.forall(i1 %15, i1 %24), !dbg !551
  ret i1 %25, !dbg !542
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_22(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !160 !pallas.exprWrapper !418 {
  %6 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !159, metadata !DIExpression()), !dbg !552
  call void @llvm.dbg.value(metadata i32 %1, metadata !162, metadata !DIExpression()), !dbg !552
  call void @llvm.dbg.value(metadata i32 %2, metadata !164, metadata !DIExpression()), !dbg !552
  call void @llvm.dbg.value(metadata i32 %3, metadata !166, metadata !DIExpression()), !dbg !552
  call void @llvm.dbg.value(metadata i32 %4, metadata !168, metadata !DIExpression()), !dbg !552
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !553
  %8 = icmp sle i32 %1, %7, !dbg !553
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !553
  %10 = icmp slt i32 %9, %2, !dbg !553
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !553
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !554
  %13 = sext i32 %12 to i64, !dbg !555
  %14 = getelementptr inbounds i32, ptr %0, i64 %13, !dbg !555
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %6, i32 noundef 1, i32 noundef 2), !dbg !556
  %15 = call i1 @pallas.perm(ptr noundef %14, ptr noundef byval(%pallas.fracT) %6), !dbg !557
  %16 = call i1 @pallas.forallSep(i1 %11, i1 %15), !dbg !558
  ret i1 %16, !dbg !552
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_27(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !410 !pallas.exprWrapper !418 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !409, metadata !DIExpression()), !dbg !559
  call void @llvm.dbg.value(metadata i32 %1, metadata !412, metadata !DIExpression()), !dbg !559
  call void @llvm.dbg.value(metadata i32 %2, metadata !414, metadata !DIExpression()), !dbg !559
  call void @llvm.dbg.value(metadata i32 %3, metadata !416, metadata !DIExpression()), !dbg !559
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !560
  %6 = icmp sle i32 0, %5, !dbg !560
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !560
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !560
  %9 = icmp slt i32 %7, %8, !dbg !560
  %10 = call i1 @pallas.scAnd(i1 %6, i1 %9), !dbg !560
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !561
  %12 = icmp slt i32 %11, %3, !dbg !562
  %13 = call i1 @pallas.scAnd(i1 %10, i1 %12), !dbg !563
  %14 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !564
  %15 = sext i32 %14 to i64, !dbg !565
  %16 = getelementptr inbounds i32, ptr %0, i64 %15, !dbg !565
  %17 = load i32, ptr %16, align 4, !dbg !565
  %18 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !566
  %19 = sext i32 %18 to i64, !dbg !567
  %20 = getelementptr inbounds i32, ptr %0, i64 %19, !dbg !567
  %21 = load i32, ptr %20, align 4, !dbg !567
  %22 = icmp sle i32 %17, %21, !dbg !568
  %23 = call i1 @pallas.forall(i1 %13, i1 %22), !dbg !569
  ret i1 %23, !dbg !559
}

declare !pallas.specLib !570 i32 @"pallas.result i32"()

declare !pallas.specLib !571 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !572 i32 @"pallas.old i32_noundef i32"(i32 noundef)

declare !pallas.specLib !573 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !574 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !575 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !576 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !577 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !578 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !579 i32 @"pallas.boundVar i32"(ptr)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!9, !11}
!llvm.module.flags = !{!15, !16, !17, !18, !19, !20, !21}
!llvm.ident = !{!22, !22}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 38, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp_spectral/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "aafc73d6238fb39fab88c455c63ad6da")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 281, type: !3, isLocal: true, isDefinition: true)
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !10, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!10 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/C/sort.c", directory: ".", checksumkind: CSK_MD5, checksum: "0609eba1f20650945cb01458fe186fc1")
!11 = distinct !DICompileUnit(language: DW_LANG_C11, file: !2, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !12, globals: !14, splitDebugInlining: false, nameTableKind: None)
!12 = !{!13}
!13 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!14 = !{!0, !7}
!15 = !{i32 7, !"Dwarf Version", i32 5}
!16 = !{i32 2, !"Debug Info Version", i32 3}
!17 = !{i32 1, !"wchar_size", i32 4}
!18 = !{i32 8, !"PIC Level", i32 2}
!19 = !{i32 7, !"PIE Level", i32 2}
!20 = !{i32 7, !"uwtable", i32 2}
!21 = !{i32 7, !"frame-pointer", i32 2}
!22 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!23 = distinct !DISubprogram(name: "getMinIdx", scope: !10, file: !10, line: 19, type: !24, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!24 = !DISubroutineType(types: !25)
!25 = !{!26, !27, !26, !26}
!26 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!27 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !26, size: 64)
!28 = !{}
!29 = !{!30, i1 false, i1 false, !28, !28, !32, !48, !58, !68, !78, !88}
!30 = !{!"pallas.srcLoc", i64 8, i64 1, i64 18, i64 1, !31}
!31 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/C/sort.c", directory: "", checksumkind: CSK_MD5, checksum: "0609eba1f20650945cb01458fe186fc1")
!32 = !{!"pallas.requires", !33, ptr @PALLAS_SPEC_0, !28, !28, !34}
!33 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 21, !31}
!34 = !{!35, !42, !45}
!35 = !{!36, !37}
!36 = !DILocalVariable(name: "arr", arg: 1, scope: !23, file: !10, line: 19, type: !27)
!37 = !DILocalVariable(name: "arr", arg: 1, scope: !38, file: !10, line: 9, type: !27)
!38 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !10, file: !10, line: 9, type: !39, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!39 = !DISubroutineType(types: !40)
!40 = !{!41, !27, !26, !26}
!41 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!42 = !{!43, !44}
!43 = !DILocalVariable(name: "startIdx", arg: 2, scope: !23, file: !10, line: 19, type: !26)
!44 = !DILocalVariable(name: "startIdx", arg: 2, scope: !38, file: !10, line: 9, type: !26)
!45 = !{!46, !47}
!46 = !DILocalVariable(name: "endIdx", arg: 3, scope: !23, file: !10, line: 19, type: !26)
!47 = !DILocalVariable(name: "endIdx", arg: 3, scope: !38, file: !10, line: 9, type: !26)
!48 = !{!"pallas.requires", !49, ptr @PALLAS_SPEC_1, !28, !28, !50}
!49 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 74, !31}
!50 = !{!51, !54, !56}
!51 = !{!36, !52}
!52 = !DILocalVariable(name: "arr", arg: 1, scope: !53, file: !10, line: 10, type: !27)
!53 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !10, file: !10, line: 10, type: !39, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!54 = !{!43, !55}
!55 = !DILocalVariable(name: "startIdx", arg: 2, scope: !53, file: !10, line: 10, type: !26)
!56 = !{!46, !57}
!57 = !DILocalVariable(name: "endIdx", arg: 3, scope: !53, file: !10, line: 10, type: !26)
!58 = !{!"pallas.requires", !59, ptr @PALLAS_SPEC_2, !28, !28, !60}
!59 = !{!"pallas.srcLoc", i64 11, i64 1, i64 12, i64 59, !31}
!60 = !{!61, !64, !66}
!61 = !{!36, !62}
!62 = !DILocalVariable(name: "arr", arg: 1, scope: !63, file: !10, line: 11, type: !27)
!63 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !10, file: !10, line: 11, type: !39, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!64 = !{!43, !65}
!65 = !DILocalVariable(name: "startIdx", arg: 2, scope: !63, file: !10, line: 11, type: !26)
!66 = !{!46, !67}
!67 = !DILocalVariable(name: "endIdx", arg: 3, scope: !63, file: !10, line: 11, type: !26)
!68 = !{!"pallas.ensures", !69, ptr @PALLAS_SPEC_3, !28, !28, !70}
!69 = !{!"pallas.srcLoc", i64 13, i64 1, i64 14, i64 58, !31}
!70 = !{!71, !74, !76}
!71 = !{!36, !72}
!72 = !DILocalVariable(name: "arr", arg: 1, scope: !73, file: !10, line: 13, type: !27)
!73 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !10, file: !10, line: 13, type: !39, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!74 = !{!43, !75}
!75 = !DILocalVariable(name: "startIdx", arg: 2, scope: !73, file: !10, line: 13, type: !26)
!76 = !{!46, !77}
!77 = !DILocalVariable(name: "endIdx", arg: 3, scope: !73, file: !10, line: 13, type: !26)
!78 = !{!"pallas.ensures", !79, ptr @PALLAS_SPEC_4, !28, !28, !80}
!79 = !{!"pallas.srcLoc", i64 15, i64 1, i64 15, i64 49, !31}
!80 = !{!81, !84, !86}
!81 = !{!36, !82}
!82 = !DILocalVariable(name: "arr", arg: 1, scope: !83, file: !10, line: 15, type: !27)
!83 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !10, file: !10, line: 15, type: !39, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!84 = !{!43, !85}
!85 = !DILocalVariable(name: "startIdx", arg: 2, scope: !83, file: !10, line: 15, type: !26)
!86 = !{!46, !87}
!87 = !DILocalVariable(name: "endIdx", arg: 3, scope: !83, file: !10, line: 15, type: !26)
!88 = !{!"pallas.ensures", !89, ptr @PALLAS_SPEC_5, !28, !28, !90}
!89 = !{!"pallas.srcLoc", i64 16, i64 1, i64 17, i64 55, !31}
!90 = !{!91, !94, !96}
!91 = !{!36, !92}
!92 = !DILocalVariable(name: "arr", arg: 1, scope: !93, file: !10, line: 16, type: !27)
!93 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !10, file: !10, line: 16, type: !39, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!94 = !{!43, !95}
!95 = !DILocalVariable(name: "startIdx", arg: 2, scope: !93, file: !10, line: 16, type: !26)
!96 = !{!46, !97}
!97 = !DILocalVariable(name: "endIdx", arg: 3, scope: !93, file: !10, line: 16, type: !26)
!98 = !DILocation(line: 0, scope: !23)
!99 = !DILocalVariable(name: "minIdx", scope: !23, file: !10, line: 20, type: !26)
!100 = !DILocation(line: 29, column: 29, scope: !101)
!101 = distinct !DILexicalBlock(scope: !23, file: !10, line: 29, column: 5)
!102 = !DILocalVariable(name: "idx", scope: !101, file: !10, line: 29, type: !26)
!103 = !DILocation(line: 0, scope: !101)
!104 = !DILocation(line: 29, column: 10, scope: !101)
!105 = !DILocation(line: 29, scope: !101)
!106 = !DILocation(line: 29, column: 38, scope: !107)
!107 = distinct !DILexicalBlock(scope: !101, file: !10, line: 29, column: 5)
!108 = !DILocation(line: 29, column: 5, scope: !101)
!109 = !DILocation(line: 30, column: 13, scope: !110)
!110 = distinct !DILexicalBlock(scope: !111, file: !10, line: 30, column: 13)
!111 = distinct !DILexicalBlock(scope: !107, file: !10, line: 29, column: 55)
!112 = !DILocation(line: 30, column: 24, scope: !110)
!113 = !DILocation(line: 30, column: 22, scope: !110)
!114 = !DILocation(line: 30, column: 13, scope: !111)
!115 = !DILocation(line: 32, column: 9, scope: !116)
!116 = distinct !DILexicalBlock(scope: !110, file: !10, line: 30, column: 37)
!117 = !DILocation(line: 33, column: 5, scope: !111)
!118 = !DILocation(line: 29, column: 51, scope: !107)
!119 = !DILocation(line: 29, column: 5, scope: !107)
!120 = distinct !{!120, !108, !121, !122, !123}
!121 = !DILocation(line: 33, column: 5, scope: !101)
!122 = !{!"llvm.loop.mustprogress"}
!123 = !{!"pallas.loopInvBlock", !124, !125, !141, !155, !169}
!124 = !{!"pallas.srcLoc", i64 21, i64 5, i64 28, i64 5, !31}
!125 = !{!"pallas.loopInv", !126, ptr @PALLAS_SPEC_20, !28, !28, !127}
!126 = !{!"pallas.srcLoc", i64 22, i64 5, i64 22, i64 55, !31}
!127 = !{!128, !133, !135, !137, !139}
!128 = !{!36, !129}
!129 = !DILocalVariable(name: "arr", arg: 1, scope: !130, file: !10, line: 22, type: !27)
!130 = distinct !DISubprogram(name: "PALLAS_SPEC_20", scope: !10, file: !10, line: 22, type: !131, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!131 = !DISubroutineType(types: !132)
!132 = !{!41, !27, !26, !26, !26, !26}
!133 = !{!43, !134}
!134 = !DILocalVariable(name: "startIdx", arg: 2, scope: !130, file: !10, line: 22, type: !26)
!135 = !{!46, !136}
!136 = !DILocalVariable(name: "endIdx", arg: 3, scope: !130, file: !10, line: 22, type: !26)
!137 = !{!99, !138}
!138 = !DILocalVariable(name: "minIdx", arg: 4, scope: !130, file: !10, line: 22, type: !26)
!139 = !{!102, !140}
!140 = !DILocalVariable(name: "idx", arg: 5, scope: !130, file: !10, line: 22, type: !26)
!141 = !{!"pallas.loopInv", !142, ptr @PALLAS_SPEC_21, !28, !28, !143}
!142 = !{!"pallas.srcLoc", i64 23, i64 5, i64 23, i64 54, !31}
!143 = !{!144, !147, !149, !151, !153}
!144 = !{!36, !145}
!145 = !DILocalVariable(name: "arr", arg: 1, scope: !146, file: !10, line: 23, type: !27)
!146 = distinct !DISubprogram(name: "PALLAS_SPEC_21", scope: !10, file: !10, line: 23, type: !131, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!147 = !{!43, !148}
!148 = !DILocalVariable(name: "startIdx", arg: 2, scope: !146, file: !10, line: 23, type: !26)
!149 = !{!46, !150}
!150 = !DILocalVariable(name: "endIdx", arg: 3, scope: !146, file: !10, line: 23, type: !26)
!151 = !{!99, !152}
!152 = !DILocalVariable(name: "minIdx", arg: 4, scope: !146, file: !10, line: 23, type: !26)
!153 = !{!102, !154}
!154 = !DILocalVariable(name: "idx", arg: 5, scope: !146, file: !10, line: 23, type: !26)
!155 = !{!"pallas.loopInv", !156, ptr @PALLAS_SPEC_22, !28, !28, !157}
!156 = !{!"pallas.srcLoc", i64 24, i64 5, i64 25, i64 69, !31}
!157 = !{!158, !161, !163, !165, !167}
!158 = !{!36, !159}
!159 = !DILocalVariable(name: "arr", arg: 1, scope: !160, file: !10, line: 24, type: !27)
!160 = distinct !DISubprogram(name: "PALLAS_SPEC_22", scope: !10, file: !10, line: 24, type: !131, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!161 = !{!43, !162}
!162 = !DILocalVariable(name: "startIdx", arg: 2, scope: !160, file: !10, line: 24, type: !26)
!163 = !{!46, !164}
!164 = !DILocalVariable(name: "endIdx", arg: 3, scope: !160, file: !10, line: 24, type: !26)
!165 = !{!99, !166}
!166 = !DILocalVariable(name: "minIdx", arg: 4, scope: !160, file: !10, line: 24, type: !26)
!167 = !{!102, !168}
!168 = !DILocalVariable(name: "idx", arg: 5, scope: !160, file: !10, line: 24, type: !26)
!169 = !{!"pallas.loopInv", !170, ptr @PALLAS_SPEC_23, !28, !28, !171}
!170 = !{!"pallas.srcLoc", i64 26, i64 5, i64 27, i64 60, !31}
!171 = !{!172, !175, !177, !179, !181}
!172 = !{!36, !173}
!173 = !DILocalVariable(name: "arr", arg: 1, scope: !174, file: !10, line: 26, type: !27)
!174 = distinct !DISubprogram(name: "PALLAS_SPEC_23", scope: !10, file: !10, line: 26, type: !131, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!175 = !{!43, !176}
!176 = !DILocalVariable(name: "startIdx", arg: 2, scope: !174, file: !10, line: 26, type: !26)
!177 = !{!46, !178}
!178 = !DILocalVariable(name: "endIdx", arg: 3, scope: !174, file: !10, line: 26, type: !26)
!179 = !{!99, !180}
!180 = !DILocalVariable(name: "minIdx", arg: 4, scope: !174, file: !10, line: 26, type: !26)
!181 = !{!102, !182}
!182 = !DILocalVariable(name: "idx", arg: 5, scope: !174, file: !10, line: 26, type: !26)
!183 = !DILocation(line: 34, column: 5, scope: !23)
!184 = distinct !DISubprogram(name: "swap", scope: !10, file: !10, line: 49, type: !185, scopeLine: 49, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!185 = !DISubroutineType(types: !186)
!186 = !{null, !27, !26, !26}
!187 = !{!188, i1 false, i1 false, !28, !28, !189, !202, !212, !222, !232, !242, !252, !262}
!188 = !{!"pallas.srcLoc", i64 37, i64 1, i64 48, i64 1, !31}
!189 = !{!"pallas.requires", !190, ptr @PALLAS_SPEC_6, !28, !28, !191}
!190 = !{!"pallas.srcLoc", i64 38, i64 1, i64 38, i64 21, !31}
!191 = !{!192, !196, !199}
!192 = !{!193, !194}
!193 = !DILocalVariable(name: "arr", arg: 1, scope: !184, file: !10, line: 49, type: !27)
!194 = !DILocalVariable(name: "arr", arg: 1, scope: !195, file: !10, line: 38, type: !27)
!195 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !10, file: !10, line: 38, type: !39, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!196 = !{!197, !198}
!197 = !DILocalVariable(name: "idx1", arg: 2, scope: !184, file: !10, line: 49, type: !26)
!198 = !DILocalVariable(name: "idx1", arg: 2, scope: !195, file: !10, line: 38, type: !26)
!199 = !{!200, !201}
!200 = !DILocalVariable(name: "idx2", arg: 3, scope: !184, file: !10, line: 49, type: !26)
!201 = !DILocalVariable(name: "idx2", arg: 3, scope: !195, file: !10, line: 38, type: !26)
!202 = !{!"pallas.requires", !203, ptr @PALLAS_SPEC_7, !28, !28, !204}
!203 = !{!"pallas.srcLoc", i64 39, i64 1, i64 39, i64 45, !31}
!204 = !{!205, !208, !210}
!205 = !{!193, !206}
!206 = !DILocalVariable(name: "arr", arg: 1, scope: !207, file: !10, line: 39, type: !27)
!207 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !10, file: !10, line: 39, type: !39, scopeLine: 39, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!208 = !{!197, !209}
!209 = !DILocalVariable(name: "idx1", arg: 2, scope: !207, file: !10, line: 39, type: !26)
!210 = !{!200, !211}
!211 = !DILocalVariable(name: "idx2", arg: 3, scope: !207, file: !10, line: 39, type: !26)
!212 = !{!"pallas.requires", !213, ptr @PALLAS_SPEC_8, !28, !28, !214}
!213 = !{!"pallas.srcLoc", i64 40, i64 1, i64 40, i64 45, !31}
!214 = !{!215, !218, !220}
!215 = !{!193, !216}
!216 = !DILocalVariable(name: "arr", arg: 1, scope: !217, file: !10, line: 40, type: !27)
!217 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !10, file: !10, line: 40, type: !39, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!218 = !{!197, !219}
!219 = !DILocalVariable(name: "idx1", arg: 2, scope: !217, file: !10, line: 40, type: !26)
!220 = !{!200, !221}
!221 = !DILocalVariable(name: "idx2", arg: 3, scope: !217, file: !10, line: 40, type: !26)
!222 = !{!"pallas.requires", !223, ptr @PALLAS_SPEC_9, !28, !28, !224}
!223 = !{!"pallas.srcLoc", i64 41, i64 1, i64 41, i64 22, !31}
!224 = !{!225, !228, !230}
!225 = !{!193, !226}
!226 = !DILocalVariable(name: "arr", arg: 1, scope: !227, file: !10, line: 41, type: !27)
!227 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !10, file: !10, line: 41, type: !39, scopeLine: 41, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!228 = !{!197, !229}
!229 = !DILocalVariable(name: "idx1", arg: 2, scope: !227, file: !10, line: 41, type: !26)
!230 = !{!200, !231}
!231 = !DILocalVariable(name: "idx2", arg: 3, scope: !227, file: !10, line: 41, type: !26)
!232 = !{!"pallas.requires", !233, ptr @PALLAS_SPEC_10, !28, !28, !234}
!233 = !{!"pallas.srcLoc", i64 42, i64 1, i64 43, i64 41, !31}
!234 = !{!235, !238, !240}
!235 = !{!193, !236}
!236 = !DILocalVariable(name: "arr", arg: 1, scope: !237, file: !10, line: 42, type: !27)
!237 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !10, file: !10, line: 42, type: !39, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!238 = !{!197, !239}
!239 = !DILocalVariable(name: "idx1", arg: 2, scope: !237, file: !10, line: 42, type: !26)
!240 = !{!200, !241}
!241 = !DILocalVariable(name: "idx2", arg: 3, scope: !237, file: !10, line: 42, type: !26)
!242 = !{!"pallas.ensures", !243, ptr @PALLAS_SPEC_11, !28, !28, !244}
!243 = !{!"pallas.srcLoc", i64 44, i64 1, i64 45, i64 41, !31}
!244 = !{!245, !248, !250}
!245 = !{!193, !246}
!246 = !DILocalVariable(name: "arr", arg: 1, scope: !247, file: !10, line: 44, type: !27)
!247 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !10, file: !10, line: 44, type: !39, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!248 = !{!197, !249}
!249 = !DILocalVariable(name: "idx1", arg: 2, scope: !247, file: !10, line: 44, type: !26)
!250 = !{!200, !251}
!251 = !DILocalVariable(name: "idx2", arg: 3, scope: !247, file: !10, line: 44, type: !26)
!252 = !{!"pallas.ensures", !253, ptr @PALLAS_SPEC_12, !28, !28, !254}
!253 = !{!"pallas.srcLoc", i64 46, i64 1, i64 46, i64 42, !31}
!254 = !{!255, !258, !260}
!255 = !{!193, !256}
!256 = !DILocalVariable(name: "arr", arg: 1, scope: !257, file: !10, line: 46, type: !27)
!257 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !10, file: !10, line: 46, type: !39, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!258 = !{!197, !259}
!259 = !DILocalVariable(name: "idx1", arg: 2, scope: !257, file: !10, line: 46, type: !26)
!260 = !{!200, !261}
!261 = !DILocalVariable(name: "idx2", arg: 3, scope: !257, file: !10, line: 46, type: !26)
!262 = !{!"pallas.ensures", !263, ptr @PALLAS_SPEC_13, !28, !28, !264}
!263 = !{!"pallas.srcLoc", i64 47, i64 1, i64 47, i64 42, !31}
!264 = !{!265, !268, !270}
!265 = !{!193, !266}
!266 = !DILocalVariable(name: "arr", arg: 1, scope: !267, file: !10, line: 47, type: !27)
!267 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !10, file: !10, line: 47, type: !39, scopeLine: 47, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!268 = !{!197, !269}
!269 = !DILocalVariable(name: "idx1", arg: 2, scope: !267, file: !10, line: 47, type: !26)
!270 = !{!200, !271}
!271 = !DILocalVariable(name: "idx2", arg: 3, scope: !267, file: !10, line: 47, type: !26)
!272 = !DILocation(line: 0, scope: !184)
!273 = !DILocation(line: 50, column: 15, scope: !184)
!274 = !DILocalVariable(name: "tmp", scope: !184, file: !10, line: 50, type: !26)
!275 = !DILocation(line: 51, column: 17, scope: !184)
!276 = !DILocation(line: 51, column: 5, scope: !184)
!277 = !DILocation(line: 51, column: 15, scope: !184)
!278 = !DILocation(line: 52, column: 5, scope: !184)
!279 = !DILocation(line: 52, column: 15, scope: !184)
!280 = !DILocation(line: 53, column: 1, scope: !184)
!281 = distinct !DISubprogram(name: "selectSort", scope: !10, file: !10, line: 64, type: !282, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!282 = !DISubroutineType(types: !283)
!283 = !{null, !27, !26}
!284 = !{!285, i1 false, i1 false, !28, !28, !286, !298, !306, !314, !322, !330}
!285 = !{!"pallas.srcLoc", i64 55, i64 1, i64 63, i64 1, !31}
!286 = !{!"pallas.requires", !287, ptr @PALLAS_SPEC_14, !28, !28, !288}
!287 = !{!"pallas.srcLoc", i64 56, i64 1, i64 56, i64 21, !31}
!288 = !{!289, !295}
!289 = !{!290, !291}
!290 = !DILocalVariable(name: "arr", arg: 1, scope: !281, file: !10, line: 64, type: !27)
!291 = !DILocalVariable(name: "arr", arg: 1, scope: !292, file: !10, line: 56, type: !27)
!292 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !10, file: !10, line: 56, type: !293, scopeLine: 56, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!293 = !DISubroutineType(types: !294)
!294 = !{!41, !27, !26}
!295 = !{!296, !297}
!296 = !DILocalVariable(name: "n", arg: 2, scope: !281, file: !10, line: 64, type: !26)
!297 = !DILocalVariable(name: "n", arg: 2, scope: !292, file: !10, line: 56, type: !26)
!298 = !{!"pallas.requires", !299, ptr @PALLAS_SPEC_15, !28, !28, !300}
!299 = !{!"pallas.srcLoc", i64 57, i64 1, i64 57, i64 15, !31}
!300 = !{!301, !304}
!301 = !{!290, !302}
!302 = !DILocalVariable(name: "arr", arg: 1, scope: !303, file: !10, line: 57, type: !27)
!303 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !10, file: !10, line: 57, type: !293, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!304 = !{!296, !305}
!305 = !DILocalVariable(name: "n", arg: 2, scope: !303, file: !10, line: 57, type: !26)
!306 = !{!"pallas.requires", !307, ptr @PALLAS_SPEC_16, !28, !28, !308}
!307 = !{!"pallas.srcLoc", i64 58, i64 1, i64 58, i64 31, !31}
!308 = !{!309, !312}
!309 = !{!290, !310}
!310 = !DILocalVariable(name: "arr", arg: 1, scope: !311, file: !10, line: 58, type: !27)
!311 = distinct !DISubprogram(name: "PALLAS_SPEC_16", scope: !10, file: !10, line: 58, type: !293, scopeLine: 58, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!312 = !{!296, !313}
!313 = !DILocalVariable(name: "n", arg: 2, scope: !311, file: !10, line: 58, type: !26)
!314 = !{!"pallas.requires", !315, ptr @PALLAS_SPEC_17, !28, !28, !316}
!315 = !{!"pallas.srcLoc", i64 59, i64 1, i64 59, i64 81, !31}
!316 = !{!317, !320}
!317 = !{!290, !318}
!318 = !DILocalVariable(name: "arr", arg: 1, scope: !319, file: !10, line: 59, type: !27)
!319 = distinct !DISubprogram(name: "PALLAS_SPEC_17", scope: !10, file: !10, line: 59, type: !293, scopeLine: 59, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!320 = !{!296, !321}
!321 = !DILocalVariable(name: "n", arg: 2, scope: !319, file: !10, line: 59, type: !26)
!322 = !{!"pallas.ensures", !323, ptr @PALLAS_SPEC_18, !28, !28, !324}
!323 = !{!"pallas.srcLoc", i64 60, i64 1, i64 60, i64 81, !31}
!324 = !{!325, !328}
!325 = !{!290, !326}
!326 = !DILocalVariable(name: "arr", arg: 1, scope: !327, file: !10, line: 60, type: !27)
!327 = distinct !DISubprogram(name: "PALLAS_SPEC_18", scope: !10, file: !10, line: 60, type: !293, scopeLine: 60, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!328 = !{!296, !329}
!329 = !DILocalVariable(name: "n", arg: 2, scope: !327, file: !10, line: 60, type: !26)
!330 = !{!"pallas.ensures", !331, ptr @PALLAS_SPEC_19, !28, !28, !332}
!331 = !{!"pallas.srcLoc", i64 61, i64 1, i64 62, i64 56, !31}
!332 = !{!333, !336}
!333 = !{!290, !334}
!334 = !DILocalVariable(name: "arr", arg: 1, scope: !335, file: !10, line: 61, type: !27)
!335 = distinct !DISubprogram(name: "PALLAS_SPEC_19", scope: !10, file: !10, line: 61, type: !293, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!336 = !{!296, !337}
!337 = !DILocalVariable(name: "n", arg: 2, scope: !335, file: !10, line: 61, type: !26)
!338 = !DILocation(line: 0, scope: !281)
!339 = !DILocation(line: 66, column: 17, scope: !281)
!340 = !DILocalVariable(name: "first", scope: !281, file: !10, line: 66, type: !26)
!341 = !DILocalVariable(name: "idx", scope: !342, file: !10, line: 76, type: !26)
!342 = distinct !DILexicalBlock(scope: !281, file: !10, line: 76, column: 5)
!343 = !DILocation(line: 0, scope: !342)
!344 = !DILocation(line: 76, column: 10, scope: !342)
!345 = !DILocation(line: 76, scope: !342)
!346 = !DILocation(line: 76, column: 30, scope: !347)
!347 = distinct !DILexicalBlock(scope: !342, file: !10, line: 76, column: 5)
!348 = !DILocation(line: 76, column: 27, scope: !347)
!349 = !DILocation(line: 76, column: 5, scope: !342)
!350 = !DILocation(line: 77, column: 22, scope: !351)
!351 = distinct !DILexicalBlock(scope: !347, file: !10, line: 76, column: 41)
!352 = !DILocalVariable(name: "minIdx", scope: !351, file: !10, line: 77, type: !26)
!353 = !DILocation(line: 0, scope: !351)
!354 = !DILocation(line: 78, column: 20, scope: !355)
!355 = distinct !DILexicalBlock(scope: !351, file: !10, line: 78, column: 13)
!356 = !DILocation(line: 78, column: 13, scope: !351)
!357 = !DILocation(line: 79, column: 13, scope: !358)
!358 = distinct !DILexicalBlock(scope: !355, file: !10, line: 78, column: 28)
!359 = !DILocation(line: 80, column: 9, scope: !358)
!360 = !DILocation(line: 81, column: 5, scope: !351)
!361 = !DILocation(line: 76, column: 34, scope: !347)
!362 = !DILocation(line: 76, column: 5, scope: !347)
!363 = distinct !{!363, !349, !364, !122, !365}
!364 = !DILocation(line: 81, column: 5, scope: !342)
!365 = !{!"pallas.loopInvBlock", !366, !367, !381, !393, !405}
!366 = !{!"pallas.srcLoc", i64 67, i64 5, i64 75, i64 5, !31}
!367 = !{!"pallas.loopInv", !368, ptr @PALLAS_SPEC_24, !28, !28, !369}
!368 = !{!"pallas.srcLoc", i64 68, i64 5, i64 68, i64 39, !31}
!369 = !{!370, !375, !377, !379}
!370 = !{!290, !371}
!371 = !DILocalVariable(name: "arr", arg: 1, scope: !372, file: !10, line: 68, type: !27)
!372 = distinct !DISubprogram(name: "PALLAS_SPEC_24", scope: !10, file: !10, line: 68, type: !373, scopeLine: 68, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!373 = !DISubroutineType(types: !374)
!374 = !{!41, !27, !26, !26, !26}
!375 = !{!296, !376}
!376 = !DILocalVariable(name: "n", arg: 2, scope: !372, file: !10, line: 68, type: !26)
!377 = !{!340, !378}
!378 = !DILocalVariable(name: "first", arg: 3, scope: !372, file: !10, line: 68, type: !26)
!379 = !{!341, !380}
!380 = !DILocalVariable(name: "idx", arg: 4, scope: !372, file: !10, line: 68, type: !26)
!381 = !{!"pallas.loopInv", !382, ptr @PALLAS_SPEC_25, !28, !28, !383}
!382 = !{!"pallas.srcLoc", i64 69, i64 5, i64 69, i64 91, !31}
!383 = !{!384, !387, !389, !391}
!384 = !{!290, !385}
!385 = !DILocalVariable(name: "arr", arg: 1, scope: !386, file: !10, line: 69, type: !27)
!386 = distinct !DISubprogram(name: "PALLAS_SPEC_25", scope: !10, file: !10, line: 69, type: !373, scopeLine: 69, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!387 = !{!296, !388}
!388 = !DILocalVariable(name: "n", arg: 2, scope: !386, file: !10, line: 69, type: !26)
!389 = !{!340, !390}
!390 = !DILocalVariable(name: "first", arg: 3, scope: !386, file: !10, line: 69, type: !26)
!391 = !{!341, !392}
!392 = !DILocalVariable(name: "idx", arg: 4, scope: !386, file: !10, line: 69, type: !26)
!393 = !{!"pallas.loopInv", !394, ptr @PALLAS_SPEC_26, !28, !28, !395}
!394 = !{!"pallas.srcLoc", i64 70, i64 5, i64 72, i64 65, !31}
!395 = !{!396, !399, !401, !403}
!396 = !{!290, !397}
!397 = !DILocalVariable(name: "arr", arg: 1, scope: !398, file: !10, line: 70, type: !27)
!398 = distinct !DISubprogram(name: "PALLAS_SPEC_26", scope: !10, file: !10, line: 70, type: !373, scopeLine: 70, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!399 = !{!296, !400}
!400 = !DILocalVariable(name: "n", arg: 2, scope: !398, file: !10, line: 70, type: !26)
!401 = !{!340, !402}
!402 = !DILocalVariable(name: "first", arg: 3, scope: !398, file: !10, line: 70, type: !26)
!403 = !{!341, !404}
!404 = !DILocalVariable(name: "idx", arg: 4, scope: !398, file: !10, line: 70, type: !26)
!405 = !{!"pallas.loopInv", !406, ptr @PALLAS_SPEC_27, !28, !28, !407}
!406 = !{!"pallas.srcLoc", i64 73, i64 5, i64 74, i64 65, !31}
!407 = !{!408, !411, !413, !415}
!408 = !{!290, !409}
!409 = !DILocalVariable(name: "arr", arg: 1, scope: !410, file: !10, line: 73, type: !27)
!410 = distinct !DISubprogram(name: "PALLAS_SPEC_27", scope: !10, file: !10, line: 73, type: !373, scopeLine: 73, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!411 = !{!296, !412}
!412 = !DILocalVariable(name: "n", arg: 2, scope: !410, file: !10, line: 73, type: !26)
!413 = !{!340, !414}
!414 = !DILocalVariable(name: "first", arg: 3, scope: !410, file: !10, line: 73, type: !26)
!415 = !{!341, !416}
!416 = !DILocalVariable(name: "idx", arg: 4, scope: !410, file: !10, line: 73, type: !26)
!417 = !DILocation(line: 82, column: 1, scope: !281)
!418 = !{!""}
!419 = !DILocation(line: 0, scope: !38)
!420 = !DILocation(line: 9, column: 14, scope: !38)
!421 = !DILocation(line: 0, scope: !53)
!422 = !DILocation(line: 10, column: 12, scope: !53)
!423 = !DILocation(line: 10, column: 24, scope: !53)
!424 = !DILocation(line: 10, column: 36, scope: !53)
!425 = !DILocation(line: 10, column: 45, scope: !53)
!426 = !DILocation(line: 10, column: 48, scope: !53)
!427 = !DILocation(line: 10, column: 58, scope: !53)
!428 = !DILocation(line: 10, column: 55, scope: !53)
!429 = !DILocation(line: 0, scope: !63)
!430 = !DILocation(line: 11, column: 19, scope: !63)
!431 = !DILocation(line: 12, column: 30, scope: !63)
!432 = !DILocation(line: 12, column: 26, scope: !63)
!433 = !DILocation(line: 12, column: 44, scope: !63)
!434 = !DILocation(line: 12, column: 19, scope: !63)
!435 = !DILocation(line: 11, column: 10, scope: !63)
!436 = !DILocation(line: 0, scope: !73)
!437 = !DILocation(line: 13, column: 18, scope: !73)
!438 = !DILocation(line: 14, column: 29, scope: !73)
!439 = !DILocation(line: 14, column: 25, scope: !73)
!440 = !DILocation(line: 14, column: 43, scope: !73)
!441 = !DILocation(line: 14, column: 18, scope: !73)
!442 = !DILocation(line: 13, column: 9, scope: !73)
!443 = !DILocation(line: 0, scope: !83)
!444 = !DILocation(line: 15, column: 9, scope: !83)
!445 = !DILocation(line: 0, scope: !93)
!446 = !DILocation(line: 16, column: 17, scope: !93)
!447 = !DILocation(line: 17, column: 21, scope: !93)
!448 = !DILocation(line: 17, column: 17, scope: !93)
!449 = !DILocation(line: 17, column: 42, scope: !93)
!450 = !DILocation(line: 17, column: 38, scope: !93)
!451 = !DILocation(line: 17, column: 35, scope: !93)
!452 = !DILocation(line: 16, column: 9, scope: !93)
!453 = !DILocation(line: 0, scope: !195)
!454 = !DILocation(line: 38, column: 14, scope: !195)
!455 = !DILocation(line: 0, scope: !207)
!456 = !DILocation(line: 39, column: 10, scope: !207)
!457 = !DILocation(line: 0, scope: !217)
!458 = !DILocation(line: 40, column: 10, scope: !217)
!459 = !DILocation(line: 0, scope: !227)
!460 = !DILocation(line: 41, column: 15, scope: !227)
!461 = !DILocation(line: 0, scope: !237)
!462 = !DILocation(line: 42, column: 22, scope: !237)
!463 = !DILocation(line: 42, column: 33, scope: !237)
!464 = !DILocation(line: 42, column: 15, scope: !237)
!465 = !DILocation(line: 43, column: 22, scope: !237)
!466 = !DILocation(line: 43, column: 33, scope: !237)
!467 = !DILocation(line: 43, column: 15, scope: !237)
!468 = !DILocation(line: 42, column: 10, scope: !237)
!469 = !DILocation(line: 0, scope: !247)
!470 = !DILocation(line: 44, column: 22, scope: !247)
!471 = !DILocation(line: 44, column: 33, scope: !247)
!472 = !DILocation(line: 44, column: 15, scope: !247)
!473 = !DILocation(line: 45, column: 22, scope: !247)
!474 = !DILocation(line: 45, column: 33, scope: !247)
!475 = !DILocation(line: 45, column: 15, scope: !247)
!476 = !DILocation(line: 44, column: 10, scope: !247)
!477 = !DILocation(line: 0, scope: !257)
!478 = !DILocation(line: 46, column: 9, scope: !257)
!479 = !DILocation(line: 46, column: 32, scope: !257)
!480 = !DILocation(line: 46, column: 22, scope: !257)
!481 = !DILocation(line: 46, column: 19, scope: !257)
!482 = !DILocation(line: 0, scope: !267)
!483 = !DILocation(line: 47, column: 9, scope: !267)
!484 = !DILocation(line: 47, column: 32, scope: !267)
!485 = !DILocation(line: 47, column: 22, scope: !267)
!486 = !DILocation(line: 47, column: 19, scope: !267)
!487 = !DILocation(line: 0, scope: !292)
!488 = !DILocation(line: 56, column: 14, scope: !292)
!489 = !DILocation(line: 0, scope: !303)
!490 = !DILocation(line: 57, column: 12, scope: !303)
!491 = !DILocation(line: 0, scope: !311)
!492 = !DILocation(line: 58, column: 10, scope: !311)
!493 = !DILocation(line: 58, column: 30, scope: !311)
!494 = !DILocation(line: 58, column: 27, scope: !311)
!495 = !DILocation(line: 0, scope: !319)
!496 = !DILocation(line: 59, column: 19, scope: !319)
!497 = !DILocation(line: 59, column: 59, scope: !319)
!498 = !DILocation(line: 59, column: 55, scope: !319)
!499 = !DILocation(line: 59, column: 73, scope: !319)
!500 = !DILocation(line: 59, column: 48, scope: !319)
!501 = !DILocation(line: 59, column: 10, scope: !319)
!502 = !DILocation(line: 0, scope: !327)
!503 = !DILocation(line: 60, column: 19, scope: !327)
!504 = !DILocation(line: 60, column: 59, scope: !327)
!505 = !DILocation(line: 60, column: 55, scope: !327)
!506 = !DILocation(line: 60, column: 73, scope: !327)
!507 = !DILocation(line: 60, column: 48, scope: !327)
!508 = !DILocation(line: 60, column: 10, scope: !327)
!509 = !DILocation(line: 0, scope: !335)
!510 = !DILocation(line: 61, column: 24, scope: !335)
!511 = !DILocation(line: 61, column: 63, scope: !335)
!512 = !DILocation(line: 61, column: 75, scope: !335)
!513 = !DILocation(line: 61, column: 19, scope: !335)
!514 = !DILocation(line: 62, column: 23, scope: !335)
!515 = !DILocation(line: 62, column: 19, scope: !335)
!516 = !DILocation(line: 62, column: 43, scope: !335)
!517 = !DILocation(line: 62, column: 39, scope: !335)
!518 = !DILocation(line: 62, column: 36, scope: !335)
!519 = !DILocation(line: 61, column: 10, scope: !335)
!520 = !DILocation(line: 0, scope: !146)
!521 = !DILocation(line: 23, column: 20, scope: !146)
!522 = !DILocation(line: 0, scope: !130)
!523 = !DILocation(line: 22, column: 20, scope: !130)
!524 = !DILocation(line: 0, scope: !174)
!525 = !DILocation(line: 26, column: 28, scope: !174)
!526 = !DILocation(line: 27, column: 28, scope: !174)
!527 = !DILocation(line: 27, column: 47, scope: !174)
!528 = !DILocation(line: 27, column: 43, scope: !174)
!529 = !DILocation(line: 27, column: 40, scope: !174)
!530 = !DILocation(line: 26, column: 20, scope: !174)
!531 = !DILocation(line: 0, scope: !372)
!532 = !DILocation(line: 68, column: 22, scope: !372)
!533 = !DILocation(line: 68, column: 29, scope: !372)
!534 = !DILocation(line: 68, column: 36, scope: !372)
!535 = !DILocation(line: 0, scope: !386)
!536 = !DILocation(line: 69, column: 29, scope: !386)
!537 = !DILocation(line: 69, column: 69, scope: !386)
!538 = !DILocation(line: 69, column: 65, scope: !386)
!539 = !DILocation(line: 69, column: 83, scope: !386)
!540 = !DILocation(line: 69, column: 58, scope: !386)
!541 = !DILocation(line: 69, column: 20, scope: !386)
!542 = !DILocation(line: 0, scope: !398)
!543 = !DILocation(line: 70, column: 33, scope: !398)
!544 = !DILocation(line: 71, column: 33, scope: !398)
!545 = !DILocation(line: 70, column: 28, scope: !398)
!546 = !DILocation(line: 72, column: 32, scope: !398)
!547 = !DILocation(line: 72, column: 28, scope: !398)
!548 = !DILocation(line: 72, column: 52, scope: !398)
!549 = !DILocation(line: 72, column: 48, scope: !398)
!550 = !DILocation(line: 72, column: 45, scope: !398)
!551 = !DILocation(line: 70, column: 20, scope: !398)
!552 = !DILocation(line: 0, scope: !160)
!553 = !DILocation(line: 24, column: 29, scope: !160)
!554 = !DILocation(line: 25, column: 40, scope: !160)
!555 = !DILocation(line: 25, column: 36, scope: !160)
!556 = !DILocation(line: 25, column: 54, scope: !160)
!557 = !DILocation(line: 25, column: 29, scope: !160)
!558 = !DILocation(line: 24, column: 20, scope: !160)
!559 = !DILocation(line: 0, scope: !410)
!560 = !DILocation(line: 73, column: 33, scope: !410)
!561 = !DILocation(line: 73, column: 72, scope: !410)
!562 = !DILocation(line: 73, column: 84, scope: !410)
!563 = !DILocation(line: 73, column: 28, scope: !410)
!564 = !DILocation(line: 74, column: 32, scope: !410)
!565 = !DILocation(line: 74, column: 28, scope: !410)
!566 = !DILocation(line: 74, column: 52, scope: !410)
!567 = !DILocation(line: 74, column: 48, scope: !410)
!568 = !DILocation(line: 74, column: 45, scope: !410)
!569 = !DILocation(line: 73, column: 20, scope: !410)
!570 = !{!"pallas.result"}
!571 = !{!"pallas.sepConj"}
!572 = !{!"pallas.old"}
!573 = !{!"pallas.ptrLength"}
!574 = !{!"pallas.forallSep"}
!575 = !{!"pallas.perm"}
!576 = !{!"pallas.fracOf"}
!577 = !{!"pallas.forall"}
!578 = !{!"pallas.scAnd"}
!579 = !{!"pallas.boundVar"}
