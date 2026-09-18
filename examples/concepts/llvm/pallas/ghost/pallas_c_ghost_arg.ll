; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/ghost/pallas_c_ghost_arg.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [20 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_15, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_16, ptr @PALLAS_SPEC_17, ptr @PALLAS_SPEC_18, ptr @PALLAS_SPEC_19], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0
@.str.1 = private unnamed_addr constant [2 x i8] c"j\00", align 1, !dbg !7

; Function Attrs: noinline nounwind uwtable
define dso_local void @clear_arr(ptr noundef %0, i32 noundef %1) #0 !dbg !23 !pallas.fcontract !29 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !49, metadata !DIExpression()), !dbg !120
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !52, metadata !DIExpression()), !dbg !121
  call void @llvm.dbg.declare(metadata ptr %5, metadata !122, metadata !DIExpression()), !dbg !124
  store i32 0, ptr %5, align 4, !dbg !124, !pallas.stmntBlock !125
  br label %6, !dbg !145

6:                                                ; preds = %15, %2
  %7 = load i32, ptr %5, align 4, !dbg !146
  %8 = load i32, ptr %4, align 4, !dbg !148
  %9 = icmp slt i32 %7, %8, !dbg !149
  br i1 %9, label %10, label %18, !dbg !150

10:                                               ; preds = %6
  %11 = load ptr, ptr %3, align 8, !dbg !151, !pallas.stmntBlock !153
  %12 = load i32, ptr %5, align 4, !dbg !171
  %13 = sext i32 %12 to i64, !dbg !151
  %14 = getelementptr inbounds i32, ptr %11, i64 %13, !dbg !151
  store i32 0, ptr %14, align 4, !dbg !172
  br label %15, !dbg !173

15:                                               ; preds = %10
  %16 = load i32, ptr %5, align 4, !dbg !174
  %17 = add nsw i32 %16, 1, !dbg !174
  store i32 %17, ptr %5, align 4, !dbg !174
  br label %6, !dbg !175, !llvm.loop !176

18:                                               ; preds = %6
  ret void, !dbg !261
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @foo(ptr noundef %0, i32 noundef %1) #0 !dbg !262 !pallas.fcontract !263 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !272, metadata !DIExpression()), !dbg !317
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !278, metadata !DIExpression()), !dbg !318
  %5 = load ptr, ptr %3, align 8, !dbg !319
  %6 = load i32, ptr %4, align 4, !dbg !320
  call void @clear_arr(ptr noundef %5, i32 noundef %6), !dbg !321, !pallas.givenBindings !322, !pallas.yieldsBindings !337
  ret void, !dbg !341, !pallas.stmntBlock !342
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !43 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !50, metadata !DIExpression()), !dbg !356
  call void @llvm.dbg.value(metadata i32 %1, metadata !53, metadata !DIExpression()), !dbg !356
  call void @llvm.dbg.value(metadata i32 %2, metadata !42, metadata !DIExpression()), !dbg !356
  %4 = icmp sge i32 %1, 0, !dbg !357
  ret i1 %4, !dbg !356
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !59 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !62, metadata !DIExpression()), !dbg !358
  call void @llvm.dbg.value(metadata i32 %1, metadata !64, metadata !DIExpression()), !dbg !358
  call void @llvm.dbg.value(metadata i32 %2, metadata !58, metadata !DIExpression()), !dbg !358
  %4 = icmp ne ptr %0, null, !dbg !359
  br i1 %4, label %5, label %9, !dbg !360

5:                                                ; preds = %3
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !361
  %7 = sext i32 %1 to i64, !dbg !362
  %8 = icmp sge i64 %6, %7, !dbg !363
  br label %9

9:                                                ; preds = %5, %3
  %10 = phi i1 [ false, %3 ], [ %8, %5 ], !dbg !358
  ret i1 %10, !dbg !358
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !70 !pallas.exprWrapper !355 {
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !73, metadata !DIExpression()), !dbg !364
  call void @llvm.dbg.value(metadata i32 %1, metadata !75, metadata !DIExpression()), !dbg !364
  call void @llvm.dbg.value(metadata i32 %2, metadata !69, metadata !DIExpression()), !dbg !364
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !365
  %6 = icmp sle i32 0, %5, !dbg !365
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !365
  %8 = icmp slt i32 %7, %1, !dbg !365
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !365
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !366
  %11 = sext i32 %10 to i64, !dbg !367
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !367
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 1), !dbg !368
  %13 = call i1 @pallas.perm(ptr noundef %12, ptr noundef byval(%pallas.fracT) %4), !dbg !369
  %14 = call i1 @pallas.forallSep(i1 %9, i1 %13), !dbg !370
  ret i1 %14, !dbg !364
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1, i32 noundef %2, i1 noundef zeroext %3) #0 !dbg !81 !pallas.exprWrapper !355 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !89, metadata !DIExpression()), !dbg !371
  call void @llvm.dbg.value(metadata i32 %1, metadata !91, metadata !DIExpression()), !dbg !371
  call void @llvm.dbg.value(metadata i32 %2, metadata !80, metadata !DIExpression()), !dbg !371
  %6 = zext i1 %3 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !86, metadata !DIExpression()), !dbg !371
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !372
  %8 = icmp sle i32 0, %7, !dbg !372
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !372
  %10 = icmp slt i32 %9, %1, !dbg !372
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !372
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !373
  %13 = sext i32 %12 to i64, !dbg !374
  %14 = getelementptr inbounds i32, ptr %0, i64 %13, !dbg !374
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 1), !dbg !375
  %15 = call i1 @pallas.perm(ptr noundef %14, ptr noundef byval(%pallas.fracT) %5), !dbg !376
  %16 = call i1 @pallas.forallSep(i1 %11, i1 %15), !dbg !377
  ret i1 %16, !dbg !371
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1, i32 noundef %2, i1 noundef zeroext %3) #0 !dbg !97 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !103, metadata !DIExpression()), !dbg !378
  call void @llvm.dbg.value(metadata i32 %1, metadata !105, metadata !DIExpression()), !dbg !378
  call void @llvm.dbg.value(metadata i32 %2, metadata !96, metadata !DIExpression()), !dbg !378
  %5 = zext i1 %3 to i8
  call void @llvm.dbg.value(metadata i8 %5, metadata !100, metadata !DIExpression()), !dbg !378
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !379
  %7 = icmp sle i32 0, %6, !dbg !379
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !379
  %9 = icmp slt i32 %8, %1, !dbg !379
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !379
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !380
  %12 = sext i32 %11 to i64, !dbg !381
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !381
  %14 = load i32, ptr %13, align 4, !dbg !381
  %15 = icmp eq i32 %14, 0, !dbg !382
  %16 = call i1 @pallas.forall(i1 %10, i1 %15), !dbg !383
  ret i1 %16, !dbg !378
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1, i32 noundef %2, i1 noundef zeroext %3) #0 !dbg !111 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !117, metadata !DIExpression()), !dbg !384
  call void @llvm.dbg.value(metadata i32 %1, metadata !119, metadata !DIExpression()), !dbg !384
  call void @llvm.dbg.value(metadata i32 %2, metadata !110, metadata !DIExpression()), !dbg !384
  %5 = zext i1 %3 to i8
  call void @llvm.dbg.value(metadata i8 %5, metadata !114, metadata !DIExpression()), !dbg !384
  %6 = trunc i8 %5 to i1, !dbg !385
  %7 = zext i1 %6 to i32, !dbg !385
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !386
  %9 = icmp sle i32 0, %8, !dbg !386
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !386
  %11 = icmp slt i32 %10, %1, !dbg !386
  %12 = call i1 @pallas.scAnd(i1 %9, i1 %11), !dbg !386
  %13 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !387
  %14 = sext i32 %13 to i64, !dbg !388
  %15 = getelementptr inbounds i32, ptr %0, i64 %14, !dbg !388
  %16 = load i32, ptr %15, align 4, !dbg !388
  %17 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %16), !dbg !389
  %18 = icmp sgt i32 %17, %2, !dbg !390
  %19 = call i1 @pallas.forall(i1 %12, i1 %18), !dbg !391
  %20 = zext i1 %19 to i32, !dbg !391
  %21 = icmp eq i32 %7, %20, !dbg !392
  ret i1 %21, !dbg !384
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1) #0 !dbg !274 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !273, metadata !DIExpression()), !dbg !393
  call void @llvm.dbg.value(metadata i32 %1, metadata !279, metadata !DIExpression()), !dbg !393
  %3 = icmp sgt i32 %1, 0, !dbg !394
  ret i1 %3, !dbg !393
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1) #0 !dbg !285 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !284, metadata !DIExpression()), !dbg !395
  call void @llvm.dbg.value(metadata i32 %1, metadata !287, metadata !DIExpression()), !dbg !395
  %3 = icmp ne ptr %0, null, !dbg !396
  br i1 %3, label %4, label %8, !dbg !397

4:                                                ; preds = %2
  %5 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !398
  %6 = sext i32 %1 to i64, !dbg !399
  %7 = icmp sge i64 %5, %6, !dbg !400
  br label %8

8:                                                ; preds = %4, %2
  %9 = phi i1 [ false, %2 ], [ %7, %4 ], !dbg !395
  ret i1 %9, !dbg !395
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0, i32 noundef %1) #0 !dbg !293 !pallas.exprWrapper !355 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !292, metadata !DIExpression()), !dbg !401
  call void @llvm.dbg.value(metadata i32 %1, metadata !295, metadata !DIExpression()), !dbg !401
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !402
  %5 = icmp sle i32 0, %4, !dbg !402
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !402
  %7 = icmp slt i32 %6, %1, !dbg !402
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !402
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !403
  %10 = sext i32 %9 to i64, !dbg !404
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !404
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !405
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !406
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !407
  ret i1 %13, !dbg !401
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0, i32 noundef %1) #0 !dbg !301 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !300, metadata !DIExpression()), !dbg !408
  call void @llvm.dbg.value(metadata i32 %1, metadata !303, metadata !DIExpression()), !dbg !408
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !409
  %4 = icmp sle i32 0, %3, !dbg !409
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !409
  %6 = icmp slt i32 %5, %1, !dbg !409
  %7 = call i1 @pallas.scAnd(i1 %4, i1 %6), !dbg !409
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !410
  %9 = sext i32 %8 to i64, !dbg !411
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !411
  %11 = load i32, ptr %10, align 4, !dbg !411
  %12 = add nsw i32 %1, 1, !dbg !412
  %13 = icmp eq i32 %11, %12, !dbg !413
  %14 = call i1 @pallas.forall(i1 %7, i1 %13), !dbg !414
  ret i1 %14, !dbg !408
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0, i32 noundef %1, i1 noundef zeroext %2) #0 !dbg !309 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !314, metadata !DIExpression()), !dbg !415
  call void @llvm.dbg.value(metadata i32 %1, metadata !316, metadata !DIExpression()), !dbg !415
  %4 = zext i1 %2 to i8
  call void @llvm.dbg.value(metadata i8 %4, metadata !308, metadata !DIExpression()), !dbg !415
  %5 = trunc i8 %4 to i1, !dbg !416
  %6 = zext i1 %5 to i32, !dbg !416
  %7 = icmp eq i32 %6, 1, !dbg !417
  ret i1 %7, !dbg !415
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !250 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !256, metadata !DIExpression()), !dbg !418
  call void @llvm.dbg.value(metadata i32 %1, metadata !258, metadata !DIExpression()), !dbg !418
  call void @llvm.dbg.value(metadata i32 %2, metadata !260, metadata !DIExpression()), !dbg !418
  call void @llvm.dbg.value(metadata i32 %3, metadata !249, metadata !DIExpression()), !dbg !418
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !253, metadata !DIExpression()), !dbg !418
  %7 = trunc i8 %6 to i1, !dbg !419
  %8 = zext i1 %7 to i32, !dbg !419
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !420
  %10 = icmp sle i32 0, %9, !dbg !420
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !420
  %12 = icmp slt i32 %11, %2, !dbg !420
  %13 = call i1 @pallas.scAnd(i1 %10, i1 %12), !dbg !420
  %14 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !421
  %15 = sext i32 %14 to i64, !dbg !422
  %16 = getelementptr inbounds i32, ptr %0, i64 %15, !dbg !422
  %17 = load i32, ptr %16, align 4, !dbg !422
  %18 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %17), !dbg !423
  %19 = icmp sgt i32 %18, %3, !dbg !424
  %20 = call i1 @pallas.forall(i1 %13, i1 %19), !dbg !425
  %21 = zext i1 %20 to i32, !dbg !425
  %22 = icmp eq i32 %8, %21, !dbg !426
  ret i1 %22, !dbg !418
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !186 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !192, metadata !DIExpression()), !dbg !427
  call void @llvm.dbg.value(metadata i32 %1, metadata !194, metadata !DIExpression()), !dbg !427
  call void @llvm.dbg.value(metadata i32 %2, metadata !196, metadata !DIExpression()), !dbg !427
  call void @llvm.dbg.value(metadata i32 %3, metadata !185, metadata !DIExpression()), !dbg !427
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !189, metadata !DIExpression()), !dbg !427
  %7 = icmp sle i32 0, %2, !dbg !428
  br i1 %7, label %8, label %10, !dbg !429

8:                                                ; preds = %5
  %9 = icmp sle i32 %2, %1, !dbg !430
  br label %10

10:                                               ; preds = %8, %5
  %11 = phi i1 [ false, %5 ], [ %9, %8 ], !dbg !427
  ret i1 %11, !dbg !427
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !202 !pallas.exprWrapper !355 {
  %6 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !208, metadata !DIExpression()), !dbg !431
  call void @llvm.dbg.value(metadata i32 %1, metadata !210, metadata !DIExpression()), !dbg !431
  call void @llvm.dbg.value(metadata i32 %2, metadata !212, metadata !DIExpression()), !dbg !431
  call void @llvm.dbg.value(metadata i32 %3, metadata !201, metadata !DIExpression()), !dbg !431
  %7 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %7, metadata !205, metadata !DIExpression()), !dbg !431
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !432
  %9 = icmp sle i32 0, %8, !dbg !432
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !432
  %11 = icmp slt i32 %10, %1, !dbg !432
  %12 = call i1 @pallas.scAnd(i1 %9, i1 %11), !dbg !432
  %13 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !433
  %14 = sext i32 %13 to i64, !dbg !434
  %15 = getelementptr inbounds i32, ptr %0, i64 %14, !dbg !434
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %6, i32 noundef 1, i32 noundef 1), !dbg !435
  %16 = call i1 @pallas.perm(ptr noundef %15, ptr noundef byval(%pallas.fracT) %6), !dbg !436
  %17 = call i1 @pallas.forallSep(i1 %12, i1 %16), !dbg !437
  ret i1 %17, !dbg !431
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !234 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !240, metadata !DIExpression()), !dbg !438
  call void @llvm.dbg.value(metadata i32 %1, metadata !242, metadata !DIExpression()), !dbg !438
  call void @llvm.dbg.value(metadata i32 %2, metadata !244, metadata !DIExpression()), !dbg !438
  call void @llvm.dbg.value(metadata i32 %3, metadata !233, metadata !DIExpression()), !dbg !438
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !237, metadata !DIExpression()), !dbg !438
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !439
  %8 = icmp sle i32 %2, %7, !dbg !439
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !439
  %10 = icmp slt i32 %9, %1, !dbg !439
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !439
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !440
  %13 = sext i32 %12 to i64, !dbg !441
  %14 = getelementptr inbounds i32, ptr %0, i64 %13, !dbg !441
  %15 = load i32, ptr %14, align 4, !dbg !441
  %16 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !442
  %17 = sext i32 %16 to i64, !dbg !443
  %18 = getelementptr inbounds i32, ptr %0, i64 %17, !dbg !443
  %19 = load i32, ptr %18, align 4, !dbg !443
  %20 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %19), !dbg !444
  %21 = icmp eq i32 %15, %20, !dbg !445
  %22 = call i1 @pallas.forall(i1 %11, i1 %21), !dbg !446
  ret i1 %22, !dbg !438
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !218 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !224, metadata !DIExpression()), !dbg !447
  call void @llvm.dbg.value(metadata i32 %1, metadata !226, metadata !DIExpression()), !dbg !447
  call void @llvm.dbg.value(metadata i32 %2, metadata !228, metadata !DIExpression()), !dbg !447
  call void @llvm.dbg.value(metadata i32 %3, metadata !217, metadata !DIExpression()), !dbg !447
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !221, metadata !DIExpression()), !dbg !447
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !448
  %8 = icmp sle i32 0, %7, !dbg !448
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !448
  %10 = icmp slt i32 %9, %2, !dbg !448
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !448
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !449
  %13 = sext i32 %12 to i64, !dbg !450
  %14 = getelementptr inbounds i32, ptr %0, i64 %13, !dbg !450
  %15 = load i32, ptr %14, align 4, !dbg !450
  %16 = icmp eq i32 %15, 0, !dbg !451
  %17 = call i1 @pallas.forall(i1 %11, i1 %16), !dbg !452
  ret i1 %17, !dbg !447
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !132 !pallas.ghostWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !140, metadata !DIExpression()), !dbg !453
  call void @llvm.dbg.value(metadata i32 %1, metadata !142, metadata !DIExpression()), !dbg !453
  call void @llvm.dbg.value(metadata i32 %2, metadata !144, metadata !DIExpression()), !dbg !453
  call void @llvm.dbg.value(metadata i32 %3, metadata !131, metadata !DIExpression()), !dbg !453
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !137, metadata !DIExpression()), !dbg !453
  ret i1 true, !dbg !453
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !160 !pallas.ghostWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !166, metadata !DIExpression()), !dbg !454
  call void @llvm.dbg.value(metadata i32 %1, metadata !168, metadata !DIExpression()), !dbg !454
  call void @llvm.dbg.value(metadata i32 %2, metadata !170, metadata !DIExpression()), !dbg !454
  call void @llvm.dbg.value(metadata i32 %3, metadata !159, metadata !DIExpression()), !dbg !454
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !163, metadata !DIExpression()), !dbg !454
  %7 = trunc i8 %6 to i1, !dbg !455
  br i1 %7, label %8, label %13, !dbg !456

8:                                                ; preds = %5
  %9 = sext i32 %2 to i64, !dbg !457
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !457
  %11 = load i32, ptr %10, align 4, !dbg !457
  %12 = icmp sgt i32 %11, %3, !dbg !458
  br label %13

13:                                               ; preds = %8, %5
  %14 = phi i1 [ false, %5 ], [ %12, %8 ], !dbg !454
  ret i1 %14, !dbg !454
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(ptr noundef %0, i32 noundef %1, i1 noundef zeroext %2) #0 !dbg !349 !pallas.exprWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !352, metadata !DIExpression()), !dbg !459
  call void @llvm.dbg.value(metadata i32 %1, metadata !354, metadata !DIExpression()), !dbg !459
  %4 = zext i1 %2 to i8
  call void @llvm.dbg.value(metadata i8 %4, metadata !348, metadata !DIExpression()), !dbg !459
  %5 = trunc i8 %4 to i1, !dbg !460
  %6 = zext i1 %5 to i32, !dbg !460
  %7 = icmp eq i32 %6, 1, !dbg !461
  ret i1 %7, !dbg !459
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @PALLAS_SPEC_19(ptr noundef %0, i32 noundef %1, i1 noundef zeroext %2) #0 !dbg !329 !pallas.ghostWrapper !355 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !334, metadata !DIExpression()), !dbg !462
  call void @llvm.dbg.value(metadata i32 %1, metadata !336, metadata !DIExpression()), !dbg !462
  %4 = zext i1 %2 to i8
  call void @llvm.dbg.value(metadata i8 %4, metadata !328, metadata !DIExpression()), !dbg !462
  ret i32 %1, !dbg !462
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !463 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !464 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !465 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !466 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !467 i32 @"pallas.old i32_noundef i32"(i32 noundef)

declare !pallas.specLib !468 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !469 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !470 i32 @"pallas.boundVar i32"(ptr)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!9, !11}
!llvm.module.flags = !{!15, !16, !17, !18, !19, !20, !21}
!llvm.ident = !{!22, !22}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 36, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "fed5ce90daf6be81c87527974573c0b4")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 76, type: !3, isLocal: true, isDefinition: true)
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !10, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!10 = !DIFile(filename: "examples/concepts/llvm/pallas/ghost/pallas_c_ghost_arg.c", directory: ".", checksumkind: CSK_MD5, checksum: "7d7292920e65d5711869736e4a303e4e")
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
!23 = distinct !DISubprogram(name: "clear_arr", scope: !10, file: !10, line: 22, type: !24, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!24 = !DISubroutineType(types: !25)
!25 = !{null, !26, !27}
!26 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !27, size: 64)
!27 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!28 = !{}
!29 = !{!30, i1 false, i1 false, !32, !35, !38, !54, !65, !76, !92, !106}
!30 = !{!"pallas.srcLoc", i64 8, i64 1, i64 21, i64 1, !31}
!31 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/ghost/pallas_c_ghost_arg.c", directory: "", checksumkind: CSK_MD5, checksum: "7d7292920e65d5711869736e4a303e4e")
!32 = !{!33}
!33 = !{!34, !"x"}
!34 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 13, !31}
!35 = !{!36}
!36 = !{!37, !"all_gt_x"}
!37 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 21, !31}
!38 = !{!"pallas.requires", !39, ptr @PALLAS_SPEC_0, !40, !28, !47}
!39 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 16, !31}
!40 = !{!41}
!41 = !{!33, !42}
!42 = !DILocalVariable(name: "x", arg: 3, scope: !43, file: !10, line: 11, type: !27)
!43 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !10, file: !10, line: 11, type: !44, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!44 = !DISubroutineType(types: !45)
!45 = !{!46, !26, !27, !27}
!46 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!47 = !{!48, !51}
!48 = !{!49, !50}
!49 = !DILocalVariable(name: "arr", arg: 1, scope: !23, file: !10, line: 22, type: !26)
!50 = !DILocalVariable(name: "arr", arg: 1, scope: !43, file: !10, line: 11, type: !26)
!51 = !{!52, !53}
!52 = !DILocalVariable(name: "n", arg: 2, scope: !23, file: !10, line: 22, type: !27)
!53 = !DILocalVariable(name: "n", arg: 2, scope: !43, file: !10, line: 11, type: !27)
!54 = !{!"pallas.requires", !55, ptr @PALLAS_SPEC_1, !56, !28, !60}
!55 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 46, !31}
!56 = !{!57}
!57 = !{!33, !58}
!58 = !DILocalVariable(name: "x", arg: 3, scope: !59, file: !10, line: 12, type: !27)
!59 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !10, file: !10, line: 12, type: !44, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!60 = !{!61, !63}
!61 = !{!49, !62}
!62 = !DILocalVariable(name: "arr", arg: 1, scope: !59, file: !10, line: 12, type: !26)
!63 = !{!52, !64}
!64 = !DILocalVariable(name: "n", arg: 2, scope: !59, file: !10, line: 12, type: !27)
!65 = !{!"pallas.requires", !66, ptr @PALLAS_SPEC_2, !67, !28, !71}
!66 = !{!"pallas.srcLoc", i64 13, i64 1, i64 14, i64 52, !31}
!67 = !{!68}
!68 = !{!33, !69}
!69 = !DILocalVariable(name: "x", arg: 3, scope: !70, file: !10, line: 13, type: !27)
!70 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !10, file: !10, line: 13, type: !44, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!71 = !{!72, !74}
!72 = !{!49, !73}
!73 = !DILocalVariable(name: "arr", arg: 1, scope: !70, file: !10, line: 13, type: !26)
!74 = !{!52, !75}
!75 = !DILocalVariable(name: "n", arg: 2, scope: !70, file: !10, line: 13, type: !27)
!76 = !{!"pallas.ensures", !77, ptr @PALLAS_SPEC_3, !78, !84, !87}
!77 = !{!"pallas.srcLoc", i64 15, i64 1, i64 16, i64 52, !31}
!78 = !{!79}
!79 = !{!33, !80}
!80 = !DILocalVariable(name: "x", arg: 3, scope: !81, file: !10, line: 15, type: !27)
!81 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !10, file: !10, line: 15, type: !82, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!82 = !DISubroutineType(types: !83)
!83 = !{!46, !26, !27, !27, !46}
!84 = !{!85}
!85 = !{!36, !86}
!86 = !DILocalVariable(name: "all_gt_x", arg: 4, scope: !81, file: !10, line: 15, type: !46)
!87 = !{!88, !90}
!88 = !{!49, !89}
!89 = !DILocalVariable(name: "arr", arg: 1, scope: !81, file: !10, line: 15, type: !26)
!90 = !{!52, !91}
!91 = !DILocalVariable(name: "n", arg: 2, scope: !81, file: !10, line: 15, type: !27)
!92 = !{!"pallas.ensures", !93, ptr @PALLAS_SPEC_4, !94, !98, !101}
!93 = !{!"pallas.srcLoc", i64 17, i64 1, i64 18, i64 41, !31}
!94 = !{!95}
!95 = !{!33, !96}
!96 = !DILocalVariable(name: "x", arg: 3, scope: !97, file: !10, line: 17, type: !27)
!97 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !10, file: !10, line: 17, type: !82, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!98 = !{!99}
!99 = !{!36, !100}
!100 = !DILocalVariable(name: "all_gt_x", arg: 4, scope: !97, file: !10, line: 17, type: !46)
!101 = !{!102, !104}
!102 = !{!49, !103}
!103 = !DILocalVariable(name: "arr", arg: 1, scope: !97, file: !10, line: 17, type: !26)
!104 = !{!52, !105}
!105 = !DILocalVariable(name: "n", arg: 2, scope: !97, file: !10, line: 17, type: !27)
!106 = !{!"pallas.ensures", !107, ptr @PALLAS_SPEC_5, !108, !112, !115}
!107 = !{!"pallas.srcLoc", i64 19, i64 1, i64 20, i64 61, !31}
!108 = !{!109}
!109 = !{!33, !110}
!110 = !DILocalVariable(name: "x", arg: 3, scope: !111, file: !10, line: 19, type: !27)
!111 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !10, file: !10, line: 19, type: !82, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!112 = !{!113}
!113 = !{!36, !114}
!114 = !DILocalVariable(name: "all_gt_x", arg: 4, scope: !111, file: !10, line: 19, type: !46)
!115 = !{!116, !118}
!116 = !{!49, !117}
!117 = !DILocalVariable(name: "arr", arg: 1, scope: !111, file: !10, line: 19, type: !26)
!118 = !{!52, !119}
!119 = !DILocalVariable(name: "n", arg: 2, scope: !111, file: !10, line: 19, type: !27)
!120 = !DILocation(line: 22, column: 21, scope: !23)
!121 = !DILocation(line: 22, column: 30, scope: !23)
!122 = !DILocalVariable(name: "i", scope: !123, file: !10, line: 39, type: !27)
!123 = distinct !DILexicalBlock(scope: !23, file: !10, line: 39, column: 5)
!124 = !DILocation(line: 39, column: 14, scope: !123)
!125 = !{!126, !127}
!126 = !{!"pallas.srcLoc", i64 24, i64 5, i64 26, i64 5, !31}
!127 = !{!"pallas.gAssign", !128, ptr @PALLAS_SPEC_16, !129, !135, !138, !36}
!128 = !{!"pallas.srcLoc", i64 25, i64 5, i64 25, i64 33, !31}
!129 = !{!130}
!130 = !{!33, !131}
!131 = !DILocalVariable(name: "x", arg: 4, scope: !132, file: !10, line: 25, type: !27)
!132 = distinct !DISubprogram(name: "PALLAS_SPEC_16", scope: !10, file: !10, line: 25, type: !133, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!133 = !DISubroutineType(types: !134)
!134 = !{!46, !26, !27, !27, !27, !46}
!135 = !{!136}
!136 = !{!36, !137}
!137 = !DILocalVariable(name: "all_gt_x", arg: 5, scope: !132, file: !10, line: 25, type: !46)
!138 = !{!139, !141, !143}
!139 = !{!49, !140}
!140 = !DILocalVariable(name: "arr", arg: 1, scope: !132, file: !10, line: 25, type: !26)
!141 = !{!52, !142}
!142 = !DILocalVariable(name: "n", arg: 2, scope: !132, file: !10, line: 25, type: !27)
!143 = !{!122, !144}
!144 = !DILocalVariable(name: "i", arg: 3, scope: !132, file: !10, line: 25, type: !27)
!145 = !DILocation(line: 39, column: 10, scope: !123)
!146 = !DILocation(line: 39, column: 21, scope: !147)
!147 = distinct !DILexicalBlock(scope: !123, file: !10, line: 39, column: 5)
!148 = !DILocation(line: 39, column: 25, scope: !147)
!149 = !DILocation(line: 39, column: 23, scope: !147)
!150 = !DILocation(line: 39, column: 5, scope: !123)
!151 = !DILocation(line: 43, column: 9, scope: !152)
!152 = distinct !DILexicalBlock(scope: !147, file: !10, line: 39, column: 33)
!153 = !{!154, !155}
!154 = !{!"pallas.srcLoc", i64 40, i64 9, i64 42, i64 9, !31}
!155 = !{!"pallas.gAssign", !156, ptr @PALLAS_SPEC_17, !157, !161, !164, !36}
!156 = !{!"pallas.srcLoc", i64 41, i64 9, i64 41, i64 57, !31}
!157 = !{!158}
!158 = !{!33, !159}
!159 = !DILocalVariable(name: "x", arg: 4, scope: !160, file: !10, line: 41, type: !27)
!160 = distinct !DISubprogram(name: "PALLAS_SPEC_17", scope: !10, file: !10, line: 41, type: !133, scopeLine: 41, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!161 = !{!162}
!162 = !{!36, !163}
!163 = !DILocalVariable(name: "all_gt_x", arg: 5, scope: !160, file: !10, line: 41, type: !46)
!164 = !{!165, !167, !169}
!165 = !{!49, !166}
!166 = !DILocalVariable(name: "arr", arg: 1, scope: !160, file: !10, line: 41, type: !26)
!167 = !{!52, !168}
!168 = !DILocalVariable(name: "n", arg: 2, scope: !160, file: !10, line: 41, type: !27)
!169 = !{!122, !170}
!170 = !DILocalVariable(name: "i", arg: 3, scope: !160, file: !10, line: 41, type: !27)
!171 = !DILocation(line: 43, column: 13, scope: !152)
!172 = !DILocation(line: 43, column: 16, scope: !152)
!173 = !DILocation(line: 44, column: 5, scope: !152)
!174 = !DILocation(line: 39, column: 28, scope: !147)
!175 = !DILocation(line: 39, column: 5, scope: !147)
!176 = distinct !{!176, !150, !177, !178, !179}
!177 = !DILocation(line: 44, column: 5, scope: !123)
!178 = !{!"llvm.loop.mustprogress"}
!179 = !{!"pallas.loopInvBlock", !180, !181, !197, !213, !229, !245}
!180 = !{!"pallas.srcLoc", i64 28, i64 5, i64 38, i64 5, !31}
!181 = !{!"pallas.loopInv", !182, ptr @PALLAS_SPEC_11, !183, !187, !190}
!182 = !{!"pallas.srcLoc", i64 29, i64 5, i64 29, i64 36, !31}
!183 = !{!184}
!184 = !{!33, !185}
!185 = !DILocalVariable(name: "x", arg: 4, scope: !186, file: !10, line: 29, type: !27)
!186 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !10, file: !10, line: 29, type: !133, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!187 = !{!188}
!188 = !{!36, !189}
!189 = !DILocalVariable(name: "all_gt_x", arg: 5, scope: !186, file: !10, line: 29, type: !46)
!190 = !{!191, !193, !195}
!191 = !{!49, !192}
!192 = !DILocalVariable(name: "arr", arg: 1, scope: !186, file: !10, line: 29, type: !26)
!193 = !{!52, !194}
!194 = !DILocalVariable(name: "n", arg: 2, scope: !186, file: !10, line: 29, type: !27)
!195 = !{!122, !196}
!196 = !DILocalVariable(name: "i", arg: 3, scope: !186, file: !10, line: 29, type: !27)
!197 = !{!"pallas.loopInv", !198, ptr @PALLAS_SPEC_12, !199, !203, !206}
!198 = !{!"pallas.srcLoc", i64 30, i64 5, i64 31, i64 62, !31}
!199 = !{!200}
!200 = !{!33, !201}
!201 = !DILocalVariable(name: "x", arg: 4, scope: !202, file: !10, line: 30, type: !27)
!202 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !10, file: !10, line: 30, type: !133, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!203 = !{!204}
!204 = !{!36, !205}
!205 = !DILocalVariable(name: "all_gt_x", arg: 5, scope: !202, file: !10, line: 30, type: !46)
!206 = !{!207, !209, !211}
!207 = !{!49, !208}
!208 = !DILocalVariable(name: "arr", arg: 1, scope: !202, file: !10, line: 30, type: !26)
!209 = !{!52, !210}
!210 = !DILocalVariable(name: "n", arg: 2, scope: !202, file: !10, line: 30, type: !27)
!211 = !{!122, !212}
!212 = !DILocalVariable(name: "i", arg: 3, scope: !202, file: !10, line: 30, type: !27)
!213 = !{!"pallas.loopInv", !214, ptr @PALLAS_SPEC_13, !215, !219, !222}
!214 = !{!"pallas.srcLoc", i64 32, i64 5, i64 33, i64 51, !31}
!215 = !{!216}
!216 = !{!33, !217}
!217 = !DILocalVariable(name: "x", arg: 4, scope: !218, file: !10, line: 32, type: !27)
!218 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !10, file: !10, line: 32, type: !133, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!219 = !{!220}
!220 = !{!36, !221}
!221 = !DILocalVariable(name: "all_gt_x", arg: 5, scope: !218, file: !10, line: 32, type: !46)
!222 = !{!223, !225, !227}
!223 = !{!49, !224}
!224 = !DILocalVariable(name: "arr", arg: 1, scope: !218, file: !10, line: 32, type: !26)
!225 = !{!52, !226}
!226 = !DILocalVariable(name: "n", arg: 2, scope: !218, file: !10, line: 32, type: !27)
!227 = !{!122, !228}
!228 = !DILocalVariable(name: "i", arg: 3, scope: !218, file: !10, line: 32, type: !27)
!229 = !{!"pallas.loopInv", !230, ptr @PALLAS_SPEC_14, !231, !235, !238}
!230 = !{!"pallas.srcLoc", i64 34, i64 5, i64 35, i64 77, !31}
!231 = !{!232}
!232 = !{!33, !233}
!233 = !DILocalVariable(name: "x", arg: 4, scope: !234, file: !10, line: 34, type: !27)
!234 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !10, file: !10, line: 34, type: !133, scopeLine: 34, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!235 = !{!236}
!236 = !{!36, !237}
!237 = !DILocalVariable(name: "all_gt_x", arg: 5, scope: !234, file: !10, line: 34, type: !46)
!238 = !{!239, !241, !243}
!239 = !{!49, !240}
!240 = !DILocalVariable(name: "arr", arg: 1, scope: !234, file: !10, line: 34, type: !26)
!241 = !{!52, !242}
!242 = !DILocalVariable(name: "n", arg: 2, scope: !234, file: !10, line: 34, type: !27)
!243 = !{!122, !244}
!244 = !DILocalVariable(name: "i", arg: 3, scope: !234, file: !10, line: 34, type: !27)
!245 = !{!"pallas.loopInv", !246, ptr @PALLAS_SPEC_15, !247, !251, !254}
!246 = !{!"pallas.srcLoc", i64 36, i64 5, i64 37, i64 72, !31}
!247 = !{!248}
!248 = !{!33, !249}
!249 = !DILocalVariable(name: "x", arg: 4, scope: !250, file: !10, line: 36, type: !27)
!250 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !10, file: !10, line: 36, type: !133, scopeLine: 36, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!251 = !{!252}
!252 = !{!36, !253}
!253 = !DILocalVariable(name: "all_gt_x", arg: 5, scope: !250, file: !10, line: 36, type: !46)
!254 = !{!255, !257, !259}
!255 = !{!49, !256}
!256 = !DILocalVariable(name: "arr", arg: 1, scope: !250, file: !10, line: 36, type: !26)
!257 = !{!52, !258}
!258 = !DILocalVariable(name: "n", arg: 2, scope: !250, file: !10, line: 36, type: !27)
!259 = !{!122, !260}
!260 = !DILocalVariable(name: "i", arg: 3, scope: !250, file: !10, line: 36, type: !27)
!261 = !DILocation(line: 45, column: 1, scope: !23)
!262 = distinct !DISubprogram(name: "foo", scope: !10, file: !10, line: 57, type: !24, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!263 = !{!264, i1 false, i1 false, !28, !265, !268, !280, !288, !296, !304}
!264 = !{!"pallas.srcLoc", i64 47, i64 1, i64 56, i64 1, !31}
!265 = !{!266}
!266 = !{!267, !"res"}
!267 = !{!"pallas.srcLoc", i64 48, i64 1, i64 48, i64 16, !31}
!268 = !{!"pallas.requires", !269, ptr @PALLAS_SPEC_6, !28, !28, !270}
!269 = !{!"pallas.srcLoc", i64 49, i64 1, i64 49, i64 15, !31}
!270 = !{!271, !277}
!271 = !{!272, !273}
!272 = !DILocalVariable(name: "arr", arg: 1, scope: !262, file: !10, line: 57, type: !26)
!273 = !DILocalVariable(name: "arr", arg: 1, scope: !274, file: !10, line: 49, type: !26)
!274 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !10, file: !10, line: 49, type: !275, scopeLine: 49, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!275 = !DISubroutineType(types: !276)
!276 = !{!46, !26, !27}
!277 = !{!278, !279}
!278 = !DILocalVariable(name: "n", arg: 2, scope: !262, file: !10, line: 57, type: !27)
!279 = !DILocalVariable(name: "n", arg: 2, scope: !274, file: !10, line: 49, type: !27)
!280 = !{!"pallas.requires", !281, ptr @PALLAS_SPEC_7, !28, !28, !282}
!281 = !{!"pallas.srcLoc", i64 50, i64 1, i64 50, i64 46, !31}
!282 = !{!283, !286}
!283 = !{!272, !284}
!284 = !DILocalVariable(name: "arr", arg: 1, scope: !285, file: !10, line: 50, type: !26)
!285 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !10, file: !10, line: 50, type: !275, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!286 = !{!278, !287}
!287 = !DILocalVariable(name: "n", arg: 2, scope: !285, file: !10, line: 50, type: !27)
!288 = !{!"pallas.requires", !289, ptr @PALLAS_SPEC_8, !28, !28, !290}
!289 = !{!"pallas.srcLoc", i64 51, i64 1, i64 52, i64 52, !31}
!290 = !{!291, !294}
!291 = !{!272, !292}
!292 = !DILocalVariable(name: "arr", arg: 1, scope: !293, file: !10, line: 51, type: !26)
!293 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !10, file: !10, line: 51, type: !275, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!294 = !{!278, !295}
!295 = !DILocalVariable(name: "n", arg: 2, scope: !293, file: !10, line: 51, type: !27)
!296 = !{!"pallas.requires", !297, ptr @PALLAS_SPEC_9, !28, !28, !298}
!297 = !{!"pallas.srcLoc", i64 53, i64 1, i64 54, i64 43, !31}
!298 = !{!299, !302}
!299 = !{!272, !300}
!300 = !DILocalVariable(name: "arr", arg: 1, scope: !301, file: !10, line: 53, type: !26)
!301 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !10, file: !10, line: 53, type: !275, scopeLine: 53, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!302 = !{!278, !303}
!303 = !DILocalVariable(name: "n", arg: 2, scope: !301, file: !10, line: 53, type: !27)
!304 = !{!"pallas.ensures", !305, ptr @PALLAS_SPEC_10, !28, !306, !312}
!305 = !{!"pallas.srcLoc", i64 55, i64 1, i64 55, i64 20, !31}
!306 = !{!307}
!307 = !{!266, !308}
!308 = !DILocalVariable(name: "res", arg: 3, scope: !309, file: !10, line: 55, type: !46)
!309 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !10, file: !10, line: 55, type: !310, scopeLine: 55, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!310 = !DISubroutineType(types: !311)
!311 = !{!46, !26, !27, !46}
!312 = !{!313, !315}
!313 = !{!272, !314}
!314 = !DILocalVariable(name: "arr", arg: 1, scope: !309, file: !10, line: 55, type: !26)
!315 = !{!278, !316}
!316 = !DILocalVariable(name: "n", arg: 2, scope: !309, file: !10, line: 55, type: !27)
!317 = !DILocation(line: 57, column: 15, scope: !262)
!318 = !DILocation(line: 57, column: 24, scope: !262)
!319 = !DILocation(line: 58, column: 68, scope: !262)
!320 = !DILocation(line: 58, column: 73, scope: !262)
!321 = !DILocation(line: 58, column: 5, scope: !262)
!322 = !{!323, !324}
!323 = !{!"pallas.srcLoc", i64 58, i64 15, i64 58, i64 32, !31}
!324 = !{!"pallas.givenBinding", !325, ptr @PALLAS_SPEC_19, !28, !326, !332, !33}
!325 = !{!"pallas.srcLoc", i64 58, i64 25, i64 58, i64 30, !31}
!326 = !{!327}
!327 = !{!266, !328}
!328 = !DILocalVariable(name: "res", arg: 3, scope: !329, file: !10, line: 58, type: !46)
!329 = distinct !DISubprogram(name: "PALLAS_SPEC_19", scope: !10, file: !10, line: 58, type: !330, scopeLine: 58, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!330 = !DISubroutineType(types: !331)
!331 = !{!27, !26, !27, !46}
!332 = !{!333, !335}
!333 = !{!272, !334}
!334 = !DILocalVariable(name: "arr", arg: 1, scope: !329, file: !10, line: 58, type: !26)
!335 = !{!278, !336}
!336 = !DILocalVariable(name: "n", arg: 2, scope: !329, file: !10, line: 58, type: !27)
!337 = !{!338, !339}
!338 = !{!"pallas.srcLoc", i64 58, i64 36, i64 58, i64 63, !31}
!339 = !{!"pallas.yieldsBinding", !340, !266, !36}
!340 = !{!"pallas.srcLoc", i64 58, i64 47, i64 58, i64 61, !31}
!341 = !DILocation(line: 63, column: 1, scope: !262)
!342 = !{!343, !344}
!343 = !{!"pallas.srcLoc", i64 60, i64 5, i64 62, i64 5, !31}
!344 = !{!"pallas.assert", !345, ptr @PALLAS_SPEC_18, !28, !346, !350}
!345 = !{!"pallas.srcLoc", i64 61, i64 5, i64 61, i64 23, !31}
!346 = !{!347}
!347 = !{!266, !348}
!348 = !DILocalVariable(name: "res", arg: 3, scope: !349, file: !10, line: 61, type: !46)
!349 = distinct !DISubprogram(name: "PALLAS_SPEC_18", scope: !10, file: !10, line: 61, type: !310, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !28)
!350 = !{!351, !353}
!351 = !{!272, !352}
!352 = !DILocalVariable(name: "arr", arg: 1, scope: !349, file: !10, line: 61, type: !26)
!353 = !{!278, !354}
!354 = !DILocalVariable(name: "n", arg: 2, scope: !349, file: !10, line: 61, type: !27)
!355 = !{!""}
!356 = !DILocation(line: 0, scope: !43)
!357 = !DILocation(line: 11, column: 12, scope: !43)
!358 = !DILocation(line: 0, scope: !59)
!359 = !DILocation(line: 12, column: 14, scope: !59)
!360 = !DILocation(line: 12, column: 22, scope: !59)
!361 = !DILocation(line: 12, column: 25, scope: !59)
!362 = !DILocation(line: 12, column: 45, scope: !59)
!363 = !DILocation(line: 12, column: 42, scope: !59)
!364 = !DILocation(line: 0, scope: !70)
!365 = !DILocation(line: 13, column: 19, scope: !70)
!366 = !DILocation(line: 14, column: 30, scope: !70)
!367 = !DILocation(line: 14, column: 26, scope: !70)
!368 = !DILocation(line: 14, column: 44, scope: !70)
!369 = !DILocation(line: 14, column: 19, scope: !70)
!370 = !DILocation(line: 13, column: 10, scope: !70)
!371 = !DILocation(line: 0, scope: !81)
!372 = !DILocation(line: 15, column: 19, scope: !81)
!373 = !DILocation(line: 16, column: 30, scope: !81)
!374 = !DILocation(line: 16, column: 26, scope: !81)
!375 = !DILocation(line: 16, column: 44, scope: !81)
!376 = !DILocation(line: 16, column: 19, scope: !81)
!377 = !DILocation(line: 15, column: 10, scope: !81)
!378 = !DILocation(line: 0, scope: !97)
!379 = !DILocation(line: 17, column: 19, scope: !97)
!380 = !DILocation(line: 18, column: 23, scope: !97)
!381 = !DILocation(line: 18, column: 19, scope: !97)
!382 = !DILocation(line: 18, column: 36, scope: !97)
!383 = !DILocation(line: 17, column: 10, scope: !97)
!384 = !DILocation(line: 0, scope: !111)
!385 = !DILocation(line: 19, column: 9, scope: !111)
!386 = !DILocation(line: 19, column: 29, scope: !111)
!387 = !DILocation(line: 20, column: 43, scope: !111)
!388 = !DILocation(line: 20, column: 39, scope: !111)
!389 = !DILocation(line: 20, column: 29, scope: !111)
!390 = !DILocation(line: 20, column: 57, scope: !111)
!391 = !DILocation(line: 19, column: 21, scope: !111)
!392 = !DILocation(line: 19, column: 18, scope: !111)
!393 = !DILocation(line: 0, scope: !274)
!394 = !DILocation(line: 49, column: 12, scope: !274)
!395 = !DILocation(line: 0, scope: !285)
!396 = !DILocation(line: 50, column: 14, scope: !285)
!397 = !DILocation(line: 50, column: 22, scope: !285)
!398 = !DILocation(line: 50, column: 25, scope: !285)
!399 = !DILocation(line: 50, column: 45, scope: !285)
!400 = !DILocation(line: 50, column: 42, scope: !285)
!401 = !DILocation(line: 0, scope: !293)
!402 = !DILocation(line: 51, column: 19, scope: !293)
!403 = !DILocation(line: 52, column: 30, scope: !293)
!404 = !DILocation(line: 52, column: 26, scope: !293)
!405 = !DILocation(line: 52, column: 44, scope: !293)
!406 = !DILocation(line: 52, column: 19, scope: !293)
!407 = !DILocation(line: 51, column: 10, scope: !293)
!408 = !DILocation(line: 0, scope: !301)
!409 = !DILocation(line: 53, column: 19, scope: !301)
!410 = !DILocation(line: 54, column: 23, scope: !301)
!411 = !DILocation(line: 54, column: 19, scope: !301)
!412 = !DILocation(line: 54, column: 40, scope: !301)
!413 = !DILocation(line: 54, column: 36, scope: !301)
!414 = !DILocation(line: 53, column: 10, scope: !301)
!415 = !DILocation(line: 0, scope: !309)
!416 = !DILocation(line: 55, column: 9, scope: !309)
!417 = !DILocation(line: 55, column: 13, scope: !309)
!418 = !DILocation(line: 0, scope: !250)
!419 = !DILocation(line: 36, column: 20, scope: !250)
!420 = !DILocation(line: 36, column: 40, scope: !250)
!421 = !DILocation(line: 37, column: 54, scope: !250)
!422 = !DILocation(line: 37, column: 50, scope: !250)
!423 = !DILocation(line: 37, column: 40, scope: !250)
!424 = !DILocation(line: 37, column: 68, scope: !250)
!425 = !DILocation(line: 36, column: 32, scope: !250)
!426 = !DILocation(line: 36, column: 29, scope: !250)
!427 = !DILocation(line: 0, scope: !186)
!428 = !DILocation(line: 29, column: 22, scope: !186)
!429 = !DILocation(line: 29, column: 27, scope: !186)
!430 = !DILocation(line: 29, column: 32, scope: !186)
!431 = !DILocation(line: 0, scope: !202)
!432 = !DILocation(line: 30, column: 29, scope: !202)
!433 = !DILocation(line: 31, column: 40, scope: !202)
!434 = !DILocation(line: 31, column: 36, scope: !202)
!435 = !DILocation(line: 31, column: 54, scope: !202)
!436 = !DILocation(line: 31, column: 29, scope: !202)
!437 = !DILocation(line: 30, column: 20, scope: !202)
!438 = !DILocation(line: 0, scope: !234)
!439 = !DILocation(line: 34, column: 29, scope: !234)
!440 = !DILocation(line: 35, column: 33, scope: !234)
!441 = !DILocation(line: 35, column: 29, scope: !234)
!442 = !DILocation(line: 35, column: 63, scope: !234)
!443 = !DILocation(line: 35, column: 59, scope: !234)
!444 = !DILocation(line: 35, column: 49, scope: !234)
!445 = !DILocation(line: 35, column: 46, scope: !234)
!446 = !DILocation(line: 34, column: 20, scope: !234)
!447 = !DILocation(line: 0, scope: !218)
!448 = !DILocation(line: 32, column: 29, scope: !218)
!449 = !DILocation(line: 33, column: 33, scope: !218)
!450 = !DILocation(line: 33, column: 29, scope: !218)
!451 = !DILocation(line: 33, column: 46, scope: !218)
!452 = !DILocation(line: 32, column: 20, scope: !218)
!453 = !DILocation(line: 0, scope: !132)
!454 = !DILocation(line: 0, scope: !160)
!455 = !DILocation(line: 41, column: 33, scope: !160)
!456 = !DILocation(line: 41, column: 42, scope: !160)
!457 = !DILocation(line: 41, column: 46, scope: !160)
!458 = !DILocation(line: 41, column: 53, scope: !160)
!459 = !DILocation(line: 0, scope: !349)
!460 = !DILocation(line: 61, column: 12, scope: !349)
!461 = !DILocation(line: 61, column: 16, scope: !349)
!462 = !DILocation(line: 0, scope: !329)
!463 = !{!"pallas.ptrLength"}
!464 = !{!"pallas.forallSep"}
!465 = !{!"pallas.perm"}
!466 = !{!"pallas.fracOf"}
!467 = !{!"pallas.old"}
!468 = !{!"pallas.forall"}
!469 = !{!"pallas.scAnd"}
!470 = !{!"pallas.boundVar"}
