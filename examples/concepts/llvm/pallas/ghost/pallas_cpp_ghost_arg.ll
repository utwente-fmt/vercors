; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/ghost/pallas_cpp_ghost_arg.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.compiler.used = appending global [18 x ptr] [ptr @_Z5isMaxiii, ptr @_Z5isMiniii, ptr @_Z13PALLAS_SPEC_0PiS_i, ptr @_Z13PALLAS_SPEC_1PiS_i, ptr @_Z13PALLAS_SPEC_2PiS_ibi, ptr @_Z13PALLAS_SPEC_3PiS_ibi, ptr @_Z13PALLAS_SPEC_4PiS_ibi, ptr @_Z13PALLAS_SPEC_5PiS_ibi, ptr @_Z13PALLAS_SPEC_8PiS_ibi, ptr @_Z13PALLAS_SPEC_9PiS_ibi, ptr @_Z13PALLAS_SPEC_6ib, ptr @_Z13PALLAS_SPEC_7ib, ptr @_Z14PALLAS_SPEC_10iiiib, ptr @_Z14PALLAS_SPEC_11iiiib, ptr @_Z14PALLAS_SPEC_12iiiib, ptr @_Z14PALLAS_SPEC_13iiiib, ptr @_Z14PALLAS_SPEC_14iiiib, ptr @_Z14PALLAS_SPEC_15iiiib], section "llvm.metadata"
@llvm.used = appending global [18 x ptr] [ptr @_Z13PALLAS_SPEC_0PiS_i, ptr @_Z13PALLAS_SPEC_1PiS_i, ptr @_Z13PALLAS_SPEC_2PiS_ibi, ptr @_Z13PALLAS_SPEC_3PiS_ibi, ptr @_Z13PALLAS_SPEC_4PiS_ibi, ptr @_Z13PALLAS_SPEC_5PiS_ibi, ptr @_Z13PALLAS_SPEC_6ib, ptr @_Z13PALLAS_SPEC_7ib, ptr @_Z13PALLAS_SPEC_8PiS_ibi, ptr @_Z13PALLAS_SPEC_9PiS_ibi, ptr @_Z14PALLAS_SPEC_10iiiib, ptr @_Z14PALLAS_SPEC_11iiiib, ptr @_Z14PALLAS_SPEC_12iiiib, ptr @_Z14PALLAS_SPEC_13iiiib, ptr @_Z14PALLAS_SPEC_14iiiib, ptr @_Z14PALLAS_SPEC_15iiiib, ptr @_Z5isMaxiii, ptr @_Z5isMiniii], section "llvm.metadata"

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef i32 @_Z7get_maxPiS_(ptr noundef %0, ptr noundef %1) #0 !dbg !112 !pallas.fcontract !117 {
  %3 = alloca i32, align 4
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  store ptr %0, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !139, metadata !DIExpression()), !dbg !221
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !142, metadata !DIExpression()), !dbg !222
  %6 = load ptr, ptr %4, align 8, !dbg !223, !pallas.stmntBlock !225
  %7 = load i32, ptr %6, align 4, !dbg !261
  %8 = load ptr, ptr %5, align 8, !dbg !262
  %9 = load i32, ptr %8, align 4, !dbg !263
  %10 = icmp sgt i32 %7, %9, !dbg !264
  br i1 %10, label %11, label %14, !dbg !265

11:                                               ; preds = %2
  %12 = load ptr, ptr %4, align 8, !dbg !266
  %13 = load i32, ptr %12, align 4, !dbg !268
  store i32 %13, ptr %3, align 4, !dbg !269
  br label %17, !dbg !269

14:                                               ; preds = %2
  %15 = load ptr, ptr %5, align 8, !dbg !270
  %16 = load i32, ptr %15, align 4, !dbg !272
  store i32 %16, ptr %3, align 4, !dbg !273
  br label %17, !dbg !273

17:                                               ; preds = %14, %11
  %18 = load i32, ptr %3, align 4, !dbg !274
  ret i32 %18, !dbg !274
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local void @_Z3runv() #0 !dbg !275 !pallas.fcontract !278 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  call void @llvm.dbg.declare(metadata ptr %1, metadata !303, metadata !DIExpression()), !dbg !304
  store i32 1, ptr %1, align 4, !dbg !304
  call void @llvm.dbg.declare(metadata ptr %2, metadata !305, metadata !DIExpression()), !dbg !306
  store i32 42, ptr %2, align 4, !dbg !306
  call void @llvm.dbg.declare(metadata ptr %3, metadata !307, metadata !DIExpression()), !dbg !308
  %4 = call noundef i32 @_Z7get_maxPiS_(ptr noundef %1, ptr noundef %2), !dbg !309, !pallas.givenBindings !310, !pallas.yieldsBindings !329
  store i32 %4, ptr %3, align 4, !dbg !308
  store i32 -1, ptr %1, align 4, !dbg !335, !pallas.stmntBlock !336
  store i32 84, ptr %2, align 4, !dbg !385
  %5 = call noundef i32 @_Z7get_maxPiS_(ptr noundef %1, ptr noundef %2), !dbg !386, !pallas.givenBindings !387, !pallas.yieldsBindings !404
  ret void, !dbg !408, !pallas.stmntBlock !409
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z5isMaxiii(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !426 !pallas.predDef !429 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !430, metadata !DIExpression()), !dbg !431
  call void @llvm.dbg.value(metadata i32 %1, metadata !432, metadata !DIExpression()), !dbg !431
  call void @llvm.dbg.value(metadata i32 %2, metadata !433, metadata !DIExpression()), !dbg !431
  %4 = icmp eq i32 %0, %1, !dbg !434
  br i1 %4, label %7, label %5, !dbg !435

5:                                                ; preds = %3
  %6 = icmp eq i32 %0, %2, !dbg !436
  br i1 %6, label %7, label %13, !dbg !437

7:                                                ; preds = %5, %3
  %8 = icmp sge i32 %0, %1, !dbg !438
  br i1 %8, label %9, label %11, !dbg !439

9:                                                ; preds = %7
  %10 = icmp sge i32 %0, %2, !dbg !440
  br label %11

11:                                               ; preds = %9, %7
  %12 = phi i1 [ false, %7 ], [ %10, %9 ], !dbg !431
  br label %13

13:                                               ; preds = %11, %5
  %14 = phi i1 [ false, %5 ], [ %12, %11 ], !dbg !431
  ret i1 %14, !dbg !431
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z5isMiniii(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !441 !pallas.predDef !429 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !442, metadata !DIExpression()), !dbg !443
  call void @llvm.dbg.value(metadata i32 %1, metadata !444, metadata !DIExpression()), !dbg !443
  call void @llvm.dbg.value(metadata i32 %2, metadata !445, metadata !DIExpression()), !dbg !443
  %4 = icmp eq i32 %0, %1, !dbg !446
  br i1 %4, label %7, label %5, !dbg !447

5:                                                ; preds = %3
  %6 = icmp eq i32 %0, %2, !dbg !448
  br i1 %6, label %7, label %13, !dbg !449

7:                                                ; preds = %5, %3
  %8 = icmp sle i32 %0, %1, !dbg !450
  br i1 %8, label %9, label %11, !dbg !451

9:                                                ; preds = %7
  %10 = icmp sle i32 %0, %2, !dbg !452
  br label %11

11:                                               ; preds = %9, %7
  %12 = phi i1 [ false, %7 ], [ %10, %9 ], !dbg !443
  br label %13

13:                                               ; preds = %11, %5
  %14 = phi i1 [ false, %5 ], [ %12, %11 ], !dbg !443
  ret i1 %14, !dbg !443
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_0PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #0 !dbg !133 !pallas.exprWrapper !453 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !140, metadata !DIExpression()), !dbg !454
  call void @llvm.dbg.value(metadata ptr %1, metadata !143, metadata !DIExpression()), !dbg !454
  call void @llvm.dbg.value(metadata i32 %2, metadata !132, metadata !DIExpression()), !dbg !454
  %4 = icmp ne ptr %0, null, !dbg !455
  br i1 %4, label %5, label %7, !dbg !456

5:                                                ; preds = %3
  %6 = icmp ne ptr %1, null, !dbg !457
  br label %7

7:                                                ; preds = %5, %3
  %8 = phi i1 [ false, %3 ], [ %6, %5 ], !dbg !454
  ret i1 %8, !dbg !454
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_1PiS_i(ptr noundef %0, ptr noundef %1, i32 noundef %2) #2 !dbg !149 !pallas.exprWrapper !453 {
  %4 = alloca %pallas.fracT, align 8
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !152, metadata !DIExpression()), !dbg !458
  call void @llvm.dbg.value(metadata ptr %1, metadata !154, metadata !DIExpression()), !dbg !458
  call void @llvm.dbg.value(metadata i32 %2, metadata !148, metadata !DIExpression()), !dbg !458
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 2), !dbg !459
  %6 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %4), !dbg !460
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !461
  %7 = call i1 @pallas.perm(ptr noundef %1, ptr noundef byval(%pallas.fracT) %5), !dbg !462
  %8 = call i1 @pallas.sepConj(i1 %6, i1 %7), !dbg !463
  ret i1 %8, !dbg !458
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_2PiS_ibi(ptr noundef %0, ptr noundef %1, i32 noundef %2, i1 noundef zeroext %3, i32 noundef %4) #2 !dbg !160 !pallas.exprWrapper !453 {
  %6 = alloca %pallas.fracT, align 8
  %7 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !170, metadata !DIExpression()), !dbg !464
  call void @llvm.dbg.value(metadata ptr %1, metadata !172, metadata !DIExpression()), !dbg !464
  call void @llvm.dbg.value(metadata i32 %2, metadata !159, metadata !DIExpression()), !dbg !464
  %8 = zext i1 %3 to i8
  call void @llvm.dbg.value(metadata i8 %8, metadata !165, metadata !DIExpression()), !dbg !464
  call void @llvm.dbg.value(metadata i32 %4, metadata !167, metadata !DIExpression()), !dbg !464
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %6, i32 noundef 1, i32 noundef 2), !dbg !465
  %9 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %6), !dbg !466
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %7, i32 noundef 1, i32 noundef 2), !dbg !467
  %10 = call i1 @pallas.perm(ptr noundef %1, ptr noundef byval(%pallas.fracT) %7), !dbg !468
  %11 = call i1 @pallas.sepConj(i1 %9, i1 %10), !dbg !469
  ret i1 %11, !dbg !464
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_3PiS_ibi(ptr noundef %0, ptr noundef %1, i32 noundef %2, i1 noundef zeroext %3, i32 noundef %4) #2 !dbg !178 !pallas.exprWrapper !453 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !186, metadata !DIExpression()), !dbg !470
  call void @llvm.dbg.value(metadata ptr %1, metadata !188, metadata !DIExpression()), !dbg !470
  call void @llvm.dbg.value(metadata i32 %2, metadata !177, metadata !DIExpression()), !dbg !470
  %6 = zext i1 %3 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !181, metadata !DIExpression()), !dbg !470
  call void @llvm.dbg.value(metadata i32 %4, metadata !183, metadata !DIExpression()), !dbg !470
  %7 = call noundef i32 @"pallas.result noundef i32"(), !dbg !471
  %8 = load i32, ptr %0, align 4, !dbg !472
  %9 = load i32, ptr %1, align 4, !dbg !473
  %10 = call noundef zeroext i1 @_Z5isMaxiii(i32 noundef %7, i32 noundef %8, i32 noundef %9), !dbg !474
  ret i1 %10, !dbg !470
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_4PiS_ibi(ptr noundef %0, ptr noundef %1, i32 noundef %2, i1 noundef zeroext %3, i32 noundef %4) #0 !dbg !194 !pallas.exprWrapper !453 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !202, metadata !DIExpression()), !dbg !475
  call void @llvm.dbg.value(metadata ptr %1, metadata !204, metadata !DIExpression()), !dbg !475
  call void @llvm.dbg.value(metadata i32 %2, metadata !193, metadata !DIExpression()), !dbg !475
  %6 = zext i1 %3 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !197, metadata !DIExpression()), !dbg !475
  call void @llvm.dbg.value(metadata i32 %4, metadata !199, metadata !DIExpression()), !dbg !475
  %7 = load i32, ptr %0, align 4, !dbg !476
  %8 = load i32, ptr %1, align 4, !dbg !477
  %9 = call noundef zeroext i1 @_Z5isMiniii(i32 noundef %4, i32 noundef %7, i32 noundef %8), !dbg !478
  ret i1 %9, !dbg !475
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_5PiS_ibi(ptr noundef %0, ptr noundef %1, i32 noundef %2, i1 noundef zeroext %3, i32 noundef %4) #0 !dbg !210 !pallas.exprWrapper !453 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !218, metadata !DIExpression()), !dbg !479
  call void @llvm.dbg.value(metadata ptr %1, metadata !220, metadata !DIExpression()), !dbg !479
  call void @llvm.dbg.value(metadata i32 %2, metadata !209, metadata !DIExpression()), !dbg !479
  %6 = zext i1 %3 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !213, metadata !DIExpression()), !dbg !479
  call void @llvm.dbg.value(metadata i32 %4, metadata !215, metadata !DIExpression()), !dbg !479
  %7 = trunc i8 %6 to i1, !dbg !480
  %8 = zext i1 %7 to i32, !dbg !480
  %9 = load i32, ptr %0, align 4, !dbg !481
  %10 = icmp sgt i32 %9, %2, !dbg !482
  br i1 %10, label %11, label %14, !dbg !483

11:                                               ; preds = %5
  %12 = load i32, ptr %1, align 4, !dbg !484
  %13 = icmp sgt i32 %12, %2, !dbg !485
  br label %14

14:                                               ; preds = %11, %5
  %15 = phi i1 [ false, %5 ], [ %13, %11 ], !dbg !479
  %16 = zext i1 %15 to i32, !dbg !486
  %17 = icmp eq i32 %8, %16, !dbg !487
  ret i1 %17, !dbg !479
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef i32 @_Z13PALLAS_SPEC_8PiS_ibi(ptr noundef %0, ptr noundef %1, i32 noundef %2, i1 noundef zeroext %3, i32 noundef %4) #0 !dbg !232 !pallas.ghostWrapper !453 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !242, metadata !DIExpression()), !dbg !488
  call void @llvm.dbg.value(metadata ptr %1, metadata !244, metadata !DIExpression()), !dbg !488
  call void @llvm.dbg.value(metadata i32 %2, metadata !231, metadata !DIExpression()), !dbg !488
  %6 = zext i1 %3 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !237, metadata !DIExpression()), !dbg !488
  call void @llvm.dbg.value(metadata i32 %4, metadata !239, metadata !DIExpression()), !dbg !488
  %7 = load i32, ptr %0, align 4, !dbg !489
  %8 = load i32, ptr %1, align 4, !dbg !490
  %9 = icmp sle i32 %7, %8, !dbg !491
  br i1 %9, label %10, label %12, !dbg !489

10:                                               ; preds = %5
  %11 = load i32, ptr %0, align 4, !dbg !492
  br label %14, !dbg !489

12:                                               ; preds = %5
  %13 = load i32, ptr %1, align 4, !dbg !493
  br label %14, !dbg !489

14:                                               ; preds = %12, %10
  %15 = phi i32 [ %11, %10 ], [ %13, %12 ], !dbg !489
  ret i32 %15, !dbg !488
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_9PiS_ibi(ptr noundef %0, ptr noundef %1, i32 noundef %2, i1 noundef zeroext %3, i32 noundef %4) #0 !dbg !250 !pallas.ghostWrapper !453 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !258, metadata !DIExpression()), !dbg !494
  call void @llvm.dbg.value(metadata ptr %1, metadata !260, metadata !DIExpression()), !dbg !494
  call void @llvm.dbg.value(metadata i32 %2, metadata !249, metadata !DIExpression()), !dbg !494
  %6 = zext i1 %3 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !253, metadata !DIExpression()), !dbg !494
  call void @llvm.dbg.value(metadata i32 %4, metadata !255, metadata !DIExpression()), !dbg !494
  %7 = load i32, ptr %0, align 4, !dbg !495
  %8 = icmp sgt i32 %7, %2, !dbg !496
  br i1 %8, label %9, label %12, !dbg !497

9:                                                ; preds = %5
  %10 = load i32, ptr %1, align 4, !dbg !498
  %11 = icmp sgt i32 %10, %2, !dbg !499
  br label %12

12:                                               ; preds = %9, %5
  %13 = phi i1 [ false, %5 ], [ %11, %9 ], !dbg !494
  ret i1 %13, !dbg !494
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_6ib(i32 noundef %0, i1 noundef zeroext %1) #0 !dbg !290 !pallas.exprWrapper !453 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !289, metadata !DIExpression()), !dbg !500
  %3 = zext i1 %1 to i8
  call void @llvm.dbg.value(metadata i8 %3, metadata !294, metadata !DIExpression()), !dbg !500
  %4 = trunc i8 %3 to i1, !dbg !501
  br i1 %4, label %8, label %5, !dbg !502

5:                                                ; preds = %2
  %6 = trunc i8 %3 to i1, !dbg !503
  %7 = xor i1 %6, true, !dbg !504
  br label %8, !dbg !502

8:                                                ; preds = %5, %2
  %9 = phi i1 [ true, %2 ], [ %7, %5 ]
  ret i1 %9, !dbg !500
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_7ib(i32 noundef %0, i1 noundef zeroext %1) #0 !dbg !300 !pallas.exprWrapper !453 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !299, metadata !DIExpression()), !dbg !505
  %3 = zext i1 %1 to i8
  call void @llvm.dbg.value(metadata i8 %3, metadata !302, metadata !DIExpression()), !dbg !505
  %4 = icmp sge i32 %0, 0, !dbg !506
  br i1 %4, label %8, label %5, !dbg !507

5:                                                ; preds = %2
  %6 = sub nsw i32 0, %0, !dbg !508
  %7 = icmp sge i32 %6, 0, !dbg !509
  br label %8, !dbg !507

8:                                                ; preds = %5, %2
  %9 = phi i1 [ true, %2 ], [ %7, %5 ]
  ret i1 %9, !dbg !505
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_10iiiib(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !343 !pallas.exprWrapper !453 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !350, metadata !DIExpression()), !dbg !510
  call void @llvm.dbg.value(metadata i32 %1, metadata !352, metadata !DIExpression()), !dbg !510
  call void @llvm.dbg.value(metadata i32 %2, metadata !354, metadata !DIExpression()), !dbg !510
  call void @llvm.dbg.value(metadata i32 %3, metadata !342, metadata !DIExpression()), !dbg !510
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !347, metadata !DIExpression()), !dbg !510
  %7 = icmp eq i32 %2, 42, !dbg !511
  ret i1 %7, !dbg !510
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_11iiiib(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !360 !pallas.exprWrapper !453 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !365, metadata !DIExpression()), !dbg !512
  call void @llvm.dbg.value(metadata i32 %1, metadata !367, metadata !DIExpression()), !dbg !512
  call void @llvm.dbg.value(metadata i32 %2, metadata !369, metadata !DIExpression()), !dbg !512
  call void @llvm.dbg.value(metadata i32 %3, metadata !359, metadata !DIExpression()), !dbg !512
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !362, metadata !DIExpression()), !dbg !512
  %7 = icmp eq i32 %3, 1, !dbg !513
  ret i1 %7, !dbg !512
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_12iiiib(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !375 !pallas.exprWrapper !453 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !380, metadata !DIExpression()), !dbg !514
  call void @llvm.dbg.value(metadata i32 %1, metadata !382, metadata !DIExpression()), !dbg !514
  call void @llvm.dbg.value(metadata i32 %2, metadata !384, metadata !DIExpression()), !dbg !514
  call void @llvm.dbg.value(metadata i32 %3, metadata !374, metadata !DIExpression()), !dbg !514
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !377, metadata !DIExpression()), !dbg !514
  %7 = trunc i8 %6 to i1, !dbg !515
  %8 = zext i1 %7 to i32, !dbg !515
  %9 = icmp eq i32 %8, 0, !dbg !516
  ret i1 %9, !dbg !514
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_13iiiib(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !416 !pallas.exprWrapper !453 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !421, metadata !DIExpression()), !dbg !517
  call void @llvm.dbg.value(metadata i32 %1, metadata !423, metadata !DIExpression()), !dbg !517
  call void @llvm.dbg.value(metadata i32 %2, metadata !425, metadata !DIExpression()), !dbg !517
  call void @llvm.dbg.value(metadata i32 %3, metadata !415, metadata !DIExpression()), !dbg !517
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !418, metadata !DIExpression()), !dbg !517
  %7 = trunc i8 %6 to i1, !dbg !518
  %8 = zext i1 %7 to i32, !dbg !518
  %9 = icmp eq i32 %8, 1, !dbg !519
  ret i1 %9, !dbg !517
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef i32 @_Z14PALLAS_SPEC_14iiiib(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !317 !pallas.ghostWrapper !453 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !324, metadata !DIExpression()), !dbg !520
  call void @llvm.dbg.value(metadata i32 %1, metadata !326, metadata !DIExpression()), !dbg !520
  call void @llvm.dbg.value(metadata i32 %2, metadata !328, metadata !DIExpression()), !dbg !520
  call void @llvm.dbg.value(metadata i32 %3, metadata !316, metadata !DIExpression()), !dbg !520
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !321, metadata !DIExpression()), !dbg !520
  ret i32 42, !dbg !520
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef i32 @_Z14PALLAS_SPEC_15iiiib(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i1 noundef zeroext %4) #0 !dbg !394 !pallas.ghostWrapper !453 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !399, metadata !DIExpression()), !dbg !521
  call void @llvm.dbg.value(metadata i32 %1, metadata !401, metadata !DIExpression()), !dbg !521
  call void @llvm.dbg.value(metadata i32 %2, metadata !403, metadata !DIExpression()), !dbg !521
  call void @llvm.dbg.value(metadata i32 %3, metadata !393, metadata !DIExpression()), !dbg !521
  %6 = zext i1 %4 to i8
  call void @llvm.dbg.value(metadata i8 %6, metadata !396, metadata !DIExpression()), !dbg !521
  ret i32 -42, !dbg !521
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !522 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !523 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !524 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !525 noundef i32 @"pallas.result noundef i32"()

attributes #0 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { mustprogress noinline uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!0, !2, !101, !103}
!llvm.module.flags = !{!104, !105, !106, !107, !108, !109, !110}
!llvm.ident = !{!111, !111}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/ghost/pallas_cpp_ghost_arg.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "816b5bc3f60d1e56695acca49c4bf85b")
!2 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !3, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, imports: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "8f67aae1e7d397e8a4dfcc012da31eca")
!4 = !{!5, !13, !17, !21, !25, !28, !30, !32, !34, !38, !41, !44, !47, !50, !52, !57, !61, !65, !69, !71, !73, !75, !77, !80, !83, !86, !89, !92, !94, !99}
!5 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !7, file: !12, line: 51)
!6 = !DINamespace(name: "std", scope: null)
!7 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !8, line: 24, baseType: !9)
!8 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!9 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int8_t", file: !10, line: 37, baseType: !11)
!10 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!11 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!12 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstdint", directory: "")
!13 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !14, file: !12, line: 52)
!14 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !8, line: 25, baseType: !15)
!15 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int16_t", file: !10, line: 39, baseType: !16)
!16 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!17 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !18, file: !12, line: 53)
!18 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !8, line: 26, baseType: !19)
!19 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !10, line: 41, baseType: !20)
!20 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!21 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !22, file: !12, line: 54)
!22 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !8, line: 27, baseType: !23)
!23 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !10, line: 44, baseType: !24)
!24 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!25 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !26, file: !12, line: 56)
!26 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !27, line: 47, baseType: !11)
!27 = !DIFile(filename: "/usr/include/stdint.h", directory: "", checksumkind: CSK_MD5, checksum: "bfb03fa9c46a839e35c32b929fbdbb8e")
!28 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !29, file: !12, line: 57)
!29 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !27, line: 49, baseType: !24)
!30 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !31, file: !12, line: 58)
!31 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !27, line: 50, baseType: !24)
!32 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !33, file: !12, line: 59)
!33 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !27, line: 51, baseType: !24)
!34 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !35, file: !12, line: 61)
!35 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !36, line: 25, baseType: !37)
!36 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-least.h", directory: "", checksumkind: CSK_MD5, checksum: "a866be81c480920b0293bd5f6336a0a3")
!37 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least8_t", file: !10, line: 52, baseType: !9)
!38 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !39, file: !12, line: 62)
!39 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !36, line: 26, baseType: !40)
!40 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least16_t", file: !10, line: 54, baseType: !15)
!41 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !42, file: !12, line: 63)
!42 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !36, line: 27, baseType: !43)
!43 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least32_t", file: !10, line: 56, baseType: !19)
!44 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !45, file: !12, line: 64)
!45 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !36, line: 28, baseType: !46)
!46 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least64_t", file: !10, line: 58, baseType: !23)
!47 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !48, file: !12, line: 66)
!48 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !27, line: 90, baseType: !49)
!49 = !DIDerivedType(tag: DW_TAG_typedef, name: "__intmax_t", file: !10, line: 72, baseType: !24)
!50 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !51, file: !12, line: 67)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !27, line: 76, baseType: !24)
!52 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !53, file: !12, line: 69)
!53 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !54, line: 24, baseType: !55)
!54 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-uintn.h", directory: "", checksumkind: CSK_MD5, checksum: "256fcabbefa27ca8cf5e6d37525e6e16")
!55 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !10, line: 38, baseType: !56)
!56 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!57 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !58, file: !12, line: 70)
!58 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !54, line: 25, baseType: !59)
!59 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !10, line: 40, baseType: !60)
!60 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!61 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !62, file: !12, line: 71)
!62 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !54, line: 26, baseType: !63)
!63 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint32_t", file: !10, line: 42, baseType: !64)
!64 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!65 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !66, file: !12, line: 72)
!66 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !54, line: 27, baseType: !67)
!67 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint64_t", file: !10, line: 45, baseType: !68)
!68 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!69 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !70, file: !12, line: 74)
!70 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !27, line: 60, baseType: !56)
!71 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !72, file: !12, line: 75)
!72 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !27, line: 62, baseType: !68)
!73 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !74, file: !12, line: 76)
!74 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !27, line: 63, baseType: !68)
!75 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !76, file: !12, line: 77)
!76 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !27, line: 64, baseType: !68)
!77 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !78, file: !12, line: 79)
!78 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !36, line: 31, baseType: !79)
!79 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least8_t", file: !10, line: 53, baseType: !55)
!80 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !81, file: !12, line: 80)
!81 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !36, line: 32, baseType: !82)
!82 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least16_t", file: !10, line: 55, baseType: !59)
!83 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !84, file: !12, line: 81)
!84 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !36, line: 33, baseType: !85)
!85 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least32_t", file: !10, line: 57, baseType: !63)
!86 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !87, file: !12, line: 82)
!87 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !36, line: 34, baseType: !88)
!88 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least64_t", file: !10, line: 59, baseType: !67)
!89 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !90, file: !12, line: 84)
!90 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !27, line: 91, baseType: !91)
!91 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uintmax_t", file: !10, line: 73, baseType: !68)
!92 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !93, file: !12, line: 85)
!93 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !27, line: 79, baseType: !68)
!94 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !95, file: !98, line: 58)
!95 = !DIDerivedType(tag: DW_TAG_typedef, name: "max_align_t", file: !96, line: 24, baseType: !97)
!96 = !DIFile(filename: "/usr/lib/llvm-17/lib/clang/17/include/__stddef_max_align_t.h", directory: "", checksumkind: CSK_MD5, checksum: "48e8e2456f77e6cda35d245130fa7259")
!97 = !DICompositeType(tag: DW_TAG_structure_type, file: !96, line: 19, size: 256, flags: DIFlagFwdDecl, identifier: "_ZTS11max_align_t")
!98 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstddef", directory: "")
!99 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !2, entity: !100, file: !3, line: 6)
!100 = !DINamespace(name: "pallasSpec", scope: null)
!101 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus, file: !102, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!102 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/ghost/pallas_cpp_ghost_arg.cpp", directory: "")
!103 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus, file: !102, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!104 = !{i32 7, !"Dwarf Version", i32 5}
!105 = !{i32 2, !"Debug Info Version", i32 3}
!106 = !{i32 1, !"wchar_size", i32 4}
!107 = !{i32 8, !"PIC Level", i32 2}
!108 = !{i32 7, !"PIE Level", i32 2}
!109 = !{i32 7, !"uwtable", i32 2}
!110 = !{i32 7, !"frame-pointer", i32 2}
!111 = !{!"Ubuntu clang version 17.0.6 (9ubuntu1)"}
!112 = distinct !DISubprogram(name: "get_max", linkageName: "_Z7get_maxPiS_", scope: !1, file: !1, line: 30, type: !113, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!113 = !DISubroutineType(types: !114)
!114 = !{!20, !115, !115}
!115 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !20, size: 64)
!116 = !{}
!117 = !{!118, i1 false, i1 false, !120, !123, !128, !144, !155, !173, !189, !205}
!118 = !{!"pallas.srcLoc", i64 17, i64 1, i64 29, i64 1, !119}
!119 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/ghost/pallas_cpp_ghost_arg.cpp", directory: "", checksumkind: CSK_MD5, checksum: "816b5bc3f60d1e56695acca49c4bf85b")
!120 = !{!121}
!121 = !{!122, !"x"}
!122 = !{!"pallas.srcLoc", i64 18, i64 1, i64 18, i64 13, !119}
!123 = !{!124, !126}
!124 = !{!125, !"both_gt_x"}
!125 = !{!"pallas.srcLoc", i64 19, i64 1, i64 19, i64 22, !119}
!126 = !{!127, !"min"}
!127 = !{!"pallas.srcLoc", i64 20, i64 1, i64 20, i64 15, !119}
!128 = !{!"pallas.requires", !129, ptr @_Z13PALLAS_SPEC_0PiS_i, !130, !116, !137}
!129 = !{!"pallas.srcLoc", i64 21, i64 1, i64 21, i64 38, !119}
!130 = !{!131}
!131 = !{!121, !132}
!132 = !DILocalVariable(name: "x", arg: 3, scope: !133, file: !1, line: 21, type: !20)
!133 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_Z13PALLAS_SPEC_0PiS_i", scope: !1, file: !1, line: 21, type: !134, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!134 = !DISubroutineType(types: !135)
!135 = !{!136, !115, !115, !20}
!136 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!137 = !{!138, !141}
!138 = !{!139, !140}
!139 = !DILocalVariable(name: "a", arg: 1, scope: !112, file: !1, line: 30, type: !115)
!140 = !DILocalVariable(name: "a", arg: 1, scope: !133, file: !1, line: 21, type: !115)
!141 = !{!142, !143}
!142 = !DILocalVariable(name: "b", arg: 2, scope: !112, file: !1, line: 30, type: !115)
!143 = !DILocalVariable(name: "b", arg: 2, scope: !133, file: !1, line: 21, type: !115)
!144 = !{!"pallas.requires", !145, ptr @_Z13PALLAS_SPEC_1PiS_i, !146, !116, !150}
!145 = !{!"pallas.srcLoc", i64 22, i64 1, i64 23, i64 39, !119}
!146 = !{!147}
!147 = !{!121, !148}
!148 = !DILocalVariable(name: "x", arg: 3, scope: !149, file: !1, line: 22, type: !20)
!149 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_Z13PALLAS_SPEC_1PiS_i", scope: !1, file: !1, line: 22, type: !134, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!150 = !{!151, !153}
!151 = !{!139, !152}
!152 = !DILocalVariable(name: "a", arg: 1, scope: !149, file: !1, line: 22, type: !115)
!153 = !{!142, !154}
!154 = !DILocalVariable(name: "b", arg: 2, scope: !149, file: !1, line: 22, type: !115)
!155 = !{!"pallas.ensures", !156, ptr @_Z13PALLAS_SPEC_2PiS_ibi, !157, !163, !168}
!156 = !{!"pallas.srcLoc", i64 24, i64 1, i64 25, i64 38, !119}
!157 = !{!158}
!158 = !{!121, !159}
!159 = !DILocalVariable(name: "x", arg: 3, scope: !160, file: !1, line: 24, type: !20)
!160 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_Z13PALLAS_SPEC_2PiS_ibi", scope: !1, file: !1, line: 24, type: !161, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!161 = !DISubroutineType(types: !162)
!162 = !{!136, !115, !115, !20, !136, !20}
!163 = !{!164, !166}
!164 = !{!124, !165}
!165 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !160, file: !1, line: 24, type: !136)
!166 = !{!126, !167}
!167 = !DILocalVariable(name: "min", arg: 5, scope: !160, file: !1, line: 24, type: !20)
!168 = !{!169, !171}
!169 = !{!139, !170}
!170 = !DILocalVariable(name: "a", arg: 1, scope: !160, file: !1, line: 24, type: !115)
!171 = !{!142, !172}
!172 = !DILocalVariable(name: "b", arg: 2, scope: !160, file: !1, line: 24, type: !115)
!173 = !{!"pallas.ensures", !174, ptr @_Z13PALLAS_SPEC_3PiS_ibi, !175, !179, !184}
!174 = !{!"pallas.srcLoc", i64 26, i64 1, i64 26, i64 38, !119}
!175 = !{!176}
!176 = !{!121, !177}
!177 = !DILocalVariable(name: "x", arg: 3, scope: !178, file: !1, line: 26, type: !20)
!178 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_Z13PALLAS_SPEC_3PiS_ibi", scope: !1, file: !1, line: 26, type: !161, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!179 = !{!180, !182}
!180 = !{!124, !181}
!181 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !178, file: !1, line: 26, type: !136)
!182 = !{!126, !183}
!183 = !DILocalVariable(name: "min", arg: 5, scope: !178, file: !1, line: 26, type: !20)
!184 = !{!185, !187}
!185 = !{!139, !186}
!186 = !DILocalVariable(name: "a", arg: 1, scope: !178, file: !1, line: 26, type: !115)
!187 = !{!142, !188}
!188 = !DILocalVariable(name: "b", arg: 2, scope: !178, file: !1, line: 26, type: !115)
!189 = !{!"pallas.ensures", !190, ptr @_Z13PALLAS_SPEC_4PiS_ibi, !191, !195, !200}
!190 = !{!"pallas.srcLoc", i64 27, i64 1, i64 27, i64 27, !119}
!191 = !{!192}
!192 = !{!121, !193}
!193 = !DILocalVariable(name: "x", arg: 3, scope: !194, file: !1, line: 27, type: !20)
!194 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_Z13PALLAS_SPEC_4PiS_ibi", scope: !1, file: !1, line: 27, type: !161, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!195 = !{!196, !198}
!196 = !{!124, !197}
!197 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !194, file: !1, line: 27, type: !136)
!198 = !{!126, !199}
!199 = !DILocalVariable(name: "min", arg: 5, scope: !194, file: !1, line: 27, type: !20)
!200 = !{!201, !203}
!201 = !{!139, !202}
!202 = !DILocalVariable(name: "a", arg: 1, scope: !194, file: !1, line: 27, type: !115)
!203 = !{!142, !204}
!204 = !DILocalVariable(name: "b", arg: 2, scope: !194, file: !1, line: 27, type: !115)
!205 = !{!"pallas.ensures", !206, ptr @_Z13PALLAS_SPEC_5PiS_ibi, !207, !211, !216}
!206 = !{!"pallas.srcLoc", i64 28, i64 1, i64 28, i64 40, !119}
!207 = !{!208}
!208 = !{!121, !209}
!209 = !DILocalVariable(name: "x", arg: 3, scope: !210, file: !1, line: 28, type: !20)
!210 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_Z13PALLAS_SPEC_5PiS_ibi", scope: !1, file: !1, line: 28, type: !161, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!211 = !{!212, !214}
!212 = !{!124, !213}
!213 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !210, file: !1, line: 28, type: !136)
!214 = !{!126, !215}
!215 = !DILocalVariable(name: "min", arg: 5, scope: !210, file: !1, line: 28, type: !20)
!216 = !{!217, !219}
!217 = !{!139, !218}
!218 = !DILocalVariable(name: "a", arg: 1, scope: !210, file: !1, line: 28, type: !115)
!219 = !{!142, !220}
!220 = !DILocalVariable(name: "b", arg: 2, scope: !210, file: !1, line: 28, type: !115)
!221 = !DILocation(line: 30, column: 18, scope: !112)
!222 = !DILocation(line: 30, column: 26, scope: !112)
!223 = !DILocation(line: 35, column: 10, scope: !224)
!224 = distinct !DILexicalBlock(scope: !112, file: !1, line: 35, column: 9)
!225 = !{!226, !227, !245}
!226 = !{!"pallas.srcLoc", i64 31, i64 5, i64 34, i64 5, !119}
!227 = !{!"pallas.gAssign", !228, ptr @_Z13PALLAS_SPEC_8PiS_ibi, !229, !235, !240, !126}
!228 = !{!"pallas.srcLoc", i64 32, i64 5, i64 32, i64 42, !119}
!229 = !{!230}
!230 = !{!121, !231}
!231 = !DILocalVariable(name: "x", arg: 3, scope: !232, file: !1, line: 32, type: !20)
!232 = distinct !DISubprogram(name: "PALLAS_SPEC_8", linkageName: "_Z13PALLAS_SPEC_8PiS_ibi", scope: !1, file: !1, line: 32, type: !233, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!233 = !DISubroutineType(types: !234)
!234 = !{!20, !115, !115, !20, !136, !20}
!235 = !{!236, !238}
!236 = !{!124, !237}
!237 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !232, file: !1, line: 32, type: !136)
!238 = !{!126, !239}
!239 = !DILocalVariable(name: "min", arg: 5, scope: !232, file: !1, line: 32, type: !20)
!240 = !{!241, !243}
!241 = !{!139, !242}
!242 = !DILocalVariable(name: "a", arg: 1, scope: !232, file: !1, line: 32, type: !115)
!243 = !{!142, !244}
!244 = !DILocalVariable(name: "b", arg: 2, scope: !232, file: !1, line: 32, type: !115)
!245 = !{!"pallas.gAssign", !246, ptr @_Z13PALLAS_SPEC_9PiS_ibi, !247, !251, !256, !124}
!246 = !{!"pallas.srcLoc", i64 33, i64 5, i64 33, i64 48, !119}
!247 = !{!248}
!248 = !{!121, !249}
!249 = !DILocalVariable(name: "x", arg: 3, scope: !250, file: !1, line: 33, type: !20)
!250 = distinct !DISubprogram(name: "PALLAS_SPEC_9", linkageName: "_Z13PALLAS_SPEC_9PiS_ibi", scope: !1, file: !1, line: 33, type: !161, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!251 = !{!252, !254}
!252 = !{!124, !253}
!253 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !250, file: !1, line: 33, type: !136)
!254 = !{!126, !255}
!255 = !DILocalVariable(name: "min", arg: 5, scope: !250, file: !1, line: 33, type: !20)
!256 = !{!257, !259}
!257 = !{!139, !258}
!258 = !DILocalVariable(name: "a", arg: 1, scope: !250, file: !1, line: 33, type: !115)
!259 = !{!142, !260}
!260 = !DILocalVariable(name: "b", arg: 2, scope: !250, file: !1, line: 33, type: !115)
!261 = !DILocation(line: 35, column: 9, scope: !224)
!262 = !DILocation(line: 35, column: 15, scope: !224)
!263 = !DILocation(line: 35, column: 14, scope: !224)
!264 = !DILocation(line: 35, column: 12, scope: !224)
!265 = !DILocation(line: 35, column: 9, scope: !112)
!266 = !DILocation(line: 36, column: 17, scope: !267)
!267 = distinct !DILexicalBlock(scope: !224, file: !1, line: 35, column: 18)
!268 = !DILocation(line: 36, column: 16, scope: !267)
!269 = !DILocation(line: 36, column: 9, scope: !267)
!270 = !DILocation(line: 38, column: 17, scope: !271)
!271 = distinct !DILexicalBlock(scope: !224, file: !1, line: 37, column: 12)
!272 = !DILocation(line: 38, column: 16, scope: !271)
!273 = !DILocation(line: 38, column: 9, scope: !271)
!274 = !DILocation(line: 40, column: 1, scope: !112)
!275 = distinct !DISubprogram(name: "run", linkageName: "_Z3runv", scope: !1, file: !1, line: 53, type: !276, scopeLine: 53, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!276 = !DISubroutineType(types: !277)
!277 = !{null}
!278 = !{!279, i1 false, i1 false, !116, !280, !285, !295}
!279 = !{!"pallas.srcLoc", i64 47, i64 1, i64 52, i64 1, !119}
!280 = !{!281, !283}
!281 = !{!282, !"min"}
!282 = !{!"pallas.srcLoc", i64 48, i64 1, i64 48, i64 15, !119}
!283 = !{!284, !"both_gt"}
!284 = !{!"pallas.srcLoc", i64 49, i64 1, i64 49, i64 20, !119}
!285 = !{!"pallas.ensures", !286, ptr @_Z13PALLAS_SPEC_6ib, !116, !287, !116}
!286 = !{!"pallas.srcLoc", i64 50, i64 1, i64 50, i64 28, !119}
!287 = !{!288, !293}
!288 = !{!281, !289}
!289 = !DILocalVariable(name: "min", arg: 1, scope: !290, file: !1, line: 50, type: !20)
!290 = distinct !DISubprogram(name: "PALLAS_SPEC_6", linkageName: "_Z13PALLAS_SPEC_6ib", scope: !1, file: !1, line: 50, type: !291, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!291 = !DISubroutineType(types: !292)
!292 = !{!136, !20, !136}
!293 = !{!283, !294}
!294 = !DILocalVariable(name: "both_gt", arg: 2, scope: !290, file: !1, line: 50, type: !136)
!295 = !{!"pallas.ensures", !296, ptr @_Z13PALLAS_SPEC_7ib, !116, !297, !116}
!296 = !{!"pallas.srcLoc", i64 51, i64 1, i64 51, i64 30, !119}
!297 = !{!298, !301}
!298 = !{!281, !299}
!299 = !DILocalVariable(name: "min", arg: 1, scope: !300, file: !1, line: 51, type: !20)
!300 = distinct !DISubprogram(name: "PALLAS_SPEC_7", linkageName: "_Z13PALLAS_SPEC_7ib", scope: !1, file: !1, line: 51, type: !291, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!301 = !{!283, !302}
!302 = !DILocalVariable(name: "both_gt", arg: 2, scope: !300, file: !1, line: 51, type: !136)
!303 = !DILocalVariable(name: "a", scope: !275, file: !1, line: 54, type: !20)
!304 = !DILocation(line: 54, column: 9, scope: !275)
!305 = !DILocalVariable(name: "b", scope: !275, file: !1, line: 55, type: !20)
!306 = !DILocation(line: 55, column: 9, scope: !275)
!307 = !DILocalVariable(name: "max", scope: !275, file: !1, line: 57, type: !20)
!308 = !DILocation(line: 57, column: 9, scope: !275)
!309 = !DILocation(line: 57, column: 15, scope: !275)
!310 = !{!311, !312}
!311 = !{!"pallas.srcLoc", i64 57, i64 23, i64 57, i64 41, !119}
!312 = !{!"pallas.givenBinding", !313, ptr @_Z14PALLAS_SPEC_14iiiib, !116, !314, !322, !121}
!313 = !{!"pallas.srcLoc", i64 57, i64 33, i64 57, i64 39, !119}
!314 = !{!315, !320}
!315 = !{!281, !316}
!316 = !DILocalVariable(name: "min", arg: 4, scope: !317, file: !1, line: 57, type: !20)
!317 = distinct !DISubprogram(name: "PALLAS_SPEC_14", linkageName: "_Z14PALLAS_SPEC_14iiiib", scope: !1, file: !1, line: 57, type: !318, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!318 = !DISubroutineType(types: !319)
!319 = !{!20, !20, !20, !20, !20, !136}
!320 = !{!283, !321}
!321 = !DILocalVariable(name: "both_gt", arg: 5, scope: !317, file: !1, line: 57, type: !136)
!322 = !{!323, !325, !327}
!323 = !{!303, !324}
!324 = !DILocalVariable(name: "a", arg: 1, scope: !317, file: !1, line: 57, type: !20)
!325 = !{!305, !326}
!326 = !DILocalVariable(name: "b", arg: 2, scope: !317, file: !1, line: 57, type: !20)
!327 = !{!307, !328}
!328 = !DILocalVariable(name: "max", arg: 3, scope: !317, file: !1, line: 57, type: !20)
!329 = !{!330, !331, !333}
!330 = !{!"pallas.srcLoc", i64 57, i64 45, i64 57, i64 88, !119}
!331 = !{!"pallas.yieldsBinding", !332, !281, !126}
!332 = !{!"pallas.srcLoc", i64 57, i64 56, i64 57, i64 65, !119}
!333 = !{!"pallas.yieldsBinding", !334, !283, !124}
!334 = !{!"pallas.srcLoc", i64 57, i64 67, i64 57, i64 86, !119}
!335 = !DILocation(line: 64, column: 7, scope: !275)
!336 = !{!337, !338, !355, !370}
!337 = !{!"pallas.srcLoc", i64 58, i64 5, i64 62, i64 5, !119}
!338 = !{!"pallas.assert", !339, ptr @_Z14PALLAS_SPEC_10iiiib, !116, !340, !348}
!339 = !{!"pallas.srcLoc", i64 59, i64 5, i64 59, i64 21, !119}
!340 = !{!341, !346}
!341 = !{!281, !342}
!342 = !DILocalVariable(name: "min", arg: 4, scope: !343, file: !1, line: 59, type: !20)
!343 = distinct !DISubprogram(name: "PALLAS_SPEC_10", linkageName: "_Z14PALLAS_SPEC_10iiiib", scope: !1, file: !1, line: 59, type: !344, scopeLine: 59, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!344 = !DISubroutineType(types: !345)
!345 = !{!136, !20, !20, !20, !20, !136}
!346 = !{!283, !347}
!347 = !DILocalVariable(name: "both_gt", arg: 5, scope: !343, file: !1, line: 59, type: !136)
!348 = !{!349, !351, !353}
!349 = !{!303, !350}
!350 = !DILocalVariable(name: "a", arg: 1, scope: !343, file: !1, line: 59, type: !20)
!351 = !{!305, !352}
!352 = !DILocalVariable(name: "b", arg: 2, scope: !343, file: !1, line: 59, type: !20)
!353 = !{!307, !354}
!354 = !DILocalVariable(name: "max", arg: 3, scope: !343, file: !1, line: 59, type: !20)
!355 = !{!"pallas.assert", !356, ptr @_Z14PALLAS_SPEC_11iiiib, !116, !357, !363}
!356 = !{!"pallas.srcLoc", i64 60, i64 5, i64 60, i64 20, !119}
!357 = !{!358, !361}
!358 = !{!281, !359}
!359 = !DILocalVariable(name: "min", arg: 4, scope: !360, file: !1, line: 60, type: !20)
!360 = distinct !DISubprogram(name: "PALLAS_SPEC_11", linkageName: "_Z14PALLAS_SPEC_11iiiib", scope: !1, file: !1, line: 60, type: !344, scopeLine: 60, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!361 = !{!283, !362}
!362 = !DILocalVariable(name: "both_gt", arg: 5, scope: !360, file: !1, line: 60, type: !136)
!363 = !{!364, !366, !368}
!364 = !{!303, !365}
!365 = !DILocalVariable(name: "a", arg: 1, scope: !360, file: !1, line: 60, type: !20)
!366 = !{!305, !367}
!367 = !DILocalVariable(name: "b", arg: 2, scope: !360, file: !1, line: 60, type: !20)
!368 = !{!307, !369}
!369 = !DILocalVariable(name: "max", arg: 3, scope: !360, file: !1, line: 60, type: !20)
!370 = !{!"pallas.assert", !371, ptr @_Z14PALLAS_SPEC_12iiiib, !116, !372, !378}
!371 = !{!"pallas.srcLoc", i64 61, i64 5, i64 61, i64 28, !119}
!372 = !{!373, !376}
!373 = !{!281, !374}
!374 = !DILocalVariable(name: "min", arg: 4, scope: !375, file: !1, line: 61, type: !20)
!375 = distinct !DISubprogram(name: "PALLAS_SPEC_12", linkageName: "_Z14PALLAS_SPEC_12iiiib", scope: !1, file: !1, line: 61, type: !344, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!376 = !{!283, !377}
!377 = !DILocalVariable(name: "both_gt", arg: 5, scope: !375, file: !1, line: 61, type: !136)
!378 = !{!379, !381, !383}
!379 = !{!303, !380}
!380 = !DILocalVariable(name: "a", arg: 1, scope: !375, file: !1, line: 61, type: !20)
!381 = !{!305, !382}
!382 = !DILocalVariable(name: "b", arg: 2, scope: !375, file: !1, line: 61, type: !20)
!383 = !{!307, !384}
!384 = !DILocalVariable(name: "max", arg: 3, scope: !375, file: !1, line: 61, type: !20)
!385 = !DILocation(line: 65, column: 7, scope: !275)
!386 = !DILocation(line: 67, column: 5, scope: !275)
!387 = !{!388, !389}
!388 = !{!"pallas.srcLoc", i64 67, i64 13, i64 67, i64 32, !119}
!389 = !{!"pallas.givenBinding", !390, ptr @_Z14PALLAS_SPEC_15iiiib, !116, !391, !397, !121}
!390 = !{!"pallas.srcLoc", i64 67, i64 23, i64 67, i64 30, !119}
!391 = !{!392, !395}
!392 = !{!281, !393}
!393 = !DILocalVariable(name: "min", arg: 4, scope: !394, file: !1, line: 67, type: !20)
!394 = distinct !DISubprogram(name: "PALLAS_SPEC_15", linkageName: "_Z14PALLAS_SPEC_15iiiib", scope: !1, file: !1, line: 67, type: !318, scopeLine: 67, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!395 = !{!283, !396}
!396 = !DILocalVariable(name: "both_gt", arg: 5, scope: !394, file: !1, line: 67, type: !136)
!397 = !{!398, !400, !402}
!398 = !{!303, !399}
!399 = !DILocalVariable(name: "a", arg: 1, scope: !394, file: !1, line: 67, type: !20)
!400 = !{!305, !401}
!401 = !DILocalVariable(name: "b", arg: 2, scope: !394, file: !1, line: 67, type: !20)
!402 = !{!307, !403}
!403 = !DILocalVariable(name: "max", arg: 3, scope: !394, file: !1, line: 67, type: !20)
!404 = !{!405, !406}
!405 = !{!"pallas.srcLoc", i64 67, i64 36, i64 67, i64 68, !119}
!406 = !{!"pallas.yieldsBinding", !407, !283, !124}
!407 = !{!"pallas.srcLoc", i64 67, i64 47, i64 67, i64 66, !119}
!408 = !DILocation(line: 73, column: 1, scope: !275)
!409 = !{!410, !411}
!410 = !{!"pallas.srcLoc", i64 69, i64 5, i64 71, i64 5, !119}
!411 = !{!"pallas.assert", !412, ptr @_Z14PALLAS_SPEC_13iiiib, !116, !413, !419}
!412 = !{!"pallas.srcLoc", i64 70, i64 5, i64 70, i64 27, !119}
!413 = !{!414, !417}
!414 = !{!281, !415}
!415 = !DILocalVariable(name: "min", arg: 4, scope: !416, file: !1, line: 70, type: !20)
!416 = distinct !DISubprogram(name: "PALLAS_SPEC_13", linkageName: "_Z14PALLAS_SPEC_13iiiib", scope: !1, file: !1, line: 70, type: !344, scopeLine: 70, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !116)
!417 = !{!283, !418}
!418 = !DILocalVariable(name: "both_gt", arg: 5, scope: !416, file: !1, line: 70, type: !136)
!419 = !{!420, !422, !424}
!420 = !{!303, !421}
!421 = !DILocalVariable(name: "a", arg: 1, scope: !416, file: !1, line: 70, type: !20)
!422 = !{!305, !423}
!423 = !DILocalVariable(name: "b", arg: 2, scope: !416, file: !1, line: 70, type: !20)
!424 = !{!307, !425}
!425 = !DILocalVariable(name: "max", arg: 3, scope: !416, file: !1, line: 70, type: !20)
!426 = distinct !DISubprogram(name: "isMax", linkageName: "_Z5isMaxiii", scope: !102, file: !102, line: 11, type: !427, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !101, retainedNodes: !116)
!427 = !DISubroutineType(types: !428)
!428 = !{!136, !20, !20, !20}
!429 = !{i1 true}
!430 = !DILocalVariable(name: "max", arg: 1, scope: !426, file: !102, line: 11, type: !20)
!431 = !DILocation(line: 0, scope: !426)
!432 = !DILocalVariable(name: "a", arg: 2, scope: !426, file: !102, line: 11, type: !20)
!433 = !DILocalVariable(name: "b", arg: 3, scope: !426, file: !102, line: 11, type: !20)
!434 = !DILocation(line: 11, column: 55, scope: !426)
!435 = !DILocation(line: 11, column: 60, scope: !426)
!436 = !DILocation(line: 11, column: 67, scope: !426)
!437 = !DILocation(line: 11, column: 73, scope: !426)
!438 = !DILocation(line: 12, column: 55, scope: !426)
!439 = !DILocation(line: 12, column: 60, scope: !426)
!440 = !DILocation(line: 12, column: 67, scope: !426)
!441 = distinct !DISubprogram(name: "isMin", linkageName: "_Z5isMiniii", scope: !102, file: !102, line: 13, type: !427, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !103, retainedNodes: !116)
!442 = !DILocalVariable(name: "min", arg: 1, scope: !441, file: !102, line: 13, type: !20)
!443 = !DILocation(line: 0, scope: !441)
!444 = !DILocalVariable(name: "a", arg: 2, scope: !441, file: !102, line: 13, type: !20)
!445 = !DILocalVariable(name: "b", arg: 3, scope: !441, file: !102, line: 13, type: !20)
!446 = !DILocation(line: 13, column: 55, scope: !441)
!447 = !DILocation(line: 13, column: 60, scope: !441)
!448 = !DILocation(line: 13, column: 67, scope: !441)
!449 = !DILocation(line: 13, column: 73, scope: !441)
!450 = !DILocation(line: 14, column: 55, scope: !441)
!451 = !DILocation(line: 14, column: 60, scope: !441)
!452 = !DILocation(line: 14, column: 67, scope: !441)
!453 = !{!""}
!454 = !DILocation(line: 0, scope: !133)
!455 = !DILocation(line: 21, column: 12, scope: !133)
!456 = !DILocation(line: 21, column: 23, scope: !133)
!457 = !DILocation(line: 21, column: 28, scope: !133)
!458 = !DILocation(line: 0, scope: !149)
!459 = !DILocation(line: 22, column: 24, scope: !149)
!460 = !DILocation(line: 22, column: 15, scope: !149)
!461 = !DILocation(line: 23, column: 24, scope: !149)
!462 = !DILocation(line: 23, column: 15, scope: !149)
!463 = !DILocation(line: 22, column: 10, scope: !149)
!464 = !DILocation(line: 0, scope: !160)
!465 = !DILocation(line: 24, column: 23, scope: !160)
!466 = !DILocation(line: 24, column: 14, scope: !160)
!467 = !DILocation(line: 25, column: 23, scope: !160)
!468 = !DILocation(line: 25, column: 14, scope: !160)
!469 = !DILocation(line: 24, column: 9, scope: !160)
!470 = !DILocation(line: 0, scope: !178)
!471 = !DILocation(line: 26, column: 15, scope: !178)
!472 = !DILocation(line: 26, column: 31, scope: !178)
!473 = !DILocation(line: 26, column: 35, scope: !178)
!474 = !DILocation(line: 26, column: 9, scope: !178)
!475 = !DILocation(line: 0, scope: !194)
!476 = !DILocation(line: 27, column: 20, scope: !194)
!477 = !DILocation(line: 27, column: 24, scope: !194)
!478 = !DILocation(line: 27, column: 9, scope: !194)
!479 = !DILocation(line: 0, scope: !210)
!480 = !DILocation(line: 28, column: 9, scope: !210)
!481 = !DILocation(line: 28, column: 23, scope: !210)
!482 = !DILocation(line: 28, column: 26, scope: !210)
!483 = !DILocation(line: 28, column: 30, scope: !210)
!484 = !DILocation(line: 28, column: 33, scope: !210)
!485 = !DILocation(line: 28, column: 36, scope: !210)
!486 = !DILocation(line: 28, column: 22, scope: !210)
!487 = !DILocation(line: 28, column: 19, scope: !210)
!488 = !DILocation(line: 0, scope: !232)
!489 = !DILocation(line: 32, column: 24, scope: !232)
!490 = !DILocation(line: 32, column: 30, scope: !232)
!491 = !DILocation(line: 32, column: 27, scope: !232)
!492 = !DILocation(line: 32, column: 35, scope: !232)
!493 = !DILocation(line: 32, column: 40, scope: !232)
!494 = !DILocation(line: 0, scope: !250)
!495 = !DILocation(line: 33, column: 31, scope: !250)
!496 = !DILocation(line: 33, column: 34, scope: !250)
!497 = !DILocation(line: 33, column: 38, scope: !250)
!498 = !DILocation(line: 33, column: 41, scope: !250)
!499 = !DILocation(line: 33, column: 44, scope: !250)
!500 = !DILocation(line: 0, scope: !290)
!501 = !DILocation(line: 50, column: 9, scope: !290)
!502 = !DILocation(line: 50, column: 17, scope: !290)
!503 = !DILocation(line: 50, column: 21, scope: !290)
!504 = !DILocation(line: 50, column: 20, scope: !290)
!505 = !DILocation(line: 0, scope: !300)
!506 = !DILocation(line: 51, column: 13, scope: !300)
!507 = !DILocation(line: 51, column: 18, scope: !300)
!508 = !DILocation(line: 51, column: 21, scope: !300)
!509 = !DILocation(line: 51, column: 26, scope: !300)
!510 = !DILocation(line: 0, scope: !343)
!511 = !DILocation(line: 59, column: 16, scope: !343)
!512 = !DILocation(line: 0, scope: !360)
!513 = !DILocation(line: 60, column: 16, scope: !360)
!514 = !DILocation(line: 0, scope: !375)
!515 = !DILocation(line: 61, column: 12, scope: !375)
!516 = !DILocation(line: 61, column: 20, scope: !375)
!517 = !DILocation(line: 0, scope: !416)
!518 = !DILocation(line: 70, column: 12, scope: !416)
!519 = !DILocation(line: 70, column: 20, scope: !416)
!520 = !DILocation(line: 0, scope: !317)
!521 = !DILocation(line: 0, scope: !394)
!522 = !{!"pallas.sepConj"}
!523 = !{!"pallas.perm"}
!524 = !{!"pallas.fracOf"}
!525 = !{!"pallas.result"}
