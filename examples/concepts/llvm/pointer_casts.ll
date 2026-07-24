; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pointer_casts.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.B = type { %struct.A }
%struct.A = type { i32, i8 }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [19 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_15, ptr @PALLAS_SPEC_16, ptr @PALLAS_SPEC_17, ptr @PALLAS_SPEC_18], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local void @canCastToInteger() #0 !dbg !17 {
  %1 = alloca %struct.B, align 4
  %2 = alloca ptr, align 8
  call void @llvm.dbg.declare(metadata ptr %1, metadata !21, metadata !DIExpression()), !dbg !30
  %3 = getelementptr inbounds %struct.B, ptr %1, i32 0, i32 0, !dbg !31
  %4 = getelementptr inbounds %struct.A, ptr %3, i32 0, i32 0, !dbg !32
  store i32 5, ptr %4, align 4, !dbg !33
  call void @llvm.dbg.declare(metadata ptr %2, metadata !34, metadata !DIExpression()), !dbg !35
  store ptr %1, ptr %2, align 8, !dbg !35
  %5 = load ptr, ptr %2, align 8, !dbg !36, !pallas.stmntBlock !37
  store i32 10, ptr %5, align 4, !dbg !74
  ret void, !dbg !75, !pallas.stmntBlock !76
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @castRemainsValidInLoop() #0 !dbg !86 {
  %1 = alloca %struct.B, align 4
  %2 = alloca ptr, align 8
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  call void @llvm.dbg.declare(metadata ptr %1, metadata !87, metadata !DIExpression()), !dbg !88
  %5 = getelementptr inbounds %struct.B, ptr %1, i32 0, i32 0, !dbg !89
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !90
  store i32 10, ptr %6, align 4, !dbg !91
  call void @llvm.dbg.declare(metadata ptr %2, metadata !92, metadata !DIExpression()), !dbg !93
  store ptr %1, ptr %2, align 8, !dbg !93
  call void @llvm.dbg.declare(metadata ptr %3, metadata !94, metadata !DIExpression()), !dbg !96
  store i32 0, ptr %3, align 4, !dbg !96
  br label %7, !dbg !97

7:                                                ; preds = %15, %0
  %8 = load i32, ptr %3, align 4, !dbg !98
  %9 = icmp slt i32 %8, 10, !dbg !100
  br i1 %9, label %10, label %18, !dbg !101

10:                                               ; preds = %7
  %11 = load ptr, ptr %2, align 8, !dbg !102
  %12 = load i32, ptr %11, align 4, !dbg !104
  %13 = sub nsw i32 %12, 1, !dbg !105
  %14 = load ptr, ptr %2, align 8, !dbg !106
  store i32 %13, ptr %14, align 4, !dbg !107
  br label %15, !dbg !108

15:                                               ; preds = %10
  %16 = load i32, ptr %3, align 4, !dbg !109
  %17 = add nsw i32 %16, 1, !dbg !109
  store i32 %17, ptr %3, align 4, !dbg !109
  br label %7, !dbg !110, !llvm.loop !111

18:                                               ; preds = %7
  %19 = getelementptr inbounds %struct.B, ptr %1, i32 0, i32 0, !dbg !158, !pallas.stmntBlock !159
  %20 = getelementptr inbounds %struct.A, ptr %19, i32 0, i32 0, !dbg !169
  store i32 10, ptr %20, align 4, !dbg !170
  call void @llvm.dbg.declare(metadata ptr %4, metadata !171, metadata !DIExpression()), !dbg !173
  store i32 0, ptr %4, align 4, !dbg !173
  br label %21, !dbg !174

21:                                               ; preds = %29, %18
  %22 = load i32, ptr %4, align 4, !dbg !175
  %23 = icmp slt i32 %22, 10, !dbg !177
  br i1 %23, label %24, label %32, !dbg !178

24:                                               ; preds = %21
  %25 = load ptr, ptr %2, align 8, !dbg !179
  %26 = load i32, ptr %25, align 4, !dbg !181
  %27 = sub nsw i32 %26, 1, !dbg !182
  %28 = load ptr, ptr %2, align 8, !dbg !183
  store i32 %27, ptr %28, align 4, !dbg !184
  br label %29, !dbg !185

29:                                               ; preds = %24
  %30 = load i32, ptr %4, align 4, !dbg !186
  %31 = add nsw i32 %30, 1, !dbg !186
  store i32 %31, ptr %4, align 4, !dbg !186
  br label %21, !dbg !187, !llvm.loop !188

32:                                               ; preds = %21
  ret void, !dbg !232, !pallas.stmntBlock !233
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @increaseByOne(ptr noundef %0) #0 !dbg !243 !pallas.fcontract !246 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !252, metadata !DIExpression()), !dbg !275
  %3 = load ptr, ptr %2, align 8, !dbg !276
  %4 = load i32, ptr %3, align 4, !dbg !277
  %5 = add nsw i32 %4, 1, !dbg !277
  store i32 %5, ptr %3, align 4, !dbg !277
  ret void, !dbg !278
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @callWithCast() #0 !dbg !279 {
  %1 = alloca %struct.B, align 4
  %2 = alloca ptr, align 8
  call void @llvm.dbg.declare(metadata ptr %1, metadata !280, metadata !DIExpression()), !dbg !281
  %3 = getelementptr inbounds %struct.B, ptr %1, i32 0, i32 0, !dbg !282
  %4 = getelementptr inbounds %struct.A, ptr %3, i32 0, i32 0, !dbg !283
  store i32 15, ptr %4, align 4, !dbg !284
  call void @llvm.dbg.declare(metadata ptr %2, metadata !285, metadata !DIExpression()), !dbg !286
  store ptr %1, ptr %2, align 8, !dbg !286
  %5 = load ptr, ptr %2, align 8, !dbg !287
  call void @increaseByOne(ptr noundef %5), !dbg !288
  ret void, !dbg !289, !pallas.stmntBlock !290
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !254 !pallas.exprWrapper !300 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !253, metadata !DIExpression()), !dbg !301
  %2 = icmp ne ptr %0, null, !dbg !302
  ret i1 %2, !dbg !301
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !262 !pallas.exprWrapper !300 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !261, metadata !DIExpression()), !dbg !303
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !304
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !305
  ret i1 %3, !dbg !303
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !268 !pallas.exprWrapper !300 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !267, metadata !DIExpression()), !dbg !306
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !307
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !308
  ret i1 %3, !dbg !306
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !274 !pallas.exprWrapper !300 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !273, metadata !DIExpression()), !dbg !309
  %2 = load i32, ptr %0, align 4, !dbg !310
  %3 = load i32, ptr %0, align 4, !dbg !311
  %4 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %3), !dbg !312
  %5 = add nsw i32 %4, 1, !dbg !313
  %6 = icmp eq i32 %2, %5, !dbg !314
  ret i1 %6, !dbg !309
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !133 !pallas.exprWrapper !300 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !132, metadata !DIExpression()), !dbg !315
  call void @llvm.dbg.value(metadata ptr %1, metadata !135, metadata !DIExpression()), !dbg !315
  call void @llvm.dbg.value(metadata i32 %2, metadata !137, metadata !DIExpression()), !dbg !315
  %6 = icmp eq ptr %1, %4, !dbg !316
  ret i1 %6, !dbg !315
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !121 !pallas.exprWrapper !300 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !120, metadata !DIExpression()), !dbg !317
  call void @llvm.dbg.value(metadata ptr %1, metadata !125, metadata !DIExpression()), !dbg !317
  call void @llvm.dbg.value(metadata i32 %2, metadata !127, metadata !DIExpression()), !dbg !317
  %6 = icmp sle i32 0, %2, !dbg !318
  br i1 %6, label %7, label %9, !dbg !319

7:                                                ; preds = %3
  %8 = icmp sle i32 %2, 10, !dbg !320
  br label %9

9:                                                ; preds = %7, %3
  %10 = phi i1 [ false, %3 ], [ %8, %7 ], !dbg !317
  ret i1 %10, !dbg !317
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !153 !pallas.exprWrapper !300 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !152, metadata !DIExpression()), !dbg !321
  call void @llvm.dbg.value(metadata ptr %1, metadata !155, metadata !DIExpression()), !dbg !321
  call void @llvm.dbg.value(metadata i32 %2, metadata !157, metadata !DIExpression()), !dbg !321
  %6 = load i32, ptr %1, align 4, !dbg !322
  %7 = sub nsw i32 10, %2, !dbg !323
  %8 = icmp eq i32 %6, %7, !dbg !324
  ret i1 %8, !dbg !321
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !197 !pallas.exprWrapper !300 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !196, metadata !DIExpression()), !dbg !325
  call void @llvm.dbg.value(metadata ptr %1, metadata !199, metadata !DIExpression()), !dbg !325
  call void @llvm.dbg.value(metadata i32 %2, metadata !201, metadata !DIExpression()), !dbg !325
  %6 = icmp sle i32 0, %2, !dbg !326
  br i1 %6, label %7, label %9, !dbg !327

7:                                                ; preds = %3
  %8 = icmp sle i32 %2, 10, !dbg !328
  br label %9

9:                                                ; preds = %7, %3
  %10 = phi i1 [ false, %3 ], [ %8, %7 ], !dbg !325
  ret i1 %10, !dbg !325
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !207 !pallas.exprWrapper !300 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !206, metadata !DIExpression()), !dbg !329
  call void @llvm.dbg.value(metadata ptr %1, metadata !209, metadata !DIExpression()), !dbg !329
  call void @llvm.dbg.value(metadata i32 %2, metadata !211, metadata !DIExpression()), !dbg !329
  %6 = icmp eq ptr %1, %4, !dbg !330
  ret i1 %6, !dbg !329
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !217 !pallas.exprWrapper !300 {
  %4 = alloca %struct.B, align 4
  %5 = alloca %pallas.fracT, align 8
  %6 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !216, metadata !DIExpression()), !dbg !331
  call void @llvm.dbg.value(metadata ptr %1, metadata !219, metadata !DIExpression()), !dbg !331
  call void @llvm.dbg.value(metadata i32 %2, metadata !221, metadata !DIExpression()), !dbg !331
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 1), !dbg !332
  %7 = call i1 @pallas.perm(ptr noundef %1, ptr noundef byval(%pallas.fracT) %5), !dbg !333
  ret i1 %7, !dbg !331
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !143 !pallas.exprWrapper !300 {
  %4 = alloca %struct.B, align 4
  %5 = alloca %pallas.fracT, align 8
  %6 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !142, metadata !DIExpression()), !dbg !334
  call void @llvm.dbg.value(metadata ptr %1, metadata !145, metadata !DIExpression()), !dbg !334
  call void @llvm.dbg.value(metadata i32 %2, metadata !147, metadata !DIExpression()), !dbg !334
  %7 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0, !dbg !335
  %8 = getelementptr inbounds %struct.A, ptr %7, i32 0, i32 0, !dbg !336
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 1), !dbg !337
  %9 = call i1 @pallas.perm(ptr noundef %8, ptr noundef byval(%pallas.fracT) %5), !dbg !338
  ret i1 %9, !dbg !334
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !227 !pallas.exprWrapper !300 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !226, metadata !DIExpression()), !dbg !339
  call void @llvm.dbg.value(metadata ptr %1, metadata !229, metadata !DIExpression()), !dbg !339
  call void @llvm.dbg.value(metadata i32 %2, metadata !231, metadata !DIExpression()), !dbg !339
  %6 = load i32, ptr %1, align 4, !dbg !340
  %7 = sub nsw i32 10, %2, !dbg !341
  %8 = icmp eq i32 %6, %7, !dbg !342
  ret i1 %8, !dbg !339
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(i64 %0, ptr noundef %1) #0 !dbg !45 !pallas.exprWrapper !300 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !44, metadata !DIExpression()), !dbg !343
  call void @llvm.dbg.value(metadata ptr %1, metadata !57, metadata !DIExpression()), !dbg !343
  %5 = load i32, ptr %1, align 4, !dbg !344
  %6 = icmp eq i32 %5, 5, !dbg !345
  ret i1 %6, !dbg !343
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(i64 %0, ptr noundef %1) #0 !dbg !63 !pallas.exprWrapper !300 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !62, metadata !DIExpression()), !dbg !346
  call void @llvm.dbg.value(metadata ptr %1, metadata !65, metadata !DIExpression()), !dbg !346
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !347
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !348
  %7 = icmp eq ptr %1, %6, !dbg !349
  ret i1 %7, !dbg !346
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(i64 %0, ptr noundef %1) #0 !dbg !71 !pallas.exprWrapper !300 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !70, metadata !DIExpression()), !dbg !350
  call void @llvm.dbg.value(metadata ptr %1, metadata !73, metadata !DIExpression()), !dbg !350
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !351
  %6 = icmp eq ptr %1, %5, !dbg !352
  ret i1 %6, !dbg !350
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(i64 %0, ptr noundef %1) #0 !dbg !83 !pallas.exprWrapper !300 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !82, metadata !DIExpression()), !dbg !353
  call void @llvm.dbg.value(metadata ptr %1, metadata !85, metadata !DIExpression()), !dbg !353
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !354
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !355
  %7 = load i32, ptr %6, align 4, !dbg !355
  %8 = icmp eq i32 %7, 10, !dbg !356
  ret i1 %8, !dbg !353
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(i64 %0, ptr noundef %1) #0 !dbg !166 !pallas.exprWrapper !300 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !165, metadata !DIExpression()), !dbg !357
  call void @llvm.dbg.value(metadata ptr %1, metadata !168, metadata !DIExpression()), !dbg !357
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !358
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !359
  %7 = load i32, ptr %6, align 4, !dbg !359
  %8 = icmp eq i32 %7, 0, !dbg !360
  ret i1 %8, !dbg !357
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(i64 %0, ptr noundef %1) #0 !dbg !240 !pallas.exprWrapper !300 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !239, metadata !DIExpression()), !dbg !361
  call void @llvm.dbg.value(metadata ptr %1, metadata !242, metadata !DIExpression()), !dbg !361
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !362
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !363
  %7 = load i32, ptr %6, align 4, !dbg !363
  %8 = icmp eq i32 %7, 0, !dbg !364
  ret i1 %8, !dbg !361
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(i64 %0, ptr noundef %1) #0 !dbg !297 !pallas.exprWrapper !300 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !296, metadata !DIExpression()), !dbg !365
  call void @llvm.dbg.value(metadata ptr %1, metadata !299, metadata !DIExpression()), !dbg !365
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !366
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !367
  %7 = load i32, ptr %6, align 4, !dbg !367
  %8 = icmp eq i32 %7, 16, !dbg !368
  ret i1 %8, !dbg !365
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !369 i32 @"pallas.old i32_noundef i32"(i32 noundef)

declare !pallas.specLib !370 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !371 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !5}
!llvm.module.flags = !{!9, !10, !11, !12, !13, !14, !15}
!llvm.ident = !{!16, !16}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pointer_casts.c", directory: ".", checksumkind: CSK_MD5, checksum: "5f415ed5499174e0d5081bbc2cd18cf0")
!2 = !{!3}
!3 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!4 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!5 = distinct !DICompileUnit(language: DW_LANG_C11, file: !6, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !7, splitDebugInlining: false, nameTableKind: None)
!6 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "b08f5978704a15ece60578eaaee50286")
!7 = !{!3, !8}
!8 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!9 = !{i32 7, !"Dwarf Version", i32 5}
!10 = !{i32 2, !"Debug Info Version", i32 3}
!11 = !{i32 1, !"wchar_size", i32 4}
!12 = !{i32 8, !"PIC Level", i32 2}
!13 = !{i32 7, !"PIE Level", i32 2}
!14 = !{i32 7, !"uwtable", i32 2}
!15 = !{i32 7, !"frame-pointer", i32 2}
!16 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!17 = distinct !DISubprogram(name: "canCastToInteger", scope: !1, file: !1, line: 19, type: !18, scopeLine: 19, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!18 = !DISubroutineType(types: !19)
!19 = !{null}
!20 = !{}
!21 = !DILocalVariable(name: "struct_b", scope: !17, file: !1, line: 20, type: !22)
!22 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "B", file: !1, line: 8, size: 64, elements: !23)
!23 = !{!24}
!24 = !DIDerivedType(tag: DW_TAG_member, name: "struct_a", scope: !22, file: !1, line: 9, baseType: !25, size: 64)
!25 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "A", file: !1, line: 3, size: 64, elements: !26)
!26 = !{!27, !28}
!27 = !DIDerivedType(tag: DW_TAG_member, name: "integer", scope: !25, file: !1, line: 4, baseType: !4, size: 32)
!28 = !DIDerivedType(tag: DW_TAG_member, name: "boolean", scope: !25, file: !1, line: 5, baseType: !29, size: 8, offset: 32)
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !DILocation(line: 20, column: 14, scope: !17)
!31 = !DILocation(line: 21, column: 14, scope: !17)
!32 = !DILocation(line: 21, column: 23, scope: !17)
!33 = !DILocation(line: 21, column: 31, scope: !17)
!34 = !DILocalVariable(name: "pointer_to_integer", scope: !17, file: !1, line: 22, type: !3)
!35 = !DILocation(line: 22, column: 10, scope: !17)
!36 = !DILocation(line: 26, column: 6, scope: !17)
!37 = !{!38, !40, !58, !66}
!38 = !{!"pallas.srcLoc", i64 23, i64 5, i64 25, i64 62, !39}
!39 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pointer_casts.c", directory: "", checksumkind: CSK_MD5, checksum: "5f415ed5499174e0d5081bbc2cd18cf0")
!40 = !{!"pallas.assert", !41, ptr @PALLAS_SPEC_12, !20, !20, !42}
!41 = !{!"pallas.srcLoc", i64 23, i64 9, i64 23, i64 40, !39}
!42 = !{!43, !56}
!43 = !{!21, !44}
!44 = !DILocalVariable(name: "struct_b", arg: 1, scope: !45, file: !1, line: 23, type: !48)
!45 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 23, type: !46, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!46 = !DISubroutineType(types: !47)
!47 = !{!29, !48, !3}
!48 = !DIDerivedType(tag: DW_TAG_typedef, name: "B", file: !6, line: 14, baseType: !49)
!49 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "B", file: !6, line: 9, size: 64, elements: !50)
!50 = !{!51}
!51 = !DIDerivedType(tag: DW_TAG_member, name: "struct_a", scope: !49, file: !6, line: 10, baseType: !52, size: 64)
!52 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "A", file: !6, line: 4, size: 64, elements: !53)
!53 = !{!54, !55}
!54 = !DIDerivedType(tag: DW_TAG_member, name: "integer", scope: !52, file: !6, line: 5, baseType: !4, size: 32)
!55 = !DIDerivedType(tag: DW_TAG_member, name: "boolean", scope: !52, file: !6, line: 6, baseType: !29, size: 8, offset: 32)
!56 = !{!34, !57}
!57 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !45, file: !1, line: 23, type: !3)
!58 = !{!"pallas.assert", !59, ptr @PALLAS_SPEC_13, !20, !20, !60}
!59 = !{!"pallas.srcLoc", i64 24, i64 6, i64 24, i64 61, !39}
!60 = !{!61, !64}
!61 = !{!21, !62}
!62 = !DILocalVariable(name: "struct_b", arg: 1, scope: !63, file: !1, line: 24, type: !48)
!63 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !1, file: !1, line: 24, type: !46, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!64 = !{!34, !65}
!65 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !63, file: !1, line: 24, type: !3)
!66 = !{!"pallas.assert", !67, ptr @PALLAS_SPEC_14, !20, !20, !68}
!67 = !{!"pallas.srcLoc", i64 25, i64 6, i64 25, i64 60, !39}
!68 = !{!69, !72}
!69 = !{!21, !70}
!70 = !DILocalVariable(name: "struct_b", arg: 1, scope: !71, file: !1, line: 25, type: !48)
!71 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !1, file: !1, line: 25, type: !46, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!72 = !{!34, !73}
!73 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !71, file: !1, line: 25, type: !3)
!74 = !DILocation(line: 26, column: 25, scope: !17)
!75 = !DILocation(line: 28, column: 1, scope: !17)
!76 = !{!77, !78}
!77 = !{!"pallas.srcLoc", i64 27, i64 5, i64 27, i64 49, !39}
!78 = !{!"pallas.assert", !79, ptr @PALLAS_SPEC_15, !20, !20, !80}
!79 = !{!"pallas.srcLoc", i64 27, i64 9, i64 27, i64 47, !39}
!80 = !{!81, !84}
!81 = !{!21, !82}
!82 = !DILocalVariable(name: "struct_b", arg: 1, scope: !83, file: !1, line: 27, type: !48)
!83 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !1, file: !1, line: 27, type: !46, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!84 = !{!34, !85}
!85 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !83, file: !1, line: 27, type: !3)
!86 = distinct !DISubprogram(name: "castRemainsValidInLoop", scope: !1, file: !1, line: 31, type: !18, scopeLine: 31, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!87 = !DILocalVariable(name: "struct_b", scope: !86, file: !1, line: 32, type: !22)
!88 = !DILocation(line: 32, column: 14, scope: !86)
!89 = !DILocation(line: 33, column: 14, scope: !86)
!90 = !DILocation(line: 33, column: 23, scope: !86)
!91 = !DILocation(line: 33, column: 31, scope: !86)
!92 = !DILocalVariable(name: "pointer_to_integer", scope: !86, file: !1, line: 35, type: !3)
!93 = !DILocation(line: 35, column: 10, scope: !86)
!94 = !DILocalVariable(name: "i", scope: !95, file: !1, line: 41, type: !4)
!95 = distinct !DILexicalBlock(scope: !86, file: !1, line: 41, column: 5)
!96 = !DILocation(line: 41, column: 14, scope: !95)
!97 = !DILocation(line: 41, column: 10, scope: !95)
!98 = !DILocation(line: 41, column: 21, scope: !99)
!99 = distinct !DILexicalBlock(scope: !95, file: !1, line: 41, column: 5)
!100 = !DILocation(line: 41, column: 23, scope: !99)
!101 = !DILocation(line: 41, column: 5, scope: !95)
!102 = !DILocation(line: 42, column: 32, scope: !103)
!103 = distinct !DILexicalBlock(scope: !99, file: !1, line: 41, column: 34)
!104 = !DILocation(line: 42, column: 31, scope: !103)
!105 = !DILocation(line: 42, column: 51, scope: !103)
!106 = !DILocation(line: 42, column: 10, scope: !103)
!107 = !DILocation(line: 42, column: 29, scope: !103)
!108 = !DILocation(line: 43, column: 5, scope: !103)
!109 = !DILocation(line: 41, column: 30, scope: !99)
!110 = !DILocation(line: 41, column: 5, scope: !99)
!111 = distinct !{!111, !101, !112, !113, !114}
!112 = !DILocation(line: 43, column: 5, scope: !95)
!113 = !{!"llvm.loop.mustprogress"}
!114 = !{!"pallas.loopInvBlock", !115, !116, !128, !138, !148}
!115 = !{!"pallas.srcLoc", i64 37, i64 5, i64 40, i64 55, !39}
!116 = !{!"pallas.loopInv", !117, ptr @PALLAS_SPEC_4, !20, !20, !118}
!117 = !{!"pallas.srcLoc", i64 37, i64 9, i64 37, i64 41, !39}
!118 = !{!119, !124, !126}
!119 = !{!87, !120}
!120 = !DILocalVariable(name: "struct_b", arg: 1, scope: !121, file: !1, line: 37, type: !48)
!121 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 37, type: !122, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!122 = !DISubroutineType(types: !123)
!123 = !{!29, !48, !3, !4}
!124 = !{!92, !125}
!125 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !121, file: !1, line: 37, type: !3)
!126 = !{!94, !127}
!127 = !DILocalVariable(name: "i", arg: 3, scope: !121, file: !1, line: 37, type: !4)
!128 = !{!"pallas.loopInv", !129, ptr @PALLAS_SPEC_5, !20, !20, !130}
!129 = !{!"pallas.srcLoc", i64 38, i64 9, i64 38, i64 62, !39}
!130 = !{!131, !134, !136}
!131 = !{!87, !132}
!132 = !DILocalVariable(name: "struct_b", arg: 1, scope: !133, file: !1, line: 38, type: !48)
!133 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 38, type: !122, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!134 = !{!92, !135}
!135 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !133, file: !1, line: 38, type: !3)
!136 = !{!94, !137}
!137 = !DILocalVariable(name: "i", arg: 3, scope: !133, file: !1, line: 38, type: !4)
!138 = !{!"pallas.loopInv", !139, ptr @PALLAS_SPEC_6, !20, !20, !140}
!139 = !{!"pallas.srcLoc", i64 39, i64 9, i64 39, i64 72, !39}
!140 = !{!141, !144, !146}
!141 = !{!87, !142}
!142 = !DILocalVariable(name: "struct_b", arg: 1, scope: !143, file: !1, line: 39, type: !48)
!143 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 39, type: !122, scopeLine: 39, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!144 = !{!92, !145}
!145 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !143, file: !1, line: 39, type: !3)
!146 = !{!94, !147}
!147 = !DILocalVariable(name: "i", arg: 3, scope: !143, file: !1, line: 39, type: !4)
!148 = !{!"pallas.loopInv", !149, ptr @PALLAS_SPEC_7, !20, !20, !150}
!149 = !{!"pallas.srcLoc", i64 40, i64 9, i64 40, i64 53, !39}
!150 = !{!151, !154, !156}
!151 = !{!87, !152}
!152 = !DILocalVariable(name: "struct_b", arg: 1, scope: !153, file: !1, line: 40, type: !48)
!153 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !1, file: !1, line: 40, type: !122, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!154 = !{!92, !155}
!155 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !153, file: !1, line: 40, type: !3)
!156 = !{!94, !157}
!157 = !DILocalVariable(name: "i", arg: 3, scope: !153, file: !1, line: 40, type: !4)
!158 = !DILocation(line: 46, column: 14, scope: !86)
!159 = !{!160, !161}
!160 = !{!"pallas.srcLoc", i64 45, i64 5, i64 45, i64 48, !39}
!161 = !{!"pallas.assert", !162, ptr @PALLAS_SPEC_16, !20, !20, !163}
!162 = !{!"pallas.srcLoc", i64 45, i64 9, i64 45, i64 46, !39}
!163 = !{!164, !167}
!164 = !{!87, !165}
!165 = !DILocalVariable(name: "struct_b", arg: 1, scope: !166, file: !1, line: 45, type: !48)
!166 = distinct !DISubprogram(name: "PALLAS_SPEC_16", scope: !1, file: !1, line: 45, type: !46, scopeLine: 45, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!167 = !{!92, !168}
!168 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !166, file: !1, line: 45, type: !3)
!169 = !DILocation(line: 46, column: 23, scope: !86)
!170 = !DILocation(line: 46, column: 31, scope: !86)
!171 = !DILocalVariable(name: "j", scope: !172, file: !1, line: 53, type: !4)
!172 = distinct !DILexicalBlock(scope: !86, file: !1, line: 53, column: 5)
!173 = !DILocation(line: 53, column: 14, scope: !172)
!174 = !DILocation(line: 53, column: 10, scope: !172)
!175 = !DILocation(line: 53, column: 21, scope: !176)
!176 = distinct !DILexicalBlock(scope: !172, file: !1, line: 53, column: 5)
!177 = !DILocation(line: 53, column: 23, scope: !176)
!178 = !DILocation(line: 53, column: 5, scope: !172)
!179 = !DILocation(line: 54, column: 32, scope: !180)
!180 = distinct !DILexicalBlock(scope: !176, file: !1, line: 53, column: 34)
!181 = !DILocation(line: 54, column: 31, scope: !180)
!182 = !DILocation(line: 54, column: 51, scope: !180)
!183 = !DILocation(line: 54, column: 10, scope: !180)
!184 = !DILocation(line: 54, column: 29, scope: !180)
!185 = !DILocation(line: 55, column: 5, scope: !180)
!186 = !DILocation(line: 53, column: 30, scope: !176)
!187 = !DILocation(line: 53, column: 5, scope: !176)
!188 = distinct !{!188, !178, !189, !113, !190}
!189 = !DILocation(line: 55, column: 5, scope: !172)
!190 = !{!"pallas.loopInvBlock", !191, !192, !202, !212, !222}
!191 = !{!"pallas.srcLoc", i64 49, i64 5, i64 52, i64 55, !39}
!192 = !{!"pallas.loopInv", !193, ptr @PALLAS_SPEC_8, !20, !20, !194}
!193 = !{!"pallas.srcLoc", i64 49, i64 9, i64 49, i64 41, !39}
!194 = !{!195, !198, !200}
!195 = !{!87, !196}
!196 = !DILocalVariable(name: "struct_b", arg: 1, scope: !197, file: !1, line: 49, type: !48)
!197 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !1, file: !1, line: 49, type: !122, scopeLine: 49, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!198 = !{!92, !199}
!199 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !197, file: !1, line: 49, type: !3)
!200 = !{!171, !201}
!201 = !DILocalVariable(name: "j", arg: 3, scope: !197, file: !1, line: 49, type: !4)
!202 = !{!"pallas.loopInv", !203, ptr @PALLAS_SPEC_9, !20, !20, !204}
!203 = !{!"pallas.srcLoc", i64 50, i64 9, i64 50, i64 62, !39}
!204 = !{!205, !208, !210}
!205 = !{!87, !206}
!206 = !DILocalVariable(name: "struct_b", arg: 1, scope: !207, file: !1, line: 50, type: !48)
!207 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !1, file: !1, line: 50, type: !122, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!208 = !{!92, !209}
!209 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !207, file: !1, line: 50, type: !3)
!210 = !{!171, !211}
!211 = !DILocalVariable(name: "j", arg: 3, scope: !207, file: !1, line: 50, type: !4)
!212 = !{!"pallas.loopInv", !213, ptr @PALLAS_SPEC_10, !20, !20, !214}
!213 = !{!"pallas.srcLoc", i64 51, i64 9, i64 51, i64 64, !39}
!214 = !{!215, !218, !220}
!215 = !{!87, !216}
!216 = !DILocalVariable(name: "struct_b", arg: 1, scope: !217, file: !1, line: 51, type: !48)
!217 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !1, file: !1, line: 51, type: !122, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!218 = !{!92, !219}
!219 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !217, file: !1, line: 51, type: !3)
!220 = !{!171, !221}
!221 = !DILocalVariable(name: "j", arg: 3, scope: !217, file: !1, line: 51, type: !4)
!222 = !{!"pallas.loopInv", !223, ptr @PALLAS_SPEC_11, !20, !20, !224}
!223 = !{!"pallas.srcLoc", i64 52, i64 9, i64 52, i64 53, !39}
!224 = !{!225, !228, !230}
!225 = !{!87, !226}
!226 = !DILocalVariable(name: "struct_b", arg: 1, scope: !227, file: !1, line: 52, type: !48)
!227 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 52, type: !122, scopeLine: 52, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!228 = !{!92, !229}
!229 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !227, file: !1, line: 52, type: !3)
!230 = !{!171, !231}
!231 = !DILocalVariable(name: "j", arg: 3, scope: !227, file: !1, line: 52, type: !4)
!232 = !DILocation(line: 58, column: 1, scope: !86)
!233 = !{!234, !235}
!234 = !{!"pallas.srcLoc", i64 57, i64 5, i64 57, i64 48, !39}
!235 = !{!"pallas.assert", !236, ptr @PALLAS_SPEC_17, !20, !20, !237}
!236 = !{!"pallas.srcLoc", i64 57, i64 9, i64 57, i64 46, !39}
!237 = !{!238, !241}
!238 = !{!87, !239}
!239 = !DILocalVariable(name: "struct_b", arg: 1, scope: !240, file: !1, line: 57, type: !48)
!240 = distinct !DISubprogram(name: "PALLAS_SPEC_17", scope: !1, file: !1, line: 57, type: !46, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!241 = !{!92, !242}
!242 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !240, file: !1, line: 57, type: !3)
!243 = distinct !DISubprogram(name: "increaseByOne", scope: !1, file: !1, line: 64, type: !244, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!244 = !DISubroutineType(types: !245)
!245 = !{null, !3}
!246 = !{!247, i1 false, i1 false, !20, !20, !248, !257, !263, !269}
!247 = !{!"pallas.srcLoc", i64 60, i64 1, i64 63, i64 34, !39}
!248 = !{!"pallas.requires", !249, ptr @PALLAS_SPEC_0, !20, !20, !250}
!249 = !{!"pallas.srcLoc", i64 60, i64 5, i64 60, i64 23, !39}
!250 = !{!251}
!251 = !{!252, !253}
!252 = !DILocalVariable(name: "a", arg: 1, scope: !243, file: !1, line: 64, type: !3)
!253 = !DILocalVariable(name: "a", arg: 1, scope: !254, file: !1, line: 60, type: !3)
!254 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 60, type: !255, scopeLine: 60, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!255 = !DISubroutineType(types: !256)
!256 = !{!29, !3}
!257 = !{!"pallas.requires", !258, ptr @PALLAS_SPEC_1, !20, !20, !259}
!258 = !{!"pallas.srcLoc", i64 61, i64 1, i64 61, i64 32, !39}
!259 = !{!260}
!260 = !{!252, !261}
!261 = !DILocalVariable(name: "a", arg: 1, scope: !262, file: !1, line: 61, type: !3)
!262 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 61, type: !255, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!263 = !{!"pallas.ensures", !264, ptr @PALLAS_SPEC_2, !20, !20, !265}
!264 = !{!"pallas.srcLoc", i64 62, i64 1, i64 62, i64 31, !39}
!265 = !{!266}
!266 = !{!252, !267}
!267 = !DILocalVariable(name: "a", arg: 1, scope: !268, file: !1, line: 62, type: !3)
!268 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 62, type: !255, scopeLine: 62, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!269 = !{!"pallas.ensures", !270, ptr @PALLAS_SPEC_3, !20, !20, !271}
!270 = !{!"pallas.srcLoc", i64 63, i64 1, i64 63, i64 32, !39}
!271 = !{!272}
!272 = !{!252, !273}
!273 = !DILocalVariable(name: "a", arg: 1, scope: !274, file: !1, line: 63, type: !3)
!274 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 63, type: !255, scopeLine: 63, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!275 = !DILocation(line: 64, column: 25, scope: !243)
!276 = !DILocation(line: 65, column: 6, scope: !243)
!277 = !DILocation(line: 65, column: 8, scope: !243)
!278 = !DILocation(line: 66, column: 1, scope: !243)
!279 = distinct !DISubprogram(name: "callWithCast", scope: !1, file: !1, line: 68, type: !18, scopeLine: 68, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!280 = !DILocalVariable(name: "struct_b", scope: !279, file: !1, line: 69, type: !22)
!281 = !DILocation(line: 69, column: 14, scope: !279)
!282 = !DILocation(line: 70, column: 14, scope: !279)
!283 = !DILocation(line: 70, column: 23, scope: !279)
!284 = !DILocation(line: 70, column: 31, scope: !279)
!285 = !DILocalVariable(name: "pointer_to_integer", scope: !279, file: !1, line: 72, type: !3)
!286 = !DILocation(line: 72, column: 10, scope: !279)
!287 = !DILocation(line: 73, column: 19, scope: !279)
!288 = !DILocation(line: 73, column: 5, scope: !279)
!289 = !DILocation(line: 76, column: 1, scope: !279)
!290 = !{!291, !292}
!291 = !{!"pallas.srcLoc", i64 75, i64 5, i64 75, i64 49, !39}
!292 = !{!"pallas.assert", !293, ptr @PALLAS_SPEC_18, !20, !20, !294}
!293 = !{!"pallas.srcLoc", i64 75, i64 9, i64 75, i64 47, !39}
!294 = !{!295, !298}
!295 = !{!280, !296}
!296 = !DILocalVariable(name: "struct_b", arg: 1, scope: !297, file: !1, line: 75, type: !48)
!297 = distinct !DISubprogram(name: "PALLAS_SPEC_18", scope: !1, file: !1, line: 75, type: !46, scopeLine: 75, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!298 = !{!285, !299}
!299 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !297, file: !1, line: 75, type: !3)
!300 = !{!""}
!301 = !DILocation(line: 0, scope: !254)
!302 = !DILocation(line: 60, column: 16, scope: !254)
!303 = !DILocation(line: 0, scope: !262)
!304 = !DILocation(line: 61, column: 19, scope: !262)
!305 = !DILocation(line: 61, column: 10, scope: !262)
!306 = !DILocation(line: 0, scope: !268)
!307 = !DILocation(line: 62, column: 18, scope: !268)
!308 = !DILocation(line: 62, column: 9, scope: !268)
!309 = !DILocation(line: 0, scope: !274)
!310 = !DILocation(line: 63, column: 9, scope: !274)
!311 = !DILocation(line: 63, column: 25, scope: !274)
!312 = !DILocation(line: 63, column: 15, scope: !274)
!313 = !DILocation(line: 63, column: 29, scope: !274)
!314 = !DILocation(line: 63, column: 12, scope: !274)
!315 = !DILocation(line: 0, scope: !133)
!316 = !DILocation(line: 38, column: 43, scope: !133)
!317 = !DILocation(line: 0, scope: !121)
!318 = !DILocation(line: 37, column: 26, scope: !121)
!319 = !DILocation(line: 37, column: 31, scope: !121)
!320 = !DILocation(line: 37, column: 36, scope: !121)
!321 = !DILocation(line: 0, scope: !153)
!322 = !DILocation(line: 40, column: 24, scope: !153)
!323 = !DILocation(line: 40, column: 50, scope: !153)
!324 = !DILocation(line: 40, column: 44, scope: !153)
!325 = !DILocation(line: 0, scope: !197)
!326 = !DILocation(line: 49, column: 26, scope: !197)
!327 = !DILocation(line: 49, column: 31, scope: !197)
!328 = !DILocation(line: 49, column: 36, scope: !197)
!329 = !DILocation(line: 0, scope: !207)
!330 = !DILocation(line: 50, column: 43, scope: !207)
!331 = !DILocation(line: 0, scope: !217)
!332 = !DILocation(line: 51, column: 50, scope: !217)
!333 = !DILocation(line: 51, column: 24, scope: !217)
!334 = !DILocation(line: 0, scope: !143)
!335 = !DILocation(line: 39, column: 40, scope: !143)
!336 = !DILocation(line: 39, column: 49, scope: !143)
!337 = !DILocation(line: 39, column: 58, scope: !143)
!338 = !DILocation(line: 39, column: 24, scope: !143)
!339 = !DILocation(line: 0, scope: !227)
!340 = !DILocation(line: 52, column: 24, scope: !227)
!341 = !DILocation(line: 52, column: 50, scope: !227)
!342 = !DILocation(line: 52, column: 44, scope: !227)
!343 = !DILocation(line: 0, scope: !45)
!344 = !DILocation(line: 23, column: 16, scope: !45)
!345 = !DILocation(line: 23, column: 36, scope: !45)
!346 = !DILocation(line: 0, scope: !63)
!347 = !DILocation(line: 24, column: 45, scope: !63)
!348 = !DILocation(line: 24, column: 54, scope: !63)
!349 = !DILocation(line: 24, column: 32, scope: !63)
!350 = !DILocation(line: 0, scope: !71)
!351 = !DILocation(line: 25, column: 52, scope: !71)
!352 = !DILocation(line: 25, column: 32, scope: !71)
!353 = !DILocation(line: 0, scope: !83)
!354 = !DILocation(line: 27, column: 25, scope: !83)
!355 = !DILocation(line: 27, column: 34, scope: !83)
!356 = !DILocation(line: 27, column: 42, scope: !83)
!357 = !DILocation(line: 0, scope: !166)
!358 = !DILocation(line: 45, column: 25, scope: !166)
!359 = !DILocation(line: 45, column: 34, scope: !166)
!360 = !DILocation(line: 45, column: 42, scope: !166)
!361 = !DILocation(line: 0, scope: !240)
!362 = !DILocation(line: 57, column: 25, scope: !240)
!363 = !DILocation(line: 57, column: 34, scope: !240)
!364 = !DILocation(line: 57, column: 42, scope: !240)
!365 = !DILocation(line: 0, scope: !297)
!366 = !DILocation(line: 75, column: 25, scope: !297)
!367 = !DILocation(line: 75, column: 34, scope: !297)
!368 = !DILocation(line: 75, column: 42, scope: !297)
!369 = !{!"pallas.old"}
!370 = !{!"pallas.perm"}
!371 = !{!"pallas.fracOf"}
