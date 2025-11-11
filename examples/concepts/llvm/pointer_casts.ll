; ModuleID = 'tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pointer_casts.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.B = type { %struct.A }
%struct.A = type { i32, i8 }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [19 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_15, ptr @PALLAS_SPEC_16, ptr @PALLAS_SPEC_17, ptr @PALLAS_SPEC_18], section "llvm.metadata"

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
  store i32 10, ptr %5, align 4, !dbg !45
  ret void, !dbg !46, !pallas.stmntBlock !47
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @castRemainsValidInLoop() #0 !dbg !51 {
  %1 = alloca %struct.B, align 4
  %2 = alloca ptr, align 8
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  call void @llvm.dbg.declare(metadata ptr %1, metadata !52, metadata !DIExpression()), !dbg !53
  %5 = getelementptr inbounds %struct.B, ptr %1, i32 0, i32 0, !dbg !54
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !55
  store i32 10, ptr %6, align 4, !dbg !56
  call void @llvm.dbg.declare(metadata ptr %2, metadata !57, metadata !DIExpression()), !dbg !58
  store ptr %1, ptr %2, align 8, !dbg !58
  call void @llvm.dbg.declare(metadata ptr %3, metadata !59, metadata !DIExpression()), !dbg !61
  store i32 0, ptr %3, align 4, !dbg !61
  br label %7, !dbg !62

7:                                                ; preds = %15, %0
  %8 = load i32, ptr %3, align 4, !dbg !63
  %9 = icmp slt i32 %8, 10, !dbg !65
  br i1 %9, label %10, label %18, !dbg !66

10:                                               ; preds = %7
  %11 = load ptr, ptr %2, align 8, !dbg !67
  %12 = load i32, ptr %11, align 4, !dbg !69
  %13 = sub nsw i32 %12, 1, !dbg !70
  %14 = load ptr, ptr %2, align 8, !dbg !71
  store i32 %13, ptr %14, align 4, !dbg !72
  br label %15, !dbg !73

15:                                               ; preds = %10
  %16 = load i32, ptr %3, align 4, !dbg !74
  %17 = add nsw i32 %16, 1, !dbg !74
  store i32 %17, ptr %3, align 4, !dbg !74
  br label %7, !dbg !75, !llvm.loop !76

18:                                               ; preds = %7
  %19 = getelementptr inbounds %struct.B, ptr %1, i32 0, i32 0, !dbg !89, !pallas.stmntBlock !90
  %20 = getelementptr inbounds %struct.A, ptr %19, i32 0, i32 0, !dbg !94
  store i32 10, ptr %20, align 4, !dbg !95
  call void @llvm.dbg.declare(metadata ptr %4, metadata !96, metadata !DIExpression()), !dbg !98
  store i32 0, ptr %4, align 4, !dbg !98
  br label %21, !dbg !99

21:                                               ; preds = %29, %18
  %22 = load i32, ptr %4, align 4, !dbg !100
  %23 = icmp slt i32 %22, 10, !dbg !102
  br i1 %23, label %24, label %32, !dbg !103

24:                                               ; preds = %21
  %25 = load ptr, ptr %2, align 8, !dbg !104
  %26 = load i32, ptr %25, align 4, !dbg !106
  %27 = sub nsw i32 %26, 1, !dbg !107
  %28 = load ptr, ptr %2, align 8, !dbg !108
  store i32 %27, ptr %28, align 4, !dbg !109
  br label %29, !dbg !110

29:                                               ; preds = %24
  %30 = load i32, ptr %4, align 4, !dbg !111
  %31 = add nsw i32 %30, 1, !dbg !111
  store i32 %31, ptr %4, align 4, !dbg !111
  br label %21, !dbg !112, !llvm.loop !113

32:                                               ; preds = %21
  ret void, !dbg !125, !pallas.stmntBlock !126
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @increaseByOne(ptr noundef %0) #0 !dbg !130 !pallas.fcontract !133 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !137, metadata !DIExpression()), !dbg !144
  %3 = load ptr, ptr %2, align 8, !dbg !145
  %4 = load i32, ptr %3, align 4, !dbg !146
  %5 = add nsw i32 %4, 1, !dbg !146
  store i32 %5, ptr %3, align 4, !dbg !146
  ret void, !dbg !147
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @callWithCast() #0 !dbg !148 {
  %1 = alloca %struct.B, align 4
  %2 = alloca ptr, align 8
  call void @llvm.dbg.declare(metadata ptr %1, metadata !149, metadata !DIExpression()), !dbg !150
  %3 = getelementptr inbounds %struct.B, ptr %1, i32 0, i32 0, !dbg !151
  %4 = getelementptr inbounds %struct.A, ptr %3, i32 0, i32 0, !dbg !152
  store i32 15, ptr %4, align 4, !dbg !153
  call void @llvm.dbg.declare(metadata ptr %2, metadata !154, metadata !DIExpression()), !dbg !155
  store ptr %1, ptr %2, align 8, !dbg !155
  %5 = load ptr, ptr %2, align 8, !dbg !156
  call void @increaseByOne(ptr noundef %5), !dbg !157
  ret void, !dbg !158, !pallas.stmntBlock !159
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !163 !pallas.exprWrapper !166 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !167, metadata !DIExpression()), !dbg !168
  %2 = icmp ne ptr %0, null, !dbg !169
  ret i1 %2, !dbg !168
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !170 !pallas.exprWrapper !166 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !171, metadata !DIExpression()), !dbg !172
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !173
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !174
  ret i1 %3, !dbg !172
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !175 !pallas.exprWrapper !166 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !176, metadata !DIExpression()), !dbg !177
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !178
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !179
  ret i1 %3, !dbg !177
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !180 !pallas.exprWrapper !166 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !181, metadata !DIExpression()), !dbg !182
  %2 = load i32, ptr %0, align 4, !dbg !183
  %3 = load i32, ptr %0, align 4, !dbg !184
  %4 = call i32 @pallas.old.0(i32 noundef %3), !dbg !185
  %5 = add nsw i32 %4, 1, !dbg !186
  %6 = icmp eq i32 %2, %5, !dbg !187
  ret i1 %6, !dbg !182
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !188 !pallas.exprWrapper !166 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !199, metadata !DIExpression()), !dbg !200
  call void @llvm.dbg.value(metadata ptr %1, metadata !201, metadata !DIExpression()), !dbg !200
  call void @llvm.dbg.value(metadata i32 %2, metadata !202, metadata !DIExpression()), !dbg !200
  %6 = icmp sle i32 0, %2, !dbg !203
  br i1 %6, label %7, label %9, !dbg !204

7:                                                ; preds = %3
  %8 = icmp sle i32 %2, 10, !dbg !205
  br label %9

9:                                                ; preds = %7, %3
  %10 = phi i1 [ false, %3 ], [ %8, %7 ], !dbg !200
  ret i1 %10, !dbg !200
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !206 !pallas.exprWrapper !166 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !207, metadata !DIExpression()), !dbg !208
  call void @llvm.dbg.value(metadata ptr %1, metadata !209, metadata !DIExpression()), !dbg !208
  call void @llvm.dbg.value(metadata i32 %2, metadata !210, metadata !DIExpression()), !dbg !208
  %6 = icmp eq ptr %1, %4, !dbg !211
  ret i1 %6, !dbg !208
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !212 !pallas.exprWrapper !166 {
  %4 = alloca %struct.B, align 4
  %5 = alloca %pallas.fracT, align 8
  %6 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !213, metadata !DIExpression()), !dbg !214
  call void @llvm.dbg.value(metadata ptr %1, metadata !215, metadata !DIExpression()), !dbg !214
  call void @llvm.dbg.value(metadata i32 %2, metadata !216, metadata !DIExpression()), !dbg !214
  %7 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0, !dbg !217
  %8 = getelementptr inbounds %struct.A, ptr %7, i32 0, i32 0, !dbg !218
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 1), !dbg !219
  %9 = call i1 @pallas.perm(ptr noundef %8, ptr noundef byval(%pallas.fracT) %5), !dbg !220
  ret i1 %9, !dbg !214
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !221 !pallas.exprWrapper !166 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !222, metadata !DIExpression()), !dbg !223
  call void @llvm.dbg.value(metadata ptr %1, metadata !224, metadata !DIExpression()), !dbg !223
  call void @llvm.dbg.value(metadata i32 %2, metadata !225, metadata !DIExpression()), !dbg !223
  %6 = load i32, ptr %1, align 4, !dbg !226
  %7 = sub nsw i32 10, %2, !dbg !227
  %8 = icmp eq i32 %6, %7, !dbg !228
  ret i1 %8, !dbg !223
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(i64 %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !229 !pallas.exprWrapper !166 {
  %5 = alloca %struct.B, align 4
  %6 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !232, metadata !DIExpression()), !dbg !233
  call void @llvm.dbg.value(metadata ptr %1, metadata !234, metadata !DIExpression()), !dbg !233
  call void @llvm.dbg.value(metadata i32 %2, metadata !235, metadata !DIExpression()), !dbg !233
  call void @llvm.dbg.value(metadata i32 %3, metadata !236, metadata !DIExpression()), !dbg !233
  %7 = icmp sle i32 0, %3, !dbg !237
  br i1 %7, label %8, label %10, !dbg !238

8:                                                ; preds = %4
  %9 = icmp sle i32 %3, 10, !dbg !239
  br label %10

10:                                               ; preds = %8, %4
  %11 = phi i1 [ false, %4 ], [ %9, %8 ], !dbg !233
  ret i1 %11, !dbg !233
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(i64 %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !240 !pallas.exprWrapper !166 {
  %5 = alloca %struct.B, align 4
  %6 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !241, metadata !DIExpression()), !dbg !242
  call void @llvm.dbg.value(metadata ptr %1, metadata !243, metadata !DIExpression()), !dbg !242
  call void @llvm.dbg.value(metadata i32 %2, metadata !244, metadata !DIExpression()), !dbg !242
  call void @llvm.dbg.value(metadata i32 %3, metadata !245, metadata !DIExpression()), !dbg !242
  %7 = icmp eq ptr %1, %5, !dbg !246
  ret i1 %7, !dbg !242
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(i64 %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !247 !pallas.exprWrapper !166 {
  %5 = alloca %struct.B, align 4
  %6 = alloca %pallas.fracT, align 8
  %7 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0
  store i64 %0, ptr %7, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !248, metadata !DIExpression()), !dbg !249
  call void @llvm.dbg.value(metadata ptr %1, metadata !250, metadata !DIExpression()), !dbg !249
  call void @llvm.dbg.value(metadata i32 %2, metadata !251, metadata !DIExpression()), !dbg !249
  call void @llvm.dbg.value(metadata i32 %3, metadata !252, metadata !DIExpression()), !dbg !249
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %6, i32 noundef 1, i32 noundef 1), !dbg !253
  %8 = call i1 @pallas.perm(ptr noundef %1, ptr noundef byval(%pallas.fracT) %6), !dbg !254
  ret i1 %8, !dbg !249
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(i64 %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !255 !pallas.exprWrapper !166 {
  %5 = alloca %struct.B, align 4
  %6 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !256, metadata !DIExpression()), !dbg !257
  call void @llvm.dbg.value(metadata ptr %1, metadata !258, metadata !DIExpression()), !dbg !257
  call void @llvm.dbg.value(metadata i32 %2, metadata !259, metadata !DIExpression()), !dbg !257
  call void @llvm.dbg.value(metadata i32 %3, metadata !260, metadata !DIExpression()), !dbg !257
  %7 = load i32, ptr %1, align 4, !dbg !261
  %8 = sub nsw i32 10, %3, !dbg !262
  %9 = icmp eq i32 %7, %8, !dbg !263
  ret i1 %9, !dbg !257
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(i64 %0, ptr noundef %1) #0 !dbg !264 !pallas.exprWrapper !166 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !267, metadata !DIExpression()), !dbg !268
  call void @llvm.dbg.value(metadata ptr %1, metadata !269, metadata !DIExpression()), !dbg !268
  %5 = load i32, ptr %1, align 4, !dbg !270
  %6 = icmp eq i32 %5, 5, !dbg !271
  ret i1 %6, !dbg !268
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(i64 %0, ptr noundef %1) #0 !dbg !272 !pallas.exprWrapper !166 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !273, metadata !DIExpression()), !dbg !274
  call void @llvm.dbg.value(metadata ptr %1, metadata !275, metadata !DIExpression()), !dbg !274
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !276
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !277
  %7 = icmp eq ptr %1, %6, !dbg !278
  ret i1 %7, !dbg !274
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(i64 %0, ptr noundef %1) #0 !dbg !279 !pallas.exprWrapper !166 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !280, metadata !DIExpression()), !dbg !281
  call void @llvm.dbg.value(metadata ptr %1, metadata !282, metadata !DIExpression()), !dbg !281
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !283
  %6 = icmp eq ptr %1, %5, !dbg !284
  ret i1 %6, !dbg !281
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(i64 %0, ptr noundef %1) #0 !dbg !285 !pallas.exprWrapper !166 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !286, metadata !DIExpression()), !dbg !287
  call void @llvm.dbg.value(metadata ptr %1, metadata !288, metadata !DIExpression()), !dbg !287
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !289
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !290
  %7 = load i32, ptr %6, align 4, !dbg !290
  %8 = icmp eq i32 %7, 10, !dbg !291
  ret i1 %8, !dbg !287
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !292 !pallas.exprWrapper !166 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !293, metadata !DIExpression()), !dbg !294
  call void @llvm.dbg.value(metadata ptr %1, metadata !295, metadata !DIExpression()), !dbg !294
  call void @llvm.dbg.value(metadata i32 %2, metadata !296, metadata !DIExpression()), !dbg !294
  %6 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0, !dbg !297
  %7 = getelementptr inbounds %struct.A, ptr %6, i32 0, i32 0, !dbg !298
  %8 = load i32, ptr %7, align 4, !dbg !298
  %9 = icmp eq i32 %8, 0, !dbg !299
  ret i1 %9, !dbg !294
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(i64 %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !300 !pallas.exprWrapper !166 {
  %5 = alloca %struct.B, align 4
  %6 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !301, metadata !DIExpression()), !dbg !302
  call void @llvm.dbg.value(metadata ptr %1, metadata !303, metadata !DIExpression()), !dbg !302
  call void @llvm.dbg.value(metadata i32 %2, metadata !304, metadata !DIExpression()), !dbg !302
  call void @llvm.dbg.value(metadata i32 %3, metadata !305, metadata !DIExpression()), !dbg !302
  %7 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0, !dbg !306
  %8 = getelementptr inbounds %struct.A, ptr %7, i32 0, i32 0, !dbg !307
  %9 = load i32, ptr %8, align 4, !dbg !307
  %10 = icmp eq i32 %9, 0, !dbg !308
  ret i1 %10, !dbg !302
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(i64 %0, ptr noundef %1) #0 !dbg !309 !pallas.exprWrapper !166 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !310, metadata !DIExpression()), !dbg !311
  call void @llvm.dbg.value(metadata ptr %1, metadata !312, metadata !DIExpression()), !dbg !311
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !313
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !314
  %7 = load i32, ptr %6, align 4, !dbg !314
  %8 = icmp eq i32 %7, 16, !dbg !315
  ret i1 %8, !dbg !311
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !316 i32 @pallas.old.0(i32 noundef)

declare !pallas.specLib !317 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !318 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

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
!6 = !DIFile(filename: "source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "cc5ba85943b8a87e6518ae7bb3b23b2d")
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
!37 = !{!38, !39, !41, !43}
!38 = !{!"pallas.srcLoc", i64 23, i64 5, i64 25, i64 62}
!39 = !{!"pallas.assert", !40, ptr @PALLAS_SPEC_12, !21, !34}
!40 = !{!"pallas.srcLoc", i64 23, i64 9, i64 23, i64 40}
!41 = !{!"pallas.assert", !42, ptr @PALLAS_SPEC_13, !21, !34}
!42 = !{!"pallas.srcLoc", i64 24, i64 6, i64 24, i64 61}
!43 = !{!"pallas.assert", !44, ptr @PALLAS_SPEC_14, !21, !34}
!44 = !{!"pallas.srcLoc", i64 25, i64 6, i64 25, i64 60}
!45 = !DILocation(line: 26, column: 25, scope: !17)
!46 = !DILocation(line: 28, column: 1, scope: !17)
!47 = !{!48, !49}
!48 = !{!"pallas.srcLoc", i64 27, i64 5, i64 27, i64 49}
!49 = !{!"pallas.assert", !50, ptr @PALLAS_SPEC_15, !21, !34}
!50 = !{!"pallas.srcLoc", i64 27, i64 9, i64 27, i64 47}
!51 = distinct !DISubprogram(name: "castRemainsValidInLoop", scope: !1, file: !1, line: 31, type: !18, scopeLine: 31, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!52 = !DILocalVariable(name: "struct_b", scope: !51, file: !1, line: 32, type: !22)
!53 = !DILocation(line: 32, column: 14, scope: !51)
!54 = !DILocation(line: 33, column: 14, scope: !51)
!55 = !DILocation(line: 33, column: 23, scope: !51)
!56 = !DILocation(line: 33, column: 31, scope: !51)
!57 = !DILocalVariable(name: "pointer_to_integer", scope: !51, file: !1, line: 35, type: !3)
!58 = !DILocation(line: 35, column: 10, scope: !51)
!59 = !DILocalVariable(name: "i", scope: !60, file: !1, line: 41, type: !4)
!60 = distinct !DILexicalBlock(scope: !51, file: !1, line: 41, column: 5)
!61 = !DILocation(line: 41, column: 14, scope: !60)
!62 = !DILocation(line: 41, column: 10, scope: !60)
!63 = !DILocation(line: 41, column: 21, scope: !64)
!64 = distinct !DILexicalBlock(scope: !60, file: !1, line: 41, column: 5)
!65 = !DILocation(line: 41, column: 23, scope: !64)
!66 = !DILocation(line: 41, column: 5, scope: !60)
!67 = !DILocation(line: 42, column: 32, scope: !68)
!68 = distinct !DILexicalBlock(scope: !64, file: !1, line: 41, column: 34)
!69 = !DILocation(line: 42, column: 31, scope: !68)
!70 = !DILocation(line: 42, column: 51, scope: !68)
!71 = !DILocation(line: 42, column: 10, scope: !68)
!72 = !DILocation(line: 42, column: 29, scope: !68)
!73 = !DILocation(line: 43, column: 5, scope: !68)
!74 = !DILocation(line: 41, column: 30, scope: !64)
!75 = !DILocation(line: 41, column: 5, scope: !64)
!76 = distinct !{!76, !66, !77, !78, !79}
!77 = !DILocation(line: 43, column: 5, scope: !60)
!78 = !{!"llvm.loop.mustprogress"}
!79 = !{!"pallas.loopInv", !80, !81, !83, !85, !87}
!80 = !{!"pallas.srcLoc", i64 37, i64 5, i64 40, i64 55}
!81 = !{!82, ptr @PALLAS_SPEC_4, !52, !57, !59}
!82 = !{!"pallas.srcLoc", i64 37, i64 9, i64 37, i64 41}
!83 = !{!84, ptr @PALLAS_SPEC_5, !52, !57, !59}
!84 = !{!"pallas.srcLoc", i64 38, i64 9, i64 38, i64 62}
!85 = !{!86, ptr @PALLAS_SPEC_6, !52, !57, !59}
!86 = !{!"pallas.srcLoc", i64 39, i64 9, i64 39, i64 72}
!87 = !{!88, ptr @PALLAS_SPEC_7, !52, !57, !59}
!88 = !{!"pallas.srcLoc", i64 40, i64 9, i64 40, i64 53}
!89 = !DILocation(line: 46, column: 14, scope: !51)
!90 = !{!91, !92}
!91 = !{!"pallas.srcLoc", i64 45, i64 5, i64 45, i64 48}
!92 = !{!"pallas.assert", !93, ptr @PALLAS_SPEC_16, !52, !57, !59}
!93 = !{!"pallas.srcLoc", i64 45, i64 9, i64 45, i64 46}
!94 = !DILocation(line: 46, column: 23, scope: !51)
!95 = !DILocation(line: 46, column: 31, scope: !51)
!96 = !DILocalVariable(name: "j", scope: !97, file: !1, line: 53, type: !4)
!97 = distinct !DILexicalBlock(scope: !51, file: !1, line: 53, column: 5)
!98 = !DILocation(line: 53, column: 14, scope: !97)
!99 = !DILocation(line: 53, column: 10, scope: !97)
!100 = !DILocation(line: 53, column: 21, scope: !101)
!101 = distinct !DILexicalBlock(scope: !97, file: !1, line: 53, column: 5)
!102 = !DILocation(line: 53, column: 23, scope: !101)
!103 = !DILocation(line: 53, column: 5, scope: !97)
!104 = !DILocation(line: 54, column: 32, scope: !105)
!105 = distinct !DILexicalBlock(scope: !101, file: !1, line: 53, column: 34)
!106 = !DILocation(line: 54, column: 31, scope: !105)
!107 = !DILocation(line: 54, column: 51, scope: !105)
!108 = !DILocation(line: 54, column: 10, scope: !105)
!109 = !DILocation(line: 54, column: 29, scope: !105)
!110 = !DILocation(line: 55, column: 5, scope: !105)
!111 = !DILocation(line: 53, column: 30, scope: !101)
!112 = !DILocation(line: 53, column: 5, scope: !101)
!113 = distinct !{!113, !103, !114, !78, !115}
!114 = !DILocation(line: 55, column: 5, scope: !97)
!115 = !{!"pallas.loopInv", !116, !117, !119, !121, !123}
!116 = !{!"pallas.srcLoc", i64 49, i64 5, i64 52, i64 55}
!117 = !{!118, ptr @PALLAS_SPEC_8, !52, !57, !59, !96}
!118 = !{!"pallas.srcLoc", i64 49, i64 9, i64 49, i64 41}
!119 = !{!120, ptr @PALLAS_SPEC_9, !52, !57, !59, !96}
!120 = !{!"pallas.srcLoc", i64 50, i64 9, i64 50, i64 62}
!121 = !{!122, ptr @PALLAS_SPEC_10, !52, !57, !59, !96}
!122 = !{!"pallas.srcLoc", i64 51, i64 9, i64 51, i64 64}
!123 = !{!124, ptr @PALLAS_SPEC_11, !52, !57, !59, !96}
!124 = !{!"pallas.srcLoc", i64 52, i64 9, i64 52, i64 53}
!125 = !DILocation(line: 58, column: 1, scope: !51)
!126 = !{!127, !128}
!127 = !{!"pallas.srcLoc", i64 57, i64 5, i64 57, i64 48}
!128 = !{!"pallas.assert", !129, ptr @PALLAS_SPEC_17, !52, !57, !59, !96}
!129 = !{!"pallas.srcLoc", i64 57, i64 9, i64 57, i64 46}
!130 = distinct !DISubprogram(name: "increaseByOne", scope: !1, file: !1, line: 64, type: !131, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!131 = !DISubroutineType(types: !132)
!132 = !{null, !3}
!133 = !{!134, i1 false, !135, !138, !140, !142}
!134 = !{!"pallas.srcLoc", i64 60, i64 1, i64 63, i64 34}
!135 = !{!"pallas.requires", !136, ptr @PALLAS_SPEC_0, !137}
!136 = !{!"pallas.srcLoc", i64 60, i64 5, i64 60, i64 23}
!137 = !DILocalVariable(name: "a", arg: 1, scope: !130, file: !1, line: 64, type: !3)
!138 = !{!"pallas.requires", !139, ptr @PALLAS_SPEC_1, !137}
!139 = !{!"pallas.srcLoc", i64 61, i64 1, i64 61, i64 32}
!140 = !{!"pallas.ensures", !141, ptr @PALLAS_SPEC_2, !137}
!141 = !{!"pallas.srcLoc", i64 62, i64 1, i64 62, i64 31}
!142 = !{!"pallas.ensures", !143, ptr @PALLAS_SPEC_3, !137}
!143 = !{!"pallas.srcLoc", i64 63, i64 1, i64 63, i64 32}
!144 = !DILocation(line: 64, column: 25, scope: !130)
!145 = !DILocation(line: 65, column: 6, scope: !130)
!146 = !DILocation(line: 65, column: 8, scope: !130)
!147 = !DILocation(line: 66, column: 1, scope: !130)
!148 = distinct !DISubprogram(name: "callWithCast", scope: !1, file: !1, line: 68, type: !18, scopeLine: 68, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!149 = !DILocalVariable(name: "struct_b", scope: !148, file: !1, line: 69, type: !22)
!150 = !DILocation(line: 69, column: 14, scope: !148)
!151 = !DILocation(line: 70, column: 14, scope: !148)
!152 = !DILocation(line: 70, column: 23, scope: !148)
!153 = !DILocation(line: 70, column: 31, scope: !148)
!154 = !DILocalVariable(name: "pointer_to_integer", scope: !148, file: !1, line: 72, type: !3)
!155 = !DILocation(line: 72, column: 10, scope: !148)
!156 = !DILocation(line: 73, column: 19, scope: !148)
!157 = !DILocation(line: 73, column: 5, scope: !148)
!158 = !DILocation(line: 76, column: 1, scope: !148)
!159 = !{!160, !161}
!160 = !{!"pallas.srcLoc", i64 75, i64 5, i64 75, i64 49}
!161 = !{!"pallas.assert", !162, ptr @PALLAS_SPEC_18, !149, !154}
!162 = !{!"pallas.srcLoc", i64 75, i64 9, i64 75, i64 47}
!163 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 60, type: !164, scopeLine: 60, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!164 = !DISubroutineType(types: !165)
!165 = !{!29, !3}
!166 = !{!""}
!167 = !DILocalVariable(name: "a", arg: 1, scope: !163, file: !1, line: 60, type: !3)
!168 = !DILocation(line: 0, scope: !163)
!169 = !DILocation(line: 60, column: 16, scope: !163)
!170 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 61, type: !164, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!171 = !DILocalVariable(name: "a", arg: 1, scope: !170, file: !1, line: 61, type: !3)
!172 = !DILocation(line: 0, scope: !170)
!173 = !DILocation(line: 61, column: 19, scope: !170)
!174 = !DILocation(line: 61, column: 10, scope: !170)
!175 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 62, type: !164, scopeLine: 62, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!176 = !DILocalVariable(name: "a", arg: 1, scope: !175, file: !1, line: 62, type: !3)
!177 = !DILocation(line: 0, scope: !175)
!178 = !DILocation(line: 62, column: 18, scope: !175)
!179 = !DILocation(line: 62, column: 9, scope: !175)
!180 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 63, type: !164, scopeLine: 63, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!181 = !DILocalVariable(name: "a", arg: 1, scope: !180, file: !1, line: 63, type: !3)
!182 = !DILocation(line: 0, scope: !180)
!183 = !DILocation(line: 63, column: 9, scope: !180)
!184 = !DILocation(line: 63, column: 25, scope: !180)
!185 = !DILocation(line: 63, column: 15, scope: !180)
!186 = !DILocation(line: 63, column: 29, scope: !180)
!187 = !DILocation(line: 63, column: 12, scope: !180)
!188 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 37, type: !189, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!189 = !DISubroutineType(types: !190)
!190 = !{!29, !191, !3, !4}
!191 = !DIDerivedType(tag: DW_TAG_typedef, name: "B", file: !6, line: 14, baseType: !192)
!192 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "B", file: !6, line: 9, size: 64, elements: !193)
!193 = !{!194}
!194 = !DIDerivedType(tag: DW_TAG_member, name: "struct_a", scope: !192, file: !6, line: 10, baseType: !195, size: 64)
!195 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "A", file: !6, line: 4, size: 64, elements: !196)
!196 = !{!197, !198}
!197 = !DIDerivedType(tag: DW_TAG_member, name: "integer", scope: !195, file: !6, line: 5, baseType: !4, size: 32)
!198 = !DIDerivedType(tag: DW_TAG_member, name: "boolean", scope: !195, file: !6, line: 6, baseType: !29, size: 8, offset: 32)
!199 = !DILocalVariable(name: "struct_b", arg: 1, scope: !188, file: !1, line: 37, type: !191)
!200 = !DILocation(line: 0, scope: !188)
!201 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !188, file: !1, line: 37, type: !3)
!202 = !DILocalVariable(name: "i", arg: 3, scope: !188, file: !1, line: 37, type: !4)
!203 = !DILocation(line: 37, column: 26, scope: !188)
!204 = !DILocation(line: 37, column: 31, scope: !188)
!205 = !DILocation(line: 37, column: 36, scope: !188)
!206 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 38, type: !189, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!207 = !DILocalVariable(name: "struct_b", arg: 1, scope: !206, file: !1, line: 38, type: !191)
!208 = !DILocation(line: 0, scope: !206)
!209 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !206, file: !1, line: 38, type: !3)
!210 = !DILocalVariable(name: "i", arg: 3, scope: !206, file: !1, line: 38, type: !4)
!211 = !DILocation(line: 38, column: 43, scope: !206)
!212 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 39, type: !189, scopeLine: 39, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!213 = !DILocalVariable(name: "struct_b", arg: 1, scope: !212, file: !1, line: 39, type: !191)
!214 = !DILocation(line: 0, scope: !212)
!215 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !212, file: !1, line: 39, type: !3)
!216 = !DILocalVariable(name: "i", arg: 3, scope: !212, file: !1, line: 39, type: !4)
!217 = !DILocation(line: 39, column: 40, scope: !212)
!218 = !DILocation(line: 39, column: 49, scope: !212)
!219 = !DILocation(line: 39, column: 58, scope: !212)
!220 = !DILocation(line: 39, column: 24, scope: !212)
!221 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !1, file: !1, line: 40, type: !189, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!222 = !DILocalVariable(name: "struct_b", arg: 1, scope: !221, file: !1, line: 40, type: !191)
!223 = !DILocation(line: 0, scope: !221)
!224 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !221, file: !1, line: 40, type: !3)
!225 = !DILocalVariable(name: "i", arg: 3, scope: !221, file: !1, line: 40, type: !4)
!226 = !DILocation(line: 40, column: 24, scope: !221)
!227 = !DILocation(line: 40, column: 50, scope: !221)
!228 = !DILocation(line: 40, column: 44, scope: !221)
!229 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !1, file: !1, line: 49, type: !230, scopeLine: 49, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!230 = !DISubroutineType(types: !231)
!231 = !{!29, !191, !3, !4, !4}
!232 = !DILocalVariable(name: "struct_b", arg: 1, scope: !229, file: !1, line: 49, type: !191)
!233 = !DILocation(line: 0, scope: !229)
!234 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !229, file: !1, line: 49, type: !3)
!235 = !DILocalVariable(name: "i", arg: 3, scope: !229, file: !1, line: 49, type: !4)
!236 = !DILocalVariable(name: "j", arg: 4, scope: !229, file: !1, line: 49, type: !4)
!237 = !DILocation(line: 49, column: 26, scope: !229)
!238 = !DILocation(line: 49, column: 31, scope: !229)
!239 = !DILocation(line: 49, column: 36, scope: !229)
!240 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !1, file: !1, line: 50, type: !230, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!241 = !DILocalVariable(name: "struct_b", arg: 1, scope: !240, file: !1, line: 50, type: !191)
!242 = !DILocation(line: 0, scope: !240)
!243 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !240, file: !1, line: 50, type: !3)
!244 = !DILocalVariable(name: "i", arg: 3, scope: !240, file: !1, line: 50, type: !4)
!245 = !DILocalVariable(name: "j", arg: 4, scope: !240, file: !1, line: 50, type: !4)
!246 = !DILocation(line: 50, column: 43, scope: !240)
!247 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !1, file: !1, line: 51, type: !230, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!248 = !DILocalVariable(name: "struct_b", arg: 1, scope: !247, file: !1, line: 51, type: !191)
!249 = !DILocation(line: 0, scope: !247)
!250 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !247, file: !1, line: 51, type: !3)
!251 = !DILocalVariable(name: "i", arg: 3, scope: !247, file: !1, line: 51, type: !4)
!252 = !DILocalVariable(name: "j", arg: 4, scope: !247, file: !1, line: 51, type: !4)
!253 = !DILocation(line: 51, column: 50, scope: !247)
!254 = !DILocation(line: 51, column: 24, scope: !247)
!255 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 52, type: !230, scopeLine: 52, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!256 = !DILocalVariable(name: "struct_b", arg: 1, scope: !255, file: !1, line: 52, type: !191)
!257 = !DILocation(line: 0, scope: !255)
!258 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !255, file: !1, line: 52, type: !3)
!259 = !DILocalVariable(name: "i", arg: 3, scope: !255, file: !1, line: 52, type: !4)
!260 = !DILocalVariable(name: "j", arg: 4, scope: !255, file: !1, line: 52, type: !4)
!261 = !DILocation(line: 52, column: 24, scope: !255)
!262 = !DILocation(line: 52, column: 50, scope: !255)
!263 = !DILocation(line: 52, column: 44, scope: !255)
!264 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 23, type: !265, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!265 = !DISubroutineType(types: !266)
!266 = !{!29, !191, !3}
!267 = !DILocalVariable(name: "struct_b", arg: 1, scope: !264, file: !1, line: 23, type: !191)
!268 = !DILocation(line: 0, scope: !264)
!269 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !264, file: !1, line: 23, type: !3)
!270 = !DILocation(line: 23, column: 16, scope: !264)
!271 = !DILocation(line: 23, column: 36, scope: !264)
!272 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !1, file: !1, line: 24, type: !265, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!273 = !DILocalVariable(name: "struct_b", arg: 1, scope: !272, file: !1, line: 24, type: !191)
!274 = !DILocation(line: 0, scope: !272)
!275 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !272, file: !1, line: 24, type: !3)
!276 = !DILocation(line: 24, column: 45, scope: !272)
!277 = !DILocation(line: 24, column: 54, scope: !272)
!278 = !DILocation(line: 24, column: 32, scope: !272)
!279 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !1, file: !1, line: 25, type: !265, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!280 = !DILocalVariable(name: "struct_b", arg: 1, scope: !279, file: !1, line: 25, type: !191)
!281 = !DILocation(line: 0, scope: !279)
!282 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !279, file: !1, line: 25, type: !3)
!283 = !DILocation(line: 25, column: 52, scope: !279)
!284 = !DILocation(line: 25, column: 32, scope: !279)
!285 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !1, file: !1, line: 27, type: !265, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!286 = !DILocalVariable(name: "struct_b", arg: 1, scope: !285, file: !1, line: 27, type: !191)
!287 = !DILocation(line: 0, scope: !285)
!288 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !285, file: !1, line: 27, type: !3)
!289 = !DILocation(line: 27, column: 25, scope: !285)
!290 = !DILocation(line: 27, column: 34, scope: !285)
!291 = !DILocation(line: 27, column: 42, scope: !285)
!292 = distinct !DISubprogram(name: "PALLAS_SPEC_16", scope: !1, file: !1, line: 45, type: !189, scopeLine: 45, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!293 = !DILocalVariable(name: "struct_b", arg: 1, scope: !292, file: !1, line: 45, type: !191)
!294 = !DILocation(line: 0, scope: !292)
!295 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !292, file: !1, line: 45, type: !3)
!296 = !DILocalVariable(name: "i", arg: 3, scope: !292, file: !1, line: 45, type: !4)
!297 = !DILocation(line: 45, column: 25, scope: !292)
!298 = !DILocation(line: 45, column: 34, scope: !292)
!299 = !DILocation(line: 45, column: 42, scope: !292)
!300 = distinct !DISubprogram(name: "PALLAS_SPEC_17", scope: !1, file: !1, line: 57, type: !230, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!301 = !DILocalVariable(name: "struct_b", arg: 1, scope: !300, file: !1, line: 57, type: !191)
!302 = !DILocation(line: 0, scope: !300)
!303 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !300, file: !1, line: 57, type: !3)
!304 = !DILocalVariable(name: "i", arg: 3, scope: !300, file: !1, line: 57, type: !4)
!305 = !DILocalVariable(name: "j", arg: 4, scope: !300, file: !1, line: 57, type: !4)
!306 = !DILocation(line: 57, column: 25, scope: !300)
!307 = !DILocation(line: 57, column: 34, scope: !300)
!308 = !DILocation(line: 57, column: 42, scope: !300)
!309 = distinct !DISubprogram(name: "PALLAS_SPEC_18", scope: !1, file: !1, line: 75, type: !265, scopeLine: 75, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!310 = !DILocalVariable(name: "struct_b", arg: 1, scope: !309, file: !1, line: 75, type: !191)
!311 = !DILocation(line: 0, scope: !309)
!312 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !309, file: !1, line: 75, type: !3)
!313 = !DILocation(line: 75, column: 25, scope: !309)
!314 = !DILocation(line: 75, column: 34, scope: !309)
!315 = !DILocation(line: 75, column: 42, scope: !309)
!316 = !{!"pallas.old"}
!317 = !{!"pallas.perm"}
!318 = !{!"pallas.fracOf"}
