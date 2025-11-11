; ModuleID = 'tmp/tmp_ir_source0.ll'
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
  store i32 10, ptr %5, align 4, !dbg !46
  ret void, !dbg !47, !pallas.stmntBlock !48
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @castRemainsValidInLoop() #0 !dbg !52 {
  %1 = alloca %struct.B, align 4
  %2 = alloca ptr, align 8
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  call void @llvm.dbg.declare(metadata ptr %1, metadata !53, metadata !DIExpression()), !dbg !54
  %5 = getelementptr inbounds %struct.B, ptr %1, i32 0, i32 0, !dbg !55
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !56
  store i32 10, ptr %6, align 4, !dbg !57
  call void @llvm.dbg.declare(metadata ptr %2, metadata !58, metadata !DIExpression()), !dbg !59
  store ptr %1, ptr %2, align 8, !dbg !59
  call void @llvm.dbg.declare(metadata ptr %3, metadata !60, metadata !DIExpression()), !dbg !62
  store i32 0, ptr %3, align 4, !dbg !62
  br label %7, !dbg !63

7:                                                ; preds = %15, %0
  %8 = load i32, ptr %3, align 4, !dbg !64
  %9 = icmp slt i32 %8, 10, !dbg !66
  br i1 %9, label %10, label %18, !dbg !67

10:                                               ; preds = %7
  %11 = load ptr, ptr %2, align 8, !dbg !68
  %12 = load i32, ptr %11, align 4, !dbg !70
  %13 = sub nsw i32 %12, 1, !dbg !71
  %14 = load ptr, ptr %2, align 8, !dbg !72
  store i32 %13, ptr %14, align 4, !dbg !73
  br label %15, !dbg !74

15:                                               ; preds = %10
  %16 = load i32, ptr %3, align 4, !dbg !75
  %17 = add nsw i32 %16, 1, !dbg !75
  store i32 %17, ptr %3, align 4, !dbg !75
  br label %7, !dbg !76, !llvm.loop !77

18:                                               ; preds = %7
  %19 = getelementptr inbounds %struct.B, ptr %1, i32 0, i32 0, !dbg !90, !pallas.stmntBlock !91
  %20 = getelementptr inbounds %struct.A, ptr %19, i32 0, i32 0, !dbg !95
  store i32 10, ptr %20, align 4, !dbg !96
  call void @llvm.dbg.declare(metadata ptr %4, metadata !97, metadata !DIExpression()), !dbg !99
  store i32 0, ptr %4, align 4, !dbg !99
  br label %21, !dbg !100

21:                                               ; preds = %29, %18
  %22 = load i32, ptr %4, align 4, !dbg !101
  %23 = icmp slt i32 %22, 10, !dbg !103
  br i1 %23, label %24, label %32, !dbg !104

24:                                               ; preds = %21
  %25 = load ptr, ptr %2, align 8, !dbg !105
  %26 = load i32, ptr %25, align 4, !dbg !107
  %27 = sub nsw i32 %26, 1, !dbg !108
  %28 = load ptr, ptr %2, align 8, !dbg !109
  store i32 %27, ptr %28, align 4, !dbg !110
  br label %29, !dbg !111

29:                                               ; preds = %24
  %30 = load i32, ptr %4, align 4, !dbg !112
  %31 = add nsw i32 %30, 1, !dbg !112
  store i32 %31, ptr %4, align 4, !dbg !112
  br label %21, !dbg !113, !llvm.loop !114

32:                                               ; preds = %21
  ret void, !dbg !126, !pallas.stmntBlock !127
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @increaseByOne(ptr noundef %0) #0 !dbg !131 !pallas.fcontract !134 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !138, metadata !DIExpression()), !dbg !145
  %3 = load ptr, ptr %2, align 8, !dbg !146
  %4 = load i32, ptr %3, align 4, !dbg !147
  %5 = add nsw i32 %4, 1, !dbg !147
  store i32 %5, ptr %3, align 4, !dbg !147
  ret void, !dbg !148
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @callWithCast() #0 !dbg !149 {
  %1 = alloca %struct.B, align 4
  %2 = alloca ptr, align 8
  call void @llvm.dbg.declare(metadata ptr %1, metadata !150, metadata !DIExpression()), !dbg !151
  %3 = getelementptr inbounds %struct.B, ptr %1, i32 0, i32 0, !dbg !152
  %4 = getelementptr inbounds %struct.A, ptr %3, i32 0, i32 0, !dbg !153
  store i32 15, ptr %4, align 4, !dbg !154
  call void @llvm.dbg.declare(metadata ptr %2, metadata !155, metadata !DIExpression()), !dbg !156
  store ptr %1, ptr %2, align 8, !dbg !156
  %5 = load ptr, ptr %2, align 8, !dbg !157
  call void @increaseByOne(ptr noundef %5), !dbg !158
  ret void, !dbg !159, !pallas.stmntBlock !160
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !164 !pallas.exprWrapper !167 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !168, metadata !DIExpression()), !dbg !169
  %2 = icmp ne ptr %0, null, !dbg !170
  ret i1 %2, !dbg !169
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !171 !pallas.exprWrapper !167 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !172, metadata !DIExpression()), !dbg !173
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !174
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !175
  ret i1 %3, !dbg !173
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !176 !pallas.exprWrapper !167 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !177, metadata !DIExpression()), !dbg !178
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !179
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !180
  ret i1 %3, !dbg !178
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !181 !pallas.exprWrapper !167 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !182, metadata !DIExpression()), !dbg !183
  %2 = load i32, ptr %0, align 4, !dbg !184
  %3 = load i32, ptr %0, align 4, !dbg !185
  %4 = call i32 @pallas.old.0(i32 noundef %3), !dbg !186
  %5 = add nsw i32 %4, 1, !dbg !187
  %6 = icmp eq i32 %2, %5, !dbg !188
  ret i1 %6, !dbg !183
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !189 !pallas.exprWrapper !167 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !200, metadata !DIExpression()), !dbg !201
  call void @llvm.dbg.value(metadata ptr %1, metadata !202, metadata !DIExpression()), !dbg !201
  call void @llvm.dbg.value(metadata i32 %2, metadata !203, metadata !DIExpression()), !dbg !201
  %6 = icmp sle i32 0, %2, !dbg !204
  br i1 %6, label %7, label %9, !dbg !205

7:                                                ; preds = %3
  %8 = icmp sle i32 %2, 10, !dbg !206
  br label %9

9:                                                ; preds = %7, %3
  %10 = phi i1 [ false, %3 ], [ %8, %7 ], !dbg !201
  ret i1 %10, !dbg !201
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !207 !pallas.exprWrapper !167 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !208, metadata !DIExpression()), !dbg !209
  call void @llvm.dbg.value(metadata ptr %1, metadata !210, metadata !DIExpression()), !dbg !209
  call void @llvm.dbg.value(metadata i32 %2, metadata !211, metadata !DIExpression()), !dbg !209
  %6 = icmp eq ptr %1, %4, !dbg !212
  ret i1 %6, !dbg !209
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !213 !pallas.exprWrapper !167 {
  %4 = alloca %struct.B, align 4
  %5 = alloca %pallas.fracT, align 8
  %6 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !214, metadata !DIExpression()), !dbg !215
  call void @llvm.dbg.value(metadata ptr %1, metadata !216, metadata !DIExpression()), !dbg !215
  call void @llvm.dbg.value(metadata i32 %2, metadata !217, metadata !DIExpression()), !dbg !215
  %7 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0, !dbg !218
  %8 = getelementptr inbounds %struct.A, ptr %7, i32 0, i32 0, !dbg !219
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 1), !dbg !220
  %9 = call i1 @pallas.perm(ptr noundef %8, ptr noundef byval(%pallas.fracT) %5), !dbg !221
  ret i1 %9, !dbg !215
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !222 !pallas.exprWrapper !167 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !223, metadata !DIExpression()), !dbg !224
  call void @llvm.dbg.value(metadata ptr %1, metadata !225, metadata !DIExpression()), !dbg !224
  call void @llvm.dbg.value(metadata i32 %2, metadata !226, metadata !DIExpression()), !dbg !224
  %6 = load i32, ptr %1, align 4, !dbg !227
  %7 = sub nsw i32 10, %2, !dbg !228
  %8 = icmp eq i32 %6, %7, !dbg !229
  ret i1 %8, !dbg !224
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(i64 %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !230 !pallas.exprWrapper !167 {
  %5 = alloca %struct.B, align 4
  %6 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !233, metadata !DIExpression()), !dbg !234
  call void @llvm.dbg.value(metadata ptr %1, metadata !235, metadata !DIExpression()), !dbg !234
  call void @llvm.dbg.value(metadata i32 %2, metadata !236, metadata !DIExpression()), !dbg !234
  call void @llvm.dbg.value(metadata i32 %3, metadata !237, metadata !DIExpression()), !dbg !234
  %7 = icmp sle i32 0, %3, !dbg !238
  br i1 %7, label %8, label %10, !dbg !239

8:                                                ; preds = %4
  %9 = icmp sle i32 %3, 10, !dbg !240
  br label %10

10:                                               ; preds = %8, %4
  %11 = phi i1 [ false, %4 ], [ %9, %8 ], !dbg !234
  ret i1 %11, !dbg !234
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(i64 %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !241 !pallas.exprWrapper !167 {
  %5 = alloca %struct.B, align 4
  %6 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !242, metadata !DIExpression()), !dbg !243
  call void @llvm.dbg.value(metadata ptr %1, metadata !244, metadata !DIExpression()), !dbg !243
  call void @llvm.dbg.value(metadata i32 %2, metadata !245, metadata !DIExpression()), !dbg !243
  call void @llvm.dbg.value(metadata i32 %3, metadata !246, metadata !DIExpression()), !dbg !243
  %7 = icmp eq ptr %1, %5, !dbg !247
  ret i1 %7, !dbg !243
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(i64 %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !248 !pallas.exprWrapper !167 {
  %5 = alloca %struct.B, align 4
  %6 = alloca %pallas.fracT, align 8
  %7 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0
  store i64 %0, ptr %7, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !249, metadata !DIExpression()), !dbg !250
  call void @llvm.dbg.value(metadata ptr %1, metadata !251, metadata !DIExpression()), !dbg !250
  call void @llvm.dbg.value(metadata i32 %2, metadata !252, metadata !DIExpression()), !dbg !250
  call void @llvm.dbg.value(metadata i32 %3, metadata !253, metadata !DIExpression()), !dbg !250
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %6, i32 noundef 1, i32 noundef 1), !dbg !254
  %8 = call i1 @pallas.perm(ptr noundef %1, ptr noundef byval(%pallas.fracT) %6), !dbg !255
  ret i1 %8, !dbg !250
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(i64 %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !256 !pallas.exprWrapper !167 {
  %5 = alloca %struct.B, align 4
  %6 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !257, metadata !DIExpression()), !dbg !258
  call void @llvm.dbg.value(metadata ptr %1, metadata !259, metadata !DIExpression()), !dbg !258
  call void @llvm.dbg.value(metadata i32 %2, metadata !260, metadata !DIExpression()), !dbg !258
  call void @llvm.dbg.value(metadata i32 %3, metadata !261, metadata !DIExpression()), !dbg !258
  %7 = load i32, ptr %1, align 4, !dbg !262
  %8 = sub nsw i32 10, %3, !dbg !263
  %9 = icmp eq i32 %7, %8, !dbg !264
  ret i1 %9, !dbg !258
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(i64 %0, ptr noundef %1) #0 !dbg !265 !pallas.exprWrapper !167 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !268, metadata !DIExpression()), !dbg !269
  call void @llvm.dbg.value(metadata ptr %1, metadata !270, metadata !DIExpression()), !dbg !269
  %5 = load i32, ptr %1, align 4, !dbg !271
  %6 = icmp eq i32 %5, 5, !dbg !272
  ret i1 %6, !dbg !269
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(i64 %0, ptr noundef %1) #0 !dbg !273 !pallas.exprWrapper !167 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !274, metadata !DIExpression()), !dbg !275
  call void @llvm.dbg.value(metadata ptr %1, metadata !276, metadata !DIExpression()), !dbg !275
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !277
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !278
  %7 = icmp eq ptr %1, %6, !dbg !279
  ret i1 %7, !dbg !275
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(i64 %0, ptr noundef %1) #0 !dbg !280 !pallas.exprWrapper !167 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !281, metadata !DIExpression()), !dbg !282
  call void @llvm.dbg.value(metadata ptr %1, metadata !283, metadata !DIExpression()), !dbg !282
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !284
  %6 = icmp eq ptr %1, %5, !dbg !285
  ret i1 %6, !dbg !282
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(i64 %0, ptr noundef %1) #0 !dbg !286 !pallas.exprWrapper !167 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !287, metadata !DIExpression()), !dbg !288
  call void @llvm.dbg.value(metadata ptr %1, metadata !289, metadata !DIExpression()), !dbg !288
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !290
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !291
  %7 = load i32, ptr %6, align 4, !dbg !291
  %8 = icmp eq i32 %7, 10, !dbg !292
  ret i1 %8, !dbg !288
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(i64 %0, ptr noundef %1, i32 noundef %2) #0 !dbg !293 !pallas.exprWrapper !167 {
  %4 = alloca %struct.B, align 4
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !294, metadata !DIExpression()), !dbg !295
  call void @llvm.dbg.value(metadata ptr %1, metadata !296, metadata !DIExpression()), !dbg !295
  call void @llvm.dbg.value(metadata i32 %2, metadata !297, metadata !DIExpression()), !dbg !295
  %6 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0, !dbg !298
  %7 = getelementptr inbounds %struct.A, ptr %6, i32 0, i32 0, !dbg !299
  %8 = load i32, ptr %7, align 4, !dbg !299
  %9 = icmp eq i32 %8, 0, !dbg !300
  ret i1 %9, !dbg !295
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(i64 %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !301 !pallas.exprWrapper !167 {
  %5 = alloca %struct.B, align 4
  %6 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0
  store i64 %0, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !302, metadata !DIExpression()), !dbg !303
  call void @llvm.dbg.value(metadata ptr %1, metadata !304, metadata !DIExpression()), !dbg !303
  call void @llvm.dbg.value(metadata i32 %2, metadata !305, metadata !DIExpression()), !dbg !303
  call void @llvm.dbg.value(metadata i32 %3, metadata !306, metadata !DIExpression()), !dbg !303
  %7 = getelementptr inbounds %struct.B, ptr %5, i32 0, i32 0, !dbg !307
  %8 = getelementptr inbounds %struct.A, ptr %7, i32 0, i32 0, !dbg !308
  %9 = load i32, ptr %8, align 4, !dbg !308
  %10 = icmp eq i32 %9, 0, !dbg !309
  ret i1 %10, !dbg !303
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(i64 %0, ptr noundef %1) #0 !dbg !310 !pallas.exprWrapper !167 {
  %3 = alloca %struct.B, align 4
  %4 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !311, metadata !DIExpression()), !dbg !312
  call void @llvm.dbg.value(metadata ptr %1, metadata !313, metadata !DIExpression()), !dbg !312
  %5 = getelementptr inbounds %struct.B, ptr %3, i32 0, i32 0, !dbg !314
  %6 = getelementptr inbounds %struct.A, ptr %5, i32 0, i32 0, !dbg !315
  %7 = load i32, ptr %6, align 4, !dbg !315
  %8 = icmp eq i32 %7, 16, !dbg !316
  ret i1 %8, !dbg !312
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !317 i32 @pallas.old.0(i32 noundef)

declare !pallas.specLib !318 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !319 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

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
!6 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "cc5ba85943b8a87e6518ae7bb3b23b2d")
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
!37 = !{!38, !40, !42, !44}
!38 = !{!"pallas.srcLoc", i64 23, i64 5, i64 25, i64 62, !39}
!39 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pointer_casts.c", directory: "", checksumkind: CSK_MD5, checksum: "5f415ed5499174e0d5081bbc2cd18cf0")
!40 = !{!"pallas.assert", !41, ptr @PALLAS_SPEC_12, !21, !34}
!41 = !{!"pallas.srcLoc", i64 23, i64 9, i64 23, i64 40, !39}
!42 = !{!"pallas.assert", !43, ptr @PALLAS_SPEC_13, !21, !34}
!43 = !{!"pallas.srcLoc", i64 24, i64 6, i64 24, i64 61, !39}
!44 = !{!"pallas.assert", !45, ptr @PALLAS_SPEC_14, !21, !34}
!45 = !{!"pallas.srcLoc", i64 25, i64 6, i64 25, i64 60, !39}
!46 = !DILocation(line: 26, column: 25, scope: !17)
!47 = !DILocation(line: 28, column: 1, scope: !17)
!48 = !{!49, !50}
!49 = !{!"pallas.srcLoc", i64 27, i64 5, i64 27, i64 49, !39}
!50 = !{!"pallas.assert", !51, ptr @PALLAS_SPEC_15, !21, !34}
!51 = !{!"pallas.srcLoc", i64 27, i64 9, i64 27, i64 47, !39}
!52 = distinct !DISubprogram(name: "castRemainsValidInLoop", scope: !1, file: !1, line: 31, type: !18, scopeLine: 31, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!53 = !DILocalVariable(name: "struct_b", scope: !52, file: !1, line: 32, type: !22)
!54 = !DILocation(line: 32, column: 14, scope: !52)
!55 = !DILocation(line: 33, column: 14, scope: !52)
!56 = !DILocation(line: 33, column: 23, scope: !52)
!57 = !DILocation(line: 33, column: 31, scope: !52)
!58 = !DILocalVariable(name: "pointer_to_integer", scope: !52, file: !1, line: 35, type: !3)
!59 = !DILocation(line: 35, column: 10, scope: !52)
!60 = !DILocalVariable(name: "i", scope: !61, file: !1, line: 41, type: !4)
!61 = distinct !DILexicalBlock(scope: !52, file: !1, line: 41, column: 5)
!62 = !DILocation(line: 41, column: 14, scope: !61)
!63 = !DILocation(line: 41, column: 10, scope: !61)
!64 = !DILocation(line: 41, column: 21, scope: !65)
!65 = distinct !DILexicalBlock(scope: !61, file: !1, line: 41, column: 5)
!66 = !DILocation(line: 41, column: 23, scope: !65)
!67 = !DILocation(line: 41, column: 5, scope: !61)
!68 = !DILocation(line: 42, column: 32, scope: !69)
!69 = distinct !DILexicalBlock(scope: !65, file: !1, line: 41, column: 34)
!70 = !DILocation(line: 42, column: 31, scope: !69)
!71 = !DILocation(line: 42, column: 51, scope: !69)
!72 = !DILocation(line: 42, column: 10, scope: !69)
!73 = !DILocation(line: 42, column: 29, scope: !69)
!74 = !DILocation(line: 43, column: 5, scope: !69)
!75 = !DILocation(line: 41, column: 30, scope: !65)
!76 = !DILocation(line: 41, column: 5, scope: !65)
!77 = distinct !{!77, !67, !78, !79, !80}
!78 = !DILocation(line: 43, column: 5, scope: !61)
!79 = !{!"llvm.loop.mustprogress"}
!80 = !{!"pallas.loopInv", !81, !82, !84, !86, !88}
!81 = !{!"pallas.srcLoc", i64 37, i64 5, i64 40, i64 55, !39}
!82 = !{!83, ptr @PALLAS_SPEC_4, !53, !58, !60}
!83 = !{!"pallas.srcLoc", i64 37, i64 9, i64 37, i64 41, !39}
!84 = !{!85, ptr @PALLAS_SPEC_5, !53, !58, !60}
!85 = !{!"pallas.srcLoc", i64 38, i64 9, i64 38, i64 62, !39}
!86 = !{!87, ptr @PALLAS_SPEC_6, !53, !58, !60}
!87 = !{!"pallas.srcLoc", i64 39, i64 9, i64 39, i64 72, !39}
!88 = !{!89, ptr @PALLAS_SPEC_7, !53, !58, !60}
!89 = !{!"pallas.srcLoc", i64 40, i64 9, i64 40, i64 53, !39}
!90 = !DILocation(line: 46, column: 14, scope: !52)
!91 = !{!92, !93}
!92 = !{!"pallas.srcLoc", i64 45, i64 5, i64 45, i64 48, !39}
!93 = !{!"pallas.assert", !94, ptr @PALLAS_SPEC_16, !53, !58, !60}
!94 = !{!"pallas.srcLoc", i64 45, i64 9, i64 45, i64 46, !39}
!95 = !DILocation(line: 46, column: 23, scope: !52)
!96 = !DILocation(line: 46, column: 31, scope: !52)
!97 = !DILocalVariable(name: "j", scope: !98, file: !1, line: 53, type: !4)
!98 = distinct !DILexicalBlock(scope: !52, file: !1, line: 53, column: 5)
!99 = !DILocation(line: 53, column: 14, scope: !98)
!100 = !DILocation(line: 53, column: 10, scope: !98)
!101 = !DILocation(line: 53, column: 21, scope: !102)
!102 = distinct !DILexicalBlock(scope: !98, file: !1, line: 53, column: 5)
!103 = !DILocation(line: 53, column: 23, scope: !102)
!104 = !DILocation(line: 53, column: 5, scope: !98)
!105 = !DILocation(line: 54, column: 32, scope: !106)
!106 = distinct !DILexicalBlock(scope: !102, file: !1, line: 53, column: 34)
!107 = !DILocation(line: 54, column: 31, scope: !106)
!108 = !DILocation(line: 54, column: 51, scope: !106)
!109 = !DILocation(line: 54, column: 10, scope: !106)
!110 = !DILocation(line: 54, column: 29, scope: !106)
!111 = !DILocation(line: 55, column: 5, scope: !106)
!112 = !DILocation(line: 53, column: 30, scope: !102)
!113 = !DILocation(line: 53, column: 5, scope: !102)
!114 = distinct !{!114, !104, !115, !79, !116}
!115 = !DILocation(line: 55, column: 5, scope: !98)
!116 = !{!"pallas.loopInv", !117, !118, !120, !122, !124}
!117 = !{!"pallas.srcLoc", i64 49, i64 5, i64 52, i64 55, !39}
!118 = !{!119, ptr @PALLAS_SPEC_8, !53, !58, !60, !97}
!119 = !{!"pallas.srcLoc", i64 49, i64 9, i64 49, i64 41, !39}
!120 = !{!121, ptr @PALLAS_SPEC_9, !53, !58, !60, !97}
!121 = !{!"pallas.srcLoc", i64 50, i64 9, i64 50, i64 62, !39}
!122 = !{!123, ptr @PALLAS_SPEC_10, !53, !58, !60, !97}
!123 = !{!"pallas.srcLoc", i64 51, i64 9, i64 51, i64 64, !39}
!124 = !{!125, ptr @PALLAS_SPEC_11, !53, !58, !60, !97}
!125 = !{!"pallas.srcLoc", i64 52, i64 9, i64 52, i64 53, !39}
!126 = !DILocation(line: 58, column: 1, scope: !52)
!127 = !{!128, !129}
!128 = !{!"pallas.srcLoc", i64 57, i64 5, i64 57, i64 48, !39}
!129 = !{!"pallas.assert", !130, ptr @PALLAS_SPEC_17, !53, !58, !60, !97}
!130 = !{!"pallas.srcLoc", i64 57, i64 9, i64 57, i64 46, !39}
!131 = distinct !DISubprogram(name: "increaseByOne", scope: !1, file: !1, line: 64, type: !132, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!132 = !DISubroutineType(types: !133)
!133 = !{null, !3}
!134 = !{!135, i1 false, i1 false, !136, !139, !141, !143}
!135 = !{!"pallas.srcLoc", i64 60, i64 1, i64 63, i64 34, !39}
!136 = !{!"pallas.requires", !137, ptr @PALLAS_SPEC_0, !138}
!137 = !{!"pallas.srcLoc", i64 60, i64 5, i64 60, i64 23, !39}
!138 = !DILocalVariable(name: "a", arg: 1, scope: !131, file: !1, line: 64, type: !3)
!139 = !{!"pallas.requires", !140, ptr @PALLAS_SPEC_1, !138}
!140 = !{!"pallas.srcLoc", i64 61, i64 1, i64 61, i64 32, !39}
!141 = !{!"pallas.ensures", !142, ptr @PALLAS_SPEC_2, !138}
!142 = !{!"pallas.srcLoc", i64 62, i64 1, i64 62, i64 31, !39}
!143 = !{!"pallas.ensures", !144, ptr @PALLAS_SPEC_3, !138}
!144 = !{!"pallas.srcLoc", i64 63, i64 1, i64 63, i64 32, !39}
!145 = !DILocation(line: 64, column: 25, scope: !131)
!146 = !DILocation(line: 65, column: 6, scope: !131)
!147 = !DILocation(line: 65, column: 8, scope: !131)
!148 = !DILocation(line: 66, column: 1, scope: !131)
!149 = distinct !DISubprogram(name: "callWithCast", scope: !1, file: !1, line: 68, type: !18, scopeLine: 68, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!150 = !DILocalVariable(name: "struct_b", scope: !149, file: !1, line: 69, type: !22)
!151 = !DILocation(line: 69, column: 14, scope: !149)
!152 = !DILocation(line: 70, column: 14, scope: !149)
!153 = !DILocation(line: 70, column: 23, scope: !149)
!154 = !DILocation(line: 70, column: 31, scope: !149)
!155 = !DILocalVariable(name: "pointer_to_integer", scope: !149, file: !1, line: 72, type: !3)
!156 = !DILocation(line: 72, column: 10, scope: !149)
!157 = !DILocation(line: 73, column: 19, scope: !149)
!158 = !DILocation(line: 73, column: 5, scope: !149)
!159 = !DILocation(line: 76, column: 1, scope: !149)
!160 = !{!161, !162}
!161 = !{!"pallas.srcLoc", i64 75, i64 5, i64 75, i64 49, !39}
!162 = !{!"pallas.assert", !163, ptr @PALLAS_SPEC_18, !150, !155}
!163 = !{!"pallas.srcLoc", i64 75, i64 9, i64 75, i64 47, !39}
!164 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 60, type: !165, scopeLine: 60, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!165 = !DISubroutineType(types: !166)
!166 = !{!29, !3}
!167 = !{!""}
!168 = !DILocalVariable(name: "a", arg: 1, scope: !164, file: !1, line: 60, type: !3)
!169 = !DILocation(line: 0, scope: !164)
!170 = !DILocation(line: 60, column: 16, scope: !164)
!171 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 61, type: !165, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!172 = !DILocalVariable(name: "a", arg: 1, scope: !171, file: !1, line: 61, type: !3)
!173 = !DILocation(line: 0, scope: !171)
!174 = !DILocation(line: 61, column: 19, scope: !171)
!175 = !DILocation(line: 61, column: 10, scope: !171)
!176 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 62, type: !165, scopeLine: 62, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!177 = !DILocalVariable(name: "a", arg: 1, scope: !176, file: !1, line: 62, type: !3)
!178 = !DILocation(line: 0, scope: !176)
!179 = !DILocation(line: 62, column: 18, scope: !176)
!180 = !DILocation(line: 62, column: 9, scope: !176)
!181 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 63, type: !165, scopeLine: 63, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!182 = !DILocalVariable(name: "a", arg: 1, scope: !181, file: !1, line: 63, type: !3)
!183 = !DILocation(line: 0, scope: !181)
!184 = !DILocation(line: 63, column: 9, scope: !181)
!185 = !DILocation(line: 63, column: 25, scope: !181)
!186 = !DILocation(line: 63, column: 15, scope: !181)
!187 = !DILocation(line: 63, column: 29, scope: !181)
!188 = !DILocation(line: 63, column: 12, scope: !181)
!189 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 37, type: !190, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!190 = !DISubroutineType(types: !191)
!191 = !{!29, !192, !3, !4}
!192 = !DIDerivedType(tag: DW_TAG_typedef, name: "B", file: !6, line: 14, baseType: !193)
!193 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "B", file: !6, line: 9, size: 64, elements: !194)
!194 = !{!195}
!195 = !DIDerivedType(tag: DW_TAG_member, name: "struct_a", scope: !193, file: !6, line: 10, baseType: !196, size: 64)
!196 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "A", file: !6, line: 4, size: 64, elements: !197)
!197 = !{!198, !199}
!198 = !DIDerivedType(tag: DW_TAG_member, name: "integer", scope: !196, file: !6, line: 5, baseType: !4, size: 32)
!199 = !DIDerivedType(tag: DW_TAG_member, name: "boolean", scope: !196, file: !6, line: 6, baseType: !29, size: 8, offset: 32)
!200 = !DILocalVariable(name: "struct_b", arg: 1, scope: !189, file: !1, line: 37, type: !192)
!201 = !DILocation(line: 0, scope: !189)
!202 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !189, file: !1, line: 37, type: !3)
!203 = !DILocalVariable(name: "i", arg: 3, scope: !189, file: !1, line: 37, type: !4)
!204 = !DILocation(line: 37, column: 26, scope: !189)
!205 = !DILocation(line: 37, column: 31, scope: !189)
!206 = !DILocation(line: 37, column: 36, scope: !189)
!207 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 38, type: !190, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!208 = !DILocalVariable(name: "struct_b", arg: 1, scope: !207, file: !1, line: 38, type: !192)
!209 = !DILocation(line: 0, scope: !207)
!210 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !207, file: !1, line: 38, type: !3)
!211 = !DILocalVariable(name: "i", arg: 3, scope: !207, file: !1, line: 38, type: !4)
!212 = !DILocation(line: 38, column: 43, scope: !207)
!213 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 39, type: !190, scopeLine: 39, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!214 = !DILocalVariable(name: "struct_b", arg: 1, scope: !213, file: !1, line: 39, type: !192)
!215 = !DILocation(line: 0, scope: !213)
!216 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !213, file: !1, line: 39, type: !3)
!217 = !DILocalVariable(name: "i", arg: 3, scope: !213, file: !1, line: 39, type: !4)
!218 = !DILocation(line: 39, column: 40, scope: !213)
!219 = !DILocation(line: 39, column: 49, scope: !213)
!220 = !DILocation(line: 39, column: 58, scope: !213)
!221 = !DILocation(line: 39, column: 24, scope: !213)
!222 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !1, file: !1, line: 40, type: !190, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!223 = !DILocalVariable(name: "struct_b", arg: 1, scope: !222, file: !1, line: 40, type: !192)
!224 = !DILocation(line: 0, scope: !222)
!225 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !222, file: !1, line: 40, type: !3)
!226 = !DILocalVariable(name: "i", arg: 3, scope: !222, file: !1, line: 40, type: !4)
!227 = !DILocation(line: 40, column: 24, scope: !222)
!228 = !DILocation(line: 40, column: 50, scope: !222)
!229 = !DILocation(line: 40, column: 44, scope: !222)
!230 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !1, file: !1, line: 49, type: !231, scopeLine: 49, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!231 = !DISubroutineType(types: !232)
!232 = !{!29, !192, !3, !4, !4}
!233 = !DILocalVariable(name: "struct_b", arg: 1, scope: !230, file: !1, line: 49, type: !192)
!234 = !DILocation(line: 0, scope: !230)
!235 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !230, file: !1, line: 49, type: !3)
!236 = !DILocalVariable(name: "i", arg: 3, scope: !230, file: !1, line: 49, type: !4)
!237 = !DILocalVariable(name: "j", arg: 4, scope: !230, file: !1, line: 49, type: !4)
!238 = !DILocation(line: 49, column: 26, scope: !230)
!239 = !DILocation(line: 49, column: 31, scope: !230)
!240 = !DILocation(line: 49, column: 36, scope: !230)
!241 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !1, file: !1, line: 50, type: !231, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!242 = !DILocalVariable(name: "struct_b", arg: 1, scope: !241, file: !1, line: 50, type: !192)
!243 = !DILocation(line: 0, scope: !241)
!244 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !241, file: !1, line: 50, type: !3)
!245 = !DILocalVariable(name: "i", arg: 3, scope: !241, file: !1, line: 50, type: !4)
!246 = !DILocalVariable(name: "j", arg: 4, scope: !241, file: !1, line: 50, type: !4)
!247 = !DILocation(line: 50, column: 43, scope: !241)
!248 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !1, file: !1, line: 51, type: !231, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!249 = !DILocalVariable(name: "struct_b", arg: 1, scope: !248, file: !1, line: 51, type: !192)
!250 = !DILocation(line: 0, scope: !248)
!251 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !248, file: !1, line: 51, type: !3)
!252 = !DILocalVariable(name: "i", arg: 3, scope: !248, file: !1, line: 51, type: !4)
!253 = !DILocalVariable(name: "j", arg: 4, scope: !248, file: !1, line: 51, type: !4)
!254 = !DILocation(line: 51, column: 50, scope: !248)
!255 = !DILocation(line: 51, column: 24, scope: !248)
!256 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 52, type: !231, scopeLine: 52, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!257 = !DILocalVariable(name: "struct_b", arg: 1, scope: !256, file: !1, line: 52, type: !192)
!258 = !DILocation(line: 0, scope: !256)
!259 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !256, file: !1, line: 52, type: !3)
!260 = !DILocalVariable(name: "i", arg: 3, scope: !256, file: !1, line: 52, type: !4)
!261 = !DILocalVariable(name: "j", arg: 4, scope: !256, file: !1, line: 52, type: !4)
!262 = !DILocation(line: 52, column: 24, scope: !256)
!263 = !DILocation(line: 52, column: 50, scope: !256)
!264 = !DILocation(line: 52, column: 44, scope: !256)
!265 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 23, type: !266, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!266 = !DISubroutineType(types: !267)
!267 = !{!29, !192, !3}
!268 = !DILocalVariable(name: "struct_b", arg: 1, scope: !265, file: !1, line: 23, type: !192)
!269 = !DILocation(line: 0, scope: !265)
!270 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !265, file: !1, line: 23, type: !3)
!271 = !DILocation(line: 23, column: 16, scope: !265)
!272 = !DILocation(line: 23, column: 36, scope: !265)
!273 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !1, file: !1, line: 24, type: !266, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!274 = !DILocalVariable(name: "struct_b", arg: 1, scope: !273, file: !1, line: 24, type: !192)
!275 = !DILocation(line: 0, scope: !273)
!276 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !273, file: !1, line: 24, type: !3)
!277 = !DILocation(line: 24, column: 45, scope: !273)
!278 = !DILocation(line: 24, column: 54, scope: !273)
!279 = !DILocation(line: 24, column: 32, scope: !273)
!280 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !1, file: !1, line: 25, type: !266, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!281 = !DILocalVariable(name: "struct_b", arg: 1, scope: !280, file: !1, line: 25, type: !192)
!282 = !DILocation(line: 0, scope: !280)
!283 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !280, file: !1, line: 25, type: !3)
!284 = !DILocation(line: 25, column: 52, scope: !280)
!285 = !DILocation(line: 25, column: 32, scope: !280)
!286 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !1, file: !1, line: 27, type: !266, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!287 = !DILocalVariable(name: "struct_b", arg: 1, scope: !286, file: !1, line: 27, type: !192)
!288 = !DILocation(line: 0, scope: !286)
!289 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !286, file: !1, line: 27, type: !3)
!290 = !DILocation(line: 27, column: 25, scope: !286)
!291 = !DILocation(line: 27, column: 34, scope: !286)
!292 = !DILocation(line: 27, column: 42, scope: !286)
!293 = distinct !DISubprogram(name: "PALLAS_SPEC_16", scope: !1, file: !1, line: 45, type: !190, scopeLine: 45, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!294 = !DILocalVariable(name: "struct_b", arg: 1, scope: !293, file: !1, line: 45, type: !192)
!295 = !DILocation(line: 0, scope: !293)
!296 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !293, file: !1, line: 45, type: !3)
!297 = !DILocalVariable(name: "i", arg: 3, scope: !293, file: !1, line: 45, type: !4)
!298 = !DILocation(line: 45, column: 25, scope: !293)
!299 = !DILocation(line: 45, column: 34, scope: !293)
!300 = !DILocation(line: 45, column: 42, scope: !293)
!301 = distinct !DISubprogram(name: "PALLAS_SPEC_17", scope: !1, file: !1, line: 57, type: !231, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!302 = !DILocalVariable(name: "struct_b", arg: 1, scope: !301, file: !1, line: 57, type: !192)
!303 = !DILocation(line: 0, scope: !301)
!304 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !301, file: !1, line: 57, type: !3)
!305 = !DILocalVariable(name: "i", arg: 3, scope: !301, file: !1, line: 57, type: !4)
!306 = !DILocalVariable(name: "j", arg: 4, scope: !301, file: !1, line: 57, type: !4)
!307 = !DILocation(line: 57, column: 25, scope: !301)
!308 = !DILocation(line: 57, column: 34, scope: !301)
!309 = !DILocation(line: 57, column: 42, scope: !301)
!310 = distinct !DISubprogram(name: "PALLAS_SPEC_18", scope: !1, file: !1, line: 75, type: !266, scopeLine: 75, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!311 = !DILocalVariable(name: "struct_b", arg: 1, scope: !310, file: !1, line: 75, type: !192)
!312 = !DILocation(line: 0, scope: !310)
!313 = !DILocalVariable(name: "pointer_to_integer", arg: 2, scope: !310, file: !1, line: 75, type: !3)
!314 = !DILocation(line: 75, column: 25, scope: !310)
!315 = !DILocation(line: 75, column: 34, scope: !310)
!316 = !DILocation(line: 75, column: 42, scope: !310)
!317 = !{!"pallas.old"}
!318 = !{!"pallas.perm"}
!319 = !{!"pallas.fracOf"}
