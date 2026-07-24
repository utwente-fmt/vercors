; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/structs.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.point = type { i32, i32 }
%struct.triangle = type { %struct.point, %struct.point, %struct.point }
%struct.polygon = type { ptr }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [64 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_15, ptr @PALLAS_SPEC_16, ptr @PALLAS_SPEC_17, ptr @PALLAS_SPEC_18, ptr @PALLAS_SPEC_19, ptr @PALLAS_SPEC_20, ptr @PALLAS_SPEC_21, ptr @PALLAS_SPEC_22, ptr @PALLAS_SPEC_23, ptr @PALLAS_SPEC_24, ptr @PALLAS_SPEC_25, ptr @PALLAS_SPEC_26, ptr @PALLAS_SPEC_27, ptr @PALLAS_SPEC_28, ptr @PALLAS_SPEC_29, ptr @PALLAS_SPEC_30, ptr @PALLAS_SPEC_31, ptr @PALLAS_SPEC_32, ptr @PALLAS_SPEC_33, ptr @PALLAS_SPEC_34, ptr @PALLAS_SPEC_35, ptr @PALLAS_SPEC_36, ptr @PALLAS_SPEC_37, ptr @PALLAS_SPEC_38, ptr @PALLAS_SPEC_39, ptr @PALLAS_SPEC_40, ptr @PALLAS_SPEC_41, ptr @PALLAS_SPEC_42, ptr @PALLAS_SPEC_43, ptr @PALLAS_SPEC_45, ptr @PALLAS_SPEC_44, ptr @PALLAS_SPEC_47, ptr @PALLAS_SPEC_48, ptr @PALLAS_SPEC_49, ptr @PALLAS_SPEC_50, ptr @PALLAS_SPEC_46, ptr @PALLAS_SPEC_51, ptr @PALLAS_SPEC_52, ptr @PALLAS_SPEC_53, ptr @PALLAS_SPEC_54, ptr @PALLAS_SPEC_55, ptr @PALLAS_SPEC_56, ptr @PALLAS_SPEC_57, ptr @PALLAS_SPEC_58, ptr @PALLAS_SPEC_59, ptr @PALLAS_SPEC_60, ptr @PALLAS_SPEC_61, ptr @PALLAS_SPEC_62, ptr @PALLAS_SPEC_63], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0
@.str.1 = private unnamed_addr constant [2 x i8] c"j\00", align 1, !dbg !7

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_struct(ptr noundef %0) #0 !dbg !23 !pallas.fcontract !34 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !41, metadata !DIExpression()), !dbg !95
  %3 = load ptr, ptr %2, align 8, !dbg !96
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !97
  store i32 0, ptr %4, align 4, !dbg !98
  %5 = load ptr, ptr %2, align 8, !dbg !99
  %6 = getelementptr inbounds %struct.point, ptr %5, i32 0, i32 1, !dbg !100
  store i32 0, ptr %6, align 4, !dbg !101
  ret void, !dbg !102
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_struct2(ptr noundef %0) #0 !dbg !103 !pallas.fcontract !104 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !110, metadata !DIExpression()), !dbg !155
  %3 = load ptr, ptr %2, align 8, !dbg !156
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !157
  store i32 0, ptr %4, align 4, !dbg !158
  %5 = load ptr, ptr %2, align 8, !dbg !159
  %6 = getelementptr inbounds %struct.point, ptr %5, i32 0, i32 1, !dbg !160
  store i32 0, ptr %6, align 4, !dbg !161
  ret void, !dbg !162
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_struct_1(ptr noundef %0) #0 !dbg !163 !pallas.fcontract !164 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !170, metadata !DIExpression()), !dbg !203
  %3 = load ptr, ptr %2, align 8, !dbg !204
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !205
  %5 = load i32, ptr %4, align 4, !dbg !205
  %6 = add nsw i32 %5, 1, !dbg !206
  %7 = load ptr, ptr %2, align 8, !dbg !207
  %8 = getelementptr inbounds %struct.point, ptr %7, i32 0, i32 0, !dbg !208
  store i32 %6, ptr %8, align 4, !dbg !209
  %9 = load ptr, ptr %2, align 8, !dbg !210
  %10 = getelementptr inbounds %struct.point, ptr %9, i32 0, i32 1, !dbg !211
  %11 = load i32, ptr %10, align 4, !dbg !211
  %12 = add nsw i32 %11, 1, !dbg !212
  %13 = load ptr, ptr %2, align 8, !dbg !213
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 1, !dbg !214
  store i32 %12, ptr %14, align 4, !dbg !215
  ret void, !dbg !216
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_copy_struct(i64 %0) #0 !dbg !217 !pallas.fcontract !220 {
  %2 = alloca %struct.point, align 4
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !226, metadata !DIExpression()), !dbg !255
  %3 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !256
  store i32 0, ptr %3, align 4, !dbg !257
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !258
  store i32 0, ptr %4, align 4, !dbg !259
  ret void, !dbg !260
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_copy_struct_2(i64 %0) #0 !dbg !261 !pallas.fcontract !262 {
  %2 = alloca %struct.point, align 4
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !268, metadata !DIExpression()), !dbg !277
  %3 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !278
  store i32 0, ptr %3, align 4, !dbg !279
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !280
  store i32 0, ptr %4, align 4, !dbg !281
  ret void, !dbg !282
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @avr_x(ptr noundef %0) #0 !dbg !283 !pallas.fcontract !293 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !299, metadata !DIExpression()), !dbg !329
  %3 = load ptr, ptr %2, align 8, !dbg !330
  %4 = getelementptr inbounds %struct.triangle, ptr %3, i32 0, i32 0, !dbg !331
  %5 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 0, !dbg !332
  %6 = load i32, ptr %5, align 4, !dbg !332
  %7 = load ptr, ptr %2, align 8, !dbg !333
  %8 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 1, !dbg !334
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !335
  %10 = load i32, ptr %9, align 4, !dbg !335
  %11 = add nsw i32 %6, %10, !dbg !336
  %12 = load ptr, ptr %2, align 8, !dbg !337
  %13 = getelementptr inbounds %struct.triangle, ptr %12, i32 0, i32 2, !dbg !338
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !339
  %15 = load i32, ptr %14, align 4, !dbg !339
  %16 = add nsw i32 %11, %15, !dbg !340
  %17 = sdiv i32 %16, 3, !dbg !341
  ret i32 %17, !dbg !342
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @avr_x_pol(ptr noundef %0, i32 noundef %1) #0 !dbg !343 !pallas.fcontract !351 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !357, metadata !DIExpression()), !dbg !450
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !368, metadata !DIExpression()), !dbg !451
  call void @llvm.dbg.declare(metadata ptr %5, metadata !452, metadata !DIExpression()), !dbg !453
  store i32 0, ptr %5, align 4, !dbg !453
  call void @llvm.dbg.declare(metadata ptr %6, metadata !454, metadata !DIExpression()), !dbg !456
  store i32 0, ptr %6, align 4, !dbg !456
  br label %7, !dbg !457

7:                                                ; preds = %22, %2
  %8 = load i32, ptr %6, align 4, !dbg !458
  %9 = load i32, ptr %4, align 4, !dbg !460
  %10 = icmp slt i32 %8, %9, !dbg !461
  br i1 %10, label %11, label %25, !dbg !462

11:                                               ; preds = %7
  %12 = load ptr, ptr %3, align 8, !dbg !463
  %13 = getelementptr inbounds %struct.polygon, ptr %12, i32 0, i32 0, !dbg !465
  %14 = load ptr, ptr %13, align 8, !dbg !465
  %15 = load i32, ptr %6, align 4, !dbg !466
  %16 = sext i32 %15 to i64, !dbg !463
  %17 = getelementptr inbounds %struct.point, ptr %14, i64 %16, !dbg !463
  %18 = getelementptr inbounds %struct.point, ptr %17, i32 0, i32 0, !dbg !467
  %19 = load i32, ptr %18, align 4, !dbg !467
  %20 = load i32, ptr %5, align 4, !dbg !468
  %21 = add nsw i32 %20, %19, !dbg !468
  store i32 %21, ptr %5, align 4, !dbg !468
  br label %22, !dbg !469

22:                                               ; preds = %11
  %23 = load i32, ptr %6, align 4, !dbg !470
  %24 = add nsw i32 %23, 1, !dbg !470
  store i32 %24, ptr %6, align 4, !dbg !470
  br label %7, !dbg !471, !llvm.loop !472

25:                                               ; preds = %7
  %26 = load i32, ptr %5, align 4, !dbg !599
  %27 = load i32, ptr %4, align 4, !dbg !600
  %28 = sdiv i32 %26, %27, !dbg !601
  ret i32 %28, !dbg !602
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @main() #0 !dbg !603 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.point, align 4
  %3 = alloca ptr, align 8
  %4 = alloca %struct.point, align 4
  %5 = alloca %struct.point, align 4
  %6 = alloca %struct.point, align 4
  %7 = alloca %struct.triangle, align 4
  %8 = alloca ptr, align 8
  %9 = alloca [3 x %struct.point], align 16
  %10 = alloca %struct.polygon, align 8
  %11 = alloca ptr, align 8
  %12 = alloca i32, align 4
  store i32 0, ptr %1, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !606, metadata !DIExpression()), !dbg !607
  call void @llvm.dbg.declare(metadata ptr %3, metadata !608, metadata !DIExpression()), !dbg !609
  store ptr %2, ptr %3, align 8, !dbg !610
  %13 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !611, !pallas.stmntBlock !612
  store i32 1, ptr %13, align 4, !dbg !624
  %14 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !625
  store i32 2, ptr %14, align 4, !dbg !626
  %15 = load i64, ptr %2, align 4, !dbg !627, !pallas.stmntBlock !628
  call void @alter_copy_struct(i64 %15), !dbg !627
  %16 = load ptr, ptr %3, align 8, !dbg !646, !pallas.stmntBlock !647
  call void @alter_struct(ptr noundef %16), !dbg !665
  %17 = load ptr, ptr %3, align 8, !dbg !666, !pallas.stmntBlock !667
  call void @alter_struct_1(ptr noundef %17), !dbg !685
  call void @llvm.dbg.declare(metadata ptr %4, metadata !686, metadata !DIExpression()), !dbg !687
  call void @llvm.dbg.declare(metadata ptr %5, metadata !688, metadata !DIExpression()), !dbg !689
  call void @llvm.dbg.declare(metadata ptr %6, metadata !690, metadata !DIExpression()), !dbg !691
  %18 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 0, !dbg !692, !pallas.stmntBlock !693
  store i32 1, ptr %18, align 4, !dbg !711
  %19 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !712
  store i32 1, ptr %19, align 4, !dbg !713
  %20 = getelementptr inbounds %struct.point, ptr %5, i32 0, i32 0, !dbg !714
  store i32 2, ptr %20, align 4, !dbg !715
  %21 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !716
  store i32 2, ptr %21, align 4, !dbg !717
  %22 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !718
  store i32 3, ptr %22, align 4, !dbg !719
  %23 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !720
  store i32 3, ptr %23, align 4, !dbg !721
  call void @llvm.dbg.declare(metadata ptr %7, metadata !722, metadata !DIExpression()), !dbg !723
  call void @llvm.dbg.declare(metadata ptr %8, metadata !724, metadata !DIExpression()), !dbg !725
  store ptr %7, ptr %8, align 8, !dbg !726
  %24 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 0, !dbg !727
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %24, ptr align 4 %4, i64 8, i1 false), !dbg !728
  %25 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 1, !dbg !729
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %25, ptr align 4 %5, i64 8, i1 false), !dbg !730
  %26 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 2, !dbg !731
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %26, ptr align 4 %6, i64 8, i1 false), !dbg !732
  call void @llvm.dbg.declare(metadata ptr %9, metadata !733, metadata !DIExpression()), !dbg !737
  %27 = getelementptr inbounds [3 x %struct.point], ptr %9, i64 0, i64 0, !dbg !738, !pallas.stmntBlock !739
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 4 %4, i64 8, i1 false), !dbg !763
  %28 = getelementptr inbounds %struct.point, ptr %27, i64 1, !dbg !738
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %28, ptr align 4 %5, i64 8, i1 false), !dbg !764
  %29 = getelementptr inbounds %struct.point, ptr %28, i64 1, !dbg !738
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %29, ptr align 4 %6, i64 8, i1 false), !dbg !765
  call void @llvm.dbg.declare(metadata ptr %10, metadata !766, metadata !DIExpression()), !dbg !767
  call void @llvm.dbg.declare(metadata ptr %11, metadata !768, metadata !DIExpression()), !dbg !769
  store ptr %10, ptr %11, align 8, !dbg !770
  %30 = getelementptr inbounds [3 x %struct.point], ptr %9, i64 0, i64 0, !dbg !771
  %31 = getelementptr inbounds %struct.polygon, ptr %10, i32 0, i32 0, !dbg !772
  store ptr %30, ptr %31, align 8, !dbg !773
  call void @llvm.dbg.declare(metadata ptr %12, metadata !774, metadata !DIExpression()), !dbg !775
  %32 = load ptr, ptr %11, align 8, !dbg !776
  %33 = call i32 @avr_x_pol(ptr noundef %32, i32 noundef 3), !dbg !777
  store i32 %33, ptr %12, align 4, !dbg !775
  ret i32 0, !dbg !778, !pallas.stmntBlock !779
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #2

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !43 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !42, metadata !DIExpression()), !dbg !810
  %2 = icmp ne ptr %0, null, !dbg !811
  ret i1 %2, !dbg !810
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !58 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !57, metadata !DIExpression()), !dbg !812
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !813
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !814
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !815
  ret i1 %4, !dbg !812
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !64 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !63, metadata !DIExpression()), !dbg !816
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !817
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !818
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !819
  ret i1 %4, !dbg !816
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !70 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !69, metadata !DIExpression()), !dbg !820
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !821
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !822
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !823
  ret i1 %4, !dbg !820
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0) #0 !dbg !76 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !75, metadata !DIExpression()), !dbg !824
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !825
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !826
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !827
  ret i1 %4, !dbg !824
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0) #0 !dbg !82 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !81, metadata !DIExpression()), !dbg !828
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !829
  %3 = load i32, ptr %2, align 4, !dbg !829
  %4 = icmp eq i32 %3, 0, !dbg !830
  ret i1 %4, !dbg !828
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0) #0 !dbg !88 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !87, metadata !DIExpression()), !dbg !831
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !832
  %3 = load i32, ptr %2, align 4, !dbg !832
  %4 = icmp eq i32 %3, 0, !dbg !833
  ret i1 %4, !dbg !831
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0) #0 !dbg !94 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !93, metadata !DIExpression()), !dbg !834
  %2 = call ptr @"pallas.old ptr_noundef ptr"(ptr noundef %0), !dbg !835
  %3 = icmp eq ptr %2, %0, !dbg !836
  ret i1 %3, !dbg !834
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0) #0 !dbg !112 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !111, metadata !DIExpression()), !dbg !837
  %2 = icmp ne ptr %0, null, !dbg !838
  ret i1 %2, !dbg !837
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0) #0 !dbg !118 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !117, metadata !DIExpression()), !dbg !839
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !840
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !841
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !842
  ret i1 %4, !dbg !839
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0) #0 !dbg !124 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !123, metadata !DIExpression()), !dbg !843
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !844
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !845
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !846
  ret i1 %4, !dbg !843
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0) #0 !dbg !130 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !129, metadata !DIExpression()), !dbg !847
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !848
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !849
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !850
  ret i1 %4, !dbg !847
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0) #0 !dbg !136 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !135, metadata !DIExpression()), !dbg !851
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !852
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !853
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !854
  ret i1 %4, !dbg !851
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0) #0 !dbg !142 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !141, metadata !DIExpression()), !dbg !855
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !856
  %3 = load i32, ptr %2, align 4, !dbg !856
  %4 = icmp eq i32 %3, 0, !dbg !857
  ret i1 %4, !dbg !855
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0) #0 !dbg !148 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !147, metadata !DIExpression()), !dbg !858
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !859
  %3 = load i32, ptr %2, align 4, !dbg !859
  %4 = icmp eq i32 %3, 0, !dbg !860
  ret i1 %4, !dbg !858
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0) #0 !dbg !154 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !153, metadata !DIExpression()), !dbg !861
  %2 = call ptr @"pallas.old ptr_noundef ptr"(ptr noundef %0), !dbg !862
  %3 = icmp eq ptr %2, %0, !dbg !863
  ret i1 %3, !dbg !861
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(ptr noundef %0) #0 !dbg !172 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !171, metadata !DIExpression()), !dbg !864
  %2 = icmp ne ptr %0, null, !dbg !865
  ret i1 %2, !dbg !864
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(ptr noundef %0) #0 !dbg !178 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !177, metadata !DIExpression()), !dbg !866
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !867
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !868
  ret i1 %3, !dbg !866
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(ptr noundef %0) #0 !dbg !184 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !183, metadata !DIExpression()), !dbg !869
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !870
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !871
  ret i1 %3, !dbg !869
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_19(ptr noundef %0) #0 !dbg !190 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !189, metadata !DIExpression()), !dbg !872
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !873
  %3 = load i32, ptr %2, align 4, !dbg !873
  %4 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !874
  %5 = load i32, ptr %4, align 4, !dbg !874
  %6 = add nsw i32 %5, 1, !dbg !875
  %7 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %6), !dbg !876
  %8 = icmp eq i32 %3, %7, !dbg !877
  ret i1 %8, !dbg !872
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_20(ptr noundef %0) #0 !dbg !196 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !195, metadata !DIExpression()), !dbg !878
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !879
  %3 = load i32, ptr %2, align 4, !dbg !879
  %4 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !880
  %5 = load i32, ptr %4, align 4, !dbg !880
  %6 = add nsw i32 %5, 1, !dbg !881
  %7 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %6), !dbg !882
  %8 = icmp eq i32 %3, %7, !dbg !883
  ret i1 %8, !dbg !878
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_21(ptr noundef %0) #0 !dbg !202 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !201, metadata !DIExpression()), !dbg !884
  %2 = call ptr @"pallas.old ptr_noundef ptr"(ptr noundef %0), !dbg !885
  %3 = icmp eq ptr %2, %0, !dbg !886
  ret i1 %3, !dbg !884
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_22(i64 %0) #0 !dbg !228 !pallas.exprWrapper !809 {
  %2 = alloca %struct.point, align 4
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !227, metadata !DIExpression()), !dbg !887
  %3 = icmp ne ptr %2, null, !dbg !888
  ret i1 %3, !dbg !887
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_23(i64 %0) #0 !dbg !236 !pallas.exprWrapper !809 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !235, metadata !DIExpression()), !dbg !889
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !890
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !891
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !892
  ret i1 %5, !dbg !889
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_24(i64 %0) #0 !dbg !242 !pallas.exprWrapper !809 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !241, metadata !DIExpression()), !dbg !893
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !894
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !895
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !896
  ret i1 %5, !dbg !893
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_25(i64 %0) #0 !dbg !248 !pallas.exprWrapper !809 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !247, metadata !DIExpression()), !dbg !897
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !898
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !899
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !900
  ret i1 %5, !dbg !897
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_26(i64 %0) #0 !dbg !254 !pallas.exprWrapper !809 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !253, metadata !DIExpression()), !dbg !901
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !902
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !903
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !904
  ret i1 %5, !dbg !901
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_27(i64 %0) #0 !dbg !270 !pallas.exprWrapper !809 {
  %2 = alloca %struct.point, align 4
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !269, metadata !DIExpression()), !dbg !905
  %3 = icmp ne ptr %2, null, !dbg !906
  ret i1 %3, !dbg !905
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_28(i64 %0) #0 !dbg !276 !pallas.exprWrapper !809 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !275, metadata !DIExpression()), !dbg !907
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !908
  %4 = call i1 @pallas.perm(ptr noundef %2, ptr noundef byval(%pallas.fracT) %3), !dbg !909
  ret i1 %4, !dbg !907
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_29(ptr noundef %0) #0 !dbg !301 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !300, metadata !DIExpression()), !dbg !910
  %2 = icmp ne ptr %0, null, !dbg !911
  ret i1 %2, !dbg !910
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_30(ptr noundef %0) #0 !dbg !316 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !315, metadata !DIExpression()), !dbg !912
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !913
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !914
  ret i1 %3, !dbg !912
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_31(ptr noundef %0) #0 !dbg !322 !pallas.exprWrapper !809 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !321, metadata !DIExpression()), !dbg !915
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !916
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !917
  ret i1 %3, !dbg !915
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_32(ptr noundef %0) #0 !dbg !328 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !327, metadata !DIExpression()), !dbg !918
  %2 = call i32 @"pallas.result i32"(), !dbg !919
  %3 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 0, !dbg !920
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !921
  %5 = load i32, ptr %4, align 4, !dbg !921
  %6 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 1, !dbg !922
  %7 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !923
  %8 = load i32, ptr %7, align 4, !dbg !923
  %9 = add nsw i32 %5, %8, !dbg !924
  %10 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 2, !dbg !925
  %11 = getelementptr inbounds %struct.point, ptr %10, i32 0, i32 0, !dbg !926
  %12 = load i32, ptr %11, align 4, !dbg !926
  %13 = add nsw i32 %9, %12, !dbg !927
  %14 = sdiv i32 %13, 3, !dbg !928
  %15 = icmp eq i32 %2, %14, !dbg !929
  ret i1 %15, !dbg !918
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_33(ptr noundef %0, i32 noundef %1) #0 !dbg !359 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !358, metadata !DIExpression()), !dbg !930
  call void @llvm.dbg.value(metadata i32 %1, metadata !369, metadata !DIExpression()), !dbg !930
  %3 = icmp sgt i32 %1, 0, !dbg !931
  ret i1 %3, !dbg !930
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_34(ptr noundef %0, i32 noundef %1) #0 !dbg !375 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !374, metadata !DIExpression()), !dbg !932
  call void @llvm.dbg.value(metadata i32 %1, metadata !377, metadata !DIExpression()), !dbg !932
  %3 = icmp ne ptr %0, null, !dbg !933
  ret i1 %3, !dbg !932
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_35(ptr noundef %0, i32 noundef %1) #0 !dbg !383 !pallas.exprWrapper !809 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !382, metadata !DIExpression()), !dbg !934
  call void @llvm.dbg.value(metadata i32 %1, metadata !385, metadata !DIExpression()), !dbg !934
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !935
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !936
  ret i1 %4, !dbg !934
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_36(ptr noundef %0, i32 noundef %1) #0 !dbg !391 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !390, metadata !DIExpression()), !dbg !937
  call void @llvm.dbg.value(metadata i32 %1, metadata !393, metadata !DIExpression()), !dbg !937
  %3 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !938
  %4 = load ptr, ptr %3, align 8, !dbg !938
  %5 = icmp ne ptr %4, null, !dbg !939
  br i1 %5, label %6, label %12, !dbg !940

6:                                                ; preds = %2
  %7 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !941
  %8 = load ptr, ptr %7, align 8, !dbg !941
  %9 = call i64 @pallas.ptrLength(ptr noundef %8), !dbg !942
  %10 = sext i32 %1 to i64, !dbg !943
  %11 = icmp sge i64 %9, %10, !dbg !944
  br label %12

12:                                               ; preds = %6, %2
  %13 = phi i1 [ false, %2 ], [ %11, %6 ], !dbg !937
  ret i1 %13, !dbg !937
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_37(ptr noundef %0, i32 noundef %1) #0 !dbg !399 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !398, metadata !DIExpression()), !dbg !945
  call void @llvm.dbg.value(metadata i32 %1, metadata !401, metadata !DIExpression()), !dbg !945
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !946
  %4 = icmp sle i32 0, %3, !dbg !947
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !948
  %6 = icmp slt i32 %5, %1, !dbg !949
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !950
  %8 = icmp sle i32 0, %7, !dbg !951
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !952
  %10 = icmp slt i32 %9, %1, !dbg !953
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !954
  %12 = call i1 @pallas.scAnd(i1 %6, i1 %11), !dbg !955
  %13 = call i1 @pallas.scAnd(i1 %4, i1 %12), !dbg !956
  %14 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !957
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !958
  %16 = icmp ne i32 %14, %15, !dbg !959
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !960
  %18 = load ptr, ptr %17, align 8, !dbg !960
  %19 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !961
  %20 = sext i32 %19 to i64, !dbg !962
  %21 = getelementptr inbounds %struct.point, ptr %18, i64 %20, !dbg !962
  %22 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !963
  %23 = load ptr, ptr %22, align 8, !dbg !963
  %24 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !964
  %25 = sext i32 %24 to i64, !dbg !965
  %26 = getelementptr inbounds %struct.point, ptr %23, i64 %25, !dbg !965
  %27 = icmp ne ptr %21, %26, !dbg !966
  %28 = call i1 @pallas.imply(i1 %16, i1 %27), !dbg !967
  %29 = call i1 @pallas.forall(i1 %13, i1 %28), !dbg !968
  ret i1 %29, !dbg !945
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_38(ptr noundef %0, i32 noundef %1) #0 !dbg !407 !pallas.exprWrapper !809 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !406, metadata !DIExpression()), !dbg !969
  call void @llvm.dbg.value(metadata i32 %1, metadata !409, metadata !DIExpression()), !dbg !969
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !970
  %5 = icmp sle i32 0, %4, !dbg !971
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !972
  %7 = icmp slt i32 %6, %1, !dbg !973
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !974
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !975
  %10 = load ptr, ptr %9, align 8, !dbg !975
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !976
  %12 = sext i32 %11 to i64, !dbg !977
  %13 = getelementptr inbounds %struct.point, ptr %10, i64 %12, !dbg !977
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !978
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %3), !dbg !979
  %15 = call i1 @pallas.forallSep(i1 %8, i1 %14), !dbg !980
  ret i1 %15, !dbg !969
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_39(ptr noundef %0, i32 noundef %1) #0 !dbg !415 !pallas.exprWrapper !809 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !414, metadata !DIExpression()), !dbg !981
  call void @llvm.dbg.value(metadata i32 %1, metadata !417, metadata !DIExpression()), !dbg !981
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !982
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !983
  ret i1 %4, !dbg !981
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_40(ptr noundef %0, i32 noundef %1) #0 !dbg !423 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !422, metadata !DIExpression()), !dbg !984
  call void @llvm.dbg.value(metadata i32 %1, metadata !425, metadata !DIExpression()), !dbg !984
  %3 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !985
  %4 = load ptr, ptr %3, align 8, !dbg !985
  %5 = icmp ne ptr %4, null, !dbg !986
  br i1 %5, label %6, label %12, !dbg !987

6:                                                ; preds = %2
  %7 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !988
  %8 = load ptr, ptr %7, align 8, !dbg !988
  %9 = call i64 @pallas.ptrLength(ptr noundef %8), !dbg !989
  %10 = sext i32 %1 to i64, !dbg !990
  %11 = icmp sge i64 %9, %10, !dbg !991
  br label %12

12:                                               ; preds = %6, %2
  %13 = phi i1 [ false, %2 ], [ %11, %6 ], !dbg !984
  ret i1 %13, !dbg !984
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_41(ptr noundef %0, i32 noundef %1) #0 !dbg !431 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !430, metadata !DIExpression()), !dbg !992
  call void @llvm.dbg.value(metadata i32 %1, metadata !433, metadata !DIExpression()), !dbg !992
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !993
  %4 = icmp sle i32 0, %3, !dbg !994
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !995
  %6 = icmp slt i32 %5, %1, !dbg !996
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !997
  %8 = icmp sle i32 0, %7, !dbg !998
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !999
  %10 = icmp slt i32 %9, %1, !dbg !1000
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !1001
  %12 = call i1 @pallas.scAnd(i1 %6, i1 %11), !dbg !1002
  %13 = call i1 @pallas.scAnd(i1 %4, i1 %12), !dbg !1003
  %14 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1004
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1005
  %16 = icmp ne i32 %14, %15, !dbg !1006
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1007
  %18 = load ptr, ptr %17, align 8, !dbg !1007
  %19 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1008
  %20 = sext i32 %19 to i64, !dbg !1009
  %21 = getelementptr inbounds %struct.point, ptr %18, i64 %20, !dbg !1009
  %22 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1010
  %23 = load ptr, ptr %22, align 8, !dbg !1010
  %24 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1011
  %25 = sext i32 %24 to i64, !dbg !1012
  %26 = getelementptr inbounds %struct.point, ptr %23, i64 %25, !dbg !1012
  %27 = icmp ne ptr %21, %26, !dbg !1013
  %28 = call i1 @pallas.imply(i1 %16, i1 %27), !dbg !1014
  %29 = call i1 @pallas.forall(i1 %13, i1 %28), !dbg !1015
  ret i1 %29, !dbg !992
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_42(ptr noundef %0, i32 noundef %1) #0 !dbg !439 !pallas.exprWrapper !809 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !438, metadata !DIExpression()), !dbg !1016
  call void @llvm.dbg.value(metadata i32 %1, metadata !441, metadata !DIExpression()), !dbg !1016
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1017
  %5 = icmp sle i32 0, %4, !dbg !1018
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1019
  %7 = icmp slt i32 %6, %1, !dbg !1020
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !1021
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1022
  %10 = load ptr, ptr %9, align 8, !dbg !1022
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1023
  %12 = sext i32 %11 to i64, !dbg !1024
  %13 = getelementptr inbounds %struct.point, ptr %10, i64 %12, !dbg !1024
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !1025
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %3), !dbg !1026
  %15 = call i1 @pallas.forallSep(i1 %8, i1 %14), !dbg !1027
  ret i1 %15, !dbg !1016
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_43(ptr noundef %0, i32 noundef %1) #0 !dbg !447 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !446, metadata !DIExpression()), !dbg !1028
  call void @llvm.dbg.value(metadata i32 %1, metadata !449, metadata !DIExpression()), !dbg !1028
  %3 = icmp eq i32 %1, 3, !dbg !1029
  %4 = call i32 @"pallas.result i32"(), !dbg !1030
  %5 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1031
  %6 = load ptr, ptr %5, align 8, !dbg !1031
  %7 = getelementptr inbounds %struct.point, ptr %6, i64 0, !dbg !1032
  %8 = getelementptr inbounds %struct.point, ptr %7, i32 0, i32 0, !dbg !1033
  %9 = load i32, ptr %8, align 4, !dbg !1033
  %10 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1034
  %11 = load ptr, ptr %10, align 8, !dbg !1034
  %12 = getelementptr inbounds %struct.point, ptr %11, i64 1, !dbg !1035
  %13 = getelementptr inbounds %struct.point, ptr %12, i32 0, i32 0, !dbg !1036
  %14 = load i32, ptr %13, align 4, !dbg !1036
  %15 = add nsw i32 %9, %14, !dbg !1037
  %16 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1038
  %17 = load ptr, ptr %16, align 8, !dbg !1038
  %18 = getelementptr inbounds %struct.point, ptr %17, i64 2, !dbg !1039
  %19 = getelementptr inbounds %struct.point, ptr %18, i32 0, i32 0, !dbg !1040
  %20 = load i32, ptr %19, align 4, !dbg !1040
  %21 = add nsw i32 %15, %20, !dbg !1041
  %22 = sdiv i32 %21, %1, !dbg !1042
  %23 = icmp eq i32 %4, %22, !dbg !1043
  %24 = call i1 @pallas.imply(i1 %3, i1 %23), !dbg !1044
  ret i1 %24, !dbg !1028
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_45(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !496 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !495, metadata !DIExpression()), !dbg !1045
  call void @llvm.dbg.value(metadata i32 %1, metadata !498, metadata !DIExpression()), !dbg !1045
  call void @llvm.dbg.value(metadata i32 %2, metadata !500, metadata !DIExpression()), !dbg !1045
  call void @llvm.dbg.value(metadata i32 %3, metadata !502, metadata !DIExpression()), !dbg !1045
  %5 = icmp ne ptr %0, null, !dbg !1046
  ret i1 %5, !dbg !1045
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_44(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !482 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !481, metadata !DIExpression()), !dbg !1047
  call void @llvm.dbg.value(metadata i32 %1, metadata !486, metadata !DIExpression()), !dbg !1047
  call void @llvm.dbg.value(metadata i32 %2, metadata !488, metadata !DIExpression()), !dbg !1047
  call void @llvm.dbg.value(metadata i32 %3, metadata !490, metadata !DIExpression()), !dbg !1047
  %5 = icmp sle i32 0, %3, !dbg !1048
  br i1 %5, label %6, label %8, !dbg !1049

6:                                                ; preds = %4
  %7 = icmp sle i32 %3, %1, !dbg !1050
  br label %8

8:                                                ; preds = %6, %4
  %9 = phi i1 [ false, %4 ], [ %7, %6 ], !dbg !1047
  ret i1 %9, !dbg !1047
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_47(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !520 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !519, metadata !DIExpression()), !dbg !1051
  call void @llvm.dbg.value(metadata i32 %1, metadata !522, metadata !DIExpression()), !dbg !1051
  call void @llvm.dbg.value(metadata i32 %2, metadata !524, metadata !DIExpression()), !dbg !1051
  call void @llvm.dbg.value(metadata i32 %3, metadata !526, metadata !DIExpression()), !dbg !1051
  %5 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1052
  %6 = load ptr, ptr %5, align 8, !dbg !1052
  %7 = icmp ne ptr %6, null, !dbg !1053
  br i1 %7, label %8, label %14, !dbg !1054

8:                                                ; preds = %4
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1055
  %10 = load ptr, ptr %9, align 8, !dbg !1055
  %11 = call i64 @pallas.ptrLength(ptr noundef %10), !dbg !1056
  %12 = sext i32 %1 to i64, !dbg !1057
  %13 = icmp sge i64 %11, %12, !dbg !1058
  br label %14

14:                                               ; preds = %8, %4
  %15 = phi i1 [ false, %4 ], [ %13, %8 ], !dbg !1051
  ret i1 %15, !dbg !1051
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_48(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !532 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !531, metadata !DIExpression()), !dbg !1059
  call void @llvm.dbg.value(metadata i32 %1, metadata !534, metadata !DIExpression()), !dbg !1059
  call void @llvm.dbg.value(metadata i32 %2, metadata !536, metadata !DIExpression()), !dbg !1059
  call void @llvm.dbg.value(metadata i32 %3, metadata !538, metadata !DIExpression()), !dbg !1059
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1060
  %6 = icmp sle i32 0, %5, !dbg !1061
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1062
  %8 = icmp slt i32 %7, %1, !dbg !1063
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1064
  %10 = icmp sle i32 0, %9, !dbg !1065
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1066
  %12 = icmp slt i32 %11, %1, !dbg !1067
  %13 = call i1 @pallas.scAnd(i1 %10, i1 %12), !dbg !1068
  %14 = call i1 @pallas.scAnd(i1 %8, i1 %13), !dbg !1069
  %15 = call i1 @pallas.scAnd(i1 %6, i1 %14), !dbg !1070
  %16 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1071
  %17 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1072
  %18 = icmp ne i32 %16, %17, !dbg !1073
  %19 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1074
  %20 = load ptr, ptr %19, align 8, !dbg !1074
  %21 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1075
  %22 = sext i32 %21 to i64, !dbg !1076
  %23 = getelementptr inbounds %struct.point, ptr %20, i64 %22, !dbg !1076
  %24 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1077
  %25 = load ptr, ptr %24, align 8, !dbg !1077
  %26 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1078
  %27 = sext i32 %26 to i64, !dbg !1079
  %28 = getelementptr inbounds %struct.point, ptr %25, i64 %27, !dbg !1079
  %29 = icmp ne ptr %23, %28, !dbg !1080
  %30 = call i1 @pallas.imply(i1 %18, i1 %29), !dbg !1081
  %31 = call i1 @pallas.forall(i1 %15, i1 %30), !dbg !1082
  ret i1 %31, !dbg !1059
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_49(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !544 !pallas.exprWrapper !809 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !543, metadata !DIExpression()), !dbg !1083
  call void @llvm.dbg.value(metadata i32 %1, metadata !546, metadata !DIExpression()), !dbg !1083
  call void @llvm.dbg.value(metadata i32 %2, metadata !548, metadata !DIExpression()), !dbg !1083
  call void @llvm.dbg.value(metadata i32 %3, metadata !550, metadata !DIExpression()), !dbg !1083
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1084
  %7 = icmp sle i32 0, %6, !dbg !1085
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1086
  %9 = icmp slt i32 %8, %1, !dbg !1087
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !1088
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1089
  %12 = load ptr, ptr %11, align 8, !dbg !1089
  %13 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1090
  %14 = sext i32 %13 to i64, !dbg !1091
  %15 = getelementptr inbounds %struct.point, ptr %12, i64 %14, !dbg !1091
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !1092
  %16 = call i1 @pallas.perm(ptr noundef %15, ptr noundef byval(%pallas.fracT) %5), !dbg !1093
  %17 = call i1 @pallas.forallSep(i1 %10, i1 %16), !dbg !1094
  ret i1 %17, !dbg !1083
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_50(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !556 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !555, metadata !DIExpression()), !dbg !1095
  call void @llvm.dbg.value(metadata i32 %1, metadata !558, metadata !DIExpression()), !dbg !1095
  call void @llvm.dbg.value(metadata i32 %2, metadata !560, metadata !DIExpression()), !dbg !1095
  call void @llvm.dbg.value(metadata i32 %3, metadata !562, metadata !DIExpression()), !dbg !1095
  %5 = icmp eq i32 %3, 0, !dbg !1096
  %6 = icmp eq i32 %2, 0, !dbg !1097
  %7 = call i1 @pallas.imply(i1 %5, i1 %6), !dbg !1098
  ret i1 %7, !dbg !1095
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_46(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !508 !pallas.exprWrapper !809 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !507, metadata !DIExpression()), !dbg !1099
  call void @llvm.dbg.value(metadata i32 %1, metadata !510, metadata !DIExpression()), !dbg !1099
  call void @llvm.dbg.value(metadata i32 %2, metadata !512, metadata !DIExpression()), !dbg !1099
  call void @llvm.dbg.value(metadata i32 %3, metadata !514, metadata !DIExpression()), !dbg !1099
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !1100
  %6 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %5), !dbg !1101
  ret i1 %6, !dbg !1099
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_51(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !568 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !567, metadata !DIExpression()), !dbg !1102
  call void @llvm.dbg.value(metadata i32 %1, metadata !570, metadata !DIExpression()), !dbg !1102
  call void @llvm.dbg.value(metadata i32 %2, metadata !572, metadata !DIExpression()), !dbg !1102
  call void @llvm.dbg.value(metadata i32 %3, metadata !574, metadata !DIExpression()), !dbg !1102
  %5 = icmp eq i32 %3, 1, !dbg !1103
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1104
  %7 = load ptr, ptr %6, align 8, !dbg !1104
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !1105
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !1106
  %10 = load i32, ptr %9, align 4, !dbg !1106
  %11 = icmp eq i32 %2, %10, !dbg !1107
  %12 = call i1 @pallas.imply(i1 %5, i1 %11), !dbg !1108
  ret i1 %12, !dbg !1102
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_52(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !580 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !579, metadata !DIExpression()), !dbg !1109
  call void @llvm.dbg.value(metadata i32 %1, metadata !582, metadata !DIExpression()), !dbg !1109
  call void @llvm.dbg.value(metadata i32 %2, metadata !584, metadata !DIExpression()), !dbg !1109
  call void @llvm.dbg.value(metadata i32 %3, metadata !586, metadata !DIExpression()), !dbg !1109
  %5 = icmp eq i32 %3, 2, !dbg !1110
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1111
  %7 = load ptr, ptr %6, align 8, !dbg !1111
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !1112
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !1113
  %10 = load i32, ptr %9, align 4, !dbg !1113
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1114
  %12 = load ptr, ptr %11, align 8, !dbg !1114
  %13 = getelementptr inbounds %struct.point, ptr %12, i64 1, !dbg !1115
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !1116
  %15 = load i32, ptr %14, align 4, !dbg !1116
  %16 = add nsw i32 %10, %15, !dbg !1117
  %17 = icmp eq i32 %2, %16, !dbg !1118
  %18 = call i1 @pallas.imply(i1 %5, i1 %17), !dbg !1119
  ret i1 %18, !dbg !1109
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_53(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !592 !pallas.exprWrapper !809 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !591, metadata !DIExpression()), !dbg !1120
  call void @llvm.dbg.value(metadata i32 %1, metadata !594, metadata !DIExpression()), !dbg !1120
  call void @llvm.dbg.value(metadata i32 %2, metadata !596, metadata !DIExpression()), !dbg !1120
  call void @llvm.dbg.value(metadata i32 %3, metadata !598, metadata !DIExpression()), !dbg !1120
  %5 = icmp eq i32 %3, 3, !dbg !1121
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1122
  %7 = load ptr, ptr %6, align 8, !dbg !1122
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !1123
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !1124
  %10 = load i32, ptr %9, align 4, !dbg !1124
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1125
  %12 = load ptr, ptr %11, align 8, !dbg !1125
  %13 = getelementptr inbounds %struct.point, ptr %12, i64 1, !dbg !1126
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !1127
  %15 = load i32, ptr %14, align 4, !dbg !1127
  %16 = add nsw i32 %10, %15, !dbg !1128
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1129
  %18 = load ptr, ptr %17, align 8, !dbg !1129
  %19 = getelementptr inbounds %struct.point, ptr %18, i64 2, !dbg !1130
  %20 = getelementptr inbounds %struct.point, ptr %19, i32 0, i32 0, !dbg !1131
  %21 = load i32, ptr %20, align 4, !dbg !1131
  %22 = add nsw i32 %16, %21, !dbg !1132
  %23 = icmp eq i32 %2, %22, !dbg !1133
  %24 = call i1 @pallas.imply(i1 %5, i1 %23), !dbg !1134
  ret i1 %24, !dbg !1120
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_54(i64 %0, ptr noundef %1) #0 !dbg !619 !pallas.exprWrapper !809 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !618, metadata !DIExpression()), !dbg !1135
  call void @llvm.dbg.value(metadata ptr %1, metadata !623, metadata !DIExpression()), !dbg !1135
  %4 = icmp ne ptr %1, null, !dbg !1136
  ret i1 %4, !dbg !1135
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_55(i64 %0, ptr noundef %1) #0 !dbg !635 !pallas.exprWrapper !809 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !634, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.value(metadata ptr %1, metadata !637, metadata !DIExpression()), !dbg !1137
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 0, !dbg !1138
  %5 = load i32, ptr %4, align 4, !dbg !1138
  %6 = icmp eq i32 %5, 1, !dbg !1139
  ret i1 %6, !dbg !1137
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_56(i64 %0, ptr noundef %1) #0 !dbg !643 !pallas.exprWrapper !809 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !642, metadata !DIExpression()), !dbg !1140
  call void @llvm.dbg.value(metadata ptr %1, metadata !645, metadata !DIExpression()), !dbg !1140
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 1, !dbg !1141
  %5 = load i32, ptr %4, align 4, !dbg !1141
  %6 = icmp eq i32 %5, 2, !dbg !1142
  ret i1 %6, !dbg !1140
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_57(i64 %0, ptr noundef %1) #0 !dbg !654 !pallas.exprWrapper !809 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !653, metadata !DIExpression()), !dbg !1143
  call void @llvm.dbg.value(metadata ptr %1, metadata !656, metadata !DIExpression()), !dbg !1143
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !1144
  %5 = load i32, ptr %4, align 4, !dbg !1144
  %6 = icmp eq i32 %5, 1, !dbg !1145
  ret i1 %6, !dbg !1143
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_58(i64 %0, ptr noundef %1) #0 !dbg !662 !pallas.exprWrapper !809 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !661, metadata !DIExpression()), !dbg !1146
  call void @llvm.dbg.value(metadata ptr %1, metadata !664, metadata !DIExpression()), !dbg !1146
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 1, !dbg !1147
  %5 = load i32, ptr %4, align 4, !dbg !1147
  %6 = icmp eq i32 %5, 2, !dbg !1148
  ret i1 %6, !dbg !1146
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_59(i64 %0, ptr noundef %1) #0 !dbg !674 !pallas.exprWrapper !809 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !673, metadata !DIExpression()), !dbg !1149
  call void @llvm.dbg.value(metadata ptr %1, metadata !676, metadata !DIExpression()), !dbg !1149
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 0, !dbg !1150
  %5 = load i32, ptr %4, align 4, !dbg !1150
  %6 = icmp eq i32 %5, 0, !dbg !1151
  ret i1 %6, !dbg !1149
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_60(i64 %0, ptr noundef %1) #0 !dbg !682 !pallas.exprWrapper !809 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !681, metadata !DIExpression()), !dbg !1152
  call void @llvm.dbg.value(metadata ptr %1, metadata !684, metadata !DIExpression()), !dbg !1152
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !1153
  %5 = load i32, ptr %4, align 4, !dbg !1153
  %6 = icmp eq i32 %5, 0, !dbg !1154
  ret i1 %6, !dbg !1152
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_61(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4) #0 !dbg !700 !pallas.exprWrapper !809 {
  %6 = alloca %struct.point, align 4
  %7 = alloca %struct.point, align 4
  %8 = alloca %struct.point, align 4
  %9 = alloca %struct.point, align 4
  store i64 %0, ptr %6, align 4
  store i64 %2, ptr %7, align 4
  store i64 %3, ptr %8, align 4
  store i64 %4, ptr %9, align 4
  call void @llvm.dbg.declare(metadata ptr %6, metadata !699, metadata !DIExpression()), !dbg !1155
  call void @llvm.dbg.value(metadata ptr %1, metadata !704, metadata !DIExpression()), !dbg !1155
  call void @llvm.dbg.declare(metadata ptr %7, metadata !706, metadata !DIExpression()), !dbg !1155
  call void @llvm.dbg.declare(metadata ptr %8, metadata !708, metadata !DIExpression()), !dbg !1155
  call void @llvm.dbg.declare(metadata ptr %9, metadata !710, metadata !DIExpression()), !dbg !1155
  %10 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !1156
  %11 = load i32, ptr %10, align 4, !dbg !1156
  %12 = icmp eq i32 %11, 1, !dbg !1157
  br i1 %12, label %13, label %17, !dbg !1158

13:                                               ; preds = %5
  %14 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 1, !dbg !1159
  %15 = load i32, ptr %14, align 4, !dbg !1159
  %16 = icmp eq i32 %15, 1, !dbg !1160
  br label %17

17:                                               ; preds = %13, %5
  %18 = phi i1 [ false, %5 ], [ %16, %13 ], !dbg !1155
  ret i1 %18, !dbg !1155
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_62(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4, ptr noundef byval(%struct.triangle) align 8 %5, ptr noundef %6, ptr noundef %7) #0 !dbg !746 !pallas.exprWrapper !809 {
  %9 = alloca %struct.point, align 4
  %10 = alloca %struct.point, align 4
  %11 = alloca %struct.point, align 4
  %12 = alloca %struct.point, align 4
  store i64 %0, ptr %9, align 4
  store i64 %2, ptr %10, align 4
  store i64 %3, ptr %11, align 4
  store i64 %4, ptr %12, align 4
  call void @llvm.dbg.declare(metadata ptr %9, metadata !745, metadata !DIExpression()), !dbg !1161
  call void @llvm.dbg.value(metadata ptr %1, metadata !750, metadata !DIExpression()), !dbg !1161
  call void @llvm.dbg.declare(metadata ptr %10, metadata !752, metadata !DIExpression()), !dbg !1161
  call void @llvm.dbg.declare(metadata ptr %11, metadata !754, metadata !DIExpression()), !dbg !1161
  call void @llvm.dbg.declare(metadata ptr %12, metadata !756, metadata !DIExpression()), !dbg !1161
  call void @llvm.dbg.declare(metadata ptr %5, metadata !758, metadata !DIExpression()), !dbg !1161
  call void @llvm.dbg.value(metadata ptr %6, metadata !760, metadata !DIExpression()), !dbg !1161
  call void @llvm.dbg.value(metadata ptr %7, metadata !762, metadata !DIExpression()), !dbg !1161
  %13 = call i32 @avr_x(ptr noundef %6), !dbg !1162
  %14 = icmp eq i32 %13, 2, !dbg !1163
  ret i1 %14, !dbg !1161
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_63(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4, ptr noundef byval(%struct.triangle) align 8 %5, ptr noundef %6, ptr noundef %7, i64 %8, ptr noundef %9, i32 noundef %10) #0 !dbg !786 !pallas.exprWrapper !809 {
  %12 = alloca %struct.point, align 4
  %13 = alloca %struct.point, align 4
  %14 = alloca %struct.point, align 4
  %15 = alloca %struct.point, align 4
  %16 = alloca %struct.polygon, align 8
  store i64 %0, ptr %12, align 4
  store i64 %2, ptr %13, align 4
  store i64 %3, ptr %14, align 4
  store i64 %4, ptr %15, align 4
  %17 = getelementptr inbounds %struct.polygon, ptr %16, i32 0, i32 0
  %18 = inttoptr i64 %8 to ptr
  store ptr %18, ptr %17, align 8
  call void @llvm.dbg.declare(metadata ptr %12, metadata !785, metadata !DIExpression()), !dbg !1164
  call void @llvm.dbg.value(metadata ptr %1, metadata !790, metadata !DIExpression()), !dbg !1164
  call void @llvm.dbg.declare(metadata ptr %13, metadata !792, metadata !DIExpression()), !dbg !1164
  call void @llvm.dbg.declare(metadata ptr %14, metadata !794, metadata !DIExpression()), !dbg !1164
  call void @llvm.dbg.declare(metadata ptr %15, metadata !796, metadata !DIExpression()), !dbg !1164
  call void @llvm.dbg.declare(metadata ptr %5, metadata !798, metadata !DIExpression()), !dbg !1164
  call void @llvm.dbg.value(metadata ptr %6, metadata !800, metadata !DIExpression()), !dbg !1164
  call void @llvm.dbg.value(metadata ptr %7, metadata !802, metadata !DIExpression()), !dbg !1164
  call void @llvm.dbg.declare(metadata ptr %16, metadata !804, metadata !DIExpression()), !dbg !1164
  call void @llvm.dbg.value(metadata ptr %9, metadata !806, metadata !DIExpression()), !dbg !1164
  call void @llvm.dbg.value(metadata i32 %10, metadata !808, metadata !DIExpression()), !dbg !1164
  %19 = icmp eq i32 %10, 2, !dbg !1165
  ret i1 %19, !dbg !1164
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !1166 i32 @"pallas.old i32_noundef i32"(i32 noundef)

declare !pallas.specLib !1166 ptr @"pallas.old ptr_noundef ptr"(ptr noundef)

declare !pallas.specLib !1167 i32 @"pallas.result i32"()

declare !pallas.specLib !1168 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !1169 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !1170 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !1171 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !1172 i32 @"pallas.boundVar i32"(ptr)

declare !pallas.specLib !1173 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !1174 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !1175 i1 @pallas.imply(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }

!llvm.dbg.cu = !{!9, !11}
!llvm.module.flags = !{!15, !16, !17, !18, !19, !20, !21}
!llvm.ident = !{!22, !22}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 447, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "33c6d918b90095d11f2c07cbd9e8a6ff")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 447, type: !3, isLocal: true, isDefinition: true)
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !10, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!10 = !DIFile(filename: "examples/concepts/llvm/structs.c", directory: ".", checksumkind: CSK_MD5, checksum: "818f8498e33117445f7416aff20ec114")
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
!23 = distinct !DISubprogram(name: "alter_struct", scope: !10, file: !10, line: 36, type: !24, scopeLine: 36, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!24 = !DISubroutineType(types: !25)
!25 = !{null, !26}
!26 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !27, size: 64)
!27 = !DIDerivedType(tag: DW_TAG_typedef, name: "point", file: !10, line: 7, baseType: !28)
!28 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "point", file: !10, line: 4, size: 64, elements: !29)
!29 = !{!30, !32}
!30 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !28, file: !10, line: 5, baseType: !31, size: 32)
!31 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!32 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !28, file: !10, line: 6, baseType: !31, size: 32, offset: 32)
!33 = !{}
!34 = !{!35, i1 false, i1 false, !33, !33, !37, !53, !59, !65, !71, !77, !83, !89}
!35 = !{!"pallas.srcLoc", i64 26, i64 1, i64 35, i64 1, !36}
!36 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/structs.c", directory: "", checksumkind: CSK_MD5, checksum: "818f8498e33117445f7416aff20ec114")
!37 = !{!"pallas.requires", !38, ptr @PALLAS_SPEC_0, !33, !33, !39}
!38 = !{!"pallas.srcLoc", i64 27, i64 5, i64 27, i64 23, !36}
!39 = !{!40}
!40 = !{!41, !42}
!41 = !DILocalVariable(name: "p", arg: 1, scope: !23, file: !10, line: 36, type: !26)
!42 = !DILocalVariable(name: "p", arg: 1, scope: !43, file: !10, line: 27, type: !47)
!43 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !10, file: !10, line: 27, type: !44, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!44 = !DISubroutineType(types: !45)
!45 = !{!46, !47}
!46 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!47 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !48, size: 64)
!48 = !DIDerivedType(tag: DW_TAG_typedef, name: "point", file: !2, line: 8, baseType: !49)
!49 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "point", file: !2, line: 5, size: 64, elements: !50)
!50 = !{!51, !52}
!51 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !49, file: !2, line: 6, baseType: !31, size: 32)
!52 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !49, file: !2, line: 7, baseType: !31, size: 32, offset: 32)
!53 = !{!"pallas.requires", !54, ptr @PALLAS_SPEC_1, !33, !33, !55}
!54 = !{!"pallas.srcLoc", i64 28, i64 5, i64 28, i64 41, !36}
!55 = !{!56}
!56 = !{!41, !57}
!57 = !DILocalVariable(name: "p", arg: 1, scope: !58, file: !10, line: 28, type: !47)
!58 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !10, file: !10, line: 28, type: !44, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!59 = !{!"pallas.requires", !60, ptr @PALLAS_SPEC_2, !33, !33, !61}
!60 = !{!"pallas.srcLoc", i64 29, i64 5, i64 29, i64 41, !36}
!61 = !{!62}
!62 = !{!41, !63}
!63 = !DILocalVariable(name: "p", arg: 1, scope: !64, file: !10, line: 29, type: !47)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !10, file: !10, line: 29, type: !44, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!65 = !{!"pallas.ensures", !66, ptr @PALLAS_SPEC_3, !33, !33, !67}
!66 = !{!"pallas.srcLoc", i64 30, i64 5, i64 30, i64 40, !36}
!67 = !{!68}
!68 = !{!41, !69}
!69 = !DILocalVariable(name: "p", arg: 1, scope: !70, file: !10, line: 30, type: !47)
!70 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !10, file: !10, line: 30, type: !44, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!71 = !{!"pallas.ensures", !72, ptr @PALLAS_SPEC_4, !33, !33, !73}
!72 = !{!"pallas.srcLoc", i64 31, i64 5, i64 31, i64 40, !36}
!73 = !{!74}
!74 = !{!41, !75}
!75 = !DILocalVariable(name: "p", arg: 1, scope: !76, file: !10, line: 31, type: !47)
!76 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !10, file: !10, line: 31, type: !44, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!77 = !{!"pallas.ensures", !78, ptr @PALLAS_SPEC_5, !33, !33, !79}
!78 = !{!"pallas.srcLoc", i64 32, i64 5, i64 32, i64 22, !36}
!79 = !{!80}
!80 = !{!41, !81}
!81 = !DILocalVariable(name: "p", arg: 1, scope: !82, file: !10, line: 32, type: !47)
!82 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !10, file: !10, line: 32, type: !44, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!83 = !{!"pallas.ensures", !84, ptr @PALLAS_SPEC_6, !33, !33, !85}
!84 = !{!"pallas.srcLoc", i64 33, i64 5, i64 33, i64 22, !36}
!85 = !{!86}
!86 = !{!41, !87}
!87 = !DILocalVariable(name: "p", arg: 1, scope: !88, file: !10, line: 33, type: !47)
!88 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !10, file: !10, line: 33, type: !44, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!89 = !{!"pallas.ensures", !90, ptr @PALLAS_SPEC_7, !33, !33, !91}
!90 = !{!"pallas.srcLoc", i64 34, i64 5, i64 34, i64 36, !36}
!91 = !{!92}
!92 = !{!41, !93}
!93 = !DILocalVariable(name: "p", arg: 1, scope: !94, file: !10, line: 34, type: !47)
!94 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !10, file: !10, line: 34, type: !44, scopeLine: 34, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!95 = !DILocation(line: 36, column: 26, scope: !23)
!96 = !DILocation(line: 37, column: 5, scope: !23)
!97 = !DILocation(line: 37, column: 8, scope: !23)
!98 = !DILocation(line: 37, column: 10, scope: !23)
!99 = !DILocation(line: 38, column: 5, scope: !23)
!100 = !DILocation(line: 38, column: 8, scope: !23)
!101 = !DILocation(line: 38, column: 10, scope: !23)
!102 = !DILocation(line: 39, column: 1, scope: !23)
!103 = distinct !DISubprogram(name: "alter_struct2", scope: !10, file: !10, line: 51, type: !24, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!104 = !{!105, i1 false, i1 false, !33, !33, !106, !113, !119, !125, !131, !137, !143, !149}
!105 = !{!"pallas.srcLoc", i64 41, i64 1, i64 50, i64 1, !36}
!106 = !{!"pallas.requires", !107, ptr @PALLAS_SPEC_8, !33, !33, !108}
!107 = !{!"pallas.srcLoc", i64 42, i64 5, i64 42, i64 23, !36}
!108 = !{!109}
!109 = !{!110, !111}
!110 = !DILocalVariable(name: "p", arg: 1, scope: !103, file: !10, line: 51, type: !26)
!111 = !DILocalVariable(name: "p", arg: 1, scope: !112, file: !10, line: 42, type: !47)
!112 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !10, file: !10, line: 42, type: !44, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!113 = !{!"pallas.requires", !114, ptr @PALLAS_SPEC_9, !33, !33, !115}
!114 = !{!"pallas.srcLoc", i64 43, i64 5, i64 43, i64 41, !36}
!115 = !{!116}
!116 = !{!110, !117}
!117 = !DILocalVariable(name: "p", arg: 1, scope: !118, file: !10, line: 43, type: !47)
!118 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !10, file: !10, line: 43, type: !44, scopeLine: 43, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!119 = !{!"pallas.requires", !120, ptr @PALLAS_SPEC_10, !33, !33, !121}
!120 = !{!"pallas.srcLoc", i64 44, i64 5, i64 44, i64 41, !36}
!121 = !{!122}
!122 = !{!110, !123}
!123 = !DILocalVariable(name: "p", arg: 1, scope: !124, file: !10, line: 44, type: !47)
!124 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !10, file: !10, line: 44, type: !44, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!125 = !{!"pallas.ensures", !126, ptr @PALLAS_SPEC_11, !33, !33, !127}
!126 = !{!"pallas.srcLoc", i64 45, i64 5, i64 45, i64 40, !36}
!127 = !{!128}
!128 = !{!110, !129}
!129 = !DILocalVariable(name: "p", arg: 1, scope: !130, file: !10, line: 45, type: !47)
!130 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !10, file: !10, line: 45, type: !44, scopeLine: 45, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!131 = !{!"pallas.ensures", !132, ptr @PALLAS_SPEC_12, !33, !33, !133}
!132 = !{!"pallas.srcLoc", i64 46, i64 5, i64 46, i64 40, !36}
!133 = !{!134}
!134 = !{!110, !135}
!135 = !DILocalVariable(name: "p", arg: 1, scope: !136, file: !10, line: 46, type: !47)
!136 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !10, file: !10, line: 46, type: !44, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!137 = !{!"pallas.ensures", !138, ptr @PALLAS_SPEC_13, !33, !33, !139}
!138 = !{!"pallas.srcLoc", i64 47, i64 5, i64 47, i64 22, !36}
!139 = !{!140}
!140 = !{!110, !141}
!141 = !DILocalVariable(name: "p", arg: 1, scope: !142, file: !10, line: 47, type: !47)
!142 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !10, file: !10, line: 47, type: !44, scopeLine: 47, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!143 = !{!"pallas.ensures", !144, ptr @PALLAS_SPEC_14, !33, !33, !145}
!144 = !{!"pallas.srcLoc", i64 48, i64 5, i64 48, i64 22, !36}
!145 = !{!146}
!146 = !{!110, !147}
!147 = !DILocalVariable(name: "p", arg: 1, scope: !148, file: !10, line: 48, type: !47)
!148 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !10, file: !10, line: 48, type: !44, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!149 = !{!"pallas.ensures", !150, ptr @PALLAS_SPEC_15, !33, !33, !151}
!150 = !{!"pallas.srcLoc", i64 49, i64 5, i64 49, i64 36, !36}
!151 = !{!152}
!152 = !{!110, !153}
!153 = !DILocalVariable(name: "p", arg: 1, scope: !154, file: !10, line: 49, type: !47)
!154 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !10, file: !10, line: 49, type: !44, scopeLine: 49, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!155 = !DILocation(line: 51, column: 26, scope: !103)
!156 = !DILocation(line: 52, column: 5, scope: !103)
!157 = !DILocation(line: 52, column: 8, scope: !103)
!158 = !DILocation(line: 52, column: 10, scope: !103)
!159 = !DILocation(line: 53, column: 5, scope: !103)
!160 = !DILocation(line: 53, column: 8, scope: !103)
!161 = !DILocation(line: 53, column: 10, scope: !103)
!162 = !DILocation(line: 54, column: 1, scope: !103)
!163 = distinct !DISubprogram(name: "alter_struct_1", scope: !10, file: !10, line: 64, type: !24, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!164 = !{!165, i1 false, i1 false, !33, !33, !166, !173, !179, !185, !191, !197}
!165 = !{!"pallas.srcLoc", i64 56, i64 1, i64 63, i64 1, !36}
!166 = !{!"pallas.requires", !167, ptr @PALLAS_SPEC_16, !33, !33, !168}
!167 = !{!"pallas.srcLoc", i64 57, i64 5, i64 57, i64 23, !36}
!168 = !{!169}
!169 = !{!170, !171}
!170 = !DILocalVariable(name: "p", arg: 1, scope: !163, file: !10, line: 64, type: !26)
!171 = !DILocalVariable(name: "p", arg: 1, scope: !172, file: !10, line: 57, type: !47)
!172 = distinct !DISubprogram(name: "PALLAS_SPEC_16", scope: !10, file: !10, line: 57, type: !44, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!173 = !{!"pallas.requires", !174, ptr @PALLAS_SPEC_17, !33, !33, !175}
!174 = !{!"pallas.srcLoc", i64 58, i64 5, i64 58, i64 39, !36}
!175 = !{!176}
!176 = !{!170, !177}
!177 = !DILocalVariable(name: "p", arg: 1, scope: !178, file: !10, line: 58, type: !47)
!178 = distinct !DISubprogram(name: "PALLAS_SPEC_17", scope: !10, file: !10, line: 58, type: !44, scopeLine: 58, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!179 = !{!"pallas.ensures", !180, ptr @PALLAS_SPEC_18, !33, !33, !181}
!180 = !{!"pallas.srcLoc", i64 59, i64 5, i64 59, i64 38, !36}
!181 = !{!182}
!182 = !{!170, !183}
!183 = !DILocalVariable(name: "p", arg: 1, scope: !184, file: !10, line: 59, type: !47)
!184 = distinct !DISubprogram(name: "PALLAS_SPEC_18", scope: !10, file: !10, line: 59, type: !44, scopeLine: 59, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!185 = !{!"pallas.ensures", !186, ptr @PALLAS_SPEC_19, !33, !33, !187}
!186 = !{!"pallas.srcLoc", i64 60, i64 5, i64 60, i64 40, !36}
!187 = !{!188}
!188 = !{!170, !189}
!189 = !DILocalVariable(name: "p", arg: 1, scope: !190, file: !10, line: 60, type: !47)
!190 = distinct !DISubprogram(name: "PALLAS_SPEC_19", scope: !10, file: !10, line: 60, type: !44, scopeLine: 60, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!191 = !{!"pallas.ensures", !192, ptr @PALLAS_SPEC_20, !33, !33, !193}
!192 = !{!"pallas.srcLoc", i64 61, i64 5, i64 61, i64 40, !36}
!193 = !{!194}
!194 = !{!170, !195}
!195 = !DILocalVariable(name: "p", arg: 1, scope: !196, file: !10, line: 61, type: !47)
!196 = distinct !DISubprogram(name: "PALLAS_SPEC_20", scope: !10, file: !10, line: 61, type: !44, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!197 = !{!"pallas.ensures", !198, ptr @PALLAS_SPEC_21, !33, !33, !199}
!198 = !{!"pallas.srcLoc", i64 62, i64 5, i64 62, i64 36, !36}
!199 = !{!200}
!200 = !{!170, !201}
!201 = !DILocalVariable(name: "p", arg: 1, scope: !202, file: !10, line: 62, type: !47)
!202 = distinct !DISubprogram(name: "PALLAS_SPEC_21", scope: !10, file: !10, line: 62, type: !44, scopeLine: 62, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!203 = !DILocation(line: 64, column: 28, scope: !163)
!204 = !DILocation(line: 65, column: 12, scope: !163)
!205 = !DILocation(line: 65, column: 15, scope: !163)
!206 = !DILocation(line: 65, column: 16, scope: !163)
!207 = !DILocation(line: 65, column: 5, scope: !163)
!208 = !DILocation(line: 65, column: 8, scope: !163)
!209 = !DILocation(line: 65, column: 10, scope: !163)
!210 = !DILocation(line: 66, column: 12, scope: !163)
!211 = !DILocation(line: 66, column: 15, scope: !163)
!212 = !DILocation(line: 66, column: 16, scope: !163)
!213 = !DILocation(line: 66, column: 5, scope: !163)
!214 = !DILocation(line: 66, column: 8, scope: !163)
!215 = !DILocation(line: 66, column: 10, scope: !163)
!216 = !DILocation(line: 67, column: 1, scope: !163)
!217 = distinct !DISubprogram(name: "alter_copy_struct", scope: !10, file: !10, line: 76, type: !218, scopeLine: 76, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!218 = !DISubroutineType(types: !219)
!219 = !{null, !27}
!220 = !{!221, i1 false, i1 false, !33, !33, !222, !231, !237, !243, !249}
!221 = !{!"pallas.srcLoc", i64 69, i64 1, i64 75, i64 1, !36}
!222 = !{!"pallas.requires", !223, ptr @PALLAS_SPEC_22, !33, !33, !224}
!223 = !{!"pallas.srcLoc", i64 70, i64 3, i64 70, i64 24, !36}
!224 = !{!225}
!225 = !{!226, !227}
!226 = !DILocalVariable(name: "p", arg: 1, scope: !217, file: !10, line: 76, type: !27)
!227 = !DILocalVariable(name: "p", arg: 1, scope: !228, file: !10, line: 70, type: !48)
!228 = distinct !DISubprogram(name: "PALLAS_SPEC_22", scope: !10, file: !10, line: 70, type: !229, scopeLine: 70, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!229 = !DISubroutineType(types: !230)
!230 = !{!46, !48}
!231 = !{!"pallas.requires", !232, ptr @PALLAS_SPEC_23, !33, !33, !233}
!232 = !{!"pallas.srcLoc", i64 71, i64 3, i64 71, i64 38, !36}
!233 = !{!234}
!234 = !{!226, !235}
!235 = !DILocalVariable(name: "p", arg: 1, scope: !236, file: !10, line: 71, type: !48)
!236 = distinct !DISubprogram(name: "PALLAS_SPEC_23", scope: !10, file: !10, line: 71, type: !229, scopeLine: 71, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!237 = !{!"pallas.requires", !238, ptr @PALLAS_SPEC_24, !33, !33, !239}
!238 = !{!"pallas.srcLoc", i64 72, i64 3, i64 72, i64 38, !36}
!239 = !{!240}
!240 = !{!226, !241}
!241 = !DILocalVariable(name: "p", arg: 1, scope: !242, file: !10, line: 72, type: !48)
!242 = distinct !DISubprogram(name: "PALLAS_SPEC_24", scope: !10, file: !10, line: 72, type: !229, scopeLine: 72, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!243 = !{!"pallas.ensures", !244, ptr @PALLAS_SPEC_25, !33, !33, !245}
!244 = !{!"pallas.srcLoc", i64 73, i64 3, i64 73, i64 37, !36}
!245 = !{!246}
!246 = !{!226, !247}
!247 = !DILocalVariable(name: "p", arg: 1, scope: !248, file: !10, line: 73, type: !48)
!248 = distinct !DISubprogram(name: "PALLAS_SPEC_25", scope: !10, file: !10, line: 73, type: !229, scopeLine: 73, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!249 = !{!"pallas.ensures", !250, ptr @PALLAS_SPEC_26, !33, !33, !251}
!250 = !{!"pallas.srcLoc", i64 74, i64 3, i64 74, i64 37, !36}
!251 = !{!252}
!252 = !{!226, !253}
!253 = !DILocalVariable(name: "p", arg: 1, scope: !254, file: !10, line: 74, type: !48)
!254 = distinct !DISubprogram(name: "PALLAS_SPEC_26", scope: !10, file: !10, line: 74, type: !229, scopeLine: 74, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!255 = !DILocation(line: 76, column: 30, scope: !217)
!256 = !DILocation(line: 77, column: 7, scope: !217)
!257 = !DILocation(line: 77, column: 9, scope: !217)
!258 = !DILocation(line: 78, column: 7, scope: !217)
!259 = !DILocation(line: 78, column: 9, scope: !217)
!260 = !DILocation(line: 79, column: 1, scope: !217)
!261 = distinct !DISubprogram(name: "alter_copy_struct_2", scope: !10, file: !10, line: 85, type: !218, scopeLine: 85, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!262 = !{!263, i1 false, i1 false, !33, !33, !264, !271}
!263 = !{!"pallas.srcLoc", i64 81, i64 1, i64 84, i64 1, !36}
!264 = !{!"pallas.requires", !265, ptr @PALLAS_SPEC_27, !33, !33, !266}
!265 = !{!"pallas.srcLoc", i64 82, i64 3, i64 82, i64 24, !36}
!266 = !{!267}
!267 = !{!268, !269}
!268 = !DILocalVariable(name: "p", arg: 1, scope: !261, file: !10, line: 85, type: !27)
!269 = !DILocalVariable(name: "p", arg: 1, scope: !270, file: !10, line: 82, type: !48)
!270 = distinct !DISubprogram(name: "PALLAS_SPEC_27", scope: !10, file: !10, line: 82, type: !229, scopeLine: 82, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!271 = !{!"pallas.requires", !272, ptr @PALLAS_SPEC_28, !33, !33, !273}
!272 = !{!"pallas.srcLoc", i64 83, i64 3, i64 83, i64 36, !36}
!273 = !{!274}
!274 = !{!268, !275}
!275 = !DILocalVariable(name: "p", arg: 1, scope: !276, file: !10, line: 83, type: !48)
!276 = distinct !DISubprogram(name: "PALLAS_SPEC_28", scope: !10, file: !10, line: 83, type: !229, scopeLine: 83, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!277 = !DILocation(line: 85, column: 32, scope: !261)
!278 = !DILocation(line: 86, column: 7, scope: !261)
!279 = !DILocation(line: 86, column: 9, scope: !261)
!280 = !DILocation(line: 87, column: 7, scope: !261)
!281 = !DILocation(line: 87, column: 9, scope: !261)
!282 = !DILocation(line: 88, column: 1, scope: !261)
!283 = distinct !DISubprogram(name: "avr_x", scope: !10, file: !10, line: 96, type: !284, scopeLine: 96, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!284 = !DISubroutineType(types: !285)
!285 = !{!31, !286}
!286 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !287, size: 64)
!287 = !DIDerivedType(tag: DW_TAG_typedef, name: "triangle", file: !10, line: 11, baseType: !288)
!288 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "triangle", file: !10, line: 9, size: 192, elements: !289)
!289 = !{!290, !291, !292}
!290 = !DIDerivedType(tag: DW_TAG_member, name: "p1", scope: !288, file: !10, line: 10, baseType: !27, size: 64)
!291 = !DIDerivedType(tag: DW_TAG_member, name: "p2", scope: !288, file: !10, line: 10, baseType: !27, size: 64, offset: 64)
!292 = !DIDerivedType(tag: DW_TAG_member, name: "p3", scope: !288, file: !10, line: 10, baseType: !27, size: 64, offset: 128)
!293 = !{!294, i1 false, i1 false, !33, !33, !295, !311, !317, !323}
!294 = !{!"pallas.srcLoc", i64 90, i64 1, i64 95, i64 1, !36}
!295 = !{!"pallas.requires", !296, ptr @PALLAS_SPEC_29, !33, !33, !297}
!296 = !{!"pallas.srcLoc", i64 91, i64 3, i64 91, i64 21, !36}
!297 = !{!298}
!298 = !{!299, !300}
!299 = !DILocalVariable(name: "r", arg: 1, scope: !283, file: !10, line: 96, type: !286)
!300 = !DILocalVariable(name: "r", arg: 1, scope: !301, file: !10, line: 91, type: !304)
!301 = distinct !DISubprogram(name: "PALLAS_SPEC_29", scope: !10, file: !10, line: 91, type: !302, scopeLine: 91, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!302 = !DISubroutineType(types: !303)
!303 = !{!46, !304}
!304 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !305, size: 64)
!305 = !DIDerivedType(tag: DW_TAG_typedef, name: "triangle", file: !2, line: 12, baseType: !306)
!306 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "triangle", file: !2, line: 10, size: 192, elements: !307)
!307 = !{!308, !309, !310}
!308 = !DIDerivedType(tag: DW_TAG_member, name: "p1", scope: !306, file: !2, line: 11, baseType: !48, size: 64)
!309 = !DIDerivedType(tag: DW_TAG_member, name: "p2", scope: !306, file: !2, line: 11, baseType: !48, size: 64, offset: 64)
!310 = !DIDerivedType(tag: DW_TAG_member, name: "p3", scope: !306, file: !2, line: 11, baseType: !48, size: 64, offset: 128)
!311 = !{!"pallas.requires", !312, ptr @PALLAS_SPEC_30, !33, !33, !313}
!312 = !{!"pallas.srcLoc", i64 92, i64 3, i64 92, i64 37, !36}
!313 = !{!314}
!314 = !{!299, !315}
!315 = !DILocalVariable(name: "r", arg: 1, scope: !316, file: !10, line: 92, type: !304)
!316 = distinct !DISubprogram(name: "PALLAS_SPEC_30", scope: !10, file: !10, line: 92, type: !302, scopeLine: 92, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!317 = !{!"pallas.ensures", !318, ptr @PALLAS_SPEC_31, !33, !33, !319}
!318 = !{!"pallas.srcLoc", i64 93, i64 3, i64 93, i64 36, !36}
!319 = !{!320}
!320 = !{!299, !321}
!321 = !DILocalVariable(name: "r", arg: 1, scope: !322, file: !10, line: 93, type: !304)
!322 = distinct !DISubprogram(name: "PALLAS_SPEC_31", scope: !10, file: !10, line: 93, type: !302, scopeLine: 93, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!323 = !{!"pallas.ensures", !324, ptr @PALLAS_SPEC_32, !33, !33, !325}
!324 = !{!"pallas.srcLoc", i64 94, i64 3, i64 94, i64 58, !36}
!325 = !{!326}
!326 = !{!299, !327}
!327 = !DILocalVariable(name: "r", arg: 1, scope: !328, file: !10, line: 94, type: !304)
!328 = distinct !DISubprogram(name: "PALLAS_SPEC_32", scope: !10, file: !10, line: 94, type: !302, scopeLine: 94, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!329 = !DILocation(line: 96, column: 21, scope: !283)
!330 = !DILocation(line: 97, column: 13, scope: !283)
!331 = !DILocation(line: 97, column: 16, scope: !283)
!332 = !DILocation(line: 97, column: 19, scope: !283)
!333 = !DILocation(line: 97, column: 23, scope: !283)
!334 = !DILocation(line: 97, column: 26, scope: !283)
!335 = !DILocation(line: 97, column: 29, scope: !283)
!336 = !DILocation(line: 97, column: 21, scope: !283)
!337 = !DILocation(line: 97, column: 33, scope: !283)
!338 = !DILocation(line: 97, column: 36, scope: !283)
!339 = !DILocation(line: 97, column: 39, scope: !283)
!340 = !DILocation(line: 97, column: 31, scope: !283)
!341 = !DILocation(line: 97, column: 41, scope: !283)
!342 = !DILocation(line: 97, column: 5, scope: !283)
!343 = distinct !DISubprogram(name: "avr_x_pol", scope: !10, file: !10, line: 113, type: !344, scopeLine: 113, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!344 = !DISubroutineType(types: !345)
!345 = !{!31, !346, !31}
!346 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !347, size: 64)
!347 = !DIDerivedType(tag: DW_TAG_typedef, name: "polygon", file: !10, line: 15, baseType: !348)
!348 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "polygon", file: !10, line: 13, size: 64, elements: !349)
!349 = !{!350}
!350 = !DIDerivedType(tag: DW_TAG_member, name: "ps", scope: !348, file: !10, line: 14, baseType: !26, size: 64)
!351 = !{!352, i1 false, i1 false, !33, !33, !353, !370, !378, !386, !394, !402, !410, !418, !426, !434, !442}
!352 = !{!"pallas.srcLoc", i64 100, i64 1, i64 112, i64 1, !36}
!353 = !{!"pallas.requires", !354, ptr @PALLAS_SPEC_33, !33, !33, !355}
!354 = !{!"pallas.srcLoc", i64 101, i64 3, i64 101, i64 19, !36}
!355 = !{!356, !367}
!356 = !{!357, !358}
!357 = !DILocalVariable(name: "p", arg: 1, scope: !343, file: !10, line: 113, type: !346)
!358 = !DILocalVariable(name: "p", arg: 1, scope: !359, file: !10, line: 101, type: !362)
!359 = distinct !DISubprogram(name: "PALLAS_SPEC_33", scope: !10, file: !10, line: 101, type: !360, scopeLine: 101, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!360 = !DISubroutineType(types: !361)
!361 = !{!46, !362, !31}
!362 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !363, size: 64)
!363 = !DIDerivedType(tag: DW_TAG_typedef, name: "polygon", file: !2, line: 16, baseType: !364)
!364 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "polygon", file: !2, line: 14, size: 64, elements: !365)
!365 = !{!366}
!366 = !DIDerivedType(tag: DW_TAG_member, name: "ps", scope: !364, file: !2, line: 15, baseType: !47, size: 64)
!367 = !{!368, !369}
!368 = !DILocalVariable(name: "len", arg: 2, scope: !343, file: !10, line: 113, type: !31)
!369 = !DILocalVariable(name: "len", arg: 2, scope: !359, file: !10, line: 101, type: !31)
!370 = !{!"pallas.requires", !371, ptr @PALLAS_SPEC_34, !33, !33, !372}
!371 = !{!"pallas.srcLoc", i64 102, i64 3, i64 102, i64 21, !36}
!372 = !{!373, !376}
!373 = !{!357, !374}
!374 = !DILocalVariable(name: "p", arg: 1, scope: !375, file: !10, line: 102, type: !362)
!375 = distinct !DISubprogram(name: "PALLAS_SPEC_34", scope: !10, file: !10, line: 102, type: !360, scopeLine: 102, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!376 = !{!368, !377}
!377 = !DILocalVariable(name: "len", arg: 2, scope: !375, file: !10, line: 102, type: !31)
!378 = !{!"pallas.requires", !379, ptr @PALLAS_SPEC_35, !33, !33, !380}
!379 = !{!"pallas.srcLoc", i64 103, i64 3, i64 103, i64 37, !36}
!380 = !{!381, !384}
!381 = !{!357, !382}
!382 = !DILocalVariable(name: "p", arg: 1, scope: !383, file: !10, line: 103, type: !362)
!383 = distinct !DISubprogram(name: "PALLAS_SPEC_35", scope: !10, file: !10, line: 103, type: !360, scopeLine: 103, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!384 = !{!368, !385}
!385 = !DILocalVariable(name: "len", arg: 2, scope: !383, file: !10, line: 103, type: !31)
!386 = !{!"pallas.requires", !387, ptr @PALLAS_SPEC_36, !33, !33, !388}
!387 = !{!"pallas.srcLoc", i64 104, i64 3, i64 104, i64 54, !36}
!388 = !{!389, !392}
!389 = !{!357, !390}
!390 = !DILocalVariable(name: "p", arg: 1, scope: !391, file: !10, line: 104, type: !362)
!391 = distinct !DISubprogram(name: "PALLAS_SPEC_36", scope: !10, file: !10, line: 104, type: !360, scopeLine: 104, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!392 = !{!368, !393}
!393 = !DILocalVariable(name: "len", arg: 2, scope: !391, file: !10, line: 104, type: !31)
!394 = !{!"pallas.requires", !395, ptr @PALLAS_SPEC_37, !33, !33, !396}
!395 = !{!"pallas.srcLoc", i64 105, i64 3, i64 105, i64 191, !36}
!396 = !{!397, !400}
!397 = !{!357, !398}
!398 = !DILocalVariable(name: "p", arg: 1, scope: !399, file: !10, line: 105, type: !362)
!399 = distinct !DISubprogram(name: "PALLAS_SPEC_37", scope: !10, file: !10, line: 105, type: !360, scopeLine: 105, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!400 = !{!368, !401}
!401 = !DILocalVariable(name: "len", arg: 2, scope: !399, file: !10, line: 105, type: !31)
!402 = !{!"pallas.requires", !403, ptr @PALLAS_SPEC_38, !33, !33, !404}
!403 = !{!"pallas.srcLoc", i64 106, i64 3, i64 106, i64 106, !36}
!404 = !{!405, !408}
!405 = !{!357, !406}
!406 = !DILocalVariable(name: "p", arg: 1, scope: !407, file: !10, line: 106, type: !362)
!407 = distinct !DISubprogram(name: "PALLAS_SPEC_38", scope: !10, file: !10, line: 106, type: !360, scopeLine: 106, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!408 = !{!368, !409}
!409 = !DILocalVariable(name: "len", arg: 2, scope: !407, file: !10, line: 106, type: !31)
!410 = !{!"pallas.ensures", !411, ptr @PALLAS_SPEC_39, !33, !33, !412}
!411 = !{!"pallas.srcLoc", i64 107, i64 3, i64 107, i64 36, !36}
!412 = !{!413, !416}
!413 = !{!357, !414}
!414 = !DILocalVariable(name: "p", arg: 1, scope: !415, file: !10, line: 107, type: !362)
!415 = distinct !DISubprogram(name: "PALLAS_SPEC_39", scope: !10, file: !10, line: 107, type: !360, scopeLine: 107, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!416 = !{!368, !417}
!417 = !DILocalVariable(name: "len", arg: 2, scope: !415, file: !10, line: 107, type: !31)
!418 = !{!"pallas.ensures", !419, ptr @PALLAS_SPEC_40, !33, !33, !420}
!419 = !{!"pallas.srcLoc", i64 108, i64 3, i64 108, i64 53, !36}
!420 = !{!421, !424}
!421 = !{!357, !422}
!422 = !DILocalVariable(name: "p", arg: 1, scope: !423, file: !10, line: 108, type: !362)
!423 = distinct !DISubprogram(name: "PALLAS_SPEC_40", scope: !10, file: !10, line: 108, type: !360, scopeLine: 108, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!424 = !{!368, !425}
!425 = !DILocalVariable(name: "len", arg: 2, scope: !423, file: !10, line: 108, type: !31)
!426 = !{!"pallas.ensures", !427, ptr @PALLAS_SPEC_41, !33, !33, !428}
!427 = !{!"pallas.srcLoc", i64 109, i64 3, i64 109, i64 190, !36}
!428 = !{!429, !432}
!429 = !{!357, !430}
!430 = !DILocalVariable(name: "p", arg: 1, scope: !431, file: !10, line: 109, type: !362)
!431 = distinct !DISubprogram(name: "PALLAS_SPEC_41", scope: !10, file: !10, line: 109, type: !360, scopeLine: 109, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!432 = !{!368, !433}
!433 = !DILocalVariable(name: "len", arg: 2, scope: !431, file: !10, line: 109, type: !31)
!434 = !{!"pallas.ensures", !435, ptr @PALLAS_SPEC_42, !33, !33, !436}
!435 = !{!"pallas.srcLoc", i64 110, i64 3, i64 110, i64 105, !36}
!436 = !{!437, !440}
!437 = !{!357, !438}
!438 = !DILocalVariable(name: "p", arg: 1, scope: !439, file: !10, line: 110, type: !362)
!439 = distinct !DISubprogram(name: "PALLAS_SPEC_42", scope: !10, file: !10, line: 110, type: !360, scopeLine: 110, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!440 = !{!368, !441}
!441 = !DILocalVariable(name: "len", arg: 2, scope: !439, file: !10, line: 110, type: !31)
!442 = !{!"pallas.ensures", !443, ptr @PALLAS_SPEC_43, !33, !33, !444}
!443 = !{!"pallas.srcLoc", i64 111, i64 3, i64 111, i64 87, !36}
!444 = !{!445, !448}
!445 = !{!357, !446}
!446 = !DILocalVariable(name: "p", arg: 1, scope: !447, file: !10, line: 111, type: !362)
!447 = distinct !DISubprogram(name: "PALLAS_SPEC_43", scope: !10, file: !10, line: 111, type: !360, scopeLine: 111, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!448 = !{!368, !449}
!449 = !DILocalVariable(name: "len", arg: 2, scope: !447, file: !10, line: 111, type: !31)
!450 = !DILocation(line: 113, column: 24, scope: !343)
!451 = !DILocation(line: 113, column: 31, scope: !343)
!452 = !DILocalVariable(name: "sum", scope: !343, file: !10, line: 114, type: !31)
!453 = !DILocation(line: 114, column: 9, scope: !343)
!454 = !DILocalVariable(name: "i", scope: !455, file: !10, line: 127, type: !31)
!455 = distinct !DILexicalBlock(scope: !343, file: !10, line: 127, column: 5)
!456 = !DILocation(line: 127, column: 13, scope: !455)
!457 = !DILocation(line: 127, column: 9, scope: !455)
!458 = !DILocation(line: 127, column: 18, scope: !459)
!459 = distinct !DILexicalBlock(scope: !455, file: !10, line: 127, column: 5)
!460 = !DILocation(line: 127, column: 20, scope: !459)
!461 = !DILocation(line: 127, column: 19, scope: !459)
!462 = !DILocation(line: 127, column: 5, scope: !455)
!463 = !DILocation(line: 128, column: 16, scope: !464)
!464 = distinct !DILexicalBlock(scope: !459, file: !10, line: 127, column: 29)
!465 = !DILocation(line: 128, column: 19, scope: !464)
!466 = !DILocation(line: 128, column: 22, scope: !464)
!467 = !DILocation(line: 128, column: 25, scope: !464)
!468 = !DILocation(line: 128, column: 13, scope: !464)
!469 = !DILocation(line: 129, column: 5, scope: !464)
!470 = !DILocation(line: 127, column: 26, scope: !459)
!471 = !DILocation(line: 127, column: 5, scope: !459)
!472 = distinct !{!472, !462, !473, !474, !475}
!473 = !DILocation(line: 129, column: 5, scope: !455)
!474 = !{!"llvm.loop.mustprogress"}
!475 = !{!"pallas.loopInvBlock", !476, !477, !491, !503, !515, !527, !539, !551, !563, !575, !587}
!476 = !{!"pallas.srcLoc", i64 115, i64 5, i64 126, i64 5, !36}
!477 = !{!"pallas.loopInv", !478, ptr @PALLAS_SPEC_44, !33, !33, !479}
!478 = !{!"pallas.srcLoc", i64 116, i64 7, i64 116, i64 36, !36}
!479 = !{!480, !485, !487, !489}
!480 = !{!357, !481}
!481 = !DILocalVariable(name: "p", arg: 1, scope: !482, file: !10, line: 116, type: !362)
!482 = distinct !DISubprogram(name: "PALLAS_SPEC_44", scope: !10, file: !10, line: 116, type: !483, scopeLine: 116, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!483 = !DISubroutineType(types: !484)
!484 = !{!46, !362, !31, !31, !31}
!485 = !{!368, !486}
!486 = !DILocalVariable(name: "len", arg: 2, scope: !482, file: !10, line: 116, type: !31)
!487 = !{!452, !488}
!488 = !DILocalVariable(name: "sum", arg: 3, scope: !482, file: !10, line: 116, type: !31)
!489 = !{!454, !490}
!490 = !DILocalVariable(name: "i", arg: 4, scope: !482, file: !10, line: 116, type: !31)
!491 = !{!"pallas.loopInv", !492, ptr @PALLAS_SPEC_45, !33, !33, !493}
!492 = !{!"pallas.srcLoc", i64 117, i64 7, i64 117, i64 31, !36}
!493 = !{!494, !497, !499, !501}
!494 = !{!357, !495}
!495 = !DILocalVariable(name: "p", arg: 1, scope: !496, file: !10, line: 117, type: !362)
!496 = distinct !DISubprogram(name: "PALLAS_SPEC_45", scope: !10, file: !10, line: 117, type: !483, scopeLine: 117, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!497 = !{!368, !498}
!498 = !DILocalVariable(name: "len", arg: 2, scope: !496, file: !10, line: 117, type: !31)
!499 = !{!452, !500}
!500 = !DILocalVariable(name: "sum", arg: 3, scope: !496, file: !10, line: 117, type: !31)
!501 = !{!454, !502}
!502 = !DILocalVariable(name: "i", arg: 4, scope: !496, file: !10, line: 117, type: !31)
!503 = !{!"pallas.loopInv", !504, ptr @PALLAS_SPEC_46, !33, !33, !505}
!504 = !{!"pallas.srcLoc", i64 118, i64 7, i64 118, i64 47, !36}
!505 = !{!506, !509, !511, !513}
!506 = !{!357, !507}
!507 = !DILocalVariable(name: "p", arg: 1, scope: !508, file: !10, line: 118, type: !362)
!508 = distinct !DISubprogram(name: "PALLAS_SPEC_46", scope: !10, file: !10, line: 118, type: !483, scopeLine: 118, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!509 = !{!368, !510}
!510 = !DILocalVariable(name: "len", arg: 2, scope: !508, file: !10, line: 118, type: !31)
!511 = !{!452, !512}
!512 = !DILocalVariable(name: "sum", arg: 3, scope: !508, file: !10, line: 118, type: !31)
!513 = !{!454, !514}
!514 = !DILocalVariable(name: "i", arg: 4, scope: !508, file: !10, line: 118, type: !31)
!515 = !{!"pallas.loopInv", !516, ptr @PALLAS_SPEC_47, !33, !33, !517}
!516 = !{!"pallas.srcLoc", i64 119, i64 7, i64 119, i64 64, !36}
!517 = !{!518, !521, !523, !525}
!518 = !{!357, !519}
!519 = !DILocalVariable(name: "p", arg: 1, scope: !520, file: !10, line: 119, type: !362)
!520 = distinct !DISubprogram(name: "PALLAS_SPEC_47", scope: !10, file: !10, line: 119, type: !483, scopeLine: 119, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!521 = !{!368, !522}
!522 = !DILocalVariable(name: "len", arg: 2, scope: !520, file: !10, line: 119, type: !31)
!523 = !{!452, !524}
!524 = !DILocalVariable(name: "sum", arg: 3, scope: !520, file: !10, line: 119, type: !31)
!525 = !{!454, !526}
!526 = !DILocalVariable(name: "i", arg: 4, scope: !520, file: !10, line: 119, type: !31)
!527 = !{!"pallas.loopInv", !528, ptr @PALLAS_SPEC_48, !33, !33, !529}
!528 = !{!"pallas.srcLoc", i64 120, i64 7, i64 120, i64 201, !36}
!529 = !{!530, !533, !535, !537}
!530 = !{!357, !531}
!531 = !DILocalVariable(name: "p", arg: 1, scope: !532, file: !10, line: 120, type: !362)
!532 = distinct !DISubprogram(name: "PALLAS_SPEC_48", scope: !10, file: !10, line: 120, type: !483, scopeLine: 120, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!533 = !{!368, !534}
!534 = !DILocalVariable(name: "len", arg: 2, scope: !532, file: !10, line: 120, type: !31)
!535 = !{!452, !536}
!536 = !DILocalVariable(name: "sum", arg: 3, scope: !532, file: !10, line: 120, type: !31)
!537 = !{!454, !538}
!538 = !DILocalVariable(name: "i", arg: 4, scope: !532, file: !10, line: 120, type: !31)
!539 = !{!"pallas.loopInv", !540, ptr @PALLAS_SPEC_49, !33, !33, !541}
!540 = !{!"pallas.srcLoc", i64 121, i64 7, i64 121, i64 116, !36}
!541 = !{!542, !545, !547, !549}
!542 = !{!357, !543}
!543 = !DILocalVariable(name: "p", arg: 1, scope: !544, file: !10, line: 121, type: !362)
!544 = distinct !DISubprogram(name: "PALLAS_SPEC_49", scope: !10, file: !10, line: 121, type: !483, scopeLine: 121, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!545 = !{!368, !546}
!546 = !DILocalVariable(name: "len", arg: 2, scope: !544, file: !10, line: 121, type: !31)
!547 = !{!452, !548}
!548 = !DILocalVariable(name: "sum", arg: 3, scope: !544, file: !10, line: 121, type: !31)
!549 = !{!454, !550}
!550 = !DILocalVariable(name: "i", arg: 4, scope: !544, file: !10, line: 121, type: !31)
!551 = !{!"pallas.loopInv", !552, ptr @PALLAS_SPEC_50, !33, !33, !553}
!552 = !{!"pallas.srcLoc", i64 122, i64 7, i64 122, i64 48, !36}
!553 = !{!554, !557, !559, !561}
!554 = !{!357, !555}
!555 = !DILocalVariable(name: "p", arg: 1, scope: !556, file: !10, line: 122, type: !362)
!556 = distinct !DISubprogram(name: "PALLAS_SPEC_50", scope: !10, file: !10, line: 122, type: !483, scopeLine: 122, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!557 = !{!368, !558}
!558 = !DILocalVariable(name: "len", arg: 2, scope: !556, file: !10, line: 122, type: !31)
!559 = !{!452, !560}
!560 = !DILocalVariable(name: "sum", arg: 3, scope: !556, file: !10, line: 122, type: !31)
!561 = !{!454, !562}
!562 = !DILocalVariable(name: "i", arg: 4, scope: !556, file: !10, line: 122, type: !31)
!563 = !{!"pallas.loopInv", !564, ptr @PALLAS_SPEC_51, !33, !33, !565}
!564 = !{!"pallas.srcLoc", i64 123, i64 7, i64 123, i64 57, !36}
!565 = !{!566, !569, !571, !573}
!566 = !{!357, !567}
!567 = !DILocalVariable(name: "p", arg: 1, scope: !568, file: !10, line: 123, type: !362)
!568 = distinct !DISubprogram(name: "PALLAS_SPEC_51", scope: !10, file: !10, line: 123, type: !483, scopeLine: 123, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!569 = !{!368, !570}
!570 = !DILocalVariable(name: "len", arg: 2, scope: !568, file: !10, line: 123, type: !31)
!571 = !{!452, !572}
!572 = !DILocalVariable(name: "sum", arg: 3, scope: !568, file: !10, line: 123, type: !31)
!573 = !{!454, !574}
!574 = !DILocalVariable(name: "i", arg: 4, scope: !568, file: !10, line: 123, type: !31)
!575 = !{!"pallas.loopInv", !576, ptr @PALLAS_SPEC_52, !33, !33, !577}
!576 = !{!"pallas.srcLoc", i64 124, i64 7, i64 124, i64 70, !36}
!577 = !{!578, !581, !583, !585}
!578 = !{!357, !579}
!579 = !DILocalVariable(name: "p", arg: 1, scope: !580, file: !10, line: 124, type: !362)
!580 = distinct !DISubprogram(name: "PALLAS_SPEC_52", scope: !10, file: !10, line: 124, type: !483, scopeLine: 124, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!581 = !{!368, !582}
!582 = !DILocalVariable(name: "len", arg: 2, scope: !580, file: !10, line: 124, type: !31)
!583 = !{!452, !584}
!584 = !DILocalVariable(name: "sum", arg: 3, scope: !580, file: !10, line: 124, type: !31)
!585 = !{!454, !586}
!586 = !DILocalVariable(name: "i", arg: 4, scope: !580, file: !10, line: 124, type: !31)
!587 = !{!"pallas.loopInv", !588, ptr @PALLAS_SPEC_53, !33, !33, !589}
!588 = !{!"pallas.srcLoc", i64 125, i64 7, i64 125, i64 83, !36}
!589 = !{!590, !593, !595, !597}
!590 = !{!357, !591}
!591 = !DILocalVariable(name: "p", arg: 1, scope: !592, file: !10, line: 125, type: !362)
!592 = distinct !DISubprogram(name: "PALLAS_SPEC_53", scope: !10, file: !10, line: 125, type: !483, scopeLine: 125, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!593 = !{!368, !594}
!594 = !DILocalVariable(name: "len", arg: 2, scope: !592, file: !10, line: 125, type: !31)
!595 = !{!452, !596}
!596 = !DILocalVariable(name: "sum", arg: 3, scope: !592, file: !10, line: 125, type: !31)
!597 = !{!454, !598}
!598 = !DILocalVariable(name: "i", arg: 4, scope: !592, file: !10, line: 125, type: !31)
!599 = !DILocation(line: 131, column: 12, scope: !343)
!600 = !DILocation(line: 131, column: 16, scope: !343)
!601 = !DILocation(line: 131, column: 15, scope: !343)
!602 = !DILocation(line: 131, column: 5, scope: !343)
!603 = distinct !DISubprogram(name: "main", scope: !10, file: !10, line: 135, type: !604, scopeLine: 135, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!604 = !DISubroutineType(types: !605)
!605 = !{!31}
!606 = !DILocalVariable(name: "p", scope: !603, file: !10, line: 136, type: !27)
!607 = !DILocation(line: 136, column: 11, scope: !603)
!608 = !DILocalVariable(name: "pp", scope: !603, file: !10, line: 137, type: !26)
!609 = !DILocation(line: 137, column: 12, scope: !603)
!610 = !DILocation(line: 138, column: 8, scope: !603)
!611 = !DILocation(line: 142, column: 7, scope: !603)
!612 = !{!613, !614}
!613 = !{!"pallas.srcLoc", i64 140, i64 5, i64 140, i64 29, !36}
!614 = !{!"pallas.assert", !615, ptr @PALLAS_SPEC_54, !33, !33, !616}
!615 = !{!"pallas.srcLoc", i64 140, i64 9, i64 140, i64 27, !36}
!616 = !{!617, !622}
!617 = !{!606, !618}
!618 = !DILocalVariable(name: "p", arg: 1, scope: !619, file: !10, line: 140, type: !48)
!619 = distinct !DISubprogram(name: "PALLAS_SPEC_54", scope: !10, file: !10, line: 140, type: !620, scopeLine: 140, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!620 = !DISubroutineType(types: !621)
!621 = !{!46, !48, !47}
!622 = !{!608, !623}
!623 = !DILocalVariable(name: "pp", arg: 2, scope: !619, file: !10, line: 140, type: !47)
!624 = !DILocation(line: 142, column: 9, scope: !603)
!625 = !DILocation(line: 143, column: 7, scope: !603)
!626 = !DILocation(line: 143, column: 9, scope: !603)
!627 = !DILocation(line: 146, column: 5, scope: !603)
!628 = !{!629, !630, !638}
!629 = !{!"pallas.srcLoc", i64 144, i64 5, i64 145, i64 24, !36}
!630 = !{!"pallas.assert", !631, ptr @PALLAS_SPEC_55, !33, !33, !632}
!631 = !{!"pallas.srcLoc", i64 144, i64 9, i64 144, i64 26, !36}
!632 = !{!633, !636}
!633 = !{!606, !634}
!634 = !DILocalVariable(name: "p", arg: 1, scope: !635, file: !10, line: 144, type: !48)
!635 = distinct !DISubprogram(name: "PALLAS_SPEC_55", scope: !10, file: !10, line: 144, type: !620, scopeLine: 144, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!636 = !{!608, !637}
!637 = !DILocalVariable(name: "pp", arg: 2, scope: !635, file: !10, line: 144, type: !47)
!638 = !{!"pallas.assert", !639, ptr @PALLAS_SPEC_56, !33, !33, !640}
!639 = !{!"pallas.srcLoc", i64 145, i64 5, i64 145, i64 22, !36}
!640 = !{!641, !644}
!641 = !{!606, !642}
!642 = !DILocalVariable(name: "p", arg: 1, scope: !643, file: !10, line: 145, type: !48)
!643 = distinct !DISubprogram(name: "PALLAS_SPEC_56", scope: !10, file: !10, line: 145, type: !620, scopeLine: 145, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!644 = !{!608, !645}
!645 = !DILocalVariable(name: "pp", arg: 2, scope: !643, file: !10, line: 145, type: !47)
!646 = !DILocation(line: 150, column: 18, scope: !603)
!647 = !{!648, !649, !657}
!648 = !{!"pallas.srcLoc", i64 147, i64 5, i64 148, i64 22, !36}
!649 = !{!"pallas.assert", !650, ptr @PALLAS_SPEC_57, !33, !33, !651}
!650 = !{!"pallas.srcLoc", i64 147, i64 9, i64 147, i64 24, !36}
!651 = !{!652, !655}
!652 = !{!606, !653}
!653 = !DILocalVariable(name: "p", arg: 1, scope: !654, file: !10, line: 147, type: !48)
!654 = distinct !DISubprogram(name: "PALLAS_SPEC_57", scope: !10, file: !10, line: 147, type: !620, scopeLine: 147, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!655 = !{!608, !656}
!656 = !DILocalVariable(name: "pp", arg: 2, scope: !654, file: !10, line: 147, type: !47)
!657 = !{!"pallas.assert", !658, ptr @PALLAS_SPEC_58, !33, !33, !659}
!658 = !{!"pallas.srcLoc", i64 148, i64 5, i64 148, i64 20, !36}
!659 = !{!660, !663}
!660 = !{!606, !661}
!661 = !DILocalVariable(name: "p", arg: 1, scope: !662, file: !10, line: 148, type: !48)
!662 = distinct !DISubprogram(name: "PALLAS_SPEC_58", scope: !10, file: !10, line: 148, type: !620, scopeLine: 148, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!663 = !{!608, !664}
!664 = !DILocalVariable(name: "pp", arg: 2, scope: !662, file: !10, line: 148, type: !47)
!665 = !DILocation(line: 150, column: 5, scope: !603)
!666 = !DILocation(line: 153, column: 20, scope: !603)
!667 = !{!668, !669, !677}
!668 = !{!"pallas.srcLoc", i64 151, i64 5, i64 152, i64 22, !36}
!669 = !{!"pallas.assert", !670, ptr @PALLAS_SPEC_59, !33, !33, !671}
!670 = !{!"pallas.srcLoc", i64 151, i64 9, i64 151, i64 26, !36}
!671 = !{!672, !675}
!672 = !{!606, !673}
!673 = !DILocalVariable(name: "p", arg: 1, scope: !674, file: !10, line: 151, type: !48)
!674 = distinct !DISubprogram(name: "PALLAS_SPEC_59", scope: !10, file: !10, line: 151, type: !620, scopeLine: 151, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!675 = !{!608, !676}
!676 = !DILocalVariable(name: "pp", arg: 2, scope: !674, file: !10, line: 151, type: !47)
!677 = !{!"pallas.assert", !678, ptr @PALLAS_SPEC_60, !33, !33, !679}
!678 = !{!"pallas.srcLoc", i64 152, i64 5, i64 152, i64 20, !36}
!679 = !{!680, !683}
!680 = !{!606, !681}
!681 = !DILocalVariable(name: "p", arg: 1, scope: !682, file: !10, line: 152, type: !48)
!682 = distinct !DISubprogram(name: "PALLAS_SPEC_60", scope: !10, file: !10, line: 152, type: !620, scopeLine: 152, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!683 = !{!608, !684}
!684 = !DILocalVariable(name: "pp", arg: 2, scope: !682, file: !10, line: 152, type: !47)
!685 = !DILocation(line: 153, column: 5, scope: !603)
!686 = !DILocalVariable(name: "p1", scope: !603, file: !10, line: 156, type: !27)
!687 = !DILocation(line: 156, column: 11, scope: !603)
!688 = !DILocalVariable(name: "p2", scope: !603, file: !10, line: 156, type: !27)
!689 = !DILocation(line: 156, column: 15, scope: !603)
!690 = !DILocalVariable(name: "p3", scope: !603, file: !10, line: 156, type: !27)
!691 = !DILocation(line: 156, column: 19, scope: !603)
!692 = !DILocation(line: 157, column: 8, scope: !603)
!693 = !{!694, !695}
!694 = !{!"pallas.srcLoc", i64 154, i64 5, i64 154, i64 38, !36}
!695 = !{!"pallas.assert", !696, ptr @PALLAS_SPEC_61, !33, !33, !697}
!696 = !{!"pallas.srcLoc", i64 154, i64 9, i64 154, i64 36, !36}
!697 = !{!698, !703, !705, !707, !709}
!698 = !{!606, !699}
!699 = !DILocalVariable(name: "p", arg: 1, scope: !700, file: !10, line: 154, type: !48)
!700 = distinct !DISubprogram(name: "PALLAS_SPEC_61", scope: !10, file: !10, line: 154, type: !701, scopeLine: 154, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!701 = !DISubroutineType(types: !702)
!702 = !{!46, !48, !47, !48, !48, !48}
!703 = !{!608, !704}
!704 = !DILocalVariable(name: "pp", arg: 2, scope: !700, file: !10, line: 154, type: !47)
!705 = !{!686, !706}
!706 = !DILocalVariable(name: "p1", arg: 3, scope: !700, file: !10, line: 154, type: !48)
!707 = !{!688, !708}
!708 = !DILocalVariable(name: "p2", arg: 4, scope: !700, file: !10, line: 154, type: !48)
!709 = !{!690, !710}
!710 = !DILocalVariable(name: "p3", arg: 5, scope: !700, file: !10, line: 154, type: !48)
!711 = !DILocation(line: 157, column: 10, scope: !603)
!712 = !DILocation(line: 157, column: 18, scope: !603)
!713 = !DILocation(line: 157, column: 20, scope: !603)
!714 = !DILocation(line: 158, column: 8, scope: !603)
!715 = !DILocation(line: 158, column: 10, scope: !603)
!716 = !DILocation(line: 158, column: 18, scope: !603)
!717 = !DILocation(line: 158, column: 20, scope: !603)
!718 = !DILocation(line: 159, column: 8, scope: !603)
!719 = !DILocation(line: 159, column: 10, scope: !603)
!720 = !DILocation(line: 159, column: 18, scope: !603)
!721 = !DILocation(line: 159, column: 20, scope: !603)
!722 = !DILocalVariable(name: "r", scope: !603, file: !10, line: 160, type: !287)
!723 = !DILocation(line: 160, column: 14, scope: !603)
!724 = !DILocalVariable(name: "rr", scope: !603, file: !10, line: 160, type: !286)
!725 = !DILocation(line: 160, column: 18, scope: !603)
!726 = !DILocation(line: 161, column: 8, scope: !603)
!727 = !DILocation(line: 162, column: 7, scope: !603)
!728 = !DILocation(line: 162, column: 12, scope: !603)
!729 = !DILocation(line: 163, column: 7, scope: !603)
!730 = !DILocation(line: 163, column: 12, scope: !603)
!731 = !DILocation(line: 164, column: 7, scope: !603)
!732 = !DILocation(line: 164, column: 12, scope: !603)
!733 = !DILocalVariable(name: "ps", scope: !603, file: !10, line: 166, type: !734)
!734 = !DICompositeType(tag: DW_TAG_array_type, baseType: !27, size: 192, elements: !735)
!735 = !{!736}
!736 = !DISubrange(count: 3)
!737 = !DILocation(line: 166, column: 11, scope: !603)
!738 = !DILocation(line: 166, column: 19, scope: !603)
!739 = !{!740, !741}
!740 = !{!"pallas.srcLoc", i64 165, i64 5, i64 165, i64 32, !36}
!741 = !{!"pallas.assert", !742, ptr @PALLAS_SPEC_62, !33, !33, !743}
!742 = !{!"pallas.srcLoc", i64 165, i64 9, i64 165, i64 30, !36}
!743 = !{!744, !749, !751, !753, !755, !757, !759, !761}
!744 = !{!606, !745}
!745 = !DILocalVariable(name: "p", arg: 1, scope: !746, file: !10, line: 165, type: !48)
!746 = distinct !DISubprogram(name: "PALLAS_SPEC_62", scope: !10, file: !10, line: 165, type: !747, scopeLine: 165, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!747 = !DISubroutineType(types: !748)
!748 = !{!46, !48, !47, !48, !48, !48, !305, !304, !47}
!749 = !{!608, !750}
!750 = !DILocalVariable(name: "pp", arg: 2, scope: !746, file: !10, line: 165, type: !47)
!751 = !{!686, !752}
!752 = !DILocalVariable(name: "p1", arg: 3, scope: !746, file: !10, line: 165, type: !48)
!753 = !{!688, !754}
!754 = !DILocalVariable(name: "p2", arg: 4, scope: !746, file: !10, line: 165, type: !48)
!755 = !{!690, !756}
!756 = !DILocalVariable(name: "p3", arg: 5, scope: !746, file: !10, line: 165, type: !48)
!757 = !{!722, !758}
!758 = !DILocalVariable(name: "r", arg: 6, scope: !746, file: !10, line: 165, type: !305)
!759 = !{!724, !760}
!760 = !DILocalVariable(name: "rr", arg: 7, scope: !746, file: !10, line: 165, type: !304)
!761 = !{!733, !762}
!762 = !DILocalVariable(name: "ps", arg: 8, scope: !746, file: !10, line: 165, type: !47)
!763 = !DILocation(line: 166, column: 20, scope: !603)
!764 = !DILocation(line: 166, column: 24, scope: !603)
!765 = !DILocation(line: 166, column: 28, scope: !603)
!766 = !DILocalVariable(name: "pol", scope: !603, file: !10, line: 167, type: !347)
!767 = !DILocation(line: 167, column: 13, scope: !603)
!768 = !DILocalVariable(name: "ppols", scope: !603, file: !10, line: 167, type: !346)
!769 = !DILocation(line: 167, column: 19, scope: !603)
!770 = !DILocation(line: 168, column: 11, scope: !603)
!771 = !DILocation(line: 169, column: 14, scope: !603)
!772 = !DILocation(line: 169, column: 9, scope: !603)
!773 = !DILocation(line: 169, column: 12, scope: !603)
!774 = !DILocalVariable(name: "avr_pol", scope: !603, file: !10, line: 170, type: !31)
!775 = !DILocation(line: 170, column: 9, scope: !603)
!776 = !DILocation(line: 170, column: 29, scope: !603)
!777 = !DILocation(line: 170, column: 19, scope: !603)
!778 = !DILocation(line: 173, column: 5, scope: !603)
!779 = !{!780, !781}
!780 = !{!"pallas.srcLoc", i64 171, i64 5, i64 171, i64 30, !36}
!781 = !{!"pallas.assert", !782, ptr @PALLAS_SPEC_63, !33, !33, !783}
!782 = !{!"pallas.srcLoc", i64 171, i64 9, i64 171, i64 28, !36}
!783 = !{!784, !789, !791, !793, !795, !797, !799, !801, !803, !805, !807}
!784 = !{!606, !785}
!785 = !DILocalVariable(name: "p", arg: 1, scope: !786, file: !10, line: 171, type: !48)
!786 = distinct !DISubprogram(name: "PALLAS_SPEC_63", scope: !10, file: !10, line: 171, type: !787, scopeLine: 171, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!787 = !DISubroutineType(types: !788)
!788 = !{!46, !48, !47, !48, !48, !48, !305, !304, !47, !363, !362, !31}
!789 = !{!608, !790}
!790 = !DILocalVariable(name: "pp", arg: 2, scope: !786, file: !10, line: 171, type: !47)
!791 = !{!686, !792}
!792 = !DILocalVariable(name: "p1", arg: 3, scope: !786, file: !10, line: 171, type: !48)
!793 = !{!688, !794}
!794 = !DILocalVariable(name: "p2", arg: 4, scope: !786, file: !10, line: 171, type: !48)
!795 = !{!690, !796}
!796 = !DILocalVariable(name: "p3", arg: 5, scope: !786, file: !10, line: 171, type: !48)
!797 = !{!722, !798}
!798 = !DILocalVariable(name: "r", arg: 6, scope: !786, file: !10, line: 171, type: !305)
!799 = !{!724, !800}
!800 = !DILocalVariable(name: "rr", arg: 7, scope: !786, file: !10, line: 171, type: !304)
!801 = !{!733, !802}
!802 = !DILocalVariable(name: "ps", arg: 8, scope: !786, file: !10, line: 171, type: !47)
!803 = !{!766, !804}
!804 = !DILocalVariable(name: "pol", arg: 9, scope: !786, file: !10, line: 171, type: !363)
!805 = !{!768, !806}
!806 = !DILocalVariable(name: "ppols", arg: 10, scope: !786, file: !10, line: 171, type: !362)
!807 = !{!774, !808}
!808 = !DILocalVariable(name: "avr_pol", arg: 11, scope: !786, file: !10, line: 171, type: !31)
!809 = !{!""}
!810 = !DILocation(line: 0, scope: !43)
!811 = !DILocation(line: 27, column: 16, scope: !43)
!812 = !DILocation(line: 0, scope: !58)
!813 = !DILocation(line: 28, column: 24, scope: !58)
!814 = !DILocation(line: 28, column: 27, scope: !58)
!815 = !DILocation(line: 28, column: 14, scope: !58)
!816 = !DILocation(line: 0, scope: !64)
!817 = !DILocation(line: 29, column: 24, scope: !64)
!818 = !DILocation(line: 29, column: 27, scope: !64)
!819 = !DILocation(line: 29, column: 14, scope: !64)
!820 = !DILocation(line: 0, scope: !70)
!821 = !DILocation(line: 30, column: 23, scope: !70)
!822 = !DILocation(line: 30, column: 26, scope: !70)
!823 = !DILocation(line: 30, column: 13, scope: !70)
!824 = !DILocation(line: 0, scope: !76)
!825 = !DILocation(line: 31, column: 23, scope: !76)
!826 = !DILocation(line: 31, column: 26, scope: !76)
!827 = !DILocation(line: 31, column: 13, scope: !76)
!828 = !DILocation(line: 0, scope: !82)
!829 = !DILocation(line: 32, column: 16, scope: !82)
!830 = !DILocation(line: 32, column: 18, scope: !82)
!831 = !DILocation(line: 0, scope: !88)
!832 = !DILocation(line: 33, column: 16, scope: !88)
!833 = !DILocation(line: 33, column: 18, scope: !88)
!834 = !DILocation(line: 0, scope: !94)
!835 = !DILocation(line: 34, column: 13, scope: !94)
!836 = !DILocation(line: 34, column: 32, scope: !94)
!837 = !DILocation(line: 0, scope: !112)
!838 = !DILocation(line: 42, column: 16, scope: !112)
!839 = !DILocation(line: 0, scope: !118)
!840 = !DILocation(line: 43, column: 24, scope: !118)
!841 = !DILocation(line: 43, column: 27, scope: !118)
!842 = !DILocation(line: 43, column: 14, scope: !118)
!843 = !DILocation(line: 0, scope: !124)
!844 = !DILocation(line: 44, column: 24, scope: !124)
!845 = !DILocation(line: 44, column: 27, scope: !124)
!846 = !DILocation(line: 44, column: 14, scope: !124)
!847 = !DILocation(line: 0, scope: !130)
!848 = !DILocation(line: 45, column: 23, scope: !130)
!849 = !DILocation(line: 45, column: 26, scope: !130)
!850 = !DILocation(line: 45, column: 13, scope: !130)
!851 = !DILocation(line: 0, scope: !136)
!852 = !DILocation(line: 46, column: 23, scope: !136)
!853 = !DILocation(line: 46, column: 26, scope: !136)
!854 = !DILocation(line: 46, column: 13, scope: !136)
!855 = !DILocation(line: 0, scope: !142)
!856 = !DILocation(line: 47, column: 16, scope: !142)
!857 = !DILocation(line: 47, column: 18, scope: !142)
!858 = !DILocation(line: 0, scope: !148)
!859 = !DILocation(line: 48, column: 16, scope: !148)
!860 = !DILocation(line: 48, column: 18, scope: !148)
!861 = !DILocation(line: 0, scope: !154)
!862 = !DILocation(line: 49, column: 13, scope: !154)
!863 = !DILocation(line: 49, column: 32, scope: !154)
!864 = !DILocation(line: 0, scope: !172)
!865 = !DILocation(line: 57, column: 16, scope: !172)
!866 = !DILocation(line: 0, scope: !178)
!867 = !DILocation(line: 58, column: 25, scope: !178)
!868 = !DILocation(line: 58, column: 14, scope: !178)
!869 = !DILocation(line: 0, scope: !184)
!870 = !DILocation(line: 59, column: 24, scope: !184)
!871 = !DILocation(line: 59, column: 13, scope: !184)
!872 = !DILocation(line: 0, scope: !190)
!873 = !DILocation(line: 60, column: 16, scope: !190)
!874 = !DILocation(line: 60, column: 34, scope: !190)
!875 = !DILocation(line: 60, column: 36, scope: !190)
!876 = !DILocation(line: 60, column: 21, scope: !190)
!877 = !DILocation(line: 60, column: 18, scope: !190)
!878 = !DILocation(line: 0, scope: !196)
!879 = !DILocation(line: 61, column: 16, scope: !196)
!880 = !DILocation(line: 61, column: 34, scope: !196)
!881 = !DILocation(line: 61, column: 36, scope: !196)
!882 = !DILocation(line: 61, column: 21, scope: !196)
!883 = !DILocation(line: 61, column: 18, scope: !196)
!884 = !DILocation(line: 0, scope: !202)
!885 = !DILocation(line: 62, column: 13, scope: !202)
!886 = !DILocation(line: 62, column: 32, scope: !202)
!887 = !DILocation(line: 0, scope: !228)
!888 = !DILocation(line: 70, column: 17, scope: !228)
!889 = !DILocation(line: 0, scope: !236)
!890 = !DILocation(line: 71, column: 21, scope: !236)
!891 = !DILocation(line: 71, column: 24, scope: !236)
!892 = !DILocation(line: 71, column: 12, scope: !236)
!893 = !DILocation(line: 0, scope: !242)
!894 = !DILocation(line: 72, column: 21, scope: !242)
!895 = !DILocation(line: 72, column: 24, scope: !242)
!896 = !DILocation(line: 72, column: 12, scope: !242)
!897 = !DILocation(line: 0, scope: !248)
!898 = !DILocation(line: 73, column: 20, scope: !248)
!899 = !DILocation(line: 73, column: 23, scope: !248)
!900 = !DILocation(line: 73, column: 11, scope: !248)
!901 = !DILocation(line: 0, scope: !254)
!902 = !DILocation(line: 74, column: 20, scope: !254)
!903 = !DILocation(line: 74, column: 23, scope: !254)
!904 = !DILocation(line: 74, column: 11, scope: !254)
!905 = !DILocation(line: 0, scope: !270)
!906 = !DILocation(line: 82, column: 17, scope: !270)
!907 = !DILocation(line: 0, scope: !276)
!908 = !DILocation(line: 83, column: 22, scope: !276)
!909 = !DILocation(line: 83, column: 12, scope: !276)
!910 = !DILocation(line: 0, scope: !301)
!911 = !DILocation(line: 91, column: 14, scope: !301)
!912 = !DILocation(line: 0, scope: !316)
!913 = !DILocation(line: 92, column: 23, scope: !316)
!914 = !DILocation(line: 92, column: 12, scope: !316)
!915 = !DILocation(line: 0, scope: !322)
!916 = !DILocation(line: 93, column: 22, scope: !322)
!917 = !DILocation(line: 93, column: 11, scope: !322)
!918 = !DILocation(line: 0, scope: !328)
!919 = !DILocation(line: 94, column: 11, scope: !328)
!920 = !DILocation(line: 94, column: 31, scope: !328)
!921 = !DILocation(line: 94, column: 34, scope: !328)
!922 = !DILocation(line: 94, column: 41, scope: !328)
!923 = !DILocation(line: 94, column: 44, scope: !328)
!924 = !DILocation(line: 94, column: 36, scope: !328)
!925 = !DILocation(line: 94, column: 51, scope: !328)
!926 = !DILocation(line: 94, column: 54, scope: !328)
!927 = !DILocation(line: 94, column: 46, scope: !328)
!928 = !DILocation(line: 94, column: 56, scope: !328)
!929 = !DILocation(line: 94, column: 24, scope: !328)
!930 = !DILocation(line: 0, scope: !359)
!931 = !DILocation(line: 101, column: 16, scope: !359)
!932 = !DILocation(line: 0, scope: !375)
!933 = !DILocation(line: 102, column: 14, scope: !375)
!934 = !DILocation(line: 0, scope: !383)
!935 = !DILocation(line: 103, column: 23, scope: !383)
!936 = !DILocation(line: 103, column: 12, scope: !383)
!937 = !DILocation(line: 0, scope: !391)
!938 = !DILocation(line: 104, column: 15, scope: !391)
!939 = !DILocation(line: 104, column: 18, scope: !391)
!940 = !DILocation(line: 104, column: 26, scope: !391)
!941 = !DILocation(line: 104, column: 44, scope: !391)
!942 = !DILocation(line: 104, column: 29, scope: !391)
!943 = !DILocation(line: 104, column: 51, scope: !391)
!944 = !DILocation(line: 104, column: 48, scope: !391)
!945 = !DILocation(line: 0, scope: !399)
!946 = !DILocation(line: 105, column: 30, scope: !399)
!947 = !DILocation(line: 105, column: 27, scope: !399)
!948 = !DILocation(line: 105, column: 48, scope: !399)
!949 = !DILocation(line: 105, column: 60, scope: !399)
!950 = !DILocation(line: 105, column: 77, scope: !399)
!951 = !DILocation(line: 105, column: 74, scope: !399)
!952 = !DILocation(line: 105, column: 90, scope: !399)
!953 = !DILocation(line: 105, column: 102, scope: !399)
!954 = !DILocation(line: 105, column: 67, scope: !399)
!955 = !DILocation(line: 105, column: 43, scope: !399)
!956 = !DILocation(line: 105, column: 20, scope: !399)
!957 = !DILocation(line: 105, column: 119, scope: !399)
!958 = !DILocation(line: 105, column: 134, scope: !399)
!959 = !DILocation(line: 105, column: 131, scope: !399)
!960 = !DILocation(line: 105, column: 150, scope: !399)
!961 = !DILocation(line: 105, column: 155, scope: !399)
!962 = !DILocation(line: 105, column: 153, scope: !399)
!963 = !DILocation(line: 105, column: 173, scope: !399)
!964 = !DILocation(line: 105, column: 178, scope: !399)
!965 = !DILocation(line: 105, column: 176, scope: !399)
!966 = !DILocation(line: 105, column: 167, scope: !399)
!967 = !DILocation(line: 105, column: 112, scope: !399)
!968 = !DILocation(line: 105, column: 12, scope: !399)
!969 = !DILocation(line: 0, scope: !407)
!970 = !DILocation(line: 106, column: 31, scope: !407)
!971 = !DILocation(line: 106, column: 28, scope: !407)
!972 = !DILocation(line: 106, column: 44, scope: !407)
!973 = !DILocation(line: 106, column: 56, scope: !407)
!974 = !DILocation(line: 106, column: 21, scope: !407)
!975 = !DILocation(line: 106, column: 74, scope: !407)
!976 = !DILocation(line: 106, column: 77, scope: !407)
!977 = !DILocation(line: 106, column: 71, scope: !407)
!978 = !DILocation(line: 106, column: 91, scope: !407)
!979 = !DILocation(line: 106, column: 64, scope: !407)
!980 = !DILocation(line: 106, column: 12, scope: !407)
!981 = !DILocation(line: 0, scope: !415)
!982 = !DILocation(line: 107, column: 22, scope: !415)
!983 = !DILocation(line: 107, column: 11, scope: !415)
!984 = !DILocation(line: 0, scope: !423)
!985 = !DILocation(line: 108, column: 14, scope: !423)
!986 = !DILocation(line: 108, column: 17, scope: !423)
!987 = !DILocation(line: 108, column: 25, scope: !423)
!988 = !DILocation(line: 108, column: 43, scope: !423)
!989 = !DILocation(line: 108, column: 28, scope: !423)
!990 = !DILocation(line: 108, column: 50, scope: !423)
!991 = !DILocation(line: 108, column: 47, scope: !423)
!992 = !DILocation(line: 0, scope: !431)
!993 = !DILocation(line: 109, column: 29, scope: !431)
!994 = !DILocation(line: 109, column: 26, scope: !431)
!995 = !DILocation(line: 109, column: 47, scope: !431)
!996 = !DILocation(line: 109, column: 59, scope: !431)
!997 = !DILocation(line: 109, column: 76, scope: !431)
!998 = !DILocation(line: 109, column: 73, scope: !431)
!999 = !DILocation(line: 109, column: 89, scope: !431)
!1000 = !DILocation(line: 109, column: 101, scope: !431)
!1001 = !DILocation(line: 109, column: 66, scope: !431)
!1002 = !DILocation(line: 109, column: 42, scope: !431)
!1003 = !DILocation(line: 109, column: 19, scope: !431)
!1004 = !DILocation(line: 109, column: 118, scope: !431)
!1005 = !DILocation(line: 109, column: 133, scope: !431)
!1006 = !DILocation(line: 109, column: 130, scope: !431)
!1007 = !DILocation(line: 109, column: 149, scope: !431)
!1008 = !DILocation(line: 109, column: 154, scope: !431)
!1009 = !DILocation(line: 109, column: 152, scope: !431)
!1010 = !DILocation(line: 109, column: 172, scope: !431)
!1011 = !DILocation(line: 109, column: 177, scope: !431)
!1012 = !DILocation(line: 109, column: 175, scope: !431)
!1013 = !DILocation(line: 109, column: 166, scope: !431)
!1014 = !DILocation(line: 109, column: 111, scope: !431)
!1015 = !DILocation(line: 109, column: 11, scope: !431)
!1016 = !DILocation(line: 0, scope: !439)
!1017 = !DILocation(line: 110, column: 30, scope: !439)
!1018 = !DILocation(line: 110, column: 27, scope: !439)
!1019 = !DILocation(line: 110, column: 43, scope: !439)
!1020 = !DILocation(line: 110, column: 55, scope: !439)
!1021 = !DILocation(line: 110, column: 20, scope: !439)
!1022 = !DILocation(line: 110, column: 73, scope: !439)
!1023 = !DILocation(line: 110, column: 76, scope: !439)
!1024 = !DILocation(line: 110, column: 70, scope: !439)
!1025 = !DILocation(line: 110, column: 90, scope: !439)
!1026 = !DILocation(line: 110, column: 63, scope: !439)
!1027 = !DILocation(line: 110, column: 11, scope: !439)
!1028 = !DILocation(line: 0, scope: !447)
!1029 = !DILocation(line: 111, column: 22, scope: !447)
!1030 = !DILocation(line: 111, column: 28, scope: !447)
!1031 = !DILocation(line: 111, column: 48, scope: !447)
!1032 = !DILocation(line: 111, column: 45, scope: !447)
!1033 = !DILocation(line: 111, column: 54, scope: !447)
!1034 = !DILocation(line: 111, column: 61, scope: !447)
!1035 = !DILocation(line: 111, column: 58, scope: !447)
!1036 = !DILocation(line: 111, column: 67, scope: !447)
!1037 = !DILocation(line: 111, column: 56, scope: !447)
!1038 = !DILocation(line: 111, column: 74, scope: !447)
!1039 = !DILocation(line: 111, column: 71, scope: !447)
!1040 = !DILocation(line: 111, column: 80, scope: !447)
!1041 = !DILocation(line: 111, column: 69, scope: !447)
!1042 = !DILocation(line: 111, column: 82, scope: !447)
!1043 = !DILocation(line: 111, column: 41, scope: !447)
!1044 = !DILocation(line: 111, column: 11, scope: !447)
!1045 = !DILocation(line: 0, scope: !496)
!1046 = !DILocation(line: 117, column: 24, scope: !496)
!1047 = !DILocation(line: 0, scope: !482)
!1048 = !DILocation(line: 116, column: 23, scope: !482)
!1049 = !DILocation(line: 116, column: 27, scope: !482)
!1050 = !DILocation(line: 116, column: 31, scope: !482)
!1051 = !DILocation(line: 0, scope: !520)
!1052 = !DILocation(line: 119, column: 25, scope: !520)
!1053 = !DILocation(line: 119, column: 28, scope: !520)
!1054 = !DILocation(line: 119, column: 36, scope: !520)
!1055 = !DILocation(line: 119, column: 54, scope: !520)
!1056 = !DILocation(line: 119, column: 39, scope: !520)
!1057 = !DILocation(line: 119, column: 61, scope: !520)
!1058 = !DILocation(line: 119, column: 58, scope: !520)
!1059 = !DILocation(line: 0, scope: !532)
!1060 = !DILocation(line: 120, column: 40, scope: !532)
!1061 = !DILocation(line: 120, column: 37, scope: !532)
!1062 = !DILocation(line: 120, column: 58, scope: !532)
!1063 = !DILocation(line: 120, column: 70, scope: !532)
!1064 = !DILocation(line: 120, column: 87, scope: !532)
!1065 = !DILocation(line: 120, column: 84, scope: !532)
!1066 = !DILocation(line: 120, column: 100, scope: !532)
!1067 = !DILocation(line: 120, column: 112, scope: !532)
!1068 = !DILocation(line: 120, column: 77, scope: !532)
!1069 = !DILocation(line: 120, column: 53, scope: !532)
!1070 = !DILocation(line: 120, column: 30, scope: !532)
!1071 = !DILocation(line: 120, column: 129, scope: !532)
!1072 = !DILocation(line: 120, column: 144, scope: !532)
!1073 = !DILocation(line: 120, column: 141, scope: !532)
!1074 = !DILocation(line: 120, column: 160, scope: !532)
!1075 = !DILocation(line: 120, column: 165, scope: !532)
!1076 = !DILocation(line: 120, column: 163, scope: !532)
!1077 = !DILocation(line: 120, column: 183, scope: !532)
!1078 = !DILocation(line: 120, column: 188, scope: !532)
!1079 = !DILocation(line: 120, column: 186, scope: !532)
!1080 = !DILocation(line: 120, column: 177, scope: !532)
!1081 = !DILocation(line: 120, column: 122, scope: !532)
!1082 = !DILocation(line: 120, column: 22, scope: !532)
!1083 = !DILocation(line: 0, scope: !544)
!1084 = !DILocation(line: 121, column: 41, scope: !544)
!1085 = !DILocation(line: 121, column: 38, scope: !544)
!1086 = !DILocation(line: 121, column: 54, scope: !544)
!1087 = !DILocation(line: 121, column: 66, scope: !544)
!1088 = !DILocation(line: 121, column: 31, scope: !544)
!1089 = !DILocation(line: 121, column: 84, scope: !544)
!1090 = !DILocation(line: 121, column: 87, scope: !544)
!1091 = !DILocation(line: 121, column: 81, scope: !544)
!1092 = !DILocation(line: 121, column: 101, scope: !544)
!1093 = !DILocation(line: 121, column: 74, scope: !544)
!1094 = !DILocation(line: 121, column: 22, scope: !544)
!1095 = !DILocation(line: 0, scope: !556)
!1096 = !DILocation(line: 122, column: 31, scope: !556)
!1097 = !DILocation(line: 122, column: 41, scope: !556)
!1098 = !DILocation(line: 122, column: 22, scope: !556)
!1099 = !DILocation(line: 0, scope: !508)
!1100 = !DILocation(line: 118, column: 33, scope: !508)
!1101 = !DILocation(line: 118, column: 22, scope: !508)
!1102 = !DILocation(line: 0, scope: !568)
!1103 = !DILocation(line: 123, column: 31, scope: !568)
!1104 = !DILocation(line: 123, column: 48, scope: !568)
!1105 = !DILocation(line: 123, column: 45, scope: !568)
!1106 = !DILocation(line: 123, column: 54, scope: !568)
!1107 = !DILocation(line: 123, column: 41, scope: !568)
!1108 = !DILocation(line: 123, column: 22, scope: !568)
!1109 = !DILocation(line: 0, scope: !580)
!1110 = !DILocation(line: 124, column: 31, scope: !580)
!1111 = !DILocation(line: 124, column: 48, scope: !580)
!1112 = !DILocation(line: 124, column: 45, scope: !580)
!1113 = !DILocation(line: 124, column: 54, scope: !580)
!1114 = !DILocation(line: 124, column: 61, scope: !580)
!1115 = !DILocation(line: 124, column: 58, scope: !580)
!1116 = !DILocation(line: 124, column: 67, scope: !580)
!1117 = !DILocation(line: 124, column: 56, scope: !580)
!1118 = !DILocation(line: 124, column: 41, scope: !580)
!1119 = !DILocation(line: 124, column: 22, scope: !580)
!1120 = !DILocation(line: 0, scope: !592)
!1121 = !DILocation(line: 125, column: 31, scope: !592)
!1122 = !DILocation(line: 125, column: 48, scope: !592)
!1123 = !DILocation(line: 125, column: 45, scope: !592)
!1124 = !DILocation(line: 125, column: 54, scope: !592)
!1125 = !DILocation(line: 125, column: 61, scope: !592)
!1126 = !DILocation(line: 125, column: 58, scope: !592)
!1127 = !DILocation(line: 125, column: 67, scope: !592)
!1128 = !DILocation(line: 125, column: 56, scope: !592)
!1129 = !DILocation(line: 125, column: 74, scope: !592)
!1130 = !DILocation(line: 125, column: 71, scope: !592)
!1131 = !DILocation(line: 125, column: 80, scope: !592)
!1132 = !DILocation(line: 125, column: 69, scope: !592)
!1133 = !DILocation(line: 125, column: 41, scope: !592)
!1134 = !DILocation(line: 125, column: 22, scope: !592)
!1135 = !DILocation(line: 0, scope: !619)
!1136 = !DILocation(line: 140, column: 19, scope: !619)
!1137 = !DILocation(line: 0, scope: !635)
!1138 = !DILocation(line: 144, column: 20, scope: !635)
!1139 = !DILocation(line: 144, column: 22, scope: !635)
!1140 = !DILocation(line: 0, scope: !643)
!1141 = !DILocation(line: 145, column: 16, scope: !643)
!1142 = !DILocation(line: 145, column: 18, scope: !643)
!1143 = !DILocation(line: 0, scope: !654)
!1144 = !DILocation(line: 147, column: 18, scope: !654)
!1145 = !DILocation(line: 147, column: 20, scope: !654)
!1146 = !DILocation(line: 0, scope: !662)
!1147 = !DILocation(line: 148, column: 14, scope: !662)
!1148 = !DILocation(line: 148, column: 16, scope: !662)
!1149 = !DILocation(line: 0, scope: !674)
!1150 = !DILocation(line: 151, column: 20, scope: !674)
!1151 = !DILocation(line: 151, column: 22, scope: !674)
!1152 = !DILocation(line: 0, scope: !682)
!1153 = !DILocation(line: 152, column: 14, scope: !682)
!1154 = !DILocation(line: 152, column: 16, scope: !682)
!1155 = !DILocation(line: 0, scope: !700)
!1156 = !DILocation(line: 154, column: 18, scope: !700)
!1157 = !DILocation(line: 154, column: 20, scope: !700)
!1158 = !DILocation(line: 154, column: 25, scope: !700)
!1159 = !DILocation(line: 154, column: 30, scope: !700)
!1160 = !DILocation(line: 154, column: 32, scope: !700)
!1161 = !DILocation(line: 0, scope: !746)
!1162 = !DILocation(line: 165, column: 16, scope: !746)
!1163 = !DILocation(line: 165, column: 26, scope: !746)
!1164 = !DILocation(line: 0, scope: !786)
!1165 = !DILocation(line: 171, column: 24, scope: !786)
!1166 = !{!"pallas.old"}
!1167 = !{!"pallas.result"}
!1168 = !{!"pallas.ptrLength"}
!1169 = !{!"pallas.forall"}
!1170 = !{!"pallas.forallSep"}
!1171 = !{!"pallas.scAnd"}
!1172 = !{!"pallas.boundVar"}
!1173 = !{!"pallas.perm"}
!1174 = !{!"pallas.fracOf"}
!1175 = !{!"pallas.imply"}
