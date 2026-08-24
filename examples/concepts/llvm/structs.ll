; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/structs.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.point = type { i32, i32 }
%struct.triangle = type { %struct.point, %struct.point, %struct.point }
%struct.polygon = type { ptr }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [62 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_15, ptr @PALLAS_SPEC_16, ptr @PALLAS_SPEC_17, ptr @PALLAS_SPEC_18, ptr @PALLAS_SPEC_19, ptr @PALLAS_SPEC_20, ptr @PALLAS_SPEC_21, ptr @PALLAS_SPEC_22, ptr @PALLAS_SPEC_23, ptr @PALLAS_SPEC_24, ptr @PALLAS_SPEC_25, ptr @PALLAS_SPEC_26, ptr @PALLAS_SPEC_27, ptr @PALLAS_SPEC_28, ptr @PALLAS_SPEC_29, ptr @PALLAS_SPEC_30, ptr @PALLAS_SPEC_31, ptr @PALLAS_SPEC_32, ptr @PALLAS_SPEC_33, ptr @PALLAS_SPEC_34, ptr @PALLAS_SPEC_35, ptr @PALLAS_SPEC_36, ptr @PALLAS_SPEC_37, ptr @PALLAS_SPEC_38, ptr @PALLAS_SPEC_39, ptr @PALLAS_SPEC_40, ptr @PALLAS_SPEC_41, ptr @PALLAS_SPEC_43, ptr @PALLAS_SPEC_42, ptr @PALLAS_SPEC_45, ptr @PALLAS_SPEC_46, ptr @PALLAS_SPEC_47, ptr @PALLAS_SPEC_48, ptr @PALLAS_SPEC_44, ptr @PALLAS_SPEC_49, ptr @PALLAS_SPEC_50, ptr @PALLAS_SPEC_51, ptr @PALLAS_SPEC_52, ptr @PALLAS_SPEC_53, ptr @PALLAS_SPEC_54, ptr @PALLAS_SPEC_55, ptr @PALLAS_SPEC_56, ptr @PALLAS_SPEC_57, ptr @PALLAS_SPEC_58, ptr @PALLAS_SPEC_59, ptr @PALLAS_SPEC_60, ptr @PALLAS_SPEC_61], section "llvm.metadata"
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
  call void @llvm.dbg.declare(metadata ptr %2, metadata !226, metadata !DIExpression()), !dbg !249
  %3 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !250
  store i32 0, ptr %3, align 4, !dbg !251
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !252
  store i32 0, ptr %4, align 4, !dbg !253
  ret void, !dbg !254
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_copy_struct_2(i64 %0) #0 !dbg !255 !pallas.fcontract !256 {
  %2 = alloca %struct.point, align 4
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !262, metadata !DIExpression()), !dbg !265
  %3 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !266
  store i32 0, ptr %3, align 4, !dbg !267
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !268
  store i32 0, ptr %4, align 4, !dbg !269
  ret void, !dbg !270
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @avr_x(ptr noundef %0) #0 !dbg !271 !pallas.fcontract !281 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !287, metadata !DIExpression()), !dbg !317
  %3 = load ptr, ptr %2, align 8, !dbg !318
  %4 = getelementptr inbounds %struct.triangle, ptr %3, i32 0, i32 0, !dbg !319
  %5 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 0, !dbg !320
  %6 = load i32, ptr %5, align 4, !dbg !320
  %7 = load ptr, ptr %2, align 8, !dbg !321
  %8 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 1, !dbg !322
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !323
  %10 = load i32, ptr %9, align 4, !dbg !323
  %11 = add nsw i32 %6, %10, !dbg !324
  %12 = load ptr, ptr %2, align 8, !dbg !325
  %13 = getelementptr inbounds %struct.triangle, ptr %12, i32 0, i32 2, !dbg !326
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !327
  %15 = load i32, ptr %14, align 4, !dbg !327
  %16 = add nsw i32 %11, %15, !dbg !328
  %17 = sdiv i32 %16, 3, !dbg !329
  ret i32 %17, !dbg !330
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @avr_x_pol(ptr noundef %0, i32 noundef %1) #0 !dbg !331 !pallas.fcontract !339 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !345, metadata !DIExpression()), !dbg !438
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !356, metadata !DIExpression()), !dbg !439
  call void @llvm.dbg.declare(metadata ptr %5, metadata !440, metadata !DIExpression()), !dbg !441
  store i32 0, ptr %5, align 4, !dbg !441
  call void @llvm.dbg.declare(metadata ptr %6, metadata !442, metadata !DIExpression()), !dbg !444
  store i32 0, ptr %6, align 4, !dbg !444
  br label %7, !dbg !445

7:                                                ; preds = %22, %2
  %8 = load i32, ptr %6, align 4, !dbg !446
  %9 = load i32, ptr %4, align 4, !dbg !448
  %10 = icmp slt i32 %8, %9, !dbg !449
  br i1 %10, label %11, label %25, !dbg !450

11:                                               ; preds = %7
  %12 = load ptr, ptr %3, align 8, !dbg !451
  %13 = getelementptr inbounds %struct.polygon, ptr %12, i32 0, i32 0, !dbg !453
  %14 = load ptr, ptr %13, align 8, !dbg !453
  %15 = load i32, ptr %6, align 4, !dbg !454
  %16 = sext i32 %15 to i64, !dbg !451
  %17 = getelementptr inbounds %struct.point, ptr %14, i64 %16, !dbg !451
  %18 = getelementptr inbounds %struct.point, ptr %17, i32 0, i32 0, !dbg !455
  %19 = load i32, ptr %18, align 4, !dbg !455
  %20 = load i32, ptr %5, align 4, !dbg !456
  %21 = add nsw i32 %20, %19, !dbg !456
  store i32 %21, ptr %5, align 4, !dbg !456
  br label %22, !dbg !457

22:                                               ; preds = %11
  %23 = load i32, ptr %6, align 4, !dbg !458
  %24 = add nsw i32 %23, 1, !dbg !458
  store i32 %24, ptr %6, align 4, !dbg !458
  br label %7, !dbg !459, !llvm.loop !460

25:                                               ; preds = %7
  %26 = load i32, ptr %5, align 4, !dbg !587
  %27 = load i32, ptr %4, align 4, !dbg !588
  %28 = sdiv i32 %26, %27, !dbg !589
  ret i32 %28, !dbg !590
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @main() #0 !dbg !591 {
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
  call void @llvm.dbg.declare(metadata ptr %2, metadata !594, metadata !DIExpression()), !dbg !595
  call void @llvm.dbg.declare(metadata ptr %3, metadata !596, metadata !DIExpression()), !dbg !597
  store ptr %2, ptr %3, align 8, !dbg !598
  %13 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !599, !pallas.stmntBlock !600
  store i32 1, ptr %13, align 4, !dbg !612
  %14 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !613
  store i32 2, ptr %14, align 4, !dbg !614
  %15 = load i64, ptr %2, align 4, !dbg !615, !pallas.stmntBlock !616
  call void @alter_copy_struct(i64 %15), !dbg !615
  %16 = load ptr, ptr %3, align 8, !dbg !634, !pallas.stmntBlock !635
  call void @alter_struct(ptr noundef %16), !dbg !653
  %17 = load ptr, ptr %3, align 8, !dbg !654, !pallas.stmntBlock !655
  call void @alter_struct_1(ptr noundef %17), !dbg !673
  call void @llvm.dbg.declare(metadata ptr %4, metadata !674, metadata !DIExpression()), !dbg !675
  call void @llvm.dbg.declare(metadata ptr %5, metadata !676, metadata !DIExpression()), !dbg !677
  call void @llvm.dbg.declare(metadata ptr %6, metadata !678, metadata !DIExpression()), !dbg !679
  %18 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 0, !dbg !680, !pallas.stmntBlock !681
  store i32 1, ptr %18, align 4, !dbg !699
  %19 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !700
  store i32 1, ptr %19, align 4, !dbg !701
  %20 = getelementptr inbounds %struct.point, ptr %5, i32 0, i32 0, !dbg !702
  store i32 2, ptr %20, align 4, !dbg !703
  %21 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !704
  store i32 2, ptr %21, align 4, !dbg !705
  %22 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !706
  store i32 3, ptr %22, align 4, !dbg !707
  %23 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !708
  store i32 3, ptr %23, align 4, !dbg !709
  call void @llvm.dbg.declare(metadata ptr %7, metadata !710, metadata !DIExpression()), !dbg !711
  call void @llvm.dbg.declare(metadata ptr %8, metadata !712, metadata !DIExpression()), !dbg !713
  store ptr %7, ptr %8, align 8, !dbg !714
  %24 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 0, !dbg !715
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %24, ptr align 4 %4, i64 8, i1 false), !dbg !716
  %25 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 1, !dbg !717
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %25, ptr align 4 %5, i64 8, i1 false), !dbg !718
  %26 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 2, !dbg !719
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %26, ptr align 4 %6, i64 8, i1 false), !dbg !720
  call void @llvm.dbg.declare(metadata ptr %9, metadata !721, metadata !DIExpression()), !dbg !725
  %27 = getelementptr inbounds [3 x %struct.point], ptr %9, i64 0, i64 0, !dbg !726, !pallas.stmntBlock !727
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 4 %4, i64 8, i1 false), !dbg !751
  %28 = getelementptr inbounds %struct.point, ptr %27, i64 1, !dbg !726
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %28, ptr align 4 %5, i64 8, i1 false), !dbg !752
  %29 = getelementptr inbounds %struct.point, ptr %28, i64 1, !dbg !726
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %29, ptr align 4 %6, i64 8, i1 false), !dbg !753
  call void @llvm.dbg.declare(metadata ptr %10, metadata !754, metadata !DIExpression()), !dbg !755
  call void @llvm.dbg.declare(metadata ptr %11, metadata !756, metadata !DIExpression()), !dbg !757
  store ptr %10, ptr %11, align 8, !dbg !758
  %30 = getelementptr inbounds [3 x %struct.point], ptr %9, i64 0, i64 0, !dbg !759
  %31 = getelementptr inbounds %struct.polygon, ptr %10, i32 0, i32 0, !dbg !760
  store ptr %30, ptr %31, align 8, !dbg !761
  call void @llvm.dbg.declare(metadata ptr %12, metadata !762, metadata !DIExpression()), !dbg !763
  %32 = load ptr, ptr %11, align 8, !dbg !764
  %33 = call i32 @avr_x_pol(ptr noundef %32, i32 noundef 3), !dbg !765
  store i32 %33, ptr %12, align 4, !dbg !763
  ret i32 0, !dbg !766, !pallas.stmntBlock !767
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #2

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !43 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !42, metadata !DIExpression()), !dbg !798
  %2 = icmp ne ptr %0, null, !dbg !799
  ret i1 %2, !dbg !798
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !58 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !57, metadata !DIExpression()), !dbg !800
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !801
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !802
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !803
  ret i1 %4, !dbg !800
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !64 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !63, metadata !DIExpression()), !dbg !804
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !805
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !806
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !807
  ret i1 %4, !dbg !804
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !70 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !69, metadata !DIExpression()), !dbg !808
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !809
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !810
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !811
  ret i1 %4, !dbg !808
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0) #0 !dbg !76 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !75, metadata !DIExpression()), !dbg !812
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !813
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !814
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !815
  ret i1 %4, !dbg !812
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0) #0 !dbg !82 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !81, metadata !DIExpression()), !dbg !816
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !817
  %3 = load i32, ptr %2, align 4, !dbg !817
  %4 = icmp eq i32 %3, 0, !dbg !818
  ret i1 %4, !dbg !816
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0) #0 !dbg !88 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !87, metadata !DIExpression()), !dbg !819
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !820
  %3 = load i32, ptr %2, align 4, !dbg !820
  %4 = icmp eq i32 %3, 0, !dbg !821
  ret i1 %4, !dbg !819
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0) #0 !dbg !94 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !93, metadata !DIExpression()), !dbg !822
  %2 = call ptr @"pallas.old ptr_noundef ptr"(ptr noundef %0), !dbg !823
  %3 = icmp eq ptr %2, %0, !dbg !824
  ret i1 %3, !dbg !822
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0) #0 !dbg !112 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !111, metadata !DIExpression()), !dbg !825
  %2 = icmp ne ptr %0, null, !dbg !826
  ret i1 %2, !dbg !825
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0) #0 !dbg !118 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !117, metadata !DIExpression()), !dbg !827
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !828
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !829
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !830
  ret i1 %4, !dbg !827
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0) #0 !dbg !124 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !123, metadata !DIExpression()), !dbg !831
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !832
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !833
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !834
  ret i1 %4, !dbg !831
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0) #0 !dbg !130 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !129, metadata !DIExpression()), !dbg !835
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !836
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !837
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !838
  ret i1 %4, !dbg !835
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0) #0 !dbg !136 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !135, metadata !DIExpression()), !dbg !839
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !840
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !841
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !842
  ret i1 %4, !dbg !839
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0) #0 !dbg !142 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !141, metadata !DIExpression()), !dbg !843
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !844
  %3 = load i32, ptr %2, align 4, !dbg !844
  %4 = icmp eq i32 %3, 0, !dbg !845
  ret i1 %4, !dbg !843
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0) #0 !dbg !148 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !147, metadata !DIExpression()), !dbg !846
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !847
  %3 = load i32, ptr %2, align 4, !dbg !847
  %4 = icmp eq i32 %3, 0, !dbg !848
  ret i1 %4, !dbg !846
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0) #0 !dbg !154 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !153, metadata !DIExpression()), !dbg !849
  %2 = call ptr @"pallas.old ptr_noundef ptr"(ptr noundef %0), !dbg !850
  %3 = icmp eq ptr %2, %0, !dbg !851
  ret i1 %3, !dbg !849
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(ptr noundef %0) #0 !dbg !172 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !171, metadata !DIExpression()), !dbg !852
  %2 = icmp ne ptr %0, null, !dbg !853
  ret i1 %2, !dbg !852
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(ptr noundef %0) #0 !dbg !178 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !177, metadata !DIExpression()), !dbg !854
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !855
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !856
  ret i1 %3, !dbg !854
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(ptr noundef %0) #0 !dbg !184 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !183, metadata !DIExpression()), !dbg !857
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !858
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !859
  ret i1 %3, !dbg !857
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_19(ptr noundef %0) #0 !dbg !190 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !189, metadata !DIExpression()), !dbg !860
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !861
  %3 = load i32, ptr %2, align 4, !dbg !861
  %4 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !862
  %5 = load i32, ptr %4, align 4, !dbg !862
  %6 = add nsw i32 %5, 1, !dbg !863
  %7 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %6), !dbg !864
  %8 = icmp eq i32 %3, %7, !dbg !865
  ret i1 %8, !dbg !860
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_20(ptr noundef %0) #0 !dbg !196 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !195, metadata !DIExpression()), !dbg !866
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !867
  %3 = load i32, ptr %2, align 4, !dbg !867
  %4 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !868
  %5 = load i32, ptr %4, align 4, !dbg !868
  %6 = add nsw i32 %5, 1, !dbg !869
  %7 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %6), !dbg !870
  %8 = icmp eq i32 %3, %7, !dbg !871
  ret i1 %8, !dbg !866
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_21(ptr noundef %0) #0 !dbg !202 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !201, metadata !DIExpression()), !dbg !872
  %2 = call ptr @"pallas.old ptr_noundef ptr"(ptr noundef %0), !dbg !873
  %3 = icmp eq ptr %2, %0, !dbg !874
  ret i1 %3, !dbg !872
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_22(i64 %0) #0 !dbg !228 !pallas.exprWrapper !797 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !227, metadata !DIExpression()), !dbg !875
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !876
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !877
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !878
  ret i1 %5, !dbg !875
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_23(i64 %0) #0 !dbg !236 !pallas.exprWrapper !797 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !235, metadata !DIExpression()), !dbg !879
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !880
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !881
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !882
  ret i1 %5, !dbg !879
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_24(i64 %0) #0 !dbg !242 !pallas.exprWrapper !797 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !241, metadata !DIExpression()), !dbg !883
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !884
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !885
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !886
  ret i1 %5, !dbg !883
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_25(i64 %0) #0 !dbg !248 !pallas.exprWrapper !797 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !247, metadata !DIExpression()), !dbg !887
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !888
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !889
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !890
  ret i1 %5, !dbg !887
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_26(i64 %0) #0 !dbg !264 !pallas.exprWrapper !797 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !263, metadata !DIExpression()), !dbg !891
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !892
  %4 = call i1 @pallas.perm(ptr noundef %2, ptr noundef byval(%pallas.fracT) %3), !dbg !893
  ret i1 %4, !dbg !891
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_27(ptr noundef %0) #0 !dbg !289 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !288, metadata !DIExpression()), !dbg !894
  %2 = icmp ne ptr %0, null, !dbg !895
  ret i1 %2, !dbg !894
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_28(ptr noundef %0) #0 !dbg !304 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !303, metadata !DIExpression()), !dbg !896
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !897
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !898
  ret i1 %3, !dbg !896
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_29(ptr noundef %0) #0 !dbg !310 !pallas.exprWrapper !797 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !309, metadata !DIExpression()), !dbg !899
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !900
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !901
  ret i1 %3, !dbg !899
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_30(ptr noundef %0) #0 !dbg !316 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !315, metadata !DIExpression()), !dbg !902
  %2 = call i32 @"pallas.result i32"(), !dbg !903
  %3 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 0, !dbg !904
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !905
  %5 = load i32, ptr %4, align 4, !dbg !905
  %6 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 1, !dbg !906
  %7 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !907
  %8 = load i32, ptr %7, align 4, !dbg !907
  %9 = add nsw i32 %5, %8, !dbg !908
  %10 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 2, !dbg !909
  %11 = getelementptr inbounds %struct.point, ptr %10, i32 0, i32 0, !dbg !910
  %12 = load i32, ptr %11, align 4, !dbg !910
  %13 = add nsw i32 %9, %12, !dbg !911
  %14 = sdiv i32 %13, 3, !dbg !912
  %15 = icmp eq i32 %2, %14, !dbg !913
  ret i1 %15, !dbg !902
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_31(ptr noundef %0, i32 noundef %1) #0 !dbg !347 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !346, metadata !DIExpression()), !dbg !914
  call void @llvm.dbg.value(metadata i32 %1, metadata !357, metadata !DIExpression()), !dbg !914
  %3 = icmp sgt i32 %1, 0, !dbg !915
  ret i1 %3, !dbg !914
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_32(ptr noundef %0, i32 noundef %1) #0 !dbg !363 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !362, metadata !DIExpression()), !dbg !916
  call void @llvm.dbg.value(metadata i32 %1, metadata !365, metadata !DIExpression()), !dbg !916
  %3 = icmp ne ptr %0, null, !dbg !917
  ret i1 %3, !dbg !916
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_33(ptr noundef %0, i32 noundef %1) #0 !dbg !371 !pallas.exprWrapper !797 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !370, metadata !DIExpression()), !dbg !918
  call void @llvm.dbg.value(metadata i32 %1, metadata !373, metadata !DIExpression()), !dbg !918
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !919
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !920
  ret i1 %4, !dbg !918
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_34(ptr noundef %0, i32 noundef %1) #0 !dbg !379 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !378, metadata !DIExpression()), !dbg !921
  call void @llvm.dbg.value(metadata i32 %1, metadata !381, metadata !DIExpression()), !dbg !921
  %3 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !922
  %4 = load ptr, ptr %3, align 8, !dbg !922
  %5 = icmp ne ptr %4, null, !dbg !923
  br i1 %5, label %6, label %12, !dbg !924

6:                                                ; preds = %2
  %7 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !925
  %8 = load ptr, ptr %7, align 8, !dbg !925
  %9 = call i64 @pallas.ptrLength(ptr noundef %8), !dbg !926
  %10 = sext i32 %1 to i64, !dbg !927
  %11 = icmp sge i64 %9, %10, !dbg !928
  br label %12

12:                                               ; preds = %6, %2
  %13 = phi i1 [ false, %2 ], [ %11, %6 ], !dbg !921
  ret i1 %13, !dbg !921
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_35(ptr noundef %0, i32 noundef %1) #0 !dbg !387 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !386, metadata !DIExpression()), !dbg !929
  call void @llvm.dbg.value(metadata i32 %1, metadata !389, metadata !DIExpression()), !dbg !929
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !930
  %4 = icmp sle i32 0, %3, !dbg !931
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !932
  %6 = icmp slt i32 %5, %1, !dbg !933
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !934
  %8 = icmp sle i32 0, %7, !dbg !935
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !936
  %10 = icmp slt i32 %9, %1, !dbg !937
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !938
  %12 = call i1 @pallas.scAnd(i1 %6, i1 %11), !dbg !939
  %13 = call i1 @pallas.scAnd(i1 %4, i1 %12), !dbg !940
  %14 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !941
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !942
  %16 = icmp ne i32 %14, %15, !dbg !943
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !944
  %18 = load ptr, ptr %17, align 8, !dbg !944
  %19 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !945
  %20 = sext i32 %19 to i64, !dbg !946
  %21 = getelementptr inbounds %struct.point, ptr %18, i64 %20, !dbg !946
  %22 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !947
  %23 = load ptr, ptr %22, align 8, !dbg !947
  %24 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !948
  %25 = sext i32 %24 to i64, !dbg !949
  %26 = getelementptr inbounds %struct.point, ptr %23, i64 %25, !dbg !949
  %27 = icmp ne ptr %21, %26, !dbg !950
  %28 = call i1 @pallas.imply(i1 %16, i1 %27), !dbg !951
  %29 = call i1 @pallas.forall(i1 %13, i1 %28), !dbg !952
  ret i1 %29, !dbg !929
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_36(ptr noundef %0, i32 noundef %1) #0 !dbg !395 !pallas.exprWrapper !797 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !394, metadata !DIExpression()), !dbg !953
  call void @llvm.dbg.value(metadata i32 %1, metadata !397, metadata !DIExpression()), !dbg !953
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !954
  %5 = icmp sle i32 0, %4, !dbg !955
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !956
  %7 = icmp slt i32 %6, %1, !dbg !957
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !958
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !959
  %10 = load ptr, ptr %9, align 8, !dbg !959
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !960
  %12 = sext i32 %11 to i64, !dbg !961
  %13 = getelementptr inbounds %struct.point, ptr %10, i64 %12, !dbg !961
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !962
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %3), !dbg !963
  %15 = call i1 @pallas.forallSep(i1 %8, i1 %14), !dbg !964
  ret i1 %15, !dbg !953
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_37(ptr noundef %0, i32 noundef %1) #0 !dbg !403 !pallas.exprWrapper !797 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !402, metadata !DIExpression()), !dbg !965
  call void @llvm.dbg.value(metadata i32 %1, metadata !405, metadata !DIExpression()), !dbg !965
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !966
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !967
  ret i1 %4, !dbg !965
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_38(ptr noundef %0, i32 noundef %1) #0 !dbg !411 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !410, metadata !DIExpression()), !dbg !968
  call void @llvm.dbg.value(metadata i32 %1, metadata !413, metadata !DIExpression()), !dbg !968
  %3 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !969
  %4 = load ptr, ptr %3, align 8, !dbg !969
  %5 = icmp ne ptr %4, null, !dbg !970
  br i1 %5, label %6, label %12, !dbg !971

6:                                                ; preds = %2
  %7 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !972
  %8 = load ptr, ptr %7, align 8, !dbg !972
  %9 = call i64 @pallas.ptrLength(ptr noundef %8), !dbg !973
  %10 = sext i32 %1 to i64, !dbg !974
  %11 = icmp sge i64 %9, %10, !dbg !975
  br label %12

12:                                               ; preds = %6, %2
  %13 = phi i1 [ false, %2 ], [ %11, %6 ], !dbg !968
  ret i1 %13, !dbg !968
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_39(ptr noundef %0, i32 noundef %1) #0 !dbg !419 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !418, metadata !DIExpression()), !dbg !976
  call void @llvm.dbg.value(metadata i32 %1, metadata !421, metadata !DIExpression()), !dbg !976
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !977
  %4 = icmp sle i32 0, %3, !dbg !978
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !979
  %6 = icmp slt i32 %5, %1, !dbg !980
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !981
  %8 = icmp sle i32 0, %7, !dbg !982
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !983
  %10 = icmp slt i32 %9, %1, !dbg !984
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !985
  %12 = call i1 @pallas.scAnd(i1 %6, i1 %11), !dbg !986
  %13 = call i1 @pallas.scAnd(i1 %4, i1 %12), !dbg !987
  %14 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !988
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !989
  %16 = icmp ne i32 %14, %15, !dbg !990
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !991
  %18 = load ptr, ptr %17, align 8, !dbg !991
  %19 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !992
  %20 = sext i32 %19 to i64, !dbg !993
  %21 = getelementptr inbounds %struct.point, ptr %18, i64 %20, !dbg !993
  %22 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !994
  %23 = load ptr, ptr %22, align 8, !dbg !994
  %24 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !995
  %25 = sext i32 %24 to i64, !dbg !996
  %26 = getelementptr inbounds %struct.point, ptr %23, i64 %25, !dbg !996
  %27 = icmp ne ptr %21, %26, !dbg !997
  %28 = call i1 @pallas.imply(i1 %16, i1 %27), !dbg !998
  %29 = call i1 @pallas.forall(i1 %13, i1 %28), !dbg !999
  ret i1 %29, !dbg !976
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_40(ptr noundef %0, i32 noundef %1) #0 !dbg !427 !pallas.exprWrapper !797 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !426, metadata !DIExpression()), !dbg !1000
  call void @llvm.dbg.value(metadata i32 %1, metadata !429, metadata !DIExpression()), !dbg !1000
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1001
  %5 = icmp sle i32 0, %4, !dbg !1002
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1003
  %7 = icmp slt i32 %6, %1, !dbg !1004
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !1005
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1006
  %10 = load ptr, ptr %9, align 8, !dbg !1006
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1007
  %12 = sext i32 %11 to i64, !dbg !1008
  %13 = getelementptr inbounds %struct.point, ptr %10, i64 %12, !dbg !1008
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !1009
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %3), !dbg !1010
  %15 = call i1 @pallas.forallSep(i1 %8, i1 %14), !dbg !1011
  ret i1 %15, !dbg !1000
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_41(ptr noundef %0, i32 noundef %1) #0 !dbg !435 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !434, metadata !DIExpression()), !dbg !1012
  call void @llvm.dbg.value(metadata i32 %1, metadata !437, metadata !DIExpression()), !dbg !1012
  %3 = icmp eq i32 %1, 3, !dbg !1013
  %4 = call i32 @"pallas.result i32"(), !dbg !1014
  %5 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1015
  %6 = load ptr, ptr %5, align 8, !dbg !1015
  %7 = getelementptr inbounds %struct.point, ptr %6, i64 0, !dbg !1016
  %8 = getelementptr inbounds %struct.point, ptr %7, i32 0, i32 0, !dbg !1017
  %9 = load i32, ptr %8, align 4, !dbg !1017
  %10 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1018
  %11 = load ptr, ptr %10, align 8, !dbg !1018
  %12 = getelementptr inbounds %struct.point, ptr %11, i64 1, !dbg !1019
  %13 = getelementptr inbounds %struct.point, ptr %12, i32 0, i32 0, !dbg !1020
  %14 = load i32, ptr %13, align 4, !dbg !1020
  %15 = add nsw i32 %9, %14, !dbg !1021
  %16 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1022
  %17 = load ptr, ptr %16, align 8, !dbg !1022
  %18 = getelementptr inbounds %struct.point, ptr %17, i64 2, !dbg !1023
  %19 = getelementptr inbounds %struct.point, ptr %18, i32 0, i32 0, !dbg !1024
  %20 = load i32, ptr %19, align 4, !dbg !1024
  %21 = add nsw i32 %15, %20, !dbg !1025
  %22 = sdiv i32 %21, %1, !dbg !1026
  %23 = icmp eq i32 %4, %22, !dbg !1027
  %24 = call i1 @pallas.imply(i1 %3, i1 %23), !dbg !1028
  ret i1 %24, !dbg !1012
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_43(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !484 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !483, metadata !DIExpression()), !dbg !1029
  call void @llvm.dbg.value(metadata i32 %1, metadata !486, metadata !DIExpression()), !dbg !1029
  call void @llvm.dbg.value(metadata i32 %2, metadata !488, metadata !DIExpression()), !dbg !1029
  call void @llvm.dbg.value(metadata i32 %3, metadata !490, metadata !DIExpression()), !dbg !1029
  %5 = icmp ne ptr %0, null, !dbg !1030
  ret i1 %5, !dbg !1029
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_42(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !470 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !469, metadata !DIExpression()), !dbg !1031
  call void @llvm.dbg.value(metadata i32 %1, metadata !474, metadata !DIExpression()), !dbg !1031
  call void @llvm.dbg.value(metadata i32 %2, metadata !476, metadata !DIExpression()), !dbg !1031
  call void @llvm.dbg.value(metadata i32 %3, metadata !478, metadata !DIExpression()), !dbg !1031
  %5 = icmp sle i32 0, %3, !dbg !1032
  br i1 %5, label %6, label %8, !dbg !1033

6:                                                ; preds = %4
  %7 = icmp sle i32 %3, %1, !dbg !1034
  br label %8

8:                                                ; preds = %6, %4
  %9 = phi i1 [ false, %4 ], [ %7, %6 ], !dbg !1031
  ret i1 %9, !dbg !1031
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_45(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !508 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !507, metadata !DIExpression()), !dbg !1035
  call void @llvm.dbg.value(metadata i32 %1, metadata !510, metadata !DIExpression()), !dbg !1035
  call void @llvm.dbg.value(metadata i32 %2, metadata !512, metadata !DIExpression()), !dbg !1035
  call void @llvm.dbg.value(metadata i32 %3, metadata !514, metadata !DIExpression()), !dbg !1035
  %5 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1036
  %6 = load ptr, ptr %5, align 8, !dbg !1036
  %7 = icmp ne ptr %6, null, !dbg !1037
  br i1 %7, label %8, label %14, !dbg !1038

8:                                                ; preds = %4
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1039
  %10 = load ptr, ptr %9, align 8, !dbg !1039
  %11 = call i64 @pallas.ptrLength(ptr noundef %10), !dbg !1040
  %12 = sext i32 %1 to i64, !dbg !1041
  %13 = icmp sge i64 %11, %12, !dbg !1042
  br label %14

14:                                               ; preds = %8, %4
  %15 = phi i1 [ false, %4 ], [ %13, %8 ], !dbg !1035
  ret i1 %15, !dbg !1035
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_46(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !520 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !519, metadata !DIExpression()), !dbg !1043
  call void @llvm.dbg.value(metadata i32 %1, metadata !522, metadata !DIExpression()), !dbg !1043
  call void @llvm.dbg.value(metadata i32 %2, metadata !524, metadata !DIExpression()), !dbg !1043
  call void @llvm.dbg.value(metadata i32 %3, metadata !526, metadata !DIExpression()), !dbg !1043
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1044
  %6 = icmp sle i32 0, %5, !dbg !1045
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1046
  %8 = icmp slt i32 %7, %1, !dbg !1047
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1048
  %10 = icmp sle i32 0, %9, !dbg !1049
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1050
  %12 = icmp slt i32 %11, %1, !dbg !1051
  %13 = call i1 @pallas.scAnd(i1 %10, i1 %12), !dbg !1052
  %14 = call i1 @pallas.scAnd(i1 %8, i1 %13), !dbg !1053
  %15 = call i1 @pallas.scAnd(i1 %6, i1 %14), !dbg !1054
  %16 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1055
  %17 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1056
  %18 = icmp ne i32 %16, %17, !dbg !1057
  %19 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1058
  %20 = load ptr, ptr %19, align 8, !dbg !1058
  %21 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1059
  %22 = sext i32 %21 to i64, !dbg !1060
  %23 = getelementptr inbounds %struct.point, ptr %20, i64 %22, !dbg !1060
  %24 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1061
  %25 = load ptr, ptr %24, align 8, !dbg !1061
  %26 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1062
  %27 = sext i32 %26 to i64, !dbg !1063
  %28 = getelementptr inbounds %struct.point, ptr %25, i64 %27, !dbg !1063
  %29 = icmp ne ptr %23, %28, !dbg !1064
  %30 = call i1 @pallas.imply(i1 %18, i1 %29), !dbg !1065
  %31 = call i1 @pallas.forall(i1 %15, i1 %30), !dbg !1066
  ret i1 %31, !dbg !1043
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_47(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !532 !pallas.exprWrapper !797 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !531, metadata !DIExpression()), !dbg !1067
  call void @llvm.dbg.value(metadata i32 %1, metadata !534, metadata !DIExpression()), !dbg !1067
  call void @llvm.dbg.value(metadata i32 %2, metadata !536, metadata !DIExpression()), !dbg !1067
  call void @llvm.dbg.value(metadata i32 %3, metadata !538, metadata !DIExpression()), !dbg !1067
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1068
  %7 = icmp sle i32 0, %6, !dbg !1069
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1070
  %9 = icmp slt i32 %8, %1, !dbg !1071
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !1072
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1073
  %12 = load ptr, ptr %11, align 8, !dbg !1073
  %13 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1074
  %14 = sext i32 %13 to i64, !dbg !1075
  %15 = getelementptr inbounds %struct.point, ptr %12, i64 %14, !dbg !1075
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !1076
  %16 = call i1 @pallas.perm(ptr noundef %15, ptr noundef byval(%pallas.fracT) %5), !dbg !1077
  %17 = call i1 @pallas.forallSep(i1 %10, i1 %16), !dbg !1078
  ret i1 %17, !dbg !1067
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_48(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !544 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !543, metadata !DIExpression()), !dbg !1079
  call void @llvm.dbg.value(metadata i32 %1, metadata !546, metadata !DIExpression()), !dbg !1079
  call void @llvm.dbg.value(metadata i32 %2, metadata !548, metadata !DIExpression()), !dbg !1079
  call void @llvm.dbg.value(metadata i32 %3, metadata !550, metadata !DIExpression()), !dbg !1079
  %5 = icmp eq i32 %3, 0, !dbg !1080
  %6 = icmp eq i32 %2, 0, !dbg !1081
  %7 = call i1 @pallas.imply(i1 %5, i1 %6), !dbg !1082
  ret i1 %7, !dbg !1079
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_44(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !496 !pallas.exprWrapper !797 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !495, metadata !DIExpression()), !dbg !1083
  call void @llvm.dbg.value(metadata i32 %1, metadata !498, metadata !DIExpression()), !dbg !1083
  call void @llvm.dbg.value(metadata i32 %2, metadata !500, metadata !DIExpression()), !dbg !1083
  call void @llvm.dbg.value(metadata i32 %3, metadata !502, metadata !DIExpression()), !dbg !1083
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !1084
  %6 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %5), !dbg !1085
  ret i1 %6, !dbg !1083
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_49(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !556 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !555, metadata !DIExpression()), !dbg !1086
  call void @llvm.dbg.value(metadata i32 %1, metadata !558, metadata !DIExpression()), !dbg !1086
  call void @llvm.dbg.value(metadata i32 %2, metadata !560, metadata !DIExpression()), !dbg !1086
  call void @llvm.dbg.value(metadata i32 %3, metadata !562, metadata !DIExpression()), !dbg !1086
  %5 = icmp eq i32 %3, 1, !dbg !1087
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1088
  %7 = load ptr, ptr %6, align 8, !dbg !1088
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !1089
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !1090
  %10 = load i32, ptr %9, align 4, !dbg !1090
  %11 = icmp eq i32 %2, %10, !dbg !1091
  %12 = call i1 @pallas.imply(i1 %5, i1 %11), !dbg !1092
  ret i1 %12, !dbg !1086
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_50(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !568 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !567, metadata !DIExpression()), !dbg !1093
  call void @llvm.dbg.value(metadata i32 %1, metadata !570, metadata !DIExpression()), !dbg !1093
  call void @llvm.dbg.value(metadata i32 %2, metadata !572, metadata !DIExpression()), !dbg !1093
  call void @llvm.dbg.value(metadata i32 %3, metadata !574, metadata !DIExpression()), !dbg !1093
  %5 = icmp eq i32 %3, 2, !dbg !1094
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1095
  %7 = load ptr, ptr %6, align 8, !dbg !1095
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !1096
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !1097
  %10 = load i32, ptr %9, align 4, !dbg !1097
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1098
  %12 = load ptr, ptr %11, align 8, !dbg !1098
  %13 = getelementptr inbounds %struct.point, ptr %12, i64 1, !dbg !1099
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !1100
  %15 = load i32, ptr %14, align 4, !dbg !1100
  %16 = add nsw i32 %10, %15, !dbg !1101
  %17 = icmp eq i32 %2, %16, !dbg !1102
  %18 = call i1 @pallas.imply(i1 %5, i1 %17), !dbg !1103
  ret i1 %18, !dbg !1093
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_51(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !580 !pallas.exprWrapper !797 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !579, metadata !DIExpression()), !dbg !1104
  call void @llvm.dbg.value(metadata i32 %1, metadata !582, metadata !DIExpression()), !dbg !1104
  call void @llvm.dbg.value(metadata i32 %2, metadata !584, metadata !DIExpression()), !dbg !1104
  call void @llvm.dbg.value(metadata i32 %3, metadata !586, metadata !DIExpression()), !dbg !1104
  %5 = icmp eq i32 %3, 3, !dbg !1105
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1106
  %7 = load ptr, ptr %6, align 8, !dbg !1106
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !1107
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !1108
  %10 = load i32, ptr %9, align 4, !dbg !1108
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1109
  %12 = load ptr, ptr %11, align 8, !dbg !1109
  %13 = getelementptr inbounds %struct.point, ptr %12, i64 1, !dbg !1110
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !1111
  %15 = load i32, ptr %14, align 4, !dbg !1111
  %16 = add nsw i32 %10, %15, !dbg !1112
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1113
  %18 = load ptr, ptr %17, align 8, !dbg !1113
  %19 = getelementptr inbounds %struct.point, ptr %18, i64 2, !dbg !1114
  %20 = getelementptr inbounds %struct.point, ptr %19, i32 0, i32 0, !dbg !1115
  %21 = load i32, ptr %20, align 4, !dbg !1115
  %22 = add nsw i32 %16, %21, !dbg !1116
  %23 = icmp eq i32 %2, %22, !dbg !1117
  %24 = call i1 @pallas.imply(i1 %5, i1 %23), !dbg !1118
  ret i1 %24, !dbg !1104
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_52(i64 %0, ptr noundef %1) #0 !dbg !607 !pallas.exprWrapper !797 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !606, metadata !DIExpression()), !dbg !1119
  call void @llvm.dbg.value(metadata ptr %1, metadata !611, metadata !DIExpression()), !dbg !1119
  %4 = icmp ne ptr %1, null, !dbg !1120
  ret i1 %4, !dbg !1119
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_53(i64 %0, ptr noundef %1) #0 !dbg !623 !pallas.exprWrapper !797 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !622, metadata !DIExpression()), !dbg !1121
  call void @llvm.dbg.value(metadata ptr %1, metadata !625, metadata !DIExpression()), !dbg !1121
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 0, !dbg !1122
  %5 = load i32, ptr %4, align 4, !dbg !1122
  %6 = icmp eq i32 %5, 1, !dbg !1123
  ret i1 %6, !dbg !1121
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_54(i64 %0, ptr noundef %1) #0 !dbg !631 !pallas.exprWrapper !797 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !630, metadata !DIExpression()), !dbg !1124
  call void @llvm.dbg.value(metadata ptr %1, metadata !633, metadata !DIExpression()), !dbg !1124
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 1, !dbg !1125
  %5 = load i32, ptr %4, align 4, !dbg !1125
  %6 = icmp eq i32 %5, 2, !dbg !1126
  ret i1 %6, !dbg !1124
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_55(i64 %0, ptr noundef %1) #0 !dbg !642 !pallas.exprWrapper !797 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !641, metadata !DIExpression()), !dbg !1127
  call void @llvm.dbg.value(metadata ptr %1, metadata !644, metadata !DIExpression()), !dbg !1127
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !1128
  %5 = load i32, ptr %4, align 4, !dbg !1128
  %6 = icmp eq i32 %5, 1, !dbg !1129
  ret i1 %6, !dbg !1127
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_56(i64 %0, ptr noundef %1) #0 !dbg !650 !pallas.exprWrapper !797 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !649, metadata !DIExpression()), !dbg !1130
  call void @llvm.dbg.value(metadata ptr %1, metadata !652, metadata !DIExpression()), !dbg !1130
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 1, !dbg !1131
  %5 = load i32, ptr %4, align 4, !dbg !1131
  %6 = icmp eq i32 %5, 2, !dbg !1132
  ret i1 %6, !dbg !1130
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_57(i64 %0, ptr noundef %1) #0 !dbg !662 !pallas.exprWrapper !797 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !661, metadata !DIExpression()), !dbg !1133
  call void @llvm.dbg.value(metadata ptr %1, metadata !664, metadata !DIExpression()), !dbg !1133
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 0, !dbg !1134
  %5 = load i32, ptr %4, align 4, !dbg !1134
  %6 = icmp eq i32 %5, 0, !dbg !1135
  ret i1 %6, !dbg !1133
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_58(i64 %0, ptr noundef %1) #0 !dbg !670 !pallas.exprWrapper !797 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !669, metadata !DIExpression()), !dbg !1136
  call void @llvm.dbg.value(metadata ptr %1, metadata !672, metadata !DIExpression()), !dbg !1136
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !1137
  %5 = load i32, ptr %4, align 4, !dbg !1137
  %6 = icmp eq i32 %5, 0, !dbg !1138
  ret i1 %6, !dbg !1136
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_59(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4) #0 !dbg !688 !pallas.exprWrapper !797 {
  %6 = alloca %struct.point, align 4
  %7 = alloca %struct.point, align 4
  %8 = alloca %struct.point, align 4
  %9 = alloca %struct.point, align 4
  store i64 %0, ptr %6, align 4
  store i64 %2, ptr %7, align 4
  store i64 %3, ptr %8, align 4
  store i64 %4, ptr %9, align 4
  call void @llvm.dbg.declare(metadata ptr %6, metadata !687, metadata !DIExpression()), !dbg !1139
  call void @llvm.dbg.value(metadata ptr %1, metadata !692, metadata !DIExpression()), !dbg !1139
  call void @llvm.dbg.declare(metadata ptr %7, metadata !694, metadata !DIExpression()), !dbg !1139
  call void @llvm.dbg.declare(metadata ptr %8, metadata !696, metadata !DIExpression()), !dbg !1139
  call void @llvm.dbg.declare(metadata ptr %9, metadata !698, metadata !DIExpression()), !dbg !1139
  %10 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !1140
  %11 = load i32, ptr %10, align 4, !dbg !1140
  %12 = icmp eq i32 %11, 1, !dbg !1141
  br i1 %12, label %13, label %17, !dbg !1142

13:                                               ; preds = %5
  %14 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 1, !dbg !1143
  %15 = load i32, ptr %14, align 4, !dbg !1143
  %16 = icmp eq i32 %15, 1, !dbg !1144
  br label %17

17:                                               ; preds = %13, %5
  %18 = phi i1 [ false, %5 ], [ %16, %13 ], !dbg !1139
  ret i1 %18, !dbg !1139
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_60(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4, ptr noundef byval(%struct.triangle) align 8 %5, ptr noundef %6, ptr noundef %7) #0 !dbg !734 !pallas.exprWrapper !797 {
  %9 = alloca %struct.point, align 4
  %10 = alloca %struct.point, align 4
  %11 = alloca %struct.point, align 4
  %12 = alloca %struct.point, align 4
  store i64 %0, ptr %9, align 4
  store i64 %2, ptr %10, align 4
  store i64 %3, ptr %11, align 4
  store i64 %4, ptr %12, align 4
  call void @llvm.dbg.declare(metadata ptr %9, metadata !733, metadata !DIExpression()), !dbg !1145
  call void @llvm.dbg.value(metadata ptr %1, metadata !738, metadata !DIExpression()), !dbg !1145
  call void @llvm.dbg.declare(metadata ptr %10, metadata !740, metadata !DIExpression()), !dbg !1145
  call void @llvm.dbg.declare(metadata ptr %11, metadata !742, metadata !DIExpression()), !dbg !1145
  call void @llvm.dbg.declare(metadata ptr %12, metadata !744, metadata !DIExpression()), !dbg !1145
  call void @llvm.dbg.declare(metadata ptr %5, metadata !746, metadata !DIExpression()), !dbg !1145
  call void @llvm.dbg.value(metadata ptr %6, metadata !748, metadata !DIExpression()), !dbg !1145
  call void @llvm.dbg.value(metadata ptr %7, metadata !750, metadata !DIExpression()), !dbg !1145
  %13 = call i32 @avr_x(ptr noundef %6), !dbg !1146
  %14 = icmp eq i32 %13, 2, !dbg !1147
  ret i1 %14, !dbg !1145
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_61(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4, ptr noundef byval(%struct.triangle) align 8 %5, ptr noundef %6, ptr noundef %7, i64 %8, ptr noundef %9, i32 noundef %10) #0 !dbg !774 !pallas.exprWrapper !797 {
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
  call void @llvm.dbg.declare(metadata ptr %12, metadata !773, metadata !DIExpression()), !dbg !1148
  call void @llvm.dbg.value(metadata ptr %1, metadata !778, metadata !DIExpression()), !dbg !1148
  call void @llvm.dbg.declare(metadata ptr %13, metadata !780, metadata !DIExpression()), !dbg !1148
  call void @llvm.dbg.declare(metadata ptr %14, metadata !782, metadata !DIExpression()), !dbg !1148
  call void @llvm.dbg.declare(metadata ptr %15, metadata !784, metadata !DIExpression()), !dbg !1148
  call void @llvm.dbg.declare(metadata ptr %5, metadata !786, metadata !DIExpression()), !dbg !1148
  call void @llvm.dbg.value(metadata ptr %6, metadata !788, metadata !DIExpression()), !dbg !1148
  call void @llvm.dbg.value(metadata ptr %7, metadata !790, metadata !DIExpression()), !dbg !1148
  call void @llvm.dbg.declare(metadata ptr %16, metadata !792, metadata !DIExpression()), !dbg !1148
  call void @llvm.dbg.value(metadata ptr %9, metadata !794, metadata !DIExpression()), !dbg !1148
  call void @llvm.dbg.value(metadata i32 %10, metadata !796, metadata !DIExpression()), !dbg !1148
  %19 = icmp eq i32 %10, 2, !dbg !1149
  ret i1 %19, !dbg !1148
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !1150 i32 @"pallas.old i32_noundef i32"(i32 noundef)

declare !pallas.specLib !1150 ptr @"pallas.old ptr_noundef ptr"(ptr noundef)

declare !pallas.specLib !1151 i32 @"pallas.result i32"()

declare !pallas.specLib !1152 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !1153 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !1154 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !1155 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !1156 i32 @"pallas.boundVar i32"(ptr)

declare !pallas.specLib !1157 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !1158 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !1159 i1 @pallas.imply(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }

!llvm.dbg.cu = !{!9, !11}
!llvm.module.flags = !{!15, !16, !17, !18, !19, !20, !21}
!llvm.ident = !{!22, !22}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 427, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "ca2da618bcb8e2258570e80a095c4f3a")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 427, type: !3, isLocal: true, isDefinition: true)
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !10, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!10 = !DIFile(filename: "examples/concepts/llvm/structs.c", directory: ".", checksumkind: CSK_MD5, checksum: "e8ddae5173bf602971eb4d88519ee05b")
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
!36 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/structs.c", directory: "", checksumkind: CSK_MD5, checksum: "e8ddae5173bf602971eb4d88519ee05b")
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
!217 = distinct !DISubprogram(name: "alter_copy_struct", scope: !10, file: !10, line: 75, type: !218, scopeLine: 75, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!218 = !DISubroutineType(types: !219)
!219 = !{null, !27}
!220 = !{!221, i1 false, i1 false, !33, !33, !222, !231, !237, !243}
!221 = !{!"pallas.srcLoc", i64 69, i64 1, i64 74, i64 1, !36}
!222 = !{!"pallas.requires", !223, ptr @PALLAS_SPEC_22, !33, !33, !224}
!223 = !{!"pallas.srcLoc", i64 70, i64 3, i64 70, i64 38, !36}
!224 = !{!225}
!225 = !{!226, !227}
!226 = !DILocalVariable(name: "p", arg: 1, scope: !217, file: !10, line: 75, type: !27)
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
!237 = !{!"pallas.ensures", !238, ptr @PALLAS_SPEC_24, !33, !33, !239}
!238 = !{!"pallas.srcLoc", i64 72, i64 3, i64 72, i64 37, !36}
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
!249 = !DILocation(line: 75, column: 30, scope: !217)
!250 = !DILocation(line: 76, column: 7, scope: !217)
!251 = !DILocation(line: 76, column: 9, scope: !217)
!252 = !DILocation(line: 77, column: 7, scope: !217)
!253 = !DILocation(line: 77, column: 9, scope: !217)
!254 = !DILocation(line: 78, column: 1, scope: !217)
!255 = distinct !DISubprogram(name: "alter_copy_struct_2", scope: !10, file: !10, line: 83, type: !218, scopeLine: 83, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!256 = !{!257, i1 false, i1 false, !33, !33, !258}
!257 = !{!"pallas.srcLoc", i64 80, i64 1, i64 82, i64 1, !36}
!258 = !{!"pallas.requires", !259, ptr @PALLAS_SPEC_26, !33, !33, !260}
!259 = !{!"pallas.srcLoc", i64 81, i64 3, i64 81, i64 29, !36}
!260 = !{!261}
!261 = !{!262, !263}
!262 = !DILocalVariable(name: "p", arg: 1, scope: !255, file: !10, line: 83, type: !27)
!263 = !DILocalVariable(name: "p", arg: 1, scope: !264, file: !10, line: 81, type: !48)
!264 = distinct !DISubprogram(name: "PALLAS_SPEC_26", scope: !10, file: !10, line: 81, type: !229, scopeLine: 81, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!265 = !DILocation(line: 83, column: 32, scope: !255)
!266 = !DILocation(line: 84, column: 7, scope: !255)
!267 = !DILocation(line: 84, column: 9, scope: !255)
!268 = !DILocation(line: 85, column: 7, scope: !255)
!269 = !DILocation(line: 85, column: 9, scope: !255)
!270 = !DILocation(line: 86, column: 1, scope: !255)
!271 = distinct !DISubprogram(name: "avr_x", scope: !10, file: !10, line: 94, type: !272, scopeLine: 94, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!272 = !DISubroutineType(types: !273)
!273 = !{!31, !274}
!274 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !275, size: 64)
!275 = !DIDerivedType(tag: DW_TAG_typedef, name: "triangle", file: !10, line: 11, baseType: !276)
!276 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "triangle", file: !10, line: 9, size: 192, elements: !277)
!277 = !{!278, !279, !280}
!278 = !DIDerivedType(tag: DW_TAG_member, name: "p1", scope: !276, file: !10, line: 10, baseType: !27, size: 64)
!279 = !DIDerivedType(tag: DW_TAG_member, name: "p2", scope: !276, file: !10, line: 10, baseType: !27, size: 64, offset: 64)
!280 = !DIDerivedType(tag: DW_TAG_member, name: "p3", scope: !276, file: !10, line: 10, baseType: !27, size: 64, offset: 128)
!281 = !{!282, i1 false, i1 false, !33, !33, !283, !299, !305, !311}
!282 = !{!"pallas.srcLoc", i64 88, i64 1, i64 93, i64 1, !36}
!283 = !{!"pallas.requires", !284, ptr @PALLAS_SPEC_27, !33, !33, !285}
!284 = !{!"pallas.srcLoc", i64 89, i64 3, i64 89, i64 21, !36}
!285 = !{!286}
!286 = !{!287, !288}
!287 = !DILocalVariable(name: "r", arg: 1, scope: !271, file: !10, line: 94, type: !274)
!288 = !DILocalVariable(name: "r", arg: 1, scope: !289, file: !10, line: 89, type: !292)
!289 = distinct !DISubprogram(name: "PALLAS_SPEC_27", scope: !10, file: !10, line: 89, type: !290, scopeLine: 89, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!290 = !DISubroutineType(types: !291)
!291 = !{!46, !292}
!292 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !293, size: 64)
!293 = !DIDerivedType(tag: DW_TAG_typedef, name: "triangle", file: !2, line: 12, baseType: !294)
!294 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "triangle", file: !2, line: 10, size: 192, elements: !295)
!295 = !{!296, !297, !298}
!296 = !DIDerivedType(tag: DW_TAG_member, name: "p1", scope: !294, file: !2, line: 11, baseType: !48, size: 64)
!297 = !DIDerivedType(tag: DW_TAG_member, name: "p2", scope: !294, file: !2, line: 11, baseType: !48, size: 64, offset: 64)
!298 = !DIDerivedType(tag: DW_TAG_member, name: "p3", scope: !294, file: !2, line: 11, baseType: !48, size: 64, offset: 128)
!299 = !{!"pallas.requires", !300, ptr @PALLAS_SPEC_28, !33, !33, !301}
!300 = !{!"pallas.srcLoc", i64 90, i64 3, i64 90, i64 37, !36}
!301 = !{!302}
!302 = !{!287, !303}
!303 = !DILocalVariable(name: "r", arg: 1, scope: !304, file: !10, line: 90, type: !292)
!304 = distinct !DISubprogram(name: "PALLAS_SPEC_28", scope: !10, file: !10, line: 90, type: !290, scopeLine: 90, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!305 = !{!"pallas.ensures", !306, ptr @PALLAS_SPEC_29, !33, !33, !307}
!306 = !{!"pallas.srcLoc", i64 91, i64 3, i64 91, i64 36, !36}
!307 = !{!308}
!308 = !{!287, !309}
!309 = !DILocalVariable(name: "r", arg: 1, scope: !310, file: !10, line: 91, type: !292)
!310 = distinct !DISubprogram(name: "PALLAS_SPEC_29", scope: !10, file: !10, line: 91, type: !290, scopeLine: 91, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!311 = !{!"pallas.ensures", !312, ptr @PALLAS_SPEC_30, !33, !33, !313}
!312 = !{!"pallas.srcLoc", i64 92, i64 3, i64 92, i64 58, !36}
!313 = !{!314}
!314 = !{!287, !315}
!315 = !DILocalVariable(name: "r", arg: 1, scope: !316, file: !10, line: 92, type: !292)
!316 = distinct !DISubprogram(name: "PALLAS_SPEC_30", scope: !10, file: !10, line: 92, type: !290, scopeLine: 92, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!317 = !DILocation(line: 94, column: 21, scope: !271)
!318 = !DILocation(line: 95, column: 13, scope: !271)
!319 = !DILocation(line: 95, column: 16, scope: !271)
!320 = !DILocation(line: 95, column: 19, scope: !271)
!321 = !DILocation(line: 95, column: 23, scope: !271)
!322 = !DILocation(line: 95, column: 26, scope: !271)
!323 = !DILocation(line: 95, column: 29, scope: !271)
!324 = !DILocation(line: 95, column: 21, scope: !271)
!325 = !DILocation(line: 95, column: 33, scope: !271)
!326 = !DILocation(line: 95, column: 36, scope: !271)
!327 = !DILocation(line: 95, column: 39, scope: !271)
!328 = !DILocation(line: 95, column: 31, scope: !271)
!329 = !DILocation(line: 95, column: 41, scope: !271)
!330 = !DILocation(line: 95, column: 5, scope: !271)
!331 = distinct !DISubprogram(name: "avr_x_pol", scope: !10, file: !10, line: 111, type: !332, scopeLine: 111, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!332 = !DISubroutineType(types: !333)
!333 = !{!31, !334, !31}
!334 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !335, size: 64)
!335 = !DIDerivedType(tag: DW_TAG_typedef, name: "polygon", file: !10, line: 15, baseType: !336)
!336 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "polygon", file: !10, line: 13, size: 64, elements: !337)
!337 = !{!338}
!338 = !DIDerivedType(tag: DW_TAG_member, name: "ps", scope: !336, file: !10, line: 14, baseType: !26, size: 64)
!339 = !{!340, i1 false, i1 false, !33, !33, !341, !358, !366, !374, !382, !390, !398, !406, !414, !422, !430}
!340 = !{!"pallas.srcLoc", i64 98, i64 1, i64 110, i64 1, !36}
!341 = !{!"pallas.requires", !342, ptr @PALLAS_SPEC_31, !33, !33, !343}
!342 = !{!"pallas.srcLoc", i64 99, i64 3, i64 99, i64 19, !36}
!343 = !{!344, !355}
!344 = !{!345, !346}
!345 = !DILocalVariable(name: "p", arg: 1, scope: !331, file: !10, line: 111, type: !334)
!346 = !DILocalVariable(name: "p", arg: 1, scope: !347, file: !10, line: 99, type: !350)
!347 = distinct !DISubprogram(name: "PALLAS_SPEC_31", scope: !10, file: !10, line: 99, type: !348, scopeLine: 99, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!348 = !DISubroutineType(types: !349)
!349 = !{!46, !350, !31}
!350 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !351, size: 64)
!351 = !DIDerivedType(tag: DW_TAG_typedef, name: "polygon", file: !2, line: 16, baseType: !352)
!352 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "polygon", file: !2, line: 14, size: 64, elements: !353)
!353 = !{!354}
!354 = !DIDerivedType(tag: DW_TAG_member, name: "ps", scope: !352, file: !2, line: 15, baseType: !47, size: 64)
!355 = !{!356, !357}
!356 = !DILocalVariable(name: "len", arg: 2, scope: !331, file: !10, line: 111, type: !31)
!357 = !DILocalVariable(name: "len", arg: 2, scope: !347, file: !10, line: 99, type: !31)
!358 = !{!"pallas.requires", !359, ptr @PALLAS_SPEC_32, !33, !33, !360}
!359 = !{!"pallas.srcLoc", i64 100, i64 3, i64 100, i64 21, !36}
!360 = !{!361, !364}
!361 = !{!345, !362}
!362 = !DILocalVariable(name: "p", arg: 1, scope: !363, file: !10, line: 100, type: !350)
!363 = distinct !DISubprogram(name: "PALLAS_SPEC_32", scope: !10, file: !10, line: 100, type: !348, scopeLine: 100, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!364 = !{!356, !365}
!365 = !DILocalVariable(name: "len", arg: 2, scope: !363, file: !10, line: 100, type: !31)
!366 = !{!"pallas.requires", !367, ptr @PALLAS_SPEC_33, !33, !33, !368}
!367 = !{!"pallas.srcLoc", i64 101, i64 3, i64 101, i64 37, !36}
!368 = !{!369, !372}
!369 = !{!345, !370}
!370 = !DILocalVariable(name: "p", arg: 1, scope: !371, file: !10, line: 101, type: !350)
!371 = distinct !DISubprogram(name: "PALLAS_SPEC_33", scope: !10, file: !10, line: 101, type: !348, scopeLine: 101, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!372 = !{!356, !373}
!373 = !DILocalVariable(name: "len", arg: 2, scope: !371, file: !10, line: 101, type: !31)
!374 = !{!"pallas.requires", !375, ptr @PALLAS_SPEC_34, !33, !33, !376}
!375 = !{!"pallas.srcLoc", i64 102, i64 3, i64 102, i64 54, !36}
!376 = !{!377, !380}
!377 = !{!345, !378}
!378 = !DILocalVariable(name: "p", arg: 1, scope: !379, file: !10, line: 102, type: !350)
!379 = distinct !DISubprogram(name: "PALLAS_SPEC_34", scope: !10, file: !10, line: 102, type: !348, scopeLine: 102, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!380 = !{!356, !381}
!381 = !DILocalVariable(name: "len", arg: 2, scope: !379, file: !10, line: 102, type: !31)
!382 = !{!"pallas.requires", !383, ptr @PALLAS_SPEC_35, !33, !33, !384}
!383 = !{!"pallas.srcLoc", i64 103, i64 3, i64 103, i64 191, !36}
!384 = !{!385, !388}
!385 = !{!345, !386}
!386 = !DILocalVariable(name: "p", arg: 1, scope: !387, file: !10, line: 103, type: !350)
!387 = distinct !DISubprogram(name: "PALLAS_SPEC_35", scope: !10, file: !10, line: 103, type: !348, scopeLine: 103, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!388 = !{!356, !389}
!389 = !DILocalVariable(name: "len", arg: 2, scope: !387, file: !10, line: 103, type: !31)
!390 = !{!"pallas.requires", !391, ptr @PALLAS_SPEC_36, !33, !33, !392}
!391 = !{!"pallas.srcLoc", i64 104, i64 3, i64 104, i64 106, !36}
!392 = !{!393, !396}
!393 = !{!345, !394}
!394 = !DILocalVariable(name: "p", arg: 1, scope: !395, file: !10, line: 104, type: !350)
!395 = distinct !DISubprogram(name: "PALLAS_SPEC_36", scope: !10, file: !10, line: 104, type: !348, scopeLine: 104, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!396 = !{!356, !397}
!397 = !DILocalVariable(name: "len", arg: 2, scope: !395, file: !10, line: 104, type: !31)
!398 = !{!"pallas.ensures", !399, ptr @PALLAS_SPEC_37, !33, !33, !400}
!399 = !{!"pallas.srcLoc", i64 105, i64 3, i64 105, i64 36, !36}
!400 = !{!401, !404}
!401 = !{!345, !402}
!402 = !DILocalVariable(name: "p", arg: 1, scope: !403, file: !10, line: 105, type: !350)
!403 = distinct !DISubprogram(name: "PALLAS_SPEC_37", scope: !10, file: !10, line: 105, type: !348, scopeLine: 105, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!404 = !{!356, !405}
!405 = !DILocalVariable(name: "len", arg: 2, scope: !403, file: !10, line: 105, type: !31)
!406 = !{!"pallas.ensures", !407, ptr @PALLAS_SPEC_38, !33, !33, !408}
!407 = !{!"pallas.srcLoc", i64 106, i64 3, i64 106, i64 53, !36}
!408 = !{!409, !412}
!409 = !{!345, !410}
!410 = !DILocalVariable(name: "p", arg: 1, scope: !411, file: !10, line: 106, type: !350)
!411 = distinct !DISubprogram(name: "PALLAS_SPEC_38", scope: !10, file: !10, line: 106, type: !348, scopeLine: 106, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!412 = !{!356, !413}
!413 = !DILocalVariable(name: "len", arg: 2, scope: !411, file: !10, line: 106, type: !31)
!414 = !{!"pallas.ensures", !415, ptr @PALLAS_SPEC_39, !33, !33, !416}
!415 = !{!"pallas.srcLoc", i64 107, i64 3, i64 107, i64 190, !36}
!416 = !{!417, !420}
!417 = !{!345, !418}
!418 = !DILocalVariable(name: "p", arg: 1, scope: !419, file: !10, line: 107, type: !350)
!419 = distinct !DISubprogram(name: "PALLAS_SPEC_39", scope: !10, file: !10, line: 107, type: !348, scopeLine: 107, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!420 = !{!356, !421}
!421 = !DILocalVariable(name: "len", arg: 2, scope: !419, file: !10, line: 107, type: !31)
!422 = !{!"pallas.ensures", !423, ptr @PALLAS_SPEC_40, !33, !33, !424}
!423 = !{!"pallas.srcLoc", i64 108, i64 3, i64 108, i64 105, !36}
!424 = !{!425, !428}
!425 = !{!345, !426}
!426 = !DILocalVariable(name: "p", arg: 1, scope: !427, file: !10, line: 108, type: !350)
!427 = distinct !DISubprogram(name: "PALLAS_SPEC_40", scope: !10, file: !10, line: 108, type: !348, scopeLine: 108, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!428 = !{!356, !429}
!429 = !DILocalVariable(name: "len", arg: 2, scope: !427, file: !10, line: 108, type: !31)
!430 = !{!"pallas.ensures", !431, ptr @PALLAS_SPEC_41, !33, !33, !432}
!431 = !{!"pallas.srcLoc", i64 109, i64 3, i64 109, i64 87, !36}
!432 = !{!433, !436}
!433 = !{!345, !434}
!434 = !DILocalVariable(name: "p", arg: 1, scope: !435, file: !10, line: 109, type: !350)
!435 = distinct !DISubprogram(name: "PALLAS_SPEC_41", scope: !10, file: !10, line: 109, type: !348, scopeLine: 109, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!436 = !{!356, !437}
!437 = !DILocalVariable(name: "len", arg: 2, scope: !435, file: !10, line: 109, type: !31)
!438 = !DILocation(line: 111, column: 24, scope: !331)
!439 = !DILocation(line: 111, column: 31, scope: !331)
!440 = !DILocalVariable(name: "sum", scope: !331, file: !10, line: 112, type: !31)
!441 = !DILocation(line: 112, column: 9, scope: !331)
!442 = !DILocalVariable(name: "i", scope: !443, file: !10, line: 125, type: !31)
!443 = distinct !DILexicalBlock(scope: !331, file: !10, line: 125, column: 5)
!444 = !DILocation(line: 125, column: 13, scope: !443)
!445 = !DILocation(line: 125, column: 9, scope: !443)
!446 = !DILocation(line: 125, column: 18, scope: !447)
!447 = distinct !DILexicalBlock(scope: !443, file: !10, line: 125, column: 5)
!448 = !DILocation(line: 125, column: 20, scope: !447)
!449 = !DILocation(line: 125, column: 19, scope: !447)
!450 = !DILocation(line: 125, column: 5, scope: !443)
!451 = !DILocation(line: 126, column: 16, scope: !452)
!452 = distinct !DILexicalBlock(scope: !447, file: !10, line: 125, column: 29)
!453 = !DILocation(line: 126, column: 19, scope: !452)
!454 = !DILocation(line: 126, column: 22, scope: !452)
!455 = !DILocation(line: 126, column: 25, scope: !452)
!456 = !DILocation(line: 126, column: 13, scope: !452)
!457 = !DILocation(line: 127, column: 5, scope: !452)
!458 = !DILocation(line: 125, column: 26, scope: !447)
!459 = !DILocation(line: 125, column: 5, scope: !447)
!460 = distinct !{!460, !450, !461, !462, !463}
!461 = !DILocation(line: 127, column: 5, scope: !443)
!462 = !{!"llvm.loop.mustprogress"}
!463 = !{!"pallas.loopInvBlock", !464, !465, !479, !491, !503, !515, !527, !539, !551, !563, !575}
!464 = !{!"pallas.srcLoc", i64 113, i64 5, i64 124, i64 5, !36}
!465 = !{!"pallas.loopInv", !466, ptr @PALLAS_SPEC_42, !33, !33, !467}
!466 = !{!"pallas.srcLoc", i64 114, i64 7, i64 114, i64 36, !36}
!467 = !{!468, !473, !475, !477}
!468 = !{!345, !469}
!469 = !DILocalVariable(name: "p", arg: 1, scope: !470, file: !10, line: 114, type: !350)
!470 = distinct !DISubprogram(name: "PALLAS_SPEC_42", scope: !10, file: !10, line: 114, type: !471, scopeLine: 114, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!471 = !DISubroutineType(types: !472)
!472 = !{!46, !350, !31, !31, !31}
!473 = !{!356, !474}
!474 = !DILocalVariable(name: "len", arg: 2, scope: !470, file: !10, line: 114, type: !31)
!475 = !{!440, !476}
!476 = !DILocalVariable(name: "sum", arg: 3, scope: !470, file: !10, line: 114, type: !31)
!477 = !{!442, !478}
!478 = !DILocalVariable(name: "i", arg: 4, scope: !470, file: !10, line: 114, type: !31)
!479 = !{!"pallas.loopInv", !480, ptr @PALLAS_SPEC_43, !33, !33, !481}
!480 = !{!"pallas.srcLoc", i64 115, i64 7, i64 115, i64 31, !36}
!481 = !{!482, !485, !487, !489}
!482 = !{!345, !483}
!483 = !DILocalVariable(name: "p", arg: 1, scope: !484, file: !10, line: 115, type: !350)
!484 = distinct !DISubprogram(name: "PALLAS_SPEC_43", scope: !10, file: !10, line: 115, type: !471, scopeLine: 115, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!485 = !{!356, !486}
!486 = !DILocalVariable(name: "len", arg: 2, scope: !484, file: !10, line: 115, type: !31)
!487 = !{!440, !488}
!488 = !DILocalVariable(name: "sum", arg: 3, scope: !484, file: !10, line: 115, type: !31)
!489 = !{!442, !490}
!490 = !DILocalVariable(name: "i", arg: 4, scope: !484, file: !10, line: 115, type: !31)
!491 = !{!"pallas.loopInv", !492, ptr @PALLAS_SPEC_44, !33, !33, !493}
!492 = !{!"pallas.srcLoc", i64 116, i64 7, i64 116, i64 47, !36}
!493 = !{!494, !497, !499, !501}
!494 = !{!345, !495}
!495 = !DILocalVariable(name: "p", arg: 1, scope: !496, file: !10, line: 116, type: !350)
!496 = distinct !DISubprogram(name: "PALLAS_SPEC_44", scope: !10, file: !10, line: 116, type: !471, scopeLine: 116, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!497 = !{!356, !498}
!498 = !DILocalVariable(name: "len", arg: 2, scope: !496, file: !10, line: 116, type: !31)
!499 = !{!440, !500}
!500 = !DILocalVariable(name: "sum", arg: 3, scope: !496, file: !10, line: 116, type: !31)
!501 = !{!442, !502}
!502 = !DILocalVariable(name: "i", arg: 4, scope: !496, file: !10, line: 116, type: !31)
!503 = !{!"pallas.loopInv", !504, ptr @PALLAS_SPEC_45, !33, !33, !505}
!504 = !{!"pallas.srcLoc", i64 117, i64 7, i64 117, i64 64, !36}
!505 = !{!506, !509, !511, !513}
!506 = !{!345, !507}
!507 = !DILocalVariable(name: "p", arg: 1, scope: !508, file: !10, line: 117, type: !350)
!508 = distinct !DISubprogram(name: "PALLAS_SPEC_45", scope: !10, file: !10, line: 117, type: !471, scopeLine: 117, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!509 = !{!356, !510}
!510 = !DILocalVariable(name: "len", arg: 2, scope: !508, file: !10, line: 117, type: !31)
!511 = !{!440, !512}
!512 = !DILocalVariable(name: "sum", arg: 3, scope: !508, file: !10, line: 117, type: !31)
!513 = !{!442, !514}
!514 = !DILocalVariable(name: "i", arg: 4, scope: !508, file: !10, line: 117, type: !31)
!515 = !{!"pallas.loopInv", !516, ptr @PALLAS_SPEC_46, !33, !33, !517}
!516 = !{!"pallas.srcLoc", i64 118, i64 7, i64 118, i64 201, !36}
!517 = !{!518, !521, !523, !525}
!518 = !{!345, !519}
!519 = !DILocalVariable(name: "p", arg: 1, scope: !520, file: !10, line: 118, type: !350)
!520 = distinct !DISubprogram(name: "PALLAS_SPEC_46", scope: !10, file: !10, line: 118, type: !471, scopeLine: 118, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!521 = !{!356, !522}
!522 = !DILocalVariable(name: "len", arg: 2, scope: !520, file: !10, line: 118, type: !31)
!523 = !{!440, !524}
!524 = !DILocalVariable(name: "sum", arg: 3, scope: !520, file: !10, line: 118, type: !31)
!525 = !{!442, !526}
!526 = !DILocalVariable(name: "i", arg: 4, scope: !520, file: !10, line: 118, type: !31)
!527 = !{!"pallas.loopInv", !528, ptr @PALLAS_SPEC_47, !33, !33, !529}
!528 = !{!"pallas.srcLoc", i64 119, i64 7, i64 119, i64 116, !36}
!529 = !{!530, !533, !535, !537}
!530 = !{!345, !531}
!531 = !DILocalVariable(name: "p", arg: 1, scope: !532, file: !10, line: 119, type: !350)
!532 = distinct !DISubprogram(name: "PALLAS_SPEC_47", scope: !10, file: !10, line: 119, type: !471, scopeLine: 119, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!533 = !{!356, !534}
!534 = !DILocalVariable(name: "len", arg: 2, scope: !532, file: !10, line: 119, type: !31)
!535 = !{!440, !536}
!536 = !DILocalVariable(name: "sum", arg: 3, scope: !532, file: !10, line: 119, type: !31)
!537 = !{!442, !538}
!538 = !DILocalVariable(name: "i", arg: 4, scope: !532, file: !10, line: 119, type: !31)
!539 = !{!"pallas.loopInv", !540, ptr @PALLAS_SPEC_48, !33, !33, !541}
!540 = !{!"pallas.srcLoc", i64 120, i64 7, i64 120, i64 48, !36}
!541 = !{!542, !545, !547, !549}
!542 = !{!345, !543}
!543 = !DILocalVariable(name: "p", arg: 1, scope: !544, file: !10, line: 120, type: !350)
!544 = distinct !DISubprogram(name: "PALLAS_SPEC_48", scope: !10, file: !10, line: 120, type: !471, scopeLine: 120, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!545 = !{!356, !546}
!546 = !DILocalVariable(name: "len", arg: 2, scope: !544, file: !10, line: 120, type: !31)
!547 = !{!440, !548}
!548 = !DILocalVariable(name: "sum", arg: 3, scope: !544, file: !10, line: 120, type: !31)
!549 = !{!442, !550}
!550 = !DILocalVariable(name: "i", arg: 4, scope: !544, file: !10, line: 120, type: !31)
!551 = !{!"pallas.loopInv", !552, ptr @PALLAS_SPEC_49, !33, !33, !553}
!552 = !{!"pallas.srcLoc", i64 121, i64 7, i64 121, i64 57, !36}
!553 = !{!554, !557, !559, !561}
!554 = !{!345, !555}
!555 = !DILocalVariable(name: "p", arg: 1, scope: !556, file: !10, line: 121, type: !350)
!556 = distinct !DISubprogram(name: "PALLAS_SPEC_49", scope: !10, file: !10, line: 121, type: !471, scopeLine: 121, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!557 = !{!356, !558}
!558 = !DILocalVariable(name: "len", arg: 2, scope: !556, file: !10, line: 121, type: !31)
!559 = !{!440, !560}
!560 = !DILocalVariable(name: "sum", arg: 3, scope: !556, file: !10, line: 121, type: !31)
!561 = !{!442, !562}
!562 = !DILocalVariable(name: "i", arg: 4, scope: !556, file: !10, line: 121, type: !31)
!563 = !{!"pallas.loopInv", !564, ptr @PALLAS_SPEC_50, !33, !33, !565}
!564 = !{!"pallas.srcLoc", i64 122, i64 7, i64 122, i64 70, !36}
!565 = !{!566, !569, !571, !573}
!566 = !{!345, !567}
!567 = !DILocalVariable(name: "p", arg: 1, scope: !568, file: !10, line: 122, type: !350)
!568 = distinct !DISubprogram(name: "PALLAS_SPEC_50", scope: !10, file: !10, line: 122, type: !471, scopeLine: 122, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!569 = !{!356, !570}
!570 = !DILocalVariable(name: "len", arg: 2, scope: !568, file: !10, line: 122, type: !31)
!571 = !{!440, !572}
!572 = !DILocalVariable(name: "sum", arg: 3, scope: !568, file: !10, line: 122, type: !31)
!573 = !{!442, !574}
!574 = !DILocalVariable(name: "i", arg: 4, scope: !568, file: !10, line: 122, type: !31)
!575 = !{!"pallas.loopInv", !576, ptr @PALLAS_SPEC_51, !33, !33, !577}
!576 = !{!"pallas.srcLoc", i64 123, i64 7, i64 123, i64 83, !36}
!577 = !{!578, !581, !583, !585}
!578 = !{!345, !579}
!579 = !DILocalVariable(name: "p", arg: 1, scope: !580, file: !10, line: 123, type: !350)
!580 = distinct !DISubprogram(name: "PALLAS_SPEC_51", scope: !10, file: !10, line: 123, type: !471, scopeLine: 123, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!581 = !{!356, !582}
!582 = !DILocalVariable(name: "len", arg: 2, scope: !580, file: !10, line: 123, type: !31)
!583 = !{!440, !584}
!584 = !DILocalVariable(name: "sum", arg: 3, scope: !580, file: !10, line: 123, type: !31)
!585 = !{!442, !586}
!586 = !DILocalVariable(name: "i", arg: 4, scope: !580, file: !10, line: 123, type: !31)
!587 = !DILocation(line: 129, column: 12, scope: !331)
!588 = !DILocation(line: 129, column: 16, scope: !331)
!589 = !DILocation(line: 129, column: 15, scope: !331)
!590 = !DILocation(line: 129, column: 5, scope: !331)
!591 = distinct !DISubprogram(name: "main", scope: !10, file: !10, line: 133, type: !592, scopeLine: 133, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!592 = !DISubroutineType(types: !593)
!593 = !{!31}
!594 = !DILocalVariable(name: "p", scope: !591, file: !10, line: 134, type: !27)
!595 = !DILocation(line: 134, column: 11, scope: !591)
!596 = !DILocalVariable(name: "pp", scope: !591, file: !10, line: 135, type: !26)
!597 = !DILocation(line: 135, column: 12, scope: !591)
!598 = !DILocation(line: 136, column: 8, scope: !591)
!599 = !DILocation(line: 140, column: 7, scope: !591)
!600 = !{!601, !602}
!601 = !{!"pallas.srcLoc", i64 138, i64 5, i64 138, i64 29, !36}
!602 = !{!"pallas.assert", !603, ptr @PALLAS_SPEC_52, !33, !33, !604}
!603 = !{!"pallas.srcLoc", i64 138, i64 9, i64 138, i64 27, !36}
!604 = !{!605, !610}
!605 = !{!594, !606}
!606 = !DILocalVariable(name: "p", arg: 1, scope: !607, file: !10, line: 138, type: !48)
!607 = distinct !DISubprogram(name: "PALLAS_SPEC_52", scope: !10, file: !10, line: 138, type: !608, scopeLine: 138, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!608 = !DISubroutineType(types: !609)
!609 = !{!46, !48, !47}
!610 = !{!596, !611}
!611 = !DILocalVariable(name: "pp", arg: 2, scope: !607, file: !10, line: 138, type: !47)
!612 = !DILocation(line: 140, column: 9, scope: !591)
!613 = !DILocation(line: 141, column: 7, scope: !591)
!614 = !DILocation(line: 141, column: 9, scope: !591)
!615 = !DILocation(line: 144, column: 5, scope: !591)
!616 = !{!617, !618, !626}
!617 = !{!"pallas.srcLoc", i64 142, i64 5, i64 143, i64 24, !36}
!618 = !{!"pallas.assert", !619, ptr @PALLAS_SPEC_53, !33, !33, !620}
!619 = !{!"pallas.srcLoc", i64 142, i64 9, i64 142, i64 26, !36}
!620 = !{!621, !624}
!621 = !{!594, !622}
!622 = !DILocalVariable(name: "p", arg: 1, scope: !623, file: !10, line: 142, type: !48)
!623 = distinct !DISubprogram(name: "PALLAS_SPEC_53", scope: !10, file: !10, line: 142, type: !608, scopeLine: 142, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!624 = !{!596, !625}
!625 = !DILocalVariable(name: "pp", arg: 2, scope: !623, file: !10, line: 142, type: !47)
!626 = !{!"pallas.assert", !627, ptr @PALLAS_SPEC_54, !33, !33, !628}
!627 = !{!"pallas.srcLoc", i64 143, i64 5, i64 143, i64 22, !36}
!628 = !{!629, !632}
!629 = !{!594, !630}
!630 = !DILocalVariable(name: "p", arg: 1, scope: !631, file: !10, line: 143, type: !48)
!631 = distinct !DISubprogram(name: "PALLAS_SPEC_54", scope: !10, file: !10, line: 143, type: !608, scopeLine: 143, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!632 = !{!596, !633}
!633 = !DILocalVariable(name: "pp", arg: 2, scope: !631, file: !10, line: 143, type: !47)
!634 = !DILocation(line: 148, column: 18, scope: !591)
!635 = !{!636, !637, !645}
!636 = !{!"pallas.srcLoc", i64 145, i64 5, i64 146, i64 22, !36}
!637 = !{!"pallas.assert", !638, ptr @PALLAS_SPEC_55, !33, !33, !639}
!638 = !{!"pallas.srcLoc", i64 145, i64 9, i64 145, i64 24, !36}
!639 = !{!640, !643}
!640 = !{!594, !641}
!641 = !DILocalVariable(name: "p", arg: 1, scope: !642, file: !10, line: 145, type: !48)
!642 = distinct !DISubprogram(name: "PALLAS_SPEC_55", scope: !10, file: !10, line: 145, type: !608, scopeLine: 145, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!643 = !{!596, !644}
!644 = !DILocalVariable(name: "pp", arg: 2, scope: !642, file: !10, line: 145, type: !47)
!645 = !{!"pallas.assert", !646, ptr @PALLAS_SPEC_56, !33, !33, !647}
!646 = !{!"pallas.srcLoc", i64 146, i64 5, i64 146, i64 20, !36}
!647 = !{!648, !651}
!648 = !{!594, !649}
!649 = !DILocalVariable(name: "p", arg: 1, scope: !650, file: !10, line: 146, type: !48)
!650 = distinct !DISubprogram(name: "PALLAS_SPEC_56", scope: !10, file: !10, line: 146, type: !608, scopeLine: 146, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!651 = !{!596, !652}
!652 = !DILocalVariable(name: "pp", arg: 2, scope: !650, file: !10, line: 146, type: !47)
!653 = !DILocation(line: 148, column: 5, scope: !591)
!654 = !DILocation(line: 151, column: 20, scope: !591)
!655 = !{!656, !657, !665}
!656 = !{!"pallas.srcLoc", i64 149, i64 5, i64 150, i64 22, !36}
!657 = !{!"pallas.assert", !658, ptr @PALLAS_SPEC_57, !33, !33, !659}
!658 = !{!"pallas.srcLoc", i64 149, i64 9, i64 149, i64 26, !36}
!659 = !{!660, !663}
!660 = !{!594, !661}
!661 = !DILocalVariable(name: "p", arg: 1, scope: !662, file: !10, line: 149, type: !48)
!662 = distinct !DISubprogram(name: "PALLAS_SPEC_57", scope: !10, file: !10, line: 149, type: !608, scopeLine: 149, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!663 = !{!596, !664}
!664 = !DILocalVariable(name: "pp", arg: 2, scope: !662, file: !10, line: 149, type: !47)
!665 = !{!"pallas.assert", !666, ptr @PALLAS_SPEC_58, !33, !33, !667}
!666 = !{!"pallas.srcLoc", i64 150, i64 5, i64 150, i64 20, !36}
!667 = !{!668, !671}
!668 = !{!594, !669}
!669 = !DILocalVariable(name: "p", arg: 1, scope: !670, file: !10, line: 150, type: !48)
!670 = distinct !DISubprogram(name: "PALLAS_SPEC_58", scope: !10, file: !10, line: 150, type: !608, scopeLine: 150, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!671 = !{!596, !672}
!672 = !DILocalVariable(name: "pp", arg: 2, scope: !670, file: !10, line: 150, type: !47)
!673 = !DILocation(line: 151, column: 5, scope: !591)
!674 = !DILocalVariable(name: "p1", scope: !591, file: !10, line: 154, type: !27)
!675 = !DILocation(line: 154, column: 11, scope: !591)
!676 = !DILocalVariable(name: "p2", scope: !591, file: !10, line: 154, type: !27)
!677 = !DILocation(line: 154, column: 15, scope: !591)
!678 = !DILocalVariable(name: "p3", scope: !591, file: !10, line: 154, type: !27)
!679 = !DILocation(line: 154, column: 19, scope: !591)
!680 = !DILocation(line: 155, column: 8, scope: !591)
!681 = !{!682, !683}
!682 = !{!"pallas.srcLoc", i64 152, i64 5, i64 152, i64 38, !36}
!683 = !{!"pallas.assert", !684, ptr @PALLAS_SPEC_59, !33, !33, !685}
!684 = !{!"pallas.srcLoc", i64 152, i64 9, i64 152, i64 36, !36}
!685 = !{!686, !691, !693, !695, !697}
!686 = !{!594, !687}
!687 = !DILocalVariable(name: "p", arg: 1, scope: !688, file: !10, line: 152, type: !48)
!688 = distinct !DISubprogram(name: "PALLAS_SPEC_59", scope: !10, file: !10, line: 152, type: !689, scopeLine: 152, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!689 = !DISubroutineType(types: !690)
!690 = !{!46, !48, !47, !48, !48, !48}
!691 = !{!596, !692}
!692 = !DILocalVariable(name: "pp", arg: 2, scope: !688, file: !10, line: 152, type: !47)
!693 = !{!674, !694}
!694 = !DILocalVariable(name: "p1", arg: 3, scope: !688, file: !10, line: 152, type: !48)
!695 = !{!676, !696}
!696 = !DILocalVariable(name: "p2", arg: 4, scope: !688, file: !10, line: 152, type: !48)
!697 = !{!678, !698}
!698 = !DILocalVariable(name: "p3", arg: 5, scope: !688, file: !10, line: 152, type: !48)
!699 = !DILocation(line: 155, column: 10, scope: !591)
!700 = !DILocation(line: 155, column: 18, scope: !591)
!701 = !DILocation(line: 155, column: 20, scope: !591)
!702 = !DILocation(line: 156, column: 8, scope: !591)
!703 = !DILocation(line: 156, column: 10, scope: !591)
!704 = !DILocation(line: 156, column: 18, scope: !591)
!705 = !DILocation(line: 156, column: 20, scope: !591)
!706 = !DILocation(line: 157, column: 8, scope: !591)
!707 = !DILocation(line: 157, column: 10, scope: !591)
!708 = !DILocation(line: 157, column: 18, scope: !591)
!709 = !DILocation(line: 157, column: 20, scope: !591)
!710 = !DILocalVariable(name: "r", scope: !591, file: !10, line: 158, type: !275)
!711 = !DILocation(line: 158, column: 14, scope: !591)
!712 = !DILocalVariable(name: "rr", scope: !591, file: !10, line: 158, type: !274)
!713 = !DILocation(line: 158, column: 18, scope: !591)
!714 = !DILocation(line: 159, column: 8, scope: !591)
!715 = !DILocation(line: 160, column: 7, scope: !591)
!716 = !DILocation(line: 160, column: 12, scope: !591)
!717 = !DILocation(line: 161, column: 7, scope: !591)
!718 = !DILocation(line: 161, column: 12, scope: !591)
!719 = !DILocation(line: 162, column: 7, scope: !591)
!720 = !DILocation(line: 162, column: 12, scope: !591)
!721 = !DILocalVariable(name: "ps", scope: !591, file: !10, line: 164, type: !722)
!722 = !DICompositeType(tag: DW_TAG_array_type, baseType: !27, size: 192, elements: !723)
!723 = !{!724}
!724 = !DISubrange(count: 3)
!725 = !DILocation(line: 164, column: 11, scope: !591)
!726 = !DILocation(line: 164, column: 19, scope: !591)
!727 = !{!728, !729}
!728 = !{!"pallas.srcLoc", i64 163, i64 5, i64 163, i64 32, !36}
!729 = !{!"pallas.assert", !730, ptr @PALLAS_SPEC_60, !33, !33, !731}
!730 = !{!"pallas.srcLoc", i64 163, i64 9, i64 163, i64 30, !36}
!731 = !{!732, !737, !739, !741, !743, !745, !747, !749}
!732 = !{!594, !733}
!733 = !DILocalVariable(name: "p", arg: 1, scope: !734, file: !10, line: 163, type: !48)
!734 = distinct !DISubprogram(name: "PALLAS_SPEC_60", scope: !10, file: !10, line: 163, type: !735, scopeLine: 163, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!735 = !DISubroutineType(types: !736)
!736 = !{!46, !48, !47, !48, !48, !48, !293, !292, !47}
!737 = !{!596, !738}
!738 = !DILocalVariable(name: "pp", arg: 2, scope: !734, file: !10, line: 163, type: !47)
!739 = !{!674, !740}
!740 = !DILocalVariable(name: "p1", arg: 3, scope: !734, file: !10, line: 163, type: !48)
!741 = !{!676, !742}
!742 = !DILocalVariable(name: "p2", arg: 4, scope: !734, file: !10, line: 163, type: !48)
!743 = !{!678, !744}
!744 = !DILocalVariable(name: "p3", arg: 5, scope: !734, file: !10, line: 163, type: !48)
!745 = !{!710, !746}
!746 = !DILocalVariable(name: "r", arg: 6, scope: !734, file: !10, line: 163, type: !293)
!747 = !{!712, !748}
!748 = !DILocalVariable(name: "rr", arg: 7, scope: !734, file: !10, line: 163, type: !292)
!749 = !{!721, !750}
!750 = !DILocalVariable(name: "ps", arg: 8, scope: !734, file: !10, line: 163, type: !47)
!751 = !DILocation(line: 164, column: 20, scope: !591)
!752 = !DILocation(line: 164, column: 24, scope: !591)
!753 = !DILocation(line: 164, column: 28, scope: !591)
!754 = !DILocalVariable(name: "pol", scope: !591, file: !10, line: 165, type: !335)
!755 = !DILocation(line: 165, column: 13, scope: !591)
!756 = !DILocalVariable(name: "ppols", scope: !591, file: !10, line: 165, type: !334)
!757 = !DILocation(line: 165, column: 19, scope: !591)
!758 = !DILocation(line: 166, column: 11, scope: !591)
!759 = !DILocation(line: 167, column: 14, scope: !591)
!760 = !DILocation(line: 167, column: 9, scope: !591)
!761 = !DILocation(line: 167, column: 12, scope: !591)
!762 = !DILocalVariable(name: "avr_pol", scope: !591, file: !10, line: 168, type: !31)
!763 = !DILocation(line: 168, column: 9, scope: !591)
!764 = !DILocation(line: 168, column: 29, scope: !591)
!765 = !DILocation(line: 168, column: 19, scope: !591)
!766 = !DILocation(line: 171, column: 5, scope: !591)
!767 = !{!768, !769}
!768 = !{!"pallas.srcLoc", i64 169, i64 5, i64 169, i64 30, !36}
!769 = !{!"pallas.assert", !770, ptr @PALLAS_SPEC_61, !33, !33, !771}
!770 = !{!"pallas.srcLoc", i64 169, i64 9, i64 169, i64 28, !36}
!771 = !{!772, !777, !779, !781, !783, !785, !787, !789, !791, !793, !795}
!772 = !{!594, !773}
!773 = !DILocalVariable(name: "p", arg: 1, scope: !774, file: !10, line: 169, type: !48)
!774 = distinct !DISubprogram(name: "PALLAS_SPEC_61", scope: !10, file: !10, line: 169, type: !775, scopeLine: 169, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!775 = !DISubroutineType(types: !776)
!776 = !{!46, !48, !47, !48, !48, !48, !293, !292, !47, !351, !350, !31}
!777 = !{!596, !778}
!778 = !DILocalVariable(name: "pp", arg: 2, scope: !774, file: !10, line: 169, type: !47)
!779 = !{!674, !780}
!780 = !DILocalVariable(name: "p1", arg: 3, scope: !774, file: !10, line: 169, type: !48)
!781 = !{!676, !782}
!782 = !DILocalVariable(name: "p2", arg: 4, scope: !774, file: !10, line: 169, type: !48)
!783 = !{!678, !784}
!784 = !DILocalVariable(name: "p3", arg: 5, scope: !774, file: !10, line: 169, type: !48)
!785 = !{!710, !786}
!786 = !DILocalVariable(name: "r", arg: 6, scope: !774, file: !10, line: 169, type: !293)
!787 = !{!712, !788}
!788 = !DILocalVariable(name: "rr", arg: 7, scope: !774, file: !10, line: 169, type: !292)
!789 = !{!721, !790}
!790 = !DILocalVariable(name: "ps", arg: 8, scope: !774, file: !10, line: 169, type: !47)
!791 = !{!754, !792}
!792 = !DILocalVariable(name: "pol", arg: 9, scope: !774, file: !10, line: 169, type: !351)
!793 = !{!756, !794}
!794 = !DILocalVariable(name: "ppols", arg: 10, scope: !774, file: !10, line: 169, type: !350)
!795 = !{!762, !796}
!796 = !DILocalVariable(name: "avr_pol", arg: 11, scope: !774, file: !10, line: 169, type: !31)
!797 = !{!""}
!798 = !DILocation(line: 0, scope: !43)
!799 = !DILocation(line: 27, column: 16, scope: !43)
!800 = !DILocation(line: 0, scope: !58)
!801 = !DILocation(line: 28, column: 24, scope: !58)
!802 = !DILocation(line: 28, column: 27, scope: !58)
!803 = !DILocation(line: 28, column: 14, scope: !58)
!804 = !DILocation(line: 0, scope: !64)
!805 = !DILocation(line: 29, column: 24, scope: !64)
!806 = !DILocation(line: 29, column: 27, scope: !64)
!807 = !DILocation(line: 29, column: 14, scope: !64)
!808 = !DILocation(line: 0, scope: !70)
!809 = !DILocation(line: 30, column: 23, scope: !70)
!810 = !DILocation(line: 30, column: 26, scope: !70)
!811 = !DILocation(line: 30, column: 13, scope: !70)
!812 = !DILocation(line: 0, scope: !76)
!813 = !DILocation(line: 31, column: 23, scope: !76)
!814 = !DILocation(line: 31, column: 26, scope: !76)
!815 = !DILocation(line: 31, column: 13, scope: !76)
!816 = !DILocation(line: 0, scope: !82)
!817 = !DILocation(line: 32, column: 16, scope: !82)
!818 = !DILocation(line: 32, column: 18, scope: !82)
!819 = !DILocation(line: 0, scope: !88)
!820 = !DILocation(line: 33, column: 16, scope: !88)
!821 = !DILocation(line: 33, column: 18, scope: !88)
!822 = !DILocation(line: 0, scope: !94)
!823 = !DILocation(line: 34, column: 13, scope: !94)
!824 = !DILocation(line: 34, column: 32, scope: !94)
!825 = !DILocation(line: 0, scope: !112)
!826 = !DILocation(line: 42, column: 16, scope: !112)
!827 = !DILocation(line: 0, scope: !118)
!828 = !DILocation(line: 43, column: 24, scope: !118)
!829 = !DILocation(line: 43, column: 27, scope: !118)
!830 = !DILocation(line: 43, column: 14, scope: !118)
!831 = !DILocation(line: 0, scope: !124)
!832 = !DILocation(line: 44, column: 24, scope: !124)
!833 = !DILocation(line: 44, column: 27, scope: !124)
!834 = !DILocation(line: 44, column: 14, scope: !124)
!835 = !DILocation(line: 0, scope: !130)
!836 = !DILocation(line: 45, column: 23, scope: !130)
!837 = !DILocation(line: 45, column: 26, scope: !130)
!838 = !DILocation(line: 45, column: 13, scope: !130)
!839 = !DILocation(line: 0, scope: !136)
!840 = !DILocation(line: 46, column: 23, scope: !136)
!841 = !DILocation(line: 46, column: 26, scope: !136)
!842 = !DILocation(line: 46, column: 13, scope: !136)
!843 = !DILocation(line: 0, scope: !142)
!844 = !DILocation(line: 47, column: 16, scope: !142)
!845 = !DILocation(line: 47, column: 18, scope: !142)
!846 = !DILocation(line: 0, scope: !148)
!847 = !DILocation(line: 48, column: 16, scope: !148)
!848 = !DILocation(line: 48, column: 18, scope: !148)
!849 = !DILocation(line: 0, scope: !154)
!850 = !DILocation(line: 49, column: 13, scope: !154)
!851 = !DILocation(line: 49, column: 32, scope: !154)
!852 = !DILocation(line: 0, scope: !172)
!853 = !DILocation(line: 57, column: 16, scope: !172)
!854 = !DILocation(line: 0, scope: !178)
!855 = !DILocation(line: 58, column: 25, scope: !178)
!856 = !DILocation(line: 58, column: 14, scope: !178)
!857 = !DILocation(line: 0, scope: !184)
!858 = !DILocation(line: 59, column: 24, scope: !184)
!859 = !DILocation(line: 59, column: 13, scope: !184)
!860 = !DILocation(line: 0, scope: !190)
!861 = !DILocation(line: 60, column: 16, scope: !190)
!862 = !DILocation(line: 60, column: 34, scope: !190)
!863 = !DILocation(line: 60, column: 36, scope: !190)
!864 = !DILocation(line: 60, column: 21, scope: !190)
!865 = !DILocation(line: 60, column: 18, scope: !190)
!866 = !DILocation(line: 0, scope: !196)
!867 = !DILocation(line: 61, column: 16, scope: !196)
!868 = !DILocation(line: 61, column: 34, scope: !196)
!869 = !DILocation(line: 61, column: 36, scope: !196)
!870 = !DILocation(line: 61, column: 21, scope: !196)
!871 = !DILocation(line: 61, column: 18, scope: !196)
!872 = !DILocation(line: 0, scope: !202)
!873 = !DILocation(line: 62, column: 13, scope: !202)
!874 = !DILocation(line: 62, column: 32, scope: !202)
!875 = !DILocation(line: 0, scope: !228)
!876 = !DILocation(line: 70, column: 21, scope: !228)
!877 = !DILocation(line: 70, column: 24, scope: !228)
!878 = !DILocation(line: 70, column: 12, scope: !228)
!879 = !DILocation(line: 0, scope: !236)
!880 = !DILocation(line: 71, column: 21, scope: !236)
!881 = !DILocation(line: 71, column: 24, scope: !236)
!882 = !DILocation(line: 71, column: 12, scope: !236)
!883 = !DILocation(line: 0, scope: !242)
!884 = !DILocation(line: 72, column: 20, scope: !242)
!885 = !DILocation(line: 72, column: 23, scope: !242)
!886 = !DILocation(line: 72, column: 11, scope: !242)
!887 = !DILocation(line: 0, scope: !248)
!888 = !DILocation(line: 73, column: 20, scope: !248)
!889 = !DILocation(line: 73, column: 23, scope: !248)
!890 = !DILocation(line: 73, column: 11, scope: !248)
!891 = !DILocation(line: 0, scope: !264)
!892 = !DILocation(line: 81, column: 22, scope: !264)
!893 = !DILocation(line: 81, column: 12, scope: !264)
!894 = !DILocation(line: 0, scope: !289)
!895 = !DILocation(line: 89, column: 14, scope: !289)
!896 = !DILocation(line: 0, scope: !304)
!897 = !DILocation(line: 90, column: 23, scope: !304)
!898 = !DILocation(line: 90, column: 12, scope: !304)
!899 = !DILocation(line: 0, scope: !310)
!900 = !DILocation(line: 91, column: 22, scope: !310)
!901 = !DILocation(line: 91, column: 11, scope: !310)
!902 = !DILocation(line: 0, scope: !316)
!903 = !DILocation(line: 92, column: 11, scope: !316)
!904 = !DILocation(line: 92, column: 31, scope: !316)
!905 = !DILocation(line: 92, column: 34, scope: !316)
!906 = !DILocation(line: 92, column: 41, scope: !316)
!907 = !DILocation(line: 92, column: 44, scope: !316)
!908 = !DILocation(line: 92, column: 36, scope: !316)
!909 = !DILocation(line: 92, column: 51, scope: !316)
!910 = !DILocation(line: 92, column: 54, scope: !316)
!911 = !DILocation(line: 92, column: 46, scope: !316)
!912 = !DILocation(line: 92, column: 56, scope: !316)
!913 = !DILocation(line: 92, column: 24, scope: !316)
!914 = !DILocation(line: 0, scope: !347)
!915 = !DILocation(line: 99, column: 16, scope: !347)
!916 = !DILocation(line: 0, scope: !363)
!917 = !DILocation(line: 100, column: 14, scope: !363)
!918 = !DILocation(line: 0, scope: !371)
!919 = !DILocation(line: 101, column: 23, scope: !371)
!920 = !DILocation(line: 101, column: 12, scope: !371)
!921 = !DILocation(line: 0, scope: !379)
!922 = !DILocation(line: 102, column: 15, scope: !379)
!923 = !DILocation(line: 102, column: 18, scope: !379)
!924 = !DILocation(line: 102, column: 26, scope: !379)
!925 = !DILocation(line: 102, column: 44, scope: !379)
!926 = !DILocation(line: 102, column: 29, scope: !379)
!927 = !DILocation(line: 102, column: 51, scope: !379)
!928 = !DILocation(line: 102, column: 48, scope: !379)
!929 = !DILocation(line: 0, scope: !387)
!930 = !DILocation(line: 103, column: 30, scope: !387)
!931 = !DILocation(line: 103, column: 27, scope: !387)
!932 = !DILocation(line: 103, column: 48, scope: !387)
!933 = !DILocation(line: 103, column: 60, scope: !387)
!934 = !DILocation(line: 103, column: 77, scope: !387)
!935 = !DILocation(line: 103, column: 74, scope: !387)
!936 = !DILocation(line: 103, column: 90, scope: !387)
!937 = !DILocation(line: 103, column: 102, scope: !387)
!938 = !DILocation(line: 103, column: 67, scope: !387)
!939 = !DILocation(line: 103, column: 43, scope: !387)
!940 = !DILocation(line: 103, column: 20, scope: !387)
!941 = !DILocation(line: 103, column: 119, scope: !387)
!942 = !DILocation(line: 103, column: 134, scope: !387)
!943 = !DILocation(line: 103, column: 131, scope: !387)
!944 = !DILocation(line: 103, column: 150, scope: !387)
!945 = !DILocation(line: 103, column: 155, scope: !387)
!946 = !DILocation(line: 103, column: 153, scope: !387)
!947 = !DILocation(line: 103, column: 173, scope: !387)
!948 = !DILocation(line: 103, column: 178, scope: !387)
!949 = !DILocation(line: 103, column: 176, scope: !387)
!950 = !DILocation(line: 103, column: 167, scope: !387)
!951 = !DILocation(line: 103, column: 112, scope: !387)
!952 = !DILocation(line: 103, column: 12, scope: !387)
!953 = !DILocation(line: 0, scope: !395)
!954 = !DILocation(line: 104, column: 31, scope: !395)
!955 = !DILocation(line: 104, column: 28, scope: !395)
!956 = !DILocation(line: 104, column: 44, scope: !395)
!957 = !DILocation(line: 104, column: 56, scope: !395)
!958 = !DILocation(line: 104, column: 21, scope: !395)
!959 = !DILocation(line: 104, column: 74, scope: !395)
!960 = !DILocation(line: 104, column: 77, scope: !395)
!961 = !DILocation(line: 104, column: 71, scope: !395)
!962 = !DILocation(line: 104, column: 91, scope: !395)
!963 = !DILocation(line: 104, column: 64, scope: !395)
!964 = !DILocation(line: 104, column: 12, scope: !395)
!965 = !DILocation(line: 0, scope: !403)
!966 = !DILocation(line: 105, column: 22, scope: !403)
!967 = !DILocation(line: 105, column: 11, scope: !403)
!968 = !DILocation(line: 0, scope: !411)
!969 = !DILocation(line: 106, column: 14, scope: !411)
!970 = !DILocation(line: 106, column: 17, scope: !411)
!971 = !DILocation(line: 106, column: 25, scope: !411)
!972 = !DILocation(line: 106, column: 43, scope: !411)
!973 = !DILocation(line: 106, column: 28, scope: !411)
!974 = !DILocation(line: 106, column: 50, scope: !411)
!975 = !DILocation(line: 106, column: 47, scope: !411)
!976 = !DILocation(line: 0, scope: !419)
!977 = !DILocation(line: 107, column: 29, scope: !419)
!978 = !DILocation(line: 107, column: 26, scope: !419)
!979 = !DILocation(line: 107, column: 47, scope: !419)
!980 = !DILocation(line: 107, column: 59, scope: !419)
!981 = !DILocation(line: 107, column: 76, scope: !419)
!982 = !DILocation(line: 107, column: 73, scope: !419)
!983 = !DILocation(line: 107, column: 89, scope: !419)
!984 = !DILocation(line: 107, column: 101, scope: !419)
!985 = !DILocation(line: 107, column: 66, scope: !419)
!986 = !DILocation(line: 107, column: 42, scope: !419)
!987 = !DILocation(line: 107, column: 19, scope: !419)
!988 = !DILocation(line: 107, column: 118, scope: !419)
!989 = !DILocation(line: 107, column: 133, scope: !419)
!990 = !DILocation(line: 107, column: 130, scope: !419)
!991 = !DILocation(line: 107, column: 149, scope: !419)
!992 = !DILocation(line: 107, column: 154, scope: !419)
!993 = !DILocation(line: 107, column: 152, scope: !419)
!994 = !DILocation(line: 107, column: 172, scope: !419)
!995 = !DILocation(line: 107, column: 177, scope: !419)
!996 = !DILocation(line: 107, column: 175, scope: !419)
!997 = !DILocation(line: 107, column: 166, scope: !419)
!998 = !DILocation(line: 107, column: 111, scope: !419)
!999 = !DILocation(line: 107, column: 11, scope: !419)
!1000 = !DILocation(line: 0, scope: !427)
!1001 = !DILocation(line: 108, column: 30, scope: !427)
!1002 = !DILocation(line: 108, column: 27, scope: !427)
!1003 = !DILocation(line: 108, column: 43, scope: !427)
!1004 = !DILocation(line: 108, column: 55, scope: !427)
!1005 = !DILocation(line: 108, column: 20, scope: !427)
!1006 = !DILocation(line: 108, column: 73, scope: !427)
!1007 = !DILocation(line: 108, column: 76, scope: !427)
!1008 = !DILocation(line: 108, column: 70, scope: !427)
!1009 = !DILocation(line: 108, column: 90, scope: !427)
!1010 = !DILocation(line: 108, column: 63, scope: !427)
!1011 = !DILocation(line: 108, column: 11, scope: !427)
!1012 = !DILocation(line: 0, scope: !435)
!1013 = !DILocation(line: 109, column: 22, scope: !435)
!1014 = !DILocation(line: 109, column: 28, scope: !435)
!1015 = !DILocation(line: 109, column: 48, scope: !435)
!1016 = !DILocation(line: 109, column: 45, scope: !435)
!1017 = !DILocation(line: 109, column: 54, scope: !435)
!1018 = !DILocation(line: 109, column: 61, scope: !435)
!1019 = !DILocation(line: 109, column: 58, scope: !435)
!1020 = !DILocation(line: 109, column: 67, scope: !435)
!1021 = !DILocation(line: 109, column: 56, scope: !435)
!1022 = !DILocation(line: 109, column: 74, scope: !435)
!1023 = !DILocation(line: 109, column: 71, scope: !435)
!1024 = !DILocation(line: 109, column: 80, scope: !435)
!1025 = !DILocation(line: 109, column: 69, scope: !435)
!1026 = !DILocation(line: 109, column: 82, scope: !435)
!1027 = !DILocation(line: 109, column: 41, scope: !435)
!1028 = !DILocation(line: 109, column: 11, scope: !435)
!1029 = !DILocation(line: 0, scope: !484)
!1030 = !DILocation(line: 115, column: 24, scope: !484)
!1031 = !DILocation(line: 0, scope: !470)
!1032 = !DILocation(line: 114, column: 23, scope: !470)
!1033 = !DILocation(line: 114, column: 27, scope: !470)
!1034 = !DILocation(line: 114, column: 31, scope: !470)
!1035 = !DILocation(line: 0, scope: !508)
!1036 = !DILocation(line: 117, column: 25, scope: !508)
!1037 = !DILocation(line: 117, column: 28, scope: !508)
!1038 = !DILocation(line: 117, column: 36, scope: !508)
!1039 = !DILocation(line: 117, column: 54, scope: !508)
!1040 = !DILocation(line: 117, column: 39, scope: !508)
!1041 = !DILocation(line: 117, column: 61, scope: !508)
!1042 = !DILocation(line: 117, column: 58, scope: !508)
!1043 = !DILocation(line: 0, scope: !520)
!1044 = !DILocation(line: 118, column: 40, scope: !520)
!1045 = !DILocation(line: 118, column: 37, scope: !520)
!1046 = !DILocation(line: 118, column: 58, scope: !520)
!1047 = !DILocation(line: 118, column: 70, scope: !520)
!1048 = !DILocation(line: 118, column: 87, scope: !520)
!1049 = !DILocation(line: 118, column: 84, scope: !520)
!1050 = !DILocation(line: 118, column: 100, scope: !520)
!1051 = !DILocation(line: 118, column: 112, scope: !520)
!1052 = !DILocation(line: 118, column: 77, scope: !520)
!1053 = !DILocation(line: 118, column: 53, scope: !520)
!1054 = !DILocation(line: 118, column: 30, scope: !520)
!1055 = !DILocation(line: 118, column: 129, scope: !520)
!1056 = !DILocation(line: 118, column: 144, scope: !520)
!1057 = !DILocation(line: 118, column: 141, scope: !520)
!1058 = !DILocation(line: 118, column: 160, scope: !520)
!1059 = !DILocation(line: 118, column: 165, scope: !520)
!1060 = !DILocation(line: 118, column: 163, scope: !520)
!1061 = !DILocation(line: 118, column: 183, scope: !520)
!1062 = !DILocation(line: 118, column: 188, scope: !520)
!1063 = !DILocation(line: 118, column: 186, scope: !520)
!1064 = !DILocation(line: 118, column: 177, scope: !520)
!1065 = !DILocation(line: 118, column: 122, scope: !520)
!1066 = !DILocation(line: 118, column: 22, scope: !520)
!1067 = !DILocation(line: 0, scope: !532)
!1068 = !DILocation(line: 119, column: 41, scope: !532)
!1069 = !DILocation(line: 119, column: 38, scope: !532)
!1070 = !DILocation(line: 119, column: 54, scope: !532)
!1071 = !DILocation(line: 119, column: 66, scope: !532)
!1072 = !DILocation(line: 119, column: 31, scope: !532)
!1073 = !DILocation(line: 119, column: 84, scope: !532)
!1074 = !DILocation(line: 119, column: 87, scope: !532)
!1075 = !DILocation(line: 119, column: 81, scope: !532)
!1076 = !DILocation(line: 119, column: 101, scope: !532)
!1077 = !DILocation(line: 119, column: 74, scope: !532)
!1078 = !DILocation(line: 119, column: 22, scope: !532)
!1079 = !DILocation(line: 0, scope: !544)
!1080 = !DILocation(line: 120, column: 31, scope: !544)
!1081 = !DILocation(line: 120, column: 41, scope: !544)
!1082 = !DILocation(line: 120, column: 22, scope: !544)
!1083 = !DILocation(line: 0, scope: !496)
!1084 = !DILocation(line: 116, column: 33, scope: !496)
!1085 = !DILocation(line: 116, column: 22, scope: !496)
!1086 = !DILocation(line: 0, scope: !556)
!1087 = !DILocation(line: 121, column: 31, scope: !556)
!1088 = !DILocation(line: 121, column: 48, scope: !556)
!1089 = !DILocation(line: 121, column: 45, scope: !556)
!1090 = !DILocation(line: 121, column: 54, scope: !556)
!1091 = !DILocation(line: 121, column: 41, scope: !556)
!1092 = !DILocation(line: 121, column: 22, scope: !556)
!1093 = !DILocation(line: 0, scope: !568)
!1094 = !DILocation(line: 122, column: 31, scope: !568)
!1095 = !DILocation(line: 122, column: 48, scope: !568)
!1096 = !DILocation(line: 122, column: 45, scope: !568)
!1097 = !DILocation(line: 122, column: 54, scope: !568)
!1098 = !DILocation(line: 122, column: 61, scope: !568)
!1099 = !DILocation(line: 122, column: 58, scope: !568)
!1100 = !DILocation(line: 122, column: 67, scope: !568)
!1101 = !DILocation(line: 122, column: 56, scope: !568)
!1102 = !DILocation(line: 122, column: 41, scope: !568)
!1103 = !DILocation(line: 122, column: 22, scope: !568)
!1104 = !DILocation(line: 0, scope: !580)
!1105 = !DILocation(line: 123, column: 31, scope: !580)
!1106 = !DILocation(line: 123, column: 48, scope: !580)
!1107 = !DILocation(line: 123, column: 45, scope: !580)
!1108 = !DILocation(line: 123, column: 54, scope: !580)
!1109 = !DILocation(line: 123, column: 61, scope: !580)
!1110 = !DILocation(line: 123, column: 58, scope: !580)
!1111 = !DILocation(line: 123, column: 67, scope: !580)
!1112 = !DILocation(line: 123, column: 56, scope: !580)
!1113 = !DILocation(line: 123, column: 74, scope: !580)
!1114 = !DILocation(line: 123, column: 71, scope: !580)
!1115 = !DILocation(line: 123, column: 80, scope: !580)
!1116 = !DILocation(line: 123, column: 69, scope: !580)
!1117 = !DILocation(line: 123, column: 41, scope: !580)
!1118 = !DILocation(line: 123, column: 22, scope: !580)
!1119 = !DILocation(line: 0, scope: !607)
!1120 = !DILocation(line: 138, column: 19, scope: !607)
!1121 = !DILocation(line: 0, scope: !623)
!1122 = !DILocation(line: 142, column: 20, scope: !623)
!1123 = !DILocation(line: 142, column: 22, scope: !623)
!1124 = !DILocation(line: 0, scope: !631)
!1125 = !DILocation(line: 143, column: 16, scope: !631)
!1126 = !DILocation(line: 143, column: 18, scope: !631)
!1127 = !DILocation(line: 0, scope: !642)
!1128 = !DILocation(line: 145, column: 18, scope: !642)
!1129 = !DILocation(line: 145, column: 20, scope: !642)
!1130 = !DILocation(line: 0, scope: !650)
!1131 = !DILocation(line: 146, column: 14, scope: !650)
!1132 = !DILocation(line: 146, column: 16, scope: !650)
!1133 = !DILocation(line: 0, scope: !662)
!1134 = !DILocation(line: 149, column: 20, scope: !662)
!1135 = !DILocation(line: 149, column: 22, scope: !662)
!1136 = !DILocation(line: 0, scope: !670)
!1137 = !DILocation(line: 150, column: 14, scope: !670)
!1138 = !DILocation(line: 150, column: 16, scope: !670)
!1139 = !DILocation(line: 0, scope: !688)
!1140 = !DILocation(line: 152, column: 18, scope: !688)
!1141 = !DILocation(line: 152, column: 20, scope: !688)
!1142 = !DILocation(line: 152, column: 25, scope: !688)
!1143 = !DILocation(line: 152, column: 30, scope: !688)
!1144 = !DILocation(line: 152, column: 32, scope: !688)
!1145 = !DILocation(line: 0, scope: !734)
!1146 = !DILocation(line: 163, column: 16, scope: !734)
!1147 = !DILocation(line: 163, column: 26, scope: !734)
!1148 = !DILocation(line: 0, scope: !774)
!1149 = !DILocation(line: 169, column: 24, scope: !774)
!1150 = !{!"pallas.old"}
!1151 = !{!"pallas.result"}
!1152 = !{!"pallas.ptrLength"}
!1153 = !{!"pallas.forall"}
!1154 = !{!"pallas.forallSep"}
!1155 = !{!"pallas.scAnd"}
!1156 = !{!"pallas.boundVar"}
!1157 = !{!"pallas.perm"}
!1158 = !{!"pallas.fracOf"}
!1159 = !{!"pallas.imply"}
