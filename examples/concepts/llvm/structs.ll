; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/structs.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.point = type { i32, i32 }
%struct.triangle = type { %struct.point, %struct.point, %struct.point }
%struct.polygon = type { ptr }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [61 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_15, ptr @PALLAS_SPEC_16, ptr @PALLAS_SPEC_17, ptr @PALLAS_SPEC_18, ptr @PALLAS_SPEC_19, ptr @PALLAS_SPEC_20, ptr @PALLAS_SPEC_21, ptr @PALLAS_SPEC_22, ptr @PALLAS_SPEC_23, ptr @PALLAS_SPEC_24, ptr @PALLAS_SPEC_25, ptr @PALLAS_SPEC_26, ptr @PALLAS_SPEC_27, ptr @PALLAS_SPEC_28, ptr @PALLAS_SPEC_29, ptr @PALLAS_SPEC_30, ptr @PALLAS_SPEC_31, ptr @PALLAS_SPEC_32, ptr @PALLAS_SPEC_33, ptr @PALLAS_SPEC_34, ptr @PALLAS_SPEC_35, ptr @PALLAS_SPEC_36, ptr @PALLAS_SPEC_37, ptr @PALLAS_SPEC_38, ptr @PALLAS_SPEC_39, ptr @PALLAS_SPEC_40, ptr @PALLAS_SPEC_42, ptr @PALLAS_SPEC_41, ptr @PALLAS_SPEC_44, ptr @PALLAS_SPEC_45, ptr @PALLAS_SPEC_46, ptr @PALLAS_SPEC_47, ptr @PALLAS_SPEC_43, ptr @PALLAS_SPEC_48, ptr @PALLAS_SPEC_49, ptr @PALLAS_SPEC_50, ptr @PALLAS_SPEC_51, ptr @PALLAS_SPEC_52, ptr @PALLAS_SPEC_53, ptr @PALLAS_SPEC_54, ptr @PALLAS_SPEC_55, ptr @PALLAS_SPEC_56, ptr @PALLAS_SPEC_57, ptr @PALLAS_SPEC_58, ptr @PALLAS_SPEC_59, ptr @PALLAS_SPEC_60], section "llvm.metadata"
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
define dso_local void @alter_copy_struct_2(i64 %0) #0 !dbg !255 {
  %2 = alloca %struct.point, align 4
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !256, metadata !DIExpression()), !dbg !257
  %3 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !258
  store i32 0, ptr %3, align 4, !dbg !259
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !260
  store i32 0, ptr %4, align 4, !dbg !261
  ret void, !dbg !262
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @avr_x(ptr noundef %0) #0 !dbg !263 !pallas.fcontract !273 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !279, metadata !DIExpression()), !dbg !309
  %3 = load ptr, ptr %2, align 8, !dbg !310
  %4 = getelementptr inbounds %struct.triangle, ptr %3, i32 0, i32 0, !dbg !311
  %5 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 0, !dbg !312
  %6 = load i32, ptr %5, align 4, !dbg !312
  %7 = load ptr, ptr %2, align 8, !dbg !313
  %8 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 1, !dbg !314
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !315
  %10 = load i32, ptr %9, align 4, !dbg !315
  %11 = add nsw i32 %6, %10, !dbg !316
  %12 = load ptr, ptr %2, align 8, !dbg !317
  %13 = getelementptr inbounds %struct.triangle, ptr %12, i32 0, i32 2, !dbg !318
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !319
  %15 = load i32, ptr %14, align 4, !dbg !319
  %16 = add nsw i32 %11, %15, !dbg !320
  %17 = sdiv i32 %16, 3, !dbg !321
  ret i32 %17, !dbg !322
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @avr_x_pol(ptr noundef %0, i32 noundef %1) #0 !dbg !323 !pallas.fcontract !331 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !337, metadata !DIExpression()), !dbg !430
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !348, metadata !DIExpression()), !dbg !431
  call void @llvm.dbg.declare(metadata ptr %5, metadata !432, metadata !DIExpression()), !dbg !433
  store i32 0, ptr %5, align 4, !dbg !433
  call void @llvm.dbg.declare(metadata ptr %6, metadata !434, metadata !DIExpression()), !dbg !436
  store i32 0, ptr %6, align 4, !dbg !436
  br label %7, !dbg !437

7:                                                ; preds = %22, %2
  %8 = load i32, ptr %6, align 4, !dbg !438
  %9 = load i32, ptr %4, align 4, !dbg !440
  %10 = icmp slt i32 %8, %9, !dbg !441
  br i1 %10, label %11, label %25, !dbg !442

11:                                               ; preds = %7
  %12 = load ptr, ptr %3, align 8, !dbg !443
  %13 = getelementptr inbounds %struct.polygon, ptr %12, i32 0, i32 0, !dbg !445
  %14 = load ptr, ptr %13, align 8, !dbg !445
  %15 = load i32, ptr %6, align 4, !dbg !446
  %16 = sext i32 %15 to i64, !dbg !443
  %17 = getelementptr inbounds %struct.point, ptr %14, i64 %16, !dbg !443
  %18 = getelementptr inbounds %struct.point, ptr %17, i32 0, i32 0, !dbg !447
  %19 = load i32, ptr %18, align 4, !dbg !447
  %20 = load i32, ptr %5, align 4, !dbg !448
  %21 = add nsw i32 %20, %19, !dbg !448
  store i32 %21, ptr %5, align 4, !dbg !448
  br label %22, !dbg !449

22:                                               ; preds = %11
  %23 = load i32, ptr %6, align 4, !dbg !450
  %24 = add nsw i32 %23, 1, !dbg !450
  store i32 %24, ptr %6, align 4, !dbg !450
  br label %7, !dbg !451, !llvm.loop !452

25:                                               ; preds = %7
  %26 = load i32, ptr %5, align 4, !dbg !579
  %27 = load i32, ptr %4, align 4, !dbg !580
  %28 = sdiv i32 %26, %27, !dbg !581
  ret i32 %28, !dbg !582
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @main() #0 !dbg !583 {
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
  call void @llvm.dbg.declare(metadata ptr %2, metadata !586, metadata !DIExpression()), !dbg !587
  call void @llvm.dbg.declare(metadata ptr %3, metadata !588, metadata !DIExpression()), !dbg !589
  store ptr %2, ptr %3, align 8, !dbg !590
  %13 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !591, !pallas.stmntBlock !592
  store i32 1, ptr %13, align 4, !dbg !604
  %14 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !605
  store i32 2, ptr %14, align 4, !dbg !606
  %15 = load i64, ptr %2, align 4, !dbg !607, !pallas.stmntBlock !608
  call void @alter_copy_struct(i64 %15), !dbg !607
  %16 = load ptr, ptr %3, align 8, !dbg !626, !pallas.stmntBlock !627
  call void @alter_struct(ptr noundef %16), !dbg !645
  %17 = load ptr, ptr %3, align 8, !dbg !646, !pallas.stmntBlock !647
  call void @alter_struct_1(ptr noundef %17), !dbg !665
  call void @llvm.dbg.declare(metadata ptr %4, metadata !666, metadata !DIExpression()), !dbg !667
  call void @llvm.dbg.declare(metadata ptr %5, metadata !668, metadata !DIExpression()), !dbg !669
  call void @llvm.dbg.declare(metadata ptr %6, metadata !670, metadata !DIExpression()), !dbg !671
  %18 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 0, !dbg !672, !pallas.stmntBlock !673
  store i32 1, ptr %18, align 4, !dbg !691
  %19 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !692
  store i32 1, ptr %19, align 4, !dbg !693
  %20 = getelementptr inbounds %struct.point, ptr %5, i32 0, i32 0, !dbg !694
  store i32 2, ptr %20, align 4, !dbg !695
  %21 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !696
  store i32 2, ptr %21, align 4, !dbg !697
  %22 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !698
  store i32 3, ptr %22, align 4, !dbg !699
  %23 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !700
  store i32 3, ptr %23, align 4, !dbg !701
  call void @llvm.dbg.declare(metadata ptr %7, metadata !702, metadata !DIExpression()), !dbg !703
  call void @llvm.dbg.declare(metadata ptr %8, metadata !704, metadata !DIExpression()), !dbg !705
  store ptr %7, ptr %8, align 8, !dbg !706
  %24 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 0, !dbg !707
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %24, ptr align 4 %4, i64 8, i1 false), !dbg !708
  %25 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 1, !dbg !709
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %25, ptr align 4 %5, i64 8, i1 false), !dbg !710
  %26 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 2, !dbg !711
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %26, ptr align 4 %6, i64 8, i1 false), !dbg !712
  call void @llvm.dbg.declare(metadata ptr %9, metadata !713, metadata !DIExpression()), !dbg !717
  %27 = getelementptr inbounds [3 x %struct.point], ptr %9, i64 0, i64 0, !dbg !718, !pallas.stmntBlock !719
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 4 %4, i64 8, i1 false), !dbg !743
  %28 = getelementptr inbounds %struct.point, ptr %27, i64 1, !dbg !718
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %28, ptr align 4 %5, i64 8, i1 false), !dbg !744
  %29 = getelementptr inbounds %struct.point, ptr %28, i64 1, !dbg !718
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %29, ptr align 4 %6, i64 8, i1 false), !dbg !745
  call void @llvm.dbg.declare(metadata ptr %10, metadata !746, metadata !DIExpression()), !dbg !747
  call void @llvm.dbg.declare(metadata ptr %11, metadata !748, metadata !DIExpression()), !dbg !749
  store ptr %10, ptr %11, align 8, !dbg !750
  %30 = getelementptr inbounds [3 x %struct.point], ptr %9, i64 0, i64 0, !dbg !751
  %31 = getelementptr inbounds %struct.polygon, ptr %10, i32 0, i32 0, !dbg !752
  store ptr %30, ptr %31, align 8, !dbg !753
  call void @llvm.dbg.declare(metadata ptr %12, metadata !754, metadata !DIExpression()), !dbg !755
  %32 = load ptr, ptr %11, align 8, !dbg !756
  %33 = call i32 @avr_x_pol(ptr noundef %32, i32 noundef 3), !dbg !757
  store i32 %33, ptr %12, align 4, !dbg !755
  ret i32 0, !dbg !758, !pallas.stmntBlock !759
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #2

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !43 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !42, metadata !DIExpression()), !dbg !790
  %2 = icmp ne ptr %0, null, !dbg !791
  ret i1 %2, !dbg !790
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !58 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !57, metadata !DIExpression()), !dbg !792
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !793
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !794
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !795
  ret i1 %4, !dbg !792
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !64 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !63, metadata !DIExpression()), !dbg !796
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !797
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !798
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !799
  ret i1 %4, !dbg !796
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !70 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !69, metadata !DIExpression()), !dbg !800
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !801
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !802
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !803
  ret i1 %4, !dbg !800
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0) #0 !dbg !76 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !75, metadata !DIExpression()), !dbg !804
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !805
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !806
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !807
  ret i1 %4, !dbg !804
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0) #0 !dbg !82 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !81, metadata !DIExpression()), !dbg !808
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !809
  %3 = load i32, ptr %2, align 4, !dbg !809
  %4 = icmp eq i32 %3, 0, !dbg !810
  ret i1 %4, !dbg !808
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0) #0 !dbg !88 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !87, metadata !DIExpression()), !dbg !811
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !812
  %3 = load i32, ptr %2, align 4, !dbg !812
  %4 = icmp eq i32 %3, 0, !dbg !813
  ret i1 %4, !dbg !811
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0) #0 !dbg !94 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !93, metadata !DIExpression()), !dbg !814
  %2 = call ptr @"pallas.old ptr_noundef ptr"(ptr noundef %0), !dbg !815
  %3 = icmp eq ptr %2, %0, !dbg !816
  ret i1 %3, !dbg !814
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0) #0 !dbg !112 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !111, metadata !DIExpression()), !dbg !817
  %2 = icmp ne ptr %0, null, !dbg !818
  ret i1 %2, !dbg !817
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0) #0 !dbg !118 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !117, metadata !DIExpression()), !dbg !819
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !820
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !821
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !822
  ret i1 %4, !dbg !819
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0) #0 !dbg !124 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !123, metadata !DIExpression()), !dbg !823
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !824
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !825
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !826
  ret i1 %4, !dbg !823
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0) #0 !dbg !130 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !129, metadata !DIExpression()), !dbg !827
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !828
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !829
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !830
  ret i1 %4, !dbg !827
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0) #0 !dbg !136 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !135, metadata !DIExpression()), !dbg !831
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !832
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !833
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !834
  ret i1 %4, !dbg !831
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0) #0 !dbg !142 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !141, metadata !DIExpression()), !dbg !835
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !836
  %3 = load i32, ptr %2, align 4, !dbg !836
  %4 = icmp eq i32 %3, 0, !dbg !837
  ret i1 %4, !dbg !835
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0) #0 !dbg !148 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !147, metadata !DIExpression()), !dbg !838
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !839
  %3 = load i32, ptr %2, align 4, !dbg !839
  %4 = icmp eq i32 %3, 0, !dbg !840
  ret i1 %4, !dbg !838
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0) #0 !dbg !154 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !153, metadata !DIExpression()), !dbg !841
  %2 = call ptr @"pallas.old ptr_noundef ptr"(ptr noundef %0), !dbg !842
  %3 = icmp eq ptr %2, %0, !dbg !843
  ret i1 %3, !dbg !841
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(ptr noundef %0) #0 !dbg !172 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !171, metadata !DIExpression()), !dbg !844
  %2 = icmp ne ptr %0, null, !dbg !845
  ret i1 %2, !dbg !844
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(ptr noundef %0) #0 !dbg !178 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !177, metadata !DIExpression()), !dbg !846
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !847
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !848
  ret i1 %3, !dbg !846
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(ptr noundef %0) #0 !dbg !184 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !183, metadata !DIExpression()), !dbg !849
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !850
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !851
  ret i1 %3, !dbg !849
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_19(ptr noundef %0) #0 !dbg !190 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !189, metadata !DIExpression()), !dbg !852
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !853
  %3 = load i32, ptr %2, align 4, !dbg !853
  %4 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !854
  %5 = load i32, ptr %4, align 4, !dbg !854
  %6 = add nsw i32 %5, 1, !dbg !855
  %7 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %6), !dbg !856
  %8 = icmp eq i32 %3, %7, !dbg !857
  ret i1 %8, !dbg !852
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_20(ptr noundef %0) #0 !dbg !196 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !195, metadata !DIExpression()), !dbg !858
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !859
  %3 = load i32, ptr %2, align 4, !dbg !859
  %4 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !860
  %5 = load i32, ptr %4, align 4, !dbg !860
  %6 = add nsw i32 %5, 1, !dbg !861
  %7 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %6), !dbg !862
  %8 = icmp eq i32 %3, %7, !dbg !863
  ret i1 %8, !dbg !858
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_21(ptr noundef %0) #0 !dbg !202 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !201, metadata !DIExpression()), !dbg !864
  %2 = call ptr @"pallas.old ptr_noundef ptr"(ptr noundef %0), !dbg !865
  %3 = icmp eq ptr %2, %0, !dbg !866
  ret i1 %3, !dbg !864
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_22(i64 %0) #0 !dbg !228 !pallas.exprWrapper !789 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !227, metadata !DIExpression()), !dbg !867
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !868
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !869
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !870
  ret i1 %5, !dbg !867
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_23(i64 %0) #0 !dbg !236 !pallas.exprWrapper !789 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !235, metadata !DIExpression()), !dbg !871
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !872
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !873
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !874
  ret i1 %5, !dbg !871
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_24(i64 %0) #0 !dbg !242 !pallas.exprWrapper !789 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !241, metadata !DIExpression()), !dbg !875
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !876
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !877
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !878
  ret i1 %5, !dbg !875
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_25(i64 %0) #0 !dbg !248 !pallas.exprWrapper !789 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !247, metadata !DIExpression()), !dbg !879
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !880
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !881
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !882
  ret i1 %5, !dbg !879
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_26(ptr noundef %0) #0 !dbg !281 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !280, metadata !DIExpression()), !dbg !883
  %2 = icmp ne ptr %0, null, !dbg !884
  ret i1 %2, !dbg !883
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_27(ptr noundef %0) #0 !dbg !296 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !295, metadata !DIExpression()), !dbg !885
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !886
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !887
  ret i1 %3, !dbg !885
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_28(ptr noundef %0) #0 !dbg !302 !pallas.exprWrapper !789 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !301, metadata !DIExpression()), !dbg !888
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !889
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !890
  ret i1 %3, !dbg !888
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_29(ptr noundef %0) #0 !dbg !308 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !307, metadata !DIExpression()), !dbg !891
  %2 = call i32 @"pallas.result i32"(), !dbg !892
  %3 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 0, !dbg !893
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !894
  %5 = load i32, ptr %4, align 4, !dbg !894
  %6 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 1, !dbg !895
  %7 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !896
  %8 = load i32, ptr %7, align 4, !dbg !896
  %9 = add nsw i32 %5, %8, !dbg !897
  %10 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 2, !dbg !898
  %11 = getelementptr inbounds %struct.point, ptr %10, i32 0, i32 0, !dbg !899
  %12 = load i32, ptr %11, align 4, !dbg !899
  %13 = add nsw i32 %9, %12, !dbg !900
  %14 = sdiv i32 %13, 3, !dbg !901
  %15 = icmp eq i32 %2, %14, !dbg !902
  ret i1 %15, !dbg !891
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_30(ptr noundef %0, i32 noundef %1) #0 !dbg !339 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !338, metadata !DIExpression()), !dbg !903
  call void @llvm.dbg.value(metadata i32 %1, metadata !349, metadata !DIExpression()), !dbg !903
  %3 = icmp sgt i32 %1, 0, !dbg !904
  ret i1 %3, !dbg !903
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_31(ptr noundef %0, i32 noundef %1) #0 !dbg !355 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !354, metadata !DIExpression()), !dbg !905
  call void @llvm.dbg.value(metadata i32 %1, metadata !357, metadata !DIExpression()), !dbg !905
  %3 = icmp ne ptr %0, null, !dbg !906
  ret i1 %3, !dbg !905
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_32(ptr noundef %0, i32 noundef %1) #0 !dbg !363 !pallas.exprWrapper !789 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !362, metadata !DIExpression()), !dbg !907
  call void @llvm.dbg.value(metadata i32 %1, metadata !365, metadata !DIExpression()), !dbg !907
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !908
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !909
  ret i1 %4, !dbg !907
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_33(ptr noundef %0, i32 noundef %1) #0 !dbg !371 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !370, metadata !DIExpression()), !dbg !910
  call void @llvm.dbg.value(metadata i32 %1, metadata !373, metadata !DIExpression()), !dbg !910
  %3 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !911
  %4 = load ptr, ptr %3, align 8, !dbg !911
  %5 = icmp ne ptr %4, null, !dbg !912
  br i1 %5, label %6, label %12, !dbg !913

6:                                                ; preds = %2
  %7 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !914
  %8 = load ptr, ptr %7, align 8, !dbg !914
  %9 = call i64 @pallas.ptrLength(ptr noundef %8), !dbg !915
  %10 = sext i32 %1 to i64, !dbg !916
  %11 = icmp sge i64 %9, %10, !dbg !917
  br label %12

12:                                               ; preds = %6, %2
  %13 = phi i1 [ false, %2 ], [ %11, %6 ], !dbg !910
  ret i1 %13, !dbg !910
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_34(ptr noundef %0, i32 noundef %1) #0 !dbg !379 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !378, metadata !DIExpression()), !dbg !918
  call void @llvm.dbg.value(metadata i32 %1, metadata !381, metadata !DIExpression()), !dbg !918
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !919
  %4 = icmp sle i32 0, %3, !dbg !920
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !921
  %6 = icmp slt i32 %5, %1, !dbg !922
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !923
  %8 = icmp sle i32 0, %7, !dbg !924
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !925
  %10 = icmp slt i32 %9, %1, !dbg !926
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !927
  %12 = call i1 @pallas.scAnd(i1 %6, i1 %11), !dbg !928
  %13 = call i1 @pallas.scAnd(i1 %4, i1 %12), !dbg !929
  %14 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !930
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !931
  %16 = icmp ne i32 %14, %15, !dbg !932
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !933
  %18 = load ptr, ptr %17, align 8, !dbg !933
  %19 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !934
  %20 = sext i32 %19 to i64, !dbg !935
  %21 = getelementptr inbounds %struct.point, ptr %18, i64 %20, !dbg !935
  %22 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !936
  %23 = load ptr, ptr %22, align 8, !dbg !936
  %24 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !937
  %25 = sext i32 %24 to i64, !dbg !938
  %26 = getelementptr inbounds %struct.point, ptr %23, i64 %25, !dbg !938
  %27 = icmp ne ptr %21, %26, !dbg !939
  %28 = call i1 @pallas.imply(i1 %16, i1 %27), !dbg !940
  %29 = call i1 @pallas.forall(i1 %13, i1 %28), !dbg !941
  ret i1 %29, !dbg !918
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_35(ptr noundef %0, i32 noundef %1) #0 !dbg !387 !pallas.exprWrapper !789 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !386, metadata !DIExpression()), !dbg !942
  call void @llvm.dbg.value(metadata i32 %1, metadata !389, metadata !DIExpression()), !dbg !942
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !943
  %5 = icmp sle i32 0, %4, !dbg !944
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !945
  %7 = icmp slt i32 %6, %1, !dbg !946
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !947
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !948
  %10 = load ptr, ptr %9, align 8, !dbg !948
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !949
  %12 = sext i32 %11 to i64, !dbg !950
  %13 = getelementptr inbounds %struct.point, ptr %10, i64 %12, !dbg !950
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !951
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %3), !dbg !952
  %15 = call i1 @pallas.forallSep(i1 %8, i1 %14), !dbg !953
  ret i1 %15, !dbg !942
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_36(ptr noundef %0, i32 noundef %1) #0 !dbg !395 !pallas.exprWrapper !789 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !394, metadata !DIExpression()), !dbg !954
  call void @llvm.dbg.value(metadata i32 %1, metadata !397, metadata !DIExpression()), !dbg !954
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !955
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !956
  ret i1 %4, !dbg !954
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_37(ptr noundef %0, i32 noundef %1) #0 !dbg !403 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !402, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.value(metadata i32 %1, metadata !405, metadata !DIExpression()), !dbg !957
  %3 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !958
  %4 = load ptr, ptr %3, align 8, !dbg !958
  %5 = icmp ne ptr %4, null, !dbg !959
  br i1 %5, label %6, label %12, !dbg !960

6:                                                ; preds = %2
  %7 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !961
  %8 = load ptr, ptr %7, align 8, !dbg !961
  %9 = call i64 @pallas.ptrLength(ptr noundef %8), !dbg !962
  %10 = sext i32 %1 to i64, !dbg !963
  %11 = icmp sge i64 %9, %10, !dbg !964
  br label %12

12:                                               ; preds = %6, %2
  %13 = phi i1 [ false, %2 ], [ %11, %6 ], !dbg !957
  ret i1 %13, !dbg !957
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_38(ptr noundef %0, i32 noundef %1) #0 !dbg !411 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !410, metadata !DIExpression()), !dbg !965
  call void @llvm.dbg.value(metadata i32 %1, metadata !413, metadata !DIExpression()), !dbg !965
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !966
  %4 = icmp sle i32 0, %3, !dbg !967
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !968
  %6 = icmp slt i32 %5, %1, !dbg !969
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !970
  %8 = icmp sle i32 0, %7, !dbg !971
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !972
  %10 = icmp slt i32 %9, %1, !dbg !973
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !974
  %12 = call i1 @pallas.scAnd(i1 %6, i1 %11), !dbg !975
  %13 = call i1 @pallas.scAnd(i1 %4, i1 %12), !dbg !976
  %14 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !977
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !978
  %16 = icmp ne i32 %14, %15, !dbg !979
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !980
  %18 = load ptr, ptr %17, align 8, !dbg !980
  %19 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !981
  %20 = sext i32 %19 to i64, !dbg !982
  %21 = getelementptr inbounds %struct.point, ptr %18, i64 %20, !dbg !982
  %22 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !983
  %23 = load ptr, ptr %22, align 8, !dbg !983
  %24 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !984
  %25 = sext i32 %24 to i64, !dbg !985
  %26 = getelementptr inbounds %struct.point, ptr %23, i64 %25, !dbg !985
  %27 = icmp ne ptr %21, %26, !dbg !986
  %28 = call i1 @pallas.imply(i1 %16, i1 %27), !dbg !987
  %29 = call i1 @pallas.forall(i1 %13, i1 %28), !dbg !988
  ret i1 %29, !dbg !965
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_39(ptr noundef %0, i32 noundef %1) #0 !dbg !419 !pallas.exprWrapper !789 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !418, metadata !DIExpression()), !dbg !989
  call void @llvm.dbg.value(metadata i32 %1, metadata !421, metadata !DIExpression()), !dbg !989
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !990
  %5 = icmp sle i32 0, %4, !dbg !991
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !992
  %7 = icmp slt i32 %6, %1, !dbg !993
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !994
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !995
  %10 = load ptr, ptr %9, align 8, !dbg !995
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !996
  %12 = sext i32 %11 to i64, !dbg !997
  %13 = getelementptr inbounds %struct.point, ptr %10, i64 %12, !dbg !997
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !998
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %3), !dbg !999
  %15 = call i1 @pallas.forallSep(i1 %8, i1 %14), !dbg !1000
  ret i1 %15, !dbg !989
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_40(ptr noundef %0, i32 noundef %1) #0 !dbg !427 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !426, metadata !DIExpression()), !dbg !1001
  call void @llvm.dbg.value(metadata i32 %1, metadata !429, metadata !DIExpression()), !dbg !1001
  %3 = icmp eq i32 %1, 3, !dbg !1002
  %4 = call i32 @"pallas.result i32"(), !dbg !1003
  %5 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1004
  %6 = load ptr, ptr %5, align 8, !dbg !1004
  %7 = getelementptr inbounds %struct.point, ptr %6, i64 0, !dbg !1005
  %8 = getelementptr inbounds %struct.point, ptr %7, i32 0, i32 0, !dbg !1006
  %9 = load i32, ptr %8, align 4, !dbg !1006
  %10 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1007
  %11 = load ptr, ptr %10, align 8, !dbg !1007
  %12 = getelementptr inbounds %struct.point, ptr %11, i64 1, !dbg !1008
  %13 = getelementptr inbounds %struct.point, ptr %12, i32 0, i32 0, !dbg !1009
  %14 = load i32, ptr %13, align 4, !dbg !1009
  %15 = add nsw i32 %9, %14, !dbg !1010
  %16 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1011
  %17 = load ptr, ptr %16, align 8, !dbg !1011
  %18 = getelementptr inbounds %struct.point, ptr %17, i64 2, !dbg !1012
  %19 = getelementptr inbounds %struct.point, ptr %18, i32 0, i32 0, !dbg !1013
  %20 = load i32, ptr %19, align 4, !dbg !1013
  %21 = add nsw i32 %15, %20, !dbg !1014
  %22 = sdiv i32 %21, %1, !dbg !1015
  %23 = icmp eq i32 %4, %22, !dbg !1016
  %24 = call i1 @pallas.imply(i1 %3, i1 %23), !dbg !1017
  ret i1 %24, !dbg !1001
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_42(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !476 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !475, metadata !DIExpression()), !dbg !1018
  call void @llvm.dbg.value(metadata i32 %1, metadata !478, metadata !DIExpression()), !dbg !1018
  call void @llvm.dbg.value(metadata i32 %2, metadata !480, metadata !DIExpression()), !dbg !1018
  call void @llvm.dbg.value(metadata i32 %3, metadata !482, metadata !DIExpression()), !dbg !1018
  %5 = icmp ne ptr %0, null, !dbg !1019
  ret i1 %5, !dbg !1018
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_41(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !462 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !461, metadata !DIExpression()), !dbg !1020
  call void @llvm.dbg.value(metadata i32 %1, metadata !466, metadata !DIExpression()), !dbg !1020
  call void @llvm.dbg.value(metadata i32 %2, metadata !468, metadata !DIExpression()), !dbg !1020
  call void @llvm.dbg.value(metadata i32 %3, metadata !470, metadata !DIExpression()), !dbg !1020
  %5 = icmp sle i32 0, %3, !dbg !1021
  br i1 %5, label %6, label %8, !dbg !1022

6:                                                ; preds = %4
  %7 = icmp sle i32 %3, %1, !dbg !1023
  br label %8

8:                                                ; preds = %6, %4
  %9 = phi i1 [ false, %4 ], [ %7, %6 ], !dbg !1020
  ret i1 %9, !dbg !1020
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_44(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !500 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !499, metadata !DIExpression()), !dbg !1024
  call void @llvm.dbg.value(metadata i32 %1, metadata !502, metadata !DIExpression()), !dbg !1024
  call void @llvm.dbg.value(metadata i32 %2, metadata !504, metadata !DIExpression()), !dbg !1024
  call void @llvm.dbg.value(metadata i32 %3, metadata !506, metadata !DIExpression()), !dbg !1024
  %5 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1025
  %6 = load ptr, ptr %5, align 8, !dbg !1025
  %7 = icmp ne ptr %6, null, !dbg !1026
  br i1 %7, label %8, label %14, !dbg !1027

8:                                                ; preds = %4
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1028
  %10 = load ptr, ptr %9, align 8, !dbg !1028
  %11 = call i64 @pallas.ptrLength(ptr noundef %10), !dbg !1029
  %12 = sext i32 %1 to i64, !dbg !1030
  %13 = icmp sge i64 %11, %12, !dbg !1031
  br label %14

14:                                               ; preds = %8, %4
  %15 = phi i1 [ false, %4 ], [ %13, %8 ], !dbg !1024
  ret i1 %15, !dbg !1024
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_45(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !512 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !511, metadata !DIExpression()), !dbg !1032
  call void @llvm.dbg.value(metadata i32 %1, metadata !514, metadata !DIExpression()), !dbg !1032
  call void @llvm.dbg.value(metadata i32 %2, metadata !516, metadata !DIExpression()), !dbg !1032
  call void @llvm.dbg.value(metadata i32 %3, metadata !518, metadata !DIExpression()), !dbg !1032
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1033
  %6 = icmp sle i32 0, %5, !dbg !1034
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1035
  %8 = icmp slt i32 %7, %1, !dbg !1036
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1037
  %10 = icmp sle i32 0, %9, !dbg !1038
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1039
  %12 = icmp slt i32 %11, %1, !dbg !1040
  %13 = call i1 @pallas.scAnd(i1 %10, i1 %12), !dbg !1041
  %14 = call i1 @pallas.scAnd(i1 %8, i1 %13), !dbg !1042
  %15 = call i1 @pallas.scAnd(i1 %6, i1 %14), !dbg !1043
  %16 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1044
  %17 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1045
  %18 = icmp ne i32 %16, %17, !dbg !1046
  %19 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1047
  %20 = load ptr, ptr %19, align 8, !dbg !1047
  %21 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1048
  %22 = sext i32 %21 to i64, !dbg !1049
  %23 = getelementptr inbounds %struct.point, ptr %20, i64 %22, !dbg !1049
  %24 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1050
  %25 = load ptr, ptr %24, align 8, !dbg !1050
  %26 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !1051
  %27 = sext i32 %26 to i64, !dbg !1052
  %28 = getelementptr inbounds %struct.point, ptr %25, i64 %27, !dbg !1052
  %29 = icmp ne ptr %23, %28, !dbg !1053
  %30 = call i1 @pallas.imply(i1 %18, i1 %29), !dbg !1054
  %31 = call i1 @pallas.forall(i1 %15, i1 %30), !dbg !1055
  ret i1 %31, !dbg !1032
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_46(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !524 !pallas.exprWrapper !789 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !523, metadata !DIExpression()), !dbg !1056
  call void @llvm.dbg.value(metadata i32 %1, metadata !526, metadata !DIExpression()), !dbg !1056
  call void @llvm.dbg.value(metadata i32 %2, metadata !528, metadata !DIExpression()), !dbg !1056
  call void @llvm.dbg.value(metadata i32 %3, metadata !530, metadata !DIExpression()), !dbg !1056
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1057
  %7 = icmp sle i32 0, %6, !dbg !1058
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1059
  %9 = icmp slt i32 %8, %1, !dbg !1060
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !1061
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1062
  %12 = load ptr, ptr %11, align 8, !dbg !1062
  %13 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !1063
  %14 = sext i32 %13 to i64, !dbg !1064
  %15 = getelementptr inbounds %struct.point, ptr %12, i64 %14, !dbg !1064
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !1065
  %16 = call i1 @pallas.perm(ptr noundef %15, ptr noundef byval(%pallas.fracT) %5), !dbg !1066
  %17 = call i1 @pallas.forallSep(i1 %10, i1 %16), !dbg !1067
  ret i1 %17, !dbg !1056
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_47(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !536 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !535, metadata !DIExpression()), !dbg !1068
  call void @llvm.dbg.value(metadata i32 %1, metadata !538, metadata !DIExpression()), !dbg !1068
  call void @llvm.dbg.value(metadata i32 %2, metadata !540, metadata !DIExpression()), !dbg !1068
  call void @llvm.dbg.value(metadata i32 %3, metadata !542, metadata !DIExpression()), !dbg !1068
  %5 = icmp eq i32 %3, 0, !dbg !1069
  %6 = icmp eq i32 %2, 0, !dbg !1070
  %7 = call i1 @pallas.imply(i1 %5, i1 %6), !dbg !1071
  ret i1 %7, !dbg !1068
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_43(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !488 !pallas.exprWrapper !789 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !487, metadata !DIExpression()), !dbg !1072
  call void @llvm.dbg.value(metadata i32 %1, metadata !490, metadata !DIExpression()), !dbg !1072
  call void @llvm.dbg.value(metadata i32 %2, metadata !492, metadata !DIExpression()), !dbg !1072
  call void @llvm.dbg.value(metadata i32 %3, metadata !494, metadata !DIExpression()), !dbg !1072
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !1073
  %6 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %5), !dbg !1074
  ret i1 %6, !dbg !1072
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_48(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !548 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !547, metadata !DIExpression()), !dbg !1075
  call void @llvm.dbg.value(metadata i32 %1, metadata !550, metadata !DIExpression()), !dbg !1075
  call void @llvm.dbg.value(metadata i32 %2, metadata !552, metadata !DIExpression()), !dbg !1075
  call void @llvm.dbg.value(metadata i32 %3, metadata !554, metadata !DIExpression()), !dbg !1075
  %5 = icmp eq i32 %3, 1, !dbg !1076
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1077
  %7 = load ptr, ptr %6, align 8, !dbg !1077
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !1078
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !1079
  %10 = load i32, ptr %9, align 4, !dbg !1079
  %11 = icmp eq i32 %2, %10, !dbg !1080
  %12 = call i1 @pallas.imply(i1 %5, i1 %11), !dbg !1081
  ret i1 %12, !dbg !1075
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_49(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !560 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !559, metadata !DIExpression()), !dbg !1082
  call void @llvm.dbg.value(metadata i32 %1, metadata !562, metadata !DIExpression()), !dbg !1082
  call void @llvm.dbg.value(metadata i32 %2, metadata !564, metadata !DIExpression()), !dbg !1082
  call void @llvm.dbg.value(metadata i32 %3, metadata !566, metadata !DIExpression()), !dbg !1082
  %5 = icmp eq i32 %3, 2, !dbg !1083
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1084
  %7 = load ptr, ptr %6, align 8, !dbg !1084
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !1085
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !1086
  %10 = load i32, ptr %9, align 4, !dbg !1086
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1087
  %12 = load ptr, ptr %11, align 8, !dbg !1087
  %13 = getelementptr inbounds %struct.point, ptr %12, i64 1, !dbg !1088
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !1089
  %15 = load i32, ptr %14, align 4, !dbg !1089
  %16 = add nsw i32 %10, %15, !dbg !1090
  %17 = icmp eq i32 %2, %16, !dbg !1091
  %18 = call i1 @pallas.imply(i1 %5, i1 %17), !dbg !1092
  ret i1 %18, !dbg !1082
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_50(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !572 !pallas.exprWrapper !789 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !571, metadata !DIExpression()), !dbg !1093
  call void @llvm.dbg.value(metadata i32 %1, metadata !574, metadata !DIExpression()), !dbg !1093
  call void @llvm.dbg.value(metadata i32 %2, metadata !576, metadata !DIExpression()), !dbg !1093
  call void @llvm.dbg.value(metadata i32 %3, metadata !578, metadata !DIExpression()), !dbg !1093
  %5 = icmp eq i32 %3, 3, !dbg !1094
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
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !1102
  %18 = load ptr, ptr %17, align 8, !dbg !1102
  %19 = getelementptr inbounds %struct.point, ptr %18, i64 2, !dbg !1103
  %20 = getelementptr inbounds %struct.point, ptr %19, i32 0, i32 0, !dbg !1104
  %21 = load i32, ptr %20, align 4, !dbg !1104
  %22 = add nsw i32 %16, %21, !dbg !1105
  %23 = icmp eq i32 %2, %22, !dbg !1106
  %24 = call i1 @pallas.imply(i1 %5, i1 %23), !dbg !1107
  ret i1 %24, !dbg !1093
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_51(i64 %0, ptr noundef %1) #0 !dbg !599 !pallas.exprWrapper !789 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !598, metadata !DIExpression()), !dbg !1108
  call void @llvm.dbg.value(metadata ptr %1, metadata !603, metadata !DIExpression()), !dbg !1108
  %4 = icmp ne ptr %1, null, !dbg !1109
  ret i1 %4, !dbg !1108
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_52(i64 %0, ptr noundef %1) #0 !dbg !615 !pallas.exprWrapper !789 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !614, metadata !DIExpression()), !dbg !1110
  call void @llvm.dbg.value(metadata ptr %1, metadata !617, metadata !DIExpression()), !dbg !1110
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 0, !dbg !1111
  %5 = load i32, ptr %4, align 4, !dbg !1111
  %6 = icmp eq i32 %5, 1, !dbg !1112
  ret i1 %6, !dbg !1110
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_53(i64 %0, ptr noundef %1) #0 !dbg !623 !pallas.exprWrapper !789 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !622, metadata !DIExpression()), !dbg !1113
  call void @llvm.dbg.value(metadata ptr %1, metadata !625, metadata !DIExpression()), !dbg !1113
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 1, !dbg !1114
  %5 = load i32, ptr %4, align 4, !dbg !1114
  %6 = icmp eq i32 %5, 2, !dbg !1115
  ret i1 %6, !dbg !1113
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_54(i64 %0, ptr noundef %1) #0 !dbg !634 !pallas.exprWrapper !789 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !633, metadata !DIExpression()), !dbg !1116
  call void @llvm.dbg.value(metadata ptr %1, metadata !636, metadata !DIExpression()), !dbg !1116
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !1117
  %5 = load i32, ptr %4, align 4, !dbg !1117
  %6 = icmp eq i32 %5, 1, !dbg !1118
  ret i1 %6, !dbg !1116
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_55(i64 %0, ptr noundef %1) #0 !dbg !642 !pallas.exprWrapper !789 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !641, metadata !DIExpression()), !dbg !1119
  call void @llvm.dbg.value(metadata ptr %1, metadata !644, metadata !DIExpression()), !dbg !1119
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 1, !dbg !1120
  %5 = load i32, ptr %4, align 4, !dbg !1120
  %6 = icmp eq i32 %5, 2, !dbg !1121
  ret i1 %6, !dbg !1119
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_56(i64 %0, ptr noundef %1) #0 !dbg !654 !pallas.exprWrapper !789 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !653, metadata !DIExpression()), !dbg !1122
  call void @llvm.dbg.value(metadata ptr %1, metadata !656, metadata !DIExpression()), !dbg !1122
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 0, !dbg !1123
  %5 = load i32, ptr %4, align 4, !dbg !1123
  %6 = icmp eq i32 %5, 0, !dbg !1124
  ret i1 %6, !dbg !1122
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_57(i64 %0, ptr noundef %1) #0 !dbg !662 !pallas.exprWrapper !789 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !661, metadata !DIExpression()), !dbg !1125
  call void @llvm.dbg.value(metadata ptr %1, metadata !664, metadata !DIExpression()), !dbg !1125
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !1126
  %5 = load i32, ptr %4, align 4, !dbg !1126
  %6 = icmp eq i32 %5, 0, !dbg !1127
  ret i1 %6, !dbg !1125
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_58(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4) #0 !dbg !680 !pallas.exprWrapper !789 {
  %6 = alloca %struct.point, align 4
  %7 = alloca %struct.point, align 4
  %8 = alloca %struct.point, align 4
  %9 = alloca %struct.point, align 4
  store i64 %0, ptr %6, align 4
  store i64 %2, ptr %7, align 4
  store i64 %3, ptr %8, align 4
  store i64 %4, ptr %9, align 4
  call void @llvm.dbg.declare(metadata ptr %6, metadata !679, metadata !DIExpression()), !dbg !1128
  call void @llvm.dbg.value(metadata ptr %1, metadata !684, metadata !DIExpression()), !dbg !1128
  call void @llvm.dbg.declare(metadata ptr %7, metadata !686, metadata !DIExpression()), !dbg !1128
  call void @llvm.dbg.declare(metadata ptr %8, metadata !688, metadata !DIExpression()), !dbg !1128
  call void @llvm.dbg.declare(metadata ptr %9, metadata !690, metadata !DIExpression()), !dbg !1128
  %10 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !1129
  %11 = load i32, ptr %10, align 4, !dbg !1129
  %12 = icmp eq i32 %11, 1, !dbg !1130
  br i1 %12, label %13, label %17, !dbg !1131

13:                                               ; preds = %5
  %14 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 1, !dbg !1132
  %15 = load i32, ptr %14, align 4, !dbg !1132
  %16 = icmp eq i32 %15, 1, !dbg !1133
  br label %17

17:                                               ; preds = %13, %5
  %18 = phi i1 [ false, %5 ], [ %16, %13 ], !dbg !1128
  ret i1 %18, !dbg !1128
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_59(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4, ptr noundef byval(%struct.triangle) align 8 %5, ptr noundef %6, ptr noundef %7) #0 !dbg !726 !pallas.exprWrapper !789 {
  %9 = alloca %struct.point, align 4
  %10 = alloca %struct.point, align 4
  %11 = alloca %struct.point, align 4
  %12 = alloca %struct.point, align 4
  store i64 %0, ptr %9, align 4
  store i64 %2, ptr %10, align 4
  store i64 %3, ptr %11, align 4
  store i64 %4, ptr %12, align 4
  call void @llvm.dbg.declare(metadata ptr %9, metadata !725, metadata !DIExpression()), !dbg !1134
  call void @llvm.dbg.value(metadata ptr %1, metadata !730, metadata !DIExpression()), !dbg !1134
  call void @llvm.dbg.declare(metadata ptr %10, metadata !732, metadata !DIExpression()), !dbg !1134
  call void @llvm.dbg.declare(metadata ptr %11, metadata !734, metadata !DIExpression()), !dbg !1134
  call void @llvm.dbg.declare(metadata ptr %12, metadata !736, metadata !DIExpression()), !dbg !1134
  call void @llvm.dbg.declare(metadata ptr %5, metadata !738, metadata !DIExpression()), !dbg !1134
  call void @llvm.dbg.value(metadata ptr %6, metadata !740, metadata !DIExpression()), !dbg !1134
  call void @llvm.dbg.value(metadata ptr %7, metadata !742, metadata !DIExpression()), !dbg !1134
  %13 = call i32 @avr_x(ptr noundef %6), !dbg !1135
  %14 = icmp eq i32 %13, 2, !dbg !1136
  ret i1 %14, !dbg !1134
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_60(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4, ptr noundef byval(%struct.triangle) align 8 %5, ptr noundef %6, ptr noundef %7, i64 %8, ptr noundef %9, i32 noundef %10) #0 !dbg !766 !pallas.exprWrapper !789 {
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
  call void @llvm.dbg.declare(metadata ptr %12, metadata !765, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.value(metadata ptr %1, metadata !770, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.declare(metadata ptr %13, metadata !772, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.declare(metadata ptr %14, metadata !774, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.declare(metadata ptr %15, metadata !776, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.declare(metadata ptr %5, metadata !778, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.value(metadata ptr %6, metadata !780, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.value(metadata ptr %7, metadata !782, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.declare(metadata ptr %16, metadata !784, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.value(metadata ptr %9, metadata !786, metadata !DIExpression()), !dbg !1137
  call void @llvm.dbg.value(metadata i32 %10, metadata !788, metadata !DIExpression()), !dbg !1137
  %19 = icmp eq i32 %10, 2, !dbg !1138
  ret i1 %19, !dbg !1137
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !1139 i32 @"pallas.old i32_noundef i32"(i32 noundef)

declare !pallas.specLib !1139 ptr @"pallas.old ptr_noundef ptr"(ptr noundef)

declare !pallas.specLib !1140 i32 @"pallas.result i32"()

declare !pallas.specLib !1141 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !1142 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !1143 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !1144 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !1145 i32 @"pallas.boundVar i32"(ptr)

declare !pallas.specLib !1146 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !1147 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !1148 i1 @pallas.imply(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }

!llvm.dbg.cu = !{!9, !11}
!llvm.module.flags = !{!15, !16, !17, !18, !19, !20, !21}
!llvm.ident = !{!22, !22}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 416, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "85000a33acffbd9feee7d05d2fa81fa1")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 416, type: !3, isLocal: true, isDefinition: true)
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !10, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!10 = !DIFile(filename: "examples/concepts/llvm/structs.c", directory: ".", checksumkind: CSK_MD5, checksum: "4e4b9d081ed3b340de809376168b5d9c")
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
!36 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/structs.c", directory: "", checksumkind: CSK_MD5, checksum: "4e4b9d081ed3b340de809376168b5d9c")
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
!255 = distinct !DISubprogram(name: "alter_copy_struct_2", scope: !10, file: !10, line: 81, type: !218, scopeLine: 81, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!256 = !DILocalVariable(name: "p", arg: 1, scope: !255, file: !10, line: 81, type: !27)
!257 = !DILocation(line: 81, column: 32, scope: !255)
!258 = !DILocation(line: 82, column: 7, scope: !255)
!259 = !DILocation(line: 82, column: 9, scope: !255)
!260 = !DILocation(line: 83, column: 7, scope: !255)
!261 = !DILocation(line: 83, column: 9, scope: !255)
!262 = !DILocation(line: 84, column: 1, scope: !255)
!263 = distinct !DISubprogram(name: "avr_x", scope: !10, file: !10, line: 92, type: !264, scopeLine: 92, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!264 = !DISubroutineType(types: !265)
!265 = !{!31, !266}
!266 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !267, size: 64)
!267 = !DIDerivedType(tag: DW_TAG_typedef, name: "triangle", file: !10, line: 11, baseType: !268)
!268 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "triangle", file: !10, line: 9, size: 192, elements: !269)
!269 = !{!270, !271, !272}
!270 = !DIDerivedType(tag: DW_TAG_member, name: "p1", scope: !268, file: !10, line: 10, baseType: !27, size: 64)
!271 = !DIDerivedType(tag: DW_TAG_member, name: "p2", scope: !268, file: !10, line: 10, baseType: !27, size: 64, offset: 64)
!272 = !DIDerivedType(tag: DW_TAG_member, name: "p3", scope: !268, file: !10, line: 10, baseType: !27, size: 64, offset: 128)
!273 = !{!274, i1 false, i1 false, !33, !33, !275, !291, !297, !303}
!274 = !{!"pallas.srcLoc", i64 86, i64 1, i64 91, i64 1, !36}
!275 = !{!"pallas.requires", !276, ptr @PALLAS_SPEC_26, !33, !33, !277}
!276 = !{!"pallas.srcLoc", i64 87, i64 3, i64 87, i64 21, !36}
!277 = !{!278}
!278 = !{!279, !280}
!279 = !DILocalVariable(name: "r", arg: 1, scope: !263, file: !10, line: 92, type: !266)
!280 = !DILocalVariable(name: "r", arg: 1, scope: !281, file: !10, line: 87, type: !284)
!281 = distinct !DISubprogram(name: "PALLAS_SPEC_26", scope: !10, file: !10, line: 87, type: !282, scopeLine: 87, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!282 = !DISubroutineType(types: !283)
!283 = !{!46, !284}
!284 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !285, size: 64)
!285 = !DIDerivedType(tag: DW_TAG_typedef, name: "triangle", file: !2, line: 12, baseType: !286)
!286 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "triangle", file: !2, line: 10, size: 192, elements: !287)
!287 = !{!288, !289, !290}
!288 = !DIDerivedType(tag: DW_TAG_member, name: "p1", scope: !286, file: !2, line: 11, baseType: !48, size: 64)
!289 = !DIDerivedType(tag: DW_TAG_member, name: "p2", scope: !286, file: !2, line: 11, baseType: !48, size: 64, offset: 64)
!290 = !DIDerivedType(tag: DW_TAG_member, name: "p3", scope: !286, file: !2, line: 11, baseType: !48, size: 64, offset: 128)
!291 = !{!"pallas.requires", !292, ptr @PALLAS_SPEC_27, !33, !33, !293}
!292 = !{!"pallas.srcLoc", i64 88, i64 3, i64 88, i64 37, !36}
!293 = !{!294}
!294 = !{!279, !295}
!295 = !DILocalVariable(name: "r", arg: 1, scope: !296, file: !10, line: 88, type: !284)
!296 = distinct !DISubprogram(name: "PALLAS_SPEC_27", scope: !10, file: !10, line: 88, type: !282, scopeLine: 88, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!297 = !{!"pallas.ensures", !298, ptr @PALLAS_SPEC_28, !33, !33, !299}
!298 = !{!"pallas.srcLoc", i64 89, i64 3, i64 89, i64 36, !36}
!299 = !{!300}
!300 = !{!279, !301}
!301 = !DILocalVariable(name: "r", arg: 1, scope: !302, file: !10, line: 89, type: !284)
!302 = distinct !DISubprogram(name: "PALLAS_SPEC_28", scope: !10, file: !10, line: 89, type: !282, scopeLine: 89, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!303 = !{!"pallas.ensures", !304, ptr @PALLAS_SPEC_29, !33, !33, !305}
!304 = !{!"pallas.srcLoc", i64 90, i64 3, i64 90, i64 58, !36}
!305 = !{!306}
!306 = !{!279, !307}
!307 = !DILocalVariable(name: "r", arg: 1, scope: !308, file: !10, line: 90, type: !284)
!308 = distinct !DISubprogram(name: "PALLAS_SPEC_29", scope: !10, file: !10, line: 90, type: !282, scopeLine: 90, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!309 = !DILocation(line: 92, column: 21, scope: !263)
!310 = !DILocation(line: 93, column: 13, scope: !263)
!311 = !DILocation(line: 93, column: 16, scope: !263)
!312 = !DILocation(line: 93, column: 19, scope: !263)
!313 = !DILocation(line: 93, column: 23, scope: !263)
!314 = !DILocation(line: 93, column: 26, scope: !263)
!315 = !DILocation(line: 93, column: 29, scope: !263)
!316 = !DILocation(line: 93, column: 21, scope: !263)
!317 = !DILocation(line: 93, column: 33, scope: !263)
!318 = !DILocation(line: 93, column: 36, scope: !263)
!319 = !DILocation(line: 93, column: 39, scope: !263)
!320 = !DILocation(line: 93, column: 31, scope: !263)
!321 = !DILocation(line: 93, column: 41, scope: !263)
!322 = !DILocation(line: 93, column: 5, scope: !263)
!323 = distinct !DISubprogram(name: "avr_x_pol", scope: !10, file: !10, line: 109, type: !324, scopeLine: 109, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!324 = !DISubroutineType(types: !325)
!325 = !{!31, !326, !31}
!326 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !327, size: 64)
!327 = !DIDerivedType(tag: DW_TAG_typedef, name: "polygon", file: !10, line: 15, baseType: !328)
!328 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "polygon", file: !10, line: 13, size: 64, elements: !329)
!329 = !{!330}
!330 = !DIDerivedType(tag: DW_TAG_member, name: "ps", scope: !328, file: !10, line: 14, baseType: !26, size: 64)
!331 = !{!332, i1 false, i1 false, !33, !33, !333, !350, !358, !366, !374, !382, !390, !398, !406, !414, !422}
!332 = !{!"pallas.srcLoc", i64 96, i64 1, i64 108, i64 1, !36}
!333 = !{!"pallas.requires", !334, ptr @PALLAS_SPEC_30, !33, !33, !335}
!334 = !{!"pallas.srcLoc", i64 97, i64 3, i64 97, i64 19, !36}
!335 = !{!336, !347}
!336 = !{!337, !338}
!337 = !DILocalVariable(name: "p", arg: 1, scope: !323, file: !10, line: 109, type: !326)
!338 = !DILocalVariable(name: "p", arg: 1, scope: !339, file: !10, line: 97, type: !342)
!339 = distinct !DISubprogram(name: "PALLAS_SPEC_30", scope: !10, file: !10, line: 97, type: !340, scopeLine: 97, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!340 = !DISubroutineType(types: !341)
!341 = !{!46, !342, !31}
!342 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !343, size: 64)
!343 = !DIDerivedType(tag: DW_TAG_typedef, name: "polygon", file: !2, line: 16, baseType: !344)
!344 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "polygon", file: !2, line: 14, size: 64, elements: !345)
!345 = !{!346}
!346 = !DIDerivedType(tag: DW_TAG_member, name: "ps", scope: !344, file: !2, line: 15, baseType: !47, size: 64)
!347 = !{!348, !349}
!348 = !DILocalVariable(name: "len", arg: 2, scope: !323, file: !10, line: 109, type: !31)
!349 = !DILocalVariable(name: "len", arg: 2, scope: !339, file: !10, line: 97, type: !31)
!350 = !{!"pallas.requires", !351, ptr @PALLAS_SPEC_31, !33, !33, !352}
!351 = !{!"pallas.srcLoc", i64 98, i64 3, i64 98, i64 21, !36}
!352 = !{!353, !356}
!353 = !{!337, !354}
!354 = !DILocalVariable(name: "p", arg: 1, scope: !355, file: !10, line: 98, type: !342)
!355 = distinct !DISubprogram(name: "PALLAS_SPEC_31", scope: !10, file: !10, line: 98, type: !340, scopeLine: 98, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!356 = !{!348, !357}
!357 = !DILocalVariable(name: "len", arg: 2, scope: !355, file: !10, line: 98, type: !31)
!358 = !{!"pallas.requires", !359, ptr @PALLAS_SPEC_32, !33, !33, !360}
!359 = !{!"pallas.srcLoc", i64 99, i64 3, i64 99, i64 37, !36}
!360 = !{!361, !364}
!361 = !{!337, !362}
!362 = !DILocalVariable(name: "p", arg: 1, scope: !363, file: !10, line: 99, type: !342)
!363 = distinct !DISubprogram(name: "PALLAS_SPEC_32", scope: !10, file: !10, line: 99, type: !340, scopeLine: 99, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!364 = !{!348, !365}
!365 = !DILocalVariable(name: "len", arg: 2, scope: !363, file: !10, line: 99, type: !31)
!366 = !{!"pallas.requires", !367, ptr @PALLAS_SPEC_33, !33, !33, !368}
!367 = !{!"pallas.srcLoc", i64 100, i64 3, i64 100, i64 54, !36}
!368 = !{!369, !372}
!369 = !{!337, !370}
!370 = !DILocalVariable(name: "p", arg: 1, scope: !371, file: !10, line: 100, type: !342)
!371 = distinct !DISubprogram(name: "PALLAS_SPEC_33", scope: !10, file: !10, line: 100, type: !340, scopeLine: 100, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!372 = !{!348, !373}
!373 = !DILocalVariable(name: "len", arg: 2, scope: !371, file: !10, line: 100, type: !31)
!374 = !{!"pallas.requires", !375, ptr @PALLAS_SPEC_34, !33, !33, !376}
!375 = !{!"pallas.srcLoc", i64 101, i64 3, i64 101, i64 191, !36}
!376 = !{!377, !380}
!377 = !{!337, !378}
!378 = !DILocalVariable(name: "p", arg: 1, scope: !379, file: !10, line: 101, type: !342)
!379 = distinct !DISubprogram(name: "PALLAS_SPEC_34", scope: !10, file: !10, line: 101, type: !340, scopeLine: 101, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!380 = !{!348, !381}
!381 = !DILocalVariable(name: "len", arg: 2, scope: !379, file: !10, line: 101, type: !31)
!382 = !{!"pallas.requires", !383, ptr @PALLAS_SPEC_35, !33, !33, !384}
!383 = !{!"pallas.srcLoc", i64 102, i64 3, i64 102, i64 106, !36}
!384 = !{!385, !388}
!385 = !{!337, !386}
!386 = !DILocalVariable(name: "p", arg: 1, scope: !387, file: !10, line: 102, type: !342)
!387 = distinct !DISubprogram(name: "PALLAS_SPEC_35", scope: !10, file: !10, line: 102, type: !340, scopeLine: 102, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!388 = !{!348, !389}
!389 = !DILocalVariable(name: "len", arg: 2, scope: !387, file: !10, line: 102, type: !31)
!390 = !{!"pallas.ensures", !391, ptr @PALLAS_SPEC_36, !33, !33, !392}
!391 = !{!"pallas.srcLoc", i64 103, i64 3, i64 103, i64 36, !36}
!392 = !{!393, !396}
!393 = !{!337, !394}
!394 = !DILocalVariable(name: "p", arg: 1, scope: !395, file: !10, line: 103, type: !342)
!395 = distinct !DISubprogram(name: "PALLAS_SPEC_36", scope: !10, file: !10, line: 103, type: !340, scopeLine: 103, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!396 = !{!348, !397}
!397 = !DILocalVariable(name: "len", arg: 2, scope: !395, file: !10, line: 103, type: !31)
!398 = !{!"pallas.ensures", !399, ptr @PALLAS_SPEC_37, !33, !33, !400}
!399 = !{!"pallas.srcLoc", i64 104, i64 3, i64 104, i64 53, !36}
!400 = !{!401, !404}
!401 = !{!337, !402}
!402 = !DILocalVariable(name: "p", arg: 1, scope: !403, file: !10, line: 104, type: !342)
!403 = distinct !DISubprogram(name: "PALLAS_SPEC_37", scope: !10, file: !10, line: 104, type: !340, scopeLine: 104, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!404 = !{!348, !405}
!405 = !DILocalVariable(name: "len", arg: 2, scope: !403, file: !10, line: 104, type: !31)
!406 = !{!"pallas.ensures", !407, ptr @PALLAS_SPEC_38, !33, !33, !408}
!407 = !{!"pallas.srcLoc", i64 105, i64 3, i64 105, i64 190, !36}
!408 = !{!409, !412}
!409 = !{!337, !410}
!410 = !DILocalVariable(name: "p", arg: 1, scope: !411, file: !10, line: 105, type: !342)
!411 = distinct !DISubprogram(name: "PALLAS_SPEC_38", scope: !10, file: !10, line: 105, type: !340, scopeLine: 105, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!412 = !{!348, !413}
!413 = !DILocalVariable(name: "len", arg: 2, scope: !411, file: !10, line: 105, type: !31)
!414 = !{!"pallas.ensures", !415, ptr @PALLAS_SPEC_39, !33, !33, !416}
!415 = !{!"pallas.srcLoc", i64 106, i64 3, i64 106, i64 105, !36}
!416 = !{!417, !420}
!417 = !{!337, !418}
!418 = !DILocalVariable(name: "p", arg: 1, scope: !419, file: !10, line: 106, type: !342)
!419 = distinct !DISubprogram(name: "PALLAS_SPEC_39", scope: !10, file: !10, line: 106, type: !340, scopeLine: 106, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!420 = !{!348, !421}
!421 = !DILocalVariable(name: "len", arg: 2, scope: !419, file: !10, line: 106, type: !31)
!422 = !{!"pallas.ensures", !423, ptr @PALLAS_SPEC_40, !33, !33, !424}
!423 = !{!"pallas.srcLoc", i64 107, i64 3, i64 107, i64 87, !36}
!424 = !{!425, !428}
!425 = !{!337, !426}
!426 = !DILocalVariable(name: "p", arg: 1, scope: !427, file: !10, line: 107, type: !342)
!427 = distinct !DISubprogram(name: "PALLAS_SPEC_40", scope: !10, file: !10, line: 107, type: !340, scopeLine: 107, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!428 = !{!348, !429}
!429 = !DILocalVariable(name: "len", arg: 2, scope: !427, file: !10, line: 107, type: !31)
!430 = !DILocation(line: 109, column: 24, scope: !323)
!431 = !DILocation(line: 109, column: 31, scope: !323)
!432 = !DILocalVariable(name: "sum", scope: !323, file: !10, line: 110, type: !31)
!433 = !DILocation(line: 110, column: 9, scope: !323)
!434 = !DILocalVariable(name: "i", scope: !435, file: !10, line: 123, type: !31)
!435 = distinct !DILexicalBlock(scope: !323, file: !10, line: 123, column: 5)
!436 = !DILocation(line: 123, column: 13, scope: !435)
!437 = !DILocation(line: 123, column: 9, scope: !435)
!438 = !DILocation(line: 123, column: 18, scope: !439)
!439 = distinct !DILexicalBlock(scope: !435, file: !10, line: 123, column: 5)
!440 = !DILocation(line: 123, column: 20, scope: !439)
!441 = !DILocation(line: 123, column: 19, scope: !439)
!442 = !DILocation(line: 123, column: 5, scope: !435)
!443 = !DILocation(line: 124, column: 16, scope: !444)
!444 = distinct !DILexicalBlock(scope: !439, file: !10, line: 123, column: 29)
!445 = !DILocation(line: 124, column: 19, scope: !444)
!446 = !DILocation(line: 124, column: 22, scope: !444)
!447 = !DILocation(line: 124, column: 25, scope: !444)
!448 = !DILocation(line: 124, column: 13, scope: !444)
!449 = !DILocation(line: 125, column: 5, scope: !444)
!450 = !DILocation(line: 123, column: 26, scope: !439)
!451 = !DILocation(line: 123, column: 5, scope: !439)
!452 = distinct !{!452, !442, !453, !454, !455}
!453 = !DILocation(line: 125, column: 5, scope: !435)
!454 = !{!"llvm.loop.mustprogress"}
!455 = !{!"pallas.loopInvBlock", !456, !457, !471, !483, !495, !507, !519, !531, !543, !555, !567}
!456 = !{!"pallas.srcLoc", i64 111, i64 5, i64 122, i64 5, !36}
!457 = !{!"pallas.loopInv", !458, ptr @PALLAS_SPEC_41, !33, !33, !459}
!458 = !{!"pallas.srcLoc", i64 112, i64 7, i64 112, i64 36, !36}
!459 = !{!460, !465, !467, !469}
!460 = !{!337, !461}
!461 = !DILocalVariable(name: "p", arg: 1, scope: !462, file: !10, line: 112, type: !342)
!462 = distinct !DISubprogram(name: "PALLAS_SPEC_41", scope: !10, file: !10, line: 112, type: !463, scopeLine: 112, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!463 = !DISubroutineType(types: !464)
!464 = !{!46, !342, !31, !31, !31}
!465 = !{!348, !466}
!466 = !DILocalVariable(name: "len", arg: 2, scope: !462, file: !10, line: 112, type: !31)
!467 = !{!432, !468}
!468 = !DILocalVariable(name: "sum", arg: 3, scope: !462, file: !10, line: 112, type: !31)
!469 = !{!434, !470}
!470 = !DILocalVariable(name: "i", arg: 4, scope: !462, file: !10, line: 112, type: !31)
!471 = !{!"pallas.loopInv", !472, ptr @PALLAS_SPEC_42, !33, !33, !473}
!472 = !{!"pallas.srcLoc", i64 113, i64 7, i64 113, i64 31, !36}
!473 = !{!474, !477, !479, !481}
!474 = !{!337, !475}
!475 = !DILocalVariable(name: "p", arg: 1, scope: !476, file: !10, line: 113, type: !342)
!476 = distinct !DISubprogram(name: "PALLAS_SPEC_42", scope: !10, file: !10, line: 113, type: !463, scopeLine: 113, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!477 = !{!348, !478}
!478 = !DILocalVariable(name: "len", arg: 2, scope: !476, file: !10, line: 113, type: !31)
!479 = !{!432, !480}
!480 = !DILocalVariable(name: "sum", arg: 3, scope: !476, file: !10, line: 113, type: !31)
!481 = !{!434, !482}
!482 = !DILocalVariable(name: "i", arg: 4, scope: !476, file: !10, line: 113, type: !31)
!483 = !{!"pallas.loopInv", !484, ptr @PALLAS_SPEC_43, !33, !33, !485}
!484 = !{!"pallas.srcLoc", i64 114, i64 7, i64 114, i64 47, !36}
!485 = !{!486, !489, !491, !493}
!486 = !{!337, !487}
!487 = !DILocalVariable(name: "p", arg: 1, scope: !488, file: !10, line: 114, type: !342)
!488 = distinct !DISubprogram(name: "PALLAS_SPEC_43", scope: !10, file: !10, line: 114, type: !463, scopeLine: 114, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!489 = !{!348, !490}
!490 = !DILocalVariable(name: "len", arg: 2, scope: !488, file: !10, line: 114, type: !31)
!491 = !{!432, !492}
!492 = !DILocalVariable(name: "sum", arg: 3, scope: !488, file: !10, line: 114, type: !31)
!493 = !{!434, !494}
!494 = !DILocalVariable(name: "i", arg: 4, scope: !488, file: !10, line: 114, type: !31)
!495 = !{!"pallas.loopInv", !496, ptr @PALLAS_SPEC_44, !33, !33, !497}
!496 = !{!"pallas.srcLoc", i64 115, i64 7, i64 115, i64 64, !36}
!497 = !{!498, !501, !503, !505}
!498 = !{!337, !499}
!499 = !DILocalVariable(name: "p", arg: 1, scope: !500, file: !10, line: 115, type: !342)
!500 = distinct !DISubprogram(name: "PALLAS_SPEC_44", scope: !10, file: !10, line: 115, type: !463, scopeLine: 115, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!501 = !{!348, !502}
!502 = !DILocalVariable(name: "len", arg: 2, scope: !500, file: !10, line: 115, type: !31)
!503 = !{!432, !504}
!504 = !DILocalVariable(name: "sum", arg: 3, scope: !500, file: !10, line: 115, type: !31)
!505 = !{!434, !506}
!506 = !DILocalVariable(name: "i", arg: 4, scope: !500, file: !10, line: 115, type: !31)
!507 = !{!"pallas.loopInv", !508, ptr @PALLAS_SPEC_45, !33, !33, !509}
!508 = !{!"pallas.srcLoc", i64 116, i64 7, i64 116, i64 201, !36}
!509 = !{!510, !513, !515, !517}
!510 = !{!337, !511}
!511 = !DILocalVariable(name: "p", arg: 1, scope: !512, file: !10, line: 116, type: !342)
!512 = distinct !DISubprogram(name: "PALLAS_SPEC_45", scope: !10, file: !10, line: 116, type: !463, scopeLine: 116, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!513 = !{!348, !514}
!514 = !DILocalVariable(name: "len", arg: 2, scope: !512, file: !10, line: 116, type: !31)
!515 = !{!432, !516}
!516 = !DILocalVariable(name: "sum", arg: 3, scope: !512, file: !10, line: 116, type: !31)
!517 = !{!434, !518}
!518 = !DILocalVariable(name: "i", arg: 4, scope: !512, file: !10, line: 116, type: !31)
!519 = !{!"pallas.loopInv", !520, ptr @PALLAS_SPEC_46, !33, !33, !521}
!520 = !{!"pallas.srcLoc", i64 117, i64 7, i64 117, i64 116, !36}
!521 = !{!522, !525, !527, !529}
!522 = !{!337, !523}
!523 = !DILocalVariable(name: "p", arg: 1, scope: !524, file: !10, line: 117, type: !342)
!524 = distinct !DISubprogram(name: "PALLAS_SPEC_46", scope: !10, file: !10, line: 117, type: !463, scopeLine: 117, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!525 = !{!348, !526}
!526 = !DILocalVariable(name: "len", arg: 2, scope: !524, file: !10, line: 117, type: !31)
!527 = !{!432, !528}
!528 = !DILocalVariable(name: "sum", arg: 3, scope: !524, file: !10, line: 117, type: !31)
!529 = !{!434, !530}
!530 = !DILocalVariable(name: "i", arg: 4, scope: !524, file: !10, line: 117, type: !31)
!531 = !{!"pallas.loopInv", !532, ptr @PALLAS_SPEC_47, !33, !33, !533}
!532 = !{!"pallas.srcLoc", i64 118, i64 7, i64 118, i64 48, !36}
!533 = !{!534, !537, !539, !541}
!534 = !{!337, !535}
!535 = !DILocalVariable(name: "p", arg: 1, scope: !536, file: !10, line: 118, type: !342)
!536 = distinct !DISubprogram(name: "PALLAS_SPEC_47", scope: !10, file: !10, line: 118, type: !463, scopeLine: 118, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!537 = !{!348, !538}
!538 = !DILocalVariable(name: "len", arg: 2, scope: !536, file: !10, line: 118, type: !31)
!539 = !{!432, !540}
!540 = !DILocalVariable(name: "sum", arg: 3, scope: !536, file: !10, line: 118, type: !31)
!541 = !{!434, !542}
!542 = !DILocalVariable(name: "i", arg: 4, scope: !536, file: !10, line: 118, type: !31)
!543 = !{!"pallas.loopInv", !544, ptr @PALLAS_SPEC_48, !33, !33, !545}
!544 = !{!"pallas.srcLoc", i64 119, i64 7, i64 119, i64 57, !36}
!545 = !{!546, !549, !551, !553}
!546 = !{!337, !547}
!547 = !DILocalVariable(name: "p", arg: 1, scope: !548, file: !10, line: 119, type: !342)
!548 = distinct !DISubprogram(name: "PALLAS_SPEC_48", scope: !10, file: !10, line: 119, type: !463, scopeLine: 119, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!549 = !{!348, !550}
!550 = !DILocalVariable(name: "len", arg: 2, scope: !548, file: !10, line: 119, type: !31)
!551 = !{!432, !552}
!552 = !DILocalVariable(name: "sum", arg: 3, scope: !548, file: !10, line: 119, type: !31)
!553 = !{!434, !554}
!554 = !DILocalVariable(name: "i", arg: 4, scope: !548, file: !10, line: 119, type: !31)
!555 = !{!"pallas.loopInv", !556, ptr @PALLAS_SPEC_49, !33, !33, !557}
!556 = !{!"pallas.srcLoc", i64 120, i64 7, i64 120, i64 70, !36}
!557 = !{!558, !561, !563, !565}
!558 = !{!337, !559}
!559 = !DILocalVariable(name: "p", arg: 1, scope: !560, file: !10, line: 120, type: !342)
!560 = distinct !DISubprogram(name: "PALLAS_SPEC_49", scope: !10, file: !10, line: 120, type: !463, scopeLine: 120, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!561 = !{!348, !562}
!562 = !DILocalVariable(name: "len", arg: 2, scope: !560, file: !10, line: 120, type: !31)
!563 = !{!432, !564}
!564 = !DILocalVariable(name: "sum", arg: 3, scope: !560, file: !10, line: 120, type: !31)
!565 = !{!434, !566}
!566 = !DILocalVariable(name: "i", arg: 4, scope: !560, file: !10, line: 120, type: !31)
!567 = !{!"pallas.loopInv", !568, ptr @PALLAS_SPEC_50, !33, !33, !569}
!568 = !{!"pallas.srcLoc", i64 121, i64 7, i64 121, i64 83, !36}
!569 = !{!570, !573, !575, !577}
!570 = !{!337, !571}
!571 = !DILocalVariable(name: "p", arg: 1, scope: !572, file: !10, line: 121, type: !342)
!572 = distinct !DISubprogram(name: "PALLAS_SPEC_50", scope: !10, file: !10, line: 121, type: !463, scopeLine: 121, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!573 = !{!348, !574}
!574 = !DILocalVariable(name: "len", arg: 2, scope: !572, file: !10, line: 121, type: !31)
!575 = !{!432, !576}
!576 = !DILocalVariable(name: "sum", arg: 3, scope: !572, file: !10, line: 121, type: !31)
!577 = !{!434, !578}
!578 = !DILocalVariable(name: "i", arg: 4, scope: !572, file: !10, line: 121, type: !31)
!579 = !DILocation(line: 127, column: 12, scope: !323)
!580 = !DILocation(line: 127, column: 16, scope: !323)
!581 = !DILocation(line: 127, column: 15, scope: !323)
!582 = !DILocation(line: 127, column: 5, scope: !323)
!583 = distinct !DISubprogram(name: "main", scope: !10, file: !10, line: 131, type: !584, scopeLine: 131, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!584 = !DISubroutineType(types: !585)
!585 = !{!31}
!586 = !DILocalVariable(name: "p", scope: !583, file: !10, line: 132, type: !27)
!587 = !DILocation(line: 132, column: 11, scope: !583)
!588 = !DILocalVariable(name: "pp", scope: !583, file: !10, line: 133, type: !26)
!589 = !DILocation(line: 133, column: 12, scope: !583)
!590 = !DILocation(line: 134, column: 8, scope: !583)
!591 = !DILocation(line: 138, column: 7, scope: !583)
!592 = !{!593, !594}
!593 = !{!"pallas.srcLoc", i64 136, i64 5, i64 136, i64 29, !36}
!594 = !{!"pallas.assert", !595, ptr @PALLAS_SPEC_51, !33, !33, !596}
!595 = !{!"pallas.srcLoc", i64 136, i64 9, i64 136, i64 27, !36}
!596 = !{!597, !602}
!597 = !{!586, !598}
!598 = !DILocalVariable(name: "p", arg: 1, scope: !599, file: !10, line: 136, type: !48)
!599 = distinct !DISubprogram(name: "PALLAS_SPEC_51", scope: !10, file: !10, line: 136, type: !600, scopeLine: 136, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!600 = !DISubroutineType(types: !601)
!601 = !{!46, !48, !47}
!602 = !{!588, !603}
!603 = !DILocalVariable(name: "pp", arg: 2, scope: !599, file: !10, line: 136, type: !47)
!604 = !DILocation(line: 138, column: 9, scope: !583)
!605 = !DILocation(line: 139, column: 7, scope: !583)
!606 = !DILocation(line: 139, column: 9, scope: !583)
!607 = !DILocation(line: 142, column: 5, scope: !583)
!608 = !{!609, !610, !618}
!609 = !{!"pallas.srcLoc", i64 140, i64 5, i64 141, i64 24, !36}
!610 = !{!"pallas.assert", !611, ptr @PALLAS_SPEC_52, !33, !33, !612}
!611 = !{!"pallas.srcLoc", i64 140, i64 9, i64 140, i64 26, !36}
!612 = !{!613, !616}
!613 = !{!586, !614}
!614 = !DILocalVariable(name: "p", arg: 1, scope: !615, file: !10, line: 140, type: !48)
!615 = distinct !DISubprogram(name: "PALLAS_SPEC_52", scope: !10, file: !10, line: 140, type: !600, scopeLine: 140, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!616 = !{!588, !617}
!617 = !DILocalVariable(name: "pp", arg: 2, scope: !615, file: !10, line: 140, type: !47)
!618 = !{!"pallas.assert", !619, ptr @PALLAS_SPEC_53, !33, !33, !620}
!619 = !{!"pallas.srcLoc", i64 141, i64 5, i64 141, i64 22, !36}
!620 = !{!621, !624}
!621 = !{!586, !622}
!622 = !DILocalVariable(name: "p", arg: 1, scope: !623, file: !10, line: 141, type: !48)
!623 = distinct !DISubprogram(name: "PALLAS_SPEC_53", scope: !10, file: !10, line: 141, type: !600, scopeLine: 141, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!624 = !{!588, !625}
!625 = !DILocalVariable(name: "pp", arg: 2, scope: !623, file: !10, line: 141, type: !47)
!626 = !DILocation(line: 146, column: 18, scope: !583)
!627 = !{!628, !629, !637}
!628 = !{!"pallas.srcLoc", i64 143, i64 5, i64 144, i64 22, !36}
!629 = !{!"pallas.assert", !630, ptr @PALLAS_SPEC_54, !33, !33, !631}
!630 = !{!"pallas.srcLoc", i64 143, i64 9, i64 143, i64 24, !36}
!631 = !{!632, !635}
!632 = !{!586, !633}
!633 = !DILocalVariable(name: "p", arg: 1, scope: !634, file: !10, line: 143, type: !48)
!634 = distinct !DISubprogram(name: "PALLAS_SPEC_54", scope: !10, file: !10, line: 143, type: !600, scopeLine: 143, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!635 = !{!588, !636}
!636 = !DILocalVariable(name: "pp", arg: 2, scope: !634, file: !10, line: 143, type: !47)
!637 = !{!"pallas.assert", !638, ptr @PALLAS_SPEC_55, !33, !33, !639}
!638 = !{!"pallas.srcLoc", i64 144, i64 5, i64 144, i64 20, !36}
!639 = !{!640, !643}
!640 = !{!586, !641}
!641 = !DILocalVariable(name: "p", arg: 1, scope: !642, file: !10, line: 144, type: !48)
!642 = distinct !DISubprogram(name: "PALLAS_SPEC_55", scope: !10, file: !10, line: 144, type: !600, scopeLine: 144, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!643 = !{!588, !644}
!644 = !DILocalVariable(name: "pp", arg: 2, scope: !642, file: !10, line: 144, type: !47)
!645 = !DILocation(line: 146, column: 5, scope: !583)
!646 = !DILocation(line: 149, column: 20, scope: !583)
!647 = !{!648, !649, !657}
!648 = !{!"pallas.srcLoc", i64 147, i64 5, i64 148, i64 22, !36}
!649 = !{!"pallas.assert", !650, ptr @PALLAS_SPEC_56, !33, !33, !651}
!650 = !{!"pallas.srcLoc", i64 147, i64 9, i64 147, i64 26, !36}
!651 = !{!652, !655}
!652 = !{!586, !653}
!653 = !DILocalVariable(name: "p", arg: 1, scope: !654, file: !10, line: 147, type: !48)
!654 = distinct !DISubprogram(name: "PALLAS_SPEC_56", scope: !10, file: !10, line: 147, type: !600, scopeLine: 147, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!655 = !{!588, !656}
!656 = !DILocalVariable(name: "pp", arg: 2, scope: !654, file: !10, line: 147, type: !47)
!657 = !{!"pallas.assert", !658, ptr @PALLAS_SPEC_57, !33, !33, !659}
!658 = !{!"pallas.srcLoc", i64 148, i64 5, i64 148, i64 20, !36}
!659 = !{!660, !663}
!660 = !{!586, !661}
!661 = !DILocalVariable(name: "p", arg: 1, scope: !662, file: !10, line: 148, type: !48)
!662 = distinct !DISubprogram(name: "PALLAS_SPEC_57", scope: !10, file: !10, line: 148, type: !600, scopeLine: 148, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!663 = !{!588, !664}
!664 = !DILocalVariable(name: "pp", arg: 2, scope: !662, file: !10, line: 148, type: !47)
!665 = !DILocation(line: 149, column: 5, scope: !583)
!666 = !DILocalVariable(name: "p1", scope: !583, file: !10, line: 152, type: !27)
!667 = !DILocation(line: 152, column: 11, scope: !583)
!668 = !DILocalVariable(name: "p2", scope: !583, file: !10, line: 152, type: !27)
!669 = !DILocation(line: 152, column: 15, scope: !583)
!670 = !DILocalVariable(name: "p3", scope: !583, file: !10, line: 152, type: !27)
!671 = !DILocation(line: 152, column: 19, scope: !583)
!672 = !DILocation(line: 153, column: 8, scope: !583)
!673 = !{!674, !675}
!674 = !{!"pallas.srcLoc", i64 150, i64 5, i64 150, i64 38, !36}
!675 = !{!"pallas.assert", !676, ptr @PALLAS_SPEC_58, !33, !33, !677}
!676 = !{!"pallas.srcLoc", i64 150, i64 9, i64 150, i64 36, !36}
!677 = !{!678, !683, !685, !687, !689}
!678 = !{!586, !679}
!679 = !DILocalVariable(name: "p", arg: 1, scope: !680, file: !10, line: 150, type: !48)
!680 = distinct !DISubprogram(name: "PALLAS_SPEC_58", scope: !10, file: !10, line: 150, type: !681, scopeLine: 150, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!681 = !DISubroutineType(types: !682)
!682 = !{!46, !48, !47, !48, !48, !48}
!683 = !{!588, !684}
!684 = !DILocalVariable(name: "pp", arg: 2, scope: !680, file: !10, line: 150, type: !47)
!685 = !{!666, !686}
!686 = !DILocalVariable(name: "p1", arg: 3, scope: !680, file: !10, line: 150, type: !48)
!687 = !{!668, !688}
!688 = !DILocalVariable(name: "p2", arg: 4, scope: !680, file: !10, line: 150, type: !48)
!689 = !{!670, !690}
!690 = !DILocalVariable(name: "p3", arg: 5, scope: !680, file: !10, line: 150, type: !48)
!691 = !DILocation(line: 153, column: 10, scope: !583)
!692 = !DILocation(line: 153, column: 18, scope: !583)
!693 = !DILocation(line: 153, column: 20, scope: !583)
!694 = !DILocation(line: 154, column: 8, scope: !583)
!695 = !DILocation(line: 154, column: 10, scope: !583)
!696 = !DILocation(line: 154, column: 18, scope: !583)
!697 = !DILocation(line: 154, column: 20, scope: !583)
!698 = !DILocation(line: 155, column: 8, scope: !583)
!699 = !DILocation(line: 155, column: 10, scope: !583)
!700 = !DILocation(line: 155, column: 18, scope: !583)
!701 = !DILocation(line: 155, column: 20, scope: !583)
!702 = !DILocalVariable(name: "r", scope: !583, file: !10, line: 156, type: !267)
!703 = !DILocation(line: 156, column: 14, scope: !583)
!704 = !DILocalVariable(name: "rr", scope: !583, file: !10, line: 156, type: !266)
!705 = !DILocation(line: 156, column: 18, scope: !583)
!706 = !DILocation(line: 157, column: 8, scope: !583)
!707 = !DILocation(line: 158, column: 7, scope: !583)
!708 = !DILocation(line: 158, column: 12, scope: !583)
!709 = !DILocation(line: 159, column: 7, scope: !583)
!710 = !DILocation(line: 159, column: 12, scope: !583)
!711 = !DILocation(line: 160, column: 7, scope: !583)
!712 = !DILocation(line: 160, column: 12, scope: !583)
!713 = !DILocalVariable(name: "ps", scope: !583, file: !10, line: 162, type: !714)
!714 = !DICompositeType(tag: DW_TAG_array_type, baseType: !27, size: 192, elements: !715)
!715 = !{!716}
!716 = !DISubrange(count: 3)
!717 = !DILocation(line: 162, column: 11, scope: !583)
!718 = !DILocation(line: 162, column: 19, scope: !583)
!719 = !{!720, !721}
!720 = !{!"pallas.srcLoc", i64 161, i64 5, i64 161, i64 32, !36}
!721 = !{!"pallas.assert", !722, ptr @PALLAS_SPEC_59, !33, !33, !723}
!722 = !{!"pallas.srcLoc", i64 161, i64 9, i64 161, i64 30, !36}
!723 = !{!724, !729, !731, !733, !735, !737, !739, !741}
!724 = !{!586, !725}
!725 = !DILocalVariable(name: "p", arg: 1, scope: !726, file: !10, line: 161, type: !48)
!726 = distinct !DISubprogram(name: "PALLAS_SPEC_59", scope: !10, file: !10, line: 161, type: !727, scopeLine: 161, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!727 = !DISubroutineType(types: !728)
!728 = !{!46, !48, !47, !48, !48, !48, !285, !284, !47}
!729 = !{!588, !730}
!730 = !DILocalVariable(name: "pp", arg: 2, scope: !726, file: !10, line: 161, type: !47)
!731 = !{!666, !732}
!732 = !DILocalVariable(name: "p1", arg: 3, scope: !726, file: !10, line: 161, type: !48)
!733 = !{!668, !734}
!734 = !DILocalVariable(name: "p2", arg: 4, scope: !726, file: !10, line: 161, type: !48)
!735 = !{!670, !736}
!736 = !DILocalVariable(name: "p3", arg: 5, scope: !726, file: !10, line: 161, type: !48)
!737 = !{!702, !738}
!738 = !DILocalVariable(name: "r", arg: 6, scope: !726, file: !10, line: 161, type: !285)
!739 = !{!704, !740}
!740 = !DILocalVariable(name: "rr", arg: 7, scope: !726, file: !10, line: 161, type: !284)
!741 = !{!713, !742}
!742 = !DILocalVariable(name: "ps", arg: 8, scope: !726, file: !10, line: 161, type: !47)
!743 = !DILocation(line: 162, column: 20, scope: !583)
!744 = !DILocation(line: 162, column: 24, scope: !583)
!745 = !DILocation(line: 162, column: 28, scope: !583)
!746 = !DILocalVariable(name: "pol", scope: !583, file: !10, line: 163, type: !327)
!747 = !DILocation(line: 163, column: 13, scope: !583)
!748 = !DILocalVariable(name: "ppols", scope: !583, file: !10, line: 163, type: !326)
!749 = !DILocation(line: 163, column: 19, scope: !583)
!750 = !DILocation(line: 164, column: 11, scope: !583)
!751 = !DILocation(line: 165, column: 14, scope: !583)
!752 = !DILocation(line: 165, column: 9, scope: !583)
!753 = !DILocation(line: 165, column: 12, scope: !583)
!754 = !DILocalVariable(name: "avr_pol", scope: !583, file: !10, line: 166, type: !31)
!755 = !DILocation(line: 166, column: 9, scope: !583)
!756 = !DILocation(line: 166, column: 29, scope: !583)
!757 = !DILocation(line: 166, column: 19, scope: !583)
!758 = !DILocation(line: 169, column: 5, scope: !583)
!759 = !{!760, !761}
!760 = !{!"pallas.srcLoc", i64 167, i64 5, i64 167, i64 30, !36}
!761 = !{!"pallas.assert", !762, ptr @PALLAS_SPEC_60, !33, !33, !763}
!762 = !{!"pallas.srcLoc", i64 167, i64 9, i64 167, i64 28, !36}
!763 = !{!764, !769, !771, !773, !775, !777, !779, !781, !783, !785, !787}
!764 = !{!586, !765}
!765 = !DILocalVariable(name: "p", arg: 1, scope: !766, file: !10, line: 167, type: !48)
!766 = distinct !DISubprogram(name: "PALLAS_SPEC_60", scope: !10, file: !10, line: 167, type: !767, scopeLine: 167, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!767 = !DISubroutineType(types: !768)
!768 = !{!46, !48, !47, !48, !48, !48, !285, !284, !47, !343, !342, !31}
!769 = !{!588, !770}
!770 = !DILocalVariable(name: "pp", arg: 2, scope: !766, file: !10, line: 167, type: !47)
!771 = !{!666, !772}
!772 = !DILocalVariable(name: "p1", arg: 3, scope: !766, file: !10, line: 167, type: !48)
!773 = !{!668, !774}
!774 = !DILocalVariable(name: "p2", arg: 4, scope: !766, file: !10, line: 167, type: !48)
!775 = !{!670, !776}
!776 = !DILocalVariable(name: "p3", arg: 5, scope: !766, file: !10, line: 167, type: !48)
!777 = !{!702, !778}
!778 = !DILocalVariable(name: "r", arg: 6, scope: !766, file: !10, line: 167, type: !285)
!779 = !{!704, !780}
!780 = !DILocalVariable(name: "rr", arg: 7, scope: !766, file: !10, line: 167, type: !284)
!781 = !{!713, !782}
!782 = !DILocalVariable(name: "ps", arg: 8, scope: !766, file: !10, line: 167, type: !47)
!783 = !{!746, !784}
!784 = !DILocalVariable(name: "pol", arg: 9, scope: !766, file: !10, line: 167, type: !343)
!785 = !{!748, !786}
!786 = !DILocalVariable(name: "ppols", arg: 10, scope: !766, file: !10, line: 167, type: !342)
!787 = !{!754, !788}
!788 = !DILocalVariable(name: "avr_pol", arg: 11, scope: !766, file: !10, line: 167, type: !31)
!789 = !{!""}
!790 = !DILocation(line: 0, scope: !43)
!791 = !DILocation(line: 27, column: 16, scope: !43)
!792 = !DILocation(line: 0, scope: !58)
!793 = !DILocation(line: 28, column: 24, scope: !58)
!794 = !DILocation(line: 28, column: 27, scope: !58)
!795 = !DILocation(line: 28, column: 14, scope: !58)
!796 = !DILocation(line: 0, scope: !64)
!797 = !DILocation(line: 29, column: 24, scope: !64)
!798 = !DILocation(line: 29, column: 27, scope: !64)
!799 = !DILocation(line: 29, column: 14, scope: !64)
!800 = !DILocation(line: 0, scope: !70)
!801 = !DILocation(line: 30, column: 23, scope: !70)
!802 = !DILocation(line: 30, column: 26, scope: !70)
!803 = !DILocation(line: 30, column: 13, scope: !70)
!804 = !DILocation(line: 0, scope: !76)
!805 = !DILocation(line: 31, column: 23, scope: !76)
!806 = !DILocation(line: 31, column: 26, scope: !76)
!807 = !DILocation(line: 31, column: 13, scope: !76)
!808 = !DILocation(line: 0, scope: !82)
!809 = !DILocation(line: 32, column: 16, scope: !82)
!810 = !DILocation(line: 32, column: 18, scope: !82)
!811 = !DILocation(line: 0, scope: !88)
!812 = !DILocation(line: 33, column: 16, scope: !88)
!813 = !DILocation(line: 33, column: 18, scope: !88)
!814 = !DILocation(line: 0, scope: !94)
!815 = !DILocation(line: 34, column: 13, scope: !94)
!816 = !DILocation(line: 34, column: 32, scope: !94)
!817 = !DILocation(line: 0, scope: !112)
!818 = !DILocation(line: 42, column: 16, scope: !112)
!819 = !DILocation(line: 0, scope: !118)
!820 = !DILocation(line: 43, column: 24, scope: !118)
!821 = !DILocation(line: 43, column: 27, scope: !118)
!822 = !DILocation(line: 43, column: 14, scope: !118)
!823 = !DILocation(line: 0, scope: !124)
!824 = !DILocation(line: 44, column: 24, scope: !124)
!825 = !DILocation(line: 44, column: 27, scope: !124)
!826 = !DILocation(line: 44, column: 14, scope: !124)
!827 = !DILocation(line: 0, scope: !130)
!828 = !DILocation(line: 45, column: 23, scope: !130)
!829 = !DILocation(line: 45, column: 26, scope: !130)
!830 = !DILocation(line: 45, column: 13, scope: !130)
!831 = !DILocation(line: 0, scope: !136)
!832 = !DILocation(line: 46, column: 23, scope: !136)
!833 = !DILocation(line: 46, column: 26, scope: !136)
!834 = !DILocation(line: 46, column: 13, scope: !136)
!835 = !DILocation(line: 0, scope: !142)
!836 = !DILocation(line: 47, column: 16, scope: !142)
!837 = !DILocation(line: 47, column: 18, scope: !142)
!838 = !DILocation(line: 0, scope: !148)
!839 = !DILocation(line: 48, column: 16, scope: !148)
!840 = !DILocation(line: 48, column: 18, scope: !148)
!841 = !DILocation(line: 0, scope: !154)
!842 = !DILocation(line: 49, column: 13, scope: !154)
!843 = !DILocation(line: 49, column: 32, scope: !154)
!844 = !DILocation(line: 0, scope: !172)
!845 = !DILocation(line: 57, column: 16, scope: !172)
!846 = !DILocation(line: 0, scope: !178)
!847 = !DILocation(line: 58, column: 25, scope: !178)
!848 = !DILocation(line: 58, column: 14, scope: !178)
!849 = !DILocation(line: 0, scope: !184)
!850 = !DILocation(line: 59, column: 24, scope: !184)
!851 = !DILocation(line: 59, column: 13, scope: !184)
!852 = !DILocation(line: 0, scope: !190)
!853 = !DILocation(line: 60, column: 16, scope: !190)
!854 = !DILocation(line: 60, column: 34, scope: !190)
!855 = !DILocation(line: 60, column: 36, scope: !190)
!856 = !DILocation(line: 60, column: 21, scope: !190)
!857 = !DILocation(line: 60, column: 18, scope: !190)
!858 = !DILocation(line: 0, scope: !196)
!859 = !DILocation(line: 61, column: 16, scope: !196)
!860 = !DILocation(line: 61, column: 34, scope: !196)
!861 = !DILocation(line: 61, column: 36, scope: !196)
!862 = !DILocation(line: 61, column: 21, scope: !196)
!863 = !DILocation(line: 61, column: 18, scope: !196)
!864 = !DILocation(line: 0, scope: !202)
!865 = !DILocation(line: 62, column: 13, scope: !202)
!866 = !DILocation(line: 62, column: 32, scope: !202)
!867 = !DILocation(line: 0, scope: !228)
!868 = !DILocation(line: 70, column: 21, scope: !228)
!869 = !DILocation(line: 70, column: 24, scope: !228)
!870 = !DILocation(line: 70, column: 12, scope: !228)
!871 = !DILocation(line: 0, scope: !236)
!872 = !DILocation(line: 71, column: 21, scope: !236)
!873 = !DILocation(line: 71, column: 24, scope: !236)
!874 = !DILocation(line: 71, column: 12, scope: !236)
!875 = !DILocation(line: 0, scope: !242)
!876 = !DILocation(line: 72, column: 20, scope: !242)
!877 = !DILocation(line: 72, column: 23, scope: !242)
!878 = !DILocation(line: 72, column: 11, scope: !242)
!879 = !DILocation(line: 0, scope: !248)
!880 = !DILocation(line: 73, column: 20, scope: !248)
!881 = !DILocation(line: 73, column: 23, scope: !248)
!882 = !DILocation(line: 73, column: 11, scope: !248)
!883 = !DILocation(line: 0, scope: !281)
!884 = !DILocation(line: 87, column: 14, scope: !281)
!885 = !DILocation(line: 0, scope: !296)
!886 = !DILocation(line: 88, column: 23, scope: !296)
!887 = !DILocation(line: 88, column: 12, scope: !296)
!888 = !DILocation(line: 0, scope: !302)
!889 = !DILocation(line: 89, column: 22, scope: !302)
!890 = !DILocation(line: 89, column: 11, scope: !302)
!891 = !DILocation(line: 0, scope: !308)
!892 = !DILocation(line: 90, column: 11, scope: !308)
!893 = !DILocation(line: 90, column: 31, scope: !308)
!894 = !DILocation(line: 90, column: 34, scope: !308)
!895 = !DILocation(line: 90, column: 41, scope: !308)
!896 = !DILocation(line: 90, column: 44, scope: !308)
!897 = !DILocation(line: 90, column: 36, scope: !308)
!898 = !DILocation(line: 90, column: 51, scope: !308)
!899 = !DILocation(line: 90, column: 54, scope: !308)
!900 = !DILocation(line: 90, column: 46, scope: !308)
!901 = !DILocation(line: 90, column: 56, scope: !308)
!902 = !DILocation(line: 90, column: 24, scope: !308)
!903 = !DILocation(line: 0, scope: !339)
!904 = !DILocation(line: 97, column: 16, scope: !339)
!905 = !DILocation(line: 0, scope: !355)
!906 = !DILocation(line: 98, column: 14, scope: !355)
!907 = !DILocation(line: 0, scope: !363)
!908 = !DILocation(line: 99, column: 23, scope: !363)
!909 = !DILocation(line: 99, column: 12, scope: !363)
!910 = !DILocation(line: 0, scope: !371)
!911 = !DILocation(line: 100, column: 15, scope: !371)
!912 = !DILocation(line: 100, column: 18, scope: !371)
!913 = !DILocation(line: 100, column: 26, scope: !371)
!914 = !DILocation(line: 100, column: 44, scope: !371)
!915 = !DILocation(line: 100, column: 29, scope: !371)
!916 = !DILocation(line: 100, column: 51, scope: !371)
!917 = !DILocation(line: 100, column: 48, scope: !371)
!918 = !DILocation(line: 0, scope: !379)
!919 = !DILocation(line: 101, column: 30, scope: !379)
!920 = !DILocation(line: 101, column: 27, scope: !379)
!921 = !DILocation(line: 101, column: 48, scope: !379)
!922 = !DILocation(line: 101, column: 60, scope: !379)
!923 = !DILocation(line: 101, column: 77, scope: !379)
!924 = !DILocation(line: 101, column: 74, scope: !379)
!925 = !DILocation(line: 101, column: 90, scope: !379)
!926 = !DILocation(line: 101, column: 102, scope: !379)
!927 = !DILocation(line: 101, column: 67, scope: !379)
!928 = !DILocation(line: 101, column: 43, scope: !379)
!929 = !DILocation(line: 101, column: 20, scope: !379)
!930 = !DILocation(line: 101, column: 119, scope: !379)
!931 = !DILocation(line: 101, column: 134, scope: !379)
!932 = !DILocation(line: 101, column: 131, scope: !379)
!933 = !DILocation(line: 101, column: 150, scope: !379)
!934 = !DILocation(line: 101, column: 155, scope: !379)
!935 = !DILocation(line: 101, column: 153, scope: !379)
!936 = !DILocation(line: 101, column: 173, scope: !379)
!937 = !DILocation(line: 101, column: 178, scope: !379)
!938 = !DILocation(line: 101, column: 176, scope: !379)
!939 = !DILocation(line: 101, column: 167, scope: !379)
!940 = !DILocation(line: 101, column: 112, scope: !379)
!941 = !DILocation(line: 101, column: 12, scope: !379)
!942 = !DILocation(line: 0, scope: !387)
!943 = !DILocation(line: 102, column: 31, scope: !387)
!944 = !DILocation(line: 102, column: 28, scope: !387)
!945 = !DILocation(line: 102, column: 44, scope: !387)
!946 = !DILocation(line: 102, column: 56, scope: !387)
!947 = !DILocation(line: 102, column: 21, scope: !387)
!948 = !DILocation(line: 102, column: 74, scope: !387)
!949 = !DILocation(line: 102, column: 77, scope: !387)
!950 = !DILocation(line: 102, column: 71, scope: !387)
!951 = !DILocation(line: 102, column: 91, scope: !387)
!952 = !DILocation(line: 102, column: 64, scope: !387)
!953 = !DILocation(line: 102, column: 12, scope: !387)
!954 = !DILocation(line: 0, scope: !395)
!955 = !DILocation(line: 103, column: 22, scope: !395)
!956 = !DILocation(line: 103, column: 11, scope: !395)
!957 = !DILocation(line: 0, scope: !403)
!958 = !DILocation(line: 104, column: 14, scope: !403)
!959 = !DILocation(line: 104, column: 17, scope: !403)
!960 = !DILocation(line: 104, column: 25, scope: !403)
!961 = !DILocation(line: 104, column: 43, scope: !403)
!962 = !DILocation(line: 104, column: 28, scope: !403)
!963 = !DILocation(line: 104, column: 50, scope: !403)
!964 = !DILocation(line: 104, column: 47, scope: !403)
!965 = !DILocation(line: 0, scope: !411)
!966 = !DILocation(line: 105, column: 29, scope: !411)
!967 = !DILocation(line: 105, column: 26, scope: !411)
!968 = !DILocation(line: 105, column: 47, scope: !411)
!969 = !DILocation(line: 105, column: 59, scope: !411)
!970 = !DILocation(line: 105, column: 76, scope: !411)
!971 = !DILocation(line: 105, column: 73, scope: !411)
!972 = !DILocation(line: 105, column: 89, scope: !411)
!973 = !DILocation(line: 105, column: 101, scope: !411)
!974 = !DILocation(line: 105, column: 66, scope: !411)
!975 = !DILocation(line: 105, column: 42, scope: !411)
!976 = !DILocation(line: 105, column: 19, scope: !411)
!977 = !DILocation(line: 105, column: 118, scope: !411)
!978 = !DILocation(line: 105, column: 133, scope: !411)
!979 = !DILocation(line: 105, column: 130, scope: !411)
!980 = !DILocation(line: 105, column: 149, scope: !411)
!981 = !DILocation(line: 105, column: 154, scope: !411)
!982 = !DILocation(line: 105, column: 152, scope: !411)
!983 = !DILocation(line: 105, column: 172, scope: !411)
!984 = !DILocation(line: 105, column: 177, scope: !411)
!985 = !DILocation(line: 105, column: 175, scope: !411)
!986 = !DILocation(line: 105, column: 166, scope: !411)
!987 = !DILocation(line: 105, column: 111, scope: !411)
!988 = !DILocation(line: 105, column: 11, scope: !411)
!989 = !DILocation(line: 0, scope: !419)
!990 = !DILocation(line: 106, column: 30, scope: !419)
!991 = !DILocation(line: 106, column: 27, scope: !419)
!992 = !DILocation(line: 106, column: 43, scope: !419)
!993 = !DILocation(line: 106, column: 55, scope: !419)
!994 = !DILocation(line: 106, column: 20, scope: !419)
!995 = !DILocation(line: 106, column: 73, scope: !419)
!996 = !DILocation(line: 106, column: 76, scope: !419)
!997 = !DILocation(line: 106, column: 70, scope: !419)
!998 = !DILocation(line: 106, column: 90, scope: !419)
!999 = !DILocation(line: 106, column: 63, scope: !419)
!1000 = !DILocation(line: 106, column: 11, scope: !419)
!1001 = !DILocation(line: 0, scope: !427)
!1002 = !DILocation(line: 107, column: 22, scope: !427)
!1003 = !DILocation(line: 107, column: 28, scope: !427)
!1004 = !DILocation(line: 107, column: 48, scope: !427)
!1005 = !DILocation(line: 107, column: 45, scope: !427)
!1006 = !DILocation(line: 107, column: 54, scope: !427)
!1007 = !DILocation(line: 107, column: 61, scope: !427)
!1008 = !DILocation(line: 107, column: 58, scope: !427)
!1009 = !DILocation(line: 107, column: 67, scope: !427)
!1010 = !DILocation(line: 107, column: 56, scope: !427)
!1011 = !DILocation(line: 107, column: 74, scope: !427)
!1012 = !DILocation(line: 107, column: 71, scope: !427)
!1013 = !DILocation(line: 107, column: 80, scope: !427)
!1014 = !DILocation(line: 107, column: 69, scope: !427)
!1015 = !DILocation(line: 107, column: 82, scope: !427)
!1016 = !DILocation(line: 107, column: 41, scope: !427)
!1017 = !DILocation(line: 107, column: 11, scope: !427)
!1018 = !DILocation(line: 0, scope: !476)
!1019 = !DILocation(line: 113, column: 24, scope: !476)
!1020 = !DILocation(line: 0, scope: !462)
!1021 = !DILocation(line: 112, column: 23, scope: !462)
!1022 = !DILocation(line: 112, column: 27, scope: !462)
!1023 = !DILocation(line: 112, column: 31, scope: !462)
!1024 = !DILocation(line: 0, scope: !500)
!1025 = !DILocation(line: 115, column: 25, scope: !500)
!1026 = !DILocation(line: 115, column: 28, scope: !500)
!1027 = !DILocation(line: 115, column: 36, scope: !500)
!1028 = !DILocation(line: 115, column: 54, scope: !500)
!1029 = !DILocation(line: 115, column: 39, scope: !500)
!1030 = !DILocation(line: 115, column: 61, scope: !500)
!1031 = !DILocation(line: 115, column: 58, scope: !500)
!1032 = !DILocation(line: 0, scope: !512)
!1033 = !DILocation(line: 116, column: 40, scope: !512)
!1034 = !DILocation(line: 116, column: 37, scope: !512)
!1035 = !DILocation(line: 116, column: 58, scope: !512)
!1036 = !DILocation(line: 116, column: 70, scope: !512)
!1037 = !DILocation(line: 116, column: 87, scope: !512)
!1038 = !DILocation(line: 116, column: 84, scope: !512)
!1039 = !DILocation(line: 116, column: 100, scope: !512)
!1040 = !DILocation(line: 116, column: 112, scope: !512)
!1041 = !DILocation(line: 116, column: 77, scope: !512)
!1042 = !DILocation(line: 116, column: 53, scope: !512)
!1043 = !DILocation(line: 116, column: 30, scope: !512)
!1044 = !DILocation(line: 116, column: 129, scope: !512)
!1045 = !DILocation(line: 116, column: 144, scope: !512)
!1046 = !DILocation(line: 116, column: 141, scope: !512)
!1047 = !DILocation(line: 116, column: 160, scope: !512)
!1048 = !DILocation(line: 116, column: 165, scope: !512)
!1049 = !DILocation(line: 116, column: 163, scope: !512)
!1050 = !DILocation(line: 116, column: 183, scope: !512)
!1051 = !DILocation(line: 116, column: 188, scope: !512)
!1052 = !DILocation(line: 116, column: 186, scope: !512)
!1053 = !DILocation(line: 116, column: 177, scope: !512)
!1054 = !DILocation(line: 116, column: 122, scope: !512)
!1055 = !DILocation(line: 116, column: 22, scope: !512)
!1056 = !DILocation(line: 0, scope: !524)
!1057 = !DILocation(line: 117, column: 41, scope: !524)
!1058 = !DILocation(line: 117, column: 38, scope: !524)
!1059 = !DILocation(line: 117, column: 54, scope: !524)
!1060 = !DILocation(line: 117, column: 66, scope: !524)
!1061 = !DILocation(line: 117, column: 31, scope: !524)
!1062 = !DILocation(line: 117, column: 84, scope: !524)
!1063 = !DILocation(line: 117, column: 87, scope: !524)
!1064 = !DILocation(line: 117, column: 81, scope: !524)
!1065 = !DILocation(line: 117, column: 101, scope: !524)
!1066 = !DILocation(line: 117, column: 74, scope: !524)
!1067 = !DILocation(line: 117, column: 22, scope: !524)
!1068 = !DILocation(line: 0, scope: !536)
!1069 = !DILocation(line: 118, column: 31, scope: !536)
!1070 = !DILocation(line: 118, column: 41, scope: !536)
!1071 = !DILocation(line: 118, column: 22, scope: !536)
!1072 = !DILocation(line: 0, scope: !488)
!1073 = !DILocation(line: 114, column: 33, scope: !488)
!1074 = !DILocation(line: 114, column: 22, scope: !488)
!1075 = !DILocation(line: 0, scope: !548)
!1076 = !DILocation(line: 119, column: 31, scope: !548)
!1077 = !DILocation(line: 119, column: 48, scope: !548)
!1078 = !DILocation(line: 119, column: 45, scope: !548)
!1079 = !DILocation(line: 119, column: 54, scope: !548)
!1080 = !DILocation(line: 119, column: 41, scope: !548)
!1081 = !DILocation(line: 119, column: 22, scope: !548)
!1082 = !DILocation(line: 0, scope: !560)
!1083 = !DILocation(line: 120, column: 31, scope: !560)
!1084 = !DILocation(line: 120, column: 48, scope: !560)
!1085 = !DILocation(line: 120, column: 45, scope: !560)
!1086 = !DILocation(line: 120, column: 54, scope: !560)
!1087 = !DILocation(line: 120, column: 61, scope: !560)
!1088 = !DILocation(line: 120, column: 58, scope: !560)
!1089 = !DILocation(line: 120, column: 67, scope: !560)
!1090 = !DILocation(line: 120, column: 56, scope: !560)
!1091 = !DILocation(line: 120, column: 41, scope: !560)
!1092 = !DILocation(line: 120, column: 22, scope: !560)
!1093 = !DILocation(line: 0, scope: !572)
!1094 = !DILocation(line: 121, column: 31, scope: !572)
!1095 = !DILocation(line: 121, column: 48, scope: !572)
!1096 = !DILocation(line: 121, column: 45, scope: !572)
!1097 = !DILocation(line: 121, column: 54, scope: !572)
!1098 = !DILocation(line: 121, column: 61, scope: !572)
!1099 = !DILocation(line: 121, column: 58, scope: !572)
!1100 = !DILocation(line: 121, column: 67, scope: !572)
!1101 = !DILocation(line: 121, column: 56, scope: !572)
!1102 = !DILocation(line: 121, column: 74, scope: !572)
!1103 = !DILocation(line: 121, column: 71, scope: !572)
!1104 = !DILocation(line: 121, column: 80, scope: !572)
!1105 = !DILocation(line: 121, column: 69, scope: !572)
!1106 = !DILocation(line: 121, column: 41, scope: !572)
!1107 = !DILocation(line: 121, column: 22, scope: !572)
!1108 = !DILocation(line: 0, scope: !599)
!1109 = !DILocation(line: 136, column: 19, scope: !599)
!1110 = !DILocation(line: 0, scope: !615)
!1111 = !DILocation(line: 140, column: 20, scope: !615)
!1112 = !DILocation(line: 140, column: 22, scope: !615)
!1113 = !DILocation(line: 0, scope: !623)
!1114 = !DILocation(line: 141, column: 16, scope: !623)
!1115 = !DILocation(line: 141, column: 18, scope: !623)
!1116 = !DILocation(line: 0, scope: !634)
!1117 = !DILocation(line: 143, column: 18, scope: !634)
!1118 = !DILocation(line: 143, column: 20, scope: !634)
!1119 = !DILocation(line: 0, scope: !642)
!1120 = !DILocation(line: 144, column: 14, scope: !642)
!1121 = !DILocation(line: 144, column: 16, scope: !642)
!1122 = !DILocation(line: 0, scope: !654)
!1123 = !DILocation(line: 147, column: 20, scope: !654)
!1124 = !DILocation(line: 147, column: 22, scope: !654)
!1125 = !DILocation(line: 0, scope: !662)
!1126 = !DILocation(line: 148, column: 14, scope: !662)
!1127 = !DILocation(line: 148, column: 16, scope: !662)
!1128 = !DILocation(line: 0, scope: !680)
!1129 = !DILocation(line: 150, column: 18, scope: !680)
!1130 = !DILocation(line: 150, column: 20, scope: !680)
!1131 = !DILocation(line: 150, column: 25, scope: !680)
!1132 = !DILocation(line: 150, column: 30, scope: !680)
!1133 = !DILocation(line: 150, column: 32, scope: !680)
!1134 = !DILocation(line: 0, scope: !726)
!1135 = !DILocation(line: 161, column: 16, scope: !726)
!1136 = !DILocation(line: 161, column: 26, scope: !726)
!1137 = !DILocation(line: 0, scope: !766)
!1138 = !DILocation(line: 167, column: 24, scope: !766)
!1139 = !{!"pallas.old"}
!1140 = !{!"pallas.result"}
!1141 = !{!"pallas.ptrLength"}
!1142 = !{!"pallas.forall"}
!1143 = !{!"pallas.forallSep"}
!1144 = !{!"pallas.scAnd"}
!1145 = !{!"pallas.boundVar"}
!1146 = !{!"pallas.perm"}
!1147 = !{!"pallas.fracOf"}
!1148 = !{!"pallas.imply"}
