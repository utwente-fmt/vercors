; ModuleID = 'tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/structs.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.point = type { i32, i32 }
%struct.triangle = type { %struct.point, %struct.point, %struct.point }
%struct.polygon = type { ptr }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [64 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_15, ptr @PALLAS_SPEC_16, ptr @PALLAS_SPEC_17, ptr @PALLAS_SPEC_18, ptr @PALLAS_SPEC_19, ptr @PALLAS_SPEC_20, ptr @PALLAS_SPEC_21, ptr @PALLAS_SPEC_22, ptr @PALLAS_SPEC_23, ptr @PALLAS_SPEC_24, ptr @PALLAS_SPEC_25, ptr @PALLAS_SPEC_26, ptr @PALLAS_SPEC_27, ptr @PALLAS_SPEC_28, ptr @PALLAS_SPEC_29, ptr @PALLAS_SPEC_30, ptr @PALLAS_SPEC_31, ptr @PALLAS_SPEC_32, ptr @PALLAS_SPEC_33, ptr @PALLAS_SPEC_34, ptr @PALLAS_SPEC_35, ptr @PALLAS_SPEC_36, ptr @PALLAS_SPEC_37, ptr @PALLAS_SPEC_38, ptr @PALLAS_SPEC_39, ptr @PALLAS_SPEC_40, ptr @PALLAS_SPEC_41, ptr @PALLAS_SPEC_42, ptr @PALLAS_SPEC_43, ptr @PALLAS_SPEC_47, ptr @PALLAS_SPEC_44, ptr @PALLAS_SPEC_45, ptr @PALLAS_SPEC_46, ptr @PALLAS_SPEC_48, ptr @PALLAS_SPEC_49, ptr @PALLAS_SPEC_50, ptr @PALLAS_SPEC_51, ptr @PALLAS_SPEC_52, ptr @PALLAS_SPEC_53, ptr @PALLAS_SPEC_54, ptr @PALLAS_SPEC_55, ptr @PALLAS_SPEC_56, ptr @PALLAS_SPEC_57, ptr @PALLAS_SPEC_58, ptr @PALLAS_SPEC_59, ptr @PALLAS_SPEC_60, ptr @PALLAS_SPEC_61, ptr @PALLAS_SPEC_62, ptr @PALLAS_SPEC_63], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0
@.str.1 = private unnamed_addr constant [2 x i8] c"j\00", align 1, !dbg !7

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_struct(ptr noundef %0) #0 !dbg !23 !pallas.fcontract !34 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !39, metadata !DIExpression()), !dbg !54
  %3 = load ptr, ptr %2, align 8, !dbg !55
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !56
  store i32 0, ptr %4, align 4, !dbg !57
  %5 = load ptr, ptr %2, align 8, !dbg !58
  %6 = getelementptr inbounds %struct.point, ptr %5, i32 0, i32 1, !dbg !59
  store i32 0, ptr %6, align 4, !dbg !60
  ret void, !dbg !61
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_struct2(ptr noundef %0) #0 !dbg !62 !pallas.fcontract !63 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !67, metadata !DIExpression()), !dbg !82
  %3 = load ptr, ptr %2, align 8, !dbg !83
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !84
  store i32 0, ptr %4, align 4, !dbg !85
  %5 = load ptr, ptr %2, align 8, !dbg !86
  %6 = getelementptr inbounds %struct.point, ptr %5, i32 0, i32 1, !dbg !87
  store i32 0, ptr %6, align 4, !dbg !88
  ret void, !dbg !89
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_struct_1(ptr noundef %0) #0 !dbg !90 !pallas.fcontract !91 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !95, metadata !DIExpression()), !dbg !106
  %3 = load ptr, ptr %2, align 8, !dbg !107
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !108
  %5 = load i32, ptr %4, align 4, !dbg !108
  %6 = add nsw i32 %5, 1, !dbg !109
  %7 = load ptr, ptr %2, align 8, !dbg !110
  %8 = getelementptr inbounds %struct.point, ptr %7, i32 0, i32 0, !dbg !111
  store i32 %6, ptr %8, align 4, !dbg !112
  %9 = load ptr, ptr %2, align 8, !dbg !113
  %10 = getelementptr inbounds %struct.point, ptr %9, i32 0, i32 1, !dbg !114
  %11 = load i32, ptr %10, align 4, !dbg !114
  %12 = add nsw i32 %11, 1, !dbg !115
  %13 = load ptr, ptr %2, align 8, !dbg !116
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 1, !dbg !117
  store i32 %12, ptr %14, align 4, !dbg !118
  ret void, !dbg !119
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_copy_struct(i64 %0) #0 !dbg !120 !pallas.fcontract !123 {
  %2 = alloca %struct.point, align 4
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !127, metadata !DIExpression()), !dbg !136
  %3 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !137
  store i32 0, ptr %3, align 4, !dbg !138
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !139
  store i32 0, ptr %4, align 4, !dbg !140
  ret void, !dbg !141
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @alter_copy_struct_2(i64 %0) #0 !dbg !142 !pallas.fcontract !143 {
  %2 = alloca %struct.point, align 4
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !147, metadata !DIExpression()), !dbg !150
  %3 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !151
  store i32 0, ptr %3, align 4, !dbg !152
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !153
  store i32 0, ptr %4, align 4, !dbg !154
  ret void, !dbg !155
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @avr_x(ptr noundef %0) #0 !dbg !156 !pallas.fcontract !166 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !170, metadata !DIExpression()), !dbg !177
  %3 = load ptr, ptr %2, align 8, !dbg !178
  %4 = getelementptr inbounds %struct.triangle, ptr %3, i32 0, i32 0, !dbg !179
  %5 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 0, !dbg !180
  %6 = load i32, ptr %5, align 4, !dbg !180
  %7 = load ptr, ptr %2, align 8, !dbg !181
  %8 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 1, !dbg !182
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !183
  %10 = load i32, ptr %9, align 4, !dbg !183
  %11 = add nsw i32 %6, %10, !dbg !184
  %12 = load ptr, ptr %2, align 8, !dbg !185
  %13 = getelementptr inbounds %struct.triangle, ptr %12, i32 0, i32 2, !dbg !186
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !187
  %15 = load i32, ptr %14, align 4, !dbg !187
  %16 = add nsw i32 %11, %15, !dbg !188
  %17 = sdiv i32 %16, 3, !dbg !189
  ret i32 %17, !dbg !190
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @avr_x_pol(ptr noundef %0, i32 noundef %1) #0 !dbg !191 !pallas.fcontract !199 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !203, metadata !DIExpression()), !dbg !225
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !204, metadata !DIExpression()), !dbg !226
  call void @llvm.dbg.declare(metadata ptr %5, metadata !227, metadata !DIExpression()), !dbg !228
  store i32 0, ptr %5, align 4, !dbg !228
  call void @llvm.dbg.declare(metadata ptr %6, metadata !229, metadata !DIExpression()), !dbg !231
  store i32 0, ptr %6, align 4, !dbg !231
  br label %7, !dbg !232

7:                                                ; preds = %22, %2
  %8 = load i32, ptr %6, align 4, !dbg !233
  %9 = load i32, ptr %4, align 4, !dbg !235
  %10 = icmp slt i32 %8, %9, !dbg !236
  br i1 %10, label %11, label %25, !dbg !237

11:                                               ; preds = %7
  %12 = load ptr, ptr %3, align 8, !dbg !238
  %13 = getelementptr inbounds %struct.polygon, ptr %12, i32 0, i32 0, !dbg !240
  %14 = load ptr, ptr %13, align 8, !dbg !240
  %15 = load i32, ptr %6, align 4, !dbg !241
  %16 = sext i32 %15 to i64, !dbg !238
  %17 = getelementptr inbounds %struct.point, ptr %14, i64 %16, !dbg !238
  %18 = getelementptr inbounds %struct.point, ptr %17, i32 0, i32 0, !dbg !242
  %19 = load i32, ptr %18, align 4, !dbg !242
  %20 = load i32, ptr %5, align 4, !dbg !243
  %21 = add nsw i32 %20, %19, !dbg !243
  store i32 %21, ptr %5, align 4, !dbg !243
  br label %22, !dbg !244

22:                                               ; preds = %11
  %23 = load i32, ptr %6, align 4, !dbg !245
  %24 = add nsw i32 %23, 1, !dbg !245
  store i32 %24, ptr %6, align 4, !dbg !245
  br label %7, !dbg !246, !llvm.loop !247

25:                                               ; preds = %7
  %26 = load i32, ptr %5, align 4, !dbg !272
  %27 = load i32, ptr %4, align 4, !dbg !273
  %28 = sdiv i32 %26, %27, !dbg !274
  ret i32 %28, !dbg !275
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @main() #0 !dbg !276 {
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
  call void @llvm.dbg.declare(metadata ptr %2, metadata !279, metadata !DIExpression()), !dbg !280
  call void @llvm.dbg.declare(metadata ptr %3, metadata !281, metadata !DIExpression()), !dbg !282
  store ptr %2, ptr %3, align 8, !dbg !283
  %13 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !284, !pallas.stmntBlock !285
  store i32 1, ptr %13, align 4, !dbg !289
  %14 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !290
  store i32 2, ptr %14, align 4, !dbg !291
  %15 = load i64, ptr %2, align 4, !dbg !292, !pallas.stmntBlock !293
  call void @alter_copy_struct(i64 %15), !dbg !292
  %16 = load ptr, ptr %3, align 8, !dbg !299, !pallas.stmntBlock !300
  call void @alter_struct(ptr noundef %16), !dbg !306
  %17 = load ptr, ptr %3, align 8, !dbg !307, !pallas.stmntBlock !308
  call void @alter_struct_1(ptr noundef %17), !dbg !314
  call void @llvm.dbg.declare(metadata ptr %4, metadata !315, metadata !DIExpression()), !dbg !316
  call void @llvm.dbg.declare(metadata ptr %5, metadata !317, metadata !DIExpression()), !dbg !318
  call void @llvm.dbg.declare(metadata ptr %6, metadata !319, metadata !DIExpression()), !dbg !320
  %18 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 0, !dbg !321, !pallas.stmntBlock !322
  store i32 1, ptr %18, align 4, !dbg !326
  %19 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !327
  store i32 1, ptr %19, align 4, !dbg !328
  %20 = getelementptr inbounds %struct.point, ptr %5, i32 0, i32 0, !dbg !329
  store i32 2, ptr %20, align 4, !dbg !330
  %21 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !331
  store i32 2, ptr %21, align 4, !dbg !332
  %22 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !333
  store i32 3, ptr %22, align 4, !dbg !334
  %23 = getelementptr inbounds %struct.point, ptr %4, i32 0, i32 1, !dbg !335
  store i32 3, ptr %23, align 4, !dbg !336
  call void @llvm.dbg.declare(metadata ptr %7, metadata !337, metadata !DIExpression()), !dbg !338
  call void @llvm.dbg.declare(metadata ptr %8, metadata !339, metadata !DIExpression()), !dbg !340
  store ptr %7, ptr %8, align 8, !dbg !341
  %24 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 0, !dbg !342
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %24, ptr align 4 %4, i64 8, i1 false), !dbg !343
  %25 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 1, !dbg !344
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %25, ptr align 4 %5, i64 8, i1 false), !dbg !345
  %26 = getelementptr inbounds %struct.triangle, ptr %7, i32 0, i32 2, !dbg !346
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %26, ptr align 4 %6, i64 8, i1 false), !dbg !347
  call void @llvm.dbg.declare(metadata ptr %9, metadata !348, metadata !DIExpression()), !dbg !352
  %27 = getelementptr inbounds [3 x %struct.point], ptr %9, i64 0, i64 0, !dbg !353, !pallas.stmntBlock !354
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 4 %4, i64 8, i1 false), !dbg !358
  %28 = getelementptr inbounds %struct.point, ptr %27, i64 1, !dbg !353
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %28, ptr align 4 %5, i64 8, i1 false), !dbg !359
  %29 = getelementptr inbounds %struct.point, ptr %28, i64 1, !dbg !353
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %29, ptr align 4 %6, i64 8, i1 false), !dbg !360
  call void @llvm.dbg.declare(metadata ptr %10, metadata !361, metadata !DIExpression()), !dbg !362
  call void @llvm.dbg.declare(metadata ptr %11, metadata !363, metadata !DIExpression()), !dbg !364
  store ptr %10, ptr %11, align 8, !dbg !365
  %30 = getelementptr inbounds [3 x %struct.point], ptr %9, i64 0, i64 0, !dbg !366
  %31 = getelementptr inbounds %struct.polygon, ptr %10, i32 0, i32 0, !dbg !367
  store ptr %30, ptr %31, align 8, !dbg !368
  call void @llvm.dbg.declare(metadata ptr %12, metadata !369, metadata !DIExpression()), !dbg !370
  %32 = load ptr, ptr %11, align 8, !dbg !371
  %33 = call i32 @avr_x_pol(ptr noundef %32, i32 noundef 3), !dbg !372
  store i32 %33, ptr %12, align 4, !dbg !370
  ret i32 0, !dbg !373, !pallas.stmntBlock !374
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #2

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !378 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !389, metadata !DIExpression()), !dbg !390
  %2 = icmp ne ptr %0, null, !dbg !391
  ret i1 %2, !dbg !390
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !392 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !393, metadata !DIExpression()), !dbg !394
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !395
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !396
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !397
  ret i1 %4, !dbg !394
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !398 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !399, metadata !DIExpression()), !dbg !400
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !401
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !402
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !403
  ret i1 %4, !dbg !400
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !404 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !405, metadata !DIExpression()), !dbg !406
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !407
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !408
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !409
  ret i1 %4, !dbg !406
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0) #0 !dbg !410 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !411, metadata !DIExpression()), !dbg !412
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !413
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !414
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !415
  ret i1 %4, !dbg !412
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0) #0 !dbg !416 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !417, metadata !DIExpression()), !dbg !418
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !419
  %3 = load i32, ptr %2, align 4, !dbg !419
  %4 = icmp eq i32 %3, 0, !dbg !420
  ret i1 %4, !dbg !418
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0) #0 !dbg !421 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !422, metadata !DIExpression()), !dbg !423
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !424
  %3 = load i32, ptr %2, align 4, !dbg !424
  %4 = icmp eq i32 %3, 0, !dbg !425
  ret i1 %4, !dbg !423
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0) #0 !dbg !426 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !427, metadata !DIExpression()), !dbg !428
  %2 = call ptr @pallas.old.1(ptr noundef %0), !dbg !429
  %3 = icmp eq ptr %2, %0, !dbg !430
  ret i1 %3, !dbg !428
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0) #0 !dbg !431 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !432, metadata !DIExpression()), !dbg !433
  %2 = icmp ne ptr %0, null, !dbg !434
  ret i1 %2, !dbg !433
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0) #0 !dbg !435 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !436, metadata !DIExpression()), !dbg !437
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !438
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !439
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !440
  ret i1 %4, !dbg !437
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0) #0 !dbg !441 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !442, metadata !DIExpression()), !dbg !443
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !444
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !445
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !446
  ret i1 %4, !dbg !443
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0) #0 !dbg !447 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !448, metadata !DIExpression()), !dbg !449
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !450
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !451
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !452
  ret i1 %4, !dbg !449
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0) #0 !dbg !453 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !454, metadata !DIExpression()), !dbg !455
  %3 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !456
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !457
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !458
  ret i1 %4, !dbg !455
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0) #0 !dbg !459 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !460, metadata !DIExpression()), !dbg !461
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !462
  %3 = load i32, ptr %2, align 4, !dbg !462
  %4 = icmp eq i32 %3, 0, !dbg !463
  ret i1 %4, !dbg !461
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0) #0 !dbg !464 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !465, metadata !DIExpression()), !dbg !466
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !467
  %3 = load i32, ptr %2, align 4, !dbg !467
  %4 = icmp eq i32 %3, 0, !dbg !468
  ret i1 %4, !dbg !466
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0) #0 !dbg !469 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !470, metadata !DIExpression()), !dbg !471
  %2 = call ptr @pallas.old.1(ptr noundef %0), !dbg !472
  %3 = icmp eq ptr %2, %0, !dbg !473
  ret i1 %3, !dbg !471
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(ptr noundef %0) #0 !dbg !474 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !475, metadata !DIExpression()), !dbg !476
  %2 = icmp ne ptr %0, null, !dbg !477
  ret i1 %2, !dbg !476
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(ptr noundef %0) #0 !dbg !478 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !479, metadata !DIExpression()), !dbg !480
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !481
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !482
  ret i1 %3, !dbg !480
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(ptr noundef %0) #0 !dbg !483 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !484, metadata !DIExpression()), !dbg !485
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !486
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !487
  ret i1 %3, !dbg !485
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_19(ptr noundef %0) #0 !dbg !488 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !489, metadata !DIExpression()), !dbg !490
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !491
  %3 = load i32, ptr %2, align 4, !dbg !491
  %4 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 0, !dbg !492
  %5 = load i32, ptr %4, align 4, !dbg !492
  %6 = add nsw i32 %5, 1, !dbg !493
  %7 = call i32 @pallas.old.0(i32 noundef %6), !dbg !494
  %8 = icmp eq i32 %3, %7, !dbg !495
  ret i1 %8, !dbg !490
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_20(ptr noundef %0) #0 !dbg !496 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !497, metadata !DIExpression()), !dbg !498
  %2 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !499
  %3 = load i32, ptr %2, align 4, !dbg !499
  %4 = getelementptr inbounds %struct.point, ptr %0, i32 0, i32 1, !dbg !500
  %5 = load i32, ptr %4, align 4, !dbg !500
  %6 = add nsw i32 %5, 1, !dbg !501
  %7 = call i32 @pallas.old.0(i32 noundef %6), !dbg !502
  %8 = icmp eq i32 %3, %7, !dbg !503
  ret i1 %8, !dbg !498
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_21(ptr noundef %0) #0 !dbg !504 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !505, metadata !DIExpression()), !dbg !506
  %2 = call ptr @pallas.old.1(ptr noundef %0), !dbg !507
  %3 = icmp eq ptr %2, %0, !dbg !508
  ret i1 %3, !dbg !506
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_22(i64 %0) #0 !dbg !509 !pallas.exprWrapper !388 {
  %2 = alloca %struct.point, align 4
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !512, metadata !DIExpression()), !dbg !513
  %3 = icmp ne ptr %2, null, !dbg !514
  ret i1 %3, !dbg !513
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_23(i64 %0) #0 !dbg !515 !pallas.exprWrapper !388 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !516, metadata !DIExpression()), !dbg !517
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !518
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !519
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !520
  ret i1 %5, !dbg !517
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_24(i64 %0) #0 !dbg !521 !pallas.exprWrapper !388 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !522, metadata !DIExpression()), !dbg !523
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !524
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !525
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !526
  ret i1 %5, !dbg !523
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_25(i64 %0) #0 !dbg !527 !pallas.exprWrapper !388 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !528, metadata !DIExpression()), !dbg !529
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 0, !dbg !530
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !531
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !532
  ret i1 %5, !dbg !529
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_26(i64 %0) #0 !dbg !533 !pallas.exprWrapper !388 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !534, metadata !DIExpression()), !dbg !535
  %4 = getelementptr inbounds %struct.point, ptr %2, i32 0, i32 1, !dbg !536
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !537
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %3), !dbg !538
  ret i1 %5, !dbg !535
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_27(i64 %0) #0 !dbg !539 !pallas.exprWrapper !388 {
  %2 = alloca %struct.point, align 4
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !540, metadata !DIExpression()), !dbg !541
  %3 = icmp ne ptr %2, null, !dbg !542
  ret i1 %3, !dbg !541
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_28(i64 %0) #0 !dbg !543 !pallas.exprWrapper !388 {
  %2 = alloca %struct.point, align 4
  %3 = alloca %pallas.fracT, align 8
  store i64 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !544, metadata !DIExpression()), !dbg !545
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !546
  %4 = call i1 @pallas.perm(ptr noundef %2, ptr noundef byval(%pallas.fracT) %3), !dbg !547
  ret i1 %4, !dbg !545
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_29(ptr noundef %0) #0 !dbg !548 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !558, metadata !DIExpression()), !dbg !559
  %2 = icmp ne ptr %0, null, !dbg !560
  ret i1 %2, !dbg !559
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_30(ptr noundef %0) #0 !dbg !561 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !562, metadata !DIExpression()), !dbg !563
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !564
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !565
  ret i1 %3, !dbg !563
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_31(ptr noundef %0) #0 !dbg !566 !pallas.exprWrapper !388 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !567, metadata !DIExpression()), !dbg !568
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !569
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !570
  ret i1 %3, !dbg !568
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_32(ptr noundef %0) #0 !dbg !571 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !572, metadata !DIExpression()), !dbg !573
  %2 = call i32 @pallas.result.0(), !dbg !574
  %3 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 0, !dbg !575
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !576
  %5 = load i32, ptr %4, align 4, !dbg !576
  %6 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 1, !dbg !577
  %7 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !578
  %8 = load i32, ptr %7, align 4, !dbg !578
  %9 = add nsw i32 %5, %8, !dbg !579
  %10 = getelementptr inbounds %struct.triangle, ptr %0, i32 0, i32 2, !dbg !580
  %11 = getelementptr inbounds %struct.point, ptr %10, i32 0, i32 0, !dbg !581
  %12 = load i32, ptr %11, align 4, !dbg !581
  %13 = add nsw i32 %9, %12, !dbg !582
  %14 = sdiv i32 %13, 3, !dbg !583
  %15 = icmp eq i32 %2, %14, !dbg !584
  ret i1 %15, !dbg !573
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_33(ptr noundef %0, i32 noundef %1) #0 !dbg !585 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !593, metadata !DIExpression()), !dbg !594
  call void @llvm.dbg.value(metadata i32 %1, metadata !595, metadata !DIExpression()), !dbg !594
  %3 = icmp sgt i32 %1, 0, !dbg !596
  ret i1 %3, !dbg !594
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_34(ptr noundef %0, i32 noundef %1) #0 !dbg !597 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !598, metadata !DIExpression()), !dbg !599
  call void @llvm.dbg.value(metadata i32 %1, metadata !600, metadata !DIExpression()), !dbg !599
  %3 = icmp ne ptr %0, null, !dbg !601
  ret i1 %3, !dbg !599
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_35(ptr noundef %0, i32 noundef %1) #0 !dbg !602 !pallas.exprWrapper !388 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !603, metadata !DIExpression()), !dbg !604
  call void @llvm.dbg.value(metadata i32 %1, metadata !605, metadata !DIExpression()), !dbg !604
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !606
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !607
  ret i1 %4, !dbg !604
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_36(ptr noundef %0, i32 noundef %1) #0 !dbg !608 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !609, metadata !DIExpression()), !dbg !610
  call void @llvm.dbg.value(metadata i32 %1, metadata !611, metadata !DIExpression()), !dbg !610
  %3 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !612
  %4 = load ptr, ptr %3, align 8, !dbg !612
  %5 = icmp ne ptr %4, null, !dbg !613
  br i1 %5, label %6, label %12, !dbg !614

6:                                                ; preds = %2
  %7 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !615
  %8 = load ptr, ptr %7, align 8, !dbg !615
  %9 = call i64 @pallas.ptrLength(ptr noundef %8), !dbg !616
  %10 = sext i32 %1 to i64, !dbg !617
  %11 = icmp sge i64 %9, %10, !dbg !618
  br label %12

12:                                               ; preds = %6, %2
  %13 = phi i1 [ false, %2 ], [ %11, %6 ], !dbg !610
  ret i1 %13, !dbg !610
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_37(ptr noundef %0, i32 noundef %1) #0 !dbg !619 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !620, metadata !DIExpression()), !dbg !621
  call void @llvm.dbg.value(metadata i32 %1, metadata !622, metadata !DIExpression()), !dbg !621
  %3 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !623
  %4 = icmp sle i32 0, %3, !dbg !624
  %5 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !625
  %6 = icmp slt i32 %5, %1, !dbg !626
  %7 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !627
  %8 = icmp sle i32 0, %7, !dbg !628
  %9 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !629
  %10 = icmp slt i32 %9, %1, !dbg !630
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !631
  %12 = call i1 @pallas.scAnd(i1 %6, i1 %11), !dbg !632
  %13 = call i1 @pallas.scAnd(i1 %4, i1 %12), !dbg !633
  %14 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !634
  %15 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !635
  %16 = icmp ne i32 %14, %15, !dbg !636
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !637
  %18 = load ptr, ptr %17, align 8, !dbg !637
  %19 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !638
  %20 = sext i32 %19 to i64, !dbg !639
  %21 = getelementptr inbounds %struct.point, ptr %18, i64 %20, !dbg !639
  %22 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !640
  %23 = load ptr, ptr %22, align 8, !dbg !640
  %24 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !641
  %25 = sext i32 %24 to i64, !dbg !642
  %26 = getelementptr inbounds %struct.point, ptr %23, i64 %25, !dbg !642
  %27 = icmp ne ptr %21, %26, !dbg !643
  %28 = call i1 @pallas.imply(i1 %16, i1 %27), !dbg !644
  %29 = call i1 @pallas.forall(i1 %13, i1 %28), !dbg !645
  ret i1 %29, !dbg !621
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_38(ptr noundef %0, i32 noundef %1) #0 !dbg !646 !pallas.exprWrapper !388 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !647, metadata !DIExpression()), !dbg !648
  call void @llvm.dbg.value(metadata i32 %1, metadata !649, metadata !DIExpression()), !dbg !648
  %4 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !650
  %5 = icmp sle i32 0, %4, !dbg !651
  %6 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !652
  %7 = icmp slt i32 %6, %1, !dbg !653
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !654
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !655
  %10 = load ptr, ptr %9, align 8, !dbg !655
  %11 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !656
  %12 = sext i32 %11 to i64, !dbg !657
  %13 = getelementptr inbounds %struct.point, ptr %10, i64 %12, !dbg !657
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !658
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %3), !dbg !659
  %15 = call i1 @pallas.forallSep(i1 %8, i1 %14), !dbg !660
  ret i1 %15, !dbg !648
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_39(ptr noundef %0, i32 noundef %1) #0 !dbg !661 !pallas.exprWrapper !388 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !662, metadata !DIExpression()), !dbg !663
  call void @llvm.dbg.value(metadata i32 %1, metadata !664, metadata !DIExpression()), !dbg !663
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !665
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !666
  ret i1 %4, !dbg !663
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_40(ptr noundef %0, i32 noundef %1) #0 !dbg !667 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !668, metadata !DIExpression()), !dbg !669
  call void @llvm.dbg.value(metadata i32 %1, metadata !670, metadata !DIExpression()), !dbg !669
  %3 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !671
  %4 = load ptr, ptr %3, align 8, !dbg !671
  %5 = icmp ne ptr %4, null, !dbg !672
  br i1 %5, label %6, label %12, !dbg !673

6:                                                ; preds = %2
  %7 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !674
  %8 = load ptr, ptr %7, align 8, !dbg !674
  %9 = call i64 @pallas.ptrLength(ptr noundef %8), !dbg !675
  %10 = sext i32 %1 to i64, !dbg !676
  %11 = icmp sge i64 %9, %10, !dbg !677
  br label %12

12:                                               ; preds = %6, %2
  %13 = phi i1 [ false, %2 ], [ %11, %6 ], !dbg !669
  ret i1 %13, !dbg !669
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_41(ptr noundef %0, i32 noundef %1) #0 !dbg !678 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !679, metadata !DIExpression()), !dbg !680
  call void @llvm.dbg.value(metadata i32 %1, metadata !681, metadata !DIExpression()), !dbg !680
  %3 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !682
  %4 = icmp sle i32 0, %3, !dbg !683
  %5 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !684
  %6 = icmp slt i32 %5, %1, !dbg !685
  %7 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !686
  %8 = icmp sle i32 0, %7, !dbg !687
  %9 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !688
  %10 = icmp slt i32 %9, %1, !dbg !689
  %11 = call i1 @pallas.scAnd(i1 %8, i1 %10), !dbg !690
  %12 = call i1 @pallas.scAnd(i1 %6, i1 %11), !dbg !691
  %13 = call i1 @pallas.scAnd(i1 %4, i1 %12), !dbg !692
  %14 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !693
  %15 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !694
  %16 = icmp ne i32 %14, %15, !dbg !695
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !696
  %18 = load ptr, ptr %17, align 8, !dbg !696
  %19 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !697
  %20 = sext i32 %19 to i64, !dbg !698
  %21 = getelementptr inbounds %struct.point, ptr %18, i64 %20, !dbg !698
  %22 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !699
  %23 = load ptr, ptr %22, align 8, !dbg !699
  %24 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !700
  %25 = sext i32 %24 to i64, !dbg !701
  %26 = getelementptr inbounds %struct.point, ptr %23, i64 %25, !dbg !701
  %27 = icmp ne ptr %21, %26, !dbg !702
  %28 = call i1 @pallas.imply(i1 %16, i1 %27), !dbg !703
  %29 = call i1 @pallas.forall(i1 %13, i1 %28), !dbg !704
  ret i1 %29, !dbg !680
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_42(ptr noundef %0, i32 noundef %1) #0 !dbg !705 !pallas.exprWrapper !388 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !706, metadata !DIExpression()), !dbg !707
  call void @llvm.dbg.value(metadata i32 %1, metadata !708, metadata !DIExpression()), !dbg !707
  %4 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !709
  %5 = icmp sle i32 0, %4, !dbg !710
  %6 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !711
  %7 = icmp slt i32 %6, %1, !dbg !712
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !713
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !714
  %10 = load ptr, ptr %9, align 8, !dbg !714
  %11 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !715
  %12 = sext i32 %11 to i64, !dbg !716
  %13 = getelementptr inbounds %struct.point, ptr %10, i64 %12, !dbg !716
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !717
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %3), !dbg !718
  %15 = call i1 @pallas.forallSep(i1 %8, i1 %14), !dbg !719
  ret i1 %15, !dbg !707
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_43(ptr noundef %0, i32 noundef %1) #0 !dbg !720 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !721, metadata !DIExpression()), !dbg !722
  call void @llvm.dbg.value(metadata i32 %1, metadata !723, metadata !DIExpression()), !dbg !722
  %3 = icmp eq i32 %1, 3, !dbg !724
  %4 = call i32 @pallas.result.0(), !dbg !725
  %5 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !726
  %6 = load ptr, ptr %5, align 8, !dbg !726
  %7 = getelementptr inbounds %struct.point, ptr %6, i64 0, !dbg !727
  %8 = getelementptr inbounds %struct.point, ptr %7, i32 0, i32 0, !dbg !728
  %9 = load i32, ptr %8, align 4, !dbg !728
  %10 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !729
  %11 = load ptr, ptr %10, align 8, !dbg !729
  %12 = getelementptr inbounds %struct.point, ptr %11, i64 1, !dbg !730
  %13 = getelementptr inbounds %struct.point, ptr %12, i32 0, i32 0, !dbg !731
  %14 = load i32, ptr %13, align 4, !dbg !731
  %15 = add nsw i32 %9, %14, !dbg !732
  %16 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !733
  %17 = load ptr, ptr %16, align 8, !dbg !733
  %18 = getelementptr inbounds %struct.point, ptr %17, i64 2, !dbg !734
  %19 = getelementptr inbounds %struct.point, ptr %18, i32 0, i32 0, !dbg !735
  %20 = load i32, ptr %19, align 4, !dbg !735
  %21 = add nsw i32 %15, %20, !dbg !736
  %22 = sdiv i32 %21, %1, !dbg !737
  %23 = icmp eq i32 %4, %22, !dbg !738
  %24 = call i1 @pallas.imply(i1 %3, i1 %23), !dbg !739
  ret i1 %24, !dbg !722
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_47(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !740 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !743, metadata !DIExpression()), !dbg !744
  call void @llvm.dbg.value(metadata i32 %1, metadata !745, metadata !DIExpression()), !dbg !744
  call void @llvm.dbg.value(metadata i32 %2, metadata !746, metadata !DIExpression()), !dbg !744
  call void @llvm.dbg.value(metadata i32 %3, metadata !747, metadata !DIExpression()), !dbg !744
  %5 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !748
  %6 = load ptr, ptr %5, align 8, !dbg !748
  %7 = icmp ne ptr %6, null, !dbg !749
  br i1 %7, label %8, label %14, !dbg !750

8:                                                ; preds = %4
  %9 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !751
  %10 = load ptr, ptr %9, align 8, !dbg !751
  %11 = call i64 @pallas.ptrLength(ptr noundef %10), !dbg !752
  %12 = sext i32 %1 to i64, !dbg !753
  %13 = icmp sge i64 %11, %12, !dbg !754
  br label %14

14:                                               ; preds = %8, %4
  %15 = phi i1 [ false, %4 ], [ %13, %8 ], !dbg !744
  ret i1 %15, !dbg !744
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_44(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !755 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !756, metadata !DIExpression()), !dbg !757
  call void @llvm.dbg.value(metadata i32 %1, metadata !758, metadata !DIExpression()), !dbg !757
  call void @llvm.dbg.value(metadata i32 %2, metadata !759, metadata !DIExpression()), !dbg !757
  call void @llvm.dbg.value(metadata i32 %3, metadata !760, metadata !DIExpression()), !dbg !757
  %5 = icmp sle i32 0, %3, !dbg !761
  br i1 %5, label %6, label %8, !dbg !762

6:                                                ; preds = %4
  %7 = icmp sle i32 %3, %1, !dbg !763
  br label %8

8:                                                ; preds = %6, %4
  %9 = phi i1 [ false, %4 ], [ %7, %6 ], !dbg !757
  ret i1 %9, !dbg !757
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_45(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !764 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !765, metadata !DIExpression()), !dbg !766
  call void @llvm.dbg.value(metadata i32 %1, metadata !767, metadata !DIExpression()), !dbg !766
  call void @llvm.dbg.value(metadata i32 %2, metadata !768, metadata !DIExpression()), !dbg !766
  call void @llvm.dbg.value(metadata i32 %3, metadata !769, metadata !DIExpression()), !dbg !766
  %5 = icmp ne ptr %0, null, !dbg !770
  ret i1 %5, !dbg !766
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_46(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !771 !pallas.exprWrapper !388 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !772, metadata !DIExpression()), !dbg !773
  call void @llvm.dbg.value(metadata i32 %1, metadata !774, metadata !DIExpression()), !dbg !773
  call void @llvm.dbg.value(metadata i32 %2, metadata !775, metadata !DIExpression()), !dbg !773
  call void @llvm.dbg.value(metadata i32 %3, metadata !776, metadata !DIExpression()), !dbg !773
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !777
  %6 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %5), !dbg !778
  ret i1 %6, !dbg !773
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_48(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !779 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !780, metadata !DIExpression()), !dbg !781
  call void @llvm.dbg.value(metadata i32 %1, metadata !782, metadata !DIExpression()), !dbg !781
  call void @llvm.dbg.value(metadata i32 %2, metadata !783, metadata !DIExpression()), !dbg !781
  call void @llvm.dbg.value(metadata i32 %3, metadata !784, metadata !DIExpression()), !dbg !781
  %5 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !785
  %6 = icmp sle i32 0, %5, !dbg !786
  %7 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !787
  %8 = icmp slt i32 %7, %1, !dbg !788
  %9 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !789
  %10 = icmp sle i32 0, %9, !dbg !790
  %11 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !791
  %12 = icmp slt i32 %11, %1, !dbg !792
  %13 = call i1 @pallas.scAnd(i1 %10, i1 %12), !dbg !793
  %14 = call i1 @pallas.scAnd(i1 %8, i1 %13), !dbg !794
  %15 = call i1 @pallas.scAnd(i1 %6, i1 %14), !dbg !795
  %16 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !796
  %17 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !797
  %18 = icmp ne i32 %16, %17, !dbg !798
  %19 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !799
  %20 = load ptr, ptr %19, align 8, !dbg !799
  %21 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !800
  %22 = sext i32 %21 to i64, !dbg !801
  %23 = getelementptr inbounds %struct.point, ptr %20, i64 %22, !dbg !801
  %24 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !802
  %25 = load ptr, ptr %24, align 8, !dbg !802
  %26 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !803
  %27 = sext i32 %26 to i64, !dbg !804
  %28 = getelementptr inbounds %struct.point, ptr %25, i64 %27, !dbg !804
  %29 = icmp ne ptr %23, %28, !dbg !805
  %30 = call i1 @pallas.imply(i1 %18, i1 %29), !dbg !806
  %31 = call i1 @pallas.forall(i1 %15, i1 %30), !dbg !807
  ret i1 %31, !dbg !781
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_49(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !808 !pallas.exprWrapper !388 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !809, metadata !DIExpression()), !dbg !810
  call void @llvm.dbg.value(metadata i32 %1, metadata !811, metadata !DIExpression()), !dbg !810
  call void @llvm.dbg.value(metadata i32 %2, metadata !812, metadata !DIExpression()), !dbg !810
  call void @llvm.dbg.value(metadata i32 %3, metadata !813, metadata !DIExpression()), !dbg !810
  %6 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !814
  %7 = icmp sle i32 0, %6, !dbg !815
  %8 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !816
  %9 = icmp slt i32 %8, %1, !dbg !817
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !818
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !819
  %12 = load ptr, ptr %11, align 8, !dbg !819
  %13 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !820
  %14 = sext i32 %13 to i64, !dbg !821
  %15 = getelementptr inbounds %struct.point, ptr %12, i64 %14, !dbg !821
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !822
  %16 = call i1 @pallas.perm(ptr noundef %15, ptr noundef byval(%pallas.fracT) %5), !dbg !823
  %17 = call i1 @pallas.forallSep(i1 %10, i1 %16), !dbg !824
  ret i1 %17, !dbg !810
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_50(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !825 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !826, metadata !DIExpression()), !dbg !827
  call void @llvm.dbg.value(metadata i32 %1, metadata !828, metadata !DIExpression()), !dbg !827
  call void @llvm.dbg.value(metadata i32 %2, metadata !829, metadata !DIExpression()), !dbg !827
  call void @llvm.dbg.value(metadata i32 %3, metadata !830, metadata !DIExpression()), !dbg !827
  %5 = icmp eq i32 %3, 0, !dbg !831
  %6 = icmp eq i32 %2, 0, !dbg !832
  %7 = call i1 @pallas.imply(i1 %5, i1 %6), !dbg !833
  ret i1 %7, !dbg !827
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_51(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !834 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !835, metadata !DIExpression()), !dbg !836
  call void @llvm.dbg.value(metadata i32 %1, metadata !837, metadata !DIExpression()), !dbg !836
  call void @llvm.dbg.value(metadata i32 %2, metadata !838, metadata !DIExpression()), !dbg !836
  call void @llvm.dbg.value(metadata i32 %3, metadata !839, metadata !DIExpression()), !dbg !836
  %5 = icmp eq i32 %3, 1, !dbg !840
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !841
  %7 = load ptr, ptr %6, align 8, !dbg !841
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !842
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !843
  %10 = load i32, ptr %9, align 4, !dbg !843
  %11 = icmp eq i32 %2, %10, !dbg !844
  %12 = call i1 @pallas.imply(i1 %5, i1 %11), !dbg !845
  ret i1 %12, !dbg !836
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_52(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !846 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !847, metadata !DIExpression()), !dbg !848
  call void @llvm.dbg.value(metadata i32 %1, metadata !849, metadata !DIExpression()), !dbg !848
  call void @llvm.dbg.value(metadata i32 %2, metadata !850, metadata !DIExpression()), !dbg !848
  call void @llvm.dbg.value(metadata i32 %3, metadata !851, metadata !DIExpression()), !dbg !848
  %5 = icmp eq i32 %3, 2, !dbg !852
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !853
  %7 = load ptr, ptr %6, align 8, !dbg !853
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !854
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !855
  %10 = load i32, ptr %9, align 4, !dbg !855
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !856
  %12 = load ptr, ptr %11, align 8, !dbg !856
  %13 = getelementptr inbounds %struct.point, ptr %12, i64 1, !dbg !857
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !858
  %15 = load i32, ptr %14, align 4, !dbg !858
  %16 = add nsw i32 %10, %15, !dbg !859
  %17 = icmp eq i32 %2, %16, !dbg !860
  %18 = call i1 @pallas.imply(i1 %5, i1 %17), !dbg !861
  ret i1 %18, !dbg !848
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_53(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !862 !pallas.exprWrapper !388 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !863, metadata !DIExpression()), !dbg !864
  call void @llvm.dbg.value(metadata i32 %1, metadata !865, metadata !DIExpression()), !dbg !864
  call void @llvm.dbg.value(metadata i32 %2, metadata !866, metadata !DIExpression()), !dbg !864
  call void @llvm.dbg.value(metadata i32 %3, metadata !867, metadata !DIExpression()), !dbg !864
  %5 = icmp eq i32 %3, 3, !dbg !868
  %6 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !869
  %7 = load ptr, ptr %6, align 8, !dbg !869
  %8 = getelementptr inbounds %struct.point, ptr %7, i64 0, !dbg !870
  %9 = getelementptr inbounds %struct.point, ptr %8, i32 0, i32 0, !dbg !871
  %10 = load i32, ptr %9, align 4, !dbg !871
  %11 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !872
  %12 = load ptr, ptr %11, align 8, !dbg !872
  %13 = getelementptr inbounds %struct.point, ptr %12, i64 1, !dbg !873
  %14 = getelementptr inbounds %struct.point, ptr %13, i32 0, i32 0, !dbg !874
  %15 = load i32, ptr %14, align 4, !dbg !874
  %16 = add nsw i32 %10, %15, !dbg !875
  %17 = getelementptr inbounds %struct.polygon, ptr %0, i32 0, i32 0, !dbg !876
  %18 = load ptr, ptr %17, align 8, !dbg !876
  %19 = getelementptr inbounds %struct.point, ptr %18, i64 2, !dbg !877
  %20 = getelementptr inbounds %struct.point, ptr %19, i32 0, i32 0, !dbg !878
  %21 = load i32, ptr %20, align 4, !dbg !878
  %22 = add nsw i32 %16, %21, !dbg !879
  %23 = icmp eq i32 %2, %22, !dbg !880
  %24 = call i1 @pallas.imply(i1 %5, i1 %23), !dbg !881
  ret i1 %24, !dbg !864
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_54(i64 %0, ptr noundef %1) #0 !dbg !882 !pallas.exprWrapper !388 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !885, metadata !DIExpression()), !dbg !886
  call void @llvm.dbg.value(metadata ptr %1, metadata !887, metadata !DIExpression()), !dbg !886
  %4 = icmp ne ptr %1, null, !dbg !888
  ret i1 %4, !dbg !886
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_55(i64 %0, ptr noundef %1) #0 !dbg !889 !pallas.exprWrapper !388 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !890, metadata !DIExpression()), !dbg !891
  call void @llvm.dbg.value(metadata ptr %1, metadata !892, metadata !DIExpression()), !dbg !891
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 0, !dbg !893
  %5 = load i32, ptr %4, align 4, !dbg !893
  %6 = icmp eq i32 %5, 1, !dbg !894
  ret i1 %6, !dbg !891
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_56(i64 %0, ptr noundef %1) #0 !dbg !895 !pallas.exprWrapper !388 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !896, metadata !DIExpression()), !dbg !897
  call void @llvm.dbg.value(metadata ptr %1, metadata !898, metadata !DIExpression()), !dbg !897
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 1, !dbg !899
  %5 = load i32, ptr %4, align 4, !dbg !899
  %6 = icmp eq i32 %5, 2, !dbg !900
  ret i1 %6, !dbg !897
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_57(i64 %0, ptr noundef %1) #0 !dbg !901 !pallas.exprWrapper !388 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !902, metadata !DIExpression()), !dbg !903
  call void @llvm.dbg.value(metadata ptr %1, metadata !904, metadata !DIExpression()), !dbg !903
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !905
  %5 = load i32, ptr %4, align 4, !dbg !905
  %6 = icmp eq i32 %5, 1, !dbg !906
  ret i1 %6, !dbg !903
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_58(i64 %0, ptr noundef %1) #0 !dbg !907 !pallas.exprWrapper !388 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !908, metadata !DIExpression()), !dbg !909
  call void @llvm.dbg.value(metadata ptr %1, metadata !910, metadata !DIExpression()), !dbg !909
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 1, !dbg !911
  %5 = load i32, ptr %4, align 4, !dbg !911
  %6 = icmp eq i32 %5, 2, !dbg !912
  ret i1 %6, !dbg !909
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_59(i64 %0, ptr noundef %1) #0 !dbg !913 !pallas.exprWrapper !388 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !914, metadata !DIExpression()), !dbg !915
  call void @llvm.dbg.value(metadata ptr %1, metadata !916, metadata !DIExpression()), !dbg !915
  %4 = getelementptr inbounds %struct.point, ptr %1, i32 0, i32 0, !dbg !917
  %5 = load i32, ptr %4, align 4, !dbg !917
  %6 = icmp eq i32 %5, 0, !dbg !918
  ret i1 %6, !dbg !915
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_60(i64 %0, ptr noundef %1) #0 !dbg !919 !pallas.exprWrapper !388 {
  %3 = alloca %struct.point, align 4
  store i64 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !920, metadata !DIExpression()), !dbg !921
  call void @llvm.dbg.value(metadata ptr %1, metadata !922, metadata !DIExpression()), !dbg !921
  %4 = getelementptr inbounds %struct.point, ptr %3, i32 0, i32 0, !dbg !923
  %5 = load i32, ptr %4, align 4, !dbg !923
  %6 = icmp eq i32 %5, 0, !dbg !924
  ret i1 %6, !dbg !921
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_61(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4) #0 !dbg !925 !pallas.exprWrapper !388 {
  %6 = alloca %struct.point, align 4
  %7 = alloca %struct.point, align 4
  %8 = alloca %struct.point, align 4
  %9 = alloca %struct.point, align 4
  store i64 %0, ptr %6, align 4
  store i64 %2, ptr %7, align 4
  store i64 %3, ptr %8, align 4
  store i64 %4, ptr %9, align 4
  call void @llvm.dbg.declare(metadata ptr %6, metadata !928, metadata !DIExpression()), !dbg !929
  call void @llvm.dbg.value(metadata ptr %1, metadata !930, metadata !DIExpression()), !dbg !929
  call void @llvm.dbg.declare(metadata ptr %7, metadata !931, metadata !DIExpression()), !dbg !929
  call void @llvm.dbg.declare(metadata ptr %8, metadata !932, metadata !DIExpression()), !dbg !929
  call void @llvm.dbg.declare(metadata ptr %9, metadata !933, metadata !DIExpression()), !dbg !929
  %10 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 0, !dbg !934
  %11 = load i32, ptr %10, align 4, !dbg !934
  %12 = icmp eq i32 %11, 1, !dbg !935
  br i1 %12, label %13, label %17, !dbg !936

13:                                               ; preds = %5
  %14 = getelementptr inbounds %struct.point, ptr %6, i32 0, i32 1, !dbg !937
  %15 = load i32, ptr %14, align 4, !dbg !937
  %16 = icmp eq i32 %15, 1, !dbg !938
  br label %17

17:                                               ; preds = %13, %5
  %18 = phi i1 [ false, %5 ], [ %16, %13 ], !dbg !929
  ret i1 %18, !dbg !929
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_62(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4, ptr noundef byval(%struct.triangle) align 8 %5, ptr noundef %6, ptr noundef %7) #0 !dbg !939 !pallas.exprWrapper !388 {
  %9 = alloca %struct.point, align 4
  %10 = alloca %struct.point, align 4
  %11 = alloca %struct.point, align 4
  %12 = alloca %struct.point, align 4
  store i64 %0, ptr %9, align 4
  store i64 %2, ptr %10, align 4
  store i64 %3, ptr %11, align 4
  store i64 %4, ptr %12, align 4
  call void @llvm.dbg.declare(metadata ptr %9, metadata !942, metadata !DIExpression()), !dbg !943
  call void @llvm.dbg.value(metadata ptr %1, metadata !944, metadata !DIExpression()), !dbg !943
  call void @llvm.dbg.declare(metadata ptr %10, metadata !945, metadata !DIExpression()), !dbg !943
  call void @llvm.dbg.declare(metadata ptr %11, metadata !946, metadata !DIExpression()), !dbg !943
  call void @llvm.dbg.declare(metadata ptr %12, metadata !947, metadata !DIExpression()), !dbg !943
  call void @llvm.dbg.declare(metadata ptr %5, metadata !948, metadata !DIExpression()), !dbg !943
  call void @llvm.dbg.value(metadata ptr %6, metadata !949, metadata !DIExpression()), !dbg !943
  call void @llvm.dbg.value(metadata ptr %7, metadata !950, metadata !DIExpression()), !dbg !943
  %13 = call i32 @avr_x(ptr noundef %6), !dbg !951
  %14 = icmp eq i32 %13, 2, !dbg !952
  ret i1 %14, !dbg !943
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_63(i64 %0, ptr noundef %1, i64 %2, i64 %3, i64 %4, ptr noundef byval(%struct.triangle) align 8 %5, ptr noundef %6, ptr noundef %7, i64 %8, ptr noundef %9, i32 noundef %10) #0 !dbg !953 !pallas.exprWrapper !388 {
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
  call void @llvm.dbg.declare(metadata ptr %12, metadata !956, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.value(metadata ptr %1, metadata !958, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.declare(metadata ptr %13, metadata !959, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.declare(metadata ptr %14, metadata !960, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.declare(metadata ptr %15, metadata !961, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.declare(metadata ptr %5, metadata !962, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.value(metadata ptr %6, metadata !963, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.value(metadata ptr %7, metadata !964, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.declare(metadata ptr %16, metadata !965, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.value(metadata ptr %9, metadata !966, metadata !DIExpression()), !dbg !957
  call void @llvm.dbg.value(metadata i32 %10, metadata !967, metadata !DIExpression()), !dbg !957
  %19 = icmp eq i32 %10, 2, !dbg !968
  ret i1 %19, !dbg !957
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !969 i32 @pallas.old.0(i32 noundef)

declare !pallas.specLib !969 ptr @pallas.old.1(ptr noundef)

declare !pallas.specLib !970 i32 @pallas.result.0()

declare !pallas.specLib !971 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !972 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !973 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !974 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !975 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !976 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !977 i32 @pallas.boundVar.0(ptr)

declare !pallas.specLib !978 i1 @pallas.imply(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }

!llvm.dbg.cu = !{!9, !11}
!llvm.module.flags = !{!15, !16, !17, !18, !19, !20, !21}
!llvm.ident = !{!22, !22}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 447, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "3db50ce31b87701b7825d6ce13a36772")
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
!34 = !{!35, i1 false, i1 false, !37, !40, !42, !44, !46, !48, !50, !52}
!35 = !{!"pallas.srcLoc", i64 26, i64 1, i64 35, i64 1, !36}
!36 = !DIFile(filename: "/workspaces/pallas_spec2ir/examples/concepts/llvm/structs.c", directory: "", checksumkind: CSK_MD5, checksum: "818f8498e33117445f7416aff20ec114")
!37 = !{!"pallas.requires", !38, ptr @PALLAS_SPEC_0, !39}
!38 = !{!"pallas.srcLoc", i64 27, i64 5, i64 27, i64 23, !36}
!39 = !DILocalVariable(name: "p", arg: 1, scope: !23, file: !10, line: 36, type: !26)
!40 = !{!"pallas.requires", !41, ptr @PALLAS_SPEC_1, !39}
!41 = !{!"pallas.srcLoc", i64 28, i64 5, i64 28, i64 41, !36}
!42 = !{!"pallas.requires", !43, ptr @PALLAS_SPEC_2, !39}
!43 = !{!"pallas.srcLoc", i64 29, i64 5, i64 29, i64 41, !36}
!44 = !{!"pallas.ensures", !45, ptr @PALLAS_SPEC_3, !39}
!45 = !{!"pallas.srcLoc", i64 30, i64 5, i64 30, i64 40, !36}
!46 = !{!"pallas.ensures", !47, ptr @PALLAS_SPEC_4, !39}
!47 = !{!"pallas.srcLoc", i64 31, i64 5, i64 31, i64 40, !36}
!48 = !{!"pallas.ensures", !49, ptr @PALLAS_SPEC_5, !39}
!49 = !{!"pallas.srcLoc", i64 32, i64 5, i64 32, i64 22, !36}
!50 = !{!"pallas.ensures", !51, ptr @PALLAS_SPEC_6, !39}
!51 = !{!"pallas.srcLoc", i64 33, i64 5, i64 33, i64 22, !36}
!52 = !{!"pallas.ensures", !53, ptr @PALLAS_SPEC_7, !39}
!53 = !{!"pallas.srcLoc", i64 34, i64 5, i64 34, i64 36, !36}
!54 = !DILocation(line: 36, column: 26, scope: !23)
!55 = !DILocation(line: 37, column: 5, scope: !23)
!56 = !DILocation(line: 37, column: 8, scope: !23)
!57 = !DILocation(line: 37, column: 10, scope: !23)
!58 = !DILocation(line: 38, column: 5, scope: !23)
!59 = !DILocation(line: 38, column: 8, scope: !23)
!60 = !DILocation(line: 38, column: 10, scope: !23)
!61 = !DILocation(line: 39, column: 1, scope: !23)
!62 = distinct !DISubprogram(name: "alter_struct2", scope: !10, file: !10, line: 51, type: !24, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!63 = !{!64, i1 false, i1 false, !65, !68, !70, !72, !74, !76, !78, !80}
!64 = !{!"pallas.srcLoc", i64 41, i64 1, i64 50, i64 1, !36}
!65 = !{!"pallas.requires", !66, ptr @PALLAS_SPEC_8, !67}
!66 = !{!"pallas.srcLoc", i64 42, i64 5, i64 42, i64 23, !36}
!67 = !DILocalVariable(name: "p", arg: 1, scope: !62, file: !10, line: 51, type: !26)
!68 = !{!"pallas.requires", !69, ptr @PALLAS_SPEC_9, !67}
!69 = !{!"pallas.srcLoc", i64 43, i64 5, i64 43, i64 41, !36}
!70 = !{!"pallas.requires", !71, ptr @PALLAS_SPEC_10, !67}
!71 = !{!"pallas.srcLoc", i64 44, i64 5, i64 44, i64 41, !36}
!72 = !{!"pallas.ensures", !73, ptr @PALLAS_SPEC_11, !67}
!73 = !{!"pallas.srcLoc", i64 45, i64 5, i64 45, i64 40, !36}
!74 = !{!"pallas.ensures", !75, ptr @PALLAS_SPEC_12, !67}
!75 = !{!"pallas.srcLoc", i64 46, i64 5, i64 46, i64 40, !36}
!76 = !{!"pallas.ensures", !77, ptr @PALLAS_SPEC_13, !67}
!77 = !{!"pallas.srcLoc", i64 47, i64 5, i64 47, i64 22, !36}
!78 = !{!"pallas.ensures", !79, ptr @PALLAS_SPEC_14, !67}
!79 = !{!"pallas.srcLoc", i64 48, i64 5, i64 48, i64 22, !36}
!80 = !{!"pallas.ensures", !81, ptr @PALLAS_SPEC_15, !67}
!81 = !{!"pallas.srcLoc", i64 49, i64 5, i64 49, i64 36, !36}
!82 = !DILocation(line: 51, column: 26, scope: !62)
!83 = !DILocation(line: 52, column: 5, scope: !62)
!84 = !DILocation(line: 52, column: 8, scope: !62)
!85 = !DILocation(line: 52, column: 10, scope: !62)
!86 = !DILocation(line: 53, column: 5, scope: !62)
!87 = !DILocation(line: 53, column: 8, scope: !62)
!88 = !DILocation(line: 53, column: 10, scope: !62)
!89 = !DILocation(line: 54, column: 1, scope: !62)
!90 = distinct !DISubprogram(name: "alter_struct_1", scope: !10, file: !10, line: 64, type: !24, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!91 = !{!92, i1 false, i1 false, !93, !96, !98, !100, !102, !104}
!92 = !{!"pallas.srcLoc", i64 56, i64 1, i64 63, i64 1, !36}
!93 = !{!"pallas.requires", !94, ptr @PALLAS_SPEC_16, !95}
!94 = !{!"pallas.srcLoc", i64 57, i64 5, i64 57, i64 23, !36}
!95 = !DILocalVariable(name: "p", arg: 1, scope: !90, file: !10, line: 64, type: !26)
!96 = !{!"pallas.requires", !97, ptr @PALLAS_SPEC_17, !95}
!97 = !{!"pallas.srcLoc", i64 58, i64 5, i64 58, i64 39, !36}
!98 = !{!"pallas.ensures", !99, ptr @PALLAS_SPEC_18, !95}
!99 = !{!"pallas.srcLoc", i64 59, i64 5, i64 59, i64 38, !36}
!100 = !{!"pallas.ensures", !101, ptr @PALLAS_SPEC_19, !95}
!101 = !{!"pallas.srcLoc", i64 60, i64 5, i64 60, i64 40, !36}
!102 = !{!"pallas.ensures", !103, ptr @PALLAS_SPEC_20, !95}
!103 = !{!"pallas.srcLoc", i64 61, i64 5, i64 61, i64 40, !36}
!104 = !{!"pallas.ensures", !105, ptr @PALLAS_SPEC_21, !95}
!105 = !{!"pallas.srcLoc", i64 62, i64 5, i64 62, i64 36, !36}
!106 = !DILocation(line: 64, column: 28, scope: !90)
!107 = !DILocation(line: 65, column: 12, scope: !90)
!108 = !DILocation(line: 65, column: 15, scope: !90)
!109 = !DILocation(line: 65, column: 16, scope: !90)
!110 = !DILocation(line: 65, column: 5, scope: !90)
!111 = !DILocation(line: 65, column: 8, scope: !90)
!112 = !DILocation(line: 65, column: 10, scope: !90)
!113 = !DILocation(line: 66, column: 12, scope: !90)
!114 = !DILocation(line: 66, column: 15, scope: !90)
!115 = !DILocation(line: 66, column: 16, scope: !90)
!116 = !DILocation(line: 66, column: 5, scope: !90)
!117 = !DILocation(line: 66, column: 8, scope: !90)
!118 = !DILocation(line: 66, column: 10, scope: !90)
!119 = !DILocation(line: 67, column: 1, scope: !90)
!120 = distinct !DISubprogram(name: "alter_copy_struct", scope: !10, file: !10, line: 76, type: !121, scopeLine: 76, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!121 = !DISubroutineType(types: !122)
!122 = !{null, !27}
!123 = !{!124, i1 false, i1 false, !125, !128, !130, !132, !134}
!124 = !{!"pallas.srcLoc", i64 69, i64 1, i64 75, i64 1, !36}
!125 = !{!"pallas.requires", !126, ptr @PALLAS_SPEC_22, !127}
!126 = !{!"pallas.srcLoc", i64 70, i64 3, i64 70, i64 24, !36}
!127 = !DILocalVariable(name: "p", arg: 1, scope: !120, file: !10, line: 76, type: !27)
!128 = !{!"pallas.requires", !129, ptr @PALLAS_SPEC_23, !127}
!129 = !{!"pallas.srcLoc", i64 71, i64 3, i64 71, i64 38, !36}
!130 = !{!"pallas.requires", !131, ptr @PALLAS_SPEC_24, !127}
!131 = !{!"pallas.srcLoc", i64 72, i64 3, i64 72, i64 38, !36}
!132 = !{!"pallas.ensures", !133, ptr @PALLAS_SPEC_25, !127}
!133 = !{!"pallas.srcLoc", i64 73, i64 3, i64 73, i64 37, !36}
!134 = !{!"pallas.ensures", !135, ptr @PALLAS_SPEC_26, !127}
!135 = !{!"pallas.srcLoc", i64 74, i64 3, i64 74, i64 37, !36}
!136 = !DILocation(line: 76, column: 30, scope: !120)
!137 = !DILocation(line: 77, column: 7, scope: !120)
!138 = !DILocation(line: 77, column: 9, scope: !120)
!139 = !DILocation(line: 78, column: 7, scope: !120)
!140 = !DILocation(line: 78, column: 9, scope: !120)
!141 = !DILocation(line: 79, column: 1, scope: !120)
!142 = distinct !DISubprogram(name: "alter_copy_struct_2", scope: !10, file: !10, line: 85, type: !121, scopeLine: 85, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!143 = !{!144, i1 false, i1 false, !145, !148}
!144 = !{!"pallas.srcLoc", i64 81, i64 1, i64 84, i64 1, !36}
!145 = !{!"pallas.requires", !146, ptr @PALLAS_SPEC_27, !147}
!146 = !{!"pallas.srcLoc", i64 82, i64 3, i64 82, i64 24, !36}
!147 = !DILocalVariable(name: "p", arg: 1, scope: !142, file: !10, line: 85, type: !27)
!148 = !{!"pallas.requires", !149, ptr @PALLAS_SPEC_28, !147}
!149 = !{!"pallas.srcLoc", i64 83, i64 3, i64 83, i64 36, !36}
!150 = !DILocation(line: 85, column: 32, scope: !142)
!151 = !DILocation(line: 86, column: 7, scope: !142)
!152 = !DILocation(line: 86, column: 9, scope: !142)
!153 = !DILocation(line: 87, column: 7, scope: !142)
!154 = !DILocation(line: 87, column: 9, scope: !142)
!155 = !DILocation(line: 88, column: 1, scope: !142)
!156 = distinct !DISubprogram(name: "avr_x", scope: !10, file: !10, line: 96, type: !157, scopeLine: 96, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!157 = !DISubroutineType(types: !158)
!158 = !{!31, !159}
!159 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !160, size: 64)
!160 = !DIDerivedType(tag: DW_TAG_typedef, name: "triangle", file: !10, line: 11, baseType: !161)
!161 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "triangle", file: !10, line: 9, size: 192, elements: !162)
!162 = !{!163, !164, !165}
!163 = !DIDerivedType(tag: DW_TAG_member, name: "p1", scope: !161, file: !10, line: 10, baseType: !27, size: 64)
!164 = !DIDerivedType(tag: DW_TAG_member, name: "p2", scope: !161, file: !10, line: 10, baseType: !27, size: 64, offset: 64)
!165 = !DIDerivedType(tag: DW_TAG_member, name: "p3", scope: !161, file: !10, line: 10, baseType: !27, size: 64, offset: 128)
!166 = !{!167, i1 false, i1 false, !168, !171, !173, !175}
!167 = !{!"pallas.srcLoc", i64 90, i64 1, i64 95, i64 1, !36}
!168 = !{!"pallas.requires", !169, ptr @PALLAS_SPEC_29, !170}
!169 = !{!"pallas.srcLoc", i64 91, i64 3, i64 91, i64 21, !36}
!170 = !DILocalVariable(name: "r", arg: 1, scope: !156, file: !10, line: 96, type: !159)
!171 = !{!"pallas.requires", !172, ptr @PALLAS_SPEC_30, !170}
!172 = !{!"pallas.srcLoc", i64 92, i64 3, i64 92, i64 37, !36}
!173 = !{!"pallas.ensures", !174, ptr @PALLAS_SPEC_31, !170}
!174 = !{!"pallas.srcLoc", i64 93, i64 3, i64 93, i64 36, !36}
!175 = !{!"pallas.ensures", !176, ptr @PALLAS_SPEC_32, !170}
!176 = !{!"pallas.srcLoc", i64 94, i64 3, i64 94, i64 58, !36}
!177 = !DILocation(line: 96, column: 21, scope: !156)
!178 = !DILocation(line: 97, column: 13, scope: !156)
!179 = !DILocation(line: 97, column: 16, scope: !156)
!180 = !DILocation(line: 97, column: 19, scope: !156)
!181 = !DILocation(line: 97, column: 23, scope: !156)
!182 = !DILocation(line: 97, column: 26, scope: !156)
!183 = !DILocation(line: 97, column: 29, scope: !156)
!184 = !DILocation(line: 97, column: 21, scope: !156)
!185 = !DILocation(line: 97, column: 33, scope: !156)
!186 = !DILocation(line: 97, column: 36, scope: !156)
!187 = !DILocation(line: 97, column: 39, scope: !156)
!188 = !DILocation(line: 97, column: 31, scope: !156)
!189 = !DILocation(line: 97, column: 41, scope: !156)
!190 = !DILocation(line: 97, column: 5, scope: !156)
!191 = distinct !DISubprogram(name: "avr_x_pol", scope: !10, file: !10, line: 113, type: !192, scopeLine: 113, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!192 = !DISubroutineType(types: !193)
!193 = !{!31, !194, !31}
!194 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !195, size: 64)
!195 = !DIDerivedType(tag: DW_TAG_typedef, name: "polygon", file: !10, line: 15, baseType: !196)
!196 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "polygon", file: !10, line: 13, size: 64, elements: !197)
!197 = !{!198}
!198 = !DIDerivedType(tag: DW_TAG_member, name: "ps", scope: !196, file: !10, line: 14, baseType: !26, size: 64)
!199 = !{!200, i1 false, i1 false, !201, !205, !207, !209, !211, !213, !215, !217, !219, !221, !223}
!200 = !{!"pallas.srcLoc", i64 100, i64 1, i64 112, i64 1, !36}
!201 = !{!"pallas.requires", !202, ptr @PALLAS_SPEC_33, !203, !204}
!202 = !{!"pallas.srcLoc", i64 101, i64 3, i64 101, i64 19, !36}
!203 = !DILocalVariable(name: "p", arg: 1, scope: !191, file: !10, line: 113, type: !194)
!204 = !DILocalVariable(name: "len", arg: 2, scope: !191, file: !10, line: 113, type: !31)
!205 = !{!"pallas.requires", !206, ptr @PALLAS_SPEC_34, !203, !204}
!206 = !{!"pallas.srcLoc", i64 102, i64 3, i64 102, i64 21, !36}
!207 = !{!"pallas.requires", !208, ptr @PALLAS_SPEC_35, !203, !204}
!208 = !{!"pallas.srcLoc", i64 103, i64 3, i64 103, i64 37, !36}
!209 = !{!"pallas.requires", !210, ptr @PALLAS_SPEC_36, !203, !204}
!210 = !{!"pallas.srcLoc", i64 104, i64 3, i64 104, i64 54, !36}
!211 = !{!"pallas.requires", !212, ptr @PALLAS_SPEC_37, !203, !204}
!212 = !{!"pallas.srcLoc", i64 105, i64 3, i64 105, i64 191, !36}
!213 = !{!"pallas.requires", !214, ptr @PALLAS_SPEC_38, !203, !204}
!214 = !{!"pallas.srcLoc", i64 106, i64 3, i64 106, i64 106, !36}
!215 = !{!"pallas.ensures", !216, ptr @PALLAS_SPEC_39, !203, !204}
!216 = !{!"pallas.srcLoc", i64 107, i64 3, i64 107, i64 36, !36}
!217 = !{!"pallas.ensures", !218, ptr @PALLAS_SPEC_40, !203, !204}
!218 = !{!"pallas.srcLoc", i64 108, i64 3, i64 108, i64 53, !36}
!219 = !{!"pallas.ensures", !220, ptr @PALLAS_SPEC_41, !203, !204}
!220 = !{!"pallas.srcLoc", i64 109, i64 3, i64 109, i64 190, !36}
!221 = !{!"pallas.ensures", !222, ptr @PALLAS_SPEC_42, !203, !204}
!222 = !{!"pallas.srcLoc", i64 110, i64 3, i64 110, i64 105, !36}
!223 = !{!"pallas.ensures", !224, ptr @PALLAS_SPEC_43, !203, !204}
!224 = !{!"pallas.srcLoc", i64 111, i64 3, i64 111, i64 87, !36}
!225 = !DILocation(line: 113, column: 24, scope: !191)
!226 = !DILocation(line: 113, column: 31, scope: !191)
!227 = !DILocalVariable(name: "sum", scope: !191, file: !10, line: 114, type: !31)
!228 = !DILocation(line: 114, column: 9, scope: !191)
!229 = !DILocalVariable(name: "i", scope: !230, file: !10, line: 127, type: !31)
!230 = distinct !DILexicalBlock(scope: !191, file: !10, line: 127, column: 5)
!231 = !DILocation(line: 127, column: 13, scope: !230)
!232 = !DILocation(line: 127, column: 9, scope: !230)
!233 = !DILocation(line: 127, column: 18, scope: !234)
!234 = distinct !DILexicalBlock(scope: !230, file: !10, line: 127, column: 5)
!235 = !DILocation(line: 127, column: 20, scope: !234)
!236 = !DILocation(line: 127, column: 19, scope: !234)
!237 = !DILocation(line: 127, column: 5, scope: !230)
!238 = !DILocation(line: 128, column: 16, scope: !239)
!239 = distinct !DILexicalBlock(scope: !234, file: !10, line: 127, column: 29)
!240 = !DILocation(line: 128, column: 19, scope: !239)
!241 = !DILocation(line: 128, column: 22, scope: !239)
!242 = !DILocation(line: 128, column: 25, scope: !239)
!243 = !DILocation(line: 128, column: 13, scope: !239)
!244 = !DILocation(line: 129, column: 5, scope: !239)
!245 = !DILocation(line: 127, column: 26, scope: !234)
!246 = !DILocation(line: 127, column: 5, scope: !234)
!247 = distinct !{!247, !237, !248, !249, !250}
!248 = !DILocation(line: 129, column: 5, scope: !230)
!249 = !{!"llvm.loop.mustprogress"}
!250 = !{!"pallas.loopInv", !251, !252, !254, !256, !258, !260, !262, !264, !266, !268, !270}
!251 = !{!"pallas.srcLoc", i64 115, i64 5, i64 126, i64 5, !36}
!252 = !{!253, ptr @PALLAS_SPEC_44, !203, !204, !227, !229}
!253 = !{!"pallas.srcLoc", i64 116, i64 7, i64 116, i64 36, !36}
!254 = !{!255, ptr @PALLAS_SPEC_45, !203, !204, !227, !229}
!255 = !{!"pallas.srcLoc", i64 117, i64 7, i64 117, i64 31, !36}
!256 = !{!257, ptr @PALLAS_SPEC_46, !203, !204, !227, !229}
!257 = !{!"pallas.srcLoc", i64 118, i64 7, i64 118, i64 47, !36}
!258 = !{!259, ptr @PALLAS_SPEC_47, !203, !204, !227, !229}
!259 = !{!"pallas.srcLoc", i64 119, i64 7, i64 119, i64 64, !36}
!260 = !{!261, ptr @PALLAS_SPEC_48, !203, !204, !227, !229}
!261 = !{!"pallas.srcLoc", i64 120, i64 7, i64 120, i64 201, !36}
!262 = !{!263, ptr @PALLAS_SPEC_49, !203, !204, !227, !229}
!263 = !{!"pallas.srcLoc", i64 121, i64 7, i64 121, i64 116, !36}
!264 = !{!265, ptr @PALLAS_SPEC_50, !203, !204, !227, !229}
!265 = !{!"pallas.srcLoc", i64 122, i64 7, i64 122, i64 48, !36}
!266 = !{!267, ptr @PALLAS_SPEC_51, !203, !204, !227, !229}
!267 = !{!"pallas.srcLoc", i64 123, i64 7, i64 123, i64 57, !36}
!268 = !{!269, ptr @PALLAS_SPEC_52, !203, !204, !227, !229}
!269 = !{!"pallas.srcLoc", i64 124, i64 7, i64 124, i64 70, !36}
!270 = !{!271, ptr @PALLAS_SPEC_53, !203, !204, !227, !229}
!271 = !{!"pallas.srcLoc", i64 125, i64 7, i64 125, i64 83, !36}
!272 = !DILocation(line: 131, column: 12, scope: !191)
!273 = !DILocation(line: 131, column: 16, scope: !191)
!274 = !DILocation(line: 131, column: 15, scope: !191)
!275 = !DILocation(line: 131, column: 5, scope: !191)
!276 = distinct !DISubprogram(name: "main", scope: !10, file: !10, line: 135, type: !277, scopeLine: 135, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!277 = !DISubroutineType(types: !278)
!278 = !{!31}
!279 = !DILocalVariable(name: "p", scope: !276, file: !10, line: 136, type: !27)
!280 = !DILocation(line: 136, column: 11, scope: !276)
!281 = !DILocalVariable(name: "pp", scope: !276, file: !10, line: 137, type: !26)
!282 = !DILocation(line: 137, column: 12, scope: !276)
!283 = !DILocation(line: 138, column: 8, scope: !276)
!284 = !DILocation(line: 142, column: 7, scope: !276)
!285 = !{!286, !287}
!286 = !{!"pallas.srcLoc", i64 140, i64 5, i64 140, i64 29, !36}
!287 = !{!"pallas.assert", !288, ptr @PALLAS_SPEC_54, !279, !281}
!288 = !{!"pallas.srcLoc", i64 140, i64 9, i64 140, i64 27, !36}
!289 = !DILocation(line: 142, column: 9, scope: !276)
!290 = !DILocation(line: 143, column: 7, scope: !276)
!291 = !DILocation(line: 143, column: 9, scope: !276)
!292 = !DILocation(line: 146, column: 5, scope: !276)
!293 = !{!294, !295, !297}
!294 = !{!"pallas.srcLoc", i64 144, i64 5, i64 145, i64 24, !36}
!295 = !{!"pallas.assert", !296, ptr @PALLAS_SPEC_55, !279, !281}
!296 = !{!"pallas.srcLoc", i64 144, i64 9, i64 144, i64 26, !36}
!297 = !{!"pallas.assert", !298, ptr @PALLAS_SPEC_56, !279, !281}
!298 = !{!"pallas.srcLoc", i64 145, i64 5, i64 145, i64 22, !36}
!299 = !DILocation(line: 150, column: 18, scope: !276)
!300 = !{!301, !302, !304}
!301 = !{!"pallas.srcLoc", i64 147, i64 5, i64 148, i64 22, !36}
!302 = !{!"pallas.assert", !303, ptr @PALLAS_SPEC_57, !279, !281}
!303 = !{!"pallas.srcLoc", i64 147, i64 9, i64 147, i64 24, !36}
!304 = !{!"pallas.assert", !305, ptr @PALLAS_SPEC_58, !279, !281}
!305 = !{!"pallas.srcLoc", i64 148, i64 5, i64 148, i64 20, !36}
!306 = !DILocation(line: 150, column: 5, scope: !276)
!307 = !DILocation(line: 153, column: 20, scope: !276)
!308 = !{!309, !310, !312}
!309 = !{!"pallas.srcLoc", i64 151, i64 5, i64 152, i64 22, !36}
!310 = !{!"pallas.assert", !311, ptr @PALLAS_SPEC_59, !279, !281}
!311 = !{!"pallas.srcLoc", i64 151, i64 9, i64 151, i64 26, !36}
!312 = !{!"pallas.assert", !313, ptr @PALLAS_SPEC_60, !279, !281}
!313 = !{!"pallas.srcLoc", i64 152, i64 5, i64 152, i64 20, !36}
!314 = !DILocation(line: 153, column: 5, scope: !276)
!315 = !DILocalVariable(name: "p1", scope: !276, file: !10, line: 156, type: !27)
!316 = !DILocation(line: 156, column: 11, scope: !276)
!317 = !DILocalVariable(name: "p2", scope: !276, file: !10, line: 156, type: !27)
!318 = !DILocation(line: 156, column: 15, scope: !276)
!319 = !DILocalVariable(name: "p3", scope: !276, file: !10, line: 156, type: !27)
!320 = !DILocation(line: 156, column: 19, scope: !276)
!321 = !DILocation(line: 157, column: 8, scope: !276)
!322 = !{!323, !324}
!323 = !{!"pallas.srcLoc", i64 154, i64 5, i64 154, i64 38, !36}
!324 = !{!"pallas.assert", !325, ptr @PALLAS_SPEC_61, !279, !281, !315, !317, !319}
!325 = !{!"pallas.srcLoc", i64 154, i64 9, i64 154, i64 36, !36}
!326 = !DILocation(line: 157, column: 10, scope: !276)
!327 = !DILocation(line: 157, column: 18, scope: !276)
!328 = !DILocation(line: 157, column: 20, scope: !276)
!329 = !DILocation(line: 158, column: 8, scope: !276)
!330 = !DILocation(line: 158, column: 10, scope: !276)
!331 = !DILocation(line: 158, column: 18, scope: !276)
!332 = !DILocation(line: 158, column: 20, scope: !276)
!333 = !DILocation(line: 159, column: 8, scope: !276)
!334 = !DILocation(line: 159, column: 10, scope: !276)
!335 = !DILocation(line: 159, column: 18, scope: !276)
!336 = !DILocation(line: 159, column: 20, scope: !276)
!337 = !DILocalVariable(name: "r", scope: !276, file: !10, line: 160, type: !160)
!338 = !DILocation(line: 160, column: 14, scope: !276)
!339 = !DILocalVariable(name: "rr", scope: !276, file: !10, line: 160, type: !159)
!340 = !DILocation(line: 160, column: 18, scope: !276)
!341 = !DILocation(line: 161, column: 8, scope: !276)
!342 = !DILocation(line: 162, column: 7, scope: !276)
!343 = !DILocation(line: 162, column: 12, scope: !276)
!344 = !DILocation(line: 163, column: 7, scope: !276)
!345 = !DILocation(line: 163, column: 12, scope: !276)
!346 = !DILocation(line: 164, column: 7, scope: !276)
!347 = !DILocation(line: 164, column: 12, scope: !276)
!348 = !DILocalVariable(name: "ps", scope: !276, file: !10, line: 166, type: !349)
!349 = !DICompositeType(tag: DW_TAG_array_type, baseType: !27, size: 192, elements: !350)
!350 = !{!351}
!351 = !DISubrange(count: 3)
!352 = !DILocation(line: 166, column: 11, scope: !276)
!353 = !DILocation(line: 166, column: 19, scope: !276)
!354 = !{!355, !356}
!355 = !{!"pallas.srcLoc", i64 165, i64 5, i64 165, i64 32, !36}
!356 = !{!"pallas.assert", !357, ptr @PALLAS_SPEC_62, !279, !281, !315, !317, !319, !337, !339, !348}
!357 = !{!"pallas.srcLoc", i64 165, i64 9, i64 165, i64 30, !36}
!358 = !DILocation(line: 166, column: 20, scope: !276)
!359 = !DILocation(line: 166, column: 24, scope: !276)
!360 = !DILocation(line: 166, column: 28, scope: !276)
!361 = !DILocalVariable(name: "pol", scope: !276, file: !10, line: 167, type: !195)
!362 = !DILocation(line: 167, column: 13, scope: !276)
!363 = !DILocalVariable(name: "ppols", scope: !276, file: !10, line: 167, type: !194)
!364 = !DILocation(line: 167, column: 19, scope: !276)
!365 = !DILocation(line: 168, column: 11, scope: !276)
!366 = !DILocation(line: 169, column: 14, scope: !276)
!367 = !DILocation(line: 169, column: 9, scope: !276)
!368 = !DILocation(line: 169, column: 12, scope: !276)
!369 = !DILocalVariable(name: "avr_pol", scope: !276, file: !10, line: 170, type: !31)
!370 = !DILocation(line: 170, column: 9, scope: !276)
!371 = !DILocation(line: 170, column: 29, scope: !276)
!372 = !DILocation(line: 170, column: 19, scope: !276)
!373 = !DILocation(line: 173, column: 5, scope: !276)
!374 = !{!375, !376}
!375 = !{!"pallas.srcLoc", i64 171, i64 5, i64 171, i64 30, !36}
!376 = !{!"pallas.assert", !377, ptr @PALLAS_SPEC_63, !279, !281, !315, !317, !319, !337, !339, !348, !361, !363, !369}
!377 = !{!"pallas.srcLoc", i64 171, i64 9, i64 171, i64 28, !36}
!378 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !10, file: !10, line: 27, type: !379, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!379 = !DISubroutineType(types: !380)
!380 = !{!381, !382}
!381 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!382 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !383, size: 64)
!383 = !DIDerivedType(tag: DW_TAG_typedef, name: "point", file: !2, line: 8, baseType: !384)
!384 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "point", file: !2, line: 5, size: 64, elements: !385)
!385 = !{!386, !387}
!386 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !384, file: !2, line: 6, baseType: !31, size: 32)
!387 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !384, file: !2, line: 7, baseType: !31, size: 32, offset: 32)
!388 = !{!""}
!389 = !DILocalVariable(name: "p", arg: 1, scope: !378, file: !10, line: 27, type: !382)
!390 = !DILocation(line: 0, scope: !378)
!391 = !DILocation(line: 27, column: 16, scope: !378)
!392 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !10, file: !10, line: 28, type: !379, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!393 = !DILocalVariable(name: "p", arg: 1, scope: !392, file: !10, line: 28, type: !382)
!394 = !DILocation(line: 0, scope: !392)
!395 = !DILocation(line: 28, column: 24, scope: !392)
!396 = !DILocation(line: 28, column: 27, scope: !392)
!397 = !DILocation(line: 28, column: 14, scope: !392)
!398 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !10, file: !10, line: 29, type: !379, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!399 = !DILocalVariable(name: "p", arg: 1, scope: !398, file: !10, line: 29, type: !382)
!400 = !DILocation(line: 0, scope: !398)
!401 = !DILocation(line: 29, column: 24, scope: !398)
!402 = !DILocation(line: 29, column: 27, scope: !398)
!403 = !DILocation(line: 29, column: 14, scope: !398)
!404 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !10, file: !10, line: 30, type: !379, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!405 = !DILocalVariable(name: "p", arg: 1, scope: !404, file: !10, line: 30, type: !382)
!406 = !DILocation(line: 0, scope: !404)
!407 = !DILocation(line: 30, column: 23, scope: !404)
!408 = !DILocation(line: 30, column: 26, scope: !404)
!409 = !DILocation(line: 30, column: 13, scope: !404)
!410 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !10, file: !10, line: 31, type: !379, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!411 = !DILocalVariable(name: "p", arg: 1, scope: !410, file: !10, line: 31, type: !382)
!412 = !DILocation(line: 0, scope: !410)
!413 = !DILocation(line: 31, column: 23, scope: !410)
!414 = !DILocation(line: 31, column: 26, scope: !410)
!415 = !DILocation(line: 31, column: 13, scope: !410)
!416 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !10, file: !10, line: 32, type: !379, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!417 = !DILocalVariable(name: "p", arg: 1, scope: !416, file: !10, line: 32, type: !382)
!418 = !DILocation(line: 0, scope: !416)
!419 = !DILocation(line: 32, column: 16, scope: !416)
!420 = !DILocation(line: 32, column: 18, scope: !416)
!421 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !10, file: !10, line: 33, type: !379, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!422 = !DILocalVariable(name: "p", arg: 1, scope: !421, file: !10, line: 33, type: !382)
!423 = !DILocation(line: 0, scope: !421)
!424 = !DILocation(line: 33, column: 16, scope: !421)
!425 = !DILocation(line: 33, column: 18, scope: !421)
!426 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !10, file: !10, line: 34, type: !379, scopeLine: 34, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!427 = !DILocalVariable(name: "p", arg: 1, scope: !426, file: !10, line: 34, type: !382)
!428 = !DILocation(line: 0, scope: !426)
!429 = !DILocation(line: 34, column: 13, scope: !426)
!430 = !DILocation(line: 34, column: 32, scope: !426)
!431 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !10, file: !10, line: 42, type: !379, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!432 = !DILocalVariable(name: "p", arg: 1, scope: !431, file: !10, line: 42, type: !382)
!433 = !DILocation(line: 0, scope: !431)
!434 = !DILocation(line: 42, column: 16, scope: !431)
!435 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !10, file: !10, line: 43, type: !379, scopeLine: 43, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!436 = !DILocalVariable(name: "p", arg: 1, scope: !435, file: !10, line: 43, type: !382)
!437 = !DILocation(line: 0, scope: !435)
!438 = !DILocation(line: 43, column: 24, scope: !435)
!439 = !DILocation(line: 43, column: 27, scope: !435)
!440 = !DILocation(line: 43, column: 14, scope: !435)
!441 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !10, file: !10, line: 44, type: !379, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!442 = !DILocalVariable(name: "p", arg: 1, scope: !441, file: !10, line: 44, type: !382)
!443 = !DILocation(line: 0, scope: !441)
!444 = !DILocation(line: 44, column: 24, scope: !441)
!445 = !DILocation(line: 44, column: 27, scope: !441)
!446 = !DILocation(line: 44, column: 14, scope: !441)
!447 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !10, file: !10, line: 45, type: !379, scopeLine: 45, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!448 = !DILocalVariable(name: "p", arg: 1, scope: !447, file: !10, line: 45, type: !382)
!449 = !DILocation(line: 0, scope: !447)
!450 = !DILocation(line: 45, column: 23, scope: !447)
!451 = !DILocation(line: 45, column: 26, scope: !447)
!452 = !DILocation(line: 45, column: 13, scope: !447)
!453 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !10, file: !10, line: 46, type: !379, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!454 = !DILocalVariable(name: "p", arg: 1, scope: !453, file: !10, line: 46, type: !382)
!455 = !DILocation(line: 0, scope: !453)
!456 = !DILocation(line: 46, column: 23, scope: !453)
!457 = !DILocation(line: 46, column: 26, scope: !453)
!458 = !DILocation(line: 46, column: 13, scope: !453)
!459 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !10, file: !10, line: 47, type: !379, scopeLine: 47, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!460 = !DILocalVariable(name: "p", arg: 1, scope: !459, file: !10, line: 47, type: !382)
!461 = !DILocation(line: 0, scope: !459)
!462 = !DILocation(line: 47, column: 16, scope: !459)
!463 = !DILocation(line: 47, column: 18, scope: !459)
!464 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !10, file: !10, line: 48, type: !379, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!465 = !DILocalVariable(name: "p", arg: 1, scope: !464, file: !10, line: 48, type: !382)
!466 = !DILocation(line: 0, scope: !464)
!467 = !DILocation(line: 48, column: 16, scope: !464)
!468 = !DILocation(line: 48, column: 18, scope: !464)
!469 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !10, file: !10, line: 49, type: !379, scopeLine: 49, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!470 = !DILocalVariable(name: "p", arg: 1, scope: !469, file: !10, line: 49, type: !382)
!471 = !DILocation(line: 0, scope: !469)
!472 = !DILocation(line: 49, column: 13, scope: !469)
!473 = !DILocation(line: 49, column: 32, scope: !469)
!474 = distinct !DISubprogram(name: "PALLAS_SPEC_16", scope: !10, file: !10, line: 57, type: !379, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!475 = !DILocalVariable(name: "p", arg: 1, scope: !474, file: !10, line: 57, type: !382)
!476 = !DILocation(line: 0, scope: !474)
!477 = !DILocation(line: 57, column: 16, scope: !474)
!478 = distinct !DISubprogram(name: "PALLAS_SPEC_17", scope: !10, file: !10, line: 58, type: !379, scopeLine: 58, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!479 = !DILocalVariable(name: "p", arg: 1, scope: !478, file: !10, line: 58, type: !382)
!480 = !DILocation(line: 0, scope: !478)
!481 = !DILocation(line: 58, column: 25, scope: !478)
!482 = !DILocation(line: 58, column: 14, scope: !478)
!483 = distinct !DISubprogram(name: "PALLAS_SPEC_18", scope: !10, file: !10, line: 59, type: !379, scopeLine: 59, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!484 = !DILocalVariable(name: "p", arg: 1, scope: !483, file: !10, line: 59, type: !382)
!485 = !DILocation(line: 0, scope: !483)
!486 = !DILocation(line: 59, column: 24, scope: !483)
!487 = !DILocation(line: 59, column: 13, scope: !483)
!488 = distinct !DISubprogram(name: "PALLAS_SPEC_19", scope: !10, file: !10, line: 60, type: !379, scopeLine: 60, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!489 = !DILocalVariable(name: "p", arg: 1, scope: !488, file: !10, line: 60, type: !382)
!490 = !DILocation(line: 0, scope: !488)
!491 = !DILocation(line: 60, column: 16, scope: !488)
!492 = !DILocation(line: 60, column: 34, scope: !488)
!493 = !DILocation(line: 60, column: 36, scope: !488)
!494 = !DILocation(line: 60, column: 21, scope: !488)
!495 = !DILocation(line: 60, column: 18, scope: !488)
!496 = distinct !DISubprogram(name: "PALLAS_SPEC_20", scope: !10, file: !10, line: 61, type: !379, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!497 = !DILocalVariable(name: "p", arg: 1, scope: !496, file: !10, line: 61, type: !382)
!498 = !DILocation(line: 0, scope: !496)
!499 = !DILocation(line: 61, column: 16, scope: !496)
!500 = !DILocation(line: 61, column: 34, scope: !496)
!501 = !DILocation(line: 61, column: 36, scope: !496)
!502 = !DILocation(line: 61, column: 21, scope: !496)
!503 = !DILocation(line: 61, column: 18, scope: !496)
!504 = distinct !DISubprogram(name: "PALLAS_SPEC_21", scope: !10, file: !10, line: 62, type: !379, scopeLine: 62, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!505 = !DILocalVariable(name: "p", arg: 1, scope: !504, file: !10, line: 62, type: !382)
!506 = !DILocation(line: 0, scope: !504)
!507 = !DILocation(line: 62, column: 13, scope: !504)
!508 = !DILocation(line: 62, column: 32, scope: !504)
!509 = distinct !DISubprogram(name: "PALLAS_SPEC_22", scope: !10, file: !10, line: 70, type: !510, scopeLine: 70, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!510 = !DISubroutineType(types: !511)
!511 = !{!381, !383}
!512 = !DILocalVariable(name: "p", arg: 1, scope: !509, file: !10, line: 70, type: !383)
!513 = !DILocation(line: 0, scope: !509)
!514 = !DILocation(line: 70, column: 17, scope: !509)
!515 = distinct !DISubprogram(name: "PALLAS_SPEC_23", scope: !10, file: !10, line: 71, type: !510, scopeLine: 71, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!516 = !DILocalVariable(name: "p", arg: 1, scope: !515, file: !10, line: 71, type: !383)
!517 = !DILocation(line: 0, scope: !515)
!518 = !DILocation(line: 71, column: 21, scope: !515)
!519 = !DILocation(line: 71, column: 24, scope: !515)
!520 = !DILocation(line: 71, column: 12, scope: !515)
!521 = distinct !DISubprogram(name: "PALLAS_SPEC_24", scope: !10, file: !10, line: 72, type: !510, scopeLine: 72, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!522 = !DILocalVariable(name: "p", arg: 1, scope: !521, file: !10, line: 72, type: !383)
!523 = !DILocation(line: 0, scope: !521)
!524 = !DILocation(line: 72, column: 21, scope: !521)
!525 = !DILocation(line: 72, column: 24, scope: !521)
!526 = !DILocation(line: 72, column: 12, scope: !521)
!527 = distinct !DISubprogram(name: "PALLAS_SPEC_25", scope: !10, file: !10, line: 73, type: !510, scopeLine: 73, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!528 = !DILocalVariable(name: "p", arg: 1, scope: !527, file: !10, line: 73, type: !383)
!529 = !DILocation(line: 0, scope: !527)
!530 = !DILocation(line: 73, column: 20, scope: !527)
!531 = !DILocation(line: 73, column: 23, scope: !527)
!532 = !DILocation(line: 73, column: 11, scope: !527)
!533 = distinct !DISubprogram(name: "PALLAS_SPEC_26", scope: !10, file: !10, line: 74, type: !510, scopeLine: 74, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!534 = !DILocalVariable(name: "p", arg: 1, scope: !533, file: !10, line: 74, type: !383)
!535 = !DILocation(line: 0, scope: !533)
!536 = !DILocation(line: 74, column: 20, scope: !533)
!537 = !DILocation(line: 74, column: 23, scope: !533)
!538 = !DILocation(line: 74, column: 11, scope: !533)
!539 = distinct !DISubprogram(name: "PALLAS_SPEC_27", scope: !10, file: !10, line: 82, type: !510, scopeLine: 82, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!540 = !DILocalVariable(name: "p", arg: 1, scope: !539, file: !10, line: 82, type: !383)
!541 = !DILocation(line: 0, scope: !539)
!542 = !DILocation(line: 82, column: 17, scope: !539)
!543 = distinct !DISubprogram(name: "PALLAS_SPEC_28", scope: !10, file: !10, line: 83, type: !510, scopeLine: 83, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!544 = !DILocalVariable(name: "p", arg: 1, scope: !543, file: !10, line: 83, type: !383)
!545 = !DILocation(line: 0, scope: !543)
!546 = !DILocation(line: 83, column: 22, scope: !543)
!547 = !DILocation(line: 83, column: 12, scope: !543)
!548 = distinct !DISubprogram(name: "PALLAS_SPEC_29", scope: !10, file: !10, line: 91, type: !549, scopeLine: 91, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!549 = !DISubroutineType(types: !550)
!550 = !{!381, !551}
!551 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !552, size: 64)
!552 = !DIDerivedType(tag: DW_TAG_typedef, name: "triangle", file: !2, line: 12, baseType: !553)
!553 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "triangle", file: !2, line: 10, size: 192, elements: !554)
!554 = !{!555, !556, !557}
!555 = !DIDerivedType(tag: DW_TAG_member, name: "p1", scope: !553, file: !2, line: 11, baseType: !383, size: 64)
!556 = !DIDerivedType(tag: DW_TAG_member, name: "p2", scope: !553, file: !2, line: 11, baseType: !383, size: 64, offset: 64)
!557 = !DIDerivedType(tag: DW_TAG_member, name: "p3", scope: !553, file: !2, line: 11, baseType: !383, size: 64, offset: 128)
!558 = !DILocalVariable(name: "r", arg: 1, scope: !548, file: !10, line: 91, type: !551)
!559 = !DILocation(line: 0, scope: !548)
!560 = !DILocation(line: 91, column: 14, scope: !548)
!561 = distinct !DISubprogram(name: "PALLAS_SPEC_30", scope: !10, file: !10, line: 92, type: !549, scopeLine: 92, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!562 = !DILocalVariable(name: "r", arg: 1, scope: !561, file: !10, line: 92, type: !551)
!563 = !DILocation(line: 0, scope: !561)
!564 = !DILocation(line: 92, column: 23, scope: !561)
!565 = !DILocation(line: 92, column: 12, scope: !561)
!566 = distinct !DISubprogram(name: "PALLAS_SPEC_31", scope: !10, file: !10, line: 93, type: !549, scopeLine: 93, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!567 = !DILocalVariable(name: "r", arg: 1, scope: !566, file: !10, line: 93, type: !551)
!568 = !DILocation(line: 0, scope: !566)
!569 = !DILocation(line: 93, column: 22, scope: !566)
!570 = !DILocation(line: 93, column: 11, scope: !566)
!571 = distinct !DISubprogram(name: "PALLAS_SPEC_32", scope: !10, file: !10, line: 94, type: !549, scopeLine: 94, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!572 = !DILocalVariable(name: "r", arg: 1, scope: !571, file: !10, line: 94, type: !551)
!573 = !DILocation(line: 0, scope: !571)
!574 = !DILocation(line: 94, column: 11, scope: !571)
!575 = !DILocation(line: 94, column: 31, scope: !571)
!576 = !DILocation(line: 94, column: 34, scope: !571)
!577 = !DILocation(line: 94, column: 41, scope: !571)
!578 = !DILocation(line: 94, column: 44, scope: !571)
!579 = !DILocation(line: 94, column: 36, scope: !571)
!580 = !DILocation(line: 94, column: 51, scope: !571)
!581 = !DILocation(line: 94, column: 54, scope: !571)
!582 = !DILocation(line: 94, column: 46, scope: !571)
!583 = !DILocation(line: 94, column: 56, scope: !571)
!584 = !DILocation(line: 94, column: 24, scope: !571)
!585 = distinct !DISubprogram(name: "PALLAS_SPEC_33", scope: !10, file: !10, line: 101, type: !586, scopeLine: 101, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!586 = !DISubroutineType(types: !587)
!587 = !{!381, !588, !31}
!588 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !589, size: 64)
!589 = !DIDerivedType(tag: DW_TAG_typedef, name: "polygon", file: !2, line: 16, baseType: !590)
!590 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "polygon", file: !2, line: 14, size: 64, elements: !591)
!591 = !{!592}
!592 = !DIDerivedType(tag: DW_TAG_member, name: "ps", scope: !590, file: !2, line: 15, baseType: !382, size: 64)
!593 = !DILocalVariable(name: "p", arg: 1, scope: !585, file: !10, line: 101, type: !588)
!594 = !DILocation(line: 0, scope: !585)
!595 = !DILocalVariable(name: "len", arg: 2, scope: !585, file: !10, line: 101, type: !31)
!596 = !DILocation(line: 101, column: 16, scope: !585)
!597 = distinct !DISubprogram(name: "PALLAS_SPEC_34", scope: !10, file: !10, line: 102, type: !586, scopeLine: 102, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!598 = !DILocalVariable(name: "p", arg: 1, scope: !597, file: !10, line: 102, type: !588)
!599 = !DILocation(line: 0, scope: !597)
!600 = !DILocalVariable(name: "len", arg: 2, scope: !597, file: !10, line: 102, type: !31)
!601 = !DILocation(line: 102, column: 14, scope: !597)
!602 = distinct !DISubprogram(name: "PALLAS_SPEC_35", scope: !10, file: !10, line: 103, type: !586, scopeLine: 103, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!603 = !DILocalVariable(name: "p", arg: 1, scope: !602, file: !10, line: 103, type: !588)
!604 = !DILocation(line: 0, scope: !602)
!605 = !DILocalVariable(name: "len", arg: 2, scope: !602, file: !10, line: 103, type: !31)
!606 = !DILocation(line: 103, column: 23, scope: !602)
!607 = !DILocation(line: 103, column: 12, scope: !602)
!608 = distinct !DISubprogram(name: "PALLAS_SPEC_36", scope: !10, file: !10, line: 104, type: !586, scopeLine: 104, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!609 = !DILocalVariable(name: "p", arg: 1, scope: !608, file: !10, line: 104, type: !588)
!610 = !DILocation(line: 0, scope: !608)
!611 = !DILocalVariable(name: "len", arg: 2, scope: !608, file: !10, line: 104, type: !31)
!612 = !DILocation(line: 104, column: 15, scope: !608)
!613 = !DILocation(line: 104, column: 18, scope: !608)
!614 = !DILocation(line: 104, column: 26, scope: !608)
!615 = !DILocation(line: 104, column: 44, scope: !608)
!616 = !DILocation(line: 104, column: 29, scope: !608)
!617 = !DILocation(line: 104, column: 51, scope: !608)
!618 = !DILocation(line: 104, column: 48, scope: !608)
!619 = distinct !DISubprogram(name: "PALLAS_SPEC_37", scope: !10, file: !10, line: 105, type: !586, scopeLine: 105, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!620 = !DILocalVariable(name: "p", arg: 1, scope: !619, file: !10, line: 105, type: !588)
!621 = !DILocation(line: 0, scope: !619)
!622 = !DILocalVariable(name: "len", arg: 2, scope: !619, file: !10, line: 105, type: !31)
!623 = !DILocation(line: 105, column: 30, scope: !619)
!624 = !DILocation(line: 105, column: 27, scope: !619)
!625 = !DILocation(line: 105, column: 48, scope: !619)
!626 = !DILocation(line: 105, column: 60, scope: !619)
!627 = !DILocation(line: 105, column: 77, scope: !619)
!628 = !DILocation(line: 105, column: 74, scope: !619)
!629 = !DILocation(line: 105, column: 90, scope: !619)
!630 = !DILocation(line: 105, column: 102, scope: !619)
!631 = !DILocation(line: 105, column: 67, scope: !619)
!632 = !DILocation(line: 105, column: 43, scope: !619)
!633 = !DILocation(line: 105, column: 20, scope: !619)
!634 = !DILocation(line: 105, column: 119, scope: !619)
!635 = !DILocation(line: 105, column: 134, scope: !619)
!636 = !DILocation(line: 105, column: 131, scope: !619)
!637 = !DILocation(line: 105, column: 150, scope: !619)
!638 = !DILocation(line: 105, column: 155, scope: !619)
!639 = !DILocation(line: 105, column: 153, scope: !619)
!640 = !DILocation(line: 105, column: 173, scope: !619)
!641 = !DILocation(line: 105, column: 178, scope: !619)
!642 = !DILocation(line: 105, column: 176, scope: !619)
!643 = !DILocation(line: 105, column: 167, scope: !619)
!644 = !DILocation(line: 105, column: 112, scope: !619)
!645 = !DILocation(line: 105, column: 12, scope: !619)
!646 = distinct !DISubprogram(name: "PALLAS_SPEC_38", scope: !10, file: !10, line: 106, type: !586, scopeLine: 106, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!647 = !DILocalVariable(name: "p", arg: 1, scope: !646, file: !10, line: 106, type: !588)
!648 = !DILocation(line: 0, scope: !646)
!649 = !DILocalVariable(name: "len", arg: 2, scope: !646, file: !10, line: 106, type: !31)
!650 = !DILocation(line: 106, column: 31, scope: !646)
!651 = !DILocation(line: 106, column: 28, scope: !646)
!652 = !DILocation(line: 106, column: 44, scope: !646)
!653 = !DILocation(line: 106, column: 56, scope: !646)
!654 = !DILocation(line: 106, column: 21, scope: !646)
!655 = !DILocation(line: 106, column: 74, scope: !646)
!656 = !DILocation(line: 106, column: 77, scope: !646)
!657 = !DILocation(line: 106, column: 71, scope: !646)
!658 = !DILocation(line: 106, column: 91, scope: !646)
!659 = !DILocation(line: 106, column: 64, scope: !646)
!660 = !DILocation(line: 106, column: 12, scope: !646)
!661 = distinct !DISubprogram(name: "PALLAS_SPEC_39", scope: !10, file: !10, line: 107, type: !586, scopeLine: 107, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!662 = !DILocalVariable(name: "p", arg: 1, scope: !661, file: !10, line: 107, type: !588)
!663 = !DILocation(line: 0, scope: !661)
!664 = !DILocalVariable(name: "len", arg: 2, scope: !661, file: !10, line: 107, type: !31)
!665 = !DILocation(line: 107, column: 22, scope: !661)
!666 = !DILocation(line: 107, column: 11, scope: !661)
!667 = distinct !DISubprogram(name: "PALLAS_SPEC_40", scope: !10, file: !10, line: 108, type: !586, scopeLine: 108, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!668 = !DILocalVariable(name: "p", arg: 1, scope: !667, file: !10, line: 108, type: !588)
!669 = !DILocation(line: 0, scope: !667)
!670 = !DILocalVariable(name: "len", arg: 2, scope: !667, file: !10, line: 108, type: !31)
!671 = !DILocation(line: 108, column: 14, scope: !667)
!672 = !DILocation(line: 108, column: 17, scope: !667)
!673 = !DILocation(line: 108, column: 25, scope: !667)
!674 = !DILocation(line: 108, column: 43, scope: !667)
!675 = !DILocation(line: 108, column: 28, scope: !667)
!676 = !DILocation(line: 108, column: 50, scope: !667)
!677 = !DILocation(line: 108, column: 47, scope: !667)
!678 = distinct !DISubprogram(name: "PALLAS_SPEC_41", scope: !10, file: !10, line: 109, type: !586, scopeLine: 109, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!679 = !DILocalVariable(name: "p", arg: 1, scope: !678, file: !10, line: 109, type: !588)
!680 = !DILocation(line: 0, scope: !678)
!681 = !DILocalVariable(name: "len", arg: 2, scope: !678, file: !10, line: 109, type: !31)
!682 = !DILocation(line: 109, column: 29, scope: !678)
!683 = !DILocation(line: 109, column: 26, scope: !678)
!684 = !DILocation(line: 109, column: 47, scope: !678)
!685 = !DILocation(line: 109, column: 59, scope: !678)
!686 = !DILocation(line: 109, column: 76, scope: !678)
!687 = !DILocation(line: 109, column: 73, scope: !678)
!688 = !DILocation(line: 109, column: 89, scope: !678)
!689 = !DILocation(line: 109, column: 101, scope: !678)
!690 = !DILocation(line: 109, column: 66, scope: !678)
!691 = !DILocation(line: 109, column: 42, scope: !678)
!692 = !DILocation(line: 109, column: 19, scope: !678)
!693 = !DILocation(line: 109, column: 118, scope: !678)
!694 = !DILocation(line: 109, column: 133, scope: !678)
!695 = !DILocation(line: 109, column: 130, scope: !678)
!696 = !DILocation(line: 109, column: 149, scope: !678)
!697 = !DILocation(line: 109, column: 154, scope: !678)
!698 = !DILocation(line: 109, column: 152, scope: !678)
!699 = !DILocation(line: 109, column: 172, scope: !678)
!700 = !DILocation(line: 109, column: 177, scope: !678)
!701 = !DILocation(line: 109, column: 175, scope: !678)
!702 = !DILocation(line: 109, column: 166, scope: !678)
!703 = !DILocation(line: 109, column: 111, scope: !678)
!704 = !DILocation(line: 109, column: 11, scope: !678)
!705 = distinct !DISubprogram(name: "PALLAS_SPEC_42", scope: !10, file: !10, line: 110, type: !586, scopeLine: 110, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!706 = !DILocalVariable(name: "p", arg: 1, scope: !705, file: !10, line: 110, type: !588)
!707 = !DILocation(line: 0, scope: !705)
!708 = !DILocalVariable(name: "len", arg: 2, scope: !705, file: !10, line: 110, type: !31)
!709 = !DILocation(line: 110, column: 30, scope: !705)
!710 = !DILocation(line: 110, column: 27, scope: !705)
!711 = !DILocation(line: 110, column: 43, scope: !705)
!712 = !DILocation(line: 110, column: 55, scope: !705)
!713 = !DILocation(line: 110, column: 20, scope: !705)
!714 = !DILocation(line: 110, column: 73, scope: !705)
!715 = !DILocation(line: 110, column: 76, scope: !705)
!716 = !DILocation(line: 110, column: 70, scope: !705)
!717 = !DILocation(line: 110, column: 90, scope: !705)
!718 = !DILocation(line: 110, column: 63, scope: !705)
!719 = !DILocation(line: 110, column: 11, scope: !705)
!720 = distinct !DISubprogram(name: "PALLAS_SPEC_43", scope: !10, file: !10, line: 111, type: !586, scopeLine: 111, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!721 = !DILocalVariable(name: "p", arg: 1, scope: !720, file: !10, line: 111, type: !588)
!722 = !DILocation(line: 0, scope: !720)
!723 = !DILocalVariable(name: "len", arg: 2, scope: !720, file: !10, line: 111, type: !31)
!724 = !DILocation(line: 111, column: 22, scope: !720)
!725 = !DILocation(line: 111, column: 28, scope: !720)
!726 = !DILocation(line: 111, column: 48, scope: !720)
!727 = !DILocation(line: 111, column: 45, scope: !720)
!728 = !DILocation(line: 111, column: 54, scope: !720)
!729 = !DILocation(line: 111, column: 61, scope: !720)
!730 = !DILocation(line: 111, column: 58, scope: !720)
!731 = !DILocation(line: 111, column: 67, scope: !720)
!732 = !DILocation(line: 111, column: 56, scope: !720)
!733 = !DILocation(line: 111, column: 74, scope: !720)
!734 = !DILocation(line: 111, column: 71, scope: !720)
!735 = !DILocation(line: 111, column: 80, scope: !720)
!736 = !DILocation(line: 111, column: 69, scope: !720)
!737 = !DILocation(line: 111, column: 82, scope: !720)
!738 = !DILocation(line: 111, column: 41, scope: !720)
!739 = !DILocation(line: 111, column: 11, scope: !720)
!740 = distinct !DISubprogram(name: "PALLAS_SPEC_47", scope: !10, file: !10, line: 119, type: !741, scopeLine: 119, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!741 = !DISubroutineType(types: !742)
!742 = !{!381, !588, !31, !31, !31}
!743 = !DILocalVariable(name: "p", arg: 1, scope: !740, file: !10, line: 119, type: !588)
!744 = !DILocation(line: 0, scope: !740)
!745 = !DILocalVariable(name: "len", arg: 2, scope: !740, file: !10, line: 119, type: !31)
!746 = !DILocalVariable(name: "sum", arg: 3, scope: !740, file: !10, line: 119, type: !31)
!747 = !DILocalVariable(name: "i", arg: 4, scope: !740, file: !10, line: 119, type: !31)
!748 = !DILocation(line: 119, column: 25, scope: !740)
!749 = !DILocation(line: 119, column: 28, scope: !740)
!750 = !DILocation(line: 119, column: 36, scope: !740)
!751 = !DILocation(line: 119, column: 54, scope: !740)
!752 = !DILocation(line: 119, column: 39, scope: !740)
!753 = !DILocation(line: 119, column: 61, scope: !740)
!754 = !DILocation(line: 119, column: 58, scope: !740)
!755 = distinct !DISubprogram(name: "PALLAS_SPEC_44", scope: !10, file: !10, line: 116, type: !741, scopeLine: 116, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!756 = !DILocalVariable(name: "p", arg: 1, scope: !755, file: !10, line: 116, type: !588)
!757 = !DILocation(line: 0, scope: !755)
!758 = !DILocalVariable(name: "len", arg: 2, scope: !755, file: !10, line: 116, type: !31)
!759 = !DILocalVariable(name: "sum", arg: 3, scope: !755, file: !10, line: 116, type: !31)
!760 = !DILocalVariable(name: "i", arg: 4, scope: !755, file: !10, line: 116, type: !31)
!761 = !DILocation(line: 116, column: 23, scope: !755)
!762 = !DILocation(line: 116, column: 27, scope: !755)
!763 = !DILocation(line: 116, column: 31, scope: !755)
!764 = distinct !DISubprogram(name: "PALLAS_SPEC_45", scope: !10, file: !10, line: 117, type: !741, scopeLine: 117, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!765 = !DILocalVariable(name: "p", arg: 1, scope: !764, file: !10, line: 117, type: !588)
!766 = !DILocation(line: 0, scope: !764)
!767 = !DILocalVariable(name: "len", arg: 2, scope: !764, file: !10, line: 117, type: !31)
!768 = !DILocalVariable(name: "sum", arg: 3, scope: !764, file: !10, line: 117, type: !31)
!769 = !DILocalVariable(name: "i", arg: 4, scope: !764, file: !10, line: 117, type: !31)
!770 = !DILocation(line: 117, column: 24, scope: !764)
!771 = distinct !DISubprogram(name: "PALLAS_SPEC_46", scope: !10, file: !10, line: 118, type: !741, scopeLine: 118, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!772 = !DILocalVariable(name: "p", arg: 1, scope: !771, file: !10, line: 118, type: !588)
!773 = !DILocation(line: 0, scope: !771)
!774 = !DILocalVariable(name: "len", arg: 2, scope: !771, file: !10, line: 118, type: !31)
!775 = !DILocalVariable(name: "sum", arg: 3, scope: !771, file: !10, line: 118, type: !31)
!776 = !DILocalVariable(name: "i", arg: 4, scope: !771, file: !10, line: 118, type: !31)
!777 = !DILocation(line: 118, column: 33, scope: !771)
!778 = !DILocation(line: 118, column: 22, scope: !771)
!779 = distinct !DISubprogram(name: "PALLAS_SPEC_48", scope: !10, file: !10, line: 120, type: !741, scopeLine: 120, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!780 = !DILocalVariable(name: "p", arg: 1, scope: !779, file: !10, line: 120, type: !588)
!781 = !DILocation(line: 0, scope: !779)
!782 = !DILocalVariable(name: "len", arg: 2, scope: !779, file: !10, line: 120, type: !31)
!783 = !DILocalVariable(name: "sum", arg: 3, scope: !779, file: !10, line: 120, type: !31)
!784 = !DILocalVariable(name: "i", arg: 4, scope: !779, file: !10, line: 120, type: !31)
!785 = !DILocation(line: 120, column: 40, scope: !779)
!786 = !DILocation(line: 120, column: 37, scope: !779)
!787 = !DILocation(line: 120, column: 58, scope: !779)
!788 = !DILocation(line: 120, column: 70, scope: !779)
!789 = !DILocation(line: 120, column: 87, scope: !779)
!790 = !DILocation(line: 120, column: 84, scope: !779)
!791 = !DILocation(line: 120, column: 100, scope: !779)
!792 = !DILocation(line: 120, column: 112, scope: !779)
!793 = !DILocation(line: 120, column: 77, scope: !779)
!794 = !DILocation(line: 120, column: 53, scope: !779)
!795 = !DILocation(line: 120, column: 30, scope: !779)
!796 = !DILocation(line: 120, column: 129, scope: !779)
!797 = !DILocation(line: 120, column: 144, scope: !779)
!798 = !DILocation(line: 120, column: 141, scope: !779)
!799 = !DILocation(line: 120, column: 160, scope: !779)
!800 = !DILocation(line: 120, column: 165, scope: !779)
!801 = !DILocation(line: 120, column: 163, scope: !779)
!802 = !DILocation(line: 120, column: 183, scope: !779)
!803 = !DILocation(line: 120, column: 188, scope: !779)
!804 = !DILocation(line: 120, column: 186, scope: !779)
!805 = !DILocation(line: 120, column: 177, scope: !779)
!806 = !DILocation(line: 120, column: 122, scope: !779)
!807 = !DILocation(line: 120, column: 22, scope: !779)
!808 = distinct !DISubprogram(name: "PALLAS_SPEC_49", scope: !10, file: !10, line: 121, type: !741, scopeLine: 121, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!809 = !DILocalVariable(name: "p", arg: 1, scope: !808, file: !10, line: 121, type: !588)
!810 = !DILocation(line: 0, scope: !808)
!811 = !DILocalVariable(name: "len", arg: 2, scope: !808, file: !10, line: 121, type: !31)
!812 = !DILocalVariable(name: "sum", arg: 3, scope: !808, file: !10, line: 121, type: !31)
!813 = !DILocalVariable(name: "i", arg: 4, scope: !808, file: !10, line: 121, type: !31)
!814 = !DILocation(line: 121, column: 41, scope: !808)
!815 = !DILocation(line: 121, column: 38, scope: !808)
!816 = !DILocation(line: 121, column: 54, scope: !808)
!817 = !DILocation(line: 121, column: 66, scope: !808)
!818 = !DILocation(line: 121, column: 31, scope: !808)
!819 = !DILocation(line: 121, column: 84, scope: !808)
!820 = !DILocation(line: 121, column: 87, scope: !808)
!821 = !DILocation(line: 121, column: 81, scope: !808)
!822 = !DILocation(line: 121, column: 101, scope: !808)
!823 = !DILocation(line: 121, column: 74, scope: !808)
!824 = !DILocation(line: 121, column: 22, scope: !808)
!825 = distinct !DISubprogram(name: "PALLAS_SPEC_50", scope: !10, file: !10, line: 122, type: !741, scopeLine: 122, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!826 = !DILocalVariable(name: "p", arg: 1, scope: !825, file: !10, line: 122, type: !588)
!827 = !DILocation(line: 0, scope: !825)
!828 = !DILocalVariable(name: "len", arg: 2, scope: !825, file: !10, line: 122, type: !31)
!829 = !DILocalVariable(name: "sum", arg: 3, scope: !825, file: !10, line: 122, type: !31)
!830 = !DILocalVariable(name: "i", arg: 4, scope: !825, file: !10, line: 122, type: !31)
!831 = !DILocation(line: 122, column: 31, scope: !825)
!832 = !DILocation(line: 122, column: 41, scope: !825)
!833 = !DILocation(line: 122, column: 22, scope: !825)
!834 = distinct !DISubprogram(name: "PALLAS_SPEC_51", scope: !10, file: !10, line: 123, type: !741, scopeLine: 123, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!835 = !DILocalVariable(name: "p", arg: 1, scope: !834, file: !10, line: 123, type: !588)
!836 = !DILocation(line: 0, scope: !834)
!837 = !DILocalVariable(name: "len", arg: 2, scope: !834, file: !10, line: 123, type: !31)
!838 = !DILocalVariable(name: "sum", arg: 3, scope: !834, file: !10, line: 123, type: !31)
!839 = !DILocalVariable(name: "i", arg: 4, scope: !834, file: !10, line: 123, type: !31)
!840 = !DILocation(line: 123, column: 31, scope: !834)
!841 = !DILocation(line: 123, column: 48, scope: !834)
!842 = !DILocation(line: 123, column: 45, scope: !834)
!843 = !DILocation(line: 123, column: 54, scope: !834)
!844 = !DILocation(line: 123, column: 41, scope: !834)
!845 = !DILocation(line: 123, column: 22, scope: !834)
!846 = distinct !DISubprogram(name: "PALLAS_SPEC_52", scope: !10, file: !10, line: 124, type: !741, scopeLine: 124, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!847 = !DILocalVariable(name: "p", arg: 1, scope: !846, file: !10, line: 124, type: !588)
!848 = !DILocation(line: 0, scope: !846)
!849 = !DILocalVariable(name: "len", arg: 2, scope: !846, file: !10, line: 124, type: !31)
!850 = !DILocalVariable(name: "sum", arg: 3, scope: !846, file: !10, line: 124, type: !31)
!851 = !DILocalVariable(name: "i", arg: 4, scope: !846, file: !10, line: 124, type: !31)
!852 = !DILocation(line: 124, column: 31, scope: !846)
!853 = !DILocation(line: 124, column: 48, scope: !846)
!854 = !DILocation(line: 124, column: 45, scope: !846)
!855 = !DILocation(line: 124, column: 54, scope: !846)
!856 = !DILocation(line: 124, column: 61, scope: !846)
!857 = !DILocation(line: 124, column: 58, scope: !846)
!858 = !DILocation(line: 124, column: 67, scope: !846)
!859 = !DILocation(line: 124, column: 56, scope: !846)
!860 = !DILocation(line: 124, column: 41, scope: !846)
!861 = !DILocation(line: 124, column: 22, scope: !846)
!862 = distinct !DISubprogram(name: "PALLAS_SPEC_53", scope: !10, file: !10, line: 125, type: !741, scopeLine: 125, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!863 = !DILocalVariable(name: "p", arg: 1, scope: !862, file: !10, line: 125, type: !588)
!864 = !DILocation(line: 0, scope: !862)
!865 = !DILocalVariable(name: "len", arg: 2, scope: !862, file: !10, line: 125, type: !31)
!866 = !DILocalVariable(name: "sum", arg: 3, scope: !862, file: !10, line: 125, type: !31)
!867 = !DILocalVariable(name: "i", arg: 4, scope: !862, file: !10, line: 125, type: !31)
!868 = !DILocation(line: 125, column: 31, scope: !862)
!869 = !DILocation(line: 125, column: 48, scope: !862)
!870 = !DILocation(line: 125, column: 45, scope: !862)
!871 = !DILocation(line: 125, column: 54, scope: !862)
!872 = !DILocation(line: 125, column: 61, scope: !862)
!873 = !DILocation(line: 125, column: 58, scope: !862)
!874 = !DILocation(line: 125, column: 67, scope: !862)
!875 = !DILocation(line: 125, column: 56, scope: !862)
!876 = !DILocation(line: 125, column: 74, scope: !862)
!877 = !DILocation(line: 125, column: 71, scope: !862)
!878 = !DILocation(line: 125, column: 80, scope: !862)
!879 = !DILocation(line: 125, column: 69, scope: !862)
!880 = !DILocation(line: 125, column: 41, scope: !862)
!881 = !DILocation(line: 125, column: 22, scope: !862)
!882 = distinct !DISubprogram(name: "PALLAS_SPEC_54", scope: !10, file: !10, line: 140, type: !883, scopeLine: 140, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!883 = !DISubroutineType(types: !884)
!884 = !{!381, !383, !382}
!885 = !DILocalVariable(name: "p", arg: 1, scope: !882, file: !10, line: 140, type: !383)
!886 = !DILocation(line: 0, scope: !882)
!887 = !DILocalVariable(name: "pp", arg: 2, scope: !882, file: !10, line: 140, type: !382)
!888 = !DILocation(line: 140, column: 19, scope: !882)
!889 = distinct !DISubprogram(name: "PALLAS_SPEC_55", scope: !10, file: !10, line: 144, type: !883, scopeLine: 144, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!890 = !DILocalVariable(name: "p", arg: 1, scope: !889, file: !10, line: 144, type: !383)
!891 = !DILocation(line: 0, scope: !889)
!892 = !DILocalVariable(name: "pp", arg: 2, scope: !889, file: !10, line: 144, type: !382)
!893 = !DILocation(line: 144, column: 20, scope: !889)
!894 = !DILocation(line: 144, column: 22, scope: !889)
!895 = distinct !DISubprogram(name: "PALLAS_SPEC_56", scope: !10, file: !10, line: 145, type: !883, scopeLine: 145, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!896 = !DILocalVariable(name: "p", arg: 1, scope: !895, file: !10, line: 145, type: !383)
!897 = !DILocation(line: 0, scope: !895)
!898 = !DILocalVariable(name: "pp", arg: 2, scope: !895, file: !10, line: 145, type: !382)
!899 = !DILocation(line: 145, column: 16, scope: !895)
!900 = !DILocation(line: 145, column: 18, scope: !895)
!901 = distinct !DISubprogram(name: "PALLAS_SPEC_57", scope: !10, file: !10, line: 147, type: !883, scopeLine: 147, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!902 = !DILocalVariable(name: "p", arg: 1, scope: !901, file: !10, line: 147, type: !383)
!903 = !DILocation(line: 0, scope: !901)
!904 = !DILocalVariable(name: "pp", arg: 2, scope: !901, file: !10, line: 147, type: !382)
!905 = !DILocation(line: 147, column: 18, scope: !901)
!906 = !DILocation(line: 147, column: 20, scope: !901)
!907 = distinct !DISubprogram(name: "PALLAS_SPEC_58", scope: !10, file: !10, line: 148, type: !883, scopeLine: 148, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!908 = !DILocalVariable(name: "p", arg: 1, scope: !907, file: !10, line: 148, type: !383)
!909 = !DILocation(line: 0, scope: !907)
!910 = !DILocalVariable(name: "pp", arg: 2, scope: !907, file: !10, line: 148, type: !382)
!911 = !DILocation(line: 148, column: 14, scope: !907)
!912 = !DILocation(line: 148, column: 16, scope: !907)
!913 = distinct !DISubprogram(name: "PALLAS_SPEC_59", scope: !10, file: !10, line: 151, type: !883, scopeLine: 151, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!914 = !DILocalVariable(name: "p", arg: 1, scope: !913, file: !10, line: 151, type: !383)
!915 = !DILocation(line: 0, scope: !913)
!916 = !DILocalVariable(name: "pp", arg: 2, scope: !913, file: !10, line: 151, type: !382)
!917 = !DILocation(line: 151, column: 20, scope: !913)
!918 = !DILocation(line: 151, column: 22, scope: !913)
!919 = distinct !DISubprogram(name: "PALLAS_SPEC_60", scope: !10, file: !10, line: 152, type: !883, scopeLine: 152, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!920 = !DILocalVariable(name: "p", arg: 1, scope: !919, file: !10, line: 152, type: !383)
!921 = !DILocation(line: 0, scope: !919)
!922 = !DILocalVariable(name: "pp", arg: 2, scope: !919, file: !10, line: 152, type: !382)
!923 = !DILocation(line: 152, column: 14, scope: !919)
!924 = !DILocation(line: 152, column: 16, scope: !919)
!925 = distinct !DISubprogram(name: "PALLAS_SPEC_61", scope: !10, file: !10, line: 154, type: !926, scopeLine: 154, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!926 = !DISubroutineType(types: !927)
!927 = !{!381, !383, !382, !383, !383, !383}
!928 = !DILocalVariable(name: "p", arg: 1, scope: !925, file: !10, line: 154, type: !383)
!929 = !DILocation(line: 0, scope: !925)
!930 = !DILocalVariable(name: "pp", arg: 2, scope: !925, file: !10, line: 154, type: !382)
!931 = !DILocalVariable(name: "p1", arg: 3, scope: !925, file: !10, line: 154, type: !383)
!932 = !DILocalVariable(name: "p2", arg: 4, scope: !925, file: !10, line: 154, type: !383)
!933 = !DILocalVariable(name: "p3", arg: 5, scope: !925, file: !10, line: 154, type: !383)
!934 = !DILocation(line: 154, column: 18, scope: !925)
!935 = !DILocation(line: 154, column: 20, scope: !925)
!936 = !DILocation(line: 154, column: 25, scope: !925)
!937 = !DILocation(line: 154, column: 30, scope: !925)
!938 = !DILocation(line: 154, column: 32, scope: !925)
!939 = distinct !DISubprogram(name: "PALLAS_SPEC_62", scope: !10, file: !10, line: 165, type: !940, scopeLine: 165, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!940 = !DISubroutineType(types: !941)
!941 = !{!381, !383, !382, !383, !383, !383, !552, !551, !382}
!942 = !DILocalVariable(name: "p", arg: 1, scope: !939, file: !10, line: 165, type: !383)
!943 = !DILocation(line: 0, scope: !939)
!944 = !DILocalVariable(name: "pp", arg: 2, scope: !939, file: !10, line: 165, type: !382)
!945 = !DILocalVariable(name: "p1", arg: 3, scope: !939, file: !10, line: 165, type: !383)
!946 = !DILocalVariable(name: "p2", arg: 4, scope: !939, file: !10, line: 165, type: !383)
!947 = !DILocalVariable(name: "p3", arg: 5, scope: !939, file: !10, line: 165, type: !383)
!948 = !DILocalVariable(name: "r", arg: 6, scope: !939, file: !10, line: 165, type: !552)
!949 = !DILocalVariable(name: "rr", arg: 7, scope: !939, file: !10, line: 165, type: !551)
!950 = !DILocalVariable(name: "ps", arg: 8, scope: !939, file: !10, line: 165, type: !382)
!951 = !DILocation(line: 165, column: 16, scope: !939)
!952 = !DILocation(line: 165, column: 26, scope: !939)
!953 = distinct !DISubprogram(name: "PALLAS_SPEC_63", scope: !10, file: !10, line: 171, type: !954, scopeLine: 171, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !33)
!954 = !DISubroutineType(types: !955)
!955 = !{!381, !383, !382, !383, !383, !383, !552, !551, !382, !589, !588, !31}
!956 = !DILocalVariable(name: "p", arg: 1, scope: !953, file: !10, line: 171, type: !383)
!957 = !DILocation(line: 0, scope: !953)
!958 = !DILocalVariable(name: "pp", arg: 2, scope: !953, file: !10, line: 171, type: !382)
!959 = !DILocalVariable(name: "p1", arg: 3, scope: !953, file: !10, line: 171, type: !383)
!960 = !DILocalVariable(name: "p2", arg: 4, scope: !953, file: !10, line: 171, type: !383)
!961 = !DILocalVariable(name: "p3", arg: 5, scope: !953, file: !10, line: 171, type: !383)
!962 = !DILocalVariable(name: "r", arg: 6, scope: !953, file: !10, line: 171, type: !552)
!963 = !DILocalVariable(name: "rr", arg: 7, scope: !953, file: !10, line: 171, type: !551)
!964 = !DILocalVariable(name: "ps", arg: 8, scope: !953, file: !10, line: 171, type: !382)
!965 = !DILocalVariable(name: "pol", arg: 9, scope: !953, file: !10, line: 171, type: !589)
!966 = !DILocalVariable(name: "ppols", arg: 10, scope: !953, file: !10, line: 171, type: !588)
!967 = !DILocalVariable(name: "avr_pol", arg: 11, scope: !953, file: !10, line: 171, type: !31)
!968 = !DILocation(line: 171, column: 24, scope: !953)
!969 = !{!"pallas.old"}
!970 = !{!"pallas.result"}
!971 = !{!"pallas.ptrLength"}
!972 = !{!"pallas.forall"}
!973 = !{!"pallas.forallSep"}
!974 = !{!"pallas.perm"}
!975 = !{!"pallas.fracOf"}
!976 = !{!"pallas.scAnd"}
!977 = !{!"pallas.boundVar"}
!978 = !{!"pallas.imply"}
