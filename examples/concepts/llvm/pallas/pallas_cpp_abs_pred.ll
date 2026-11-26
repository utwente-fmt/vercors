; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_cpp_abs_pred.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%pallas.seq.i32 = type { i32, i64, i64, i64 }

$_ZN6Buffer11make_bufferEi = comdat any

$_ZN6Buffer3setEii = comdat any

$_ZN6Buffer3getEi = comdat any

$_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE = comdat any

$_ZN6Buffer13PALLAS_SPEC_0EPS_iN10pallasSpec3seqIiEE = comdat any

$_ZN6Buffer13PALLAS_SPEC_3EPS_iN10pallasSpec3seqIiEE = comdat any

$_ZN6Buffer13PALLAS_SPEC_2EPS_iN10pallasSpec3seqIiEE = comdat any

$_ZN6Buffer13PALLAS_SPEC_4EPS_iiN10pallasSpec3seqIiEE = comdat any

$_ZN6Buffer13PALLAS_SPEC_1EPS_iN10pallasSpec3seqIiEE = comdat any

$_ZN6Buffer13PALLAS_SPEC_5EPS_iiN10pallasSpec3seqIiEE = comdat any

$_ZN6Buffer13PALLAS_SPEC_6EPS_iiN10pallasSpec3seqIiEES3_ = comdat any

$_ZN6Buffer13PALLAS_SPEC_7EPS_iiN10pallasSpec3seqIiEES3_ = comdat any

$_ZN6Buffer13PALLAS_SPEC_8EPS_iiN10pallasSpec3seqIiEES3_ = comdat any

$_ZN6Buffer13PALLAS_SPEC_9EPS_iiN10pallasSpec3seqIiEES3_ = comdat any

$_ZN6Buffer14PALLAS_SPEC_10Ei = comdat any

$_ZN6Buffer14PALLAS_SPEC_11EiN10pallasSpec3seqIiEE = comdat any

$_ZN6Buffer14PALLAS_SPEC_12EiN10pallasSpec3seqIiEE = comdat any

$_ZN6Buffer14PALLAS_SPEC_13EiN10pallasSpec3seqIiEE = comdat any

$_ZN6Buffer14PALLAS_SPEC_14EiN10pallasSpec3seqIiEE = comdat any

@llvm.compiler.used = appending global [5 x ptr] [ptr @_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE, ptr @_Z14PALLAS_SPEC_15N10pallasSpec3seqIiEE, ptr @_Z14PALLAS_SPEC_16P6BufferiN10pallasSpec3seqIiEE, ptr @_Z14PALLAS_SPEC_17P6BufferN10pallasSpec3seqIiEE, ptr @_Z14PALLAS_SPEC_18P6BufferiN10pallasSpec3seqIiEE], section "llvm.metadata"
@llvm.used = appending global [20 x ptr] [ptr @_ZN6Buffer13PALLAS_SPEC_0EPS_iN10pallasSpec3seqIiEE, ptr @_ZN6Buffer13PALLAS_SPEC_3EPS_iN10pallasSpec3seqIiEE, ptr @_ZN6Buffer13PALLAS_SPEC_2EPS_iN10pallasSpec3seqIiEE, ptr @_ZN6Buffer13PALLAS_SPEC_4EPS_iiN10pallasSpec3seqIiEE, ptr @_ZN6Buffer13PALLAS_SPEC_1EPS_iN10pallasSpec3seqIiEE, ptr @_ZN6Buffer13PALLAS_SPEC_5EPS_iiN10pallasSpec3seqIiEE, ptr @_ZN6Buffer13PALLAS_SPEC_6EPS_iiN10pallasSpec3seqIiEES3_, ptr @_ZN6Buffer13PALLAS_SPEC_7EPS_iiN10pallasSpec3seqIiEES3_, ptr @_ZN6Buffer13PALLAS_SPEC_8EPS_iiN10pallasSpec3seqIiEES3_, ptr @_ZN6Buffer13PALLAS_SPEC_9EPS_iiN10pallasSpec3seqIiEES3_, ptr @_ZN6Buffer14PALLAS_SPEC_10Ei, ptr @_ZN6Buffer14PALLAS_SPEC_11EiN10pallasSpec3seqIiEE, ptr @_ZN6Buffer14PALLAS_SPEC_12EiN10pallasSpec3seqIiEE, ptr @_ZN6Buffer14PALLAS_SPEC_13EiN10pallasSpec3seqIiEE, ptr @_ZN6Buffer14PALLAS_SPEC_14EiN10pallasSpec3seqIiEE, ptr @_Z14PALLAS_SPEC_15N10pallasSpec3seqIiEE, ptr @_Z14PALLAS_SPEC_16P6BufferiN10pallasSpec3seqIiEE, ptr @_Z14PALLAS_SPEC_17P6BufferN10pallasSpec3seqIiEE, ptr @_Z14PALLAS_SPEC_18P6BufferiN10pallasSpec3seqIiEE, ptr @_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0

; Function Attrs: mustprogress noinline norecurse nounwind uwtable
define dso_local noundef i32 @main() #0 !dbg !122 !pallas.fcontract !126 {
  %1 = alloca i32, align 4
  %2 = alloca ptr, align 8
  %3 = alloca i32, align 4
  store i32 0, ptr %1, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !151, metadata !DIExpression()), !dbg !168
  %4 = call noundef ptr @_ZN6Buffer11make_bufferEi(i32 noundef 42), !dbg !169, !pallas.yieldsBindings !170
  store ptr %4, ptr %2, align 8, !dbg !168
  %5 = load ptr, ptr %2, align 8, !dbg !176
  call void @_ZN6Buffer3setEii(ptr noundef nonnull align 8 dereferenceable(12) %5, i32 noundef 0, i32 noundef 16), !dbg !177, !pallas.givenBindings !178, !pallas.yieldsBindings !236
  call void @llvm.dbg.declare(metadata ptr %3, metadata !242, metadata !DIExpression()), !dbg !243
  %6 = load ptr, ptr %2, align 8, !dbg !244
  %7 = call noundef i32 @_ZN6Buffer3getEi(ptr noundef nonnull align 8 dereferenceable(12) %6, i32 noundef 0), !dbg !245, !pallas.givenBindings !246
  store i32 %7, ptr %3, align 4, !dbg !243
  ret i32 0, !dbg !263, !pallas.stmntBlock !264
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef ptr @_ZN6Buffer11make_bufferEi(i32 noundef %0) #2 comdat align 2 !dbg !279 !pallas.fcontract !280 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !287, metadata !DIExpression()), !dbg !330
  ret ptr null, !dbg !331
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local void @_ZN6Buffer3setEii(ptr noundef nonnull align 8 dereferenceable(12) %0, i32 noundef %1, i32 noundef %2) #2 comdat align 2 !dbg !332 !pallas.fcontract !333 {
  %4 = alloca ptr, align 8
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store ptr %0, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !347, metadata !DIExpression()), !dbg !434
  store i32 %1, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !350, metadata !DIExpression()), !dbg !435
  store i32 %2, ptr %6, align 4
  call void @llvm.dbg.declare(metadata ptr %6, metadata !353, metadata !DIExpression()), !dbg !436
  %7 = load ptr, ptr %4, align 8
  ret void, !dbg !437
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef i32 @_ZN6Buffer3getEi(ptr noundef nonnull align 8 dereferenceable(12) %0, i32 noundef %1) #2 comdat align 2 !dbg !438 !pallas.fcontract !439 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !450, metadata !DIExpression()), !dbg !488
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !453, metadata !DIExpression()), !dbg !489
  %5 = load ptr, ptr %3, align 8
  ret i32 42, !dbg !490
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE(ptr noundef nonnull align 8 dereferenceable(12) %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #2 comdat align 2 !dbg !491 !pallas.predDef !492 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !493, metadata !DIExpression()), !dbg !494
  call void @llvm.dbg.declare(metadata ptr %1, metadata !495, metadata !DIExpression()), !dbg !494
  ret i1 true, !dbg !494
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_15N10pallasSpec3seqIiEE(ptr noundef byval(%pallas.seq.i32) align 8 %0) #2 !dbg !137 !pallas.exprWrapper !496 {
  %2 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.declare(metadata ptr %0, metadata !136, metadata !DIExpression()), !dbg !497
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %0, i64 60500880, i1 false), !dbg !498
  %3 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %2), !dbg !499
  %4 = icmp ugt i64 %3, 0, !dbg !500
  ret i1 %4, !dbg !497
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z14PALLAS_SPEC_16P6BufferiN10pallasSpec3seqIiEE(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #2 !dbg !271 !pallas.exprWrapper !496 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !276, metadata !DIExpression()), !dbg !501
  call void @llvm.dbg.value(metadata i32 %1, metadata !278, metadata !DIExpression()), !dbg !501
  call void @llvm.dbg.declare(metadata ptr %2, metadata !270, metadata !DIExpression()), !dbg !501
  %4 = icmp eq i32 %1, 16, !dbg !502
  ret i1 %4, !dbg !501
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local void @_Z14PALLAS_SPEC_17P6BufferN10pallasSpec3seqIiEE(ptr noalias sret(%pallas.seq.i32) align 8 %0, ptr noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #2 !dbg !185 !pallas.ghostWrapper !496 {
  call void @llvm.dbg.value(metadata ptr %1, metadata !233, metadata !DIExpression()), !dbg !503
  call void @llvm.dbg.declare(metadata ptr %2, metadata !184, metadata !DIExpression()), !dbg !503
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %2, i64 60500880, i1 false), !dbg !504
  ret void, !dbg !503
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local void @_Z14PALLAS_SPEC_18P6BufferiN10pallasSpec3seqIiEE(ptr noalias sret(%pallas.seq.i32) align 8 %0, ptr noundef %1, i32 noundef %2, ptr noundef byval(%pallas.seq.i32) align 8 %3) #2 !dbg !253 !pallas.ghostWrapper !496 {
  call void @llvm.dbg.value(metadata ptr %1, metadata !258, metadata !DIExpression()), !dbg !505
  call void @llvm.dbg.value(metadata i32 %2, metadata !260, metadata !DIExpression()), !dbg !505
  call void @llvm.dbg.declare(metadata ptr %3, metadata !252, metadata !DIExpression()), !dbg !505
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %3, i64 60500880, i1 false), !dbg !506
  ret void, !dbg !505
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #3

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer13PALLAS_SPEC_0EPS_iN10pallasSpec3seqIiEE(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #2 comdat align 2 !dbg !447 !pallas.exprWrapper !496 {
  %4 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !451, metadata !DIExpression()), !dbg !507
  call void @llvm.dbg.value(metadata i32 %1, metadata !454, metadata !DIExpression()), !dbg !507
  call void @llvm.dbg.declare(metadata ptr %2, metadata !446, metadata !DIExpression()), !dbg !507
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %2, i64 60500880, i1 false), !dbg !508
  %5 = call noundef zeroext i1 @_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE(ptr noundef nonnull align 8 dereferenceable(12) %0, ptr noundef byval(%pallas.seq.i32) align 8 %4), !dbg !509
  ret i1 %5, !dbg !507
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer13PALLAS_SPEC_3EPS_iN10pallasSpec3seqIiEE(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #2 comdat align 2 !dbg !482 !pallas.exprWrapper !496 {
  %4 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !485, metadata !DIExpression()), !dbg !510
  call void @llvm.dbg.value(metadata i32 %1, metadata !487, metadata !DIExpression()), !dbg !510
  call void @llvm.dbg.declare(metadata ptr %2, metadata !481, metadata !DIExpression()), !dbg !510
  %5 = call noundef i32 @"pallas.result noundef i32"(), !dbg !511
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %2, i64 60500880, i1 false), !dbg !512
  %6 = sext i32 %1 to i64, !dbg !513
  %7 = call noundef i32 @"pallas.seq.get i32_noundef i32"(ptr noundef byval(%pallas.seq.i32) %4, i64 noundef %6), !dbg !514
  %8 = icmp eq i32 %5, %7, !dbg !515
  ret i1 %8, !dbg !510
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer13PALLAS_SPEC_2EPS_iN10pallasSpec3seqIiEE(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #2 comdat align 2 !dbg !471 !pallas.exprWrapper !496 {
  %4 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !474, metadata !DIExpression()), !dbg !516
  call void @llvm.dbg.value(metadata i32 %1, metadata !476, metadata !DIExpression()), !dbg !516
  call void @llvm.dbg.declare(metadata ptr %2, metadata !470, metadata !DIExpression()), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %2, i64 60500880, i1 false), !dbg !517
  %5 = call noundef zeroext i1 @_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE(ptr noundef nonnull align 8 dereferenceable(12) %0, ptr noundef byval(%pallas.seq.i32) align 8 %4), !dbg !518
  ret i1 %5, !dbg !516
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer13PALLAS_SPEC_4EPS_iiN10pallasSpec3seqIiEE(ptr noundef %0, i32 noundef %1, i32 noundef %2, ptr noundef byval(%pallas.seq.i32) align 8 %3) #2 comdat align 2 !dbg !342 !pallas.exprWrapper !496 {
  %5 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !348, metadata !DIExpression()), !dbg !519
  call void @llvm.dbg.value(metadata i32 %1, metadata !351, metadata !DIExpression()), !dbg !519
  call void @llvm.dbg.value(metadata i32 %2, metadata !354, metadata !DIExpression()), !dbg !519
  call void @llvm.dbg.declare(metadata ptr %3, metadata !341, metadata !DIExpression()), !dbg !519
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %3, i64 60500880, i1 false), !dbg !520
  %6 = call noundef zeroext i1 @_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE(ptr noundef nonnull align 8 dereferenceable(12) %0, ptr noundef byval(%pallas.seq.i32) align 8 %5), !dbg !521
  ret i1 %6, !dbg !519
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer13PALLAS_SPEC_1EPS_iN10pallasSpec3seqIiEE(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #2 comdat align 2 !dbg !460 !pallas.exprWrapper !496 {
  %4 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !463, metadata !DIExpression()), !dbg !522
  call void @llvm.dbg.value(metadata i32 %1, metadata !465, metadata !DIExpression()), !dbg !522
  call void @llvm.dbg.declare(metadata ptr %2, metadata !459, metadata !DIExpression()), !dbg !522
  %5 = icmp sle i32 0, %1, !dbg !523
  %6 = sext i32 %1 to i64, !dbg !523
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %2, i64 60500880, i1 false), !dbg !523
  %7 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %4), !dbg !523
  %8 = icmp ult i64 %6, %7, !dbg !523
  %9 = call i1 @pallas.scAnd(i1 %5, i1 %8), !dbg !523
  ret i1 %9, !dbg !522
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer13PALLAS_SPEC_5EPS_iiN10pallasSpec3seqIiEE(ptr noundef %0, i32 noundef %1, i32 noundef %2, ptr noundef byval(%pallas.seq.i32) align 8 %3) #2 comdat align 2 !dbg !360 !pallas.exprWrapper !496 {
  %5 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !363, metadata !DIExpression()), !dbg !524
  call void @llvm.dbg.value(metadata i32 %1, metadata !365, metadata !DIExpression()), !dbg !524
  call void @llvm.dbg.value(metadata i32 %2, metadata !367, metadata !DIExpression()), !dbg !524
  call void @llvm.dbg.declare(metadata ptr %3, metadata !359, metadata !DIExpression()), !dbg !524
  %6 = icmp sle i32 0, %1, !dbg !525
  %7 = sext i32 %1 to i64, !dbg !525
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %3, i64 60500880, i1 false), !dbg !525
  %8 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %5), !dbg !525
  %9 = icmp ult i64 %7, %8, !dbg !525
  %10 = call i1 @pallas.scAnd(i1 %6, i1 %9), !dbg !525
  ret i1 %10, !dbg !524
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer13PALLAS_SPEC_6EPS_iiN10pallasSpec3seqIiEES3_(ptr noundef %0, i32 noundef %1, i32 noundef %2, ptr noundef byval(%pallas.seq.i32) align 8 %3, ptr noundef byval(%pallas.seq.i32) align 8 %4) #2 comdat align 2 !dbg !373 !pallas.exprWrapper !496 {
  %6 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !381, metadata !DIExpression()), !dbg !526
  call void @llvm.dbg.value(metadata i32 %1, metadata !383, metadata !DIExpression()), !dbg !526
  call void @llvm.dbg.value(metadata i32 %2, metadata !385, metadata !DIExpression()), !dbg !526
  call void @llvm.dbg.declare(metadata ptr %3, metadata !372, metadata !DIExpression()), !dbg !526
  call void @llvm.dbg.declare(metadata ptr %4, metadata !378, metadata !DIExpression()), !dbg !526
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %4, i64 60500880, i1 false), !dbg !527
  %7 = call noundef zeroext i1 @_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE(ptr noundef nonnull align 8 dereferenceable(12) %0, ptr noundef byval(%pallas.seq.i32) align 8 %6), !dbg !528
  ret i1 %7, !dbg !526
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer13PALLAS_SPEC_7EPS_iiN10pallasSpec3seqIiEES3_(ptr noundef %0, i32 noundef %1, i32 noundef %2, ptr noundef byval(%pallas.seq.i32) align 8 %3, ptr noundef byval(%pallas.seq.i32) align 8 %4) #2 comdat align 2 !dbg !391 !pallas.exprWrapper !496 {
  %6 = alloca %pallas.seq.i32, align 8
  %7 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !397, metadata !DIExpression()), !dbg !529
  call void @llvm.dbg.value(metadata i32 %1, metadata !399, metadata !DIExpression()), !dbg !529
  call void @llvm.dbg.value(metadata i32 %2, metadata !401, metadata !DIExpression()), !dbg !529
  call void @llvm.dbg.declare(metadata ptr %3, metadata !390, metadata !DIExpression()), !dbg !529
  call void @llvm.dbg.declare(metadata ptr %4, metadata !394, metadata !DIExpression()), !dbg !529
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %3, i64 60500880, i1 false), !dbg !530
  %8 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %6), !dbg !531
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %4, i64 60500880, i1 false), !dbg !532
  %9 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %7), !dbg !533
  %10 = icmp eq i64 %8, %9, !dbg !534
  ret i1 %10, !dbg !529
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer13PALLAS_SPEC_8EPS_iiN10pallasSpec3seqIiEES3_(ptr noundef %0, i32 noundef %1, i32 noundef %2, ptr noundef byval(%pallas.seq.i32) align 8 %3, ptr noundef byval(%pallas.seq.i32) align 8 %4) #2 comdat align 2 !dbg !407 !pallas.exprWrapper !496 {
  %6 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !413, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.value(metadata i32 %1, metadata !415, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.value(metadata i32 %2, metadata !417, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.declare(metadata ptr %3, metadata !406, metadata !DIExpression()), !dbg !535
  call void @llvm.dbg.declare(metadata ptr %4, metadata !410, metadata !DIExpression()), !dbg !535
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %4, i64 60500880, i1 false), !dbg !536
  %7 = sext i32 %1 to i64, !dbg !537
  %8 = call noundef i32 @"pallas.seq.get i32_noundef i32"(ptr noundef byval(%pallas.seq.i32) %6, i64 noundef %7), !dbg !538
  %9 = icmp eq i32 %8, %2, !dbg !539
  ret i1 %9, !dbg !535
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer13PALLAS_SPEC_9EPS_iiN10pallasSpec3seqIiEES3_(ptr noundef %0, i32 noundef %1, i32 noundef %2, ptr noundef byval(%pallas.seq.i32) align 8 %3, ptr noundef byval(%pallas.seq.i32) align 8 %4) #2 comdat align 2 !dbg !423 !pallas.exprWrapper !496 {
  %6 = alloca %pallas.seq.i32, align 8
  %7 = alloca %pallas.seq.i32, align 8
  %8 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !429, metadata !DIExpression()), !dbg !540
  call void @llvm.dbg.value(metadata i32 %1, metadata !431, metadata !DIExpression()), !dbg !540
  call void @llvm.dbg.value(metadata i32 %2, metadata !433, metadata !DIExpression()), !dbg !540
  call void @llvm.dbg.declare(metadata ptr %3, metadata !422, metadata !DIExpression()), !dbg !540
  call void @llvm.dbg.declare(metadata ptr %4, metadata !426, metadata !DIExpression()), !dbg !540
  %9 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !541
  %10 = icmp sle i32 0, %9, !dbg !541
  %11 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !541
  %12 = sext i32 %11 to i64, !dbg !541
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %4, i64 60500880, i1 false), !dbg !541
  %13 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %6), !dbg !541
  %14 = icmp ult i64 %12, %13, !dbg !541
  %15 = call i1 @pallas.scAnd(i1 %10, i1 %14), !dbg !541
  %16 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !542
  %17 = icmp ne i32 %16, %1, !dbg !543
  %18 = call i1 @pallas.scAnd(i1 %15, i1 %17), !dbg !544
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %4, i64 60500880, i1 false), !dbg !545
  %19 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !546
  %20 = sext i32 %19 to i64, !dbg !546
  %21 = call noundef i32 @"pallas.seq.get i32_noundef i32"(ptr noundef byval(%pallas.seq.i32) %7, i64 noundef %20), !dbg !547
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %3, i64 60500880, i1 false), !dbg !548
  %22 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !549
  %23 = sext i32 %22 to i64, !dbg !549
  %24 = call noundef i32 @"pallas.seq.get i32_noundef i32"(ptr noundef byval(%pallas.seq.i32) %8, i64 noundef %23), !dbg !550
  %25 = icmp eq i32 %21, %24, !dbg !551
  %26 = call i1 @pallas.forall(i1 %18, i1 %25), !dbg !552
  ret i1 %26, !dbg !540
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer14PALLAS_SPEC_10Ei(i32 noundef %0) #2 comdat align 2 !dbg !289 !pallas.exprWrapper !496 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !288, metadata !DIExpression()), !dbg !553
  %2 = icmp sgt i32 %0, 0, !dbg !554
  ret i1 %2, !dbg !553
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer14PALLAS_SPEC_11EiN10pallasSpec3seqIiEE(i32 noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #2 comdat align 2 !dbg !297 !pallas.exprWrapper !496 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !302, metadata !DIExpression()), !dbg !555
  call void @llvm.dbg.declare(metadata ptr %1, metadata !296, metadata !DIExpression()), !dbg !555
  %3 = call noundef ptr @"pallas.result noundef ptr"(), !dbg !556
  %4 = icmp ne ptr %3, null, !dbg !557
  ret i1 %4, !dbg !555
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer14PALLAS_SPEC_12EiN10pallasSpec3seqIiEE(i32 noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #2 comdat align 2 !dbg !308 !pallas.exprWrapper !496 {
  %3 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata i32 %0, metadata !311, metadata !DIExpression()), !dbg !558
  call void @llvm.dbg.declare(metadata ptr %1, metadata !307, metadata !DIExpression()), !dbg !558
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %1, i64 60500880, i1 false), !dbg !559
  %4 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %3), !dbg !560
  %5 = sext i32 %0 to i64, !dbg !561
  %6 = icmp eq i64 %4, %5, !dbg !562
  ret i1 %6, !dbg !558
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer14PALLAS_SPEC_13EiN10pallasSpec3seqIiEE(i32 noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #2 comdat align 2 !dbg !317 !pallas.exprWrapper !496 {
  %3 = alloca %pallas.seq.i32, align 8
  %4 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata i32 %0, metadata !320, metadata !DIExpression()), !dbg !563
  call void @llvm.dbg.declare(metadata ptr %1, metadata !316, metadata !DIExpression()), !dbg !563
  %5 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !564
  %6 = icmp sle i32 0, %5, !dbg !564
  %7 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !564
  %8 = sext i32 %7 to i64, !dbg !564
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %1, i64 60500880, i1 false), !dbg !564
  %9 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %3), !dbg !564
  %10 = icmp ult i64 %8, %9, !dbg !564
  %11 = call i1 @pallas.scAnd(i1 %6, i1 %10), !dbg !564
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %1, i64 60500880, i1 false), !dbg !565
  %12 = call noundef i32 @"pallas.boundVar noundef i32"(ptr @.str), !dbg !566
  %13 = sext i32 %12 to i64, !dbg !566
  %14 = call noundef i32 @"pallas.seq.get i32_noundef i32"(ptr noundef byval(%pallas.seq.i32) %4, i64 noundef %13), !dbg !567
  %15 = icmp eq i32 %14, 0, !dbg !568
  %16 = call i1 @pallas.forall(i1 %11, i1 %15), !dbg !569
  ret i1 %16, !dbg !563
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6Buffer14PALLAS_SPEC_14EiN10pallasSpec3seqIiEE(i32 noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #2 comdat align 2 !dbg !326 !pallas.exprWrapper !496 {
  %3 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata i32 %0, metadata !329, metadata !DIExpression()), !dbg !570
  call void @llvm.dbg.declare(metadata ptr %1, metadata !325, metadata !DIExpression()), !dbg !570
  %4 = call noundef ptr @"pallas.result noundef ptr"(), !dbg !571
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %1, i64 60500880, i1 false), !dbg !572
  %5 = call noundef zeroext i1 @_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE(ptr noundef nonnull align 8 dereferenceable(12) %4, ptr noundef byval(%pallas.seq.i32) align 8 %3), !dbg !573
  ret i1 %5, !dbg !570
}

declare !pallas.specLib !574 noundef i32 @"pallas.result noundef i32"()

declare !pallas.specLib !575 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !576 noundef i32 @"pallas.seq.get i32_noundef i32"(ptr noundef byval(%pallas.seq.i32), i64 noundef)

declare !pallas.specLib !577 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !578 noundef i32 @"pallas.boundVar noundef i32"(ptr)

declare !pallas.specLib !574 noundef ptr @"pallas.result noundef ptr"()

declare !pallas.specLib !579 i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32))

attributes #0 = { mustprogress noinline norecurse nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }

!llvm.dbg.cu = !{!8, !10, !109, !111, !112, !113}
!llvm.module.flags = !{!114, !115, !116, !117, !118, !119, !120}
!llvm.ident = !{!121, !121}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 145, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "f7fed6df1562a57fc92e15e1d81d6cc1")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !6)
!4 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !5)
!5 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!6 = !{!7}
!7 = !DISubrange(count: 2)
!8 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !9, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!9 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_cpp_abs_pred.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "0db04483f6902fc2a4f062d0cb96e8bf")
!10 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !2, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, globals: !11, imports: !12, splitDebugInlining: false, nameTableKind: None)
!11 = !{!0}
!12 = !{!13, !21, !25, !29, !33, !36, !38, !40, !42, !46, !49, !52, !55, !58, !60, !65, !69, !73, !77, !79, !81, !83, !85, !88, !91, !94, !97, !100, !102, !107}
!13 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !15, file: !20, line: 51)
!14 = !DINamespace(name: "std", scope: null)
!15 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !16, line: 24, baseType: !17)
!16 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!17 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int8_t", file: !18, line: 37, baseType: !19)
!18 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!19 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!20 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstdint", directory: "")
!21 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !22, file: !20, line: 52)
!22 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !16, line: 25, baseType: !23)
!23 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int16_t", file: !18, line: 39, baseType: !24)
!24 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!25 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !26, file: !20, line: 53)
!26 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !16, line: 26, baseType: !27)
!27 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !18, line: 41, baseType: !28)
!28 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!29 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !30, file: !20, line: 54)
!30 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !16, line: 27, baseType: !31)
!31 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !18, line: 44, baseType: !32)
!32 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!33 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !34, file: !20, line: 56)
!34 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !35, line: 47, baseType: !19)
!35 = !DIFile(filename: "/usr/include/stdint.h", directory: "", checksumkind: CSK_MD5, checksum: "bfb03fa9c46a839e35c32b929fbdbb8e")
!36 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !37, file: !20, line: 57)
!37 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !35, line: 49, baseType: !32)
!38 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !39, file: !20, line: 58)
!39 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !35, line: 50, baseType: !32)
!40 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !41, file: !20, line: 59)
!41 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !35, line: 51, baseType: !32)
!42 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !43, file: !20, line: 61)
!43 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !44, line: 25, baseType: !45)
!44 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-least.h", directory: "", checksumkind: CSK_MD5, checksum: "a866be81c480920b0293bd5f6336a0a3")
!45 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least8_t", file: !18, line: 52, baseType: !17)
!46 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !47, file: !20, line: 62)
!47 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !44, line: 26, baseType: !48)
!48 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least16_t", file: !18, line: 54, baseType: !23)
!49 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !50, file: !20, line: 63)
!50 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !44, line: 27, baseType: !51)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least32_t", file: !18, line: 56, baseType: !27)
!52 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !53, file: !20, line: 64)
!53 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !44, line: 28, baseType: !54)
!54 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least64_t", file: !18, line: 58, baseType: !31)
!55 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !56, file: !20, line: 66)
!56 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !35, line: 90, baseType: !57)
!57 = !DIDerivedType(tag: DW_TAG_typedef, name: "__intmax_t", file: !18, line: 72, baseType: !32)
!58 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !59, file: !20, line: 67)
!59 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !35, line: 76, baseType: !32)
!60 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !61, file: !20, line: 69)
!61 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !62, line: 24, baseType: !63)
!62 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-uintn.h", directory: "", checksumkind: CSK_MD5, checksum: "256fcabbefa27ca8cf5e6d37525e6e16")
!63 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !18, line: 38, baseType: !64)
!64 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!65 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !66, file: !20, line: 70)
!66 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !62, line: 25, baseType: !67)
!67 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !18, line: 40, baseType: !68)
!68 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!69 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !70, file: !20, line: 71)
!70 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !62, line: 26, baseType: !71)
!71 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint32_t", file: !18, line: 42, baseType: !72)
!72 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!73 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !74, file: !20, line: 72)
!74 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !62, line: 27, baseType: !75)
!75 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint64_t", file: !18, line: 45, baseType: !76)
!76 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!77 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !78, file: !20, line: 74)
!78 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !35, line: 60, baseType: !64)
!79 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !80, file: !20, line: 75)
!80 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !35, line: 62, baseType: !76)
!81 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !82, file: !20, line: 76)
!82 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !35, line: 63, baseType: !76)
!83 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !84, file: !20, line: 77)
!84 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !35, line: 64, baseType: !76)
!85 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !86, file: !20, line: 79)
!86 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !44, line: 31, baseType: !87)
!87 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least8_t", file: !18, line: 53, baseType: !63)
!88 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !89, file: !20, line: 80)
!89 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !44, line: 32, baseType: !90)
!90 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least16_t", file: !18, line: 55, baseType: !67)
!91 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !92, file: !20, line: 81)
!92 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !44, line: 33, baseType: !93)
!93 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least32_t", file: !18, line: 57, baseType: !71)
!94 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !95, file: !20, line: 82)
!95 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !44, line: 34, baseType: !96)
!96 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least64_t", file: !18, line: 59, baseType: !75)
!97 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !98, file: !20, line: 84)
!98 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !35, line: 91, baseType: !99)
!99 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uintmax_t", file: !18, line: 73, baseType: !76)
!100 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !101, file: !20, line: 85)
!101 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !35, line: 79, baseType: !76)
!102 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !14, entity: !103, file: !106, line: 58)
!103 = !DIDerivedType(tag: DW_TAG_typedef, name: "max_align_t", file: !104, line: 24, baseType: !105)
!104 = !DIFile(filename: "/usr/lib/llvm-17/lib/clang/17/include/__stddef_max_align_t.h", directory: "", checksumkind: CSK_MD5, checksum: "48e8e2456f77e6cda35d245130fa7259")
!105 = !DICompositeType(tag: DW_TAG_structure_type, file: !104, line: 19, size: 256, flags: DIFlagFwdDecl, identifier: "_ZTS11max_align_t")
!106 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstddef", directory: "")
!107 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !10, entity: !108, file: !2, line: 6)
!108 = !DINamespace(name: "pallasSpec", scope: null)
!109 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus, file: !110, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!110 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_cpp_abs_pred.cpp", directory: "")
!111 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus, file: !110, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!112 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus, file: !110, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!113 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus, file: !110, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!114 = !{i32 7, !"Dwarf Version", i32 5}
!115 = !{i32 2, !"Debug Info Version", i32 3}
!116 = !{i32 1, !"wchar_size", i32 4}
!117 = !{i32 8, !"PIC Level", i32 2}
!118 = !{i32 7, !"PIE Level", i32 2}
!119 = !{i32 7, !"uwtable", i32 2}
!120 = !{i32 7, !"frame-pointer", i32 2}
!121 = !{!"Ubuntu clang version 17.0.6 (9ubuntu1)"}
!122 = distinct !DISubprogram(name: "main", scope: !9, file: !9, line: 78, type: !123, scopeLine: 78, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !125)
!123 = !DISubroutineType(types: !124)
!124 = !{!28}
!125 = !{}
!126 = !{!127, i1 false, i8 0, !125, !129, !132}
!127 = !{!"pallas.srcLoc", i64 74, i64 1, i64 77, i64 1, !128}
!128 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_cpp_abs_pred.cpp", directory: "", checksumkind: CSK_MD5, checksum: "0db04483f6902fc2a4f062d0cb96e8bf")
!129 = !{!130}
!130 = !{!131, !"data"}
!131 = !{!"pallas.srcLoc", i64 75, i64 1, i64 75, i64 21, !128}
!132 = !{!"pallas.ensures", !133, ptr @_Z14PALLAS_SPEC_15N10pallasSpec3seqIiEE, !125, !134, !125}
!133 = !{!"pallas.srcLoc", i64 76, i64 1, i64 76, i64 27, !128}
!134 = !{!135}
!135 = !{!130, !136}
!136 = !DILocalVariable(name: "data", arg: 1, scope: !137, file: !9, line: 76, type: !141)
!137 = distinct !DISubprogram(name: "PALLAS_SPEC_15", linkageName: "_Z14PALLAS_SPEC_15N10pallasSpec3seqIiEE", scope: !9, file: !9, line: 76, type: !138, scopeLine: 76, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !125)
!138 = !DISubroutineType(types: !139)
!139 = !{!140, !141}
!140 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!141 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "seq<int>", scope: !108, file: !142, line: 22, size: 484007040, flags: DIFlagTypePassByValue, elements: !143, templateParams: !149, identifier: "_ZTSN10pallasSpec3seqIiEE")
!142 = !DIFile(filename: "spectral/res/spec_libs/cpp/cpp_spec_lib.h", directory: "/home/rme/repos", checksumkind: CSK_MD5, checksum: "85705cc0aaeee6328cd7caa7316a6ffb")
!143 = !{!144, !145}
!144 = !DIDerivedType(tag: DW_TAG_member, name: "contentType", scope: !141, file: !142, line: 24, baseType: !28, size: 32)
!145 = !DIDerivedType(tag: DW_TAG_member, name: "dummy", scope: !141, file: !142, line: 25, baseType: !146, size: 484006976, offset: 64)
!146 = !DICompositeType(tag: DW_TAG_array_type, baseType: !30, size: 484006976, elements: !147)
!147 = !{!148}
!148 = !DISubrange(count: 7562609)
!149 = !{!150}
!150 = !DITemplateTypeParameter(name: "T", type: !28)
!151 = !DILocalVariable(name: "b", scope: !122, file: !9, line: 79, type: !152)
!152 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !153, size: 64)
!153 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "Buffer", file: !9, line: 9, size: 128, flags: DIFlagTypePassByValue, elements: !154, identifier: "_ZTS6Buffer")
!154 = !{!155, !157, !158, !162, !165}
!155 = !DIDerivedType(tag: DW_TAG_member, name: "data", scope: !153, file: !9, line: 11, baseType: !156, size: 64)
!156 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !28, size: 64)
!157 = !DIDerivedType(tag: DW_TAG_member, name: "size", scope: !153, file: !9, line: 12, baseType: !28, size: 32, offset: 64)
!158 = !DISubprogram(name: "get", linkageName: "_ZN6Buffer3getEi", scope: !153, file: !9, line: 30, type: !159, scopeLine: 30, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!159 = !DISubroutineType(types: !160)
!160 = !{!28, !161, !28}
!161 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !153, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!162 = !DISubprogram(name: "set", linkageName: "_ZN6Buffer3setEii", scope: !153, file: !9, line: 50, type: !163, scopeLine: 50, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!163 = !DISubroutineType(types: !164)
!164 = !{null, !161, !28, !28}
!165 = !DISubprogram(name: "make_buffer", linkageName: "_ZN6Buffer11make_bufferEi", scope: !153, file: !9, line: 66, type: !166, scopeLine: 66, flags: DIFlagPublic | DIFlagPrototyped | DIFlagStaticMember, spFlags: 0)
!166 = !DISubroutineType(types: !167)
!167 = !{!152, !28}
!168 = !DILocation(line: 79, column: 11, scope: !122)
!169 = !DILocation(line: 79, column: 15, scope: !122)
!170 = !{!171, !172}
!171 = !{!"pallas.srcLoc", i64 79, i64 35, i64 79, i64 59, !128}
!172 = !{!"pallas.yieldsBinding", !173, !130, !174}
!173 = !{!"pallas.srcLoc", i64 79, i64 46, i64 79, i64 57, !128}
!174 = !{!175, !"sNew"}
!175 = !{!"pallas.srcLoc", i64 58, i64 5, i64 58, i64 25, !128}
!176 = !DILocation(line: 80, column: 5, scope: !122)
!177 = !DILocation(line: 80, column: 8, scope: !122)
!178 = !{!179, !180}
!179 = !{!"pallas.srcLoc", i64 80, i64 12, i64 80, i64 32, !128}
!180 = !{!"pallas.givenBinding", !181, ptr @_Z14PALLAS_SPEC_17P6BufferN10pallasSpec3seqIiEE, !125, !182, !231, !234}
!181 = !{!"pallas.srcLoc", i64 80, i64 22, i64 80, i64 30, !128}
!182 = !{!183}
!183 = !{!130, !184}
!184 = !DILocalVariable(name: "data", arg: 2, scope: !185, file: !9, line: 80, type: !141)
!185 = distinct !DISubprogram(name: "PALLAS_SPEC_17", linkageName: "_Z14PALLAS_SPEC_17P6BufferN10pallasSpec3seqIiEE", scope: !9, file: !9, line: 80, type: !186, scopeLine: 80, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !125)
!186 = !DISubroutineType(types: !187)
!187 = !{!141, !188, !141}
!188 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !189, size: 64)
!189 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "Buffer", file: !2, line: 12, size: 128, flags: DIFlagTypePassByValue, elements: !190, identifier: "_ZTS6Buffer")
!190 = !{!191, !192, !193, !197, !200, !201, !202, !203, !206, !209, !210, !213, !214, !215, !216, !219, !222, !225, !226, !227, !228}
!191 = !DIDerivedType(tag: DW_TAG_member, name: "data", scope: !189, file: !2, line: 14, baseType: !156, size: 64)
!192 = !DIDerivedType(tag: DW_TAG_member, name: "size", scope: !189, file: !2, line: 15, baseType: !28, size: 32, offset: 64)
!193 = !DISubprogram(name: "pBuffValid", linkageName: "_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 25, type: !194, scopeLine: 25, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!194 = !DISubroutineType(types: !195)
!195 = !{!140, !196, !141}
!196 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !189, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!197 = !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_ZN6Buffer13PALLAS_SPEC_0EPS_iN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 39, type: !198, scopeLine: 39, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!198 = !DISubroutineType(types: !199)
!199 = !{!140, !196, !188, !28, !141}
!200 = !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_ZN6Buffer13PALLAS_SPEC_3EPS_iN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 49, type: !198, scopeLine: 49, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!201 = !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_ZN6Buffer13PALLAS_SPEC_2EPS_iN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 59, type: !198, scopeLine: 59, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!202 = !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_ZN6Buffer13PALLAS_SPEC_1EPS_iN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 69, type: !198, scopeLine: 69, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!203 = !DISubprogram(name: "get", linkageName: "_ZN6Buffer3getEi", scope: !189, file: !2, line: 84, type: !204, scopeLine: 84, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!204 = !DISubroutineType(types: !205)
!205 = !{!28, !196, !28}
!206 = !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_ZN6Buffer13PALLAS_SPEC_4EPS_iiN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 94, type: !207, scopeLine: 94, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!207 = !DISubroutineType(types: !208)
!208 = !{!140, !196, !188, !28, !28, !141}
!209 = !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_ZN6Buffer13PALLAS_SPEC_5EPS_iiN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 104, type: !207, scopeLine: 104, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!210 = !DISubprogram(name: "PALLAS_SPEC_6", linkageName: "_ZN6Buffer13PALLAS_SPEC_6EPS_iiN10pallasSpec3seqIiEES3_", scope: !189, file: !2, line: 114, type: !211, scopeLine: 114, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!211 = !DISubroutineType(types: !212)
!212 = !{!140, !196, !188, !28, !28, !141, !141}
!213 = !DISubprogram(name: "PALLAS_SPEC_7", linkageName: "_ZN6Buffer13PALLAS_SPEC_7EPS_iiN10pallasSpec3seqIiEES3_", scope: !189, file: !2, line: 124, type: !211, scopeLine: 124, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!214 = !DISubprogram(name: "PALLAS_SPEC_8", linkageName: "_ZN6Buffer13PALLAS_SPEC_8EPS_iiN10pallasSpec3seqIiEES3_", scope: !189, file: !2, line: 134, type: !211, scopeLine: 134, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!215 = !DISubprogram(name: "PALLAS_SPEC_9", linkageName: "_ZN6Buffer13PALLAS_SPEC_9EPS_iiN10pallasSpec3seqIiEES3_", scope: !189, file: !2, line: 144, type: !211, scopeLine: 144, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!216 = !DISubprogram(name: "set", linkageName: "_ZN6Buffer3setEii", scope: !189, file: !2, line: 166, type: !217, scopeLine: 166, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!217 = !DISubroutineType(types: !218)
!218 = !{null, !196, !28, !28}
!219 = !DISubprogram(name: "PALLAS_SPEC_10", linkageName: "_ZN6Buffer14PALLAS_SPEC_10Ei", scope: !189, file: !2, line: 175, type: !220, scopeLine: 175, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!220 = !DISubroutineType(types: !221)
!221 = !{!140, !196, !28}
!222 = !DISubprogram(name: "PALLAS_SPEC_11", linkageName: "_ZN6Buffer14PALLAS_SPEC_11EiN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 185, type: !223, scopeLine: 185, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!223 = !DISubroutineType(types: !224)
!224 = !{!140, !196, !28, !141}
!225 = !DISubprogram(name: "PALLAS_SPEC_12", linkageName: "_ZN6Buffer14PALLAS_SPEC_12EiN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 195, type: !223, scopeLine: 195, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!226 = !DISubprogram(name: "PALLAS_SPEC_13", linkageName: "_ZN6Buffer14PALLAS_SPEC_13EiN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 205, type: !223, scopeLine: 205, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!227 = !DISubprogram(name: "PALLAS_SPEC_14", linkageName: "_ZN6Buffer14PALLAS_SPEC_14EiN10pallasSpec3seqIiEE", scope: !189, file: !2, line: 216, type: !223, scopeLine: 216, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!228 = !DISubprogram(name: "make_buffer", linkageName: "_ZN6Buffer11make_bufferEi", scope: !189, file: !2, line: 233, type: !229, scopeLine: 233, flags: DIFlagPublic | DIFlagPrototyped | DIFlagStaticMember, spFlags: 0)
!229 = !DISubroutineType(types: !230)
!230 = !{!188, !28}
!231 = !{!232}
!232 = !{!151, !233}
!233 = !DILocalVariable(name: "b", arg: 1, scope: !185, file: !9, line: 80, type: !188)
!234 = !{!235, !"s"}
!235 = !{!"pallas.srcLoc", i64 39, i64 5, i64 39, i64 22, !128}
!236 = !{!237, !238}
!237 = !{!"pallas.srcLoc", i64 80, i64 36, i64 80, i64 60, !128}
!238 = !{!"pallas.yieldsBinding", !239, !130, !240}
!239 = !{!"pallas.srcLoc", i64 80, i64 47, i64 80, i64 58, !128}
!240 = !{!241, !"sNew"}
!241 = !{!"pallas.srcLoc", i64 40, i64 5, i64 40, i64 25, !128}
!242 = !DILocalVariable(name: "res", scope: !122, file: !9, line: 81, type: !28)
!243 = !DILocation(line: 81, column: 9, scope: !122)
!244 = !DILocation(line: 81, column: 15, scope: !122)
!245 = !DILocation(line: 81, column: 18, scope: !122)
!246 = !{!247, !248}
!247 = !{!"pallas.srcLoc", i64 81, i64 22, i64 81, i64 42, !128}
!248 = !{!"pallas.givenBinding", !249, ptr @_Z14PALLAS_SPEC_18P6BufferiN10pallasSpec3seqIiEE, !125, !250, !256, !261}
!249 = !{!"pallas.srcLoc", i64 81, i64 32, i64 81, i64 40, !128}
!250 = !{!251}
!251 = !{!130, !252}
!252 = !DILocalVariable(name: "data", arg: 3, scope: !253, file: !9, line: 81, type: !141)
!253 = distinct !DISubprogram(name: "PALLAS_SPEC_18", linkageName: "_Z14PALLAS_SPEC_18P6BufferiN10pallasSpec3seqIiEE", scope: !9, file: !9, line: 81, type: !254, scopeLine: 81, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !125)
!254 = !DISubroutineType(types: !255)
!255 = !{!141, !188, !28, !141}
!256 = !{!257, !259}
!257 = !{!151, !258}
!258 = !DILocalVariable(name: "b", arg: 1, scope: !253, file: !9, line: 81, type: !188)
!259 = !{!242, !260}
!260 = !DILocalVariable(name: "res", arg: 2, scope: !253, file: !9, line: 81, type: !28)
!261 = !{!262, !"s"}
!262 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 21, !128}
!263 = !DILocation(line: 85, column: 5, scope: !122)
!264 = !{!265, !266}
!265 = !{!"pallas.srcLoc", i64 82, i64 5, i64 84, i64 5, !128}
!266 = !{!"pallas.assert", !267, ptr @_Z14PALLAS_SPEC_16P6BufferiN10pallasSpec3seqIiEE, !125, !268, !274}
!267 = !{!"pallas.srcLoc", i64 83, i64 5, i64 83, i64 21, !128}
!268 = !{!269}
!269 = !{!130, !270}
!270 = !DILocalVariable(name: "data", arg: 3, scope: !271, file: !9, line: 83, type: !141)
!271 = distinct !DISubprogram(name: "PALLAS_SPEC_16", linkageName: "_Z14PALLAS_SPEC_16P6BufferiN10pallasSpec3seqIiEE", scope: !9, file: !9, line: 83, type: !272, scopeLine: 83, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, retainedNodes: !125)
!272 = !DISubroutineType(types: !273)
!273 = !{!140, !188, !28, !141}
!274 = !{!275, !277}
!275 = !{!151, !276}
!276 = !DILocalVariable(name: "b", arg: 1, scope: !271, file: !9, line: 83, type: !188)
!277 = !{!242, !278}
!278 = !DILocalVariable(name: "res", arg: 2, scope: !271, file: !9, line: 83, type: !28)
!279 = distinct !DISubprogram(name: "make_buffer", linkageName: "_ZN6Buffer11make_bufferEi", scope: !153, file: !9, line: 66, type: !166, scopeLine: 66, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, declaration: !165, retainedNodes: !125)
!280 = !{!281, i1 false, i8 1, !125, !282, !283, !292, !303, !312, !321}
!281 = !{!"pallas.srcLoc", i64 54, i64 5, i64 65, i64 5, !128}
!282 = !{!174}
!283 = !{!"pallas.requires", !284, ptr @_ZN6Buffer14PALLAS_SPEC_10Ei, !125, !125, !285}
!284 = !{!"pallas.srcLoc", i64 59, i64 5, i64 59, i64 19, !128}
!285 = !{!286}
!286 = !{!287, !288}
!287 = !DILocalVariable(name: "n", arg: 1, scope: !279, file: !9, line: 66, type: !28)
!288 = !DILocalVariable(name: "n", arg: 2, scope: !289, file: !110, line: 59, type: !28)
!289 = distinct !DISubprogram(name: "PALLAS_SPEC_10", linkageName: "_ZN6Buffer14PALLAS_SPEC_10Ei", scope: !110, file: !110, line: 59, type: !290, scopeLine: 59, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !113, declaration: !219, retainedNodes: !125)
!290 = !DISubroutineType(types: !291)
!291 = !{!140, !28}
!292 = !{!"pallas.ensures", !293, ptr @_ZN6Buffer14PALLAS_SPEC_11EiN10pallasSpec3seqIiEE, !125, !294, !300}
!293 = !{!"pallas.srcLoc", i64 60, i64 5, i64 60, i64 42, !128}
!294 = !{!295}
!295 = !{!174, !296}
!296 = !DILocalVariable(name: "sNew", arg: 3, scope: !297, file: !110, line: 60, type: !141)
!297 = distinct !DISubprogram(name: "PALLAS_SPEC_11", linkageName: "_ZN6Buffer14PALLAS_SPEC_11EiN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 60, type: !298, scopeLine: 60, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !113, declaration: !222, retainedNodes: !125)
!298 = !DISubroutineType(types: !299)
!299 = !{!140, !28, !141}
!300 = !{!301}
!301 = !{!287, !302}
!302 = !DILocalVariable(name: "n", arg: 2, scope: !297, file: !110, line: 60, type: !28)
!303 = !{!"pallas.ensures", !304, ptr @_ZN6Buffer14PALLAS_SPEC_12EiN10pallasSpec3seqIiEE, !125, !305, !309}
!304 = !{!"pallas.srcLoc", i64 61, i64 5, i64 61, i64 32, !128}
!305 = !{!306}
!306 = !{!174, !307}
!307 = !DILocalVariable(name: "sNew", arg: 3, scope: !308, file: !110, line: 61, type: !141)
!308 = distinct !DISubprogram(name: "PALLAS_SPEC_12", linkageName: "_ZN6Buffer14PALLAS_SPEC_12EiN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 61, type: !298, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !113, declaration: !225, retainedNodes: !125)
!309 = !{!310}
!310 = !{!287, !311}
!311 = !DILocalVariable(name: "n", arg: 2, scope: !308, file: !110, line: 61, type: !28)
!312 = !{!"pallas.ensures", !313, ptr @_ZN6Buffer14PALLAS_SPEC_13EiN10pallasSpec3seqIiEE, !125, !314, !318}
!313 = !{!"pallas.srcLoc", i64 62, i64 5, i64 63, i64 55, !128}
!314 = !{!315}
!315 = !{!174, !316}
!316 = !DILocalVariable(name: "sNew", arg: 3, scope: !317, file: !110, line: 62, type: !141)
!317 = distinct !DISubprogram(name: "PALLAS_SPEC_13", linkageName: "_ZN6Buffer14PALLAS_SPEC_13EiN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 62, type: !298, scopeLine: 62, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !113, declaration: !226, retainedNodes: !125)
!318 = !{!319}
!319 = !{!287, !320}
!320 = !DILocalVariable(name: "n", arg: 2, scope: !317, file: !110, line: 62, type: !28)
!321 = !{!"pallas.ensures", !322, ptr @_ZN6Buffer14PALLAS_SPEC_14EiN10pallasSpec3seqIiEE, !125, !323, !327}
!322 = !{!"pallas.srcLoc", i64 64, i64 5, i64 64, i64 49, !128}
!323 = !{!324}
!324 = !{!174, !325}
!325 = !DILocalVariable(name: "sNew", arg: 3, scope: !326, file: !110, line: 64, type: !141)
!326 = distinct !DISubprogram(name: "PALLAS_SPEC_14", linkageName: "_ZN6Buffer14PALLAS_SPEC_14EiN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 64, type: !298, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !113, declaration: !227, retainedNodes: !125)
!327 = !{!328}
!328 = !{!287, !329}
!329 = !DILocalVariable(name: "n", arg: 2, scope: !326, file: !110, line: 64, type: !28)
!330 = !DILocation(line: 66, column: 36, scope: !279)
!331 = !DILocation(line: 68, column: 9, scope: !279)
!332 = distinct !DISubprogram(name: "set", linkageName: "_ZN6Buffer3setEii", scope: !153, file: !9, line: 50, type: !163, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, declaration: !162, retainedNodes: !125)
!333 = !{!334, i1 false, i8 1, !335, !336, !337, !355, !368, !386, !402, !418}
!334 = !{!"pallas.srcLoc", i64 35, i64 5, i64 49, i64 5, !128}
!335 = !{!234}
!336 = !{!240}
!337 = !{!"pallas.requires", !338, ptr @_ZN6Buffer13PALLAS_SPEC_4EPS_iiN10pallasSpec3seqIiEE, !339, !125, !345}
!338 = !{!"pallas.srcLoc", i64 41, i64 5, i64 41, i64 34, !128}
!339 = !{!340}
!340 = !{!234, !341}
!341 = !DILocalVariable(name: "s", arg: 5, scope: !342, file: !110, line: 41, type: !141)
!342 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_ZN6Buffer13PALLAS_SPEC_4EPS_iiN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 41, type: !343, scopeLine: 41, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !112, declaration: !206, retainedNodes: !125)
!343 = !DISubroutineType(types: !344)
!344 = !{!140, !188, !28, !28, !141}
!345 = !{!346, !349, !352}
!346 = !{!347, !348}
!347 = !DILocalVariable(name: "this", arg: 1, scope: !332, type: !152, flags: DIFlagArtificial | DIFlagObjectPointer)
!348 = !DILocalVariable(name: "_this", arg: 2, scope: !342, file: !110, line: 41, type: !188)
!349 = !{!350, !351}
!350 = !DILocalVariable(name: "idx", arg: 2, scope: !332, file: !9, line: 50, type: !28)
!351 = !DILocalVariable(name: "idx", arg: 3, scope: !342, file: !110, line: 41, type: !28)
!352 = !{!353, !354}
!353 = !DILocalVariable(name: "val", arg: 3, scope: !332, file: !9, line: 50, type: !28)
!354 = !DILocalVariable(name: "val", arg: 4, scope: !342, file: !110, line: 41, type: !28)
!355 = !{!"pallas.requires", !356, ptr @_ZN6Buffer13PALLAS_SPEC_5EPS_iiN10pallasSpec3seqIiEE, !357, !125, !361}
!356 = !{!"pallas.srcLoc", i64 42, i64 5, i64 42, i64 43, !128}
!357 = !{!358}
!358 = !{!234, !359}
!359 = !DILocalVariable(name: "s", arg: 5, scope: !360, file: !110, line: 42, type: !141)
!360 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_ZN6Buffer13PALLAS_SPEC_5EPS_iiN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 42, type: !343, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !112, declaration: !209, retainedNodes: !125)
!361 = !{!362, !364, !366}
!362 = !{!347, !363}
!363 = !DILocalVariable(name: "_this", arg: 2, scope: !360, file: !110, line: 42, type: !188)
!364 = !{!350, !365}
!365 = !DILocalVariable(name: "idx", arg: 3, scope: !360, file: !110, line: 42, type: !28)
!366 = !{!353, !367}
!367 = !DILocalVariable(name: "val", arg: 4, scope: !360, file: !110, line: 42, type: !28)
!368 = !{!"pallas.ensures", !369, ptr @_ZN6Buffer13PALLAS_SPEC_6EPS_iiN10pallasSpec3seqIiEES3_, !370, !376, !379}
!369 = !{!"pallas.srcLoc", i64 43, i64 5, i64 43, i64 36, !128}
!370 = !{!371}
!371 = !{!234, !372}
!372 = !DILocalVariable(name: "s", arg: 5, scope: !373, file: !110, line: 43, type: !141)
!373 = distinct !DISubprogram(name: "PALLAS_SPEC_6", linkageName: "_ZN6Buffer13PALLAS_SPEC_6EPS_iiN10pallasSpec3seqIiEES3_", scope: !110, file: !110, line: 43, type: !374, scopeLine: 43, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !112, declaration: !210, retainedNodes: !125)
!374 = !DISubroutineType(types: !375)
!375 = !{!140, !188, !28, !28, !141, !141}
!376 = !{!377}
!377 = !{!240, !378}
!378 = !DILocalVariable(name: "sNew", arg: 6, scope: !373, file: !110, line: 43, type: !141)
!379 = !{!380, !382, !384}
!380 = !{!347, !381}
!381 = !DILocalVariable(name: "_this", arg: 2, scope: !373, file: !110, line: 43, type: !188)
!382 = !{!350, !383}
!383 = !DILocalVariable(name: "idx", arg: 3, scope: !373, file: !110, line: 43, type: !28)
!384 = !{!353, !385}
!385 = !DILocalVariable(name: "val", arg: 4, scope: !373, file: !110, line: 43, type: !28)
!386 = !{!"pallas.ensures", !387, ptr @_ZN6Buffer13PALLAS_SPEC_7EPS_iiN10pallasSpec3seqIiEES3_, !388, !392, !395}
!387 = !{!"pallas.srcLoc", i64 44, i64 5, i64 44, i64 42, !128}
!388 = !{!389}
!389 = !{!234, !390}
!390 = !DILocalVariable(name: "s", arg: 5, scope: !391, file: !110, line: 44, type: !141)
!391 = distinct !DISubprogram(name: "PALLAS_SPEC_7", linkageName: "_ZN6Buffer13PALLAS_SPEC_7EPS_iiN10pallasSpec3seqIiEES3_", scope: !110, file: !110, line: 44, type: !374, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !112, declaration: !213, retainedNodes: !125)
!392 = !{!393}
!393 = !{!240, !394}
!394 = !DILocalVariable(name: "sNew", arg: 6, scope: !391, file: !110, line: 44, type: !141)
!395 = !{!396, !398, !400}
!396 = !{!347, !397}
!397 = !DILocalVariable(name: "_this", arg: 2, scope: !391, file: !110, line: 44, type: !188)
!398 = !{!350, !399}
!399 = !DILocalVariable(name: "idx", arg: 3, scope: !391, file: !110, line: 44, type: !28)
!400 = !{!353, !401}
!401 = !DILocalVariable(name: "val", arg: 4, scope: !391, file: !110, line: 44, type: !28)
!402 = !{!"pallas.ensures", !403, ptr @_ZN6Buffer13PALLAS_SPEC_8EPS_iiN10pallasSpec3seqIiEES3_, !404, !408, !411}
!403 = !{!"pallas.srcLoc", i64 45, i64 5, i64 45, i64 38, !128}
!404 = !{!405}
!405 = !{!234, !406}
!406 = !DILocalVariable(name: "s", arg: 5, scope: !407, file: !110, line: 45, type: !141)
!407 = distinct !DISubprogram(name: "PALLAS_SPEC_8", linkageName: "_ZN6Buffer13PALLAS_SPEC_8EPS_iiN10pallasSpec3seqIiEES3_", scope: !110, file: !110, line: 45, type: !374, scopeLine: 45, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !112, declaration: !214, retainedNodes: !125)
!408 = !{!409}
!409 = !{!240, !410}
!410 = !DILocalVariable(name: "sNew", arg: 6, scope: !407, file: !110, line: 45, type: !141)
!411 = !{!412, !414, !416}
!412 = !{!347, !413}
!413 = !DILocalVariable(name: "_this", arg: 2, scope: !407, file: !110, line: 45, type: !188)
!414 = !{!350, !415}
!415 = !DILocalVariable(name: "idx", arg: 3, scope: !407, file: !110, line: 45, type: !28)
!416 = !{!353, !417}
!417 = !DILocalVariable(name: "val", arg: 4, scope: !407, file: !110, line: 45, type: !28)
!418 = !{!"pallas.ensures", !419, ptr @_ZN6Buffer13PALLAS_SPEC_9EPS_iiN10pallasSpec3seqIiEES3_, !420, !424, !427}
!419 = !{!"pallas.srcLoc", i64 46, i64 5, i64 48, i64 79, !128}
!420 = !{!421}
!421 = !{!234, !422}
!422 = !DILocalVariable(name: "s", arg: 5, scope: !423, file: !110, line: 46, type: !141)
!423 = distinct !DISubprogram(name: "PALLAS_SPEC_9", linkageName: "_ZN6Buffer13PALLAS_SPEC_9EPS_iiN10pallasSpec3seqIiEES3_", scope: !110, file: !110, line: 46, type: !374, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !112, declaration: !215, retainedNodes: !125)
!424 = !{!425}
!425 = !{!240, !426}
!426 = !DILocalVariable(name: "sNew", arg: 6, scope: !423, file: !110, line: 46, type: !141)
!427 = !{!428, !430, !432}
!428 = !{!347, !429}
!429 = !DILocalVariable(name: "_this", arg: 2, scope: !423, file: !110, line: 46, type: !188)
!430 = !{!350, !431}
!431 = !DILocalVariable(name: "idx", arg: 3, scope: !423, file: !110, line: 46, type: !28)
!432 = !{!353, !433}
!433 = !DILocalVariable(name: "val", arg: 4, scope: !423, file: !110, line: 46, type: !28)
!434 = !DILocation(line: 0, scope: !332)
!435 = !DILocation(line: 50, column: 18, scope: !332)
!436 = !DILocation(line: 50, column: 27, scope: !332)
!437 = !DILocation(line: 52, column: 5, scope: !332)
!438 = distinct !DISubprogram(name: "get", linkageName: "_ZN6Buffer3getEi", scope: !153, file: !9, line: 30, type: !159, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8, declaration: !158, retainedNodes: !125)
!439 = !{!440, i1 false, i8 1, !441, !125, !442, !455, !466, !477}
!440 = !{!"pallas.srcLoc", i64 20, i64 5, i64 29, i64 5, !128}
!441 = !{!261}
!442 = !{!"pallas.requires", !443, ptr @_ZN6Buffer13PALLAS_SPEC_0EPS_iN10pallasSpec3seqIiEE, !444, !125, !448}
!443 = !{!"pallas.srcLoc", i64 25, i64 5, i64 25, i64 34, !128}
!444 = !{!445}
!445 = !{!261, !446}
!446 = !DILocalVariable(name: "s", arg: 4, scope: !447, file: !110, line: 25, type: !141)
!447 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_ZN6Buffer13PALLAS_SPEC_0EPS_iN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 25, type: !272, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !111, declaration: !197, retainedNodes: !125)
!448 = !{!449, !452}
!449 = !{!450, !451}
!450 = !DILocalVariable(name: "this", arg: 1, scope: !438, type: !152, flags: DIFlagArtificial | DIFlagObjectPointer)
!451 = !DILocalVariable(name: "_this", arg: 2, scope: !447, file: !110, line: 25, type: !188)
!452 = !{!453, !454}
!453 = !DILocalVariable(name: "idx", arg: 2, scope: !438, file: !9, line: 30, type: !28)
!454 = !DILocalVariable(name: "idx", arg: 3, scope: !447, file: !110, line: 25, type: !28)
!455 = !{!"pallas.requires", !456, ptr @_ZN6Buffer13PALLAS_SPEC_1EPS_iN10pallasSpec3seqIiEE, !457, !125, !461}
!456 = !{!"pallas.srcLoc", i64 26, i64 5, i64 26, i64 43, !128}
!457 = !{!458}
!458 = !{!261, !459}
!459 = !DILocalVariable(name: "s", arg: 4, scope: !460, file: !110, line: 26, type: !141)
!460 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_ZN6Buffer13PALLAS_SPEC_1EPS_iN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 26, type: !272, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !111, declaration: !202, retainedNodes: !125)
!461 = !{!462, !464}
!462 = !{!450, !463}
!463 = !DILocalVariable(name: "_this", arg: 2, scope: !460, file: !110, line: 26, type: !188)
!464 = !{!453, !465}
!465 = !DILocalVariable(name: "idx", arg: 3, scope: !460, file: !110, line: 26, type: !28)
!466 = !{!"pallas.ensures", !467, ptr @_ZN6Buffer13PALLAS_SPEC_2EPS_iN10pallasSpec3seqIiEE, !468, !125, !472}
!467 = !{!"pallas.srcLoc", i64 27, i64 5, i64 27, i64 33, !128}
!468 = !{!469}
!469 = !{!261, !470}
!470 = !DILocalVariable(name: "s", arg: 4, scope: !471, file: !110, line: 27, type: !141)
!471 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_ZN6Buffer13PALLAS_SPEC_2EPS_iN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 27, type: !272, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !111, declaration: !201, retainedNodes: !125)
!472 = !{!473, !475}
!473 = !{!450, !474}
!474 = !DILocalVariable(name: "_this", arg: 2, scope: !471, file: !110, line: 27, type: !188)
!475 = !{!453, !476}
!476 = !DILocalVariable(name: "idx", arg: 3, scope: !471, file: !110, line: 27, type: !28)
!477 = !{!"pallas.ensures", !478, ptr @_ZN6Buffer13PALLAS_SPEC_3EPS_iN10pallasSpec3seqIiEE, !479, !125, !483}
!478 = !{!"pallas.srcLoc", i64 28, i64 5, i64 28, i64 46, !128}
!479 = !{!480}
!480 = !{!261, !481}
!481 = !DILocalVariable(name: "s", arg: 4, scope: !482, file: !110, line: 28, type: !141)
!482 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_ZN6Buffer13PALLAS_SPEC_3EPS_iN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 28, type: !272, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !111, declaration: !200, retainedNodes: !125)
!483 = !{!484, !486}
!484 = !{!450, !485}
!485 = !DILocalVariable(name: "_this", arg: 2, scope: !482, file: !110, line: 28, type: !188)
!486 = !{!453, !487}
!487 = !DILocalVariable(name: "idx", arg: 3, scope: !482, file: !110, line: 28, type: !28)
!488 = !DILocation(line: 0, scope: !438)
!489 = !DILocation(line: 30, column: 17, scope: !438)
!490 = !DILocation(line: 32, column: 9, scope: !438)
!491 = distinct !DISubprogram(name: "pBuffValid", linkageName: "_ZN6Buffer10pBuffValidEN10pallasSpec3seqIiEE", scope: !110, file: !110, line: 17, type: !194, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !109, declaration: !193, retainedNodes: !125)
!492 = !{i1 false, i1 true}
!493 = !DILocalVariable(name: "this", arg: 1, scope: !491, file: !110, line: 17, type: !188, flags: DIFlagArtificial | DIFlagObjectPointer)
!494 = !DILocation(line: 0, scope: !491)
!495 = !DILocalVariable(name: "s", arg: 2, scope: !491, file: !110, line: 17, type: !141)
!496 = !{!""}
!497 = !DILocation(line: 0, scope: !137)
!498 = !DILocation(line: 76, column: 18, scope: !137)
!499 = !DILocation(line: 76, column: 9, scope: !137)
!500 = !DILocation(line: 76, column: 24, scope: !137)
!501 = !DILocation(line: 0, scope: !271)
!502 = !DILocation(line: 83, column: 16, scope: !271)
!503 = !DILocation(line: 0, scope: !185)
!504 = !DILocation(line: 80, column: 26, scope: !185)
!505 = !DILocation(line: 0, scope: !253)
!506 = !DILocation(line: 81, column: 36, scope: !253)
!507 = !DILocation(line: 0, scope: !447)
!508 = !DILocation(line: 25, column: 32, scope: !447)
!509 = !DILocation(line: 25, column: 21, scope: !447)
!510 = !DILocation(line: 0, scope: !482)
!511 = !DILocation(line: 28, column: 13, scope: !482)
!512 = !DILocation(line: 28, column: 39, scope: !482)
!513 = !DILocation(line: 28, column: 42, scope: !482)
!514 = !DILocation(line: 28, column: 31, scope: !482)
!515 = !DILocation(line: 28, column: 28, scope: !482)
!516 = !DILocation(line: 0, scope: !471)
!517 = !DILocation(line: 27, column: 31, scope: !471)
!518 = !DILocation(line: 27, column: 20, scope: !471)
!519 = !DILocation(line: 0, scope: !342)
!520 = !DILocation(line: 41, column: 32, scope: !342)
!521 = !DILocation(line: 41, column: 21, scope: !342)
!522 = !DILocation(line: 0, scope: !460)
!523 = !DILocation(line: 26, column: 14, scope: !460)
!524 = !DILocation(line: 0, scope: !360)
!525 = !DILocation(line: 42, column: 14, scope: !360)
!526 = !DILocation(line: 0, scope: !373)
!527 = !DILocation(line: 43, column: 31, scope: !373)
!528 = !DILocation(line: 43, column: 20, scope: !373)
!529 = !DILocation(line: 0, scope: !391)
!530 = !DILocation(line: 44, column: 22, scope: !391)
!531 = !DILocation(line: 44, column: 13, scope: !391)
!532 = !DILocation(line: 44, column: 37, scope: !391)
!533 = !DILocation(line: 44, column: 28, scope: !391)
!534 = !DILocation(line: 44, column: 25, scope: !391)
!535 = !DILocation(line: 0, scope: !407)
!536 = !DILocation(line: 45, column: 21, scope: !407)
!537 = !DILocation(line: 45, column: 27, scope: !407)
!538 = !DILocation(line: 45, column: 13, scope: !407)
!539 = !DILocation(line: 45, column: 32, scope: !407)
!540 = !DILocation(line: 0, scope: !423)
!541 = !DILocation(line: 46, column: 26, scope: !423)
!542 = !DILocation(line: 47, column: 26, scope: !423)
!543 = !DILocation(line: 47, column: 40, scope: !423)
!544 = !DILocation(line: 46, column: 21, scope: !423)
!545 = !DILocation(line: 48, column: 29, scope: !423)
!546 = !DILocation(line: 48, column: 35, scope: !423)
!547 = !DILocation(line: 48, column: 21, scope: !423)
!548 = !DILocation(line: 48, column: 61, scope: !423)
!549 = !DILocation(line: 48, column: 64, scope: !423)
!550 = !DILocation(line: 48, column: 53, scope: !423)
!551 = !DILocation(line: 48, column: 50, scope: !423)
!552 = !DILocation(line: 46, column: 13, scope: !423)
!553 = !DILocation(line: 0, scope: !289)
!554 = !DILocation(line: 59, column: 16, scope: !289)
!555 = !DILocation(line: 0, scope: !297)
!556 = !DILocation(line: 60, column: 13, scope: !297)
!557 = !DILocation(line: 60, column: 32, scope: !297)
!558 = !DILocation(line: 0, scope: !308)
!559 = !DILocation(line: 61, column: 22, scope: !308)
!560 = !DILocation(line: 61, column: 13, scope: !308)
!561 = !DILocation(line: 61, column: 31, scope: !308)
!562 = !DILocation(line: 61, column: 28, scope: !308)
!563 = !DILocation(line: 0, scope: !317)
!564 = !DILocation(line: 62, column: 21, scope: !317)
!565 = !DILocation(line: 63, column: 29, scope: !317)
!566 = !DILocation(line: 63, column: 35, scope: !317)
!567 = !DILocation(line: 63, column: 21, scope: !317)
!568 = !DILocation(line: 63, column: 50, scope: !317)
!569 = !DILocation(line: 62, column: 13, scope: !317)
!570 = !DILocation(line: 0, scope: !326)
!571 = !DILocation(line: 64, column: 13, scope: !326)
!572 = !DILocation(line: 64, column: 44, scope: !326)
!573 = !DILocation(line: 64, column: 33, scope: !326)
!574 = !{!"pallas.result"}
!575 = !{!"pallas.forall"}
!576 = !{!"pallas.seq.get"}
!577 = !{!"pallas.scAnd"}
!578 = !{!"pallas.boundVar"}
!579 = !{!"pallas.seq.size"}
