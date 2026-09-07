; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_seq.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.IntListT = type { i32, ptr }
%pallas.seq.i32 = type { i32, i64, i64, i64 }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [25 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_15, ptr @PALLAS_SPEC_16, ptr @PALLAS_SPEC_17, ptr @PALLAS_SPEC_18, ptr @PALLAS_SPEC_19, ptr @PALLAS_SPEC_20, ptr @PALLAS_SPEC_21, ptr @PALLAS_SPEC_22, ptr @PALLAS_SPEC_23, ptr @listWrite], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @list_size(ptr noundef %0) #0 !dbg !17 !pallas.fcontract !29 {
  %2 = alloca i32, align 4
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !65, metadata !DIExpression()), !dbg !85
  %5 = load ptr, ptr %3, align 8, !dbg !86
  %6 = icmp eq ptr %5, null, !dbg !88
  br i1 %6, label %7, label %8, !dbg !89

7:                                                ; preds = %1
  store i32 0, ptr %2, align 4, !dbg !90, !pallas.stmntBlock !92
  br label %15, !dbg !90

8:                                                ; preds = %1
  call void @llvm.dbg.declare(metadata ptr %4, metadata !103, metadata !DIExpression()), !dbg !105
  %9 = load ptr, ptr %3, align 8, !dbg !106, !pallas.stmntBlock !107
  %10 = getelementptr inbounds %struct.IntListT, ptr %9, i32 0, i32 1, !dbg !122
  %11 = load ptr, ptr %10, align 8, !dbg !122
  %12 = call i32 @list_size(ptr noundef %11), !dbg !123, !pallas.givenBindings !124
  %13 = add nsw i32 1, %12, !dbg !139
  store i32 %13, ptr %4, align 4, !dbg !105
  %14 = load i32, ptr %4, align 4, !dbg !140, !pallas.stmntBlock !141
  store i32 %14, ptr %2, align 4, !dbg !154
  br label %15, !dbg !154

15:                                               ; preds = %8, %7
  %16 = load i32, ptr %2, align 4, !dbg !155
  ret i32 %16, !dbg !155
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @get_head(ptr noundef %0) #0 !dbg !156 !pallas.fcontract !157 {
  %2 = alloca ptr, align 8
  %3 = alloca i32, align 4
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !170, metadata !DIExpression()), !dbg !199
  call void @llvm.dbg.declare(metadata ptr %3, metadata !200, metadata !DIExpression()), !dbg !201
  %4 = load ptr, ptr %2, align 8, !dbg !202, !pallas.stmntBlock !203
  %5 = getelementptr inbounds %struct.IntListT, ptr %4, i32 0, i32 0, !dbg !216
  %6 = load i32, ptr %5, align 8, !dbg !216
  store i32 %6, ptr %3, align 4, !dbg !201
  %7 = load i32, ptr %3, align 4, !dbg !217, !pallas.stmntBlock !218
  ret i32 %7, !dbg !231
}

; Function Attrs: noinline nounwind uwtable
define dso_local ptr @prepend(ptr noundef %0, i32 noundef %1) #0 !dbg !232 !pallas.fcontract !235 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !251, metadata !DIExpression()), !dbg !314
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !254, metadata !DIExpression()), !dbg !315
  call void @llvm.dbg.declare(metadata ptr %5, metadata !316, metadata !DIExpression()), !dbg !317
  %6 = call ptr (...) @allocIntList(), !dbg !318
  store ptr %6, ptr %5, align 8, !dbg !317
  %7 = load i32, ptr %4, align 4, !dbg !319
  %8 = load ptr, ptr %5, align 8, !dbg !320
  %9 = getelementptr inbounds %struct.IntListT, ptr %8, i32 0, i32 0, !dbg !321
  store i32 %7, ptr %9, align 8, !dbg !322
  %10 = load ptr, ptr %3, align 8, !dbg !323
  %11 = load ptr, ptr %5, align 8, !dbg !324
  %12 = getelementptr inbounds %struct.IntListT, ptr %11, i32 0, i32 1, !dbg !325
  store ptr %10, ptr %12, align 8, !dbg !326
  %13 = load ptr, ptr %5, align 8, !dbg !327, !pallas.stmntBlock !328
  ret ptr %13, !dbg !366
}

declare !pallas.extContract !367 ptr @allocIntList(...) #2

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !40 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !66, metadata !DIExpression()), !dbg !378
  call void @llvm.dbg.declare(metadata ptr %1, metadata !39, metadata !DIExpression()), !dbg !378
  %3 = call zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1), !dbg !379
  ret i1 %3, !dbg !378
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !72 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !75, metadata !DIExpression()), !dbg !380
  call void @llvm.dbg.declare(metadata ptr %1, metadata !71, metadata !DIExpression()), !dbg !380
  %3 = call zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1), !dbg !381
  ret i1 %3, !dbg !380
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !81 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !84, metadata !DIExpression()), !dbg !382
  call void @llvm.dbg.declare(metadata ptr %1, metadata !80, metadata !DIExpression()), !dbg !382
  %3 = call i32 @"pallas.result i32"(), !dbg !383
  %4 = sext i32 %3 to i64, !dbg !383
  %5 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %1), !dbg !384
  %6 = icmp eq i64 %4, %5, !dbg !385
  ret i1 %6, !dbg !382
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !167 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !171, metadata !DIExpression()), !dbg !386
  call void @llvm.dbg.declare(metadata ptr %1, metadata !166, metadata !DIExpression()), !dbg !386
  %3 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %1), !dbg !387
  %4 = icmp ugt i64 %3, 0, !dbg !388
  ret i1 %4, !dbg !386
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !177 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !180, metadata !DIExpression()), !dbg !389
  call void @llvm.dbg.declare(metadata ptr %1, metadata !176, metadata !DIExpression()), !dbg !389
  %3 = call zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1), !dbg !390
  ret i1 %3, !dbg !389
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !186 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !189, metadata !DIExpression()), !dbg !391
  call void @llvm.dbg.declare(metadata ptr %1, metadata !185, metadata !DIExpression()), !dbg !391
  %3 = call zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1), !dbg !392
  ret i1 %3, !dbg !391
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !195 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !198, metadata !DIExpression()), !dbg !393
  call void @llvm.dbg.declare(metadata ptr %1, metadata !194, metadata !DIExpression()), !dbg !393
  %3 = call i32 @"pallas.result i32"(), !dbg !394
  %4 = call i32 @"pallas.seq.get i32_i32"(ptr noundef byval(%pallas.seq.i32) %1, i64 noundef 0), !dbg !395
  %5 = icmp eq i32 %3, %4, !dbg !396
  ret i1 %5, !dbg !393
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7() #0 !dbg !397 !pallas.exprWrapper !377 {
  %1 = call ptr @"pallas.result ptr"(), !dbg !400
  %2 = icmp ne ptr %1, null, !dbg !401
  ret i1 %2, !dbg !402
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8() #0 !dbg !403 !pallas.exprWrapper !377 {
  %1 = alloca %pallas.fracT, align 8
  %2 = call ptr @"pallas.result ptr"(), !dbg !404
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %1, i32 noundef 1, i32 noundef 1), !dbg !405
  %3 = call i1 @pallas.perm(ptr noundef %2, ptr noundef byval(%pallas.fracT) %1), !dbg !406
  ret i1 %3, !dbg !407
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9() #0 !dbg !408 !pallas.exprWrapper !377 {
  %1 = call ptr @"pallas.result ptr"(), !dbg !409
  %2 = getelementptr inbounds %struct.IntListT, ptr %1, i32 0, i32 1, !dbg !410
  %3 = load ptr, ptr %2, align 8, !dbg !410
  %4 = icmp eq ptr %3, null, !dbg !411
  ret i1 %4, !dbg !412
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10() #0 !dbg !413 !pallas.exprWrapper !377 {
  %1 = call ptr @"pallas.result ptr"(), !dbg !414
  %2 = getelementptr inbounds %struct.IntListT, ptr %1, i32 0, i32 0, !dbg !415
  %3 = load i32, ptr %2, align 8, !dbg !415
  %4 = icmp eq i32 %3, 0, !dbg !416
  ret i1 %4, !dbg !417
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #0 !dbg !248 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !252, metadata !DIExpression()), !dbg !418
  call void @llvm.dbg.value(metadata i32 %1, metadata !255, metadata !DIExpression()), !dbg !418
  call void @llvm.dbg.declare(metadata ptr %2, metadata !247, metadata !DIExpression()), !dbg !418
  %4 = call zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %2), !dbg !419
  ret i1 %4, !dbg !418
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2, ptr noundef byval(%pallas.seq.i32) align 8 %3) #0 !dbg !261 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !269, metadata !DIExpression()), !dbg !420
  call void @llvm.dbg.value(metadata i32 %1, metadata !271, metadata !DIExpression()), !dbg !420
  call void @llvm.dbg.declare(metadata ptr %2, metadata !260, metadata !DIExpression()), !dbg !420
  call void @llvm.dbg.declare(metadata ptr %3, metadata !266, metadata !DIExpression()), !dbg !420
  %5 = call ptr @"pallas.result ptr"(), !dbg !421
  %6 = call zeroext i1 @listWrite(ptr noundef %5, ptr noundef byval(%pallas.seq.i32) align 8 %3), !dbg !422
  ret i1 %6, !dbg !420
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2, ptr noundef byval(%pallas.seq.i32) align 8 %3) #0 !dbg !277 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !283, metadata !DIExpression()), !dbg !423
  call void @llvm.dbg.value(metadata i32 %1, metadata !285, metadata !DIExpression()), !dbg !423
  call void @llvm.dbg.declare(metadata ptr %2, metadata !276, metadata !DIExpression()), !dbg !423
  call void @llvm.dbg.declare(metadata ptr %3, metadata !280, metadata !DIExpression()), !dbg !423
  %5 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %3), !dbg !424
  %6 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %2), !dbg !425
  %7 = add i64 %6, 1, !dbg !426
  %8 = icmp eq i64 %5, %7, !dbg !427
  ret i1 %8, !dbg !423
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2, ptr noundef byval(%pallas.seq.i32) align 8 %3) #0 !dbg !291 !pallas.exprWrapper !377 {
  %5 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !297, metadata !DIExpression()), !dbg !428
  call void @llvm.dbg.value(metadata i32 %1, metadata !299, metadata !DIExpression()), !dbg !428
  call void @llvm.dbg.declare(metadata ptr %2, metadata !290, metadata !DIExpression()), !dbg !428
  call void @llvm.dbg.declare(metadata ptr %3, metadata !294, metadata !DIExpression()), !dbg !428
  %6 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %3), !dbg !429
  call void @"pallas.seq.slice i32"(ptr noundef sret(%pallas.seq.i32) %5, ptr noundef byval(%pallas.seq.i32) %3, i64 1, i64 %6), !dbg !429
  %7 = call i1 @"pallas.seq.equals i32"(ptr noundef byval(%pallas.seq.i32) %2, ptr noundef byval(%pallas.seq.i32) %5), !dbg !430
  ret i1 %7, !dbg !428
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2, ptr noundef byval(%pallas.seq.i32) align 8 %3) #0 !dbg !305 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !311, metadata !DIExpression()), !dbg !431
  call void @llvm.dbg.value(metadata i32 %1, metadata !313, metadata !DIExpression()), !dbg !431
  call void @llvm.dbg.declare(metadata ptr %2, metadata !304, metadata !DIExpression()), !dbg !431
  call void @llvm.dbg.declare(metadata ptr %3, metadata !308, metadata !DIExpression()), !dbg !431
  %5 = call i32 @"pallas.seq.get i32_i32"(ptr noundef byval(%pallas.seq.i32) %3, i64 noundef 0), !dbg !432
  %6 = icmp eq i32 %5, %1, !dbg !433
  ret i1 %6, !dbg !431
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_16(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !99 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !102, metadata !DIExpression()), !dbg !434
  call void @llvm.dbg.declare(metadata ptr %1, metadata !98, metadata !DIExpression()), !dbg !434
  %3 = call zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1), !dbg !435
  %4 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %1), !dbg !436
  %5 = icmp eq i64 %4, 0, !dbg !437
  %6 = call zeroext i1 @"pallas.unfolding zeroext i1_noundef zeroext i1"(i1 %3, i1 noundef zeroext %5), !dbg !438
  ret i1 %6, !dbg !434
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_17(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #0 !dbg !114 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !119, metadata !DIExpression()), !dbg !439
  call void @llvm.dbg.value(metadata i32 %1, metadata !121, metadata !DIExpression()), !dbg !439
  call void @llvm.dbg.declare(metadata ptr %2, metadata !113, metadata !DIExpression()), !dbg !439
  %4 = call zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %2), !dbg !440
  ret i1 %4, !dbg !439
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_18(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #0 !dbg !148 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !151, metadata !DIExpression()), !dbg !441
  call void @llvm.dbg.value(metadata i32 %1, metadata !153, metadata !DIExpression()), !dbg !441
  call void @llvm.dbg.declare(metadata ptr %2, metadata !147, metadata !DIExpression()), !dbg !441
  %4 = call zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %2), !dbg !442
  ret i1 %4, !dbg !441
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_19(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #0 !dbg !210 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !213, metadata !DIExpression()), !dbg !443
  call void @llvm.dbg.value(metadata i32 %1, metadata !215, metadata !DIExpression()), !dbg !443
  call void @llvm.dbg.declare(metadata ptr %2, metadata !209, metadata !DIExpression()), !dbg !443
  %4 = call zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %2), !dbg !444
  ret i1 %4, !dbg !443
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_20(ptr noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #0 !dbg !225 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !228, metadata !DIExpression()), !dbg !445
  call void @llvm.dbg.value(metadata i32 %1, metadata !230, metadata !DIExpression()), !dbg !445
  call void @llvm.dbg.declare(metadata ptr %2, metadata !224, metadata !DIExpression()), !dbg !445
  %4 = call zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %2), !dbg !446
  ret i1 %4, !dbg !445
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @PALLAS_SPEC_21(ptr noalias sret(%pallas.seq.i32) align 4 %0, ptr noundef %1, i32 noundef %2, ptr noundef %3, ptr noundef byval(%pallas.seq.i32) align 8 %4, ptr noundef byval(%pallas.seq.i32) align 8 %5) #0 !dbg !335 !pallas.ghostWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %1, metadata !343, metadata !DIExpression()), !dbg !447
  call void @llvm.dbg.value(metadata i32 %2, metadata !345, metadata !DIExpression()), !dbg !447
  call void @llvm.dbg.value(metadata ptr %3, metadata !347, metadata !DIExpression()), !dbg !447
  call void @llvm.dbg.declare(metadata ptr %4, metadata !334, metadata !DIExpression()), !dbg !447
  call void @llvm.dbg.declare(metadata ptr %5, metadata !340, metadata !DIExpression()), !dbg !447
  call void @"pallas.seq.prepend i32_noundef i32"(ptr sret(%pallas.seq.i32) %0, i32 noundef %2, ptr noundef byval(%pallas.seq.i32) %4), !dbg !448
  ret void, !dbg !447
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_22(ptr noundef %0, i32 noundef %1, ptr noundef %2, ptr noundef byval(%pallas.seq.i32) align 8 %3, ptr noundef byval(%pallas.seq.i32) align 8 %4) #0 !dbg !353 !pallas.exprWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !361, metadata !DIExpression()), !dbg !449
  call void @llvm.dbg.value(metadata i32 %1, metadata !363, metadata !DIExpression()), !dbg !449
  call void @llvm.dbg.value(metadata ptr %2, metadata !365, metadata !DIExpression()), !dbg !449
  call void @llvm.dbg.declare(metadata ptr %3, metadata !352, metadata !DIExpression()), !dbg !449
  call void @llvm.dbg.declare(metadata ptr %4, metadata !358, metadata !DIExpression()), !dbg !449
  %6 = call zeroext i1 @listWrite(ptr noundef %2, ptr noundef byval(%pallas.seq.i32) align 8 %4), !dbg !450
  ret i1 %6, !dbg !449
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @PALLAS_SPEC_23(ptr noalias sret(%pallas.seq.i32) align 4 %0, ptr noundef %1, i32 noundef %2, ptr noundef byval(%pallas.seq.i32) align 8 %3) #0 !dbg !131 !pallas.ghostWrapper !377 {
  call void @llvm.dbg.value(metadata ptr %1, metadata !136, metadata !DIExpression()), !dbg !451
  call void @llvm.dbg.value(metadata i32 %2, metadata !138, metadata !DIExpression()), !dbg !451
  call void @llvm.dbg.declare(metadata ptr %3, metadata !130, metadata !DIExpression()), !dbg !451
  %5 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %3), !dbg !452
  call void @"pallas.seq.slice i32"(ptr noundef sret(%pallas.seq.i32) %0, ptr noundef byval(%pallas.seq.i32) %3, i64 1, i64 %5), !dbg !452
  ret void, !dbg !451
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @listWrite(ptr noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !453 !pallas.predDef !454 {
  %3 = alloca %pallas.fracT, align 8
  %4 = alloca %pallas.seq.i32, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !455, metadata !DIExpression()), !dbg !456
  call void @llvm.dbg.declare(metadata ptr %1, metadata !457, metadata !DIExpression()), !dbg !456
  %5 = icmp eq ptr %0, null, !dbg !458
  %6 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %1), !dbg !459
  %7 = icmp eq i64 %6, 0, !dbg !460
  %8 = call i1 @pallas.imply(i1 %5, i1 %7), !dbg !461
  %9 = icmp ne ptr %0, null, !dbg !462
  %10 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %1), !dbg !463
  %11 = icmp ugt i64 %10, 0, !dbg !464
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !465
  %12 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !466
  %13 = getelementptr inbounds %struct.IntListT, ptr %0, i32 0, i32 0, !dbg !467
  %14 = load i32, ptr %13, align 8, !dbg !467
  %15 = call i32 @"pallas.seq.get i32_i32"(ptr noundef byval(%pallas.seq.i32) %1, i64 noundef 0), !dbg !468
  %16 = icmp eq i32 %14, %15, !dbg !469
  %17 = getelementptr inbounds %struct.IntListT, ptr %0, i32 0, i32 1, !dbg !470
  %18 = load ptr, ptr %17, align 8, !dbg !470
  %19 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %1), !dbg !471
  call void @"pallas.seq.slice i32"(ptr noundef sret(%pallas.seq.i32) %4, ptr noundef byval(%pallas.seq.i32) %1, i64 1, i64 %19), !dbg !471
  %20 = call zeroext i1 @listWrite(ptr noundef %18, ptr noundef byval(%pallas.seq.i32) align 8 %4), !dbg !472
  %21 = call i1 @pallas.sepConj(i1 %16, i1 %20), !dbg !473
  %22 = call i1 @pallas.sepConj(i1 %12, i1 %21), !dbg !474
  %23 = call i1 @pallas.sepConj(i1 %11, i1 %22), !dbg !475
  %24 = call i1 @pallas.imply(i1 %9, i1 %23), !dbg !476
  %25 = call i1 @pallas.sepConj(i1 %8, i1 %24), !dbg !477
  ret i1 %25, !dbg !456
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !478 i32 @"pallas.result i32"()

declare !pallas.specLib !478 ptr @"pallas.result ptr"()

declare !pallas.specLib !479 i1 @"pallas.seq.equals i32"(ptr noundef byval(%pallas.seq.i32), ptr noundef byval(%pallas.seq.i32))

declare !pallas.specLib !480 zeroext i1 @"pallas.unfolding zeroext i1_noundef zeroext i1"(i1 noundef zeroext, i1 noundef zeroext)

declare !pallas.specLib !481 void @"pallas.seq.prepend i32_noundef i32"(ptr sret(%pallas.seq.i32), i32 noundef, ptr noundef byval(%pallas.seq.i32))

declare !pallas.specLib !482 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !483 void @"pallas.seq.slice i32"(ptr sret(%pallas.seq.i32), ptr noundef byval(%pallas.seq.i32), i64, i64)

declare !pallas.specLib !484 i32 @"pallas.seq.get i32_i32"(ptr noundef byval(%pallas.seq.i32), i64 noundef)

declare !pallas.specLib !485 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !486 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !487 i1 @pallas.imply(i1, i1)

declare !pallas.specLib !488 i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32))

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!0, !4, !6, !8}
!llvm.module.flags = !{!9, !10, !11, !12, !13, !14, !15}
!llvm.ident = !{!16, !16}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_seq.c", directory: ".", checksumkind: CSK_MD5, checksum: "9bd78d33fbffbfdd99d713bfcb8ce7db")
!2 = !{!3}
!3 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!4 = distinct !DICompileUnit(language: DW_LANG_C11, file: !5, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2, splitDebugInlining: false, nameTableKind: None)
!5 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "04266fd7365ff0ace5afbca638e39d66")
!6 = distinct !DICompileUnit(language: DW_LANG_C, file: !7, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!7 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_seq.c", directory: "")
!8 = distinct !DICompileUnit(language: DW_LANG_C, file: !7, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!9 = !{i32 7, !"Dwarf Version", i32 5}
!10 = !{i32 2, !"Debug Info Version", i32 3}
!11 = !{i32 1, !"wchar_size", i32 4}
!12 = !{i32 8, !"PIC Level", i32 2}
!13 = !{i32 7, !"PIE Level", i32 2}
!14 = !{i32 7, !"uwtable", i32 2}
!15 = !{i32 7, !"frame-pointer", i32 2}
!16 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!17 = distinct !DISubprogram(name: "list_size", scope: !1, file: !1, line: 39, type: !18, scopeLine: 39, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!18 = !DISubroutineType(types: !19)
!19 = !{!20, !21}
!20 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!21 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !22, size: 64)
!22 = !DIDerivedType(tag: DW_TAG_typedef, name: "IntList", file: !1, line: 11, baseType: !23)
!23 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "IntListT", file: !1, line: 8, size: 128, elements: !24)
!24 = !{!25, !26}
!25 = !DIDerivedType(tag: DW_TAG_member, name: "value", scope: !23, file: !1, line: 9, baseType: !20, size: 32)
!26 = !DIDerivedType(tag: DW_TAG_member, name: "next", scope: !23, file: !1, line: 10, baseType: !27, size: 64, offset: 64)
!27 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !23, size: 64)
!28 = !{}
!29 = !{!30, i1 false, i1 false, !32, !28, !35, !67, !76}
!30 = !{!"pallas.srcLoc", i64 33, i64 1, i64 38, i64 1, !31}
!31 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_seq.c", directory: "", checksumkind: CSK_MD5, checksum: "9bd78d33fbffbfdd99d713bfcb8ce7db")
!32 = !{!33}
!33 = !{!34, !"s"}
!34 = !{!"pallas.srcLoc", i64 34, i64 1, i64 34, i64 17, !31}
!35 = !{!"pallas.requires", !36, ptr @PALLAS_SPEC_0, !37, !28, !63}
!36 = !{!"pallas.srcLoc", i64 35, i64 1, i64 35, i64 25, !31}
!37 = !{!38}
!38 = !{!33, !39}
!39 = !DILocalVariable(name: "s", arg: 2, scope: !40, file: !1, line: 35, type: !51)
!40 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 35, type: !41, scopeLine: 35, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!41 = !DISubroutineType(types: !42)
!42 = !{!43, !44, !51}
!43 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!44 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !45, size: 64)
!45 = !DIDerivedType(tag: DW_TAG_typedef, name: "IntList", file: !5, line: 12, baseType: !46)
!46 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "IntListT", file: !5, line: 9, size: 128, elements: !47)
!47 = !{!48, !49}
!48 = !DIDerivedType(tag: DW_TAG_member, name: "value", scope: !46, file: !5, line: 10, baseType: !20, size: 32)
!49 = !DIDerivedType(tag: DW_TAG_member, name: "next", scope: !46, file: !5, line: 11, baseType: !50, size: 64, offset: 64)
!50 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !46, size: 64)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "PALLAS_SEQ___int", file: !5, line: 16, baseType: !52)
!52 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !5, line: 16, size: 242003520, elements: !53)
!53 = !{!54, !55}
!54 = !DIDerivedType(tag: DW_TAG_member, name: "contentType", scope: !52, file: !5, line: 16, baseType: !20, size: 32)
!55 = !DIDerivedType(tag: DW_TAG_member, name: "dummy", scope: !52, file: !5, line: 16, baseType: !56, size: 242003488, offset: 32)
!56 = !DICompositeType(tag: DW_TAG_array_type, baseType: !57, size: 242003488, elements: !61)
!57 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !58, line: 26, baseType: !59)
!58 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!59 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !60, line: 41, baseType: !20)
!60 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!61 = !{!62}
!62 = !DISubrange(count: 7562609)
!63 = !{!64}
!64 = !{!65, !66}
!65 = !DILocalVariable(name: "l", arg: 1, scope: !17, file: !1, line: 39, type: !21)
!66 = !DILocalVariable(name: "l", arg: 1, scope: !40, file: !1, line: 35, type: !44)
!67 = !{!"pallas.ensures", !68, ptr @PALLAS_SPEC_1, !69, !28, !73}
!68 = !{!"pallas.srcLoc", i64 36, i64 1, i64 36, i64 25, !31}
!69 = !{!70}
!70 = !{!33, !71}
!71 = !DILocalVariable(name: "s", arg: 2, scope: !72, file: !1, line: 36, type: !51)
!72 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 36, type: !41, scopeLine: 36, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!73 = !{!74}
!74 = !{!65, !75}
!75 = !DILocalVariable(name: "l", arg: 1, scope: !72, file: !1, line: 36, type: !44)
!76 = !{!"pallas.ensures", !77, ptr @PALLAS_SPEC_2, !78, !28, !82}
!77 = !{!"pallas.srcLoc", i64 37, i64 1, i64 37, i64 42, !31}
!78 = !{!79}
!79 = !{!33, !80}
!80 = !DILocalVariable(name: "s", arg: 2, scope: !81, file: !1, line: 37, type: !51)
!81 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 37, type: !41, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!82 = !{!83}
!83 = !{!65, !84}
!84 = !DILocalVariable(name: "l", arg: 1, scope: !81, file: !1, line: 37, type: !44)
!85 = !DILocation(line: 39, column: 24, scope: !17)
!86 = !DILocation(line: 40, column: 9, scope: !87)
!87 = distinct !DILexicalBlock(scope: !17, file: !1, line: 40, column: 9)
!88 = !DILocation(line: 40, column: 11, scope: !87)
!89 = !DILocation(line: 40, column: 9, scope: !17)
!90 = !DILocation(line: 45, column: 9, scope: !91)
!91 = distinct !DILexicalBlock(scope: !87, file: !1, line: 40, column: 20)
!92 = !{!93, !94}
!93 = !{!"pallas.srcLoc", i64 41, i64 9, i64 44, i64 9, !31}
!94 = !{!"pallas.assert", !95, ptr @PALLAS_SPEC_16, !96, !28, !100}
!95 = !{!"pallas.srcLoc", i64 42, i64 9, i64 43, i64 55, !31}
!96 = !{!97}
!97 = !{!33, !98}
!98 = !DILocalVariable(name: "s", arg: 2, scope: !99, file: !1, line: 42, type: !51)
!99 = distinct !DISubprogram(name: "PALLAS_SPEC_16", scope: !1, file: !1, line: 42, type: !41, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!100 = !{!101}
!101 = !{!65, !102}
!102 = !DILocalVariable(name: "l", arg: 1, scope: !99, file: !1, line: 42, type: !44)
!103 = !DILocalVariable(name: "len", scope: !104, file: !1, line: 50, type: !20)
!104 = distinct !DILexicalBlock(scope: !87, file: !1, line: 46, column: 12)
!105 = !DILocation(line: 50, column: 13, scope: !104)
!106 = !DILocation(line: 50, column: 71, scope: !104)
!107 = !{!108, !109}
!108 = !{!"pallas.srcLoc", i64 47, i64 9, i64 49, i64 9, !31}
!109 = !{!"pallas.unfold", !110, ptr @PALLAS_SPEC_17, !111, !28, !117}
!110 = !{!"pallas.srcLoc", i64 48, i64 9, i64 48, i64 31, !31}
!111 = !{!112}
!112 = !{!33, !113}
!113 = !DILocalVariable(name: "s", arg: 3, scope: !114, file: !1, line: 48, type: !51)
!114 = distinct !DISubprogram(name: "PALLAS_SPEC_17", scope: !1, file: !1, line: 48, type: !115, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!115 = !DISubroutineType(types: !116)
!116 = !{!43, !44, !20, !51}
!117 = !{!118, !120}
!118 = !{!65, !119}
!119 = !DILocalVariable(name: "l", arg: 1, scope: !114, file: !1, line: 48, type: !44)
!120 = !{!103, !121}
!121 = !DILocalVariable(name: "len", arg: 2, scope: !114, file: !1, line: 48, type: !20)
!122 = !DILocation(line: 50, column: 74, scope: !104)
!123 = !DILocation(line: 50, column: 24, scope: !104)
!124 = !{!125, !126}
!125 = !{!"pallas.srcLoc", i64 50, i64 34, i64 50, i64 66, !31}
!126 = !{!"pallas.givenBinding", !127, ptr @PALLAS_SPEC_23, !128, !28, !134, !33}
!127 = !{!"pallas.srcLoc", i64 50, i64 44, i64 50, i64 64, !31}
!128 = !{!129}
!129 = !{!33, !130}
!130 = !DILocalVariable(name: "s", arg: 3, scope: !131, file: !1, line: 50, type: !51)
!131 = distinct !DISubprogram(name: "PALLAS_SPEC_23", scope: !1, file: !1, line: 50, type: !132, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!132 = !DISubroutineType(types: !133)
!133 = !{!51, !44, !20, !51}
!134 = !{!135, !137}
!135 = !{!65, !136}
!136 = !DILocalVariable(name: "l", arg: 1, scope: !131, file: !1, line: 50, type: !44)
!137 = !{!103, !138}
!138 = !DILocalVariable(name: "len", arg: 2, scope: !131, file: !1, line: 50, type: !20)
!139 = !DILocation(line: 50, column: 22, scope: !104)
!140 = !DILocation(line: 54, column: 16, scope: !104)
!141 = !{!142, !143}
!142 = !{!"pallas.srcLoc", i64 51, i64 9, i64 53, i64 9, !31}
!143 = !{!"pallas.fold", !144, ptr @PALLAS_SPEC_18, !145, !28, !149}
!144 = !{!"pallas.srcLoc", i64 52, i64 9, i64 52, i64 29, !31}
!145 = !{!146}
!146 = !{!33, !147}
!147 = !DILocalVariable(name: "s", arg: 3, scope: !148, file: !1, line: 52, type: !51)
!148 = distinct !DISubprogram(name: "PALLAS_SPEC_18", scope: !1, file: !1, line: 52, type: !115, scopeLine: 52, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!149 = !{!150, !152}
!150 = !{!65, !151}
!151 = !DILocalVariable(name: "l", arg: 1, scope: !148, file: !1, line: 52, type: !44)
!152 = !{!103, !153}
!153 = !DILocalVariable(name: "len", arg: 2, scope: !148, file: !1, line: 52, type: !20)
!154 = !DILocation(line: 54, column: 9, scope: !104)
!155 = !DILocation(line: 56, column: 1, scope: !17)
!156 = distinct !DISubprogram(name: "get_head", scope: !1, file: !1, line: 66, type: !18, scopeLine: 66, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!157 = !{!158, i1 false, i1 false, !159, !28, !162, !172, !181, !190}
!158 = !{!"pallas.srcLoc", i64 59, i64 1, i64 65, i64 1, !31}
!159 = !{!160}
!160 = !{!161, !"s"}
!161 = !{!"pallas.srcLoc", i64 60, i64 1, i64 60, i64 18, !31}
!162 = !{!"pallas.requires", !163, ptr @PALLAS_SPEC_3, !164, !28, !168}
!163 = !{!"pallas.srcLoc", i64 61, i64 1, i64 61, i64 30, !31}
!164 = !{!165}
!165 = !{!160, !166}
!166 = !DILocalVariable(name: "s", arg: 2, scope: !167, file: !1, line: 61, type: !51)
!167 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 61, type: !41, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!168 = !{!169}
!169 = !{!170, !171}
!170 = !DILocalVariable(name: "l", arg: 1, scope: !156, file: !1, line: 66, type: !21)
!171 = !DILocalVariable(name: "l", arg: 1, scope: !167, file: !1, line: 61, type: !44)
!172 = !{!"pallas.requires", !173, ptr @PALLAS_SPEC_4, !174, !28, !178}
!173 = !{!"pallas.srcLoc", i64 62, i64 1, i64 62, i64 25, !31}
!174 = !{!175}
!175 = !{!160, !176}
!176 = !DILocalVariable(name: "s", arg: 2, scope: !177, file: !1, line: 62, type: !51)
!177 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 62, type: !41, scopeLine: 62, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!178 = !{!179}
!179 = !{!170, !180}
!180 = !DILocalVariable(name: "l", arg: 1, scope: !177, file: !1, line: 62, type: !44)
!181 = !{!"pallas.ensures", !182, ptr @PALLAS_SPEC_5, !183, !28, !187}
!182 = !{!"pallas.srcLoc", i64 63, i64 1, i64 63, i64 25, !31}
!183 = !{!184}
!184 = !{!160, !185}
!185 = !DILocalVariable(name: "s", arg: 2, scope: !186, file: !1, line: 63, type: !51)
!186 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 63, type: !41, scopeLine: 63, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!187 = !{!188}
!188 = !{!170, !189}
!189 = !DILocalVariable(name: "l", arg: 1, scope: !186, file: !1, line: 63, type: !44)
!190 = !{!"pallas.ensures", !191, ptr @PALLAS_SPEC_6, !192, !28, !196}
!191 = !{!"pallas.srcLoc", i64 64, i64 1, i64 64, i64 41, !31}
!192 = !{!193}
!193 = !{!160, !194}
!194 = !DILocalVariable(name: "s", arg: 2, scope: !195, file: !1, line: 64, type: !51)
!195 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 64, type: !41, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!196 = !{!197}
!197 = !{!170, !198}
!198 = !DILocalVariable(name: "l", arg: 1, scope: !195, file: !1, line: 64, type: !44)
!199 = !DILocation(line: 66, column: 23, scope: !156)
!200 = !DILocalVariable(name: "res", scope: !156, file: !1, line: 68, type: !20)
!201 = !DILocation(line: 68, column: 9, scope: !156)
!202 = !DILocation(line: 68, column: 15, scope: !156)
!203 = !{!204, !205}
!204 = !{!"pallas.srcLoc", i64 67, i64 5, i64 67, i64 33, !31}
!205 = !{!"pallas.unfold", !206, ptr @PALLAS_SPEC_19, !207, !28, !211}
!206 = !{!"pallas.srcLoc", i64 67, i64 9, i64 67, i64 31, !31}
!207 = !{!208}
!208 = !{!160, !209}
!209 = !DILocalVariable(name: "s", arg: 3, scope: !210, file: !1, line: 67, type: !51)
!210 = distinct !DISubprogram(name: "PALLAS_SPEC_19", scope: !1, file: !1, line: 67, type: !115, scopeLine: 67, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!211 = !{!212, !214}
!212 = !{!170, !213}
!213 = !DILocalVariable(name: "l", arg: 1, scope: !210, file: !1, line: 67, type: !44)
!214 = !{!200, !215}
!215 = !DILocalVariable(name: "res", arg: 2, scope: !210, file: !1, line: 67, type: !20)
!216 = !DILocation(line: 68, column: 18, scope: !156)
!217 = !DILocation(line: 70, column: 12, scope: !156)
!218 = !{!219, !220}
!219 = !{!"pallas.srcLoc", i64 69, i64 5, i64 69, i64 33, !31}
!220 = !{!"pallas.fold", !221, ptr @PALLAS_SPEC_20, !222, !28, !226}
!221 = !{!"pallas.srcLoc", i64 69, i64 9, i64 69, i64 31, !31}
!222 = !{!223}
!223 = !{!160, !224}
!224 = !DILocalVariable(name: "s", arg: 3, scope: !225, file: !1, line: 69, type: !51)
!225 = distinct !DISubprogram(name: "PALLAS_SPEC_20", scope: !1, file: !1, line: 69, type: !115, scopeLine: 69, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!226 = !{!227, !229}
!227 = !{!170, !228}
!228 = !DILocalVariable(name: "l", arg: 1, scope: !225, file: !1, line: 69, type: !44)
!229 = !{!200, !230}
!230 = !DILocalVariable(name: "res", arg: 2, scope: !225, file: !1, line: 69, type: !20)
!231 = !DILocation(line: 70, column: 5, scope: !156)
!232 = distinct !DISubprogram(name: "prepend", scope: !1, file: !1, line: 97, type: !233, scopeLine: 97, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!233 = !DISubroutineType(types: !234)
!234 = !{!21, !21, !20}
!235 = !{!236, i1 false, i1 false, !237, !240, !243, !256, !272, !286, !300}
!236 = !{!"pallas.srcLoc", i64 88, i64 1, i64 96, i64 1, !31}
!237 = !{!238}
!238 = !{!239, !"s"}
!239 = !{!"pallas.srcLoc", i64 89, i64 1, i64 89, i64 18, !31}
!240 = !{!241}
!241 = !{!242, !"sNew"}
!242 = !{!"pallas.srcLoc", i64 90, i64 1, i64 90, i64 21, !31}
!243 = !{!"pallas.requires", !244, ptr @PALLAS_SPEC_11, !245, !28, !249}
!244 = !{!"pallas.srcLoc", i64 91, i64 1, i64 91, i64 25, !31}
!245 = !{!246}
!246 = !{!238, !247}
!247 = !DILocalVariable(name: "s", arg: 3, scope: !248, file: !1, line: 91, type: !51)
!248 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 91, type: !115, scopeLine: 91, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!249 = !{!250, !253}
!250 = !{!251, !252}
!251 = !DILocalVariable(name: "l", arg: 1, scope: !232, file: !1, line: 97, type: !21)
!252 = !DILocalVariable(name: "l", arg: 1, scope: !248, file: !1, line: 91, type: !44)
!253 = !{!254, !255}
!254 = !DILocalVariable(name: "elem", arg: 2, scope: !232, file: !1, line: 97, type: !20)
!255 = !DILocalVariable(name: "elem", arg: 2, scope: !248, file: !1, line: 91, type: !20)
!256 = !{!"pallas.ensures", !257, ptr @PALLAS_SPEC_12, !258, !264, !267}
!257 = !{!"pallas.srcLoc", i64 92, i64 1, i64 92, i64 43, !31}
!258 = !{!259}
!259 = !{!238, !260}
!260 = !DILocalVariable(name: "s", arg: 3, scope: !261, file: !1, line: 92, type: !51)
!261 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 92, type: !262, scopeLine: 92, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!262 = !DISubroutineType(types: !263)
!263 = !{!43, !44, !20, !51, !51}
!264 = !{!265}
!265 = !{!241, !266}
!266 = !DILocalVariable(name: "sNew", arg: 4, scope: !261, file: !1, line: 92, type: !51)
!267 = !{!268, !270}
!268 = !{!251, !269}
!269 = !DILocalVariable(name: "l", arg: 1, scope: !261, file: !1, line: 92, type: !44)
!270 = !{!254, !271}
!271 = !DILocalVariable(name: "elem", arg: 2, scope: !261, file: !1, line: 92, type: !20)
!272 = !{!"pallas.ensures", !273, ptr @PALLAS_SPEC_13, !274, !278, !281}
!273 = !{!"pallas.srcLoc", i64 93, i64 1, i64 93, i64 53, !31}
!274 = !{!275}
!275 = !{!238, !276}
!276 = !DILocalVariable(name: "s", arg: 3, scope: !277, file: !1, line: 93, type: !51)
!277 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !1, file: !1, line: 93, type: !262, scopeLine: 93, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!278 = !{!279}
!279 = !{!241, !280}
!280 = !DILocalVariable(name: "sNew", arg: 4, scope: !277, file: !1, line: 93, type: !51)
!281 = !{!282, !284}
!282 = !{!251, !283}
!283 = !DILocalVariable(name: "l", arg: 1, scope: !277, file: !1, line: 93, type: !44)
!284 = !{!254, !285}
!285 = !DILocalVariable(name: "elem", arg: 2, scope: !277, file: !1, line: 93, type: !20)
!286 = !{!"pallas.ensures", !287, ptr @PALLAS_SPEC_14, !288, !292, !295}
!287 = !{!"pallas.srcLoc", i64 94, i64 1, i64 94, i64 45, !31}
!288 = !{!289}
!289 = !{!238, !290}
!290 = !DILocalVariable(name: "s", arg: 3, scope: !291, file: !1, line: 94, type: !51)
!291 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !1, file: !1, line: 94, type: !262, scopeLine: 94, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!292 = !{!293}
!293 = !{!241, !294}
!294 = !DILocalVariable(name: "sNew", arg: 4, scope: !291, file: !1, line: 94, type: !51)
!295 = !{!296, !298}
!296 = !{!251, !297}
!297 = !DILocalVariable(name: "l", arg: 1, scope: !291, file: !1, line: 94, type: !44)
!298 = !{!254, !299}
!299 = !DILocalVariable(name: "elem", arg: 2, scope: !291, file: !1, line: 94, type: !20)
!300 = !{!"pallas.ensures", !301, ptr @PALLAS_SPEC_15, !302, !306, !309}
!301 = !{!"pallas.srcLoc", i64 95, i64 1, i64 95, i64 37, !31}
!302 = !{!303}
!303 = !{!238, !304}
!304 = !DILocalVariable(name: "s", arg: 3, scope: !305, file: !1, line: 95, type: !51)
!305 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !1, file: !1, line: 95, type: !262, scopeLine: 95, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!306 = !{!307}
!307 = !{!241, !308}
!308 = !DILocalVariable(name: "sNew", arg: 4, scope: !305, file: !1, line: 95, type: !51)
!309 = !{!310, !312}
!310 = !{!251, !311}
!311 = !DILocalVariable(name: "l", arg: 1, scope: !305, file: !1, line: 95, type: !44)
!312 = !{!254, !313}
!313 = !DILocalVariable(name: "elem", arg: 2, scope: !305, file: !1, line: 95, type: !20)
!314 = !DILocation(line: 97, column: 27, scope: !232)
!315 = !DILocation(line: 97, column: 34, scope: !232)
!316 = !DILocalVariable(name: "newHead", scope: !232, file: !1, line: 98, type: !21)
!317 = !DILocation(line: 98, column: 14, scope: !232)
!318 = !DILocation(line: 98, column: 24, scope: !232)
!319 = !DILocation(line: 99, column: 22, scope: !232)
!320 = !DILocation(line: 99, column: 5, scope: !232)
!321 = !DILocation(line: 99, column: 14, scope: !232)
!322 = !DILocation(line: 99, column: 20, scope: !232)
!323 = !DILocation(line: 100, column: 21, scope: !232)
!324 = !DILocation(line: 100, column: 5, scope: !232)
!325 = !DILocation(line: 100, column: 14, scope: !232)
!326 = !DILocation(line: 100, column: 19, scope: !232)
!327 = !DILocation(line: 105, column: 12, scope: !232)
!328 = !{!329, !330, !348}
!329 = !{!"pallas.srcLoc", i64 101, i64 5, i64 104, i64 5, !31}
!330 = !{!"pallas.gAssign", !331, ptr @PALLAS_SPEC_21, !332, !338, !341, !241}
!331 = !{!"pallas.srcLoc", i64 102, i64 5, i64 102, i64 50, !31}
!332 = !{!333}
!333 = !{!238, !334}
!334 = !DILocalVariable(name: "s", arg: 4, scope: !335, file: !1, line: 102, type: !51)
!335 = distinct !DISubprogram(name: "PALLAS_SPEC_21", scope: !1, file: !1, line: 102, type: !336, scopeLine: 102, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!336 = !DISubroutineType(types: !337)
!337 = !{!51, !44, !20, !44, !51, !51}
!338 = !{!339}
!339 = !{!241, !340}
!340 = !DILocalVariable(name: "sNew", arg: 5, scope: !335, file: !1, line: 102, type: !51)
!341 = !{!342, !344, !346}
!342 = !{!251, !343}
!343 = !DILocalVariable(name: "l", arg: 1, scope: !335, file: !1, line: 102, type: !44)
!344 = !{!254, !345}
!345 = !DILocalVariable(name: "elem", arg: 2, scope: !335, file: !1, line: 102, type: !20)
!346 = !{!316, !347}
!347 = !DILocalVariable(name: "newHead", arg: 3, scope: !335, file: !1, line: 102, type: !44)
!348 = !{!"pallas.fold", !349, ptr @PALLAS_SPEC_22, !350, !356, !359}
!349 = !{!"pallas.srcLoc", i64 103, i64 5, i64 103, i64 34, !31}
!350 = !{!351}
!351 = !{!238, !352}
!352 = !DILocalVariable(name: "s", arg: 4, scope: !353, file: !1, line: 103, type: !51)
!353 = distinct !DISubprogram(name: "PALLAS_SPEC_22", scope: !1, file: !1, line: 103, type: !354, scopeLine: 103, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!354 = !DISubroutineType(types: !355)
!355 = !{!43, !44, !20, !44, !51, !51}
!356 = !{!357}
!357 = !{!241, !358}
!358 = !DILocalVariable(name: "sNew", arg: 5, scope: !353, file: !1, line: 103, type: !51)
!359 = !{!360, !362, !364}
!360 = !{!251, !361}
!361 = !DILocalVariable(name: "l", arg: 1, scope: !353, file: !1, line: 103, type: !44)
!362 = !{!254, !363}
!363 = !DILocalVariable(name: "elem", arg: 2, scope: !353, file: !1, line: 103, type: !20)
!364 = !{!316, !365}
!365 = !DILocalVariable(name: "newHead", arg: 3, scope: !353, file: !1, line: 103, type: !44)
!366 = !DILocation(line: 105, column: 5, scope: !232)
!367 = !{!368, i1 false, i1 true, !28, !28, !369, !371, !373, !375}
!368 = !{!"pallas.srcLoc", i64 75, i64 1, i64 83, i64 1, !31}
!369 = !{!"pallas.ensures", !370, ptr @PALLAS_SPEC_7, !28, !28, !28}
!370 = !{!"pallas.srcLoc", i64 79, i64 1, i64 79, i64 33, !31}
!371 = !{!"pallas.ensures", !372, ptr @PALLAS_SPEC_8, !28, !28, !28}
!372 = !{!"pallas.srcLoc", i64 80, i64 1, i64 80, i64 40, !31}
!373 = !{!"pallas.ensures", !374, ptr @PALLAS_SPEC_9, !28, !28, !28}
!374 = !{!"pallas.srcLoc", i64 81, i64 1, i64 81, i64 39, !31}
!375 = !{!"pallas.ensures", !376, ptr @PALLAS_SPEC_10, !28, !28, !28}
!376 = !{!"pallas.srcLoc", i64 82, i64 1, i64 82, i64 37, !31}
!377 = !{!""}
!378 = !DILocation(line: 0, scope: !40)
!379 = !DILocation(line: 35, column: 10, scope: !40)
!380 = !DILocation(line: 0, scope: !72)
!381 = !DILocation(line: 36, column: 10, scope: !72)
!382 = !DILocation(line: 0, scope: !81)
!383 = !DILocation(line: 37, column: 10, scope: !81)
!384 = !DILocation(line: 37, column: 26, scope: !81)
!385 = !DILocation(line: 37, column: 23, scope: !81)
!386 = !DILocation(line: 0, scope: !167)
!387 = !DILocation(line: 61, column: 10, scope: !167)
!388 = !DILocation(line: 61, column: 27, scope: !167)
!389 = !DILocation(line: 0, scope: !177)
!390 = !DILocation(line: 62, column: 10, scope: !177)
!391 = !DILocation(line: 0, scope: !186)
!392 = !DILocation(line: 63, column: 10, scope: !186)
!393 = !DILocation(line: 0, scope: !195)
!394 = !DILocation(line: 64, column: 9, scope: !195)
!395 = !DILocation(line: 64, column: 25, scope: !195)
!396 = !DILocation(line: 64, column: 22, scope: !195)
!397 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !7, file: !7, line: 79, type: !398, scopeLine: 79, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8)
!398 = !DISubroutineType(types: !399)
!399 = !{!43}
!400 = !DILocation(line: 79, column: 9, scope: !397)
!401 = !DILocation(line: 79, column: 26, scope: !397)
!402 = !DILocation(line: 0, scope: !397)
!403 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !7, file: !7, line: 80, type: !398, scopeLine: 80, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8)
!404 = !DILocation(line: 80, column: 15, scope: !403)
!405 = !DILocation(line: 80, column: 33, scope: !403)
!406 = !DILocation(line: 80, column: 9, scope: !403)
!407 = !DILocation(line: 0, scope: !403)
!408 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !7, file: !7, line: 81, type: !398, scopeLine: 81, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8)
!409 = !DILocation(line: 81, column: 9, scope: !408)
!410 = !DILocation(line: 81, column: 27, scope: !408)
!411 = !DILocation(line: 81, column: 32, scope: !408)
!412 = !DILocation(line: 0, scope: !408)
!413 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !7, file: !7, line: 82, type: !398, scopeLine: 82, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !8)
!414 = !DILocation(line: 82, column: 9, scope: !413)
!415 = !DILocation(line: 82, column: 27, scope: !413)
!416 = !DILocation(line: 82, column: 33, scope: !413)
!417 = !DILocation(line: 0, scope: !413)
!418 = !DILocation(line: 0, scope: !248)
!419 = !DILocation(line: 91, column: 10, scope: !248)
!420 = !DILocation(line: 0, scope: !261)
!421 = !DILocation(line: 92, column: 20, scope: !261)
!422 = !DILocation(line: 92, column: 10, scope: !261)
!423 = !DILocation(line: 0, scope: !277)
!424 = !DILocation(line: 93, column: 10, scope: !277)
!425 = !DILocation(line: 93, column: 33, scope: !277)
!426 = !DILocation(line: 93, column: 50, scope: !277)
!427 = !DILocation(line: 93, column: 30, scope: !277)
!428 = !DILocation(line: 0, scope: !291)
!429 = !DILocation(line: 94, column: 25, scope: !291)
!430 = !DILocation(line: 94, column: 10, scope: !291)
!431 = !DILocation(line: 0, scope: !305)
!432 = !DILocation(line: 95, column: 10, scope: !305)
!433 = !DILocation(line: 95, column: 30, scope: !305)
!434 = !DILocation(line: 0, scope: !99)
!435 = !DILocation(line: 42, column: 33, scope: !99)
!436 = !DILocation(line: 43, column: 33, scope: !99)
!437 = !DILocation(line: 43, column: 50, scope: !99)
!438 = !DILocation(line: 42, column: 16, scope: !99)
!439 = !DILocation(line: 0, scope: !114)
!440 = !DILocation(line: 48, column: 16, scope: !114)
!441 = !DILocation(line: 0, scope: !148)
!442 = !DILocation(line: 52, column: 14, scope: !148)
!443 = !DILocation(line: 0, scope: !210)
!444 = !DILocation(line: 67, column: 16, scope: !210)
!445 = !DILocation(line: 0, scope: !225)
!446 = !DILocation(line: 69, column: 16, scope: !225)
!447 = !DILocation(line: 0, scope: !335)
!448 = !DILocation(line: 102, column: 25, scope: !335)
!449 = !DILocation(line: 0, scope: !353)
!450 = !DILocation(line: 103, column: 10, scope: !353)
!451 = !DILocation(line: 0, scope: !131)
!452 = !DILocation(line: 50, column: 48, scope: !131)
!453 = distinct !DISubprogram(name: "listWrite", scope: !7, file: !7, line: 22, type: !41, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !6, retainedNodes: !28)
!454 = !{i1 false}
!455 = !DILocalVariable(name: "l", arg: 1, scope: !453, file: !7, line: 22, type: !44)
!456 = !DILocation(line: 0, scope: !453)
!457 = !DILocalVariable(name: "s", arg: 2, scope: !453, file: !7, line: 22, type: !51)
!458 = !DILocation(line: 23, column: 18, scope: !453)
!459 = !DILocation(line: 23, column: 27, scope: !453)
!460 = !DILocation(line: 23, column: 44, scope: !453)
!461 = !DILocation(line: 23, column: 9, scope: !453)
!462 = !DILocation(line: 24, column: 18, scope: !453)
!463 = !DILocation(line: 24, column: 32, scope: !453)
!464 = !DILocation(line: 24, column: 49, scope: !453)
!465 = !DILocation(line: 25, column: 41, scope: !453)
!466 = !DILocation(line: 25, column: 32, scope: !453)
!467 = !DILocation(line: 26, column: 35, scope: !453)
!468 = !DILocation(line: 26, column: 44, scope: !453)
!469 = !DILocation(line: 26, column: 41, scope: !453)
!470 = !DILocation(line: 27, column: 40, scope: !453)
!471 = !DILocation(line: 27, column: 46, scope: !453)
!472 = !DILocation(line: 27, column: 27, scope: !453)
!473 = !DILocation(line: 26, column: 27, scope: !453)
!474 = !DILocation(line: 25, column: 27, scope: !453)
!475 = !DILocation(line: 24, column: 27, scope: !453)
!476 = !DILocation(line: 24, column: 9, scope: !453)
!477 = !DILocation(line: 22, column: 5, scope: !453)
!478 = !{!"pallas.result"}
!479 = !{!"pallas.seq.equals"}
!480 = !{!"pallas.unfolding"}
!481 = !{!"pallas.seq.prepend"}
!482 = !{!"pallas.sepConj"}
!483 = !{!"pallas.seq.slice"}
!484 = !{!"pallas.seq.get"}
!485 = !{!"pallas.perm"}
!486 = !{!"pallas.fracOf"}
!487 = !{!"pallas.imply"}
!488 = !{!"pallas.seq.size"}
