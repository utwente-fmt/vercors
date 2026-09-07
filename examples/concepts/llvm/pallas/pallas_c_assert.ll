; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_assert.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [13 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bar(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !24, metadata !DIExpression()), !dbg !46
  call void @llvm.dbg.value(metadata i32 %1, metadata !31, metadata !DIExpression()), !dbg !46
  call void @llvm.dbg.value(metadata i32 %2, metadata !34, metadata !DIExpression()), !dbg !46
  call void @llvm.dbg.value(metadata i32 0, metadata !47, metadata !DIExpression()), !dbg !46
  call void @llvm.dbg.value(metadata i32 0, metadata !48, metadata !DIExpression()), !dbg !50
  br label %4, !dbg !51

4:                                                ; preds = %8, %3
  %.01 = phi i32 [ 0, %3 ], [ %7, %8 ], !dbg !46
  %.0 = phi i32 [ 0, %3 ], [ %9, %8 ], !dbg !52
  call void @llvm.dbg.value(metadata i32 %.0, metadata !48, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.value(metadata i32 %.01, metadata !47, metadata !DIExpression()), !dbg !46
  %5 = icmp slt i32 %.0, %1, !dbg !53
  br i1 %5, label %6, label %10, !dbg !55

6:                                                ; preds = %4
  %7 = add nsw i32 %.01, %0, !dbg !56
  call void @llvm.dbg.value(metadata i32 %7, metadata !47, metadata !DIExpression()), !dbg !46
  br label %8, !dbg !58

8:                                                ; preds = %6
  %9 = add nsw i32 %.0, 1, !dbg !59
  call void @llvm.dbg.value(metadata i32 %9, metadata !48, metadata !DIExpression()), !dbg !50
  br label %4, !dbg !60, !llvm.loop !61

10:                                               ; preds = %4
  %11 = add nsw i32 %.01, %2, !dbg !96, !pallas.stmntBlock !97
  call void @llvm.dbg.value(metadata i32 %11, metadata !113, metadata !DIExpression()), !dbg !46
  ret i32 %11, !dbg !114, !pallas.stmntBlock !115
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !131 !pallas.fcontract !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !140, metadata !DIExpression()), !dbg !156
  call void @llvm.dbg.value(metadata i32 %1, metadata !146, metadata !DIExpression()), !dbg !156
  call void @llvm.dbg.value(metadata i32 %0, metadata !157, metadata !DIExpression()), !dbg !156
  %3 = icmp sgt i32 %0, %1, !dbg !158, !pallas.stmntBlock !160
  br i1 %3, label %4, label %6, !dbg !172

4:                                                ; preds = %2
  %5 = add nsw i32 %0, 1, !dbg !173
  call void @llvm.dbg.value(metadata i32 %5, metadata !140, metadata !DIExpression()), !dbg !156
  br label %8, !dbg !175

6:                                                ; preds = %2
  %7 = add nsw i32 %1, 1, !dbg !176
  call void @llvm.dbg.value(metadata i32 %7, metadata !146, metadata !DIExpression()), !dbg !156
  br label %8

8:                                                ; preds = %6, %4
  %.01 = phi i32 [ %1, %4 ], [ %7, %6 ]
  %.0 = phi i32 [ %5, %4 ], [ %0, %6 ]
  call void @llvm.dbg.value(metadata i32 %.0, metadata !140, metadata !DIExpression()), !dbg !156
  call void @llvm.dbg.value(metadata i32 %.01, metadata !146, metadata !DIExpression()), !dbg !156
  %9 = add nsw i32 %0, %.01, !dbg !178, !pallas.stmntBlock !179
  call void @llvm.dbg.value(metadata i32 %9, metadata !157, metadata !DIExpression()), !dbg !156
  %10 = add nsw i32 %.0, %.01, !dbg !191
  ret i32 %10, !dbg !192
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @amazingFunctionWithSomeBranches(i32 noundef %0, i32 noundef %1) #0 !dbg !193 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !194, metadata !DIExpression()), !dbg !195
  call void @llvm.dbg.value(metadata i32 %1, metadata !196, metadata !DIExpression()), !dbg !195
  call void @llvm.dbg.value(metadata i32 %0, metadata !197, metadata !DIExpression()), !dbg !195
  %3 = icmp slt i32 %0, 0, !dbg !198
  br i1 %3, label %4, label %13, !dbg !200

4:                                                ; preds = %2
  %5 = mul nsw i32 %0, -1, !dbg !201
  call void @llvm.dbg.value(metadata i32 %5, metadata !197, metadata !DIExpression()), !dbg !195
  %6 = icmp slt i32 %1, 0, !dbg !203, !pallas.stmntBlock !205
  br i1 %6, label %7, label %10, !dbg !217

7:                                                ; preds = %4
  %8 = sub nsw i32 0, %1, !dbg !218
  %9 = mul nsw i32 %5, %8, !dbg !220
  call void @llvm.dbg.value(metadata i32 %9, metadata !197, metadata !DIExpression()), !dbg !195
  br label %12, !dbg !221

10:                                               ; preds = %4
  %11 = mul nsw i32 %5, %1, !dbg !222
  call void @llvm.dbg.value(metadata i32 %11, metadata !197, metadata !DIExpression()), !dbg !195
  br label %12

12:                                               ; preds = %10, %7
  %.0 = phi i32 [ %9, %7 ], [ %11, %10 ], !dbg !224
  call void @llvm.dbg.value(metadata i32 %.0, metadata !197, metadata !DIExpression()), !dbg !195
  br label %15, !dbg !225, !pallas.stmntBlock !226

13:                                               ; preds = %2
  %14 = mul nsw i32 %0, %1, !dbg !238
  call void @llvm.dbg.value(metadata i32 %14, metadata !197, metadata !DIExpression()), !dbg !195
  br label %15

15:                                               ; preds = %13, %12
  %.1 = phi i32 [ %.0, %12 ], [ %14, %13 ], !dbg !240
  call void @llvm.dbg.value(metadata i32 %.1, metadata !197, metadata !DIExpression()), !dbg !195
  ret i32 %.1, !dbg !241, !pallas.stmntBlock !242
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !26 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !255
  call void @llvm.dbg.value(metadata i32 %1, metadata !32, metadata !DIExpression()), !dbg !255
  call void @llvm.dbg.value(metadata i32 %2, metadata !35, metadata !DIExpression()), !dbg !255
  %4 = icmp sgt i32 %0, 0, !dbg !256
  br i1 %4, label %5, label %9, !dbg !257

5:                                                ; preds = %3
  %6 = icmp sgt i32 %1, 0, !dbg !258
  br i1 %6, label %7, label %9, !dbg !259

7:                                                ; preds = %5
  %8 = icmp sgt i32 %2, 0, !dbg !260
  br label %9

9:                                                ; preds = %7, %5, %3
  %10 = phi i1 [ false, %5 ], [ false, %3 ], [ %8, %7 ], !dbg !255
  ret i1 %10, !dbg !255
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !41 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !40, metadata !DIExpression()), !dbg !261
  call void @llvm.dbg.value(metadata i32 %1, metadata !43, metadata !DIExpression()), !dbg !261
  call void @llvm.dbg.value(metadata i32 %2, metadata !45, metadata !DIExpression()), !dbg !261
  %4 = call i32 @"pallas.result i32"(), !dbg !262
  %5 = mul nsw i32 %0, %1, !dbg !263
  %6 = add nsw i32 %5, %2, !dbg !264
  %7 = icmp sge i32 %4, %6, !dbg !265
  ret i1 %7, !dbg !261
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1) #0 !dbg !142 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !141, metadata !DIExpression()), !dbg !266
  call void @llvm.dbg.value(metadata i32 %1, metadata !147, metadata !DIExpression()), !dbg !266
  %3 = icmp sgt i32 %0, 0, !dbg !267
  br i1 %3, label %4, label %6, !dbg !268

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !269
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !266
  ret i1 %7, !dbg !266
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1) #0 !dbg !153 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !152, metadata !DIExpression()), !dbg !270
  call void @llvm.dbg.value(metadata i32 %1, metadata !155, metadata !DIExpression()), !dbg !270
  %3 = call i32 @"pallas.result i32"(), !dbg !271
  %4 = icmp sgt i32 %3, 0, !dbg !272
  ret i1 %4, !dbg !270
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !87 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !86, metadata !DIExpression()), !dbg !273
  call void @llvm.dbg.value(metadata i32 %1, metadata !89, metadata !DIExpression()), !dbg !273
  call void @llvm.dbg.value(metadata i32 %2, metadata !91, metadata !DIExpression()), !dbg !273
  call void @llvm.dbg.value(metadata i32 %3, metadata !93, metadata !DIExpression()), !dbg !273
  call void @llvm.dbg.value(metadata i32 %4, metadata !95, metadata !DIExpression()), !dbg !273
  %6 = mul nsw i32 %4, %0, !dbg !274
  %7 = icmp eq i32 %3, %6, !dbg !275
  ret i1 %7, !dbg !273
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !71 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !70, metadata !DIExpression()), !dbg !276
  call void @llvm.dbg.value(metadata i32 %1, metadata !75, metadata !DIExpression()), !dbg !276
  call void @llvm.dbg.value(metadata i32 %2, metadata !77, metadata !DIExpression()), !dbg !276
  call void @llvm.dbg.value(metadata i32 %3, metadata !79, metadata !DIExpression()), !dbg !276
  call void @llvm.dbg.value(metadata i32 %4, metadata !81, metadata !DIExpression()), !dbg !276
  %6 = icmp sle i32 0, %4, !dbg !277
  br i1 %6, label %7, label %9, !dbg !278

7:                                                ; preds = %5
  %8 = icmp sle i32 %4, %1, !dbg !279
  br label %9

9:                                                ; preds = %7, %5
  %10 = phi i1 [ false, %5 ], [ %8, %7 ], !dbg !276
  ret i1 %10, !dbg !276
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !104 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !103, metadata !DIExpression()), !dbg !280
  call void @llvm.dbg.value(metadata i32 %1, metadata !108, metadata !DIExpression()), !dbg !280
  call void @llvm.dbg.value(metadata i32 %2, metadata !110, metadata !DIExpression()), !dbg !280
  call void @llvm.dbg.value(metadata i32 %3, metadata !112, metadata !DIExpression()), !dbg !280
  %5 = mul nsw i32 %0, %1, !dbg !281
  %6 = icmp eq i32 %3, %5, !dbg !282
  ret i1 %6, !dbg !280
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !122 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !121, metadata !DIExpression()), !dbg !283
  call void @llvm.dbg.value(metadata i32 %1, metadata !124, metadata !DIExpression()), !dbg !283
  call void @llvm.dbg.value(metadata i32 %2, metadata !126, metadata !DIExpression()), !dbg !283
  call void @llvm.dbg.value(metadata i32 %3, metadata !128, metadata !DIExpression()), !dbg !283
  call void @llvm.dbg.value(metadata i32 %4, metadata !130, metadata !DIExpression()), !dbg !283
  %6 = mul nsw i32 %0, %1, !dbg !284
  %7 = add nsw i32 %6, %2, !dbg !285
  call void @llvm.dbg.value(metadata i32 %7, metadata !130, metadata !DIExpression()), !dbg !283
  %8 = icmp ne i32 %7, 0, !dbg !286
  ret i1 %8, !dbg !283
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !167 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !166, metadata !DIExpression()), !dbg !287
  call void @llvm.dbg.value(metadata i32 %1, metadata !169, metadata !DIExpression()), !dbg !287
  call void @llvm.dbg.value(metadata i32 %2, metadata !171, metadata !DIExpression()), !dbg !287
  %4 = icmp sgt i32 %2, 0, !dbg !288
  ret i1 %4, !dbg !287
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !186 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !185, metadata !DIExpression()), !dbg !289
  call void @llvm.dbg.value(metadata i32 %1, metadata !188, metadata !DIExpression()), !dbg !289
  call void @llvm.dbg.value(metadata i32 %2, metadata !190, metadata !DIExpression()), !dbg !289
  %4 = icmp sle i32 %2, %0, !dbg !290
  ret i1 %4, !dbg !289
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !212 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !211, metadata !DIExpression()), !dbg !291
  call void @llvm.dbg.value(metadata i32 %1, metadata !214, metadata !DIExpression()), !dbg !291
  call void @llvm.dbg.value(metadata i32 %2, metadata !216, metadata !DIExpression()), !dbg !291
  %4 = sub nsw i32 0, %0, !dbg !292
  %5 = icmp eq i32 %2, %4, !dbg !293
  ret i1 %5, !dbg !291
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !233 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !232, metadata !DIExpression()), !dbg !294
  call void @llvm.dbg.value(metadata i32 %1, metadata !235, metadata !DIExpression()), !dbg !294
  call void @llvm.dbg.value(metadata i32 %2, metadata !237, metadata !DIExpression()), !dbg !294
  %4 = icmp sge i32 %2, 0, !dbg !295
  ret i1 %4, !dbg !294
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !249 !pallas.exprWrapper !254 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !248, metadata !DIExpression()), !dbg !296
  call void @llvm.dbg.value(metadata i32 %1, metadata !251, metadata !DIExpression()), !dbg !296
  call void @llvm.dbg.value(metadata i32 %2, metadata !253, metadata !DIExpression()), !dbg !296
  %4 = icmp slt i32 %0, 0, !dbg !297
  %5 = icmp sge i32 %2, 0, !dbg !298
  %6 = call i1 @pallas.imply(i1 %4, i1 %5), !dbg !299
  ret i1 %6, !dbg !296
}

declare !pallas.specLib !300 i32 @"pallas.result i32"()

declare !pallas.specLib !301 i1 @pallas.imply(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_assert.c", directory: ".", checksumkind: CSK_MD5, checksum: "75228d18208f31bd8d07e4ab62411fd6")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "92db1ea9a9355183e842a378325294c8")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 14, type: !13, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15, !15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, i1 false, !16, !16, !20, !36}
!18 = !{!"pallas.srcLoc", i64 10, i64 1, i64 13, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_assert.c", directory: "", checksumkind: CSK_MD5, checksum: "75228d18208f31bd8d07e4ab62411fd6")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !16, !16, !22}
!21 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 33, !19}
!22 = !{!23, !30, !33}
!23 = !{!24, !25}
!24 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 14, type: !15)
!25 = !DILocalVariable(name: "a", arg: 1, scope: !26, file: !1, line: 11, type: !15)
!26 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 11, type: !27, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!27 = !DISubroutineType(types: !28)
!28 = !{!29, !15, !15, !15}
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !{!31, !32}
!31 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 14, type: !15)
!32 = !DILocalVariable(name: "b", arg: 2, scope: !26, file: !1, line: 11, type: !15)
!33 = !{!34, !35}
!34 = !DILocalVariable(name: "c", arg: 3, scope: !12, file: !1, line: 14, type: !15)
!35 = !DILocalVariable(name: "c", arg: 3, scope: !26, file: !1, line: 11, type: !15)
!36 = !{!"pallas.ensures", !37, ptr @PALLAS_SPEC_1, !16, !16, !38}
!37 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 36, !19}
!38 = !{!39, !42, !44}
!39 = !{!24, !40}
!40 = !DILocalVariable(name: "a", arg: 1, scope: !41, file: !1, line: 12, type: !15)
!41 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 12, type: !27, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!42 = !{!31, !43}
!43 = !DILocalVariable(name: "b", arg: 2, scope: !41, file: !1, line: 12, type: !15)
!44 = !{!34, !45}
!45 = !DILocalVariable(name: "c", arg: 3, scope: !41, file: !1, line: 12, type: !15)
!46 = !DILocation(line: 0, scope: !12)
!47 = !DILocalVariable(name: "tmp", scope: !12, file: !1, line: 15, type: !15)
!48 = !DILocalVariable(name: "i", scope: !49, file: !1, line: 20, type: !15)
!49 = distinct !DILexicalBlock(scope: !12, file: !1, line: 20, column: 5)
!50 = !DILocation(line: 0, scope: !49)
!51 = !DILocation(line: 20, column: 10, scope: !49)
!52 = !DILocation(line: 20, scope: !49)
!53 = !DILocation(line: 20, column: 23, scope: !54)
!54 = distinct !DILexicalBlock(scope: !49, file: !1, line: 20, column: 5)
!55 = !DILocation(line: 20, column: 5, scope: !49)
!56 = !DILocation(line: 21, column: 13, scope: !57)
!57 = distinct !DILexicalBlock(scope: !54, file: !1, line: 20, column: 33)
!58 = !DILocation(line: 22, column: 5, scope: !57)
!59 = !DILocation(line: 20, column: 28, scope: !54)
!60 = !DILocation(line: 20, column: 5, scope: !54)
!61 = distinct !{!61, !55, !62, !63, !64}
!62 = !DILocation(line: 22, column: 5, scope: !49)
!63 = !{!"llvm.loop.mustprogress"}
!64 = !{!"pallas.loopInvBlock", !65, !66, !82}
!65 = !{!"pallas.srcLoc", i64 16, i64 5, i64 19, i64 5, !19}
!66 = !{!"pallas.loopInv", !67, ptr @PALLAS_SPEC_4, !16, !16, !68}
!67 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 36, !19}
!68 = !{!69, !74, !76, !78, !80}
!69 = !{!24, !70}
!70 = !DILocalVariable(name: "a", arg: 1, scope: !71, file: !1, line: 17, type: !15)
!71 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 17, type: !72, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!72 = !DISubroutineType(types: !73)
!73 = !{!29, !15, !15, !15, !15, !15}
!74 = !{!31, !75}
!75 = !DILocalVariable(name: "b", arg: 2, scope: !71, file: !1, line: 17, type: !15)
!76 = !{!34, !77}
!77 = !DILocalVariable(name: "c", arg: 3, scope: !71, file: !1, line: 17, type: !15)
!78 = !{!47, !79}
!79 = !DILocalVariable(name: "tmp", arg: 4, scope: !71, file: !1, line: 17, type: !15)
!80 = !{!48, !81}
!81 = !DILocalVariable(name: "i", arg: 5, scope: !71, file: !1, line: 17, type: !15)
!82 = !{!"pallas.loopInv", !83, ptr @PALLAS_SPEC_5, !16, !16, !84}
!83 = !{!"pallas.srcLoc", i64 18, i64 5, i64 18, i64 32, !19}
!84 = !{!85, !88, !90, !92, !94}
!85 = !{!24, !86}
!86 = !DILocalVariable(name: "a", arg: 1, scope: !87, file: !1, line: 18, type: !15)
!87 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 18, type: !72, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!88 = !{!31, !89}
!89 = !DILocalVariable(name: "b", arg: 2, scope: !87, file: !1, line: 18, type: !15)
!90 = !{!34, !91}
!91 = !DILocalVariable(name: "c", arg: 3, scope: !87, file: !1, line: 18, type: !15)
!92 = !{!47, !93}
!93 = !DILocalVariable(name: "tmp", arg: 4, scope: !87, file: !1, line: 18, type: !15)
!94 = !{!48, !95}
!95 = !DILocalVariable(name: "i", arg: 5, scope: !87, file: !1, line: 18, type: !15)
!96 = !DILocation(line: 27, column: 20, scope: !12)
!97 = !{!98, !99}
!98 = !{!"pallas.srcLoc", i64 23, i64 5, i64 25, i64 5, !19}
!99 = !{!"pallas.assert", !100, ptr @PALLAS_SPEC_6, !16, !16, !101}
!100 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 24, !19}
!101 = !{!102, !107, !109, !111}
!102 = !{!24, !103}
!103 = !DILocalVariable(name: "a", arg: 1, scope: !104, file: !1, line: 24, type: !15)
!104 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 24, type: !105, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!105 = !DISubroutineType(types: !106)
!106 = !{!29, !15, !15, !15, !15}
!107 = !{!31, !108}
!108 = !DILocalVariable(name: "b", arg: 2, scope: !104, file: !1, line: 24, type: !15)
!109 = !{!34, !110}
!110 = !DILocalVariable(name: "c", arg: 3, scope: !104, file: !1, line: 24, type: !15)
!111 = !{!47, !112}
!112 = !DILocalVariable(name: "tmp", arg: 4, scope: !104, file: !1, line: 24, type: !15)
!113 = !DILocalVariable(name: "tmp2", scope: !12, file: !1, line: 27, type: !15)
!114 = !DILocation(line: 31, column: 5, scope: !12)
!115 = !{!116, !117}
!116 = !{!"pallas.srcLoc", i64 28, i64 5, i64 30, i64 5, !19}
!117 = !{!"pallas.assert", !118, ptr @PALLAS_SPEC_7, !16, !16, !119}
!118 = !{!"pallas.srcLoc", i64 29, i64 5, i64 29, i64 30, !19}
!119 = !{!120, !123, !125, !127, !129}
!120 = !{!24, !121}
!121 = !DILocalVariable(name: "a", arg: 1, scope: !122, file: !1, line: 29, type: !15)
!122 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !1, file: !1, line: 29, type: !72, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!123 = !{!31, !124}
!124 = !DILocalVariable(name: "b", arg: 2, scope: !122, file: !1, line: 29, type: !15)
!125 = !{!34, !126}
!126 = !DILocalVariable(name: "c", arg: 3, scope: !122, file: !1, line: 29, type: !15)
!127 = !{!47, !128}
!128 = !DILocalVariable(name: "tmp", arg: 4, scope: !122, file: !1, line: 29, type: !15)
!129 = !{!113, !130}
!130 = !DILocalVariable(name: "tmp2", arg: 5, scope: !122, file: !1, line: 29, type: !15)
!131 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 38, type: !132, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!132 = !DISubroutineType(types: !133)
!133 = !{!15, !15, !15}
!134 = !{!135, i1 false, i1 false, !16, !16, !136, !148}
!135 = !{!"pallas.srcLoc", i64 34, i64 1, i64 37, i64 1, !19}
!136 = !{!"pallas.requires", !137, ptr @PALLAS_SPEC_2, !16, !16, !138}
!137 = !{!"pallas.srcLoc", i64 35, i64 1, i64 35, i64 24, !19}
!138 = !{!139, !145}
!139 = !{!140, !141}
!140 = !DILocalVariable(name: "a", arg: 1, scope: !131, file: !1, line: 38, type: !15)
!141 = !DILocalVariable(name: "a", arg: 1, scope: !142, file: !1, line: 35, type: !15)
!142 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 35, type: !143, scopeLine: 35, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!143 = !DISubroutineType(types: !144)
!144 = !{!29, !15, !15}
!145 = !{!146, !147}
!146 = !DILocalVariable(name: "b", arg: 2, scope: !131, file: !1, line: 38, type: !15)
!147 = !DILocalVariable(name: "b", arg: 2, scope: !142, file: !1, line: 35, type: !15)
!148 = !{!"pallas.ensures", !149, ptr @PALLAS_SPEC_3, !16, !16, !150}
!149 = !{!"pallas.srcLoc", i64 36, i64 1, i64 36, i64 26, !19}
!150 = !{!151, !154}
!151 = !{!140, !152}
!152 = !DILocalVariable(name: "a", arg: 1, scope: !153, file: !1, line: 36, type: !15)
!153 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 36, type: !143, scopeLine: 36, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!154 = !{!146, !155}
!155 = !DILocalVariable(name: "b", arg: 2, scope: !153, file: !1, line: 36, type: !15)
!156 = !DILocation(line: 0, scope: !131)
!157 = !DILocalVariable(name: "tmp", scope: !131, file: !1, line: 39, type: !15)
!158 = !DILocation(line: 41, column: 11, scope: !159)
!159 = distinct !DILexicalBlock(scope: !131, file: !1, line: 41, column: 9)
!160 = !{!161, !162}
!161 = !{!"pallas.srcLoc", i64 40, i64 5, i64 40, i64 25, !19}
!162 = !{!"pallas.assert", !163, ptr @PALLAS_SPEC_8, !16, !16, !164}
!163 = !{!"pallas.srcLoc", i64 40, i64 9, i64 40, i64 23, !19}
!164 = !{!165, !168, !170}
!165 = !{!140, !166}
!166 = !DILocalVariable(name: "a", arg: 1, scope: !167, file: !1, line: 40, type: !15)
!167 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !1, file: !1, line: 40, type: !27, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!168 = !{!146, !169}
!169 = !DILocalVariable(name: "b", arg: 2, scope: !167, file: !1, line: 40, type: !15)
!170 = !{!157, !171}
!171 = !DILocalVariable(name: "tmp", arg: 3, scope: !167, file: !1, line: 40, type: !15)
!172 = !DILocation(line: 41, column: 9, scope: !131)
!173 = !DILocation(line: 42, column: 10, scope: !174)
!174 = distinct !DILexicalBlock(scope: !159, file: !1, line: 41, column: 16)
!175 = !DILocation(line: 43, column: 5, scope: !174)
!176 = !DILocation(line: 44, column: 10, scope: !177)
!177 = distinct !DILexicalBlock(scope: !159, file: !1, line: 43, column: 12)
!178 = !DILocation(line: 47, column: 9, scope: !131)
!179 = !{!180, !181}
!180 = !{!"pallas.srcLoc", i64 46, i64 5, i64 46, i64 26, !19}
!181 = !{!"pallas.assert", !182, ptr @PALLAS_SPEC_9, !16, !16, !183}
!182 = !{!"pallas.srcLoc", i64 46, i64 9, i64 46, i64 24, !19}
!183 = !{!184, !187, !189}
!184 = !{!140, !185}
!185 = !DILocalVariable(name: "a", arg: 1, scope: !186, file: !1, line: 46, type: !15)
!186 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !1, file: !1, line: 46, type: !27, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!187 = !{!146, !188}
!188 = !DILocalVariable(name: "b", arg: 2, scope: !186, file: !1, line: 46, type: !15)
!189 = !{!157, !190}
!190 = !DILocalVariable(name: "tmp", arg: 3, scope: !186, file: !1, line: 46, type: !15)
!191 = !DILocation(line: 48, column: 14, scope: !131)
!192 = !DILocation(line: 48, column: 5, scope: !131)
!193 = distinct !DISubprogram(name: "amazingFunctionWithSomeBranches", scope: !1, file: !1, line: 51, type: !132, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!194 = !DILocalVariable(name: "a", arg: 1, scope: !193, file: !1, line: 51, type: !15)
!195 = !DILocation(line: 0, scope: !193)
!196 = !DILocalVariable(name: "b", arg: 2, scope: !193, file: !1, line: 51, type: !15)
!197 = !DILocalVariable(name: "aVariable", scope: !193, file: !1, line: 52, type: !15)
!198 = !DILocation(line: 53, column: 11, scope: !199)
!199 = distinct !DILexicalBlock(scope: !193, file: !1, line: 53, column: 9)
!200 = !DILocation(line: 53, column: 9, scope: !193)
!201 = !DILocation(line: 54, column: 19, scope: !202)
!202 = distinct !DILexicalBlock(scope: !199, file: !1, line: 53, column: 16)
!203 = !DILocation(line: 58, column: 16, scope: !204)
!204 = distinct !DILexicalBlock(scope: !202, file: !1, line: 58, column: 14)
!205 = !{!206, !207}
!206 = !{!"pallas.srcLoc", i64 55, i64 9, i64 57, i64 9, !19}
!207 = !{!"pallas.assert", !208, ptr @PALLAS_SPEC_10, !16, !16, !209}
!208 = !{!"pallas.srcLoc", i64 56, i64 9, i64 56, i64 31, !19}
!209 = !{!210, !213, !215}
!210 = !{!194, !211}
!211 = !DILocalVariable(name: "a", arg: 1, scope: !212, file: !1, line: 56, type: !15)
!212 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !1, file: !1, line: 56, type: !27, scopeLine: 56, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!213 = !{!196, !214}
!214 = !DILocalVariable(name: "b", arg: 2, scope: !212, file: !1, line: 56, type: !15)
!215 = !{!197, !216}
!216 = !DILocalVariable(name: "aVariable", arg: 3, scope: !212, file: !1, line: 56, type: !15)
!217 = !DILocation(line: 58, column: 14, scope: !202)
!218 = !DILocation(line: 59, column: 26, scope: !219)
!219 = distinct !DILexicalBlock(scope: !204, file: !1, line: 58, column: 21)
!220 = !DILocation(line: 59, column: 23, scope: !219)
!221 = !DILocation(line: 60, column: 9, scope: !219)
!222 = !DILocation(line: 61, column: 23, scope: !223)
!223 = distinct !DILexicalBlock(scope: !204, file: !1, line: 60, column: 16)
!224 = !DILocation(line: 0, scope: !204)
!225 = !DILocation(line: 66, column: 5, scope: !202)
!226 = !{!227, !228}
!227 = !{!"pallas.srcLoc", i64 63, i64 9, i64 65, i64 9, !19}
!228 = !{!"pallas.assert", !229, ptr @PALLAS_SPEC_11, !16, !16, !230}
!229 = !{!"pallas.srcLoc", i64 64, i64 9, i64 64, i64 30, !19}
!230 = !{!231, !234, !236}
!231 = !{!194, !232}
!232 = !DILocalVariable(name: "a", arg: 1, scope: !233, file: !1, line: 64, type: !15)
!233 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 64, type: !27, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!234 = !{!196, !235}
!235 = !DILocalVariable(name: "b", arg: 2, scope: !233, file: !1, line: 64, type: !15)
!236 = !{!197, !237}
!237 = !DILocalVariable(name: "aVariable", arg: 3, scope: !233, file: !1, line: 64, type: !15)
!238 = !DILocation(line: 67, column: 19, scope: !239)
!239 = distinct !DILexicalBlock(scope: !199, file: !1, line: 66, column: 12)
!240 = !DILocation(line: 0, scope: !199)
!241 = !DILocation(line: 74, column: 5, scope: !193)
!242 = !{!243, !244}
!243 = !{!"pallas.srcLoc", i64 70, i64 5, i64 73, i64 5, !19}
!244 = !{!"pallas.assert", !245, ptr @PALLAS_SPEC_12, !16, !16, !246}
!245 = !{!"pallas.srcLoc", i64 71, i64 5, i64 72, i64 34, !19}
!246 = !{!247, !250, !252}
!247 = !{!194, !248}
!248 = !DILocalVariable(name: "a", arg: 1, scope: !249, file: !1, line: 71, type: !15)
!249 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 71, type: !27, scopeLine: 71, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!250 = !{!196, !251}
!251 = !DILocalVariable(name: "b", arg: 2, scope: !249, file: !1, line: 71, type: !15)
!252 = !{!197, !253}
!253 = !DILocalVariable(name: "aVariable", arg: 3, scope: !249, file: !1, line: 71, type: !15)
!254 = !{!""}
!255 = !DILocation(line: 0, scope: !26)
!256 = !DILocation(line: 11, column: 12, scope: !26)
!257 = !DILocation(line: 11, column: 16, scope: !26)
!258 = !DILocation(line: 11, column: 21, scope: !26)
!259 = !DILocation(line: 11, column: 25, scope: !26)
!260 = !DILocation(line: 11, column: 30, scope: !26)
!261 = !DILocation(line: 0, scope: !41)
!262 = !DILocation(line: 12, column: 9, scope: !41)
!263 = !DILocation(line: 12, column: 28, scope: !41)
!264 = !DILocation(line: 12, column: 33, scope: !41)
!265 = !DILocation(line: 12, column: 22, scope: !41)
!266 = !DILocation(line: 0, scope: !142)
!267 = !DILocation(line: 35, column: 12, scope: !142)
!268 = !DILocation(line: 35, column: 16, scope: !142)
!269 = !DILocation(line: 35, column: 21, scope: !142)
!270 = !DILocation(line: 0, scope: !153)
!271 = !DILocation(line: 36, column: 10, scope: !153)
!272 = !DILocation(line: 36, column: 23, scope: !153)
!273 = !DILocation(line: 0, scope: !87)
!274 = !DILocation(line: 18, column: 29, scope: !87)
!275 = !DILocation(line: 18, column: 24, scope: !87)
!276 = !DILocation(line: 0, scope: !71)
!277 = !DILocation(line: 17, column: 22, scope: !71)
!278 = !DILocation(line: 17, column: 27, scope: !71)
!279 = !DILocation(line: 17, column: 32, scope: !71)
!280 = !DILocation(line: 0, scope: !104)
!281 = !DILocation(line: 24, column: 21, scope: !104)
!282 = !DILocation(line: 24, column: 16, scope: !104)
!283 = !DILocation(line: 0, scope: !122)
!284 = !DILocation(line: 29, column: 22, scope: !122)
!285 = !DILocation(line: 29, column: 27, scope: !122)
!286 = !DILocation(line: 29, column: 12, scope: !122)
!287 = !DILocation(line: 0, scope: !167)
!288 = !DILocation(line: 40, column: 20, scope: !167)
!289 = !DILocation(line: 0, scope: !186)
!290 = !DILocation(line: 46, column: 20, scope: !186)
!291 = !DILocation(line: 0, scope: !212)
!292 = !DILocation(line: 56, column: 29, scope: !212)
!293 = !DILocation(line: 56, column: 26, scope: !212)
!294 = !DILocation(line: 0, scope: !233)
!295 = !DILocation(line: 64, column: 26, scope: !233)
!296 = !DILocation(line: 0, scope: !249)
!297 = !DILocation(line: 71, column: 21, scope: !249)
!298 = !DILocation(line: 72, column: 29, scope: !249)
!299 = !DILocation(line: 71, column: 12, scope: !249)
!300 = !{!"pallas.result"}
!301 = !{!"pallas.imply"}
