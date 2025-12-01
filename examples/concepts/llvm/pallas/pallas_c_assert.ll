; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_assert.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [13 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bar(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !22, metadata !DIExpression()), !dbg !27
  call void @llvm.dbg.value(metadata i32 %1, metadata !23, metadata !DIExpression()), !dbg !27
  call void @llvm.dbg.value(metadata i32 %2, metadata !24, metadata !DIExpression()), !dbg !27
  call void @llvm.dbg.value(metadata i32 0, metadata !28, metadata !DIExpression()), !dbg !27
  call void @llvm.dbg.value(metadata i32 0, metadata !29, metadata !DIExpression()), !dbg !31
  br label %4, !dbg !32

4:                                                ; preds = %8, %3
  %.01 = phi i32 [ 0, %3 ], [ %7, %8 ], !dbg !27
  %.0 = phi i32 [ 0, %3 ], [ %9, %8 ], !dbg !33
  call void @llvm.dbg.value(metadata i32 %.0, metadata !29, metadata !DIExpression()), !dbg !31
  call void @llvm.dbg.value(metadata i32 %.01, metadata !28, metadata !DIExpression()), !dbg !27
  %5 = icmp slt i32 %.0, %1, !dbg !34
  br i1 %5, label %6, label %10, !dbg !36

6:                                                ; preds = %4
  %7 = add nsw i32 %.01, %0, !dbg !37
  call void @llvm.dbg.value(metadata i32 %7, metadata !28, metadata !DIExpression()), !dbg !27
  br label %8, !dbg !39

8:                                                ; preds = %6
  %9 = add nsw i32 %.0, 1, !dbg !40
  call void @llvm.dbg.value(metadata i32 %9, metadata !29, metadata !DIExpression()), !dbg !31
  br label %4, !dbg !41, !llvm.loop !42

10:                                               ; preds = %4
  %11 = add nsw i32 %.01, %2, !dbg !51, !pallas.stmntBlock !52
  call void @llvm.dbg.value(metadata i32 %11, metadata !56, metadata !DIExpression()), !dbg !27
  ret i32 %11, !dbg !57, !pallas.stmntBlock !58
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !62 !pallas.fcontract !65 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !69, metadata !DIExpression()), !dbg !73
  call void @llvm.dbg.value(metadata i32 %1, metadata !70, metadata !DIExpression()), !dbg !73
  call void @llvm.dbg.value(metadata i32 %0, metadata !74, metadata !DIExpression()), !dbg !73
  %3 = icmp sgt i32 %0, %1, !dbg !75, !pallas.stmntBlock !77
  br i1 %3, label %4, label %6, !dbg !81

4:                                                ; preds = %2
  %5 = add nsw i32 %0, 1, !dbg !82
  call void @llvm.dbg.value(metadata i32 %5, metadata !69, metadata !DIExpression()), !dbg !73
  br label %8, !dbg !84

6:                                                ; preds = %2
  %7 = add nsw i32 %1, 1, !dbg !85
  call void @llvm.dbg.value(metadata i32 %7, metadata !70, metadata !DIExpression()), !dbg !73
  br label %8

8:                                                ; preds = %6, %4
  %.01 = phi i32 [ %1, %4 ], [ %7, %6 ]
  %.0 = phi i32 [ %5, %4 ], [ %0, %6 ]
  call void @llvm.dbg.value(metadata i32 %.0, metadata !69, metadata !DIExpression()), !dbg !73
  call void @llvm.dbg.value(metadata i32 %.01, metadata !70, metadata !DIExpression()), !dbg !73
  %9 = add nsw i32 %0, %.01, !dbg !87, !pallas.stmntBlock !88
  call void @llvm.dbg.value(metadata i32 %9, metadata !74, metadata !DIExpression()), !dbg !73
  %10 = add nsw i32 %.0, %.01, !dbg !92
  ret i32 %10, !dbg !93
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @amazingFunctionWithSomeBranches(i32 noundef %0, i32 noundef %1) #0 !dbg !94 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !95, metadata !DIExpression()), !dbg !96
  call void @llvm.dbg.value(metadata i32 %1, metadata !97, metadata !DIExpression()), !dbg !96
  call void @llvm.dbg.value(metadata i32 %0, metadata !98, metadata !DIExpression()), !dbg !96
  %3 = icmp slt i32 %0, 0, !dbg !99
  br i1 %3, label %4, label %13, !dbg !101

4:                                                ; preds = %2
  %5 = mul nsw i32 %0, -1, !dbg !102
  call void @llvm.dbg.value(metadata i32 %5, metadata !98, metadata !DIExpression()), !dbg !96
  %6 = icmp slt i32 %1, 0, !dbg !104, !pallas.stmntBlock !106
  br i1 %6, label %7, label %10, !dbg !110

7:                                                ; preds = %4
  %8 = sub nsw i32 0, %1, !dbg !111
  %9 = mul nsw i32 %5, %8, !dbg !113
  call void @llvm.dbg.value(metadata i32 %9, metadata !98, metadata !DIExpression()), !dbg !96
  br label %12, !dbg !114

10:                                               ; preds = %4
  %11 = mul nsw i32 %5, %1, !dbg !115
  call void @llvm.dbg.value(metadata i32 %11, metadata !98, metadata !DIExpression()), !dbg !96
  br label %12

12:                                               ; preds = %10, %7
  %.0 = phi i32 [ %9, %7 ], [ %11, %10 ], !dbg !117
  call void @llvm.dbg.value(metadata i32 %.0, metadata !98, metadata !DIExpression()), !dbg !96
  br label %15, !dbg !118, !pallas.stmntBlock !119

13:                                               ; preds = %2
  %14 = mul nsw i32 %0, %1, !dbg !123
  call void @llvm.dbg.value(metadata i32 %14, metadata !98, metadata !DIExpression()), !dbg !96
  br label %15

15:                                               ; preds = %13, %12
  %.1 = phi i32 [ %.0, %12 ], [ %14, %13 ], !dbg !125
  call void @llvm.dbg.value(metadata i32 %.1, metadata !98, metadata !DIExpression()), !dbg !96
  ret i32 %.1, !dbg !126, !pallas.stmntBlock !127
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !131 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !136, metadata !DIExpression()), !dbg !137
  call void @llvm.dbg.value(metadata i32 %1, metadata !138, metadata !DIExpression()), !dbg !137
  call void @llvm.dbg.value(metadata i32 %2, metadata !139, metadata !DIExpression()), !dbg !137
  %4 = icmp sgt i32 %0, 0, !dbg !140
  br i1 %4, label %5, label %9, !dbg !141

5:                                                ; preds = %3
  %6 = icmp sgt i32 %1, 0, !dbg !142
  br i1 %6, label %7, label %9, !dbg !143

7:                                                ; preds = %5
  %8 = icmp sgt i32 %2, 0, !dbg !144
  br label %9

9:                                                ; preds = %7, %5, %3
  %10 = phi i1 [ false, %5 ], [ false, %3 ], [ %8, %7 ], !dbg !137
  ret i1 %10, !dbg !137
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !145 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !146, metadata !DIExpression()), !dbg !147
  call void @llvm.dbg.value(metadata i32 %1, metadata !148, metadata !DIExpression()), !dbg !147
  call void @llvm.dbg.value(metadata i32 %2, metadata !149, metadata !DIExpression()), !dbg !147
  %4 = call i32 @"pallas.result i32"(), !dbg !150
  %5 = mul nsw i32 %0, %1, !dbg !151
  %6 = add nsw i32 %5, %2, !dbg !152
  %7 = icmp sge i32 %4, %6, !dbg !153
  ret i1 %7, !dbg !147
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1) #0 !dbg !154 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !157, metadata !DIExpression()), !dbg !158
  call void @llvm.dbg.value(metadata i32 %1, metadata !159, metadata !DIExpression()), !dbg !158
  %3 = icmp sgt i32 %0, 0, !dbg !160
  br i1 %3, label %4, label %6, !dbg !161

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !162
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !158
  ret i1 %7, !dbg !158
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1) #0 !dbg !163 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !164, metadata !DIExpression()), !dbg !165
  call void @llvm.dbg.value(metadata i32 %1, metadata !166, metadata !DIExpression()), !dbg !165
  %3 = call i32 @"pallas.result i32"(), !dbg !167
  %4 = icmp sgt i32 %3, 0, !dbg !168
  ret i1 %4, !dbg !165
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !169 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !172, metadata !DIExpression()), !dbg !173
  call void @llvm.dbg.value(metadata i32 %1, metadata !174, metadata !DIExpression()), !dbg !173
  call void @llvm.dbg.value(metadata i32 %2, metadata !175, metadata !DIExpression()), !dbg !173
  call void @llvm.dbg.value(metadata i32 %3, metadata !176, metadata !DIExpression()), !dbg !173
  call void @llvm.dbg.value(metadata i32 %4, metadata !177, metadata !DIExpression()), !dbg !173
  %6 = mul nsw i32 %4, %0, !dbg !178
  %7 = icmp eq i32 %3, %6, !dbg !179
  ret i1 %7, !dbg !173
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !180 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !181, metadata !DIExpression()), !dbg !182
  call void @llvm.dbg.value(metadata i32 %1, metadata !183, metadata !DIExpression()), !dbg !182
  call void @llvm.dbg.value(metadata i32 %2, metadata !184, metadata !DIExpression()), !dbg !182
  call void @llvm.dbg.value(metadata i32 %3, metadata !185, metadata !DIExpression()), !dbg !182
  call void @llvm.dbg.value(metadata i32 %4, metadata !186, metadata !DIExpression()), !dbg !182
  %6 = icmp sle i32 0, %4, !dbg !187
  br i1 %6, label %7, label %9, !dbg !188

7:                                                ; preds = %5
  %8 = icmp sle i32 %4, %1, !dbg !189
  br label %9

9:                                                ; preds = %7, %5
  %10 = phi i1 [ false, %5 ], [ %8, %7 ], !dbg !182
  ret i1 %10, !dbg !182
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !190 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !191, metadata !DIExpression()), !dbg !192
  call void @llvm.dbg.value(metadata i32 %1, metadata !193, metadata !DIExpression()), !dbg !192
  call void @llvm.dbg.value(metadata i32 %2, metadata !194, metadata !DIExpression()), !dbg !192
  call void @llvm.dbg.value(metadata i32 %3, metadata !195, metadata !DIExpression()), !dbg !192
  call void @llvm.dbg.value(metadata i32 %4, metadata !196, metadata !DIExpression()), !dbg !192
  %6 = mul nsw i32 %0, %1, !dbg !197
  %7 = add nsw i32 %6, %2, !dbg !198
  call void @llvm.dbg.value(metadata i32 %7, metadata !196, metadata !DIExpression()), !dbg !192
  %8 = icmp ne i32 %7, 0, !dbg !199
  ret i1 %8, !dbg !192
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !200 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !203, metadata !DIExpression()), !dbg !204
  call void @llvm.dbg.value(metadata i32 %1, metadata !205, metadata !DIExpression()), !dbg !204
  call void @llvm.dbg.value(metadata i32 %2, metadata !206, metadata !DIExpression()), !dbg !204
  call void @llvm.dbg.value(metadata i32 %3, metadata !207, metadata !DIExpression()), !dbg !204
  %5 = mul nsw i32 %0, %1, !dbg !208
  %6 = icmp eq i32 %3, %5, !dbg !209
  ret i1 %6, !dbg !204
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !210 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !211, metadata !DIExpression()), !dbg !212
  call void @llvm.dbg.value(metadata i32 %1, metadata !213, metadata !DIExpression()), !dbg !212
  call void @llvm.dbg.value(metadata i32 %2, metadata !214, metadata !DIExpression()), !dbg !212
  %4 = icmp sgt i32 %2, 0, !dbg !215
  ret i1 %4, !dbg !212
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !216 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !217, metadata !DIExpression()), !dbg !218
  call void @llvm.dbg.value(metadata i32 %1, metadata !219, metadata !DIExpression()), !dbg !218
  call void @llvm.dbg.value(metadata i32 %2, metadata !220, metadata !DIExpression()), !dbg !218
  %4 = icmp sle i32 %2, %0, !dbg !221
  ret i1 %4, !dbg !218
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !222 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !223, metadata !DIExpression()), !dbg !224
  call void @llvm.dbg.value(metadata i32 %1, metadata !225, metadata !DIExpression()), !dbg !224
  call void @llvm.dbg.value(metadata i32 %2, metadata !226, metadata !DIExpression()), !dbg !224
  %4 = sub nsw i32 0, %0, !dbg !227
  %5 = icmp eq i32 %2, %4, !dbg !228
  ret i1 %5, !dbg !224
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !229 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !230, metadata !DIExpression()), !dbg !231
  call void @llvm.dbg.value(metadata i32 %1, metadata !232, metadata !DIExpression()), !dbg !231
  call void @llvm.dbg.value(metadata i32 %2, metadata !233, metadata !DIExpression()), !dbg !231
  %4 = icmp sge i32 %2, 0, !dbg !234
  ret i1 %4, !dbg !231
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !235 !pallas.exprWrapper !135 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !236, metadata !DIExpression()), !dbg !237
  call void @llvm.dbg.value(metadata i32 %1, metadata !238, metadata !DIExpression()), !dbg !237
  call void @llvm.dbg.value(metadata i32 %2, metadata !239, metadata !DIExpression()), !dbg !237
  %4 = icmp slt i32 %0, 0, !dbg !240
  %5 = icmp sge i32 %2, 0, !dbg !241
  %6 = call i1 @pallas.imply(i1 %4, i1 %5), !dbg !242
  ret i1 %6, !dbg !237
}

declare !pallas.specLib !243 i32 @"pallas.result i32"()

declare !pallas.specLib !244 i1 @pallas.imply(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_assert.c", directory: ".", checksumkind: CSK_MD5, checksum: "75228d18208f31bd8d07e4ab62411fd6")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "bf4658dc807724070c5b758c930dd2fd")
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
!17 = !{!18, i1 false, i1 false, !20, !25}
!18 = !{!"pallas.srcLoc", i64 10, i64 1, i64 13, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_assert.c", directory: "", checksumkind: CSK_MD5, checksum: "75228d18208f31bd8d07e4ab62411fd6")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !22, !23, !24}
!21 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 33, !19}
!22 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 14, type: !15)
!23 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 14, type: !15)
!24 = !DILocalVariable(name: "c", arg: 3, scope: !12, file: !1, line: 14, type: !15)
!25 = !{!"pallas.ensures", !26, ptr @PALLAS_SPEC_1, !22, !23, !24}
!26 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 36, !19}
!27 = !DILocation(line: 0, scope: !12)
!28 = !DILocalVariable(name: "tmp", scope: !12, file: !1, line: 15, type: !15)
!29 = !DILocalVariable(name: "i", scope: !30, file: !1, line: 20, type: !15)
!30 = distinct !DILexicalBlock(scope: !12, file: !1, line: 20, column: 5)
!31 = !DILocation(line: 0, scope: !30)
!32 = !DILocation(line: 20, column: 10, scope: !30)
!33 = !DILocation(line: 20, scope: !30)
!34 = !DILocation(line: 20, column: 23, scope: !35)
!35 = distinct !DILexicalBlock(scope: !30, file: !1, line: 20, column: 5)
!36 = !DILocation(line: 20, column: 5, scope: !30)
!37 = !DILocation(line: 21, column: 13, scope: !38)
!38 = distinct !DILexicalBlock(scope: !35, file: !1, line: 20, column: 33)
!39 = !DILocation(line: 22, column: 5, scope: !38)
!40 = !DILocation(line: 20, column: 28, scope: !35)
!41 = !DILocation(line: 20, column: 5, scope: !35)
!42 = distinct !{!42, !36, !43, !44, !45}
!43 = !DILocation(line: 22, column: 5, scope: !30)
!44 = !{!"llvm.loop.mustprogress"}
!45 = !{!"pallas.loopInv", !46, !47, !49}
!46 = !{!"pallas.srcLoc", i64 16, i64 5, i64 19, i64 5, !19}
!47 = !{!48, ptr @PALLAS_SPEC_4, !22, !23, !24, !28, !29}
!48 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 36, !19}
!49 = !{!50, ptr @PALLAS_SPEC_5, !22, !23, !24, !28, !29}
!50 = !{!"pallas.srcLoc", i64 18, i64 5, i64 18, i64 32, !19}
!51 = !DILocation(line: 27, column: 20, scope: !12)
!52 = !{!53, !54}
!53 = !{!"pallas.srcLoc", i64 23, i64 5, i64 25, i64 5, !19}
!54 = !{!"pallas.assert", !55, ptr @PALLAS_SPEC_6, !22, !23, !24, !28}
!55 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 24, !19}
!56 = !DILocalVariable(name: "tmp2", scope: !12, file: !1, line: 27, type: !15)
!57 = !DILocation(line: 31, column: 5, scope: !12)
!58 = !{!59, !60}
!59 = !{!"pallas.srcLoc", i64 28, i64 5, i64 30, i64 5, !19}
!60 = !{!"pallas.assert", !61, ptr @PALLAS_SPEC_7, !22, !23, !24, !28, !56}
!61 = !{!"pallas.srcLoc", i64 29, i64 5, i64 29, i64 30, !19}
!62 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 38, type: !63, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!63 = !DISubroutineType(types: !64)
!64 = !{!15, !15, !15}
!65 = !{!66, i1 false, i1 false, !67, !71}
!66 = !{!"pallas.srcLoc", i64 34, i64 1, i64 37, i64 1, !19}
!67 = !{!"pallas.requires", !68, ptr @PALLAS_SPEC_2, !69, !70}
!68 = !{!"pallas.srcLoc", i64 35, i64 1, i64 35, i64 24, !19}
!69 = !DILocalVariable(name: "a", arg: 1, scope: !62, file: !1, line: 38, type: !15)
!70 = !DILocalVariable(name: "b", arg: 2, scope: !62, file: !1, line: 38, type: !15)
!71 = !{!"pallas.ensures", !72, ptr @PALLAS_SPEC_3, !69, !70}
!72 = !{!"pallas.srcLoc", i64 36, i64 1, i64 36, i64 26, !19}
!73 = !DILocation(line: 0, scope: !62)
!74 = !DILocalVariable(name: "tmp", scope: !62, file: !1, line: 39, type: !15)
!75 = !DILocation(line: 41, column: 11, scope: !76)
!76 = distinct !DILexicalBlock(scope: !62, file: !1, line: 41, column: 9)
!77 = !{!78, !79}
!78 = !{!"pallas.srcLoc", i64 40, i64 5, i64 40, i64 25, !19}
!79 = !{!"pallas.assert", !80, ptr @PALLAS_SPEC_8, !69, !70, !74}
!80 = !{!"pallas.srcLoc", i64 40, i64 9, i64 40, i64 23, !19}
!81 = !DILocation(line: 41, column: 9, scope: !62)
!82 = !DILocation(line: 42, column: 10, scope: !83)
!83 = distinct !DILexicalBlock(scope: !76, file: !1, line: 41, column: 16)
!84 = !DILocation(line: 43, column: 5, scope: !83)
!85 = !DILocation(line: 44, column: 10, scope: !86)
!86 = distinct !DILexicalBlock(scope: !76, file: !1, line: 43, column: 12)
!87 = !DILocation(line: 47, column: 9, scope: !62)
!88 = !{!89, !90}
!89 = !{!"pallas.srcLoc", i64 46, i64 5, i64 46, i64 26, !19}
!90 = !{!"pallas.assert", !91, ptr @PALLAS_SPEC_9, !69, !70, !74}
!91 = !{!"pallas.srcLoc", i64 46, i64 9, i64 46, i64 24, !19}
!92 = !DILocation(line: 48, column: 14, scope: !62)
!93 = !DILocation(line: 48, column: 5, scope: !62)
!94 = distinct !DISubprogram(name: "amazingFunctionWithSomeBranches", scope: !1, file: !1, line: 51, type: !63, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!95 = !DILocalVariable(name: "a", arg: 1, scope: !94, file: !1, line: 51, type: !15)
!96 = !DILocation(line: 0, scope: !94)
!97 = !DILocalVariable(name: "b", arg: 2, scope: !94, file: !1, line: 51, type: !15)
!98 = !DILocalVariable(name: "aVariable", scope: !94, file: !1, line: 52, type: !15)
!99 = !DILocation(line: 53, column: 11, scope: !100)
!100 = distinct !DILexicalBlock(scope: !94, file: !1, line: 53, column: 9)
!101 = !DILocation(line: 53, column: 9, scope: !94)
!102 = !DILocation(line: 54, column: 19, scope: !103)
!103 = distinct !DILexicalBlock(scope: !100, file: !1, line: 53, column: 16)
!104 = !DILocation(line: 58, column: 16, scope: !105)
!105 = distinct !DILexicalBlock(scope: !103, file: !1, line: 58, column: 14)
!106 = !{!107, !108}
!107 = !{!"pallas.srcLoc", i64 55, i64 9, i64 57, i64 9, !19}
!108 = !{!"pallas.assert", !109, ptr @PALLAS_SPEC_10, !95, !97, !98}
!109 = !{!"pallas.srcLoc", i64 56, i64 9, i64 56, i64 31, !19}
!110 = !DILocation(line: 58, column: 14, scope: !103)
!111 = !DILocation(line: 59, column: 26, scope: !112)
!112 = distinct !DILexicalBlock(scope: !105, file: !1, line: 58, column: 21)
!113 = !DILocation(line: 59, column: 23, scope: !112)
!114 = !DILocation(line: 60, column: 9, scope: !112)
!115 = !DILocation(line: 61, column: 23, scope: !116)
!116 = distinct !DILexicalBlock(scope: !105, file: !1, line: 60, column: 16)
!117 = !DILocation(line: 0, scope: !105)
!118 = !DILocation(line: 66, column: 5, scope: !103)
!119 = !{!120, !121}
!120 = !{!"pallas.srcLoc", i64 63, i64 9, i64 65, i64 9, !19}
!121 = !{!"pallas.assert", !122, ptr @PALLAS_SPEC_11, !95, !97, !98}
!122 = !{!"pallas.srcLoc", i64 64, i64 9, i64 64, i64 30, !19}
!123 = !DILocation(line: 67, column: 19, scope: !124)
!124 = distinct !DILexicalBlock(scope: !100, file: !1, line: 66, column: 12)
!125 = !DILocation(line: 0, scope: !100)
!126 = !DILocation(line: 74, column: 5, scope: !94)
!127 = !{!128, !129}
!128 = !{!"pallas.srcLoc", i64 70, i64 5, i64 73, i64 5, !19}
!129 = !{!"pallas.assert", !130, ptr @PALLAS_SPEC_12, !95, !97, !98}
!130 = !{!"pallas.srcLoc", i64 71, i64 5, i64 72, i64 34, !19}
!131 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 11, type: !132, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!132 = !DISubroutineType(types: !133)
!133 = !{!134, !15, !15, !15}
!134 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!135 = !{!""}
!136 = !DILocalVariable(name: "a", arg: 1, scope: !131, file: !1, line: 11, type: !15)
!137 = !DILocation(line: 0, scope: !131)
!138 = !DILocalVariable(name: "b", arg: 2, scope: !131, file: !1, line: 11, type: !15)
!139 = !DILocalVariable(name: "c", arg: 3, scope: !131, file: !1, line: 11, type: !15)
!140 = !DILocation(line: 11, column: 12, scope: !131)
!141 = !DILocation(line: 11, column: 16, scope: !131)
!142 = !DILocation(line: 11, column: 21, scope: !131)
!143 = !DILocation(line: 11, column: 25, scope: !131)
!144 = !DILocation(line: 11, column: 30, scope: !131)
!145 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 12, type: !132, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!146 = !DILocalVariable(name: "a", arg: 1, scope: !145, file: !1, line: 12, type: !15)
!147 = !DILocation(line: 0, scope: !145)
!148 = !DILocalVariable(name: "b", arg: 2, scope: !145, file: !1, line: 12, type: !15)
!149 = !DILocalVariable(name: "c", arg: 3, scope: !145, file: !1, line: 12, type: !15)
!150 = !DILocation(line: 12, column: 9, scope: !145)
!151 = !DILocation(line: 12, column: 28, scope: !145)
!152 = !DILocation(line: 12, column: 33, scope: !145)
!153 = !DILocation(line: 12, column: 22, scope: !145)
!154 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 35, type: !155, scopeLine: 35, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!155 = !DISubroutineType(types: !156)
!156 = !{!134, !15, !15}
!157 = !DILocalVariable(name: "a", arg: 1, scope: !154, file: !1, line: 35, type: !15)
!158 = !DILocation(line: 0, scope: !154)
!159 = !DILocalVariable(name: "b", arg: 2, scope: !154, file: !1, line: 35, type: !15)
!160 = !DILocation(line: 35, column: 12, scope: !154)
!161 = !DILocation(line: 35, column: 16, scope: !154)
!162 = !DILocation(line: 35, column: 21, scope: !154)
!163 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 36, type: !155, scopeLine: 36, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!164 = !DILocalVariable(name: "a", arg: 1, scope: !163, file: !1, line: 36, type: !15)
!165 = !DILocation(line: 0, scope: !163)
!166 = !DILocalVariable(name: "b", arg: 2, scope: !163, file: !1, line: 36, type: !15)
!167 = !DILocation(line: 36, column: 10, scope: !163)
!168 = !DILocation(line: 36, column: 23, scope: !163)
!169 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 18, type: !170, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!170 = !DISubroutineType(types: !171)
!171 = !{!134, !15, !15, !15, !15, !15}
!172 = !DILocalVariable(name: "a", arg: 1, scope: !169, file: !1, line: 18, type: !15)
!173 = !DILocation(line: 0, scope: !169)
!174 = !DILocalVariable(name: "b", arg: 2, scope: !169, file: !1, line: 18, type: !15)
!175 = !DILocalVariable(name: "c", arg: 3, scope: !169, file: !1, line: 18, type: !15)
!176 = !DILocalVariable(name: "tmp", arg: 4, scope: !169, file: !1, line: 18, type: !15)
!177 = !DILocalVariable(name: "i", arg: 5, scope: !169, file: !1, line: 18, type: !15)
!178 = !DILocation(line: 18, column: 29, scope: !169)
!179 = !DILocation(line: 18, column: 24, scope: !169)
!180 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 17, type: !170, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!181 = !DILocalVariable(name: "a", arg: 1, scope: !180, file: !1, line: 17, type: !15)
!182 = !DILocation(line: 0, scope: !180)
!183 = !DILocalVariable(name: "b", arg: 2, scope: !180, file: !1, line: 17, type: !15)
!184 = !DILocalVariable(name: "c", arg: 3, scope: !180, file: !1, line: 17, type: !15)
!185 = !DILocalVariable(name: "tmp", arg: 4, scope: !180, file: !1, line: 17, type: !15)
!186 = !DILocalVariable(name: "i", arg: 5, scope: !180, file: !1, line: 17, type: !15)
!187 = !DILocation(line: 17, column: 22, scope: !180)
!188 = !DILocation(line: 17, column: 27, scope: !180)
!189 = !DILocation(line: 17, column: 32, scope: !180)
!190 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !1, file: !1, line: 29, type: !170, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!191 = !DILocalVariable(name: "a", arg: 1, scope: !190, file: !1, line: 29, type: !15)
!192 = !DILocation(line: 0, scope: !190)
!193 = !DILocalVariable(name: "b", arg: 2, scope: !190, file: !1, line: 29, type: !15)
!194 = !DILocalVariable(name: "c", arg: 3, scope: !190, file: !1, line: 29, type: !15)
!195 = !DILocalVariable(name: "tmp", arg: 4, scope: !190, file: !1, line: 29, type: !15)
!196 = !DILocalVariable(name: "tmp2", arg: 5, scope: !190, file: !1, line: 29, type: !15)
!197 = !DILocation(line: 29, column: 22, scope: !190)
!198 = !DILocation(line: 29, column: 27, scope: !190)
!199 = !DILocation(line: 29, column: 12, scope: !190)
!200 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 24, type: !201, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!201 = !DISubroutineType(types: !202)
!202 = !{!134, !15, !15, !15, !15}
!203 = !DILocalVariable(name: "a", arg: 1, scope: !200, file: !1, line: 24, type: !15)
!204 = !DILocation(line: 0, scope: !200)
!205 = !DILocalVariable(name: "b", arg: 2, scope: !200, file: !1, line: 24, type: !15)
!206 = !DILocalVariable(name: "c", arg: 3, scope: !200, file: !1, line: 24, type: !15)
!207 = !DILocalVariable(name: "tmp", arg: 4, scope: !200, file: !1, line: 24, type: !15)
!208 = !DILocation(line: 24, column: 21, scope: !200)
!209 = !DILocation(line: 24, column: 16, scope: !200)
!210 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !1, file: !1, line: 40, type: !132, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!211 = !DILocalVariable(name: "a", arg: 1, scope: !210, file: !1, line: 40, type: !15)
!212 = !DILocation(line: 0, scope: !210)
!213 = !DILocalVariable(name: "b", arg: 2, scope: !210, file: !1, line: 40, type: !15)
!214 = !DILocalVariable(name: "tmp", arg: 3, scope: !210, file: !1, line: 40, type: !15)
!215 = !DILocation(line: 40, column: 20, scope: !210)
!216 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !1, file: !1, line: 46, type: !132, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!217 = !DILocalVariable(name: "a", arg: 1, scope: !216, file: !1, line: 46, type: !15)
!218 = !DILocation(line: 0, scope: !216)
!219 = !DILocalVariable(name: "b", arg: 2, scope: !216, file: !1, line: 46, type: !15)
!220 = !DILocalVariable(name: "tmp", arg: 3, scope: !216, file: !1, line: 46, type: !15)
!221 = !DILocation(line: 46, column: 20, scope: !216)
!222 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !1, file: !1, line: 56, type: !132, scopeLine: 56, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!223 = !DILocalVariable(name: "a", arg: 1, scope: !222, file: !1, line: 56, type: !15)
!224 = !DILocation(line: 0, scope: !222)
!225 = !DILocalVariable(name: "b", arg: 2, scope: !222, file: !1, line: 56, type: !15)
!226 = !DILocalVariable(name: "aVariable", arg: 3, scope: !222, file: !1, line: 56, type: !15)
!227 = !DILocation(line: 56, column: 29, scope: !222)
!228 = !DILocation(line: 56, column: 26, scope: !222)
!229 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 64, type: !132, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!230 = !DILocalVariable(name: "a", arg: 1, scope: !229, file: !1, line: 64, type: !15)
!231 = !DILocation(line: 0, scope: !229)
!232 = !DILocalVariable(name: "b", arg: 2, scope: !229, file: !1, line: 64, type: !15)
!233 = !DILocalVariable(name: "aVariable", arg: 3, scope: !229, file: !1, line: 64, type: !15)
!234 = !DILocation(line: 64, column: 26, scope: !229)
!235 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 71, type: !132, scopeLine: 71, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!236 = !DILocalVariable(name: "a", arg: 1, scope: !235, file: !1, line: 71, type: !15)
!237 = !DILocation(line: 0, scope: !235)
!238 = !DILocalVariable(name: "b", arg: 2, scope: !235, file: !1, line: 71, type: !15)
!239 = !DILocalVariable(name: "aVariable", arg: 3, scope: !235, file: !1, line: 71, type: !15)
!240 = !DILocation(line: 71, column: 21, scope: !235)
!241 = !DILocation(line: 72, column: 29, scope: !235)
!242 = !DILocation(line: 71, column: 12, scope: !235)
!243 = !{!"pallas.result"}
!244 = !{!"pallas.imply"}
