; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_assert.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [13 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bar(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !21, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.value(metadata i32 %1, metadata !22, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.value(metadata i32 %2, metadata !23, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.value(metadata i32 0, metadata !27, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.value(metadata i32 0, metadata !28, metadata !DIExpression()), !dbg !30
  br label %4, !dbg !31

4:                                                ; preds = %8, %3
  %.01 = phi i32 [ 0, %3 ], [ %7, %8 ], !dbg !26
  %.0 = phi i32 [ 0, %3 ], [ %9, %8 ], !dbg !32
  call void @llvm.dbg.value(metadata i32 %.0, metadata !28, metadata !DIExpression()), !dbg !30
  call void @llvm.dbg.value(metadata i32 %.01, metadata !27, metadata !DIExpression()), !dbg !26
  %5 = icmp slt i32 %.0, %1, !dbg !33
  br i1 %5, label %6, label %10, !dbg !35

6:                                                ; preds = %4
  %7 = add nsw i32 %.01, %0, !dbg !36
  call void @llvm.dbg.value(metadata i32 %7, metadata !27, metadata !DIExpression()), !dbg !26
  br label %8, !dbg !38

8:                                                ; preds = %6
  %9 = add nsw i32 %.0, 1, !dbg !39
  call void @llvm.dbg.value(metadata i32 %9, metadata !28, metadata !DIExpression()), !dbg !30
  br label %4, !dbg !40, !llvm.loop !41

10:                                               ; preds = %4
  %11 = add nsw i32 %.01, %2, !dbg !50, !pallas.stmntBlock !51
  call void @llvm.dbg.value(metadata i32 %11, metadata !55, metadata !DIExpression()), !dbg !26
  ret i32 %11, !dbg !56, !pallas.stmntBlock !57
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !61 !pallas.fcontract !64 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !68, metadata !DIExpression()), !dbg !72
  call void @llvm.dbg.value(metadata i32 %1, metadata !69, metadata !DIExpression()), !dbg !72
  call void @llvm.dbg.value(metadata i32 %0, metadata !73, metadata !DIExpression()), !dbg !72
  %3 = icmp sgt i32 %0, %1, !dbg !74, !pallas.stmntBlock !76
  br i1 %3, label %4, label %6, !dbg !80

4:                                                ; preds = %2
  %5 = add nsw i32 %0, 1, !dbg !81
  call void @llvm.dbg.value(metadata i32 %5, metadata !68, metadata !DIExpression()), !dbg !72
  br label %8, !dbg !83

6:                                                ; preds = %2
  %7 = add nsw i32 %1, 1, !dbg !84
  call void @llvm.dbg.value(metadata i32 %7, metadata !69, metadata !DIExpression()), !dbg !72
  br label %8

8:                                                ; preds = %6, %4
  %.01 = phi i32 [ %1, %4 ], [ %7, %6 ]
  %.0 = phi i32 [ %5, %4 ], [ %0, %6 ]
  call void @llvm.dbg.value(metadata i32 %.0, metadata !68, metadata !DIExpression()), !dbg !72
  call void @llvm.dbg.value(metadata i32 %.01, metadata !69, metadata !DIExpression()), !dbg !72
  %9 = add nsw i32 %0, %.01, !dbg !86, !pallas.stmntBlock !87
  call void @llvm.dbg.value(metadata i32 %9, metadata !73, metadata !DIExpression()), !dbg !72
  %10 = add nsw i32 %.0, %.01, !dbg !91
  ret i32 %10, !dbg !92
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @amazingFunctionWithSomeBranches(i32 noundef %0, i32 noundef %1) #0 !dbg !93 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !94, metadata !DIExpression()), !dbg !95
  call void @llvm.dbg.value(metadata i32 %1, metadata !96, metadata !DIExpression()), !dbg !95
  call void @llvm.dbg.value(metadata i32 %0, metadata !97, metadata !DIExpression()), !dbg !95
  %3 = icmp slt i32 %0, 0, !dbg !98
  br i1 %3, label %4, label %13, !dbg !100

4:                                                ; preds = %2
  %5 = mul nsw i32 %0, -1, !dbg !101
  call void @llvm.dbg.value(metadata i32 %5, metadata !97, metadata !DIExpression()), !dbg !95
  %6 = icmp slt i32 %1, 0, !dbg !103, !pallas.stmntBlock !105
  br i1 %6, label %7, label %10, !dbg !109

7:                                                ; preds = %4
  %8 = sub nsw i32 0, %1, !dbg !110
  %9 = mul nsw i32 %5, %8, !dbg !112
  call void @llvm.dbg.value(metadata i32 %9, metadata !97, metadata !DIExpression()), !dbg !95
  br label %12, !dbg !113

10:                                               ; preds = %4
  %11 = mul nsw i32 %5, %1, !dbg !114
  call void @llvm.dbg.value(metadata i32 %11, metadata !97, metadata !DIExpression()), !dbg !95
  br label %12

12:                                               ; preds = %10, %7
  %.0 = phi i32 [ %9, %7 ], [ %11, %10 ], !dbg !116
  call void @llvm.dbg.value(metadata i32 %.0, metadata !97, metadata !DIExpression()), !dbg !95
  br label %15, !dbg !117, !pallas.stmntBlock !118

13:                                               ; preds = %2
  %14 = mul nsw i32 %0, %1, !dbg !122
  call void @llvm.dbg.value(metadata i32 %14, metadata !97, metadata !DIExpression()), !dbg !95
  br label %15

15:                                               ; preds = %13, %12
  %.1 = phi i32 [ %.0, %12 ], [ %14, %13 ], !dbg !124
  call void @llvm.dbg.value(metadata i32 %.1, metadata !97, metadata !DIExpression()), !dbg !95
  ret i32 %.1, !dbg !125, !pallas.stmntBlock !126
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !130 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !135, metadata !DIExpression()), !dbg !136
  call void @llvm.dbg.value(metadata i32 %1, metadata !137, metadata !DIExpression()), !dbg !136
  call void @llvm.dbg.value(metadata i32 %2, metadata !138, metadata !DIExpression()), !dbg !136
  %4 = icmp sgt i32 %0, 0, !dbg !139
  br i1 %4, label %5, label %9, !dbg !140

5:                                                ; preds = %3
  %6 = icmp sgt i32 %1, 0, !dbg !141
  br i1 %6, label %7, label %9, !dbg !142

7:                                                ; preds = %5
  %8 = icmp sgt i32 %2, 0, !dbg !143
  br label %9

9:                                                ; preds = %7, %5, %3
  %10 = phi i1 [ false, %5 ], [ false, %3 ], [ %8, %7 ], !dbg !136
  ret i1 %10, !dbg !136
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !144 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !145, metadata !DIExpression()), !dbg !146
  call void @llvm.dbg.value(metadata i32 %1, metadata !147, metadata !DIExpression()), !dbg !146
  call void @llvm.dbg.value(metadata i32 %2, metadata !148, metadata !DIExpression()), !dbg !146
  %4 = call i32 @pallas.result.0(), !dbg !149
  %5 = mul nsw i32 %0, %1, !dbg !150
  %6 = add nsw i32 %5, %2, !dbg !151
  %7 = icmp sge i32 %4, %6, !dbg !152
  ret i1 %7, !dbg !146
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1) #0 !dbg !153 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !156, metadata !DIExpression()), !dbg !157
  call void @llvm.dbg.value(metadata i32 %1, metadata !158, metadata !DIExpression()), !dbg !157
  %3 = icmp sgt i32 %0, 0, !dbg !159
  br i1 %3, label %4, label %6, !dbg !160

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !161
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !157
  ret i1 %7, !dbg !157
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1) #0 !dbg !162 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !163, metadata !DIExpression()), !dbg !164
  call void @llvm.dbg.value(metadata i32 %1, metadata !165, metadata !DIExpression()), !dbg !164
  %3 = call i32 @pallas.result.0(), !dbg !166
  %4 = icmp sgt i32 %3, 0, !dbg !167
  ret i1 %4, !dbg !164
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !168 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !171, metadata !DIExpression()), !dbg !172
  call void @llvm.dbg.value(metadata i32 %1, metadata !173, metadata !DIExpression()), !dbg !172
  call void @llvm.dbg.value(metadata i32 %2, metadata !174, metadata !DIExpression()), !dbg !172
  call void @llvm.dbg.value(metadata i32 %3, metadata !175, metadata !DIExpression()), !dbg !172
  call void @llvm.dbg.value(metadata i32 %4, metadata !176, metadata !DIExpression()), !dbg !172
  %6 = icmp sle i32 0, %4, !dbg !177
  br i1 %6, label %7, label %9, !dbg !178

7:                                                ; preds = %5
  %8 = icmp sle i32 %4, %1, !dbg !179
  br label %9

9:                                                ; preds = %7, %5
  %10 = phi i1 [ false, %5 ], [ %8, %7 ], !dbg !172
  ret i1 %10, !dbg !172
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !180 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !181, metadata !DIExpression()), !dbg !182
  call void @llvm.dbg.value(metadata i32 %1, metadata !183, metadata !DIExpression()), !dbg !182
  call void @llvm.dbg.value(metadata i32 %2, metadata !184, metadata !DIExpression()), !dbg !182
  call void @llvm.dbg.value(metadata i32 %3, metadata !185, metadata !DIExpression()), !dbg !182
  call void @llvm.dbg.value(metadata i32 %4, metadata !186, metadata !DIExpression()), !dbg !182
  %6 = mul nsw i32 %4, %0, !dbg !187
  %7 = icmp eq i32 %3, %6, !dbg !188
  ret i1 %7, !dbg !182
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4) #0 !dbg !189 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !190, metadata !DIExpression()), !dbg !191
  call void @llvm.dbg.value(metadata i32 %1, metadata !192, metadata !DIExpression()), !dbg !191
  call void @llvm.dbg.value(metadata i32 %2, metadata !193, metadata !DIExpression()), !dbg !191
  call void @llvm.dbg.value(metadata i32 %3, metadata !194, metadata !DIExpression()), !dbg !191
  call void @llvm.dbg.value(metadata i32 %4, metadata !195, metadata !DIExpression()), !dbg !191
  %6 = mul nsw i32 %0, %1, !dbg !196
  %7 = icmp eq i32 %3, %6, !dbg !197
  ret i1 %7, !dbg !191
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 !dbg !198 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !201, metadata !DIExpression()), !dbg !202
  call void @llvm.dbg.value(metadata i32 %1, metadata !203, metadata !DIExpression()), !dbg !202
  call void @llvm.dbg.value(metadata i32 %2, metadata !204, metadata !DIExpression()), !dbg !202
  call void @llvm.dbg.value(metadata i32 %3, metadata !205, metadata !DIExpression()), !dbg !202
  call void @llvm.dbg.value(metadata i32 %4, metadata !206, metadata !DIExpression()), !dbg !202
  call void @llvm.dbg.value(metadata i32 %5, metadata !207, metadata !DIExpression()), !dbg !202
  %7 = mul nsw i32 %0, %1, !dbg !208
  %8 = add nsw i32 %7, %2, !dbg !209
  call void @llvm.dbg.value(metadata i32 %8, metadata !207, metadata !DIExpression()), !dbg !202
  %9 = icmp ne i32 %8, 0, !dbg !210
  ret i1 %9, !dbg !202
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !211 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !212, metadata !DIExpression()), !dbg !213
  call void @llvm.dbg.value(metadata i32 %1, metadata !214, metadata !DIExpression()), !dbg !213
  call void @llvm.dbg.value(metadata i32 %2, metadata !215, metadata !DIExpression()), !dbg !213
  %4 = icmp sgt i32 %2, 0, !dbg !216
  ret i1 %4, !dbg !213
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !217 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !218, metadata !DIExpression()), !dbg !219
  call void @llvm.dbg.value(metadata i32 %1, metadata !220, metadata !DIExpression()), !dbg !219
  call void @llvm.dbg.value(metadata i32 %2, metadata !221, metadata !DIExpression()), !dbg !219
  %4 = icmp sle i32 %2, %0, !dbg !222
  ret i1 %4, !dbg !219
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !223 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !224, metadata !DIExpression()), !dbg !225
  call void @llvm.dbg.value(metadata i32 %1, metadata !226, metadata !DIExpression()), !dbg !225
  call void @llvm.dbg.value(metadata i32 %2, metadata !227, metadata !DIExpression()), !dbg !225
  %4 = sub nsw i32 0, %0, !dbg !228
  %5 = icmp eq i32 %2, %4, !dbg !229
  ret i1 %5, !dbg !225
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !230 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !231, metadata !DIExpression()), !dbg !232
  call void @llvm.dbg.value(metadata i32 %1, metadata !233, metadata !DIExpression()), !dbg !232
  call void @llvm.dbg.value(metadata i32 %2, metadata !234, metadata !DIExpression()), !dbg !232
  %4 = icmp sge i32 %2, 0, !dbg !235
  ret i1 %4, !dbg !232
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !236 !pallas.exprWrapper !134 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !237, metadata !DIExpression()), !dbg !238
  call void @llvm.dbg.value(metadata i32 %1, metadata !239, metadata !DIExpression()), !dbg !238
  call void @llvm.dbg.value(metadata i32 %2, metadata !240, metadata !DIExpression()), !dbg !238
  %4 = icmp slt i32 %0, 0, !dbg !241
  %5 = icmp sge i32 %2, 0, !dbg !242
  %6 = call i1 @pallas.imply(i1 %4, i1 %5), !dbg !243
  ret i1 %6, !dbg !238
}

declare !pallas.specLib !244 i32 @pallas.result.0()

declare !pallas.specLib !245 i1 @pallas.imply(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_assert.c", directory: ".", checksumkind: CSK_MD5, checksum: "75228d18208f31bd8d07e4ab62411fd6")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "a71f53b59d6d7a335111517fc46721cb")
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
!17 = !{!18, i1 false, !19, !24}
!18 = !{!"pallas.srcLoc", i64 10, i64 1, i64 13, i64 1}
!19 = !{!"pallas.requires", !20, ptr @PALLAS_SPEC_0, !21, !22, !23}
!20 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 33}
!21 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 14, type: !15)
!22 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 14, type: !15)
!23 = !DILocalVariable(name: "c", arg: 3, scope: !12, file: !1, line: 14, type: !15)
!24 = !{!"pallas.ensures", !25, ptr @PALLAS_SPEC_1, !21, !22, !23}
!25 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 36}
!26 = !DILocation(line: 0, scope: !12)
!27 = !DILocalVariable(name: "tmp", scope: !12, file: !1, line: 15, type: !15)
!28 = !DILocalVariable(name: "i", scope: !29, file: !1, line: 20, type: !15)
!29 = distinct !DILexicalBlock(scope: !12, file: !1, line: 20, column: 5)
!30 = !DILocation(line: 0, scope: !29)
!31 = !DILocation(line: 20, column: 10, scope: !29)
!32 = !DILocation(line: 20, scope: !29)
!33 = !DILocation(line: 20, column: 23, scope: !34)
!34 = distinct !DILexicalBlock(scope: !29, file: !1, line: 20, column: 5)
!35 = !DILocation(line: 20, column: 5, scope: !29)
!36 = !DILocation(line: 21, column: 13, scope: !37)
!37 = distinct !DILexicalBlock(scope: !34, file: !1, line: 20, column: 33)
!38 = !DILocation(line: 22, column: 5, scope: !37)
!39 = !DILocation(line: 20, column: 28, scope: !34)
!40 = !DILocation(line: 20, column: 5, scope: !34)
!41 = distinct !{!41, !35, !42, !43, !44}
!42 = !DILocation(line: 22, column: 5, scope: !29)
!43 = !{!"llvm.loop.mustprogress"}
!44 = !{!"pallas.loopInv", !45, !46, !48}
!45 = !{!"pallas.srcLoc", i64 16, i64 5, i64 19, i64 5}
!46 = !{!47, ptr @PALLAS_SPEC_4, !21, !22, !23, !27, !28}
!47 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 36}
!48 = !{!49, ptr @PALLAS_SPEC_5, !21, !22, !23, !27, !28}
!49 = !{!"pallas.srcLoc", i64 18, i64 5, i64 18, i64 32}
!50 = !DILocation(line: 27, column: 20, scope: !12)
!51 = !{!52, !53}
!52 = !{!"pallas.srcLoc", i64 23, i64 5, i64 25, i64 5}
!53 = !{!"pallas.assert", !54, ptr @PALLAS_SPEC_6, !21, !22, !23, !27, !28}
!54 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 24}
!55 = !DILocalVariable(name: "tmp2", scope: !12, file: !1, line: 27, type: !15)
!56 = !DILocation(line: 31, column: 5, scope: !12)
!57 = !{!58, !59}
!58 = !{!"pallas.srcLoc", i64 28, i64 5, i64 30, i64 5}
!59 = !{!"pallas.assert", !60, ptr @PALLAS_SPEC_7, !21, !22, !23, !27, !28, !55}
!60 = !{!"pallas.srcLoc", i64 29, i64 5, i64 29, i64 30}
!61 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 38, type: !62, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!62 = !DISubroutineType(types: !63)
!63 = !{!15, !15, !15}
!64 = !{!65, i1 false, !66, !70}
!65 = !{!"pallas.srcLoc", i64 34, i64 1, i64 37, i64 1}
!66 = !{!"pallas.requires", !67, ptr @PALLAS_SPEC_2, !68, !69}
!67 = !{!"pallas.srcLoc", i64 35, i64 1, i64 35, i64 24}
!68 = !DILocalVariable(name: "a", arg: 1, scope: !61, file: !1, line: 38, type: !15)
!69 = !DILocalVariable(name: "b", arg: 2, scope: !61, file: !1, line: 38, type: !15)
!70 = !{!"pallas.ensures", !71, ptr @PALLAS_SPEC_3, !68, !69}
!71 = !{!"pallas.srcLoc", i64 36, i64 1, i64 36, i64 26}
!72 = !DILocation(line: 0, scope: !61)
!73 = !DILocalVariable(name: "tmp", scope: !61, file: !1, line: 39, type: !15)
!74 = !DILocation(line: 41, column: 11, scope: !75)
!75 = distinct !DILexicalBlock(scope: !61, file: !1, line: 41, column: 9)
!76 = !{!77, !78}
!77 = !{!"pallas.srcLoc", i64 40, i64 5, i64 40, i64 25}
!78 = !{!"pallas.assert", !79, ptr @PALLAS_SPEC_8, !68, !69, !73}
!79 = !{!"pallas.srcLoc", i64 40, i64 9, i64 40, i64 23}
!80 = !DILocation(line: 41, column: 9, scope: !61)
!81 = !DILocation(line: 42, column: 10, scope: !82)
!82 = distinct !DILexicalBlock(scope: !75, file: !1, line: 41, column: 16)
!83 = !DILocation(line: 43, column: 5, scope: !82)
!84 = !DILocation(line: 44, column: 10, scope: !85)
!85 = distinct !DILexicalBlock(scope: !75, file: !1, line: 43, column: 12)
!86 = !DILocation(line: 47, column: 9, scope: !61)
!87 = !{!88, !89}
!88 = !{!"pallas.srcLoc", i64 46, i64 5, i64 46, i64 26}
!89 = !{!"pallas.assert", !90, ptr @PALLAS_SPEC_9, !68, !69, !73}
!90 = !{!"pallas.srcLoc", i64 46, i64 9, i64 46, i64 24}
!91 = !DILocation(line: 48, column: 14, scope: !61)
!92 = !DILocation(line: 48, column: 5, scope: !61)
!93 = distinct !DISubprogram(name: "amazingFunctionWithSomeBranches", scope: !1, file: !1, line: 51, type: !62, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!94 = !DILocalVariable(name: "a", arg: 1, scope: !93, file: !1, line: 51, type: !15)
!95 = !DILocation(line: 0, scope: !93)
!96 = !DILocalVariable(name: "b", arg: 2, scope: !93, file: !1, line: 51, type: !15)
!97 = !DILocalVariable(name: "aVariable", scope: !93, file: !1, line: 52, type: !15)
!98 = !DILocation(line: 53, column: 11, scope: !99)
!99 = distinct !DILexicalBlock(scope: !93, file: !1, line: 53, column: 9)
!100 = !DILocation(line: 53, column: 9, scope: !93)
!101 = !DILocation(line: 54, column: 19, scope: !102)
!102 = distinct !DILexicalBlock(scope: !99, file: !1, line: 53, column: 16)
!103 = !DILocation(line: 58, column: 16, scope: !104)
!104 = distinct !DILexicalBlock(scope: !102, file: !1, line: 58, column: 14)
!105 = !{!106, !107}
!106 = !{!"pallas.srcLoc", i64 55, i64 9, i64 57, i64 9}
!107 = !{!"pallas.assert", !108, ptr @PALLAS_SPEC_10, !94, !96, !97}
!108 = !{!"pallas.srcLoc", i64 56, i64 9, i64 56, i64 31}
!109 = !DILocation(line: 58, column: 14, scope: !102)
!110 = !DILocation(line: 59, column: 26, scope: !111)
!111 = distinct !DILexicalBlock(scope: !104, file: !1, line: 58, column: 21)
!112 = !DILocation(line: 59, column: 23, scope: !111)
!113 = !DILocation(line: 60, column: 9, scope: !111)
!114 = !DILocation(line: 61, column: 23, scope: !115)
!115 = distinct !DILexicalBlock(scope: !104, file: !1, line: 60, column: 16)
!116 = !DILocation(line: 0, scope: !104)
!117 = !DILocation(line: 66, column: 5, scope: !102)
!118 = !{!119, !120}
!119 = !{!"pallas.srcLoc", i64 63, i64 9, i64 65, i64 9}
!120 = !{!"pallas.assert", !121, ptr @PALLAS_SPEC_11, !94, !96, !97}
!121 = !{!"pallas.srcLoc", i64 64, i64 9, i64 64, i64 30}
!122 = !DILocation(line: 67, column: 19, scope: !123)
!123 = distinct !DILexicalBlock(scope: !99, file: !1, line: 66, column: 12)
!124 = !DILocation(line: 0, scope: !99)
!125 = !DILocation(line: 74, column: 5, scope: !93)
!126 = !{!127, !128}
!127 = !{!"pallas.srcLoc", i64 70, i64 5, i64 73, i64 5}
!128 = !{!"pallas.assert", !129, ptr @PALLAS_SPEC_12, !94, !96, !97}
!129 = !{!"pallas.srcLoc", i64 71, i64 5, i64 72, i64 34}
!130 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 11, type: !131, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!131 = !DISubroutineType(types: !132)
!132 = !{!133, !15, !15, !15}
!133 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!134 = !{!""}
!135 = !DILocalVariable(name: "a", arg: 1, scope: !130, file: !1, line: 11, type: !15)
!136 = !DILocation(line: 0, scope: !130)
!137 = !DILocalVariable(name: "b", arg: 2, scope: !130, file: !1, line: 11, type: !15)
!138 = !DILocalVariable(name: "c", arg: 3, scope: !130, file: !1, line: 11, type: !15)
!139 = !DILocation(line: 11, column: 12, scope: !130)
!140 = !DILocation(line: 11, column: 16, scope: !130)
!141 = !DILocation(line: 11, column: 21, scope: !130)
!142 = !DILocation(line: 11, column: 25, scope: !130)
!143 = !DILocation(line: 11, column: 30, scope: !130)
!144 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 12, type: !131, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!145 = !DILocalVariable(name: "a", arg: 1, scope: !144, file: !1, line: 12, type: !15)
!146 = !DILocation(line: 0, scope: !144)
!147 = !DILocalVariable(name: "b", arg: 2, scope: !144, file: !1, line: 12, type: !15)
!148 = !DILocalVariable(name: "c", arg: 3, scope: !144, file: !1, line: 12, type: !15)
!149 = !DILocation(line: 12, column: 9, scope: !144)
!150 = !DILocation(line: 12, column: 28, scope: !144)
!151 = !DILocation(line: 12, column: 33, scope: !144)
!152 = !DILocation(line: 12, column: 22, scope: !144)
!153 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 35, type: !154, scopeLine: 35, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!154 = !DISubroutineType(types: !155)
!155 = !{!133, !15, !15}
!156 = !DILocalVariable(name: "a", arg: 1, scope: !153, file: !1, line: 35, type: !15)
!157 = !DILocation(line: 0, scope: !153)
!158 = !DILocalVariable(name: "b", arg: 2, scope: !153, file: !1, line: 35, type: !15)
!159 = !DILocation(line: 35, column: 12, scope: !153)
!160 = !DILocation(line: 35, column: 16, scope: !153)
!161 = !DILocation(line: 35, column: 21, scope: !153)
!162 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 36, type: !154, scopeLine: 36, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!163 = !DILocalVariable(name: "a", arg: 1, scope: !162, file: !1, line: 36, type: !15)
!164 = !DILocation(line: 0, scope: !162)
!165 = !DILocalVariable(name: "b", arg: 2, scope: !162, file: !1, line: 36, type: !15)
!166 = !DILocation(line: 36, column: 10, scope: !162)
!167 = !DILocation(line: 36, column: 23, scope: !162)
!168 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 17, type: !169, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!169 = !DISubroutineType(types: !170)
!170 = !{!133, !15, !15, !15, !15, !15}
!171 = !DILocalVariable(name: "a", arg: 1, scope: !168, file: !1, line: 17, type: !15)
!172 = !DILocation(line: 0, scope: !168)
!173 = !DILocalVariable(name: "b", arg: 2, scope: !168, file: !1, line: 17, type: !15)
!174 = !DILocalVariable(name: "c", arg: 3, scope: !168, file: !1, line: 17, type: !15)
!175 = !DILocalVariable(name: "tmp", arg: 4, scope: !168, file: !1, line: 17, type: !15)
!176 = !DILocalVariable(name: "i", arg: 5, scope: !168, file: !1, line: 17, type: !15)
!177 = !DILocation(line: 17, column: 22, scope: !168)
!178 = !DILocation(line: 17, column: 27, scope: !168)
!179 = !DILocation(line: 17, column: 32, scope: !168)
!180 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 18, type: !169, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!181 = !DILocalVariable(name: "a", arg: 1, scope: !180, file: !1, line: 18, type: !15)
!182 = !DILocation(line: 0, scope: !180)
!183 = !DILocalVariable(name: "b", arg: 2, scope: !180, file: !1, line: 18, type: !15)
!184 = !DILocalVariable(name: "c", arg: 3, scope: !180, file: !1, line: 18, type: !15)
!185 = !DILocalVariable(name: "tmp", arg: 4, scope: !180, file: !1, line: 18, type: !15)
!186 = !DILocalVariable(name: "i", arg: 5, scope: !180, file: !1, line: 18, type: !15)
!187 = !DILocation(line: 18, column: 29, scope: !180)
!188 = !DILocation(line: 18, column: 24, scope: !180)
!189 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 24, type: !169, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!190 = !DILocalVariable(name: "a", arg: 1, scope: !189, file: !1, line: 24, type: !15)
!191 = !DILocation(line: 0, scope: !189)
!192 = !DILocalVariable(name: "b", arg: 2, scope: !189, file: !1, line: 24, type: !15)
!193 = !DILocalVariable(name: "c", arg: 3, scope: !189, file: !1, line: 24, type: !15)
!194 = !DILocalVariable(name: "tmp", arg: 4, scope: !189, file: !1, line: 24, type: !15)
!195 = !DILocalVariable(name: "i", arg: 5, scope: !189, file: !1, line: 24, type: !15)
!196 = !DILocation(line: 24, column: 21, scope: !189)
!197 = !DILocation(line: 24, column: 16, scope: !189)
!198 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !1, file: !1, line: 29, type: !199, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!199 = !DISubroutineType(types: !200)
!200 = !{!133, !15, !15, !15, !15, !15, !15}
!201 = !DILocalVariable(name: "a", arg: 1, scope: !198, file: !1, line: 29, type: !15)
!202 = !DILocation(line: 0, scope: !198)
!203 = !DILocalVariable(name: "b", arg: 2, scope: !198, file: !1, line: 29, type: !15)
!204 = !DILocalVariable(name: "c", arg: 3, scope: !198, file: !1, line: 29, type: !15)
!205 = !DILocalVariable(name: "tmp", arg: 4, scope: !198, file: !1, line: 29, type: !15)
!206 = !DILocalVariable(name: "i", arg: 5, scope: !198, file: !1, line: 29, type: !15)
!207 = !DILocalVariable(name: "tmp2", arg: 6, scope: !198, file: !1, line: 29, type: !15)
!208 = !DILocation(line: 29, column: 22, scope: !198)
!209 = !DILocation(line: 29, column: 27, scope: !198)
!210 = !DILocation(line: 29, column: 12, scope: !198)
!211 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !1, file: !1, line: 40, type: !131, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!212 = !DILocalVariable(name: "a", arg: 1, scope: !211, file: !1, line: 40, type: !15)
!213 = !DILocation(line: 0, scope: !211)
!214 = !DILocalVariable(name: "b", arg: 2, scope: !211, file: !1, line: 40, type: !15)
!215 = !DILocalVariable(name: "tmp", arg: 3, scope: !211, file: !1, line: 40, type: !15)
!216 = !DILocation(line: 40, column: 20, scope: !211)
!217 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !1, file: !1, line: 46, type: !131, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!218 = !DILocalVariable(name: "a", arg: 1, scope: !217, file: !1, line: 46, type: !15)
!219 = !DILocation(line: 0, scope: !217)
!220 = !DILocalVariable(name: "b", arg: 2, scope: !217, file: !1, line: 46, type: !15)
!221 = !DILocalVariable(name: "tmp", arg: 3, scope: !217, file: !1, line: 46, type: !15)
!222 = !DILocation(line: 46, column: 20, scope: !217)
!223 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !1, file: !1, line: 56, type: !131, scopeLine: 56, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!224 = !DILocalVariable(name: "a", arg: 1, scope: !223, file: !1, line: 56, type: !15)
!225 = !DILocation(line: 0, scope: !223)
!226 = !DILocalVariable(name: "b", arg: 2, scope: !223, file: !1, line: 56, type: !15)
!227 = !DILocalVariable(name: "aVariable", arg: 3, scope: !223, file: !1, line: 56, type: !15)
!228 = !DILocation(line: 56, column: 29, scope: !223)
!229 = !DILocation(line: 56, column: 26, scope: !223)
!230 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 64, type: !131, scopeLine: 64, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!231 = !DILocalVariable(name: "a", arg: 1, scope: !230, file: !1, line: 64, type: !15)
!232 = !DILocation(line: 0, scope: !230)
!233 = !DILocalVariable(name: "b", arg: 2, scope: !230, file: !1, line: 64, type: !15)
!234 = !DILocalVariable(name: "aVariable", arg: 3, scope: !230, file: !1, line: 64, type: !15)
!235 = !DILocation(line: 64, column: 26, scope: !230)
!236 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 71, type: !131, scopeLine: 71, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!237 = !DILocalVariable(name: "a", arg: 1, scope: !236, file: !1, line: 71, type: !15)
!238 = !DILocation(line: 0, scope: !236)
!239 = !DILocalVariable(name: "b", arg: 2, scope: !236, file: !1, line: 71, type: !15)
!240 = !DILocalVariable(name: "aVariable", arg: 3, scope: !236, file: !1, line: 71, type: !15)
!241 = !DILocation(line: 71, column: 21, scope: !236)
!242 = !DILocation(line: 72, column: 29, scope: !236)
!243 = !DILocation(line: 71, column: 12, scope: !236)
!244 = !{!"pallas.result"}
!245 = !{!"pallas.imply"}
