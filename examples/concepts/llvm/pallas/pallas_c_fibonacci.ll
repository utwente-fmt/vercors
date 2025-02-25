; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_fibonacci.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibRec(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !21, metadata !DIExpression()), !dbg !22
  %2 = icmp eq i32 %0, 0, !dbg !23
  br i1 %2, label %3, label %4, !dbg !25

3:                                                ; preds = %1
  br label %13, !dbg !26

4:                                                ; preds = %1
  %5 = icmp eq i32 %0, 1, !dbg !28
  br i1 %5, label %6, label %7, !dbg !30

6:                                                ; preds = %4
  br label %13, !dbg !31

7:                                                ; preds = %4
  %8 = sub nsw i32 %0, 1, !dbg !33
  %9 = call i32 @fibRec(i32 noundef %8), !dbg !35
  %10 = sub nsw i32 %0, 2, !dbg !36
  %11 = call i32 @fibRec(i32 noundef %10), !dbg !37
  %12 = add nsw i32 %9, %11, !dbg !38
  br label %13, !dbg !39

13:                                               ; preds = %7, %6, %3
  %.0 = phi i32 [ 0, %3 ], [ 1, %6 ], [ %12, %7 ], !dbg !40
  ret i32 %.0, !dbg !41
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibIt(i32 noundef %0) #0 !dbg !42 !pallas.fcontract !43 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !47, metadata !DIExpression()), !dbg !50
  %2 = icmp eq i32 %0, 0, !dbg !51
  br i1 %2, label %3, label %4, !dbg !53

3:                                                ; preds = %1
  br label %16, !dbg !54

4:                                                ; preds = %1
  %5 = icmp eq i32 %0, 1, !dbg !56
  br i1 %5, label %6, label %7, !dbg !58

6:                                                ; preds = %4
  br label %16, !dbg !59

7:                                                ; preds = %4
  br label %8

8:                                                ; preds = %7
  call void @llvm.dbg.value(metadata i32 0, metadata !61, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.value(metadata i32 1, metadata !62, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.value(metadata i32 2, metadata !63, metadata !DIExpression()), !dbg !65
  br label %9, !dbg !66

9:                                                ; preds = %13, %8
  %.03 = phi i32 [ 1, %8 ], [ %12, %13 ], !dbg !50
  %.02 = phi i32 [ 0, %8 ], [ %.03, %13 ], !dbg !50
  %.01 = phi i32 [ 2, %8 ], [ %14, %13 ], !dbg !67
  call void @llvm.dbg.value(metadata i32 %.01, metadata !63, metadata !DIExpression()), !dbg !65
  call void @llvm.dbg.value(metadata i32 %.02, metadata !61, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.value(metadata i32 %.03, metadata !62, metadata !DIExpression()), !dbg !50
  %10 = icmp sle i32 %.01, %0, !dbg !68
  br i1 %10, label %11, label %15, !dbg !70

11:                                               ; preds = %9
  %12 = add nsw i32 %.02, %.03, !dbg !71
  call void @llvm.dbg.value(metadata i32 %12, metadata !73, metadata !DIExpression()), !dbg !74
  call void @llvm.dbg.value(metadata i32 %.03, metadata !61, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.value(metadata i32 %12, metadata !62, metadata !DIExpression()), !dbg !50
  br label %13, !dbg !75

13:                                               ; preds = %11
  %14 = add nsw i32 %.01, 1, !dbg !76
  call void @llvm.dbg.value(metadata i32 %14, metadata !63, metadata !DIExpression()), !dbg !65
  br label %9, !dbg !77, !llvm.loop !78

15:                                               ; preds = %9
  br label %16, !dbg !89

16:                                               ; preds = %15, %6, %3
  %.0 = phi i32 [ 0, %3 ], [ 1, %6 ], [ %.03, %15 ], !dbg !50
  ret i32 %.0, !dbg !90
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0) #0 !dbg !91 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !96, metadata !DIExpression()), !dbg !97
  %2 = call i32 @pallas.result.0(), !dbg !98
  %3 = call i32 @fibRec(i32 noundef %0), !dbg !99
  %4 = icmp eq i32 %2, %3, !dbg !100
  ret i1 %4, !dbg !97
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !101 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !102, metadata !DIExpression()), !dbg !103
  %2 = icmp sge i32 %0, 0, !dbg !104
  ret i1 %2, !dbg !103
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !105 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !106, metadata !DIExpression()), !dbg !107
  %2 = icmp sge i32 %0, 0, !dbg !108
  ret i1 %2, !dbg !107
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !109 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !112, metadata !DIExpression()), !dbg !113
  call void @llvm.dbg.value(metadata i32 %1, metadata !114, metadata !DIExpression()), !dbg !113
  call void @llvm.dbg.value(metadata i32 %2, metadata !115, metadata !DIExpression()), !dbg !113
  call void @llvm.dbg.value(metadata i32 %3, metadata !116, metadata !DIExpression()), !dbg !113
  %5 = icmp sle i32 2, %3, !dbg !117
  %6 = add nsw i32 %0, 1, !dbg !118
  %7 = icmp sle i32 %3, %6, !dbg !119
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !120
  ret i1 %8, !dbg !113
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !121 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !122, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %1, metadata !124, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %2, metadata !125, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %3, metadata !126, metadata !DIExpression()), !dbg !123
  %5 = sub nsw i32 %3, 1, !dbg !127
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !128
  %7 = icmp eq i32 %2, %6, !dbg !129
  ret i1 %7, !dbg !123
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !130 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !131, metadata !DIExpression()), !dbg !132
  call void @llvm.dbg.value(metadata i32 %1, metadata !133, metadata !DIExpression()), !dbg !132
  call void @llvm.dbg.value(metadata i32 %2, metadata !134, metadata !DIExpression()), !dbg !132
  call void @llvm.dbg.value(metadata i32 %3, metadata !135, metadata !DIExpression()), !dbg !132
  %5 = sub nsw i32 %3, 2, !dbg !136
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !137
  %7 = icmp eq i32 %1, %6, !dbg !138
  ret i1 %7, !dbg !132
}

declare !pallas.specLib !139 i32 @pallas.result.0()

declare !pallas.specLib !140 i1 @pallas.scAnd(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_fibonacci.c", directory: ".", checksumkind: CSK_MD5, checksum: "66d6beef95c73c9cc2e6bdfa196e7ed3")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "aa619c5f4f705d14d88bd20925b86f7f")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "fibRec", scope: !1, file: !1, line: 16, type: !13, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 true, !19}
!18 = !{!"pallas.srcLoc", i64 12, i64 1, i64 15, i64 1}
!19 = !{!"pallas.requires", !20, ptr @PALLAS_SPEC_0, !21}
!20 = !{!"pallas.srcLoc", i64 14, i64 1, i64 14, i64 16}
!21 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 16, type: !15)
!22 = !DILocation(line: 0, scope: !12)
!23 = !DILocation(line: 17, column: 11, scope: !24)
!24 = distinct !DILexicalBlock(scope: !12, file: !1, line: 17, column: 9)
!25 = !DILocation(line: 17, column: 9, scope: !12)
!26 = !DILocation(line: 18, column: 9, scope: !27)
!27 = distinct !DILexicalBlock(scope: !24, file: !1, line: 17, column: 17)
!28 = !DILocation(line: 19, column: 18, scope: !29)
!29 = distinct !DILexicalBlock(scope: !24, file: !1, line: 19, column: 16)
!30 = !DILocation(line: 19, column: 16, scope: !24)
!31 = !DILocation(line: 20, column: 9, scope: !32)
!32 = distinct !DILexicalBlock(scope: !29, file: !1, line: 19, column: 24)
!33 = !DILocation(line: 22, column: 25, scope: !34)
!34 = distinct !DILexicalBlock(scope: !29, file: !1, line: 21, column: 12)
!35 = !DILocation(line: 22, column: 16, scope: !34)
!36 = !DILocation(line: 22, column: 41, scope: !34)
!37 = !DILocation(line: 22, column: 32, scope: !34)
!38 = !DILocation(line: 22, column: 30, scope: !34)
!39 = !DILocation(line: 22, column: 9, scope: !34)
!40 = !DILocation(line: 0, scope: !24)
!41 = !DILocation(line: 24, column: 1, scope: !12)
!42 = distinct !DISubprogram(name: "fibIt", scope: !1, file: !1, line: 31, type: !13, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!43 = !{!44, i1 false, !45, !48}
!44 = !{!"pallas.srcLoc", i64 27, i64 1, i64 30, i64 1}
!45 = !{!"pallas.requires", !46, ptr @PALLAS_SPEC_1, !47}
!46 = !{!"pallas.srcLoc", i64 28, i64 1, i64 28, i64 16}
!47 = !DILocalVariable(name: "n", arg: 1, scope: !42, file: !1, line: 31, type: !15)
!48 = !{!"pallas.ensures", !49, ptr @PALLAS_SPEC_2, !47}
!49 = !{!"pallas.srcLoc", i64 29, i64 1, i64 29, i64 35}
!50 = !DILocation(line: 0, scope: !42)
!51 = !DILocation(line: 32, column: 12, scope: !52)
!52 = distinct !DILexicalBlock(scope: !42, file: !1, line: 32, column: 10)
!53 = !DILocation(line: 32, column: 10, scope: !42)
!54 = !DILocation(line: 33, column: 9, scope: !55)
!55 = distinct !DILexicalBlock(scope: !52, file: !1, line: 32, column: 18)
!56 = !DILocation(line: 34, column: 20, scope: !57)
!57 = distinct !DILexicalBlock(scope: !52, file: !1, line: 34, column: 18)
!58 = !DILocation(line: 34, column: 18, scope: !52)
!59 = !DILocation(line: 35, column: 9, scope: !60)
!60 = distinct !DILexicalBlock(scope: !57, file: !1, line: 34, column: 26)
!61 = !DILocalVariable(name: "prevRes", scope: !42, file: !1, line: 38, type: !15)
!62 = !DILocalVariable(name: "res", scope: !42, file: !1, line: 39, type: !15)
!63 = !DILocalVariable(name: "i", scope: !64, file: !1, line: 46, type: !15)
!64 = distinct !DILexicalBlock(scope: !42, file: !1, line: 46, column: 5)
!65 = !DILocation(line: 0, scope: !64)
!66 = !DILocation(line: 46, column: 10, scope: !64)
!67 = !DILocation(line: 46, scope: !64)
!68 = !DILocation(line: 46, column: 23, scope: !69)
!69 = distinct !DILexicalBlock(scope: !64, file: !1, line: 46, column: 5)
!70 = !DILocation(line: 46, column: 5, scope: !64)
!71 = !DILocation(line: 47, column: 27, scope: !72)
!72 = distinct !DILexicalBlock(scope: !69, file: !1, line: 46, column: 34)
!73 = !DILocalVariable(name: "tmp", scope: !72, file: !1, line: 47, type: !15)
!74 = !DILocation(line: 0, scope: !72)
!75 = !DILocation(line: 50, column: 5, scope: !72)
!76 = !DILocation(line: 46, column: 30, scope: !69)
!77 = !DILocation(line: 46, column: 5, scope: !69)
!78 = distinct !{!78, !70, !79, !80, !81}
!79 = !DILocation(line: 50, column: 5, scope: !64)
!80 = !{!"llvm.loop.mustprogress"}
!81 = !{!"pallas.loopInv", !82, !83, !85, !87}
!82 = !{!"pallas.srcLoc", i64 41, i64 5, i64 45, i64 5}
!83 = !{!84, ptr @PALLAS_SPEC_3, !47, !61, !62, !63}
!84 = !{!"pallas.srcLoc", i64 42, i64 5, i64 42, i64 42}
!85 = !{!86, ptr @PALLAS_SPEC_4, !47, !61, !62, !63}
!86 = !{!"pallas.srcLoc", i64 43, i64 5, i64 43, i64 38}
!87 = !{!88, ptr @PALLAS_SPEC_5, !47, !61, !62, !63}
!88 = !{!"pallas.srcLoc", i64 44, i64 5, i64 44, i64 42}
!89 = !DILocation(line: 51, column: 5, scope: !42)
!90 = !DILocation(line: 52, column: 1, scope: !42)
!91 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 29, type: !92, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!92 = !DISubroutineType(types: !93)
!93 = !{!94, !15}
!94 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!95 = !{!""}
!96 = !DILocalVariable(name: "n", arg: 1, scope: !91, file: !1, line: 29, type: !15)
!97 = !DILocation(line: 0, scope: !91)
!98 = !DILocation(line: 29, column: 9, scope: !91)
!99 = !DILocation(line: 29, column: 26, scope: !91)
!100 = !DILocation(line: 29, column: 23, scope: !91)
!101 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 14, type: !92, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!102 = !DILocalVariable(name: "n", arg: 1, scope: !101, file: !1, line: 14, type: !15)
!103 = !DILocation(line: 0, scope: !101)
!104 = !DILocation(line: 14, column: 12, scope: !101)
!105 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 28, type: !92, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!106 = !DILocalVariable(name: "n", arg: 1, scope: !105, file: !1, line: 28, type: !15)
!107 = !DILocation(line: 0, scope: !105)
!108 = !DILocation(line: 28, column: 12, scope: !105)
!109 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 42, type: !110, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!110 = !DISubroutineType(types: !111)
!111 = !{!94, !15, !15, !15, !15}
!112 = !DILocalVariable(name: "n", arg: 1, scope: !109, file: !1, line: 42, type: !15)
!113 = !DILocation(line: 0, scope: !109)
!114 = !DILocalVariable(name: "prevRes", arg: 2, scope: !109, file: !1, line: 42, type: !15)
!115 = !DILocalVariable(name: "res", arg: 3, scope: !109, file: !1, line: 42, type: !15)
!116 = !DILocalVariable(name: "i", arg: 4, scope: !109, file: !1, line: 42, type: !15)
!117 = !DILocation(line: 42, column: 27, scope: !109)
!118 = !DILocation(line: 42, column: 39, scope: !109)
!119 = !DILocation(line: 42, column: 35, scope: !109)
!120 = !DILocation(line: 42, column: 20, scope: !109)
!121 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 43, type: !110, scopeLine: 43, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!122 = !DILocalVariable(name: "n", arg: 1, scope: !121, file: !1, line: 43, type: !15)
!123 = !DILocation(line: 0, scope: !121)
!124 = !DILocalVariable(name: "prevRes", arg: 2, scope: !121, file: !1, line: 43, type: !15)
!125 = !DILocalVariable(name: "res", arg: 3, scope: !121, file: !1, line: 43, type: !15)
!126 = !DILocalVariable(name: "i", arg: 4, scope: !121, file: !1, line: 43, type: !15)
!127 = !DILocation(line: 43, column: 35, scope: !121)
!128 = !DILocation(line: 43, column: 27, scope: !121)
!129 = !DILocation(line: 43, column: 24, scope: !121)
!130 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 44, type: !110, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!131 = !DILocalVariable(name: "n", arg: 1, scope: !130, file: !1, line: 44, type: !15)
!132 = !DILocation(line: 0, scope: !130)
!133 = !DILocalVariable(name: "prevRes", arg: 2, scope: !130, file: !1, line: 44, type: !15)
!134 = !DILocalVariable(name: "res", arg: 3, scope: !130, file: !1, line: 44, type: !15)
!135 = !DILocalVariable(name: "i", arg: 4, scope: !130, file: !1, line: 44, type: !15)
!136 = !DILocation(line: 44, column: 39, scope: !130)
!137 = !DILocation(line: 44, column: 31, scope: !130)
!138 = !DILocation(line: 44, column: 28, scope: !130)
!139 = !{!"pallas.result"}
!140 = !{!"pallas.scAnd"}
