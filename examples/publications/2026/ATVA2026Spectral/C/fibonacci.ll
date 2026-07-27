; ModuleID = 'tmp_spectral/tmp_ir_source0.ll'
source_filename = "examples/publications/2026/ATVA2026Spectral/C/fibonacci.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_5], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibRec(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !24, metadata !DIExpression()), !dbg !30
  %2 = icmp eq i32 %0, 0, !dbg !31
  br i1 %2, label %3, label %4, !dbg !33

3:                                                ; preds = %1
  br label %13, !dbg !34

4:                                                ; preds = %1
  %5 = icmp eq i32 %0, 1, !dbg !36
  br i1 %5, label %6, label %7, !dbg !38

6:                                                ; preds = %4
  br label %13, !dbg !39

7:                                                ; preds = %4
  %8 = sub nsw i32 %0, 1, !dbg !41
  %9 = call i32 @fibRec(i32 noundef %8), !dbg !43
  %10 = sub nsw i32 %0, 2, !dbg !44
  %11 = call i32 @fibRec(i32 noundef %10), !dbg !45
  %12 = add nsw i32 %9, %11, !dbg !46
  br label %13, !dbg !47

13:                                               ; preds = %7, %6, %3
  %.0 = phi i32 [ 0, %3 ], [ 1, %6 ], [ %12, %7 ], !dbg !48
  ret i32 %.0, !dbg !49
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibIt(i32 noundef %0) #0 !dbg !50 !pallas.fcontract !51 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !57, metadata !DIExpression()), !dbg !66
  %2 = icmp eq i32 %0, 0, !dbg !67
  br i1 %2, label %3, label %4, !dbg !69

3:                                                ; preds = %1
  br label %16, !dbg !70

4:                                                ; preds = %1
  %5 = icmp eq i32 %0, 1, !dbg !72
  br i1 %5, label %6, label %7, !dbg !74

6:                                                ; preds = %4
  br label %16, !dbg !75

7:                                                ; preds = %4
  br label %8

8:                                                ; preds = %7
  call void @llvm.dbg.value(metadata i32 0, metadata !77, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.value(metadata i32 1, metadata !78, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.value(metadata i32 2, metadata !79, metadata !DIExpression()), !dbg !81
  br label %9, !dbg !82

9:                                                ; preds = %13, %8
  %.03 = phi i32 [ 1, %8 ], [ %12, %13 ], !dbg !66
  %.02 = phi i32 [ 0, %8 ], [ %.03, %13 ], !dbg !66
  %.01 = phi i32 [ 2, %8 ], [ %14, %13 ], !dbg !83
  call void @llvm.dbg.value(metadata i32 %.01, metadata !79, metadata !DIExpression()), !dbg !81
  call void @llvm.dbg.value(metadata i32 %.02, metadata !77, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.value(metadata i32 %.03, metadata !78, metadata !DIExpression()), !dbg !66
  %10 = icmp sle i32 %.01, %0, !dbg !84
  br i1 %10, label %11, label %15, !dbg !86

11:                                               ; preds = %9
  %12 = add nsw i32 %.02, %.03, !dbg !87
  call void @llvm.dbg.value(metadata i32 %12, metadata !89, metadata !DIExpression()), !dbg !90
  call void @llvm.dbg.value(metadata i32 %.03, metadata !77, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.value(metadata i32 %12, metadata !78, metadata !DIExpression()), !dbg !66
  br label %13, !dbg !91

13:                                               ; preds = %11
  %14 = add nsw i32 %.01, 1, !dbg !92
  call void @llvm.dbg.value(metadata i32 %14, metadata !79, metadata !DIExpression()), !dbg !81
  br label %9, !dbg !93, !llvm.loop !94

15:                                               ; preds = %9
  br label %16, !dbg !137

16:                                               ; preds = %15, %6, %3
  %.0 = phi i32 [ 0, %3 ], [ 1, %6 ], [ %.03, %15 ], !dbg !66
  ret i32 %.0, !dbg !138
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !26 !pallas.exprWrapper !139 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !140
  %2 = icmp sge i32 %0, 0, !dbg !141
  ret i1 %2, !dbg !140
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !59 !pallas.exprWrapper !139 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !58, metadata !DIExpression()), !dbg !142
  %2 = icmp sge i32 %0, 0, !dbg !143
  ret i1 %2, !dbg !142
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0) #0 !dbg !65 !pallas.exprWrapper !139 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !64, metadata !DIExpression()), !dbg !144
  %2 = call i32 @"pallas.result i32"(), !dbg !145
  %3 = call i32 @fibRec(i32 noundef %0), !dbg !146
  %4 = icmp eq i32 %2, %3, !dbg !147
  ret i1 %4, !dbg !144
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !118 !pallas.exprWrapper !139 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !117, metadata !DIExpression()), !dbg !148
  call void @llvm.dbg.value(metadata i32 %1, metadata !120, metadata !DIExpression()), !dbg !148
  call void @llvm.dbg.value(metadata i32 %2, metadata !122, metadata !DIExpression()), !dbg !148
  call void @llvm.dbg.value(metadata i32 %3, metadata !124, metadata !DIExpression()), !dbg !148
  %5 = sub nsw i32 %3, 1, !dbg !149
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !150
  %7 = icmp eq i32 %2, %6, !dbg !151
  ret i1 %7, !dbg !148
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !104 !pallas.exprWrapper !139 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !103, metadata !DIExpression()), !dbg !152
  call void @llvm.dbg.value(metadata i32 %1, metadata !108, metadata !DIExpression()), !dbg !152
  call void @llvm.dbg.value(metadata i32 %2, metadata !110, metadata !DIExpression()), !dbg !152
  call void @llvm.dbg.value(metadata i32 %3, metadata !112, metadata !DIExpression()), !dbg !152
  %5 = icmp sle i32 2, %3, !dbg !153
  %6 = add nsw i32 %0, 1, !dbg !154
  %7 = icmp sle i32 %3, %6, !dbg !155
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !156
  ret i1 %8, !dbg !152
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !130 !pallas.exprWrapper !139 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !129, metadata !DIExpression()), !dbg !157
  call void @llvm.dbg.value(metadata i32 %1, metadata !132, metadata !DIExpression()), !dbg !157
  call void @llvm.dbg.value(metadata i32 %2, metadata !134, metadata !DIExpression()), !dbg !157
  call void @llvm.dbg.value(metadata i32 %3, metadata !136, metadata !DIExpression()), !dbg !157
  %5 = sub nsw i32 %3, 2, !dbg !158
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !159
  %7 = icmp eq i32 %1, %6, !dbg !160
  ret i1 %7, !dbg !157
}

declare !pallas.specLib !161 i32 @"pallas.result i32"()

declare !pallas.specLib !162 i1 @pallas.scAnd(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/C/fibonacci.c", directory: ".", checksumkind: CSK_MD5, checksum: "097d7dec55db99918f9d0fb7a3c61855")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp_spectral/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "1e2e110682fe4edc872ca43ff3c5ba7e")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "fibRec", scope: !1, file: !1, line: 13, type: !13, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 true, i1 false, !16, !16, !20}
!18 = !{!"pallas.srcLoc", i64 9, i64 1, i64 12, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/C/fibonacci.c", directory: "", checksumkind: CSK_MD5, checksum: "097d7dec55db99918f9d0fb7a3c61855")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !16, !16, !22}
!21 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 16, !19}
!22 = !{!23}
!23 = !{!24, !25}
!24 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 13, type: !15)
!25 = !DILocalVariable(name: "n", arg: 1, scope: !26, file: !1, line: 11, type: !15)
!26 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 11, type: !27, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!27 = !DISubroutineType(types: !28)
!28 = !{!29, !15}
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !DILocation(line: 0, scope: !12)
!31 = !DILocation(line: 14, column: 11, scope: !32)
!32 = distinct !DILexicalBlock(scope: !12, file: !1, line: 14, column: 9)
!33 = !DILocation(line: 14, column: 9, scope: !12)
!34 = !DILocation(line: 15, column: 9, scope: !35)
!35 = distinct !DILexicalBlock(scope: !32, file: !1, line: 14, column: 17)
!36 = !DILocation(line: 16, column: 18, scope: !37)
!37 = distinct !DILexicalBlock(scope: !32, file: !1, line: 16, column: 16)
!38 = !DILocation(line: 16, column: 16, scope: !32)
!39 = !DILocation(line: 17, column: 9, scope: !40)
!40 = distinct !DILexicalBlock(scope: !37, file: !1, line: 16, column: 24)
!41 = !DILocation(line: 19, column: 25, scope: !42)
!42 = distinct !DILexicalBlock(scope: !37, file: !1, line: 18, column: 12)
!43 = !DILocation(line: 19, column: 16, scope: !42)
!44 = !DILocation(line: 19, column: 41, scope: !42)
!45 = !DILocation(line: 19, column: 32, scope: !42)
!46 = !DILocation(line: 19, column: 30, scope: !42)
!47 = !DILocation(line: 19, column: 9, scope: !42)
!48 = !DILocation(line: 0, scope: !32)
!49 = !DILocation(line: 21, column: 1, scope: !12)
!50 = distinct !DISubprogram(name: "fibIt", scope: !1, file: !1, line: 28, type: !13, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!51 = !{!52, i1 false, i1 false, !16, !16, !53, !60}
!52 = !{!"pallas.srcLoc", i64 24, i64 1, i64 27, i64 1, !19}
!53 = !{!"pallas.requires", !54, ptr @PALLAS_SPEC_1, !16, !16, !55}
!54 = !{!"pallas.srcLoc", i64 25, i64 1, i64 25, i64 16, !19}
!55 = !{!56}
!56 = !{!57, !58}
!57 = !DILocalVariable(name: "n", arg: 1, scope: !50, file: !1, line: 28, type: !15)
!58 = !DILocalVariable(name: "n", arg: 1, scope: !59, file: !1, line: 25, type: !15)
!59 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 25, type: !27, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!60 = !{!"pallas.ensures", !61, ptr @PALLAS_SPEC_2, !16, !16, !62}
!61 = !{!"pallas.srcLoc", i64 26, i64 1, i64 26, i64 34, !19}
!62 = !{!63}
!63 = !{!57, !64}
!64 = !DILocalVariable(name: "n", arg: 1, scope: !65, file: !1, line: 26, type: !15)
!65 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 26, type: !27, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!66 = !DILocation(line: 0, scope: !50)
!67 = !DILocation(line: 29, column: 12, scope: !68)
!68 = distinct !DILexicalBlock(scope: !50, file: !1, line: 29, column: 10)
!69 = !DILocation(line: 29, column: 10, scope: !50)
!70 = !DILocation(line: 30, column: 9, scope: !71)
!71 = distinct !DILexicalBlock(scope: !68, file: !1, line: 29, column: 18)
!72 = !DILocation(line: 31, column: 20, scope: !73)
!73 = distinct !DILexicalBlock(scope: !68, file: !1, line: 31, column: 18)
!74 = !DILocation(line: 31, column: 18, scope: !68)
!75 = !DILocation(line: 32, column: 9, scope: !76)
!76 = distinct !DILexicalBlock(scope: !73, file: !1, line: 31, column: 26)
!77 = !DILocalVariable(name: "prevRes", scope: !50, file: !1, line: 35, type: !15)
!78 = !DILocalVariable(name: "res", scope: !50, file: !1, line: 36, type: !15)
!79 = !DILocalVariable(name: "i", scope: !80, file: !1, line: 43, type: !15)
!80 = distinct !DILexicalBlock(scope: !50, file: !1, line: 43, column: 5)
!81 = !DILocation(line: 0, scope: !80)
!82 = !DILocation(line: 43, column: 10, scope: !80)
!83 = !DILocation(line: 43, scope: !80)
!84 = !DILocation(line: 43, column: 23, scope: !85)
!85 = distinct !DILexicalBlock(scope: !80, file: !1, line: 43, column: 5)
!86 = !DILocation(line: 43, column: 5, scope: !80)
!87 = !DILocation(line: 44, column: 27, scope: !88)
!88 = distinct !DILexicalBlock(scope: !85, file: !1, line: 43, column: 34)
!89 = !DILocalVariable(name: "tmp", scope: !88, file: !1, line: 44, type: !15)
!90 = !DILocation(line: 0, scope: !88)
!91 = !DILocation(line: 47, column: 5, scope: !88)
!92 = !DILocation(line: 43, column: 30, scope: !85)
!93 = !DILocation(line: 43, column: 5, scope: !85)
!94 = distinct !{!94, !86, !95, !96, !97}
!95 = !DILocation(line: 47, column: 5, scope: !80)
!96 = !{!"llvm.loop.mustprogress"}
!97 = !{!"pallas.loopInvBlock", !98, !99, !113, !125}
!98 = !{!"pallas.srcLoc", i64 38, i64 5, i64 42, i64 5, !19}
!99 = !{!"pallas.loopInv", !100, ptr @PALLAS_SPEC_3, !16, !16, !101}
!100 = !{!"pallas.srcLoc", i64 39, i64 5, i64 39, i64 42, !19}
!101 = !{!102, !107, !109, !111}
!102 = !{!57, !103}
!103 = !DILocalVariable(name: "n", arg: 1, scope: !104, file: !1, line: 39, type: !15)
!104 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 39, type: !105, scopeLine: 39, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!105 = !DISubroutineType(types: !106)
!106 = !{!29, !15, !15, !15, !15}
!107 = !{!77, !108}
!108 = !DILocalVariable(name: "prevRes", arg: 2, scope: !104, file: !1, line: 39, type: !15)
!109 = !{!78, !110}
!110 = !DILocalVariable(name: "res", arg: 3, scope: !104, file: !1, line: 39, type: !15)
!111 = !{!79, !112}
!112 = !DILocalVariable(name: "i", arg: 4, scope: !104, file: !1, line: 39, type: !15)
!113 = !{!"pallas.loopInv", !114, ptr @PALLAS_SPEC_4, !16, !16, !115}
!114 = !{!"pallas.srcLoc", i64 40, i64 5, i64 40, i64 38, !19}
!115 = !{!116, !119, !121, !123}
!116 = !{!57, !117}
!117 = !DILocalVariable(name: "n", arg: 1, scope: !118, file: !1, line: 40, type: !15)
!118 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 40, type: !105, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!119 = !{!77, !120}
!120 = !DILocalVariable(name: "prevRes", arg: 2, scope: !118, file: !1, line: 40, type: !15)
!121 = !{!78, !122}
!122 = !DILocalVariable(name: "res", arg: 3, scope: !118, file: !1, line: 40, type: !15)
!123 = !{!79, !124}
!124 = !DILocalVariable(name: "i", arg: 4, scope: !118, file: !1, line: 40, type: !15)
!125 = !{!"pallas.loopInv", !126, ptr @PALLAS_SPEC_5, !16, !16, !127}
!126 = !{!"pallas.srcLoc", i64 41, i64 5, i64 41, i64 42, !19}
!127 = !{!128, !131, !133, !135}
!128 = !{!57, !129}
!129 = !DILocalVariable(name: "n", arg: 1, scope: !130, file: !1, line: 41, type: !15)
!130 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 41, type: !105, scopeLine: 41, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!131 = !{!77, !132}
!132 = !DILocalVariable(name: "prevRes", arg: 2, scope: !130, file: !1, line: 41, type: !15)
!133 = !{!78, !134}
!134 = !DILocalVariable(name: "res", arg: 3, scope: !130, file: !1, line: 41, type: !15)
!135 = !{!79, !136}
!136 = !DILocalVariable(name: "i", arg: 4, scope: !130, file: !1, line: 41, type: !15)
!137 = !DILocation(line: 48, column: 5, scope: !50)
!138 = !DILocation(line: 49, column: 1, scope: !50)
!139 = !{!""}
!140 = !DILocation(line: 0, scope: !26)
!141 = !DILocation(line: 11, column: 12, scope: !26)
!142 = !DILocation(line: 0, scope: !59)
!143 = !DILocation(line: 25, column: 12, scope: !59)
!144 = !DILocation(line: 0, scope: !65)
!145 = !DILocation(line: 26, column: 9, scope: !65)
!146 = !DILocation(line: 26, column: 25, scope: !65)
!147 = !DILocation(line: 26, column: 22, scope: !65)
!148 = !DILocation(line: 0, scope: !118)
!149 = !DILocation(line: 40, column: 35, scope: !118)
!150 = !DILocation(line: 40, column: 27, scope: !118)
!151 = !DILocation(line: 40, column: 24, scope: !118)
!152 = !DILocation(line: 0, scope: !104)
!153 = !DILocation(line: 39, column: 27, scope: !104)
!154 = !DILocation(line: 39, column: 39, scope: !104)
!155 = !DILocation(line: 39, column: 35, scope: !104)
!156 = !DILocation(line: 39, column: 20, scope: !104)
!157 = !DILocation(line: 0, scope: !130)
!158 = !DILocation(line: 41, column: 39, scope: !130)
!159 = !DILocation(line: 41, column: 31, scope: !130)
!160 = !DILocation(line: 41, column: 28, scope: !130)
!161 = !{!"pallas.result"}
!162 = !{!"pallas.scAnd"}
