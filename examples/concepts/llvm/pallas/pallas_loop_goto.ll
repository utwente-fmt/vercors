; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_loop_goto.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_5], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibRec(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !24, metadata !DIExpression()), !dbg !30
  %2 = icmp sle i32 %0, 1, !dbg !31
  br i1 %2, label %3, label %4, !dbg !33

3:                                                ; preds = %1
  br label %10, !dbg !34

4:                                                ; preds = %1
  %5 = sub nsw i32 %0, 1, !dbg !36
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !38
  %7 = sub nsw i32 %0, 2, !dbg !39
  %8 = call i32 @fibRec(i32 noundef %7), !dbg !40
  %9 = add nsw i32 %6, %8, !dbg !41
  br label %10, !dbg !42

10:                                               ; preds = %4, %3
  %.0 = phi i32 [ %0, %3 ], [ %9, %4 ], !dbg !43
  ret i32 %.0, !dbg !44
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibIt(i32 noundef %0) #0 !dbg !45 !pallas.fcontract !46 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !52, metadata !DIExpression()), !dbg !61
  %2 = icmp sle i32 %0, 1, !dbg !62
  br i1 %2, label %3, label %4, !dbg !64

3:                                                ; preds = %1
  br label %12, !dbg !65

4:                                                ; preds = %1
  call void @llvm.dbg.value(metadata i32 0, metadata !67, metadata !DIExpression()), !dbg !61
  call void @llvm.dbg.value(metadata i32 1, metadata !68, metadata !DIExpression()), !dbg !61
  call void @llvm.dbg.value(metadata i32 2, metadata !69, metadata !DIExpression()), !dbg !71
  br label %5, !dbg !72

5:                                                ; preds = %9, %4
  %.03 = phi i32 [ 1, %4 ], [ %8, %9 ], !dbg !61
  %.02 = phi i32 [ 0, %4 ], [ %.03, %9 ], !dbg !61
  %.01 = phi i32 [ 2, %4 ], [ %10, %9 ], !dbg !73
  call void @llvm.dbg.value(metadata i32 %.01, metadata !69, metadata !DIExpression()), !dbg !71
  call void @llvm.dbg.value(metadata i32 %.02, metadata !67, metadata !DIExpression()), !dbg !61
  call void @llvm.dbg.value(metadata i32 %.03, metadata !68, metadata !DIExpression()), !dbg !61
  %6 = icmp sle i32 %.01, %0, !dbg !74
  br i1 %6, label %7, label %11, !dbg !76

7:                                                ; preds = %5
  %8 = add nsw i32 %.02, %.03, !dbg !77
  call void @llvm.dbg.value(metadata i32 %8, metadata !79, metadata !DIExpression()), !dbg !80
  call void @llvm.dbg.value(metadata i32 %.03, metadata !67, metadata !DIExpression()), !dbg !61
  call void @llvm.dbg.value(metadata i32 %8, metadata !68, metadata !DIExpression()), !dbg !61
  br label %9, !dbg !81

9:                                                ; preds = %7
  %10 = add nsw i32 %.01, 1, !dbg !82
  call void @llvm.dbg.value(metadata i32 %10, metadata !69, metadata !DIExpression()), !dbg !71
  br label %5, !dbg !83, !llvm.loop !84

11:                                               ; preds = %5
  br label %12, !dbg !127

12:                                               ; preds = %11, %3
  %.0 = phi i32 [ %0, %3 ], [ %.03, %11 ], !dbg !61
  ret i32 %.0, !dbg !128
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !26 !pallas.exprWrapper !129 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !130
  %2 = icmp sge i32 %0, 0, !dbg !131
  ret i1 %2, !dbg !130
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !54 !pallas.exprWrapper !129 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !53, metadata !DIExpression()), !dbg !132
  %2 = icmp sge i32 %0, 0, !dbg !133
  ret i1 %2, !dbg !132
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0) #0 !dbg !60 !pallas.exprWrapper !129 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !59, metadata !DIExpression()), !dbg !134
  %2 = call i32 @"pallas.result i32"(), !dbg !135
  %3 = call i32 @fibRec(i32 noundef %0), !dbg !136
  %4 = icmp eq i32 %2, %3, !dbg !137
  ret i1 %4, !dbg !134
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !108 !pallas.exprWrapper !129 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !107, metadata !DIExpression()), !dbg !138
  call void @llvm.dbg.value(metadata i32 %1, metadata !110, metadata !DIExpression()), !dbg !138
  call void @llvm.dbg.value(metadata i32 %2, metadata !112, metadata !DIExpression()), !dbg !138
  call void @llvm.dbg.value(metadata i32 %3, metadata !114, metadata !DIExpression()), !dbg !138
  %5 = sub nsw i32 %3, 1, !dbg !139
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !140
  %7 = icmp eq i32 %2, %6, !dbg !141
  ret i1 %7, !dbg !138
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !94 !pallas.exprWrapper !129 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !93, metadata !DIExpression()), !dbg !142
  call void @llvm.dbg.value(metadata i32 %1, metadata !98, metadata !DIExpression()), !dbg !142
  call void @llvm.dbg.value(metadata i32 %2, metadata !100, metadata !DIExpression()), !dbg !142
  call void @llvm.dbg.value(metadata i32 %3, metadata !102, metadata !DIExpression()), !dbg !142
  %5 = icmp sle i32 2, %3, !dbg !143
  %6 = add nsw i32 %0, 1, !dbg !144
  %7 = icmp sle i32 %3, %6, !dbg !145
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !146
  ret i1 %8, !dbg !142
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !120 !pallas.exprWrapper !129 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !119, metadata !DIExpression()), !dbg !147
  call void @llvm.dbg.value(metadata i32 %1, metadata !122, metadata !DIExpression()), !dbg !147
  call void @llvm.dbg.value(metadata i32 %2, metadata !124, metadata !DIExpression()), !dbg !147
  call void @llvm.dbg.value(metadata i32 %3, metadata !126, metadata !DIExpression()), !dbg !147
  %5 = sub nsw i32 %3, 2, !dbg !148
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !149
  %7 = icmp eq i32 %1, %6, !dbg !150
  ret i1 %7, !dbg !147
}

declare !pallas.specLib !151 i32 @"pallas.result i32"()

declare !pallas.specLib !152 i1 @pallas.scAnd(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_loop_goto.c", directory: ".", checksumkind: CSK_MD5, checksum: "6b29c59aad6d3338a1cae3a6e193dc0c")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "f2db58ca2e56dfbb2466f422503698df")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "fibRec", scope: !1, file: !1, line: 8, type: !13, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 true, i1 false, !16, !16, !20}
!18 = !{!"pallas.srcLoc", i64 6, i64 1, i64 7, i64 22, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_loop_goto.c", directory: "", checksumkind: CSK_MD5, checksum: "6b29c59aad6d3338a1cae3a6e193dc0c")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !16, !16, !22}
!21 = !{!"pallas.srcLoc", i64 7, i64 5, i64 7, i64 20, !19}
!22 = !{!23}
!23 = !{!24, !25}
!24 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 8, type: !15)
!25 = !DILocalVariable(name: "n", arg: 1, scope: !26, file: !1, line: 7, type: !15)
!26 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 7, type: !27, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!27 = !DISubroutineType(types: !28)
!28 = !{!29, !15}
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !DILocation(line: 0, scope: !12)
!31 = !DILocation(line: 9, column: 11, scope: !32)
!32 = distinct !DILexicalBlock(scope: !12, file: !1, line: 9, column: 9)
!33 = !DILocation(line: 9, column: 9, scope: !12)
!34 = !DILocation(line: 9, column: 19, scope: !35)
!35 = distinct !DILexicalBlock(scope: !32, file: !1, line: 9, column: 17)
!36 = !DILocation(line: 9, column: 53, scope: !37)
!37 = distinct !DILexicalBlock(scope: !32, file: !1, line: 9, column: 36)
!38 = !DILocation(line: 9, column: 45, scope: !37)
!39 = !DILocation(line: 9, column: 67, scope: !37)
!40 = !DILocation(line: 9, column: 59, scope: !37)
!41 = !DILocation(line: 9, column: 57, scope: !37)
!42 = !DILocation(line: 9, column: 38, scope: !37)
!43 = !DILocation(line: 9, scope: !32)
!44 = !DILocation(line: 10, column: 1, scope: !12)
!45 = distinct !DISubprogram(name: "fibIt", scope: !1, file: !1, line: 14, type: !13, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!46 = !{!47, i1 false, i1 false, !16, !16, !48, !55}
!47 = !{!"pallas.srcLoc", i64 12, i64 1, i64 13, i64 40, !19}
!48 = !{!"pallas.requires", !49, ptr @PALLAS_SPEC_1, !16, !16, !50}
!49 = !{!"pallas.srcLoc", i64 12, i64 5, i64 12, i64 20, !19}
!50 = !{!51}
!51 = !{!52, !53}
!52 = !DILocalVariable(name: "n", arg: 1, scope: !45, file: !1, line: 14, type: !15)
!53 = !DILocalVariable(name: "n", arg: 1, scope: !54, file: !1, line: 12, type: !15)
!54 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 12, type: !27, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!55 = !{!"pallas.ensures", !56, ptr @PALLAS_SPEC_2, !16, !16, !57}
!56 = !{!"pallas.srcLoc", i64 13, i64 5, i64 13, i64 38, !19}
!57 = !{!58}
!58 = !{!52, !59}
!59 = !DILocalVariable(name: "n", arg: 1, scope: !60, file: !1, line: 13, type: !15)
!60 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 13, type: !27, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!61 = !DILocation(line: 0, scope: !45)
!62 = !DILocation(line: 15, column: 11, scope: !63)
!63 = distinct !DILexicalBlock(scope: !45, file: !1, line: 15, column: 9)
!64 = !DILocation(line: 15, column: 9, scope: !45)
!65 = !DILocation(line: 15, column: 19, scope: !66)
!66 = distinct !DILexicalBlock(scope: !63, file: !1, line: 15, column: 17)
!67 = !DILocalVariable(name: "prevRes", scope: !45, file: !1, line: 16, type: !15)
!68 = !DILocalVariable(name: "res", scope: !45, file: !1, line: 17, type: !15)
!69 = !DILocalVariable(name: "i", scope: !70, file: !1, line: 24, type: !15)
!70 = distinct !DILexicalBlock(scope: !45, file: !1, line: 24, column: 5)
!71 = !DILocation(line: 0, scope: !70)
!72 = !DILocation(line: 24, column: 10, scope: !70)
!73 = !DILocation(line: 24, scope: !70)
!74 = !DILocation(line: 24, column: 23, scope: !75)
!75 = distinct !DILexicalBlock(scope: !70, file: !1, line: 24, column: 5)
!76 = !DILocation(line: 24, column: 5, scope: !70)
!77 = !DILocation(line: 25, column: 27, scope: !78)
!78 = distinct !DILexicalBlock(scope: !75, file: !1, line: 24, column: 34)
!79 = !DILocalVariable(name: "tmp", scope: !78, file: !1, line: 25, type: !15)
!80 = !DILocation(line: 0, scope: !78)
!81 = !DILocation(line: 28, column: 5, scope: !78)
!82 = !DILocation(line: 24, column: 30, scope: !75)
!83 = !DILocation(line: 24, column: 5, scope: !75)
!84 = distinct !{!84, !76, !85, !86, !87}
!85 = !DILocation(line: 28, column: 5, scope: !70)
!86 = !{!"llvm.loop.mustprogress"}
!87 = !{!"pallas.loopInvBlock", !88, !89, !103, !115}
!88 = !{!"pallas.srcLoc", i64 19, i64 5, i64 23, i64 5, !19}
!89 = !{!"pallas.loopInv", !90, ptr @PALLAS_SPEC_3, !16, !16, !91}
!90 = !{!"pallas.srcLoc", i64 20, i64 5, i64 20, i64 42, !19}
!91 = !{!92, !97, !99, !101}
!92 = !{!52, !93}
!93 = !DILocalVariable(name: "n", arg: 1, scope: !94, file: !1, line: 20, type: !15)
!94 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 20, type: !95, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!95 = !DISubroutineType(types: !96)
!96 = !{!29, !15, !15, !15, !15}
!97 = !{!67, !98}
!98 = !DILocalVariable(name: "prevRes", arg: 2, scope: !94, file: !1, line: 20, type: !15)
!99 = !{!68, !100}
!100 = !DILocalVariable(name: "res", arg: 3, scope: !94, file: !1, line: 20, type: !15)
!101 = !{!69, !102}
!102 = !DILocalVariable(name: "i", arg: 4, scope: !94, file: !1, line: 20, type: !15)
!103 = !{!"pallas.loopInv", !104, ptr @PALLAS_SPEC_4, !16, !16, !105}
!104 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 38, !19}
!105 = !{!106, !109, !111, !113}
!106 = !{!52, !107}
!107 = !DILocalVariable(name: "n", arg: 1, scope: !108, file: !1, line: 21, type: !15)
!108 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 21, type: !95, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!109 = !{!67, !110}
!110 = !DILocalVariable(name: "prevRes", arg: 2, scope: !108, file: !1, line: 21, type: !15)
!111 = !{!68, !112}
!112 = !DILocalVariable(name: "res", arg: 3, scope: !108, file: !1, line: 21, type: !15)
!113 = !{!69, !114}
!114 = !DILocalVariable(name: "i", arg: 4, scope: !108, file: !1, line: 21, type: !15)
!115 = !{!"pallas.loopInv", !116, ptr @PALLAS_SPEC_5, !16, !16, !117}
!116 = !{!"pallas.srcLoc", i64 22, i64 5, i64 22, i64 42, !19}
!117 = !{!118, !121, !123, !125}
!118 = !{!52, !119}
!119 = !DILocalVariable(name: "n", arg: 1, scope: !120, file: !1, line: 22, type: !15)
!120 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 22, type: !95, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!121 = !{!67, !122}
!122 = !DILocalVariable(name: "prevRes", arg: 2, scope: !120, file: !1, line: 22, type: !15)
!123 = !{!68, !124}
!124 = !DILocalVariable(name: "res", arg: 3, scope: !120, file: !1, line: 22, type: !15)
!125 = !{!69, !126}
!126 = !DILocalVariable(name: "i", arg: 4, scope: !120, file: !1, line: 22, type: !15)
!127 = !DILocation(line: 29, column: 5, scope: !45)
!128 = !DILocation(line: 30, column: 1, scope: !45)
!129 = !{!""}
!130 = !DILocation(line: 0, scope: !26)
!131 = !DILocation(line: 7, column: 16, scope: !26)
!132 = !DILocation(line: 0, scope: !54)
!133 = !DILocation(line: 12, column: 16, scope: !54)
!134 = !DILocation(line: 0, scope: !60)
!135 = !DILocation(line: 13, column: 13, scope: !60)
!136 = !DILocation(line: 13, column: 29, scope: !60)
!137 = !DILocation(line: 13, column: 26, scope: !60)
!138 = !DILocation(line: 0, scope: !108)
!139 = !DILocation(line: 21, column: 35, scope: !108)
!140 = !DILocation(line: 21, column: 27, scope: !108)
!141 = !DILocation(line: 21, column: 24, scope: !108)
!142 = !DILocation(line: 0, scope: !94)
!143 = !DILocation(line: 20, column: 27, scope: !94)
!144 = !DILocation(line: 20, column: 39, scope: !94)
!145 = !DILocation(line: 20, column: 35, scope: !94)
!146 = !DILocation(line: 20, column: 20, scope: !94)
!147 = !DILocation(line: 0, scope: !120)
!148 = !DILocation(line: 22, column: 39, scope: !120)
!149 = !DILocation(line: 22, column: 31, scope: !120)
!150 = !DILocation(line: 22, column: 28, scope: !120)
!151 = !{!"pallas.result"}
!152 = !{!"pallas.scAnd"}
