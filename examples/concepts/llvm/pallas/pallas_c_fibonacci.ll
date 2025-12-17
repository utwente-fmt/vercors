; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_fibonacci.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_3], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibRec(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !22, metadata !DIExpression()), !dbg !23
  %2 = icmp eq i32 %0, 0, !dbg !24
  br i1 %2, label %3, label %4, !dbg !26

3:                                                ; preds = %1
  br label %13, !dbg !27

4:                                                ; preds = %1
  %5 = icmp eq i32 %0, 1, !dbg !29
  br i1 %5, label %6, label %7, !dbg !31

6:                                                ; preds = %4
  br label %13, !dbg !32

7:                                                ; preds = %4
  %8 = sub nsw i32 %0, 1, !dbg !34
  %9 = call i32 @fibRec(i32 noundef %8), !dbg !36
  %10 = sub nsw i32 %0, 2, !dbg !37
  %11 = call i32 @fibRec(i32 noundef %10), !dbg !38
  %12 = add nsw i32 %9, %11, !dbg !39
  br label %13, !dbg !40

13:                                               ; preds = %7, %6, %3
  %.0 = phi i32 [ 0, %3 ], [ 1, %6 ], [ %12, %7 ], !dbg !41
  ret i32 %.0, !dbg !42
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibIt(i32 noundef %0) #0 !dbg !43 !pallas.fcontract !44 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !48, metadata !DIExpression()), !dbg !51
  %2 = icmp eq i32 %0, 0, !dbg !52
  br i1 %2, label %3, label %4, !dbg !54

3:                                                ; preds = %1
  br label %16, !dbg !55

4:                                                ; preds = %1
  %5 = icmp eq i32 %0, 1, !dbg !57
  br i1 %5, label %6, label %7, !dbg !59

6:                                                ; preds = %4
  br label %16, !dbg !60

7:                                                ; preds = %4
  br label %8

8:                                                ; preds = %7
  call void @llvm.dbg.value(metadata i32 0, metadata !62, metadata !DIExpression()), !dbg !51
  call void @llvm.dbg.value(metadata i32 1, metadata !63, metadata !DIExpression()), !dbg !51
  call void @llvm.dbg.value(metadata i32 2, metadata !64, metadata !DIExpression()), !dbg !66
  br label %9, !dbg !67

9:                                                ; preds = %13, %8
  %.03 = phi i32 [ 1, %8 ], [ %12, %13 ], !dbg !51
  %.02 = phi i32 [ 0, %8 ], [ %.03, %13 ], !dbg !51
  %.01 = phi i32 [ 2, %8 ], [ %14, %13 ], !dbg !68
  call void @llvm.dbg.value(metadata i32 %.01, metadata !64, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.value(metadata i32 %.02, metadata !62, metadata !DIExpression()), !dbg !51
  call void @llvm.dbg.value(metadata i32 %.03, metadata !63, metadata !DIExpression()), !dbg !51
  %10 = icmp sle i32 %.01, %0, !dbg !69
  br i1 %10, label %11, label %15, !dbg !71

11:                                               ; preds = %9
  %12 = add nsw i32 %.02, %.03, !dbg !72
  call void @llvm.dbg.value(metadata i32 %12, metadata !74, metadata !DIExpression()), !dbg !75
  call void @llvm.dbg.value(metadata i32 %.03, metadata !62, metadata !DIExpression()), !dbg !51
  call void @llvm.dbg.value(metadata i32 %12, metadata !63, metadata !DIExpression()), !dbg !51
  br label %13, !dbg !76

13:                                               ; preds = %11
  %14 = add nsw i32 %.01, 1, !dbg !77
  call void @llvm.dbg.value(metadata i32 %14, metadata !64, metadata !DIExpression()), !dbg !66
  br label %9, !dbg !78, !llvm.loop !79

15:                                               ; preds = %9
  br label %16, !dbg !90

16:                                               ; preds = %15, %6, %3
  %.0 = phi i32 [ 0, %3 ], [ 1, %6 ], [ %.03, %15 ], !dbg !51
  ret i32 %.0, !dbg !91
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !92 !pallas.exprWrapper !96 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !97, metadata !DIExpression()), !dbg !98
  %2 = icmp sge i32 %0, 0, !dbg !99
  ret i1 %2, !dbg !98
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !100 !pallas.exprWrapper !96 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !101, metadata !DIExpression()), !dbg !102
  %2 = icmp sge i32 %0, 0, !dbg !103
  ret i1 %2, !dbg !102
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0) #0 !dbg !104 !pallas.exprWrapper !96 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !105, metadata !DIExpression()), !dbg !106
  %2 = call i32 @"pallas.result i32"(), !dbg !107
  %3 = call i32 @fibRec(i32 noundef %0), !dbg !108
  %4 = icmp eq i32 %2, %3, !dbg !109
  ret i1 %4, !dbg !106
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !110 !pallas.exprWrapper !96 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !113, metadata !DIExpression()), !dbg !114
  call void @llvm.dbg.value(metadata i32 %1, metadata !115, metadata !DIExpression()), !dbg !114
  call void @llvm.dbg.value(metadata i32 %2, metadata !116, metadata !DIExpression()), !dbg !114
  call void @llvm.dbg.value(metadata i32 %3, metadata !117, metadata !DIExpression()), !dbg !114
  %5 = sub nsw i32 %3, 2, !dbg !118
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !119
  %7 = icmp eq i32 %1, %6, !dbg !120
  ret i1 %7, !dbg !114
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !121 !pallas.exprWrapper !96 {
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
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !130 !pallas.exprWrapper !96 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !131, metadata !DIExpression()), !dbg !132
  call void @llvm.dbg.value(metadata i32 %1, metadata !133, metadata !DIExpression()), !dbg !132
  call void @llvm.dbg.value(metadata i32 %2, metadata !134, metadata !DIExpression()), !dbg !132
  call void @llvm.dbg.value(metadata i32 %3, metadata !135, metadata !DIExpression()), !dbg !132
  %5 = icmp sle i32 2, %3, !dbg !136
  %6 = add nsw i32 %0, 1, !dbg !137
  %7 = icmp sle i32 %3, %6, !dbg !138
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !139
  ret i1 %8, !dbg !132
}

declare !pallas.specLib !140 i32 @"pallas.result i32"()

declare !pallas.specLib !141 i1 @pallas.scAnd(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_fibonacci.c", directory: ".", checksumkind: CSK_MD5, checksum: "e8af48866595e75e2d19b3bd6ae34506")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "799705ce4007daa05dcee20edf0111e8")
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
!17 = !{!18, i1 true, i1 false, !20}
!18 = !{!"pallas.srcLoc", i64 12, i64 1, i64 15, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_fibonacci.c", directory: "", checksumkind: CSK_MD5, checksum: "e8af48866595e75e2d19b3bd6ae34506")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !22}
!21 = !{!"pallas.srcLoc", i64 14, i64 1, i64 14, i64 16, !19}
!22 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 16, type: !15)
!23 = !DILocation(line: 0, scope: !12)
!24 = !DILocation(line: 17, column: 11, scope: !25)
!25 = distinct !DILexicalBlock(scope: !12, file: !1, line: 17, column: 9)
!26 = !DILocation(line: 17, column: 9, scope: !12)
!27 = !DILocation(line: 18, column: 9, scope: !28)
!28 = distinct !DILexicalBlock(scope: !25, file: !1, line: 17, column: 17)
!29 = !DILocation(line: 19, column: 18, scope: !30)
!30 = distinct !DILexicalBlock(scope: !25, file: !1, line: 19, column: 16)
!31 = !DILocation(line: 19, column: 16, scope: !25)
!32 = !DILocation(line: 20, column: 9, scope: !33)
!33 = distinct !DILexicalBlock(scope: !30, file: !1, line: 19, column: 24)
!34 = !DILocation(line: 22, column: 25, scope: !35)
!35 = distinct !DILexicalBlock(scope: !30, file: !1, line: 21, column: 12)
!36 = !DILocation(line: 22, column: 16, scope: !35)
!37 = !DILocation(line: 22, column: 41, scope: !35)
!38 = !DILocation(line: 22, column: 32, scope: !35)
!39 = !DILocation(line: 22, column: 30, scope: !35)
!40 = !DILocation(line: 22, column: 9, scope: !35)
!41 = !DILocation(line: 0, scope: !25)
!42 = !DILocation(line: 24, column: 1, scope: !12)
!43 = distinct !DISubprogram(name: "fibIt", scope: !1, file: !1, line: 31, type: !13, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!44 = !{!45, i1 false, i1 false, !46, !49}
!45 = !{!"pallas.srcLoc", i64 27, i64 1, i64 30, i64 1, !19}
!46 = !{!"pallas.requires", !47, ptr @PALLAS_SPEC_1, !48}
!47 = !{!"pallas.srcLoc", i64 28, i64 1, i64 28, i64 16, !19}
!48 = !DILocalVariable(name: "n", arg: 1, scope: !43, file: !1, line: 31, type: !15)
!49 = !{!"pallas.ensures", !50, ptr @PALLAS_SPEC_2, !48}
!50 = !{!"pallas.srcLoc", i64 29, i64 1, i64 29, i64 34, !19}
!51 = !DILocation(line: 0, scope: !43)
!52 = !DILocation(line: 32, column: 12, scope: !53)
!53 = distinct !DILexicalBlock(scope: !43, file: !1, line: 32, column: 10)
!54 = !DILocation(line: 32, column: 10, scope: !43)
!55 = !DILocation(line: 33, column: 9, scope: !56)
!56 = distinct !DILexicalBlock(scope: !53, file: !1, line: 32, column: 18)
!57 = !DILocation(line: 34, column: 20, scope: !58)
!58 = distinct !DILexicalBlock(scope: !53, file: !1, line: 34, column: 18)
!59 = !DILocation(line: 34, column: 18, scope: !53)
!60 = !DILocation(line: 35, column: 9, scope: !61)
!61 = distinct !DILexicalBlock(scope: !58, file: !1, line: 34, column: 26)
!62 = !DILocalVariable(name: "prevRes", scope: !43, file: !1, line: 38, type: !15)
!63 = !DILocalVariable(name: "res", scope: !43, file: !1, line: 39, type: !15)
!64 = !DILocalVariable(name: "i", scope: !65, file: !1, line: 46, type: !15)
!65 = distinct !DILexicalBlock(scope: !43, file: !1, line: 46, column: 5)
!66 = !DILocation(line: 0, scope: !65)
!67 = !DILocation(line: 46, column: 10, scope: !65)
!68 = !DILocation(line: 46, scope: !65)
!69 = !DILocation(line: 46, column: 23, scope: !70)
!70 = distinct !DILexicalBlock(scope: !65, file: !1, line: 46, column: 5)
!71 = !DILocation(line: 46, column: 5, scope: !65)
!72 = !DILocation(line: 47, column: 27, scope: !73)
!73 = distinct !DILexicalBlock(scope: !70, file: !1, line: 46, column: 34)
!74 = !DILocalVariable(name: "tmp", scope: !73, file: !1, line: 47, type: !15)
!75 = !DILocation(line: 0, scope: !73)
!76 = !DILocation(line: 50, column: 5, scope: !73)
!77 = !DILocation(line: 46, column: 30, scope: !70)
!78 = !DILocation(line: 46, column: 5, scope: !70)
!79 = distinct !{!79, !71, !80, !81, !82}
!80 = !DILocation(line: 50, column: 5, scope: !65)
!81 = !{!"llvm.loop.mustprogress"}
!82 = !{!"pallas.loopInv", !83, !84, !86, !88}
!83 = !{!"pallas.srcLoc", i64 41, i64 5, i64 45, i64 5, !19}
!84 = !{!85, ptr @PALLAS_SPEC_3, !48, !62, !63, !64}
!85 = !{!"pallas.srcLoc", i64 42, i64 5, i64 42, i64 42, !19}
!86 = !{!87, ptr @PALLAS_SPEC_4, !48, !62, !63, !64}
!87 = !{!"pallas.srcLoc", i64 43, i64 5, i64 43, i64 38, !19}
!88 = !{!89, ptr @PALLAS_SPEC_5, !48, !62, !63, !64}
!89 = !{!"pallas.srcLoc", i64 44, i64 5, i64 44, i64 42, !19}
!90 = !DILocation(line: 51, column: 5, scope: !43)
!91 = !DILocation(line: 52, column: 1, scope: !43)
!92 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 14, type: !93, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!93 = !DISubroutineType(types: !94)
!94 = !{!95, !15}
!95 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!96 = !{!""}
!97 = !DILocalVariable(name: "n", arg: 1, scope: !92, file: !1, line: 14, type: !15)
!98 = !DILocation(line: 0, scope: !92)
!99 = !DILocation(line: 14, column: 12, scope: !92)
!100 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 28, type: !93, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!101 = !DILocalVariable(name: "n", arg: 1, scope: !100, file: !1, line: 28, type: !15)
!102 = !DILocation(line: 0, scope: !100)
!103 = !DILocation(line: 28, column: 12, scope: !100)
!104 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 29, type: !93, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!105 = !DILocalVariable(name: "n", arg: 1, scope: !104, file: !1, line: 29, type: !15)
!106 = !DILocation(line: 0, scope: !104)
!107 = !DILocation(line: 29, column: 9, scope: !104)
!108 = !DILocation(line: 29, column: 25, scope: !104)
!109 = !DILocation(line: 29, column: 22, scope: !104)
!110 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 44, type: !111, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!111 = !DISubroutineType(types: !112)
!112 = !{!95, !15, !15, !15, !15}
!113 = !DILocalVariable(name: "n", arg: 1, scope: !110, file: !1, line: 44, type: !15)
!114 = !DILocation(line: 0, scope: !110)
!115 = !DILocalVariable(name: "prevRes", arg: 2, scope: !110, file: !1, line: 44, type: !15)
!116 = !DILocalVariable(name: "res", arg: 3, scope: !110, file: !1, line: 44, type: !15)
!117 = !DILocalVariable(name: "i", arg: 4, scope: !110, file: !1, line: 44, type: !15)
!118 = !DILocation(line: 44, column: 39, scope: !110)
!119 = !DILocation(line: 44, column: 31, scope: !110)
!120 = !DILocation(line: 44, column: 28, scope: !110)
!121 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 43, type: !111, scopeLine: 43, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!122 = !DILocalVariable(name: "n", arg: 1, scope: !121, file: !1, line: 43, type: !15)
!123 = !DILocation(line: 0, scope: !121)
!124 = !DILocalVariable(name: "prevRes", arg: 2, scope: !121, file: !1, line: 43, type: !15)
!125 = !DILocalVariable(name: "res", arg: 3, scope: !121, file: !1, line: 43, type: !15)
!126 = !DILocalVariable(name: "i", arg: 4, scope: !121, file: !1, line: 43, type: !15)
!127 = !DILocation(line: 43, column: 35, scope: !121)
!128 = !DILocation(line: 43, column: 27, scope: !121)
!129 = !DILocation(line: 43, column: 24, scope: !121)
!130 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 42, type: !111, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!131 = !DILocalVariable(name: "n", arg: 1, scope: !130, file: !1, line: 42, type: !15)
!132 = !DILocation(line: 0, scope: !130)
!133 = !DILocalVariable(name: "prevRes", arg: 2, scope: !130, file: !1, line: 42, type: !15)
!134 = !DILocalVariable(name: "res", arg: 3, scope: !130, file: !1, line: 42, type: !15)
!135 = !DILocalVariable(name: "i", arg: 4, scope: !130, file: !1, line: 42, type: !15)
!136 = !DILocation(line: 42, column: 27, scope: !130)
!137 = !DILocation(line: 42, column: 39, scope: !130)
!138 = !DILocation(line: 42, column: 35, scope: !130)
!139 = !DILocation(line: 42, column: 20, scope: !130)
!140 = !{!"pallas.result"}
!141 = !{!"pallas.scAnd"}
