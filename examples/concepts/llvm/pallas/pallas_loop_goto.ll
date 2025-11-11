; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_loop_goto.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibRec(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !18 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !22, metadata !DIExpression()), !dbg !23
  %2 = icmp sle i32 %0, 1, !dbg !24
  br i1 %2, label %3, label %4, !dbg !26

3:                                                ; preds = %1
  br label %10, !dbg !27

4:                                                ; preds = %1
  %5 = sub nsw i32 %0, 1, !dbg !29
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !31
  %7 = sub nsw i32 %0, 2, !dbg !32
  %8 = call i32 @fibRec(i32 noundef %7), !dbg !33
  %9 = add nsw i32 %6, %8, !dbg !34
  br label %10, !dbg !35

10:                                               ; preds = %4, %3
  %.0 = phi i32 [ %0, %3 ], [ %9, %4 ], !dbg !36
  ret i32 %.0, !dbg !37
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibIt(i32 noundef %0) #0 !dbg !38 !pallas.fcontract !39 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !43, metadata !DIExpression()), !dbg !46
  %2 = icmp sle i32 %0, 1, !dbg !47
  br i1 %2, label %3, label %4, !dbg !49

3:                                                ; preds = %1
  br label %12, !dbg !50

4:                                                ; preds = %1
  call void @llvm.dbg.value(metadata i32 0, metadata !52, metadata !DIExpression()), !dbg !46
  call void @llvm.dbg.value(metadata i32 1, metadata !53, metadata !DIExpression()), !dbg !46
  call void @llvm.dbg.value(metadata i32 2, metadata !54, metadata !DIExpression()), !dbg !56
  br label %5, !dbg !57

5:                                                ; preds = %9, %4
  %.03 = phi i32 [ 1, %4 ], [ %8, %9 ], !dbg !46
  %.02 = phi i32 [ 0, %4 ], [ %.03, %9 ], !dbg !46
  %.01 = phi i32 [ 2, %4 ], [ %10, %9 ], !dbg !58
  call void @llvm.dbg.value(metadata i32 %.01, metadata !54, metadata !DIExpression()), !dbg !56
  call void @llvm.dbg.value(metadata i32 %.02, metadata !52, metadata !DIExpression()), !dbg !46
  call void @llvm.dbg.value(metadata i32 %.03, metadata !53, metadata !DIExpression()), !dbg !46
  %6 = icmp sle i32 %.01, %0, !dbg !59
  br i1 %6, label %7, label %11, !dbg !61

7:                                                ; preds = %5
  %8 = add nsw i32 %.02, %.03, !dbg !62
  call void @llvm.dbg.value(metadata i32 %8, metadata !64, metadata !DIExpression()), !dbg !65
  call void @llvm.dbg.value(metadata i32 %.03, metadata !52, metadata !DIExpression()), !dbg !46
  call void @llvm.dbg.value(metadata i32 %8, metadata !53, metadata !DIExpression()), !dbg !46
  br label %9, !dbg !66

9:                                                ; preds = %7
  %10 = add nsw i32 %.01, 1, !dbg !67
  call void @llvm.dbg.value(metadata i32 %10, metadata !54, metadata !DIExpression()), !dbg !56
  br label %5, !dbg !68, !llvm.loop !69

11:                                               ; preds = %5
  br label %12, !dbg !80

12:                                               ; preds = %11, %3
  %.0 = phi i32 [ %0, %3 ], [ %.03, %11 ], !dbg !46
  ret i32 %.0, !dbg !81
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !82 !pallas.exprWrapper !86 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !87, metadata !DIExpression()), !dbg !88
  %2 = icmp sge i32 %0, 0, !dbg !89
  ret i1 %2, !dbg !88
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !90 !pallas.exprWrapper !86 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !91, metadata !DIExpression()), !dbg !92
  %2 = icmp sge i32 %0, 0, !dbg !93
  ret i1 %2, !dbg !92
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0) #0 !dbg !94 !pallas.exprWrapper !86 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !95, metadata !DIExpression()), !dbg !96
  %2 = call i32 @pallas.result.0(), !dbg !97
  %3 = call i32 @fibRec(i32 noundef %0), !dbg !98
  %4 = icmp eq i32 %2, %3, !dbg !99
  ret i1 %4, !dbg !96
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !100 !pallas.exprWrapper !86 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !103, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %1, metadata !105, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %2, metadata !106, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %3, metadata !107, metadata !DIExpression()), !dbg !104
  %5 = icmp sle i32 2, %3, !dbg !108
  %6 = add nsw i32 %0, 1, !dbg !109
  %7 = icmp sle i32 %3, %6, !dbg !110
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !111
  ret i1 %8, !dbg !104
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !112 !pallas.exprWrapper !86 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !113, metadata !DIExpression()), !dbg !114
  call void @llvm.dbg.value(metadata i32 %1, metadata !115, metadata !DIExpression()), !dbg !114
  call void @llvm.dbg.value(metadata i32 %2, metadata !116, metadata !DIExpression()), !dbg !114
  call void @llvm.dbg.value(metadata i32 %3, metadata !117, metadata !DIExpression()), !dbg !114
  %5 = sub nsw i32 %3, 1, !dbg !118
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !119
  %7 = icmp eq i32 %2, %6, !dbg !120
  ret i1 %7, !dbg !114
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !121 !pallas.exprWrapper !86 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !122, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %1, metadata !124, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %2, metadata !125, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %3, metadata !126, metadata !DIExpression()), !dbg !123
  %5 = sub nsw i32 %3, 2, !dbg !127
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !128
  %7 = icmp eq i32 %1, %6, !dbg !129
  ret i1 %7, !dbg !123
}

declare !pallas.specLib !130 i32 @pallas.result.0()

declare !pallas.specLib !131 i1 @pallas.scAnd(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_loop_goto.c", directory: ".", checksumkind: CSK_MD5, checksum: "6b29c59aad6d3338a1cae3a6e193dc0c")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "b06bd891f978cf68afb97a795beaafe9")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "fibRec", scope: !13, file: !13, line: 8, type: !14, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!13 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_loop_goto.c", directory: "", checksumkind: CSK_MD5, checksum: "6b29c59aad6d3338a1cae3a6e193dc0c")
!14 = !DISubroutineType(types: !15)
!15 = !{!16, !16}
!16 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!17 = !{}
!18 = !{!19, i1 true, i1 false, !20}
!19 = !{!"pallas.srcLoc", i64 6, i64 1, i64 7, i64 22, !13}
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !22}
!21 = !{!"pallas.srcLoc", i64 7, i64 5, i64 7, i64 20, !13}
!22 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !13, line: 8, type: !16)
!23 = !DILocation(line: 0, scope: !12)
!24 = !DILocation(line: 9, column: 11, scope: !25)
!25 = distinct !DILexicalBlock(scope: !12, file: !13, line: 9, column: 9)
!26 = !DILocation(line: 9, column: 9, scope: !12)
!27 = !DILocation(line: 9, column: 19, scope: !28)
!28 = distinct !DILexicalBlock(scope: !25, file: !13, line: 9, column: 17)
!29 = !DILocation(line: 9, column: 53, scope: !30)
!30 = distinct !DILexicalBlock(scope: !25, file: !13, line: 9, column: 36)
!31 = !DILocation(line: 9, column: 45, scope: !30)
!32 = !DILocation(line: 9, column: 67, scope: !30)
!33 = !DILocation(line: 9, column: 59, scope: !30)
!34 = !DILocation(line: 9, column: 57, scope: !30)
!35 = !DILocation(line: 9, column: 38, scope: !30)
!36 = !DILocation(line: 9, scope: !25)
!37 = !DILocation(line: 10, column: 1, scope: !12)
!38 = distinct !DISubprogram(name: "fibIt", scope: !13, file: !13, line: 14, type: !14, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!39 = !{!40, i1 false, i1 false, !41, !44}
!40 = !{!"pallas.srcLoc", i64 12, i64 1, i64 13, i64 40, !13}
!41 = !{!"pallas.requires", !42, ptr @PALLAS_SPEC_1, !43}
!42 = !{!"pallas.srcLoc", i64 12, i64 5, i64 12, i64 20, !13}
!43 = !DILocalVariable(name: "n", arg: 1, scope: !38, file: !13, line: 14, type: !16)
!44 = !{!"pallas.ensures", !45, ptr @PALLAS_SPEC_2, !43}
!45 = !{!"pallas.srcLoc", i64 13, i64 5, i64 13, i64 38, !13}
!46 = !DILocation(line: 0, scope: !38)
!47 = !DILocation(line: 15, column: 11, scope: !48)
!48 = distinct !DILexicalBlock(scope: !38, file: !13, line: 15, column: 9)
!49 = !DILocation(line: 15, column: 9, scope: !38)
!50 = !DILocation(line: 15, column: 19, scope: !51)
!51 = distinct !DILexicalBlock(scope: !48, file: !13, line: 15, column: 17)
!52 = !DILocalVariable(name: "prevRes", scope: !38, file: !13, line: 16, type: !16)
!53 = !DILocalVariable(name: "res", scope: !38, file: !13, line: 17, type: !16)
!54 = !DILocalVariable(name: "i", scope: !55, file: !13, line: 24, type: !16)
!55 = distinct !DILexicalBlock(scope: !38, file: !13, line: 24, column: 5)
!56 = !DILocation(line: 0, scope: !55)
!57 = !DILocation(line: 24, column: 10, scope: !55)
!58 = !DILocation(line: 24, scope: !55)
!59 = !DILocation(line: 24, column: 23, scope: !60)
!60 = distinct !DILexicalBlock(scope: !55, file: !13, line: 24, column: 5)
!61 = !DILocation(line: 24, column: 5, scope: !55)
!62 = !DILocation(line: 25, column: 27, scope: !63)
!63 = distinct !DILexicalBlock(scope: !60, file: !13, line: 24, column: 34)
!64 = !DILocalVariable(name: "tmp", scope: !63, file: !13, line: 25, type: !16)
!65 = !DILocation(line: 0, scope: !63)
!66 = !DILocation(line: 28, column: 5, scope: !63)
!67 = !DILocation(line: 24, column: 30, scope: !60)
!68 = !DILocation(line: 24, column: 5, scope: !60)
!69 = distinct !{!69, !61, !70, !71, !72}
!70 = !DILocation(line: 28, column: 5, scope: !55)
!71 = !{!"llvm.loop.mustprogress"}
!72 = !{!"pallas.loopInv", !73, !74, !76, !78}
!73 = !{!"pallas.srcLoc", i64 19, i64 5, i64 23, i64 5, !13}
!74 = !{!75, ptr @PALLAS_SPEC_3, !43, !52, !53, !54}
!75 = !{!"pallas.srcLoc", i64 20, i64 5, i64 20, i64 42, !13}
!76 = !{!77, ptr @PALLAS_SPEC_4, !43, !52, !53, !54}
!77 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 38, !13}
!78 = !{!79, ptr @PALLAS_SPEC_5, !43, !52, !53, !54}
!79 = !{!"pallas.srcLoc", i64 22, i64 5, i64 22, i64 42, !13}
!80 = !DILocation(line: 29, column: 5, scope: !38)
!81 = !DILocation(line: 30, column: 1, scope: !38)
!82 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !13, file: !13, line: 7, type: !83, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!83 = !DISubroutineType(types: !84)
!84 = !{!85, !16}
!85 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!86 = !{!""}
!87 = !DILocalVariable(name: "n", arg: 1, scope: !82, file: !13, line: 7, type: !16)
!88 = !DILocation(line: 0, scope: !82)
!89 = !DILocation(line: 7, column: 16, scope: !82)
!90 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !13, file: !13, line: 12, type: !83, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!91 = !DILocalVariable(name: "n", arg: 1, scope: !90, file: !13, line: 12, type: !16)
!92 = !DILocation(line: 0, scope: !90)
!93 = !DILocation(line: 12, column: 16, scope: !90)
!94 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !13, file: !13, line: 13, type: !83, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!95 = !DILocalVariable(name: "n", arg: 1, scope: !94, file: !13, line: 13, type: !16)
!96 = !DILocation(line: 0, scope: !94)
!97 = !DILocation(line: 13, column: 13, scope: !94)
!98 = !DILocation(line: 13, column: 29, scope: !94)
!99 = !DILocation(line: 13, column: 26, scope: !94)
!100 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !13, file: !13, line: 20, type: !101, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!101 = !DISubroutineType(types: !102)
!102 = !{!85, !16, !16, !16, !16}
!103 = !DILocalVariable(name: "n", arg: 1, scope: !100, file: !13, line: 20, type: !16)
!104 = !DILocation(line: 0, scope: !100)
!105 = !DILocalVariable(name: "prevRes", arg: 2, scope: !100, file: !13, line: 20, type: !16)
!106 = !DILocalVariable(name: "res", arg: 3, scope: !100, file: !13, line: 20, type: !16)
!107 = !DILocalVariable(name: "i", arg: 4, scope: !100, file: !13, line: 20, type: !16)
!108 = !DILocation(line: 20, column: 27, scope: !100)
!109 = !DILocation(line: 20, column: 39, scope: !100)
!110 = !DILocation(line: 20, column: 35, scope: !100)
!111 = !DILocation(line: 20, column: 20, scope: !100)
!112 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !13, file: !13, line: 21, type: !101, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!113 = !DILocalVariable(name: "n", arg: 1, scope: !112, file: !13, line: 21, type: !16)
!114 = !DILocation(line: 0, scope: !112)
!115 = !DILocalVariable(name: "prevRes", arg: 2, scope: !112, file: !13, line: 21, type: !16)
!116 = !DILocalVariable(name: "res", arg: 3, scope: !112, file: !13, line: 21, type: !16)
!117 = !DILocalVariable(name: "i", arg: 4, scope: !112, file: !13, line: 21, type: !16)
!118 = !DILocation(line: 21, column: 35, scope: !112)
!119 = !DILocation(line: 21, column: 27, scope: !112)
!120 = !DILocation(line: 21, column: 24, scope: !112)
!121 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !13, file: !13, line: 22, type: !101, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!122 = !DILocalVariable(name: "n", arg: 1, scope: !121, file: !13, line: 22, type: !16)
!123 = !DILocation(line: 0, scope: !121)
!124 = !DILocalVariable(name: "prevRes", arg: 2, scope: !121, file: !13, line: 22, type: !16)
!125 = !DILocalVariable(name: "res", arg: 3, scope: !121, file: !13, line: 22, type: !16)
!126 = !DILocalVariable(name: "i", arg: 4, scope: !121, file: !13, line: 22, type: !16)
!127 = !DILocation(line: 22, column: 39, scope: !121)
!128 = !DILocation(line: 22, column: 31, scope: !121)
!129 = !DILocation(line: 22, column: 28, scope: !121)
!130 = !{!"pallas.result"}
!131 = !{!"pallas.scAnd"}
