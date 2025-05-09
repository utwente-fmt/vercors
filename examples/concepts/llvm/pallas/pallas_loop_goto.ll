; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_loop_goto.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibRec(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !21, metadata !DIExpression()), !dbg !22
  %2 = icmp sle i32 %0, 1, !dbg !23
  br i1 %2, label %3, label %4, !dbg !25

3:                                                ; preds = %1
  br label %10, !dbg !26

4:                                                ; preds = %1
  %5 = sub nsw i32 %0, 1, !dbg !28
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !30
  %7 = sub nsw i32 %0, 2, !dbg !31
  %8 = call i32 @fibRec(i32 noundef %7), !dbg !32
  %9 = add nsw i32 %6, %8, !dbg !33
  br label %10, !dbg !34

10:                                               ; preds = %4, %3
  %.0 = phi i32 [ %0, %3 ], [ %9, %4 ], !dbg !35
  ret i32 %.0, !dbg !36
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @fibIt(i32 noundef %0) #0 !dbg !37 !pallas.fcontract !38 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !42, metadata !DIExpression()), !dbg !45
  %2 = icmp sle i32 %0, 1, !dbg !46
  br i1 %2, label %3, label %4, !dbg !48

3:                                                ; preds = %1
  br label %12, !dbg !49

4:                                                ; preds = %1
  call void @llvm.dbg.value(metadata i32 0, metadata !51, metadata !DIExpression()), !dbg !45
  call void @llvm.dbg.value(metadata i32 1, metadata !52, metadata !DIExpression()), !dbg !45
  call void @llvm.dbg.value(metadata i32 2, metadata !53, metadata !DIExpression()), !dbg !55
  br label %5, !dbg !56

5:                                                ; preds = %9, %4
  %.03 = phi i32 [ 1, %4 ], [ %8, %9 ], !dbg !45
  %.02 = phi i32 [ 0, %4 ], [ %.03, %9 ], !dbg !45
  %.01 = phi i32 [ 2, %4 ], [ %10, %9 ], !dbg !57
  call void @llvm.dbg.value(metadata i32 %.01, metadata !53, metadata !DIExpression()), !dbg !55
  call void @llvm.dbg.value(metadata i32 %.02, metadata !51, metadata !DIExpression()), !dbg !45
  call void @llvm.dbg.value(metadata i32 %.03, metadata !52, metadata !DIExpression()), !dbg !45
  %6 = icmp sle i32 %.01, %0, !dbg !58
  br i1 %6, label %7, label %11, !dbg !60

7:                                                ; preds = %5
  %8 = add nsw i32 %.02, %.03, !dbg !61
  call void @llvm.dbg.value(metadata i32 %8, metadata !63, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %.03, metadata !51, metadata !DIExpression()), !dbg !45
  call void @llvm.dbg.value(metadata i32 %8, metadata !52, metadata !DIExpression()), !dbg !45
  br label %9, !dbg !65

9:                                                ; preds = %7
  %10 = add nsw i32 %.01, 1, !dbg !66
  call void @llvm.dbg.value(metadata i32 %10, metadata !53, metadata !DIExpression()), !dbg !55
  br label %5, !dbg !67, !llvm.loop !68

11:                                               ; preds = %5
  br label %12, !dbg !79

12:                                               ; preds = %11, %3
  %.0 = phi i32 [ %0, %3 ], [ %.03, %11 ], !dbg !45
  ret i32 %.0, !dbg !80
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !81 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !86, metadata !DIExpression()), !dbg !87
  %2 = icmp sge i32 %0, 0, !dbg !88
  ret i1 %2, !dbg !87
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !89 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !90, metadata !DIExpression()), !dbg !91
  %2 = icmp sge i32 %0, 0, !dbg !92
  ret i1 %2, !dbg !91
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0) #0 !dbg !93 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !94, metadata !DIExpression()), !dbg !95
  %2 = call i32 @pallas.result.0(), !dbg !96
  %3 = call i32 @fibRec(i32 noundef %0), !dbg !97
  %4 = icmp eq i32 %2, %3, !dbg !98
  ret i1 %4, !dbg !95
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !99 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !102, metadata !DIExpression()), !dbg !103
  call void @llvm.dbg.value(metadata i32 %1, metadata !104, metadata !DIExpression()), !dbg !103
  call void @llvm.dbg.value(metadata i32 %2, metadata !105, metadata !DIExpression()), !dbg !103
  call void @llvm.dbg.value(metadata i32 %3, metadata !106, metadata !DIExpression()), !dbg !103
  %5 = icmp sle i32 2, %3, !dbg !107
  %6 = add nsw i32 %0, 1, !dbg !108
  %7 = icmp sle i32 %3, %6, !dbg !109
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !110
  ret i1 %8, !dbg !103
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !111 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !112, metadata !DIExpression()), !dbg !113
  call void @llvm.dbg.value(metadata i32 %1, metadata !114, metadata !DIExpression()), !dbg !113
  call void @llvm.dbg.value(metadata i32 %2, metadata !115, metadata !DIExpression()), !dbg !113
  call void @llvm.dbg.value(metadata i32 %3, metadata !116, metadata !DIExpression()), !dbg !113
  %5 = sub nsw i32 %3, 1, !dbg !117
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !118
  %7 = icmp eq i32 %2, %6, !dbg !119
  ret i1 %7, !dbg !113
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !120 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !121, metadata !DIExpression()), !dbg !122
  call void @llvm.dbg.value(metadata i32 %1, metadata !123, metadata !DIExpression()), !dbg !122
  call void @llvm.dbg.value(metadata i32 %2, metadata !124, metadata !DIExpression()), !dbg !122
  call void @llvm.dbg.value(metadata i32 %3, metadata !125, metadata !DIExpression()), !dbg !122
  %5 = sub nsw i32 %3, 2, !dbg !126
  %6 = call i32 @fibRec(i32 noundef %5), !dbg !127
  %7 = icmp eq i32 %1, %6, !dbg !128
  ret i1 %7, !dbg !122
}

declare !pallas.specLib !129 i32 @pallas.result.0()

declare !pallas.specLib !130 i1 @pallas.scAnd(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_loop_goto.c", directory: ".", checksumkind: CSK_MD5, checksum: "7670ff15920fac26bf2beefdb4a2eddc")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "b2f215a841b42adb8fed102a8c362d8e")
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
!17 = !{!18, i1 true, !19}
!18 = !{!"pallas.srcLoc", i64 6, i64 1, i64 7, i64 22}
!19 = !{!"pallas.requires", !20, ptr @PALLAS_SPEC_0, !21}
!20 = !{!"pallas.srcLoc", i64 7, i64 5, i64 7, i64 20}
!21 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 8, type: !15)
!22 = !DILocation(line: 0, scope: !12)
!23 = !DILocation(line: 9, column: 11, scope: !24)
!24 = distinct !DILexicalBlock(scope: !12, file: !1, line: 9, column: 9)
!25 = !DILocation(line: 9, column: 9, scope: !12)
!26 = !DILocation(line: 9, column: 19, scope: !27)
!27 = distinct !DILexicalBlock(scope: !24, file: !1, line: 9, column: 17)
!28 = !DILocation(line: 9, column: 53, scope: !29)
!29 = distinct !DILexicalBlock(scope: !24, file: !1, line: 9, column: 36)
!30 = !DILocation(line: 9, column: 45, scope: !29)
!31 = !DILocation(line: 9, column: 67, scope: !29)
!32 = !DILocation(line: 9, column: 59, scope: !29)
!33 = !DILocation(line: 9, column: 57, scope: !29)
!34 = !DILocation(line: 9, column: 38, scope: !29)
!35 = !DILocation(line: 9, scope: !24)
!36 = !DILocation(line: 10, column: 1, scope: !12)
!37 = distinct !DISubprogram(name: "fibIt", scope: !1, file: !1, line: 14, type: !13, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!38 = !{!39, i1 false, !40, !43}
!39 = !{!"pallas.srcLoc", i64 12, i64 1, i64 13, i64 41}
!40 = !{!"pallas.requires", !41, ptr @PALLAS_SPEC_1, !42}
!41 = !{!"pallas.srcLoc", i64 12, i64 5, i64 12, i64 20}
!42 = !DILocalVariable(name: "n", arg: 1, scope: !37, file: !1, line: 14, type: !15)
!43 = !{!"pallas.ensures", !44, ptr @PALLAS_SPEC_2, !42}
!44 = !{!"pallas.srcLoc", i64 13, i64 5, i64 13, i64 39}
!45 = !DILocation(line: 0, scope: !37)
!46 = !DILocation(line: 15, column: 11, scope: !47)
!47 = distinct !DILexicalBlock(scope: !37, file: !1, line: 15, column: 9)
!48 = !DILocation(line: 15, column: 9, scope: !37)
!49 = !DILocation(line: 15, column: 19, scope: !50)
!50 = distinct !DILexicalBlock(scope: !47, file: !1, line: 15, column: 17)
!51 = !DILocalVariable(name: "prevRes", scope: !37, file: !1, line: 16, type: !15)
!52 = !DILocalVariable(name: "res", scope: !37, file: !1, line: 17, type: !15)
!53 = !DILocalVariable(name: "i", scope: !54, file: !1, line: 24, type: !15)
!54 = distinct !DILexicalBlock(scope: !37, file: !1, line: 24, column: 5)
!55 = !DILocation(line: 0, scope: !54)
!56 = !DILocation(line: 24, column: 10, scope: !54)
!57 = !DILocation(line: 24, scope: !54)
!58 = !DILocation(line: 24, column: 23, scope: !59)
!59 = distinct !DILexicalBlock(scope: !54, file: !1, line: 24, column: 5)
!60 = !DILocation(line: 24, column: 5, scope: !54)
!61 = !DILocation(line: 25, column: 27, scope: !62)
!62 = distinct !DILexicalBlock(scope: !59, file: !1, line: 24, column: 34)
!63 = !DILocalVariable(name: "tmp", scope: !62, file: !1, line: 25, type: !15)
!64 = !DILocation(line: 0, scope: !62)
!65 = !DILocation(line: 28, column: 5, scope: !62)
!66 = !DILocation(line: 24, column: 30, scope: !59)
!67 = !DILocation(line: 24, column: 5, scope: !59)
!68 = distinct !{!68, !60, !69, !70, !71}
!69 = !DILocation(line: 28, column: 5, scope: !54)
!70 = !{!"llvm.loop.mustprogress"}
!71 = !{!"pallas.loopInv", !72, !73, !75, !77}
!72 = !{!"pallas.srcLoc", i64 19, i64 5, i64 23, i64 5}
!73 = !{!74, ptr @PALLAS_SPEC_3, !42, !51, !52, !53}
!74 = !{!"pallas.srcLoc", i64 20, i64 5, i64 20, i64 42}
!75 = !{!76, ptr @PALLAS_SPEC_4, !42, !51, !52, !53}
!76 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 38}
!77 = !{!78, ptr @PALLAS_SPEC_5, !42, !51, !52, !53}
!78 = !{!"pallas.srcLoc", i64 22, i64 5, i64 22, i64 42}
!79 = !DILocation(line: 29, column: 5, scope: !37)
!80 = !DILocation(line: 30, column: 1, scope: !37)
!81 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 7, type: !82, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!82 = !DISubroutineType(types: !83)
!83 = !{!84, !15}
!84 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!85 = !{!""}
!86 = !DILocalVariable(name: "n", arg: 1, scope: !81, file: !1, line: 7, type: !15)
!87 = !DILocation(line: 0, scope: !81)
!88 = !DILocation(line: 7, column: 16, scope: !81)
!89 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 12, type: !82, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!90 = !DILocalVariable(name: "n", arg: 1, scope: !89, file: !1, line: 12, type: !15)
!91 = !DILocation(line: 0, scope: !89)
!92 = !DILocation(line: 12, column: 16, scope: !89)
!93 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 13, type: !82, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!94 = !DILocalVariable(name: "n", arg: 1, scope: !93, file: !1, line: 13, type: !15)
!95 = !DILocation(line: 0, scope: !93)
!96 = !DILocation(line: 13, column: 13, scope: !93)
!97 = !DILocation(line: 13, column: 30, scope: !93)
!98 = !DILocation(line: 13, column: 27, scope: !93)
!99 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 20, type: !100, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!100 = !DISubroutineType(types: !101)
!101 = !{!84, !15, !15, !15, !15}
!102 = !DILocalVariable(name: "n", arg: 1, scope: !99, file: !1, line: 20, type: !15)
!103 = !DILocation(line: 0, scope: !99)
!104 = !DILocalVariable(name: "prevRes", arg: 2, scope: !99, file: !1, line: 20, type: !15)
!105 = !DILocalVariable(name: "res", arg: 3, scope: !99, file: !1, line: 20, type: !15)
!106 = !DILocalVariable(name: "i", arg: 4, scope: !99, file: !1, line: 20, type: !15)
!107 = !DILocation(line: 20, column: 27, scope: !99)
!108 = !DILocation(line: 20, column: 39, scope: !99)
!109 = !DILocation(line: 20, column: 35, scope: !99)
!110 = !DILocation(line: 20, column: 20, scope: !99)
!111 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 21, type: !100, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!112 = !DILocalVariable(name: "n", arg: 1, scope: !111, file: !1, line: 21, type: !15)
!113 = !DILocation(line: 0, scope: !111)
!114 = !DILocalVariable(name: "prevRes", arg: 2, scope: !111, file: !1, line: 21, type: !15)
!115 = !DILocalVariable(name: "res", arg: 3, scope: !111, file: !1, line: 21, type: !15)
!116 = !DILocalVariable(name: "i", arg: 4, scope: !111, file: !1, line: 21, type: !15)
!117 = !DILocation(line: 21, column: 35, scope: !111)
!118 = !DILocation(line: 21, column: 27, scope: !111)
!119 = !DILocation(line: 21, column: 24, scope: !111)
!120 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 22, type: !100, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!121 = !DILocalVariable(name: "n", arg: 1, scope: !120, file: !1, line: 22, type: !15)
!122 = !DILocation(line: 0, scope: !120)
!123 = !DILocalVariable(name: "prevRes", arg: 2, scope: !120, file: !1, line: 22, type: !15)
!124 = !DILocalVariable(name: "res", arg: 3, scope: !120, file: !1, line: 22, type: !15)
!125 = !DILocalVariable(name: "i", arg: 4, scope: !120, file: !1, line: 22, type: !15)
!126 = !DILocation(line: 22, column: 39, scope: !120)
!127 = !DILocation(line: 22, column: 31, scope: !120)
!128 = !DILocation(line: 22, column: 28, scope: !120)
!129 = !{!"pallas.result"}
!130 = !{!"pallas.scAnd"}
