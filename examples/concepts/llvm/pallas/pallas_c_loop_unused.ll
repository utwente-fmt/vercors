; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_loop_unused.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [4 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_3], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !24, metadata !DIExpression()), !dbg !30
  call void @llvm.dbg.value(metadata i32 %0, metadata !31, metadata !DIExpression()), !dbg !30
  %2 = add nsw i32 %0, 1, !dbg !32
  call void @llvm.dbg.value(metadata i32 %2, metadata !31, metadata !DIExpression()), !dbg !30
  %3 = sub nsw i32 %2, 1, !dbg !33
  call void @llvm.dbg.value(metadata i32 %3, metadata !31, metadata !DIExpression()), !dbg !30
  %4 = icmp slt i32 %0, 42, !dbg !34
  br i1 %4, label %5, label %6, !dbg !36

5:                                                ; preds = %1
  br label %14, !dbg !37

6:                                                ; preds = %1
  call void @llvm.dbg.value(metadata i32 0, metadata !39, metadata !DIExpression()), !dbg !30
  call void @llvm.dbg.value(metadata i32 0, metadata !40, metadata !DIExpression()), !dbg !42
  br label %7, !dbg !43

7:                                                ; preds = %11, %6
  %.02 = phi i32 [ 0, %6 ], [ %10, %11 ], !dbg !30
  %.01 = phi i32 [ 0, %6 ], [ %12, %11 ], !dbg !44
  call void @llvm.dbg.value(metadata i32 %.01, metadata !40, metadata !DIExpression()), !dbg !42
  call void @llvm.dbg.value(metadata i32 %.02, metadata !39, metadata !DIExpression()), !dbg !30
  %8 = icmp sle i32 %.01, %0, !dbg !45
  br i1 %8, label %9, label %13, !dbg !47

9:                                                ; preds = %7
  %10 = add nsw i32 %.02, %.01, !dbg !48
  call void @llvm.dbg.value(metadata i32 %10, metadata !39, metadata !DIExpression()), !dbg !30
  br label %11, !dbg !50

11:                                               ; preds = %9
  %12 = add nsw i32 %.01, 1, !dbg !51
  call void @llvm.dbg.value(metadata i32 %12, metadata !40, metadata !DIExpression()), !dbg !42
  br label %7, !dbg !52, !llvm.loop !53

13:                                               ; preds = %7
  br label %14, !dbg !96

14:                                               ; preds = %13, %5
  %.0 = phi i32 [ %0, %5 ], [ %.02, %13 ], !dbg !30
  ret i32 %.0, !dbg !97
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !26 !pallas.exprWrapper !98 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !99
  %2 = icmp sge i32 %0, 0, !dbg !100
  ret i1 %2, !dbg !99
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !77 !pallas.exprWrapper !98 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !76, metadata !DIExpression()), !dbg !101
  call void @llvm.dbg.value(metadata i32 %1, metadata !79, metadata !DIExpression()), !dbg !101
  call void @llvm.dbg.value(metadata i32 %2, metadata !81, metadata !DIExpression()), !dbg !101
  call void @llvm.dbg.value(metadata i32 %3, metadata !83, metadata !DIExpression()), !dbg !101
  %5 = icmp sge i32 %2, 0, !dbg !102
  ret i1 %5, !dbg !101
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !63 !pallas.exprWrapper !98 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !62, metadata !DIExpression()), !dbg !103
  call void @llvm.dbg.value(metadata i32 %1, metadata !67, metadata !DIExpression()), !dbg !103
  call void @llvm.dbg.value(metadata i32 %2, metadata !69, metadata !DIExpression()), !dbg !103
  call void @llvm.dbg.value(metadata i32 %3, metadata !71, metadata !DIExpression()), !dbg !103
  %5 = icmp sle i32 0, %3, !dbg !104
  br i1 %5, label %6, label %9, !dbg !105

6:                                                ; preds = %4
  %7 = add nsw i32 %0, 1, !dbg !106
  %8 = icmp sle i32 %3, %7, !dbg !107
  br label %9

9:                                                ; preds = %6, %4
  %10 = phi i1 [ false, %4 ], [ %8, %6 ], !dbg !103
  ret i1 %10, !dbg !103
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !89 !pallas.exprWrapper !98 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !88, metadata !DIExpression()), !dbg !108
  call void @llvm.dbg.value(metadata i32 %1, metadata !91, metadata !DIExpression()), !dbg !108
  call void @llvm.dbg.value(metadata i32 %2, metadata !93, metadata !DIExpression()), !dbg !108
  call void @llvm.dbg.value(metadata i32 %3, metadata !95, metadata !DIExpression()), !dbg !108
  %5 = icmp eq i32 %1, %0, !dbg !109
  ret i1 %5, !dbg !108
}

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_loop_unused.c", directory: ".", checksumkind: CSK_MD5, checksum: "67104b87daf0bcc4264fcc6d168b3db1")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "7f50a7d38a126e21e5fe1225a429e26e")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 12, type: !13, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, i1 false, !16, !16, !20}
!18 = !{!"pallas.srcLoc", i64 9, i64 1, i64 11, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_loop_unused.c", directory: "", checksumkind: CSK_MD5, checksum: "67104b87daf0bcc4264fcc6d168b3db1")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !16, !16, !22}
!21 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 16, !19}
!22 = !{!23}
!23 = !{!24, !25}
!24 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 12, type: !15)
!25 = !DILocalVariable(name: "n", arg: 1, scope: !26, file: !1, line: 10, type: !15)
!26 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 10, type: !27, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!27 = !DISubroutineType(types: !28)
!28 = !{!29, !15}
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !DILocation(line: 0, scope: !12)
!31 = !DILocalVariable(name: "oldN", scope: !12, file: !1, line: 13, type: !15)
!32 = !DILocation(line: 14, column: 10, scope: !12)
!33 = !DILocation(line: 15, column: 10, scope: !12)
!34 = !DILocation(line: 16, column: 11, scope: !35)
!35 = distinct !DILexicalBlock(scope: !12, file: !1, line: 16, column: 9)
!36 = !DILocation(line: 16, column: 9, scope: !12)
!37 = !DILocation(line: 17, column: 9, scope: !38)
!38 = distinct !DILexicalBlock(scope: !35, file: !1, line: 16, column: 17)
!39 = !DILocalVariable(name: "res", scope: !12, file: !1, line: 19, type: !15)
!40 = !DILocalVariable(name: "i", scope: !41, file: !1, line: 25, type: !15)
!41 = distinct !DILexicalBlock(scope: !12, file: !1, line: 25, column: 5)
!42 = !DILocation(line: 0, scope: !41)
!43 = !DILocation(line: 25, column: 10, scope: !41)
!44 = !DILocation(line: 25, scope: !41)
!45 = !DILocation(line: 25, column: 23, scope: !46)
!46 = distinct !DILexicalBlock(scope: !41, file: !1, line: 25, column: 5)
!47 = !DILocation(line: 25, column: 5, scope: !41)
!48 = !DILocation(line: 26, column: 13, scope: !49)
!49 = distinct !DILexicalBlock(scope: !46, file: !1, line: 25, column: 34)
!50 = !DILocation(line: 27, column: 5, scope: !49)
!51 = !DILocation(line: 25, column: 30, scope: !46)
!52 = !DILocation(line: 25, column: 5, scope: !46)
!53 = distinct !{!53, !47, !54, !55, !56}
!54 = !DILocation(line: 27, column: 5, scope: !41)
!55 = !{!"llvm.loop.mustprogress"}
!56 = !{!"pallas.loopInvBlock", !57, !58, !72, !84}
!57 = !{!"pallas.srcLoc", i64 20, i64 5, i64 24, i64 5, !19}
!58 = !{!"pallas.loopInv", !59, ptr @PALLAS_SPEC_1, !16, !16, !60}
!59 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 40, !19}
!60 = !{!61, !66, !68, !70}
!61 = !{!24, !62}
!62 = !DILocalVariable(name: "n", arg: 1, scope: !63, file: !1, line: 21, type: !15)
!63 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 21, type: !64, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!64 = !DISubroutineType(types: !65)
!65 = !{!29, !15, !15, !15, !15}
!66 = !{!31, !67}
!67 = !DILocalVariable(name: "oldN", arg: 2, scope: !63, file: !1, line: 21, type: !15)
!68 = !{!39, !69}
!69 = !DILocalVariable(name: "res", arg: 3, scope: !63, file: !1, line: 21, type: !15)
!70 = !{!40, !71}
!71 = !DILocalVariable(name: "i", arg: 4, scope: !63, file: !1, line: 21, type: !15)
!72 = !{!"pallas.loopInv", !73, ptr @PALLAS_SPEC_2, !16, !16, !74}
!73 = !{!"pallas.srcLoc", i64 22, i64 5, i64 22, i64 29, !19}
!74 = !{!75, !78, !80, !82}
!75 = !{!24, !76}
!76 = !DILocalVariable(name: "n", arg: 1, scope: !77, file: !1, line: 22, type: !15)
!77 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 22, type: !64, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!78 = !{!31, !79}
!79 = !DILocalVariable(name: "oldN", arg: 2, scope: !77, file: !1, line: 22, type: !15)
!80 = !{!39, !81}
!81 = !DILocalVariable(name: "res", arg: 3, scope: !77, file: !1, line: 22, type: !15)
!82 = !{!40, !83}
!83 = !DILocalVariable(name: "i", arg: 4, scope: !77, file: !1, line: 22, type: !15)
!84 = !{!"pallas.loopInv", !85, ptr @PALLAS_SPEC_3, !16, !16, !86}
!85 = !{!"pallas.srcLoc", i64 23, i64 5, i64 23, i64 29, !19}
!86 = !{!87, !90, !92, !94}
!87 = !{!24, !88}
!88 = !DILocalVariable(name: "n", arg: 1, scope: !89, file: !1, line: 23, type: !15)
!89 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 23, type: !64, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!90 = !{!31, !91}
!91 = !DILocalVariable(name: "oldN", arg: 2, scope: !89, file: !1, line: 23, type: !15)
!92 = !{!39, !93}
!93 = !DILocalVariable(name: "res", arg: 3, scope: !89, file: !1, line: 23, type: !15)
!94 = !{!40, !95}
!95 = !DILocalVariable(name: "i", arg: 4, scope: !89, file: !1, line: 23, type: !15)
!96 = !DILocation(line: 28, column: 5, scope: !12)
!97 = !DILocation(line: 29, column: 1, scope: !12)
!98 = !{!""}
!99 = !DILocation(line: 0, scope: !26)
!100 = !DILocation(line: 10, column: 12, scope: !26)
!101 = !DILocation(line: 0, scope: !77)
!102 = !DILocation(line: 22, column: 24, scope: !77)
!103 = !DILocation(line: 0, scope: !63)
!104 = !DILocation(line: 21, column: 22, scope: !63)
!105 = !DILocation(line: 21, column: 27, scope: !63)
!106 = !DILocation(line: 21, column: 37, scope: !63)
!107 = !DILocation(line: 21, column: 32, scope: !63)
!108 = !DILocation(line: 0, scope: !89)
!109 = !DILocation(line: 23, column: 25, scope: !89)
