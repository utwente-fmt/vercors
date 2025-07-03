; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_loop_unused.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [4 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !18 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !22, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.value(metadata i32 %0, metadata !24, metadata !DIExpression()), !dbg !23
  %2 = add nsw i32 %0, 1, !dbg !25
  call void @llvm.dbg.value(metadata i32 %2, metadata !24, metadata !DIExpression()), !dbg !23
  %3 = sub nsw i32 %2, 1, !dbg !26
  call void @llvm.dbg.value(metadata i32 %3, metadata !24, metadata !DIExpression()), !dbg !23
  %4 = icmp slt i32 %0, 42, !dbg !27
  br i1 %4, label %5, label %6, !dbg !29

5:                                                ; preds = %1
  br label %14, !dbg !30

6:                                                ; preds = %1
  call void @llvm.dbg.value(metadata i32 0, metadata !32, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.value(metadata i32 0, metadata !33, metadata !DIExpression()), !dbg !35
  br label %7, !dbg !36

7:                                                ; preds = %11, %6
  %.02 = phi i32 [ 0, %6 ], [ %10, %11 ], !dbg !23
  %.01 = phi i32 [ 0, %6 ], [ %12, %11 ], !dbg !37
  call void @llvm.dbg.value(metadata i32 %.01, metadata !33, metadata !DIExpression()), !dbg !35
  call void @llvm.dbg.value(metadata i32 %.02, metadata !32, metadata !DIExpression()), !dbg !23
  %8 = icmp sle i32 %.01, %0, !dbg !38
  br i1 %8, label %9, label %13, !dbg !40

9:                                                ; preds = %7
  %10 = add nsw i32 %.02, %.01, !dbg !41
  call void @llvm.dbg.value(metadata i32 %10, metadata !32, metadata !DIExpression()), !dbg !23
  br label %11, !dbg !43

11:                                               ; preds = %9
  %12 = add nsw i32 %.01, 1, !dbg !44
  call void @llvm.dbg.value(metadata i32 %12, metadata !33, metadata !DIExpression()), !dbg !35
  br label %7, !dbg !45, !llvm.loop !46

13:                                               ; preds = %7
  br label %14, !dbg !57

14:                                               ; preds = %13, %5
  %.0 = phi i32 [ %0, %5 ], [ %.02, %13 ], !dbg !23
  ret i32 %.0, !dbg !58
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !59 !pallas.exprWrapper !63 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !64, metadata !DIExpression()), !dbg !65
  %2 = icmp sge i32 %0, 0, !dbg !66
  ret i1 %2, !dbg !65
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !67 !pallas.exprWrapper !63 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !70, metadata !DIExpression()), !dbg !71
  call void @llvm.dbg.value(metadata i32 %1, metadata !72, metadata !DIExpression()), !dbg !71
  call void @llvm.dbg.value(metadata i32 %2, metadata !73, metadata !DIExpression()), !dbg !71
  call void @llvm.dbg.value(metadata i32 %3, metadata !74, metadata !DIExpression()), !dbg !71
  %5 = icmp sle i32 0, %3, !dbg !75
  br i1 %5, label %6, label %9, !dbg !76

6:                                                ; preds = %4
  %7 = add nsw i32 %0, 1, !dbg !77
  %8 = icmp sle i32 %3, %7, !dbg !78
  br label %9

9:                                                ; preds = %6, %4
  %10 = phi i1 [ false, %4 ], [ %8, %6 ], !dbg !71
  ret i1 %10, !dbg !71
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !79 !pallas.exprWrapper !63 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !80, metadata !DIExpression()), !dbg !81
  call void @llvm.dbg.value(metadata i32 %1, metadata !82, metadata !DIExpression()), !dbg !81
  call void @llvm.dbg.value(metadata i32 %2, metadata !83, metadata !DIExpression()), !dbg !81
  call void @llvm.dbg.value(metadata i32 %3, metadata !84, metadata !DIExpression()), !dbg !81
  %5 = icmp sge i32 %2, 0, !dbg !85
  ret i1 %5, !dbg !81
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !86 !pallas.exprWrapper !63 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !87, metadata !DIExpression()), !dbg !88
  call void @llvm.dbg.value(metadata i32 %1, metadata !89, metadata !DIExpression()), !dbg !88
  call void @llvm.dbg.value(metadata i32 %2, metadata !90, metadata !DIExpression()), !dbg !88
  call void @llvm.dbg.value(metadata i32 %3, metadata !91, metadata !DIExpression()), !dbg !88
  %5 = icmp eq i32 %1, %0, !dbg !92
  ret i1 %5, !dbg !88
}

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_loop_unused.c", directory: ".", checksumkind: CSK_MD5, checksum: "67104b87daf0bcc4264fcc6d168b3db1")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "d28113e15d1f62c0f6d8240fa261d236")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "foo", scope: !13, file: !13, line: 12, type: !14, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!13 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_loop_unused.c", directory: "", checksumkind: CSK_MD5, checksum: "67104b87daf0bcc4264fcc6d168b3db1")
!14 = !DISubroutineType(types: !15)
!15 = !{!16, !16}
!16 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!17 = !{}
!18 = !{!19, i1 false, i1 false, !20}
!19 = !{!"pallas.srcLoc", i64 9, i64 1, i64 11, i64 1, !13}
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !22}
!21 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 16, !13}
!22 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !13, line: 12, type: !16)
!23 = !DILocation(line: 0, scope: !12)
!24 = !DILocalVariable(name: "oldN", scope: !12, file: !13, line: 13, type: !16)
!25 = !DILocation(line: 14, column: 10, scope: !12)
!26 = !DILocation(line: 15, column: 10, scope: !12)
!27 = !DILocation(line: 16, column: 11, scope: !28)
!28 = distinct !DILexicalBlock(scope: !12, file: !13, line: 16, column: 9)
!29 = !DILocation(line: 16, column: 9, scope: !12)
!30 = !DILocation(line: 17, column: 9, scope: !31)
!31 = distinct !DILexicalBlock(scope: !28, file: !13, line: 16, column: 17)
!32 = !DILocalVariable(name: "res", scope: !12, file: !13, line: 19, type: !16)
!33 = !DILocalVariable(name: "i", scope: !34, file: !13, line: 25, type: !16)
!34 = distinct !DILexicalBlock(scope: !12, file: !13, line: 25, column: 5)
!35 = !DILocation(line: 0, scope: !34)
!36 = !DILocation(line: 25, column: 10, scope: !34)
!37 = !DILocation(line: 25, scope: !34)
!38 = !DILocation(line: 25, column: 23, scope: !39)
!39 = distinct !DILexicalBlock(scope: !34, file: !13, line: 25, column: 5)
!40 = !DILocation(line: 25, column: 5, scope: !34)
!41 = !DILocation(line: 26, column: 13, scope: !42)
!42 = distinct !DILexicalBlock(scope: !39, file: !13, line: 25, column: 34)
!43 = !DILocation(line: 27, column: 5, scope: !42)
!44 = !DILocation(line: 25, column: 30, scope: !39)
!45 = !DILocation(line: 25, column: 5, scope: !39)
!46 = distinct !{!46, !40, !47, !48, !49}
!47 = !DILocation(line: 27, column: 5, scope: !34)
!48 = !{!"llvm.loop.mustprogress"}
!49 = !{!"pallas.loopInv", !50, !51, !53, !55}
!50 = !{!"pallas.srcLoc", i64 20, i64 5, i64 24, i64 5, !13}
!51 = !{!52, ptr @PALLAS_SPEC_1, !22, !24, !32, !33}
!52 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 40, !13}
!53 = !{!54, ptr @PALLAS_SPEC_2, !22, !24, !32, !33}
!54 = !{!"pallas.srcLoc", i64 22, i64 5, i64 22, i64 29, !13}
!55 = !{!56, ptr @PALLAS_SPEC_3, !22, !24, !32, !33}
!56 = !{!"pallas.srcLoc", i64 23, i64 5, i64 23, i64 29, !13}
!57 = !DILocation(line: 28, column: 5, scope: !12)
!58 = !DILocation(line: 29, column: 1, scope: !12)
!59 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !13, file: !13, line: 10, type: !60, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!60 = !DISubroutineType(types: !61)
!61 = !{!62, !16}
!62 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!63 = !{!""}
!64 = !DILocalVariable(name: "n", arg: 1, scope: !59, file: !13, line: 10, type: !16)
!65 = !DILocation(line: 0, scope: !59)
!66 = !DILocation(line: 10, column: 12, scope: !59)
!67 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !13, file: !13, line: 21, type: !68, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!68 = !DISubroutineType(types: !69)
!69 = !{!62, !16, !16, !16, !16}
!70 = !DILocalVariable(name: "n", arg: 1, scope: !67, file: !13, line: 21, type: !16)
!71 = !DILocation(line: 0, scope: !67)
!72 = !DILocalVariable(name: "oldN", arg: 2, scope: !67, file: !13, line: 21, type: !16)
!73 = !DILocalVariable(name: "res", arg: 3, scope: !67, file: !13, line: 21, type: !16)
!74 = !DILocalVariable(name: "i", arg: 4, scope: !67, file: !13, line: 21, type: !16)
!75 = !DILocation(line: 21, column: 22, scope: !67)
!76 = !DILocation(line: 21, column: 27, scope: !67)
!77 = !DILocation(line: 21, column: 37, scope: !67)
!78 = !DILocation(line: 21, column: 32, scope: !67)
!79 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !13, file: !13, line: 22, type: !68, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!80 = !DILocalVariable(name: "n", arg: 1, scope: !79, file: !13, line: 22, type: !16)
!81 = !DILocation(line: 0, scope: !79)
!82 = !DILocalVariable(name: "oldN", arg: 2, scope: !79, file: !13, line: 22, type: !16)
!83 = !DILocalVariable(name: "res", arg: 3, scope: !79, file: !13, line: 22, type: !16)
!84 = !DILocalVariable(name: "i", arg: 4, scope: !79, file: !13, line: 22, type: !16)
!85 = !DILocation(line: 22, column: 24, scope: !79)
!86 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !13, file: !13, line: 23, type: !68, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!87 = !DILocalVariable(name: "n", arg: 1, scope: !86, file: !13, line: 23, type: !16)
!88 = !DILocation(line: 0, scope: !86)
!89 = !DILocalVariable(name: "oldN", arg: 2, scope: !86, file: !13, line: 23, type: !16)
!90 = !DILocalVariable(name: "res", arg: 3, scope: !86, file: !13, line: 23, type: !16)
!91 = !DILocalVariable(name: "i", arg: 4, scope: !86, file: !13, line: 23, type: !16)
!92 = !DILocation(line: 23, column: 25, scope: !86)
