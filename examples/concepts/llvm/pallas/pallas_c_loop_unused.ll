; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_loop_unused.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [4 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !21, metadata !DIExpression()), !dbg !22
  call void @llvm.dbg.value(metadata i32 %0, metadata !23, metadata !DIExpression()), !dbg !22
  %2 = add nsw i32 %0, 1, !dbg !24
  call void @llvm.dbg.value(metadata i32 %2, metadata !23, metadata !DIExpression()), !dbg !22
  %3 = sub nsw i32 %2, 1, !dbg !25
  call void @llvm.dbg.value(metadata i32 %3, metadata !23, metadata !DIExpression()), !dbg !22
  %4 = icmp slt i32 %0, 42, !dbg !26
  br i1 %4, label %5, label %6, !dbg !28

5:                                                ; preds = %1
  br label %14, !dbg !29

6:                                                ; preds = %1
  call void @llvm.dbg.value(metadata i32 0, metadata !31, metadata !DIExpression()), !dbg !22
  call void @llvm.dbg.value(metadata i32 0, metadata !32, metadata !DIExpression()), !dbg !34
  br label %7, !dbg !35

7:                                                ; preds = %11, %6
  %.02 = phi i32 [ 0, %6 ], [ %10, %11 ], !dbg !22
  %.01 = phi i32 [ 0, %6 ], [ %12, %11 ], !dbg !36
  call void @llvm.dbg.value(metadata i32 %.01, metadata !32, metadata !DIExpression()), !dbg !34
  call void @llvm.dbg.value(metadata i32 %.02, metadata !31, metadata !DIExpression()), !dbg !22
  %8 = icmp sle i32 %.01, %0, !dbg !37
  br i1 %8, label %9, label %13, !dbg !39

9:                                                ; preds = %7
  %10 = add nsw i32 %.02, %.01, !dbg !40
  call void @llvm.dbg.value(metadata i32 %10, metadata !31, metadata !DIExpression()), !dbg !22
  br label %11, !dbg !42

11:                                               ; preds = %9
  %12 = add nsw i32 %.01, 1, !dbg !43
  call void @llvm.dbg.value(metadata i32 %12, metadata !32, metadata !DIExpression()), !dbg !34
  br label %7, !dbg !44, !llvm.loop !45

13:                                               ; preds = %7
  br label %14, !dbg !56

14:                                               ; preds = %13, %5
  %.0 = phi i32 [ %0, %5 ], [ %.02, %13 ], !dbg !22
  ret i32 %.0, !dbg !57
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !58 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !63, metadata !DIExpression()), !dbg !64
  %2 = icmp sge i32 %0, 0, !dbg !65
  ret i1 %2, !dbg !64
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !66 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !69, metadata !DIExpression()), !dbg !70
  call void @llvm.dbg.value(metadata i32 %1, metadata !71, metadata !DIExpression()), !dbg !70
  call void @llvm.dbg.value(metadata i32 %2, metadata !72, metadata !DIExpression()), !dbg !70
  call void @llvm.dbg.value(metadata i32 %3, metadata !73, metadata !DIExpression()), !dbg !70
  %5 = icmp eq i32 %1, %0, !dbg !74
  ret i1 %5, !dbg !70
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !75 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !76, metadata !DIExpression()), !dbg !77
  call void @llvm.dbg.value(metadata i32 %1, metadata !78, metadata !DIExpression()), !dbg !77
  call void @llvm.dbg.value(metadata i32 %2, metadata !79, metadata !DIExpression()), !dbg !77
  call void @llvm.dbg.value(metadata i32 %3, metadata !80, metadata !DIExpression()), !dbg !77
  %5 = icmp sge i32 %2, 0, !dbg !81
  ret i1 %5, !dbg !77
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !82 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !83, metadata !DIExpression()), !dbg !84
  call void @llvm.dbg.value(metadata i32 %1, metadata !85, metadata !DIExpression()), !dbg !84
  call void @llvm.dbg.value(metadata i32 %2, metadata !86, metadata !DIExpression()), !dbg !84
  call void @llvm.dbg.value(metadata i32 %3, metadata !87, metadata !DIExpression()), !dbg !84
  %5 = icmp sle i32 0, %3, !dbg !88
  br i1 %5, label %6, label %9, !dbg !89

6:                                                ; preds = %4
  %7 = add nsw i32 %0, 1, !dbg !90
  %8 = icmp sle i32 %3, %7, !dbg !91
  br label %9

9:                                                ; preds = %6, %4
  %10 = phi i1 [ false, %4 ], [ %8, %6 ], !dbg !84
  ret i1 %10, !dbg !84
}

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_loop_unused.c", directory: ".", checksumkind: CSK_MD5, checksum: "67104b87daf0bcc4264fcc6d168b3db1")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "a98407805655147725deb8c55f55fc4c")
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
!17 = !{!18, i1 false, !19}
!18 = !{!"pallas.srcLoc", i64 9, i64 1, i64 11, i64 1}
!19 = !{!"pallas.requires", !20, ptr @PALLAS_SPEC_0, !21}
!20 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 16}
!21 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 12, type: !15)
!22 = !DILocation(line: 0, scope: !12)
!23 = !DILocalVariable(name: "oldN", scope: !12, file: !1, line: 13, type: !15)
!24 = !DILocation(line: 14, column: 10, scope: !12)
!25 = !DILocation(line: 15, column: 10, scope: !12)
!26 = !DILocation(line: 16, column: 11, scope: !27)
!27 = distinct !DILexicalBlock(scope: !12, file: !1, line: 16, column: 9)
!28 = !DILocation(line: 16, column: 9, scope: !12)
!29 = !DILocation(line: 17, column: 9, scope: !30)
!30 = distinct !DILexicalBlock(scope: !27, file: !1, line: 16, column: 17)
!31 = !DILocalVariable(name: "res", scope: !12, file: !1, line: 19, type: !15)
!32 = !DILocalVariable(name: "i", scope: !33, file: !1, line: 25, type: !15)
!33 = distinct !DILexicalBlock(scope: !12, file: !1, line: 25, column: 5)
!34 = !DILocation(line: 0, scope: !33)
!35 = !DILocation(line: 25, column: 10, scope: !33)
!36 = !DILocation(line: 25, scope: !33)
!37 = !DILocation(line: 25, column: 23, scope: !38)
!38 = distinct !DILexicalBlock(scope: !33, file: !1, line: 25, column: 5)
!39 = !DILocation(line: 25, column: 5, scope: !33)
!40 = !DILocation(line: 26, column: 13, scope: !41)
!41 = distinct !DILexicalBlock(scope: !38, file: !1, line: 25, column: 34)
!42 = !DILocation(line: 27, column: 5, scope: !41)
!43 = !DILocation(line: 25, column: 30, scope: !38)
!44 = !DILocation(line: 25, column: 5, scope: !38)
!45 = distinct !{!45, !39, !46, !47, !48}
!46 = !DILocation(line: 27, column: 5, scope: !33)
!47 = !{!"llvm.loop.mustprogress"}
!48 = !{!"pallas.loopInv", !49, !50, !52, !54}
!49 = !{!"pallas.srcLoc", i64 20, i64 5, i64 24, i64 5}
!50 = !{!51, ptr @PALLAS_SPEC_1, !21, !23, !31, !32}
!51 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 40}
!52 = !{!53, ptr @PALLAS_SPEC_2, !21, !23, !31, !32}
!53 = !{!"pallas.srcLoc", i64 22, i64 5, i64 22, i64 29}
!54 = !{!55, ptr @PALLAS_SPEC_3, !21, !23, !31, !32}
!55 = !{!"pallas.srcLoc", i64 23, i64 5, i64 23, i64 29}
!56 = !DILocation(line: 28, column: 5, scope: !12)
!57 = !DILocation(line: 29, column: 1, scope: !12)
!58 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 10, type: !59, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!59 = !DISubroutineType(types: !60)
!60 = !{!61, !15}
!61 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!62 = !{!""}
!63 = !DILocalVariable(name: "n", arg: 1, scope: !58, file: !1, line: 10, type: !15)
!64 = !DILocation(line: 0, scope: !58)
!65 = !DILocation(line: 10, column: 12, scope: !58)
!66 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 23, type: !67, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!67 = !DISubroutineType(types: !68)
!68 = !{!61, !15, !15, !15, !15}
!69 = !DILocalVariable(name: "n", arg: 1, scope: !66, file: !1, line: 23, type: !15)
!70 = !DILocation(line: 0, scope: !66)
!71 = !DILocalVariable(name: "oldN", arg: 2, scope: !66, file: !1, line: 23, type: !15)
!72 = !DILocalVariable(name: "res", arg: 3, scope: !66, file: !1, line: 23, type: !15)
!73 = !DILocalVariable(name: "i", arg: 4, scope: !66, file: !1, line: 23, type: !15)
!74 = !DILocation(line: 23, column: 25, scope: !66)
!75 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 22, type: !67, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!76 = !DILocalVariable(name: "n", arg: 1, scope: !75, file: !1, line: 22, type: !15)
!77 = !DILocation(line: 0, scope: !75)
!78 = !DILocalVariable(name: "oldN", arg: 2, scope: !75, file: !1, line: 22, type: !15)
!79 = !DILocalVariable(name: "res", arg: 3, scope: !75, file: !1, line: 22, type: !15)
!80 = !DILocalVariable(name: "i", arg: 4, scope: !75, file: !1, line: 22, type: !15)
!81 = !DILocation(line: 22, column: 24, scope: !75)
!82 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 21, type: !67, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!83 = !DILocalVariable(name: "n", arg: 1, scope: !82, file: !1, line: 21, type: !15)
!84 = !DILocation(line: 0, scope: !82)
!85 = !DILocalVariable(name: "oldN", arg: 2, scope: !82, file: !1, line: 21, type: !15)
!86 = !DILocalVariable(name: "res", arg: 3, scope: !82, file: !1, line: 21, type: !15)
!87 = !DILocalVariable(name: "i", arg: 4, scope: !82, file: !1, line: 21, type: !15)
!88 = !DILocation(line: 21, column: 22, scope: !82)
!89 = !DILocation(line: 21, column: 27, scope: !82)
!90 = !DILocation(line: 21, column: 37, scope: !82)
!91 = !DILocation(line: 21, column: 32, scope: !82)
