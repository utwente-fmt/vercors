; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_assert_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [4 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !21, metadata !DIExpression()), !dbg !25
  call void @llvm.dbg.value(metadata i32 %1, metadata !22, metadata !DIExpression()), !dbg !25
  call void @llvm.dbg.value(metadata i32 %0, metadata !26, metadata !DIExpression()), !dbg !25
  %3 = icmp sgt i32 %0, %1, !dbg !27, !pallas.stmntBlock !29
  br i1 %3, label %4, label %6, !dbg !33

4:                                                ; preds = %2
  %5 = add nsw i32 %0, 1, !dbg !34
  call void @llvm.dbg.value(metadata i32 %5, metadata !21, metadata !DIExpression()), !dbg !25
  br label %8, !dbg !36

6:                                                ; preds = %2
  %7 = add nsw i32 %1, 1, !dbg !37
  call void @llvm.dbg.value(metadata i32 %7, metadata !22, metadata !DIExpression()), !dbg !25
  br label %8

8:                                                ; preds = %6, %4
  %.01 = phi i32 [ %1, %4 ], [ %7, %6 ]
  %.0 = phi i32 [ %5, %4 ], [ %0, %6 ]
  call void @llvm.dbg.value(metadata i32 %.0, metadata !21, metadata !DIExpression()), !dbg !25
  call void @llvm.dbg.value(metadata i32 %.01, metadata !22, metadata !DIExpression()), !dbg !25
  %9 = add nsw i32 %0, %.01, !dbg !39, !pallas.stmntBlock !40
  call void @llvm.dbg.value(metadata i32 %9, metadata !26, metadata !DIExpression()), !dbg !25
  %10 = add nsw i32 %.0, %.01, !dbg !44
  ret i32 %10, !dbg !45
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !46 !pallas.exprWrapper !50 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !51, metadata !DIExpression()), !dbg !52
  call void @llvm.dbg.value(metadata i32 %1, metadata !53, metadata !DIExpression()), !dbg !52
  %3 = icmp sgt i32 %0, 0, !dbg !54
  br i1 %3, label %4, label %6, !dbg !55

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !56
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !52
  ret i1 %7, !dbg !52
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !57 !pallas.exprWrapper !50 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !58, metadata !DIExpression()), !dbg !59
  call void @llvm.dbg.value(metadata i32 %1, metadata !60, metadata !DIExpression()), !dbg !59
  %3 = call i32 @pallas.result.0(), !dbg !61
  %4 = icmp sgt i32 %3, 0, !dbg !62
  ret i1 %4, !dbg !59
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !63 !pallas.exprWrapper !50 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !66, metadata !DIExpression()), !dbg !67
  call void @llvm.dbg.value(metadata i32 %1, metadata !68, metadata !DIExpression()), !dbg !67
  call void @llvm.dbg.value(metadata i32 %2, metadata !69, metadata !DIExpression()), !dbg !67
  %4 = icmp sgt i32 %2, 0, !dbg !70
  ret i1 %4, !dbg !67
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !71 !pallas.exprWrapper !50 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !72, metadata !DIExpression()), !dbg !73
  call void @llvm.dbg.value(metadata i32 %1, metadata !74, metadata !DIExpression()), !dbg !73
  call void @llvm.dbg.value(metadata i32 %2, metadata !75, metadata !DIExpression()), !dbg !73
  %4 = add nsw i32 %0, %1, !dbg !76
  %5 = icmp sgt i32 %2, %4, !dbg !77
  ret i1 %5, !dbg !73
}

declare !pallas.specLib !78 i32 @pallas.result.0()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_assert_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "455f5e90dde899bbae9b8c904f093ec8")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "1c234a459fdce3fbdbfdd8accef04f96")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 14, type: !13, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, !19, !23}
!18 = !{!"pallas.srcLoc", i64 10, i64 1, i64 13, i64 1}
!19 = !{!"pallas.requires", !20, ptr @PALLAS_SPEC_0, !21, !22}
!20 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 24}
!21 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 14, type: !15)
!22 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 14, type: !15)
!23 = !{!"pallas.ensures", !24, ptr @PALLAS_SPEC_1, !21, !22}
!24 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 26}
!25 = !DILocation(line: 0, scope: !12)
!26 = !DILocalVariable(name: "tmp", scope: !12, file: !1, line: 15, type: !15)
!27 = !DILocation(line: 17, column: 11, scope: !28)
!28 = distinct !DILexicalBlock(scope: !12, file: !1, line: 17, column: 9)
!29 = !{!30, !31}
!30 = !{!"pallas.srcLoc", i64 16, i64 5, i64 16, i64 25}
!31 = !{!"pallas.assert", !32, ptr @PALLAS_SPEC_2, !21, !22, !26}
!32 = !{!"pallas.srcLoc", i64 16, i64 9, i64 16, i64 23}
!33 = !DILocation(line: 17, column: 9, scope: !12)
!34 = !DILocation(line: 18, column: 10, scope: !35)
!35 = distinct !DILexicalBlock(scope: !28, file: !1, line: 17, column: 16)
!36 = !DILocation(line: 19, column: 5, scope: !35)
!37 = !DILocation(line: 20, column: 10, scope: !38)
!38 = distinct !DILexicalBlock(scope: !28, file: !1, line: 19, column: 12)
!39 = !DILocation(line: 23, column: 9, scope: !12)
!40 = !{!41, !42}
!41 = !{!"pallas.srcLoc", i64 22, i64 5, i64 22, i64 29}
!42 = !{!"pallas.assert", !43, ptr @PALLAS_SPEC_3, !21, !22, !26}
!43 = !{!"pallas.srcLoc", i64 22, i64 9, i64 22, i64 27}
!44 = !DILocation(line: 24, column: 14, scope: !12)
!45 = !DILocation(line: 24, column: 5, scope: !12)
!46 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 11, type: !47, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!47 = !DISubroutineType(types: !48)
!48 = !{!49, !15, !15}
!49 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!50 = !{!""}
!51 = !DILocalVariable(name: "a", arg: 1, scope: !46, file: !1, line: 11, type: !15)
!52 = !DILocation(line: 0, scope: !46)
!53 = !DILocalVariable(name: "b", arg: 2, scope: !46, file: !1, line: 11, type: !15)
!54 = !DILocation(line: 11, column: 12, scope: !46)
!55 = !DILocation(line: 11, column: 16, scope: !46)
!56 = !DILocation(line: 11, column: 21, scope: !46)
!57 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 12, type: !47, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!58 = !DILocalVariable(name: "a", arg: 1, scope: !57, file: !1, line: 12, type: !15)
!59 = !DILocation(line: 0, scope: !57)
!60 = !DILocalVariable(name: "b", arg: 2, scope: !57, file: !1, line: 12, type: !15)
!61 = !DILocation(line: 12, column: 10, scope: !57)
!62 = !DILocation(line: 12, column: 23, scope: !57)
!63 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 16, type: !64, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!64 = !DISubroutineType(types: !65)
!65 = !{!49, !15, !15, !15}
!66 = !DILocalVariable(name: "a", arg: 1, scope: !63, file: !1, line: 16, type: !15)
!67 = !DILocation(line: 0, scope: !63)
!68 = !DILocalVariable(name: "b", arg: 2, scope: !63, file: !1, line: 16, type: !15)
!69 = !DILocalVariable(name: "tmp", arg: 3, scope: !63, file: !1, line: 16, type: !15)
!70 = !DILocation(line: 16, column: 20, scope: !63)
!71 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 22, type: !64, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!72 = !DILocalVariable(name: "a", arg: 1, scope: !71, file: !1, line: 22, type: !15)
!73 = !DILocation(line: 0, scope: !71)
!74 = !DILocalVariable(name: "b", arg: 2, scope: !71, file: !1, line: 22, type: !15)
!75 = !DILocalVariable(name: "tmp", arg: 3, scope: !71, file: !1, line: 22, type: !15)
!76 = !DILocation(line: 22, column: 24, scope: !71)
!77 = !DILocation(line: 22, column: 20, scope: !71)
!78 = !{!"pallas.result"}
