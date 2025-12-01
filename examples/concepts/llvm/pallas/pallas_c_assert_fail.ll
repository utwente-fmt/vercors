; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_assert_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [4 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_2], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !22, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.value(metadata i32 %1, metadata !23, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.value(metadata i32 %0, metadata !27, metadata !DIExpression()), !dbg !26
  %3 = icmp sgt i32 %0, %1, !dbg !28, !pallas.stmntBlock !30
  br i1 %3, label %4, label %6, !dbg !34

4:                                                ; preds = %2
  %5 = add nsw i32 %0, 1, !dbg !35
  call void @llvm.dbg.value(metadata i32 %5, metadata !22, metadata !DIExpression()), !dbg !26
  br label %8, !dbg !37

6:                                                ; preds = %2
  %7 = add nsw i32 %1, 1, !dbg !38
  call void @llvm.dbg.value(metadata i32 %7, metadata !23, metadata !DIExpression()), !dbg !26
  br label %8

8:                                                ; preds = %6, %4
  %.01 = phi i32 [ %1, %4 ], [ %7, %6 ]
  %.0 = phi i32 [ %5, %4 ], [ %0, %6 ]
  call void @llvm.dbg.value(metadata i32 %.0, metadata !22, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.value(metadata i32 %.01, metadata !23, metadata !DIExpression()), !dbg !26
  %9 = add nsw i32 %0, %.01, !dbg !40, !pallas.stmntBlock !41
  call void @llvm.dbg.value(metadata i32 %9, metadata !27, metadata !DIExpression()), !dbg !26
  %10 = add nsw i32 %.0, %.01, !dbg !45
  ret i32 %10, !dbg !46
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !47 !pallas.exprWrapper !51 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !52, metadata !DIExpression()), !dbg !53
  call void @llvm.dbg.value(metadata i32 %1, metadata !54, metadata !DIExpression()), !dbg !53
  %3 = icmp sgt i32 %0, 0, !dbg !55
  br i1 %3, label %4, label %6, !dbg !56

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !57
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !53
  ret i1 %7, !dbg !53
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !58 !pallas.exprWrapper !51 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !59, metadata !DIExpression()), !dbg !60
  call void @llvm.dbg.value(metadata i32 %1, metadata !61, metadata !DIExpression()), !dbg !60
  %3 = call i32 @"pallas.result i32"(), !dbg !62
  %4 = icmp sgt i32 %3, 0, !dbg !63
  ret i1 %4, !dbg !60
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !64 !pallas.exprWrapper !51 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !67, metadata !DIExpression()), !dbg !68
  call void @llvm.dbg.value(metadata i32 %1, metadata !69, metadata !DIExpression()), !dbg !68
  call void @llvm.dbg.value(metadata i32 %2, metadata !70, metadata !DIExpression()), !dbg !68
  %4 = add nsw i32 %0, %1, !dbg !71
  %5 = icmp sgt i32 %2, %4, !dbg !72
  ret i1 %5, !dbg !68
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !73 !pallas.exprWrapper !51 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !74, metadata !DIExpression()), !dbg !75
  call void @llvm.dbg.value(metadata i32 %1, metadata !76, metadata !DIExpression()), !dbg !75
  call void @llvm.dbg.value(metadata i32 %2, metadata !77, metadata !DIExpression()), !dbg !75
  %4 = icmp sgt i32 %2, 0, !dbg !78
  ret i1 %4, !dbg !75
}

declare !pallas.specLib !79 i32 @"pallas.result i32"()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_assert_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "455f5e90dde899bbae9b8c904f093ec8")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "8dcda8ee677565f59f875b40faeb7f12")
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
!17 = !{!18, i1 false, i1 false, !20, !24}
!18 = !{!"pallas.srcLoc", i64 10, i64 1, i64 13, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_assert_fail.c", directory: "", checksumkind: CSK_MD5, checksum: "455f5e90dde899bbae9b8c904f093ec8")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !22, !23}
!21 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 24, !19}
!22 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 14, type: !15)
!23 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 14, type: !15)
!24 = !{!"pallas.ensures", !25, ptr @PALLAS_SPEC_1, !22, !23}
!25 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 26, !19}
!26 = !DILocation(line: 0, scope: !12)
!27 = !DILocalVariable(name: "tmp", scope: !12, file: !1, line: 15, type: !15)
!28 = !DILocation(line: 17, column: 11, scope: !29)
!29 = distinct !DILexicalBlock(scope: !12, file: !1, line: 17, column: 9)
!30 = !{!31, !32}
!31 = !{!"pallas.srcLoc", i64 16, i64 5, i64 16, i64 25, !19}
!32 = !{!"pallas.assert", !33, ptr @PALLAS_SPEC_2, !22, !23, !27}
!33 = !{!"pallas.srcLoc", i64 16, i64 9, i64 16, i64 23, !19}
!34 = !DILocation(line: 17, column: 9, scope: !12)
!35 = !DILocation(line: 18, column: 10, scope: !36)
!36 = distinct !DILexicalBlock(scope: !29, file: !1, line: 17, column: 16)
!37 = !DILocation(line: 19, column: 5, scope: !36)
!38 = !DILocation(line: 20, column: 10, scope: !39)
!39 = distinct !DILexicalBlock(scope: !29, file: !1, line: 19, column: 12)
!40 = !DILocation(line: 23, column: 9, scope: !12)
!41 = !{!42, !43}
!42 = !{!"pallas.srcLoc", i64 22, i64 5, i64 22, i64 29, !19}
!43 = !{!"pallas.assert", !44, ptr @PALLAS_SPEC_3, !22, !23, !27}
!44 = !{!"pallas.srcLoc", i64 22, i64 9, i64 22, i64 27, !19}
!45 = !DILocation(line: 24, column: 14, scope: !12)
!46 = !DILocation(line: 24, column: 5, scope: !12)
!47 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 11, type: !48, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!48 = !DISubroutineType(types: !49)
!49 = !{!50, !15, !15}
!50 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!51 = !{!""}
!52 = !DILocalVariable(name: "a", arg: 1, scope: !47, file: !1, line: 11, type: !15)
!53 = !DILocation(line: 0, scope: !47)
!54 = !DILocalVariable(name: "b", arg: 2, scope: !47, file: !1, line: 11, type: !15)
!55 = !DILocation(line: 11, column: 12, scope: !47)
!56 = !DILocation(line: 11, column: 16, scope: !47)
!57 = !DILocation(line: 11, column: 21, scope: !47)
!58 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 12, type: !48, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!59 = !DILocalVariable(name: "a", arg: 1, scope: !58, file: !1, line: 12, type: !15)
!60 = !DILocation(line: 0, scope: !58)
!61 = !DILocalVariable(name: "b", arg: 2, scope: !58, file: !1, line: 12, type: !15)
!62 = !DILocation(line: 12, column: 10, scope: !58)
!63 = !DILocation(line: 12, column: 23, scope: !58)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 22, type: !65, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!65 = !DISubroutineType(types: !66)
!66 = !{!50, !15, !15, !15}
!67 = !DILocalVariable(name: "a", arg: 1, scope: !64, file: !1, line: 22, type: !15)
!68 = !DILocation(line: 0, scope: !64)
!69 = !DILocalVariable(name: "b", arg: 2, scope: !64, file: !1, line: 22, type: !15)
!70 = !DILocalVariable(name: "tmp", arg: 3, scope: !64, file: !1, line: 22, type: !15)
!71 = !DILocation(line: 22, column: 24, scope: !64)
!72 = !DILocation(line: 22, column: 20, scope: !64)
!73 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 16, type: !65, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!74 = !DILocalVariable(name: "a", arg: 1, scope: !73, file: !1, line: 16, type: !15)
!75 = !DILocation(line: 0, scope: !73)
!76 = !DILocalVariable(name: "b", arg: 2, scope: !73, file: !1, line: 16, type: !15)
!77 = !DILocalVariable(name: "tmp", arg: 3, scope: !73, file: !1, line: 16, type: !15)
!78 = !DILocation(line: 16, column: 20, scope: !73)
!79 = !{!"pallas.result"}
