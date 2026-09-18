; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_assert_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [4 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !24, metadata !DIExpression()), !dbg !41
  call void @llvm.dbg.value(metadata i32 %1, metadata !31, metadata !DIExpression()), !dbg !41
  call void @llvm.dbg.value(metadata i32 %0, metadata !42, metadata !DIExpression()), !dbg !41
  %3 = icmp sgt i32 %0, %1, !dbg !43, !pallas.stmntBlock !45
  br i1 %3, label %4, label %6, !dbg !59

4:                                                ; preds = %2
  %5 = add nsw i32 %0, 1, !dbg !60
  call void @llvm.dbg.value(metadata i32 %5, metadata !24, metadata !DIExpression()), !dbg !41
  br label %8, !dbg !62

6:                                                ; preds = %2
  %7 = add nsw i32 %1, 1, !dbg !63
  call void @llvm.dbg.value(metadata i32 %7, metadata !31, metadata !DIExpression()), !dbg !41
  br label %8

8:                                                ; preds = %6, %4
  %.01 = phi i32 [ %1, %4 ], [ %7, %6 ]
  %.0 = phi i32 [ %5, %4 ], [ %0, %6 ]
  call void @llvm.dbg.value(metadata i32 %.0, metadata !24, metadata !DIExpression()), !dbg !41
  call void @llvm.dbg.value(metadata i32 %.01, metadata !31, metadata !DIExpression()), !dbg !41
  %9 = add nsw i32 %0, %.01, !dbg !65, !pallas.stmntBlock !66
  call void @llvm.dbg.value(metadata i32 %9, metadata !42, metadata !DIExpression()), !dbg !41
  %10 = add nsw i32 %.0, %.01, !dbg !78
  ret i32 %10, !dbg !79
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !26 !pallas.exprWrapper !80 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !81
  call void @llvm.dbg.value(metadata i32 %1, metadata !32, metadata !DIExpression()), !dbg !81
  %3 = icmp sgt i32 %0, 0, !dbg !82
  br i1 %3, label %4, label %6, !dbg !83

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !84
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !81
  ret i1 %7, !dbg !81
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !38 !pallas.exprWrapper !80 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !37, metadata !DIExpression()), !dbg !85
  call void @llvm.dbg.value(metadata i32 %1, metadata !40, metadata !DIExpression()), !dbg !85
  %3 = call i32 @"pallas.result i32"(), !dbg !86
  %4 = icmp sgt i32 %3, 0, !dbg !87
  ret i1 %4, !dbg !85
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !52 !pallas.exprWrapper !80 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !51, metadata !DIExpression()), !dbg !88
  call void @llvm.dbg.value(metadata i32 %1, metadata !56, metadata !DIExpression()), !dbg !88
  call void @llvm.dbg.value(metadata i32 %2, metadata !58, metadata !DIExpression()), !dbg !88
  %4 = icmp sgt i32 %2, 0, !dbg !89
  ret i1 %4, !dbg !88
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !73 !pallas.exprWrapper !80 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !72, metadata !DIExpression()), !dbg !90
  call void @llvm.dbg.value(metadata i32 %1, metadata !75, metadata !DIExpression()), !dbg !90
  call void @llvm.dbg.value(metadata i32 %2, metadata !77, metadata !DIExpression()), !dbg !90
  %4 = add nsw i32 %0, %1, !dbg !91
  %5 = icmp sgt i32 %2, %4, !dbg !92
  ret i1 %5, !dbg !90
}

declare !pallas.specLib !93 i32 @"pallas.result i32"()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_assert_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "455f5e90dde899bbae9b8c904f093ec8")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "fc7e9348d2b05a8f444712470f442ff4")
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
!17 = !{!18, i1 false, i1 false, !16, !16, !20, !33}
!18 = !{!"pallas.srcLoc", i64 10, i64 1, i64 13, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_assert_fail.c", directory: "", checksumkind: CSK_MD5, checksum: "455f5e90dde899bbae9b8c904f093ec8")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !16, !16, !22}
!21 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 24, !19}
!22 = !{!23, !30}
!23 = !{!24, !25}
!24 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 14, type: !15)
!25 = !DILocalVariable(name: "a", arg: 1, scope: !26, file: !1, line: 11, type: !15)
!26 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 11, type: !27, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!27 = !DISubroutineType(types: !28)
!28 = !{!29, !15, !15}
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !{!31, !32}
!31 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 14, type: !15)
!32 = !DILocalVariable(name: "b", arg: 2, scope: !26, file: !1, line: 11, type: !15)
!33 = !{!"pallas.ensures", !34, ptr @PALLAS_SPEC_1, !16, !16, !35}
!34 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 26, !19}
!35 = !{!36, !39}
!36 = !{!24, !37}
!37 = !DILocalVariable(name: "a", arg: 1, scope: !38, file: !1, line: 12, type: !15)
!38 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 12, type: !27, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!39 = !{!31, !40}
!40 = !DILocalVariable(name: "b", arg: 2, scope: !38, file: !1, line: 12, type: !15)
!41 = !DILocation(line: 0, scope: !12)
!42 = !DILocalVariable(name: "tmp", scope: !12, file: !1, line: 15, type: !15)
!43 = !DILocation(line: 17, column: 11, scope: !44)
!44 = distinct !DILexicalBlock(scope: !12, file: !1, line: 17, column: 9)
!45 = !{!46, !47}
!46 = !{!"pallas.srcLoc", i64 16, i64 5, i64 16, i64 25, !19}
!47 = !{!"pallas.assert", !48, ptr @PALLAS_SPEC_2, !16, !16, !49}
!48 = !{!"pallas.srcLoc", i64 16, i64 9, i64 16, i64 23, !19}
!49 = !{!50, !55, !57}
!50 = !{!24, !51}
!51 = !DILocalVariable(name: "a", arg: 1, scope: !52, file: !1, line: 16, type: !15)
!52 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 16, type: !53, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!53 = !DISubroutineType(types: !54)
!54 = !{!29, !15, !15, !15}
!55 = !{!31, !56}
!56 = !DILocalVariable(name: "b", arg: 2, scope: !52, file: !1, line: 16, type: !15)
!57 = !{!42, !58}
!58 = !DILocalVariable(name: "tmp", arg: 3, scope: !52, file: !1, line: 16, type: !15)
!59 = !DILocation(line: 17, column: 9, scope: !12)
!60 = !DILocation(line: 18, column: 10, scope: !61)
!61 = distinct !DILexicalBlock(scope: !44, file: !1, line: 17, column: 16)
!62 = !DILocation(line: 19, column: 5, scope: !61)
!63 = !DILocation(line: 20, column: 10, scope: !64)
!64 = distinct !DILexicalBlock(scope: !44, file: !1, line: 19, column: 12)
!65 = !DILocation(line: 23, column: 9, scope: !12)
!66 = !{!67, !68}
!67 = !{!"pallas.srcLoc", i64 22, i64 5, i64 22, i64 29, !19}
!68 = !{!"pallas.assert", !69, ptr @PALLAS_SPEC_3, !16, !16, !70}
!69 = !{!"pallas.srcLoc", i64 22, i64 9, i64 22, i64 27, !19}
!70 = !{!71, !74, !76}
!71 = !{!24, !72}
!72 = !DILocalVariable(name: "a", arg: 1, scope: !73, file: !1, line: 22, type: !15)
!73 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 22, type: !53, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!74 = !{!31, !75}
!75 = !DILocalVariable(name: "b", arg: 2, scope: !73, file: !1, line: 22, type: !15)
!76 = !{!42, !77}
!77 = !DILocalVariable(name: "tmp", arg: 3, scope: !73, file: !1, line: 22, type: !15)
!78 = !DILocation(line: 24, column: 14, scope: !12)
!79 = !DILocation(line: 24, column: 5, scope: !12)
!80 = !{!""}
!81 = !DILocation(line: 0, scope: !26)
!82 = !DILocation(line: 11, column: 12, scope: !26)
!83 = !DILocation(line: 11, column: 16, scope: !26)
!84 = !DILocation(line: 11, column: 21, scope: !26)
!85 = !DILocation(line: 0, scope: !38)
!86 = !DILocation(line: 12, column: 10, scope: !38)
!87 = !DILocation(line: 12, column: 23, scope: !38)
!88 = !DILocation(line: 0, scope: !52)
!89 = !DILocation(line: 16, column: 20, scope: !52)
!90 = !DILocation(line: 0, scope: !73)
!91 = !DILocation(line: 22, column: 24, scope: !73)
!92 = !DILocation(line: 22, column: 20, scope: !73)
!93 = !{!"pallas.result"}
