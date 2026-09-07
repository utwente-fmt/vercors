; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_assume.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [3 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !24, metadata !DIExpression()), !dbg !33
  call void @llvm.dbg.value(metadata i32 %1, metadata !31, metadata !DIExpression()), !dbg !33
  call void @llvm.dbg.value(metadata i32 %0, metadata !34, metadata !DIExpression()), !dbg !33
  %3 = icmp sgt i32 %0, %1, !dbg !35, !pallas.stmntBlock !37
  br i1 %3, label %4, label %6, !dbg !51

4:                                                ; preds = %2
  %5 = add nsw i32 %0, 1, !dbg !52
  call void @llvm.dbg.value(metadata i32 %5, metadata !24, metadata !DIExpression()), !dbg !33
  br label %8, !dbg !54

6:                                                ; preds = %2
  %7 = add nsw i32 %1, 1, !dbg !55
  call void @llvm.dbg.value(metadata i32 %7, metadata !31, metadata !DIExpression()), !dbg !33
  br label %8

8:                                                ; preds = %6, %4
  %.01 = phi i32 [ %1, %4 ], [ %7, %6 ]
  %.0 = phi i32 [ %5, %4 ], [ %0, %6 ]
  call void @llvm.dbg.value(metadata i32 %.0, metadata !24, metadata !DIExpression()), !dbg !33
  call void @llvm.dbg.value(metadata i32 %.01, metadata !31, metadata !DIExpression()), !dbg !33
  %9 = add nsw i32 %0, %.01, !dbg !57, !pallas.stmntBlock !58
  call void @llvm.dbg.value(metadata i32 %9, metadata !34, metadata !DIExpression()), !dbg !33
  %10 = add nsw i32 %.0, %.01, !dbg !70
  ret i32 %10, !dbg !71
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !26 !pallas.exprWrapper !72 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !73
  call void @llvm.dbg.value(metadata i32 %1, metadata !32, metadata !DIExpression()), !dbg !73
  %3 = icmp sgt i32 %0, 0, !dbg !74
  br i1 %3, label %4, label %6, !dbg !75

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !76
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !73
  ret i1 %7, !dbg !73
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !44 !pallas.exprWrapper !72 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !43, metadata !DIExpression()), !dbg !77
  call void @llvm.dbg.value(metadata i32 %1, metadata !48, metadata !DIExpression()), !dbg !77
  call void @llvm.dbg.value(metadata i32 %2, metadata !50, metadata !DIExpression()), !dbg !77
  %4 = icmp slt i32 %2, 0, !dbg !78
  ret i1 %4, !dbg !77
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !65 !pallas.exprWrapper !72 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !64, metadata !DIExpression()), !dbg !79
  call void @llvm.dbg.value(metadata i32 %1, metadata !67, metadata !DIExpression()), !dbg !79
  call void @llvm.dbg.value(metadata i32 %2, metadata !69, metadata !DIExpression()), !dbg !79
  ret i1 false, !dbg !79
}

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_assume.c", directory: ".", checksumkind: CSK_MD5, checksum: "00c1312a5879ddcb9e3d548233a7e7d7")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "438b62d260bb8837cd3b90afd68d4372")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 13, type: !13, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, i1 false, !16, !16, !20}
!18 = !{!"pallas.srcLoc", i64 10, i64 1, i64 12, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_assume.c", directory: "", checksumkind: CSK_MD5, checksum: "00c1312a5879ddcb9e3d548233a7e7d7")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !16, !16, !22}
!21 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 24, !19}
!22 = !{!23, !30}
!23 = !{!24, !25}
!24 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 13, type: !15)
!25 = !DILocalVariable(name: "a", arg: 1, scope: !26, file: !1, line: 11, type: !15)
!26 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 11, type: !27, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!27 = !DISubroutineType(types: !28)
!28 = !{!29, !15, !15}
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !{!31, !32}
!31 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 13, type: !15)
!32 = !DILocalVariable(name: "b", arg: 2, scope: !26, file: !1, line: 11, type: !15)
!33 = !DILocation(line: 0, scope: !12)
!34 = !DILocalVariable(name: "tmp", scope: !12, file: !1, line: 14, type: !15)
!35 = !DILocation(line: 19, column: 11, scope: !36)
!36 = distinct !DILexicalBlock(scope: !12, file: !1, line: 19, column: 9)
!37 = !{!38, !39}
!38 = !{!"pallas.srcLoc", i64 16, i64 5, i64 18, i64 5, !19}
!39 = !{!"pallas.assume", !40, ptr @PALLAS_SPEC_1, !16, !16, !41}
!40 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 19, !19}
!41 = !{!42, !47, !49}
!42 = !{!24, !43}
!43 = !DILocalVariable(name: "a", arg: 1, scope: !44, file: !1, line: 17, type: !15)
!44 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 17, type: !45, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!45 = !DISubroutineType(types: !46)
!46 = !{!29, !15, !15, !15}
!47 = !{!31, !48}
!48 = !DILocalVariable(name: "b", arg: 2, scope: !44, file: !1, line: 17, type: !15)
!49 = !{!34, !50}
!50 = !DILocalVariable(name: "tmp", arg: 3, scope: !44, file: !1, line: 17, type: !15)
!51 = !DILocation(line: 19, column: 9, scope: !12)
!52 = !DILocation(line: 20, column: 10, scope: !53)
!53 = distinct !DILexicalBlock(scope: !36, file: !1, line: 19, column: 16)
!54 = !DILocation(line: 21, column: 5, scope: !53)
!55 = !DILocation(line: 22, column: 10, scope: !56)
!56 = distinct !DILexicalBlock(scope: !36, file: !1, line: 21, column: 12)
!57 = !DILocation(line: 25, column: 9, scope: !12)
!58 = !{!59, !60}
!59 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 23, !19}
!60 = !{!"pallas.assert", !61, ptr @PALLAS_SPEC_2, !16, !16, !62}
!61 = !{!"pallas.srcLoc", i64 24, i64 9, i64 24, i64 21, !19}
!62 = !{!63, !66, !68}
!63 = !{!24, !64}
!64 = !DILocalVariable(name: "a", arg: 1, scope: !65, file: !1, line: 24, type: !15)
!65 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 24, type: !45, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!66 = !{!31, !67}
!67 = !DILocalVariable(name: "b", arg: 2, scope: !65, file: !1, line: 24, type: !15)
!68 = !{!34, !69}
!69 = !DILocalVariable(name: "tmp", arg: 3, scope: !65, file: !1, line: 24, type: !15)
!70 = !DILocation(line: 26, column: 14, scope: !12)
!71 = !DILocation(line: 26, column: 5, scope: !12)
!72 = !{!""}
!73 = !DILocation(line: 0, scope: !26)
!74 = !DILocation(line: 11, column: 12, scope: !26)
!75 = !DILocation(line: 11, column: 16, scope: !26)
!76 = !DILocation(line: 11, column: 21, scope: !26)
!77 = !DILocation(line: 0, scope: !44)
!78 = !DILocation(line: 17, column: 16, scope: !44)
!79 = !DILocation(line: 0, scope: !65)
