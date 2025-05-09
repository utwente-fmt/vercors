; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_assume.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [3 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !21, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.value(metadata i32 %1, metadata !22, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.value(metadata i32 %0, metadata !24, metadata !DIExpression()), !dbg !23
  %3 = icmp sgt i32 %0, %1, !dbg !25, !pallas.stmntBlock !27
  br i1 %3, label %4, label %6, !dbg !31

4:                                                ; preds = %2
  %5 = add nsw i32 %0, 1, !dbg !32
  call void @llvm.dbg.value(metadata i32 %5, metadata !21, metadata !DIExpression()), !dbg !23
  br label %8, !dbg !34

6:                                                ; preds = %2
  %7 = add nsw i32 %1, 1, !dbg !35
  call void @llvm.dbg.value(metadata i32 %7, metadata !22, metadata !DIExpression()), !dbg !23
  br label %8

8:                                                ; preds = %6, %4
  %.01 = phi i32 [ %1, %4 ], [ %7, %6 ]
  %.0 = phi i32 [ %5, %4 ], [ %0, %6 ]
  call void @llvm.dbg.value(metadata i32 %.0, metadata !21, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.value(metadata i32 %.01, metadata !22, metadata !DIExpression()), !dbg !23
  %9 = add nsw i32 %0, %.01, !dbg !37, !pallas.stmntBlock !38
  call void @llvm.dbg.value(metadata i32 %9, metadata !24, metadata !DIExpression()), !dbg !23
  %10 = add nsw i32 %.0, %.01, !dbg !42
  ret i32 %10, !dbg !43
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !44 !pallas.exprWrapper !48 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !49, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.value(metadata i32 %1, metadata !51, metadata !DIExpression()), !dbg !50
  %3 = icmp sgt i32 %0, 0, !dbg !52
  br i1 %3, label %4, label %6, !dbg !53

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !54
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !50
  ret i1 %7, !dbg !50
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !55 !pallas.exprWrapper !48 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !58, metadata !DIExpression()), !dbg !59
  call void @llvm.dbg.value(metadata i32 %1, metadata !60, metadata !DIExpression()), !dbg !59
  call void @llvm.dbg.value(metadata i32 %2, metadata !61, metadata !DIExpression()), !dbg !59
  %4 = icmp slt i32 %2, 0, !dbg !62
  ret i1 %4, !dbg !59
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !63 !pallas.exprWrapper !48 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !64, metadata !DIExpression()), !dbg !65
  call void @llvm.dbg.value(metadata i32 %1, metadata !66, metadata !DIExpression()), !dbg !65
  call void @llvm.dbg.value(metadata i32 %2, metadata !67, metadata !DIExpression()), !dbg !65
  ret i1 false, !dbg !65
}

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_assume.c", directory: ".", checksumkind: CSK_MD5, checksum: "00c1312a5879ddcb9e3d548233a7e7d7")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "484103b55637f643c5d225521294a8c5")
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
!17 = !{!18, i1 false, !19}
!18 = !{!"pallas.srcLoc", i64 10, i64 1, i64 12, i64 1}
!19 = !{!"pallas.requires", !20, ptr @PALLAS_SPEC_0, !21, !22}
!20 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 24}
!21 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 13, type: !15)
!22 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 13, type: !15)
!23 = !DILocation(line: 0, scope: !12)
!24 = !DILocalVariable(name: "tmp", scope: !12, file: !1, line: 14, type: !15)
!25 = !DILocation(line: 19, column: 11, scope: !26)
!26 = distinct !DILexicalBlock(scope: !12, file: !1, line: 19, column: 9)
!27 = !{!28, !29}
!28 = !{!"pallas.srcLoc", i64 16, i64 5, i64 18, i64 5}
!29 = !{!"pallas.assume", !30, ptr @PALLAS_SPEC_1, !21, !22, !24}
!30 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 19}
!31 = !DILocation(line: 19, column: 9, scope: !12)
!32 = !DILocation(line: 20, column: 10, scope: !33)
!33 = distinct !DILexicalBlock(scope: !26, file: !1, line: 19, column: 16)
!34 = !DILocation(line: 21, column: 5, scope: !33)
!35 = !DILocation(line: 22, column: 10, scope: !36)
!36 = distinct !DILexicalBlock(scope: !26, file: !1, line: 21, column: 12)
!37 = !DILocation(line: 25, column: 9, scope: !12)
!38 = !{!39, !40}
!39 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 23}
!40 = !{!"pallas.assert", !41, ptr @PALLAS_SPEC_2, !21, !22, !24}
!41 = !{!"pallas.srcLoc", i64 24, i64 9, i64 24, i64 21}
!42 = !DILocation(line: 26, column: 14, scope: !12)
!43 = !DILocation(line: 26, column: 5, scope: !12)
!44 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 11, type: !45, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!45 = !DISubroutineType(types: !46)
!46 = !{!47, !15, !15}
!47 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!48 = !{!""}
!49 = !DILocalVariable(name: "a", arg: 1, scope: !44, file: !1, line: 11, type: !15)
!50 = !DILocation(line: 0, scope: !44)
!51 = !DILocalVariable(name: "b", arg: 2, scope: !44, file: !1, line: 11, type: !15)
!52 = !DILocation(line: 11, column: 12, scope: !44)
!53 = !DILocation(line: 11, column: 16, scope: !44)
!54 = !DILocation(line: 11, column: 21, scope: !44)
!55 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 17, type: !56, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!56 = !DISubroutineType(types: !57)
!57 = !{!47, !15, !15, !15}
!58 = !DILocalVariable(name: "a", arg: 1, scope: !55, file: !1, line: 17, type: !15)
!59 = !DILocation(line: 0, scope: !55)
!60 = !DILocalVariable(name: "b", arg: 2, scope: !55, file: !1, line: 17, type: !15)
!61 = !DILocalVariable(name: "tmp", arg: 3, scope: !55, file: !1, line: 17, type: !15)
!62 = !DILocation(line: 17, column: 16, scope: !55)
!63 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 24, type: !56, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!64 = !DILocalVariable(name: "a", arg: 1, scope: !63, file: !1, line: 24, type: !15)
!65 = !DILocation(line: 0, scope: !63)
!66 = !DILocalVariable(name: "b", arg: 2, scope: !63, file: !1, line: 24, type: !15)
!67 = !DILocalVariable(name: "tmp", arg: 3, scope: !63, file: !1, line: 24, type: !15)
