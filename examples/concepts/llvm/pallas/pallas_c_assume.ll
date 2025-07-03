; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_assume.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [3 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !18 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !22, metadata !DIExpression()), !dbg !24
  call void @llvm.dbg.value(metadata i32 %1, metadata !23, metadata !DIExpression()), !dbg !24
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !24
  %3 = icmp sgt i32 %0, %1, !dbg !26, !pallas.stmntBlock !28
  br i1 %3, label %4, label %6, !dbg !32

4:                                                ; preds = %2
  %5 = add nsw i32 %0, 1, !dbg !33
  call void @llvm.dbg.value(metadata i32 %5, metadata !22, metadata !DIExpression()), !dbg !24
  br label %8, !dbg !35

6:                                                ; preds = %2
  %7 = add nsw i32 %1, 1, !dbg !36
  call void @llvm.dbg.value(metadata i32 %7, metadata !23, metadata !DIExpression()), !dbg !24
  br label %8

8:                                                ; preds = %6, %4
  %.01 = phi i32 [ %1, %4 ], [ %7, %6 ]
  %.0 = phi i32 [ %5, %4 ], [ %0, %6 ]
  call void @llvm.dbg.value(metadata i32 %.0, metadata !22, metadata !DIExpression()), !dbg !24
  call void @llvm.dbg.value(metadata i32 %.01, metadata !23, metadata !DIExpression()), !dbg !24
  %9 = add nsw i32 %0, %.01, !dbg !38, !pallas.stmntBlock !39
  call void @llvm.dbg.value(metadata i32 %9, metadata !25, metadata !DIExpression()), !dbg !24
  %10 = add nsw i32 %.0, %.01, !dbg !43
  ret i32 %10, !dbg !44
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !45 !pallas.exprWrapper !49 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !50, metadata !DIExpression()), !dbg !51
  call void @llvm.dbg.value(metadata i32 %1, metadata !52, metadata !DIExpression()), !dbg !51
  %3 = icmp sgt i32 %0, 0, !dbg !53
  br i1 %3, label %4, label %6, !dbg !54

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !55
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !51
  ret i1 %7, !dbg !51
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !56 !pallas.exprWrapper !49 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !59, metadata !DIExpression()), !dbg !60
  call void @llvm.dbg.value(metadata i32 %1, metadata !61, metadata !DIExpression()), !dbg !60
  call void @llvm.dbg.value(metadata i32 %2, metadata !62, metadata !DIExpression()), !dbg !60
  %4 = icmp slt i32 %2, 0, !dbg !63
  ret i1 %4, !dbg !60
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !64 !pallas.exprWrapper !49 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !65, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.value(metadata i32 %1, metadata !67, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.value(metadata i32 %2, metadata !68, metadata !DIExpression()), !dbg !66
  ret i1 false, !dbg !66
}

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_assume.c", directory: ".", checksumkind: CSK_MD5, checksum: "00c1312a5879ddcb9e3d548233a7e7d7")
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
!12 = distinct !DISubprogram(name: "foo", scope: !13, file: !13, line: 13, type: !14, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!13 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_assume.c", directory: "", checksumkind: CSK_MD5, checksum: "00c1312a5879ddcb9e3d548233a7e7d7")
!14 = !DISubroutineType(types: !15)
!15 = !{!16, !16, !16}
!16 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!17 = !{}
!18 = !{!19, i1 false, i1 false, !20}
!19 = !{!"pallas.srcLoc", i64 10, i64 1, i64 12, i64 1, !13}
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !22, !23}
!21 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 24, !13}
!22 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !13, line: 13, type: !16)
!23 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !13, line: 13, type: !16)
!24 = !DILocation(line: 0, scope: !12)
!25 = !DILocalVariable(name: "tmp", scope: !12, file: !13, line: 14, type: !16)
!26 = !DILocation(line: 19, column: 11, scope: !27)
!27 = distinct !DILexicalBlock(scope: !12, file: !13, line: 19, column: 9)
!28 = !{!29, !30}
!29 = !{!"pallas.srcLoc", i64 16, i64 5, i64 18, i64 5, !13}
!30 = !{!"pallas.assume", !31, ptr @PALLAS_SPEC_1, !22, !23, !25}
!31 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 19, !13}
!32 = !DILocation(line: 19, column: 9, scope: !12)
!33 = !DILocation(line: 20, column: 10, scope: !34)
!34 = distinct !DILexicalBlock(scope: !27, file: !13, line: 19, column: 16)
!35 = !DILocation(line: 21, column: 5, scope: !34)
!36 = !DILocation(line: 22, column: 10, scope: !37)
!37 = distinct !DILexicalBlock(scope: !27, file: !13, line: 21, column: 12)
!38 = !DILocation(line: 25, column: 9, scope: !12)
!39 = !{!40, !41}
!40 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 23, !13}
!41 = !{!"pallas.assert", !42, ptr @PALLAS_SPEC_2, !22, !23, !25}
!42 = !{!"pallas.srcLoc", i64 24, i64 9, i64 24, i64 21, !13}
!43 = !DILocation(line: 26, column: 14, scope: !12)
!44 = !DILocation(line: 26, column: 5, scope: !12)
!45 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !13, file: !13, line: 11, type: !46, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!46 = !DISubroutineType(types: !47)
!47 = !{!48, !16, !16}
!48 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!49 = !{!""}
!50 = !DILocalVariable(name: "a", arg: 1, scope: !45, file: !13, line: 11, type: !16)
!51 = !DILocation(line: 0, scope: !45)
!52 = !DILocalVariable(name: "b", arg: 2, scope: !45, file: !13, line: 11, type: !16)
!53 = !DILocation(line: 11, column: 12, scope: !45)
!54 = !DILocation(line: 11, column: 16, scope: !45)
!55 = !DILocation(line: 11, column: 21, scope: !45)
!56 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !13, file: !13, line: 17, type: !57, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!57 = !DISubroutineType(types: !58)
!58 = !{!48, !16, !16, !16}
!59 = !DILocalVariable(name: "a", arg: 1, scope: !56, file: !13, line: 17, type: !16)
!60 = !DILocation(line: 0, scope: !56)
!61 = !DILocalVariable(name: "b", arg: 2, scope: !56, file: !13, line: 17, type: !16)
!62 = !DILocalVariable(name: "tmp", arg: 3, scope: !56, file: !13, line: 17, type: !16)
!63 = !DILocation(line: 17, column: 16, scope: !56)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !13, file: !13, line: 24, type: !57, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!65 = !DILocalVariable(name: "a", arg: 1, scope: !64, file: !13, line: 24, type: !16)
!66 = !DILocation(line: 0, scope: !64)
!67 = !DILocalVariable(name: "b", arg: 2, scope: !64, file: !13, line: 24, type: !16)
!68 = !DILocalVariable(name: "tmp", arg: 3, scope: !64, file: !13, line: 24, type: !16)
