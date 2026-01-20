; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/ghost/pallas_ghost_func.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_4, ptr @ghost_mult], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @my_mult(i32 noundef %0, i32 noundef %1) #0 !dbg !14 !pallas.fcontract !19 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !23, metadata !DIExpression()), !dbg !27
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !24, metadata !DIExpression()), !dbg !28
  call void @llvm.dbg.declare(metadata ptr %5, metadata !29, metadata !DIExpression()), !dbg !30
  store i32 0, ptr %5, align 4, !dbg !30
  call void @llvm.dbg.declare(metadata ptr %6, metadata !31, metadata !DIExpression()), !dbg !33
  store i32 0, ptr %6, align 4, !dbg !33
  br label %7, !dbg !34

7:                                                ; preds = %15, %2
  %8 = load i32, ptr %6, align 4, !dbg !35
  %9 = load i32, ptr %4, align 4, !dbg !37
  %10 = icmp slt i32 %8, %9, !dbg !38
  br i1 %10, label %11, label %18, !dbg !39

11:                                               ; preds = %7
  %12 = load i32, ptr %3, align 4, !dbg !40
  %13 = load i32, ptr %5, align 4, !dbg !42
  %14 = add nsw i32 %13, %12, !dbg !42
  store i32 %14, ptr %5, align 4, !dbg !42
  br label %15, !dbg !43

15:                                               ; preds = %11
  %16 = load i32, ptr %6, align 4, !dbg !44
  %17 = add nsw i32 %16, 1, !dbg !44
  store i32 %17, ptr %6, align 4, !dbg !44
  br label %7, !dbg !45, !llvm.loop !46

18:                                               ; preds = %7
  %19 = load i32, ptr %5, align 4, !dbg !55
  ret i32 %19, !dbg !56
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !57 !pallas.exprWrapper !61 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !62, metadata !DIExpression()), !dbg !63
  call void @llvm.dbg.value(metadata i32 %1, metadata !64, metadata !DIExpression()), !dbg !63
  %3 = icmp sge i32 %0, 0, !dbg !65
  br i1 %3, label %4, label %6, !dbg !66

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !67
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !63
  ret i1 %7, !dbg !63
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !68 !pallas.exprWrapper !61 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !69, metadata !DIExpression()), !dbg !70
  call void @llvm.dbg.value(metadata i32 %1, metadata !71, metadata !DIExpression()), !dbg !70
  %3 = call i32 @"pallas.result i32"(), !dbg !72
  %4 = call i32 @ghost_mult(i32 noundef %0, i32 noundef %1), !dbg !73
  %5 = icmp eq i32 %3, %4, !dbg !74
  ret i1 %5, !dbg !70
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !75 !pallas.exprWrapper !61 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !78, metadata !DIExpression()), !dbg !79
  call void @llvm.dbg.value(metadata i32 %1, metadata !80, metadata !DIExpression()), !dbg !79
  call void @llvm.dbg.value(metadata i32 %2, metadata !81, metadata !DIExpression()), !dbg !79
  call void @llvm.dbg.value(metadata i32 %3, metadata !82, metadata !DIExpression()), !dbg !79
  %5 = mul nsw i32 %3, %0, !dbg !83
  %6 = icmp eq i32 %2, %5, !dbg !84
  ret i1 %6, !dbg !79
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !85 !pallas.exprWrapper !61 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !86, metadata !DIExpression()), !dbg !87
  call void @llvm.dbg.value(metadata i32 %1, metadata !88, metadata !DIExpression()), !dbg !87
  call void @llvm.dbg.value(metadata i32 %2, metadata !89, metadata !DIExpression()), !dbg !87
  call void @llvm.dbg.value(metadata i32 %3, metadata !90, metadata !DIExpression()), !dbg !87
  %5 = icmp sle i32 0, %3, !dbg !91
  br i1 %5, label %6, label %8, !dbg !92

6:                                                ; preds = %4
  %7 = icmp sle i32 %3, %1, !dbg !93
  br label %8

8:                                                ; preds = %6, %4
  %9 = phi i1 [ false, %4 ], [ %7, %6 ], !dbg !87
  ret i1 %9, !dbg !87
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1) #0 !dbg !94 !pallas.exprWrapper !61 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !95, metadata !DIExpression()), !dbg !96
  call void @llvm.dbg.value(metadata i32 %1, metadata !97, metadata !DIExpression()), !dbg !96
  %3 = icmp sge i32 %0, 0, !dbg !98
  br i1 %3, label %4, label %6, !dbg !99

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !100
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !96
  ret i1 %7, !dbg !96
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @ghost_mult(i32 noundef %0, i32 noundef %1) #0 !dbg !101 !pallas.ghost !61 !pallas.ghostContract !102 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !105, metadata !DIExpression()), !dbg !106
  call void @llvm.dbg.value(metadata i32 %1, metadata !107, metadata !DIExpression()), !dbg !106
  %3 = mul nsw i32 %0, %1, !dbg !108
  ret i32 %3, !dbg !109
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !110 i32 @"pallas.result i32"()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2, !4}
!llvm.module.flags = !{!6, !7, !8, !9, !10, !11, !12}
!llvm.ident = !{!13, !13}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/ghost/pallas_ghost_func.c", directory: ".", checksumkind: CSK_MD5, checksum: "beb09737e9a38b4634976cee631b7fc8")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "be4504a25eba79d48b1c32c7df525ee1")
!4 = distinct !DICompileUnit(language: DW_LANG_C, file: !5, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!5 = !DIFile(filename: "examples/concepts/llvm/pallas/ghost/pallas_ghost_func.c", directory: ".")
!6 = !{i32 7, !"Dwarf Version", i32 5}
!7 = !{i32 2, !"Debug Info Version", i32 3}
!8 = !{i32 1, !"wchar_size", i32 4}
!9 = !{i32 8, !"PIC Level", i32 2}
!10 = !{i32 7, !"PIE Level", i32 2}
!11 = !{i32 7, !"uwtable", i32 2}
!12 = !{i32 7, !"frame-pointer", i32 2}
!13 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!14 = distinct !DISubprogram(name: "my_mult", scope: !1, file: !1, line: 17, type: !15, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !18)
!15 = !DISubroutineType(types: !16)
!16 = !{!17, !17, !17}
!17 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!18 = !{}
!19 = !{!20, i1 false, i1 false, !21, !25}
!20 = !{!"pallas.srcLoc", i64 13, i64 1, i64 16, i64 1, !1}
!21 = !{!"pallas.requires", !22, ptr @PALLAS_SPEC_0, !23, !24}
!22 = !{!"pallas.srcLoc", i64 14, i64 1, i64 14, i64 26, !1}
!23 = !DILocalVariable(name: "a", arg: 1, scope: !14, file: !1, line: 17, type: !17)
!24 = !DILocalVariable(name: "b", arg: 2, scope: !14, file: !1, line: 17, type: !17)
!25 = !{!"pallas.ensures", !26, ptr @PALLAS_SPEC_1, !23, !24}
!26 = !{!"pallas.srcLoc", i64 15, i64 1, i64 15, i64 41, !1}
!27 = !DILocation(line: 17, column: 17, scope: !14)
!28 = !DILocation(line: 17, column: 24, scope: !14)
!29 = !DILocalVariable(name: "res", scope: !14, file: !1, line: 18, type: !17)
!30 = !DILocation(line: 18, column: 9, scope: !14)
!31 = !DILocalVariable(name: "i", scope: !32, file: !1, line: 23, type: !17)
!32 = distinct !DILexicalBlock(scope: !14, file: !1, line: 23, column: 5)
!33 = !DILocation(line: 23, column: 14, scope: !32)
!34 = !DILocation(line: 23, column: 10, scope: !32)
!35 = !DILocation(line: 23, column: 21, scope: !36)
!36 = distinct !DILexicalBlock(scope: !32, file: !1, line: 23, column: 5)
!37 = !DILocation(line: 23, column: 25, scope: !36)
!38 = !DILocation(line: 23, column: 23, scope: !36)
!39 = !DILocation(line: 23, column: 5, scope: !32)
!40 = !DILocation(line: 24, column: 16, scope: !41)
!41 = distinct !DILexicalBlock(scope: !36, file: !1, line: 23, column: 33)
!42 = !DILocation(line: 24, column: 13, scope: !41)
!43 = !DILocation(line: 25, column: 5, scope: !41)
!44 = !DILocation(line: 23, column: 29, scope: !36)
!45 = !DILocation(line: 23, column: 5, scope: !36)
!46 = distinct !{!46, !39, !47, !48, !49}
!47 = !DILocation(line: 25, column: 5, scope: !32)
!48 = !{!"llvm.loop.mustprogress"}
!49 = !{!"pallas.loopInv", !50, !51, !53}
!50 = !{!"pallas.srcLoc", i64 19, i64 5, i64 22, i64 5, !1}
!51 = !{!52, ptr @PALLAS_SPEC_2, !23, !24, !29, !31}
!52 = !{!"pallas.srcLoc", i64 20, i64 5, i64 20, i64 36, !1}
!53 = !{!54, ptr @PALLAS_SPEC_3, !23, !24, !29, !31}
!54 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 32, !1}
!55 = !DILocation(line: 26, column: 12, scope: !14)
!56 = !DILocation(line: 26, column: 5, scope: !14)
!57 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 14, type: !58, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !18)
!58 = !DISubroutineType(types: !59)
!59 = !{!60, !17, !17}
!60 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!61 = !{!""}
!62 = !DILocalVariable(name: "a", arg: 1, scope: !57, file: !1, line: 14, type: !17)
!63 = !DILocation(line: 0, scope: !57)
!64 = !DILocalVariable(name: "b", arg: 2, scope: !57, file: !1, line: 14, type: !17)
!65 = !DILocation(line: 14, column: 12, scope: !57)
!66 = !DILocation(line: 14, column: 17, scope: !57)
!67 = !DILocation(line: 14, column: 22, scope: !57)
!68 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 15, type: !58, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !18)
!69 = !DILocalVariable(name: "a", arg: 1, scope: !68, file: !1, line: 15, type: !17)
!70 = !DILocation(line: 0, scope: !68)
!71 = !DILocalVariable(name: "b", arg: 2, scope: !68, file: !1, line: 15, type: !17)
!72 = !DILocation(line: 15, column: 9, scope: !68)
!73 = !DILocation(line: 15, column: 25, scope: !68)
!74 = !DILocation(line: 15, column: 22, scope: !68)
!75 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 21, type: !76, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !18)
!76 = !DISubroutineType(types: !77)
!77 = !{!60, !17, !17, !17, !17}
!78 = !DILocalVariable(name: "a", arg: 1, scope: !75, file: !1, line: 21, type: !17)
!79 = !DILocation(line: 0, scope: !75)
!80 = !DILocalVariable(name: "b", arg: 2, scope: !75, file: !1, line: 21, type: !17)
!81 = !DILocalVariable(name: "res", arg: 3, scope: !75, file: !1, line: 21, type: !17)
!82 = !DILocalVariable(name: "i", arg: 4, scope: !75, file: !1, line: 21, type: !17)
!83 = !DILocation(line: 21, column: 29, scope: !75)
!84 = !DILocation(line: 21, column: 24, scope: !75)
!85 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 20, type: !76, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !18)
!86 = !DILocalVariable(name: "a", arg: 1, scope: !85, file: !1, line: 20, type: !17)
!87 = !DILocation(line: 0, scope: !85)
!88 = !DILocalVariable(name: "b", arg: 2, scope: !85, file: !1, line: 20, type: !17)
!89 = !DILocalVariable(name: "res", arg: 3, scope: !85, file: !1, line: 20, type: !17)
!90 = !DILocalVariable(name: "i", arg: 4, scope: !85, file: !1, line: 20, type: !17)
!91 = !DILocation(line: 20, column: 22, scope: !85)
!92 = !DILocation(line: 20, column: 27, scope: !85)
!93 = !DILocation(line: 20, column: 32, scope: !85)
!94 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !5, file: !5, line: 7, type: !58, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !4, retainedNodes: !18)
!95 = !DILocalVariable(name: "a", arg: 1, scope: !94, file: !5, line: 7, type: !17)
!96 = !DILocation(line: 0, scope: !94)
!97 = !DILocalVariable(name: "b", arg: 2, scope: !94, file: !5, line: 7, type: !17)
!98 = !DILocation(line: 7, column: 12, scope: !94)
!99 = !DILocation(line: 7, column: 17, scope: !94)
!100 = !DILocation(line: 7, column: 22, scope: !94)
!101 = distinct !DISubprogram(name: "ghost_mult", scope: !5, file: !5, line: 8, type: !15, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !4, retainedNodes: !18)
!102 = !{!103, i1 true, i1 false, !104}
!103 = !{!"pallas.srcLoc", i64 7, i64 1, i64 7, i64 26, !1}
!104 = !{!"pallas.requires", !103, ptr @PALLAS_SPEC_4}
!105 = !DILocalVariable(name: "a", arg: 1, scope: !101, file: !5, line: 8, type: !17)
!106 = !DILocation(line: 0, scope: !101)
!107 = !DILocalVariable(name: "b", arg: 2, scope: !101, file: !5, line: 8, type: !17)
!108 = !DILocation(line: 9, column: 14, scope: !101)
!109 = !DILocation(line: 9, column: 5, scope: !101)
!110 = !{!"pallas.result"}
