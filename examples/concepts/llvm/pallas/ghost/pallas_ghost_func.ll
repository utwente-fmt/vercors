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
  call void @llvm.dbg.declare(metadata ptr %3, metadata !26, metadata !DIExpression()), !dbg !43
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !33, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.declare(metadata ptr %5, metadata !45, metadata !DIExpression()), !dbg !46
  store i32 0, ptr %5, align 4, !dbg !46
  call void @llvm.dbg.declare(metadata ptr %6, metadata !47, metadata !DIExpression()), !dbg !49
  store i32 0, ptr %6, align 4, !dbg !49
  br label %7, !dbg !50

7:                                                ; preds = %15, %2
  %8 = load i32, ptr %6, align 4, !dbg !51
  %9 = load i32, ptr %4, align 4, !dbg !53
  %10 = icmp slt i32 %8, %9, !dbg !54
  br i1 %10, label %11, label %18, !dbg !55

11:                                               ; preds = %7
  %12 = load i32, ptr %3, align 4, !dbg !56
  %13 = load i32, ptr %5, align 4, !dbg !58
  %14 = add nsw i32 %13, %12, !dbg !58
  store i32 %14, ptr %5, align 4, !dbg !58
  br label %15, !dbg !59

15:                                               ; preds = %11
  %16 = load i32, ptr %6, align 4, !dbg !60
  %17 = add nsw i32 %16, 1, !dbg !60
  store i32 %17, ptr %6, align 4, !dbg !60
  br label %7, !dbg !61, !llvm.loop !62

18:                                               ; preds = %7
  %19 = load i32, ptr %5, align 4, !dbg !93
  ret i32 %19, !dbg !94
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !28 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !27, metadata !DIExpression()), !dbg !96
  call void @llvm.dbg.value(metadata i32 %1, metadata !34, metadata !DIExpression()), !dbg !96
  %3 = icmp sge i32 %0, 0, !dbg !97
  br i1 %3, label %4, label %6, !dbg !98

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !99
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !96
  ret i1 %7, !dbg !96
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !40 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !39, metadata !DIExpression()), !dbg !100
  call void @llvm.dbg.value(metadata i32 %1, metadata !42, metadata !DIExpression()), !dbg !100
  %3 = call i32 @"pallas.result i32"(), !dbg !101
  %4 = call i32 @ghost_mult(i32 noundef %0, i32 noundef %1), !dbg !102
  %5 = icmp eq i32 %3, %4, !dbg !103
  ret i1 %5, !dbg !100
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !86 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !85, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %1, metadata !88, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %2, metadata !90, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %3, metadata !92, metadata !DIExpression()), !dbg !104
  %5 = mul nsw i32 %3, %0, !dbg !105
  %6 = icmp eq i32 %2, %5, !dbg !106
  ret i1 %6, !dbg !104
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !72 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !71, metadata !DIExpression()), !dbg !107
  call void @llvm.dbg.value(metadata i32 %1, metadata !76, metadata !DIExpression()), !dbg !107
  call void @llvm.dbg.value(metadata i32 %2, metadata !78, metadata !DIExpression()), !dbg !107
  call void @llvm.dbg.value(metadata i32 %3, metadata !80, metadata !DIExpression()), !dbg !107
  %5 = icmp sle i32 0, %3, !dbg !108
  br i1 %5, label %6, label %8, !dbg !109

6:                                                ; preds = %4
  %7 = icmp sle i32 %3, %1, !dbg !110
  br label %8

8:                                                ; preds = %6, %4
  %9 = phi i1 [ false, %4 ], [ %7, %6 ], !dbg !107
  ret i1 %9, !dbg !107
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1) #0 !dbg !111 !pallas.exprWrapper !95 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !112, metadata !DIExpression()), !dbg !113
  call void @llvm.dbg.value(metadata i32 %1, metadata !114, metadata !DIExpression()), !dbg !113
  %3 = icmp sge i32 %0, 0, !dbg !115
  br i1 %3, label %4, label %6, !dbg !116

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !117
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !113
  ret i1 %7, !dbg !113
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @ghost_mult(i32 noundef %0, i32 noundef %1) #0 !dbg !118 !pallas.ghost !95 !pallas.fcontract !119 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !124, metadata !DIExpression()), !dbg !127
  call void @llvm.dbg.value(metadata i32 %1, metadata !126, metadata !DIExpression()), !dbg !127
  %3 = mul nsw i32 %0, %1, !dbg !128
  ret i32 %3, !dbg !129
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !130 i32 @"pallas.result i32"()

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
!5 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/ghost/pallas_ghost_func.c", directory: "")
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
!19 = !{!20, i1 false, i1 false, !18, !18, !22, !35}
!20 = !{!"pallas.srcLoc", i64 13, i64 1, i64 16, i64 1, !21}
!21 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/ghost/pallas_ghost_func.c", directory: "", checksumkind: CSK_MD5, checksum: "beb09737e9a38b4634976cee631b7fc8")
!22 = !{!"pallas.requires", !23, ptr @PALLAS_SPEC_0, !18, !18, !24}
!23 = !{!"pallas.srcLoc", i64 14, i64 1, i64 14, i64 26, !21}
!24 = !{!25, !32}
!25 = !{!26, !27}
!26 = !DILocalVariable(name: "a", arg: 1, scope: !14, file: !1, line: 17, type: !17)
!27 = !DILocalVariable(name: "a", arg: 1, scope: !28, file: !1, line: 14, type: !17)
!28 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 14, type: !29, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !18)
!29 = !DISubroutineType(types: !30)
!30 = !{!31, !17, !17}
!31 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!32 = !{!33, !34}
!33 = !DILocalVariable(name: "b", arg: 2, scope: !14, file: !1, line: 17, type: !17)
!34 = !DILocalVariable(name: "b", arg: 2, scope: !28, file: !1, line: 14, type: !17)
!35 = !{!"pallas.ensures", !36, ptr @PALLAS_SPEC_1, !18, !18, !37}
!36 = !{!"pallas.srcLoc", i64 15, i64 1, i64 15, i64 41, !21}
!37 = !{!38, !41}
!38 = !{!26, !39}
!39 = !DILocalVariable(name: "a", arg: 1, scope: !40, file: !1, line: 15, type: !17)
!40 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 15, type: !29, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !18)
!41 = !{!33, !42}
!42 = !DILocalVariable(name: "b", arg: 2, scope: !40, file: !1, line: 15, type: !17)
!43 = !DILocation(line: 17, column: 17, scope: !14)
!44 = !DILocation(line: 17, column: 24, scope: !14)
!45 = !DILocalVariable(name: "res", scope: !14, file: !1, line: 18, type: !17)
!46 = !DILocation(line: 18, column: 9, scope: !14)
!47 = !DILocalVariable(name: "i", scope: !48, file: !1, line: 23, type: !17)
!48 = distinct !DILexicalBlock(scope: !14, file: !1, line: 23, column: 5)
!49 = !DILocation(line: 23, column: 14, scope: !48)
!50 = !DILocation(line: 23, column: 10, scope: !48)
!51 = !DILocation(line: 23, column: 21, scope: !52)
!52 = distinct !DILexicalBlock(scope: !48, file: !1, line: 23, column: 5)
!53 = !DILocation(line: 23, column: 25, scope: !52)
!54 = !DILocation(line: 23, column: 23, scope: !52)
!55 = !DILocation(line: 23, column: 5, scope: !48)
!56 = !DILocation(line: 24, column: 16, scope: !57)
!57 = distinct !DILexicalBlock(scope: !52, file: !1, line: 23, column: 33)
!58 = !DILocation(line: 24, column: 13, scope: !57)
!59 = !DILocation(line: 25, column: 5, scope: !57)
!60 = !DILocation(line: 23, column: 29, scope: !52)
!61 = !DILocation(line: 23, column: 5, scope: !52)
!62 = distinct !{!62, !55, !63, !64, !65}
!63 = !DILocation(line: 25, column: 5, scope: !48)
!64 = !{!"llvm.loop.mustprogress"}
!65 = !{!"pallas.loopInvBlock", !66, !67, !81}
!66 = !{!"pallas.srcLoc", i64 19, i64 5, i64 22, i64 5, !21}
!67 = !{!"pallas.loopInv", !68, ptr @PALLAS_SPEC_2, !18, !18, !69}
!68 = !{!"pallas.srcLoc", i64 20, i64 5, i64 20, i64 36, !21}
!69 = !{!70, !75, !77, !79}
!70 = !{!26, !71}
!71 = !DILocalVariable(name: "a", arg: 1, scope: !72, file: !1, line: 20, type: !17)
!72 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 20, type: !73, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !18)
!73 = !DISubroutineType(types: !74)
!74 = !{!31, !17, !17, !17, !17}
!75 = !{!33, !76}
!76 = !DILocalVariable(name: "b", arg: 2, scope: !72, file: !1, line: 20, type: !17)
!77 = !{!45, !78}
!78 = !DILocalVariable(name: "res", arg: 3, scope: !72, file: !1, line: 20, type: !17)
!79 = !{!47, !80}
!80 = !DILocalVariable(name: "i", arg: 4, scope: !72, file: !1, line: 20, type: !17)
!81 = !{!"pallas.loopInv", !82, ptr @PALLAS_SPEC_3, !18, !18, !83}
!82 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 32, !21}
!83 = !{!84, !87, !89, !91}
!84 = !{!26, !85}
!85 = !DILocalVariable(name: "a", arg: 1, scope: !86, file: !1, line: 21, type: !17)
!86 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 21, type: !73, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !18)
!87 = !{!33, !88}
!88 = !DILocalVariable(name: "b", arg: 2, scope: !86, file: !1, line: 21, type: !17)
!89 = !{!45, !90}
!90 = !DILocalVariable(name: "res", arg: 3, scope: !86, file: !1, line: 21, type: !17)
!91 = !{!47, !92}
!92 = !DILocalVariable(name: "i", arg: 4, scope: !86, file: !1, line: 21, type: !17)
!93 = !DILocation(line: 26, column: 12, scope: !14)
!94 = !DILocation(line: 26, column: 5, scope: !14)
!95 = !{!""}
!96 = !DILocation(line: 0, scope: !28)
!97 = !DILocation(line: 14, column: 12, scope: !28)
!98 = !DILocation(line: 14, column: 17, scope: !28)
!99 = !DILocation(line: 14, column: 22, scope: !28)
!100 = !DILocation(line: 0, scope: !40)
!101 = !DILocation(line: 15, column: 9, scope: !40)
!102 = !DILocation(line: 15, column: 25, scope: !40)
!103 = !DILocation(line: 15, column: 22, scope: !40)
!104 = !DILocation(line: 0, scope: !86)
!105 = !DILocation(line: 21, column: 29, scope: !86)
!106 = !DILocation(line: 21, column: 24, scope: !86)
!107 = !DILocation(line: 0, scope: !72)
!108 = !DILocation(line: 20, column: 22, scope: !72)
!109 = !DILocation(line: 20, column: 27, scope: !72)
!110 = !DILocation(line: 20, column: 32, scope: !72)
!111 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !5, file: !5, line: 7, type: !29, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !4, retainedNodes: !18)
!112 = !DILocalVariable(name: "a", arg: 1, scope: !111, file: !5, line: 7, type: !17)
!113 = !DILocation(line: 0, scope: !111)
!114 = !DILocalVariable(name: "b", arg: 2, scope: !111, file: !5, line: 7, type: !17)
!115 = !DILocation(line: 7, column: 12, scope: !111)
!116 = !DILocation(line: 7, column: 17, scope: !111)
!117 = !DILocation(line: 7, column: 22, scope: !111)
!118 = distinct !DISubprogram(name: "ghost_mult", scope: !5, file: !5, line: 8, type: !15, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !4, retainedNodes: !18)
!119 = !{!120, i1 true, i1 false, !18, !18, !121}
!120 = !{!"pallas.srcLoc", i64 7, i64 1, i64 7, i64 26, !21}
!121 = !{!"pallas.requires", !120, ptr @PALLAS_SPEC_4, !18, !18, !122}
!122 = !{!123, !125}
!123 = !{!124, !112}
!124 = !DILocalVariable(name: "a", arg: 1, scope: !118, file: !5, line: 8, type: !17)
!125 = !{!126, !114}
!126 = !DILocalVariable(name: "b", arg: 2, scope: !118, file: !5, line: 8, type: !17)
!127 = !DILocation(line: 0, scope: !118)
!128 = !DILocation(line: 9, column: 14, scope: !118)
!129 = !DILocation(line: 9, column: 5, scope: !118)
!130 = !{!"pallas.result"}
