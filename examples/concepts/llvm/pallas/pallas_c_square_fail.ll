; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_square_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [5 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_4], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bad_square(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !24, metadata !DIExpression()), !dbg !36
  call void @llvm.dbg.declare(metadata ptr %3, metadata !37, metadata !DIExpression()), !dbg !38
  store i32 0, ptr %3, align 4, !dbg !38
  call void @llvm.dbg.declare(metadata ptr %4, metadata !39, metadata !DIExpression()), !dbg !41
  store i32 0, ptr %4, align 4, !dbg !41
  br label %5, !dbg !42

5:                                                ; preds = %13, %1
  %6 = load i32, ptr %4, align 4, !dbg !43
  %7 = load i32, ptr %2, align 4, !dbg !45
  %8 = icmp sle i32 %6, %7, !dbg !46
  br i1 %8, label %9, label %16, !dbg !47

9:                                                ; preds = %5
  %10 = load i32, ptr %2, align 4, !dbg !48
  %11 = load i32, ptr %3, align 4, !dbg !50
  %12 = add nsw i32 %11, %10, !dbg !50
  store i32 %12, ptr %3, align 4, !dbg !50
  br label %13, !dbg !51

13:                                               ; preds = %9
  %14 = load i32, ptr %4, align 4, !dbg !52
  %15 = add nsw i32 %14, 1, !dbg !52
  store i32 %15, ptr %4, align 4, !dbg !52
  br label %5, !dbg !53, !llvm.loop !54

16:                                               ; preds = %5
  %17 = load i32, ptr %3, align 4, !dbg !91
  ret i32 %17, !dbg !92
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !26 !pallas.exprWrapper !93 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !94
  %2 = icmp sge i32 %0, 0, !dbg !95
  ret i1 %2, !dbg !94
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !35 !pallas.exprWrapper !93 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !34, metadata !DIExpression()), !dbg !96
  %2 = call i32 @"pallas.result i32"(), !dbg !97
  %3 = mul nsw i32 %0, %0, !dbg !98
  %4 = icmp eq i32 %2, %3, !dbg !99
  ret i1 %4, !dbg !96
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !76 !pallas.exprWrapper !93 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !75, metadata !DIExpression()), !dbg !100
  call void @llvm.dbg.value(metadata i32 %1, metadata !78, metadata !DIExpression()), !dbg !100
  call void @llvm.dbg.value(metadata i32 %2, metadata !80, metadata !DIExpression()), !dbg !100
  %4 = icmp sle i32 %2, %0, !dbg !101
  ret i1 %4, !dbg !100
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !64 !pallas.exprWrapper !93 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !63, metadata !DIExpression()), !dbg !102
  call void @llvm.dbg.value(metadata i32 %1, metadata !68, metadata !DIExpression()), !dbg !102
  call void @llvm.dbg.value(metadata i32 %2, metadata !70, metadata !DIExpression()), !dbg !102
  %4 = icmp sle i32 0, %2, !dbg !103
  ret i1 %4, !dbg !102
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !86 !pallas.exprWrapper !93 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !85, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %1, metadata !88, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %2, metadata !90, metadata !DIExpression()), !dbg !104
  %4 = mul nsw i32 %2, %0, !dbg !105
  %5 = icmp eq i32 %1, %4, !dbg !106
  ret i1 %5, !dbg !104
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !107 i32 @"pallas.result i32"()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_square_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "d122845f728f7e8c41ceb9d20b6c24f5")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "220100436795c5ab5710423f9deded8f")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "bad_square", scope: !1, file: !1, line: 11, type: !13, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, i1 false, !16, !16, !20, !30}
!18 = !{!"pallas.srcLoc", i64 7, i64 1, i64 10, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_square_fail.c", directory: "", checksumkind: CSK_MD5, checksum: "d122845f728f7e8c41ceb9d20b6c24f5")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !16, !16, !22}
!21 = !{!"pallas.srcLoc", i64 8, i64 1, i64 8, i64 16, !19}
!22 = !{!23}
!23 = !{!24, !25}
!24 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 11, type: !15)
!25 = !DILocalVariable(name: "n", arg: 1, scope: !26, file: !1, line: 8, type: !15)
!26 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 8, type: !27, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!27 = !DISubroutineType(types: !28)
!28 = !{!29, !15}
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !{!"pallas.ensures", !31, ptr @PALLAS_SPEC_1, !16, !16, !32}
!31 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 30, !19}
!32 = !{!33}
!33 = !{!24, !34}
!34 = !DILocalVariable(name: "n", arg: 1, scope: !35, file: !1, line: 9, type: !15)
!35 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 9, type: !27, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!36 = !DILocation(line: 11, column: 20, scope: !12)
!37 = !DILocalVariable(name: "res", scope: !12, file: !1, line: 12, type: !15)
!38 = !DILocation(line: 12, column: 9, scope: !12)
!39 = !DILocalVariable(name: "i", scope: !40, file: !1, line: 19, type: !15)
!40 = distinct !DILexicalBlock(scope: !12, file: !1, line: 19, column: 5)
!41 = !DILocation(line: 19, column: 14, scope: !40)
!42 = !DILocation(line: 19, column: 10, scope: !40)
!43 = !DILocation(line: 19, column: 21, scope: !44)
!44 = distinct !DILexicalBlock(scope: !40, file: !1, line: 19, column: 5)
!45 = !DILocation(line: 19, column: 26, scope: !44)
!46 = !DILocation(line: 19, column: 23, scope: !44)
!47 = !DILocation(line: 19, column: 5, scope: !40)
!48 = !DILocation(line: 20, column: 16, scope: !49)
!49 = distinct !DILexicalBlock(scope: !44, file: !1, line: 19, column: 34)
!50 = !DILocation(line: 20, column: 13, scope: !49)
!51 = !DILocation(line: 21, column: 5, scope: !49)
!52 = !DILocation(line: 19, column: 30, scope: !44)
!53 = !DILocation(line: 19, column: 5, scope: !44)
!54 = distinct !{!54, !47, !55, !56, !57}
!55 = !DILocation(line: 21, column: 5, scope: !40)
!56 = !{!"llvm.loop.mustprogress"}
!57 = !{!"pallas.loopInvBlock", !58, !59, !71, !81}
!58 = !{!"pallas.srcLoc", i64 14, i64 5, i64 18, i64 5, !19}
!59 = !{!"pallas.loopInv", !60, ptr @PALLAS_SPEC_2, !16, !16, !61}
!60 = !{!"pallas.srcLoc", i64 15, i64 5, i64 15, i64 26, !19}
!61 = !{!62, !67, !69}
!62 = !{!24, !63}
!63 = !DILocalVariable(name: "n", arg: 1, scope: !64, file: !1, line: 15, type: !15)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 15, type: !65, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!65 = !DISubroutineType(types: !66)
!66 = !{!29, !15, !15, !15}
!67 = !{!37, !68}
!68 = !DILocalVariable(name: "res", arg: 2, scope: !64, file: !1, line: 15, type: !15)
!69 = !{!39, !70}
!70 = !DILocalVariable(name: "i", arg: 3, scope: !64, file: !1, line: 15, type: !15)
!71 = !{!"pallas.loopInv", !72, ptr @PALLAS_SPEC_3, !16, !16, !73}
!72 = !{!"pallas.srcLoc", i64 16, i64 5, i64 16, i64 26, !19}
!73 = !{!74, !77, !79}
!74 = !{!24, !75}
!75 = !DILocalVariable(name: "n", arg: 1, scope: !76, file: !1, line: 16, type: !15)
!76 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 16, type: !65, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!77 = !{!37, !78}
!78 = !DILocalVariable(name: "res", arg: 2, scope: !76, file: !1, line: 16, type: !15)
!79 = !{!39, !80}
!80 = !DILocalVariable(name: "i", arg: 3, scope: !76, file: !1, line: 16, type: !15)
!81 = !{!"pallas.loopInv", !82, ptr @PALLAS_SPEC_4, !16, !16, !83}
!82 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 32, !19}
!83 = !{!84, !87, !89}
!84 = !{!24, !85}
!85 = !DILocalVariable(name: "n", arg: 1, scope: !86, file: !1, line: 17, type: !15)
!86 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 17, type: !65, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!87 = !{!37, !88}
!88 = !DILocalVariable(name: "res", arg: 2, scope: !86, file: !1, line: 17, type: !15)
!89 = !{!39, !90}
!90 = !DILocalVariable(name: "i", arg: 3, scope: !86, file: !1, line: 17, type: !15)
!91 = !DILocation(line: 23, column: 12, scope: !12)
!92 = !DILocation(line: 23, column: 5, scope: !12)
!93 = !{!""}
!94 = !DILocation(line: 0, scope: !26)
!95 = !DILocation(line: 8, column: 12, scope: !26)
!96 = !DILocation(line: 0, scope: !35)
!97 = !DILocation(line: 9, column: 9, scope: !35)
!98 = !DILocation(line: 9, column: 27, scope: !35)
!99 = !DILocation(line: 9, column: 22, scope: !35)
!100 = !DILocation(line: 0, scope: !76)
!101 = !DILocation(line: 16, column: 22, scope: !76)
!102 = !DILocation(line: 0, scope: !64)
!103 = !DILocation(line: 15, column: 22, scope: !64)
!104 = !DILocation(line: 0, scope: !86)
!105 = !DILocation(line: 17, column: 29, scope: !86)
!106 = !DILocation(line: 17, column: 24, scope: !86)
!107 = !{!"pallas.result"}
