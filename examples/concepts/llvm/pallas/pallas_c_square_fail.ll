; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_square_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [5 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_2], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bad_square(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !21, metadata !DIExpression()), !dbg !24
  call void @llvm.dbg.declare(metadata ptr %3, metadata !25, metadata !DIExpression()), !dbg !26
  store i32 0, ptr %3, align 4, !dbg !26
  call void @llvm.dbg.declare(metadata ptr %4, metadata !27, metadata !DIExpression()), !dbg !29
  store i32 0, ptr %4, align 4, !dbg !29
  br label %5, !dbg !30

5:                                                ; preds = %13, %1
  %6 = load i32, ptr %4, align 4, !dbg !31
  %7 = load i32, ptr %2, align 4, !dbg !33
  %8 = icmp sle i32 %6, %7, !dbg !34
  br i1 %8, label %9, label %16, !dbg !35

9:                                                ; preds = %5
  %10 = load i32, ptr %2, align 4, !dbg !36
  %11 = load i32, ptr %3, align 4, !dbg !38
  %12 = add nsw i32 %11, %10, !dbg !38
  store i32 %12, ptr %3, align 4, !dbg !38
  br label %13, !dbg !39

13:                                               ; preds = %9
  %14 = load i32, ptr %4, align 4, !dbg !40
  %15 = add nsw i32 %14, 1, !dbg !40
  store i32 %15, ptr %4, align 4, !dbg !40
  br label %5, !dbg !41, !llvm.loop !42

16:                                               ; preds = %5
  %17 = load i32, ptr %3, align 4, !dbg !53
  ret i32 %17, !dbg !54
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !55 !pallas.exprWrapper !59 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !60, metadata !DIExpression()), !dbg !61
  %2 = icmp sge i32 %0, 0, !dbg !62
  ret i1 %2, !dbg !61
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !63 !pallas.exprWrapper !59 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !64, metadata !DIExpression()), !dbg !65
  %2 = call i32 @pallas.result.0(), !dbg !66
  %3 = mul nsw i32 %0, %0, !dbg !67
  %4 = icmp eq i32 %2, %3, !dbg !68
  ret i1 %4, !dbg !65
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !69 !pallas.exprWrapper !59 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !72, metadata !DIExpression()), !dbg !73
  call void @llvm.dbg.value(metadata i32 %1, metadata !74, metadata !DIExpression()), !dbg !73
  call void @llvm.dbg.value(metadata i32 %2, metadata !75, metadata !DIExpression()), !dbg !73
  %4 = mul nsw i32 %2, %0, !dbg !76
  %5 = icmp eq i32 %1, %4, !dbg !77
  ret i1 %5, !dbg !73
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !78 !pallas.exprWrapper !59 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !79, metadata !DIExpression()), !dbg !80
  call void @llvm.dbg.value(metadata i32 %1, metadata !81, metadata !DIExpression()), !dbg !80
  call void @llvm.dbg.value(metadata i32 %2, metadata !82, metadata !DIExpression()), !dbg !80
  %4 = icmp sle i32 %2, %0, !dbg !83
  ret i1 %4, !dbg !80
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !84 !pallas.exprWrapper !59 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !85, metadata !DIExpression()), !dbg !86
  call void @llvm.dbg.value(metadata i32 %1, metadata !87, metadata !DIExpression()), !dbg !86
  call void @llvm.dbg.value(metadata i32 %2, metadata !88, metadata !DIExpression()), !dbg !86
  %4 = icmp sle i32 0, %2, !dbg !89
  ret i1 %4, !dbg !86
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !90 i32 @pallas.result.0()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_square_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "b3e75c6dd3d9b88fc5c7290d3a2d8ada")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "91d0fc7a21695dd31753da618612fa66")
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
!17 = !{!18, i1 false, !19, !22}
!18 = !{!"pallas.srcLoc", i64 7, i64 1, i64 10, i64 1}
!19 = !{!"pallas.requires", !20, ptr @PALLAS_SPEC_0, !21}
!20 = !{!"pallas.srcLoc", i64 8, i64 1, i64 8, i64 16}
!21 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 11, type: !15)
!22 = !{!"pallas.ensures", !23, ptr @PALLAS_SPEC_1, !21}
!23 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 31}
!24 = !DILocation(line: 11, column: 20, scope: !12)
!25 = !DILocalVariable(name: "res", scope: !12, file: !1, line: 12, type: !15)
!26 = !DILocation(line: 12, column: 9, scope: !12)
!27 = !DILocalVariable(name: "i", scope: !28, file: !1, line: 19, type: !15)
!28 = distinct !DILexicalBlock(scope: !12, file: !1, line: 19, column: 5)
!29 = !DILocation(line: 19, column: 14, scope: !28)
!30 = !DILocation(line: 19, column: 10, scope: !28)
!31 = !DILocation(line: 19, column: 21, scope: !32)
!32 = distinct !DILexicalBlock(scope: !28, file: !1, line: 19, column: 5)
!33 = !DILocation(line: 19, column: 26, scope: !32)
!34 = !DILocation(line: 19, column: 23, scope: !32)
!35 = !DILocation(line: 19, column: 5, scope: !28)
!36 = !DILocation(line: 20, column: 16, scope: !37)
!37 = distinct !DILexicalBlock(scope: !32, file: !1, line: 19, column: 34)
!38 = !DILocation(line: 20, column: 13, scope: !37)
!39 = !DILocation(line: 21, column: 5, scope: !37)
!40 = !DILocation(line: 19, column: 30, scope: !32)
!41 = !DILocation(line: 19, column: 5, scope: !32)
!42 = distinct !{!42, !35, !43, !44, !45}
!43 = !DILocation(line: 21, column: 5, scope: !28)
!44 = !{!"llvm.loop.mustprogress"}
!45 = !{!"pallas.loopInv", !46, !47, !49, !51}
!46 = !{!"pallas.srcLoc", i64 14, i64 5, i64 18, i64 5}
!47 = !{!48, ptr @PALLAS_SPEC_2, !21, !25, !27}
!48 = !{!"pallas.srcLoc", i64 15, i64 5, i64 15, i64 26}
!49 = !{!50, ptr @PALLAS_SPEC_3, !21, !25, !27}
!50 = !{!"pallas.srcLoc", i64 16, i64 5, i64 16, i64 26}
!51 = !{!52, ptr @PALLAS_SPEC_4, !21, !25, !27}
!52 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 32}
!53 = !DILocation(line: 23, column: 12, scope: !12)
!54 = !DILocation(line: 23, column: 5, scope: !12)
!55 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 8, type: !56, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!56 = !DISubroutineType(types: !57)
!57 = !{!58, !15}
!58 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!59 = !{!""}
!60 = !DILocalVariable(name: "n", arg: 1, scope: !55, file: !1, line: 8, type: !15)
!61 = !DILocation(line: 0, scope: !55)
!62 = !DILocation(line: 8, column: 12, scope: !55)
!63 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 9, type: !56, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!64 = !DILocalVariable(name: "n", arg: 1, scope: !63, file: !1, line: 9, type: !15)
!65 = !DILocation(line: 0, scope: !63)
!66 = !DILocation(line: 9, column: 9, scope: !63)
!67 = !DILocation(line: 9, column: 28, scope: !63)
!68 = !DILocation(line: 9, column: 23, scope: !63)
!69 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 17, type: !70, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!70 = !DISubroutineType(types: !71)
!71 = !{!58, !15, !15, !15}
!72 = !DILocalVariable(name: "n", arg: 1, scope: !69, file: !1, line: 17, type: !15)
!73 = !DILocation(line: 0, scope: !69)
!74 = !DILocalVariable(name: "res", arg: 2, scope: !69, file: !1, line: 17, type: !15)
!75 = !DILocalVariable(name: "i", arg: 3, scope: !69, file: !1, line: 17, type: !15)
!76 = !DILocation(line: 17, column: 29, scope: !69)
!77 = !DILocation(line: 17, column: 24, scope: !69)
!78 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 16, type: !70, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!79 = !DILocalVariable(name: "n", arg: 1, scope: !78, file: !1, line: 16, type: !15)
!80 = !DILocation(line: 0, scope: !78)
!81 = !DILocalVariable(name: "res", arg: 2, scope: !78, file: !1, line: 16, type: !15)
!82 = !DILocalVariable(name: "i", arg: 3, scope: !78, file: !1, line: 16, type: !15)
!83 = !DILocation(line: 16, column: 22, scope: !78)
!84 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 15, type: !70, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!85 = !DILocalVariable(name: "n", arg: 1, scope: !84, file: !1, line: 15, type: !15)
!86 = !DILocation(line: 0, scope: !84)
!87 = !DILocalVariable(name: "res", arg: 2, scope: !84, file: !1, line: 15, type: !15)
!88 = !DILocalVariable(name: "i", arg: 3, scope: !84, file: !1, line: 15, type: !15)
!89 = !DILocation(line: 15, column: 22, scope: !84)
!90 = !{!"pallas.result"}
