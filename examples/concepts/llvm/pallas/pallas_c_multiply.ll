; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_multiply.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [5 x ptr] [ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_3], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @mult(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !17 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !21, metadata !DIExpression()), !dbg !27
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !22, metadata !DIExpression()), !dbg !28
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
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !57 !pallas.exprWrapper !61 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !62, metadata !DIExpression()), !dbg !63
  call void @llvm.dbg.value(metadata i32 %1, metadata !64, metadata !DIExpression()), !dbg !63
  %3 = icmp sge i32 %1, 0, !dbg !65
  ret i1 %3, !dbg !63
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1) #0 !dbg !66 !pallas.exprWrapper !61 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !67, metadata !DIExpression()), !dbg !68
  call void @llvm.dbg.value(metadata i32 %1, metadata !69, metadata !DIExpression()), !dbg !68
  %3 = call i32 @pallas.result.0(), !dbg !70
  %4 = mul nsw i32 %0, %1, !dbg !71
  %5 = icmp eq i32 %3, %4, !dbg !72
  ret i1 %5, !dbg !68
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !73 !pallas.exprWrapper !61 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !74, metadata !DIExpression()), !dbg !75
  call void @llvm.dbg.value(metadata i32 %1, metadata !76, metadata !DIExpression()), !dbg !75
  %3 = icmp sge i32 %0, 0, !dbg !77
  ret i1 %3, !dbg !75
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !78 !pallas.exprWrapper !61 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !81, metadata !DIExpression()), !dbg !82
  call void @llvm.dbg.value(metadata i32 %1, metadata !83, metadata !DIExpression()), !dbg !82
  call void @llvm.dbg.value(metadata i32 %2, metadata !84, metadata !DIExpression()), !dbg !82
  call void @llvm.dbg.value(metadata i32 %3, metadata !85, metadata !DIExpression()), !dbg !82
  %5 = mul nsw i32 %3, %0, !dbg !86
  %6 = icmp eq i32 %2, %5, !dbg !87
  ret i1 %6, !dbg !82
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !88 !pallas.exprWrapper !61 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !89, metadata !DIExpression()), !dbg !90
  call void @llvm.dbg.value(metadata i32 %1, metadata !91, metadata !DIExpression()), !dbg !90
  call void @llvm.dbg.value(metadata i32 %2, metadata !92, metadata !DIExpression()), !dbg !90
  call void @llvm.dbg.value(metadata i32 %3, metadata !93, metadata !DIExpression()), !dbg !90
  %5 = icmp sle i32 0, %3, !dbg !94
  %6 = icmp sle i32 %3, %1, !dbg !95
  %7 = call i1 @pallas.scAnd(i1 %5, i1 %6), !dbg !96
  ret i1 %7, !dbg !90
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !97 i32 @pallas.result.0()

declare !pallas.specLib !98 i1 @pallas.scAnd(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_multiply.c", directory: ".", checksumkind: CSK_MD5, checksum: "e91ed3496f662b95570dd6df96bcdec2")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "e4b208734b088887308e71ffce67547e")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "mult", scope: !1, file: !1, line: 13, type: !13, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, !19, !23, !25}
!18 = !{!"pallas.srcLoc", i64 8, i64 1, i64 12, i64 1}
!19 = !{!"pallas.requires", !20, ptr @PALLAS_SPEC_0, !21, !22}
!20 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 16}
!21 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 13, type: !15)
!22 = !DILocalVariable(name: "k", arg: 2, scope: !12, file: !1, line: 13, type: !15)
!23 = !{!"pallas.requires", !24, ptr @PALLAS_SPEC_1, !21, !22}
!24 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 16}
!25 = !{!"pallas.ensures", !26, ptr @PALLAS_SPEC_2, !21, !22}
!26 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 31}
!27 = !DILocation(line: 13, column: 14, scope: !12)
!28 = !DILocation(line: 13, column: 21, scope: !12)
!29 = !DILocalVariable(name: "res", scope: !12, file: !1, line: 14, type: !15)
!30 = !DILocation(line: 14, column: 9, scope: !12)
!31 = !DILocalVariable(name: "i", scope: !32, file: !1, line: 20, type: !15)
!32 = distinct !DILexicalBlock(scope: !12, file: !1, line: 20, column: 5)
!33 = !DILocation(line: 20, column: 14, scope: !32)
!34 = !DILocation(line: 20, column: 10, scope: !32)
!35 = !DILocation(line: 20, column: 21, scope: !36)
!36 = distinct !DILexicalBlock(scope: !32, file: !1, line: 20, column: 5)
!37 = !DILocation(line: 20, column: 25, scope: !36)
!38 = !DILocation(line: 20, column: 23, scope: !36)
!39 = !DILocation(line: 20, column: 5, scope: !32)
!40 = !DILocation(line: 21, column: 16, scope: !41)
!41 = distinct !DILexicalBlock(scope: !36, file: !1, line: 20, column: 33)
!42 = !DILocation(line: 21, column: 13, scope: !41)
!43 = !DILocation(line: 22, column: 5, scope: !41)
!44 = !DILocation(line: 20, column: 29, scope: !36)
!45 = !DILocation(line: 20, column: 5, scope: !36)
!46 = distinct !{!46, !39, !47, !48, !49}
!47 = !DILocation(line: 22, column: 5, scope: !32)
!48 = !{!"llvm.loop.mustprogress"}
!49 = !{!"pallas.loopInv", !50, !51, !53}
!50 = !{!"pallas.srcLoc", i64 16, i64 5, i64 19, i64 5}
!51 = !{!52, ptr @PALLAS_SPEC_3, !21, !22, !29, !31}
!52 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 40}
!53 = !{!54, ptr @PALLAS_SPEC_4, !21, !22, !29, !31}
!54 = !{!"pallas.srcLoc", i64 18, i64 5, i64 18, i64 32}
!55 = !DILocation(line: 24, column: 12, scope: !12)
!56 = !DILocation(line: 24, column: 5, scope: !12)
!57 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 10, type: !58, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!58 = !DISubroutineType(types: !59)
!59 = !{!60, !15, !15}
!60 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!61 = !{!""}
!62 = !DILocalVariable(name: "n", arg: 1, scope: !57, file: !1, line: 10, type: !15)
!63 = !DILocation(line: 0, scope: !57)
!64 = !DILocalVariable(name: "k", arg: 2, scope: !57, file: !1, line: 10, type: !15)
!65 = !DILocation(line: 10, column: 12, scope: !57)
!66 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 11, type: !58, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!67 = !DILocalVariable(name: "n", arg: 1, scope: !66, file: !1, line: 11, type: !15)
!68 = !DILocation(line: 0, scope: !66)
!69 = !DILocalVariable(name: "k", arg: 2, scope: !66, file: !1, line: 11, type: !15)
!70 = !DILocation(line: 11, column: 9, scope: !66)
!71 = !DILocation(line: 11, column: 28, scope: !66)
!72 = !DILocation(line: 11, column: 23, scope: !66)
!73 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 9, type: !58, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!74 = !DILocalVariable(name: "n", arg: 1, scope: !73, file: !1, line: 9, type: !15)
!75 = !DILocation(line: 0, scope: !73)
!76 = !DILocalVariable(name: "k", arg: 2, scope: !73, file: !1, line: 9, type: !15)
!77 = !DILocation(line: 9, column: 12, scope: !73)
!78 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 18, type: !79, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!79 = !DISubroutineType(types: !80)
!80 = !{!60, !15, !15, !15, !15}
!81 = !DILocalVariable(name: "n", arg: 1, scope: !78, file: !1, line: 18, type: !15)
!82 = !DILocation(line: 0, scope: !78)
!83 = !DILocalVariable(name: "k", arg: 2, scope: !78, file: !1, line: 18, type: !15)
!84 = !DILocalVariable(name: "res", arg: 3, scope: !78, file: !1, line: 18, type: !15)
!85 = !DILocalVariable(name: "i", arg: 4, scope: !78, file: !1, line: 18, type: !15)
!86 = !DILocation(line: 18, column: 29, scope: !78)
!87 = !DILocation(line: 18, column: 24, scope: !78)
!88 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 17, type: !79, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!89 = !DILocalVariable(name: "n", arg: 1, scope: !88, file: !1, line: 17, type: !15)
!90 = !DILocation(line: 0, scope: !88)
!91 = !DILocalVariable(name: "k", arg: 2, scope: !88, file: !1, line: 17, type: !15)
!92 = !DILocalVariable(name: "res", arg: 3, scope: !88, file: !1, line: 17, type: !15)
!93 = !DILocalVariable(name: "i", arg: 4, scope: !88, file: !1, line: 17, type: !15)
!94 = !DILocation(line: 17, column: 27, scope: !88)
!95 = !DILocation(line: 17, column: 35, scope: !88)
!96 = !DILocation(line: 17, column: 20, scope: !88)
!97 = !{!"pallas.result"}
!98 = !{!"pallas.scAnd"}
