; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_multiply.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [5 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @mult(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !17 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !22, metadata !DIExpression()), !dbg !28
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !23, metadata !DIExpression()), !dbg !29
  call void @llvm.dbg.declare(metadata ptr %5, metadata !30, metadata !DIExpression()), !dbg !31
  store i32 0, ptr %5, align 4, !dbg !31
  call void @llvm.dbg.declare(metadata ptr %6, metadata !32, metadata !DIExpression()), !dbg !34
  store i32 0, ptr %6, align 4, !dbg !34
  br label %7, !dbg !35

7:                                                ; preds = %15, %2
  %8 = load i32, ptr %6, align 4, !dbg !36
  %9 = load i32, ptr %4, align 4, !dbg !38
  %10 = icmp slt i32 %8, %9, !dbg !39
  br i1 %10, label %11, label %18, !dbg !40

11:                                               ; preds = %7
  %12 = load i32, ptr %3, align 4, !dbg !41
  %13 = load i32, ptr %5, align 4, !dbg !43
  %14 = add nsw i32 %13, %12, !dbg !43
  store i32 %14, ptr %5, align 4, !dbg !43
  br label %15, !dbg !44

15:                                               ; preds = %11
  %16 = load i32, ptr %6, align 4, !dbg !45
  %17 = add nsw i32 %16, 1, !dbg !45
  store i32 %17, ptr %6, align 4, !dbg !45
  br label %7, !dbg !46, !llvm.loop !47

18:                                               ; preds = %7
  %19 = load i32, ptr %5, align 4, !dbg !56
  ret i32 %19, !dbg !57
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !58 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !63, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %1, metadata !65, metadata !DIExpression()), !dbg !64
  %3 = icmp sge i32 %0, 0, !dbg !66
  ret i1 %3, !dbg !64
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !67 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !68, metadata !DIExpression()), !dbg !69
  call void @llvm.dbg.value(metadata i32 %1, metadata !70, metadata !DIExpression()), !dbg !69
  %3 = icmp sge i32 %1, 0, !dbg !71
  ret i1 %3, !dbg !69
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1) #0 !dbg !72 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !73, metadata !DIExpression()), !dbg !74
  call void @llvm.dbg.value(metadata i32 %1, metadata !75, metadata !DIExpression()), !dbg !74
  %3 = call i32 @pallas.result.0(), !dbg !76
  %4 = mul nsw i32 %0, %1, !dbg !77
  %5 = icmp eq i32 %3, %4, !dbg !78
  ret i1 %5, !dbg !74
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !79 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !82, metadata !DIExpression()), !dbg !83
  call void @llvm.dbg.value(metadata i32 %1, metadata !84, metadata !DIExpression()), !dbg !83
  call void @llvm.dbg.value(metadata i32 %2, metadata !85, metadata !DIExpression()), !dbg !83
  call void @llvm.dbg.value(metadata i32 %3, metadata !86, metadata !DIExpression()), !dbg !83
  %5 = icmp sle i32 0, %3, !dbg !87
  %6 = icmp sle i32 %3, %1, !dbg !88
  %7 = call i1 @pallas.scAnd(i1 %5, i1 %6), !dbg !89
  ret i1 %7, !dbg !83
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !90 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !91, metadata !DIExpression()), !dbg !92
  call void @llvm.dbg.value(metadata i32 %1, metadata !93, metadata !DIExpression()), !dbg !92
  call void @llvm.dbg.value(metadata i32 %2, metadata !94, metadata !DIExpression()), !dbg !92
  call void @llvm.dbg.value(metadata i32 %3, metadata !95, metadata !DIExpression()), !dbg !92
  %5 = mul nsw i32 %3, %0, !dbg !96
  %6 = icmp eq i32 %2, %5, !dbg !97
  ret i1 %6, !dbg !92
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !98 i32 @pallas.result.0()

declare !pallas.specLib !99 i1 @pallas.scAnd(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_multiply.c", directory: ".", checksumkind: CSK_MD5, checksum: "61f5e88430c114818b97faa59f80d4ba")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "627ea6c4b4681fb665bbd491befac468")
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
!17 = !{!18, i1 false, i1 false, !20, !24, !26}
!18 = !{!"pallas.srcLoc", i64 8, i64 1, i64 12, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_multiply.c", directory: "", checksumkind: CSK_MD5, checksum: "61f5e88430c114818b97faa59f80d4ba")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !22, !23}
!21 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 16, !19}
!22 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 13, type: !15)
!23 = !DILocalVariable(name: "k", arg: 2, scope: !12, file: !1, line: 13, type: !15)
!24 = !{!"pallas.requires", !25, ptr @PALLAS_SPEC_1, !22, !23}
!25 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 16, !19}
!26 = !{!"pallas.ensures", !27, ptr @PALLAS_SPEC_2, !22, !23}
!27 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 30, !19}
!28 = !DILocation(line: 13, column: 14, scope: !12)
!29 = !DILocation(line: 13, column: 21, scope: !12)
!30 = !DILocalVariable(name: "res", scope: !12, file: !1, line: 14, type: !15)
!31 = !DILocation(line: 14, column: 9, scope: !12)
!32 = !DILocalVariable(name: "i", scope: !33, file: !1, line: 20, type: !15)
!33 = distinct !DILexicalBlock(scope: !12, file: !1, line: 20, column: 5)
!34 = !DILocation(line: 20, column: 14, scope: !33)
!35 = !DILocation(line: 20, column: 10, scope: !33)
!36 = !DILocation(line: 20, column: 21, scope: !37)
!37 = distinct !DILexicalBlock(scope: !33, file: !1, line: 20, column: 5)
!38 = !DILocation(line: 20, column: 25, scope: !37)
!39 = !DILocation(line: 20, column: 23, scope: !37)
!40 = !DILocation(line: 20, column: 5, scope: !33)
!41 = !DILocation(line: 21, column: 16, scope: !42)
!42 = distinct !DILexicalBlock(scope: !37, file: !1, line: 20, column: 33)
!43 = !DILocation(line: 21, column: 13, scope: !42)
!44 = !DILocation(line: 22, column: 5, scope: !42)
!45 = !DILocation(line: 20, column: 29, scope: !37)
!46 = !DILocation(line: 20, column: 5, scope: !37)
!47 = distinct !{!47, !40, !48, !49, !50}
!48 = !DILocation(line: 22, column: 5, scope: !33)
!49 = !{!"llvm.loop.mustprogress"}
!50 = !{!"pallas.loopInv", !51, !52, !54}
!51 = !{!"pallas.srcLoc", i64 16, i64 5, i64 19, i64 5, !19}
!52 = !{!53, ptr @PALLAS_SPEC_3, !22, !23, !30, !32}
!53 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 40, !19}
!54 = !{!55, ptr @PALLAS_SPEC_4, !22, !23, !30, !32}
!55 = !{!"pallas.srcLoc", i64 18, i64 5, i64 18, i64 32, !19}
!56 = !DILocation(line: 24, column: 12, scope: !12)
!57 = !DILocation(line: 24, column: 5, scope: !12)
!58 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 9, type: !59, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!59 = !DISubroutineType(types: !60)
!60 = !{!61, !15, !15}
!61 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!62 = !{!""}
!63 = !DILocalVariable(name: "n", arg: 1, scope: !58, file: !1, line: 9, type: !15)
!64 = !DILocation(line: 0, scope: !58)
!65 = !DILocalVariable(name: "k", arg: 2, scope: !58, file: !1, line: 9, type: !15)
!66 = !DILocation(line: 9, column: 12, scope: !58)
!67 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 10, type: !59, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!68 = !DILocalVariable(name: "n", arg: 1, scope: !67, file: !1, line: 10, type: !15)
!69 = !DILocation(line: 0, scope: !67)
!70 = !DILocalVariable(name: "k", arg: 2, scope: !67, file: !1, line: 10, type: !15)
!71 = !DILocation(line: 10, column: 12, scope: !67)
!72 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 11, type: !59, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!73 = !DILocalVariable(name: "n", arg: 1, scope: !72, file: !1, line: 11, type: !15)
!74 = !DILocation(line: 0, scope: !72)
!75 = !DILocalVariable(name: "k", arg: 2, scope: !72, file: !1, line: 11, type: !15)
!76 = !DILocation(line: 11, column: 9, scope: !72)
!77 = !DILocation(line: 11, column: 27, scope: !72)
!78 = !DILocation(line: 11, column: 22, scope: !72)
!79 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 17, type: !80, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!80 = !DISubroutineType(types: !81)
!81 = !{!61, !15, !15, !15, !15}
!82 = !DILocalVariable(name: "n", arg: 1, scope: !79, file: !1, line: 17, type: !15)
!83 = !DILocation(line: 0, scope: !79)
!84 = !DILocalVariable(name: "k", arg: 2, scope: !79, file: !1, line: 17, type: !15)
!85 = !DILocalVariable(name: "res", arg: 3, scope: !79, file: !1, line: 17, type: !15)
!86 = !DILocalVariable(name: "i", arg: 4, scope: !79, file: !1, line: 17, type: !15)
!87 = !DILocation(line: 17, column: 27, scope: !79)
!88 = !DILocation(line: 17, column: 35, scope: !79)
!89 = !DILocation(line: 17, column: 20, scope: !79)
!90 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 18, type: !80, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!91 = !DILocalVariable(name: "n", arg: 1, scope: !90, file: !1, line: 18, type: !15)
!92 = !DILocation(line: 0, scope: !90)
!93 = !DILocalVariable(name: "k", arg: 2, scope: !90, file: !1, line: 18, type: !15)
!94 = !DILocalVariable(name: "res", arg: 3, scope: !90, file: !1, line: 18, type: !15)
!95 = !DILocalVariable(name: "i", arg: 4, scope: !90, file: !1, line: 18, type: !15)
!96 = !DILocation(line: 18, column: 29, scope: !90)
!97 = !DILocation(line: 18, column: 24, scope: !90)
!98 = !{!"pallas.result"}
!99 = !{!"pallas.scAnd"}
