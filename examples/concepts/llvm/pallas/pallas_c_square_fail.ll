; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_square_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [5 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bad_square(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !18 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !22, metadata !DIExpression()), !dbg !25
  call void @llvm.dbg.declare(metadata ptr %3, metadata !26, metadata !DIExpression()), !dbg !27
  store i32 0, ptr %3, align 4, !dbg !27
  call void @llvm.dbg.declare(metadata ptr %4, metadata !28, metadata !DIExpression()), !dbg !30
  store i32 0, ptr %4, align 4, !dbg !30
  br label %5, !dbg !31

5:                                                ; preds = %13, %1
  %6 = load i32, ptr %4, align 4, !dbg !32
  %7 = load i32, ptr %2, align 4, !dbg !34
  %8 = icmp sle i32 %6, %7, !dbg !35
  br i1 %8, label %9, label %16, !dbg !36

9:                                                ; preds = %5
  %10 = load i32, ptr %2, align 4, !dbg !37
  %11 = load i32, ptr %3, align 4, !dbg !39
  %12 = add nsw i32 %11, %10, !dbg !39
  store i32 %12, ptr %3, align 4, !dbg !39
  br label %13, !dbg !40

13:                                               ; preds = %9
  %14 = load i32, ptr %4, align 4, !dbg !41
  %15 = add nsw i32 %14, 1, !dbg !41
  store i32 %15, ptr %4, align 4, !dbg !41
  br label %5, !dbg !42, !llvm.loop !43

16:                                               ; preds = %5
  %17 = load i32, ptr %3, align 4, !dbg !54
  ret i32 %17, !dbg !55
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !56 !pallas.exprWrapper !60 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !61, metadata !DIExpression()), !dbg !62
  %2 = icmp sge i32 %0, 0, !dbg !63
  ret i1 %2, !dbg !62
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !64 !pallas.exprWrapper !60 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !65, metadata !DIExpression()), !dbg !66
  %2 = call i32 @pallas.result.0(), !dbg !67
  %3 = mul nsw i32 %0, %0, !dbg !68
  %4 = icmp eq i32 %2, %3, !dbg !69
  ret i1 %4, !dbg !66
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !70 !pallas.exprWrapper !60 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !73, metadata !DIExpression()), !dbg !74
  call void @llvm.dbg.value(metadata i32 %1, metadata !75, metadata !DIExpression()), !dbg !74
  call void @llvm.dbg.value(metadata i32 %2, metadata !76, metadata !DIExpression()), !dbg !74
  %4 = icmp sle i32 0, %2, !dbg !77
  ret i1 %4, !dbg !74
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !78 !pallas.exprWrapper !60 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !79, metadata !DIExpression()), !dbg !80
  call void @llvm.dbg.value(metadata i32 %1, metadata !81, metadata !DIExpression()), !dbg !80
  call void @llvm.dbg.value(metadata i32 %2, metadata !82, metadata !DIExpression()), !dbg !80
  %4 = icmp sle i32 %2, %0, !dbg !83
  ret i1 %4, !dbg !80
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !84 !pallas.exprWrapper !60 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !85, metadata !DIExpression()), !dbg !86
  call void @llvm.dbg.value(metadata i32 %1, metadata !87, metadata !DIExpression()), !dbg !86
  call void @llvm.dbg.value(metadata i32 %2, metadata !88, metadata !DIExpression()), !dbg !86
  %4 = mul nsw i32 %2, %0, !dbg !89
  %5 = icmp eq i32 %1, %4, !dbg !90
  ret i1 %5, !dbg !86
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !91 i32 @pallas.result.0()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_square_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "d122845f728f7e8c41ceb9d20b6c24f5")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "006a6c3070d497a7a07b83a1fc1fd87b")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "bad_square", scope: !13, file: !13, line: 11, type: !14, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!13 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_square_fail.c", directory: "", checksumkind: CSK_MD5, checksum: "d122845f728f7e8c41ceb9d20b6c24f5")
!14 = !DISubroutineType(types: !15)
!15 = !{!16, !16}
!16 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!17 = !{}
!18 = !{!19, i1 false, i1 false, !20, !23}
!19 = !{!"pallas.srcLoc", i64 7, i64 1, i64 10, i64 1, !13}
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !22}
!21 = !{!"pallas.srcLoc", i64 8, i64 1, i64 8, i64 16, !13}
!22 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !13, line: 11, type: !16)
!23 = !{!"pallas.ensures", !24, ptr @PALLAS_SPEC_1, !22}
!24 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 30, !13}
!25 = !DILocation(line: 11, column: 20, scope: !12)
!26 = !DILocalVariable(name: "res", scope: !12, file: !13, line: 12, type: !16)
!27 = !DILocation(line: 12, column: 9, scope: !12)
!28 = !DILocalVariable(name: "i", scope: !29, file: !13, line: 19, type: !16)
!29 = distinct !DILexicalBlock(scope: !12, file: !13, line: 19, column: 5)
!30 = !DILocation(line: 19, column: 14, scope: !29)
!31 = !DILocation(line: 19, column: 10, scope: !29)
!32 = !DILocation(line: 19, column: 21, scope: !33)
!33 = distinct !DILexicalBlock(scope: !29, file: !13, line: 19, column: 5)
!34 = !DILocation(line: 19, column: 26, scope: !33)
!35 = !DILocation(line: 19, column: 23, scope: !33)
!36 = !DILocation(line: 19, column: 5, scope: !29)
!37 = !DILocation(line: 20, column: 16, scope: !38)
!38 = distinct !DILexicalBlock(scope: !33, file: !13, line: 19, column: 34)
!39 = !DILocation(line: 20, column: 13, scope: !38)
!40 = !DILocation(line: 21, column: 5, scope: !38)
!41 = !DILocation(line: 19, column: 30, scope: !33)
!42 = !DILocation(line: 19, column: 5, scope: !33)
!43 = distinct !{!43, !36, !44, !45, !46}
!44 = !DILocation(line: 21, column: 5, scope: !29)
!45 = !{!"llvm.loop.mustprogress"}
!46 = !{!"pallas.loopInv", !47, !48, !50, !52}
!47 = !{!"pallas.srcLoc", i64 14, i64 5, i64 18, i64 5, !13}
!48 = !{!49, ptr @PALLAS_SPEC_2, !22, !26, !28}
!49 = !{!"pallas.srcLoc", i64 15, i64 5, i64 15, i64 26, !13}
!50 = !{!51, ptr @PALLAS_SPEC_3, !22, !26, !28}
!51 = !{!"pallas.srcLoc", i64 16, i64 5, i64 16, i64 26, !13}
!52 = !{!53, ptr @PALLAS_SPEC_4, !22, !26, !28}
!53 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 32, !13}
!54 = !DILocation(line: 23, column: 12, scope: !12)
!55 = !DILocation(line: 23, column: 5, scope: !12)
!56 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !13, file: !13, line: 8, type: !57, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!57 = !DISubroutineType(types: !58)
!58 = !{!59, !16}
!59 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!60 = !{!""}
!61 = !DILocalVariable(name: "n", arg: 1, scope: !56, file: !13, line: 8, type: !16)
!62 = !DILocation(line: 0, scope: !56)
!63 = !DILocation(line: 8, column: 12, scope: !56)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !13, file: !13, line: 9, type: !57, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!65 = !DILocalVariable(name: "n", arg: 1, scope: !64, file: !13, line: 9, type: !16)
!66 = !DILocation(line: 0, scope: !64)
!67 = !DILocation(line: 9, column: 9, scope: !64)
!68 = !DILocation(line: 9, column: 27, scope: !64)
!69 = !DILocation(line: 9, column: 22, scope: !64)
!70 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !13, file: !13, line: 15, type: !71, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!71 = !DISubroutineType(types: !72)
!72 = !{!59, !16, !16, !16}
!73 = !DILocalVariable(name: "n", arg: 1, scope: !70, file: !13, line: 15, type: !16)
!74 = !DILocation(line: 0, scope: !70)
!75 = !DILocalVariable(name: "res", arg: 2, scope: !70, file: !13, line: 15, type: !16)
!76 = !DILocalVariable(name: "i", arg: 3, scope: !70, file: !13, line: 15, type: !16)
!77 = !DILocation(line: 15, column: 22, scope: !70)
!78 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !13, file: !13, line: 16, type: !71, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!79 = !DILocalVariable(name: "n", arg: 1, scope: !78, file: !13, line: 16, type: !16)
!80 = !DILocation(line: 0, scope: !78)
!81 = !DILocalVariable(name: "res", arg: 2, scope: !78, file: !13, line: 16, type: !16)
!82 = !DILocalVariable(name: "i", arg: 3, scope: !78, file: !13, line: 16, type: !16)
!83 = !DILocation(line: 16, column: 22, scope: !78)
!84 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !13, file: !13, line: 17, type: !71, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!85 = !DILocalVariable(name: "n", arg: 1, scope: !84, file: !13, line: 17, type: !16)
!86 = !DILocation(line: 0, scope: !84)
!87 = !DILocalVariable(name: "res", arg: 2, scope: !84, file: !13, line: 17, type: !16)
!88 = !DILocalVariable(name: "i", arg: 3, scope: !84, file: !13, line: 17, type: !16)
!89 = !DILocation(line: 17, column: 29, scope: !84)
!90 = !DILocation(line: 17, column: 24, scope: !84)
!91 = !{!"pallas.result"}
