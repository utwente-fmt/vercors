; ModuleID = 'tmp_spectral/tmp_ir_source0.ll'
source_filename = "examples/publications/2026/ATVA2026Spectral/C/cantor.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [3 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @triangular(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !24, metadata !DIExpression()), !dbg !30
  %2 = add nsw i32 %0, 1, !dbg !31
  %3 = mul nsw i32 %0, %2, !dbg !32
  %4 = sdiv i32 %3, 2, !dbg !33
  ret i32 %4, !dbg !34
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @square(i32 noundef %0) #0 !dbg !35 !pallas.fcontract !36 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !42, metadata !DIExpression()), !dbg !45
  %2 = mul nsw i32 %0, %0, !dbg !46
  ret i32 %2, !dbg !47
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @cantorPair(i32 noundef %0, i32 noundef %1) #0 !dbg !48 !pallas.fcontract !51 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !57, metadata !DIExpression()), !dbg !65
  call void @llvm.dbg.value(metadata i32 %1, metadata !63, metadata !DIExpression()), !dbg !65
  %3 = add nsw i32 %0, %1, !dbg !66
  %4 = call i32 @square(i32 noundef %3), !dbg !67
  %5 = add nsw i32 %4, %0, !dbg !68
  %6 = mul nsw i32 3, %1, !dbg !69
  %7 = add nsw i32 %5, %6, !dbg !70
  %8 = sdiv i32 %7, 2, !dbg !71
  ret i32 %8, !dbg !72
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !26 !pallas.exprWrapper !73 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !74
  ret i1 true, !dbg !74
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !44 !pallas.exprWrapper !73 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !43, metadata !DIExpression()), !dbg !75
  %2 = call i32 @"pallas.result i32"(), !dbg !76
  %3 = mul nsw i32 %0, %0, !dbg !77
  %4 = icmp eq i32 %2, %3, !dbg !78
  ret i1 %4, !dbg !75
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1) #0 !dbg !59 !pallas.exprWrapper !73 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !58, metadata !DIExpression()), !dbg !79
  call void @llvm.dbg.value(metadata i32 %1, metadata !64, metadata !DIExpression()), !dbg !79
  %3 = icmp eq i32 %1, 0, !dbg !80
  %4 = call i32 @"pallas.result i32"(), !dbg !81
  %5 = call i32 @triangular(i32 noundef %0), !dbg !82
  %6 = icmp eq i32 %4, %5, !dbg !83
  %7 = call i1 @pallas.imply(i1 %3, i1 %6), !dbg !84
  ret i1 %7, !dbg !79
}

declare !pallas.specLib !85 i1 @pallas.imply(i1, i1)

declare !pallas.specLib !86 i32 @"pallas.result i32"()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/C/cantor.c", directory: ".", checksumkind: CSK_MD5, checksum: "b96dbaf1c65573a9415d38d7c7819880")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp_spectral/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "bbff5907312f38cdf7b39381a85e5b46")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "triangular", scope: !1, file: !1, line: 11, type: !13, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 true, i1 false, !16, !16, !20}
!18 = !{!"pallas.srcLoc", i64 7, i64 1, i64 10, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/C/cantor.c", directory: "", checksumkind: CSK_MD5, checksum: "b96dbaf1c65573a9415d38d7c7819880")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !16, !16, !22}
!21 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 14, !19}
!22 = !{!23}
!23 = !{!24, !25}
!24 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 11, type: !15)
!25 = !DILocalVariable(name: "n", arg: 1, scope: !26, file: !1, line: 9, type: !15)
!26 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 9, type: !27, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!27 = !DISubroutineType(types: !28)
!28 = !{!29, !15}
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !DILocation(line: 0, scope: !12)
!31 = !DILocation(line: 12, column: 20, scope: !12)
!32 = !DILocation(line: 12, column: 15, scope: !12)
!33 = !DILocation(line: 12, column: 26, scope: !12)
!34 = !DILocation(line: 12, column: 5, scope: !12)
!35 = distinct !DISubprogram(name: "square", scope: !1, file: !1, line: 19, type: !13, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!36 = !{!37, i1 true, i1 false, !16, !16, !38}
!37 = !{!"pallas.srcLoc", i64 15, i64 1, i64 18, i64 1, !19}
!38 = !{!"pallas.ensures", !39, ptr @PALLAS_SPEC_1, !16, !16, !40}
!39 = !{!"pallas.srcLoc", i64 17, i64 1, i64 17, i64 30, !19}
!40 = !{!41}
!41 = !{!42, !43}
!42 = !DILocalVariable(name: "n", arg: 1, scope: !35, file: !1, line: 19, type: !15)
!43 = !DILocalVariable(name: "n", arg: 1, scope: !44, file: !1, line: 17, type: !15)
!44 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 17, type: !27, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!45 = !DILocation(line: 0, scope: !35)
!46 = !DILocation(line: 20, column: 14, scope: !35)
!47 = !DILocation(line: 20, column: 5, scope: !35)
!48 = distinct !DISubprogram(name: "cantorPair", scope: !1, file: !1, line: 28, type: !49, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!49 = !DISubroutineType(types: !50)
!50 = !{!15, !15, !15}
!51 = !{!52, i1 true, i1 false, !16, !16, !53}
!52 = !{!"pallas.srcLoc", i64 23, i64 1, i64 27, i64 1, !19}
!53 = !{!"pallas.ensures", !54, ptr @PALLAS_SPEC_2, !16, !16, !55}
!54 = !{!"pallas.srcLoc", i64 25, i64 1, i64 26, i64 46, !19}
!55 = !{!56, !62}
!56 = !{!57, !58}
!57 = !DILocalVariable(name: "x", arg: 1, scope: !48, file: !1, line: 28, type: !15)
!58 = !DILocalVariable(name: "x", arg: 1, scope: !59, file: !1, line: 25, type: !15)
!59 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 25, type: !60, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!60 = !DISubroutineType(types: !61)
!61 = !{!29, !15, !15}
!62 = !{!63, !64}
!63 = !DILocalVariable(name: "y", arg: 2, scope: !48, file: !1, line: 28, type: !15)
!64 = !DILocalVariable(name: "y", arg: 2, scope: !59, file: !1, line: 25, type: !15)
!65 = !DILocation(line: 0, scope: !48)
!66 = !DILocation(line: 29, column: 22, scope: !48)
!67 = !DILocation(line: 29, column: 13, scope: !48)
!68 = !DILocation(line: 29, column: 27, scope: !48)
!69 = !DILocation(line: 29, column: 36, scope: !48)
!70 = !DILocation(line: 29, column: 31, scope: !48)
!71 = !DILocation(line: 29, column: 42, scope: !48)
!72 = !DILocation(line: 29, column: 5, scope: !48)
!73 = !{!""}
!74 = !DILocation(line: 0, scope: !26)
!75 = !DILocation(line: 0, scope: !44)
!76 = !DILocation(line: 17, column: 9, scope: !44)
!77 = !DILocation(line: 17, column: 27, scope: !44)
!78 = !DILocation(line: 17, column: 22, scope: !44)
!79 = !DILocation(line: 0, scope: !59)
!80 = !DILocation(line: 25, column: 18, scope: !59)
!81 = !DILocation(line: 26, column: 16, scope: !59)
!82 = !DILocation(line: 26, column: 32, scope: !59)
!83 = !DILocation(line: 26, column: 29, scope: !59)
!84 = !DILocation(line: 25, column: 9, scope: !59)
!85 = !{!"pallas.imply"}
!86 = !{!"pallas.result"}
