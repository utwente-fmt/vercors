; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_function_contract_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [2 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !18 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !22, metadata !DIExpression()), !dbg !26
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !23, metadata !DIExpression()), !dbg !27
  call void @llvm.dbg.declare(metadata ptr %5, metadata !28, metadata !DIExpression()), !dbg !29
  %6 = load i32, ptr %3, align 4, !dbg !30
  %7 = load i32, ptr %4, align 4, !dbg !31
  %8 = add nsw i32 %6, %7, !dbg !32
  store i32 %8, ptr %5, align 4, !dbg !29
  %9 = load i32, ptr %5, align 4, !dbg !33
  %10 = add nsw i32 %9, 1, !dbg !33
  store i32 %10, ptr %5, align 4, !dbg !33
  %11 = load i32, ptr %5, align 4, !dbg !34
  %12 = load i32, ptr %4, align 4, !dbg !35
  %13 = mul nsw i32 %11, %12, !dbg !36
  %14 = add nsw i32 %13, 1, !dbg !37
  ret i32 %14, !dbg !38
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !39 !pallas.exprWrapper !43 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !44, metadata !DIExpression()), !dbg !45
  call void @llvm.dbg.value(metadata i32 %1, metadata !46, metadata !DIExpression()), !dbg !45
  %3 = icmp sge i32 %0, 0, !dbg !47
  br i1 %3, label %4, label %6, !dbg !48

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !49
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !45
  ret i1 %7, !dbg !45
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !50 !pallas.exprWrapper !43 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !51, metadata !DIExpression()), !dbg !52
  call void @llvm.dbg.value(metadata i32 %1, metadata !53, metadata !DIExpression()), !dbg !52
  %3 = icmp sgt i32 %0, 0, !dbg !54
  ret i1 %3, !dbg !52
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_function_contract_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "b2c55039ef8597bdf6b1007bdadab617")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "54a7e1800bc69cedbeef7efd936420b6")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "foo", scope: !13, file: !13, line: 7, type: !14, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!13 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_function_contract_fail.c", directory: "", checksumkind: CSK_MD5, checksum: "b2c55039ef8597bdf6b1007bdadab617")
!14 = !DISubroutineType(types: !15)
!15 = !{!16, !16, !16}
!16 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!17 = !{}
!18 = !{!19, i1 false, i1 false, !20, !24}
!19 = !{!"pallas.srcLoc", i64 3, i64 1, i64 6, i64 2, !13}
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !22, !23}
!21 = !{!"pallas.srcLoc", i64 4, i64 2, i64 4, i64 27, !13}
!22 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !13, line: 7, type: !16)
!23 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !13, line: 7, type: !16)
!24 = !{!"pallas.ensures", !25, ptr @PALLAS_SPEC_1, !22, !23}
!25 = !{!"pallas.srcLoc", i64 5, i64 2, i64 5, i64 15, !13}
!26 = !DILocation(line: 7, column: 14, scope: !12)
!27 = !DILocation(line: 7, column: 21, scope: !12)
!28 = !DILocalVariable(name: "x", scope: !12, file: !13, line: 8, type: !16)
!29 = !DILocation(line: 8, column: 9, scope: !12)
!30 = !DILocation(line: 8, column: 13, scope: !12)
!31 = !DILocation(line: 8, column: 17, scope: !12)
!32 = !DILocation(line: 8, column: 15, scope: !12)
!33 = !DILocation(line: 9, column: 7, scope: !12)
!34 = !DILocation(line: 10, column: 12, scope: !12)
!35 = !DILocation(line: 10, column: 16, scope: !12)
!36 = !DILocation(line: 10, column: 14, scope: !12)
!37 = !DILocation(line: 10, column: 18, scope: !12)
!38 = !DILocation(line: 10, column: 5, scope: !12)
!39 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !13, file: !13, line: 4, type: !40, scopeLine: 4, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!40 = !DISubroutineType(types: !41)
!41 = !{!42, !16, !16}
!42 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!43 = !{!""}
!44 = !DILocalVariable(name: "a", arg: 1, scope: !39, file: !13, line: 4, type: !16)
!45 = !DILocation(line: 0, scope: !39)
!46 = !DILocalVariable(name: "b", arg: 2, scope: !39, file: !13, line: 4, type: !16)
!47 = !DILocation(line: 4, column: 13, scope: !39)
!48 = !DILocation(line: 4, column: 18, scope: !39)
!49 = !DILocation(line: 4, column: 23, scope: !39)
!50 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !13, file: !13, line: 5, type: !40, scopeLine: 5, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!51 = !DILocalVariable(name: "a", arg: 1, scope: !50, file: !13, line: 5, type: !16)
!52 = !DILocation(line: 0, scope: !50)
!53 = !DILocalVariable(name: "b", arg: 2, scope: !50, file: !13, line: 5, type: !16)
!54 = !DILocation(line: 5, column: 12, scope: !50)
