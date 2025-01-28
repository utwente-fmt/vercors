; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_function_contract_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [2 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !17 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !21, metadata !DIExpression()), !dbg !25
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !22, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.declare(metadata ptr %5, metadata !27, metadata !DIExpression()), !dbg !28
  %6 = load i32, ptr %3, align 4, !dbg !29
  %7 = load i32, ptr %4, align 4, !dbg !30
  %8 = add nsw i32 %6, %7, !dbg !31
  store i32 %8, ptr %5, align 4, !dbg !28
  %9 = load i32, ptr %5, align 4, !dbg !32
  %10 = add nsw i32 %9, 1, !dbg !32
  store i32 %10, ptr %5, align 4, !dbg !32
  %11 = load i32, ptr %5, align 4, !dbg !33
  %12 = load i32, ptr %4, align 4, !dbg !34
  %13 = mul nsw i32 %11, %12, !dbg !35
  %14 = add nsw i32 %13, 1, !dbg !36
  ret i32 %14, !dbg !37
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !38 !pallas.exprWrapper !42 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !43, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata i32 %1, metadata !45, metadata !DIExpression()), !dbg !44
  %3 = icmp sge i32 %0, 0, !dbg !46
  br i1 %3, label %4, label %6, !dbg !47

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !48
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !44
  ret i1 %7, !dbg !44
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !49 !pallas.exprWrapper !42 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !50, metadata !DIExpression()), !dbg !51
  call void @llvm.dbg.value(metadata i32 %1, metadata !52, metadata !DIExpression()), !dbg !51
  %3 = icmp sgt i32 %0, 0, !dbg !53
  ret i1 %3, !dbg !51
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_function_contract_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "b2c55039ef8597bdf6b1007bdadab617")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "db2f546b04510fe9d3ba505f9cc0522b")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 7, type: !13, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, !19, !23}
!18 = !{!"pallas.srcLoc", i64 3, i64 1, i64 6, i64 2}
!19 = !{!"pallas.requires", !20, ptr @PALLAS_SPEC_0, !21, !22}
!20 = !{!"pallas.srcLoc", i64 4, i64 2, i64 4, i64 27}
!21 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 7, type: !15)
!22 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 7, type: !15)
!23 = !{!"pallas.ensures", !24, ptr @PALLAS_SPEC_1, !21, !22}
!24 = !{!"pallas.srcLoc", i64 5, i64 2, i64 5, i64 15}
!25 = !DILocation(line: 7, column: 14, scope: !12)
!26 = !DILocation(line: 7, column: 21, scope: !12)
!27 = !DILocalVariable(name: "x", scope: !12, file: !1, line: 8, type: !15)
!28 = !DILocation(line: 8, column: 9, scope: !12)
!29 = !DILocation(line: 8, column: 13, scope: !12)
!30 = !DILocation(line: 8, column: 17, scope: !12)
!31 = !DILocation(line: 8, column: 15, scope: !12)
!32 = !DILocation(line: 9, column: 7, scope: !12)
!33 = !DILocation(line: 10, column: 12, scope: !12)
!34 = !DILocation(line: 10, column: 16, scope: !12)
!35 = !DILocation(line: 10, column: 14, scope: !12)
!36 = !DILocation(line: 10, column: 18, scope: !12)
!37 = !DILocation(line: 10, column: 5, scope: !12)
!38 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 4, type: !39, scopeLine: 4, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!39 = !DISubroutineType(types: !40)
!40 = !{!41, !15, !15}
!41 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!42 = !{!""}
!43 = !DILocalVariable(name: "a", arg: 1, scope: !38, file: !1, line: 4, type: !15)
!44 = !DILocation(line: 0, scope: !38)
!45 = !DILocalVariable(name: "b", arg: 2, scope: !38, file: !1, line: 4, type: !15)
!46 = !DILocation(line: 4, column: 13, scope: !38)
!47 = !DILocation(line: 4, column: 18, scope: !38)
!48 = !DILocation(line: 4, column: 23, scope: !38)
!49 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 5, type: !39, scopeLine: 5, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!50 = !DILocalVariable(name: "a", arg: 1, scope: !49, file: !1, line: 5, type: !15)
!51 = !DILocation(line: 0, scope: !49)
!52 = !DILocalVariable(name: "b", arg: 2, scope: !49, file: !1, line: 5, type: !15)
!53 = !DILocation(line: 5, column: 12, scope: !49)
