; ModuleID = 'tmp/tmp_ir_source0.ll'
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
  call void @llvm.dbg.declare(metadata ptr %3, metadata !24, metadata !DIExpression()), !dbg !41
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !31, metadata !DIExpression()), !dbg !42
  call void @llvm.dbg.declare(metadata ptr %5, metadata !43, metadata !DIExpression()), !dbg !44
  %6 = load i32, ptr %3, align 4, !dbg !45
  %7 = load i32, ptr %4, align 4, !dbg !46
  %8 = add nsw i32 %6, %7, !dbg !47
  store i32 %8, ptr %5, align 4, !dbg !44
  %9 = load i32, ptr %5, align 4, !dbg !48
  %10 = add nsw i32 %9, 1, !dbg !48
  store i32 %10, ptr %5, align 4, !dbg !48
  %11 = load i32, ptr %5, align 4, !dbg !49
  %12 = load i32, ptr %4, align 4, !dbg !50
  %13 = mul nsw i32 %11, %12, !dbg !51
  %14 = add nsw i32 %13, 1, !dbg !52
  ret i32 %14, !dbg !53
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !26 !pallas.exprWrapper !54 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !55
  call void @llvm.dbg.value(metadata i32 %1, metadata !32, metadata !DIExpression()), !dbg !55
  %3 = icmp sge i32 %0, 0, !dbg !56
  br i1 %3, label %4, label %6, !dbg !57

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !58
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !55
  ret i1 %7, !dbg !55
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !38 !pallas.exprWrapper !54 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !37, metadata !DIExpression()), !dbg !59
  call void @llvm.dbg.value(metadata i32 %1, metadata !40, metadata !DIExpression()), !dbg !59
  %3 = icmp sgt i32 %0, 0, !dbg !60
  ret i1 %3, !dbg !59
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
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "54a7e1800bc69cedbeef7efd936420b6")
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
!17 = !{!18, i1 false, i1 false, !16, !16, !20, !33}
!18 = !{!"pallas.srcLoc", i64 3, i64 1, i64 6, i64 2, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_function_contract_fail.c", directory: "", checksumkind: CSK_MD5, checksum: "b2c55039ef8597bdf6b1007bdadab617")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !16, !16, !22}
!21 = !{!"pallas.srcLoc", i64 4, i64 2, i64 4, i64 27, !19}
!22 = !{!23, !30}
!23 = !{!24, !25}
!24 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 7, type: !15)
!25 = !DILocalVariable(name: "a", arg: 1, scope: !26, file: !1, line: 4, type: !15)
!26 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 4, type: !27, scopeLine: 4, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!27 = !DISubroutineType(types: !28)
!28 = !{!29, !15, !15}
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !{!31, !32}
!31 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 7, type: !15)
!32 = !DILocalVariable(name: "b", arg: 2, scope: !26, file: !1, line: 4, type: !15)
!33 = !{!"pallas.ensures", !34, ptr @PALLAS_SPEC_1, !16, !16, !35}
!34 = !{!"pallas.srcLoc", i64 5, i64 2, i64 5, i64 15, !19}
!35 = !{!36, !39}
!36 = !{!24, !37}
!37 = !DILocalVariable(name: "a", arg: 1, scope: !38, file: !1, line: 5, type: !15)
!38 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 5, type: !27, scopeLine: 5, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!39 = !{!31, !40}
!40 = !DILocalVariable(name: "b", arg: 2, scope: !38, file: !1, line: 5, type: !15)
!41 = !DILocation(line: 7, column: 14, scope: !12)
!42 = !DILocation(line: 7, column: 21, scope: !12)
!43 = !DILocalVariable(name: "x", scope: !12, file: !1, line: 8, type: !15)
!44 = !DILocation(line: 8, column: 9, scope: !12)
!45 = !DILocation(line: 8, column: 13, scope: !12)
!46 = !DILocation(line: 8, column: 17, scope: !12)
!47 = !DILocation(line: 8, column: 15, scope: !12)
!48 = !DILocation(line: 9, column: 7, scope: !12)
!49 = !DILocation(line: 10, column: 12, scope: !12)
!50 = !DILocation(line: 10, column: 16, scope: !12)
!51 = !DILocation(line: 10, column: 14, scope: !12)
!52 = !DILocation(line: 10, column: 18, scope: !12)
!53 = !DILocation(line: 10, column: 5, scope: !12)
!54 = !{!""}
!55 = !DILocation(line: 0, scope: !26)
!56 = !DILocation(line: 4, column: 13, scope: !26)
!57 = !DILocation(line: 4, column: 18, scope: !26)
!58 = !DILocation(line: 4, column: 23, scope: !26)
!59 = !DILocation(line: 0, scope: !38)
!60 = !DILocation(line: 5, column: 12, scope: !38)
