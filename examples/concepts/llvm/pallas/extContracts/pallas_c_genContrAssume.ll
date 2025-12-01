; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/extContracts/pallas_c_genContrAssume.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [2 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo() #0 !dbg !12 !pallas.fcontract !17 {
  %1 = alloca i32, align 4
  call void @llvm.dbg.declare(metadata ptr %1, metadata !22, metadata !DIExpression()), !dbg !23
  store i32 0, ptr %1, align 4, !dbg !23
  %2 = load i32, ptr %1, align 4, !dbg !24
  %3 = add nsw i32 %2, 3, !dbg !24
  store i32 %3, ptr %1, align 4, !dbg !24
  %4 = load i32, ptr %1, align 4, !dbg !25
  %5 = mul nsw i32 %4, 2, !dbg !25
  store i32 %5, ptr %1, align 4, !dbg !25
  %6 = load i32, ptr %1, align 4, !dbg !26
  ret i32 %6, !dbg !27
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @main() #0 !dbg !28 !pallas.fcontract !29 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 0, ptr %1, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !33, metadata !DIExpression()), !dbg !34
  store i32 1, ptr %2, align 4, !dbg !34
  call void @llvm.dbg.declare(metadata ptr %3, metadata !35, metadata !DIExpression()), !dbg !36
  %4 = call i32 @foo(), !dbg !37
  store i32 %4, ptr %3, align 4, !dbg !36
  %5 = load i32, ptr %2, align 4, !dbg !38
  %6 = load i32, ptr %3, align 4, !dbg !39
  %7 = add nsw i32 %5, %6, !dbg !40
  ret i32 %7, !dbg !41
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0() #0 !dbg !42 !pallas.exprWrapper !46 {
  %1 = call i32 @"pallas.result i32"(), !dbg !47
  %2 = icmp eq i32 %1, 2, !dbg !48
  ret i1 %2, !dbg !49
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1() #0 !dbg !50 !pallas.exprWrapper !46 {
  %1 = call i32 @"pallas.result i32"(), !dbg !51
  %2 = icmp eq i32 %1, 3, !dbg !52
  ret i1 %2, !dbg !53
}

declare !pallas.specLib !54 i32 @"pallas.result i32"()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/extContracts/pallas_c_genContrAssume.c", directory: ".", checksumkind: CSK_MD5, checksum: "02bf3a76cd098a78dde2d257e6705b8f")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "9bc5ca613ee83ef22c6024fbfa34bad1")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 15, type: !13, scopeLine: 15, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, i1 true, !20}
!18 = !{!"pallas.srcLoc", i64 7, i64 1, i64 10, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_c_genContrAssume.c", directory: "", checksumkind: CSK_MD5, checksum: "02bf3a76cd098a78dde2d257e6705b8f")
!20 = !{!"pallas.ensures", !21, ptr @PALLAS_SPEC_0}
!21 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 26, !19}
!22 = !DILocalVariable(name: "i", scope: !12, file: !1, line: 16, type: !15)
!23 = !DILocation(line: 16, column: 9, scope: !12)
!24 = !DILocation(line: 17, column: 7, scope: !12)
!25 = !DILocation(line: 18, column: 7, scope: !12)
!26 = !DILocation(line: 19, column: 12, scope: !12)
!27 = !DILocation(line: 19, column: 5, scope: !12)
!28 = distinct !DISubprogram(name: "main", scope: !1, file: !1, line: 26, type: !13, scopeLine: 26, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!29 = !{!30, i1 false, i1 false, !31}
!30 = !{!"pallas.srcLoc", i64 23, i64 1, i64 25, i64 1, !19}
!31 = !{!"pallas.ensures", !32, ptr @PALLAS_SPEC_1}
!32 = !{!"pallas.srcLoc", i64 24, i64 1, i64 24, i64 26, !19}
!33 = !DILocalVariable(name: "a", scope: !28, file: !1, line: 27, type: !15)
!34 = !DILocation(line: 27, column: 9, scope: !28)
!35 = !DILocalVariable(name: "b", scope: !28, file: !1, line: 28, type: !15)
!36 = !DILocation(line: 28, column: 9, scope: !28)
!37 = !DILocation(line: 28, column: 13, scope: !28)
!38 = !DILocation(line: 29, column: 12, scope: !28)
!39 = !DILocation(line: 29, column: 16, scope: !28)
!40 = !DILocation(line: 29, column: 14, scope: !28)
!41 = !DILocation(line: 29, column: 5, scope: !28)
!42 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 9, type: !43, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0)
!43 = !DISubroutineType(types: !44)
!44 = !{!45}
!45 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!46 = !{!""}
!47 = !DILocation(line: 9, column: 9, scope: !42)
!48 = !DILocation(line: 9, column: 22, scope: !42)
!49 = !DILocation(line: 0, scope: !42)
!50 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 24, type: !43, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0)
!51 = !DILocation(line: 24, column: 9, scope: !50)
!52 = !DILocation(line: 24, column: 22, scope: !50)
!53 = !DILocation(line: 0, scope: !50)
!54 = !{!"pallas.result"}
