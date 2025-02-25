; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_perm_fail_3.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [2 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(ptr noundef %0) #0 !dbg !14 !pallas.fcontract !20 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !24, metadata !DIExpression()), !dbg !27
  %3 = load ptr, ptr %2, align 8, !dbg !28
  %4 = load i32, ptr %3, align 4, !dbg !29
  %5 = add nsw i32 %4, 1, !dbg !30
  ret i32 %5, !dbg !31
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !32 !pallas.exprWrapper !36 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !37, metadata !DIExpression()), !dbg !38
  %3 = icmp ne ptr %0, null, !dbg !39
  br i1 %3, label %4, label %6, !dbg !40

4:                                                ; preds = %1
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !41
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !42
  br label %6

6:                                                ; preds = %4, %1
  %7 = phi i1 [ false, %1 ], [ %5, %4 ], !dbg !38
  ret i1 %7, !dbg !38
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !43 !pallas.exprWrapper !36 {
  %2 = alloca %pallas.fracT, align 8
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !44, metadata !DIExpression()), !dbg !45
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 2, i32 noundef 3), !dbg !46
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !47
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 2, i32 noundef 3), !dbg !48
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !49
  %6 = call i1 @pallas.sepConj(i1 %4, i1 %5), !dbg !50
  ret i1 %6, !dbg !45
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !51 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !52 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !53 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!6, !7, !8, !9, !10, !11, !12}
!llvm.ident = !{!13, !13}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_perm_fail_3.c", directory: ".", checksumkind: CSK_MD5, checksum: "fe9d820b810aa50367688f9fb5bdfe9c")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "23608d8f44e0bb8c40f4faaffd1db314")
!4 = !{!5}
!5 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!6 = !{i32 7, !"Dwarf Version", i32 5}
!7 = !{i32 2, !"Debug Info Version", i32 3}
!8 = !{i32 1, !"wchar_size", i32 4}
!9 = !{i32 8, !"PIC Level", i32 2}
!10 = !{i32 7, !"PIE Level", i32 2}
!11 = !{i32 7, !"uwtable", i32 2}
!12 = !{i32 7, !"frame-pointer", i32 2}
!13 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!14 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 6, type: !15, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!15 = !DISubroutineType(types: !16)
!16 = !{!17, !18}
!17 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!18 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !17, size: 64)
!19 = !{}
!20 = !{!21, i1 false, !22, !25}
!21 = !{!"pallas.srcLoc", i64 2, i64 1, i64 5, i64 1}
!22 = !{!"pallas.requires", !23, ptr @PALLAS_SPEC_0, !24}
!23 = !{!"pallas.srcLoc", i64 3, i64 1, i64 3, i64 50}
!24 = !DILocalVariable(name: "iPtr", arg: 1, scope: !14, file: !1, line: 6, type: !18)
!25 = !{!"pallas.ensures", !26, ptr @PALLAS_SPEC_1, !24}
!26 = !{!"pallas.srcLoc", i64 4, i64 1, i64 4, i64 62}
!27 = !DILocation(line: 6, column: 14, scope: !14)
!28 = !DILocation(line: 7, column: 13, scope: !14)
!29 = !DILocation(line: 7, column: 12, scope: !14)
!30 = !DILocation(line: 7, column: 18, scope: !14)
!31 = !DILocation(line: 7, column: 5, scope: !14)
!32 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 3, type: !33, scopeLine: 3, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!33 = !DISubroutineType(types: !34)
!34 = !{!35, !18}
!35 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!36 = !{!""}
!37 = !DILocalVariable(name: "iPtr", arg: 1, scope: !32, file: !1, line: 3, type: !18)
!38 = !DILocation(line: 0, scope: !32)
!39 = !DILocation(line: 3, column: 15, scope: !32)
!40 = !DILocation(line: 3, column: 23, scope: !32)
!41 = !DILocation(line: 3, column: 37, scope: !32)
!42 = !DILocation(line: 3, column: 26, scope: !32)
!43 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 4, type: !33, scopeLine: 4, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!44 = !DILocalVariable(name: "iPtr", arg: 1, scope: !43, file: !1, line: 4, type: !18)
!45 = !DILocation(line: 0, scope: !43)
!46 = !DILocation(line: 4, column: 24, scope: !43)
!47 = !DILocation(line: 4, column: 13, scope: !43)
!48 = !DILocation(line: 4, column: 49, scope: !43)
!49 = !DILocation(line: 4, column: 38, scope: !43)
!50 = !DILocation(line: 4, column: 9, scope: !43)
!51 = !{!"pallas.sepConj"}
!52 = !{!"pallas.perm"}
!53 = !{!"pallas.fracOf"}
