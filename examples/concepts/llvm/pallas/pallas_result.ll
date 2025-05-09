; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_result.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.s = type { i64, i64, i64, i64, i64, i64, i64 }

@llvm.used = appending global [2 x ptr] [ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_0], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local void @fun(ptr noalias sret(%struct.s) align 8 %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !32 {
  %3 = alloca i32, align 4
  store i32 %1, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !36, metadata !DIExpression()), !dbg !37
  call void @llvm.dbg.declare(metadata ptr %0, metadata !38, metadata !DIExpression()), !dbg !39
  %4 = getelementptr inbounds %struct.s, ptr %0, i32 0, i32 0, !dbg !40
  store i64 0, ptr %4, align 8, !dbg !41
  %5 = getelementptr inbounds %struct.s, ptr %0, i32 0, i32 1, !dbg !42
  store i64 1, ptr %5, align 8, !dbg !43
  ret void, !dbg !44
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bar(i32 noundef %0) #0 !dbg !45 !pallas.fcontract !48 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !52, metadata !DIExpression()), !dbg !53
  call void @llvm.dbg.declare(metadata ptr %3, metadata !54, metadata !DIExpression()), !dbg !55
  %4 = load i32, ptr %2, align 4, !dbg !56
  %5 = icmp sgt i32 %4, 0, !dbg !57
  br i1 %5, label %6, label %8, !dbg !56

6:                                                ; preds = %1
  %7 = load i32, ptr %2, align 4, !dbg !58
  br label %11, !dbg !56

8:                                                ; preds = %1
  %9 = load i32, ptr %2, align 4, !dbg !59
  %10 = sub nsw i32 0, %9, !dbg !60
  br label %11, !dbg !56

11:                                               ; preds = %8, %6
  %12 = phi i32 [ %7, %6 ], [ %10, %8 ], !dbg !56
  store i32 %12, ptr %3, align 4, !dbg !55
  %13 = load i32, ptr %3, align 4, !dbg !61
  ret i32 %13, !dbg !62
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !63 !pallas.exprWrapper !67 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !68, metadata !DIExpression()), !dbg !69
  %2 = call i32 @pallas.result.0(), !dbg !70
  %3 = icmp sge i32 %2, 0, !dbg !71
  ret i1 %3, !dbg !69
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !72 !pallas.exprWrapper !67 {
  %2 = alloca %struct.s, align 8
  call void @llvm.dbg.value(metadata i32 %0, metadata !73, metadata !DIExpression()), !dbg !74
  call void @pallas.result.1(ptr sret(%struct.s) align 8 %2), !dbg !75
  %3 = getelementptr inbounds %struct.s, ptr %2, i32 0, i32 0, !dbg !76
  %4 = load i64, ptr %3, align 8, !dbg !76
  %5 = icmp sge i64 %4, 0, !dbg !77
  ret i1 %5, !dbg !74
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !78 i32 @pallas.result.0()

declare !pallas.specLib !78 void @pallas.result.1(ptr sret(%struct.s) align 8)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_result.c", directory: ".", checksumkind: CSK_MD5, checksum: "13e3e1392be78a2bfa168a38fc479422")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "91ae7c52e1a2a3d7711d4b22bf259e3e")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "fun", scope: !1, file: !1, line: 29, type: !13, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !30}
!15 = !DIDerivedType(tag: DW_TAG_typedef, name: "BigStruct", file: !1, line: 17, baseType: !16)
!16 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "s", file: !1, line: 9, size: 448, elements: !17)
!17 = !{!18, !24, !25, !26, !27, !28, !29}
!18 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !16, file: !1, line: 10, baseType: !19, size: 64)
!19 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !20, line: 27, baseType: !21)
!20 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!21 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !22, line: 44, baseType: !23)
!22 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!23 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!24 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !16, file: !1, line: 11, baseType: !19, size: 64, offset: 64)
!25 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !16, file: !1, line: 12, baseType: !19, size: 64, offset: 128)
!26 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !16, file: !1, line: 13, baseType: !19, size: 64, offset: 192)
!27 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !16, file: !1, line: 14, baseType: !19, size: 64, offset: 256)
!28 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !16, file: !1, line: 15, baseType: !19, size: 64, offset: 320)
!29 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !16, file: !1, line: 16, baseType: !19, size: 64, offset: 384)
!30 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!31 = !{}
!32 = !{!33, i1 false, !34}
!33 = !{!"pallas.srcLoc", i64 26, i64 1, i64 28, i64 1}
!34 = !{!"pallas.ensures", !35, ptr @PALLAS_SPEC_0, !36}
!35 = !{!"pallas.srcLoc", i64 27, i64 1, i64 27, i64 35}
!36 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 29, type: !30)
!37 = !DILocation(line: 29, column: 20, scope: !12)
!38 = !DILocalVariable(name: "s", scope: !12, file: !1, line: 30, type: !15)
!39 = !DILocation(line: 30, column: 15, scope: !12)
!40 = !DILocation(line: 31, column: 7, scope: !12)
!41 = !DILocation(line: 31, column: 9, scope: !12)
!42 = !DILocation(line: 32, column: 7, scope: !12)
!43 = !DILocation(line: 32, column: 9, scope: !12)
!44 = !DILocation(line: 33, column: 5, scope: !12)
!45 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 40, type: !46, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!46 = !DISubroutineType(types: !47)
!47 = !{!30, !30}
!48 = !{!49, i1 false, !50}
!49 = !{!"pallas.srcLoc", i64 37, i64 1, i64 39, i64 1}
!50 = !{!"pallas.ensures", !51, ptr @PALLAS_SPEC_1, !52}
!51 = !{!"pallas.srcLoc", i64 38, i64 1, i64 38, i64 27}
!52 = !DILocalVariable(name: "x", arg: 1, scope: !45, file: !1, line: 40, type: !30)
!53 = !DILocation(line: 40, column: 14, scope: !45)
!54 = !DILocalVariable(name: "y", scope: !45, file: !1, line: 41, type: !30)
!55 = !DILocation(line: 41, column: 9, scope: !45)
!56 = !DILocation(line: 41, column: 13, scope: !45)
!57 = !DILocation(line: 41, column: 15, scope: !45)
!58 = !DILocation(line: 41, column: 21, scope: !45)
!59 = !DILocation(line: 41, column: 26, scope: !45)
!60 = !DILocation(line: 41, column: 25, scope: !45)
!61 = !DILocation(line: 42, column: 12, scope: !45)
!62 = !DILocation(line: 42, column: 5, scope: !45)
!63 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 38, type: !64, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!64 = !DISubroutineType(types: !65)
!65 = !{!66, !30}
!66 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!67 = !{!""}
!68 = !DILocalVariable(name: "x", arg: 1, scope: !63, file: !1, line: 38, type: !30)
!69 = !DILocation(line: 0, scope: !63)
!70 = !DILocation(line: 38, column: 9, scope: !63)
!71 = !DILocation(line: 38, column: 23, scope: !63)
!72 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 27, type: !64, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!73 = !DILocalVariable(name: "a", arg: 1, scope: !72, file: !1, line: 27, type: !30)
!74 = !DILocation(line: 0, scope: !72)
!75 = !DILocation(line: 27, column: 9, scope: !72)
!76 = !DILocation(line: 27, column: 29, scope: !72)
!77 = !DILocation(line: 27, column: 31, scope: !72)
!78 = !{!"pallas.result"}
