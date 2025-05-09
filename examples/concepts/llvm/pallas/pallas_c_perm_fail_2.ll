; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_perm_fail_2.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.S = type { i64, i64, i64, i64, i64, i64, i64 }

@llvm.used = appending global [2 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local void @bar(ptr noundef %0) #0 !dbg !14 !pallas.fcontract !34 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !38, metadata !DIExpression()), !dbg !41
  %3 = load ptr, ptr %2, align 8, !dbg !42
  %4 = getelementptr inbounds %struct.S, ptr %3, i32 0, i32 0, !dbg !43
  store i64 0, ptr %4, align 8, !dbg !44
  ret void, !dbg !45
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !46 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !63, metadata !DIExpression()), !dbg !64
  %2 = icmp ne ptr %0, null, !dbg !65
  ret i1 %2, !dbg !64
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !66 !pallas.exprWrapper !62 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !67, metadata !DIExpression()), !dbg !68
  %2 = getelementptr inbounds %struct.S, ptr %0, i32 0, i32 0, !dbg !69
  %3 = load i64, ptr %2, align 8, !dbg !69
  %4 = icmp eq i64 %3, 0, !dbg !70
  ret i1 %4, !dbg !68
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!6, !7, !8, !9, !10, !11, !12}
!llvm.ident = !{!13, !13}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_perm_fail_2.c", directory: ".", checksumkind: CSK_MD5, checksum: "6e88c295299820f9e6cdc988aadaa1d0")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "b0e0828c26c7850eec017e1ce77b1789")
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
!14 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 13, type: !15, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !33)
!15 = !DISubroutineType(types: !16)
!16 = !{null, !17}
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !18, size: 64)
!18 = !DIDerivedType(tag: DW_TAG_typedef, name: "BigStruct", file: !1, line: 7, baseType: !19)
!19 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "S", file: !1, line: 5, size: 448, elements: !20)
!20 = !{!21, !27, !28, !29, !30, !31, !32}
!21 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !19, file: !1, line: 6, baseType: !22, size: 64)
!22 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !23, line: 27, baseType: !24)
!23 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!24 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !25, line: 44, baseType: !26)
!25 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!26 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!27 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 64)
!28 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 128)
!29 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 192)
!30 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 256)
!31 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 320)
!32 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 384)
!33 = !{}
!34 = !{!35, i1 false, !36, !39}
!35 = !{!"pallas.srcLoc", i64 9, i64 1, i64 12, i64 1}
!36 = !{!"pallas.requires", !37, ptr @PALLAS_SPEC_0, !38}
!37 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 19}
!38 = !DILocalVariable(name: "s", arg: 1, scope: !14, file: !1, line: 13, type: !17)
!39 = !{!"pallas.ensures", !40, ptr @PALLAS_SPEC_1, !38}
!40 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 18}
!41 = !DILocation(line: 13, column: 21, scope: !14)
!42 = !DILocation(line: 14, column: 5, scope: !14)
!43 = !DILocation(line: 14, column: 8, scope: !14)
!44 = !DILocation(line: 14, column: 10, scope: !14)
!45 = !DILocation(line: 15, column: 1, scope: !14)
!46 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 10, type: !47, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !33)
!47 = !DISubroutineType(types: !48)
!48 = !{!49, !50}
!49 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!50 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !51, size: 64)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "BigStruct", file: !52, line: 8, baseType: !53)
!52 = !DIFile(filename: "./tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "b0e0828c26c7850eec017e1ce77b1789")
!53 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "S", file: !52, line: 6, size: 448, elements: !54)
!54 = !{!55, !56, !57, !58, !59, !60, !61}
!55 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !53, file: !52, line: 7, baseType: !22, size: 64)
!56 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !53, file: !52, line: 7, baseType: !22, size: 64, offset: 64)
!57 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !53, file: !52, line: 7, baseType: !22, size: 64, offset: 128)
!58 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !53, file: !52, line: 7, baseType: !22, size: 64, offset: 192)
!59 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !53, file: !52, line: 7, baseType: !22, size: 64, offset: 256)
!60 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !53, file: !52, line: 7, baseType: !22, size: 64, offset: 320)
!61 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !53, file: !52, line: 7, baseType: !22, size: 64, offset: 384)
!62 = !{!""}
!63 = !DILocalVariable(name: "s", arg: 1, scope: !46, file: !1, line: 10, type: !50)
!64 = !DILocation(line: 0, scope: !46)
!65 = !DILocation(line: 10, column: 12, scope: !46)
!66 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 11, type: !47, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !33)
!67 = !DILocalVariable(name: "s", arg: 1, scope: !66, file: !1, line: 11, type: !50)
!68 = !DILocation(line: 0, scope: !66)
!69 = !DILocation(line: 11, column: 12, scope: !66)
!70 = !DILocation(line: 11, column: 14, scope: !66)
