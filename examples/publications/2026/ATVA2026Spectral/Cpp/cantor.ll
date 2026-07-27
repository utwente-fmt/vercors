; ModuleID = 'tmp_spectral/tmp_ir_source0.ll'
source_filename = "examples/publications/2026/ATVA2026Spectral/Cpp/cantor.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@llvm.compiler.used = appending global [3 x ptr] [ptr @_Z13PALLAS_SPEC_0i, ptr @_Z13PALLAS_SPEC_1i, ptr @_Z13PALLAS_SPEC_2ii], section "llvm.metadata"
@llvm.used = appending global [3 x ptr] [ptr @_Z13PALLAS_SPEC_0i, ptr @_Z13PALLAS_SPEC_1i, ptr @_Z13PALLAS_SPEC_2ii], section "llvm.metadata"

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef i32 @_Z10triangulari(i32 noundef %0) #0 !dbg !109 !pallas.fcontract !113 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !120, metadata !DIExpression()), !dbg !126
  %2 = add nsw i32 %0, 1, !dbg !127
  %3 = mul nsw i32 %0, %2, !dbg !128
  %4 = sdiv i32 %3, 2, !dbg !129
  ret i32 %4, !dbg !130
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef i32 @_Z6squarei(i32 noundef %0) #0 !dbg !131 !pallas.fcontract !132 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !138, metadata !DIExpression()), !dbg !141
  %2 = mul nsw i32 %0, %0, !dbg !142
  ret i32 %2, !dbg !143
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef i32 @_Z10cantorPairii(i32 noundef %0, i32 noundef %1) #0 !dbg !144 !pallas.fcontract !147 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !153, metadata !DIExpression()), !dbg !161
  call void @llvm.dbg.value(metadata i32 %1, metadata !159, metadata !DIExpression()), !dbg !161
  %3 = add nsw i32 %0, %1, !dbg !162
  %4 = call noundef i32 @_Z6squarei(i32 noundef %3), !dbg !163
  %5 = add nsw i32 %4, %0, !dbg !164
  %6 = mul nsw i32 3, %1, !dbg !165
  %7 = add nsw i32 %5, %6, !dbg !166
  %8 = sdiv i32 %7, 2, !dbg !167
  ret i32 %8, !dbg !168
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_0i(i32 noundef %0) #0 !dbg !122 !pallas.exprWrapper !169 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !121, metadata !DIExpression()), !dbg !170
  ret i1 true, !dbg !170
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_1i(i32 noundef %0) #2 !dbg !140 !pallas.exprWrapper !169 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !139, metadata !DIExpression()), !dbg !171
  %2 = call noundef i32 @"pallas.result noundef i32"(), !dbg !172
  %3 = mul nsw i32 %0, %0, !dbg !173
  %4 = icmp eq i32 %2, %3, !dbg !174
  ret i1 %4, !dbg !171
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_2ii(i32 noundef %0, i32 noundef %1) #2 !dbg !155 !pallas.exprWrapper !169 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !154, metadata !DIExpression()), !dbg !175
  call void @llvm.dbg.value(metadata i32 %1, metadata !160, metadata !DIExpression()), !dbg !175
  %3 = icmp eq i32 %1, 0, !dbg !176
  %4 = call noundef i32 @"pallas.result noundef i32"(), !dbg !177
  %5 = call noundef i32 @_Z10triangulari(i32 noundef %0), !dbg !178
  %6 = icmp eq i32 %4, %5, !dbg !179
  %7 = call i1 @pallas.imply(i1 %3, i1 %6), !dbg !180
  ret i1 %7, !dbg !175
}

declare !pallas.specLib !181 i1 @pallas.imply(i1, i1)

declare !pallas.specLib !182 noundef i32 @"pallas.result noundef i32"()

attributes #0 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { mustprogress noinline uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!101, !102, !103, !104, !105, !106, !107}
!llvm.ident = !{!108, !108}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/Cpp/cantor.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "1dde5542d25ea760a73ca7fc13a1fe00")
!2 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !3, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, imports: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp_spectral/source_wrappers.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "7cba08fc4daee479e17a16052ebfd6d2")
!4 = !{!5, !13, !17, !21, !25, !28, !30, !32, !34, !38, !41, !44, !47, !50, !52, !57, !61, !65, !69, !71, !73, !75, !77, !80, !83, !86, !89, !92, !94, !99}
!5 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !7, file: !12, line: 51)
!6 = !DINamespace(name: "std", scope: null)
!7 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !8, line: 24, baseType: !9)
!8 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!9 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int8_t", file: !10, line: 37, baseType: !11)
!10 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!11 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!12 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstdint", directory: "")
!13 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !14, file: !12, line: 52)
!14 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !8, line: 25, baseType: !15)
!15 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int16_t", file: !10, line: 39, baseType: !16)
!16 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!17 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !18, file: !12, line: 53)
!18 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !8, line: 26, baseType: !19)
!19 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !10, line: 41, baseType: !20)
!20 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!21 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !22, file: !12, line: 54)
!22 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !8, line: 27, baseType: !23)
!23 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !10, line: 44, baseType: !24)
!24 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!25 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !26, file: !12, line: 56)
!26 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !27, line: 47, baseType: !11)
!27 = !DIFile(filename: "/usr/include/stdint.h", directory: "", checksumkind: CSK_MD5, checksum: "bfb03fa9c46a839e35c32b929fbdbb8e")
!28 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !29, file: !12, line: 57)
!29 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !27, line: 49, baseType: !24)
!30 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !31, file: !12, line: 58)
!31 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !27, line: 50, baseType: !24)
!32 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !33, file: !12, line: 59)
!33 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !27, line: 51, baseType: !24)
!34 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !35, file: !12, line: 61)
!35 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !36, line: 25, baseType: !37)
!36 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-least.h", directory: "", checksumkind: CSK_MD5, checksum: "a866be81c480920b0293bd5f6336a0a3")
!37 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least8_t", file: !10, line: 52, baseType: !9)
!38 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !39, file: !12, line: 62)
!39 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !36, line: 26, baseType: !40)
!40 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least16_t", file: !10, line: 54, baseType: !15)
!41 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !42, file: !12, line: 63)
!42 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !36, line: 27, baseType: !43)
!43 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least32_t", file: !10, line: 56, baseType: !19)
!44 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !45, file: !12, line: 64)
!45 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !36, line: 28, baseType: !46)
!46 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least64_t", file: !10, line: 58, baseType: !23)
!47 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !48, file: !12, line: 66)
!48 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !27, line: 90, baseType: !49)
!49 = !DIDerivedType(tag: DW_TAG_typedef, name: "__intmax_t", file: !10, line: 72, baseType: !24)
!50 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !51, file: !12, line: 67)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !27, line: 76, baseType: !24)
!52 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !53, file: !12, line: 69)
!53 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !54, line: 24, baseType: !55)
!54 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-uintn.h", directory: "", checksumkind: CSK_MD5, checksum: "256fcabbefa27ca8cf5e6d37525e6e16")
!55 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !10, line: 38, baseType: !56)
!56 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!57 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !58, file: !12, line: 70)
!58 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !54, line: 25, baseType: !59)
!59 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !10, line: 40, baseType: !60)
!60 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!61 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !62, file: !12, line: 71)
!62 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !54, line: 26, baseType: !63)
!63 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint32_t", file: !10, line: 42, baseType: !64)
!64 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!65 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !66, file: !12, line: 72)
!66 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !54, line: 27, baseType: !67)
!67 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint64_t", file: !10, line: 45, baseType: !68)
!68 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!69 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !70, file: !12, line: 74)
!70 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !27, line: 60, baseType: !56)
!71 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !72, file: !12, line: 75)
!72 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !27, line: 62, baseType: !68)
!73 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !74, file: !12, line: 76)
!74 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !27, line: 63, baseType: !68)
!75 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !76, file: !12, line: 77)
!76 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !27, line: 64, baseType: !68)
!77 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !78, file: !12, line: 79)
!78 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !36, line: 31, baseType: !79)
!79 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least8_t", file: !10, line: 53, baseType: !55)
!80 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !81, file: !12, line: 80)
!81 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !36, line: 32, baseType: !82)
!82 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least16_t", file: !10, line: 55, baseType: !59)
!83 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !84, file: !12, line: 81)
!84 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !36, line: 33, baseType: !85)
!85 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least32_t", file: !10, line: 57, baseType: !63)
!86 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !87, file: !12, line: 82)
!87 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !36, line: 34, baseType: !88)
!88 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least64_t", file: !10, line: 59, baseType: !67)
!89 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !90, file: !12, line: 84)
!90 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !27, line: 91, baseType: !91)
!91 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uintmax_t", file: !10, line: 73, baseType: !68)
!92 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !93, file: !12, line: 85)
!93 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !27, line: 79, baseType: !68)
!94 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !6, entity: !95, file: !98, line: 58)
!95 = !DIDerivedType(tag: DW_TAG_typedef, name: "max_align_t", file: !96, line: 24, baseType: !97)
!96 = !DIFile(filename: "/usr/lib/llvm-17/lib/clang/17/include/__stddef_max_align_t.h", directory: "", checksumkind: CSK_MD5, checksum: "48e8e2456f77e6cda35d245130fa7259")
!97 = !DICompositeType(tag: DW_TAG_structure_type, file: !96, line: 19, size: 256, flags: DIFlagFwdDecl, identifier: "_ZTS11max_align_t")
!98 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstddef", directory: "")
!99 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !2, entity: !100, file: !3, line: 4)
!100 = !DINamespace(name: "pallasSpec", scope: null)
!101 = !{i32 7, !"Dwarf Version", i32 5}
!102 = !{i32 2, !"Debug Info Version", i32 3}
!103 = !{i32 1, !"wchar_size", i32 4}
!104 = !{i32 8, !"PIC Level", i32 2}
!105 = !{i32 7, !"PIE Level", i32 2}
!106 = !{i32 7, !"uwtable", i32 2}
!107 = !{i32 7, !"frame-pointer", i32 2}
!108 = !{!"Ubuntu clang version 17.0.6 (9ubuntu1)"}
!109 = distinct !DISubprogram(name: "triangular", linkageName: "_Z10triangulari", scope: !1, file: !1, line: 11, type: !110, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!110 = !DISubroutineType(types: !111)
!111 = !{!20, !20}
!112 = !{}
!113 = !{!114, i1 true, i1 false, !112, !112, !116}
!114 = !{!"pallas.srcLoc", i64 7, i64 1, i64 10, i64 1, !115}
!115 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/Cpp/cantor.cpp", directory: "", checksumkind: CSK_MD5, checksum: "1dde5542d25ea760a73ca7fc13a1fe00")
!116 = !{!"pallas.requires", !117, ptr @_Z13PALLAS_SPEC_0i, !112, !112, !118}
!117 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 14, !115}
!118 = !{!119}
!119 = !{!120, !121}
!120 = !DILocalVariable(name: "n", arg: 1, scope: !109, file: !1, line: 11, type: !20)
!121 = !DILocalVariable(name: "n", arg: 1, scope: !122, file: !1, line: 9, type: !20)
!122 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_Z13PALLAS_SPEC_0i", scope: !1, file: !1, line: 9, type: !123, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!123 = !DISubroutineType(types: !124)
!124 = !{!125, !20}
!125 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!126 = !DILocation(line: 0, scope: !109)
!127 = !DILocation(line: 12, column: 20, scope: !109)
!128 = !DILocation(line: 12, column: 15, scope: !109)
!129 = !DILocation(line: 12, column: 26, scope: !109)
!130 = !DILocation(line: 12, column: 5, scope: !109)
!131 = distinct !DISubprogram(name: "square", linkageName: "_Z6squarei", scope: !1, file: !1, line: 19, type: !110, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!132 = !{!133, i1 true, i1 false, !112, !112, !134}
!133 = !{!"pallas.srcLoc", i64 15, i64 1, i64 18, i64 1, !115}
!134 = !{!"pallas.ensures", !135, ptr @_Z13PALLAS_SPEC_1i, !112, !112, !136}
!135 = !{!"pallas.srcLoc", i64 17, i64 1, i64 17, i64 32, !115}
!136 = !{!137}
!137 = !{!138, !139}
!138 = !DILocalVariable(name: "n", arg: 1, scope: !131, file: !1, line: 19, type: !20)
!139 = !DILocalVariable(name: "n", arg: 1, scope: !140, file: !1, line: 17, type: !20)
!140 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_Z13PALLAS_SPEC_1i", scope: !1, file: !1, line: 17, type: !123, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!141 = !DILocation(line: 0, scope: !131)
!142 = !DILocation(line: 20, column: 14, scope: !131)
!143 = !DILocation(line: 20, column: 5, scope: !131)
!144 = distinct !DISubprogram(name: "cantorPair", linkageName: "_Z10cantorPairii", scope: !1, file: !1, line: 28, type: !145, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!145 = !DISubroutineType(types: !146)
!146 = !{!20, !20, !20}
!147 = !{!148, i1 true, i1 false, !112, !112, !149}
!148 = !{!"pallas.srcLoc", i64 23, i64 1, i64 27, i64 1, !115}
!149 = !{!"pallas.ensures", !150, ptr @_Z13PALLAS_SPEC_2ii, !112, !112, !151}
!150 = !{!"pallas.srcLoc", i64 25, i64 1, i64 26, i64 48, !115}
!151 = !{!152, !158}
!152 = !{!153, !154}
!153 = !DILocalVariable(name: "x", arg: 1, scope: !144, file: !1, line: 28, type: !20)
!154 = !DILocalVariable(name: "x", arg: 1, scope: !155, file: !1, line: 25, type: !20)
!155 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_Z13PALLAS_SPEC_2ii", scope: !1, file: !1, line: 25, type: !156, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!156 = !DISubroutineType(types: !157)
!157 = !{!125, !20, !20}
!158 = !{!159, !160}
!159 = !DILocalVariable(name: "y", arg: 2, scope: !144, file: !1, line: 28, type: !20)
!160 = !DILocalVariable(name: "y", arg: 2, scope: !155, file: !1, line: 25, type: !20)
!161 = !DILocation(line: 0, scope: !144)
!162 = !DILocation(line: 29, column: 22, scope: !144)
!163 = !DILocation(line: 29, column: 13, scope: !144)
!164 = !DILocation(line: 29, column: 27, scope: !144)
!165 = !DILocation(line: 29, column: 36, scope: !144)
!166 = !DILocation(line: 29, column: 31, scope: !144)
!167 = !DILocation(line: 29, column: 42, scope: !144)
!168 = !DILocation(line: 29, column: 5, scope: !144)
!169 = !{!""}
!170 = !DILocation(line: 0, scope: !122)
!171 = !DILocation(line: 0, scope: !140)
!172 = !DILocation(line: 17, column: 9, scope: !140)
!173 = !DILocation(line: 17, column: 29, scope: !140)
!174 = !DILocation(line: 17, column: 24, scope: !140)
!175 = !DILocation(line: 0, scope: !155)
!176 = !DILocation(line: 25, column: 18, scope: !155)
!177 = !DILocation(line: 26, column: 16, scope: !155)
!178 = !DILocation(line: 26, column: 34, scope: !155)
!179 = !DILocation(line: 26, column: 31, scope: !155)
!180 = !DILocation(line: 25, column: 9, scope: !155)
!181 = !{!"pallas.imply"}
!182 = !{!"pallas.result"}
