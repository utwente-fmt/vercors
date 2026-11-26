; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_cpp_resultRef.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.AClass = type { i32, i32 }
%pallas.fracT = type { i64, i64, i64, i64 }

$_ZN6AClassD2Ev = comdat any

$_ZN6AClass13PALLAS_SPEC_0Ev = comdat any

@llvm.compiler.used = appending global [3 x ptr] [ptr @_ZN6AClass13PALLAS_SPEC_0Ev, ptr @_Z13PALLAS_SPEC_1v, ptr @_Z13PALLAS_SPEC_2v], section "llvm.metadata"
@llvm.used = appending global [3 x ptr] [ptr @_ZN6AClass13PALLAS_SPEC_0Ev, ptr @_Z13PALLAS_SPEC_1v, ptr @_Z13PALLAS_SPEC_2v], section "llvm.metadata"

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local void @_Z15make_init_classv(ptr noalias sret(%class.AClass) align 4 %0) #0 !dbg !109 !pallas.fcontract !121 {
  %2 = alloca ptr, align 8
  %3 = alloca i1, align 1
  store ptr %0, ptr %2, align 8
  store i1 false, ptr %3, align 1, !dbg !128
  call void @llvm.dbg.declare(metadata ptr %2, metadata !129, metadata !DIExpression(DW_OP_deref)), !dbg !130
  %4 = getelementptr inbounds %class.AClass, ptr %0, i32 0, i32 0, !dbg !131
  store i32 1, ptr %4, align 4, !dbg !132
  %5 = getelementptr inbounds %class.AClass, ptr %0, i32 0, i32 1, !dbg !133
  store i32 2, ptr %5, align 4, !dbg !134
  store i1 true, ptr %3, align 1, !dbg !135
  %6 = load i1, ptr %3, align 1, !dbg !136
  br i1 %6, label %8, label %7, !dbg !136

7:                                                ; preds = %1
  call void @_ZN6AClassD2Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #3, !dbg !136
  br label %8, !dbg !136

8:                                                ; preds = %7, %1
  ret void, !dbg !136
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define linkonce_odr dso_local void @_ZN6AClassD2Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) unnamed_addr #2 comdat align 2 !dbg !137 !pallas.fcontract !138 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !144, metadata !DIExpression()), !dbg !161
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr inbounds %class.AClass, ptr %3, i32 0, i32 0, !dbg !162
  store i32 0, ptr %4, align 4, !dbg !164
  %5 = getelementptr inbounds %class.AClass, ptr %3, i32 0, i32 1, !dbg !165
  store i32 0, ptr %5, align 4, !dbg !166
  ret void, !dbg !167
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN6AClass13PALLAS_SPEC_0Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #0 comdat align 2 !dbg !147 !pallas.exprWrapper !168 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !146, metadata !DIExpression()), !dbg !169
  %3 = icmp ne ptr %0, null, !dbg !170
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !171
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !172
  %5 = call i1 @pallas.sepConj(i1 %3, i1 %4), !dbg !173
  ret i1 %5, !dbg !169
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_1v() #0 !dbg !174 !pallas.exprWrapper !168 {
  %1 = call noundef nonnull align 4 dereferenceable(8) ptr @pallas.resultRef(), !dbg !177
  %2 = getelementptr inbounds %class.AClass, ptr %1, i32 0, i32 0, !dbg !178
  %3 = load i32, ptr %2, align 4, !dbg !178
  %4 = icmp eq i32 %3, 1, !dbg !179
  ret i1 %4, !dbg !180
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_2v() #0 !dbg !181 !pallas.exprWrapper !168 {
  %1 = call noundef nonnull align 4 dereferenceable(8) ptr @pallas.resultRef(), !dbg !182
  %2 = getelementptr inbounds %class.AClass, ptr %1, i32 0, i32 1, !dbg !183
  %3 = load i32, ptr %2, align 4, !dbg !183
  %4 = icmp eq i32 %3, 2, !dbg !184
  ret i1 %4, !dbg !185
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !186 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !187 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !188 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !189 ptr @pallas.resultRef()

attributes #0 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { nounwind }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!101, !102, !103, !104, !105, !106, !107}
!llvm.ident = !{!108, !108}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_cpp_resultRef.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "ce3e12bf046806f98c3c77904787ec43")
!2 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !3, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, imports: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "dd4c68e3a0858e8df90f9879929ee162")
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
!99 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !2, entity: !100, file: !3, line: 6)
!100 = !DINamespace(name: "pallasSpec", scope: null)
!101 = !{i32 7, !"Dwarf Version", i32 5}
!102 = !{i32 2, !"Debug Info Version", i32 3}
!103 = !{i32 1, !"wchar_size", i32 4}
!104 = !{i32 8, !"PIC Level", i32 2}
!105 = !{i32 7, !"PIE Level", i32 2}
!106 = !{i32 7, !"uwtable", i32 2}
!107 = !{i32 7, !"frame-pointer", i32 2}
!108 = !{!"Ubuntu clang version 17.0.6 (9ubuntu1)"}
!109 = distinct !DISubprogram(name: "make_init_class", linkageName: "_Z15make_init_classv", scope: !1, file: !1, line: 30, type: !110, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !120)
!110 = !DISubroutineType(types: !111)
!111 = !{!112}
!112 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "AClass", file: !1, line: 9, size: 64, flags: DIFlagTypePassByReference | DIFlagNonTrivial, elements: !113, identifier: "_ZTS6AClass")
!113 = !{!114, !115, !116}
!114 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !112, file: !1, line: 11, baseType: !20, size: 32, flags: DIFlagPublic)
!115 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !112, file: !1, line: 12, baseType: !20, size: 32, offset: 32, flags: DIFlagPublic)
!116 = !DISubprogram(name: "~AClass", scope: !112, file: !1, line: 19, type: !117, scopeLine: 19, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!117 = !DISubroutineType(types: !118)
!118 = !{null, !119}
!119 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !112, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!120 = !{}
!121 = !{!122, i1 false, i8 0, !120, !120, !124, !126}
!122 = !{!"pallas.srcLoc", i64 26, i64 1, i64 29, i64 1, !123}
!123 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_cpp_resultRef.cpp", directory: "", checksumkind: CSK_MD5, checksum: "ce3e12bf046806f98c3c77904787ec43")
!124 = !{!"pallas.ensures", !125, ptr @_Z13PALLAS_SPEC_1v, !120, !120, !120}
!125 = !{!"pallas.srcLoc", i64 27, i64 1, i64 27, i64 36, !123}
!126 = !{!"pallas.ensures", !127, ptr @_Z13PALLAS_SPEC_2v, !120, !120, !120}
!127 = !{!"pallas.srcLoc", i64 28, i64 1, i64 28, i64 36, !123}
!128 = !DILocation(line: 31, column: 5, scope: !109)
!129 = !DILocalVariable(name: "c", scope: !109, file: !1, line: 31, type: !112)
!130 = !DILocation(line: 31, column: 12, scope: !109)
!131 = !DILocation(line: 32, column: 7, scope: !109)
!132 = !DILocation(line: 32, column: 9, scope: !109)
!133 = !DILocation(line: 33, column: 7, scope: !109)
!134 = !DILocation(line: 33, column: 9, scope: !109)
!135 = !DILocation(line: 34, column: 5, scope: !109)
!136 = !DILocation(line: 35, column: 1, scope: !109)
!137 = distinct !DISubprogram(name: "~AClass", linkageName: "_ZN6AClassD2Ev", scope: !112, file: !1, line: 19, type: !117, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !116, retainedNodes: !120)
!138 = !{!139, i1 false, i8 0, !120, !120, !140}
!139 = !{!"pallas.srcLoc", i64 14, i64 5, i64 18, i64 5, !123}
!140 = !{!"pallas.requires", !141, ptr @_ZN6AClass13PALLAS_SPEC_0Ev, !120, !120, !142}
!141 = !{!"pallas.srcLoc", i64 16, i64 5, i64 17, i64 39, !123}
!142 = !{!143}
!143 = !{!144, !146}
!144 = !DILocalVariable(name: "this", arg: 1, scope: !137, type: !145, flags: DIFlagArtificial | DIFlagObjectPointer)
!145 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !112, size: 64)
!146 = !DILocalVariable(name: "this", arg: 1, scope: !147, file: !1, line: 16, type: !160, flags: DIFlagArtificial | DIFlagObjectPointer)
!147 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_ZN6AClass13PALLAS_SPEC_0Ev", scope: !112, file: !1, line: 16, type: !148, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !156, retainedNodes: !120)
!148 = !DISubroutineType(types: !149)
!149 = !{!150, !151}
!150 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!151 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !152, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!152 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "AClass", file: !3, line: 12, size: 64, flags: DIFlagTypePassByReference | DIFlagNonTrivial, elements: !153, identifier: "_ZTS6AClass")
!153 = !{!154, !155, !156, !157}
!154 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !152, file: !3, line: 14, baseType: !20, size: 32, flags: DIFlagPublic)
!155 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !152, file: !3, line: 15, baseType: !20, size: 32, offset: 32, flags: DIFlagPublic)
!156 = !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_ZN6AClass13PALLAS_SPEC_0Ev", scope: !152, file: !3, line: 22, type: !148, scopeLine: 22, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!157 = !DISubprogram(name: "~AClass", scope: !152, file: !3, line: 33, type: !158, scopeLine: 33, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!158 = !DISubroutineType(types: !159)
!159 = !{null, !151}
!160 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !152, size: 64)
!161 = !DILocation(line: 0, scope: !137)
!162 = !DILocation(line: 21, column: 9, scope: !163)
!163 = distinct !DILexicalBlock(scope: !137, file: !1, line: 20, column: 6)
!164 = !DILocation(line: 21, column: 11, scope: !163)
!165 = !DILocation(line: 22, column: 9, scope: !163)
!166 = !DILocation(line: 22, column: 11, scope: !163)
!167 = !DILocation(line: 23, column: 5, scope: !137)
!168 = !{!""}
!169 = !DILocation(line: 0, scope: !147)
!170 = !DILocation(line: 16, column: 26, scope: !147)
!171 = !DILocation(line: 17, column: 31, scope: !147)
!172 = !DILocation(line: 17, column: 19, scope: !147)
!173 = !DILocation(line: 16, column: 14, scope: !147)
!174 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_Z13PALLAS_SPEC_1v", scope: !1, file: !1, line: 27, type: !175, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0)
!175 = !DISubroutineType(types: !176)
!176 = !{!150}
!177 = !DILocation(line: 27, column: 9, scope: !174)
!178 = !DILocation(line: 27, column: 30, scope: !174)
!179 = !DILocation(line: 27, column: 32, scope: !174)
!180 = !DILocation(line: 0, scope: !174)
!181 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_Z13PALLAS_SPEC_2v", scope: !1, file: !1, line: 28, type: !175, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0)
!182 = !DILocation(line: 28, column: 9, scope: !181)
!183 = !DILocation(line: 28, column: 30, scope: !181)
!184 = !DILocation(line: 28, column: 32, scope: !181)
!185 = !DILocation(line: 0, scope: !181)
!186 = !{!"pallas.sepConj"}
!187 = !{!"pallas.perm"}
!188 = !{!"pallas.fracOf"}
!189 = !{!"pallas.resultRef"}
