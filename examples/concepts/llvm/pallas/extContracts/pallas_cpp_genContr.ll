; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/extContracts/pallas_cpp_genContr.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.ClassyClass = type { i32, i32 }
%pallas.fracT = type { i64, i64, i64, i64 }

$_ZN11ClassyClassC2Ev = comdat any

$_ZN11ClassyClass13PALLAS_SPEC_0Ev = comdat any

$_ZN11ClassyClass13PALLAS_SPEC_1Ev = comdat any

$_ZN11ClassyClass13PALLAS_SPEC_2Ev = comdat any

$_ZN11ClassyClass13PALLAS_SPEC_3Ev = comdat any

$_ZN11ClassyClass13PALLAS_SPEC_4Ev = comdat any

@llvm.compiler.used = appending global [6 x ptr] [ptr @_ZN11ClassyClass13PALLAS_SPEC_0Ev, ptr @_ZN11ClassyClass13PALLAS_SPEC_1Ev, ptr @_ZN11ClassyClass13PALLAS_SPEC_2Ev, ptr @_ZN11ClassyClass13PALLAS_SPEC_3Ev, ptr @_ZN11ClassyClass13PALLAS_SPEC_4Ev, ptr @_Z13PALLAS_SPEC_5v], section "llvm.metadata"
@llvm.used = appending global [6 x ptr] [ptr @_ZN11ClassyClass13PALLAS_SPEC_0Ev, ptr @_ZN11ClassyClass13PALLAS_SPEC_1Ev, ptr @_ZN11ClassyClass13PALLAS_SPEC_2Ev, ptr @_ZN11ClassyClass13PALLAS_SPEC_3Ev, ptr @_ZN11ClassyClass13PALLAS_SPEC_4Ev, ptr @_Z13PALLAS_SPEC_5v], section "llvm.metadata"

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef i32 @_Z3foov() #0 !dbg !128 !pallas.fcontract !132 {
  %1 = alloca %class.ClassyClass, align 4
  %2 = alloca i32, align 4
  call void @llvm.dbg.declare(metadata ptr %1, metadata !137, metadata !DIExpression()), !dbg !138
  call void @_ZN11ClassyClassC2Ev(ptr noundef nonnull align 4 dereferenceable(8) %1) #4, !dbg !138
  call void @llvm.dbg.declare(metadata ptr %2, metadata !139, metadata !DIExpression()), !dbg !140
  %3 = getelementptr inbounds %class.ClassyClass, ptr %1, i32 0, i32 0, !dbg !141
  %4 = load i32, ptr %3, align 4, !dbg !141
  %5 = getelementptr inbounds %class.ClassyClass, ptr %1, i32 0, i32 1, !dbg !142
  %6 = load i32, ptr %5, align 4, !dbg !142
  %7 = add nsw i32 %4, %6, !dbg !143
  store i32 %7, ptr %2, align 4, !dbg !140
  %8 = load i32, ptr %2, align 4, !dbg !144
  ret i32 %8, !dbg !145
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define linkonce_odr dso_local void @_ZN11ClassyClassC2Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) unnamed_addr #2 comdat align 2 !dbg !146 !pallas.fcontract !151 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !155, metadata !DIExpression()), !dbg !165
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr inbounds %class.ClassyClass, ptr %3, i32 0, i32 0, !dbg !166
  store i32 1, ptr %4, align 4, !dbg !166
  %5 = getelementptr inbounds %class.ClassyClass, ptr %3, i32 0, i32 1, !dbg !167
  store i32 2, ptr %5, align 4, !dbg !167
  ret void, !dbg !168
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN11ClassyClass13PALLAS_SPEC_0Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #0 comdat align 2 !dbg !169 !pallas.exprWrapper !170 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !171, metadata !DIExpression()), !dbg !173
  %2 = icmp ne ptr %0, null, !dbg !174
  ret i1 %2, !dbg !173
}

; Function Attrs: mustprogress noinline uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN11ClassyClass13PALLAS_SPEC_1Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #3 comdat align 2 !dbg !175 !pallas.exprWrapper !170 {
  %2 = alloca %pallas.fracT, align 8
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !176, metadata !DIExpression()), !dbg !177
  %4 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 0, !dbg !178
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !179
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %2), !dbg !180
  %6 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 1, !dbg !181
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !182
  %7 = call i1 @pallas.perm(ptr noundef %6, ptr noundef byval(%pallas.fracT) %3), !dbg !183
  %8 = call i1 @pallas.sepConj(i1 %5, i1 %7), !dbg !184
  ret i1 %8, !dbg !177
}

; Function Attrs: mustprogress noinline uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN11ClassyClass13PALLAS_SPEC_2Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #3 comdat align 2 !dbg !185 !pallas.exprWrapper !170 {
  %2 = alloca %pallas.fracT, align 8
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !186, metadata !DIExpression()), !dbg !187
  %4 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 0, !dbg !188
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !189
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %2), !dbg !190
  %6 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 1, !dbg !191
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !192
  %7 = call i1 @pallas.perm(ptr noundef %6, ptr noundef byval(%pallas.fracT) %3), !dbg !193
  %8 = call i1 @pallas.sepConj(i1 %5, i1 %7), !dbg !194
  ret i1 %8, !dbg !187
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN11ClassyClass13PALLAS_SPEC_3Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #0 comdat align 2 !dbg !195 !pallas.exprWrapper !170 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !196, metadata !DIExpression()), !dbg !197
  %2 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 0, !dbg !198
  %3 = load i32, ptr %2, align 4, !dbg !198
  %4 = icmp eq i32 %3, 1, !dbg !199
  ret i1 %4, !dbg !197
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN11ClassyClass13PALLAS_SPEC_4Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #0 comdat align 2 !dbg !200 !pallas.exprWrapper !170 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !201, metadata !DIExpression()), !dbg !202
  %2 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 1, !dbg !203
  %3 = load i32, ptr %2, align 4, !dbg !203
  %4 = icmp eq i32 %3, 2, !dbg !204
  ret i1 %4, !dbg !202
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_5v() #3 !dbg !205 !pallas.exprWrapper !170 {
  %1 = call noundef i32 @pallas.result.0(), !dbg !208
  %2 = icmp eq i32 %1, 3, !dbg !209
  ret i1 %2, !dbg !210
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !211 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !212 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !213 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !214 noundef i32 @pallas.result.0()

attributes #0 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { mustprogress noinline uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { nounwind }

!llvm.dbg.cu = !{!0, !8}
!llvm.module.flags = !{!120, !121, !122, !123, !124, !125, !126}
!llvm.ident = !{!127, !127}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/extContracts/pallas_cpp_genContr.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "a4fc16def7ba67032488ab6234166f1f")
!2 = !{!3}
!3 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "ClassyClass", file: !1, line: 7, size: 64, flags: DIFlagTypePassByValue | DIFlagNonTrivial, elements: !4, identifier: "_ZTS11ClassyClass")
!4 = !{!5, !7}
!5 = !DIDerivedType(tag: DW_TAG_member, name: "i", scope: !3, file: !1, line: 9, baseType: !6, size: 32, flags: DIFlagPublic)
!6 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!7 = !DIDerivedType(tag: DW_TAG_member, name: "j", scope: !3, file: !1, line: 10, baseType: !6, size: 32, offset: 32, flags: DIFlagPublic)
!8 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !9, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !10, imports: !24, splitDebugInlining: false, nameTableKind: None)
!9 = !DIFile(filename: "tmp/source_wrappers.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "5ac60cb93d1af8a22c2284e632681d7f")
!10 = !{!11}
!11 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "ClassyClass", file: !9, line: 10, size: 64, flags: DIFlagTypePassByValue | DIFlagNonTrivial, elements: !12, identifier: "_ZTS11ClassyClass")
!12 = !{!13, !14, !15, !20, !21, !22, !23}
!13 = !DIDerivedType(tag: DW_TAG_member, name: "i", scope: !11, file: !9, line: 12, baseType: !6, size: 32, flags: DIFlagPublic)
!14 = !DIDerivedType(tag: DW_TAG_member, name: "j", scope: !11, file: !9, line: 13, baseType: !6, size: 32, offset: 32, flags: DIFlagPublic)
!15 = !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_0Ev", scope: !11, file: !9, line: 20, type: !16, scopeLine: 20, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!16 = !DISubroutineType(types: !17)
!17 = !{!18, !19}
!18 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!19 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !11, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!20 = !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_1Ev", scope: !11, file: !9, line: 30, type: !16, scopeLine: 30, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!21 = !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_2Ev", scope: !11, file: !9, line: 40, type: !16, scopeLine: 40, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!22 = !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_3Ev", scope: !11, file: !9, line: 50, type: !16, scopeLine: 50, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!23 = !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_4Ev", scope: !11, file: !9, line: 60, type: !16, scopeLine: 60, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!24 = !{!25, !33, !37, !40, !44, !47, !49, !51, !53, !57, !60, !63, !66, !69, !71, !76, !80, !84, !88, !90, !92, !94, !96, !99, !102, !105, !108, !111, !113, !118}
!25 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !27, file: !32, line: 51)
!26 = !DINamespace(name: "std", scope: null)
!27 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !28, line: 24, baseType: !29)
!28 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!29 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int8_t", file: !30, line: 37, baseType: !31)
!30 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!31 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!32 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstdint", directory: "")
!33 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !34, file: !32, line: 52)
!34 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !28, line: 25, baseType: !35)
!35 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int16_t", file: !30, line: 39, baseType: !36)
!36 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!37 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !38, file: !32, line: 53)
!38 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !28, line: 26, baseType: !39)
!39 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !30, line: 41, baseType: !6)
!40 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !41, file: !32, line: 54)
!41 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !28, line: 27, baseType: !42)
!42 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !30, line: 44, baseType: !43)
!43 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!44 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !45, file: !32, line: 56)
!45 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !46, line: 47, baseType: !31)
!46 = !DIFile(filename: "/usr/include/stdint.h", directory: "", checksumkind: CSK_MD5, checksum: "bfb03fa9c46a839e35c32b929fbdbb8e")
!47 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !48, file: !32, line: 57)
!48 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !46, line: 49, baseType: !43)
!49 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !50, file: !32, line: 58)
!50 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !46, line: 50, baseType: !43)
!51 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !52, file: !32, line: 59)
!52 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !46, line: 51, baseType: !43)
!53 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !54, file: !32, line: 61)
!54 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !55, line: 25, baseType: !56)
!55 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-least.h", directory: "", checksumkind: CSK_MD5, checksum: "a866be81c480920b0293bd5f6336a0a3")
!56 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least8_t", file: !30, line: 52, baseType: !29)
!57 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !58, file: !32, line: 62)
!58 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !55, line: 26, baseType: !59)
!59 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least16_t", file: !30, line: 54, baseType: !35)
!60 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !61, file: !32, line: 63)
!61 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !55, line: 27, baseType: !62)
!62 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least32_t", file: !30, line: 56, baseType: !39)
!63 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !64, file: !32, line: 64)
!64 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !55, line: 28, baseType: !65)
!65 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least64_t", file: !30, line: 58, baseType: !42)
!66 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !67, file: !32, line: 66)
!67 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !46, line: 90, baseType: !68)
!68 = !DIDerivedType(tag: DW_TAG_typedef, name: "__intmax_t", file: !30, line: 72, baseType: !43)
!69 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !70, file: !32, line: 67)
!70 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !46, line: 76, baseType: !43)
!71 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !72, file: !32, line: 69)
!72 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !73, line: 24, baseType: !74)
!73 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-uintn.h", directory: "", checksumkind: CSK_MD5, checksum: "256fcabbefa27ca8cf5e6d37525e6e16")
!74 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !30, line: 38, baseType: !75)
!75 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!76 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !77, file: !32, line: 70)
!77 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !73, line: 25, baseType: !78)
!78 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !30, line: 40, baseType: !79)
!79 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!80 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !81, file: !32, line: 71)
!81 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !73, line: 26, baseType: !82)
!82 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint32_t", file: !30, line: 42, baseType: !83)
!83 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!84 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !85, file: !32, line: 72)
!85 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !73, line: 27, baseType: !86)
!86 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint64_t", file: !30, line: 45, baseType: !87)
!87 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!88 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !89, file: !32, line: 74)
!89 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !46, line: 60, baseType: !75)
!90 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !91, file: !32, line: 75)
!91 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !46, line: 62, baseType: !87)
!92 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !93, file: !32, line: 76)
!93 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !46, line: 63, baseType: !87)
!94 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !95, file: !32, line: 77)
!95 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !46, line: 64, baseType: !87)
!96 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !97, file: !32, line: 79)
!97 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !55, line: 31, baseType: !98)
!98 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least8_t", file: !30, line: 53, baseType: !74)
!99 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !100, file: !32, line: 80)
!100 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !55, line: 32, baseType: !101)
!101 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least16_t", file: !30, line: 55, baseType: !78)
!102 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !103, file: !32, line: 81)
!103 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !55, line: 33, baseType: !104)
!104 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least32_t", file: !30, line: 57, baseType: !82)
!105 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !106, file: !32, line: 82)
!106 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !55, line: 34, baseType: !107)
!107 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least64_t", file: !30, line: 59, baseType: !86)
!108 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !109, file: !32, line: 84)
!109 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !46, line: 91, baseType: !110)
!110 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uintmax_t", file: !30, line: 73, baseType: !87)
!111 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !112, file: !32, line: 85)
!112 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !46, line: 79, baseType: !87)
!113 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !26, entity: !114, file: !117, line: 58)
!114 = !DIDerivedType(tag: DW_TAG_typedef, name: "max_align_t", file: !115, line: 24, baseType: !116)
!115 = !DIFile(filename: "/usr/lib/llvm-17/lib/clang/17/include/__stddef_max_align_t.h", directory: "", checksumkind: CSK_MD5, checksum: "48e8e2456f77e6cda35d245130fa7259")
!116 = !DICompositeType(tag: DW_TAG_structure_type, file: !115, line: 19, size: 256, flags: DIFlagFwdDecl, identifier: "_ZTS11max_align_t")
!117 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstddef", directory: "")
!118 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !8, entity: !119, file: !9, line: 4)
!119 = !DINamespace(name: "pallasSpec", scope: null)
!120 = !{i32 7, !"Dwarf Version", i32 5}
!121 = !{i32 2, !"Debug Info Version", i32 3}
!122 = !{i32 1, !"wchar_size", i32 4}
!123 = !{i32 8, !"PIC Level", i32 2}
!124 = !{i32 7, !"PIE Level", i32 2}
!125 = !{i32 7, !"uwtable", i32 2}
!126 = !{i32 7, !"frame-pointer", i32 2}
!127 = !{!"Ubuntu clang version 17.0.6 (9ubuntu1)"}
!128 = distinct !DISubprogram(name: "foo", linkageName: "_Z3foov", scope: !1, file: !1, line: 26, type: !129, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !131)
!129 = !DISubroutineType(types: !130)
!130 = !{!6}
!131 = !{}
!132 = !{!133, i1 false, i1 false, !135}
!133 = !{!"pallas.srcLoc", i64 23, i64 1, i64 25, i64 1, !134}
!134 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_genContr.cpp", directory: "", checksumkind: CSK_MD5, checksum: "a4fc16def7ba67032488ab6234166f1f")
!135 = !{!"pallas.ensures", !136, ptr @_Z13PALLAS_SPEC_5v}
!136 = !{!"pallas.srcLoc", i64 24, i64 1, i64 24, i64 28, !134}
!137 = !DILocalVariable(name: "c", scope: !128, file: !1, line: 27, type: !3)
!138 = !DILocation(line: 27, column: 17, scope: !128)
!139 = !DILocalVariable(name: "res", scope: !128, file: !1, line: 28, type: !6)
!140 = !DILocation(line: 28, column: 9, scope: !128)
!141 = !DILocation(line: 28, column: 17, scope: !128)
!142 = !DILocation(line: 28, column: 23, scope: !128)
!143 = !DILocation(line: 28, column: 19, scope: !128)
!144 = !DILocation(line: 29, column: 12, scope: !128)
!145 = !DILocation(line: 29, column: 5, scope: !128)
!146 = distinct !DISubprogram(name: "ClassyClass", linkageName: "_ZN11ClassyClassC2Ev", scope: !3, file: !1, line: 7, type: !147, scopeLine: 7, flags: DIFlagArtificial | DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !150, retainedNodes: !131)
!147 = !DISubroutineType(types: !148)
!148 = !{null, !149}
!149 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!150 = !DISubprogram(name: "ClassyClass", scope: !3, type: !147, flags: DIFlagPublic | DIFlagArtificial | DIFlagPrototyped, spFlags: 0)
!151 = !{!152, i1 false, i1 false, !153, !157, !159, !161, !163}
!152 = !{!"pallas.srcLoc", i64 13, i64 5, i64 20, i64 5, !134}
!153 = !{!"pallas.requires", !154, ptr @_ZN11ClassyClass13PALLAS_SPEC_0Ev, !155}
!154 = !{!"pallas.srcLoc", i64 15, i64 5, i64 15, i64 31, !134}
!155 = !DILocalVariable(name: "this", arg: 1, scope: !146, type: !156, flags: DIFlagArtificial | DIFlagObjectPointer)
!156 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3, size: 64)
!157 = !{!"pallas.requires", !158, ptr @_ZN11ClassyClass13PALLAS_SPEC_1Ev, !155}
!158 = !{!"pallas.srcLoc", i64 16, i64 5, i64 16, i64 68, !134}
!159 = !{!"pallas.ensures", !160, ptr @_ZN11ClassyClass13PALLAS_SPEC_2Ev, !155}
!160 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 68, !134}
!161 = !{!"pallas.ensures", !162, ptr @_ZN11ClassyClass13PALLAS_SPEC_3Ev, !155}
!162 = !{!"pallas.srcLoc", i64 18, i64 5, i64 18, i64 25, !134}
!163 = !{!"pallas.ensures", !164, ptr @_ZN11ClassyClass13PALLAS_SPEC_4Ev, !155}
!164 = !{!"pallas.srcLoc", i64 19, i64 5, i64 19, i64 25, !134}
!165 = !DILocation(line: 0, scope: !146)
!166 = !DILocation(line: 9, column: 13, scope: !146)
!167 = !DILocation(line: 10, column: 13, scope: !146)
!168 = !DILocation(line: 7, column: 7, scope: !146)
!169 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_0Ev", scope: !3, file: !1, line: 15, type: !16, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !15, retainedNodes: !131)
!170 = !{!""}
!171 = !DILocalVariable(name: "this", arg: 1, scope: !169, file: !1, line: 15, type: !172, flags: DIFlagArtificial | DIFlagObjectPointer)
!172 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !11, size: 64)
!173 = !DILocation(line: 0, scope: !169)
!174 = !DILocation(line: 15, column: 21, scope: !169)
!175 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_1Ev", scope: !3, file: !1, line: 16, type: !16, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !20, retainedNodes: !131)
!176 = !DILocalVariable(name: "this", arg: 1, scope: !175, file: !1, line: 16, type: !172, flags: DIFlagArtificial | DIFlagObjectPointer)
!177 = !DILocation(line: 0, scope: !175)
!178 = !DILocation(line: 16, column: 32, scope: !175)
!179 = !DILocation(line: 16, column: 35, scope: !175)
!180 = !DILocation(line: 16, column: 19, scope: !175)
!181 = !DILocation(line: 16, column: 57, scope: !175)
!182 = !DILocation(line: 16, column: 60, scope: !175)
!183 = !DILocation(line: 16, column: 44, scope: !175)
!184 = !DILocation(line: 16, column: 14, scope: !175)
!185 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_2Ev", scope: !3, file: !1, line: 17, type: !16, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !21, retainedNodes: !131)
!186 = !DILocalVariable(name: "this", arg: 1, scope: !185, file: !1, line: 17, type: !172, flags: DIFlagArtificial | DIFlagObjectPointer)
!187 = !DILocation(line: 0, scope: !185)
!188 = !DILocation(line: 17, column: 32, scope: !185)
!189 = !DILocation(line: 17, column: 35, scope: !185)
!190 = !DILocation(line: 17, column: 19, scope: !185)
!191 = !DILocation(line: 17, column: 57, scope: !185)
!192 = !DILocation(line: 17, column: 60, scope: !185)
!193 = !DILocation(line: 17, column: 44, scope: !185)
!194 = !DILocation(line: 17, column: 14, scope: !185)
!195 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_3Ev", scope: !3, file: !1, line: 18, type: !16, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !22, retainedNodes: !131)
!196 = !DILocalVariable(name: "this", arg: 1, scope: !195, file: !1, line: 18, type: !172, flags: DIFlagArtificial | DIFlagObjectPointer)
!197 = !DILocation(line: 0, scope: !195)
!198 = !DILocation(line: 18, column: 19, scope: !195)
!199 = !DILocation(line: 18, column: 21, scope: !195)
!200 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_4Ev", scope: !3, file: !1, line: 19, type: !16, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !23, retainedNodes: !131)
!201 = !DILocalVariable(name: "this", arg: 1, scope: !200, file: !1, line: 19, type: !172, flags: DIFlagArtificial | DIFlagObjectPointer)
!202 = !DILocation(line: 0, scope: !200)
!203 = !DILocation(line: 19, column: 19, scope: !200)
!204 = !DILocation(line: 19, column: 21, scope: !200)
!205 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_Z13PALLAS_SPEC_5v", scope: !1, file: !1, line: 24, type: !206, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0)
!206 = !DISubroutineType(types: !207)
!207 = !{!18}
!208 = !DILocation(line: 24, column: 9, scope: !205)
!209 = !DILocation(line: 24, column: 24, scope: !205)
!210 = !DILocation(line: 0, scope: !205)
!211 = !{!"pallas.sepConj"}
!212 = !{!"pallas.perm"}
!213 = !{!"pallas.fracOf"}
!214 = !{!"pallas.result"}
