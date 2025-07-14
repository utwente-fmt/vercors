; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_genContr.cpp"
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
define dso_local noundef i32 @_Z3foov() #0 !dbg !129 !pallas.fcontract !133 {
  %1 = alloca %class.ClassyClass, align 4
  %2 = alloca i32, align 4
  call void @llvm.dbg.declare(metadata ptr %1, metadata !138, metadata !DIExpression()), !dbg !139
  call void @_ZN11ClassyClassC2Ev(ptr noundef nonnull align 4 dereferenceable(8) %1) #4, !dbg !139
  call void @llvm.dbg.declare(metadata ptr %2, metadata !140, metadata !DIExpression()), !dbg !141
  %3 = getelementptr inbounds %class.ClassyClass, ptr %1, i32 0, i32 0, !dbg !142
  %4 = load i32, ptr %3, align 4, !dbg !142
  %5 = getelementptr inbounds %class.ClassyClass, ptr %1, i32 0, i32 1, !dbg !143
  %6 = load i32, ptr %5, align 4, !dbg !143
  %7 = add nsw i32 %4, %6, !dbg !144
  store i32 %7, ptr %2, align 4, !dbg !141
  %8 = load i32, ptr %2, align 4, !dbg !145
  ret i32 %8, !dbg !146
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define linkonce_odr dso_local void @_ZN11ClassyClassC2Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) unnamed_addr #2 comdat align 2 !dbg !147 !pallas.fcontract !152 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !156, metadata !DIExpression()), !dbg !166
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr inbounds %class.ClassyClass, ptr %3, i32 0, i32 0, !dbg !167
  store i32 1, ptr %4, align 4, !dbg !167
  %5 = getelementptr inbounds %class.ClassyClass, ptr %3, i32 0, i32 1, !dbg !168
  store i32 2, ptr %5, align 4, !dbg !168
  ret void, !dbg !169
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN11ClassyClass13PALLAS_SPEC_0Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #0 comdat align 2 !dbg !170 !pallas.exprWrapper !171 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !172, metadata !DIExpression()), !dbg !174
  %2 = icmp ne ptr %0, null, !dbg !175
  ret i1 %2, !dbg !174
}

; Function Attrs: mustprogress noinline uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN11ClassyClass13PALLAS_SPEC_1Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #3 comdat align 2 !dbg !176 !pallas.exprWrapper !171 {
  %2 = alloca %pallas.fracT, align 8
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !177, metadata !DIExpression()), !dbg !178
  %4 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 0, !dbg !179
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !180
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %2), !dbg !181
  %6 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 1, !dbg !182
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !183
  %7 = call i1 @pallas.perm(ptr noundef %6, ptr noundef byval(%pallas.fracT) %3), !dbg !184
  %8 = call i1 @pallas.sepConj(i1 %5, i1 %7), !dbg !185
  ret i1 %8, !dbg !178
}

; Function Attrs: mustprogress noinline uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN11ClassyClass13PALLAS_SPEC_2Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #3 comdat align 2 !dbg !186 !pallas.exprWrapper !171 {
  %2 = alloca %pallas.fracT, align 8
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !187, metadata !DIExpression()), !dbg !188
  %4 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 0, !dbg !189
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !190
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %2), !dbg !191
  %6 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 1, !dbg !192
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !193
  %7 = call i1 @pallas.perm(ptr noundef %6, ptr noundef byval(%pallas.fracT) %3), !dbg !194
  %8 = call i1 @pallas.sepConj(i1 %5, i1 %7), !dbg !195
  ret i1 %8, !dbg !188
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN11ClassyClass13PALLAS_SPEC_3Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #0 comdat align 2 !dbg !196 !pallas.exprWrapper !171 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !197, metadata !DIExpression()), !dbg !198
  %2 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 0, !dbg !199
  %3 = load i32, ptr %2, align 4, !dbg !199
  %4 = icmp eq i32 %3, 1, !dbg !200
  ret i1 %4, !dbg !198
}

; Function Attrs: mustprogress noinline nounwind uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZN11ClassyClass13PALLAS_SPEC_4Ev(ptr noundef nonnull align 4 dereferenceable(8) %0) #0 comdat align 2 !dbg !201 !pallas.exprWrapper !171 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !202, metadata !DIExpression()), !dbg !203
  %2 = getelementptr inbounds %class.ClassyClass, ptr %0, i32 0, i32 1, !dbg !204
  %3 = load i32, ptr %2, align 4, !dbg !204
  %4 = icmp eq i32 %3, 2, !dbg !205
  ret i1 %4, !dbg !203
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_5v() #3 !dbg !206 !pallas.exprWrapper !171 {
  %1 = call noundef i32 @pallas.result.0(), !dbg !209
  %2 = icmp eq i32 %1, 3, !dbg !210
  ret i1 %2, !dbg !211
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !212 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !213 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !214 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !215 noundef i32 @pallas.result.0()

attributes #0 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { mustprogress noinline uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { nounwind }

!llvm.dbg.cu = !{!0, !9}
!llvm.module.flags = !{!121, !122, !123, !124, !125, !126, !127}
!llvm.ident = !{!128, !128}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_genContr.cpp", directory: "/home/rme/repos/vercors/examples/concepts/llvm/pallas", checksumkind: CSK_MD5, checksum: "40d9d4e29d8decf08f1082668d8a700c")
!2 = !{!3}
!3 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "ClassyClass", file: !4, line: 7, size: 64, flags: DIFlagTypePassByValue | DIFlagNonTrivial, elements: !5, identifier: "_ZTS11ClassyClass")
!4 = !DIFile(filename: "extContracts/pallas_cpp_genContr.cpp", directory: "/home/rme/repos/vercors/examples/concepts/llvm/pallas", checksumkind: CSK_MD5, checksum: "40d9d4e29d8decf08f1082668d8a700c")
!5 = !{!6, !8}
!6 = !DIDerivedType(tag: DW_TAG_member, name: "i", scope: !3, file: !4, line: 9, baseType: !7, size: 32, flags: DIFlagPublic)
!7 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!8 = !DIDerivedType(tag: DW_TAG_member, name: "j", scope: !3, file: !4, line: 10, baseType: !7, size: 32, offset: 32, flags: DIFlagPublic)
!9 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !10, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !11, imports: !25, splitDebugInlining: false, nameTableKind: None)
!10 = !DIFile(filename: "tmp/source_wrappers.cpp", directory: "/home/rme/repos/vercors/examples/concepts/llvm/pallas", checksumkind: CSK_MD5, checksum: "a47d96a8d1863a8062b61914ec80c68d")
!11 = !{!12}
!12 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "ClassyClass", file: !10, line: 10, size: 64, flags: DIFlagTypePassByValue | DIFlagNonTrivial, elements: !13, identifier: "_ZTS11ClassyClass")
!13 = !{!14, !15, !16, !21, !22, !23, !24}
!14 = !DIDerivedType(tag: DW_TAG_member, name: "i", scope: !12, file: !10, line: 12, baseType: !7, size: 32, flags: DIFlagPublic)
!15 = !DIDerivedType(tag: DW_TAG_member, name: "j", scope: !12, file: !10, line: 13, baseType: !7, size: 32, offset: 32, flags: DIFlagPublic)
!16 = !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_0Ev", scope: !12, file: !10, line: 16, type: !17, scopeLine: 16, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!17 = !DISubroutineType(types: !18)
!18 = !{!19, !20}
!19 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!20 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !12, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!21 = !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_1Ev", scope: !12, file: !10, line: 21, type: !17, scopeLine: 21, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!22 = !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_2Ev", scope: !12, file: !10, line: 26, type: !17, scopeLine: 26, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!23 = !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_3Ev", scope: !12, file: !10, line: 31, type: !17, scopeLine: 31, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!24 = !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_4Ev", scope: !12, file: !10, line: 36, type: !17, scopeLine: 36, flags: DIFlagPublic | DIFlagPrototyped, spFlags: 0)
!25 = !{!26, !34, !38, !41, !45, !48, !50, !52, !54, !58, !61, !64, !67, !70, !72, !77, !81, !85, !89, !91, !93, !95, !97, !100, !103, !106, !109, !112, !114, !119}
!26 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !28, file: !33, line: 51)
!27 = !DINamespace(name: "std", scope: null)
!28 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !29, line: 24, baseType: !30)
!29 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!30 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int8_t", file: !31, line: 37, baseType: !32)
!31 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!32 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!33 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstdint", directory: "")
!34 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !35, file: !33, line: 52)
!35 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !29, line: 25, baseType: !36)
!36 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int16_t", file: !31, line: 39, baseType: !37)
!37 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!38 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !39, file: !33, line: 53)
!39 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !29, line: 26, baseType: !40)
!40 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !31, line: 41, baseType: !7)
!41 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !42, file: !33, line: 54)
!42 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !29, line: 27, baseType: !43)
!43 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !31, line: 44, baseType: !44)
!44 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!45 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !46, file: !33, line: 56)
!46 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !47, line: 47, baseType: !32)
!47 = !DIFile(filename: "/usr/include/stdint.h", directory: "", checksumkind: CSK_MD5, checksum: "bfb03fa9c46a839e35c32b929fbdbb8e")
!48 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !49, file: !33, line: 57)
!49 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !47, line: 49, baseType: !44)
!50 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !51, file: !33, line: 58)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !47, line: 50, baseType: !44)
!52 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !53, file: !33, line: 59)
!53 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !47, line: 51, baseType: !44)
!54 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !55, file: !33, line: 61)
!55 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !56, line: 25, baseType: !57)
!56 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-least.h", directory: "", checksumkind: CSK_MD5, checksum: "a866be81c480920b0293bd5f6336a0a3")
!57 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least8_t", file: !31, line: 52, baseType: !30)
!58 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !59, file: !33, line: 62)
!59 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !56, line: 26, baseType: !60)
!60 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least16_t", file: !31, line: 54, baseType: !36)
!61 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !62, file: !33, line: 63)
!62 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !56, line: 27, baseType: !63)
!63 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least32_t", file: !31, line: 56, baseType: !40)
!64 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !65, file: !33, line: 64)
!65 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !56, line: 28, baseType: !66)
!66 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least64_t", file: !31, line: 58, baseType: !43)
!67 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !68, file: !33, line: 66)
!68 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !47, line: 90, baseType: !69)
!69 = !DIDerivedType(tag: DW_TAG_typedef, name: "__intmax_t", file: !31, line: 72, baseType: !44)
!70 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !71, file: !33, line: 67)
!71 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !47, line: 76, baseType: !44)
!72 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !73, file: !33, line: 69)
!73 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !74, line: 24, baseType: !75)
!74 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-uintn.h", directory: "", checksumkind: CSK_MD5, checksum: "256fcabbefa27ca8cf5e6d37525e6e16")
!75 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !31, line: 38, baseType: !76)
!76 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!77 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !78, file: !33, line: 70)
!78 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !74, line: 25, baseType: !79)
!79 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !31, line: 40, baseType: !80)
!80 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!81 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !82, file: !33, line: 71)
!82 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !74, line: 26, baseType: !83)
!83 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint32_t", file: !31, line: 42, baseType: !84)
!84 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!85 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !86, file: !33, line: 72)
!86 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !74, line: 27, baseType: !87)
!87 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint64_t", file: !31, line: 45, baseType: !88)
!88 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!89 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !90, file: !33, line: 74)
!90 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !47, line: 60, baseType: !76)
!91 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !92, file: !33, line: 75)
!92 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !47, line: 62, baseType: !88)
!93 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !94, file: !33, line: 76)
!94 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !47, line: 63, baseType: !88)
!95 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !96, file: !33, line: 77)
!96 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !47, line: 64, baseType: !88)
!97 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !98, file: !33, line: 79)
!98 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !56, line: 31, baseType: !99)
!99 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least8_t", file: !31, line: 53, baseType: !75)
!100 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !101, file: !33, line: 80)
!101 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !56, line: 32, baseType: !102)
!102 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least16_t", file: !31, line: 55, baseType: !79)
!103 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !104, file: !33, line: 81)
!104 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !56, line: 33, baseType: !105)
!105 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least32_t", file: !31, line: 57, baseType: !83)
!106 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !107, file: !33, line: 82)
!107 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !56, line: 34, baseType: !108)
!108 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least64_t", file: !31, line: 59, baseType: !87)
!109 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !110, file: !33, line: 84)
!110 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !47, line: 91, baseType: !111)
!111 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uintmax_t", file: !31, line: 73, baseType: !88)
!112 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !113, file: !33, line: 85)
!113 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !47, line: 79, baseType: !88)
!114 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !27, entity: !115, file: !118, line: 58)
!115 = !DIDerivedType(tag: DW_TAG_typedef, name: "max_align_t", file: !116, line: 24, baseType: !117)
!116 = !DIFile(filename: "/usr/lib/llvm-17/lib/clang/17/include/__stddef_max_align_t.h", directory: "", checksumkind: CSK_MD5, checksum: "48e8e2456f77e6cda35d245130fa7259")
!117 = !DICompositeType(tag: DW_TAG_structure_type, file: !116, line: 19, size: 256, flags: DIFlagFwdDecl, identifier: "_ZTS11max_align_t")
!118 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/13/../../../../include/c++/13/cstddef", directory: "")
!119 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !9, entity: !120, file: !10, line: 4)
!120 = !DINamespace(name: "pallasSpec", scope: null)
!121 = !{i32 7, !"Dwarf Version", i32 5}
!122 = !{i32 2, !"Debug Info Version", i32 3}
!123 = !{i32 1, !"wchar_size", i32 4}
!124 = !{i32 8, !"PIC Level", i32 2}
!125 = !{i32 7, !"PIE Level", i32 2}
!126 = !{i32 7, !"uwtable", i32 2}
!127 = !{i32 7, !"frame-pointer", i32 2}
!128 = !{!"Ubuntu clang version 17.0.6 (9ubuntu1)"}
!129 = distinct !DISubprogram(name: "foo", linkageName: "_Z3foov", scope: !4, file: !4, line: 26, type: !130, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !132)
!130 = !DISubroutineType(types: !131)
!131 = !{!7}
!132 = !{}
!133 = !{!134, i1 false, i1 false, !136}
!134 = !{!"pallas.srcLoc", i64 23, i64 1, i64 25, i64 1, !135}
!135 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_genContr.cpp", directory: "", checksumkind: CSK_MD5, checksum: "40d9d4e29d8decf08f1082668d8a700c")
!136 = !{!"pallas.ensures", !137, ptr @_Z13PALLAS_SPEC_5v}
!137 = !{!"pallas.srcLoc", i64 24, i64 1, i64 24, i64 28, !135}
!138 = !DILocalVariable(name: "c", scope: !129, file: !4, line: 27, type: !3)
!139 = !DILocation(line: 27, column: 17, scope: !129)
!140 = !DILocalVariable(name: "res", scope: !129, file: !4, line: 28, type: !7)
!141 = !DILocation(line: 28, column: 9, scope: !129)
!142 = !DILocation(line: 28, column: 17, scope: !129)
!143 = !DILocation(line: 28, column: 23, scope: !129)
!144 = !DILocation(line: 28, column: 19, scope: !129)
!145 = !DILocation(line: 29, column: 12, scope: !129)
!146 = !DILocation(line: 29, column: 5, scope: !129)
!147 = distinct !DISubprogram(name: "ClassyClass", linkageName: "_ZN11ClassyClassC2Ev", scope: !3, file: !4, line: 7, type: !148, scopeLine: 7, flags: DIFlagArtificial | DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !151, retainedNodes: !132)
!148 = !DISubroutineType(types: !149)
!149 = !{null, !150}
!150 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!151 = !DISubprogram(name: "ClassyClass", scope: !3, type: !148, flags: DIFlagPublic | DIFlagArtificial | DIFlagPrototyped, spFlags: 0)
!152 = !{!153, i1 false, i1 false, !154, !158, !160, !162, !164}
!153 = !{!"pallas.srcLoc", i64 13, i64 5, i64 20, i64 5, !135}
!154 = !{!"pallas.requires", !155, ptr @_ZN11ClassyClass13PALLAS_SPEC_0Ev, !156}
!155 = !{!"pallas.srcLoc", i64 15, i64 5, i64 15, i64 29, !135}
!156 = !DILocalVariable(name: "this", arg: 1, scope: !147, type: !157, flags: DIFlagArtificial | DIFlagObjectPointer)
!157 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3, size: 64)
!158 = !{!"pallas.requires", !159, ptr @_ZN11ClassyClass13PALLAS_SPEC_1Ev, !156}
!159 = !{!"pallas.srcLoc", i64 16, i64 5, i64 16, i64 68, !135}
!160 = !{!"pallas.ensures", !161, ptr @_ZN11ClassyClass13PALLAS_SPEC_2Ev, !156}
!161 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 68, !135}
!162 = !{!"pallas.ensures", !163, ptr @_ZN11ClassyClass13PALLAS_SPEC_3Ev, !156}
!163 = !{!"pallas.srcLoc", i64 18, i64 5, i64 18, i64 25, !135}
!164 = !{!"pallas.ensures", !165, ptr @_ZN11ClassyClass13PALLAS_SPEC_4Ev, !156}
!165 = !{!"pallas.srcLoc", i64 19, i64 5, i64 19, i64 25, !135}
!166 = !DILocation(line: 0, scope: !147)
!167 = !DILocation(line: 9, column: 13, scope: !147)
!168 = !DILocation(line: 10, column: 13, scope: !147)
!169 = !DILocation(line: 7, column: 7, scope: !147)
!170 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_0Ev", scope: !3, file: !4, line: 15, type: !17, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !16, retainedNodes: !132)
!171 = !{!""}
!172 = !DILocalVariable(name: "this", arg: 1, scope: !170, file: !4, line: 15, type: !173, flags: DIFlagArtificial | DIFlagObjectPointer)
!173 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !12, size: 64)
!174 = !DILocation(line: 0, scope: !170)
!175 = !DILocation(line: 15, column: 19, scope: !170)
!176 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_1Ev", scope: !3, file: !4, line: 16, type: !17, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !21, retainedNodes: !132)
!177 = !DILocalVariable(name: "this", arg: 1, scope: !176, file: !4, line: 16, type: !173, flags: DIFlagArtificial | DIFlagObjectPointer)
!178 = !DILocation(line: 0, scope: !176)
!179 = !DILocation(line: 16, column: 32, scope: !176)
!180 = !DILocation(line: 16, column: 35, scope: !176)
!181 = !DILocation(line: 16, column: 19, scope: !176)
!182 = !DILocation(line: 16, column: 57, scope: !176)
!183 = !DILocation(line: 16, column: 60, scope: !176)
!184 = !DILocation(line: 16, column: 44, scope: !176)
!185 = !DILocation(line: 16, column: 14, scope: !176)
!186 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_2Ev", scope: !3, file: !4, line: 17, type: !17, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !22, retainedNodes: !132)
!187 = !DILocalVariable(name: "this", arg: 1, scope: !186, file: !4, line: 17, type: !173, flags: DIFlagArtificial | DIFlagObjectPointer)
!188 = !DILocation(line: 0, scope: !186)
!189 = !DILocation(line: 17, column: 32, scope: !186)
!190 = !DILocation(line: 17, column: 35, scope: !186)
!191 = !DILocation(line: 17, column: 19, scope: !186)
!192 = !DILocation(line: 17, column: 57, scope: !186)
!193 = !DILocation(line: 17, column: 60, scope: !186)
!194 = !DILocation(line: 17, column: 44, scope: !186)
!195 = !DILocation(line: 17, column: 14, scope: !186)
!196 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_3Ev", scope: !3, file: !4, line: 18, type: !17, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !23, retainedNodes: !132)
!197 = !DILocalVariable(name: "this", arg: 1, scope: !196, file: !4, line: 18, type: !173, flags: DIFlagArtificial | DIFlagObjectPointer)
!198 = !DILocation(line: 0, scope: !196)
!199 = !DILocation(line: 18, column: 19, scope: !196)
!200 = !DILocation(line: 18, column: 21, scope: !196)
!201 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_ZN11ClassyClass13PALLAS_SPEC_4Ev", scope: !3, file: !4, line: 19, type: !17, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, declaration: !24, retainedNodes: !132)
!202 = !DILocalVariable(name: "this", arg: 1, scope: !201, file: !4, line: 19, type: !173, flags: DIFlagArtificial | DIFlagObjectPointer)
!203 = !DILocation(line: 0, scope: !201)
!204 = !DILocation(line: 19, column: 19, scope: !201)
!205 = !DILocation(line: 19, column: 21, scope: !201)
!206 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_Z13PALLAS_SPEC_5v", scope: !4, file: !4, line: 24, type: !207, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0)
!207 = !DISubroutineType(types: !208)
!208 = !{!19}
!209 = !DILocation(line: 24, column: 9, scope: !206)
!210 = !DILocation(line: 24, column: 24, scope: !206)
!211 = !DILocation(line: 0, scope: !206)
!212 = !{!"pallas.sepConj"}
!213 = !{!"pallas.perm"}
!214 = !{!"pallas.fracOf"}
!215 = !{!"pallas.result"}
