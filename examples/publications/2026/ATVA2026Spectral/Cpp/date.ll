; ModuleID = 'tmp_spectral/tmp_ir_source0.ll'
source_filename = "examples/publications/2026/ATVA2026Spectral/Cpp/date.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@llvm.compiler.used = appending global [6 x ptr] [ptr @_Z13PALLAS_SPEC_0iiiiii, ptr @_Z13PALLAS_SPEC_1iiiiii, ptr @_Z13PALLAS_SPEC_2iiiiii, ptr @_Z13PALLAS_SPEC_3iiiiii, ptr @_Z13PALLAS_SPEC_4iiiiii, ptr @_Z13PALLAS_SPEC_5iiiiii], section "llvm.metadata"
@llvm.used = appending global [6 x ptr] [ptr @_Z13PALLAS_SPEC_0iiiiii, ptr @_Z13PALLAS_SPEC_1iiiiii, ptr @_Z13PALLAS_SPEC_2iiiiii, ptr @_Z13PALLAS_SPEC_3iiiiii, ptr @_Z13PALLAS_SPEC_4iiiiii, ptr @_Z13PALLAS_SPEC_5iiiiii], section "llvm.metadata"

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z5lateriiiiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 !dbg !109 !pallas.fcontract !114 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !121, metadata !DIExpression()), !dbg !219
  call void @llvm.dbg.value(metadata i32 %1, metadata !125, metadata !DIExpression()), !dbg !219
  call void @llvm.dbg.value(metadata i32 %2, metadata !128, metadata !DIExpression()), !dbg !219
  call void @llvm.dbg.value(metadata i32 %3, metadata !131, metadata !DIExpression()), !dbg !219
  call void @llvm.dbg.value(metadata i32 %4, metadata !134, metadata !DIExpression()), !dbg !219
  call void @llvm.dbg.value(metadata i32 %5, metadata !137, metadata !DIExpression()), !dbg !219
  %7 = icmp ne i32 %0, %3, !dbg !220
  br i1 %7, label %8, label %10, !dbg !222

8:                                                ; preds = %6
  %9 = icmp sgt i32 %0, %3, !dbg !223
  br label %16, !dbg !225

10:                                               ; preds = %6
  %11 = icmp ne i32 %1, %4, !dbg !226
  br i1 %11, label %12, label %14, !dbg !228

12:                                               ; preds = %10
  %13 = icmp sgt i32 %1, %4, !dbg !229
  br label %16, !dbg !231

14:                                               ; preds = %10
  %15 = icmp sgt i32 %2, %5, !dbg !232
  br label %16, !dbg !234

16:                                               ; preds = %14, %12, %8
  %.0 = phi i1 [ %9, %8 ], [ %13, %12 ], [ %15, %14 ], !dbg !235
  ret i1 %.0, !dbg !236
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef i32 @_Z4testv() #0 !dbg !237 {
  %1 = call noundef zeroext i1 @_Z5lateriiiiii(i32 noundef 2023, i32 noundef 3, i32 noundef 7, i32 noundef 2023, i32 noundef 1, i32 noundef 1), !dbg !240
  %2 = call noundef zeroext i1 @_Z5lateriiiiii(i32 noundef 1, i32 noundef 1, i32 noundef 2023, i32 noundef 15, i32 noundef 3, i32 noundef 2023), !dbg !241
  ret i32 0, !dbg !242
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_0iiiiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #2 !dbg !123 !pallas.exprWrapper !243 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !122, metadata !DIExpression()), !dbg !244
  call void @llvm.dbg.value(metadata i32 %1, metadata !126, metadata !DIExpression()), !dbg !244
  call void @llvm.dbg.value(metadata i32 %2, metadata !129, metadata !DIExpression()), !dbg !244
  call void @llvm.dbg.value(metadata i32 %3, metadata !132, metadata !DIExpression()), !dbg !244
  call void @llvm.dbg.value(metadata i32 %4, metadata !135, metadata !DIExpression()), !dbg !244
  call void @llvm.dbg.value(metadata i32 %5, metadata !138, metadata !DIExpression()), !dbg !244
  %7 = icmp sle i32 1, %1, !dbg !245
  %8 = icmp sle i32 %1, 12, !dbg !246
  %9 = call i1 @pallas.scAnd(i1 %7, i1 %8), !dbg !247
  ret i1 %9, !dbg !244
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_1iiiiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #2 !dbg !144 !pallas.exprWrapper !243 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !143, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %1, metadata !146, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %2, metadata !148, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %3, metadata !150, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %4, metadata !152, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %5, metadata !154, metadata !DIExpression()), !dbg !248
  %7 = icmp sle i32 1, %2, !dbg !249
  %8 = icmp sle i32 %2, 31, !dbg !250
  %9 = call i1 @pallas.scAnd(i1 %7, i1 %8), !dbg !251
  ret i1 %9, !dbg !248
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_2iiiiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #2 !dbg !160 !pallas.exprWrapper !243 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !159, metadata !DIExpression()), !dbg !252
  call void @llvm.dbg.value(metadata i32 %1, metadata !162, metadata !DIExpression()), !dbg !252
  call void @llvm.dbg.value(metadata i32 %2, metadata !164, metadata !DIExpression()), !dbg !252
  call void @llvm.dbg.value(metadata i32 %3, metadata !166, metadata !DIExpression()), !dbg !252
  call void @llvm.dbg.value(metadata i32 %4, metadata !168, metadata !DIExpression()), !dbg !252
  call void @llvm.dbg.value(metadata i32 %5, metadata !170, metadata !DIExpression()), !dbg !252
  %7 = icmp sle i32 1, %4, !dbg !253
  %8 = icmp sle i32 %4, 12, !dbg !254
  %9 = call i1 @pallas.scAnd(i1 %7, i1 %8), !dbg !255
  ret i1 %9, !dbg !252
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_3iiiiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #2 !dbg !176 !pallas.exprWrapper !243 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !175, metadata !DIExpression()), !dbg !256
  call void @llvm.dbg.value(metadata i32 %1, metadata !178, metadata !DIExpression()), !dbg !256
  call void @llvm.dbg.value(metadata i32 %2, metadata !180, metadata !DIExpression()), !dbg !256
  call void @llvm.dbg.value(metadata i32 %3, metadata !182, metadata !DIExpression()), !dbg !256
  call void @llvm.dbg.value(metadata i32 %4, metadata !184, metadata !DIExpression()), !dbg !256
  call void @llvm.dbg.value(metadata i32 %5, metadata !186, metadata !DIExpression()), !dbg !256
  %7 = icmp sle i32 1, %5, !dbg !257
  %8 = icmp sle i32 %5, 31, !dbg !258
  %9 = call i1 @pallas.scAnd(i1 %7, i1 %8), !dbg !259
  ret i1 %9, !dbg !256
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_4iiiiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #2 !dbg !192 !pallas.exprWrapper !243 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !191, metadata !DIExpression()), !dbg !260
  call void @llvm.dbg.value(metadata i32 %1, metadata !194, metadata !DIExpression()), !dbg !260
  call void @llvm.dbg.value(metadata i32 %2, metadata !196, metadata !DIExpression()), !dbg !260
  call void @llvm.dbg.value(metadata i32 %3, metadata !198, metadata !DIExpression()), !dbg !260
  call void @llvm.dbg.value(metadata i32 %4, metadata !200, metadata !DIExpression()), !dbg !260
  call void @llvm.dbg.value(metadata i32 %5, metadata !202, metadata !DIExpression()), !dbg !260
  %7 = icmp sgt i32 %0, %3, !dbg !261
  %8 = call noundef zeroext i1 @"pallas.result noundef zeroext i1"(), !dbg !262
  %9 = zext i1 %8 to i32, !dbg !262
  %10 = icmp eq i32 %9, 1, !dbg !263
  %11 = call i1 @pallas.imply(i1 %7, i1 %10), !dbg !264
  ret i1 %11, !dbg !260
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_5iiiiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #2 !dbg !208 !pallas.exprWrapper !243 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !207, metadata !DIExpression()), !dbg !265
  call void @llvm.dbg.value(metadata i32 %1, metadata !210, metadata !DIExpression()), !dbg !265
  call void @llvm.dbg.value(metadata i32 %2, metadata !212, metadata !DIExpression()), !dbg !265
  call void @llvm.dbg.value(metadata i32 %3, metadata !214, metadata !DIExpression()), !dbg !265
  call void @llvm.dbg.value(metadata i32 %4, metadata !216, metadata !DIExpression()), !dbg !265
  call void @llvm.dbg.value(metadata i32 %5, metadata !218, metadata !DIExpression()), !dbg !265
  %7 = icmp eq i32 %0, %3, !dbg !266
  %8 = icmp eq i32 %1, %4, !dbg !267
  %9 = call i1 @pallas.scAnd(i1 %7, i1 %8), !dbg !268
  %10 = call noundef zeroext i1 @"pallas.result noundef zeroext i1"(), !dbg !269
  %11 = zext i1 %10 to i32, !dbg !269
  %12 = icmp sgt i32 %2, %5, !dbg !270
  %13 = zext i1 %12 to i32, !dbg !271
  %14 = icmp eq i32 %11, %13, !dbg !272
  %15 = call i1 @pallas.imply(i1 %9, i1 %14), !dbg !273
  ret i1 %15, !dbg !265
}

declare !pallas.specLib !274 i1 @pallas.imply(i1, i1)

declare !pallas.specLib !275 noundef zeroext i1 @"pallas.result noundef zeroext i1"()

declare !pallas.specLib !276 i1 @pallas.scAnd(i1, i1)

attributes #0 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { mustprogress noinline uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!101, !102, !103, !104, !105, !106, !107}
!llvm.ident = !{!108, !108}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/Cpp/date.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "624a2c405d2e5b1c967e1855c9a53e40")
!2 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !3, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, imports: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp_spectral/source_wrappers.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "4557e3afe1d13e280708d2e52b0f1880")
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
!109 = distinct !DISubprogram(name: "later", linkageName: "_Z5lateriiiiii", scope: !1, file: !1, line: 17, type: !110, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !113)
!110 = !DISubroutineType(types: !111)
!111 = !{!112, !20, !20, !20, !20, !20, !20}
!112 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!113 = !{}
!114 = !{!115, i1 false, i1 false, !113, !113, !117, !139, !155, !171, !187, !203}
!115 = !{!"pallas.srcLoc", i64 7, i64 1, i64 16, i64 1, !116}
!116 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/Cpp/date.cpp", directory: "", checksumkind: CSK_MD5, checksum: "624a2c405d2e5b1c967e1855c9a53e40")
!117 = !{!"pallas.requires", !118, ptr @_Z13PALLAS_SPEC_0iiiiii, !113, !113, !119}
!118 = !{!"pallas.srcLoc", i64 8, i64 1, i64 8, i64 33, !116}
!119 = !{!120, !124, !127, !130, !133, !136}
!120 = !{!121, !122}
!121 = !DILocalVariable(name: "y1", arg: 1, scope: !109, file: !1, line: 17, type: !20)
!122 = !DILocalVariable(name: "y1", arg: 1, scope: !123, file: !1, line: 8, type: !20)
!123 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_Z13PALLAS_SPEC_0iiiiii", scope: !1, file: !1, line: 8, type: !110, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !113)
!124 = !{!125, !126}
!125 = !DILocalVariable(name: "m1", arg: 2, scope: !109, file: !1, line: 17, type: !20)
!126 = !DILocalVariable(name: "m1", arg: 2, scope: !123, file: !1, line: 8, type: !20)
!127 = !{!128, !129}
!128 = !DILocalVariable(name: "d1", arg: 3, scope: !109, file: !1, line: 17, type: !20)
!129 = !DILocalVariable(name: "d1", arg: 3, scope: !123, file: !1, line: 8, type: !20)
!130 = !{!131, !132}
!131 = !DILocalVariable(name: "y2", arg: 4, scope: !109, file: !1, line: 18, type: !20)
!132 = !DILocalVariable(name: "y2", arg: 4, scope: !123, file: !1, line: 8, type: !20)
!133 = !{!134, !135}
!134 = !DILocalVariable(name: "m2", arg: 5, scope: !109, file: !1, line: 18, type: !20)
!135 = !DILocalVariable(name: "m2", arg: 5, scope: !123, file: !1, line: 8, type: !20)
!136 = !{!137, !138}
!137 = !DILocalVariable(name: "d2", arg: 6, scope: !109, file: !1, line: 18, type: !20)
!138 = !DILocalVariable(name: "d2", arg: 6, scope: !123, file: !1, line: 8, type: !20)
!139 = !{!"pallas.requires", !140, ptr @_Z13PALLAS_SPEC_1iiiiii, !113, !113, !141}
!140 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 33, !116}
!141 = !{!142, !145, !147, !149, !151, !153}
!142 = !{!121, !143}
!143 = !DILocalVariable(name: "y1", arg: 1, scope: !144, file: !1, line: 9, type: !20)
!144 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_Z13PALLAS_SPEC_1iiiiii", scope: !1, file: !1, line: 9, type: !110, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !113)
!145 = !{!125, !146}
!146 = !DILocalVariable(name: "m1", arg: 2, scope: !144, file: !1, line: 9, type: !20)
!147 = !{!128, !148}
!148 = !DILocalVariable(name: "d1", arg: 3, scope: !144, file: !1, line: 9, type: !20)
!149 = !{!131, !150}
!150 = !DILocalVariable(name: "y2", arg: 4, scope: !144, file: !1, line: 9, type: !20)
!151 = !{!134, !152}
!152 = !DILocalVariable(name: "m2", arg: 5, scope: !144, file: !1, line: 9, type: !20)
!153 = !{!137, !154}
!154 = !DILocalVariable(name: "d2", arg: 6, scope: !144, file: !1, line: 9, type: !20)
!155 = !{!"pallas.requires", !156, ptr @_Z13PALLAS_SPEC_2iiiiii, !113, !113, !157}
!156 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 33, !116}
!157 = !{!158, !161, !163, !165, !167, !169}
!158 = !{!121, !159}
!159 = !DILocalVariable(name: "y1", arg: 1, scope: !160, file: !1, line: 10, type: !20)
!160 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_Z13PALLAS_SPEC_2iiiiii", scope: !1, file: !1, line: 10, type: !110, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !113)
!161 = !{!125, !162}
!162 = !DILocalVariable(name: "m1", arg: 2, scope: !160, file: !1, line: 10, type: !20)
!163 = !{!128, !164}
!164 = !DILocalVariable(name: "d1", arg: 3, scope: !160, file: !1, line: 10, type: !20)
!165 = !{!131, !166}
!166 = !DILocalVariable(name: "y2", arg: 4, scope: !160, file: !1, line: 10, type: !20)
!167 = !{!134, !168}
!168 = !DILocalVariable(name: "m2", arg: 5, scope: !160, file: !1, line: 10, type: !20)
!169 = !{!137, !170}
!170 = !DILocalVariable(name: "d2", arg: 6, scope: !160, file: !1, line: 10, type: !20)
!171 = !{!"pallas.requires", !172, ptr @_Z13PALLAS_SPEC_3iiiiii, !113, !113, !173}
!172 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 33, !116}
!173 = !{!174, !177, !179, !181, !183, !185}
!174 = !{!121, !175}
!175 = !DILocalVariable(name: "y1", arg: 1, scope: !176, file: !1, line: 11, type: !20)
!176 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_Z13PALLAS_SPEC_3iiiiii", scope: !1, file: !1, line: 11, type: !110, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !113)
!177 = !{!125, !178}
!178 = !DILocalVariable(name: "m1", arg: 2, scope: !176, file: !1, line: 11, type: !20)
!179 = !{!128, !180}
!180 = !DILocalVariable(name: "d1", arg: 3, scope: !176, file: !1, line: 11, type: !20)
!181 = !{!131, !182}
!182 = !DILocalVariable(name: "y2", arg: 4, scope: !176, file: !1, line: 11, type: !20)
!183 = !{!134, !184}
!184 = !DILocalVariable(name: "m2", arg: 5, scope: !176, file: !1, line: 11, type: !20)
!185 = !{!137, !186}
!186 = !DILocalVariable(name: "d2", arg: 6, scope: !176, file: !1, line: 11, type: !20)
!187 = !{!"pallas.ensures", !188, ptr @_Z13PALLAS_SPEC_4iiiiii, !113, !113, !189}
!188 = !{!"pallas.srcLoc", i64 12, i64 1, i64 13, i64 40, !116}
!189 = !{!190, !193, !195, !197, !199, !201}
!190 = !{!121, !191}
!191 = !DILocalVariable(name: "y1", arg: 1, scope: !192, file: !1, line: 12, type: !20)
!192 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_Z13PALLAS_SPEC_4iiiiii", scope: !1, file: !1, line: 12, type: !110, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !113)
!193 = !{!125, !194}
!194 = !DILocalVariable(name: "m1", arg: 2, scope: !192, file: !1, line: 12, type: !20)
!195 = !{!128, !196}
!196 = !DILocalVariable(name: "d1", arg: 3, scope: !192, file: !1, line: 12, type: !20)
!197 = !{!131, !198}
!198 = !DILocalVariable(name: "y2", arg: 4, scope: !192, file: !1, line: 12, type: !20)
!199 = !{!134, !200}
!200 = !DILocalVariable(name: "m2", arg: 5, scope: !192, file: !1, line: 12, type: !20)
!201 = !{!137, !202}
!202 = !DILocalVariable(name: "d2", arg: 6, scope: !192, file: !1, line: 12, type: !20)
!203 = !{!"pallas.ensures", !204, ptr @_Z13PALLAS_SPEC_5iiiiii, !113, !113, !205}
!204 = !{!"pallas.srcLoc", i64 14, i64 1, i64 15, i64 43, !116}
!205 = !{!206, !209, !211, !213, !215, !217}
!206 = !{!121, !207}
!207 = !DILocalVariable(name: "y1", arg: 1, scope: !208, file: !1, line: 14, type: !20)
!208 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_Z13PALLAS_SPEC_5iiiiii", scope: !1, file: !1, line: 14, type: !110, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !113)
!209 = !{!125, !210}
!210 = !DILocalVariable(name: "m1", arg: 2, scope: !208, file: !1, line: 14, type: !20)
!211 = !{!128, !212}
!212 = !DILocalVariable(name: "d1", arg: 3, scope: !208, file: !1, line: 14, type: !20)
!213 = !{!131, !214}
!214 = !DILocalVariable(name: "y2", arg: 4, scope: !208, file: !1, line: 14, type: !20)
!215 = !{!134, !216}
!216 = !DILocalVariable(name: "m2", arg: 5, scope: !208, file: !1, line: 14, type: !20)
!217 = !{!137, !218}
!218 = !DILocalVariable(name: "d2", arg: 6, scope: !208, file: !1, line: 14, type: !20)
!219 = !DILocation(line: 0, scope: !109)
!220 = !DILocation(line: 19, column: 12, scope: !221)
!221 = distinct !DILexicalBlock(scope: !109, file: !1, line: 19, column: 9)
!222 = !DILocation(line: 19, column: 9, scope: !109)
!223 = !DILocation(line: 20, column: 19, scope: !224)
!224 = distinct !DILexicalBlock(scope: !221, file: !1, line: 19, column: 19)
!225 = !DILocation(line: 20, column: 9, scope: !224)
!226 = !DILocation(line: 21, column: 19, scope: !227)
!227 = distinct !DILexicalBlock(scope: !221, file: !1, line: 21, column: 16)
!228 = !DILocation(line: 21, column: 16, scope: !221)
!229 = !DILocation(line: 22, column: 19, scope: !230)
!230 = distinct !DILexicalBlock(scope: !227, file: !1, line: 21, column: 26)
!231 = !DILocation(line: 22, column: 9, scope: !230)
!232 = !DILocation(line: 24, column: 19, scope: !233)
!233 = distinct !DILexicalBlock(scope: !227, file: !1, line: 23, column: 12)
!234 = !DILocation(line: 24, column: 9, scope: !233)
!235 = !DILocation(line: 0, scope: !221)
!236 = !DILocation(line: 26, column: 1, scope: !109)
!237 = distinct !DISubprogram(name: "test", linkageName: "_Z4testv", scope: !1, file: !1, line: 28, type: !238, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0)
!238 = !DISubroutineType(types: !239)
!239 = !{!20}
!240 = !DILocation(line: 29, column: 5, scope: !237)
!241 = !DILocation(line: 31, column: 5, scope: !237)
!242 = !DILocation(line: 33, column: 5, scope: !237)
!243 = !{!""}
!244 = !DILocation(line: 0, scope: !123)
!245 = !DILocation(line: 8, column: 17, scope: !123)
!246 = !DILocation(line: 8, column: 27, scope: !123)
!247 = !DILocation(line: 8, column: 10, scope: !123)
!248 = !DILocation(line: 0, scope: !144)
!249 = !DILocation(line: 9, column: 17, scope: !144)
!250 = !DILocation(line: 9, column: 27, scope: !144)
!251 = !DILocation(line: 9, column: 10, scope: !144)
!252 = !DILocation(line: 0, scope: !160)
!253 = !DILocation(line: 10, column: 17, scope: !160)
!254 = !DILocation(line: 10, column: 27, scope: !160)
!255 = !DILocation(line: 10, column: 10, scope: !160)
!256 = !DILocation(line: 0, scope: !176)
!257 = !DILocation(line: 11, column: 17, scope: !176)
!258 = !DILocation(line: 11, column: 27, scope: !176)
!259 = !DILocation(line: 11, column: 10, scope: !176)
!260 = !DILocation(line: 0, scope: !192)
!261 = !DILocation(line: 12, column: 19, scope: !192)
!262 = !DILocation(line: 13, column: 16, scope: !192)
!263 = !DILocation(line: 13, column: 32, scope: !192)
!264 = !DILocation(line: 12, column: 9, scope: !192)
!265 = !DILocation(line: 0, scope: !208)
!266 = !DILocation(line: 14, column: 24, scope: !208)
!267 = !DILocation(line: 14, column: 34, scope: !208)
!268 = !DILocation(line: 14, column: 16, scope: !208)
!269 = !DILocation(line: 15, column: 16, scope: !208)
!270 = !DILocation(line: 15, column: 38, scope: !208)
!271 = !DILocation(line: 15, column: 35, scope: !208)
!272 = !DILocation(line: 15, column: 32, scope: !208)
!273 = !DILocation(line: 14, column: 9, scope: !208)
!274 = !{!"pallas.imply"}
!275 = !{!"pallas.result"}
!276 = !{!"pallas.scAnd"}
