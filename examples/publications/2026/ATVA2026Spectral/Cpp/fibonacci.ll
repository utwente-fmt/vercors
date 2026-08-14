; ModuleID = 'tmp_spectral/tmp_ir_source0.ll'
source_filename = "examples/publications/2026/ATVA2026Spectral/Cpp/fibonacci.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@llvm.compiler.used = appending global [6 x ptr] [ptr @_Z13PALLAS_SPEC_0i, ptr @_Z13PALLAS_SPEC_1i, ptr @_Z13PALLAS_SPEC_2i, ptr @_Z13PALLAS_SPEC_4iiii, ptr @_Z13PALLAS_SPEC_3iiii, ptr @_Z13PALLAS_SPEC_5iiii], section "llvm.metadata"
@llvm.used = appending global [6 x ptr] [ptr @_Z13PALLAS_SPEC_0i, ptr @_Z13PALLAS_SPEC_1i, ptr @_Z13PALLAS_SPEC_2i, ptr @_Z13PALLAS_SPEC_4iiii, ptr @_Z13PALLAS_SPEC_3iiii, ptr @_Z13PALLAS_SPEC_5iiii], section "llvm.metadata"

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef i32 @_Z6fibReci(i32 noundef %0) #0 !dbg !109 !pallas.fcontract !113 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !120, metadata !DIExpression()), !dbg !126
  %2 = icmp eq i32 %0, 0, !dbg !127
  br i1 %2, label %3, label %4, !dbg !129

3:                                                ; preds = %1
  br label %13, !dbg !130

4:                                                ; preds = %1
  %5 = icmp eq i32 %0, 1, !dbg !132
  br i1 %5, label %6, label %7, !dbg !134

6:                                                ; preds = %4
  br label %13, !dbg !135

7:                                                ; preds = %4
  %8 = sub nsw i32 %0, 1, !dbg !137
  %9 = call noundef i32 @_Z6fibReci(i32 noundef %8), !dbg !139
  %10 = sub nsw i32 %0, 2, !dbg !140
  %11 = call noundef i32 @_Z6fibReci(i32 noundef %10), !dbg !141
  %12 = add nsw i32 %9, %11, !dbg !142
  br label %13, !dbg !143

13:                                               ; preds = %7, %6, %3
  %.0 = phi i32 [ 0, %3 ], [ 1, %6 ], [ %12, %7 ], !dbg !144
  ret i32 %.0, !dbg !145
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef i32 @_Z5fibIti(i32 noundef %0) #2 !dbg !146 !pallas.fcontract !147 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !153, metadata !DIExpression()), !dbg !162
  %2 = icmp eq i32 %0, 0, !dbg !163
  br i1 %2, label %3, label %4, !dbg !165

3:                                                ; preds = %1
  br label %16, !dbg !166

4:                                                ; preds = %1
  %5 = icmp eq i32 %0, 1, !dbg !168
  br i1 %5, label %6, label %7, !dbg !170

6:                                                ; preds = %4
  br label %16, !dbg !171

7:                                                ; preds = %4
  br label %8

8:                                                ; preds = %7
  call void @llvm.dbg.value(metadata i32 0, metadata !173, metadata !DIExpression()), !dbg !162
  call void @llvm.dbg.value(metadata i32 1, metadata !174, metadata !DIExpression()), !dbg !162
  call void @llvm.dbg.value(metadata i32 2, metadata !175, metadata !DIExpression()), !dbg !177
  br label %9, !dbg !178

9:                                                ; preds = %13, %8
  %.03 = phi i32 [ 1, %8 ], [ %12, %13 ], !dbg !162
  %.02 = phi i32 [ 0, %8 ], [ %.03, %13 ], !dbg !162
  %.01 = phi i32 [ 2, %8 ], [ %14, %13 ], !dbg !179
  call void @llvm.dbg.value(metadata i32 %.01, metadata !175, metadata !DIExpression()), !dbg !177
  call void @llvm.dbg.value(metadata i32 %.02, metadata !173, metadata !DIExpression()), !dbg !162
  call void @llvm.dbg.value(metadata i32 %.03, metadata !174, metadata !DIExpression()), !dbg !162
  %10 = icmp sle i32 %.01, %0, !dbg !180
  br i1 %10, label %11, label %15, !dbg !182

11:                                               ; preds = %9
  %12 = add nsw i32 %.02, %.03, !dbg !183
  call void @llvm.dbg.value(metadata i32 %12, metadata !185, metadata !DIExpression()), !dbg !186
  call void @llvm.dbg.value(metadata i32 %.03, metadata !173, metadata !DIExpression()), !dbg !162
  call void @llvm.dbg.value(metadata i32 %12, metadata !174, metadata !DIExpression()), !dbg !162
  br label %13, !dbg !187

13:                                               ; preds = %11
  %14 = add nsw i32 %.01, 1, !dbg !188
  call void @llvm.dbg.value(metadata i32 %14, metadata !175, metadata !DIExpression()), !dbg !177
  br label %9, !dbg !189, !llvm.loop !190

15:                                               ; preds = %9
  br label %16, !dbg !233

16:                                               ; preds = %15, %6, %3
  %.0 = phi i32 [ 0, %3 ], [ 1, %6 ], [ %.03, %15 ], !dbg !162
  ret i32 %.0, !dbg !234
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_0i(i32 noundef %0) #2 !dbg !122 !pallas.exprWrapper !235 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !121, metadata !DIExpression()), !dbg !236
  %2 = icmp sge i32 %0, 0, !dbg !237
  ret i1 %2, !dbg !236
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_1i(i32 noundef %0) #2 !dbg !155 !pallas.exprWrapper !235 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !154, metadata !DIExpression()), !dbg !238
  %2 = icmp sge i32 %0, 0, !dbg !239
  ret i1 %2, !dbg !238
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_2i(i32 noundef %0) #0 !dbg !161 !pallas.exprWrapper !235 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !160, metadata !DIExpression()), !dbg !240
  %2 = call noundef i32 @"pallas.result noundef i32"(), !dbg !241
  %3 = call noundef i32 @_Z6fibReci(i32 noundef %0), !dbg !242
  %4 = icmp eq i32 %2, %3, !dbg !243
  ret i1 %4, !dbg !240
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_4iiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !214 !pallas.exprWrapper !235 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !213, metadata !DIExpression()), !dbg !244
  call void @llvm.dbg.value(metadata i32 %1, metadata !216, metadata !DIExpression()), !dbg !244
  call void @llvm.dbg.value(metadata i32 %2, metadata !218, metadata !DIExpression()), !dbg !244
  call void @llvm.dbg.value(metadata i32 %3, metadata !220, metadata !DIExpression()), !dbg !244
  %5 = sub nsw i32 %3, 1, !dbg !245
  %6 = call noundef i32 @_Z6fibReci(i32 noundef %5), !dbg !246
  %7 = icmp eq i32 %2, %6, !dbg !247
  ret i1 %7, !dbg !244
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_3iiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !200 !pallas.exprWrapper !235 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !199, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %1, metadata !204, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %2, metadata !206, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %3, metadata !208, metadata !DIExpression()), !dbg !248
  %5 = icmp sle i32 2, %3, !dbg !249
  %6 = add nsw i32 %0, 1, !dbg !250
  %7 = icmp sle i32 %3, %6, !dbg !251
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !252
  ret i1 %8, !dbg !248
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_5iiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !226 !pallas.exprWrapper !235 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !225, metadata !DIExpression()), !dbg !253
  call void @llvm.dbg.value(metadata i32 %1, metadata !228, metadata !DIExpression()), !dbg !253
  call void @llvm.dbg.value(metadata i32 %2, metadata !230, metadata !DIExpression()), !dbg !253
  call void @llvm.dbg.value(metadata i32 %3, metadata !232, metadata !DIExpression()), !dbg !253
  %5 = sub nsw i32 %3, 2, !dbg !254
  %6 = call noundef i32 @_Z6fibReci(i32 noundef %5), !dbg !255
  %7 = icmp eq i32 %1, %6, !dbg !256
  ret i1 %7, !dbg !253
}

declare !pallas.specLib !257 noundef i32 @"pallas.result noundef i32"()

declare !pallas.specLib !258 i1 @pallas.scAnd(i1, i1)

attributes #0 = { mustprogress noinline uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!101, !102, !103, !104, !105, !106, !107}
!llvm.ident = !{!108, !108}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/Cpp/fibonacci.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "051e98647abf765f2d682db68e8fb05b")
!2 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !3, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, imports: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp_spectral/source_wrappers.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "da01c156cfccf5289287757cb93871ef")
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
!109 = distinct !DISubprogram(name: "fibRec", linkageName: "_Z6fibReci", scope: !1, file: !1, line: 12, type: !110, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!110 = !DISubroutineType(types: !111)
!111 = !{!20, !20}
!112 = !{}
!113 = !{!114, i1 true, i1 false, !112, !112, !116}
!114 = !{!"pallas.srcLoc", i64 8, i64 1, i64 11, i64 1, !115}
!115 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/Cpp/fibonacci.cpp", directory: "", checksumkind: CSK_MD5, checksum: "051e98647abf765f2d682db68e8fb05b")
!116 = !{!"pallas.requires", !117, ptr @_Z13PALLAS_SPEC_0i, !112, !112, !118}
!117 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 16, !115}
!118 = !{!119}
!119 = !{!120, !121}
!120 = !DILocalVariable(name: "n", arg: 1, scope: !109, file: !1, line: 12, type: !20)
!121 = !DILocalVariable(name: "n", arg: 1, scope: !122, file: !1, line: 10, type: !20)
!122 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_Z13PALLAS_SPEC_0i", scope: !1, file: !1, line: 10, type: !123, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!123 = !DISubroutineType(types: !124)
!124 = !{!125, !20}
!125 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!126 = !DILocation(line: 0, scope: !109)
!127 = !DILocation(line: 13, column: 11, scope: !128)
!128 = distinct !DILexicalBlock(scope: !109, file: !1, line: 13, column: 9)
!129 = !DILocation(line: 13, column: 9, scope: !109)
!130 = !DILocation(line: 14, column: 9, scope: !131)
!131 = distinct !DILexicalBlock(scope: !128, file: !1, line: 13, column: 17)
!132 = !DILocation(line: 15, column: 18, scope: !133)
!133 = distinct !DILexicalBlock(scope: !128, file: !1, line: 15, column: 16)
!134 = !DILocation(line: 15, column: 16, scope: !128)
!135 = !DILocation(line: 16, column: 9, scope: !136)
!136 = distinct !DILexicalBlock(scope: !133, file: !1, line: 15, column: 24)
!137 = !DILocation(line: 18, column: 25, scope: !138)
!138 = distinct !DILexicalBlock(scope: !133, file: !1, line: 17, column: 12)
!139 = !DILocation(line: 18, column: 16, scope: !138)
!140 = !DILocation(line: 18, column: 41, scope: !138)
!141 = !DILocation(line: 18, column: 32, scope: !138)
!142 = !DILocation(line: 18, column: 30, scope: !138)
!143 = !DILocation(line: 18, column: 9, scope: !138)
!144 = !DILocation(line: 0, scope: !128)
!145 = !DILocation(line: 20, column: 1, scope: !109)
!146 = distinct !DISubprogram(name: "fibIt", linkageName: "_Z5fibIti", scope: !1, file: !1, line: 27, type: !110, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!147 = !{!148, i1 false, i1 false, !112, !112, !149, !156}
!148 = !{!"pallas.srcLoc", i64 23, i64 1, i64 26, i64 1, !115}
!149 = !{!"pallas.requires", !150, ptr @_Z13PALLAS_SPEC_1i, !112, !112, !151}
!150 = !{!"pallas.srcLoc", i64 24, i64 1, i64 24, i64 16, !115}
!151 = !{!152}
!152 = !{!153, !154}
!153 = !DILocalVariable(name: "n", arg: 1, scope: !146, file: !1, line: 27, type: !20)
!154 = !DILocalVariable(name: "n", arg: 1, scope: !155, file: !1, line: 24, type: !20)
!155 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_Z13PALLAS_SPEC_1i", scope: !1, file: !1, line: 24, type: !123, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!156 = !{!"pallas.ensures", !157, ptr @_Z13PALLAS_SPEC_2i, !112, !112, !158}
!157 = !{!"pallas.srcLoc", i64 25, i64 1, i64 25, i64 36, !115}
!158 = !{!159}
!159 = !{!153, !160}
!160 = !DILocalVariable(name: "n", arg: 1, scope: !161, file: !1, line: 25, type: !20)
!161 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_Z13PALLAS_SPEC_2i", scope: !1, file: !1, line: 25, type: !123, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!162 = !DILocation(line: 0, scope: !146)
!163 = !DILocation(line: 28, column: 12, scope: !164)
!164 = distinct !DILexicalBlock(scope: !146, file: !1, line: 28, column: 10)
!165 = !DILocation(line: 28, column: 10, scope: !146)
!166 = !DILocation(line: 29, column: 9, scope: !167)
!167 = distinct !DILexicalBlock(scope: !164, file: !1, line: 28, column: 18)
!168 = !DILocation(line: 30, column: 20, scope: !169)
!169 = distinct !DILexicalBlock(scope: !164, file: !1, line: 30, column: 18)
!170 = !DILocation(line: 30, column: 18, scope: !164)
!171 = !DILocation(line: 31, column: 9, scope: !172)
!172 = distinct !DILexicalBlock(scope: !169, file: !1, line: 30, column: 26)
!173 = !DILocalVariable(name: "prevRes", scope: !146, file: !1, line: 34, type: !20)
!174 = !DILocalVariable(name: "res", scope: !146, file: !1, line: 35, type: !20)
!175 = !DILocalVariable(name: "i", scope: !176, file: !1, line: 42, type: !20)
!176 = distinct !DILexicalBlock(scope: !146, file: !1, line: 42, column: 5)
!177 = !DILocation(line: 0, scope: !176)
!178 = !DILocation(line: 42, column: 10, scope: !176)
!179 = !DILocation(line: 42, scope: !176)
!180 = !DILocation(line: 42, column: 23, scope: !181)
!181 = distinct !DILexicalBlock(scope: !176, file: !1, line: 42, column: 5)
!182 = !DILocation(line: 42, column: 5, scope: !176)
!183 = !DILocation(line: 43, column: 27, scope: !184)
!184 = distinct !DILexicalBlock(scope: !181, file: !1, line: 42, column: 34)
!185 = !DILocalVariable(name: "tmp", scope: !184, file: !1, line: 43, type: !20)
!186 = !DILocation(line: 0, scope: !184)
!187 = !DILocation(line: 46, column: 5, scope: !184)
!188 = !DILocation(line: 42, column: 30, scope: !181)
!189 = !DILocation(line: 42, column: 5, scope: !181)
!190 = distinct !{!190, !182, !191, !192, !193}
!191 = !DILocation(line: 46, column: 5, scope: !176)
!192 = !{!"llvm.loop.mustprogress"}
!193 = !{!"pallas.loopInvBlock", !194, !195, !209, !221}
!194 = !{!"pallas.srcLoc", i64 37, i64 5, i64 41, i64 5, !115}
!195 = !{!"pallas.loopInv", !196, ptr @_Z13PALLAS_SPEC_3iiii, !112, !112, !197}
!196 = !{!"pallas.srcLoc", i64 38, i64 5, i64 38, i64 42, !115}
!197 = !{!198, !203, !205, !207}
!198 = !{!153, !199}
!199 = !DILocalVariable(name: "n", arg: 1, scope: !200, file: !1, line: 38, type: !20)
!200 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_Z13PALLAS_SPEC_3iiii", scope: !1, file: !1, line: 38, type: !201, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!201 = !DISubroutineType(types: !202)
!202 = !{!125, !20, !20, !20, !20}
!203 = !{!173, !204}
!204 = !DILocalVariable(name: "prevRes", arg: 2, scope: !200, file: !1, line: 38, type: !20)
!205 = !{!174, !206}
!206 = !DILocalVariable(name: "res", arg: 3, scope: !200, file: !1, line: 38, type: !20)
!207 = !{!175, !208}
!208 = !DILocalVariable(name: "i", arg: 4, scope: !200, file: !1, line: 38, type: !20)
!209 = !{!"pallas.loopInv", !210, ptr @_Z13PALLAS_SPEC_4iiii, !112, !112, !211}
!210 = !{!"pallas.srcLoc", i64 39, i64 5, i64 39, i64 38, !115}
!211 = !{!212, !215, !217, !219}
!212 = !{!153, !213}
!213 = !DILocalVariable(name: "n", arg: 1, scope: !214, file: !1, line: 39, type: !20)
!214 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_Z13PALLAS_SPEC_4iiii", scope: !1, file: !1, line: 39, type: !201, scopeLine: 39, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!215 = !{!173, !216}
!216 = !DILocalVariable(name: "prevRes", arg: 2, scope: !214, file: !1, line: 39, type: !20)
!217 = !{!174, !218}
!218 = !DILocalVariable(name: "res", arg: 3, scope: !214, file: !1, line: 39, type: !20)
!219 = !{!175, !220}
!220 = !DILocalVariable(name: "i", arg: 4, scope: !214, file: !1, line: 39, type: !20)
!221 = !{!"pallas.loopInv", !222, ptr @_Z13PALLAS_SPEC_5iiii, !112, !112, !223}
!222 = !{!"pallas.srcLoc", i64 40, i64 5, i64 40, i64 42, !115}
!223 = !{!224, !227, !229, !231}
!224 = !{!153, !225}
!225 = !DILocalVariable(name: "n", arg: 1, scope: !226, file: !1, line: 40, type: !20)
!226 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_Z13PALLAS_SPEC_5iiii", scope: !1, file: !1, line: 40, type: !201, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !112)
!227 = !{!173, !228}
!228 = !DILocalVariable(name: "prevRes", arg: 2, scope: !226, file: !1, line: 40, type: !20)
!229 = !{!174, !230}
!230 = !DILocalVariable(name: "res", arg: 3, scope: !226, file: !1, line: 40, type: !20)
!231 = !{!175, !232}
!232 = !DILocalVariable(name: "i", arg: 4, scope: !226, file: !1, line: 40, type: !20)
!233 = !DILocation(line: 47, column: 5, scope: !146)
!234 = !DILocation(line: 48, column: 1, scope: !146)
!235 = !{!""}
!236 = !DILocation(line: 0, scope: !122)
!237 = !DILocation(line: 10, column: 12, scope: !122)
!238 = !DILocation(line: 0, scope: !155)
!239 = !DILocation(line: 24, column: 12, scope: !155)
!240 = !DILocation(line: 0, scope: !161)
!241 = !DILocation(line: 25, column: 9, scope: !161)
!242 = !DILocation(line: 25, column: 27, scope: !161)
!243 = !DILocation(line: 25, column: 24, scope: !161)
!244 = !DILocation(line: 0, scope: !214)
!245 = !DILocation(line: 39, column: 35, scope: !214)
!246 = !DILocation(line: 39, column: 27, scope: !214)
!247 = !DILocation(line: 39, column: 24, scope: !214)
!248 = !DILocation(line: 0, scope: !200)
!249 = !DILocation(line: 38, column: 27, scope: !200)
!250 = !DILocation(line: 38, column: 39, scope: !200)
!251 = !DILocation(line: 38, column: 35, scope: !200)
!252 = !DILocation(line: 38, column: 20, scope: !200)
!253 = !DILocation(line: 0, scope: !226)
!254 = !DILocation(line: 40, column: 39, scope: !226)
!255 = !DILocation(line: 40, column: 31, scope: !226)
!256 = !DILocation(line: 40, column: 28, scope: !226)
!257 = !{!"pallas.result"}
!258 = !{!"pallas.scAnd"}
