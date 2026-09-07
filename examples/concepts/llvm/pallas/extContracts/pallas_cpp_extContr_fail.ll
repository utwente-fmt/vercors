; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@llvm.compiler.used = appending global [7 x ptr] [ptr @_Z13PALLAS_SPEC_0i, ptr @_Z13PALLAS_SPEC_1i, ptr @_Z13PALLAS_SPEC_5iiii, ptr @_Z13PALLAS_SPEC_4iiii, ptr @_Z13PALLAS_SPEC_6iii, ptr @_Z13PALLAS_SPEC_2ii, ptr @_Z13PALLAS_SPEC_3ii], section "llvm.metadata"
@llvm.used = appending global [7 x ptr] [ptr @_Z13PALLAS_SPEC_0i, ptr @_Z13PALLAS_SPEC_1i, ptr @_Z13PALLAS_SPEC_2ii, ptr @_Z13PALLAS_SPEC_3ii, ptr @_Z13PALLAS_SPEC_5iiii, ptr @_Z13PALLAS_SPEC_4iiii, ptr @_Z13PALLAS_SPEC_6iii], section "llvm.metadata"

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef i32 @_Z3fooi(i32 noundef %0) #0 !dbg !111 !pallas.fcontract !115 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !122, metadata !DIExpression()), !dbg !134
  call void @llvm.dbg.declare(metadata ptr %4, metadata !135, metadata !DIExpression()), !dbg !136
  %7 = load i32, ptr %3, align 4, !dbg !137
  store i32 %7, ptr %4, align 4, !dbg !136
  %8 = load i32, ptr %4, align 4, !dbg !138
  %9 = add nsw i32 %8, 1, !dbg !138
  store i32 %9, ptr %4, align 4, !dbg !138
  %10 = load i32, ptr %4, align 4, !dbg !139
  %11 = sub nsw i32 %10, 1, !dbg !139
  store i32 %11, ptr %4, align 4, !dbg !139
  %12 = load i32, ptr %3, align 4, !dbg !140
  %13 = icmp slt i32 %12, 42, !dbg !142
  br i1 %13, label %14, label %16, !dbg !143

14:                                               ; preds = %1
  %15 = load i32, ptr %3, align 4, !dbg !144
  store i32 %15, ptr %2, align 4, !dbg !146
  br label %34, !dbg !146

16:                                               ; preds = %1
  call void @llvm.dbg.declare(metadata ptr %5, metadata !147, metadata !DIExpression()), !dbg !148
  store i32 0, ptr %5, align 4, !dbg !148
  call void @llvm.dbg.declare(metadata ptr %6, metadata !149, metadata !DIExpression()), !dbg !151
  store i32 0, ptr %6, align 4, !dbg !151
  br label %17, !dbg !152

17:                                               ; preds = %25, %16
  %18 = load i32, ptr %6, align 4, !dbg !153
  %19 = load i32, ptr %3, align 4, !dbg !155
  %20 = icmp sle i32 %18, %19, !dbg !156
  br i1 %20, label %21, label %28, !dbg !157

21:                                               ; preds = %17
  %22 = load i32, ptr %6, align 4, !dbg !158
  %23 = load i32, ptr %5, align 4, !dbg !160
  %24 = add nsw i32 %23, %22, !dbg !160
  store i32 %24, ptr %5, align 4, !dbg !160
  br label %25, !dbg !161

25:                                               ; preds = %21
  %26 = load i32, ptr %6, align 4, !dbg !162
  %27 = add nsw i32 %26, 1, !dbg !162
  store i32 %27, ptr %6, align 4, !dbg !162
  br label %17, !dbg !163, !llvm.loop !164

28:                                               ; preds = %17
  %29 = load i32, ptr %5, align 4, !dbg !195, !pallas.stmntBlock !196
  %30 = call noundef i32 @_Z25anAmazingExternalFunctionii(i32 noundef %29, i32 noundef 1), !dbg !210
  %31 = load i32, ptr %5, align 4, !dbg !211
  %32 = add nsw i32 %31, %30, !dbg !211
  store i32 %32, ptr %5, align 4, !dbg !211
  %33 = load i32, ptr %5, align 4, !dbg !212
  store i32 %33, ptr %2, align 4, !dbg !213
  br label %34, !dbg !213

34:                                               ; preds = %28, %14
  %35 = load i32, ptr %2, align 4, !dbg !214
  ret i32 %35, !dbg !214
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare !pallas.extContract !215 noundef i32 @_Z25anAmazingExternalFunctionii(i32 noundef, i32 noundef) #2

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_0i(i32 noundef %0) #3 !dbg !124 !pallas.exprWrapper !222 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !123, metadata !DIExpression()), !dbg !223
  %2 = icmp sge i32 %0, 0, !dbg !224
  ret i1 %2, !dbg !223
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_1i(i32 noundef %0) #0 !dbg !133 !pallas.exprWrapper !222 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !132, metadata !DIExpression()), !dbg !225
  %2 = call noundef i32 @"pallas.result noundef i32"(), !dbg !226
  %3 = icmp sge i32 %2, 0, !dbg !227
  ret i1 %3, !dbg !225
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_5iiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #3 !dbg !188 !pallas.exprWrapper !222 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !187, metadata !DIExpression()), !dbg !228
  call void @llvm.dbg.value(metadata i32 %1, metadata !190, metadata !DIExpression()), !dbg !228
  call void @llvm.dbg.value(metadata i32 %2, metadata !192, metadata !DIExpression()), !dbg !228
  call void @llvm.dbg.value(metadata i32 %3, metadata !194, metadata !DIExpression()), !dbg !228
  %5 = icmp sge i32 %2, 0, !dbg !229
  ret i1 %5, !dbg !228
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_4iiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #3 !dbg !174 !pallas.exprWrapper !222 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !173, metadata !DIExpression()), !dbg !230
  call void @llvm.dbg.value(metadata i32 %1, metadata !178, metadata !DIExpression()), !dbg !230
  call void @llvm.dbg.value(metadata i32 %2, metadata !180, metadata !DIExpression()), !dbg !230
  call void @llvm.dbg.value(metadata i32 %3, metadata !182, metadata !DIExpression()), !dbg !230
  %5 = icmp sle i32 0, %3, !dbg !231
  br i1 %5, label %6, label %9, !dbg !232

6:                                                ; preds = %4
  %7 = add nsw i32 %0, 1, !dbg !233
  %8 = icmp sle i32 %3, %7, !dbg !234
  br label %9

9:                                                ; preds = %6, %4
  %10 = phi i1 [ false, %4 ], [ %8, %6 ], !dbg !230
  ret i1 %10, !dbg !230
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_6iii(i32 noundef %0, i32 noundef %1, i32 noundef %2) #3 !dbg !203 !pallas.exprWrapper !222 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !202, metadata !DIExpression()), !dbg !235
  call void @llvm.dbg.value(metadata i32 %1, metadata !207, metadata !DIExpression()), !dbg !235
  call void @llvm.dbg.value(metadata i32 %2, metadata !209, metadata !DIExpression()), !dbg !235
  %4 = icmp eq i32 %1, %0, !dbg !236
  ret i1 %4, !dbg !235
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_2ii(i32 noundef %0, i32 noundef %1) #3 !dbg !237 !pallas.exprWrapper !222 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !240, metadata !DIExpression()), !dbg !241
  call void @llvm.dbg.value(metadata i32 %1, metadata !242, metadata !DIExpression()), !dbg !241
  %3 = icmp sge i32 %0, 42, !dbg !243
  br i1 %3, label %4, label %6, !dbg !244

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !245
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !241
  ret i1 %7, !dbg !241
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_3ii(i32 noundef %0, i32 noundef %1) #0 !dbg !246 !pallas.exprWrapper !222 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !247, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %1, metadata !249, metadata !DIExpression()), !dbg !248
  %3 = call noundef i32 @"pallas.result noundef i32"(), !dbg !250
  %4 = icmp sge i32 %3, 0, !dbg !251
  ret i1 %4, !dbg !248
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !252 noundef i32 @"pallas.result noundef i32"()

attributes #0 = { mustprogress noinline uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!0, !2, !101}
!llvm.module.flags = !{!103, !104, !105, !106, !107, !108, !109}
!llvm.ident = !{!110, !110}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "3ce0804a8eb5623727f4076ae2ae8562")
!2 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !3, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, imports: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.cpp", directory: "/home/rme/repos/vercors", checksumkind: CSK_MD5, checksum: "173b156f9a2d15643306176e8a0d7eec")
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
!99 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !2, entity: !100, file: !3, line: 5)
!100 = !DINamespace(name: "pallasSpec", scope: null)
!101 = distinct !DICompileUnit(language: DW_LANG_C, file: !102, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!102 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr_fail.h", directory: "")
!103 = !{i32 7, !"Dwarf Version", i32 5}
!104 = !{i32 2, !"Debug Info Version", i32 3}
!105 = !{i32 1, !"wchar_size", i32 4}
!106 = !{i32 8, !"PIC Level", i32 2}
!107 = !{i32 7, !"PIE Level", i32 2}
!108 = !{i32 7, !"uwtable", i32 2}
!109 = !{i32 7, !"frame-pointer", i32 2}
!110 = !{!"Ubuntu clang version 17.0.6 (9ubuntu1)"}
!111 = distinct !DISubprogram(name: "foo", linkageName: "_Z3fooi", scope: !1, file: !1, line: 15, type: !112, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !114)
!112 = !DISubroutineType(types: !113)
!113 = !{!20, !20}
!114 = !{}
!115 = !{!116, i1 false, i1 false, !114, !114, !118, !128}
!116 = !{!"pallas.srcLoc", i64 11, i64 1, i64 14, i64 1, !117}
!117 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr.cpp", directory: "", checksumkind: CSK_MD5, checksum: "3ce0804a8eb5623727f4076ae2ae8562")
!118 = !{!"pallas.requires", !119, ptr @_Z13PALLAS_SPEC_0i, !114, !114, !120}
!119 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 16, !117}
!120 = !{!121}
!121 = !{!122, !123}
!122 = !DILocalVariable(name: "n", arg: 1, scope: !111, file: !1, line: 15, type: !20)
!123 = !DILocalVariable(name: "n", arg: 1, scope: !124, file: !1, line: 12, type: !20)
!124 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_Z13PALLAS_SPEC_0i", scope: !1, file: !1, line: 12, type: !125, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !114)
!125 = !DISubroutineType(types: !126)
!126 = !{!127, !20}
!127 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!128 = !{!"pallas.ensures", !129, ptr @_Z13PALLAS_SPEC_1i, !114, !114, !130}
!129 = !{!"pallas.srcLoc", i64 13, i64 1, i64 13, i64 28, !117}
!130 = !{!131}
!131 = !{!122, !132}
!132 = !DILocalVariable(name: "n", arg: 1, scope: !133, file: !1, line: 13, type: !20)
!133 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_Z13PALLAS_SPEC_1i", scope: !1, file: !1, line: 13, type: !125, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !114)
!134 = !DILocation(line: 15, column: 13, scope: !111)
!135 = !DILocalVariable(name: "oldN", scope: !111, file: !1, line: 16, type: !20)
!136 = !DILocation(line: 16, column: 9, scope: !111)
!137 = !DILocation(line: 16, column: 16, scope: !111)
!138 = !DILocation(line: 17, column: 10, scope: !111)
!139 = !DILocation(line: 18, column: 10, scope: !111)
!140 = !DILocation(line: 19, column: 9, scope: !141)
!141 = distinct !DILexicalBlock(scope: !111, file: !1, line: 19, column: 9)
!142 = !DILocation(line: 19, column: 11, scope: !141)
!143 = !DILocation(line: 19, column: 9, scope: !111)
!144 = !DILocation(line: 20, column: 16, scope: !145)
!145 = distinct !DILexicalBlock(scope: !141, file: !1, line: 19, column: 17)
!146 = !DILocation(line: 20, column: 9, scope: !145)
!147 = !DILocalVariable(name: "res", scope: !111, file: !1, line: 22, type: !20)
!148 = !DILocation(line: 22, column: 9, scope: !111)
!149 = !DILocalVariable(name: "i", scope: !150, file: !1, line: 27, type: !20)
!150 = distinct !DILexicalBlock(scope: !111, file: !1, line: 27, column: 5)
!151 = !DILocation(line: 27, column: 14, scope: !150)
!152 = !DILocation(line: 27, column: 10, scope: !150)
!153 = !DILocation(line: 27, column: 21, scope: !154)
!154 = distinct !DILexicalBlock(scope: !150, file: !1, line: 27, column: 5)
!155 = !DILocation(line: 27, column: 26, scope: !154)
!156 = !DILocation(line: 27, column: 23, scope: !154)
!157 = !DILocation(line: 27, column: 5, scope: !150)
!158 = !DILocation(line: 28, column: 16, scope: !159)
!159 = distinct !DILexicalBlock(scope: !154, file: !1, line: 27, column: 34)
!160 = !DILocation(line: 28, column: 13, scope: !159)
!161 = !DILocation(line: 29, column: 5, scope: !159)
!162 = !DILocation(line: 27, column: 30, scope: !154)
!163 = !DILocation(line: 27, column: 5, scope: !154)
!164 = distinct !{!164, !157, !165, !166, !167}
!165 = !DILocation(line: 29, column: 5, scope: !150)
!166 = !{!"llvm.loop.mustprogress"}
!167 = !{!"pallas.loopInvBlock", !168, !169, !183}
!168 = !{!"pallas.srcLoc", i64 23, i64 5, i64 26, i64 5, !117}
!169 = !{!"pallas.loopInv", !170, ptr @_Z13PALLAS_SPEC_4iiii, !114, !114, !171}
!170 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 40, !117}
!171 = !{!172, !177, !179, !181}
!172 = !{!122, !173}
!173 = !DILocalVariable(name: "n", arg: 1, scope: !174, file: !1, line: 24, type: !20)
!174 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_Z13PALLAS_SPEC_4iiii", scope: !1, file: !1, line: 24, type: !175, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !114)
!175 = !DISubroutineType(types: !176)
!176 = !{!127, !20, !20, !20, !20}
!177 = !{!135, !178}
!178 = !DILocalVariable(name: "oldN", arg: 2, scope: !174, file: !1, line: 24, type: !20)
!179 = !{!147, !180}
!180 = !DILocalVariable(name: "res", arg: 3, scope: !174, file: !1, line: 24, type: !20)
!181 = !{!149, !182}
!182 = !DILocalVariable(name: "i", arg: 4, scope: !174, file: !1, line: 24, type: !20)
!183 = !{!"pallas.loopInv", !184, ptr @_Z13PALLAS_SPEC_5iiii, !114, !114, !185}
!184 = !{!"pallas.srcLoc", i64 25, i64 5, i64 25, i64 29, !117}
!185 = !{!186, !189, !191, !193}
!186 = !{!122, !187}
!187 = !DILocalVariable(name: "n", arg: 1, scope: !188, file: !1, line: 25, type: !20)
!188 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_Z13PALLAS_SPEC_5iiii", scope: !1, file: !1, line: 25, type: !175, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !114)
!189 = !{!135, !190}
!190 = !DILocalVariable(name: "oldN", arg: 2, scope: !188, file: !1, line: 25, type: !20)
!191 = !{!147, !192}
!192 = !DILocalVariable(name: "res", arg: 3, scope: !188, file: !1, line: 25, type: !20)
!193 = !{!149, !194}
!194 = !DILocalVariable(name: "i", arg: 4, scope: !188, file: !1, line: 25, type: !20)
!195 = !DILocation(line: 35, column: 38, scope: !111)
!196 = !{!197, !198}
!197 = !{!"pallas.srcLoc", i64 31, i64 5, i64 33, i64 5, !117}
!198 = !{!"pallas.assert", !199, ptr @_Z13PALLAS_SPEC_6iii, !114, !114, !200}
!199 = !{!"pallas.srcLoc", i64 32, i64 5, i64 32, i64 21, !117}
!200 = !{!201, !206, !208}
!201 = !{!122, !202}
!202 = !DILocalVariable(name: "n", arg: 1, scope: !203, file: !1, line: 32, type: !20)
!203 = distinct !DISubprogram(name: "PALLAS_SPEC_6", linkageName: "_Z13PALLAS_SPEC_6iii", scope: !1, file: !1, line: 32, type: !204, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !114)
!204 = !DISubroutineType(types: !205)
!205 = !{!127, !20, !20, !20}
!206 = !{!135, !207}
!207 = !DILocalVariable(name: "oldN", arg: 2, scope: !203, file: !1, line: 32, type: !20)
!208 = !{!147, !209}
!209 = !DILocalVariable(name: "res", arg: 3, scope: !203, file: !1, line: 32, type: !20)
!210 = !DILocation(line: 35, column: 12, scope: !111)
!211 = !DILocation(line: 35, column: 9, scope: !111)
!212 = !DILocation(line: 36, column: 12, scope: !111)
!213 = !DILocation(line: 36, column: 5, scope: !111)
!214 = !DILocation(line: 37, column: 1, scope: !111)
!215 = !{!216, i1 false, i1 false, !114, !114, !218, !220}
!216 = !{!"pallas.srcLoc", i64 2, i64 1, i64 8, i64 1, !217}
!217 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr_fail.h", directory: "", checksumkind: CSK_MD5, checksum: "a014e13024b6d8c995bff72e4faa5c7f")
!218 = !{!"pallas.requires", !219, ptr @_Z13PALLAS_SPEC_2ii, !114, !114, !114}
!219 = !{!"pallas.srcLoc", i64 6, i64 1, i64 6, i64 27, !217}
!220 = !{!"pallas.ensures", !221, ptr @_Z13PALLAS_SPEC_3ii, !114, !114, !114}
!221 = !{!"pallas.srcLoc", i64 7, i64 1, i64 7, i64 28, !217}
!222 = !{!""}
!223 = !DILocation(line: 0, scope: !124)
!224 = !DILocation(line: 12, column: 12, scope: !124)
!225 = !DILocation(line: 0, scope: !133)
!226 = !DILocation(line: 13, column: 9, scope: !133)
!227 = !DILocation(line: 13, column: 24, scope: !133)
!228 = !DILocation(line: 0, scope: !188)
!229 = !DILocation(line: 25, column: 24, scope: !188)
!230 = !DILocation(line: 0, scope: !174)
!231 = !DILocation(line: 24, column: 22, scope: !174)
!232 = !DILocation(line: 24, column: 27, scope: !174)
!233 = !DILocation(line: 24, column: 37, scope: !174)
!234 = !DILocation(line: 24, column: 32, scope: !174)
!235 = !DILocation(line: 0, scope: !203)
!236 = !DILocation(line: 32, column: 17, scope: !203)
!237 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_Z13PALLAS_SPEC_2ii", scope: !102, file: !102, line: 6, type: !238, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !101, retainedNodes: !114)
!238 = !DISubroutineType(types: !239)
!239 = !{!127, !20, !20}
!240 = !DILocalVariable(name: "a", arg: 1, scope: !237, file: !102, line: 6, type: !20)
!241 = !DILocation(line: 0, scope: !237)
!242 = !DILocalVariable(name: "b", arg: 2, scope: !237, file: !102, line: 6, type: !20)
!243 = !DILocation(line: 6, column: 12, scope: !237)
!244 = !DILocation(line: 6, column: 18, scope: !237)
!245 = !DILocation(line: 6, column: 23, scope: !237)
!246 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_Z13PALLAS_SPEC_3ii", scope: !102, file: !102, line: 7, type: !238, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !101, retainedNodes: !114)
!247 = !DILocalVariable(name: "a", arg: 1, scope: !246, file: !102, line: 7, type: !20)
!248 = !DILocation(line: 0, scope: !246)
!249 = !DILocalVariable(name: "b", arg: 2, scope: !246, file: !102, line: 7, type: !20)
!250 = !DILocation(line: 7, column: 9, scope: !246)
!251 = !DILocation(line: 7, column: 24, scope: !246)
!252 = !{!"pallas.result"}
