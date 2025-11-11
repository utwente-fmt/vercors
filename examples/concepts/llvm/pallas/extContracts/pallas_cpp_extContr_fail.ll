; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@llvm.compiler.used = appending global [7 x ptr] [ptr @_Z13PALLAS_SPEC_0i, ptr @_Z13PALLAS_SPEC_1i, ptr @_Z13PALLAS_SPEC_4iiii, ptr @_Z13PALLAS_SPEC_5iiii, ptr @_Z13PALLAS_SPEC_6iiii, ptr @_Z13PALLAS_SPEC_2ii, ptr @_Z13PALLAS_SPEC_3ii], section "llvm.metadata"
@llvm.used = appending global [7 x ptr] [ptr @_Z13PALLAS_SPEC_0i, ptr @_Z13PALLAS_SPEC_1i, ptr @_Z13PALLAS_SPEC_2ii, ptr @_Z13PALLAS_SPEC_3ii, ptr @_Z13PALLAS_SPEC_4iiii, ptr @_Z13PALLAS_SPEC_5iiii, ptr @_Z13PALLAS_SPEC_6iiii], section "llvm.metadata"

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef i32 @_Z3fooi(i32 noundef %0) #0 !dbg !111 !pallas.fcontract !116 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !121, metadata !DIExpression()), !dbg !124
  call void @llvm.dbg.declare(metadata ptr %4, metadata !125, metadata !DIExpression()), !dbg !126
  %7 = load i32, ptr %3, align 4, !dbg !127
  store i32 %7, ptr %4, align 4, !dbg !126
  %8 = load i32, ptr %4, align 4, !dbg !128
  %9 = add nsw i32 %8, 1, !dbg !128
  store i32 %9, ptr %4, align 4, !dbg !128
  %10 = load i32, ptr %4, align 4, !dbg !129
  %11 = sub nsw i32 %10, 1, !dbg !129
  store i32 %11, ptr %4, align 4, !dbg !129
  %12 = load i32, ptr %3, align 4, !dbg !130
  %13 = icmp slt i32 %12, 42, !dbg !132
  br i1 %13, label %14, label %16, !dbg !133

14:                                               ; preds = %1
  %15 = load i32, ptr %3, align 4, !dbg !134
  store i32 %15, ptr %2, align 4, !dbg !136
  br label %34, !dbg !136

16:                                               ; preds = %1
  call void @llvm.dbg.declare(metadata ptr %5, metadata !137, metadata !DIExpression()), !dbg !138
  store i32 0, ptr %5, align 4, !dbg !138
  call void @llvm.dbg.declare(metadata ptr %6, metadata !139, metadata !DIExpression()), !dbg !141
  store i32 0, ptr %6, align 4, !dbg !141
  br label %17, !dbg !142

17:                                               ; preds = %25, %16
  %18 = load i32, ptr %6, align 4, !dbg !143
  %19 = load i32, ptr %3, align 4, !dbg !145
  %20 = icmp sle i32 %18, %19, !dbg !146
  br i1 %20, label %21, label %28, !dbg !147

21:                                               ; preds = %17
  %22 = load i32, ptr %6, align 4, !dbg !148
  %23 = load i32, ptr %5, align 4, !dbg !150
  %24 = add nsw i32 %23, %22, !dbg !150
  store i32 %24, ptr %5, align 4, !dbg !150
  br label %25, !dbg !151

25:                                               ; preds = %21
  %26 = load i32, ptr %6, align 4, !dbg !152
  %27 = add nsw i32 %26, 1, !dbg !152
  store i32 %27, ptr %6, align 4, !dbg !152
  br label %17, !dbg !153, !llvm.loop !154

28:                                               ; preds = %17
  %29 = load i32, ptr %5, align 4, !dbg !163, !pallas.stmntBlock !164
  %30 = call noundef i32 @_Z25anAmazingExternalFunctionii(i32 noundef %29, i32 noundef 1), !dbg !168
  %31 = load i32, ptr %5, align 4, !dbg !169
  %32 = add nsw i32 %31, %30, !dbg !169
  store i32 %32, ptr %5, align 4, !dbg !169
  %33 = load i32, ptr %5, align 4, !dbg !170
  store i32 %33, ptr %2, align 4, !dbg !171
  br label %34, !dbg !171

34:                                               ; preds = %28, %14
  %35 = load i32, ptr %2, align 4, !dbg !172
  ret i32 %35, !dbg !172
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare !pallas.extContract !173 noundef i32 @_Z25anAmazingExternalFunctionii(i32 noundef, i32 noundef) #2

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_0i(i32 noundef %0) #3 !dbg !180 !pallas.exprWrapper !184 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !185, metadata !DIExpression()), !dbg !186
  %2 = icmp sge i32 %0, 0, !dbg !187
  ret i1 %2, !dbg !186
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_1i(i32 noundef %0) #0 !dbg !188 !pallas.exprWrapper !184 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !189, metadata !DIExpression()), !dbg !190
  %2 = call noundef i32 @pallas.result.0(), !dbg !191
  %3 = icmp sge i32 %2, 0, !dbg !192
  ret i1 %3, !dbg !190
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_4iiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #3 !dbg !193 !pallas.exprWrapper !184 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !196, metadata !DIExpression()), !dbg !197
  call void @llvm.dbg.value(metadata i32 %1, metadata !198, metadata !DIExpression()), !dbg !197
  call void @llvm.dbg.value(metadata i32 %2, metadata !199, metadata !DIExpression()), !dbg !197
  call void @llvm.dbg.value(metadata i32 %3, metadata !200, metadata !DIExpression()), !dbg !197
  %5 = icmp sle i32 0, %3, !dbg !201
  br i1 %5, label %6, label %9, !dbg !202

6:                                                ; preds = %4
  %7 = add nsw i32 %0, 1, !dbg !203
  %8 = icmp sle i32 %3, %7, !dbg !204
  br label %9

9:                                                ; preds = %6, %4
  %10 = phi i1 [ false, %4 ], [ %8, %6 ], !dbg !197
  ret i1 %10, !dbg !197
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_5iiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #3 !dbg !205 !pallas.exprWrapper !184 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !206, metadata !DIExpression()), !dbg !207
  call void @llvm.dbg.value(metadata i32 %1, metadata !208, metadata !DIExpression()), !dbg !207
  call void @llvm.dbg.value(metadata i32 %2, metadata !209, metadata !DIExpression()), !dbg !207
  call void @llvm.dbg.value(metadata i32 %3, metadata !210, metadata !DIExpression()), !dbg !207
  %5 = icmp sge i32 %2, 0, !dbg !211
  ret i1 %5, !dbg !207
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_6iiii(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #3 !dbg !212 !pallas.exprWrapper !184 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !213, metadata !DIExpression()), !dbg !214
  call void @llvm.dbg.value(metadata i32 %1, metadata !215, metadata !DIExpression()), !dbg !214
  call void @llvm.dbg.value(metadata i32 %2, metadata !216, metadata !DIExpression()), !dbg !214
  call void @llvm.dbg.value(metadata i32 %3, metadata !217, metadata !DIExpression()), !dbg !214
  %5 = icmp eq i32 %1, %0, !dbg !218
  ret i1 %5, !dbg !214
}

; Function Attrs: mustprogress noinline nounwind uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_2ii(i32 noundef %0, i32 noundef %1) #3 !dbg !219 !pallas.exprWrapper !184 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !222, metadata !DIExpression()), !dbg !223
  call void @llvm.dbg.value(metadata i32 %1, metadata !224, metadata !DIExpression()), !dbg !223
  %3 = icmp sge i32 %0, 42, !dbg !225
  br i1 %3, label %4, label %6, !dbg !226

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !227
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !223
  ret i1 %7, !dbg !223
}

; Function Attrs: mustprogress noinline uwtable
define dso_local noundef zeroext i1 @_Z13PALLAS_SPEC_3ii(i32 noundef %0, i32 noundef %1) #0 !dbg !228 !pallas.exprWrapper !184 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !229, metadata !DIExpression()), !dbg !230
  call void @llvm.dbg.value(metadata i32 %1, metadata !231, metadata !DIExpression()), !dbg !230
  %3 = call noundef i32 @pallas.result.0(), !dbg !232
  %4 = icmp sge i32 %3, 0, !dbg !233
  ret i1 %4, !dbg !230
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !234 noundef i32 @pallas.result.0()

attributes #0 = { mustprogress noinline uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!0, !2, !101}
!llvm.module.flags = !{!103, !104, !105, !106, !107, !108, !109}
!llvm.ident = !{!110, !110}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr.cpp", directory: "/home/rme/repos/vercors/examples/concepts/llvm/pallas", checksumkind: CSK_MD5, checksum: "3ce0804a8eb5623727f4076ae2ae8562")
!2 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !3, producer: "Ubuntu clang version 17.0.6 (9ubuntu1)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, imports: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.cpp", directory: "/home/rme/repos/vercors/examples/concepts/llvm/pallas", checksumkind: CSK_MD5, checksum: "32cacd5a61438d60254cb08500812dbe")
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
!111 = distinct !DISubprogram(name: "foo", linkageName: "_Z3fooi", scope: !112, file: !112, line: 15, type: !113, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !115)
!112 = !DIFile(filename: "extContracts/pallas_cpp_extContr.cpp", directory: "/home/rme/repos/vercors/examples/concepts/llvm/pallas", checksumkind: CSK_MD5, checksum: "3ce0804a8eb5623727f4076ae2ae8562")
!113 = !DISubroutineType(types: !114)
!114 = !{!20, !20}
!115 = !{}
!116 = !{!117, i1 false, i1 false, !119, !122}
!117 = !{!"pallas.srcLoc", i64 11, i64 1, i64 14, i64 1, !118}
!118 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr.cpp", directory: "", checksumkind: CSK_MD5, checksum: "3ce0804a8eb5623727f4076ae2ae8562")
!119 = !{!"pallas.requires", !120, ptr @_Z13PALLAS_SPEC_0i, !121}
!120 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 16, !118}
!121 = !DILocalVariable(name: "n", arg: 1, scope: !111, file: !112, line: 15, type: !20)
!122 = !{!"pallas.ensures", !123, ptr @_Z13PALLAS_SPEC_1i, !121}
!123 = !{!"pallas.srcLoc", i64 13, i64 1, i64 13, i64 28, !118}
!124 = !DILocation(line: 15, column: 13, scope: !111)
!125 = !DILocalVariable(name: "oldN", scope: !111, file: !112, line: 16, type: !20)
!126 = !DILocation(line: 16, column: 9, scope: !111)
!127 = !DILocation(line: 16, column: 16, scope: !111)
!128 = !DILocation(line: 17, column: 10, scope: !111)
!129 = !DILocation(line: 18, column: 10, scope: !111)
!130 = !DILocation(line: 19, column: 9, scope: !131)
!131 = distinct !DILexicalBlock(scope: !111, file: !112, line: 19, column: 9)
!132 = !DILocation(line: 19, column: 11, scope: !131)
!133 = !DILocation(line: 19, column: 9, scope: !111)
!134 = !DILocation(line: 20, column: 16, scope: !135)
!135 = distinct !DILexicalBlock(scope: !131, file: !112, line: 19, column: 17)
!136 = !DILocation(line: 20, column: 9, scope: !135)
!137 = !DILocalVariable(name: "res", scope: !111, file: !112, line: 22, type: !20)
!138 = !DILocation(line: 22, column: 9, scope: !111)
!139 = !DILocalVariable(name: "i", scope: !140, file: !112, line: 27, type: !20)
!140 = distinct !DILexicalBlock(scope: !111, file: !112, line: 27, column: 5)
!141 = !DILocation(line: 27, column: 14, scope: !140)
!142 = !DILocation(line: 27, column: 10, scope: !140)
!143 = !DILocation(line: 27, column: 21, scope: !144)
!144 = distinct !DILexicalBlock(scope: !140, file: !112, line: 27, column: 5)
!145 = !DILocation(line: 27, column: 26, scope: !144)
!146 = !DILocation(line: 27, column: 23, scope: !144)
!147 = !DILocation(line: 27, column: 5, scope: !140)
!148 = !DILocation(line: 28, column: 16, scope: !149)
!149 = distinct !DILexicalBlock(scope: !144, file: !112, line: 27, column: 34)
!150 = !DILocation(line: 28, column: 13, scope: !149)
!151 = !DILocation(line: 29, column: 5, scope: !149)
!152 = !DILocation(line: 27, column: 30, scope: !144)
!153 = !DILocation(line: 27, column: 5, scope: !144)
!154 = distinct !{!154, !147, !155, !156, !157}
!155 = !DILocation(line: 29, column: 5, scope: !140)
!156 = !{!"llvm.loop.mustprogress"}
!157 = !{!"pallas.loopInv", !158, !159, !161}
!158 = !{!"pallas.srcLoc", i64 23, i64 5, i64 26, i64 5, !118}
!159 = !{!160, ptr @_Z13PALLAS_SPEC_4iiii, !121, !125, !137, !139}
!160 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 40, !118}
!161 = !{!162, ptr @_Z13PALLAS_SPEC_5iiii, !121, !125, !137, !139}
!162 = !{!"pallas.srcLoc", i64 25, i64 5, i64 25, i64 29, !118}
!163 = !DILocation(line: 35, column: 38, scope: !111)
!164 = !{!165, !166}
!165 = !{!"pallas.srcLoc", i64 31, i64 5, i64 33, i64 5, !118}
!166 = !{!"pallas.assert", !167, ptr @_Z13PALLAS_SPEC_6iiii, !121, !125, !137, !139}
!167 = !{!"pallas.srcLoc", i64 32, i64 5, i64 32, i64 21, !118}
!168 = !DILocation(line: 35, column: 12, scope: !111)
!169 = !DILocation(line: 35, column: 9, scope: !111)
!170 = !DILocation(line: 36, column: 12, scope: !111)
!171 = !DILocation(line: 36, column: 5, scope: !111)
!172 = !DILocation(line: 37, column: 1, scope: !111)
!173 = !{!174, i1 false, i1 false, !176, !178}
!174 = !{!"pallas.srcLoc", i64 2, i64 1, i64 8, i64 1, !175}
!175 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr_fail.h", directory: "", checksumkind: CSK_MD5, checksum: "a014e13024b6d8c995bff72e4faa5c7f")
!176 = !{!"pallas.requires", !177, ptr @_Z13PALLAS_SPEC_2ii}
!177 = !{!"pallas.srcLoc", i64 6, i64 1, i64 6, i64 27, !175}
!178 = !{!"pallas.ensures", !179, ptr @_Z13PALLAS_SPEC_3ii}
!179 = !{!"pallas.srcLoc", i64 7, i64 1, i64 7, i64 28, !175}
!180 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "_Z13PALLAS_SPEC_0i", scope: !112, file: !112, line: 12, type: !181, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !115)
!181 = !DISubroutineType(types: !182)
!182 = !{!183, !20}
!183 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!184 = !{!""}
!185 = !DILocalVariable(name: "n", arg: 1, scope: !180, file: !112, line: 12, type: !20)
!186 = !DILocation(line: 0, scope: !180)
!187 = !DILocation(line: 12, column: 12, scope: !180)
!188 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "_Z13PALLAS_SPEC_1i", scope: !112, file: !112, line: 13, type: !181, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !115)
!189 = !DILocalVariable(name: "n", arg: 1, scope: !188, file: !112, line: 13, type: !20)
!190 = !DILocation(line: 0, scope: !188)
!191 = !DILocation(line: 13, column: 9, scope: !188)
!192 = !DILocation(line: 13, column: 24, scope: !188)
!193 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "_Z13PALLAS_SPEC_4iiii", scope: !112, file: !112, line: 24, type: !194, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !115)
!194 = !DISubroutineType(types: !195)
!195 = !{!183, !20, !20, !20, !20}
!196 = !DILocalVariable(name: "n", arg: 1, scope: !193, file: !112, line: 24, type: !20)
!197 = !DILocation(line: 0, scope: !193)
!198 = !DILocalVariable(name: "oldN", arg: 2, scope: !193, file: !112, line: 24, type: !20)
!199 = !DILocalVariable(name: "res", arg: 3, scope: !193, file: !112, line: 24, type: !20)
!200 = !DILocalVariable(name: "i", arg: 4, scope: !193, file: !112, line: 24, type: !20)
!201 = !DILocation(line: 24, column: 22, scope: !193)
!202 = !DILocation(line: 24, column: 27, scope: !193)
!203 = !DILocation(line: 24, column: 37, scope: !193)
!204 = !DILocation(line: 24, column: 32, scope: !193)
!205 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "_Z13PALLAS_SPEC_5iiii", scope: !112, file: !112, line: 25, type: !194, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !115)
!206 = !DILocalVariable(name: "n", arg: 1, scope: !205, file: !112, line: 25, type: !20)
!207 = !DILocation(line: 0, scope: !205)
!208 = !DILocalVariable(name: "oldN", arg: 2, scope: !205, file: !112, line: 25, type: !20)
!209 = !DILocalVariable(name: "res", arg: 3, scope: !205, file: !112, line: 25, type: !20)
!210 = !DILocalVariable(name: "i", arg: 4, scope: !205, file: !112, line: 25, type: !20)
!211 = !DILocation(line: 25, column: 24, scope: !205)
!212 = distinct !DISubprogram(name: "PALLAS_SPEC_6", linkageName: "_Z13PALLAS_SPEC_6iiii", scope: !112, file: !112, line: 32, type: !194, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !115)
!213 = !DILocalVariable(name: "n", arg: 1, scope: !212, file: !112, line: 32, type: !20)
!214 = !DILocation(line: 0, scope: !212)
!215 = !DILocalVariable(name: "oldN", arg: 2, scope: !212, file: !112, line: 32, type: !20)
!216 = !DILocalVariable(name: "res", arg: 3, scope: !212, file: !112, line: 32, type: !20)
!217 = !DILocalVariable(name: "i", arg: 4, scope: !212, file: !112, line: 32, type: !20)
!218 = !DILocation(line: 32, column: 17, scope: !212)
!219 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "_Z13PALLAS_SPEC_2ii", scope: !102, file: !102, line: 6, type: !220, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !101, retainedNodes: !115)
!220 = !DISubroutineType(types: !221)
!221 = !{!183, !20, !20}
!222 = !DILocalVariable(name: "a", arg: 1, scope: !219, file: !102, line: 6, type: !20)
!223 = !DILocation(line: 0, scope: !219)
!224 = !DILocalVariable(name: "b", arg: 2, scope: !219, file: !102, line: 6, type: !20)
!225 = !DILocation(line: 6, column: 12, scope: !219)
!226 = !DILocation(line: 6, column: 18, scope: !219)
!227 = !DILocation(line: 6, column: 23, scope: !219)
!228 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "_Z13PALLAS_SPEC_3ii", scope: !102, file: !102, line: 7, type: !220, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !101, retainedNodes: !115)
!229 = !DILocalVariable(name: "a", arg: 1, scope: !228, file: !102, line: 7, type: !20)
!230 = !DILocation(line: 0, scope: !228)
!231 = !DILocalVariable(name: "b", arg: 2, scope: !228, file: !102, line: 7, type: !20)
!232 = !DILocation(line: 7, column: 9, scope: !228)
!233 = !DILocation(line: 7, column: 24, scope: !228)
!234 = !{!"pallas.result"}
