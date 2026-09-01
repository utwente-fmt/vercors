; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_linked_list.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.List_t = type { i32, ptr }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [14 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @list_write], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local ptr @malloc_list() #0 !dbg !33 !pallas.extContract !36 {
  %1 = call noalias ptr @malloc(i64 noundef 16) #3, !dbg !46
  ret ptr %1, !dbg !47
}

; Function Attrs: nounwind allocsize(0)
declare noalias ptr @malloc(i64 noundef) #1

; Function Attrs: noinline nounwind uwtable
define dso_local ptr @prepend(i32 noundef %0, ptr noundef %1) #0 !dbg !48 !pallas.fcontract !51 {
  %3 = alloca i32, align 4
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !57, metadata !DIExpression()), !dbg !82
  store ptr %1, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !64, metadata !DIExpression()), !dbg !83
  call void @llvm.dbg.declare(metadata ptr %5, metadata !84, metadata !DIExpression()), !dbg !85
  %6 = call ptr @malloc_list(), !dbg !86
  store ptr %6, ptr %5, align 8, !dbg !85
  %7 = load i32, ptr %3, align 4, !dbg !87
  %8 = load ptr, ptr %5, align 8, !dbg !88
  %9 = getelementptr inbounds %struct.List_t, ptr %8, i32 0, i32 0, !dbg !89
  store i32 %7, ptr %9, align 8, !dbg !90
  %10 = load ptr, ptr %4, align 8, !dbg !91
  %11 = load ptr, ptr %5, align 8, !dbg !92
  %12 = getelementptr inbounds %struct.List_t, ptr %11, i32 0, i32 1, !dbg !93
  store ptr %10, ptr %12, align 8, !dbg !94
  %13 = load ptr, ptr %5, align 8, !dbg !95, !pallas.stmntBlock !96
  ret ptr %13, !dbg !110
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #2

; Function Attrs: noinline nounwind uwtable
define dso_local ptr @append(ptr noundef %0, i32 noundef %1) #0 !dbg !111 !pallas.fcontract !114 {
  %3 = alloca ptr, align 8
  %4 = alloca ptr, align 8
  %5 = alloca i32, align 4
  %6 = alloca ptr, align 8
  store ptr %0, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !120, metadata !DIExpression()), !dbg !144
  store i32 %1, ptr %5, align 4
  call void @llvm.dbg.declare(metadata ptr %5, metadata !126, metadata !DIExpression()), !dbg !145
  %7 = load ptr, ptr %4, align 8, !dbg !146
  %8 = icmp eq ptr %7, null, !dbg !148
  br i1 %8, label %9, label %15, !dbg !149

9:                                                ; preds = %2
  call void @llvm.dbg.declare(metadata ptr %6, metadata !150, metadata !DIExpression()), !dbg !152
  %10 = call ptr @malloc_list(), !dbg !153
  store ptr %10, ptr %6, align 8, !dbg !152
  %11 = load i32, ptr %5, align 4, !dbg !154
  %12 = load ptr, ptr %6, align 8, !dbg !155
  %13 = getelementptr inbounds %struct.List_t, ptr %12, i32 0, i32 0, !dbg !156
  store i32 %11, ptr %13, align 8, !dbg !157
  %14 = load ptr, ptr %6, align 8, !dbg !158, !pallas.stmntBlock !159
  store ptr %14, ptr %3, align 8, !dbg !173
  br label %24, !dbg !173

15:                                               ; preds = %2
  %16 = load ptr, ptr %4, align 8, !dbg !174, !pallas.stmntBlock !175
  %17 = getelementptr inbounds %struct.List_t, ptr %16, i32 0, i32 1, !dbg !185
  %18 = load ptr, ptr %17, align 8, !dbg !185
  %19 = load i32, ptr %5, align 4, !dbg !186
  %20 = call ptr @append(ptr noundef %18, i32 noundef %19), !dbg !187
  %21 = load ptr, ptr %4, align 8, !dbg !188
  %22 = getelementptr inbounds %struct.List_t, ptr %21, i32 0, i32 1, !dbg !189
  store ptr %20, ptr %22, align 8, !dbg !190
  %23 = load ptr, ptr %4, align 8, !dbg !191, !pallas.stmntBlock !192
  store ptr %23, ptr %3, align 8, !dbg !202
  br label %24, !dbg !202

24:                                               ; preds = %15, %9
  %25 = load ptr, ptr %3, align 8, !dbg !203
  ret ptr %25, !dbg !203
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0() #0 !dbg !204 !pallas.exprWrapper !207 {
  %1 = call ptr @"pallas.result ptr"(), !dbg !208
  %2 = icmp ne ptr %1, null, !dbg !209
  ret i1 %2, !dbg !210
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1() #0 !dbg !211 !pallas.exprWrapper !207 {
  %1 = alloca %pallas.fracT, align 8
  %2 = call ptr @"pallas.result ptr"(), !dbg !212
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %1, i32 noundef 1, i32 noundef 1), !dbg !213
  %3 = call i1 @pallas.perm(ptr noundef %2, ptr noundef byval(%pallas.fracT) %1), !dbg !214
  ret i1 %3, !dbg !215
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2() #0 !dbg !216 !pallas.exprWrapper !207 {
  %1 = call ptr @"pallas.result ptr"(), !dbg !217
  %2 = getelementptr inbounds %struct.List_t, ptr %1, i32 0, i32 1, !dbg !218
  %3 = load ptr, ptr %2, align 8, !dbg !218
  %4 = icmp eq ptr %3, null, !dbg !219
  ret i1 %4, !dbg !220
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, ptr noundef %1) #0 !dbg !59 !pallas.exprWrapper !207 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !58, metadata !DIExpression()), !dbg !221
  call void @llvm.dbg.value(metadata ptr %1, metadata !65, metadata !DIExpression()), !dbg !221
  %3 = icmp ne ptr %1, null, !dbg !222
  %4 = call zeroext i1 @list_write(ptr noundef %1), !dbg !223
  %5 = call i1 @pallas.imply(i1 %3, i1 %4), !dbg !224
  ret i1 %5, !dbg !221
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, ptr noundef %1) #0 !dbg !71 !pallas.exprWrapper !207 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !70, metadata !DIExpression()), !dbg !225
  call void @llvm.dbg.value(metadata ptr %1, metadata !73, metadata !DIExpression()), !dbg !225
  %3 = call ptr @"pallas.result ptr"(), !dbg !226
  %4 = icmp ne ptr %3, null, !dbg !227
  ret i1 %4, !dbg !225
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, ptr noundef %1) #0 !dbg !79 !pallas.exprWrapper !207 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !78, metadata !DIExpression()), !dbg !228
  call void @llvm.dbg.value(metadata ptr %1, metadata !81, metadata !DIExpression()), !dbg !228
  %3 = call ptr @"pallas.result ptr"(), !dbg !229
  %4 = call zeroext i1 @list_write(ptr noundef %3), !dbg !230
  ret i1 %4, !dbg !228
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1) #0 !dbg !122 !pallas.exprWrapper !207 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !121, metadata !DIExpression()), !dbg !231
  call void @llvm.dbg.value(metadata i32 %1, metadata !127, metadata !DIExpression()), !dbg !231
  %3 = icmp ne ptr %0, null, !dbg !232
  %4 = call zeroext i1 @list_write(ptr noundef %0), !dbg !233
  %5 = call i1 @pallas.imply(i1 %3, i1 %4), !dbg !234
  ret i1 %5, !dbg !231
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1) #0 !dbg !133 !pallas.exprWrapper !207 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !132, metadata !DIExpression()), !dbg !235
  call void @llvm.dbg.value(metadata i32 %1, metadata !135, metadata !DIExpression()), !dbg !235
  %3 = call ptr @"pallas.result ptr"(), !dbg !236
  %4 = icmp ne ptr %3, null, !dbg !237
  ret i1 %4, !dbg !235
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0, i32 noundef %1) #0 !dbg !141 !pallas.exprWrapper !207 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !140, metadata !DIExpression()), !dbg !238
  call void @llvm.dbg.value(metadata i32 %1, metadata !143, metadata !DIExpression()), !dbg !238
  %3 = call ptr @"pallas.result ptr"(), !dbg !239
  %4 = call zeroext i1 @list_write(ptr noundef %3), !dbg !240
  ret i1 %4, !dbg !238
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(i32 noundef %0, ptr noundef %1, ptr noundef %2) #0 !dbg !103 !pallas.exprWrapper !207 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !102, metadata !DIExpression()), !dbg !241
  call void @llvm.dbg.value(metadata ptr %1, metadata !107, metadata !DIExpression()), !dbg !241
  call void @llvm.dbg.value(metadata ptr %2, metadata !109, metadata !DIExpression()), !dbg !241
  %4 = call zeroext i1 @list_write(ptr noundef %2), !dbg !242
  ret i1 %4, !dbg !241
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0, i32 noundef %1, ptr noundef %2) #0 !dbg !166 !pallas.exprWrapper !207 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !165, metadata !DIExpression()), !dbg !243
  call void @llvm.dbg.value(metadata i32 %1, metadata !170, metadata !DIExpression()), !dbg !243
  call void @llvm.dbg.value(metadata ptr %2, metadata !172, metadata !DIExpression()), !dbg !243
  %4 = call zeroext i1 @list_write(ptr noundef %2), !dbg !244
  ret i1 %4, !dbg !243
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0, i32 noundef %1) #0 !dbg !182 !pallas.exprWrapper !207 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !181, metadata !DIExpression()), !dbg !245
  call void @llvm.dbg.value(metadata i32 %1, metadata !184, metadata !DIExpression()), !dbg !245
  %3 = call zeroext i1 @list_write(ptr noundef %0), !dbg !246
  ret i1 %3, !dbg !245
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0, i32 noundef %1) #0 !dbg !199 !pallas.exprWrapper !207 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !198, metadata !DIExpression()), !dbg !247
  call void @llvm.dbg.value(metadata i32 %1, metadata !201, metadata !DIExpression()), !dbg !247
  %3 = call zeroext i1 @list_write(ptr noundef %0), !dbg !248
  ret i1 %3, !dbg !247
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @list_write(ptr noundef %0) #0 !dbg !249 !pallas.predDef !252 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !253, metadata !DIExpression()), !dbg !254
  %3 = icmp ne ptr %0, null, !dbg !255
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !256
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !257
  %5 = getelementptr inbounds %struct.List_t, ptr %0, i32 0, i32 1, !dbg !258
  %6 = load ptr, ptr %5, align 8, !dbg !258
  %7 = icmp ne ptr %6, null, !dbg !259
  %8 = getelementptr inbounds %struct.List_t, ptr %0, i32 0, i32 1, !dbg !260
  %9 = load ptr, ptr %8, align 8, !dbg !260
  %10 = call zeroext i1 @list_write(ptr noundef %9), !dbg !261
  %11 = call i1 @pallas.imply(i1 %7, i1 %10), !dbg !262
  %12 = call i1 @pallas.sepConj(i1 %4, i1 %11), !dbg !263
  %13 = call i1 @pallas.sepConj(i1 %3, i1 %12), !dbg !264
  ret i1 %13, !dbg !254
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #2

declare !pallas.specLib !265 ptr @"pallas.result ptr"()

declare !pallas.specLib !266 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !267 i1 @pallas.imply(i1, i1)

declare !pallas.specLib !268 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !269 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nounwind allocsize(0) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #3 = { nounwind allocsize(0) }

!llvm.dbg.cu = !{!0, !12, !22, !24}
!llvm.module.flags = !{!25, !26, !27, !28, !29, !30, !31}
!llvm.ident = !{!32, !32}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_linked_list.c", directory: ".", checksumkind: CSK_MD5, checksum: "a484c137b0a0dc7248f11e709e3d4b62")
!2 = !{!3, !11}
!3 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!4 = !DIDerivedType(tag: DW_TAG_typedef, name: "List", file: !1, line: 7, baseType: !5)
!5 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "List_t", file: !1, line: 4, size: 128, elements: !6)
!6 = !{!7, !9}
!7 = !DIDerivedType(tag: DW_TAG_member, name: "v", scope: !5, file: !1, line: 5, baseType: !8, size: 32)
!8 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!9 = !DIDerivedType(tag: DW_TAG_member, name: "next", scope: !5, file: !1, line: 6, baseType: !10, size: 64, offset: 64)
!10 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !5, size: 64)
!11 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!12 = distinct !DICompileUnit(language: DW_LANG_C11, file: !13, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !14, splitDebugInlining: false, nameTableKind: None)
!13 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "2acd20872721d46b3833230b6394e74a")
!14 = !{!11, !15}
!15 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !16, size: 64)
!16 = !DIDerivedType(tag: DW_TAG_typedef, name: "List", file: !13, line: 8, baseType: !17)
!17 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "List_t", file: !13, line: 5, size: 128, elements: !18)
!18 = !{!19, !20}
!19 = !DIDerivedType(tag: DW_TAG_member, name: "v", scope: !17, file: !13, line: 6, baseType: !8, size: 32)
!20 = !DIDerivedType(tag: DW_TAG_member, name: "next", scope: !17, file: !13, line: 7, baseType: !21, size: 64, offset: 64)
!21 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !17, size: 64)
!22 = distinct !DICompileUnit(language: DW_LANG_C, file: !23, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!23 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_linked_list.c", directory: "")
!24 = distinct !DICompileUnit(language: DW_LANG_C, file: !23, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!25 = !{i32 7, !"Dwarf Version", i32 5}
!26 = !{i32 2, !"Debug Info Version", i32 3}
!27 = !{i32 1, !"wchar_size", i32 4}
!28 = !{i32 8, !"PIC Level", i32 2}
!29 = !{i32 7, !"PIE Level", i32 2}
!30 = !{i32 7, !"uwtable", i32 2}
!31 = !{i32 7, !"frame-pointer", i32 2}
!32 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!33 = distinct !DISubprogram(name: "malloc_list", scope: !1, file: !1, line: 22, type: !34, scopeLine: 22, spFlags: DISPFlagDefinition, unit: !0)
!34 = !DISubroutineType(types: !35)
!35 = !{!3}
!36 = !{!37, i1 false, i1 true, !39, !39, !40, !42, !44}
!37 = !{!"pallas.srcLoc", i64 14, i64 1, i64 21, i64 1, !38}
!38 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_linked_list.c", directory: "", checksumkind: CSK_MD5, checksum: "a484c137b0a0dc7248f11e709e3d4b62")
!39 = !{}
!40 = !{!"pallas.ensures", !41, ptr @PALLAS_SPEC_0, !39, !39, !39}
!41 = !{!"pallas.srcLoc", i64 18, i64 1, i64 18, i64 30, !38}
!42 = !{!"pallas.ensures", !43, ptr @PALLAS_SPEC_1, !39, !39, !39}
!43 = !{!"pallas.srcLoc", i64 19, i64 1, i64 19, i64 37, !38}
!44 = !{!"pallas.ensures", !45, ptr @PALLAS_SPEC_2, !39, !39, !39}
!45 = !{!"pallas.srcLoc", i64 20, i64 1, i64 20, i64 36, !38}
!46 = !DILocation(line: 23, column: 21, scope: !33)
!47 = !DILocation(line: 23, column: 5, scope: !33)
!48 = distinct !DISubprogram(name: "prepend", scope: !1, file: !1, line: 39, type: !49, scopeLine: 39, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!49 = !DISubroutineType(types: !50)
!50 = !{!3, !8, !3}
!51 = !{!52, i1 false, i1 false, !39, !39, !53, !66, !74}
!52 = !{!"pallas.srcLoc", i64 34, i64 1, i64 38, i64 1, !38}
!53 = !{!"pallas.requires", !54, ptr @PALLAS_SPEC_3, !39, !39, !55}
!54 = !{!"pallas.srcLoc", i64 35, i64 1, i64 35, i64 49, !38}
!55 = !{!56, !63}
!56 = !{!57, !58}
!57 = !DILocalVariable(name: "elem", arg: 1, scope: !48, file: !1, line: 39, type: !8)
!58 = !DILocalVariable(name: "elem", arg: 1, scope: !59, file: !1, line: 35, type: !8)
!59 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 35, type: !60, scopeLine: 35, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!60 = !DISubroutineType(types: !61)
!61 = !{!62, !8, !15}
!62 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!63 = !{!64, !65}
!64 = !DILocalVariable(name: "list", arg: 2, scope: !48, file: !1, line: 39, type: !3)
!65 = !DILocalVariable(name: "list", arg: 2, scope: !59, file: !1, line: 35, type: !15)
!66 = !{!"pallas.ensures", !67, ptr @PALLAS_SPEC_4, !39, !39, !68}
!67 = !{!"pallas.srcLoc", i64 36, i64 1, i64 36, i64 31, !38}
!68 = !{!69, !72}
!69 = !{!57, !70}
!70 = !DILocalVariable(name: "elem", arg: 1, scope: !71, file: !1, line: 36, type: !8)
!71 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 36, type: !60, scopeLine: 36, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!72 = !{!64, !73}
!73 = !DILocalVariable(name: "list", arg: 2, scope: !71, file: !1, line: 36, type: !15)
!74 = !{!"pallas.ensures", !75, ptr @PALLAS_SPEC_5, !39, !39, !76}
!75 = !{!"pallas.srcLoc", i64 37, i64 1, i64 37, i64 35, !38}
!76 = !{!77, !80}
!77 = !{!57, !78}
!78 = !DILocalVariable(name: "elem", arg: 1, scope: !79, file: !1, line: 37, type: !8)
!79 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 37, type: !60, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!80 = !{!64, !81}
!81 = !DILocalVariable(name: "list", arg: 2, scope: !79, file: !1, line: 37, type: !15)
!82 = !DILocation(line: 39, column: 19, scope: !48)
!83 = !DILocation(line: 39, column: 31, scope: !48)
!84 = !DILocalVariable(name: "new_head", scope: !48, file: !1, line: 40, type: !3)
!85 = !DILocation(line: 40, column: 11, scope: !48)
!86 = !DILocation(line: 40, column: 22, scope: !48)
!87 = !DILocation(line: 41, column: 19, scope: !48)
!88 = !DILocation(line: 41, column: 5, scope: !48)
!89 = !DILocation(line: 41, column: 15, scope: !48)
!90 = !DILocation(line: 41, column: 17, scope: !48)
!91 = !DILocation(line: 42, column: 22, scope: !48)
!92 = !DILocation(line: 42, column: 5, scope: !48)
!93 = !DILocation(line: 42, column: 15, scope: !48)
!94 = !DILocation(line: 42, column: 20, scope: !48)
!95 = !DILocation(line: 46, column: 12, scope: !48)
!96 = !{!97, !98}
!97 = !{!"pallas.srcLoc", i64 43, i64 5, i64 45, i64 5, !38}
!98 = !{!"pallas.fold", !99, ptr @PALLAS_SPEC_9, !39, !39, !100}
!99 = !{!"pallas.srcLoc", i64 44, i64 5, i64 44, i64 30, !38}
!100 = !{!101, !106, !108}
!101 = !{!57, !102}
!102 = !DILocalVariable(name: "elem", arg: 1, scope: !103, file: !1, line: 44, type: !8)
!103 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !1, file: !1, line: 44, type: !104, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!104 = !DISubroutineType(types: !105)
!105 = !{!62, !8, !15, !15}
!106 = !{!64, !107}
!107 = !DILocalVariable(name: "list", arg: 2, scope: !103, file: !1, line: 44, type: !15)
!108 = !{!84, !109}
!109 = !DILocalVariable(name: "new_head", arg: 3, scope: !103, file: !1, line: 44, type: !15)
!110 = !DILocation(line: 46, column: 5, scope: !48)
!111 = distinct !DISubprogram(name: "append", scope: !1, file: !1, line: 54, type: !112, scopeLine: 54, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!112 = !DISubroutineType(types: !113)
!113 = !{!3, !3, !8}
!114 = !{!115, i1 false, i1 false, !39, !39, !116, !128, !136}
!115 = !{!"pallas.srcLoc", i64 49, i64 1, i64 53, i64 1, !38}
!116 = !{!"pallas.requires", !117, ptr @PALLAS_SPEC_6, !39, !39, !118}
!117 = !{!"pallas.srcLoc", i64 50, i64 1, i64 50, i64 43, !38}
!118 = !{!119, !125}
!119 = !{!120, !121}
!120 = !DILocalVariable(name: "l", arg: 1, scope: !111, file: !1, line: 54, type: !3)
!121 = !DILocalVariable(name: "l", arg: 1, scope: !122, file: !1, line: 50, type: !15)
!122 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 50, type: !123, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!123 = !DISubroutineType(types: !124)
!124 = !{!62, !15, !8}
!125 = !{!126, !127}
!126 = !DILocalVariable(name: "elem", arg: 2, scope: !111, file: !1, line: 54, type: !8)
!127 = !DILocalVariable(name: "elem", arg: 2, scope: !122, file: !1, line: 50, type: !8)
!128 = !{!"pallas.ensures", !129, ptr @PALLAS_SPEC_7, !39, !39, !130}
!129 = !{!"pallas.srcLoc", i64 51, i64 1, i64 51, i64 31, !38}
!130 = !{!131, !134}
!131 = !{!120, !132}
!132 = !DILocalVariable(name: "l", arg: 1, scope: !133, file: !1, line: 51, type: !15)
!133 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !1, file: !1, line: 51, type: !123, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!134 = !{!126, !135}
!135 = !DILocalVariable(name: "elem", arg: 2, scope: !133, file: !1, line: 51, type: !8)
!136 = !{!"pallas.ensures", !137, ptr @PALLAS_SPEC_8, !39, !39, !138}
!137 = !{!"pallas.srcLoc", i64 52, i64 1, i64 52, i64 35, !38}
!138 = !{!139, !142}
!139 = !{!120, !140}
!140 = !DILocalVariable(name: "l", arg: 1, scope: !141, file: !1, line: 52, type: !15)
!141 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !1, file: !1, line: 52, type: !123, scopeLine: 52, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!142 = !{!126, !143}
!143 = !DILocalVariable(name: "elem", arg: 2, scope: !141, file: !1, line: 52, type: !8)
!144 = !DILocation(line: 54, column: 20, scope: !111)
!145 = !DILocation(line: 54, column: 27, scope: !111)
!146 = !DILocation(line: 55, column: 9, scope: !147)
!147 = distinct !DILexicalBlock(scope: !111, file: !1, line: 55, column: 9)
!148 = !DILocation(line: 55, column: 11, scope: !147)
!149 = !DILocation(line: 55, column: 9, scope: !111)
!150 = !DILocalVariable(name: "new_node", scope: !151, file: !1, line: 56, type: !3)
!151 = distinct !DILexicalBlock(scope: !147, file: !1, line: 55, column: 20)
!152 = !DILocation(line: 56, column: 15, scope: !151)
!153 = !DILocation(line: 56, column: 26, scope: !151)
!154 = !DILocation(line: 57, column: 23, scope: !151)
!155 = !DILocation(line: 57, column: 9, scope: !151)
!156 = !DILocation(line: 57, column: 19, scope: !151)
!157 = !DILocation(line: 57, column: 21, scope: !151)
!158 = !DILocation(line: 61, column: 16, scope: !151)
!159 = !{!160, !161}
!160 = !{!"pallas.srcLoc", i64 58, i64 9, i64 60, i64 9, !38}
!161 = !{!"pallas.fold", !162, ptr @PALLAS_SPEC_10, !39, !39, !163}
!162 = !{!"pallas.srcLoc", i64 59, i64 9, i64 59, i64 34, !38}
!163 = !{!164, !169, !171}
!164 = !{!120, !165}
!165 = !DILocalVariable(name: "l", arg: 1, scope: !166, file: !1, line: 59, type: !15)
!166 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !1, file: !1, line: 59, type: !167, scopeLine: 59, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!167 = !DISubroutineType(types: !168)
!168 = !{!62, !15, !8, !15}
!169 = !{!126, !170}
!170 = !DILocalVariable(name: "elem", arg: 2, scope: !166, file: !1, line: 59, type: !8)
!171 = !{!150, !172}
!172 = !DILocalVariable(name: "new_node", arg: 3, scope: !166, file: !1, line: 59, type: !15)
!173 = !DILocation(line: 61, column: 9, scope: !151)
!174 = !DILocation(line: 67, column: 22, scope: !111)
!175 = !{!176, !177}
!176 = !{!"pallas.srcLoc", i64 64, i64 5, i64 66, i64 5, !38}
!177 = !{!"pallas.unfold", !178, ptr @PALLAS_SPEC_11, !39, !39, !179}
!178 = !{!"pallas.srcLoc", i64 65, i64 5, i64 65, i64 25, !38}
!179 = !{!180, !183}
!180 = !{!120, !181}
!181 = !DILocalVariable(name: "l", arg: 1, scope: !182, file: !1, line: 65, type: !15)
!182 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 65, type: !123, scopeLine: 65, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!183 = !{!126, !184}
!184 = !DILocalVariable(name: "elem", arg: 2, scope: !182, file: !1, line: 65, type: !8)
!185 = !DILocation(line: 67, column: 25, scope: !111)
!186 = !DILocation(line: 67, column: 31, scope: !111)
!187 = !DILocation(line: 67, column: 15, scope: !111)
!188 = !DILocation(line: 67, column: 5, scope: !111)
!189 = !DILocation(line: 67, column: 8, scope: !111)
!190 = !DILocation(line: 67, column: 13, scope: !111)
!191 = !DILocation(line: 71, column: 12, scope: !111)
!192 = !{!193, !194}
!193 = !{!"pallas.srcLoc", i64 68, i64 5, i64 70, i64 5, !38}
!194 = !{!"pallas.fold", !195, ptr @PALLAS_SPEC_12, !39, !39, !196}
!195 = !{!"pallas.srcLoc", i64 69, i64 5, i64 69, i64 23, !38}
!196 = !{!197, !200}
!197 = !{!120, !198}
!198 = !DILocalVariable(name: "l", arg: 1, scope: !199, file: !1, line: 69, type: !15)
!199 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 69, type: !123, scopeLine: 69, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !39)
!200 = !{!126, !201}
!201 = !DILocalVariable(name: "elem", arg: 2, scope: !199, file: !1, line: 69, type: !8)
!202 = !DILocation(line: 71, column: 5, scope: !111)
!203 = !DILocation(line: 72, column: 1, scope: !111)
!204 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !23, file: !23, line: 18, type: !205, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !24)
!205 = !DISubroutineType(types: !206)
!206 = !{!62}
!207 = !{!""}
!208 = !DILocation(line: 18, column: 9, scope: !204)
!209 = !DILocation(line: 18, column: 23, scope: !204)
!210 = !DILocation(line: 0, scope: !204)
!211 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !23, file: !23, line: 19, type: !205, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !24)
!212 = !DILocation(line: 19, column: 15, scope: !211)
!213 = !DILocation(line: 19, column: 30, scope: !211)
!214 = !DILocation(line: 19, column: 9, scope: !211)
!215 = !DILocation(line: 0, scope: !211)
!216 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !23, file: !23, line: 20, type: !205, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !24)
!217 = !DILocation(line: 20, column: 9, scope: !216)
!218 = !DILocation(line: 20, column: 24, scope: !216)
!219 = !DILocation(line: 20, column: 29, scope: !216)
!220 = !DILocation(line: 0, scope: !216)
!221 = !DILocation(line: 0, scope: !59)
!222 = !DILocation(line: 35, column: 22, scope: !59)
!223 = !DILocation(line: 35, column: 32, scope: !59)
!224 = !DILocation(line: 35, column: 10, scope: !59)
!225 = !DILocation(line: 0, scope: !71)
!226 = !DILocation(line: 36, column: 10, scope: !71)
!227 = !DILocation(line: 36, column: 24, scope: !71)
!228 = !DILocation(line: 0, scope: !79)
!229 = !DILocation(line: 37, column: 21, scope: !79)
!230 = !DILocation(line: 37, column: 10, scope: !79)
!231 = !DILocation(line: 0, scope: !122)
!232 = !DILocation(line: 50, column: 19, scope: !122)
!233 = !DILocation(line: 50, column: 29, scope: !122)
!234 = !DILocation(line: 50, column: 10, scope: !122)
!235 = !DILocation(line: 0, scope: !133)
!236 = !DILocation(line: 51, column: 10, scope: !133)
!237 = !DILocation(line: 51, column: 24, scope: !133)
!238 = !DILocation(line: 0, scope: !141)
!239 = !DILocation(line: 52, column: 21, scope: !141)
!240 = !DILocation(line: 52, column: 10, scope: !141)
!241 = !DILocation(line: 0, scope: !103)
!242 = !DILocation(line: 44, column: 10, scope: !103)
!243 = !DILocation(line: 0, scope: !166)
!244 = !DILocation(line: 59, column: 14, scope: !166)
!245 = !DILocation(line: 0, scope: !182)
!246 = !DILocation(line: 65, column: 12, scope: !182)
!247 = !DILocation(line: 0, scope: !199)
!248 = !DILocation(line: 69, column: 10, scope: !199)
!249 = distinct !DISubprogram(name: "list_write", scope: !23, file: !23, line: 28, type: !250, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !22, retainedNodes: !39)
!250 = !DISubroutineType(types: !251)
!251 = !{!62, !15}
!252 = !{i1 false}
!253 = !DILocalVariable(name: "from", arg: 1, scope: !249, file: !23, line: 28, type: !15)
!254 = !DILocation(line: 0, scope: !249)
!255 = !DILocation(line: 28, column: 16, scope: !249)
!256 = !DILocation(line: 29, column: 22, scope: !249)
!257 = !DILocation(line: 29, column: 10, scope: !249)
!258 = !DILocation(line: 30, column: 23, scope: !249)
!259 = !DILocation(line: 30, column: 28, scope: !249)
!260 = !DILocation(line: 30, column: 55, scope: !249)
!261 = !DILocation(line: 30, column: 38, scope: !249)
!262 = !DILocation(line: 30, column: 10, scope: !249)
!263 = !DILocation(line: 29, column: 5, scope: !249)
!264 = !DILocation(line: 28, column: 5, scope: !249)
!265 = !{!"pallas.result"}
!266 = !{!"pallas.sepConj"}
!267 = !{!"pallas.imply"}
!268 = !{!"pallas.perm"}
!269 = !{!"pallas.fracOf"}
