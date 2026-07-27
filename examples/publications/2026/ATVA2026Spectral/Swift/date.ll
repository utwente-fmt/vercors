; ModuleID = 'tmp_spectral/tmp_ir_source.ll'
source_filename = "tmp_spectral/tmp_ir_source.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@"\01l_entry_point" = private constant { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @main to i64), i64 ptrtoint (ptr @"\01l_entry_point" to i64)) to i32), i32 0 }, section "swift5_entry", align 4
@_swift1_autolink_entries = private constant [102 x i8] c"-lswiftSwiftOnoneSupport\00-lswiftCore\00-lswift_Concurrency\00-lswift_StringProcessing\00-lswift_RegexParser\00", section ".swift1_autolink_entries", no_sanitize_address, align 8
@llvm.used = appending global [12 x ptr] [ptr @main, ptr @"$s13tmp_ir_source5laterySbSi_S5itF", ptr @"$s13tmp_ir_source4testSiyF", ptr @"\01l_entry_point", ptr @__swift_reflection_version, ptr @_swift1_autolink_entries, ptr @"$s13tmp_ir_source13PALLAS_SPEC_02y12m12d12y22m22d2SbSi_S5itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_12y12m12d12y22m22d2SbSi_S5itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_22y12m12d12y22m22d2SbSi_S5itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_32y12m12d12y22m22d2SbSi_S5itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_42y12m12d12y22m22d2SbSi_S5itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_52y12m12d12y22m22d2SbSi_S5itF"], section "llvm.metadata"
@".str.35.tmp_ir_source/source_wrappers.swift" = private unnamed_addr constant [36 x i8] c"tmp_ir_source/source_wrappers.swift\00"
@".str.11.Fatal error" = private unnamed_addr constant [12 x i8] c"Fatal error\00"
@__swift_reflection_version = linkonce_odr hidden constant i16 3

define protected i32 @main(i32 %0, ptr %1) #0 !dbg !45 {
entry:
  ret i32 0, !dbg !50
}

define hidden swiftcc i1 @"$s13tmp_ir_source5laterySbSi_S5itF"(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5) #0 !dbg !53 !pallas.fcontract !68 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !61, metadata !DIExpression()), !dbg !167
  call void @llvm.dbg.value(metadata i64 %1, metadata !63, metadata !DIExpression()), !dbg !168
  call void @llvm.dbg.value(metadata i64 %2, metadata !64, metadata !DIExpression()), !dbg !169
  call void @llvm.dbg.value(metadata i64 %3, metadata !65, metadata !DIExpression()), !dbg !170
  call void @llvm.dbg.value(metadata i64 %4, metadata !66, metadata !DIExpression()), !dbg !171
  call void @llvm.dbg.value(metadata i64 %5, metadata !67, metadata !DIExpression()), !dbg !172
  %6 = icmp eq i64 %0, %3, !dbg !173
  %7 = xor i1 %6, true, !dbg !173
  br i1 %7, label %8, label %10, !dbg !173

8:                                                ; preds = %entry
  %9 = icmp slt i64 %3, %0, !dbg !175
  br label %17, !dbg !177

10:                                               ; preds = %entry
  %11 = icmp eq i64 %1, %4, !dbg !178
  %12 = xor i1 %11, true, !dbg !178
  br i1 %12, label %13, label %15, !dbg !178

13:                                               ; preds = %10
  %14 = icmp slt i64 %4, %1, !dbg !180
  br label %17, !dbg !182

15:                                               ; preds = %10
  %16 = icmp slt i64 %5, %2, !dbg !183
  br label %17, !dbg !185

17:                                               ; preds = %15, %13, %8
  %18 = phi i1 [ %16, %15 ], [ %14, %13 ], [ %9, %8 ], !dbg !186
  ret i1 %18, !dbg !186
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

define hidden swiftcc i64 @"$s13tmp_ir_source4testSiyF"() #0 !dbg !187 {
entry:
  %0 = call swiftcc i1 @"$s13tmp_ir_source5laterySbSi_S5itF"(i64 2023, i64 3, i64 7, i64 2023, i64 1, i64 1), !dbg !190
  %1 = call swiftcc i1 @"$s13tmp_ir_source5laterySbSi_S5itF"(i64 1, i64 1, i64 2023, i64 15, i64 3, i64 2023), !dbg !191
  ret i64 0, !dbg !192
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_02y12m12d12y22m22d2SbSi_S5itF"(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5) #0 !dbg !76 !pallas.exprWrapper !193 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !75, metadata !DIExpression()), !dbg !194
  call void @llvm.dbg.value(metadata i64 %1, metadata !78, metadata !DIExpression()), !dbg !194
  call void @llvm.dbg.value(metadata i64 %2, metadata !80, metadata !DIExpression()), !dbg !194
  call void @llvm.dbg.value(metadata i64 %3, metadata !82, metadata !DIExpression()), !dbg !194
  call void @llvm.dbg.value(metadata i64 %4, metadata !84, metadata !DIExpression()), !dbg !194
  call void @llvm.dbg.value(metadata i64 %5, metadata !86, metadata !DIExpression()), !dbg !194
  %6 = icmp slt i64 %1, 1, !dbg !195
  %7 = xor i1 %6, true, !dbg !195
  %8 = icmp slt i64 12, %1, !dbg !196
  %9 = xor i1 %8, true, !dbg !196
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !197
  ret i1 %10, !dbg !194
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_12y12m12d12y22m22d2SbSi_S5itF"(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5) #0 !dbg !92 !pallas.exprWrapper !193 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !91, metadata !DIExpression()), !dbg !198
  call void @llvm.dbg.value(metadata i64 %1, metadata !94, metadata !DIExpression()), !dbg !198
  call void @llvm.dbg.value(metadata i64 %2, metadata !96, metadata !DIExpression()), !dbg !198
  call void @llvm.dbg.value(metadata i64 %3, metadata !98, metadata !DIExpression()), !dbg !198
  call void @llvm.dbg.value(metadata i64 %4, metadata !100, metadata !DIExpression()), !dbg !198
  call void @llvm.dbg.value(metadata i64 %5, metadata !102, metadata !DIExpression()), !dbg !198
  %6 = icmp slt i64 %2, 1, !dbg !199
  %7 = xor i1 %6, true, !dbg !199
  %8 = icmp slt i64 31, %2, !dbg !200
  %9 = xor i1 %8, true, !dbg !200
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !201
  ret i1 %10, !dbg !198
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_22y12m12d12y22m22d2SbSi_S5itF"(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5) #0 !dbg !108 !pallas.exprWrapper !193 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !107, metadata !DIExpression()), !dbg !202
  call void @llvm.dbg.value(metadata i64 %1, metadata !110, metadata !DIExpression()), !dbg !202
  call void @llvm.dbg.value(metadata i64 %2, metadata !112, metadata !DIExpression()), !dbg !202
  call void @llvm.dbg.value(metadata i64 %3, metadata !114, metadata !DIExpression()), !dbg !202
  call void @llvm.dbg.value(metadata i64 %4, metadata !116, metadata !DIExpression()), !dbg !202
  call void @llvm.dbg.value(metadata i64 %5, metadata !118, metadata !DIExpression()), !dbg !202
  %6 = icmp slt i64 %4, 1, !dbg !203
  %7 = xor i1 %6, true, !dbg !203
  %8 = icmp slt i64 12, %4, !dbg !204
  %9 = xor i1 %8, true, !dbg !204
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !205
  ret i1 %10, !dbg !202
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_32y12m12d12y22m22d2SbSi_S5itF"(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5) #0 !dbg !124 !pallas.exprWrapper !193 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !123, metadata !DIExpression()), !dbg !206
  call void @llvm.dbg.value(metadata i64 %1, metadata !126, metadata !DIExpression()), !dbg !206
  call void @llvm.dbg.value(metadata i64 %2, metadata !128, metadata !DIExpression()), !dbg !206
  call void @llvm.dbg.value(metadata i64 %3, metadata !130, metadata !DIExpression()), !dbg !206
  call void @llvm.dbg.value(metadata i64 %4, metadata !132, metadata !DIExpression()), !dbg !206
  call void @llvm.dbg.value(metadata i64 %5, metadata !134, metadata !DIExpression()), !dbg !206
  %6 = icmp slt i64 %5, 1, !dbg !207
  %7 = xor i1 %6, true, !dbg !207
  %8 = icmp slt i64 31, %5, !dbg !208
  %9 = xor i1 %8, true, !dbg !208
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !209
  ret i1 %10, !dbg !206
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_42y12m12d12y22m22d2SbSi_S5itF"(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5) #0 !dbg !140 !pallas.exprWrapper !193 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !139, metadata !DIExpression()), !dbg !210
  call void @llvm.dbg.value(metadata i64 %1, metadata !142, metadata !DIExpression()), !dbg !210
  call void @llvm.dbg.value(metadata i64 %2, metadata !144, metadata !DIExpression()), !dbg !210
  call void @llvm.dbg.value(metadata i64 %3, metadata !146, metadata !DIExpression()), !dbg !210
  call void @llvm.dbg.value(metadata i64 %4, metadata !148, metadata !DIExpression()), !dbg !210
  call void @llvm.dbg.value(metadata i64 %5, metadata !150, metadata !DIExpression()), !dbg !210
  %6 = icmp slt i64 %3, %0, !dbg !211
  %7 = call i1 @"pallas.result i1"(), !dbg !212
  %8 = icmp eq i1 %7, true, !dbg !213
  %9 = call i1 @pallas.imply(i1 %6, i1 %8), !dbg !214
  ret i1 %9, !dbg !210
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_52y12m12d12y22m22d2SbSi_S5itF"(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5) #0 !dbg !156 !pallas.exprWrapper !193 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !155, metadata !DIExpression()), !dbg !215
  call void @llvm.dbg.value(metadata i64 %1, metadata !158, metadata !DIExpression()), !dbg !215
  call void @llvm.dbg.value(metadata i64 %2, metadata !160, metadata !DIExpression()), !dbg !215
  call void @llvm.dbg.value(metadata i64 %3, metadata !162, metadata !DIExpression()), !dbg !215
  call void @llvm.dbg.value(metadata i64 %4, metadata !164, metadata !DIExpression()), !dbg !215
  call void @llvm.dbg.value(metadata i64 %5, metadata !166, metadata !DIExpression()), !dbg !215
  %6 = icmp eq i64 %0, %3, !dbg !216
  %7 = icmp eq i64 %1, %4, !dbg !217
  %8 = call i1 @pallas.scAnd(i1 %6, i1 %7), !dbg !218
  %9 = call i1 @"pallas.result i1"(), !dbg !219
  %10 = icmp slt i64 %5, %2, !dbg !220
  %11 = icmp eq i1 %9, %10, !dbg !221
  %12 = call i1 @pallas.imply(i1 %8, i1 %11), !dbg !222
  ret i1 %12, !dbg !215
}

define linkonce_odr hidden swiftcc { i64, ptr } @"$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_"() #0 !dbg !223 {
entry:
  %bitcast = alloca i64, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr %bitcast), !dbg !227
  store i64 -2305843009213693952, ptr %bitcast, align 8, !dbg !227
  %0 = load ptr, ptr %bitcast, align 8, !dbg !227
  call void @llvm.lifetime.end.p0(i64 8, ptr %bitcast), !dbg !227
  %1 = call ptr @swift_bridgeObjectRetain(ptr returned %0) #4, !dbg !227
  %2 = insertvalue { i64, ptr } { i64 0, ptr undef }, ptr %0, 1, !dbg !227
  ret { i64, ptr } %2, !dbg !227
}

; Function Attrs: noinline
declare swiftcc void @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_SSAHSus6UInt32VtF"(i64, i64, i8, i64, ptr, i64, i64, i8, i64, i32) #2

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #3

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #3

; Function Attrs: nounwind
declare ptr @swift_bridgeObjectRetain(ptr returned) #4

declare !pallas.specLib !228 i1 @"pallas.result i1"()

declare !pallas.specLib !229 i1 @pallas.imply(i1, i1)

declare !pallas.specLib !230 i1 @pallas.scAnd(i1, i1)

attributes #0 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { noinline "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #4 = { nounwind }

!llvm.dbg.cu = !{!0, !15, !17, !19, !31, !32}
!swift.module.flags = !{!33, !33}
!llvm.linker.options = !{}
!llvm.module.flags = !{!34, !35, !36, !37, !38, !39, !40, !41, !42, !43, !44}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !2)
!1 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/Swift/date.swift", directory: "/home/rme/repos/vercors")
!2 = !{!3, !5, !7, !9, !11, !13}
!3 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !4, file: !1)
!4 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "examples/publications/2026/ATVA2026Spectral/Swift")
!5 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !6, file: !1)
!6 = !DIModule(scope: null, name: "Swift", includePath: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!7 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !8, file: !1)
!8 = !DIModule(scope: null, name: "_StringProcessing", includePath: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/_StringProcessing.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!9 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !10, file: !1)
!10 = !DIModule(scope: null, name: "_SwiftConcurrencyShims", includePath: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/shims")
!11 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !12, file: !1)
!12 = !DIModule(scope: null, name: "_Concurrency", includePath: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/_Concurrency.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!13 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !14, file: !1)
!14 = !DIModule(scope: null, name: "SwiftOnoneSupport", includePath: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/SwiftOnoneSupport.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!15 = distinct !DICompileUnit(language: DW_LANG_C11, file: !16, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!16 = !DIFile(filename: "<swift-imported-modules>", directory: "/home/rme/repos/vercors")
!17 = distinct !DICompileUnit(language: DW_LANG_C99, file: !18, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "/home/rme/.cache/clang/ModuleCache/1T7NA3LBRX57T/_SwiftConcurrencyShims-16QL5XP1HZ73F.pcm", emissionKind: FullDebug, dwoId: 205401482013525099)
!18 = !DIFile(filename: "_SwiftConcurrencyShims", directory: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/shims")
!19 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !20, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !21)
!20 = !DIFile(filename: "tmp_spectral/source_wrappers.swift", directory: "/home/rme/repos/vercors")
!21 = !{!22, !24, !25, !27, !28, !29, !30}
!22 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !23, file: !20)
!23 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "tmp_spectral")
!24 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !6, file: !20)
!25 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !26, file: !20, line: 1)
!26 = !DIModule(scope: null, name: "PallasSpec", includePath: "/home/rme/repos/pallas_spec2ir/res/spec_libs/swift/PallasSpec/.build/debug/Modules/PallasSpec.swiftmodule")
!27 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !8, file: !20)
!28 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !10, file: !20)
!29 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !12, file: !20)
!30 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !14, file: !20)
!31 = distinct !DICompileUnit(language: DW_LANG_C11, file: !16, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!32 = distinct !DICompileUnit(language: DW_LANG_C99, file: !18, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "/home/rme/.cache/clang/ModuleCache/1T7NA3LBRX57T/_SwiftConcurrencyShims-16QL5XP1HZ73F.pcm", emissionKind: FullDebug, dwoId: 205401482013525099)
!33 = !{!"standard-library", i1 false}
!34 = !{i32 7, !"Dwarf Version", i32 4}
!35 = !{i32 2, !"Debug Info Version", i32 3}
!36 = !{i32 1, !"wchar_size", i32 4}
!37 = !{i32 8, !"PIC Level", i32 2}
!38 = !{i32 7, !"uwtable", i32 2}
!39 = !{i32 7, !"frame-pointer", i32 2}
!40 = !{i32 1, !"Objective-C Garbage Collection", i8 0}
!41 = !{i32 1, !"Swift Version", i32 7}
!42 = !{i32 1, !"Swift ABI Version", i32 7}
!43 = !{i32 1, !"Swift Major Version", i8 6}
!44 = !{i32 1, !"Swift Minor Version", i8 0}
!45 = distinct !DISubprogram(name: "main", linkageName: "main", scope: !4, file: !1, line: 1, type: !46, spFlags: DISPFlagDefinition, unit: !0)
!46 = !DISubroutineType(types: !47)
!47 = !{!48, !48, !49}
!48 = !DICompositeType(tag: DW_TAG_structure_type, name: "$ss5Int32VD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!49 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSpySpys4Int8VGSgGD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!50 = !DILocation(line: 0, scope: !51)
!51 = !DILexicalBlockFile(scope: !45, file: !52, discriminator: 0)
!52 = !DIFile(filename: "<compiler-generated>", directory: "/")
!53 = distinct !DISubprogram(name: "later", linkageName: "$s13tmp_ir_source5laterySbSi_S5itF", scope: !4, file: !1, line: 12, type: !54, scopeLine: 12, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !60)
!54 = !DISubroutineType(types: !55)
!55 = !{!56, !57, !57, !57, !57, !57, !57}
!56 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSbD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!57 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !6, file: !58, size: 64, elements: !59, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!58 = !DIFile(filename: "swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule", directory: "/home/rme")
!59 = !{}
!60 = !{!61, !63, !64, !65, !66, !67}
!61 = !DILocalVariable(name: "y1", arg: 1, scope: !53, file: !1, line: 12, type: !62)
!62 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !57)
!63 = !DILocalVariable(name: "m1", arg: 2, scope: !53, file: !1, line: 12, type: !62)
!64 = !DILocalVariable(name: "d1", arg: 3, scope: !53, file: !1, line: 12, type: !62)
!65 = !DILocalVariable(name: "y2", arg: 4, scope: !53, file: !1, line: 13, type: !62)
!66 = !DILocalVariable(name: "m2", arg: 5, scope: !53, file: !1, line: 13, type: !62)
!67 = !DILocalVariable(name: "d2", arg: 6, scope: !53, file: !1, line: 13, type: !62)
!68 = !{!69, i1 false, i1 false, !59, !59, !71, !87, !103, !119, !135, !151}
!69 = !{!"pallas.srcLoc", i64 4, i64 1, i64 11, i64 1, !70}
!70 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/Swift/date.swift", directory: "", checksumkind: CSK_MD5, checksum: "bc62a9b90b52444fb825db3ad3f53036")
!71 = !{!"pallas.requires", !72, ptr @"$s13tmp_ir_source13PALLAS_SPEC_02y12m12d12y22m22d2SbSi_S5itF", !59, !59, !73}
!72 = !{!"pallas.srcLoc", i64 5, i64 1, i64 5, i64 30, !70}
!73 = !{!74, !77, !79, !81, !83, !85}
!74 = !{!61, !75}
!75 = !DILocalVariable(name: "y1", arg: 1, scope: !76, file: !1, line: 5, type: !62)
!76 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_02y12m12d12y22m22d2SbSi_S5itF", scope: !4, file: !1, line: 5, type: !54, scopeLine: 5, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !59)
!77 = !{!63, !78}
!78 = !DILocalVariable(name: "m1", arg: 2, scope: !76, file: !1, line: 5, type: !62)
!79 = !{!64, !80}
!80 = !DILocalVariable(name: "d1", arg: 3, scope: !76, file: !1, line: 5, type: !62)
!81 = !{!65, !82}
!82 = !DILocalVariable(name: "y2", arg: 4, scope: !76, file: !1, line: 5, type: !62)
!83 = !{!66, !84}
!84 = !DILocalVariable(name: "m2", arg: 5, scope: !76, file: !1, line: 5, type: !62)
!85 = !{!67, !86}
!86 = !DILocalVariable(name: "d2", arg: 6, scope: !76, file: !1, line: 5, type: !62)
!87 = !{!"pallas.requires", !88, ptr @"$s13tmp_ir_source13PALLAS_SPEC_12y12m12d12y22m22d2SbSi_S5itF", !59, !59, !89}
!88 = !{!"pallas.srcLoc", i64 6, i64 1, i64 6, i64 30, !70}
!89 = !{!90, !93, !95, !97, !99, !101}
!90 = !{!61, !91}
!91 = !DILocalVariable(name: "y1", arg: 1, scope: !92, file: !1, line: 6, type: !62)
!92 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_12y12m12d12y22m22d2SbSi_S5itF", scope: !4, file: !1, line: 6, type: !54, scopeLine: 6, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !59)
!93 = !{!63, !94}
!94 = !DILocalVariable(name: "m1", arg: 2, scope: !92, file: !1, line: 6, type: !62)
!95 = !{!64, !96}
!96 = !DILocalVariable(name: "d1", arg: 3, scope: !92, file: !1, line: 6, type: !62)
!97 = !{!65, !98}
!98 = !DILocalVariable(name: "y2", arg: 4, scope: !92, file: !1, line: 6, type: !62)
!99 = !{!66, !100}
!100 = !DILocalVariable(name: "m2", arg: 5, scope: !92, file: !1, line: 6, type: !62)
!101 = !{!67, !102}
!102 = !DILocalVariable(name: "d2", arg: 6, scope: !92, file: !1, line: 6, type: !62)
!103 = !{!"pallas.requires", !104, ptr @"$s13tmp_ir_source13PALLAS_SPEC_22y12m12d12y22m22d2SbSi_S5itF", !59, !59, !105}
!104 = !{!"pallas.srcLoc", i64 7, i64 1, i64 7, i64 30, !70}
!105 = !{!106, !109, !111, !113, !115, !117}
!106 = !{!61, !107}
!107 = !DILocalVariable(name: "y1", arg: 1, scope: !108, file: !1, line: 7, type: !62)
!108 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_22y12m12d12y22m22d2SbSi_S5itF", scope: !4, file: !1, line: 7, type: !54, scopeLine: 7, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !59)
!109 = !{!63, !110}
!110 = !DILocalVariable(name: "m1", arg: 2, scope: !108, file: !1, line: 7, type: !62)
!111 = !{!64, !112}
!112 = !DILocalVariable(name: "d1", arg: 3, scope: !108, file: !1, line: 7, type: !62)
!113 = !{!65, !114}
!114 = !DILocalVariable(name: "y2", arg: 4, scope: !108, file: !1, line: 7, type: !62)
!115 = !{!66, !116}
!116 = !DILocalVariable(name: "m2", arg: 5, scope: !108, file: !1, line: 7, type: !62)
!117 = !{!67, !118}
!118 = !DILocalVariable(name: "d2", arg: 6, scope: !108, file: !1, line: 7, type: !62)
!119 = !{!"pallas.requires", !120, ptr @"$s13tmp_ir_source13PALLAS_SPEC_32y12m12d12y22m22d2SbSi_S5itF", !59, !59, !121}
!120 = !{!"pallas.srcLoc", i64 8, i64 1, i64 8, i64 30, !70}
!121 = !{!122, !125, !127, !129, !131, !133}
!122 = !{!61, !123}
!123 = !DILocalVariable(name: "y1", arg: 1, scope: !124, file: !1, line: 8, type: !62)
!124 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_32y12m12d12y22m22d2SbSi_S5itF", scope: !4, file: !1, line: 8, type: !54, scopeLine: 8, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !59)
!125 = !{!63, !126}
!126 = !DILocalVariable(name: "m1", arg: 2, scope: !124, file: !1, line: 8, type: !62)
!127 = !{!64, !128}
!128 = !DILocalVariable(name: "d1", arg: 3, scope: !124, file: !1, line: 8, type: !62)
!129 = !{!65, !130}
!130 = !DILocalVariable(name: "y2", arg: 4, scope: !124, file: !1, line: 8, type: !62)
!131 = !{!66, !132}
!132 = !DILocalVariable(name: "m2", arg: 5, scope: !124, file: !1, line: 8, type: !62)
!133 = !{!67, !134}
!134 = !DILocalVariable(name: "d2", arg: 6, scope: !124, file: !1, line: 8, type: !62)
!135 = !{!"pallas.ensures", !136, ptr @"$s13tmp_ir_source13PALLAS_SPEC_42y12m12d12y22m22d2SbSi_S5itF", !59, !59, !137}
!136 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 38, !70}
!137 = !{!138, !141, !143, !145, !147, !149}
!138 = !{!61, !139}
!139 = !DILocalVariable(name: "y1", arg: 1, scope: !140, file: !1, line: 9, type: !62)
!140 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_42y12m12d12y22m22d2SbSi_S5itF", scope: !4, file: !1, line: 9, type: !54, scopeLine: 9, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !59)
!141 = !{!63, !142}
!142 = !DILocalVariable(name: "m1", arg: 2, scope: !140, file: !1, line: 9, type: !62)
!143 = !{!64, !144}
!144 = !DILocalVariable(name: "d1", arg: 3, scope: !140, file: !1, line: 9, type: !62)
!145 = !{!65, !146}
!146 = !DILocalVariable(name: "y2", arg: 4, scope: !140, file: !1, line: 9, type: !62)
!147 = !{!66, !148}
!148 = !DILocalVariable(name: "m2", arg: 5, scope: !140, file: !1, line: 9, type: !62)
!149 = !{!67, !150}
!150 = !DILocalVariable(name: "d2", arg: 6, scope: !140, file: !1, line: 9, type: !62)
!151 = !{!"pallas.ensures", !152, ptr @"$s13tmp_ir_source13PALLAS_SPEC_52y12m12d12y22m22d2SbSi_S5itF", !59, !59, !153}
!152 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 61, !70}
!153 = !{!154, !157, !159, !161, !163, !165}
!154 = !{!61, !155}
!155 = !DILocalVariable(name: "y1", arg: 1, scope: !156, file: !1, line: 10, type: !62)
!156 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_52y12m12d12y22m22d2SbSi_S5itF", scope: !4, file: !1, line: 10, type: !54, scopeLine: 10, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !59)
!157 = !{!63, !158}
!158 = !DILocalVariable(name: "m1", arg: 2, scope: !156, file: !1, line: 10, type: !62)
!159 = !{!64, !160}
!160 = !DILocalVariable(name: "d1", arg: 3, scope: !156, file: !1, line: 10, type: !62)
!161 = !{!65, !162}
!162 = !DILocalVariable(name: "y2", arg: 4, scope: !156, file: !1, line: 10, type: !62)
!163 = !{!66, !164}
!164 = !DILocalVariable(name: "m2", arg: 5, scope: !156, file: !1, line: 10, type: !62)
!165 = !{!67, !166}
!166 = !DILocalVariable(name: "d2", arg: 6, scope: !156, file: !1, line: 10, type: !62)
!167 = !DILocation(line: 12, column: 12, scope: !53)
!168 = !DILocation(line: 12, column: 23, scope: !53)
!169 = !DILocation(line: 12, column: 34, scope: !53)
!170 = !DILocation(line: 13, column: 12, scope: !53)
!171 = !DILocation(line: 13, column: 23, scope: !53)
!172 = !DILocation(line: 13, column: 34, scope: !53)
!173 = !DILocation(line: 14, column: 12, scope: !174)
!174 = distinct !DILexicalBlock(scope: !53, file: !1, line: 14, column: 5)
!175 = !DILocation(line: 15, column: 19, scope: !176)
!176 = distinct !DILexicalBlock(scope: !174, file: !1, line: 14, column: 19)
!177 = !DILocation(line: 15, column: 9, scope: !176)
!178 = !DILocation(line: 16, column: 19, scope: !179)
!179 = distinct !DILexicalBlock(scope: !174, file: !1, line: 16, column: 12)
!180 = !DILocation(line: 17, column: 19, scope: !181)
!181 = distinct !DILexicalBlock(scope: !179, file: !1, line: 16, column: 26)
!182 = !DILocation(line: 17, column: 9, scope: !181)
!183 = !DILocation(line: 19, column: 19, scope: !184)
!184 = distinct !DILexicalBlock(scope: !179, file: !1, line: 18, column: 12)
!185 = !DILocation(line: 19, column: 9, scope: !184)
!186 = !DILocation(line: 21, column: 1, scope: !184)
!187 = distinct !DISubprogram(name: "test", linkageName: "$s13tmp_ir_source4testSiyF", scope: !4, file: !1, line: 23, type: !188, scopeLine: 23, spFlags: DISPFlagDefinition, unit: !0)
!188 = !DISubroutineType(types: !189)
!189 = !{!57}
!190 = !DILocation(line: 24, column: 9, scope: !187)
!191 = !DILocation(line: 26, column: 9, scope: !187)
!192 = !DILocation(line: 28, column: 5, scope: !187)
!193 = !{!""}
!194 = !DILocation(line: 0, scope: !76)
!195 = !DILocation(line: 5, column: 12, scope: !76)
!196 = !DILocation(line: 5, column: 25, scope: !76)
!197 = !DILocation(line: 5, column: 18, scope: !76)
!198 = !DILocation(line: 0, scope: !92)
!199 = !DILocation(line: 6, column: 12, scope: !92)
!200 = !DILocation(line: 6, column: 25, scope: !92)
!201 = !DILocation(line: 6, column: 18, scope: !92)
!202 = !DILocation(line: 0, scope: !108)
!203 = !DILocation(line: 7, column: 12, scope: !108)
!204 = !DILocation(line: 7, column: 25, scope: !108)
!205 = !DILocation(line: 7, column: 18, scope: !108)
!206 = !DILocation(line: 0, scope: !124)
!207 = !DILocation(line: 8, column: 12, scope: !124)
!208 = !DILocation(line: 8, column: 25, scope: !124)
!209 = !DILocation(line: 8, column: 18, scope: !124)
!210 = !DILocation(line: 0, scope: !140)
!211 = !DILocation(line: 9, column: 12, scope: !140)
!212 = !DILocation(line: 9, column: 21, scope: !140)
!213 = !DILocation(line: 9, column: 31, scope: !140)
!214 = !DILocation(line: 9, column: 17, scope: !140)
!215 = !DILocation(line: 0, scope: !156)
!216 = !DILocation(line: 10, column: 13, scope: !156)
!217 = !DILocation(line: 10, column: 26, scope: !156)
!218 = !DILocation(line: 10, column: 19, scope: !156)
!219 = !DILocation(line: 10, column: 38, scope: !156)
!220 = !DILocation(line: 10, column: 55, scope: !156)
!221 = !DILocation(line: 10, column: 48, scope: !156)
!222 = !DILocation(line: 10, column: 33, scope: !156)
!223 = distinct !DISubprogram(linkageName: "$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_", scope: !23, file: !52, type: !224, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!224 = !DISubroutineType(types: !225)
!225 = !{!226}
!226 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSSD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!227 = !DILocation(line: 0, scope: !223)
!228 = !{!"pallas.result"}
!229 = !{!"pallas.imply"}
!230 = !{!"pallas.scAnd"}
