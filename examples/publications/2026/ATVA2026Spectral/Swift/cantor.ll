; ModuleID = 'tmp_spectral/tmp_ir_source.ll'
source_filename = "tmp_spectral/tmp_ir_source.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@"\01l_entry_point" = private constant { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @main to i64), i64 ptrtoint (ptr @"\01l_entry_point" to i64)) to i32), i32 0 }, section "swift5_entry", align 4
@_swift1_autolink_entries = private constant [102 x i8] c"-lswiftSwiftOnoneSupport\00-lswiftCore\00-lswift_Concurrency\00-lswift_StringProcessing\00-lswift_RegexParser\00", section ".swift1_autolink_entries", no_sanitize_address, align 8
@llvm.used = appending global [10 x ptr] [ptr @main, ptr @"$s13tmp_ir_source10triangular1nS2i_tF", ptr @"$s13tmp_ir_source6square1nS2i_tF", ptr @"$s13tmp_ir_source10cantorPair1x1yS2i_SitF", ptr @"\01l_entry_point", ptr @__swift_reflection_version, ptr @_swift1_autolink_entries, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_21x1ySbSi_SitF"], section "llvm.metadata"
@".str.35.tmp_ir_source/source_wrappers.swift" = private unnamed_addr constant [36 x i8] c"tmp_ir_source/source_wrappers.swift\00"
@".str.11.Fatal error" = private unnamed_addr constant [12 x i8] c"Fatal error\00"
@__swift_reflection_version = linkonce_odr hidden constant i16 3

define protected i32 @main(i32 %0, ptr %1) #0 !dbg !45 {
entry:
  ret i32 0, !dbg !50
}

define hidden swiftcc i64 @"$s13tmp_ir_source10triangular1nS2i_tF"(i64 %0) #0 !dbg !53 !pallas.fcontract !62 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !60, metadata !DIExpression()), !dbg !74
  %1 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %0, i64 1), !dbg !75
  %2 = extractvalue { i64, i1 } %1, 0, !dbg !75
  %3 = extractvalue { i64, i1 } %1, 1, !dbg !75
  %4 = call i1 @llvm.expect.i1(i1 %3, i1 false), !dbg !75
  br i1 %4, label %16, label %5, !dbg !75

5:                                                ; preds = %entry
  %6 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %0, i64 %2), !dbg !76
  %7 = extractvalue { i64, i1 } %6, 0, !dbg !76
  %8 = extractvalue { i64, i1 } %6, 1, !dbg !76
  %9 = call i1 @llvm.expect.i1(i1 %8, i1 false), !dbg !76
  br i1 %9, label %17, label %10, !dbg !76

10:                                               ; preds = %5
  %11 = icmp eq i64 %7, -9223372036854775808, !dbg !77
  br i1 %11, label %12, label %13, !dbg !77

12:                                               ; preds = %10
  br label %14, !dbg !77

13:                                               ; preds = %10
  br label %14, !dbg !77

14:                                               ; preds = %13, %12
  %15 = sdiv i64 %7, 2, !dbg !77
  ret i64 %15, !dbg !78

16:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !79
  unreachable, !dbg !79

17:                                               ; preds = %5
  call void @llvm.trap(), !dbg !82
  unreachable, !dbg !82
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare i1 @llvm.expect.i1(i1, i1) #2

; Function Attrs: cold noreturn nounwind
declare void @llvm.trap() #3

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

define hidden swiftcc i64 @"$s13tmp_ir_source6square1nS2i_tF"(i64 %0) #0 !dbg !83 !pallas.fcontract !86 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !85, metadata !DIExpression()), !dbg !94
  %1 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %0, i64 %0), !dbg !95
  %2 = extractvalue { i64, i1 } %1, 0, !dbg !95
  %3 = extractvalue { i64, i1 } %1, 1, !dbg !95
  %4 = call i1 @llvm.expect.i1(i1 %3, i1 false), !dbg !95
  br i1 %4, label %6, label %5, !dbg !95

5:                                                ; preds = %entry
  ret i64 %2, !dbg !96

6:                                                ; preds = %entry
  call void @llvm.trap(), !dbg !97
  unreachable, !dbg !97
}

define hidden swiftcc i64 @"$s13tmp_ir_source10cantorPair1x1yS2i_SitF"(i64 %0, i64 %1) #0 !dbg !98 !pallas.fcontract !104 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !102, metadata !DIExpression()), !dbg !116
  call void @llvm.dbg.value(metadata i64 %1, metadata !103, metadata !DIExpression()), !dbg !117
  %2 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %0, i64 %1), !dbg !118
  %3 = extractvalue { i64, i1 } %2, 0, !dbg !118
  %4 = extractvalue { i64, i1 } %2, 1, !dbg !118
  %5 = call i1 @llvm.expect.i1(i1 %4, i1 false), !dbg !118
  br i1 %5, label %28, label %6, !dbg !118

6:                                                ; preds = %entry
  %7 = call swiftcc i64 @"$s13tmp_ir_source6square1nS2i_tF"(i64 %3), !dbg !119
  %8 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %7, i64 %0), !dbg !120
  %9 = extractvalue { i64, i1 } %8, 0, !dbg !120
  %10 = extractvalue { i64, i1 } %8, 1, !dbg !120
  %11 = call i1 @llvm.expect.i1(i1 %10, i1 false), !dbg !120
  br i1 %11, label %29, label %12, !dbg !120

12:                                               ; preds = %6
  %13 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 3, i64 %1), !dbg !121
  %14 = extractvalue { i64, i1 } %13, 0, !dbg !121
  %15 = extractvalue { i64, i1 } %13, 1, !dbg !121
  %16 = call i1 @llvm.expect.i1(i1 %15, i1 false), !dbg !121
  br i1 %16, label %30, label %17, !dbg !121

17:                                               ; preds = %12
  %18 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %9, i64 %14), !dbg !122
  %19 = extractvalue { i64, i1 } %18, 0, !dbg !122
  %20 = extractvalue { i64, i1 } %18, 1, !dbg !122
  %21 = call i1 @llvm.expect.i1(i1 %20, i1 false), !dbg !122
  br i1 %21, label %31, label %22, !dbg !122

22:                                               ; preds = %17
  %23 = icmp eq i64 %19, -9223372036854775808, !dbg !123
  br i1 %23, label %24, label %25, !dbg !123

24:                                               ; preds = %22
  br label %26, !dbg !123

25:                                               ; preds = %22
  br label %26, !dbg !123

26:                                               ; preds = %25, %24
  %27 = sdiv i64 %19, 2, !dbg !123
  ret i64 %27, !dbg !124

28:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !125
  unreachable, !dbg !125

29:                                               ; preds = %6
  call void @llvm.trap(), !dbg !126
  unreachable, !dbg !126

30:                                               ; preds = %12
  call void @llvm.trap(), !dbg !127
  unreachable, !dbg !127

31:                                               ; preds = %17
  call void @llvm.trap(), !dbg !128
  unreachable, !dbg !128
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF"(i64 %0) #0 !dbg !70 !pallas.exprWrapper !129 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !69, metadata !DIExpression()), !dbg !130
  ret i1 true, !dbg !130
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF"(i64 %0) #0 !dbg !93 !pallas.exprWrapper !129 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !92, metadata !DIExpression()), !dbg !131
  %1 = call i64 @"pallas.result i64"(), !dbg !132
  %2 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %0, i64 %0), !dbg !133
  %3 = extractvalue { i64, i1 } %2, 0, !dbg !133
  %4 = extractvalue { i64, i1 } %2, 1, !dbg !133
  %5 = call i1 @llvm.expect.i1(i1 %4, i1 false), !dbg !133
  br i1 %5, label %8, label %6, !dbg !133

6:                                                ; preds = %entry
  %7 = icmp eq i64 %1, %3, !dbg !134
  ret i1 %7, !dbg !131

8:                                                ; preds = %entry
  call void @llvm.trap(), !dbg !135
  unreachable, !dbg !135
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_21x1ySbSi_SitF"(i64 %0, i64 %1) #0 !dbg !111 !pallas.exprWrapper !129 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !110, metadata !DIExpression()), !dbg !137
  call void @llvm.dbg.value(metadata i64 %1, metadata !115, metadata !DIExpression()), !dbg !137
  %2 = icmp eq i64 %1, 0, !dbg !138
  %3 = call i64 @"pallas.result i64"(), !dbg !139
  %4 = call swiftcc i64 @"$s13tmp_ir_source10triangular1nS2i_tF"(i64 %0), !dbg !140
  %5 = icmp eq i64 %3, %4, !dbg !141
  %6 = call i1 @pallas.imply(i1 %2, i1 %5), !dbg !142
  ret i1 %6, !dbg !137
}

define linkonce_odr hidden swiftcc { i64, ptr } @"$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_"() #0 !dbg !143 {
entry:
  %bitcast = alloca i64, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr %bitcast), !dbg !147
  store i64 -2305843009213693952, ptr %bitcast, align 8, !dbg !147
  %0 = load ptr, ptr %bitcast, align 8, !dbg !147
  call void @llvm.lifetime.end.p0(i64 8, ptr %bitcast), !dbg !147
  %1 = call ptr @swift_bridgeObjectRetain(ptr returned %0) #6, !dbg !147
  %2 = insertvalue { i64, ptr } { i64 0, ptr undef }, ptr %0, 1, !dbg !147
  ret { i64, ptr } %2, !dbg !147
}

; Function Attrs: noinline
declare swiftcc void @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_SSAHSus6UInt32VtF"(i64, i64, i8, i64, ptr, i64, i64, i8, i64, i32) #4

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #5

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #5

; Function Attrs: nounwind
declare ptr @swift_bridgeObjectRetain(ptr returned) #6

declare !pallas.specLib !148 i64 @"pallas.result i64"()

declare !pallas.specLib !149 i1 @pallas.imply(i1, i1)

attributes #0 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { nocallback nofree nosync nounwind willreturn memory(none) }
attributes #3 = { cold noreturn nounwind }
attributes #4 = { noinline "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #5 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #6 = { nounwind }

!llvm.dbg.cu = !{!0, !15, !17, !19, !31, !32}
!swift.module.flags = !{!33, !33}
!llvm.linker.options = !{}
!llvm.module.flags = !{!34, !35, !36, !37, !38, !39, !40, !41, !42, !43, !44}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !2)
!1 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/Swift/cantor.swift", directory: "/home/rme/repos/vercors")
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
!53 = distinct !DISubprogram(name: "triangular", linkageName: "$s13tmp_ir_source10triangular1nS2i_tF", scope: !4, file: !1, line: 8, type: !54, scopeLine: 8, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !59)
!54 = !DISubroutineType(types: !55)
!55 = !{!56, !56}
!56 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !6, file: !57, size: 64, elements: !58, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!57 = !DIFile(filename: "swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule", directory: "/home/rme")
!58 = !{}
!59 = !{!60}
!60 = !DILocalVariable(name: "n", arg: 1, scope: !53, file: !1, line: 8, type: !61)
!61 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !56)
!62 = !{!63, i1 true, i1 false, !58, !58, !65}
!63 = !{!"pallas.srcLoc", i64 4, i64 1, i64 7, i64 1, !64}
!64 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/Swift/cantor.swift", directory: "", checksumkind: CSK_MD5, checksum: "5f879c9932197cfd699b7480b55f16a3")
!65 = !{!"pallas.requires", !66, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", !58, !58, !67}
!66 = !{!"pallas.srcLoc", i64 6, i64 1, i64 6, i64 14, !64}
!67 = !{!68}
!68 = !{!60, !69}
!69 = !DILocalVariable(name: "n", arg: 1, scope: !70, file: !1, line: 6, type: !61)
!70 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", scope: !4, file: !1, line: 6, type: !71, scopeLine: 6, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!71 = !DISubroutineType(types: !72)
!72 = !{!73, !56}
!73 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSbD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!74 = !DILocation(line: 8, column: 17, scope: !53)
!75 = !DILocation(line: 9, column: 20, scope: !53)
!76 = !DILocation(line: 9, column: 15, scope: !53)
!77 = !DILocation(line: 9, column: 26, scope: !53)
!78 = !DILocation(line: 9, column: 5, scope: !53)
!79 = !DILocation(line: 0, scope: !80, inlinedAt: !75)
!80 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !81, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !0)
!81 = !DISubroutineType(types: null)
!82 = !DILocation(line: 0, scope: !80, inlinedAt: !76)
!83 = distinct !DISubprogram(name: "square", linkageName: "$s13tmp_ir_source6square1nS2i_tF", scope: !4, file: !1, line: 16, type: !54, scopeLine: 16, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !84)
!84 = !{!85}
!85 = !DILocalVariable(name: "n", arg: 1, scope: !83, file: !1, line: 16, type: !61)
!86 = !{!87, i1 true, i1 false, !58, !58, !88}
!87 = !{!"pallas.srcLoc", i64 12, i64 1, i64 15, i64 1, !64}
!88 = !{!"pallas.ensures", !89, ptr @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", !58, !58, !90}
!89 = !{!"pallas.srcLoc", i64 14, i64 1, i64 14, i64 27, !64}
!90 = !{!91}
!91 = !{!85, !92}
!92 = !DILocalVariable(name: "n", arg: 1, scope: !93, file: !1, line: 14, type: !61)
!93 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", scope: !4, file: !1, line: 14, type: !71, scopeLine: 14, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!94 = !DILocation(line: 16, column: 13, scope: !83)
!95 = !DILocation(line: 17, column: 14, scope: !83)
!96 = !DILocation(line: 17, column: 5, scope: !83)
!97 = !DILocation(line: 0, scope: !80, inlinedAt: !95)
!98 = distinct !DISubprogram(name: "cantorPair", linkageName: "$s13tmp_ir_source10cantorPair1x1yS2i_SitF", scope: !4, file: !1, line: 24, type: !99, scopeLine: 24, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !101)
!99 = !DISubroutineType(types: !100)
!100 = !{!56, !56, !56}
!101 = !{!102, !103}
!102 = !DILocalVariable(name: "x", arg: 1, scope: !98, file: !1, line: 24, type: !61)
!103 = !DILocalVariable(name: "y", arg: 2, scope: !98, file: !1, line: 24, type: !61)
!104 = !{!105, i1 true, i1 false, !58, !58, !106}
!105 = !{!"pallas.srcLoc", i64 20, i64 1, i64 23, i64 1, !64}
!106 = !{!"pallas.ensures", !107, ptr @"$s13tmp_ir_source13PALLAS_SPEC_21x1ySbSi_SitF", !58, !58, !108}
!107 = !{!"pallas.srcLoc", i64 22, i64 1, i64 22, i64 49, !64}
!108 = !{!109, !114}
!109 = !{!102, !110}
!110 = !DILocalVariable(name: "x", arg: 1, scope: !111, file: !1, line: 22, type: !61)
!111 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_21x1ySbSi_SitF", scope: !4, file: !1, line: 22, type: !112, scopeLine: 22, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!112 = !DISubroutineType(types: !113)
!113 = !{!73, !56, !56}
!114 = !{!103, !115}
!115 = !DILocalVariable(name: "y", arg: 2, scope: !111, file: !1, line: 22, type: !61)
!116 = !DILocation(line: 24, column: 17, scope: !98)
!117 = !DILocation(line: 24, column: 25, scope: !98)
!118 = !DILocation(line: 25, column: 25, scope: !98)
!119 = !DILocation(line: 25, column: 13, scope: !98)
!120 = !DILocation(line: 25, column: 30, scope: !98)
!121 = !DILocation(line: 25, column: 39, scope: !98)
!122 = !DILocation(line: 25, column: 34, scope: !98)
!123 = !DILocation(line: 25, column: 45, scope: !98)
!124 = !DILocation(line: 25, column: 5, scope: !98)
!125 = !DILocation(line: 0, scope: !80, inlinedAt: !118)
!126 = !DILocation(line: 0, scope: !80, inlinedAt: !120)
!127 = !DILocation(line: 0, scope: !80, inlinedAt: !121)
!128 = !DILocation(line: 0, scope: !80, inlinedAt: !122)
!129 = !{!""}
!130 = !DILocation(line: 0, scope: !70)
!131 = !DILocation(line: 0, scope: !93)
!132 = !DILocation(line: 14, column: 9, scope: !93)
!133 = !DILocation(line: 14, column: 24, scope: !93)
!134 = !DILocation(line: 14, column: 19, scope: !93)
!135 = !DILocation(line: 0, scope: !136, inlinedAt: !133)
!136 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !81, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!137 = !DILocation(line: 0, scope: !111)
!138 = !DILocation(line: 22, column: 11, scope: !111)
!139 = !DILocation(line: 22, column: 20, scope: !111)
!140 = !DILocation(line: 22, column: 33, scope: !111)
!141 = !DILocation(line: 22, column: 30, scope: !111)
!142 = !DILocation(line: 22, column: 16, scope: !111)
!143 = distinct !DISubprogram(linkageName: "$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_", scope: !23, file: !52, type: !144, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!144 = !DISubroutineType(types: !145)
!145 = !{!146}
!146 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSSD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!147 = !DILocation(line: 0, scope: !143)
!148 = !{!"pallas.result"}
!149 = !{!"pallas.imply"}
