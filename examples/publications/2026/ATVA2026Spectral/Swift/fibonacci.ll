; ModuleID = 'tmp_spectral/tmp_ir_source.ll'
source_filename = "tmp_spectral/tmp_ir_source.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%TSi = type <{ i64 }>

@"\01l_entry_point" = private constant { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @main to i64), i64 ptrtoint (ptr @"\01l_entry_point" to i64)) to i32), i32 0 }, section "swift5_entry", align 4
@_swift1_autolink_entries = private constant [102 x i8] c"-lswiftSwiftOnoneSupport\00-lswiftCore\00-lswift_Concurrency\00-lswift_StringProcessing\00-lswift_RegexParser\00", section ".swift1_autolink_entries", no_sanitize_address, align 8
@llvm.used = appending global [12 x ptr] [ptr @main, ptr @"$s13tmp_ir_source6fibRecyS2iF", ptr @"$s13tmp_ir_source5fibItyS2iF", ptr @"\01l_entry_point", ptr @__swift_reflection_version, ptr @_swift1_autolink_entries, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF"], section "llvm.metadata"
@".str.35.tmp_ir_source/source_wrappers.swift" = private unnamed_addr constant [36 x i8] c"tmp_ir_source/source_wrappers.swift\00"
@".str.11.Fatal error" = private unnamed_addr constant [12 x i8] c"Fatal error\00"
@__swift_reflection_version = linkonce_odr hidden constant i16 3

define protected i32 @main(i32 %0, ptr %1) #0 !dbg !45 {
entry:
  ret i32 0, !dbg !50
}

define hidden swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %0) #0 !dbg !53 !pallas.fcontract !62 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !60, metadata !DIExpression()), !dbg !74
  %1 = icmp eq i64 %0, 0, !dbg !75
  br i1 %1, label %2, label %3, !dbg !75

2:                                                ; preds = %entry
  br label %24, !dbg !77

3:                                                ; preds = %entry
  %4 = icmp eq i64 %0, 1, !dbg !79
  br i1 %4, label %5, label %6, !dbg !79

5:                                                ; preds = %3
  br label %24, !dbg !81

6:                                                ; preds = %3
  %7 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %0, i64 1), !dbg !83
  %8 = extractvalue { i64, i1 } %7, 0, !dbg !83
  %9 = extractvalue { i64, i1 } %7, 1, !dbg !83
  %10 = call i1 @llvm.expect.i1(i1 %9, i1 false), !dbg !83
  br i1 %10, label %26, label %11, !dbg !83

11:                                               ; preds = %6
  %12 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %8), !dbg !85
  %13 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %0, i64 2), !dbg !86
  %14 = extractvalue { i64, i1 } %13, 0, !dbg !86
  %15 = extractvalue { i64, i1 } %13, 1, !dbg !86
  %16 = call i1 @llvm.expect.i1(i1 %15, i1 false), !dbg !86
  br i1 %16, label %27, label %17, !dbg !86

17:                                               ; preds = %11
  %18 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %14), !dbg !87
  %19 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %12, i64 %18), !dbg !88
  %20 = extractvalue { i64, i1 } %19, 0, !dbg !88
  %21 = extractvalue { i64, i1 } %19, 1, !dbg !88
  %22 = call i1 @llvm.expect.i1(i1 %21, i1 false), !dbg !88
  br i1 %22, label %28, label %23, !dbg !88

23:                                               ; preds = %17
  br label %24, !dbg !89

24:                                               ; preds = %23, %5, %2
  %25 = phi i64 [ %20, %23 ], [ 1, %5 ], [ 0, %2 ], !dbg !90
  ret i64 %25, !dbg !90

26:                                               ; preds = %6
  call void @llvm.trap(), !dbg !91
  unreachable, !dbg !91

27:                                               ; preds = %11
  call void @llvm.trap(), !dbg !94
  unreachable, !dbg !94

28:                                               ; preds = %17
  call void @llvm.trap(), !dbg !95
  unreachable, !dbg !95
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare i1 @llvm.expect.i1(i1, i1) #2

; Function Attrs: cold noreturn nounwind
declare void @llvm.trap() #3

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

define hidden swiftcc i64 @"$s13tmp_ir_source5fibItyS2iF"(i64 %0) #0 !dbg !96 !pallas.fcontract !109 {
entry:
  %1 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %1, metadata !99, metadata !DIExpression()), !dbg !123
  call void @llvm.memset.p0.i64(ptr align 8 %1, i8 0, i64 8, i1 false)
  %2 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !101, metadata !DIExpression()), !dbg !124
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  %3 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !103, metadata !DIExpression()), !dbg !125
  call void @llvm.memset.p0.i64(ptr align 8 %3, i8 0, i64 8, i1 false)
  call void @llvm.dbg.value(metadata i64 %0, metadata !98, metadata !DIExpression()), !dbg !126
  %4 = icmp eq i64 %0, 0, !dbg !127
  br i1 %4, label %5, label %6, !dbg !127

5:                                                ; preds = %entry
  br label %31, !dbg !129

6:                                                ; preds = %entry
  %7 = icmp eq i64 %0, 1, !dbg !131
  br i1 %7, label %8, label %9, !dbg !131

8:                                                ; preds = %6
  br label %31, !dbg !133

9:                                                ; preds = %6
  call void @llvm.lifetime.start.p0(i64 8, ptr %1), !dbg !135
  %._value = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !137
  store i64 0, ptr %._value, align 8, !dbg !137
  call void @llvm.lifetime.start.p0(i64 8, ptr %2), !dbg !139
  %._value1 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !141
  store i64 1, ptr %._value1, align 8, !dbg !141
  call void @llvm.lifetime.start.p0(i64 8, ptr %3), !dbg !143
  %._value2 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !145
  store i64 2, ptr %._value2, align 8, !dbg !145
  br label %10, !dbg !147

10:                                               ; preds = %28, %9
  %._value3 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !147
  %11 = load i64, ptr %._value3, align 8, !dbg !147
  %12 = icmp slt i64 %0, %11, !dbg !148
  %13 = xor i1 %12, true, !dbg !148
  br i1 %13, label %14, label %29, !dbg !148

14:                                               ; preds = %10
  %._value5 = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !149
  %15 = load i64, ptr %._value5, align 8, !dbg !149
  %._value6 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !149
  %16 = load i64, ptr %._value6, align 8, !dbg !149
  %17 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %15, i64 %16), !dbg !150
  %18 = extractvalue { i64, i1 } %17, 0, !dbg !150
  %19 = extractvalue { i64, i1 } %17, 1, !dbg !150
  %20 = call i1 @llvm.expect.i1(i1 %19, i1 false), !dbg !150
  br i1 %20, label %33, label %21, !dbg !150

21:                                               ; preds = %14
  call void @llvm.dbg.value(metadata i64 %18, metadata !105, metadata !DIExpression()), !dbg !152
  %._value7 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !153
  %22 = load i64, ptr %._value7, align 8, !dbg !153
  %._value8 = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !153
  store i64 %22, ptr %._value8, align 8, !dbg !153
  %._value9 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !154
  store i64 %18, ptr %._value9, align 8, !dbg !154
  %._value10 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !155
  %23 = load i64, ptr %._value10, align 8, !dbg !155
  %24 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %23, i64 1), !dbg !155
  %25 = extractvalue { i64, i1 } %24, 0, !dbg !155
  %26 = extractvalue { i64, i1 } %24, 1, !dbg !155
  %27 = call i1 @llvm.expect.i1(i1 %26, i1 false), !dbg !155
  br i1 %27, label %34, label %28, !dbg !155

28:                                               ; preds = %21
  %._value11 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !155
  store i64 %25, ptr %._value11, align 8, !dbg !155
  br label %10, !dbg !156, !llvm.loop !157

29:                                               ; preds = %10
  %._value4 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !198
  %30 = load i64, ptr %._value4, align 8, !dbg !198
  call void @llvm.lifetime.end.p0(i64 8, ptr %3), !dbg !199
  call void @llvm.lifetime.end.p0(i64 8, ptr %2), !dbg !199
  call void @llvm.lifetime.end.p0(i64 8, ptr %1), !dbg !199
  br label %31, !dbg !199

31:                                               ; preds = %29, %8, %5
  %32 = phi i64 [ %30, %29 ], [ 1, %8 ], [ 0, %5 ], !dbg !200
  ret i64 %32, !dbg !200

33:                                               ; preds = %14
  call void @llvm.trap(), !dbg !201
  unreachable, !dbg !201

34:                                               ; preds = %21
  call void @llvm.trap(), !dbg !202
  unreachable, !dbg !202
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #4

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #5

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #4

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF"(i64 %0) #0 !dbg !70 !pallas.exprWrapper !203 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !69, metadata !DIExpression()), !dbg !204
  %1 = icmp slt i64 %0, 0, !dbg !205
  %2 = xor i1 %1, true, !dbg !205
  ret i1 %2, !dbg !204
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF"(i64 %0) #0 !dbg !116 !pallas.exprWrapper !203 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !115, metadata !DIExpression()), !dbg !206
  %1 = icmp slt i64 %0, 0, !dbg !207
  %2 = xor i1 %1, true, !dbg !207
  ret i1 %2, !dbg !206
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF"(i64 %0) #0 !dbg !122 !pallas.exprWrapper !203 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !121, metadata !DIExpression()), !dbg !208
  %1 = call i64 @"pallas.result i64"(), !dbg !209
  %2 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %0), !dbg !210
  %3 = icmp eq i64 %1, %2, !dbg !211
  ret i1 %3, !dbg !208
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF"(i64 %0, i64 %1, i64 %2, i64 %3) #0 !dbg !179 !pallas.exprWrapper !203 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !178, metadata !DIExpression()), !dbg !212
  call void @llvm.dbg.value(metadata i64 %1, metadata !181, metadata !DIExpression()), !dbg !212
  call void @llvm.dbg.value(metadata i64 %2, metadata !183, metadata !DIExpression()), !dbg !212
  call void @llvm.dbg.value(metadata i64 %3, metadata !185, metadata !DIExpression()), !dbg !212
  %4 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %3, i64 1), !dbg !213
  %5 = extractvalue { i64, i1 } %4, 0, !dbg !213
  %6 = extractvalue { i64, i1 } %4, 1, !dbg !213
  %7 = call i1 @llvm.expect.i1(i1 %6, i1 false), !dbg !213
  br i1 %7, label %11, label %8, !dbg !213

8:                                                ; preds = %entry
  %9 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %5), !dbg !214
  %10 = icmp eq i64 %2, %9, !dbg !215
  ret i1 %10, !dbg !212

11:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !216
  unreachable, !dbg !216
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF"(i64 %0, i64 %1, i64 %2, i64 %3) #0 !dbg !165 !pallas.exprWrapper !203 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !164, metadata !DIExpression()), !dbg !218
  call void @llvm.dbg.value(metadata i64 %1, metadata !169, metadata !DIExpression()), !dbg !218
  call void @llvm.dbg.value(metadata i64 %2, metadata !171, metadata !DIExpression()), !dbg !218
  call void @llvm.dbg.value(metadata i64 %3, metadata !173, metadata !DIExpression()), !dbg !218
  %4 = icmp slt i64 %3, 2, !dbg !219
  %5 = xor i1 %4, true, !dbg !219
  %6 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %0, i64 1), !dbg !220
  %7 = extractvalue { i64, i1 } %6, 0, !dbg !220
  %8 = extractvalue { i64, i1 } %6, 1, !dbg !220
  %9 = call i1 @llvm.expect.i1(i1 %8, i1 false), !dbg !220
  br i1 %9, label %14, label %10, !dbg !220

10:                                               ; preds = %entry
  %11 = icmp slt i64 %7, %3, !dbg !221
  %12 = xor i1 %11, true, !dbg !221
  %13 = call i1 @pallas.scAnd(i1 %5, i1 %12), !dbg !222
  ret i1 %13, !dbg !218

14:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !223
  unreachable, !dbg !223
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF"(i64 %0, i64 %1, i64 %2, i64 %3) #0 !dbg !191 !pallas.exprWrapper !203 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !190, metadata !DIExpression()), !dbg !224
  call void @llvm.dbg.value(metadata i64 %1, metadata !193, metadata !DIExpression()), !dbg !224
  call void @llvm.dbg.value(metadata i64 %2, metadata !195, metadata !DIExpression()), !dbg !224
  call void @llvm.dbg.value(metadata i64 %3, metadata !197, metadata !DIExpression()), !dbg !224
  %4 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %3, i64 2), !dbg !225
  %5 = extractvalue { i64, i1 } %4, 0, !dbg !225
  %6 = extractvalue { i64, i1 } %4, 1, !dbg !225
  %7 = call i1 @llvm.expect.i1(i1 %6, i1 false), !dbg !225
  br i1 %7, label %11, label %8, !dbg !225

8:                                                ; preds = %entry
  %9 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %5), !dbg !226
  %10 = icmp eq i64 %1, %9, !dbg !227
  ret i1 %10, !dbg !224

11:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !228
  unreachable, !dbg !228
}

define linkonce_odr hidden swiftcc { i64, ptr } @"$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_"() #0 !dbg !229 {
entry:
  %bitcast = alloca i64, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr %bitcast), !dbg !233
  store i64 -2305843009213693952, ptr %bitcast, align 8, !dbg !233
  %0 = load ptr, ptr %bitcast, align 8, !dbg !233
  call void @llvm.lifetime.end.p0(i64 8, ptr %bitcast), !dbg !233
  %1 = call ptr @swift_bridgeObjectRetain(ptr returned %0) #7, !dbg !233
  %2 = insertvalue { i64, ptr } { i64 0, ptr undef }, ptr %0, 1, !dbg !233
  ret { i64, ptr } %2, !dbg !233
}

; Function Attrs: noinline
declare swiftcc void @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_SSAHSus6UInt32VtF"(i64, i64, i8, i64, ptr, i64, i64, i8, i64, i32) #6

; Function Attrs: nounwind
declare ptr @swift_bridgeObjectRetain(ptr returned) #7

declare !pallas.specLib !234 i64 @"pallas.result i64"()

declare !pallas.specLib !235 i1 @pallas.scAnd(i1, i1)

attributes #0 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { nocallback nofree nosync nounwind willreturn memory(none) }
attributes #3 = { cold noreturn nounwind }
attributes #4 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #5 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #6 = { noinline "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #7 = { nounwind }

!llvm.dbg.cu = !{!0, !15, !17, !19, !31, !32}
!swift.module.flags = !{!33, !33}
!llvm.linker.options = !{}
!llvm.module.flags = !{!34, !35, !36, !37, !38, !39, !40, !41, !42, !43, !44}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !2)
!1 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/Swift/fibonacci.swift", directory: "/home/rme/repos/vercors")
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
!53 = distinct !DISubprogram(name: "fibRec", linkageName: "$s13tmp_ir_source6fibRecyS2iF", scope: !4, file: !1, line: 10, type: !54, scopeLine: 10, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !59)
!54 = !DISubroutineType(types: !55)
!55 = !{!56, !56}
!56 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !6, file: !57, size: 64, elements: !58, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!57 = !DIFile(filename: "swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule", directory: "/home/rme")
!58 = !{}
!59 = !{!60}
!60 = !DILocalVariable(name: "n", arg: 1, scope: !53, file: !1, line: 10, type: !61)
!61 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !56)
!62 = !{!63, i1 true, i1 false, !58, !58, !65}
!63 = !{!"pallas.srcLoc", i64 6, i64 1, i64 9, i64 1, !64}
!64 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/Swift/fibonacci.swift", directory: "", checksumkind: CSK_MD5, checksum: "cb261b970b3cf3ddc19513012f939187")
!65 = !{!"pallas.requires", !66, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", !58, !58, !67}
!66 = !{!"pallas.srcLoc", i64 8, i64 1, i64 8, i64 16, !64}
!67 = !{!68}
!68 = !{!60, !69}
!69 = !DILocalVariable(name: "n", arg: 1, scope: !70, file: !1, line: 8, type: !61)
!70 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", scope: !4, file: !1, line: 8, type: !71, scopeLine: 8, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!71 = !DISubroutineType(types: !72)
!72 = !{!73, !56}
!73 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSbD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!74 = !DILocation(line: 10, column: 13, scope: !53)
!75 = !DILocation(line: 11, column: 11, scope: !76)
!76 = distinct !DILexicalBlock(scope: !53, file: !1, line: 11, column: 5)
!77 = !DILocation(line: 12, column: 9, scope: !78)
!78 = distinct !DILexicalBlock(scope: !76, file: !1, line: 11, column: 17)
!79 = !DILocation(line: 13, column: 18, scope: !80)
!80 = distinct !DILexicalBlock(scope: !76, file: !1, line: 13, column: 12)
!81 = !DILocation(line: 14, column: 9, scope: !82)
!82 = distinct !DILexicalBlock(scope: !80, file: !1, line: 13, column: 24)
!83 = !DILocation(line: 16, column: 25, scope: !84)
!84 = distinct !DILexicalBlock(scope: !80, file: !1, line: 15, column: 12)
!85 = !DILocation(line: 16, column: 16, scope: !84)
!86 = !DILocation(line: 16, column: 41, scope: !84)
!87 = !DILocation(line: 16, column: 32, scope: !84)
!88 = !DILocation(line: 16, column: 30, scope: !84)
!89 = !DILocation(line: 16, column: 9, scope: !84)
!90 = !DILocation(line: 18, column: 1, scope: !84)
!91 = !DILocation(line: 0, scope: !92, inlinedAt: !83)
!92 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !93, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !0)
!93 = !DISubroutineType(types: null)
!94 = !DILocation(line: 0, scope: !92, inlinedAt: !86)
!95 = !DILocation(line: 0, scope: !92, inlinedAt: !88)
!96 = distinct !DISubprogram(name: "fibIt", linkageName: "$s13tmp_ir_source5fibItyS2iF", scope: !4, file: !1, line: 25, type: !54, scopeLine: 25, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !97)
!97 = !{!98, !99, !101, !103, !105}
!98 = !DILocalVariable(name: "n", arg: 1, scope: !96, file: !1, line: 25, type: !61)
!99 = !DILocalVariable(name: "prevRes", scope: !100, file: !1, line: 32, type: !56)
!100 = distinct !DILexicalBlock(scope: !96, file: !1, line: 32, column: 9)
!101 = !DILocalVariable(name: "res", scope: !102, file: !1, line: 33, type: !56)
!102 = distinct !DILexicalBlock(scope: !100, file: !1, line: 33, column: 9)
!103 = !DILocalVariable(name: "i", scope: !104, file: !1, line: 34, type: !56)
!104 = distinct !DILexicalBlock(scope: !102, file: !1, line: 34, column: 9)
!105 = !DILocalVariable(name: "tmp", scope: !106, file: !1, line: 42, type: !61)
!106 = distinct !DILexicalBlock(scope: !107, file: !1, line: 42, column: 13)
!107 = distinct !DILexicalBlock(scope: !108, file: !1, line: 41, column: 18)
!108 = distinct !DILexicalBlock(scope: !104, file: !1, line: 41, column: 5)
!109 = !{!110, i1 false, i1 false, !58, !58, !111, !117}
!110 = !{!"pallas.srcLoc", i64 21, i64 1, i64 24, i64 1, !64}
!111 = !{!"pallas.requires", !112, ptr @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", !58, !58, !113}
!112 = !{!"pallas.srcLoc", i64 22, i64 1, i64 22, i64 16, !64}
!113 = !{!114}
!114 = !{!98, !115}
!115 = !DILocalVariable(name: "n", arg: 1, scope: !116, file: !1, line: 22, type: !61)
!116 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", scope: !4, file: !1, line: 22, type: !71, scopeLine: 22, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!117 = !{!"pallas.ensures", !118, ptr @"$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF", !58, !58, !119}
!118 = !{!"pallas.srcLoc", i64 23, i64 1, i64 23, i64 31, !64}
!119 = !{!120}
!120 = !{!98, !121}
!121 = !DILocalVariable(name: "n", arg: 1, scope: !122, file: !1, line: 23, type: !61)
!122 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF", scope: !4, file: !1, line: 23, type: !71, scopeLine: 23, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!123 = !DILocation(line: 32, column: 9, scope: !100)
!124 = !DILocation(line: 33, column: 9, scope: !102)
!125 = !DILocation(line: 34, column: 9, scope: !104)
!126 = !DILocation(line: 25, column: 12, scope: !96)
!127 = !DILocation(line: 26, column: 12, scope: !128)
!128 = distinct !DILexicalBlock(scope: !96, file: !1, line: 26, column: 6)
!129 = !DILocation(line: 27, column: 9, scope: !130)
!130 = distinct !DILexicalBlock(scope: !128, file: !1, line: 26, column: 18)
!131 = !DILocation(line: 28, column: 20, scope: !132)
!132 = distinct !DILexicalBlock(scope: !128, file: !1, line: 28, column: 14)
!133 = !DILocation(line: 29, column: 9, scope: !134)
!134 = distinct !DILexicalBlock(scope: !132, file: !1, line: 28, column: 26)
!135 = !DILocation(line: 0, scope: !136)
!136 = !DILexicalBlockFile(scope: !100, discriminator: 0)
!137 = !DILocation(line: 32, column: 19, scope: !138)
!138 = distinct !DILexicalBlock(scope: !96, file: !1, line: 32, column: 19)
!139 = !DILocation(line: 0, scope: !140)
!140 = !DILexicalBlockFile(scope: !102, discriminator: 0)
!141 = !DILocation(line: 33, column: 15, scope: !142)
!142 = distinct !DILexicalBlock(scope: !100, file: !1, line: 33, column: 15)
!143 = !DILocation(line: 0, scope: !144)
!144 = !DILexicalBlockFile(scope: !104, discriminator: 0)
!145 = !DILocation(line: 34, column: 13, scope: !146)
!146 = distinct !DILexicalBlock(scope: !102, file: !1, line: 34, column: 13)
!147 = !DILocation(line: 41, column: 5, scope: !108)
!148 = !DILocation(line: 41, column: 13, scope: !108)
!149 = !DILocation(line: 0, scope: !107)
!150 = !DILocation(line: 42, column: 27, scope: !151)
!151 = distinct !DILexicalBlock(scope: !107, file: !1, line: 42, column: 19)
!152 = !DILocation(line: 42, column: 13, scope: !106)
!153 = !DILocation(line: 43, column: 17, scope: !106)
!154 = !DILocation(line: 44, column: 13, scope: !106)
!155 = !DILocation(line: 45, column: 11, scope: !106)
!156 = !DILocation(line: 46, column: 5, scope: !106)
!157 = distinct !{!157, !147, !147, !158}
!158 = !{!"pallas.loopInvBlock", !159, !160, !174, !186}
!159 = !{!"pallas.srcLoc", i64 36, i64 5, i64 40, i64 5, !64}
!160 = !{!"pallas.loopInv", !161, ptr @"$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF", !58, !58, !162}
!161 = !{!"pallas.srcLoc", i64 37, i64 5, i64 37, i64 39, !64}
!162 = !{!163, !168, !170, !172}
!163 = !{!98, !164}
!164 = !DILocalVariable(name: "n", arg: 1, scope: !165, file: !1, line: 37, type: !61)
!165 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF", scope: !4, file: !1, line: 37, type: !166, scopeLine: 37, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!166 = !DISubroutineType(types: !167)
!167 = !{!73, !56, !56, !56, !56}
!168 = !{!99, !169}
!169 = !DILocalVariable(name: "prevRes", arg: 2, scope: !165, file: !1, line: 37, type: !61)
!170 = !{!101, !171}
!171 = !DILocalVariable(name: "res", arg: 3, scope: !165, file: !1, line: 37, type: !61)
!172 = !{!103, !173}
!173 = !DILocalVariable(name: "i", arg: 4, scope: !165, file: !1, line: 37, type: !61)
!174 = !{!"pallas.loopInv", !175, ptr @"$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF", !58, !58, !176}
!175 = !{!"pallas.srcLoc", i64 38, i64 5, i64 38, i64 38, !64}
!176 = !{!177, !180, !182, !184}
!177 = !{!98, !178}
!178 = !DILocalVariable(name: "n", arg: 1, scope: !179, file: !1, line: 38, type: !61)
!179 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF", scope: !4, file: !1, line: 38, type: !166, scopeLine: 38, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!180 = !{!99, !181}
!181 = !DILocalVariable(name: "prevRes", arg: 2, scope: !179, file: !1, line: 38, type: !61)
!182 = !{!101, !183}
!183 = !DILocalVariable(name: "res", arg: 3, scope: !179, file: !1, line: 38, type: !61)
!184 = !{!103, !185}
!185 = !DILocalVariable(name: "i", arg: 4, scope: !179, file: !1, line: 38, type: !61)
!186 = !{!"pallas.loopInv", !187, ptr @"$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF", !58, !58, !188}
!187 = !{!"pallas.srcLoc", i64 39, i64 5, i64 39, i64 42, !64}
!188 = !{!189, !192, !194, !196}
!189 = !{!98, !190}
!190 = !DILocalVariable(name: "n", arg: 1, scope: !191, file: !1, line: 39, type: !61)
!191 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF", scope: !4, file: !1, line: 39, type: !166, scopeLine: 39, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!192 = !{!99, !193}
!193 = !DILocalVariable(name: "prevRes", arg: 2, scope: !191, file: !1, line: 39, type: !61)
!194 = !{!101, !195}
!195 = !DILocalVariable(name: "res", arg: 3, scope: !191, file: !1, line: 39, type: !61)
!196 = !{!103, !197}
!197 = !DILocalVariable(name: "i", arg: 4, scope: !191, file: !1, line: 39, type: !61)
!198 = !DILocation(line: 0, scope: !106)
!199 = !DILocation(line: 47, column: 5, scope: !104)
!200 = !DILocation(line: 48, column: 1, scope: !104)
!201 = !DILocation(line: 0, scope: !92, inlinedAt: !150)
!202 = !DILocation(line: 0, scope: !92, inlinedAt: !155)
!203 = !{!""}
!204 = !DILocation(line: 0, scope: !70)
!205 = !DILocation(line: 8, column: 12, scope: !70)
!206 = !DILocation(line: 0, scope: !116)
!207 = !DILocation(line: 22, column: 12, scope: !116)
!208 = !DILocation(line: 0, scope: !122)
!209 = !DILocation(line: 23, column: 9, scope: !122)
!210 = !DILocation(line: 23, column: 22, scope: !122)
!211 = !DILocation(line: 23, column: 19, scope: !122)
!212 = !DILocation(line: 0, scope: !179)
!213 = !DILocation(line: 38, column: 35, scope: !179)
!214 = !DILocation(line: 38, column: 27, scope: !179)
!215 = !DILocation(line: 38, column: 24, scope: !179)
!216 = !DILocation(line: 0, scope: !217, inlinedAt: !213)
!217 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !93, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!218 = !DILocation(line: 0, scope: !165)
!219 = !DILocation(line: 37, column: 22, scope: !165)
!220 = !DILocation(line: 37, column: 37, scope: !165)
!221 = !DILocation(line: 37, column: 33, scope: !165)
!222 = !DILocation(line: 37, column: 27, scope: !165)
!223 = !DILocation(line: 0, scope: !217, inlinedAt: !220)
!224 = !DILocation(line: 0, scope: !191)
!225 = !DILocation(line: 39, column: 39, scope: !191)
!226 = !DILocation(line: 39, column: 31, scope: !191)
!227 = !DILocation(line: 39, column: 28, scope: !191)
!228 = !DILocation(line: 0, scope: !217, inlinedAt: !225)
!229 = distinct !DISubprogram(linkageName: "$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_", scope: !23, file: !52, type: !230, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!230 = !DISubroutineType(types: !231)
!231 = !{!232}
!232 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSSD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!233 = !DILocation(line: 0, scope: !229)
!234 = !{!"pallas.result"}
!235 = !{!"pallas.scAnd"}
