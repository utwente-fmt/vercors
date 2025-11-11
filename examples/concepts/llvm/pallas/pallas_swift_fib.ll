; ModuleID = 'tmp/tmp_ir_source.ll'
source_filename = "tmp/tmp_ir_source.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%TSi = type <{ i64 }>

@"\01l_entry_point" = private constant { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @main to i64), i64 ptrtoint (ptr @"\01l_entry_point" to i64)) to i32), i32 0 }, section "swift5_entry", align 4
@_swift1_autolink_entries = private constant [102 x i8] c"-lswiftSwiftOnoneSupport\00-lswiftCore\00-lswift_Concurrency\00-lswift_StringProcessing\00-lswift_RegexParser\00", section ".swift1_autolink_entries", no_sanitize_address, align 8
@llvm.used = appending global [12 x ptr] [ptr @main, ptr @"$s13tmp_ir_source6fibRecyS2iF", ptr @"$s13tmp_ir_source5fibItyS2iF", ptr @"\01l_entry_point", ptr @__swift_reflection_version, ptr @_swift1_autolink_entries, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF"], section "llvm.metadata"
@".str.35.tmp_ir_source/source_wrappers.swift" = private unnamed_addr constant [36 x i8] c"tmp_ir_source/source_wrappers.swift\00"
@".str.11.Fatal error" = private unnamed_addr constant [12 x i8] c"Fatal error\00"
@__swift_reflection_version = linkonce_odr hidden constant i16 3

define protected i32 @main(i32 %0, ptr %1) #0 !dbg !45 {
entry:
  ret i32 0, !dbg !50
}

define hidden swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %0) #0 !dbg !53 !pallas.fcontract !62 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !60, metadata !DIExpression()), !dbg !67
  %1 = icmp eq i64 %0, 0, !dbg !68
  br i1 %1, label %2, label %3, !dbg !68

2:                                                ; preds = %entry
  br label %24, !dbg !70

3:                                                ; preds = %entry
  %4 = icmp eq i64 %0, 1, !dbg !72
  br i1 %4, label %5, label %6, !dbg !72

5:                                                ; preds = %3
  br label %24, !dbg !74

6:                                                ; preds = %3
  %7 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %0, i64 1), !dbg !76
  %8 = extractvalue { i64, i1 } %7, 0, !dbg !76
  %9 = extractvalue { i64, i1 } %7, 1, !dbg !76
  %10 = call i1 @llvm.expect.i1(i1 %9, i1 false), !dbg !76
  br i1 %10, label %26, label %11, !dbg !76

11:                                               ; preds = %6
  %12 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %8), !dbg !78
  %13 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %0, i64 2), !dbg !79
  %14 = extractvalue { i64, i1 } %13, 0, !dbg !79
  %15 = extractvalue { i64, i1 } %13, 1, !dbg !79
  %16 = call i1 @llvm.expect.i1(i1 %15, i1 false), !dbg !79
  br i1 %16, label %27, label %17, !dbg !79

17:                                               ; preds = %11
  %18 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %14), !dbg !80
  %19 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %12, i64 %18), !dbg !81
  %20 = extractvalue { i64, i1 } %19, 0, !dbg !81
  %21 = extractvalue { i64, i1 } %19, 1, !dbg !81
  %22 = call i1 @llvm.expect.i1(i1 %21, i1 false), !dbg !81
  br i1 %22, label %28, label %23, !dbg !81

23:                                               ; preds = %17
  br label %24, !dbg !82

24:                                               ; preds = %23, %5, %2
  %25 = phi i64 [ %20, %23 ], [ 1, %5 ], [ 0, %2 ], !dbg !83
  ret i64 %25, !dbg !83

26:                                               ; preds = %6
  call void @llvm.trap(), !dbg !84
  unreachable, !dbg !84

27:                                               ; preds = %11
  call void @llvm.trap(), !dbg !87
  unreachable, !dbg !87

28:                                               ; preds = %17
  call void @llvm.trap(), !dbg !88
  unreachable, !dbg !88
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

define hidden swiftcc i64 @"$s13tmp_ir_source5fibItyS2iF"(i64 %0) #0 !dbg !89 !pallas.fcontract !102 {
entry:
  %1 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %1, metadata !92, metadata !DIExpression()), !dbg !108
  call void @llvm.memset.p0.i64(ptr align 8 %1, i8 0, i64 8, i1 false)
  %2 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !94, metadata !DIExpression()), !dbg !109
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  %3 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !96, metadata !DIExpression()), !dbg !110
  call void @llvm.memset.p0.i64(ptr align 8 %3, i8 0, i64 8, i1 false)
  call void @llvm.dbg.value(metadata i64 %0, metadata !91, metadata !DIExpression()), !dbg !111
  %4 = icmp eq i64 %0, 0, !dbg !112
  br i1 %4, label %5, label %6, !dbg !112

5:                                                ; preds = %entry
  br label %31, !dbg !114

6:                                                ; preds = %entry
  %7 = icmp eq i64 %0, 1, !dbg !116
  br i1 %7, label %8, label %9, !dbg !116

8:                                                ; preds = %6
  br label %31, !dbg !118

9:                                                ; preds = %6
  call void @llvm.lifetime.start.p0(i64 8, ptr %1), !dbg !120
  %._value = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !122
  store i64 0, ptr %._value, align 8, !dbg !122
  call void @llvm.lifetime.start.p0(i64 8, ptr %2), !dbg !124
  %._value1 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !126
  store i64 1, ptr %._value1, align 8, !dbg !126
  call void @llvm.lifetime.start.p0(i64 8, ptr %3), !dbg !128
  %._value2 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !130
  store i64 2, ptr %._value2, align 8, !dbg !130
  br label %10, !dbg !132

10:                                               ; preds = %28, %9
  %._value3 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !132
  %11 = load i64, ptr %._value3, align 8, !dbg !132
  %12 = icmp slt i64 %0, %11, !dbg !133
  %13 = xor i1 %12, true, !dbg !133
  br i1 %13, label %14, label %29, !dbg !133

14:                                               ; preds = %10
  %._value5 = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !134
  %15 = load i64, ptr %._value5, align 8, !dbg !134
  %._value6 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !134
  %16 = load i64, ptr %._value6, align 8, !dbg !134
  %17 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %15, i64 %16), !dbg !135
  %18 = extractvalue { i64, i1 } %17, 0, !dbg !135
  %19 = extractvalue { i64, i1 } %17, 1, !dbg !135
  %20 = call i1 @llvm.expect.i1(i1 %19, i1 false), !dbg !135
  br i1 %20, label %33, label %21, !dbg !135

21:                                               ; preds = %14
  call void @llvm.dbg.value(metadata i64 %18, metadata !98, metadata !DIExpression()), !dbg !137
  %._value7 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !138
  %22 = load i64, ptr %._value7, align 8, !dbg !138
  %._value8 = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !138
  store i64 %22, ptr %._value8, align 8, !dbg !138
  %._value9 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !139
  store i64 %18, ptr %._value9, align 8, !dbg !139
  %._value10 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !140
  %23 = load i64, ptr %._value10, align 8, !dbg !140
  %24 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %23, i64 1), !dbg !140
  %25 = extractvalue { i64, i1 } %24, 0, !dbg !140
  %26 = extractvalue { i64, i1 } %24, 1, !dbg !140
  %27 = call i1 @llvm.expect.i1(i1 %26, i1 false), !dbg !140
  br i1 %27, label %34, label %28, !dbg !140

28:                                               ; preds = %21
  %._value11 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !140
  store i64 %25, ptr %._value11, align 8, !dbg !140
  br label %10, !dbg !141, !llvm.loop !142

29:                                               ; preds = %10
  %._value4 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !151
  %30 = load i64, ptr %._value4, align 8, !dbg !151
  call void @llvm.lifetime.end.p0(i64 8, ptr %3), !dbg !152
  call void @llvm.lifetime.end.p0(i64 8, ptr %2), !dbg !152
  call void @llvm.lifetime.end.p0(i64 8, ptr %1), !dbg !152
  br label %31, !dbg !152

31:                                               ; preds = %29, %8, %5
  %32 = phi i64 [ %30, %29 ], [ 1, %8 ], [ 0, %5 ], !dbg !153
  ret i64 %32, !dbg !153

33:                                               ; preds = %14
  call void @llvm.trap(), !dbg !154
  unreachable, !dbg !154

34:                                               ; preds = %21
  call void @llvm.trap(), !dbg !155
  unreachable, !dbg !155
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #4

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #5

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #4

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF"(i64 %0) #0 !dbg !156 !pallas.exprWrapper !160 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !161, metadata !DIExpression()), !dbg !162
  %1 = icmp slt i64 %0, 0, !dbg !163
  %2 = xor i1 %1, true, !dbg !163
  ret i1 %2, !dbg !162
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF"(i64 %0) #0 !dbg !164 !pallas.exprWrapper !160 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !165, metadata !DIExpression()), !dbg !166
  %1 = icmp slt i64 %0, 0, !dbg !167
  %2 = xor i1 %1, true, !dbg !167
  ret i1 %2, !dbg !166
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF"(i64 %0) #0 !dbg !168 !pallas.exprWrapper !160 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !169, metadata !DIExpression()), !dbg !170
  %1 = call i64 @pallas.result.0(), !dbg !171
  %2 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %0), !dbg !172
  %3 = icmp eq i64 %1, %2, !dbg !173
  ret i1 %3, !dbg !170
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF"(i64 %0, i64 %1, i64 %2, i64 %3) #0 !dbg !174 !pallas.exprWrapper !160 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !177, metadata !DIExpression()), !dbg !178
  call void @llvm.dbg.value(metadata i64 %1, metadata !179, metadata !DIExpression()), !dbg !178
  call void @llvm.dbg.value(metadata i64 %2, metadata !180, metadata !DIExpression()), !dbg !178
  call void @llvm.dbg.value(metadata i64 %3, metadata !181, metadata !DIExpression()), !dbg !178
  %4 = icmp slt i64 %3, 2, !dbg !182
  %5 = xor i1 %4, true, !dbg !182
  %6 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %0, i64 1), !dbg !183
  %7 = extractvalue { i64, i1 } %6, 0, !dbg !183
  %8 = extractvalue { i64, i1 } %6, 1, !dbg !183
  %9 = call i1 @llvm.expect.i1(i1 %8, i1 false), !dbg !183
  br i1 %9, label %14, label %10, !dbg !183

10:                                               ; preds = %entry
  %11 = icmp slt i64 %7, %3, !dbg !184
  %12 = xor i1 %11, true, !dbg !184
  %13 = call i1 @pallas.scAnd(i1 %5, i1 %12), !dbg !185
  ret i1 %13, !dbg !178

14:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !186
  unreachable, !dbg !186
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF"(i64 %0, i64 %1, i64 %2, i64 %3) #0 !dbg !188 !pallas.exprWrapper !160 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !189, metadata !DIExpression()), !dbg !190
  call void @llvm.dbg.value(metadata i64 %1, metadata !191, metadata !DIExpression()), !dbg !190
  call void @llvm.dbg.value(metadata i64 %2, metadata !192, metadata !DIExpression()), !dbg !190
  call void @llvm.dbg.value(metadata i64 %3, metadata !193, metadata !DIExpression()), !dbg !190
  %4 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %3, i64 1), !dbg !194
  %5 = extractvalue { i64, i1 } %4, 0, !dbg !194
  %6 = extractvalue { i64, i1 } %4, 1, !dbg !194
  %7 = call i1 @llvm.expect.i1(i1 %6, i1 false), !dbg !194
  br i1 %7, label %11, label %8, !dbg !194

8:                                                ; preds = %entry
  %9 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %5), !dbg !195
  %10 = icmp eq i64 %2, %9, !dbg !196
  ret i1 %10, !dbg !190

11:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !197
  unreachable, !dbg !197
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF"(i64 %0, i64 %1, i64 %2, i64 %3) #0 !dbg !198 !pallas.exprWrapper !160 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !199, metadata !DIExpression()), !dbg !200
  call void @llvm.dbg.value(metadata i64 %1, metadata !201, metadata !DIExpression()), !dbg !200
  call void @llvm.dbg.value(metadata i64 %2, metadata !202, metadata !DIExpression()), !dbg !200
  call void @llvm.dbg.value(metadata i64 %3, metadata !203, metadata !DIExpression()), !dbg !200
  %4 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %3, i64 2), !dbg !204
  %5 = extractvalue { i64, i1 } %4, 0, !dbg !204
  %6 = extractvalue { i64, i1 } %4, 1, !dbg !204
  %7 = call i1 @llvm.expect.i1(i1 %6, i1 false), !dbg !204
  br i1 %7, label %11, label %8, !dbg !204

8:                                                ; preds = %entry
  %9 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %5), !dbg !205
  %10 = icmp eq i64 %1, %9, !dbg !206
  ret i1 %10, !dbg !200

11:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !207
  unreachable, !dbg !207
}

define linkonce_odr hidden swiftcc { i64, ptr } @"$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_"() #0 !dbg !208 {
entry:
  %bitcast = alloca i64, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr %bitcast), !dbg !212
  store i64 -2305843009213693952, ptr %bitcast, align 8, !dbg !212
  %0 = load ptr, ptr %bitcast, align 8, !dbg !212
  call void @llvm.lifetime.end.p0(i64 8, ptr %bitcast), !dbg !212
  %1 = call ptr @swift_bridgeObjectRetain(ptr returned %0) #7, !dbg !212
  %2 = insertvalue { i64, ptr } { i64 0, ptr undef }, ptr %0, 1, !dbg !212
  ret { i64, ptr } %2, !dbg !212
}

; Function Attrs: noinline
declare swiftcc void @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_SSAHSus6UInt32VtF"(i64, i64, i8, i64, ptr, i64, i64, i8, i64, i32) #6

; Function Attrs: nounwind
declare ptr @swift_bridgeObjectRetain(ptr returned) #7

declare !pallas.specLib !213 i64 @pallas.result.0()

declare !pallas.specLib !214 i1 @pallas.scAnd(i1, i1)

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
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_swift_fib.swift", directory: "/home/rme/repos/vercors")
!2 = !{!3, !5, !7, !9, !11, !13}
!3 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !4, file: !1)
!4 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "examples/concepts/llvm/pallas")
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
!20 = !DIFile(filename: "tmp/source_wrappers.swift", directory: "/home/rme/repos/vercors")
!21 = !{!22, !24, !25, !27, !28, !29, !30}
!22 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !23, file: !20)
!23 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "tmp")
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
!53 = distinct !DISubprogram(name: "fibRec", linkageName: "$s13tmp_ir_source6fibRecyS2iF", scope: !4, file: !1, line: 9, type: !54, scopeLine: 9, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !59)
!54 = !DISubroutineType(types: !55)
!55 = !{!56, !56}
!56 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !6, file: !57, size: 64, elements: !58, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!57 = !DIFile(filename: "swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule", directory: "/home/rme")
!58 = !{}
!59 = !{!60}
!60 = !DILocalVariable(name: "n", arg: 1, scope: !53, file: !1, line: 9, type: !61)
!61 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !56)
!62 = !{!63, i1 true, i1 false, !65}
!63 = !{!"pallas.srcLoc", i64 5, i64 1, i64 8, i64 1, !64}
!64 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_swift_fib.swift", directory: "", checksumkind: CSK_MD5, checksum: "95eab20825b0148aa6ed575d19862c95")
!65 = !{!"pallas.requires", !66, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", !60}
!66 = !{!"pallas.srcLoc", i64 7, i64 1, i64 7, i64 16, !64}
!67 = !DILocation(line: 9, column: 13, scope: !53)
!68 = !DILocation(line: 10, column: 11, scope: !69)
!69 = distinct !DILexicalBlock(scope: !53, file: !1, line: 10, column: 5)
!70 = !DILocation(line: 11, column: 9, scope: !71)
!71 = distinct !DILexicalBlock(scope: !69, file: !1, line: 10, column: 17)
!72 = !DILocation(line: 12, column: 18, scope: !73)
!73 = distinct !DILexicalBlock(scope: !69, file: !1, line: 12, column: 12)
!74 = !DILocation(line: 13, column: 9, scope: !75)
!75 = distinct !DILexicalBlock(scope: !73, file: !1, line: 12, column: 24)
!76 = !DILocation(line: 15, column: 25, scope: !77)
!77 = distinct !DILexicalBlock(scope: !73, file: !1, line: 14, column: 12)
!78 = !DILocation(line: 15, column: 16, scope: !77)
!79 = !DILocation(line: 15, column: 41, scope: !77)
!80 = !DILocation(line: 15, column: 32, scope: !77)
!81 = !DILocation(line: 15, column: 30, scope: !77)
!82 = !DILocation(line: 15, column: 9, scope: !77)
!83 = !DILocation(line: 17, column: 1, scope: !77)
!84 = !DILocation(line: 0, scope: !85, inlinedAt: !76)
!85 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !86, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !0)
!86 = !DISubroutineType(types: null)
!87 = !DILocation(line: 0, scope: !85, inlinedAt: !79)
!88 = !DILocation(line: 0, scope: !85, inlinedAt: !81)
!89 = distinct !DISubprogram(name: "fibIt", linkageName: "$s13tmp_ir_source5fibItyS2iF", scope: !4, file: !1, line: 24, type: !54, scopeLine: 24, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !90)
!90 = !{!91, !92, !94, !96, !98}
!91 = !DILocalVariable(name: "n", arg: 1, scope: !89, file: !1, line: 24, type: !61)
!92 = !DILocalVariable(name: "prevRes", scope: !93, file: !1, line: 31, type: !56)
!93 = distinct !DILexicalBlock(scope: !89, file: !1, line: 31, column: 9)
!94 = !DILocalVariable(name: "res", scope: !95, file: !1, line: 32, type: !56)
!95 = distinct !DILexicalBlock(scope: !93, file: !1, line: 32, column: 9)
!96 = !DILocalVariable(name: "i", scope: !97, file: !1, line: 33, type: !56)
!97 = distinct !DILexicalBlock(scope: !95, file: !1, line: 33, column: 9)
!98 = !DILocalVariable(name: "tmp", scope: !99, file: !1, line: 41, type: !61)
!99 = distinct !DILexicalBlock(scope: !100, file: !1, line: 41, column: 13)
!100 = distinct !DILexicalBlock(scope: !101, file: !1, line: 40, column: 18)
!101 = distinct !DILexicalBlock(scope: !97, file: !1, line: 40, column: 5)
!102 = !{!103, i1 false, i1 false, !104, !106}
!103 = !{!"pallas.srcLoc", i64 20, i64 1, i64 23, i64 1, !64}
!104 = !{!"pallas.requires", !105, ptr @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", !91}
!105 = !{!"pallas.srcLoc", i64 21, i64 1, i64 21, i64 16, !64}
!106 = !{!"pallas.ensures", !107, ptr @"$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF", !91}
!107 = !{!"pallas.srcLoc", i64 22, i64 1, i64 22, i64 31, !64}
!108 = !DILocation(line: 31, column: 9, scope: !93)
!109 = !DILocation(line: 32, column: 9, scope: !95)
!110 = !DILocation(line: 33, column: 9, scope: !97)
!111 = !DILocation(line: 24, column: 12, scope: !89)
!112 = !DILocation(line: 25, column: 12, scope: !113)
!113 = distinct !DILexicalBlock(scope: !89, file: !1, line: 25, column: 6)
!114 = !DILocation(line: 26, column: 9, scope: !115)
!115 = distinct !DILexicalBlock(scope: !113, file: !1, line: 25, column: 18)
!116 = !DILocation(line: 27, column: 20, scope: !117)
!117 = distinct !DILexicalBlock(scope: !113, file: !1, line: 27, column: 14)
!118 = !DILocation(line: 28, column: 9, scope: !119)
!119 = distinct !DILexicalBlock(scope: !117, file: !1, line: 27, column: 26)
!120 = !DILocation(line: 0, scope: !121)
!121 = !DILexicalBlockFile(scope: !93, discriminator: 0)
!122 = !DILocation(line: 31, column: 19, scope: !123)
!123 = distinct !DILexicalBlock(scope: !89, file: !1, line: 31, column: 19)
!124 = !DILocation(line: 0, scope: !125)
!125 = !DILexicalBlockFile(scope: !95, discriminator: 0)
!126 = !DILocation(line: 32, column: 15, scope: !127)
!127 = distinct !DILexicalBlock(scope: !93, file: !1, line: 32, column: 15)
!128 = !DILocation(line: 0, scope: !129)
!129 = !DILexicalBlockFile(scope: !97, discriminator: 0)
!130 = !DILocation(line: 33, column: 13, scope: !131)
!131 = distinct !DILexicalBlock(scope: !95, file: !1, line: 33, column: 13)
!132 = !DILocation(line: 40, column: 5, scope: !101)
!133 = !DILocation(line: 40, column: 13, scope: !101)
!134 = !DILocation(line: 0, scope: !100)
!135 = !DILocation(line: 41, column: 27, scope: !136)
!136 = distinct !DILexicalBlock(scope: !100, file: !1, line: 41, column: 19)
!137 = !DILocation(line: 41, column: 13, scope: !99)
!138 = !DILocation(line: 42, column: 17, scope: !99)
!139 = !DILocation(line: 43, column: 13, scope: !99)
!140 = !DILocation(line: 44, column: 11, scope: !99)
!141 = !DILocation(line: 45, column: 5, scope: !99)
!142 = distinct !{!142, !132, !132, !143}
!143 = !{!"pallas.loopInv", !144, !145, !147, !149}
!144 = !{!"pallas.srcLoc", i64 35, i64 5, i64 39, i64 5, !64}
!145 = !{!146, ptr @"$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF", !91, !92, !94, !96}
!146 = !{!"pallas.srcLoc", i64 36, i64 5, i64 36, i64 39, !64}
!147 = !{!148, ptr @"$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF", !91, !92, !94, !96}
!148 = !{!"pallas.srcLoc", i64 37, i64 5, i64 37, i64 38, !64}
!149 = !{!150, ptr @"$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF", !91, !92, !94, !96}
!150 = !{!"pallas.srcLoc", i64 38, i64 5, i64 38, i64 42, !64}
!151 = !DILocation(line: 0, scope: !99)
!152 = !DILocation(line: 46, column: 5, scope: !97)
!153 = !DILocation(line: 47, column: 1, scope: !97)
!154 = !DILocation(line: 0, scope: !85, inlinedAt: !135)
!155 = !DILocation(line: 0, scope: !85, inlinedAt: !140)
!156 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", scope: !4, file: !1, line: 7, type: !157, scopeLine: 7, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!157 = !DISubroutineType(types: !158)
!158 = !{!159, !56}
!159 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSbD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!160 = !{!""}
!161 = !DILocalVariable(name: "n", arg: 1, scope: !156, file: !1, line: 7, type: !61)
!162 = !DILocation(line: 0, scope: !156)
!163 = !DILocation(line: 7, column: 12, scope: !156)
!164 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", scope: !4, file: !1, line: 21, type: !157, scopeLine: 21, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!165 = !DILocalVariable(name: "n", arg: 1, scope: !164, file: !1, line: 21, type: !61)
!166 = !DILocation(line: 0, scope: !164)
!167 = !DILocation(line: 21, column: 12, scope: !164)
!168 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF", scope: !4, file: !1, line: 22, type: !157, scopeLine: 22, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!169 = !DILocalVariable(name: "n", arg: 1, scope: !168, file: !1, line: 22, type: !61)
!170 = !DILocation(line: 0, scope: !168)
!171 = !DILocation(line: 22, column: 9, scope: !168)
!172 = !DILocation(line: 22, column: 22, scope: !168)
!173 = !DILocation(line: 22, column: 19, scope: !168)
!174 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF", scope: !4, file: !1, line: 36, type: !175, scopeLine: 36, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!175 = !DISubroutineType(types: !176)
!176 = !{!159, !56, !56, !56, !56}
!177 = !DILocalVariable(name: "n", arg: 1, scope: !174, file: !1, line: 36, type: !61)
!178 = !DILocation(line: 0, scope: !174)
!179 = !DILocalVariable(name: "prevRes", arg: 2, scope: !174, file: !1, line: 36, type: !61)
!180 = !DILocalVariable(name: "res", arg: 3, scope: !174, file: !1, line: 36, type: !61)
!181 = !DILocalVariable(name: "i", arg: 4, scope: !174, file: !1, line: 36, type: !61)
!182 = !DILocation(line: 36, column: 22, scope: !174)
!183 = !DILocation(line: 36, column: 37, scope: !174)
!184 = !DILocation(line: 36, column: 33, scope: !174)
!185 = !DILocation(line: 36, column: 27, scope: !174)
!186 = !DILocation(line: 0, scope: !187, inlinedAt: !183)
!187 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !86, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!188 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF", scope: !4, file: !1, line: 37, type: !175, scopeLine: 37, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!189 = !DILocalVariable(name: "n", arg: 1, scope: !188, file: !1, line: 37, type: !61)
!190 = !DILocation(line: 0, scope: !188)
!191 = !DILocalVariable(name: "prevRes", arg: 2, scope: !188, file: !1, line: 37, type: !61)
!192 = !DILocalVariable(name: "res", arg: 3, scope: !188, file: !1, line: 37, type: !61)
!193 = !DILocalVariable(name: "i", arg: 4, scope: !188, file: !1, line: 37, type: !61)
!194 = !DILocation(line: 37, column: 35, scope: !188)
!195 = !DILocation(line: 37, column: 27, scope: !188)
!196 = !DILocation(line: 37, column: 24, scope: !188)
!197 = !DILocation(line: 0, scope: !187, inlinedAt: !194)
!198 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF", scope: !4, file: !1, line: 38, type: !175, scopeLine: 38, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!199 = !DILocalVariable(name: "n", arg: 1, scope: !198, file: !1, line: 38, type: !61)
!200 = !DILocation(line: 0, scope: !198)
!201 = !DILocalVariable(name: "prevRes", arg: 2, scope: !198, file: !1, line: 38, type: !61)
!202 = !DILocalVariable(name: "res", arg: 3, scope: !198, file: !1, line: 38, type: !61)
!203 = !DILocalVariable(name: "i", arg: 4, scope: !198, file: !1, line: 38, type: !61)
!204 = !DILocation(line: 38, column: 39, scope: !198)
!205 = !DILocation(line: 38, column: 31, scope: !198)
!206 = !DILocation(line: 38, column: 28, scope: !198)
!207 = !DILocation(line: 0, scope: !187, inlinedAt: !204)
!208 = distinct !DISubprogram(linkageName: "$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_", scope: !23, file: !52, type: !209, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!209 = !DISubroutineType(types: !210)
!210 = !{!211}
!211 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSSD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!212 = !DILocation(line: 0, scope: !208)
!213 = !{!"pallas.result"}
!214 = !{!"pallas.scAnd"}
