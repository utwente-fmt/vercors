; ModuleID = './tmp/tmp_ir_source.ll'
source_filename = "tmp/tmp_ir_source.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%TSi = type <{ i64 }>

@"\01l_entry_point" = private constant { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @main to i64), i64 ptrtoint (ptr @"\01l_entry_point" to i64)) to i32), i32 0 }, section "swift5_entry", align 4
@_swift1_autolink_entries = private constant [102 x i8] c"-lswiftSwiftOnoneSupport\00-lswiftCore\00-lswift_Concurrency\00-lswift_StringProcessing\00-lswift_RegexParser\00", section ".swift1_autolink_entries", no_sanitize_address, align 8
@llvm.used = appending global [12 x ptr] [ptr @main, ptr @"$s13tmp_ir_source6fibRecyS2iF", ptr @"$s13tmp_ir_source5fibItyS2iF", ptr @"\01l_entry_point", ptr @__swift_reflection_version, ptr @_swift1_autolink_entries, ptr @"$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF"], section "llvm.metadata"
@".str.35.tmp_ir_source/source_wrappers.swift" = private unnamed_addr constant [36 x i8] c"tmp_ir_source/source_wrappers.swift\00"
@".str.11.Fatal error" = private unnamed_addr constant [12 x i8] c"Fatal error\00"
@__swift_reflection_version = linkonce_odr hidden constant i16 3

define protected i32 @main(i32 %0, ptr %1) #0 !dbg !45 {
entry:
  ret i32 0, !dbg !50
}

define hidden swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %0) #0 !dbg !53 !pallas.fcontract !62 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !60, metadata !DIExpression()), !dbg !66
  %1 = icmp eq i64 %0, 0, !dbg !67
  br i1 %1, label %2, label %3, !dbg !67

2:                                                ; preds = %entry
  br label %24, !dbg !69

3:                                                ; preds = %entry
  %4 = icmp eq i64 %0, 1, !dbg !71
  br i1 %4, label %5, label %6, !dbg !71

5:                                                ; preds = %3
  br label %24, !dbg !73

6:                                                ; preds = %3
  %7 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %0, i64 1), !dbg !75
  %8 = extractvalue { i64, i1 } %7, 0, !dbg !75
  %9 = extractvalue { i64, i1 } %7, 1, !dbg !75
  %10 = call i1 @llvm.expect.i1(i1 %9, i1 false), !dbg !75
  br i1 %10, label %26, label %11, !dbg !75

11:                                               ; preds = %6
  %12 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %8), !dbg !77
  %13 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %0, i64 2), !dbg !78
  %14 = extractvalue { i64, i1 } %13, 0, !dbg !78
  %15 = extractvalue { i64, i1 } %13, 1, !dbg !78
  %16 = call i1 @llvm.expect.i1(i1 %15, i1 false), !dbg !78
  br i1 %16, label %27, label %17, !dbg !78

17:                                               ; preds = %11
  %18 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %14), !dbg !79
  %19 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %12, i64 %18), !dbg !80
  %20 = extractvalue { i64, i1 } %19, 0, !dbg !80
  %21 = extractvalue { i64, i1 } %19, 1, !dbg !80
  %22 = call i1 @llvm.expect.i1(i1 %21, i1 false), !dbg !80
  br i1 %22, label %28, label %23, !dbg !80

23:                                               ; preds = %17
  br label %24, !dbg !81

24:                                               ; preds = %23, %5, %2
  %25 = phi i64 [ %20, %23 ], [ 1, %5 ], [ 0, %2 ], !dbg !82
  ret i64 %25, !dbg !82

26:                                               ; preds = %6
  call void @llvm.trap(), !dbg !83
  unreachable, !dbg !83

27:                                               ; preds = %11
  call void @llvm.trap(), !dbg !86
  unreachable, !dbg !86

28:                                               ; preds = %17
  call void @llvm.trap(), !dbg !87
  unreachable, !dbg !87
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

define hidden swiftcc i64 @"$s13tmp_ir_source5fibItyS2iF"(i64 %0) #0 !dbg !88 !pallas.fcontract !101 {
entry:
  %1 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %1, metadata !91, metadata !DIExpression()), !dbg !107
  call void @llvm.memset.p0.i64(ptr align 8 %1, i8 0, i64 8, i1 false)
  %2 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !93, metadata !DIExpression()), !dbg !108
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  %3 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !95, metadata !DIExpression()), !dbg !109
  call void @llvm.memset.p0.i64(ptr align 8 %3, i8 0, i64 8, i1 false)
  call void @llvm.dbg.value(metadata i64 %0, metadata !90, metadata !DIExpression()), !dbg !110
  %4 = icmp eq i64 %0, 0, !dbg !111
  br i1 %4, label %5, label %6, !dbg !111

5:                                                ; preds = %entry
  br label %31, !dbg !113

6:                                                ; preds = %entry
  %7 = icmp eq i64 %0, 1, !dbg !115
  br i1 %7, label %8, label %9, !dbg !115

8:                                                ; preds = %6
  br label %31, !dbg !117

9:                                                ; preds = %6
  call void @llvm.lifetime.start.p0(i64 8, ptr %1), !dbg !119
  %._value = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !121
  store i64 0, ptr %._value, align 8, !dbg !121
  call void @llvm.lifetime.start.p0(i64 8, ptr %2), !dbg !123
  %._value1 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !125
  store i64 1, ptr %._value1, align 8, !dbg !125
  call void @llvm.lifetime.start.p0(i64 8, ptr %3), !dbg !127
  %._value2 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !129
  store i64 2, ptr %._value2, align 8, !dbg !129
  br label %10, !dbg !131

10:                                               ; preds = %28, %9
  %._value3 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !131
  %11 = load i64, ptr %._value3, align 8, !dbg !131
  %12 = icmp slt i64 %0, %11, !dbg !132
  %13 = xor i1 %12, true, !dbg !132
  br i1 %13, label %14, label %29, !dbg !132

14:                                               ; preds = %10
  %._value5 = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !133
  %15 = load i64, ptr %._value5, align 8, !dbg !133
  %._value6 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !133
  %16 = load i64, ptr %._value6, align 8, !dbg !133
  %17 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %15, i64 %16), !dbg !134
  %18 = extractvalue { i64, i1 } %17, 0, !dbg !134
  %19 = extractvalue { i64, i1 } %17, 1, !dbg !134
  %20 = call i1 @llvm.expect.i1(i1 %19, i1 false), !dbg !134
  br i1 %20, label %33, label %21, !dbg !134

21:                                               ; preds = %14
  call void @llvm.dbg.value(metadata i64 %18, metadata !97, metadata !DIExpression()), !dbg !136
  %._value7 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !137
  %22 = load i64, ptr %._value7, align 8, !dbg !137
  %._value8 = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !137
  store i64 %22, ptr %._value8, align 8, !dbg !137
  %._value9 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !138
  store i64 %18, ptr %._value9, align 8, !dbg !138
  %._value10 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !139
  %23 = load i64, ptr %._value10, align 8, !dbg !139
  %24 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %23, i64 1), !dbg !139
  %25 = extractvalue { i64, i1 } %24, 0, !dbg !139
  %26 = extractvalue { i64, i1 } %24, 1, !dbg !139
  %27 = call i1 @llvm.expect.i1(i1 %26, i1 false), !dbg !139
  br i1 %27, label %34, label %28, !dbg !139

28:                                               ; preds = %21
  %._value11 = getelementptr inbounds %TSi, ptr %3, i32 0, i32 0, !dbg !139
  store i64 %25, ptr %._value11, align 8, !dbg !139
  br label %10, !dbg !140, !llvm.loop !141

29:                                               ; preds = %10
  %._value4 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !150
  %30 = load i64, ptr %._value4, align 8, !dbg !150
  call void @llvm.lifetime.end.p0(i64 8, ptr %3), !dbg !151
  call void @llvm.lifetime.end.p0(i64 8, ptr %2), !dbg !151
  call void @llvm.lifetime.end.p0(i64 8, ptr %1), !dbg !151
  br label %31, !dbg !151

31:                                               ; preds = %29, %8, %5
  %32 = phi i64 [ %30, %29 ], [ 1, %8 ], [ 0, %5 ], !dbg !152
  ret i64 %32, !dbg !152

33:                                               ; preds = %14
  call void @llvm.trap(), !dbg !153
  unreachable, !dbg !153

34:                                               ; preds = %21
  call void @llvm.trap(), !dbg !154
  unreachable, !dbg !154
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #4

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #5

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #4

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF"(i64 %0) #0 !dbg !155 !pallas.exprWrapper !159 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !160, metadata !DIExpression()), !dbg !161
  %1 = call i64 @pallas.result.0(), !dbg !162
  %2 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %0), !dbg !163
  %3 = icmp eq i64 %1, %2, !dbg !164
  ret i1 %3, !dbg !161
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF"(i64 %0) #0 !dbg !165 !pallas.exprWrapper !159 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !166, metadata !DIExpression()), !dbg !167
  %1 = icmp slt i64 %0, 0, !dbg !168
  %2 = xor i1 %1, true, !dbg !168
  ret i1 %2, !dbg !167
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF"(i64 %0) #0 !dbg !169 !pallas.exprWrapper !159 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !170, metadata !DIExpression()), !dbg !171
  %1 = icmp slt i64 %0, 0, !dbg !172
  %2 = xor i1 %1, true, !dbg !172
  ret i1 %2, !dbg !171
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF"(i64 %0, i64 %1, i64 %2, i64 %3) #0 !dbg !173 !pallas.exprWrapper !159 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !176, metadata !DIExpression()), !dbg !177
  call void @llvm.dbg.value(metadata i64 %1, metadata !178, metadata !DIExpression()), !dbg !177
  call void @llvm.dbg.value(metadata i64 %2, metadata !179, metadata !DIExpression()), !dbg !177
  call void @llvm.dbg.value(metadata i64 %3, metadata !180, metadata !DIExpression()), !dbg !177
  %4 = icmp slt i64 %3, 2, !dbg !181
  %5 = xor i1 %4, true, !dbg !181
  %6 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %0, i64 1), !dbg !182
  %7 = extractvalue { i64, i1 } %6, 0, !dbg !182
  %8 = extractvalue { i64, i1 } %6, 1, !dbg !182
  %9 = call i1 @llvm.expect.i1(i1 %8, i1 false), !dbg !182
  br i1 %9, label %14, label %10, !dbg !182

10:                                               ; preds = %entry
  %11 = icmp slt i64 %7, %3, !dbg !183
  %12 = xor i1 %11, true, !dbg !183
  %13 = call i1 @pallas.scAnd(i1 %5, i1 %12), !dbg !184
  ret i1 %13, !dbg !177

14:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !185
  unreachable, !dbg !185
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF"(i64 %0, i64 %1, i64 %2, i64 %3) #0 !dbg !187 !pallas.exprWrapper !159 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !188, metadata !DIExpression()), !dbg !189
  call void @llvm.dbg.value(metadata i64 %1, metadata !190, metadata !DIExpression()), !dbg !189
  call void @llvm.dbg.value(metadata i64 %2, metadata !191, metadata !DIExpression()), !dbg !189
  call void @llvm.dbg.value(metadata i64 %3, metadata !192, metadata !DIExpression()), !dbg !189
  %4 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %3, i64 1), !dbg !193
  %5 = extractvalue { i64, i1 } %4, 0, !dbg !193
  %6 = extractvalue { i64, i1 } %4, 1, !dbg !193
  %7 = call i1 @llvm.expect.i1(i1 %6, i1 false), !dbg !193
  br i1 %7, label %11, label %8, !dbg !193

8:                                                ; preds = %entry
  %9 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %5), !dbg !194
  %10 = icmp eq i64 %1, %9, !dbg !195
  ret i1 %10, !dbg !189

11:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !196
  unreachable, !dbg !196
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF"(i64 %0, i64 %1, i64 %2, i64 %3) #0 !dbg !197 !pallas.exprWrapper !159 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !198, metadata !DIExpression()), !dbg !199
  call void @llvm.dbg.value(metadata i64 %1, metadata !200, metadata !DIExpression()), !dbg !199
  call void @llvm.dbg.value(metadata i64 %2, metadata !201, metadata !DIExpression()), !dbg !199
  call void @llvm.dbg.value(metadata i64 %3, metadata !202, metadata !DIExpression()), !dbg !199
  %4 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %3, i64 2), !dbg !203
  %5 = extractvalue { i64, i1 } %4, 0, !dbg !203
  %6 = extractvalue { i64, i1 } %4, 1, !dbg !203
  %7 = call i1 @llvm.expect.i1(i1 %6, i1 false), !dbg !203
  br i1 %7, label %11, label %8, !dbg !203

8:                                                ; preds = %entry
  %9 = call swiftcc i64 @"$s13tmp_ir_source6fibRecyS2iF"(i64 %5), !dbg !204
  %10 = icmp eq i64 %2, %9, !dbg !205
  ret i1 %10, !dbg !199

11:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !206
  unreachable, !dbg !206
}

define linkonce_odr hidden swiftcc { i64, ptr } @"$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_"() #0 !dbg !207 {
entry:
  %bitcast = alloca i64, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr %bitcast), !dbg !211
  store i64 -2305843009213693952, ptr %bitcast, align 8, !dbg !211
  %0 = load ptr, ptr %bitcast, align 8, !dbg !211
  call void @llvm.lifetime.end.p0(i64 8, ptr %bitcast), !dbg !211
  %1 = call ptr @swift_bridgeObjectRetain(ptr returned %0) #7, !dbg !211
  %2 = insertvalue { i64, ptr } { i64 0, ptr undef }, ptr %0, 1, !dbg !211
  ret { i64, ptr } %2, !dbg !211
}

; Function Attrs: noinline
declare swiftcc void @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_SSAHSus6UInt32VtF"(i64, i64, i8, i64, ptr, i64, i64, i8, i64, i32) #6

; Function Attrs: nounwind
declare ptr @swift_bridgeObjectRetain(ptr returned) #7

declare !pallas.specLib !212 i64 @pallas.result.0()

declare !pallas.specLib !213 i1 @pallas.scAnd(i1, i1)

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
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_swift_fib_fail.swift", directory: ".")
!2 = !{!3, !5, !7, !9, !11, !13}
!3 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !4, file: !1)
!4 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "examples/concepts/llvm/pallas")
!5 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !6, file: !1)
!6 = !DIModule(scope: null, name: "Swift", includePath: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!7 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !8, file: !1)
!8 = !DIModule(scope: null, name: "_StringProcessing", includePath: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/_StringProcessing.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!9 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !10, file: !1)
!10 = !DIModule(scope: null, name: "_SwiftConcurrencyShims", includePath: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/shims")
!11 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !12, file: !1)
!12 = !DIModule(scope: null, name: "_Concurrency", includePath: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/_Concurrency.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!13 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !14, file: !1)
!14 = !DIModule(scope: null, name: "SwiftOnoneSupport", includePath: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/SwiftOnoneSupport.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!15 = distinct !DICompileUnit(language: DW_LANG_C11, file: !16, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!16 = !DIFile(filename: "<swift-imported-modules>", directory: ".")
!17 = distinct !DICompileUnit(language: DW_LANG_C99, file: !18, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "/home/USERNAME/.cache/clang/ModuleCache/1T7NA3LBRX57T/_SwiftConcurrencyShims-16QL5XP1HZ73F.pcm", emissionKind: FullDebug, dwoId: 205401482013525099)
!18 = !DIFile(filename: "_SwiftConcurrencyShims", directory: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/shims")
!19 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !20, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !21)
!20 = !DIFile(filename: "tmp/source_wrappers.swift", directory: ".")
!21 = !{!22, !24, !25, !27, !28, !29, !30}
!22 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !23, file: !20)
!23 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "tmp")
!24 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !6, file: !20)
!25 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !26, file: !20, line: 1)
!26 = !DIModule(scope: null, name: "PallasSpec", includePath: "./../pallas_spec2ir/res/spec_libs/swift/PallasSpec/.build/debug/Modules/PallasSpec.swiftmodule")
!27 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !8, file: !20)
!28 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !10, file: !20)
!29 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !12, file: !20)
!30 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !14, file: !20)
!31 = distinct !DICompileUnit(language: DW_LANG_C11, file: !16, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!32 = distinct !DICompileUnit(language: DW_LANG_C99, file: !18, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "/home/USERNAME/.cache/clang/ModuleCache/1T7NA3LBRX57T/_SwiftConcurrencyShims-16QL5XP1HZ73F.pcm", emissionKind: FullDebug, dwoId: 205401482013525099)
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
!62 = !{!63, i1 true, !64}
!63 = !{!"pallas.srcLoc", i64 6, i64 1, i64 9, i64 1}
!64 = !{!"pallas.requires", !65, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", !60}
!65 = !{!"pallas.srcLoc", i64 8, i64 1, i64 8, i64 16}
!66 = !DILocation(line: 10, column: 13, scope: !53)
!67 = !DILocation(line: 11, column: 11, scope: !68)
!68 = distinct !DILexicalBlock(scope: !53, file: !1, line: 11, column: 5)
!69 = !DILocation(line: 12, column: 9, scope: !70)
!70 = distinct !DILexicalBlock(scope: !68, file: !1, line: 11, column: 17)
!71 = !DILocation(line: 13, column: 18, scope: !72)
!72 = distinct !DILexicalBlock(scope: !68, file: !1, line: 13, column: 12)
!73 = !DILocation(line: 14, column: 9, scope: !74)
!74 = distinct !DILexicalBlock(scope: !72, file: !1, line: 13, column: 24)
!75 = !DILocation(line: 16, column: 25, scope: !76)
!76 = distinct !DILexicalBlock(scope: !72, file: !1, line: 15, column: 12)
!77 = !DILocation(line: 16, column: 16, scope: !76)
!78 = !DILocation(line: 16, column: 41, scope: !76)
!79 = !DILocation(line: 16, column: 32, scope: !76)
!80 = !DILocation(line: 16, column: 30, scope: !76)
!81 = !DILocation(line: 16, column: 9, scope: !76)
!82 = !DILocation(line: 18, column: 1, scope: !76)
!83 = !DILocation(line: 0, scope: !84, inlinedAt: !75)
!84 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !85, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !0)
!85 = !DISubroutineType(types: null)
!86 = !DILocation(line: 0, scope: !84, inlinedAt: !78)
!87 = !DILocation(line: 0, scope: !84, inlinedAt: !80)
!88 = distinct !DISubprogram(name: "fibIt", linkageName: "$s13tmp_ir_source5fibItyS2iF", scope: !4, file: !1, line: 25, type: !54, scopeLine: 25, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !89)
!89 = !{!90, !91, !93, !95, !97}
!90 = !DILocalVariable(name: "n", arg: 1, scope: !88, file: !1, line: 25, type: !61)
!91 = !DILocalVariable(name: "prevRes", scope: !92, file: !1, line: 32, type: !56)
!92 = distinct !DILexicalBlock(scope: !88, file: !1, line: 32, column: 9)
!93 = !DILocalVariable(name: "res", scope: !94, file: !1, line: 33, type: !56)
!94 = distinct !DILexicalBlock(scope: !92, file: !1, line: 33, column: 9)
!95 = !DILocalVariable(name: "i", scope: !96, file: !1, line: 34, type: !56)
!96 = distinct !DILexicalBlock(scope: !94, file: !1, line: 34, column: 9)
!97 = !DILocalVariable(name: "tmp", scope: !98, file: !1, line: 42, type: !61)
!98 = distinct !DILexicalBlock(scope: !99, file: !1, line: 42, column: 13)
!99 = distinct !DILexicalBlock(scope: !100, file: !1, line: 41, column: 18)
!100 = distinct !DILexicalBlock(scope: !96, file: !1, line: 41, column: 5)
!101 = !{!102, i1 false, !103, !105}
!102 = !{!"pallas.srcLoc", i64 21, i64 1, i64 24, i64 1}
!103 = !{!"pallas.requires", !104, ptr @"$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", !90}
!104 = !{!"pallas.srcLoc", i64 22, i64 1, i64 22, i64 16}
!105 = !{!"pallas.ensures", !106, ptr @"$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF", !90}
!106 = !{!"pallas.srcLoc", i64 23, i64 1, i64 23, i64 31}
!107 = !DILocation(line: 32, column: 9, scope: !92)
!108 = !DILocation(line: 33, column: 9, scope: !94)
!109 = !DILocation(line: 34, column: 9, scope: !96)
!110 = !DILocation(line: 25, column: 12, scope: !88)
!111 = !DILocation(line: 26, column: 12, scope: !112)
!112 = distinct !DILexicalBlock(scope: !88, file: !1, line: 26, column: 6)
!113 = !DILocation(line: 27, column: 9, scope: !114)
!114 = distinct !DILexicalBlock(scope: !112, file: !1, line: 26, column: 18)
!115 = !DILocation(line: 28, column: 20, scope: !116)
!116 = distinct !DILexicalBlock(scope: !112, file: !1, line: 28, column: 14)
!117 = !DILocation(line: 29, column: 9, scope: !118)
!118 = distinct !DILexicalBlock(scope: !116, file: !1, line: 28, column: 26)
!119 = !DILocation(line: 0, scope: !120)
!120 = !DILexicalBlockFile(scope: !92, discriminator: 0)
!121 = !DILocation(line: 32, column: 19, scope: !122)
!122 = distinct !DILexicalBlock(scope: !88, file: !1, line: 32, column: 19)
!123 = !DILocation(line: 0, scope: !124)
!124 = !DILexicalBlockFile(scope: !94, discriminator: 0)
!125 = !DILocation(line: 33, column: 15, scope: !126)
!126 = distinct !DILexicalBlock(scope: !92, file: !1, line: 33, column: 15)
!127 = !DILocation(line: 0, scope: !128)
!128 = !DILexicalBlockFile(scope: !96, discriminator: 0)
!129 = !DILocation(line: 34, column: 13, scope: !130)
!130 = distinct !DILexicalBlock(scope: !94, file: !1, line: 34, column: 13)
!131 = !DILocation(line: 41, column: 5, scope: !100)
!132 = !DILocation(line: 41, column: 13, scope: !100)
!133 = !DILocation(line: 0, scope: !99)
!134 = !DILocation(line: 42, column: 27, scope: !135)
!135 = distinct !DILexicalBlock(scope: !99, file: !1, line: 42, column: 19)
!136 = !DILocation(line: 42, column: 13, scope: !98)
!137 = !DILocation(line: 43, column: 17, scope: !98)
!138 = !DILocation(line: 44, column: 13, scope: !98)
!139 = !DILocation(line: 45, column: 11, scope: !98)
!140 = !DILocation(line: 46, column: 5, scope: !98)
!141 = distinct !{!141, !131, !131, !142}
!142 = !{!"pallas.loopInv", !143, !144, !146, !148}
!143 = !{!"pallas.srcLoc", i64 36, i64 5, i64 40, i64 5}
!144 = !{!145, ptr @"$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF", !90, !91, !93, !95}
!145 = !{!"pallas.srcLoc", i64 37, i64 5, i64 37, i64 39}
!146 = !{!147, ptr @"$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF", !90, !91, !93, !95}
!147 = !{!"pallas.srcLoc", i64 38, i64 5, i64 38, i64 38}
!148 = !{!149, ptr @"$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF", !90, !91, !93, !95}
!149 = !{!"pallas.srcLoc", i64 39, i64 5, i64 39, i64 42}
!150 = !DILocation(line: 0, scope: !98)
!151 = !DILocation(line: 47, column: 5, scope: !96)
!152 = !DILocation(line: 48, column: 1, scope: !96)
!153 = !DILocation(line: 0, scope: !84, inlinedAt: !134)
!154 = !DILocation(line: 0, scope: !84, inlinedAt: !139)
!155 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_21nSbSi_tF", scope: !4, file: !1, line: 23, type: !156, scopeLine: 23, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!156 = !DISubroutineType(types: !157)
!157 = !{!158, !56}
!158 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSbD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!159 = !{!""}
!160 = !DILocalVariable(name: "n", arg: 1, scope: !155, file: !1, line: 23, type: !61)
!161 = !DILocation(line: 0, scope: !155)
!162 = !DILocation(line: 23, column: 9, scope: !155)
!163 = !DILocation(line: 23, column: 22, scope: !155)
!164 = !DILocation(line: 23, column: 19, scope: !155)
!165 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_01nSbSi_tF", scope: !4, file: !1, line: 8, type: !156, scopeLine: 8, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!166 = !DILocalVariable(name: "n", arg: 1, scope: !165, file: !1, line: 8, type: !61)
!167 = !DILocation(line: 0, scope: !165)
!168 = !DILocation(line: 8, column: 12, scope: !165)
!169 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_11nSbSi_tF", scope: !4, file: !1, line: 22, type: !156, scopeLine: 22, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!170 = !DILocalVariable(name: "n", arg: 1, scope: !169, file: !1, line: 22, type: !61)
!171 = !DILocation(line: 0, scope: !169)
!172 = !DILocation(line: 22, column: 12, scope: !169)
!173 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_31n7prevRes3res1iSbSi_S3itF", scope: !4, file: !1, line: 37, type: !174, scopeLine: 37, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!174 = !DISubroutineType(types: !175)
!175 = !{!158, !56, !56, !56, !56}
!176 = !DILocalVariable(name: "n", arg: 1, scope: !173, file: !1, line: 37, type: !61)
!177 = !DILocation(line: 0, scope: !173)
!178 = !DILocalVariable(name: "prevRes", arg: 2, scope: !173, file: !1, line: 37, type: !61)
!179 = !DILocalVariable(name: "res", arg: 3, scope: !173, file: !1, line: 37, type: !61)
!180 = !DILocalVariable(name: "i", arg: 4, scope: !173, file: !1, line: 37, type: !61)
!181 = !DILocation(line: 37, column: 22, scope: !173)
!182 = !DILocation(line: 37, column: 37, scope: !173)
!183 = !DILocation(line: 37, column: 33, scope: !173)
!184 = !DILocation(line: 37, column: 27, scope: !173)
!185 = !DILocation(line: 0, scope: !186, inlinedAt: !182)
!186 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !85, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!187 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_51n7prevRes3res1iSbSi_S3itF", scope: !4, file: !1, line: 39, type: !174, scopeLine: 39, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!188 = !DILocalVariable(name: "n", arg: 1, scope: !187, file: !1, line: 39, type: !61)
!189 = !DILocation(line: 0, scope: !187)
!190 = !DILocalVariable(name: "prevRes", arg: 2, scope: !187, file: !1, line: 39, type: !61)
!191 = !DILocalVariable(name: "res", arg: 3, scope: !187, file: !1, line: 39, type: !61)
!192 = !DILocalVariable(name: "i", arg: 4, scope: !187, file: !1, line: 39, type: !61)
!193 = !DILocation(line: 39, column: 39, scope: !187)
!194 = !DILocation(line: 39, column: 31, scope: !187)
!195 = !DILocation(line: 39, column: 28, scope: !187)
!196 = !DILocation(line: 0, scope: !186, inlinedAt: !193)
!197 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_41n7prevRes3res1iSbSi_S3itF", scope: !4, file: !1, line: 38, type: !174, scopeLine: 38, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!198 = !DILocalVariable(name: "n", arg: 1, scope: !197, file: !1, line: 38, type: !61)
!199 = !DILocation(line: 0, scope: !197)
!200 = !DILocalVariable(name: "prevRes", arg: 2, scope: !197, file: !1, line: 38, type: !61)
!201 = !DILocalVariable(name: "res", arg: 3, scope: !197, file: !1, line: 38, type: !61)
!202 = !DILocalVariable(name: "i", arg: 4, scope: !197, file: !1, line: 38, type: !61)
!203 = !DILocation(line: 38, column: 35, scope: !197)
!204 = !DILocation(line: 38, column: 27, scope: !197)
!205 = !DILocation(line: 38, column: 24, scope: !197)
!206 = !DILocation(line: 0, scope: !186, inlinedAt: !203)
!207 = distinct !DISubprogram(linkageName: "$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_", scope: !23, file: !52, type: !208, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!208 = !DISubroutineType(types: !209)
!209 = !{!210}
!210 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSSD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!211 = !DILocation(line: 0, scope: !207)
!212 = !{!"pallas.result"}
!213 = !{!"pallas.scAnd"}
