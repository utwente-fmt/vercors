; ModuleID = 'tmp/tmp_ir_source.ll'
source_filename = "tmp/tmp_ir_source.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%TSi = type <{ i64 }>

@"\01l_entry_point" = private constant { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @main to i64), i64 ptrtoint (ptr @"\01l_entry_point" to i64)) to i32), i32 0 }, section "swift5_entry", align 4
@_swift1_autolink_entries = private constant [102 x i8] c"-lswiftSwiftOnoneSupport\00-lswiftCore\00-lswift_Concurrency\00-lswift_StringProcessing\00-lswift_RegexParser\00", section ".swift1_autolink_entries", no_sanitize_address, align 8
@llvm.used = appending global [21 x ptr] [ptr @main, ptr @"$s13tmp_ir_source7get_maxyS2i_SitF", ptr @"$s13tmp_ir_source3runyyF", ptr @"\01l_entry_point", ptr @__swift_reflection_version, ptr @_swift1_autolink_entries, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01a1b1x9both_gt_x3minSbSi_S2iSbSitF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_11a1b1x9both_gt_x3minSbSi_S2iSbSitF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_21a1b1x9both_gt_x3minSbSi_S2iSbSitF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_33min7both_gtSbSi_SbtF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_43min7both_gtSbSi_SbtF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_51a1b1x9both_gt_x3minS2i_S2iSbSitF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_61a1b1x9both_gt_x3minSbSi_S2iSbSitF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_71a1b3max3min7both_gtSbSi_S3iSbtF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_81a1b3max3min7both_gtSbSi_S3iSbtF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_91a1b3max3min7both_gtSbSi_S3iSbtF", ptr @"$s13tmp_ir_source14PALLAS_SPEC_101a1b3max3min7both_gtSbSi_S3iSbtF", ptr @"$s13tmp_ir_source14PALLAS_SPEC_111a1b3min7both_gtS2i_S2iSbtF", ptr @"$s13tmp_ir_source14PALLAS_SPEC_121a1b3max3min7both_gtS2i_S3iSbtF", ptr @"$s13tmp_ir_source5isMaxySbSi_S2itF", ptr @"$s13tmp_ir_source6my_minyS2i_SitF"], section "llvm.metadata"
@".str.35.tmp_ir_source/source_wrappers.swift" = private unnamed_addr constant [36 x i8] c"tmp_ir_source/source_wrappers.swift\00"
@".str.11.Fatal error" = private unnamed_addr constant [12 x i8] c"Fatal error\00"
@__swift_reflection_version = linkonce_odr hidden constant i16 3

define protected i32 @main(i32 %0, ptr %1) #0 !dbg !48 {
entry:
  ret i32 0, !dbg !53
}

define hidden swiftcc i64 @"$s13tmp_ir_source7get_maxyS2i_SitF"(i64 %0, i64 %1) #0 !dbg !56 !pallas.fcontract !66 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !63, metadata !DIExpression()), !dbg !129
  call void @llvm.dbg.value(metadata i64 %1, metadata !65, metadata !DIExpression()), !dbg !130
  %2 = icmp slt i64 %1, %0, !dbg !131, !pallas.stmntBlock !133
  br i1 %2, label %3, label %4, !dbg !131

3:                                                ; preds = %entry
  br label %5, !dbg !169

4:                                                ; preds = %entry
  br label %5, !dbg !171

5:                                                ; preds = %4, %3
  %6 = phi i64 [ %1, %4 ], [ %0, %3 ], !dbg !172
  ret i64 %6, !dbg !172
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

define hidden swiftcc void @"$s13tmp_ir_source3runyyF"() #0 !dbg !173 !pallas.fcontract !184 {
entry:
  %0 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %0, metadata !178, metadata !DIExpression()), !dbg !209
  call void @llvm.memset.p0.i64(ptr align 8 %0, i8 0, i64 8, i1 false)
  %1 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %1, metadata !180, metadata !DIExpression()), !dbg !210
  call void @llvm.memset.p0.i64(ptr align 8 %1, i8 0, i64 8, i1 false)
  call void @llvm.lifetime.start.p0(i64 8, ptr %0), !dbg !211
  %._value = getelementptr inbounds %TSi, ptr %0, i32 0, i32 0, !dbg !213
  store i64 1, ptr %._value, align 8, !dbg !213
  call void @llvm.lifetime.start.p0(i64 8, ptr %1), !dbg !215
  %._value1 = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !217
  store i64 42, ptr %._value1, align 8, !dbg !217
  %2 = call swiftcc i64 @"$s13tmp_ir_source7get_maxyS2i_SitF"(i64 1, i64 42), !dbg !219, !pallas.givenBindings !221, !pallas.yieldsBindings !238
  call void @llvm.dbg.value(metadata i64 %2, metadata !182, metadata !DIExpression()), !dbg !244
  %._value2 = getelementptr inbounds %TSi, ptr %0, i32 0, i32 0, !dbg !245, !pallas.stmntBlock !246
  store i64 -1, ptr %._value2, align 8, !dbg !245
  %._value3 = getelementptr inbounds %TSi, ptr %1, i32 0, i32 0, !dbg !295
  store i64 84, ptr %._value3, align 8, !dbg !295
  %3 = call swiftcc i64 @"$s13tmp_ir_source7get_maxyS2i_SitF"(i64 -1, i64 84), !dbg !296, !pallas.givenBindings !297, !pallas.yieldsBindings !316
  call void @llvm.lifetime.end.p0(i64 8, ptr %1), !dbg !320, !pallas.stmntBlock !321
  call void @llvm.lifetime.end.p0(i64 8, ptr %0), !dbg !320
  ret void, !dbg !320
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #2

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #3

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #2

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_01a1b1x9both_gt_x3minSbSi_S2iSbSitF"(i64 %0, i64 %1, i64 %2, i1 %3, i64 %4) #0 !dbg !82 !pallas.exprWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !94, metadata !DIExpression()), !dbg !339
  call void @llvm.dbg.value(metadata i64 %1, metadata !96, metadata !DIExpression()), !dbg !339
  call void @llvm.dbg.value(metadata i64 %2, metadata !81, metadata !DIExpression()), !dbg !339
  call void @llvm.dbg.value(metadata i1 %3, metadata !88, metadata !DIExpression()), !dbg !339
  call void @llvm.dbg.value(metadata i64 %4, metadata !91, metadata !DIExpression()), !dbg !339
  %5 = call i64 @"pallas.result i64"(), !dbg !340
  %6 = call swiftcc i1 @"$s13tmp_ir_source5isMaxySbSi_S2itF"(i64 %5, i64 %0, i64 %1), !dbg !341
  ret i1 %6, !dbg !339
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_11a1b1x9both_gt_x3minSbSi_S2iSbSitF"(i64 %0, i64 %1, i64 %2, i1 %3, i64 %4) #0 !dbg !102 !pallas.exprWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !110, metadata !DIExpression()), !dbg !342
  call void @llvm.dbg.value(metadata i64 %1, metadata !112, metadata !DIExpression()), !dbg !342
  call void @llvm.dbg.value(metadata i64 %2, metadata !101, metadata !DIExpression()), !dbg !342
  call void @llvm.dbg.value(metadata i1 %3, metadata !105, metadata !DIExpression()), !dbg !342
  call void @llvm.dbg.value(metadata i64 %4, metadata !107, metadata !DIExpression()), !dbg !342
  %5 = call swiftcc i64 @"$s13tmp_ir_source6my_minyS2i_SitF"(i64 %0, i64 %1), !dbg !343
  %6 = icmp eq i64 %4, %5, !dbg !344
  ret i1 %6, !dbg !342
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_21a1b1x9both_gt_x3minSbSi_S2iSbSitF"(i64 %0, i64 %1, i64 %2, i1 %3, i64 %4) #0 !dbg !118 !pallas.exprWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !126, metadata !DIExpression()), !dbg !345
  call void @llvm.dbg.value(metadata i64 %1, metadata !128, metadata !DIExpression()), !dbg !345
  call void @llvm.dbg.value(metadata i64 %2, metadata !117, metadata !DIExpression()), !dbg !345
  call void @llvm.dbg.value(metadata i1 %3, metadata !121, metadata !DIExpression()), !dbg !345
  call void @llvm.dbg.value(metadata i64 %4, metadata !123, metadata !DIExpression()), !dbg !345
  %5 = icmp slt i64 %2, %0, !dbg !346
  br i1 %5, label %6, label %8, !dbg !347

6:                                                ; preds = %entry
  %7 = icmp slt i64 %2, %1, !dbg !347
  br label %9, !dbg !347

8:                                                ; preds = %entry
  br label %9, !dbg !347

9:                                                ; preds = %8, %6
  %10 = phi i1 [ false, %8 ], [ %7, %6 ], !dbg !348
  %11 = icmp eq i1 %3, %10, !dbg !348
  ret i1 %11, !dbg !345
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_33min7both_gtSbSi_SbtF"(i64 %0, i1 %1) #0 !dbg !196 !pallas.exprWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !195, metadata !DIExpression()), !dbg !349
  call void @llvm.dbg.value(metadata i1 %1, metadata !200, metadata !DIExpression()), !dbg !349
  br i1 %1, label %2, label %3, !dbg !350

2:                                                ; preds = %entry
  br label %5, !dbg !350

3:                                                ; preds = %entry
  %4 = xor i1 %1, true, !dbg !350
  br label %5, !dbg !350

5:                                                ; preds = %3, %2
  %6 = phi i1 [ %4, %3 ], [ true, %2 ], !dbg !349
  ret i1 %6, !dbg !349
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_43min7both_gtSbSi_SbtF"(i64 %0, i1 %1) #0 !dbg !206 !pallas.exprWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !205, metadata !DIExpression()), !dbg !351
  call void @llvm.dbg.value(metadata i1 %1, metadata !208, metadata !DIExpression()), !dbg !351
  %2 = icmp slt i64 %0, 0, !dbg !352
  %3 = xor i1 %2, true, !dbg !352
  br i1 %3, label %4, label %5, !dbg !353

4:                                                ; preds = %entry
  br label %13, !dbg !353

5:                                                ; preds = %entry
  %6 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 0, i64 %0), !dbg !353
  %7 = extractvalue { i64, i1 } %6, 0, !dbg !353
  %8 = extractvalue { i64, i1 } %6, 1, !dbg !353
  %9 = call i1 @llvm.expect.i1(i1 %8, i1 false), !dbg !353
  br i1 %9, label %15, label %10, !dbg !353

10:                                               ; preds = %5
  %11 = icmp slt i64 %7, 0, !dbg !353
  %12 = xor i1 %11, true, !dbg !353
  br label %13, !dbg !353

13:                                               ; preds = %10, %4
  %14 = phi i1 [ %12, %10 ], [ true, %4 ], !dbg !351
  ret i1 %14, !dbg !351

15:                                               ; preds = %5
  call void @llvm.trap(), !dbg !354
  unreachable, !dbg !354
}

define hidden swiftcc i64 @"$s13tmp_ir_source13PALLAS_SPEC_51a1b1x9both_gt_x3minS2i_S2iSbSitF"(i64 %0, i64 %1, i64 %2, i1 %3, i64 %4) #0 !dbg !140 !pallas.ghostWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !150, metadata !DIExpression()), !dbg !357
  call void @llvm.dbg.value(metadata i64 %1, metadata !152, metadata !DIExpression()), !dbg !357
  call void @llvm.dbg.value(metadata i64 %2, metadata !139, metadata !DIExpression()), !dbg !357
  call void @llvm.dbg.value(metadata i1 %3, metadata !145, metadata !DIExpression()), !dbg !357
  call void @llvm.dbg.value(metadata i64 %4, metadata !147, metadata !DIExpression()), !dbg !357
  %5 = icmp slt i64 %1, %0, !dbg !358
  %6 = xor i1 %5, true, !dbg !358
  br i1 %6, label %7, label %8, !dbg !358

7:                                                ; preds = %entry
  br label %9, !dbg !358

8:                                                ; preds = %entry
  br label %9, !dbg !358

9:                                                ; preds = %8, %7
  %10 = phi i64 [ %1, %8 ], [ %0, %7 ], !dbg !357
  ret i64 %10, !dbg !357
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_61a1b1x9both_gt_x3minSbSi_S2iSbSitF"(i64 %0, i64 %1, i64 %2, i1 %3, i64 %4) #0 !dbg !158 !pallas.ghostWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !166, metadata !DIExpression()), !dbg !359
  call void @llvm.dbg.value(metadata i64 %1, metadata !168, metadata !DIExpression()), !dbg !359
  call void @llvm.dbg.value(metadata i64 %2, metadata !157, metadata !DIExpression()), !dbg !359
  call void @llvm.dbg.value(metadata i1 %3, metadata !161, metadata !DIExpression()), !dbg !359
  call void @llvm.dbg.value(metadata i64 %4, metadata !163, metadata !DIExpression()), !dbg !359
  %5 = icmp slt i64 %2, %0, !dbg !360
  br i1 %5, label %6, label %8, !dbg !361

6:                                                ; preds = %entry
  %7 = icmp slt i64 %2, %1, !dbg !361
  br label %9, !dbg !361

8:                                                ; preds = %entry
  br label %9, !dbg !361

9:                                                ; preds = %8, %6
  %10 = phi i1 [ false, %8 ], [ %7, %6 ], !dbg !359
  ret i1 %10, !dbg !359
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_71a1b3max3min7both_gtSbSi_S3iSbtF"(i64 %0, i64 %1, i64 %2, i64 %3, i1 %4) #0 !dbg !253 !pallas.exprWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !260, metadata !DIExpression()), !dbg !362
  call void @llvm.dbg.value(metadata i64 %1, metadata !262, metadata !DIExpression()), !dbg !362
  call void @llvm.dbg.value(metadata i64 %2, metadata !264, metadata !DIExpression()), !dbg !362
  call void @llvm.dbg.value(metadata i64 %3, metadata !252, metadata !DIExpression()), !dbg !362
  call void @llvm.dbg.value(metadata i1 %4, metadata !257, metadata !DIExpression()), !dbg !362
  %5 = icmp eq i64 %2, 42, !dbg !363
  ret i1 %5, !dbg !362
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_81a1b3max3min7both_gtSbSi_S3iSbtF"(i64 %0, i64 %1, i64 %2, i64 %3, i1 %4) #0 !dbg !270 !pallas.exprWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !275, metadata !DIExpression()), !dbg !364
  call void @llvm.dbg.value(metadata i64 %1, metadata !277, metadata !DIExpression()), !dbg !364
  call void @llvm.dbg.value(metadata i64 %2, metadata !279, metadata !DIExpression()), !dbg !364
  call void @llvm.dbg.value(metadata i64 %3, metadata !269, metadata !DIExpression()), !dbg !364
  call void @llvm.dbg.value(metadata i1 %4, metadata !272, metadata !DIExpression()), !dbg !364
  %5 = icmp eq i64 %3, 1, !dbg !365
  ret i1 %5, !dbg !364
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_91a1b3max3min7both_gtSbSi_S3iSbtF"(i64 %0, i64 %1, i64 %2, i64 %3, i1 %4) #0 !dbg !285 !pallas.exprWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !290, metadata !DIExpression()), !dbg !366
  call void @llvm.dbg.value(metadata i64 %1, metadata !292, metadata !DIExpression()), !dbg !366
  call void @llvm.dbg.value(metadata i64 %2, metadata !294, metadata !DIExpression()), !dbg !366
  call void @llvm.dbg.value(metadata i64 %3, metadata !284, metadata !DIExpression()), !dbg !366
  call void @llvm.dbg.value(metadata i1 %4, metadata !287, metadata !DIExpression()), !dbg !366
  %5 = icmp eq i1 %4, false, !dbg !367
  ret i1 %5, !dbg !366
}

define hidden swiftcc i1 @"$s13tmp_ir_source14PALLAS_SPEC_101a1b3max3min7both_gtSbSi_S3iSbtF"(i64 %0, i64 %1, i64 %2, i64 %3, i1 %4) #0 !dbg !328 !pallas.exprWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !333, metadata !DIExpression()), !dbg !368
  call void @llvm.dbg.value(metadata i64 %1, metadata !335, metadata !DIExpression()), !dbg !368
  call void @llvm.dbg.value(metadata i64 %2, metadata !337, metadata !DIExpression()), !dbg !368
  call void @llvm.dbg.value(metadata i64 %3, metadata !327, metadata !DIExpression()), !dbg !368
  call void @llvm.dbg.value(metadata i1 %4, metadata !330, metadata !DIExpression()), !dbg !368
  %5 = icmp eq i1 %4, true, !dbg !369
  ret i1 %5, !dbg !368
}

define hidden swiftcc i64 @"$s13tmp_ir_source14PALLAS_SPEC_111a1b3min7both_gtS2i_S2iSbtF"(i64 %0, i64 %1, i64 %2, i1 %3) #0 !dbg !228 !pallas.ghostWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !235, metadata !DIExpression()), !dbg !370
  call void @llvm.dbg.value(metadata i64 %1, metadata !237, metadata !DIExpression()), !dbg !370
  call void @llvm.dbg.value(metadata i64 %2, metadata !227, metadata !DIExpression()), !dbg !370
  call void @llvm.dbg.value(metadata i1 %3, metadata !232, metadata !DIExpression()), !dbg !370
  ret i64 42, !dbg !370
}

define hidden swiftcc i64 @"$s13tmp_ir_source14PALLAS_SPEC_121a1b3max3min7both_gtS2i_S3iSbtF"(i64 %0, i64 %1, i64 %2, i64 %3, i1 %4) #0 !dbg !304 !pallas.ghostWrapper !338 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !311, metadata !DIExpression()), !dbg !371
  call void @llvm.dbg.value(metadata i64 %1, metadata !313, metadata !DIExpression()), !dbg !371
  call void @llvm.dbg.value(metadata i64 %2, metadata !315, metadata !DIExpression()), !dbg !371
  call void @llvm.dbg.value(metadata i64 %3, metadata !303, metadata !DIExpression()), !dbg !371
  call void @llvm.dbg.value(metadata i1 %4, metadata !308, metadata !DIExpression()), !dbg !371
  ret i64 -42, !dbg !371
}

define hidden swiftcc i1 @"$s13tmp_ir_source5isMaxySbSi_S2itF"(i64 %0, i64 %1, i64 %2) #0 !dbg !372 !pallas.predDef !375 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !376, metadata !DIExpression()), !dbg !377
  call void @llvm.dbg.value(metadata i64 %1, metadata !378, metadata !DIExpression()), !dbg !377
  call void @llvm.dbg.value(metadata i64 %2, metadata !379, metadata !DIExpression()), !dbg !377
  %3 = icmp eq i64 %0, %1, !dbg !380
  br i1 %3, label %4, label %5, !dbg !381

4:                                                ; preds = %entry
  br label %7, !dbg !381

5:                                                ; preds = %entry
  %6 = icmp eq i64 %0, %2, !dbg !381
  br label %7, !dbg !381

7:                                                ; preds = %5, %4
  %8 = phi i1 [ %6, %5 ], [ true, %4 ], !dbg !382
  br i1 %8, label %9, label %18, !dbg !382

9:                                                ; preds = %7
  %10 = icmp slt i64 %0, %1, !dbg !382
  %11 = xor i1 %10, true, !dbg !382
  br i1 %11, label %12, label %15, !dbg !382

12:                                               ; preds = %9
  %13 = icmp slt i64 %0, %2, !dbg !382
  %14 = xor i1 %13, true, !dbg !382
  br label %16, !dbg !382

15:                                               ; preds = %9
  br label %16, !dbg !382

16:                                               ; preds = %15, %12
  %17 = phi i1 [ false, %15 ], [ %14, %12 ], !dbg !382
  br label %19, !dbg !382

18:                                               ; preds = %7
  br label %19, !dbg !382

19:                                               ; preds = %18, %16
  %20 = phi i1 [ false, %18 ], [ %17, %16 ], !dbg !377
  ret i1 %20, !dbg !377
}

define hidden swiftcc i64 @"$s13tmp_ir_source6my_minyS2i_SitF"(i64 %0, i64 %1) #0 !dbg !383 !pallas.ghost !338 !pallas.fcontract !384 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !386, metadata !DIExpression()), !dbg !387
  call void @llvm.dbg.value(metadata i64 %1, metadata !388, metadata !DIExpression()), !dbg !389
  %2 = icmp slt i64 %1, %0, !dbg !390
  %3 = xor i1 %2, true, !dbg !390
  br i1 %3, label %4, label %5, !dbg !390

4:                                                ; preds = %entry
  br label %6, !dbg !390

5:                                                ; preds = %entry
  br label %6, !dbg !390

6:                                                ; preds = %5, %4
  %7 = phi i64 [ %1, %5 ], [ %0, %4 ], !dbg !391
  ret i64 %7, !dbg !391
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare i1 @llvm.expect.i1(i1, i1) #4

; Function Attrs: cold noreturn nounwind
declare void @llvm.trap() #5

define linkonce_odr hidden swiftcc { i64, ptr } @"$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_"() #0 !dbg !392 {
entry:
  %bitcast = alloca i64, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr %bitcast), !dbg !396
  store i64 -2305843009213693952, ptr %bitcast, align 8, !dbg !396
  %0 = load ptr, ptr %bitcast, align 8, !dbg !396
  call void @llvm.lifetime.end.p0(i64 8, ptr %bitcast), !dbg !396
  %1 = call ptr @swift_bridgeObjectRetain(ptr returned %0) #7, !dbg !396
  %2 = insertvalue { i64, ptr } { i64 0, ptr undef }, ptr %0, 1, !dbg !396
  ret { i64, ptr } %2, !dbg !396
}

; Function Attrs: noinline
declare swiftcc void @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_SSAHSus6UInt32VtF"(i64, i64, i8, i64, ptr, i64, i64, i8, i64, i32) #6

; Function Attrs: nounwind
declare ptr @swift_bridgeObjectRetain(ptr returned) #7

declare !pallas.specLib !397 i64 @"pallas.result i64"()

attributes #0 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #3 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #4 = { nocallback nofree nosync nounwind willreturn memory(none) }
attributes #5 = { cold noreturn nounwind }
attributes #6 = { noinline "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #7 = { nounwind }

!llvm.dbg.cu = !{!0, !15, !17, !19, !31, !32, !33, !35}
!swift.module.flags = !{!36, !36}
!llvm.linker.options = !{}
!llvm.module.flags = !{!37, !38, !39, !40, !41, !42, !43, !44, !45, !46, !47}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !2)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/ghost/pallas_swift_ghost.swift", directory: "/home/rme/repos/vercors")
!2 = !{!3, !5, !7, !9, !11, !13}
!3 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !4, file: !1)
!4 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "examples/concepts/llvm/pallas/ghost")
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
!33 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !34, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!34 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/ghost/pallas_swift_ghost.swift", directory: "")
!35 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !34, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!36 = !{!"standard-library", i1 false}
!37 = !{i32 7, !"Dwarf Version", i32 4}
!38 = !{i32 2, !"Debug Info Version", i32 3}
!39 = !{i32 1, !"wchar_size", i32 4}
!40 = !{i32 8, !"PIC Level", i32 2}
!41 = !{i32 7, !"uwtable", i32 2}
!42 = !{i32 7, !"frame-pointer", i32 2}
!43 = !{i32 1, !"Objective-C Garbage Collection", i8 0}
!44 = !{i32 1, !"Swift Version", i32 7}
!45 = !{i32 1, !"Swift ABI Version", i32 7}
!46 = !{i32 1, !"Swift Major Version", i8 6}
!47 = !{i32 1, !"Swift Minor Version", i8 0}
!48 = distinct !DISubprogram(name: "main", linkageName: "main", scope: !4, file: !1, line: 1, type: !49, spFlags: DISPFlagDefinition, unit: !0)
!49 = !DISubroutineType(types: !50)
!50 = !{!51, !51, !52}
!51 = !DICompositeType(tag: DW_TAG_structure_type, name: "$ss5Int32VD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!52 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSpySpys4Int8VGSgGD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!53 = !DILocation(line: 0, scope: !54)
!54 = !DILexicalBlockFile(scope: !48, file: !55, discriminator: 0)
!55 = !DIFile(filename: "<compiler-generated>", directory: "/")
!56 = distinct !DISubprogram(name: "get_max", linkageName: "$s13tmp_ir_source7get_maxyS2i_SitF", scope: !4, file: !1, line: 24, type: !57, scopeLine: 24, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !62)
!57 = !DISubroutineType(types: !58)
!58 = !{!59, !59, !59}
!59 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !6, file: !60, size: 64, elements: !61, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!60 = !DIFile(filename: "swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule", directory: "/home/rme")
!61 = !{}
!62 = !{!63, !65}
!63 = !DILocalVariable(name: "a", arg: 1, scope: !56, file: !1, line: 24, type: !64)
!64 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !59)
!65 = !DILocalVariable(name: "b", arg: 2, scope: !56, file: !1, line: 24, type: !64)
!66 = !{!67, i1 false, i1 false, !69, !72, !77, !97, !113}
!67 = !{!"pallas.srcLoc", i64 16, i64 1, i64 23, i64 1, !68}
!68 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/ghost/pallas_swift_ghost.swift", directory: "", checksumkind: CSK_MD5, checksum: "bb72c667149884a8150755dc6ccad986")
!69 = !{!70}
!70 = !{!71, !"x"}
!71 = !{!"pallas.srcLoc", i64 17, i64 1, i64 17, i64 14, !68}
!72 = !{!73, !75}
!73 = !{!74, !"both_gt_x"}
!74 = !{!"pallas.srcLoc", i64 18, i64 1, i64 18, i64 23, !68}
!75 = !{!76, !"min"}
!76 = !{!"pallas.srcLoc", i64 19, i64 1, i64 19, i64 16, !68}
!77 = !{!"pallas.ensures", !78, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01a1b1x9both_gt_x3minSbSi_S2iSbSitF", !79, !86, !92}
!78 = !{!"pallas.srcLoc", i64 20, i64 1, i64 20, i64 31, !68}
!79 = !{!80}
!80 = !{!70, !81}
!81 = !DILocalVariable(name: "x", arg: 3, scope: !82, file: !1, line: 20, type: !64)
!82 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_01a1b1x9both_gt_x3minSbSi_S2iSbSitF", scope: !4, file: !1, line: 20, type: !83, scopeLine: 20, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!83 = !DISubroutineType(types: !84)
!84 = !{!85, !59, !59, !59, !85, !59}
!85 = !DICompositeType(tag: DW_TAG_structure_type, name: "Bool", scope: !6, file: !60, size: 8, elements: !61, runtimeLang: DW_LANG_Swift, identifier: "$sSbD")
!86 = !{!87, !90}
!87 = !{!73, !88}
!88 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !82, file: !1, line: 20, type: !89)
!89 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !85)
!90 = !{!75, !91}
!91 = !DILocalVariable(name: "min", arg: 5, scope: !82, file: !1, line: 20, type: !64)
!92 = !{!93, !95}
!93 = !{!63, !94}
!94 = !DILocalVariable(name: "a", arg: 1, scope: !82, file: !1, line: 20, type: !64)
!95 = !{!65, !96}
!96 = !DILocalVariable(name: "b", arg: 2, scope: !82, file: !1, line: 20, type: !64)
!97 = !{!"pallas.ensures", !98, ptr @"$s13tmp_ir_source13PALLAS_SPEC_11a1b1x9both_gt_x3minSbSi_S2iSbSitF", !99, !103, !108}
!98 = !{!"pallas.srcLoc", i64 21, i64 1, i64 21, i64 28, !68}
!99 = !{!100}
!100 = !{!70, !101}
!101 = !DILocalVariable(name: "x", arg: 3, scope: !102, file: !1, line: 21, type: !64)
!102 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_11a1b1x9both_gt_x3minSbSi_S2iSbSitF", scope: !4, file: !1, line: 21, type: !83, scopeLine: 21, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!103 = !{!104, !106}
!104 = !{!73, !105}
!105 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !102, file: !1, line: 21, type: !89)
!106 = !{!75, !107}
!107 = !DILocalVariable(name: "min", arg: 5, scope: !102, file: !1, line: 21, type: !64)
!108 = !{!109, !111}
!109 = !{!63, !110}
!110 = !DILocalVariable(name: "a", arg: 1, scope: !102, file: !1, line: 21, type: !64)
!111 = !{!65, !112}
!112 = !DILocalVariable(name: "b", arg: 2, scope: !102, file: !1, line: 21, type: !64)
!113 = !{!"pallas.ensures", !114, ptr @"$s13tmp_ir_source13PALLAS_SPEC_21a1b1x9both_gt_x3minSbSi_S2iSbSitF", !115, !119, !124}
!114 = !{!"pallas.srcLoc", i64 22, i64 1, i64 22, i64 38, !68}
!115 = !{!116}
!116 = !{!70, !117}
!117 = !DILocalVariable(name: "x", arg: 3, scope: !118, file: !1, line: 22, type: !64)
!118 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_21a1b1x9both_gt_x3minSbSi_S2iSbSitF", scope: !4, file: !1, line: 22, type: !83, scopeLine: 22, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!119 = !{!120, !122}
!120 = !{!73, !121}
!121 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !118, file: !1, line: 22, type: !89)
!122 = !{!75, !123}
!123 = !DILocalVariable(name: "min", arg: 5, scope: !118, file: !1, line: 22, type: !64)
!124 = !{!125, !127}
!125 = !{!63, !126}
!126 = !DILocalVariable(name: "a", arg: 1, scope: !118, file: !1, line: 22, type: !64)
!127 = !{!65, !128}
!128 = !DILocalVariable(name: "b", arg: 2, scope: !118, file: !1, line: 22, type: !64)
!129 = !DILocation(line: 24, column: 14, scope: !56)
!130 = !DILocation(line: 24, column: 24, scope: !56)
!131 = !DILocation(line: 29, column: 10, scope: !132)
!132 = distinct !DILexicalBlock(scope: !56, file: !1, line: 29, column: 5)
!133 = !{!134, !135, !153}
!134 = !{!"pallas.srcLoc", i64 25, i64 5, i64 28, i64 5, !68}
!135 = !{!"pallas.gAssign", !136, ptr @"$s13tmp_ir_source13PALLAS_SPEC_51a1b1x9both_gt_x3minS2i_S2iSbSitF", !137, !143, !148, !75}
!136 = !{!"pallas.srcLoc", i64 26, i64 5, i64 26, i64 38, !68}
!137 = !{!138}
!138 = !{!70, !139}
!139 = !DILocalVariable(name: "x", arg: 3, scope: !140, file: !1, line: 26, type: !64)
!140 = distinct !DISubprogram(name: "PALLAS_SPEC_5", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_51a1b1x9both_gt_x3minS2i_S2iSbSitF", scope: !4, file: !1, line: 26, type: !141, scopeLine: 26, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!141 = !DISubroutineType(types: !142)
!142 = !{!59, !59, !59, !59, !85, !59}
!143 = !{!144, !146}
!144 = !{!73, !145}
!145 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !140, file: !1, line: 26, type: !89)
!146 = !{!75, !147}
!147 = !DILocalVariable(name: "min", arg: 5, scope: !140, file: !1, line: 26, type: !64)
!148 = !{!149, !151}
!149 = !{!63, !150}
!150 = !DILocalVariable(name: "a", arg: 1, scope: !140, file: !1, line: 26, type: !64)
!151 = !{!65, !152}
!152 = !DILocalVariable(name: "b", arg: 2, scope: !140, file: !1, line: 26, type: !64)
!153 = !{!"pallas.gAssign", !154, ptr @"$s13tmp_ir_source13PALLAS_SPEC_61a1b1x9both_gt_x3minSbSi_S2iSbSitF", !155, !159, !164, !73}
!154 = !{!"pallas.srcLoc", i64 27, i64 5, i64 27, i64 46, !68}
!155 = !{!156}
!156 = !{!70, !157}
!157 = !DILocalVariable(name: "x", arg: 3, scope: !158, file: !1, line: 27, type: !64)
!158 = distinct !DISubprogram(name: "PALLAS_SPEC_6", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_61a1b1x9both_gt_x3minSbSi_S2iSbSitF", scope: !4, file: !1, line: 27, type: !83, scopeLine: 27, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!159 = !{!160, !162}
!160 = !{!73, !161}
!161 = !DILocalVariable(name: "both_gt_x", arg: 4, scope: !158, file: !1, line: 27, type: !89)
!162 = !{!75, !163}
!163 = !DILocalVariable(name: "min", arg: 5, scope: !158, file: !1, line: 27, type: !64)
!164 = !{!165, !167}
!165 = !{!63, !166}
!166 = !DILocalVariable(name: "a", arg: 1, scope: !158, file: !1, line: 27, type: !64)
!167 = !{!65, !168}
!168 = !DILocalVariable(name: "b", arg: 2, scope: !158, file: !1, line: 27, type: !64)
!169 = !DILocation(line: 30, column: 9, scope: !170)
!170 = distinct !DILexicalBlock(scope: !132, file: !1, line: 29, column: 14)
!171 = !DILocation(line: 32, column: 5, scope: !56)
!172 = !DILocation(line: 33, column: 1, scope: !56)
!173 = distinct !DISubprogram(name: "run", linkageName: "$s13tmp_ir_source3runyyF", scope: !4, file: !1, line: 46, type: !174, scopeLine: 46, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !177)
!174 = !DISubroutineType(types: !175)
!175 = !{!176}
!176 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sytD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!177 = !{!178, !180, !182}
!178 = !DILocalVariable(name: "a", scope: !179, file: !1, line: 47, type: !59)
!179 = distinct !DILexicalBlock(scope: !173, file: !1, line: 47, column: 9)
!180 = !DILocalVariable(name: "b", scope: !181, file: !1, line: 48, type: !59)
!181 = distinct !DILexicalBlock(scope: !179, file: !1, line: 48, column: 9)
!182 = !DILocalVariable(name: "max", scope: !183, file: !1, line: 50, type: !64)
!183 = distinct !DILexicalBlock(scope: !181, file: !1, line: 50, column: 9)
!184 = !{!185, i1 false, i1 false, !61, !186, !191, !201}
!185 = !{!"pallas.srcLoc", i64 40, i64 1, i64 45, i64 1, !68}
!186 = !{!187, !189}
!187 = !{!188, !"min"}
!188 = !{!"pallas.srcLoc", i64 41, i64 1, i64 41, i64 16, !68}
!189 = !{!190, !"both_gt"}
!190 = !{!"pallas.srcLoc", i64 42, i64 1, i64 42, i64 21, !68}
!191 = !{!"pallas.ensures", !192, ptr @"$s13tmp_ir_source13PALLAS_SPEC_33min7both_gtSbSi_SbtF", !61, !193, !61}
!192 = !{!"pallas.srcLoc", i64 43, i64 1, i64 43, i64 28, !68}
!193 = !{!194, !199}
!194 = !{!187, !195}
!195 = !DILocalVariable(name: "min", arg: 1, scope: !196, file: !1, line: 43, type: !64)
!196 = distinct !DISubprogram(name: "PALLAS_SPEC_3", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_33min7both_gtSbSi_SbtF", scope: !4, file: !1, line: 43, type: !197, scopeLine: 43, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!197 = !DISubroutineType(types: !198)
!198 = !{!85, !59, !85}
!199 = !{!189, !200}
!200 = !DILocalVariable(name: "both_gt", arg: 2, scope: !196, file: !1, line: 43, type: !89)
!201 = !{!"pallas.ensures", !202, ptr @"$s13tmp_ir_source13PALLAS_SPEC_43min7both_gtSbSi_SbtF", !61, !203, !61}
!202 = !{!"pallas.srcLoc", i64 44, i64 1, i64 44, i64 30, !68}
!203 = !{!204, !207}
!204 = !{!187, !205}
!205 = !DILocalVariable(name: "min", arg: 1, scope: !206, file: !1, line: 44, type: !64)
!206 = distinct !DISubprogram(name: "PALLAS_SPEC_4", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_43min7both_gtSbSi_SbtF", scope: !4, file: !1, line: 44, type: !197, scopeLine: 44, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!207 = !{!189, !208}
!208 = !DILocalVariable(name: "both_gt", arg: 2, scope: !206, file: !1, line: 44, type: !89)
!209 = !DILocation(line: 47, column: 9, scope: !179)
!210 = !DILocation(line: 48, column: 9, scope: !181)
!211 = !DILocation(line: 0, scope: !212)
!212 = !DILexicalBlockFile(scope: !179, discriminator: 0)
!213 = !DILocation(line: 47, column: 18, scope: !214)
!214 = distinct !DILexicalBlock(scope: !173, file: !1, line: 47, column: 18)
!215 = !DILocation(line: 0, scope: !216)
!216 = !DILexicalBlockFile(scope: !181, discriminator: 0)
!217 = !DILocation(line: 48, column: 18, scope: !218)
!218 = distinct !DILexicalBlock(scope: !179, file: !1, line: 48, column: 18)
!219 = !DILocation(line: 50, column: 15, scope: !220)
!220 = distinct !DILexicalBlock(scope: !181, file: !1, line: 50, column: 15)
!221 = !{!222, !223}
!222 = !{!"pallas.srcLoc", i64 50, i64 23, i64 50, i64 41, !68}
!223 = !{!"pallas.givenBinding", !224, ptr @"$s13tmp_ir_source14PALLAS_SPEC_111a1b3min7both_gtS2i_S2iSbtF", !61, !225, !233, !70}
!224 = !{!"pallas.srcLoc", i64 50, i64 33, i64 50, i64 39, !68}
!225 = !{!226, !231}
!226 = !{!187, !227}
!227 = !DILocalVariable(name: "min", arg: 3, scope: !228, file: !1, line: 50, type: !64)
!228 = distinct !DISubprogram(name: "PALLAS_SPEC_11", linkageName: "$s13tmp_ir_source14PALLAS_SPEC_111a1b3min7both_gtS2i_S2iSbtF", scope: !4, file: !1, line: 50, type: !229, scopeLine: 50, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!229 = !DISubroutineType(types: !230)
!230 = !{!59, !59, !59, !59, !85}
!231 = !{!189, !232}
!232 = !DILocalVariable(name: "both_gt", arg: 4, scope: !228, file: !1, line: 50, type: !89)
!233 = !{!234, !236}
!234 = !{!178, !235}
!235 = !DILocalVariable(name: "a", arg: 1, scope: !228, file: !1, line: 50, type: !64)
!236 = !{!180, !237}
!237 = !DILocalVariable(name: "b", arg: 2, scope: !228, file: !1, line: 50, type: !64)
!238 = !{!239, !240, !242}
!239 = !{!"pallas.srcLoc", i64 50, i64 45, i64 50, i64 88, !68}
!240 = !{!"pallas.yieldsBinding", !241, !187, !75}
!241 = !{!"pallas.srcLoc", i64 50, i64 56, i64 50, i64 65, !68}
!242 = !{!"pallas.yieldsBinding", !243, !189, !73}
!243 = !{!"pallas.srcLoc", i64 50, i64 67, i64 50, i64 86, !68}
!244 = !DILocation(line: 50, column: 9, scope: !183)
!245 = !DILocation(line: 57, column: 7, scope: !183)
!246 = !{!247, !248, !265, !280}
!247 = !{!"pallas.srcLoc", i64 51, i64 5, i64 55, i64 5, !68}
!248 = !{!"pallas.assert", !249, ptr @"$s13tmp_ir_source13PALLAS_SPEC_71a1b3max3min7both_gtSbSi_S3iSbtF", !61, !250, !258}
!249 = !{!"pallas.srcLoc", i64 52, i64 5, i64 52, i64 21, !68}
!250 = !{!251, !256}
!251 = !{!187, !252}
!252 = !DILocalVariable(name: "min", arg: 4, scope: !253, file: !1, line: 52, type: !64)
!253 = distinct !DISubprogram(name: "PALLAS_SPEC_7", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_71a1b3max3min7both_gtSbSi_S3iSbtF", scope: !4, file: !1, line: 52, type: !254, scopeLine: 52, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!254 = !DISubroutineType(types: !255)
!255 = !{!85, !59, !59, !59, !59, !85}
!256 = !{!189, !257}
!257 = !DILocalVariable(name: "both_gt", arg: 5, scope: !253, file: !1, line: 52, type: !89)
!258 = !{!259, !261, !263}
!259 = !{!178, !260}
!260 = !DILocalVariable(name: "a", arg: 1, scope: !253, file: !1, line: 52, type: !64)
!261 = !{!180, !262}
!262 = !DILocalVariable(name: "b", arg: 2, scope: !253, file: !1, line: 52, type: !64)
!263 = !{!182, !264}
!264 = !DILocalVariable(name: "max", arg: 3, scope: !253, file: !1, line: 52, type: !64)
!265 = !{!"pallas.assert", !266, ptr @"$s13tmp_ir_source13PALLAS_SPEC_81a1b3max3min7both_gtSbSi_S3iSbtF", !61, !267, !273}
!266 = !{!"pallas.srcLoc", i64 53, i64 5, i64 53, i64 20, !68}
!267 = !{!268, !271}
!268 = !{!187, !269}
!269 = !DILocalVariable(name: "min", arg: 4, scope: !270, file: !1, line: 53, type: !64)
!270 = distinct !DISubprogram(name: "PALLAS_SPEC_8", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_81a1b3max3min7both_gtSbSi_S3iSbtF", scope: !4, file: !1, line: 53, type: !254, scopeLine: 53, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!271 = !{!189, !272}
!272 = !DILocalVariable(name: "both_gt", arg: 5, scope: !270, file: !1, line: 53, type: !89)
!273 = !{!274, !276, !278}
!274 = !{!178, !275}
!275 = !DILocalVariable(name: "a", arg: 1, scope: !270, file: !1, line: 53, type: !64)
!276 = !{!180, !277}
!277 = !DILocalVariable(name: "b", arg: 2, scope: !270, file: !1, line: 53, type: !64)
!278 = !{!182, !279}
!279 = !DILocalVariable(name: "max", arg: 3, scope: !270, file: !1, line: 53, type: !64)
!280 = !{!"pallas.assert", !281, ptr @"$s13tmp_ir_source13PALLAS_SPEC_91a1b3max3min7both_gtSbSi_S3iSbtF", !61, !282, !288}
!281 = !{!"pallas.srcLoc", i64 54, i64 5, i64 54, i64 28, !68}
!282 = !{!283, !286}
!283 = !{!187, !284}
!284 = !DILocalVariable(name: "min", arg: 4, scope: !285, file: !1, line: 54, type: !64)
!285 = distinct !DISubprogram(name: "PALLAS_SPEC_9", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_91a1b3max3min7both_gtSbSi_S3iSbtF", scope: !4, file: !1, line: 54, type: !254, scopeLine: 54, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!286 = !{!189, !287}
!287 = !DILocalVariable(name: "both_gt", arg: 5, scope: !285, file: !1, line: 54, type: !89)
!288 = !{!289, !291, !293}
!289 = !{!178, !290}
!290 = !DILocalVariable(name: "a", arg: 1, scope: !285, file: !1, line: 54, type: !64)
!291 = !{!180, !292}
!292 = !DILocalVariable(name: "b", arg: 2, scope: !285, file: !1, line: 54, type: !64)
!293 = !{!182, !294}
!294 = !DILocalVariable(name: "max", arg: 3, scope: !285, file: !1, line: 54, type: !64)
!295 = !DILocation(line: 58, column: 7, scope: !183)
!296 = !DILocation(line: 60, column: 9, scope: !183)
!297 = !{!298, !299}
!298 = !{!"pallas.srcLoc", i64 60, i64 17, i64 60, i64 36, !68}
!299 = !{!"pallas.givenBinding", !300, ptr @"$s13tmp_ir_source14PALLAS_SPEC_121a1b3max3min7both_gtS2i_S3iSbtF", !61, !301, !309, !70}
!300 = !{!"pallas.srcLoc", i64 60, i64 27, i64 60, i64 34, !68}
!301 = !{!302, !307}
!302 = !{!187, !303}
!303 = !DILocalVariable(name: "min", arg: 4, scope: !304, file: !1, line: 60, type: !64)
!304 = distinct !DISubprogram(name: "PALLAS_SPEC_12", linkageName: "$s13tmp_ir_source14PALLAS_SPEC_121a1b3max3min7both_gtS2i_S3iSbtF", scope: !4, file: !1, line: 60, type: !305, scopeLine: 60, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!305 = !DISubroutineType(types: !306)
!306 = !{!59, !59, !59, !59, !59, !85}
!307 = !{!189, !308}
!308 = !DILocalVariable(name: "both_gt", arg: 5, scope: !304, file: !1, line: 60, type: !89)
!309 = !{!310, !312, !314}
!310 = !{!178, !311}
!311 = !DILocalVariable(name: "a", arg: 1, scope: !304, file: !1, line: 60, type: !64)
!312 = !{!180, !313}
!313 = !DILocalVariable(name: "b", arg: 2, scope: !304, file: !1, line: 60, type: !64)
!314 = !{!182, !315}
!315 = !DILocalVariable(name: "max", arg: 3, scope: !304, file: !1, line: 60, type: !64)
!316 = !{!317, !318}
!317 = !{!"pallas.srcLoc", i64 60, i64 40, i64 60, i64 72, !68}
!318 = !{!"pallas.yieldsBinding", !319, !189, !73}
!319 = !{!"pallas.srcLoc", i64 60, i64 51, i64 60, i64 70, !68}
!320 = !DILocation(line: 66, column: 1, scope: !183)
!321 = !{!322, !323}
!322 = !{!"pallas.srcLoc", i64 62, i64 5, i64 64, i64 5, !68}
!323 = !{!"pallas.assert", !324, ptr @"$s13tmp_ir_source14PALLAS_SPEC_101a1b3max3min7both_gtSbSi_S3iSbtF", !61, !325, !331}
!324 = !{!"pallas.srcLoc", i64 63, i64 5, i64 63, i64 27, !68}
!325 = !{!326, !329}
!326 = !{!187, !327}
!327 = !DILocalVariable(name: "min", arg: 4, scope: !328, file: !1, line: 63, type: !64)
!328 = distinct !DISubprogram(name: "PALLAS_SPEC_10", linkageName: "$s13tmp_ir_source14PALLAS_SPEC_101a1b3max3min7both_gtSbSi_S3iSbtF", scope: !4, file: !1, line: 63, type: !254, scopeLine: 63, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !61)
!329 = !{!189, !330}
!330 = !DILocalVariable(name: "both_gt", arg: 5, scope: !328, file: !1, line: 63, type: !89)
!331 = !{!332, !334, !336}
!332 = !{!178, !333}
!333 = !DILocalVariable(name: "a", arg: 1, scope: !328, file: !1, line: 63, type: !64)
!334 = !{!180, !335}
!335 = !DILocalVariable(name: "b", arg: 2, scope: !328, file: !1, line: 63, type: !64)
!336 = !{!182, !337}
!337 = !DILocalVariable(name: "max", arg: 3, scope: !328, file: !1, line: 63, type: !64)
!338 = !{!""}
!339 = !DILocation(line: 0, scope: !82)
!340 = !DILocation(line: 20, column: 15, scope: !82)
!341 = !DILocation(line: 20, column: 9, scope: !82)
!342 = !DILocation(line: 0, scope: !102)
!343 = !DILocation(line: 21, column: 16, scope: !102)
!344 = !DILocation(line: 21, column: 13, scope: !102)
!345 = !DILocation(line: 0, scope: !118)
!346 = !DILocation(line: 22, column: 25, scope: !118)
!347 = !DILocation(line: 22, column: 29, scope: !118)
!348 = !DILocation(line: 22, column: 19, scope: !118)
!349 = !DILocation(line: 0, scope: !196)
!350 = !DILocation(line: 43, column: 17, scope: !196)
!351 = !DILocation(line: 0, scope: !206)
!352 = !DILocation(line: 44, column: 13, scope: !206)
!353 = !DILocation(line: 44, column: 18, scope: !206)
!354 = !DILocation(line: 0, scope: !355, inlinedAt: !353)
!355 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !55, file: !55, type: !356, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!356 = !DISubroutineType(types: null)
!357 = !DILocation(line: 0, scope: !140)
!358 = !DILocation(line: 26, column: 26, scope: !140)
!359 = !DILocation(line: 0, scope: !158)
!360 = !DILocation(line: 27, column: 33, scope: !158)
!361 = !DILocation(line: 27, column: 37, scope: !158)
!362 = !DILocation(line: 0, scope: !253)
!363 = !DILocation(line: 52, column: 16, scope: !253)
!364 = !DILocation(line: 0, scope: !270)
!365 = !DILocation(line: 53, column: 16, scope: !270)
!366 = !DILocation(line: 0, scope: !285)
!367 = !DILocation(line: 54, column: 20, scope: !285)
!368 = !DILocation(line: 0, scope: !328)
!369 = !DILocation(line: 63, column: 20, scope: !328)
!370 = !DILocation(line: 0, scope: !228)
!371 = !DILocation(line: 0, scope: !304)
!372 = distinct !DISubprogram(name: "isMax", linkageName: "$s13tmp_ir_source5isMaxySbSi_S2itF", scope: !34, file: !34, line: 6, type: !373, scopeLine: 6, spFlags: DISPFlagDefinition, unit: !35, retainedNodes: !61)
!373 = !DISubroutineType(types: !374)
!374 = !{!85, !59, !59, !59}
!375 = !{i1 true}
!376 = !DILocalVariable(name: "max", arg: 1, scope: !372, file: !34, line: 6, type: !64)
!377 = !DILocation(line: 0, scope: !372)
!378 = !DILocalVariable(name: "a", arg: 2, scope: !372, file: !34, line: 6, type: !64)
!379 = !DILocalVariable(name: "b", arg: 3, scope: !372, file: !34, line: 6, type: !64)
!380 = !DILocation(line: 6, column: 64, scope: !372)
!381 = !DILocation(line: 6, column: 69, scope: !372)
!382 = !DILocation(line: 6, column: 82, scope: !372)
!383 = distinct !DISubprogram(name: "my_min", linkageName: "$s13tmp_ir_source6my_minyS2i_SitF", scope: !34, file: !34, line: 12, type: !57, scopeLine: 12, spFlags: DISPFlagDefinition, unit: !33, retainedNodes: !61)
!384 = !{!385, i1 true, i1 false, !61, !61}
!385 = !{!"pallas.srcLoc", i64 12, i64 1, i64 11, i64 7, !68}
!386 = !DILocalVariable(name: "x", arg: 1, scope: !383, file: !34, line: 12, type: !64)
!387 = !DILocation(line: 12, column: 13, scope: !383)
!388 = !DILocalVariable(name: "y", arg: 2, scope: !383, file: !34, line: 12, type: !64)
!389 = !DILocation(line: 12, column: 23, scope: !383)
!390 = !DILocation(line: 12, column: 51, scope: !383)
!391 = !DILocation(line: 12, column: 42, scope: !383)
!392 = distinct !DISubprogram(linkageName: "$ss10fatalError_4file4lines5NeverOSSyXK_s12StaticStringVSutFfA_SSycfu_", scope: !23, file: !55, type: !393, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!393 = !DISubroutineType(types: !394)
!394 = !{!395}
!395 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSSD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!396 = !DILocation(line: 0, scope: !392)
!397 = !{!"pallas.result"}
