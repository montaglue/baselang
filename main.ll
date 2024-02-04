; ModuleID = 'main'
source_filename = "main"

define { i64, i64 } @make_pair(i64 %0, i64 %1) {
block_0:
  %x = alloca i64, align 8
  store i64 %0, ptr %x, align 4
  %y = alloca i64, align 8
  store i64 %1, ptr %y, align 4
  %tmp_0 = load i64, ptr %x, align 4
  %tmp_1 = load i64, ptr %y, align 4
  %insert0 = insertvalue { i64, i64 } zeroinitializer, i64 %tmp_0, 0
  %insert = insertvalue { i64, i64 } %insert0, i64 %tmp_1, 1
  ret { i64, i64 } %insert
}

define { ptr, i64, i64 } @make_string(i64 %0, ptr %1) {
block_0:
  %len = alloca i64, align 8
  store i64 %0, ptr %len, align 4
  %literal = alloca ptr, align 8
  store ptr %1, ptr %literal, align 8
  %tmp_3 = load ptr, ptr %literal, align 8
  %tmp_4 = load i64, ptr %len, align 4
  %tmp_5 = load i64, ptr %len, align 4
  %insert0 = insertvalue { ptr, i64, i64 } zeroinitializer, ptr %tmp_3, 0
  %insert1 = insertvalue { ptr, i64, i64 } %insert0, i64 %tmp_4, 1
  %insert = insertvalue { ptr, i64, i64 } %insert1, i64 %tmp_5, 2
  ret { ptr, i64, i64 } %insert
}

define i64 @main() {
block_0:
  %alloca = alloca [4 x i8], align 1
  store [5 x i8] c"\22hi\22\00", ptr %alloca, align 1
  %tmp_7 = call { ptr, i64, i64 } @make_string(i64 2, ptr %alloca)
  %a = alloca { ptr, i64, i64 }, align 8
  store { ptr, i64, i64 } %tmp_7, ptr %a, align 8
  ret i64 0
}
