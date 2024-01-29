; ModuleID = 'main'
source_filename = "main"

@print_int = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i64 @set_two(ptr %0) {
block_0:
  %a = alloca ptr, align 8
  store ptr %0, ptr %a, align 8
  %tmp_0 = load ptr, ptr %a, align 8
  store i64 2, ptr %tmp_0, align 4
  %tmp_1 = load ptr, ptr %a, align 8
  %tmp_2 = load ptr, ptr %tmp_1, align 8
  %call = call i32 (ptr, ...) @printf(ptr @print_int, ptr %tmp_2)
  ret i64 0
}

define i64 @main() {
block_0:
  %tmp_3 = call ptr @malloc(i64 8)
  store i64 1, ptr %tmp_3, align 4
  %a = alloca ptr, align 8
  store ptr %tmp_3, ptr %a, align 8
  %tmp_4 = load ptr, ptr %a, align 8
  %tmp_5 = call i64 @set_two(ptr %tmp_4)
  %tmp_6 = load ptr, ptr %a, align 8
  %tmp_7 = load ptr, ptr %tmp_6, align 8
  %call = call i32 (ptr, ...) @printf(ptr @print_int, ptr %tmp_7)
  ret i64 0
}

declare i32 @printf(ptr, ...)

declare ptr @malloc(i64)
