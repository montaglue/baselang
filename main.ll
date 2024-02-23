; ModuleID = 'main'
source_filename = "main"

@print_int = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i64 @main() {
block_0:
  %a = alloca i64, align 8
  store i64 1, ptr %a, align 4
  %tmp_0 = load i64, ptr %a, align 4
  %call = call i32 (ptr, ...) @printf(ptr @print_int, i64 %tmp_0)
  ret i64 0
}

declare i32 @printf(ptr, ...)
