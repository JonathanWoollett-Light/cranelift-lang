; ModuleID = 'sum.bc'
source_filename = "my_module"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"

define i32 @sum(i32 %0, i32 %1) {
entry:
  %temp = add i32 %0, %1
  ret i32 %temp
}
