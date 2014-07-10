; ModuleID = 'bar.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

define i32 @fact(i32 %n) {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  store i32 %n, i32* %2, align 4
  %3 = load i32* %2, align 4
  %4 = icmp ult i32 %3, 1
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i32 1, i32* %1
  br label %12

; <label>:6                                       ; preds = %0
  %7 = load i32* %2, align 4
  %8 = load i32* %2, align 4
  %9 = sub i32 %8, 1
  %10 = call i32 @fact(i32 %9)
  %11 = mul i32 %7, %10
  store i32 %11, i32* %1
  br label %12

; <label>:12                                      ; preds = %6, %5
  %13 = load i32* %1
  ret i32 %13
}

define i32 @main() {
  %1 = alloca i32, align 4
  store i32 0, i32* %1
  %2 = call i32 @fact(i32 42)
  switch i32 %2, label %5 [
    i32 -1, label %3
    i32 0, label %4
  ]

; <label>:3                                       ; preds = %0
  store i32 42, i32* %1
  br label %6

; <label>:4                                       ; preds = %0
  store i32 -42, i32* %1
  br label %6

; <label>:5                                       ; preds = %0
  store i32 0, i32* %1
  br label %6

; <label>:6                                       ; preds = %5, %4, %3
  %7 = load i32* %1
  ret i32 %7
}
