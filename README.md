# ollvm 0.99

ollvm library offers an interface to manipulate LLVM IR in pure OCaml.
LLVM already provides binding to its C API, but it still mainly imperative
programming with heavy use of side effects.

ollvm is different in the way that you will manipulate OCaml structures
(lists, records, variants, ...). An Ez interface is provided
to make LLVM IR writing pleasant.

You may also want to use LLVM IR, but not the whole LLVM compiler
infrastructure. ollvm allows you to stay independent from llvm library.
You **may** want to bind you code to official bindings with the provided
gateway, but you **do not have to**. Make your optimization passes, use
your own back-end if you please.

### Ez interface utilisation example

Input program:

```ocaml
open Ollvm.Ez.Value
open Ollvm.Ez.Instr
open Ollvm.Ez.Block
module M = Ollvm.Ez.Module
module T = Ollvm.Ez.Type
module P = Ollvm.Printer

let _ =
  (* module initialization *)
  let m = M.init
            "name"
            ("x86_64", "pc", "linux-gnu")
            "e-m:e-i64:64-f80:128-n8:16:32:64-S128" in

  (* variables declaration *)
  let (m, x0) =
    M.local m T.i1 "" in
  let (m, [x1; x2; x3; arg]) =
    M.locals m T.i32 [""; ""; ""; ""] in
  let (m, [entry_b; then_b; else_b]) =
    M.locals m T.label ["entry"; "then"; "else" ] in
  let (m, fact) = M.global m T.i32 "fact" in

  (* fact function definition *)
  let f = define fact [x4]
                 [ block entry_b [
                           x0 <-- eq x4 (i32 0) ;
                           br x0 then_b else_b ; ] ;
                   block then_b [
                           ret (i32 1) ; ] ;
                   block else_b [
                           x1 <-- sub x4 (i32 1) ;
                           x2 <-- call fact [x1] ;
                           x3 <-- mul x4  x2 ;
                           ret x3 ; ] ] in

  (* fact function registration in module *)
  let m = M.definition m f in
  P.modul (P.empty_env ()) Format.std_formatter m.m_module
```

Output:

```
; ModuleID = 'name'
target triple = "x86_64-pc-linux-gnu"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

define i32 @fact(i32 %0) {
entry:
        %1 = icmp eq i32 %0, 0
        br i1 %1, label %then, label %else
then:
       ret i32 1
else:
       %2 = sub i32 %0, 1
       %3 = call i32 @fact(i32 %2)
       %4 = mul i32 %0, %3
       ret i32 %4
}
```

### Ressources

* [Ollvm on Github](http://www.github.com/OCamlPro/ollvm): latest sources
  in the official GIT repository.
* [Official LLVM website](http://llvm.org/): more details on LLVM and
  its itermediate representation specification.
* [Online documentation](http://sagotch.github.io/ollvm)

### Install

* Via opam: ` opam install ollvm `
* From source: ` ./configure && make && [sudo] make install `

### Limitations

Ollvm is still work in progress.

* Parser misses some LLVM IR features (e.g. metadata attached to
  instructions).
* Ollvm does not checks types of your instruction, nor it ensures
  SSA form (i.e. you can write wrong LLVM IR).
  It *may* ensure good typing (statically) of your LLVM code in futur.

### Known bugs

* `Ez` / `Printer` modules:
  * Unnamed labels are not correctly printed with.
  * Unnamed function arguments are printed in prototypes while they
     should not.

Note that passing though Llvmgateway and printing with official
binding results in a correct LLVM IR output.
