(** Llvmgateway function. Convert Ollvm module to Llvm module. *)

(** [env] gather classic modules needed when creating a LLVM module
    and local environment. *)
type env = { c: Llvm.llcontext;
             m: Llvm.llmodule;
             b: Llvm.llbuilder;

             (** [llvalues] binded to [ident] (local variables) *)
             mem: (Ollvm_ast.ident * Llvm.llvalue) list;
             (** [llbasicblocks] binded to [string] (local labels) *)
             labels: (string * Llvm.llbasicblock) list }

val linkage : Ollvm_ast.linkage -> Llvm.Linkage.t

val dll_storage : Ollvm_ast.dll_storage -> Llvm.Linkage.t

val visibility : Ollvm_ast.visibility -> Llvm.Visibility.t

val cconv : Ollvm_ast.cconv -> int

val typ_attr : Ollvm_ast.param_attr -> Llvm.Attribute.t

val fn_attr : Ollvm_ast.fn_attr -> Llvm.Attribute.t

val typ : env -> Ollvm_ast.typ -> Llvm.lltype

val icmp : Ollvm_ast.icmp -> Llvm.Icmp.t

val fcmp : Ollvm_ast.fcmp -> Llvm.Fcmp.t

val ibinop : Ollvm_ast.ibinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue)

val fbinop : Ollvm_ast.fbinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue)

val conversion_type : Ollvm_ast.conversion_type ->
                      (Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue)

val value : env -> Ollvm_ast.typ -> Ollvm_ast.value -> Llvm.llvalue

(** [instr env i] returns [(env', v)] where [env'] is [env]
    with instruction [i] added to module [env.m] and [v] if the resulting
    value of this instruction in the [Llvm] world. *)
val instr : env -> Ollvm_ast.instr -> (env * Llvm.llvalue)

val global : env -> Ollvm_ast.global -> env

val declaration : env -> Ollvm_ast.declaration -> env * Llvm.llvalue

(** [create_block env b fn] appends a llbasicblock to [fn] and update [env]
    to keep this block in [env.labels].
    see [definition] see [block]. *)
val create_block : env -> Ollvm_ast.block -> Llvm.llvalue -> env

(** [block env b] fetch the llbasicblock corresponding previously
    associated to [b] with [create_block] function, set the position
    of the builder to add instructions to this block, and finally
    build instructions of [b].
    see [create_block] see [instr] *)
val block : env -> Ollvm_ast.block -> env

(** [definition enf df] define the function [df].
    Current implementation does not allow function redefinition.
    It means that defining a function with the same name than an
    other one will produce a crash. *)
val definition : env -> Ollvm_ast.definition -> env

val modul : Ollvm_ast.modul -> env
