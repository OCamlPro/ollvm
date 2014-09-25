(** Llvmgateway function. Convert Ollvm module to Llvm module. *)

(** [env] gather classic modules needed when creating a LLVM module
    and local environment. *)
type env = { c: Llvm.llcontext;
             m: Llvm.llmodule;
             b: Llvm.llbuilder;

             (** [llvalues] binded to [ident] (local variables) *)
             mem: (Ollvm.Ast.ident * Llvm.llvalue) list;
             (** [llbasicblocks] binded to [string] (local labels) *)
             labels: (string * Llvm.llbasicblock) list }

val linkage : Ollvm.Ast.linkage -> Llvm.Linkage.t

val dll_storage : Ollvm.Ast.dll_storage -> Llvm.Linkage.t

val visibility : Ollvm.Ast.visibility -> Llvm.Visibility.t

val cconv : Ollvm.Ast.cconv -> int

val typ_attr : Ollvm.Ast.param_attr -> Llvm.Attribute.t

val fn_attr : Ollvm.Ast.fn_attr -> Llvm.Attribute.t

val typ : env -> Ollvm.Ast.typ -> Llvm.lltype

val icmp : Ollvm.Ast.icmp -> Llvm.Icmp.t

val fcmp : Ollvm.Ast.fcmp -> Llvm.Fcmp.t

val ibinop : Ollvm.Ast.ibinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue)

val fbinop : Ollvm.Ast.fbinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue)

val conversion_type : Ollvm.Ast.conversion_type ->
                      (Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue)

val value : env -> Ollvm.Ast.typ -> Ollvm.Ast.value -> Llvm.llvalue

(** [instr env i] returns [(env', v)] where [env'] is [env]
    with instruction [i] added to module [env.m] and [v] if the resulting
    value of this instruction in the [Llvm] world. *)
val instr : env -> Ollvm.Ast.instr -> (env * Llvm.llvalue)

val global : env -> Ollvm.Ast.global -> env

val declaration : env -> Ollvm.Ast.declaration -> env * Llvm.llvalue

(** [create_block env b fn] appends a llbasicblock to [fn] and update [env]
    to keep this block in [env.labels].
    see [definition] see [block]. *)
val create_block : env -> Ollvm.Ast.block -> Llvm.llvalue -> env

(** [block env b] fetch the llbasicblock corresponding previously
    associated to [b] with [create_block] function, set the position
    of the builder to add instructions to this block, and finally
    build instructions of [b].
    see [create_block] see [instr] *)
val block : env -> Ollvm.Ast.block -> env

(** [definition enf df] define the function [df].
    Current implementation does not allow function redefinition.
    It means that defining a function with the same name than an
    other one will produce a crash. *)
val definition : env -> Ollvm.Ast.definition -> env

val modul : Ollvm.Ast.modul -> env
