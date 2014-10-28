type t = {
  local : int ref * (int, int) Hashtbl.t;
  global : int ref * (int, int) Hashtbl.t;
}
val empty_env : unit -> t
val find_env : int ref * (int, int) Hashtbl.t -> string -> int
val find_local : t -> string -> int
val find_global : t -> string -> int
val reset_local : t -> t
val pp_sep : string -> Format.formatter -> unit -> unit
val pp_space : Format.formatter -> unit -> unit
val pp_comma_space : Format.formatter -> unit -> unit
val linkage : Format.formatter -> Ollvm_ast.linkage -> unit
val dll_storage : Format.formatter -> Ollvm_ast.dll_storage -> unit
val visibility : Format.formatter -> Ollvm_ast.visibility -> unit
val cconv : Format.formatter -> Ollvm_ast.cconv -> unit
val param_attr : Format.formatter -> Ollvm_ast.param_attr -> unit
val fn_attr : Format.formatter -> Ollvm_ast.fn_attr -> unit
val ident : t -> Format.formatter -> Ollvm_ast.ident -> unit
val typ : Format.formatter -> Ollvm_ast.typ -> unit
val icmp : Format.formatter -> Ollvm_ast.icmp -> unit
val fcmp : Format.formatter -> Ollvm_ast.fcmp -> unit
val ibinop : Format.formatter -> Ollvm_ast.ibinop -> unit
val fbinop : Format.formatter -> Ollvm_ast.fbinop -> unit
val fast_math : Format.formatter -> Ollvm_ast.fast_math -> unit
val conversion_type : Format.formatter -> Ollvm_ast.conversion_type -> unit
val instr : t -> Format.formatter -> Ollvm_ast.instr -> unit
val value : t -> Format.formatter -> Ollvm_ast.value -> unit
val tvalue : t -> Format.formatter -> Ollvm_ast.tvalue -> unit
val tident : t -> Format.formatter -> Ollvm_ast.tident -> unit
val toplevelentries :
  t -> Format.formatter -> Ollvm_ast.toplevelentries -> unit
val toplevelentry : t -> Format.formatter -> Ollvm_ast.toplevelentry -> unit
val metadata : t -> Format.formatter -> Ollvm_ast.metadata -> unit
val global : t -> Format.formatter -> Ollvm_ast.global -> unit
val declaration : t -> Format.formatter -> Ollvm_ast.declaration -> unit
val definition : t -> Format.formatter -> Ollvm_ast.definition -> unit
val block : t -> Format.formatter -> Ollvm_ast.block -> unit
val modul : t -> Format.formatter -> Ollvm_ast.modul -> unit
