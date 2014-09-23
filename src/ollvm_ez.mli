(** Use Ez module to generate LLVM IR in a easy and pleasant way. *)

(** Basic predefined common types and type making functions. *)
module Type : sig

  open Ollvm_ast

  type t = Ollvm_ast.typ

  val i1 : t
  val i32 : t
  val half : t
  val float : t
  val double : t
  val pointer : t -> t
  val vector : int -> t -> t
  val label : t
  val void : t
  val array : int -> t -> t
  val structure : t list -> t

end

(** Annotate value with their type. *)
module Value : sig

  (** [ollvm] value annotated with type its. *)
  type t = Type.t * Ollvm_ast.value

 (** Values constructors.
     Do not confound with types in Type module.
     Be careful about module inclusion/opening order
     when using both Value and Type modules. *)

  val i1 : int -> t
  val i32 : int -> t
  val half : float -> t
  val float : float -> t
  val double : float -> t
  val vector : t list -> t
  val array : t list -> t
  val structure : t list -> t
  val ident : t -> Type.t * Ollvm_ast.ident

end

(** Instruction creation. *)
module Instr : sig

  (** [ollvm] instr annotated with its type. *)
  type t = Type.t * Ollvm_ast.instr

  (** [call fn args] call the function [fn] with [args] as argument. *)
  val call : Value.t -> Value.t list -> t

  (** [phi [(value1, label1); ... ; (valueN, labelN)]] return a value depending on
      the incoming block. [value1, ..., valueN] must have the same type. *)
  val phi : (Value.t * Value.t) list -> t

  (** [select cond value_true value_false] yields [value_true] or [value_false]
      depending on the value [cond]. *)
  val select : Value.t -> Value.t -> Value.t -> t

  (** [alloca ty] allocates memory on the stask of current function,
      which will be automatically freed on function returns.
      Use [nb] to specify the number of values to allocate (default is one).
      Use [align] to specify the alignment option (default is None) *)
  val alloca : ?nb:Value.t option -> ?align:int option -> Type.t -> t

  (** [load ptr] yields value stored in [ptr] alloca.
      Use [align] to specify the alignment option (default is None) *)
  val load : ?volatile:bool -> ?align:int option -> Value.t -> t

  (** [store val ptr] store [val] in [ptr] alloca. *)
  (* FIXME: should return instr instead of t? *)
  val store : ?volatile:bool -> ?align:int option -> Value.t -> Value.t -> t

  (** Int comparison. *)
  val eq  : Value.t -> Value.t -> t
  val eq  : Value.t -> Value.t -> t
  val ne  : Value.t -> Value.t -> t
  val ugt : Value.t -> Value.t -> t
  val uge : Value.t -> Value.t -> t
  val ult : Value.t -> Value.t -> t
  val ule : Value.t -> Value.t -> t
  val sgt : Value.t -> Value.t -> t
  val sge : Value.t -> Value.t -> t
  val slt : Value.t -> Value.t -> t
  val sle : Value.t -> Value.t -> t

  (** Float comparison. *)
  val ffalse : Value.t -> Value.t -> t
  val foeq   : Value.t -> Value.t -> t
  val fogt   : Value.t -> Value.t -> t
  val foge   : Value.t -> Value.t -> t
  val folt   : Value.t -> Value.t -> t
  val fole   : Value.t -> Value.t -> t
  val fone   : Value.t -> Value.t -> t
  val ord    : Value.t -> Value.t -> t
  val fueq   : Value.t -> Value.t -> t
  val fugt   : Value.t -> Value.t -> t
  val fuge   : Value.t -> Value.t -> t
  val fult   : Value.t -> Value.t -> t
  val fule   : Value.t -> Value.t -> t
  val fune   : Value.t -> Value.t -> t
  val funo   : Value.t -> Value.t -> t
  val ftrue  : Value.t -> Value.t -> t

  (** Int binary operations. *)
  val add  : ?nsw:bool -> ?nuw:bool -> Value.t -> Value.t -> t
  val sub  : ?nsw:bool -> ?nuw:bool -> Value.t -> Value.t -> t
  val mul  : ?nsw:bool -> ?nuw:bool -> Value.t -> Value.t -> t
  val udiv : ?exact:bool -> Value.t -> Value.t -> t
  val sdiv : ?exact:bool -> Value.t -> Value.t -> t
  val urem : Value.t -> Value.t -> t
  val srem : Value.t -> Value.t -> t
  val shl  : ?nsw:bool -> ?nuw:bool -> Value.t -> Value.t -> t
  val lshr : ?exact:bool -> Value.t -> Value.t -> t
  val ashr : ?exact:bool -> Value.t -> Value.t -> t
  val and_ : Value.t -> Value.t -> t
  val or_  : Value.t -> Value.t -> t
  val xor  : Value.t -> Value.t -> t

  (** Float binary operations. *)
  val fadd : ?flags:Ollvm_ast.fast_math list -> Value.t -> Value.t -> t
  val fsub : ?flags:Ollvm_ast.fast_math list -> Value.t -> Value.t -> t
  val fmul : ?flags:Ollvm_ast.fast_math list -> Value.t -> Value.t -> t
  val fdiv : ?flags:Ollvm_ast.fast_math list -> Value.t -> Value.t -> t
  val frem : ?flags:Ollvm_ast.fast_math list -> Value.t -> Value.t -> t

  (** [extractelement vec idx] returns the element contained in [vec]
      at index [idx]. *)
  val extractelement : Value.t -> Value.t -> t

  (** [insertelement vec val idx] returns a vector whose elements ares the same as
      [vec], except the element at index [idx] which will be [val] *)
  val insertelement : Value.t -> Value.t -> Value.t -> t

  val shufflevector : Value.t -> Value.t -> Value.t -> t

  (* Integer conversions. *)
  val trunc : Value.t -> Type.t -> t
  val zext  : Value.t -> Type.t -> t
  val sext  : Value.t -> Type.t -> t

  (* Float conversion *)
  val fptrunc : Value.t -> Type.t -> t
  val fpext   : Value.t -> Type.t -> t
  val fptoui  : Value.t -> Type.t -> t
  val fptosi  : Value.t -> Type.t -> t
  val uitofp  : Value.t -> Type.t -> t
  val sitofp  : Value.t -> Type.t -> t

  (** [extractvalue agg idx_list] *)
  val extractvalue : Value.t -> int list -> t

  (** [insertvalue agg val idx_list] *)
  val insertvalue : Value.t -> Value.t -> int list -> t

  (** Terminators.
      A block has to finish its instruction list with a terminator.
      These constructions return a [ollvm] instruction. *)

  (** [br cond lbl_true lbl_false] jumps to [lbl_true] or [lbl_false]
      depending on the value of [cond]. *)
  val br : Value.t -> Value.t -> Value.t -> Ollvm_ast.instr

 (** [br1 label] jumps to [label]. *)
  val br1 : Value.t -> Ollvm_ast.instr

  (** [switch cond default [(int1, labelN); ... ; (intN, labelN)]]
      jumps to the [labelX] whose associted [intX] is equal to [cond].
      If no such integer is found, then jumps to [default] label. *)
  val switch : Value.t -> Value.t -> (Value.t * Value.t) list -> Ollvm_ast.instr

  (** [ret val] returns [val]. *)
  val ret : Value.t -> Ollvm_ast.instr

  (** [ret_void] returns with no value. *)
  val ret_void : Ollvm_ast.instr

  (** Binds a [t] to an identifier.
      i. e. build a [ollvm] assignment instruction. *)
  val assign : Value.t -> t -> Ollvm_ast.instr

  (** Infix operator equivalent to [assign] function. *)
  val ( <-- ) : Value.t -> t -> Ollvm_ast.instr

  (** Converts a [t] into a [ollvm] instr. *)
  val ignore : t -> Ollvm_ast.instr

end

(** Function and block creation. *)
module Block : sig

  type block = Ollvm_ast.ident * (Ollvm_ast.instr list)

  (** [declare (ret_ty, fn) args_ty] declares [fn] as a function
      returning [ret_ty] and requiring arguments of types [args_ty]. *)
  val declare : Value.t -> Type.t list -> Ollvm_ast.declaration

  (** [define (ret_ty, fn) args instrs] defines [fn] as a function
      returning [ret_ty], with [args] as arguments and [instrs] as body. *)
  val define : Value.t -> Value.t list -> block list -> Ollvm_ast.definition

  (** [block label instrs] binds [instrs] to [label], creating a [block]. *)
  val block : Value.t -> Ollvm_ast.instr list -> block

end

  (** Module hanlder. *)
module Module : sig

  (** Local variable names memory. *)
  module Local :  sig
    (** Abstract type of the environment *)
    type t

    (** Create a local identifier (returned as a value). If a local
        variable already use [name] as identifier, it will be suffixed
        by a number in order to return a unique identifier. *)
    val local : t -> Type.t -> string -> (t * Value.t)

    (** The empty environment *)
    val empty : t
  end

  type t = {
    m_module: Ollvm_ast.modul;
    m_env: Local.t;
  }

  (** [init name (arch, vendor, os) data_layout] creates a fresh module with
      name, target triple and data layout set with given parameters and
      an empty environment. *)
  val init : string -> (string * string * string) -> string -> t

  (** [set_data_layout m new_datal_layout] returns m with new_data_layout
      as data layout. Data layout specifies how data is to be laid out
      in memory. *)
  val set_data_layout : t -> string -> t

  (** [set_target_triple m arch vendor os] returns m with target triple
      set according to [arch vendor os] parameters. *)
  val set_target_triple : t -> string -> string -> string -> t

  (** [local m t name] returns [(m', (t, v))] where [m'] is the new
      module with new local identifier declared and [(t, v)]
      is the resulting identifier and its type. If [name <> ""],
      it will be used as identifier (possibly with a number added
      as suffix), a number will be automatically assigned otherwise. *)
  val local : t -> Type.t -> string -> (t * Value.t)

  (** [locals m t n] return [(m', values)] where [m'] is the new
      module with new local identifiers declared and [values] is
      a list of length [n] of new identifiers binded to type [t].
      Identifiers will be automatically choosen (a number will be
      used). *)
  val locals : t -> Type.t -> int -> t * Value.t list

  (** [global m t name] returns [(m', g)] where [m'] is the new module
      resulting in the global variable [g] of name [name] and type [t]
      declaration. *)
  val global : t -> Type.t -> string -> (t * Value.t)

  (** [declaration m dc] returns [m'], which is the same module than [m],
      with [dc] declaration registered. *)
  val declaration : t -> Ollvm_ast.declaration -> t

  (** [definition m df] returns [m'], which is the same module than [m],
      with [df] definition registered. *)
  val definition : t -> Ollvm_ast.definition -> t

  (** [lookup_declaration m "foo"] looks for declaration of function
      named ["foo"] and returns it. Raises [Not_found] if ["foo"] is
      not declared. *)
  val lookup_declaration : t -> string -> Ollvm_ast.declaration

  (** [lookup_definition m "foo"] looks for definition of function
      named ["foo"] and returns it. Raises [Not_found] if ["foo"] is
      not defined. *)
  val lookup_definition : t -> string -> Ollvm_ast.definition
end
