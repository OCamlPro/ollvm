(** Type and Val module share some names (e.g. i1, i32). Avoid it? *)

module Type : sig

  open Ast

  type typ = Ast.typ

  val i1 : typ
  val i32 : typ
  val half : typ
  val float : typ
  val double : typ
  val pointer : typ -> typ
  val vector : int -> typ -> typ
  val label : typ
  val void : typ
  val array : int -> typ -> typ
  val structure : typ list -> typ

end

module Value : sig

  type typ = Type.typ

  (** [llvmocp] value annotated with type its. *)
  type tvalue = typ * Ast.value

 (** Values constructors.
  * Do not confound with types in Type module.
  * Be careful about module inclusion/opening order
  * when using both Value and Type modules. *)
  val i1 : int -> tvalue
  val i32 : int -> tvalue
  val half : float -> tvalue
  val float : float -> tvalue
  val double : float -> tvalue
  val vector : tvalue list -> tvalue
  val array : tvalue list -> tvalue
  val structure : tvalue list -> tvalue
  val ident : tvalue -> typ * Ast.ident

end

module Instr : sig

  type typ = Type.typ

  type tvalue = Value.tvalue

  (** [llvmocp] instr annotated with its type. *)
  type tinstr = typ * Ast.instr

  (** [call fn args] call the function [fn] with [args] as argument.
   * FIXME: functions should have their own type. *)
  val call : tvalue -> tvalue list -> tinstr

  (** [phi [(value1, label1); ... ; (valueN, labelN)]] return a value depending on
   * the incoming block. [value1, ..., valueN] must have the same type. *)
  val phi : (tvalue * tvalue) list -> tinstr

  (** [select cond value_true value_false] yields [value_true] or [value_false]
   * depending on the value [cond]. *)
  val select : tvalue -> tvalue -> tvalue -> tinstr

  (** [alloca ty] allocates memory on the stask of current function,
   * which will be automatically freed on function returns.
   * Use [nb] to specify the number of values to allocate (default is one).
   * Use [align] to specify the alignment option (default is None) *)
  val alloca : ?nb:Ast.tvalue option -> ?align:int option -> typ -> tinstr

  (** [load ptr] yields value stored in [ptr] alloca.
   * Use [align] to specify the alignment option (default is None) *)
  val load : ?volatile:bool -> ?align:int option -> tvalue -> tinstr

  (** [store val ptr] store [val] in [ptr] alloca. *)
  (* FIXME: should return instr instead of tinstr? *)
  val store : ?volatile:bool -> ?align:int option -> tvalue -> tvalue -> tinstr

  type bin_sig = tvalue -> tvalue -> tinstr

  val eq:bin_sig val eq:bin_sig val ne:bin_sig val ugt:bin_sig
  val uge:bin_sig val ult:bin_sig val ule:bin_sig val sgt:bin_sig
  val sge:bin_sig val slt:bin_sig val sle:bin_sig

  val ffalse : bin_sig val foeq : bin_sig val fogt : bin_sig
  val foge : bin_sig val folt : bin_sig val fole : bin_sig
  val fone : bin_sig val ord : bin_sig val fueq : bin_sig
  val fugt : bin_sig val fuge : bin_sig val fult : bin_sig
  val fule : bin_sig val fune : bin_sig val funo : bin_sig
  val ftrue : bin_sig

  type nsw_nuw_ibinop_sig = ?nsw:bool -> ?nuw:bool -> bin_sig
  type exact_ibinop_sig = ?exact:bool -> bin_sig

  val add : nsw_nuw_ibinop_sig
  val sub : nsw_nuw_ibinop_sig
  val mul : nsw_nuw_ibinop_sig
  val udiv : exact_ibinop_sig
  val sdiv : exact_ibinop_sig
  val urem : bin_sig
  val srem : bin_sig
  val shl : nsw_nuw_ibinop_sig
  val lshr : exact_ibinop_sig
  val ashr : exact_ibinop_sig
  val and_ : bin_sig
  val or_ : bin_sig
  val xor : bin_sig

  type fbinop_sig = ?flags:Ast.fast_math list -> bin_sig

  val fadd : fbinop_sig val fsub : fbinop_sig val fmul : fbinop_sig
  val fdiv : fbinop_sig val frem : fbinop_sig

  (** [extractelement vec idx] returns the element contained in [vec]
   * at index [idx]. *)
  val extractelement : tvalue -> tvalue -> tinstr

  (** [insertelement vec val idx] returns a vector whose elements ares the same as
   * [vec], except the element at index [idx] which will be [val] *)
  val insertelement : tvalue -> tvalue -> tvalue -> tinstr

  val shufflevector : tvalue -> tvalue -> tvalue -> tinstr

  type convert_sig = tvalue -> typ -> tinstr

  (* Integer conversions. *)
  val trunc : convert_sig val zext : convert_sig val sext : convert_sig

  (* Float conversion *)
  val fptrunc : convert_sig val fpext : convert_sig
  val fptoui : convert_sig val fptosi : convert_sig
  val uitofp : convert_sig val sitofp : convert_sig

  (** [extractvalue agg idx_list] *)
  val extractvalue : tvalue -> int list -> tinstr

  (** [insertvalue agg val idx_list] *)
  val insertvalue : tvalue -> tvalue -> int list -> tinstr

  (** Terminators.
   * A block has to finish its instruction list with a terminator.
   * These constructions return a [llvmocp] instruction. *)

  (** [br cond lbl_true lbl_false] jumps to [lbl_true] or [lbl_false]
   * depending on the value of [cond]. *)
  val br : tvalue -> tvalue -> tvalue -> Ast.instr

 (** [br1 label] jumps to [label]. *)
  val br1 : tvalue -> Ast.instr

  (** [switch cond default [(int1, labelN); ... ; (intN, labelN)]]
   * jumps to the [labelX] whose associted [intX] is equal to [cond].
   * If no such integer is found, then jumps to [default] label. *)
  val switch : tvalue -> tvalue -> (tvalue * tvalue) list -> Ast.instr

  (** [ret val] returns [val]. *)
  val ret : tvalue -> Ast.instr

  (** [ret_void] returns with no value. *)
  val ret_void : Ast.instr

  (** Binds a [tinstr] to an identifier.
   * i. e. build a [llvmocp] assignment instruction. *)
  val assign : tvalue -> tinstr -> Ast.instr

  (** Infix operator equivalent to [assign] function. *)
  val ( <-- ) : tvalue -> tinstr -> Ast.instr

  (** Converts a [tinstr] into a [llvmocp] instr. *)
  val ignore : tinstr -> Ast.instr

end

module Block : sig

  (** FIXME: llvmocp may fix its block/function type,
   * so this module may change. *)

  type block = Ast.ident * (Ast.instr list)

  type typ = Type.typ

  type tvalue = Value.tvalue

  (** [declare (ret_ty, fn) args_ty] declares [fn] as a function
   * returning [ret_ty] and requiring arguments of types [args_ty]. *)
  val declare : tvalue -> typ list -> Ast.declaration

  (** [define (ret_ty, fn) args instrs] defines [fn] as a function
   * returning [ret_ty], with [args] as arguments and [instrs] as body. *)
  val define : tvalue -> tvalue list -> block list -> Ast.definition

  (** [block label instrs] binds [instrs] to [label], creating a [block]. *)
  val block : tvalue -> Ast.instr list -> block

end

module Env :  sig
  type t = { unnamed_counter : int;
             named_counter : (string * int) list } (* FIXME *)
  val local : t -> Type.typ -> string -> (t * Value.tvalue)
  val empty : t
end

module Module : sig

  type t = {
    m_module: Ast.modul;
    m_env: Env.t;
  }

  val local : t -> Type.typ -> string -> (t * Value.tvalue)
  val global : t -> Type.typ -> string -> (t * Value.tvalue)
  val declaration : t -> Ast.declaration -> string -> t
  val definition : t -> Ast.definition -> string -> t
  val lookup_declaration : t -> string -> Ast.declaration
  val lookup_definition : t -> string -> Ast.definition
  val data_layout : string -> Ast.toplevelentry
  val target_triple : string -> string -> string -> Ast.toplevelentry
end
