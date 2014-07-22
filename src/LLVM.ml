 (* {{{ LICENSE                                                              *
  * vi: set fdm=marker fdl=0:                                                *
  *                                                                          *
  * Copyright (c) 2012 Raphaël Proust <raphlalou@gmail.com>                  *
  * Copyright (c) 2012 INRIA - Raphaël Proust <raphlalou@gmail.com>          *
  * Copyright (c) 2012 ENS - Raphaël Proust <raphlalou@gmail.com>            *
  * Copyright (c) 2014 OCamlPro - Julien Sagot <ju.sagot@gmail.com>          *
  *                                                                          *
  * Permission to use, copy, modify, and distribute this software for any    *
  * purpose with or without fee is hereby granted, provided that the above   *
  * copyright notice and this permission notice appear in all copies.        *
  *                                                                          *
  * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES *
  * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         *
  * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *
  * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   *
  * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    *
  * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *
  * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           *
  * }}}                                                                      *)

(** This module is for LLVM. It only exports types that can represent an LLVM
    module. Note that it is incomplete, it is just enough to parse and use files
    produced by clang or ghc. It might or might not be completed in the future.
    It might or might not be set in a proper library in the future. *)

type linkage =
  | LINKAGE_Private
  | LINKAGE_Linker_private
  | LINKAGE_Linker_private_weak
  | LINKAGE_Linker_private_weak_def_auto
  | LINKAGE_Internal
  | LINKAGE_Available_externally
  | LINKAGE_Linkonce
  | LINKAGE_Weak
  | LINKAGE_Common
  | LINKAGE_Appending
  | LINKAGE_Extern_weak
  | LINKAGE_Linkonce_odr
  | LINKAGE_Weak_odr
  | LINKAGE_External
  | LINKAGE_Dllimport
  | LINKAGE_Dllexport

and visibility =
  | VISIBILITY_Default
  | VISIBILITY_Hidden
  | VISIBILITY_Protected

and cconv =
  | CC_Ccc
  | CC_Fastcc
  | CC_Coldcc
  | CC_Cc of int

and typ_attr =
  | TYPEATTR_Zeroext
  | TYPEATTR_Signext
  | TYPEATTR_Inreg
  | TYPEATTR_Byval
  | TYPEATTR_Sret
  | TYPEATTR_Noalias
  | TYPEATTR_Nocapture
  | TYPEATTR_Nest

and fn_attr =
  | FNATTR_Alignstack of int
  | FNATTR_Alwaysinline
  | FNATTR_Builtin
  | FNATTR_Cold
  | FNATTR_Inlinehint
  | FNATTR_Jumptable
  | FNATTR_Minsize
  | FNATTR_Naked
  | FNATTR_Nobuiltin
  | FNATTR_Noduplicate
  | FNATTR_Noimplicitfloat
  | FNATTR_Noinline
  | FNATTR_Nonlazybind
  | FNATTR_Noredzone
  | FNATTR_Noreturn
  | FNATTR_Nounwind
  | FNATTR_Optnone
  | FNATTR_Optsize
  | FNATTR_Readnone
  | FNATTR_Readonly
  | FNATTR_Returns_twice
  | FNATTR_Sanitize_address
  | FNATTR_Sanitize_memory
  | FNATTR_Sanitize_thread
  | FNATTR_Ssp
  | FNATTR_Sspreq
  | FNATTR_Sspstrong
  | FNATTR_Uwtable

and ident_format =
  | ID_FORMAT_Named
  | ID_FORMAT_NamedString
  | ID_FORMAT_Unnamed

and ident =
  | ID_Global of ident_format * string
  | ID_Local  of ident_format * string

and typ =
  | TYPE_I of int
  | TYPE_Pointer of typ
  | TYPE_Void
  | TYPE_Half
  | TYPE_Float
  | TYPE_Double
  | TYPE_X86_fp80
  | TYPE_Fp128
  | TYPE_Ppc_fp128
  | TYPE_Label
  | TYPE_Metadata
  | TYPE_X86_mmx
  | TYPE_Array of (int * typ)
  | TYPE_Function of (typ * typ list)
  | TYPE_Struct of typ list
  | TYPE_Packed_struct of typ list
  | TYPE_Opaque
  | TYPE_Vector of (int * typ)

and icmp = Eq|Ne|Ugt|Uge|Ult|Ule|Sgt|Sge|Slt|Sle

and fcmp = False|Oeq|Ogt|Oge|Olt|Ole|One|Ord|Uno|Ueq|Ugt|Uge|Ult|Ule|Une|True

and ibinop = Add|Sub|Mul|UDiv|SDiv|URem|SRem|Shl|LShr|AShr|And|Or|Xor

and fbinop = FAdd|FSub|FMul|FDiv|FRem

and conversion_type = Trunc|Zext|Sext|Fptrunc|Fpext|Uitofp|Sitofp|Fptoui
                       |Fptosi|Inttoptr|Ptrtoint|Bitcast

and tvalue = typ * value

and tident = typ * ident

 and expr =
  | EXPR_IBinop of ibinop * typ * value * value
  | EXPR_ICmp of icmp * typ * value * value
  | EXPR_FBinop of fbinop * typ * value * value
  | EXPR_FCmp of fcmp * typ * value * value
  | EXPR_Conversion of conversion_type * typ * value * typ
  | EXPR_GetElementPtr of tvalue * tvalue list
  | EXPR_ExtractElement of tvalue * tvalue
  | EXPR_InsertElement of tvalue * tvalue * tvalue
  | EXPR_ShuffleVector
  | EXPR_ExtractValue of tvalue * int list
  | EXPR_InsertValue of tvalue * tvalue * int list
  | EXPR_Call of tident * tvalue list
  | EXPR_Alloca of int * typ
  | EXPR_Load of tvalue
  | EXPR_Phi of typ * (value * ident) list
  | EXPR_Select of tvalue * tvalue * tvalue (* if * then * else *)
  | EXPR_VAArg
  | EXPR_LandingPad

and expr_unit =
  | EXPR_UNIT_IGNORED of expr
  | EXPR_UNIT_Store of tvalue * tident
  | EXPR_UNIT_Fence
  | EXPR_UNIT_AtomicCmpXchg
  | EXPR_UNIT_AtomicRMW

and value =
  | VALUE_Ident of ident
  | VALUE_Integer of int
  | VALUE_Float of float
  | VALUE_Bool of bool
  | VALUE_Null
  | VALUE_Undef
  | VALUE_Struct of tvalue list
  | VALUE_Packed_struct of tvalue list
  | VALUE_Array of tvalue list
  | VALUE_Vector of tvalue list
  | VALUE_Zero_initializer

and terminator =
  | TERM_Invoke of (tident * tvalue list * tident * tident)

and terminator_unit =
  | TERM_UNIT_Ret of tvalue
  | TERM_UNIT_Ret_void
  | TERM_UNIT_Br of (tvalue * tident * tident) (*types are constant *)
  | TERM_UNIT_Br_1 of tident
  | TERM_UNIT_Switch of (tvalue * tvalue * (tvalue * tident) list)
  | TERM_UNIT_IndirectBr
  | TERM_UNIT_Resume of tvalue
  | TERM_UNIT_Unreachable

and module_ = toplevelentry list

and toplevelentry =
  | TLE_Target of string
  | TLE_Datalayout of string
  | TLE_Declaration of declaration
  | TLE_Definition of definition
  | TLE_Type_decl of (ident * typ)
  | TLE_Global of global
  | TLE_Metadata

and global = {
     g_ident: ident;
       g_typ: typ;
  g_constant: bool;
     g_value: value option;
}

and declaration = {
  dc_ret_typ: typ;
     dc_name: ident;
     dc_args: typ list;
}

and definition = {
  df_ret_typ: typ;
     df_name: ident;
     df_args: tident list;
    df_attrs: fn_attr list;
   df_instrs: unnamed_block * named_block list;
}

and instr =
  | INSTR_Expr_Assign of (ident * expr)
  | INSTR_Expr_Unit of expr_unit
  | INSTR_Terminator of (ident * terminator)
  | INSTR_Terminator_Unit of terminator_unit

and unnamed_block = instr list

and named_block = string * unnamed_block
