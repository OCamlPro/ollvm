 (* {{{ LICENSE                                                              *
  * vi: set fdm=marker fdl=0:                                                *
  *                                                                          *
  * Copyright (c) 2012 Raphaël Proust <raphlalou@gmail.com>                  *
  * Copyright (c) 2012 INRIA - Raphaël Proust <raphlalou@gmail.com>          *
  * Copyright (c) 2012 ENS - Raphaël Proust <raphlalou@gmail.com>            *
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

type visibility =
  | VISIBILITY_Default
  | VISIBILITY_Hidden
  | VISIBILITY_Protected

type cconv =
  | CC_Ccc
  | CC_Fastcc
  | CC_Coldcc
  | CC_Cc of int

type typ_attr =
  | TYPEATTR_Zeroext
  | TYPEATTR_Signext
  | TYPEATTR_Inreg
  | TYPEATTR_Byval
  | TYPEATTR_Sret
  | TYPEATTR_Noalias
  | TYPEATTR_Nocapture
  | TYPEATTR_Nest

type fn_attr =
  | FNATTR_Address_safety
  | FNATTR_Alignstack of int
  | FNATTR_Alwaysinline
  | FNATTR_Nonlazybind
  | FNATTR_Inlinehint
  | FNATTR_Naked
  | FNATTR_Noimplicitfloat
  | FNATTR_Noinline
  | FNATTR_Noredzone
  | FNATTR_Noreturn
  | FNATTR_Nounwind
  | FNATTR_Optsize
  | FNATTR_Readnone
  | FNATTR_Readonly
  | FNATTR_Returns_twice
  | FNATTR_Ssp
  | FNATTR_Sspreq
  | FNATTR_Uwtable

type ident =
  | ID_Global of string
  | ID_Local  of string

type typ =
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
  | TYPE_Ident of ident
  | TYPE_Array of (int * typ)
  | TYPE_Function of (typ * typ list)
  | TYPE_Struct of typ list
  | TYPE_Packed_struct of typ list
  | TYPE_Opaque
  | TYPE_Vector of (int * typ)

type tident = typ * ident

type cmp =
  | Cmp_Eq
  | Cmp_Ne
  | Cmp_Ugt
  | Cmp_Uge
  | Cmp_Ult
  | Cmp_Ule
  | Cmp_Sgt
  | Cmp_Sge
  | Cmp_Slt
  | Cmp_Sle

type binop = typ * value * value

and conversion = typ * value * typ

and expr =
  | EXPR_Add  of binop
  | EXPR_FAdd
  | EXPR_Sub  of binop
  | EXPR_FSub
  | EXPR_Mul  of binop
  | EXPR_FMul
  | EXPR_UDiv of binop
  | EXPR_SDiv of binop
  | EXPR_FDiv
  | EXPR_URem of binop
  | EXPR_SRem of binop
  | EXPR_FRem
  | EXPR_Shl  of binop
  | EXPR_LShr of binop
  | EXPR_AShr of binop
  | EXPR_And  of binop
  | EXPR_Or   of binop
  | EXPR_Xor  of binop
  | EXPR_ICmp of (cmp * typ * value * value)
  | EXPR_FCmp
  | EXPR_Trunc    of conversion
  | EXPR_ZExt     of conversion
  | EXPR_SExt     of conversion
  | EXPR_FPTrunc  of conversion
  | EXPR_FPExt    of conversion
  | EXPR_UIToFP   of conversion
  | EXPR_SIToFP   of conversion
  | EXPR_FPToUI   of conversion
  | EXPR_FPToSI   of conversion
  | EXPR_IntToPtr of conversion
  | EXPR_PtrToInt of conversion
  | EXPR_BitCast  of conversion
  | EXPR_GetElementPtr of (typ * value * (typ * value) list)
  | EXPR_ExtractElement
  | EXPR_InsertElement
  | EXPR_ShuffleVector
  | EXPR_ExtractValue
  | EXPR_InsertValue

and value =
  | VALUE_Ident of ident
  | VALUE_Integer of int
  | VALUE_Float of float
  | VALUE_Bool of bool
  | VALUE_Null
  | VALUE_Undef
  | VALUE_Struct of (typ * value) list
  | VALUE_Packed_struct of (typ * value) list
  | VALUE_Array of (typ * value) list
  | VALUE_Vector of (typ * value) list
  | VALUE_Zero_initializer
  | VALUE_Expr of expr

type tvalue = typ * value

type call = typ * ident * (typ * value) list

type terminator =
  | TERM_Ret of (typ * value)
  | TERM_Ret_void
  | TERM_Br of (value * ident * ident) (*types are constant *)
  | TERM_Br_1 of ident
  | TERM_Switch of (typ * value * value * (typ * value * ident) list)
  | TERM_IndirectBr
  | TERM_Invoke of (typ * ident * (typ * value) list * ident * ident)
  | TERM_Resume of (typ * value)
  | TERM_Unreachable

type memop =
  | MEM_Alloca of (ident * int * typ)
  | MEM_Load of (ident * typ * value)
  | MEM_Store of (typ * value * typ * ident)

type module_ = toplevelentry list

and toplevelentry =
  | TLE_Target of string
  | TLE_Datalayout of string
  | TLE_Declaration of declaration
  | TLE_Definition of definition
  | TLE_Type_decl of (ident * typ)
  | TLE_Global of global

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
   df_instrs: instr list;
}

and instr =
  | INSTR_Assign of (ident * expr)
  | INSTR_Call of (ident * call)
  | INSTR_Call_unit of call
  | INSTR_PHI of (ident * typ * (value * ident) list)
  | INSTR_Terminator of terminator
  | INSTR_Select of (ident * typ * value * typ * value * value)
  | INSTR_VAArg
  | INSTR_Mem of memop
  | INSTR_AtomicCmpXchg
  | INSTR_AtomicRMW
  | INSTR_Fence
  | INSTR_LandingPad
  | INSTR_Label of ident

