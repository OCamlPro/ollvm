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
  | FNATTR_String of string (* "no-see" *)
  | FNATTR_Key_value of string * string (* "unsafe-fp-math"="false" *)
  | FNATTR_Attr_grp of int

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

 and instr =
  | INSTR_IBinop of ibinop * typ * value * value
  | INSTR_ICmp of icmp * typ * value * value
  | INSTR_FBinop of fbinop * typ * value * value
  | INSTR_FCmp of fcmp * typ * value * value
  | INSTR_Conversion of conversion_type * typ * value * typ
  | INSTR_GetElementPtr of tvalue * tvalue list
  | INSTR_ExtractElement of tvalue * tvalue
  | INSTR_InsertElement of tvalue * tvalue * tvalue
  | INSTR_ShuffleVector of tvalue * tvalue * tvalue
  | INSTR_ExtractValue of tvalue * int list
  | INSTR_InsertValue of tvalue * tvalue * int list
  | INSTR_Call of tident * tvalue list
  | INSTR_Alloca of typ * tvalue option * int option (* typ, nb el, align *)
  | INSTR_Load of tvalue * int option (* FIXME: use tident instead of value *)
  | INSTR_Phi of typ * (value * ident) list
  | INSTR_Select of tvalue * tvalue * tvalue (* if * then * else *)
  | INSTR_VAArg
  | INSTR_LandingPad
  | INSTR_Store of tvalue * tident * int option
  | INSTR_Fence
  | INSTR_AtomicCmpXchg
  | INSTR_AtomicRMW

  (* Terminators *)
  | INSTR_Invoke of tident * tvalue list * tident * tident
  | INSTR_Ret of tvalue
  | INSTR_Ret_void
  | INSTR_Br of tvalue * tident * tident (*types are constant *)
  | INSTR_Br_1 of tident
  | INSTR_Switch of tvalue * tident * (tvalue * tident) list
  | INSTR_IndirectBr
  | INSTR_Resume of tvalue
  | INSTR_Unreachable

  (* Special `assign` instruction:
   * not a real LLVM instruction, allow to bind an identifier to an instruction *)
  | INSTR_Assign of ident * instr

and toplevelentry =
  | TLE_Target of string
  | TLE_Datalayout of string
  | TLE_Declaration of declaration
  | TLE_Definition of definition
  | TLE_Type_decl of (ident * typ)
  | TLE_Global of global
  | TLE_Metadata
  | TLE_Attribute_group of int * fn_attr list

and toplevelentries = toplevelentry list

and global = {
     g_ident: ident;
       g_typ: typ;
  g_constant: bool;
   g_section: string option;
     g_align: int option;
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
  df_section: string option;
    df_align: int option;
   df_instrs: block list;
}

and block = string * instr list
