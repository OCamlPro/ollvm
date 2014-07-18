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

%{

  open LLVM (* in productions *)

%}

(*TODO: floats *)
(*TODO: don't throw things away *)

%token<string> GLOBAL LOCAL
%token LPAREN RPAREN LCURLY RCURLY LTLCURLY RCURLYGT LSQUARE RSQUARE LT GT EQ COMMA EOF EOL STAR

%token<string> STRING
%token<int> INTEGER
%token<float> FLOAT
%token KW_NULL KW_UNDEF KW_TRUE KW_FALSE KW_ZEROINITIALIZER

%token<string> LABEL

%token KW_DEFINE KW_DECLARE KW_TARGET KW_DATALAYOUT KW_TRIPLE
%token KW_PRIVATE KW_LINKER_PRIVATE KW_LINKER_PRIVATE_WEAK KW_LINKER_PRIVATE_WEAK_DEF_AUTO KW_INTERNAL KW_AVAILABLE_EXTERNALLY KW_LINKONCE KW_WEAK KW_COMMON KW_APPENDING KW_EXTERN_WEAK KW_LINKONCE_ODR KW_WEAK_ODR KW_EXTERNAL KW_DLLIMPORT KW_DLLEXPORT
%token KW_DEFAULT KW_HIDDEN KW_PROTECTED
%token KW_CCC KW_FASTCC KW_COLDCC KW_CC
%token KW_UNNAMED_ADDR
%token KW_TYPE KW_X KW_OPAQUE
%token KW_GLOBAL KW_ADDRSPACE KW_CONSTANT KW_SECTION KW_THREAD_LOCAL
%token KW_ZEROEXT KW_SIGNEXT KW_INREG KW_BYVAL KW_SRET KW_NOALIAS KW_NOCAPTURE KW_NEST
%token KW_ALIGNSTACK KW_ALWAYSINLINE KW_BUILTIN KW_COLD KW_INLINEHINT KW_JUMPTABLE KW_MINSIZE KW_NAKED KW_NOBUILTIN KW_NODUPLICATE KW_NOIMPLICITFLOAT KW_NOINLINE KW_NONLAZYBIND KW_NOREDZONE KW_NORETURN KW_NOUNWIND KW_OPTNONE KW_OPTSIZE KW_READNONE KW_READONLY KW_RETURNS_TWICE KW_SANITIZE_ADDRESS KW_SANITIZE_MEMORY KW_SANITIZE_THREAD KW_SSP KW_SSPREQ KW_SSPSTRONG KW_UWTABLE
%token KW_ALIGN
%token KW_GC
%token KW_ADD KW_FADD KW_SUB KW_FSUB KW_MUL KW_FMUL KW_UDIV KW_SDIV KW_FDIV KW_UREM KW_SREM KW_FREM KW_SHL KW_LSHR KW_ASHR KW_AND KW_OR KW_XOR KW_ICMP KW_FCMP KW_PHI KW_CALL KW_TRUNC KW_ZEXT KW_SEXT KW_FPTRUNC KW_FPEXT KW_UITOFP KW_SITOFP KW_FPTOUI KW_FPTOSI KW_INTTOPTR KW_PTRTOINT KW_BITCAST KW_SELECT KW_VAARG KW_RET KW_BR KW_SWITCH KW_INDIRECTBR KW_INVOKE KW_RESUME KW_UNREACHABLE KW_ALLOCA KW_LOAD KW_STORE KW_ATOMICCMPXCHG KW_ATOMICRMW KW_FENCE KW_GETELEMENTPTR KW_INBOUNDS KW_EXTRACTELEMENT KW_INSERTELEMENT KW_SHUFFLEVECTOR KW_EXTRACTVALUE KW_INSERTVALUE KW_LANDINGPAD
%token<int> I
%token KW_VOID KW_HALF KW_FLOAT KW_DOUBLE KW_X86_FP80 KW_FP128 KW_PPC_FP128 KW_LABEL KW_METADATA KW_X86_MMX
%token KW_UNWIND KW_TO
%token KW_NUW KW_NSW
%token KW_EXACT
%token KW_EQ KW_NE KW_UGT KW_UGE KW_ULT KW_ULE KW_SGT KW_SGE KW_SLT KW_SLE
%token KW_TAIL
%token KW_VOLATILE


%start<LLVM.module_> module_

%%

(* NB: Will produve parsing error with file not ending with a EOL *)
module_:
  | EOL* m=terminated(toplevelentry, EOL+)* EOF { m }

toplevelentry:
  | d=definition                        { TLE_Definition d              }
  | d=declaration                       { TLE_Declaration d             }
  | KW_TARGET KW_DATALAYOUT EQ s=STRING { TLE_Datalayout s              }
  | KW_TARGET KW_TRIPLE EQ s=STRING     { TLE_Target s                  }
  | i=LOCAL EQ KW_TYPE t=typ            { TLE_Type_decl (ID_Local i, t) }
  | g=global_decl                       { TLE_Global g                  }

global_decl:
  | ident=GLOBAL EQ
      linkage? visibility? KW_THREAD_LOCAL? addrspace? KW_UNNAMED_ADDR?
      g_constant=global_is_constant g_typ=typ g_value=const?
      preceded(COMMA, global_attr)?
      { {g_ident=ID_Global ident; g_typ; g_constant; g_value;} }

global_attr:
  | KW_SECTION STRING              { }
  | KW_SECTION STRING COMMA align  { }
  | align                          { }

global_is_constant:
  | KW_GLOBAL { false }
  | KW_CONSTANT { true }

addrspace: KW_ADDRSPACE LPAREN n=INTEGER RPAREN { n }

comma_section: COMMA KW_SECTION s=STRING { s }

definition:
  | KW_DEFINE linkage? visibility? cconv?
    df_ret_typ=ret_type name=GLOBAL
    LPAREN df_args=separated_list(COMMA, df_arg) RPAREN
    df_attrs=list(fn_attr) EOL*
    LCURLY EOL*
    df_entry_block=unnamed_block
    df_other_blocks=named_block*
    RCURLY
    { {df_ret_typ;
       df_name=ID_Global name;
       df_args;
       df_attrs;
       df_instrs=(df_entry_block, df_other_blocks);} }

unnamed_block:
  | b=terminated(instr, EOL+)* { b }

named_block:
  | i=LABEL b=unnamed_block { (i, b) }

declaration:
  | KW_DECLARE linkage? visibility? cconv? KW_UNNAMED_ADDR?
    dc_ret_typ=ret_type name=GLOBAL
    LPAREN dc_args=separated_list(COMMA, dc_arg) RPAREN
    list(fn_attr_gen)
    { {dc_ret_typ; dc_name=ID_Global name; dc_args;} }

linkage:
  | KW_PRIVATE                      { LINKAGE_Private                      }
  | KW_LINKER_PRIVATE               { LINKAGE_Linker_private               }
  | KW_LINKER_PRIVATE_WEAK          { LINKAGE_Linker_private_weak          }
  | KW_LINKER_PRIVATE_WEAK_DEF_AUTO { LINKAGE_Linker_private_weak_def_auto }
  | KW_INTERNAL                     { LINKAGE_Internal                     }
  | KW_AVAILABLE_EXTERNALLY         { LINKAGE_Available_externally         }
  | KW_LINKONCE                     { LINKAGE_Linkonce                     }
  | KW_WEAK                         { LINKAGE_Weak                         }
  | KW_COMMON                       { LINKAGE_Common                       }
  | KW_APPENDING                    { LINKAGE_Appending                    }
  | KW_EXTERN_WEAK                  { LINKAGE_Extern_weak                  }
  | KW_LINKONCE_ODR                 { LINKAGE_Linkonce_odr                 }
  | KW_WEAK_ODR                     { LINKAGE_Weak_odr                     }
  | KW_EXTERNAL                     { LINKAGE_External                     }
  | KW_DLLIMPORT                    { LINKAGE_Dllimport                    }
  | KW_DLLEXPORT                    { LINKAGE_Dllexport                    }

visibility:
  | KW_DEFAULT   { VISIBILITY_Default   }
  | KW_HIDDEN    { VISIBILITY_Hidden    }
  | KW_PROTECTED { VISIBILITY_Protected }

cconv:
  |KW_CCC{CC_Ccc}|KW_FASTCC{CC_Fastcc}|KW_COLDCC{CC_Coldcc}
  |KW_CC n=INTEGER{CC_Cc n}

ret_type:
  | list(typ_attr) t=typ { t }

typ:
  | n=I                                               { TYPE_I n              }
  | KW_VOID                                           { TYPE_Void             }
  | KW_HALF                                           { TYPE_Half             }
  | KW_FLOAT                                          { TYPE_Float            }
  | KW_DOUBLE                                         { TYPE_Double           }
  | KW_X86_FP80                                       { TYPE_X86_fp80         }
  | KW_FP128                                          { TYPE_Fp128            }
  | KW_PPC_FP128                                      { TYPE_Ppc_fp128        }
  | KW_LABEL                                          { TYPE_Label            }
  | KW_METADATA                                       { TYPE_Metadata         }
  | KW_X86_MMX                                        { TYPE_X86_mmx          }
  | t=typ STAR                                        { TYPE_Pointer t        }
  | i=ident                                           { TYPE_Ident i          }
  | LSQUARE n=INTEGER KW_X t=typ RSQUARE              { TYPE_Array (n, t)     }
  | t=typ LPAREN ts=separated_list(COMMA, typ) RPAREN { TYPE_Function (t, ts) }
  | LCURLY ts=separated_list(COMMA, typ) RCURLY       { TYPE_Struct ts        }
  | LTLCURLY ts=separated_list(COMMA, typ) RCURLYGT   { TYPE_Packed_struct ts }
  | KW_OPAQUE                                         { TYPE_Opaque           }
  | LT n=INTEGER KW_X t=typ GT                        { TYPE_Vector (n, t)    }

typ_attr:
  | KW_ZEROEXT   { TYPEATTR_Zeroext   }
  | KW_SIGNEXT   { TYPEATTR_Signext   }
  | KW_INREG     { TYPEATTR_Inreg     }
  | KW_BYVAL     { TYPEATTR_Byval     }
  | KW_SRET      { TYPEATTR_Sret      }
  | KW_NOALIAS   { TYPEATTR_Noalias   }
  | KW_NOCAPTURE { TYPEATTR_Nocapture }
  | KW_NEST      { TYPEATTR_Nest      }

dc_arg: t=typ typ_attr*      { t      }
df_arg: t=dc_arg i=ident     { (t, i) }
call_arg: t=dc_arg i=value   { (t, i) }

fn_attr:
  | KW_ALIGNSTACK LPAREN p=INTEGER RPAREN { FNATTR_Alignstack p    }
  | KW_ALWAYSINLINE                       { FNATTR_Alwaysinline    }
  | KW_BUILTIN                            { FNATTR_Nobuiltin       }
  | KW_COLD                               { FNATTR_Cold            }
  | KW_INLINEHINT                         { FNATTR_Inlinehint      }
  | KW_JUMPTABLE                          { FNATTR_Jumptable       }
  | KW_MINSIZE                            { FNATTR_Minsize         }
  | KW_NAKED                              { FNATTR_Naked           }
  | KW_NOBUILTIN                          { FNATTR_Nobuiltin       }
  | KW_NODUPLICATE                        { FNATTR_Noduplicate     }
  | KW_NOIMPLICITFLOAT                    { FNATTR_Noimplicitfloat }
  | KW_NOINLINE                           { FNATTR_Noinline        }
  | KW_NONLAZYBIND                        { FNATTR_Nonlazybind     }
  | KW_NOREDZONE                          { FNATTR_Noredzone       }
  | KW_NORETURN                           { FNATTR_Noreturn        }
  | KW_NOUNWIND                           { FNATTR_Nounwind        }
  | KW_OPTNONE                            { FNATTR_Optnone         }
  | KW_OPTSIZE                            { FNATTR_Optsize         }
  | KW_READNONE                           { FNATTR_Readnone        }
  | KW_READONLY                           { FNATTR_Readonly        }
  | KW_RETURNS_TWICE                      { FNATTR_Returns_twice   }
  | KW_SANITIZE_ADDRESS                   { FNATTR_Sanitize_address}
  | KW_SANITIZE_MEMORY                    { FNATTR_Sanitize_memory }
  | KW_SANITIZE_THREAD                    { FNATTR_Sanitize_thread }
  | KW_SSP                                { FNATTR_Ssp             }
  | KW_SSPREQ                             { FNATTR_Sspreq          }
  | KW_SSPSTRONG                          { FNATTR_Sspstrong       }
  | KW_UWTABLE                            { FNATTR_Uwtable         }

fn_attr_gen:
  | f=fn_attr                                           { Some f }
  | KW_ALIGN INTEGER | KW_GC STRING | KW_SECTION STRING { None   }

align: KW_ALIGN p=INTEGER { p }

binop_nuw_nsw_opt: (* may appear with `nuw`/`nsw` keywords *)
  |KW_ADD{Add}|KW_SUB{Sub}|KW_MUL{Mul}|KW_SHL{Shl}

binop_exact_opt: (* may appear with `exact` keyword *)
  |KW_UDIV{UDiv}|KW_SDIV{SDiv}|KW_LSHR{LShr}|KW_ASHR{AShr}

binop_no_opt: (* can not appear with any keyword *)
  |KW_UREM{URem}|KW_SREM{SRem}|KW_AND{And}|KW_OR{Or}|KW_XOR{Xor}

icmp:
  |KW_EQ{Eq}|KW_NE{Ne}|KW_UGT{Ugt}|KW_UGE{Uge} |KW_ULT{Ult}|KW_ULE{Ule}
  |KW_SGT{Sgt}|KW_SGE{Sge}|KW_SLT{Slt}|KW_SLE{Sle}

conversion:
  |KW_TRUNC{Trunc}|KW_ZEXT{Zext}|KW_SEXT{Sext}|KW_FPTRUNC{Fptrunc}
  |KW_FPEXT{Fpext}|KW_UITOFP{Uitofp}|KW_SITOFP{Sitofp}|KW_FPTOUI{Fptoui}
  |KW_FPTOSI{Fptosi}|KW_INTTOPTR{Inttoptr}|KW_PTRTOINT{Ptrtoint}
  |KW_BITCAST{Bitcast}

binop:
  | op=binop_nuw_nsw_opt KW_NUW? KW_NSW? { op }
  | op=binop_exact_opt KW_EXACT? { op }
  | op=binop_no_opt { op }

expr:
  | op=binop t=typ o1=value COMMA o2=value
    { EXPR_IBinop (op, t, o1, o2) }

  | KW_ICMP op=icmp t=typ o1=value COMMA o2=value
    { EXPR_ICmp (op, t, o1, o2) }

  | c=conversion t1=typ v=value KW_TO t2=typ
    { EXPR_Conversion (c, t1, v, t2) }

  | KW_GETELEMENTPTR ?KW_INBOUNDS ptr=tvalue
    idx=preceded(COMMA, tvalue)*
    { EXPR_GetElementPtr (ptr, idx) }

  | KW_TAIL? KW_CALL cconv? list(typ_attr) t=typ
    n=ident a=delimited(LPAREN, separated_list(COMMA, call_arg), RPAREN)
    list(fn_attr)
    { EXPR_Call (t, n, a) }

  | KW_ALLOCA t=typ n=alloc_attr?
    { let n=match n with None -> 1 | Some x -> x in
      EXPR_Alloca (n, t) }

  | KW_LOAD KW_VOLATILE? tv=tvalue preceded(COMMA, align)?
    { EXPR_Load tv }

  | KW_PHI t=typ table=separated_nonempty_list(COMMA, phi_table_entry)
    { EXPR_Phi (t, table) }

  | KW_SELECT if_=tvalue COMMA then_=tvalue COMMA else_= tvalue
    { EXPR_Select (if_, then_, else_) }

  | KW_EXTRACTELEMENT vec=tvalue COMMA idx=tvalue
    { EXPR_ExtractElement (vec, idx) }

  | KW_INSERTELEMENT vec=tvalue
    COMMA new_el=tvalue COMMA idx=tvalue
    { EXPR_InsertElement (vec, new_el, idx)  }

  | KW_EXTRACTVALUE tv=tvalue COMMA
    idx=separated_nonempty_list (COMMA, INTEGER)
    { EXPR_ExtractValue (tv, idx) }

  | KW_INSERTVALUE agg=tvalue COMMA new_val=tvalue COMMA
    idx=separated_nonempty_list (COMMA, INTEGER)
    { EXPR_InsertValue (agg, new_val, idx) }

  | KW_SHUFFLEVECTOR  { failwith "EXPR_ShuffleVector"  }
  | KW_VAARG  { failwith"INSTR_VAArg"  }
  | KW_LANDINGPAD    { failwith"INSTR_LandingPad"    }

expr_unit:

  | e=expr { EXPR_UNIT_IGNORED e }

  | KW_STORE KW_VOLATILE? all=tvalue COMMA tptr=typ ptr=ident
    preceded(COMMA, align)?
    { EXPR_UNIT_Store (all, (tptr, ptr)) }

  | KW_ATOMICCMPXCHG { failwith"INSTR_AtomicCmpXchg" }
  | KW_ATOMICRMW     { failwith"INSTR_AtomicRMW"     }
  | KW_FENCE         { failwith"INSTR_Fence"         }

terminator_unit:

  | KW_RET t=typ o=value
    { TERM_UNIT_Ret (t, o) }

  | KW_RET KW_VOID
    { TERM_UNIT_Ret_void }

  | KW_BR t=typ o=value COMMA
    KW_LABEL o1=ident COMMA KW_LABEL o2=ident
    { TERM_UNIT_Br (o, o1, o2) }

  | KW_BR KW_LABEL o=ident
    { TERM_UNIT_Br_1 o }

  | KW_SWITCH t=typ v=value COMMA
    KW_LABEL def=value LSQUARE EOL? table=list(switch_table_entry) RSQUARE
    { TERM_UNIT_Switch (t, v, def, table) }

  | KW_INDIRECTBR
    { failwith "TERM_UNIT_IndirectBr" }

  | KW_RESUME t=typ o=value
    { TERM_UNIT_Resume (t, o) }

  | KW_UNREACHABLE
    { TERM_UNIT_Unreachable }

terminator:
  | KW_INVOKE cconv? t=ret_type i=ident
    LPAREN a=separated_list(COMMA, call_arg) RPAREN
    list(fn_attr) KW_TO KW_LABEL l1=ident
    KW_UNWIND KW_LABEL l2=ident
    { TERM_Invoke (t, i, a, l1, l2)  }

instr:
  | i=ident EQ e=expr       { INSTR_Expr_Assign (i, e) }
  | e=expr_unit             { INSTR_Expr_Unit e        }
  | i=ident EQ t=terminator { INSTR_Terminator (i, t)  }
  | t=terminator_unit       { INSTR_Terminator_Unit t  }

alloc_attr:
  | COMMA typ n=INTEGER             { n }
  | COMMA align                     { 1 }
  | COMMA typ n=INTEGER COMMA align { n }

phi_table_entry:
  | LSQUARE v=value COMMA l=ident RSQUARE { (v, l) }

switch_table_entry:
  | t=typ o=value COMMA KW_LABEL l=ident EOL? { (t, o, l) }

const:
  | i=INTEGER                                         { VALUE_Integer i        }
  | f=FLOAT                                           { VALUE_Float f          }
  | KW_TRUE                                           { VALUE_Bool true        }
  | KW_FALSE                                          { VALUE_Bool false       }
  | i=ident                                           { VALUE_Ident i          }
  | KW_NULL                                           { VALUE_Null             }
  | KW_UNDEF                                          { VALUE_Undef            }
  | KW_ZEROINITIALIZER                                { VALUE_Zero_initializer }
  | LCURLY l=separated_list(COMMA, tconst) RCURLY     { VALUE_Struct l         }
  | LTLCURLY l=separated_list(COMMA, tconst) RCURLYGT { VALUE_Struct l         }
  | LSQUARE l=separated_list(COMMA, tconst) RSQUARE   { VALUE_Array l          }
  | LT l=separated_list(COMMA, tconst) GT             { VALUE_Vector l         }

value:
  | c=const { c            }
  | e=expr  { VALUE_Expr e }

ident:
  | l=GLOBAL { ID_Global l }
  | l=LOCAL  { ID_Local l  }

tvalue: t=typ v=value { (t, v) }
tconst: t=typ c=const { (t, c) }
