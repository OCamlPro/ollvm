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
%token<int> CC
%token KW_CCC KW_FASTCC KW_COLDCC KW_CC
%token KW_UNNAMED_ADDR
%token KW_TYPE KW_X KW_OPAQUE
%token KW_GLOBAL KW_ADDRSPACE KW_CONSTANT KW_SECTION KW_THREAD_LOCAL
%token KW_ZEROEXT KW_SIGNEXT KW_INREG KW_BYVAL KW_SRET KW_NOALIAS KW_NOCAPTURE KW_NEST
%token KW_ADDRESS_SAFETY KW_ALIGNSTACK KW_ALWAYSINLINE KW_NONLAZYBIND KW_INLINEHINT KW_NAKED KW_NOIMPLICITFLOAT KW_NOINLINE KW_NOREDZONE KW_NORETURN KW_NOUNWIND KW_OPTSIZE KW_READNONE KW_READONLY KW_RETURNS_TWICE KW_SSP KW_SSPREQ KW_UWTABLE
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

module_:
  | EOL* m = list(toplevelentry_eol) EOF { m }

toplevelentry_eol:
  | tle = toplevelentry EOL+ { tle }

toplevelentry:
  | d = definition                        { TLE_Definition d              }
  | d = declaration                       { TLE_Declaration d             }
  | KW_TARGET KW_DATALAYOUT EQ s = STRING { TLE_Datalayout s              }
  | KW_TARGET KW_TRIPLE EQ s = STRING     { TLE_Target s                  }
  | i = LOCAL EQ KW_TYPE t = typ          { TLE_Type_decl (ID_Local i, t) }
  | g = global_decl                       { TLE_Global g                  }

global_decl:
  | ident = GLOBAL EQ
      linkage? visibility? KW_THREAD_LOCAL? addrspace? KW_UNNAMED_ADDR?
      g_constant = global_is_constant g_typ = typ g_value = value?
      global_attr?
      { {g_ident = ID_Global ident; g_typ; g_constant; g_value;} }

global_attr:
  | comma_section             { }
  | comma_section comma_align { }
  | comma_align               { }

%inline global_is_constant:
  | KW_GLOBAL { false }
  | KW_CONSTANT { true }

%inline addrspace:
  | KW_ADDRSPACE LPAREN n = INTEGER RPAREN { n }

%inline comma_section:
  | COMMA KW_SECTION s = STRING { s }

definition:
  | KW_DEFINE linkage? visibility? cconv?
           df_ret_typ = ret_type name = GLOBAL
           LPAREN df_args = separated_list(COMMA, df_arg) RPAREN
           list(fn_attr_gen) EOL?
           LCURLY EOL+
           df_instrs = procedure_body
           RCURLY
    { {df_ret_typ; df_name = ID_Global name; df_args; df_instrs;} }

declaration:
  | KW_DECLARE linkage? visibility? cconv? KW_UNNAMED_ADDR?
           dc_ret_typ = ret_type name = GLOBAL
           LPAREN dc_args = separated_list(COMMA, dc_arg) RPAREN
           list(fn_attr_gen)
    { {dc_ret_typ; dc_name = ID_Global name; dc_args;} }

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

%inline visibility:
  | KW_DEFAULT   { VISIBILITY_Default   }
  | KW_HIDDEN    { VISIBILITY_Hidden    }
  | KW_PROTECTED { VISIBILITY_Protected }

cconv:
  | KW_CCC          { CC_Ccc    }
  | KW_FASTCC       { CC_Fastcc }
  | KW_COLDCC       { CC_Coldcc }
  | KW_CC n=INTEGER { CC_Cc n   }
  | n=CC            { CC_Cc n   }

ret_type:
  | list(typ_attr) t = typ { t }

typ:
  | n = I                                    { TYPE_I n              }
  | KW_VOID                                  { TYPE_Void             }
  | KW_HALF                                  { TYPE_Half             }
  | KW_FLOAT                                 { TYPE_Float            }
  | KW_DOUBLE                                { TYPE_Double           }
  | KW_X86_FP80                              { TYPE_X86_fp80         }
  | KW_FP128                                 { TYPE_Fp128            }
  | KW_PPC_FP128                             { TYPE_Ppc_fp128        }
  | KW_LABEL                                 { TYPE_Label            }
  | KW_METADATA                              { TYPE_Metadata         }
  | KW_X86_MMX                               { TYPE_X86_mmx          }
  | t = typ STAR                             { TYPE_Pointer t        }
  | i = ident                                { TYPE_Ident i          }
  | LSQUARE n = INTEGER KW_X t = typ RSQUARE { TYPE_Array (n, t)     }
  | t = typ LPAREN ts = separated_list(COMMA, typ) RPAREN
                                             { TYPE_Function (t, ts) }
  | LCURLY ts = separated_list(COMMA, typ) RCURLY
                                             { TYPE_Struct ts        }
  | LTLCURLY ts = separated_list(COMMA, typ) RCURLYGT
                                             { TYPE_Packed_struct ts }
  | KW_OPAQUE                                { TYPE_Opaque           }
  | LT n = INTEGER KW_X t = typ GT           { TYPE_Vector (n, t)    }

%inline typ_i:
  | n = I { n }

typ_attr:
  | KW_ZEROEXT   { TYPEATTR_Zeroext   }
  | KW_SIGNEXT   { TYPEATTR_Signext   }
  | KW_INREG     { TYPEATTR_Inreg     }
  | KW_BYVAL     { TYPEATTR_Byval     }
  | KW_SRET      { TYPEATTR_Sret      }
  | KW_NOALIAS   { TYPEATTR_Noalias   }
  | KW_NOCAPTURE { TYPEATTR_Nocapture }
  | KW_NEST      { TYPEATTR_Nest      }

df_arg:
  | t = typ list(typ_attr) i = ident { (t, i) }

dc_arg:
  | t = typ list(typ_attr) { t }

call_arg:
  | t = typ list(typ_attr) i = value { (t, i) }

fn_attr:
  | KW_ADDRESS_SAFETY                      { Some FNATTR_Address_safety  }
  | KW_ALIGNSTACK LPAREN p = power2 RPAREN { Some (FNATTR_Alignstack p)  }
  | KW_ALWAYSINLINE                        { Some FNATTR_Alwaysinline    }
  | KW_NONLAZYBIND                         { Some FNATTR_Nonlazybind     }
  | KW_INLINEHINT                          { Some FNATTR_Inlinehint      }
  | KW_NAKED                               { Some FNATTR_Naked           }
  | KW_NOIMPLICITFLOAT                     { Some FNATTR_Noimplicitfloat }
  | KW_NOINLINE                            { Some FNATTR_Noinline        }
  | KW_NOREDZONE                           { Some FNATTR_Noredzone       }
  | KW_NORETURN                            { Some FNATTR_Noreturn        }
  | KW_NOUNWIND                            { Some FNATTR_Nounwind        }
  | KW_OPTSIZE                             { Some FNATTR_Optsize         }
  | KW_READNONE                            { Some FNATTR_Readnone        }
  | KW_READONLY                            { Some FNATTR_Readonly        }
  | KW_RETURNS_TWICE                       { Some FNATTR_Returns_twice   }
  | KW_SSP                                 { Some FNATTR_Ssp             }
  | KW_SSPREQ                              { Some FNATTR_Sspreq          }
  | KW_UWTABLE                             { Some FNATTR_Uwtable         }

fn_attr_gen:
  | f = fn_attr { Some f }
  | KW_ALIGN power2   { None }
  | KW_GC STRING      { None }
  | KW_SECTION STRING { None }

%inline power2:
  | n = INTEGER { assert (List.mem n [0;1;2;4;8;16;32;64]); n }

%inline align:
  | KW_ALIGN p = power2 { p }

%inline procedure_body:
  | il = list(instr_eol) { il }

%inline instr_eol:
  | i = instr EOL+ { i }

%public binop_expr(KW):
  | KW LPAREN t = typ o1 = value COMMA t2 = typ o2 = value RPAREN
    { assert (t = t2); (t, o1, o2) }

%public binop1_expr(KW,OPT1):
  | KW OPT1? LPAREN t = typ o1 = value COMMA t2 = typ o2 = value RPAREN
    { assert (t = t2); (t, o1, o2) }

%public binop2_expr(KW,OPT1,OPT2):
  | KW OPT1? OPT2? LPAREN t = typ o1 = value COMMA t2 = typ o2 = value RPAREN
    { assert (t = t2); (t, o1, o2) }

%public conversion_expr(KW):
  | KW LPAREN t = typ v = value KW_TO t2 = typ RPAREN
    { (t, v, t2) }

%public icmp_expr(KW):
  | KW_ICMP KW LPAREN t = typ o1 = value COMMA t2 = typ o2 = value RPAREN
    { assert (t = t2); (t, o1, o2) }

expr:
  (* arith binop *)
  | b = binop2_expr(KW_ADD,KW_NUW,KW_NSW) { EXPR_Add  b }
  | KW_FADD                               { failwith "EXPR_FAdd"   }
  | b = binop2_expr(KW_SUB,KW_NUW,KW_NSW) { EXPR_Sub  b }
  | KW_FSUB                               { failwith "EXPR_FSub"   }
  | b = binop2_expr(KW_MUL,KW_NUW,KW_NSW) { EXPR_Mul  b }
  | KW_FMUL                               { failwith "EXPR_FMul"   }
  | b = binop1_expr(KW_UDIV,KW_EXACT)     { EXPR_UDiv b }
  | b = binop1_expr(KW_SDIV,KW_EXACT)     { EXPR_SDiv b }
  | KW_FDIV                               { failwith "EXPR_FDiv"   }
  | b = binop_expr (KW_UREM)              { EXPR_URem b }
  | b = binop_expr (KW_SREM)              { EXPR_SRem b }
  | KW_FREM                               { failwith "EXPR_FRem"   }

  (* bitwise binop *)
  | b = binop2_expr(KW_SHL,KW_NUW,KW_NSW) { EXPR_Shl  b }
  | b = binop1_expr(KW_LSHR,KW_EXACT)     { EXPR_LShr b }
  | b = binop1_expr(KW_ASHR,KW_EXACT)     { EXPR_AShr b }
  | b = binop_expr (KW_AND)               { EXPR_And  b }
  | b = binop_expr (KW_OR)                { EXPR_Or   b }
  | b = binop_expr (KW_XOR)               { EXPR_Xor  b }

  (* comparison *)
  | i = icmp_expr(KW_EQ ) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Eq,  t, o1, o2) }
  | i = icmp_expr(KW_NE ) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Ne,  t, o1, o2) }
  | i = icmp_expr(KW_UGT) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Ugt, t, o1, o2) }
  | i = icmp_expr(KW_UGE) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Uge, t, o1, o2) }
  | i = icmp_expr(KW_ULT) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Ult, t, o1, o2) }
  | i = icmp_expr(KW_ULE) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Ule, t, o1, o2) }
  | i = icmp_expr(KW_SGT) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Sgt, t, o1, o2) }
  | i = icmp_expr(KW_SGE) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Sge, t, o1, o2) }
  | i = icmp_expr(KW_SLT) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Slt, t, o1, o2) }
  | i = icmp_expr(KW_SLE) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Sle, t, o1, o2) }

  | KW_FCMP           { failwith "KW_FCMP" }

  (* conversions *)
  | c = conversion_expr(KW_TRUNC)    { EXPR_Trunc c    }
  | c = conversion_expr(KW_ZEXT)     { EXPR_ZExt c     }
  | c = conversion_expr(KW_SEXT)     { EXPR_SExt c     }
  | c = conversion_expr(KW_FPTRUNC)  { EXPR_FPTrunc c  }
  | c = conversion_expr(KW_FPEXT)    { EXPR_FPExt c    }
  | c = conversion_expr(KW_UITOFP)   { EXPR_UIToFP c   }
  | c = conversion_expr(KW_SITOFP)   { EXPR_SIToFP c   }
  | c = conversion_expr(KW_FPTOUI)   { EXPR_FPToUI c   }
  | c = conversion_expr(KW_FPTOSI)   { EXPR_FPToSI c   }
  | c = conversion_expr(KW_INTTOPTR) { EXPR_IntToPtr c }
  | c = conversion_expr(KW_PTRTOINT) { EXPR_PtrToInt c }
  | c = conversion_expr(KW_BITCAST)  { EXPR_BitCast c  }

  | KW_GETELEMENTPTR ?KW_INBOUNDS LPAREN t = typ v = value
    ptrs = list(getelementptr_item) RPAREN
    { EXPR_GetElementPtr (t, v, ptrs) }

  (* vector ops, not supported *)
  | KW_EXTRACTELEMENT { failwith "EXPR_ExtractElement" }
  | KW_INSERTELEMENT  { failwith "EXPR_InsertElement"  }
  | KW_SHUFFLEVECTOR  { failwith "EXPR_ShuffleVector"  }

  (* aggregate ops, not supported *)
  | KW_EXTRACTVALUE { failwith "EXPR_ExtractValue" }
  | KW_INSERTVALUE  { failwith "EXPR_InsertValue"  }

getelementptr_item:
  | COMMA t = typ v = value { (t, v) }

%public binop_assign(KW):
  | KW t = typ o1 = value COMMA o2 = value
    { (t, o1, o2) }

%public binop1_assign(KW,OPT1):
  | KW OPT1? t = typ o1 = value COMMA o2 = value
    { (t, o1, o2) }

%public binop2_assign(KW,OPT1,OPT2):
  | KW OPT1? OPT2? t = typ o1 = value COMMA o2 = value
    { (t, o1, o2) }

%public conversion_assign(KW):
  | KW t = typ v = value KW_TO t2 = typ
    { (t, v, t2) }

%public icmp_assign(KW):
  | KW_ICMP KW t = typ o1 = value COMMA o2 = value
    { (t, o1, o2) }

expr_assign:
  (* arith binop *)
  | b = binop2_assign(KW_ADD,KW_NUW,KW_NSW) { EXPR_Add  b }
  | KW_FADD                                 { failwith "EXPR_FAdd"   }
  | b = binop2_assign(KW_SUB,KW_NUW,KW_NSW) { EXPR_Sub  b }
  | KW_FSUB                                 { failwith "EXPR_FSub"   }
  | b = binop2_assign(KW_MUL,KW_NUW,KW_NSW) { EXPR_Mul  b }
  | KW_FMUL                                 { failwith "EXPR_FMul"   }
  | b = binop1_assign(KW_UDIV,KW_EXACT)     { EXPR_UDiv b }
  | b = binop1_assign(KW_SDIV,KW_EXACT)     { EXPR_SDiv b }
  | KW_FDIV                                 { failwith "EXPR_FDiv"   }
  | b = binop_assign (KW_UREM)              { EXPR_URem b }
  | b = binop_assign (KW_SREM)              { EXPR_SRem b }
  | KW_FREM                                 { failwith "EXPR_FRem"   }

  (* bitwise binop *)
  | b = binop2_assign(KW_SHL,KW_NUW,KW_NSW) { EXPR_Shl  b }
  | b = binop1_assign(KW_LSHR,KW_EXACT)     { EXPR_LShr b }
  | b = binop1_assign(KW_ASHR,KW_EXACT)     { EXPR_AShr b }
  | b = binop_assign (KW_AND)               { EXPR_And  b }
  | b = binop_assign (KW_OR)                { EXPR_Or   b }
  | b = binop_assign (KW_XOR)               { EXPR_Xor  b }

  (* comparison *)
  | i = icmp_assign(KW_EQ ) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Eq,  t, o1, o2) }
  | i = icmp_assign(KW_NE ) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Ne,  t, o1, o2) }
  | i = icmp_assign(KW_UGT) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Ugt, t, o1, o2) }
  | i = icmp_assign(KW_UGE) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Uge, t, o1, o2) }
  | i = icmp_assign(KW_ULT) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Ult, t, o1, o2) }
  | i = icmp_assign(KW_ULE) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Ule, t, o1, o2) }
  | i = icmp_assign(KW_SGT) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Sgt, t, o1, o2) }
  | i = icmp_assign(KW_SGE) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Sge, t, o1, o2) }
  | i = icmp_assign(KW_SLT) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Slt, t, o1, o2) }
  | i = icmp_assign(KW_SLE) { let t, o1, o2 = i in EXPR_ICmp (Cmp_Sle, t, o1, o2) }

  | KW_FCMP           { failwith "KW_FCMP" }

  (* conversions *)
  | c = conversion_assign(KW_TRUNC)    { EXPR_Trunc c    }
  | c = conversion_assign(KW_ZEXT)     { EXPR_ZExt c     }
  | c = conversion_assign(KW_SEXT)     { EXPR_SExt c     }
  | c = conversion_assign(KW_FPTRUNC)  { EXPR_FPTrunc c  }
  | c = conversion_assign(KW_FPEXT)    { EXPR_FPExt c    }
  | c = conversion_assign(KW_UITOFP)   { EXPR_UIToFP c   }
  | c = conversion_assign(KW_SITOFP)   { EXPR_SIToFP c   }
  | c = conversion_assign(KW_FPTOUI)   { EXPR_FPToUI c   }
  | c = conversion_assign(KW_FPTOSI)   { EXPR_FPToSI c   }
  | c = conversion_assign(KW_INTTOPTR) { EXPR_IntToPtr c }
  | c = conversion_assign(KW_PTRTOINT) { EXPR_PtrToInt c }
  | c = conversion_assign(KW_BITCAST)  { EXPR_BitCast c  }

  | KW_GETELEMENTPTR ?KW_INBOUNDS t = typ v = value
    ptrs = list(getelementptr_item)
    { EXPR_GetElementPtr (t, v, ptrs) }

  (* vector ops, not supported *)
  | KW_EXTRACTELEMENT { failwith "EXPR_ExtractElement" }
  | KW_INSERTELEMENT  { failwith "EXPR_InsertElement"  }
  | KW_SHUFFLEVECTOR  { failwith "EXPR_ShuffleVector"  }

  (* aggregate ops, not supported *)
  | KW_EXTRACTVALUE { failwith "EXPR_ExtractValue" }
  | KW_INSERTVALUE  { failwith "EXPR_InsertValue"  }

call:
  | KW_TAIL? KW_CALL cconv? list(typ_attr) t = typ
                 n = ident LPAREN a = separated_list(COMMA, call_arg) RPAREN
                 list(fn_attr)
    { (t, n, a) }

instr:
  (* assignement and calls *)
  | i = ident EQ e = expr_assign { INSTR_Assign (i, e) }
  | i = ident EQ c = call        { INSTR_Call   (i, c) }
  | c = call                     { INSTR_Call_unit  c  }

  (* phi *)
  | i = ident EQ KW_PHI t = typ
                     table = separated_nonempty_list(COMMA, phi_table_entry)
    { INSTR_PHI (i, t, table) }


  (* other *)
  | i = ident EQ KW_SELECT t = typ v = value COMMA
      t1 = typ v1 = value COMMA t2 = typ v2 = value
    { assert (match t with
        | TYPE_I n -> n=1
        | TYPE_Struct l -> List.for_all ((=) (TYPE_I 1)) l
        | TYPE_Pointer _ | TYPE_Void | TYPE_Half | TYPE_Float | TYPE_Double
        | TYPE_X86_fp80 | TYPE_Fp128 | TYPE_Ppc_fp128 | TYPE_Label
        | TYPE_Metadata | TYPE_X86_mmx | TYPE_Ident _ | TYPE_Array _
        | TYPE_Function _ | TYPE_Packed_struct _ | TYPE_Opaque | TYPE_Vector _
        -> false
      );
      assert (t1 = t2);
      INSTR_Select (i, t, v, t1, v1, v2)
    }
  | KW_VAARG  { failwith"INSTR_VAArg"  }

  (* terminator *)
  | KW_RET t = typ o = value { INSTR_Terminator (TERM_Ret (t, o)) }
  | KW_RET KW_VOID           { INSTR_Terminator (TERM_Ret_void) }
  | KW_BR t = typ_i o = value COMMA
          KW_LABEL o1 = ident COMMA KW_LABEL o2 = ident
    { assert (t = 1); INSTR_Terminator (TERM_Br (o, o1, o2)) }
  | KW_BR KW_LABEL o = ident       { INSTR_Terminator (TERM_Br_1 o) }
  | KW_SWITCH t = typ v = value COMMA
              KW_LABEL def = value
              LSQUARE EOL? table = list(switch_table_entry) RSQUARE
    { INSTR_Terminator (TERM_Switch (t, v, def, table)) }
  | KW_INDIRECTBR { failwith "TERM_IndirectBr" }
  | KW_INVOKE cconv? t = ret_type i = ident
              LPAREN a = separated_list(COMMA, call_arg) RPAREN
              list(fn_attr)
              KW_TO KW_LABEL l1 = ident
              KW_UNWIND KW_LABEL l2 = ident
    { INSTR_Terminator (TERM_Invoke (t, i, a, l1, l2))  }
  | KW_RESUME t = typ o = value { INSTR_Terminator (TERM_Resume (t, o)) }
  | KW_UNREACHABLE    { INSTR_Terminator (TERM_Unreachable) }

  (* memory instrs, partial support *)
  | i = ident EQ KW_ALLOCA t = typ n = alloc_attr?
    { let n = Util.O.unopt 1 n in INSTR_Mem (MEM_Alloca (i, n, t)) }
  | i = ident EQ KW_LOAD KW_VOLATILE? tp = typ v = value comma_align? (*TODO: support more options *)
    { INSTR_Mem (MEM_Load (i, tp, v)) }
  | KW_STORE KW_VOLATILE? tv = typ v = value COMMA
                          ti = typ i = ident
                          comma_align? (*TODO: support atomic and non-temporal*)
    { assert (match ti with | TYPE_Pointer _ -> true | _ -> false);
      INSTR_Mem (MEM_Store (tv, v, ti, i)) }

  (* others *)
  | KW_ATOMICCMPXCHG { failwith"INSTR_AtomicCmpXchg" }
  | KW_ATOMICRMW     { failwith"INSTR_AtomicRMW"     }
  | KW_FENCE         { failwith"INSTR_Fence"         }
  | KW_LANDINGPAD    { failwith"INSTR_LandingPad"    }

  (* explicit labels *)
  | l = LABEL { INSTR_Label (ID_Local l) }

alloc_attr:
  | COMMA n = num_elem { n }
  | COMMA align { 1 }
  | COMMA n = num_elem COMMA align { n }

num_elem:
  | typ_i n = INTEGER { n }

comma_align:
  | COMMA align { }

%inline phi_table_entry:
  | LSQUARE v = value COMMA l = ident RSQUARE { (v, l) }

%inline switch_table_entry:
  | t = typ o = value COMMA KW_LABEL l = ident EOL? { (t, o, l) }

value:
  | i = INTEGER        { VALUE_Integer i  }
  | f = FLOAT          { VALUE_Float f    }
  | KW_TRUE            { VALUE_Bool true  }
  | KW_FALSE           { VALUE_Bool false }
  | i = ident          { VALUE_Ident i    }
  | KW_NULL            { VALUE_Null       }
  | KW_UNDEF           { VALUE_Undef      }
  | LCURLY l = separated_list(COMMA, typ_value) RCURLY
                       { VALUE_Struct l }
  | LTLCURLY l = separated_list(COMMA, typ_value) RCURLYGT
                       { VALUE_Struct l }
  | LSQUARE l = separated_list(COMMA, typ_value) RSQUARE
                       { VALUE_Array l }
  | LT l = separated_list(COMMA, typ_value) GT
                       { VALUE_Vector l }
  | KW_ZEROINITIALIZER { VALUE_Zero_initializer }
  | e = expr           { VALUE_Expr e }

%inline typ_value:
  | t = typ i = value { (t,i) }

%inline ident:
  | l = GLOBAL { ID_Global l }
  | l = LOCAL  { ID_Local l  }
