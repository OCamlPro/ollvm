open Format
open Ast

let pp_sep str =
  fun ppf () -> pp_print_string ppf str

let pp_space ppf () = pp_print_char ppf ' '

let pp_comma_space ppf () = pp_print_string ppf ", "

let rec linkage : Format.formatter -> Ast.linkage -> unit =
  fun ppf ->
  function
  | LINKAGE_Private              -> fprintf ppf "private"
  | LINKAGE_Internal             -> fprintf ppf "internal"
  | LINKAGE_Available_externally -> fprintf ppf "available_externally"
  | LINKAGE_Linkonce             -> fprintf ppf "linkonce"
  | LINKAGE_Weak                 -> fprintf ppf "weak"
  | LINKAGE_Common               -> fprintf ppf "common"
  | LINKAGE_Appending            -> fprintf ppf "appending"
  | LINKAGE_Extern_weak          -> fprintf ppf "extern_weak"
  | LINKAGE_Linkonce_odr         -> fprintf ppf "linkonce_ord"
  | LINKAGE_Weak_odr             -> fprintf ppf "weak_odr"
  | LINKAGE_External             -> fprintf ppf "external"

 and dll_storage : Format.formatter -> Ast.dll_storage -> unit =
   fun ppf ->
   function
   | DLLSTORAGE_Dllimport -> fprintf ppf "dllimport"
   | DLLSTORAGE_Dllexport -> fprintf ppf "dllexport"

and visibility : Format.formatter -> Ast.visibility -> unit =
  fun ppf ->
  function
  | VISIBILITY_Default   -> fprintf ppf "default"
  | VISIBILITY_Hidden    -> fprintf ppf "hidden"
  | VISIBILITY_Protected -> fprintf ppf "protected"

and cconv : Format.formatter -> Ast.cconv -> unit =
  fun ppf -> function
          | CC_Ccc    -> fprintf ppf "ccc"
          | CC_Fastcc -> fprintf ppf "fastcc"
          | CC_Coldcc -> fprintf ppf "coldcc"
          | CC_Cc i   -> fprintf ppf "cc %d" i

and param_attr : Format.formatter -> Ast.param_attr -> unit =
  fun ppf ->
  function
  | PARAMATTR_Zeroext           -> fprintf ppf "zeroext"
  | PARAMATTR_Signext           -> fprintf ppf "signext"
  | PARAMATTR_Inreg             -> fprintf ppf "inreg"
  | PARAMATTR_Byval             -> fprintf ppf "byval"
  | PARAMATTR_Inalloca          -> fprintf ppf "inalloca"
  | PARAMATTR_Sret              -> fprintf ppf "sret"
  | PARAMATTR_Align n           -> fprintf ppf "align %d" n
  | PARAMATTR_Noalias           -> fprintf ppf "noalias"
  | PARAMATTR_Nocapture         -> fprintf ppf "nocapture"
  | PARAMATTR_Nest              -> fprintf ppf "nest"
  | PARAMATTR_Returned          -> fprintf ppf "returned"
  | PARAMATTR_Nonnull           -> fprintf ppf "nonnull"
  | PARAMATTR_Dereferenceable n -> fprintf ppf "dereferenceable(%d)" n

and fn_attr : Format.formatter -> Ast.fn_attr -> unit =
  fun ppf ->
  function
  | FNATTR_Alignstack i     -> fprintf ppf "alignstack(%d)" i
  | FNATTR_Alwaysinline     -> fprintf ppf "alwaysinline"
  | FNATTR_Builtin          -> fprintf ppf "builtin"
  | FNATTR_Cold             -> fprintf ppf "cold"
  | FNATTR_Inlinehint       -> fprintf ppf "inlinehint"
  | FNATTR_Jumptable        -> fprintf ppf "jumptable"
  | FNATTR_Minsize          -> fprintf ppf "minsize"
  | FNATTR_Naked            -> fprintf ppf "naked"
  | FNATTR_Nobuiltin        -> fprintf ppf "nobuiltin"
  | FNATTR_Noduplicate      -> fprintf ppf "noduplicate"
  | FNATTR_Noimplicitfloat  -> fprintf ppf "noimplicitfloat"
  | FNATTR_Noinline         -> fprintf ppf "noinline"
  | FNATTR_Nonlazybind      -> fprintf ppf "nonlazybind"
  | FNATTR_Noredzone        -> fprintf ppf "noredzone"
  | FNATTR_Noreturn         -> fprintf ppf "noreturn"
  | FNATTR_Nounwind         -> fprintf ppf "nounwind"
  | FNATTR_Optnone          -> fprintf ppf "optnone"
  | FNATTR_Optsize          -> fprintf ppf "optsize"
  | FNATTR_Readnone         -> fprintf ppf "readone"
  | FNATTR_Readonly         -> fprintf ppf "readonly"
  | FNATTR_Returns_twice    -> fprintf ppf "returns_twice"
  | FNATTR_Sanitize_address -> fprintf ppf "sanitize_address"
  | FNATTR_Sanitize_memory  -> fprintf ppf "sanitize_memory"
  | FNATTR_Sanitize_thread  -> fprintf ppf "sanitize_thread"
  | FNATTR_Ssp              -> fprintf ppf "ssp"
  | FNATTR_Sspreq           -> fprintf ppf "sspreq"
  | FNATTR_Sspstrong        -> fprintf ppf "sspstrong"
  | FNATTR_Uwtable          -> fprintf ppf "uwtable"
  | FNATTR_String s         -> fprintf ppf "\"%s\"" s
  | FNATTR_Key_value (k, v) -> fprintf ppf "\"%s\"=\"%s\"" k v
  | FNATTR_Attr_grp i       -> fprintf ppf "#%d" i

and ident : Format.formatter -> Ast.ident -> unit =
  fun ppf ->
  function
  | ID_Global (f, i) -> pp_print_char ppf '@' ; ident_format ppf f i
  | ID_Local (f, i)  -> pp_print_char ppf '%' ; ident_format ppf f i

(** FIXME: see #4 *)
and ident_format : Format.formatter -> Ast.ident_format -> string -> unit =
  fun ppf f i -> match f with
             | ID_FORMAT_Named
             | ID_FORMAT_Unnamed     -> fprintf ppf "%s" i
             | ID_FORMAT_NamedString -> fprintf ppf "\"%s\"" i

and typ : Format.formatter -> Ast.typ -> unit =
  fun ppf ->
  function
  | TYPE_I i              -> fprintf ppf "i%d" i
  | TYPE_Pointer t        -> fprintf ppf "%a*" typ t ;
  | TYPE_Void             -> fprintf ppf "void"
  | TYPE_Half             -> fprintf ppf "half"
  | TYPE_Float            -> fprintf ppf "float"
  | TYPE_Double           -> fprintf ppf "double"
  | TYPE_Label            -> fprintf ppf "label"
  | TYPE_X86_fp80         -> assert false
  | TYPE_Fp128            -> assert false
  | TYPE_Ppc_fp128        -> assert false
  | TYPE_Metadata         -> fprintf ppf "metadata"
  | TYPE_X86_mmx          -> assert false
  | TYPE_Array (i, t)     -> fprintf ppf "[%d x %a]" i typ t ;
  | TYPE_Function (t, tl) -> assert false (* (t, tl) : Format.formatter -> (typ * typ list) *)
  | TYPE_Struct tl        -> fprintf ppf "{%a}"
                                     (pp_print_list ~pp_sep:pp_comma_space typ) tl
  | TYPE_Packed_struct tl -> fprintf ppf "<{%a}>"
                                     (pp_print_list ~pp_sep:pp_comma_space typ) tl
  | TYPE_Opaque           -> assert false
  | TYPE_Vector (i, t)    -> fprintf ppf "<%d x %a>" i typ t ;


and icmp : Format.formatter -> Ast.icmp -> unit =
  fun ppf icmp ->
  fprintf ppf ( match icmp with
                | Eq  -> "eq"
                | Ne  -> "neq"
                | Ugt -> "ugt"
                | Uge -> "uge"
                | Ult -> "ult"
                | Ule -> "ule"
                | Sgt -> "sgt"
                | Sge -> "sge"
                | Slt -> "slt"
                | Sle -> "cmp")

and fcmp : Format.formatter -> Ast.fcmp -> unit =
  fun ppf fcmp ->
  fprintf ppf ( match fcmp with
                | False -> "false"
                | Oeq   -> "oeq"
                | Ogt   -> "ogt"
                | Oge   -> "oge"
                | Olt   -> "olt"
                | Ole   -> "ole"
                | One   -> "one"
                | Ord   -> "ord"
                | Uno   -> "uno"
                | Ueq   -> "ueq"
                | Ugt   -> "ugt"
                | Uge   -> "uge"
                | Ult   -> "ult"
                | Ule   -> "ule"
                | Une   -> "une"
                | True  -> "true")


and ibinop : Format.formatter -> Ast.ibinop -> unit =
  fun ppf ->
  let nuw ppf flag = if flag then fprintf ppf " nuw" in
  let nsw ppf flag = if flag then fprintf ppf " nsw" in
  let exact ppf flag = if flag then fprintf ppf " exact" in
  function
  | Add (nu, ns) -> fprintf ppf "add" ; nuw ppf nu ; nsw ppf ns
  | Sub (nu, ns) -> fprintf ppf "sub" ; nuw ppf nu ; nsw ppf ns
  | Mul (nu, ns) -> fprintf ppf "mul" ; nuw ppf nu ; nsw ppf ns
  | UDiv e       -> fprintf ppf "udiv" ; exact ppf e
  | SDiv e       -> fprintf ppf "sdiv" ; exact ppf e
  | URem         -> fprintf ppf "urem"
  | SRem         -> fprintf ppf "srem"
  | Shl (nu, ns) -> fprintf ppf "shl" ; nuw ppf nu ; nsw ppf ns
  | LShr e       -> fprintf ppf "lshr" ; exact ppf e
  | AShr e       -> fprintf ppf "ashr" ; exact ppf e
  | And          -> fprintf ppf "and"
  | Or           -> fprintf ppf "or"
  | Xor          -> fprintf ppf "xor"

and fbinop =
  fun ppf fbinop ->
  fprintf ppf (match fbinop with
                 | FAdd -> "fadd"
                 | FSub -> "fsub"
                 | FMul -> "fmul"
                 | FDiv -> "fdiv"
                 | FRem -> "frem")

and fast_math =
  fun ppf fast_math ->
  pp_print_string ppf (match fast_math with
                       | Nnan -> "nnan"
                       | Ninf -> "ninf"
                       | Nsz  -> "nsz"
                       | Arcp -> "arcp"
                       | Fast -> "fast")

and conversion_type : Format.formatter -> Ast.conversion_type -> unit =
  fun ppf conv ->
  fprintf ppf (match conv with
               | Trunc    -> "trunc"
               | Zext     -> "zext"
               | Sext     -> "sext"
               | Fptrunc  -> "fptrunc"
               | Fpext    -> "fpext"
               | Uitofp   -> "uitofp"
               | Sitofp   -> "sitofp"
               | Fptoui   -> "fptoui"
               | Fptosi   -> "fptosi"
               | Inttoptr -> "inttoptr"
               | Ptrtoint -> "ptrtoint"
               | Bitcast  -> "bitcast")

and instr : Format.formatter -> Ast.instr -> unit =
  fun ppf ->
  function

  | INSTR_IBinop (op, t, v1, v2) ->
     fprintf ppf "%a %a %a, %a"
             ibinop op
             typ t
             value v1
             value v2

  | INSTR_ICmp (c, t, v1, v2) ->
     fprintf ppf "icmp %a %a %a, %a"
             icmp c
             typ t
             value v1
             value v2

  | INSTR_FBinop (op, f, t, v1, v2) ->
     fbinop ppf op ;
     if f <> [] then (pp_space ppf () ;
                      pp_print_list ~pp_sep:pp_space fast_math ppf f) ;
     fprintf ppf " %a %a, %a"
             typ t
             value v1
             value v2

  | INSTR_FCmp (c, t, v1, v2) ->
     fprintf ppf "fcmp %a %a %a, %a"
             fcmp c
             typ t
             value v1
             value v2

  | INSTR_Conversion (c, t1, v, t2) ->
     fprintf ppf "%a %a %a to %a"
             conversion_type c
             typ t1
             value v
             typ t2

  | INSTR_GetElementPtr (tv, tvl) ->
     fprintf ppf "getelementptr %a, %a"
             tvalue tv
             (pp_print_list ~pp_sep:pp_comma_space tvalue) tvl

  | INSTR_Call (ti, tvl) ->
     fprintf ppf "call %a(%a)"
             tident ti
             (pp_print_list ~pp_sep:pp_comma_space tvalue) tvl

  | INSTR_Alloca (t, n, a) ->
     fprintf ppf "alloca %a" typ t ;
     (match n with None -> ()
                 | Some n -> fprintf ppf ", %a" tvalue n) ;
     (match a with None -> ()
                 | Some a -> fprintf ppf ", align %d" a)

  | INSTR_Load (vol, tv, a) ->
     pp_print_string ppf "load " ;
     if vol then pp_print_string ppf "volatile " ;
     tvalue ppf tv ;
     (match a with None -> ()
                 | Some a -> fprintf ppf ", align %d" a)

  | INSTR_Phi (t, vil) ->
     fprintf ppf "phi %a [%a]"
             typ t
             (pp_print_list ~pp_sep:(pp_sep "], [")
                            (fun ppf (v, i) -> value ppf v ;
                                               pp_print_string ppf ", " ;
                                               ident ppf i)) vil

  | INSTR_Select (if_, then_, else_) ->
     fprintf ppf "select %a, %a, %a"
             tvalue if_
             tvalue then_
             tvalue else_

  | INSTR_VAArg -> pp_print_string ppf "vaarg"

  | INSTR_ExtractElement (vec, idx) ->
     fprintf ppf "extractelement %a, %a"
             tvalue vec
             tvalue idx

  | INSTR_InsertElement (vec, new_val, idx) ->
     fprintf ppf "insertelement %a, %a, %a"
             tvalue vec
             tvalue new_val
             tvalue idx

  | INSTR_ExtractValue (agg, idx) ->
     fprintf ppf "extractvalue %a, %a"
             tvalue agg
             (pp_print_list ~pp_sep:pp_comma_space pp_print_int) idx

  | INSTR_InsertValue (agg, new_val, idx) ->
     fprintf ppf "insertvalue %a, %a, %a"
             tvalue agg
             tvalue new_val
             (pp_print_list ~pp_sep:pp_comma_space pp_print_int) idx

  | INSTR_ShuffleVector (v1, v2, mask) ->
     fprintf ppf "shufflevector %a, %a, %a"
             tvalue v1
             tvalue v2
             tvalue mask

  | INSTR_LandingPad -> assert false

  | INSTR_Store (vol, v, ptr, a) ->
     pp_print_string ppf "store " ;
     if vol then pp_print_string ppf "volatile " ;
     fprintf ppf "%a, %a" tvalue v tident ptr ;
     (match a with None -> ()
                 | Some a -> fprintf ppf ", align %d" a)

  | INSTR_AtomicCmpXchg
  | INSTR_AtomicRMW
  | INSTR_Fence -> assert false

  | INSTR_Ret (t, v)       -> fprintf ppf "ret %a" tvalue (t, v)

  | INSTR_Ret_void         -> pp_print_string ppf "ret void"

  | INSTR_Br (c, i1, i2)   ->
     fprintf ppf "br %a, %a, %a" tvalue c tident i1 tident i2

  | INSTR_Br_1 (t, i)       -> fprintf ppf "br %a %a" typ t ident i

  | INSTR_Switch (c, def, cases) ->
     fprintf ppf "switch %a, %a [%a]"
             tvalue c
             tident def
             (pp_print_list ~pp_sep:pp_space
                            (fun ppf (v, i) -> tvalue ppf v ;
                                               pp_print_string ppf ", " ;
                                               tident ppf i)) cases

  | INSTR_Resume (t, v) -> fprintf ppf "resume %a" tvalue (t, v)

  | INSTR_Unreachable -> pp_print_string ppf "unreachable"

  | INSTR_IndirectBr     -> assert false

  | INSTR_Invoke (ti, tvl, i2, i3) ->
     fprintf ppf "invoke %a(%a) to %a unwind %a"
             tident ti
             (pp_print_list ~pp_sep:pp_comma_space tvalue) tvl
             tident i2
             tident i3

  | INSTR_Assign (id, inst) -> fprintf ppf "%a = %a" ident id instr inst

and value : Format.formatter -> Ast.value -> unit =
  fun ppf ->
  function
  | VALUE_Ident i           -> ident ppf i
  | VALUE_Integer i         -> pp_print_int ppf i
  | VALUE_Float f           -> pp_print_float ppf f
  | VALUE_Bool b            -> pp_print_bool ppf b
  | VALUE_Null              -> pp_print_string ppf "null"
  | VALUE_Undef             -> pp_print_string ppf "undef"
  | VALUE_Array tvl         -> fprintf ppf "[%a]"
                                       (pp_print_list ~pp_sep:pp_comma_space tvalue) tvl
  | VALUE_Vector tvl        -> fprintf ppf "<%a>"
                                       (pp_print_list ~pp_sep:pp_comma_space tvalue) tvl
  | VALUE_Struct tvl        -> fprintf ppf "{%a}"
                                       (pp_print_list ~pp_sep:pp_comma_space tvalue) tvl
  | VALUE_Packed_struct tvl -> fprintf ppf "<{%a}>"
                                       (pp_print_list ~pp_sep:pp_comma_space tvalue) tvl
  | VALUE_Zero_initializer  -> pp_print_string ppf "zeroinitializer"

and tvalue  = fun ppf (t, v) -> fprintf ppf "%a %a" typ t value v

and tident  = fun ppf (t, v) -> fprintf ppf "%a %a" typ t ident v

and toplevelentries : Format.formatter -> Ast.toplevelentries -> unit =
  fun ppf entries ->
  pp_print_list ~pp_sep:pp_force_newline toplevelentry ppf entries

and toplevelentry : Format.formatter -> Ast.toplevelentry -> unit =
  fun ppf ->
  function
  | TLE_Target s               -> fprintf ppf "target triple = \"%s\"" s
  | TLE_Datalayout s           -> fprintf ppf "target datalayout = \"%s\"" s
  | TLE_Declaration d          -> declaration ppf d
  | TLE_Definition d           -> definition ppf d
  | TLE_Type_decl (i, t)       -> fprintf ppf "%a %a" ident i typ t
  | TLE_Global g               -> global ppf g
  | TLE_Metadata (i, m)        -> fprintf ppf "!%s = %a" i metadata m
  | TLE_Attribute_group (i, a) -> fprintf ppf "#%d = { %a }" i
                                          (pp_print_list ~pp_sep:pp_space fn_attr) a

and metadata : Format.formatter -> Ast.metadata -> unit =
  fun ppf ->
  function
  | METADATA_Const v  -> tvalue ppf v
  | METADATA_Null     -> pp_print_string ppf "null"
  | METADATA_Id i     -> fprintf ppf "!%s" i
  | METADATA_String s -> fprintf ppf "metadata !\"%s\"" s
  | METADATA_Node m   -> fprintf ppf "metadata !{%a}"
                                 (pp_print_list ~pp_sep:pp_comma_space metadata) m
  | METADATA_Named m  -> fprintf ppf "!{%a}"
                                 (pp_print_list ~pp_sep:pp_comma_space
                                                (fun ppf i ->
                                                 fprintf ppf "!%s" i)) m

and global : Format.formatter -> Ast.global -> unit =
  fun ppf ->
  fun {
    g_ident = i;
    g_typ = t;
    g_constant = b;
    g_section = s;
    g_align = a;
    g_value = vo;
  } -> fprintf ppf "%a = %s %a"
               ident i (if b then "constant" else "global") typ t ;
       (match vo with None -> () | Some v -> value ppf v) ;
       (match s with None -> ()
                   | Some s -> fprintf ppf ", section %s" s) ;
       (match a with None -> ()
                   | Some a -> fprintf ppf ", align %d" a)

and declaration : Format.formatter -> Ast.declaration -> unit =
  fun ppf ->
  fun {
    dc_name = i;
    dc_type = TYPE_Function (ret_t, args_t);
    dc_param_attrs = (ret_attrs, args_attrs)
  } -> let typ_attr =
         fun ppf (t, attrs) ->
         typ ppf t ;
         pp_print_list ~pp_sep:pp_space param_attr ppf attrs in
       pp_print_string ppf "declare " ;
       if ret_attrs <> [] then (pp_space ppf () ;
                                pp_print_list ~pp_sep:pp_space
                                              param_attr ppf ret_attrs) ;
       fprintf ppf "%a %a(%a)"
               typ ret_t
               ident i
               (pp_print_list ~pp_sep:pp_comma_space typ_attr)
               (List.combine args_t args_attrs);

and definition : Format.formatter -> Ast.definition -> unit =
  fun ppf ->
  fun ({ df_prototype = { dc_name = i;
                          dc_type = TYPE_Function (ret_t, args_t);
                          dc_param_attrs = (ret_attrs, args_attrs) };
       } as df) ->
  let typ_attr_id =
    fun ppf ((t, attrs), id) ->
    typ ppf t ;
    pp_space ppf () ;
    if attrs <> [] then (pp_print_list ~pp_sep:pp_space
                                       param_attr ppf attrs ;
                         pp_space ppf ()) ;
    ident ppf id in
  pp_print_string ppf "define " ;
  (match df.df_linkage with
   | Some x -> linkage ppf x ; pp_space ppf ()
   | _ -> ()) ;
  (match df.df_visibility with
   | Some x -> visibility ppf x ; pp_space ppf ()
   | _ -> ()) ;
  (match df.df_dll_storage with
   | Some x -> dll_storage ppf x ; pp_space ppf ()
   | _ -> ()) ;
  (match df.df_cconv with
   | Some x -> cconv ppf x ; pp_space ppf ()
   | _ -> ()) ;
  if ret_attrs <> [] then (pp_print_list ~pp_sep:pp_space
                                         param_attr ppf ret_attrs ;
                           pp_space ppf ()) ;
  fprintf ppf "%a %a(%a) "
            typ ret_t
            ident i
            (pp_print_list ~pp_sep:pp_comma_space typ_attr_id)
            (List.combine (List.combine args_t args_attrs) df.df_args) ;
            if df.df_attrs <> [] then (pp_print_list ~pp_sep:pp_space
                                                     fn_attr ppf df.df_attrs ;
                                       pp_space ppf ()) ;
  (match df.df_section with
     Some x -> fprintf ppf "section \"%s\" " x | _ -> ()) ;
  (match df.df_align with
     Some x -> fprintf ppf "align %d " x | _ -> ()) ;
  (match df.df_gc with
     Some x -> fprintf ppf "gc \"%s\" " x | _ -> ()) ;
  pp_print_char ppf '{' ;
  pp_force_newline ppf () ;
  pp_print_list ~pp_sep:pp_force_newline block ppf df.df_instrs ;
  pp_force_newline ppf () ;
  pp_print_char ppf '}' ;

and block : Format.formatter -> Ast.block -> unit =
  fun ppf (i, b) ->
  pp_print_string ppf i ;
  pp_print_char ppf ':' ;
  pp_open_box ppf 2 ;
  pp_force_newline ppf () ;
  pp_print_list ~pp_sep:pp_force_newline instr ppf b ;
  pp_close_box ppf ()

and modul : Format.formatter -> Ast.modul -> unit =
  fun ppf m ->
  fprintf ppf "; ModuleID = '%s'" m.m_name ;
  pp_force_newline ppf () ;
  toplevelentry ppf m.m_target ;
  toplevelentry ppf m.m_datalayout ;
  pp_print_list ~pp_sep:pp_force_newline global ppf
                (List.map snd m.m_globals) ;
  pp_force_newline ppf () ;
  pp_print_list ~pp_sep:pp_force_newline declaration ppf
                (List.map snd m.m_declarations) ;
  pp_force_newline ppf () ;
  pp_print_list ~pp_sep:pp_force_newline definition ppf
                (List.map snd m.m_definitions) ;
  pp_force_newline ppf ()
