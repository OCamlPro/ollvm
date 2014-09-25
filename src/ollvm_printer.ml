open Format
open Ollvm_ast

type t = { local: int ref * (int, int) Hashtbl.t;
           global: int ref * (int, int) Hashtbl.t }

let empty_env () = { local = ref 1, Hashtbl.create 0 ;
                     global = ref 1, Hashtbl.create 0 }

let find_env (cntr, tbl) i =
  let i = int_of_string i in
  try Hashtbl.find tbl i
  with Not_found -> let i' = !cntr in
                    Hashtbl.add tbl i i' ;
                    cntr := i' + 1 ;
                    i'

let find_local env = find_env env.local

let find_global env = find_env env.global

let pp_sep str =
  fun ppf () -> pp_print_string ppf str

let pp_space ppf () = pp_print_char ppf ' '

let pp_comma_space ppf () = pp_print_string ppf ", "

let rec linkage : Format.formatter -> Ollvm_ast.linkage -> unit =
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

 and dll_storage : Format.formatter -> Ollvm_ast.dll_storage -> unit =
   fun ppf ->
   function
   | DLLSTORAGE_Dllimport -> fprintf ppf "dllimport"
   | DLLSTORAGE_Dllexport -> fprintf ppf "dllexport"

and visibility : Format.formatter -> Ollvm_ast.visibility -> unit =
  fun ppf ->
  function
  | VISIBILITY_Default   -> fprintf ppf "default"
  | VISIBILITY_Hidden    -> fprintf ppf "hidden"
  | VISIBILITY_Protected -> fprintf ppf "protected"

and cconv : Format.formatter -> Ollvm_ast.cconv -> unit =
  fun ppf -> function
          | CC_Ccc    -> fprintf ppf "ccc"
          | CC_Fastcc -> fprintf ppf "fastcc"
          | CC_Coldcc -> fprintf ppf "coldcc"
          | CC_Cc i   -> fprintf ppf "cc %d" i

and param_attr : Format.formatter -> Ollvm_ast.param_attr -> unit =
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

and fn_attr : Format.formatter -> Ollvm_ast.fn_attr -> unit =
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

and ident : t -> Format.formatter -> Ollvm_ast.ident -> unit =

  let ident_format : (string -> int) -> Format.formatter -> string -> unit =
  fun finder ppf i ->
  if i.[0] > '0' && i.[0] < '9' then pp_print_int ppf (finder i)
  else pp_print_string ppf i in

  fun env ppf ->
  function
  | ID_Global i -> pp_print_char ppf '@' ;
                   ident_format (find_global env) ppf i
  | ID_Local i  -> pp_print_char ppf '%' ;
                   ident_format (find_local env) ppf i

and typ : Format.formatter -> Ollvm_ast.typ -> unit =
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


and icmp : Format.formatter -> Ollvm_ast.icmp -> unit =
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

and fcmp : Format.formatter -> Ollvm_ast.fcmp -> unit =
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


and ibinop : Format.formatter -> Ollvm_ast.ibinop -> unit =
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

and conversion_type : Format.formatter -> Ollvm_ast.conversion_type -> unit =
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

and instr : t -> Format.formatter -> Ollvm_ast.instr -> unit =
  fun env ppf ->
  function

  | INSTR_IBinop (op, t, v1, v2) ->
     fprintf ppf "%a %a %a, %a"
             ibinop op
             typ t
             (value env) v1
             (value env) v2

  | INSTR_ICmp (c, t, v1, v2) ->
     fprintf ppf "icmp %a %a %a, %a"
             icmp c
             typ t
             (value env) v1
             (value env) v2

  | INSTR_FBinop (op, f, t, v1, v2) ->
     fbinop ppf op ;
     if f <> [] then (pp_space ppf () ;
                      pp_print_list ~pp_sep:pp_space fast_math ppf f) ;
     fprintf ppf " %a %a, %a"
             typ t
             (value env) v1
             (value env) v2

  | INSTR_FCmp (c, t, v1, v2) ->
     fprintf ppf "fcmp %a %a %a, %a"
             fcmp c
             typ t
             (value env) v1
             (value env) v2

  | INSTR_Conversion (c, t1, v, t2) ->
     fprintf ppf "%a %a %a to %a"
             conversion_type c
             typ t1
             (value env) v
             typ t2

  | INSTR_GetElementPtr (tv, tvl) ->
     fprintf ppf "getelementptr %a, %a"
             (tvalue env) tv
             (pp_print_list ~pp_sep:pp_comma_space (tvalue env)) tvl

  | INSTR_Call (ti, tvl) ->
     fprintf ppf "call %a(%a)"
             (tident env) ti
             (pp_print_list ~pp_sep:pp_comma_space (tvalue env)) tvl

  | INSTR_Alloca (t, n, a) ->
     fprintf ppf "alloca %a" typ t ;
     (match n with None -> ()
                 | Some n -> fprintf ppf ", %a" (tvalue env) n) ;
     (match a with None -> ()
                 | Some a -> fprintf ppf ", align %d" a)

  | INSTR_Load (vol, tv, a) ->
     pp_print_string ppf "load " ;
     if vol then pp_print_string ppf "volatile " ;
     (tvalue env) ppf tv ;
     (match a with None -> ()
                 | Some a -> fprintf ppf ", align %d" a)

  | INSTR_Phi (t, vil) ->
     fprintf ppf "phi %a [%a]"
             typ t
             (pp_print_list ~pp_sep:(pp_sep "], [")
                            (fun ppf (v, i) -> value env ppf v ;
                                               pp_print_string ppf ", " ;
                                               ident env ppf i)) vil

  | INSTR_Select (if_, then_, else_) ->
     fprintf ppf "select %a, %a, %a"
             (tvalue env) if_
             (tvalue env) then_
             (tvalue env) else_

  | INSTR_VAArg -> pp_print_string ppf "vaarg"

  | INSTR_ExtractElement (vec, idx) ->
     fprintf ppf "extractelement %a, %a"
             (tvalue env) vec
             (tvalue env) idx

  | INSTR_InsertElement (vec, new_val, idx) ->
     fprintf ppf "insertelement %a, %a, %a"
             (tvalue env) vec
             (tvalue env) new_val
             (tvalue env) idx

  | INSTR_ExtractValue (agg, idx) ->
     fprintf ppf "extractvalue %a, %a"
             (tvalue env) agg
             (pp_print_list ~pp_sep:pp_comma_space pp_print_int) idx

  | INSTR_InsertValue (agg, new_val, idx) ->
     fprintf ppf "insertvalue %a, %a, %a"
             (tvalue env) agg
             (tvalue env) new_val
             (pp_print_list ~pp_sep:pp_comma_space pp_print_int) idx

  | INSTR_ShuffleVector (v1, v2, mask) ->
     fprintf ppf "shufflevector %a, %a, %a"
             (tvalue env) v1
             (tvalue env) v2
             (tvalue env) mask

  | INSTR_LandingPad -> assert false

  | INSTR_Store (vol, v, ptr, a) ->
     pp_print_string ppf "store " ;
     if vol then pp_print_string ppf "volatile " ;
     fprintf ppf "%a, %a" (tvalue env) v (tident env) ptr ;
     (match a with None -> ()
                 | Some a -> fprintf ppf ", align %d" a)

  | INSTR_AtomicCmpXchg
  | INSTR_AtomicRMW
  | INSTR_Fence -> assert false

  | INSTR_Ret (t, v)       -> fprintf ppf "ret %a" (tvalue env) (t, v)

  | INSTR_Ret_void         -> pp_print_string ppf "ret void"

  | INSTR_Br (c, i1, i2)   ->
     fprintf ppf "br %a, %a, %a" (tvalue env) c (tident env) i1 (tident env) i2

  | INSTR_Br_1 (t, i)       -> fprintf ppf "br %a %a" typ t (ident env) i

  | INSTR_Switch (c, def, cases) ->
     fprintf ppf "switch %a, %a [%a]"
             (tvalue env) c
             (tident env) def
             (pp_print_list ~pp_sep:pp_space
                            (fun ppf (v, i) -> tvalue env ppf v ;
                                               pp_print_string ppf ", " ;
                                               tident env ppf i)) cases

  | INSTR_Resume (t, v) -> fprintf ppf "resume %a" (tvalue env) (t, v)

  | INSTR_Unreachable -> pp_print_string ppf "unreachable"

  | INSTR_IndirectBr     -> assert false

  | INSTR_Invoke (ti, tvl, i2, i3) ->
     fprintf ppf "invoke %a(%a) to %a unwind %a"
             (tident env) ti
             (pp_print_list ~pp_sep:pp_comma_space (tvalue env)) tvl
             (tident env) i2
             (tident env) i3

  | INSTR_Assign (id, inst) -> fprintf ppf "%a = %a" (ident env) id (instr env) inst

and value : t -> Format.formatter -> Ollvm_ast.value -> unit =
  fun env ppf ->
  function
  | VALUE_Ident i           -> ident env ppf i
  | VALUE_Integer i         -> pp_print_int ppf i
  | VALUE_Float f           -> pp_print_float ppf f
  | VALUE_Bool b            -> pp_print_bool ppf b
  | VALUE_Null              -> pp_print_string ppf "null"
  | VALUE_Undef             -> pp_print_string ppf "undef"
  | VALUE_Array tvl         -> fprintf ppf "[%a]"
                                       (pp_print_list ~pp_sep:pp_comma_space (tvalue env)) tvl
  | VALUE_Vector tvl        -> fprintf ppf "<%a>"
                                       (pp_print_list ~pp_sep:pp_comma_space (tvalue env)) tvl
  | VALUE_Struct tvl        -> fprintf ppf "{%a}"
                                       (pp_print_list ~pp_sep:pp_comma_space (tvalue env)) tvl
  | VALUE_Packed_struct tvl -> fprintf ppf "<{%a}>"
                                       (pp_print_list ~pp_sep:pp_comma_space (tvalue env)) tvl
  | VALUE_Zero_initializer  -> pp_print_string ppf "zeroinitializer"

and tvalue env ppf (t, v) = fprintf ppf "%a %a" typ t (value env) v

and tident env ppf (t, v) = fprintf ppf "%a %a" typ t (ident env) v

and toplevelentries : t -> Format.formatter -> Ollvm_ast.toplevelentries -> unit =
  fun env ppf entries ->
  pp_print_list ~pp_sep:pp_force_newline (toplevelentry env) ppf entries

and toplevelentry : t -> Format.formatter -> Ollvm_ast.toplevelentry -> unit =
  fun env ppf ->
  function
  | TLE_Target s               -> fprintf ppf "target triple = \"%s\"" s
  | TLE_Datalayout s           -> fprintf ppf "target datalayout = \"%s\"" s
  | TLE_Declaration d          -> declaration env ppf d
  | TLE_Definition d           -> definition env ppf d
  | TLE_Type_decl (i, t)       -> fprintf ppf "%a %a" (ident env) i typ t
  | TLE_Global g               -> global env ppf g
  | TLE_Metadata (i, m)        -> fprintf ppf "!%s = %a" i (metadata env) m
  | TLE_Attribute_group (i, a) -> fprintf ppf "#%d = { %a }" i
                                          (pp_print_list ~pp_sep:pp_space fn_attr) a

and metadata : t -> Format.formatter -> Ollvm_ast.metadata -> unit =
  fun env ppf ->
  function
  | METADATA_Const v  -> tvalue env ppf v
  | METADATA_Null     -> pp_print_string ppf "null"
  | METADATA_Id i     -> fprintf ppf "!%s" i
  | METADATA_String s -> fprintf ppf "metadata !\"%s\"" s
  | METADATA_Node m   -> fprintf ppf "metadata !{%a}"
                                 (pp_print_list ~pp_sep:pp_comma_space (metadata env)) m
  | METADATA_Named m  -> fprintf ppf "!{%a}"
                                 (pp_print_list ~pp_sep:pp_comma_space
                                                (fun ppf i ->
                                                 fprintf ppf "!%s" i)) m

and global : t -> Format.formatter -> Ollvm_ast.global -> unit =
  fun env ppf ->
  fun {
    g_ident = i;
    g_typ = t;
    g_constant = b;
    g_section = s;
    g_align = a;
    g_value = vo;
  } -> fprintf ppf "%a = %s %a"
               (ident env) i (if b then "constant" else "global") typ t ;
       (match vo with None -> () | Some v -> (value env) ppf v) ;
       (match s with None -> ()
                   | Some s -> fprintf ppf ", section %s" s) ;
       (match a with None -> ()
                   | Some a -> fprintf ppf ", align %d" a)

and declaration : t -> Format.formatter -> Ollvm_ast.declaration -> unit =
  fun env ppf ->
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
               (ident env) i
               (pp_print_list ~pp_sep:pp_comma_space typ_attr)
               (List.combine args_t args_attrs);

and definition : t -> Format.formatter -> Ollvm_ast.definition -> unit =
  fun env ppf ->
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
    (ident env) ppf id in
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
            (ident env) i
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
  pp_print_list ~pp_sep:pp_force_newline (block env) ppf df.df_instrs ;
  pp_force_newline ppf () ;
  pp_print_char ppf '}' ;

and block : t -> Format.formatter -> Ollvm_ast.block -> unit =
  fun env ppf (i, b) ->
  begin try pp_print_int ppf (find_local env i)
        with Failure "int_of_string" -> pp_print_string ppf i end ;
  pp_print_char ppf ':' ;
  pp_open_box ppf 2 ;
  pp_force_newline ppf () ;
  pp_print_list ~pp_sep:pp_force_newline (instr env) ppf b ;
  pp_close_box ppf ()

and modul : t -> Format.formatter -> Ollvm_ast.modul -> unit =
  fun env ppf m ->
  fprintf ppf "; ModuleID = '%s'" m.m_name ;
  pp_force_newline ppf () ;
  toplevelentry env ppf m.m_target ;
  toplevelentry env ppf m.m_datalayout ;
  pp_print_list ~pp_sep:pp_force_newline (global env) ppf
                (List.map snd m.m_globals) ;
  pp_force_newline ppf () ;
  pp_print_list ~pp_sep:pp_force_newline (declaration env) ppf
                (List.map snd m.m_declarations) ;
  pp_force_newline ppf () ;
  pp_print_list ~pp_sep:pp_force_newline (definition env) ppf
                (List.map snd m.m_definitions) ;
  pp_force_newline ppf ()
