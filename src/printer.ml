open LLVM

let quote s = "\"" ^ s ^ "\""

let list : string -> ('a -> string) -> 'a list -> string =
  fun sep printer l -> List.map printer l |> String.concat sep

let sprintf = Printf.sprintf

let rec linkage : LLVM.linkage -> string = function
  | LINKAGE_Private -> "linkage"
  | LINKAGE_Linker_private -> "linkage"
  | LINKAGE_Linker_private_weak -> "linkage"
  | LINKAGE_Linker_private_weak_def_auto -> "linkage"
  | LINKAGE_Internal -> "linkage"
  | LINKAGE_Available_externally -> "linkage"
  | LINKAGE_Linkonce -> "linkage"
  | LINKAGE_Weak -> "linkage"
  | LINKAGE_Common -> "linkage"
  | LINKAGE_Appending -> "linkage"
  | LINKAGE_Extern_weak -> "linkage"
  | LINKAGE_Linkonce_odr -> "linkage"
  | LINKAGE_Weak_odr -> "linkage"
  | LINKAGE_External -> "linkage"
  | LINKAGE_Dllimport -> "linkage"
  | LINKAGE_Dllexport -> "linkage"

and visibility : LLVM.visibility -> string = function
  | VISIBILITY_Default -> "visibility"
  | VISIBILITY_Hidden -> "visibility"
  | VISIBILITY_Protected -> "visibility"

and cconv : LLVM.cconv -> string = function
  | CC_Ccc -> "cconv"
  | CC_Fastcc -> "cconv"
  | CC_Coldcc -> "cconv"
  | CC_Cc i -> "cconv"

and typ_attr : LLVM.typ_attr -> string = function
  | TYPEATTR_Zeroext -> "typ_attr"
  | TYPEATTR_Signext -> "typ_attr"
  | TYPEATTR_Inreg -> "typ_attr"
  | TYPEATTR_Byval -> "typ_attr"
  | TYPEATTR_Sret -> "typ_attr"
  | TYPEATTR_Noalias -> "typ_attr"
  | TYPEATTR_Nocapture -> "typ_attr"
  | TYPEATTR_Nest -> "typ_attr"

and fn_attr : LLVM.fn_attr -> string = function
  | FNATTR_Alignstack i -> sprintf "alignstack(%d)" i
  | FNATTR_Alwaysinline -> "alwaysinline"
  | FNATTR_Builtin -> "builtin"
  | FNATTR_Cold -> "cold"
  | FNATTR_Inlinehint -> "inlinehint"
  | FNATTR_Jumptable -> "jumptable"
  | FNATTR_Minsize -> "minsize"
  | FNATTR_Naked -> "naked"
  | FNATTR_Nobuiltin -> "nobuiltin"
  | FNATTR_Noduplicate -> "noduplicate"
  | FNATTR_Noimplicitfloat -> "noimplicitfloat"
  | FNATTR_Noinline -> "noinline"
  | FNATTR_Nonlazybind -> "nonlazybind"
  | FNATTR_Noredzone -> "noredzone"
  | FNATTR_Noreturn -> "noreturn"
  | FNATTR_Nounwind -> "nounwind"
  | FNATTR_Optnone -> "optnone"
  | FNATTR_Optsize -> "optsize"
  | FNATTR_Readnone -> "readone"
  | FNATTR_Readonly -> "readonly"
  | FNATTR_Returns_twice -> "returns_twice"
  | FNATTR_Sanitize_address -> "sanitize_address"
  | FNATTR_Sanitize_memory -> "sanitize_memory"
  | FNATTR_Sanitize_thread -> "sanitize_thread"
  | FNATTR_Ssp -> "ssp"
  | FNATTR_Sspreq -> "sspreq"
  | FNATTR_Sspstrong -> "sspstrong"
  | FNATTR_Uwtable -> "uwtable"
  | FNATTR_String s -> "\"" ^ s ^ "\""
  | FNATTR_Key_value (k, v) -> "\"" ^ k ^ "\"=\"" ^ v ^ "\""
  | FNATTR_Attr_grp i -> "#" ^ string_of_int i

and ident : LLVM.ident -> string = function
  | ID_Global (f, i) -> "@" ^ ident_format f i
  | ID_Local (f, i)  -> "%" ^ ident_format f i

and ident_format : LLVM.ident_format -> string -> string =
  fun f i -> match f with
             | ID_FORMAT_Named
             | ID_FORMAT_Unnamed -> i
             | ID_FORMAT_NamedString -> "\"" ^ i ^ "\""

and typ : LLVM.typ -> string = function
  | TYPE_I i              -> "i" ^ string_of_int i
  | TYPE_Pointer t        -> typ t ^ "*"
  | TYPE_Void             -> "void"
  | TYPE_Half             -> "half"
  | TYPE_Float            -> "float"
  | TYPE_Double           -> "double"
  | TYPE_Label            -> "label"
  | TYPE_X86_fp80         -> assert false
  | TYPE_Fp128            -> assert false
  | TYPE_Ppc_fp128        -> assert false
  | TYPE_Metadata         -> "metadata"
  | TYPE_X86_mmx          -> assert false
  | TYPE_Array (i, t)     -> sprintf "[%d x %s]" i (typ t)
  | TYPE_Function (t, tl) -> assert false (* (t, tl) : (typ * typ list) *)
  | TYPE_Struct tl        -> "{ " ^ (list ", " typ tl) ^ " }"
  | TYPE_Packed_struct tl ->  "<{ " ^ (list ", " typ tl) ^ " }>"
  | TYPE_Opaque           -> assert false
  | TYPE_Vector (i, t)    -> sprintf "<%d x %s>" i (typ t)

and icmp : LLVM.icmp -> string = function
  | Eq  -> "eq"
  | Ne  -> "neq"
  | Ugt -> "ugt"
  | Uge -> "uge"
  | Ult -> "ult"
  | Ule -> "ule"
  | Sgt -> "sgt"
  | Sge -> "sge"
  | Slt -> "slt"
  | Sle -> "cmp"

and fcmp : LLVM.fcmp -> string = function
  | False -> "false"
  | Oeq -> "oeq"
  | Ogt -> "ogt"
  | Oge -> "oge"
  | Olt -> "olt"
  | Ole -> "ole"
  | One -> "one"
  | Ord -> "ord"
  | Uno -> "uno"
  | Ueq -> "ueq"
  | Ugt -> "ugt"
  | Uge -> "uge"
  | Ult -> "ult"
  | Ule -> "ule"
  | Une -> "une"
  | True -> "true"

and ibinop : LLVM.ibinop -> string = function
  | Add  -> "add"
  | Sub  -> "sub"
  | Mul  -> "mul"
  | UDiv -> "udiv"
  | SDiv -> "sdiv"
  | URem -> "urem"
  | SRem -> "srem"
  | Shl  -> "shl"
  | LShr -> "lshr"
  | AShr -> "ashr"
  | And  -> "and"
  | Or   -> "or"
  | Xor  -> "xor"

and fbinop = function
  | FAdd -> "fadd"
  | FSub -> "fsub"
  | FMul -> "fmul"
  | FDiv -> "fdiv"
  | FRem -> "frem"

and conversion_type : LLVM.conversion_type -> string = function
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
  | Bitcast  -> "bitcast"

and align = function
  | None -> ""
  | Some a -> ", align " ^ string_of_int a

and section = function
  | None -> ""
  | Some s -> ", section " ^ s

and instr : LLVM.instr -> string = function

  | INSTR_IBinop (op, t, v1, v2) ->
     sprintf "%s %s %s, %s" (ibinop op) (typ t) (value v1) (value v2)

  | INSTR_ICmp (c, t, v1, v2) ->
     sprintf "icmp %s %s %s, %s" (icmp c) (typ t) (value v1) (value v2)

  | INSTR_FBinop (op, t, v1, v2) ->
     sprintf "%s %s %s, %s" (fbinop op) (typ t) (value v1) (value v2)

  | INSTR_FCmp (c, t, v1, v2) ->
     sprintf "fcmp %s %s %s, %s" (fcmp c) (typ t) (value v1) (value v2)

  | INSTR_Conversion (c, t1, v, t2) ->
     sprintf "%s %s %s to %s" (conversion_type c) (typ t1) (value v) (typ t2)

  | INSTR_GetElementPtr (tv, tvl) ->
     sprintf "getelementptr %s, %s" (tvalue tv) (list ", " tvalue tvl)

  | INSTR_Call (ti, tvl) ->
     sprintf "call %s(%s)" (tident ti) (list ", " tvalue tvl)

  | INSTR_Alloca (t, n, a) ->
     "alloca " ^ (typ t)
     ^ (match n with None -> "" | Some n -> ", " ^ tvalue n)
     ^ align a

  | INSTR_Load (tv, a) ->
     "load " ^ tvalue tv ^ align a

  | INSTR_Phi (t, vil) ->
     sprintf "phi %s [%s]"
             (typ t) (list "], [" (fun (v, i) -> value v ^ ", " ^ ident i) vil)

  | INSTR_Select (if_, then_, else_) ->
     sprintf "select %s, %s, %s"
             (tvalue if_) (tvalue then_) (tvalue else_)

  | INSTR_VAArg -> "vaarg"

  | INSTR_ExtractElement (vec, idx) ->
     sprintf "extractelement %s, %s" (tvalue vec) (tvalue idx)

  | INSTR_InsertElement (vec, new_val, idx) ->
     sprintf "insertelement %s, %s, %s"
             (tvalue vec) (tvalue new_val) (tvalue idx)

  | INSTR_ExtractValue (agg, idx) ->
     sprintf "extractvalue %s, %s"
             (tvalue agg) (list ", " string_of_int idx)

  | INSTR_InsertValue (agg, new_val, idx) ->
     sprintf "insertvalue %s, %s, %s"
             (tvalue agg) (tvalue new_val) (list ", " string_of_int idx)

  | INSTR_ShuffleVector (v1, v2, mask) ->
     sprintf "shufflevector %s, %s, %s" (tvalue v1) (tvalue v2) (tvalue mask)

  | INSTR_LandingPad -> assert false

  | INSTR_Store (v, ptr, a) ->
     sprintf "store %s, %s%s" (tvalue v) (tident ptr) (align a)

  | INSTR_AtomicCmpXchg
  | INSTR_AtomicRMW
  | INSTR_Fence -> assert false

  | INSTR_Ret (t, v)       -> "ret " ^ tvalue (t, v)

  | INSTR_Ret_void         -> "ret void"

  | INSTR_Br (c, i1, i2)   ->
     sprintf "br %s, %s, %s" (tvalue c) (tident i1) (tident i2)

  | INSTR_Br_1 (t, i)       -> "br " ^ typ t ^ " " ^ ident i

  | INSTR_Switch (c, def, cases) ->
     sprintf "switch %s, %s [%s]"
             (tvalue c) (tident def)
             (list ", " (fun (v, i) -> tvalue v ^ ", " ^ tident i) cases)

  | INSTR_Resume (t, v) -> "resume " ^ tvalue (t, v)

  | INSTR_Unreachable -> "unreachable"

  | INSTR_IndirectBr     -> assert false

  | INSTR_Invoke (ti, tvl, i2, i3) ->
     sprintf "invoke %s(%s) to %s unwind %s"
             (tident ti) (list ", " tvalue tvl) (tident i2) (tident i3)

  | INSTR_Assign (id, inst) -> ident id ^ " = " ^ instr inst

and value : LLVM.value -> string = function
  | VALUE_Ident i           -> ident i
  | VALUE_Integer i         -> (string_of_int i)
  | VALUE_Float f           -> sprintf "%f" f
  | VALUE_Bool b            -> (string_of_bool b)
  | VALUE_Null              -> "null"
  | VALUE_Undef             -> "undef"
  | VALUE_Array tvl         -> "[ " ^ list ", " tvalue tvl ^ " ]"
  | VALUE_Vector tvl        -> "< " ^ list ", " tvalue tvl ^ " >"
  | VALUE_Struct tvl        -> "{ " ^ list ", " tvalue tvl ^ " }"
  | VALUE_Packed_struct tvl -> "<{ " ^ list ", " tvalue tvl ^ " }>"
  | VALUE_Zero_initializer  -> "zeroinitializer"

and tvalue  = fun (t, v) -> typ t ^ " " ^ value v

and tident  = fun (t, v) -> typ t ^ " " ^ ident v

and toplevelentries : LLVM.toplevelentries-> string =
  fun m -> list "\n" toplevelentry m

and toplevelentry : LLVM.toplevelentry -> string = function
  | TLE_Target s -> "target triple = " ^ quote s
  | TLE_Datalayout s -> "target datalayout = " ^ quote s
  | TLE_Declaration d -> declaration d
  | TLE_Definition d -> definition d
  | TLE_Type_decl (i, t) -> ident i ^ typ t
  | TLE_Global g -> global g
  | TLE_Metadata -> "; metadata were lost during parsing"
  | TLE_Attribute_group (i, a) ->
    sprintf "#%d = { %s }" i (list " " fn_attr a)

and global : LLVM.global -> string = fun {
    g_ident = i;
    g_typ = t;
    g_constant = b;
    g_section = s;
    g_align = a;
    g_value = vo;
  } -> sprintf "%s = %s %s %s%s%s"
               (ident i) (if b then "constant" else "global") (typ t)
               (match vo with None -> "" | Some v -> value v)
               (section s)
               (align a)

and declaration : LLVM.declaration -> string = fun {
    dc_ret_typ = t;
    dc_name = i;
    dc_args = tl;
  } -> sprintf "declare %s %s(%s)"
               (typ t) (ident i) (list ", " typ tl)

and definition : LLVM.definition -> string = fun {
    df_prototype = { dc_ret_typ = t;
                     dc_name = i;
                     dc_args = argt};
    df_args = argn;
    df_attrs = al;
    df_instrs = blocks;
  } -> sprintf "define %s %s(%s) %s {\n%s\n}"
               (typ t)
               (ident i)
               (list ", " tident (List.combine argt argn))
               (list " " fn_attr al)
               (list "\n" block blocks)

and block : LLVM.block -> string = fun (i, b) -> i ^ ":\n" ^ (list "\n" instr b)
