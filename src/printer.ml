open LLVM

let quote s = "\"" ^ s ^ "\""

let list : string -> ('a -> string) -> 'a list -> string =
  fun sep printer l -> List.map printer l |> String.concat sep

let sprintf = Printf.sprintf

let pprint =

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

  and ident : LLVM.ident -> string = function
    | ID_Global s -> "@" ^ s
    | ID_Local s  -> "%" ^ s

  and typ : LLVM.typ -> string = function
    | TYPE_I i              -> "i" ^ string_of_int i
    | TYPE_Pointer t        -> "*" ^ typ t
    | TYPE_Void             -> "void"
    | TYPE_Half             -> "half"
    | TYPE_Float            -> "float"
    | TYPE_Double           -> "double"
    | TYPE_Label            -> "label"
    | TYPE_X86_fp80         -> assert false
    | TYPE_Fp128            -> assert false
    | TYPE_Ppc_fp128        -> assert false
    | TYPE_Metadata         -> assert false
    | TYPE_X86_mmx          -> assert false
    | TYPE_Ident i          -> assert false (* i : ident *)
    | TYPE_Array (i, t)     -> sprintf "[%d x %s]" i (typ t)
    | TYPE_Function (t, tl) -> assert false (* (t, tl) : (typ * typ list) *)
    | TYPE_Struct tl        -> "{ " ^ (list ", " typ tl) ^ " }"
    | TYPE_Packed_struct tl ->  "<{ " ^ (list ", " typ tl) ^ " }>"
    | TYPE_Opaque           -> assert false
    | TYPE_Vector (i, t)    -> sprintf "<%d x %s>" i (typ t)

  and tident =
    fun (t, i) ->  typ t ^ " " ^ ident i

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
    | Trunc
    | Zext
    | Sext
    | Fptrunc
    | Fpext
    | Uitofp
    | Sitofp
    | Fptoui
    | Fptosi
    | Inttoptr
    | Ptrtoint
    | Bitcast -> "conversion_type"

  and expr : LLVM.expr -> string = function

    | EXPR_IBinop (op, t, v1, v2) ->
       sprintf "%s %s %s, %s" (ibinop op) (typ t) (value v1) (value v2)

    | EXPR_ICmp (c, t, v1, v2) ->
       sprintf "icmp %s %s %s, %s" (icmp c) (typ t) (value v1) (value v2)

    | EXPR_FBinop (op, t, v1, v2) ->
       sprintf "%s %s %s, %s" (fbinop op) (typ t) (value v1) (value v2)

    | EXPR_FCmp (c, t, v1, v2) ->
       sprintf "icmp %s %s %s, %s" (fcmp c) (typ t) (value v1) (value v2)

    | EXPR_Conversion (c, t1, v, t2) ->
       sprintf "%s %s %s, %s" (conversion_type c) (typ t1) (value v) (typ t2)

    | EXPR_GetElementPtr (tv, tvl) ->
       sprintf "getelementptr %s, %s" (tvalue tv) (list ", " tvalue tvl)

    | EXPR_Call (t, i, tvl) ->
       sprintf "call %s %s(%s)" (typ t) (ident i) (list ", " tvalue tvl)

    | EXPR_Alloca (n, t) ->
       sprintf "alloca %s, %s %i" (typ t) (typ t) (n)

    | EXPR_Load (tv) ->
       "load" ^ tvalue tv

    | EXPR_Phi (t, vil) ->
       sprintf "phi %s [%s]"
               (typ t) (list "], [" (fun (v, i) -> value v ^ ", " ^ ident i) vil)

    | EXPR_Select (if_, then_, else_) ->
       sprintf "select %s, %s, %s"
               (tvalue if_) (tvalue then_) (tvalue else_)

    | EXPR_VAArg -> "vaarg"

    | EXPR_ExtractElement (vec, idx) ->
       sprintf "extractelement %s, %s" (tvalue vec) (tvalue idx)

    | EXPR_InsertElement (vec, new_val, idx) ->
       sprintf "insertelement %s, %s, %s"
               (tvalue vec) (tvalue new_val) (tvalue idx)

    | EXPR_ExtractValue (agg, idx) ->
       sprintf "extractvalue %s, %s"
               (tvalue agg) (list ", " string_of_int idx)

    | EXPR_InsertValue (agg, new_val, idx) ->
       sprintf "insertvalue %s, %s, %s"
               (tvalue agg) (tvalue new_val) (list ", " string_of_int idx)

    | EXPR_LandingPad
    | EXPR_ShuffleVector
              -> assert false

  and expr_unit = function
    | EXPR_UNIT_IGNORED e -> expr e
    | EXPR_UNIT_Store (v, (tptr, ptr)) ->
       sprintf "store %s, %s %s" (tvalue v) (typ tptr) (ident ptr)
    | EXPR_UNIT_AtomicCmpXchg
    | EXPR_UNIT_AtomicRMW
    | EXPR_UNIT_Fence -> assert false

  and value : LLVM.value -> string = function
    | VALUE_Ident i           -> ident i
    | VALUE_Integer i         -> (string_of_int i)
    | VALUE_Float f           -> (string_of_float f)
    | VALUE_Bool b            -> (string_of_bool b)
    | VALUE_Null              -> "null"
    | VALUE_Undef             -> "undef"
    | VALUE_Expr e            -> expr e
    | VALUE_Struct tvl
    | VALUE_Packed_struct tvl
    | VALUE_Array tvl
    | VALUE_Vector tvl        -> assert false
    | VALUE_Zero_initializer  -> assert false

  and tvalue  = fun (t, v) -> typ t ^ " " ^ value v

  and terminator_unit : LLVM.terminator_unit -> string = function
    | TERM_UNIT_Ret (t, v)       -> "ret " ^ tvalue (t, v)
    | TERM_UNIT_Ret_void         -> "ret void"
    | TERM_UNIT_Br (v, i1, i2)   ->
       sprintf "br i1 %s, %s, %s" (value v) (ident i1) (ident i2)
    | TERM_UNIT_Br_1 i           -> "br " ^ ident i
    | TERM_UNIT_Switch (t, v1, v2, tvil) ->
       sprintf "switch %s %s, %s [%s]"
               (typ t) (value v1) (value v2)
               (list ", " (fun (t, v, i) -> tvalue (t, v) ^ ", " ^ ident i) tvil)
    | TERM_UNIT_Resume (t, v) -> "resume " ^ tvalue (t, v)
    | TERM_UNIT_Unreachable -> "unreachable"
    | TERM_UNIT_IndirectBr     -> assert false

  and terminator = function
    | TERM_Invoke (t, i1, tvl, i2, i3) ->
       sprintf "invoke %s %s(%s) to %s unwind %s"
               (typ t) (ident i1) (list ", " tvalue tvl) (ident i2) (ident i3)

  and module_ : LLVM.module_-> string =
    fun m -> list "\n" toplevelentry m

  and toplevelentry : LLVM.toplevelentry -> string = function
    | TLE_Target s -> "target triple = " ^ quote s
    | TLE_Datalayout s -> "target datalayout = " ^ quote s
    | TLE_Declaration d -> declaration d
    | TLE_Definition d -> definition d
    | TLE_Type_decl (i, t) -> ident i ^ typ t
    | TLE_Global g -> global g

  and global : LLVM.global -> string = fun {
      g_ident = i;
      g_typ = t;
      g_constant = b;
      g_value = vo;
    } -> "global" ^ ident i ^ typ t ^ (string_of_bool b)
         ^ (match vo with None -> "" | Some v -> value v)

  and declaration : LLVM.declaration -> string = fun {
      dc_ret_typ = t;
      dc_name = i;
      dc_args = tl;
    } -> sprintf "declare %s %s(%s)"
                        (typ t) (ident i) (list ", " typ tl)

  and definition : LLVM.definition -> string = fun {
      df_ret_typ = t;
      df_name = i;
      df_args = til;
      df_attrs = al;
      df_instrs = (entry_block, other_blocks);
    } -> sprintf "define %s %s(%s) %s {\n%s\n%s\n}"
                        (typ t)
                        (ident i)
                        (list ", " tident til)
                        (list " " fn_attr al)
                        (unnamed_block entry_block)
                        (list "\n" named_block other_blocks)

  and unnamed_block : LLVM.unnamed_block -> string = fun b -> list "\n" instr b

  and named_block : LLVM.named_block -> string = fun (i, b) ->
    i ^ ":\n" ^ (unnamed_block b)

  and instr : LLVM.instr -> string = function
    | INSTR_Expr_Assign (i, e) -> ident i ^ " = " ^ expr e
    | INSTR_Expr_Unit e -> expr_unit e
    | INSTR_Terminator (i, t) -> ident i ^ " = " ^ terminator t
    | INSTR_Terminator_Unit t -> terminator_unit t

  in module_
