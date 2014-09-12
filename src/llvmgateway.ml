type env = { c: Llvm.llcontext;
             m: Llvm.llmodule;
             b: Llvm.llbuilder; }

let init module_name =
  let c = Llvm.global_context () in
  let m = Llvm.create_module c module_name in
  let b = Llvm.builder c in
  {c=c; m=m; b=b}

let label : env -> Ast.ident -> Llvm.llbasicblock =
  fun env l -> assert false

let linkage : Ast.linkage -> Llvm.Linkage.t =
  let open Llvm.Linkage
  in function
  | LINKAGE_Private               -> Private
  | LINKAGE_Internal              -> Internal
  | LINKAGE_Available_externally  -> Available_externally
  | LINKAGE_Linkonce              -> Link_once
  | LINKAGE_Weak                  -> Weak
  | LINKAGE_Common                -> Common
  | LINKAGE_Appending             -> Appending
  | LINKAGE_Extern_weak           -> External_weak
  | LINKAGE_Linkonce_odr          -> Link_once_odr
  | LINKAGE_Weak_odr              -> Weak_odr
  | LINKAGE_External              -> External

let dll_storage : Ast.dll_storage -> Llvm.Linkage.t =
  let open Llvm.Linkage
  in function
  | DLLSTORAGE_Dllimport -> Dllimport
  | DLLSTORAGE_Dllexport -> Dllexport

let visibility : Ast.visibility -> Llvm.Visibility.t =
  let open Llvm.Visibility
  in function
  | VISIBILITY_Default   -> Default
  | VISIBILITY_Hidden    -> Hidden
  | VISIBILITY_Protected -> Protected

let cconv : Ast.cconv -> int =
  let open Llvm.CallConv
  in function
  | CC_Ccc    -> c
  | CC_Fastcc -> fast
  | CC_Coldcc -> cold
  | CC_Cc i   -> assert false

let typ_attr : Ast.param_attr -> Llvm.Attribute.t =
  let open Llvm.Attribute
  in function
  | PARAMATTR_Zeroext   -> Zext
  | PARAMATTR_Signext   -> Sext
  | PARAMATTR_Inreg     -> Inreg
  | PARAMATTR_Byval     -> Byval
  | PARAMATTR_Sret      -> assert false
  | PARAMATTR_Noalias   -> Noalias
  | PARAMATTR_Nocapture -> Nocapture
  | PARAMATTR_Nest      -> Nest

let fn_attr : Ast.fn_attr -> Llvm.Attribute.t =
  let open Llvm.Attribute
  in function
  | FNATTR_Alignstack i     -> Stackalignment i
  | FNATTR_Alwaysinline     -> Alwaysinline
  | FNATTR_Builtin          -> assert false
  | FNATTR_Cold             -> assert false
  | FNATTR_Inlinehint       -> Inlinehint
  | FNATTR_Jumptable        -> assert false
  | FNATTR_Minsize          -> assert false
  | FNATTR_Naked            -> Naked
  | FNATTR_Nobuiltin        -> assert false
  | FNATTR_Noduplicate      -> assert false
  | FNATTR_Noimplicitfloat  -> Noimplicitfloat
  | FNATTR_Noinline         -> Noinline
  | FNATTR_Nonlazybind      -> NonLazyBind
  | FNATTR_Noredzone        -> Noredzone
  | FNATTR_Noreturn         -> Noreturn
  | FNATTR_Nounwind         -> Nounwind
  | FNATTR_Optnone          -> assert false
  | FNATTR_Optsize          -> Optsize
  | FNATTR_Readnone         -> Readnone
  | FNATTR_Readonly         -> Readonly
  | FNATTR_Returns_twice    -> ReturnsTwice
  | FNATTR_Sanitize_address -> assert false
  | FNATTR_Sanitize_memory  -> assert false
  | FNATTR_Sanitize_thread  -> assert false
  | FNATTR_Ssp              -> Ssp
  | FNATTR_Sspreq           -> Sspreq
  | FNATTR_Sspstrong        -> assert false
  | FNATTR_Uwtable          -> UWTable
  | FNATTR_String s         -> assert false
  | FNATTR_Key_value (k, v) -> assert false
  | FNATTR_Attr_grp g       -> assert false

(*
let ident_format = function
  | ID_FORMAT_Named
  | ID_FORMAT_NamedString
  | ID_FORMAT_Unnamed
*)

(** Should lookup into env *)
let ident : env -> Ast.ident -> Llvm.llvalue =
  fun env ->
  function
  | ID_Global _
  | ID_Local _ -> assert false

let rec typ : env -> Ast.typ -> Llvm.lltype =
  fun env ->
  let ctx = env.c in
  let open Llvm
  in function
  | TYPE_I i -> begin match i with
                      | 1  -> i1_type ctx
                      | 8  -> i8_type ctx
                      | 16 -> i16_type ctx
                      | 32 -> i32_type ctx
                      | 64 -> i64_type ctx
                      | _  -> integer_type ctx i end
  | TYPE_Pointer t         -> pointer_type (typ env t)
  | TYPE_Void              -> void_type ctx
  | TYPE_Half              -> assert false
  | TYPE_Float             -> float_type ctx
  | TYPE_Double            -> double_type ctx
  | TYPE_X86_fp80          -> x86fp80_type ctx
  | TYPE_Fp128             -> fp128_type ctx
  | TYPE_Ppc_fp128         -> assert false
  | TYPE_Label             -> label_type ctx
  | TYPE_Metadata          -> assert false
  | TYPE_X86_mmx           -> x86_mmx_type ctx
  | TYPE_Array (i, t)      -> array_type (typ env t) i
  | TYPE_Function (r, a)   ->
     function_type (typ env r) (Array.of_list a |> Array.map (typ env))
  | TYPE_Struct s          ->
     struct_type ctx (Array.of_list s |> Array.map (typ env))
  | TYPE_Packed_struct s   ->
     packed_struct_type ctx (Array.of_list s |> Array.map (typ env))
  | TYPE_Opaque            -> assert false
  | TYPE_Vector (i, t)      -> vector_type (typ env t) i

(*
let metadata = function
  | METADATA_Const of tvalue
  | METADATA_Null
  | METADATA_Id of string
  | METADATA_String of string
  | METADATA_Named of string list
  | METADATA_Node of metadata list
*)

let icmp : Ast.icmp -> Llvm.Icmp.t =
  let open Llvm.Icmp
  in function
  | Eq  -> Eq
  | Ne  -> Ne
  | Ugt -> Ugt
  | Uge -> Uge
  | Ult -> Ult
  | Ule -> Ule
  | Sgt -> Sgt
  | Sge -> Sge
  | Slt -> Slt
  | Sle -> Sle

let fcmp : Ast.fcmp -> Llvm.Fcmp.t =
  let open Llvm.Fcmp
  in function
  | False -> False
  | Oeq   -> Oeq
  | Ogt   -> Ogt
  | Oge   -> Oge
  | Olt   -> Olt
  | Ole   -> Ole
  | One   -> One
  | Ord   -> Ord
  | Uno   -> Uno
  | Ueq   -> Ueq
  | Ugt   -> Ugt
  | Uge   -> Uge
  | Ult   -> Ult
  | Ule   -> Ule
  | Une   -> Une
  | True  -> True

let ibinop : Ast.ibinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue) =
  let open Llvm
  in function
  | Add (_, _) -> build_add
  | Sub (_, _) -> build_sub
  | Mul (_, _) -> build_mul
  | UDiv _     -> build_udiv
  | SDiv _     -> build_sdiv
  | URem       -> build_urem
  | SRem       -> build_srem
  | Shl (_, _) -> build_shl
  | LShr _     -> build_lshr
  | AShr _     -> build_ashr
  | And        -> build_and
  | Or         -> build_or
  | Xor        -> build_xor

let fbinop : Ast.fbinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue) =
  let open Llvm
  in function
  | FAdd -> build_fadd
  | FSub -> build_fsub
  | FMul -> build_fmul
  | FDiv -> build_fdiv
  | FRem -> build_frem

let conversion_type : Ast.conversion_type ->
                      (Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue) =
  let open Llvm
  in function
  | Trunc    -> const_trunc
  | Zext     -> const_zext
  | Sext     -> const_sext
  | Fptrunc  -> const_fptrunc
  | Fpext    -> const_fpext
  | Uitofp   -> const_uitofp
  | Sitofp   -> const_sitofp
  | Fptoui   -> const_fptoui
  | Fptosi   -> const_fptosi
  | Inttoptr -> const_inttoptr
  | Ptrtoint -> const_ptrtoint
  | Bitcast  -> const_bitcast

(*
and tvalue = typ * value

and tident = typ * ident
 *)

(** FIXME: should be splitted into const/value? *)
let rec value : env -> Ast.typ -> Ast.value -> Llvm.llvalue =
  fun env ty ->
  let open Llvm
  in function
  | VALUE_Ident i          -> assert false
  | VALUE_Integer i        -> const_int (typ env ty) i
  | VALUE_Float f          -> const_float (typ env ty) f
  | VALUE_Bool b           -> assert false
  | VALUE_Null             -> const_null (typ env ty)
  | VALUE_Undef            -> assert false
  | VALUE_Struct s         ->
     const_struct env.c (Array.of_list s |> Array.map (fun (ty, v) -> value env ty v))
  | VALUE_Packed_struct s  ->
     const_packed_struct env.c (Array.of_list s
                                |> Array.map (fun (ty, v) -> value env ty v))
  | VALUE_Array a          ->
     const_array  (typ env ty) (Array.of_list a
                                |> Array.map (fun (ty, v) -> value env ty v))
  | VALUE_Vector v         ->
     const_vector (Array.of_list v |> Array.map (fun (ty, v) -> value env ty v))
  | VALUE_Zero_initializer -> assert false

let rec instr : env -> Ast.instr -> Llvm.llvalue =
  fun env ->
  let open Llvm
  in function

  | INSTR_IBinop (op, ty, v1, v2)       ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let op = ibinop op in
     op v1 v2 "" env.b

  | INSTR_ICmp (cmp, ty, v1, v2)        ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let cmp = icmp cmp in
     build_icmp cmp v1 v2 "" env.b

  | INSTR_FBinop (op, _, ty, v1, v2)       ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let op = fbinop op in
     op v1 v2 "" env.b

  | INSTR_FCmp (cmp, ty, v1, v2)        ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let cmp = fcmp cmp in
     build_fcmp cmp v1 v2 "" env.b

  | INSTR_Conversion (conv, ty, v, ty') ->
     let v = value env ty v in
     let conv = conversion_type conv in
     conv v (typ env ty')

  | INSTR_GetElementPtr (tv, tvl)       -> assert false

  | INSTR_ExtractElement ((ty, vec), (ty', idx))      ->
     let vec = value env ty vec in
     let idx = value env ty' idx in
     build_extractelement vec idx "" env.b

  | INSTR_InsertElement ((ty, vec), (ty', el), (ty'', idx))  ->
     let vec = value env ty vec in
     let el = value env ty' el in
     let idx = value env ty'' idx in
     build_insertelement vec el idx "" env.b

  | INSTR_ShuffleVector ((t, v), (t', v'), (t'', v'')) ->
     let v = value env t v in
     let v' = value env t' v' in
     let v'' = value env t'' v'' in
     build_shufflevector v v' v'' "" env.b

  | INSTR_ExtractValue ((t, v), idx)         ->
     (* FIXME: llvm api take an int and not a list... *)
     let v = value env t v in
     build_extractvalue v (List.hd idx) "" env.b

  | INSTR_InsertValue ((t, vec), (t', el), idx)    ->
     (* FIXME: llvm api take an int and not a list... *)
     let vec = value env t vec in
     let el = value env t' el in
     build_insertvalue vec el (List.hd idx) "" env.b

  | INSTR_Call ((t, i), args)                ->
     let fn = ident env i in
     let args = Array.of_list args
                |> Array.map (fun (t, v) -> value env t v) in
     build_call fn args "" env.b

  | INSTR_Alloca (ty, nb, _)          ->
     begin
       match nb with
       | None -> build_alloca (typ env ty) "" env.b
       | Some (t, nb) ->
          build_array_alloca (typ env ty) (value env t nb) "" env.b
     end

  | INSTR_Load (_, (t, v), _)                 ->
     build_load (value env t v) "" env.b

  | INSTR_Phi (t, incoming)                 ->
     let incoming =
       List.map (fun (v, i) -> (value env t v, label env i)) incoming in
     build_phi incoming "" env.b

  | INSTR_Select ((t, cond), (t', thenv), (t'', elsev))        ->
     let cond = value env t cond in
     let thenv = value env t' thenv in
     let elsev = value env t'' elsev in
     build_select cond thenv elsev "" env.b

  | INSTR_VAArg                         -> assert false
  | INSTR_LandingPad                    -> assert false

  | INSTR_Store (_, (t, v), (_, p), _) ->
     let v = value env t v in
     let p = ident env p in
     build_store v p env.b

  | INSTR_Fence                         -> assert false
  | INSTR_AtomicCmpXchg                 -> assert false
  | INSTR_AtomicRMW                     -> assert false

  | INSTR_Invoke (ti1, tvl, ti2, ti3)   ->  assert false
  (* build_invoke fn args tobb unwindbb name b*)

  | INSTR_Ret (t, v)                    ->
     build_ret (value env t v) env.b

  | INSTR_Ret_void                      ->
     build_ret_void env.b

  | INSTR_Br ((t, v), (_, tbb), (_, fbb))   ->
     let cond = value env t v in
     let tbb = label env tbb in
     let fbb = label env fbb in
     build_cond_br cond tbb fbb env.b

  | INSTR_Br_1 (_, i)                   ->
     build_br (label env i) env.b

  | INSTR_Switch ((tv), ti, tvtil)        -> assert false
  (* (fun ((t, v), (_, l)) -> (value env t v, label env) ) *)

  | INSTR_IndirectBr                    -> assert false
  | INSTR_Resume tv                     -> assert false
  | INSTR_Unreachable                   -> assert false
  | INSTR_Assign (id, inst)             -> assert false

let toplevelentry : Ast.toplevelentry -> unit = function
  | TLE_Target _
  | TLE_Datalayout _
  | TLE_Declaration _
  | TLE_Definition _
  | TLE_Type_decl _
  | TLE_Global _
  | TLE_Metadata _
  | TLE_Attribute_group _ -> assert false

and toplevelentries = fun x -> assert false

let global = fun x -> assert false

let declaration = fun x -> assert false

let definition = fun x -> assert false

and block = fun x -> assert false

let modul = fun x -> assert false
