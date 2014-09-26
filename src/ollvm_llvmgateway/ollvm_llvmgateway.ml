open Ollvm.Ast

type env = { c: Llvm.llcontext;
             m: Llvm.llmodule;
             b: Llvm.llbuilder;

             (* llvalue/llbasicblock binded to Ollvm.Ast.ident*)
             mem: (Ollvm.Ast.ident * Llvm.llvalue) list;
             labels: (string * Llvm.llbasicblock) list }

let lookup env id = List.assoc id env.mem

let lookup_fn env (id : Ollvm.Ast.ident) : Llvm.llvalue = match id with
  | ID_Local _ -> assert false
  | ID_Global i -> match Llvm.lookup_function i env.m with
                   | Some fn -> fn
                   | _ -> assert false

let string_of_ident : Ollvm.Ast.ident -> string = function
  | ID_Local i
  | ID_Global i -> i

let label : env -> Ollvm.Ast.ident -> Llvm.llbasicblock =
  fun env id -> List.assoc (string_of_ident id) env.labels

let linkage : Ollvm.Ast.linkage -> Llvm.Linkage.t =
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

let dll_storage : Ollvm.Ast.dll_storage -> Llvm.Linkage.t =
  let open Llvm.Linkage
  in function
  | DLLSTORAGE_Dllimport -> Dllimport
  | DLLSTORAGE_Dllexport -> Dllexport

let visibility : Ollvm.Ast.visibility -> Llvm.Visibility.t =
  let open Llvm.Visibility
  in function
  | VISIBILITY_Default   -> Default
  | VISIBILITY_Hidden    -> Hidden
  | VISIBILITY_Protected -> Protected

let cconv : Ollvm.Ast.cconv -> int =
  let open Llvm.CallConv
  in function
  | CC_Ccc    -> c
  | CC_Fastcc -> fast
  | CC_Coldcc -> cold
  | CC_Cc i   -> assert false

let typ_attr : Ollvm.Ast.param_attr -> Llvm.Attribute.t =
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

let fn_attr : Ollvm.Ast.fn_attr -> Llvm.Attribute.t =
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

let rec typ : env -> Ollvm.Ast.typ -> Llvm.lltype =
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

let icmp : Ollvm.Ast.icmp -> Llvm.Icmp.t =
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

let fcmp : Ollvm.Ast.fcmp -> Llvm.Fcmp.t =
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

let ibinop : Ollvm.Ast.ibinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
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

let fbinop : Ollvm.Ast.fbinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue) =
  let open Llvm
  in function
  | FAdd -> build_fadd
  | FSub -> build_fsub
  | FMul -> build_fmul
  | FDiv -> build_fdiv
  | FRem -> build_frem

let conversion_type : Ollvm.Ast.conversion_type ->
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

(** FIXME: should be splitted into const/value? *)
let rec value : env -> Ollvm.Ast.typ -> Ollvm.Ast.value -> Llvm.llvalue =
  fun env ty ->
  let open Llvm
  in function
  | VALUE_Ident i          -> lookup env i
  | VALUE_Integer i        -> const_int (typ env ty) i
  | VALUE_Float f          -> const_float (typ env ty) f
  | VALUE_Bool b           -> const_int (Llvm.i1_type env.c) (if b then 1 else 0)
  | VALUE_Null             -> const_null (typ env ty)
  | VALUE_Undef            -> undef (typ env ty)
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

let rec instr : env -> Ollvm.Ast.instr -> (env * Llvm.llvalue) =
  fun env ->
  let open Llvm in
  function

  | INSTR_IBinop (op, ty, v1, v2)       ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let op = ibinop op in
     (env, op v1 v2 "" env.b)

  | INSTR_ICmp (cmp, ty, v1, v2)        ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let cmp = icmp cmp in
     (env, build_icmp cmp v1 v2 "" env.b)

  | INSTR_FBinop (op, _, ty, v1, v2)       ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let op = fbinop op in
     (env, op v1 v2 "" env.b)

  | INSTR_FCmp (cmp, ty, v1, v2)        ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let cmp = fcmp cmp in
     (env, build_fcmp cmp v1 v2 "" env.b)

  | INSTR_Conversion (conv, ty, v, ty') ->
     let v = value env ty v in
     let conv = conversion_type conv in
     (env, conv v (typ env ty'))

  | INSTR_GetElementPtr ((t, v), tvl)       ->
     let indices = List.map (fun (t,v) -> value env t v) tvl
                   |> Array.of_list in
     (env, build_gep (value env t v) indices "" env.b)

  | INSTR_ExtractElement ((ty, vec), (ty', idx))      ->
     let vec = value env ty vec in
     let idx = value env ty' idx in
     (env, build_extractelement vec idx "" env.b)

  | INSTR_InsertElement ((ty, vec), (ty', el), (ty'', idx))  ->
     let vec = value env ty vec in
     let el = value env ty' el in
     let idx = value env ty'' idx in
     (env, build_insertelement vec el idx "" env.b)

  | INSTR_ShuffleVector ((t, v), (t', v'), (t'', v'')) ->
     let v = value env t v in
     let v' = value env t' v' in
     let v'' = value env t'' v'' in
     (env, build_shufflevector v v' v'' "" env.b)

  | INSTR_ExtractValue ((t, v), idx)         ->
     (* FIXME: llvm api take an int and not a list... *)
     let v = value env t v in
     (env, build_extractvalue v (List.hd idx) "" env.b)

  | INSTR_InsertValue ((t, vec), (t', el), idx)    ->
     (* FIXME: llvm api take an int and not a list... *)
     let vec = value env t vec in
     let el = value env t' el in
     (env, build_insertvalue vec el (List.hd idx) "" env.b)

  | INSTR_Call ((t, i), args)                ->
     let fn = lookup_fn env i in
     let args = Array.of_list args
                |> Array.map (fun (t, v) -> value env t v) in
     (env, build_call fn args "" env.b)

  | INSTR_Alloca (ty, nb, _)          ->
     (env,
       match nb with
       | None -> build_alloca (typ env ty) "" env.b
       | Some (t, nb) ->
          build_array_alloca (typ env ty) (value env t nb) "" env.b )

  | INSTR_Load (_, (t, v), _)                 ->
     (env, build_load (value env t v) "" env.b)

  | INSTR_Phi (t, incoming)                 ->
     let incoming =
       List.map (fun (v, i) -> (value env t v, label env i)) incoming in
     (env, build_phi incoming "" env.b)

  | INSTR_Select ((t, cond), (t', thenv), (t'', elsev))        ->
     let cond = value env t cond in
     let thenv = value env t' thenv in
     let elsev = value env t'' elsev in
     (env, build_select cond thenv elsev "" env.b)

  | INSTR_VAArg                         -> assert false
  | INSTR_LandingPad                    -> assert false

  | INSTR_Store (_, (t, v), (_, p), _) ->
     let v = value env t v in
     let p = lookup env p in
     (env, build_store v p env.b)

  | INSTR_Fence                         -> assert false
  | INSTR_AtomicCmpXchg                 -> assert false
  | INSTR_AtomicRMW                     -> assert false

  | INSTR_Invoke ((t, i1), tvl, (_, i2), (_, i3))   ->
     let args = List.map (fun (t, v) -> value env t v) tvl
                |> Array.of_list in
     let fn = lookup_fn env i1 in
     (env, build_invoke fn args (label env i2) (label env i3) "" env.b)

  | INSTR_Ret (t, v)                    ->
     (env, build_ret (value env t v) env.b)

  | INSTR_Ret_void                      ->
     (env, build_ret_void env.b)

  | INSTR_Br ((t, v), (_, tbb), (_, fbb))   ->
     let cond = value env t v in
     let tbb = label env tbb in
     let fbb = label env fbb in
     (env, build_cond_br cond tbb fbb env.b)

  | INSTR_Br_1 (_, i)                   ->
     (env, build_br (label env i) env.b)

  | INSTR_Switch ((t, v), (t', i), tvtil)        ->
     let case = value env t v in
     let elsebb = label env i in
     let count = List.length tvtil in
     let switch = Llvm.build_switch case elsebb count env.b in
     List.iter (fun ((t, v), (t', i)) ->
                Llvm.add_case switch (value env t v) (label env i))
               tvtil ;
     (env, switch)


  | INSTR_IndirectBr                    -> assert false

  | INSTR_Resume (t, v)                 ->
     let llv = value env t v in
     (env, build_resume llv env.b)

  | INSTR_Unreachable                   -> (env, build_unreachable env.b)

  | INSTR_Assign (id, inst)             ->
     let (env, llv) = instr env inst in
     ({ env with mem = (id, llv) :: env.mem }, llv)

let global : env -> Ollvm.Ast.global -> env =
  fun env g ->
  let llv = value env g.g_typ (match g.g_value with Some x -> x
                                                  | None -> assert false) in
  let Ollvm.Ast.ID_Global name = g.g_ident in
  let llv = Llvm.define_global name llv env.m in
  {env with mem = (g.g_ident, llv) :: env.mem }

let declaration : env -> Ollvm.Ast.declaration -> env * Llvm.llvalue =
  fun env dc ->
  let name = (string_of_ident dc.dc_name) in
  let fn =  match Llvm.lookup_function name env.m with
    | None -> Llvm.declare_function name (typ env dc.dc_type) env.m ;
    | Some fn -> fn in
  (env, fn)

let create_block : env -> Ollvm.Ast.block -> Llvm.llvalue -> env =
  fun env b fn ->
  if List.mem_assoc (fst b) env.labels then assert false ;
  let llb = Llvm.append_block env.c (fst b) fn in
  { env with labels = (fst b, llb) :: env.labels }

let block : env -> Ollvm.Ast.block -> env =
  fun env block ->
  let bb = List.assoc (fst block) env.labels in
  Llvm.position_at_end bb env.b;
  (* process instructions *)
  let env = List.fold_left (fun env i -> instr env i |> fst) env (snd block) in
  env

let definition : env -> Ollvm.Ast.definition -> env =
  fun env df ->
  let (env, fn) = declaration env df.df_prototype in
  (* Do not allow function redefinition. May change? *)
  if Array.length (Llvm.basic_blocks fn) <> 0
  then assert false;
  let env =
    lookup_fn env df.df_prototype.dc_name
    |> Llvm.params
    |> Array.mapi (fun i a -> (List.nth df.df_args i, a ))
    |> Array.fold_left (fun env (i, a) ->
                        Llvm.set_value_name (string_of_ident i) a;
                        { env with mem = (i, a) :: env.mem }) env in
  (* First create needed blocks.
   * [block] function will use them when building instructions. *)
  let env =
    List.fold_left (fun env b -> create_block env b fn) env df.df_instrs in
  List.fold_left (fun env bl -> block env bl) env (df.df_instrs)

let modul : Ollvm.Ast.modul -> env =
  fun modul ->
  let c = Llvm.global_context () in
  let m = Llvm.create_module c modul.m_name in
  let b = Llvm.builder c in
  let Ollvm.Ast.TLE_Target target = modul.m_target in
  let Ollvm.Ast.TLE_Datalayout datalayout = modul.m_datalayout in
  Llvm.set_target_triple target m;
  Llvm.set_data_layout datalayout m;
  let env = { c = c; m = m; b = b; mem = []; labels = [] } in
  let env = List.fold_left (fun env g -> global {env with mem=[]} g)
                           env (List.map snd modul.m_globals) in
  let env = List.fold_left (fun env dc -> fst (declaration {env with mem=[]} dc))
                           env (List.map snd modul.m_declarations) in
  let env = List.fold_left (fun env df -> definition {env with mem=[];
                                                               labels=[]} df)
                           env (List.map snd modul.m_definitions) in
  { env with mem = [] ; labels = [] }
