module Type = struct

  open Ollvm_ast

  type t = Ollvm_ast.typ

  let i1 = TYPE_I 1
  let i32 = TYPE_I 32
  let half = TYPE_Half
  let float = TYPE_Float
  let double = TYPE_Double
  let pointer t = TYPE_Pointer t
  let vector n t = TYPE_Vector (n, t)
  let label = TYPE_Label
  let void = TYPE_Void
  let array n t = TYPE_Array (n, t)
  let structure s = TYPE_Struct s

end

module Value = struct

  type t = Type.t * Ollvm_ast.value

  let i1 n = (Type.i1, Ollvm_ast.VALUE_Integer n)

  let i32 n = (Type.i32, Ollvm_ast.VALUE_Integer n)

  let half f = (Type.half, Ollvm_ast.VALUE_Float f)

  let float f = (Type.float, Ollvm_ast.VALUE_Float f)

  let double f = (Type.double, Ollvm_ast.VALUE_Float f)

  let vector l =
    (Type.vector (List.length l) (fst (List.hd l)), Ollvm_ast.VALUE_Vector l)

  let array l =
    (Type.array (List.length l) (fst (List.hd l)), Ollvm_ast.VALUE_Array l)

  let structure l =
    (Type.structure (List.map fst l),
     Ollvm_ast.VALUE_Struct l)

  let ident (t, Ollvm_ast.VALUE_Ident id) = (t, id)

end

module Instr = struct

  type t = Type.t * Ollvm_ast.instr

  let ident = Value.ident

  let call ((t, _) as fn) args =
    (t, Ollvm_ast.INSTR_Call (ident fn, args))

  let phi value_label =
    let t = List.hd value_label |> fst |> fst in
    let value_label =
      List.map (fun (v, i) -> (snd v, snd (ident i))) value_label in
    (t, Ollvm_ast.INSTR_Phi (t, value_label))

  let select tcond (t, v1) tv2 =
    (t, Ollvm_ast.INSTR_Select (tcond, (t, v1), tv2))

  let alloca ?(nb=None) ?(align=None) t =
    (Type.pointer t, Ollvm_ast.INSTR_Alloca (t, nb, align))

  let load ?(volatile=false) ?(align=None) (ptr_t, value) =
    let Ollvm_ast.TYPE_Pointer t = ptr_t in
    (t, Ollvm_ast.INSTR_Load (volatile, (ptr_t, value), align))

  let store ?(volatile=false)? (align=None) value pointer =
    (Type.void, Ollvm_ast.INSTR_Store (volatile, value, ident pointer, align))

  let icmp cmp (t, op1) (_, op2) =
    (Type.i1, Ollvm_ast.INSTR_ICmp (cmp, t, op1, op2))

  let eq = icmp Ollvm_ast.Eq let ne = icmp Ollvm_ast.Ne
  let ugt = icmp Ollvm_ast.Ugt let uge = icmp Ollvm_ast.Uge
  let ult = icmp Ollvm_ast.Ult let ule = icmp Ollvm_ast.Ule
  let sgt = icmp Ollvm_ast.Sgt let sge = icmp Ollvm_ast.Sge
  let slt = icmp Ollvm_ast.Slt let sle = icmp Ollvm_ast.Sle

  let fcmp cmp (t, op1) (_, op2) =
    (Type.i1, Ollvm_ast.INSTR_FCmp (cmp, t, op1, op2))

  let ffalse = fcmp Ollvm_ast.False let foeq = fcmp Ollvm_ast.Oeq
  let fogt = fcmp Ollvm_ast.Ogt let foge = fcmp Ollvm_ast.Oge
  let folt = fcmp Ollvm_ast.Olt let fole = fcmp Ollvm_ast.Ole
  let fone = fcmp Ollvm_ast.One let ord = fcmp Ollvm_ast.Ord
  let fueq = fcmp Ollvm_ast.Ueq let fugt = fcmp Ollvm_ast.Ugt
  let fuge = fcmp Ollvm_ast.Uge let fult = fcmp Ollvm_ast.Ult
  let fule = fcmp Ollvm_ast.Ule let fune = fcmp Ollvm_ast.Une
  let funo = fcmp Ollvm_ast.Uno let ftrue = fcmp Ollvm_ast.True

  let ibinop b (t, op1) (_, op2) =
    (t, Ollvm_ast.INSTR_IBinop (b, t, op1, op2))

  let add ?(nsw=false) ?(nuw=false) = ibinop (Ollvm_ast.Add (nsw, nuw))
  let sub ?(nsw=false) ?(nuw=false) = ibinop (Ollvm_ast.Sub (nsw, nuw))
  let mul ?(nsw=false) ?(nuw=false) = ibinop (Ollvm_ast.Mul (nsw, nuw))
  let udiv ?(exact=false) = ibinop (Ollvm_ast.UDiv exact)
  let sdiv ?(exact=false) = ibinop (Ollvm_ast.SDiv exact)
  let urem = ibinop Ollvm_ast.URem
  let srem = ibinop Ollvm_ast.SRem
  let shl ?(nsw=false) ?(nuw=false) = ibinop (Ollvm_ast.Shl (nsw, nuw))
  let lshr ?(exact=false) = ibinop (Ollvm_ast.LShr exact)
  let ashr ?(exact=false) = ibinop (Ollvm_ast.AShr exact)
  let and_ = ibinop Ollvm_ast.And
  let or_ = ibinop Ollvm_ast.Or
  let xor = ibinop Ollvm_ast.Xor

  let fbinop b ?(flags=[])  (t, op1) (_, op2) =
    (t, Ollvm_ast.INSTR_FBinop (b, flags, t, op1, op2))

  let fadd = fbinop Ollvm_ast.FAdd
  let fsub = fbinop Ollvm_ast.FSub
  let fmul = fbinop Ollvm_ast.FMul
  let fdiv = fbinop Ollvm_ast.FDiv
  let frem = fbinop Ollvm_ast.FRem

  let extractelement vec idx =
    let (Ollvm_ast.TYPE_Vector (n, t), _) = vec in
    (t, Ollvm_ast.INSTR_ExtractElement (vec, idx))

  let insertelement vec el idx =
    (fst vec, Ollvm_ast.INSTR_InsertElement (vec, el, idx))

  let shufflevector v1 v2 vmask =
    let (vec_t, _) = v1 in
    (vec_t, Ollvm_ast.INSTR_ShuffleVector (v1, v2, vmask))

  let convert op (t, v) t' = (t', Ollvm_ast.INSTR_Conversion(op, t, v, t'))

  let trunc = convert Ollvm_ast.Trunc let zext = convert Ollvm_ast.Zext
  let sext = convert Ollvm_ast.Sext let fptrunc = convert Ollvm_ast.Fptrunc
  let fpext = convert Ollvm_ast.Fpext let fptoui = convert Ollvm_ast.Fptoui
  let fptosi = convert Ollvm_ast.Fptosi let uitofp = convert Ollvm_ast.Uitofp
  let sitofp = convert Ollvm_ast.Sitofp

  let extractvalue agg idx =
    (fst agg, Ollvm_ast.INSTR_ExtractValue (agg, idx))

  let insertvalue agg el idx =
    (fst agg, Ollvm_ast.INSTR_InsertValue (agg, el, idx))

  let br cond (t, Ollvm_ast.VALUE_Ident then_) (t', Ollvm_ast.VALUE_Ident else_) =
    Ollvm_ast.INSTR_Br (cond, (t, then_), (t', else_))

  let br1 (t, Ollvm_ast.VALUE_Ident branch) =
    Ollvm_ast.INSTR_Br_1 (t, branch)

  let switch sw default cases =
    let cases = List.map (fun (v, i) -> (v, ident i)) cases in
    Ollvm_ast.INSTR_Switch (sw, ident default, cases)

  let indirectbr tv til =
    Ollvm_ast.INSTR_IndirectBr (tv, List.map ident til)

  let ret x = Ollvm_ast.INSTR_Ret x

  let ret_void = Ollvm_ast.INSTR_Ret_void

  let assign id (_, expr) =
    let (_, id) = ident id in
    Ollvm_ast.INSTR_Assign (id, expr)

  let ( <-- ) tid texpr = assign tid texpr

  let ignore (_, expr) = expr

end

module Block = struct

  type block = Ollvm_ast.ident * (Ollvm_ast.instr list)

  let declare fn args_typ =
    let (t, id) = Value.ident fn in
    let open Ollvm_ast in
    { dc_type = TYPE_Function (t, args_typ);
      dc_name = id;
      dc_param_attrs = ([], List.map (fun _ -> []) args_typ) }

  let define fn args (instrs : block list) =
    let args = List.map Value.ident args in
    let open Ollvm_ast in
    let extract_name = function
      | Ollvm_ast.ID_Local s -> s
      | _ -> assert false in

    let blocks = List.map (fun (id, instrs) -> (extract_name id, instrs)) instrs

    in
    let proto = declare fn (List.map fst args) in

    { df_prototype = proto;
      df_args = List.map snd args;
      df_instrs = blocks;
      df_linkage =  None;
      df_visibility = None;
      df_dll_storage = None;
      df_cconv = None;
      df_attrs = [];
      df_section = None;
      df_align = None;
      df_gc = None;
    }

  let block  id instrs =
    let (_, id) = Value.ident id in (id, instrs)

end

module Module = struct

  module Local = struct

    type t = { unnamed_counter : int;
               named_counter : (string * int) list }

    let empty = { unnamed_counter = 0 ;
                  named_counter = [] }

    (* FIXME: Use better structure than list *)
    let local env t name =
      let (env, name) = match name with
        | "" ->
           let i = env.unnamed_counter in
           ({ env with unnamed_counter = i + 1 ; },
            string_of_int i)
        | name -> try
                  let i = List.assoc name env.named_counter in
                  ({ env with
                     named_counter = (name, i + 1) :: env.named_counter },
                   name ^ string_of_int i)
                  with Not_found ->
                    ({ env with
                       named_counter = (name, 0) :: env.named_counter },
                     name)
      in
      (env, (t, Ollvm_ast.VALUE_Ident (Ollvm_ast.ID_Local name)))

  end

  (** NOTE: declaration and definitions are kept in reverse order. **)

  type t = {
    m_module: Ollvm_ast.modul;
    m_env: Local.t;
  }

  let init name (arch, vendor, os) data_layout =
    { m_module = {
        m_name = name ;
        m_target = Ollvm_ast.TLE_Target (arch ^ "-" ^ vendor ^ "-" ^ os) ;
        m_datalayout = Ollvm_ast.TLE_Datalayout data_layout ;
        m_globals = [] ;
        m_declarations = [] ;
        m_definitions = [] ;
      };
      m_env = Local.empty }


  let set_data_layout m layout =
    { m with
      m_module = { m.m_module with
                   m_datalayout = Ollvm_ast.TLE_Datalayout layout} }

  let set_target_triple m arch vendor os =
    { m with
      m_module = { m.m_module with
                   m_target = Ollvm_ast.TLE_Target (arch^"-"^vendor^"-"^os) } }

  let local m t name =
    let (env, var) = Local.local m.m_env t name in
    ({m with m_env = env}, var)

  let locals m t list =
    let rec loop m acc = function
      | []     -> (m, List.rev acc)
      | n :: q -> let (env, x) = local m t n in
                  loop env (x :: acc) q
    in loop m [] list

  let batch_locals m list =
    let rec loop m acc = function
      | []     -> (m, List.rev acc)
      | (t, n) :: q -> let (env, x) = local m t n in
                       loop env (x :: acc) q
    in loop m [] list

  let global m t name =
    let ident = Ollvm_ast.ID_Global name in
    let var = (t, Ollvm_ast.VALUE_Ident ident) in
    (m, var)

  let lookup_declaration m name =
    List.assoc name m.m_module.m_declarations

  let lookup_definition m name =
    List.assoc name m.m_module.m_definitions

  let declaration m dc =
    let Ollvm_ast.ID_Global name = dc.Ollvm_ast.dc_name in
    { m with m_module = { m.m_module with
                          m_declarations = (name, dc)
                                           :: m.m_module.m_declarations } }

  let definition m df =
    let { Ollvm_ast.df_prototype = dc; _; } = df in
    let Ollvm_ast.ID_Global name = dc.dc_name in
    { m with m_module = { m.m_module with
                          m_declarations = (name, dc)
                                           :: m.m_module.m_declarations ;
                          m_definitions = (name, df)
                                          :: m.m_module.m_definitions } }

end
