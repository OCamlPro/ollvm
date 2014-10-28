type linkage =
    LINKAGE_Private
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
and dll_storage = DLLSTORAGE_Dllimport | DLLSTORAGE_Dllexport
and visibility =
    VISIBILITY_Default
  | VISIBILITY_Hidden
  | VISIBILITY_Protected
and cconv = CC_Ccc | CC_Fastcc | CC_Coldcc | CC_Cc of int
and param_attr =
    PARAMATTR_Zeroext
  | PARAMATTR_Signext
  | PARAMATTR_Inreg
  | PARAMATTR_Byval
  | PARAMATTR_Inalloca
  | PARAMATTR_Sret
  | PARAMATTR_Align of int
  | PARAMATTR_Noalias
  | PARAMATTR_Nocapture
  | PARAMATTR_Nest
  | PARAMATTR_Returned
  | PARAMATTR_Nonnull
  | PARAMATTR_Dereferenceable of int
and fn_attr =
    FNATTR_Alignstack of int
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
  | FNATTR_String of string
  | FNATTR_Key_value of string * string
  | FNATTR_Attr_grp of int
and ident = ID_Global of string | ID_Local of string
and typ =
    TYPE_I of int
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
and metadata =
    METADATA_Const of tvalue
  | METADATA_Null
  | METADATA_Id of string
  | METADATA_String of string
  | METADATA_Named of string list
  | METADATA_Node of metadata list
and icmp = Eq | Ne | Ugt | Uge | Ult | Ule | Sgt | Sge | Slt | Sle
and fcmp =
    False
  | Oeq
  | Ogt
  | Oge
  | Olt
  | Ole
  | One
  | Ord
  | Uno
  | Ueq
  | Ugt
  | Uge
  | Ult
  | Ule
  | Une
  | True
and ibinop =
    Add of bool * bool
  | Sub of bool * bool
  | Mul of bool * bool
  | Shl of bool * bool
  | UDiv of bool
  | SDiv of bool
  | LShr of bool
  | AShr of bool
  | URem
  | SRem
  | And
  | Or
  | Xor
and fbinop = FAdd | FSub | FMul | FDiv | FRem
and fast_math = Nnan | Ninf | Nsz | Arcp | Fast
and conversion_type =
    Trunc
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
  | Bitcast
and tvalue = typ * value
and tident = typ * ident
and value =
    VALUE_Ident of ident
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
    INSTR_IBinop of ibinop * typ * value * value
  | INSTR_ICmp of icmp * typ * value * value
  | INSTR_FBinop of fbinop * fast_math list * typ * value * value
  | INSTR_FCmp of fcmp * typ * value * value
  | INSTR_Conversion of conversion_type * typ * value * typ
  | INSTR_GetElementPtr of tvalue * tvalue list
  | INSTR_ExtractElement of tvalue * tvalue
  | INSTR_InsertElement of tvalue * tvalue * tvalue
  | INSTR_ShuffleVector of tvalue * tvalue * tvalue
  | INSTR_ExtractValue of tvalue * int list
  | INSTR_InsertValue of tvalue * tvalue * int list
  | INSTR_Call of tident * tvalue list
  | INSTR_Alloca of typ * tvalue option * int option
  | INSTR_Load of bool * tvalue * int option
  | INSTR_Phi of typ * (value * ident) list
  | INSTR_Select of tvalue * tvalue * tvalue
  | INSTR_VAArg
  | INSTR_LandingPad
  | INSTR_Store of bool * tvalue * tident * int option
  | INSTR_Fence
  | INSTR_AtomicCmpXchg
  | INSTR_AtomicRMW
  | INSTR_Invoke of tident * tvalue list * tident * tident
  | INSTR_Ret of tvalue
  | INSTR_Ret_void
  | INSTR_Br of tvalue * tident * tident
  | INSTR_Br_1 of tident
  | INSTR_Switch of tvalue * tident * (tvalue * tident) list
  | INSTR_IndirectBr of tvalue * tident list
  | INSTR_Resume of tvalue
  | INSTR_Unreachable
  | INSTR_Assign of ident * instr
and toplevelentry =
    TLE_Target of string
  | TLE_Datalayout of string
  | TLE_Declaration of declaration
  | TLE_Definition of definition
  | TLE_Type_decl of (ident * typ)
  | TLE_Global of global
  | TLE_Metadata of string * metadata
  | TLE_Attribute_group of int * fn_attr list
and toplevelentries = toplevelentry list
and global = {
  g_ident : ident;
  g_typ : typ;
  g_constant : bool;
  g_value : value option;
  g_linkage : linkage option;
  g_visibility : visibility option;
  g_dll_storage : dll_storage option;
  g_thread_local : thread_local_storage option;
  g_unnamed_addr : bool;
  g_addrspace : int option;
  g_externally_initialized : bool;
  g_section : string option;
  g_align : int option;
}
and thread_local_storage = TLS_Localdynamic | TLS_Initialexec | TLS_Localexec
and declaration = {
  dc_name : ident;
  dc_type : typ;
  dc_param_attrs : param_attr list * param_attr list list;
}
and definition = {
  df_prototype : declaration;
  df_args : ident list;
  df_instrs : block list;
  df_linkage : linkage option;
  df_visibility : visibility option;
  df_dll_storage : dll_storage option;
  df_cconv : cconv option;
  df_attrs : fn_attr list;
  df_section : string option;
  df_align : int option;
  df_gc : string option;
}
and block = string * instr list
and modul = {
  m_name : string;
  m_target : toplevelentry;
  m_datalayout : toplevelentry;
  m_globals : (string * global) list;
  m_declarations : (string * declaration) list;
  m_definitions : (string * definition) list;
}
