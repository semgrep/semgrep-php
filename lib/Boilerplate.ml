(**
   Boilerplate to be used as a template when mapping the php CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_pat_elseif (env : env) (tok : CST.pat_elseif) =
  (* pattern [eE][lL][sS][eE][iI][fF] *) token env tok

let map_pat_requ (env : env) (tok : CST.pat_requ) =
  (* pattern [rR][eE][qQ][uU][iI][rR][eE] *) token env tok

let map_tok_prec_n1_pat_524a507 (env : env) (tok : CST.tok_prec_n1_pat_524a507) =
  (* tok_prec_n1_pat_524a507 *) token env tok

let map_pat_else (env : env) (tok : CST.pat_else) =
  (* pattern [eE][lL][sS][eE] *) token env tok

let map_pat_endf (env : env) (tok : CST.pat_endf) =
  (* pattern [eE][nN][dD][fF][oO][rR][eE][aA][cC][hH] *) token env tok

let map_pat_fn (env : env) (tok : CST.pat_fn) =
  (* pattern [fF][nN] *) token env tok

let map_eof (env : env) (tok : CST.eof) =
  (* eof *) token env tok

let map_pat_ret (env : env) (tok : CST.pat_ret) =
  (* pattern [rR][eE][tT][uU][rR][nN] *) token env tok

let map_pat_while (env : env) (tok : CST.pat_while) =
  (* pattern [wW][hH][iI][lL][eE] *) token env tok

let map_pat_endw (env : env) (tok : CST.pat_endw) =
  (* pattern [eE][nN][dD][wW][hH][iI][lL][eE] *) token env tok

let map_pat_public (env : env) (tok : CST.pat_public) =
  (* pattern [pP][uU][bB][lL][iI][cC] *) token env tok

let map_pat_imples (env : env) (tok : CST.pat_imples) =
  (* pattern [iI][mM][pP][lL][eE][mM][eE][nN][tT][sS] *) token env tok

let map_pat_defa (env : env) (tok : CST.pat_defa) =
  (* pattern [dD][eE][fF][aA][uU][lL][tT] *) token env tok

let map_pat_endif (env : env) (tok : CST.pat_endif) =
  (* pattern [eE][nN][dD][iI][fF] *) token env tok

let map_pat_prot (env : env) (tok : CST.pat_prot) =
  (* pattern [pP][rR][oO][tT][eE][cC][tT][eE][dD] *) token env tok

let map_pat_requ_once (env : env) (tok : CST.pat_requ_once) =
  (* pattern [rR][eE][qQ][uU][iI][rR][eE][__][oO][nN][cC][eE] *) token env tok

let map_name (env : env) (tok : CST.name) =
  (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok

let map_pat_priv (env : env) (tok : CST.pat_priv) =
  (* pattern [pP][rR][iI][vV][aA][tT][eE] *) token env tok

let map_pat_incl (env : env) (tok : CST.pat_incl) =
  (* pattern [iI][nN][cC][lL][uU][dD][eE] *) token env tok

let map_var_modifier (env : env) (tok : CST.var_modifier) =
  (* pattern [vV][aA][rR] *) token env tok

let map_pat_abst (env : env) (tok : CST.pat_abst) =
  (* pattern [aA][bB][sS][tT][rR][aA][cC][tT] *) token env tok

let map_pat_name (env : env) (tok : CST.pat_name) =
  (* pattern [nN][aA][mM][eE][sS][pP][aA][cC][eE] *) token env tok

let map_pat_enum (env : env) (tok : CST.pat_enum) =
  (* pattern [eE][nN][uU][mM] *) token env tok

let map_pat_if (env : env) (tok : CST.pat_if) =
  (* pattern [iI][fF] *) token env tok

let map_pat_throw (env : env) (tok : CST.pat_throw) =
  (* pattern [tT][hH][rR][oO][wW] *) token env tok

let map_string_ (env : env) (tok : CST.string_) =
  (* string *) token env tok

let map_shell_command_expression (env : env) (tok : CST.shell_command_expression) =
  (* shell_command_expression *) token env tok

let map_boolean (env : env) (tok : CST.boolean) =
  (* pattern [Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee] *) token env tok

let map_pat_for (env : env) (tok : CST.pat_for) =
  (* pattern [fF][oO][rR] *) token env tok

let map_pat_endd (env : env) (tok : CST.pat_endd) =
  (* pattern [eE][nN][dD][dD][eE][cC][lL][aA][rR][eE] *) token env tok

let map_heredoc (env : env) (tok : CST.heredoc) =
  (* heredoc *) token env tok

let map_float_ (env : env) (tok : CST.float_) =
  (* pattern \d*(_\d+)*((\.\d*(_\d+)*\
  )?([eE][\+-]?\d+(_\d+)*\
  )|(\.\d\d*(_\d+)*\
  )([eE][\+-]?\d+(_\d+)*\
  )?) *) token env tok

let map_php_tag (env : env) (tok : CST.php_tag) =
  (* pattern <\?([pP][hH][pP]|=)? *) token env tok

let map_pat_case (env : env) (tok : CST.pat_case) =
  (* pattern [cC][aA][sS][eE] *) token env tok

let map_pat_goto (env : env) (tok : CST.pat_goto) =
  (* pattern [gG][oO][tT][oO] *) token env tok

let map_pat_ends (env : env) (tok : CST.pat_ends) =
  (* pattern [eE][nN][dD][sS][wW][iI][tT][cC][hH] *) token env tok

let map_pat_fina (env : env) (tok : CST.pat_fina) =
  (* pattern [fF][iI][nN][aA][lL][lL][yY] *) token env tok

let map_pat_f398476 (env : env) (tok : CST.pat_f398476) =
  (* pattern xor|XOR *) token env tok

let map_pat_b91d208 (env : env) (tok : CST.pat_b91d208) =
  (* pattern [^\s<][^<]* *) token env tok

let map_pat_final (env : env) (tok : CST.pat_final) =
  (* pattern [fF][iI][nN][aA][lL] *) token env tok

let map_pat_try (env : env) (tok : CST.pat_try) =
  (* pattern [tT][rR][yY] *) token env tok

let map_pat_extends (env : env) (tok : CST.pat_extends) =
  (* pattern [eE][xX][tT][eE][nN][dD][sS] *) token env tok

let map_pat_switch (env : env) (tok : CST.pat_switch) =
  (* pattern [sS][wW][iI][tT][cC][hH] *) token env tok

let map_pat_inte (env : env) (tok : CST.pat_inte) =
  (* pattern [iI][nN][tT][eE][rR][fF][aA][cC][eE] *) token env tok

let map_pat_match (env : env) (tok : CST.pat_match) =
  (* pattern [mM][aA][tT][cC][hH] *) token env tok

let map_pat_echo (env : env) (tok : CST.pat_echo) =
  (* pattern [eE][cC][hH][oO] *) token env tok

let map_pat_incl_once (env : env) (tok : CST.pat_incl_once) =
  (* pattern [iI][nN][cC][lL][uU][dD][eE][__][oO][nN][cC][eE] *) token env tok

let map_pat_endfor (env : env) (tok : CST.pat_endfor) =
  (* pattern [eE][nN][dD][fF][oO][rR] *) token env tok

let map_pat_func (env : env) (tok : CST.pat_func) =
  (* pattern [fF][uU][nN][cC][tT][iI][oO][nN] *) token env tok

let map_pat_e0610ac (env : env) (tok : CST.pat_e0610ac) =
  (* pattern and|AND *) token env tok

let map_pat_inst_ (env : env) (tok : CST.pat_inst_) =
  (* pattern [iI][nN][sS][tT][aA][nN][cC][eE][oO][fF] *) token env tok

let map_pat_cont (env : env) (tok : CST.pat_cont) =
  (* pattern [cC][oO][nN][tT][iI][nN][uU][eE] *) token env tok

let map_primitive_type (env : env) (x : CST.primitive_type) =
  (match x with
  | `Array tok -> R.Case ("Array",
      (* "array" *) token env tok
    )
  | `Call tok -> R.Case ("Call",
      (* "callable" *) token env tok
    )
  | `Iter tok -> R.Case ("Iter",
      (* "iterable" *) token env tok
    )
  | `Bool tok -> R.Case ("Bool",
      (* "bool" *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* "float" *) token env tok
    )
  | `Int tok -> R.Case ("Int",
      (* "int" *) token env tok
    )
  | `Str tok -> R.Case ("Str",
      (* "string" *) token env tok
    )
  | `Void tok -> R.Case ("Void",
      (* "void" *) token env tok
    )
  | `Mixed tok -> R.Case ("Mixed",
      (* "mixed" *) token env tok
    )
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `Null tok -> R.Case ("Null",
      (* "null" *) token env tok
    )
  )

let map_pat_inst (env : env) (tok : CST.pat_inst) =
  (* pattern [iI][nN][sS][tT][eE][aA][dD][oO][fF] *) token env tok

let map_null (env : env) (tok : CST.null) =
  (* pattern [nN][uU][lL][lL] *) token env tok

let map_pat_as (env : env) (tok : CST.pat_as) =
  (* pattern [aA][sS] *) token env tok

let map_pat_fore (env : env) (tok : CST.pat_fore) =
  (* pattern [fF][oO][rR][eE][aA][cC][hH] *) token env tok

let map_pat_catch (env : env) (tok : CST.pat_catch) =
  (* pattern [cC][aA][tT][cC][hH] *) token env tok

let map_cast_type (env : env) (x : CST.cast_type) =
  (match x with
  | `Array tok -> R.Case ("Array",
      (* "array" *) token env tok
    )
  | `Bin tok -> R.Case ("Bin",
      (* "binary" *) token env tok
    )
  | `Bool_c506ff1 tok -> R.Case ("Bool_c506ff1",
      (* "bool" *) token env tok
    )
  | `Bool_84e2c64 tok -> R.Case ("Bool_84e2c64",
      (* "boolean" *) token env tok
    )
  | `Double tok -> R.Case ("Double",
      (* "double" *) token env tok
    )
  | `Int_fa7153f tok -> R.Case ("Int_fa7153f",
      (* "int" *) token env tok
    )
  | `Int_157db7d tok -> R.Case ("Int_157db7d",
      (* "integer" *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* "float" *) token env tok
    )
  | `Obj tok -> R.Case ("Obj",
      (* "object" *) token env tok
    )
  | `Real tok -> R.Case ("Real",
      (* "real" *) token env tok
    )
  | `Str tok -> R.Case ("Str",
      (* "string" *) token env tok
    )
  | `Unset tok -> R.Case ("Unset",
      (* "unset" *) token env tok
    )
  )

let map_pat_class (env : env) (tok : CST.pat_class) =
  (* pattern [cC][lL][aA][sS][sS] *) token env tok

let map_pat_do (env : env) (tok : CST.pat_do) =
  (* pattern [dD][oO] *) token env tok

let map_pat_brk (env : env) (tok : CST.pat_brk) =
  (* pattern [bB][rR][eE][aA][kK] *) token env tok

let map_pat_global (env : env) (tok : CST.pat_global) =
  (* pattern [gG][lL][oO][bB][aA][lL] *) token env tok

let map_pat_static (env : env) (tok : CST.pat_static) =
  (* pattern [sS][tT][aA][tT][iI][cC] *) token env tok

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  (* automatic_semicolon *) token env tok

let map_pat_use (env : env) (tok : CST.pat_use) =
  (* pattern [uU][sS][eE] *) token env tok

let map_pat_48a4c46 (env : env) (tok : CST.pat_48a4c46) =
  (* pattern or|OR *) token env tok

let map_pat_const (env : env) (tok : CST.pat_const) =
  (* pattern [cC][oO][nN][sS][tT] *) token env tok

let map_pat_trait (env : env) (tok : CST.pat_trait) =
  (* pattern [tT][rR][aA][iI][tT] *) token env tok

let map_anon_choice_COLON_5102e09 (env : env) (x : CST.anon_choice_COLON_5102e09) =
  (match x with
  | `COLON tok -> R.Case ("COLON",
      (* ":" *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_integer (env : env) (tok : CST.integer) =
  (* integer *) token env tok

let map_named_label_statement (env : env) ((v1, v2) : CST.named_label_statement) =
  let v1 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v1
  in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [v1; v2]

let map_variable_name (env : env) ((v1, v2) : CST.variable_name) =
  let v1 = (* "$" *) token env v1 in
  let v2 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
  in
  R.Tuple [v1; v2]

let map_namespace_name (env : env) ((v1, v2) : CST.namespace_name) =
  let v1 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "\\" *) token env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_visibility_modifier (env : env) (x : CST.visibility_modifier) =
  (match x with
  | `Pat_public x -> R.Case ("Pat_public",
      map_pat_public env x
    )
  | `Pat_prot x -> R.Case ("Pat_prot",
      map_pat_prot env x
    )
  | `Pat_priv x -> R.Case ("Pat_priv",
      map_pat_priv env x
    )
  )

let map_abstract_modifier (env : env) (x : CST.abstract_modifier) =
  map_pat_abst env x

let map_string__ (env : env) (x : CST.string__) =
  (match x with
  | `Str tok -> R.Case ("Str",
      (* string *) token env tok
    )
  | `Here tok -> R.Case ("Here",
      (* heredoc *) token env tok
    )
  )

let map_text (env : env) (xs : CST.text) =
  R.List (List.map (fun x ->
    (match x with
    | `Tok_prec_n1_pat_524a507 x -> R.Case ("Tok_prec_n1_pat_524a507",
        map_tok_prec_n1_pat_524a507 env x
      )
    | `Pat_b91d208 x -> R.Case ("Pat_b91d208",
        map_pat_b91d208 env x
      )
    )
  ) xs)

let map_final_modifier (env : env) (x : CST.final_modifier) =
  map_pat_final env x

let map_namespace_aliasing_clause (env : env) ((v1, v2) : CST.namespace_aliasing_clause) =
  let v1 = map_pat_as env v1 in
  let v2 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
  in
  R.Tuple [v1; v2]

let map_static_modifier (env : env) (x : CST.static_modifier) =
  map_pat_static env x

let map_semicolon (env : env) (x : CST.semicolon) =
  (match x with
  | `Auto_semi tok -> R.Case ("Auto_semi",
      (* automatic_semicolon *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_anon_choice_pat_func_6731ab8 (env : env) (x : CST.anon_choice_pat_func_6731ab8) =
  (match x with
  | `Pat_func x -> R.Case ("Pat_func",
      map_pat_func env x
    )
  | `Pat_const x -> R.Case ("Pat_const",
      map_pat_const env x
    )
  )

let map_anonymous_function_use_clause (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.anonymous_function_use_clause) =
  let v1 = map_pat_use env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "&" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_variable_name env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_variable_name env v3 in
      R.Tuple [v1; v2; v3]
    ) v5)
  in
  let v6 =
    (match v6 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v7 = (* ")" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_namespace_name_as_prefix (env : env) (x : CST.namespace_name_as_prefix) =
  (match x with
  | `BSLASH tok -> R.Case ("BSLASH",
      (* "\\" *) token env tok
    )
  | `Opt_BSLASH_name_name_BSLASH (v1, v2, v3) -> R.Case ("Opt_BSLASH_name_name_BSLASH",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "\\" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_namespace_name env v2 in
      let v3 = (* "\\" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_name_BSLASH (v1, v2) -> R.Case ("Pat_name_BSLASH",
      let v1 = map_pat_name env v1 in
      let v2 = (* "\\" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_name_opt_BSLASH_name_name_BSLASH (v1, v2, v3, v4) -> R.Case ("Pat_name_opt_BSLASH_name_name_BSLASH",
      let v1 = map_pat_name env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "\\" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_namespace_name env v3 in
      let v4 = (* "\\" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* integer *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* pattern \d*(_\d+)*((\.\d*(_\d+)*\
  )?([eE][\+-]?\d+(_\d+)*\
  )|(\.\d\d*(_\d+)*\
  )([eE][\+-]?\d+(_\d+)*\
  )?) *) token env tok
    )
  | `Str_ x -> R.Case ("Str_",
      map_string__ env x
    )
  | `Bool tok -> R.Case ("Bool",
      (* pattern [Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee] *) token env tok
    )
  | `Null tok -> R.Case ("Null",
      (* pattern [nN][uU][lL][lL] *) token env tok
    )
  )

let map_relative_scope (env : env) (x : CST.relative_scope) =
  (match x with
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `Parent tok -> R.Case ("Parent",
      (* "parent" *) token env tok
    )
  | `Pat_static x -> R.Case ("Pat_static",
      map_static_modifier env x
    )
  )

let map_modifier (env : env) (x : CST.modifier) =
  (match x with
  | `Var_modi tok -> R.Case ("Var_modi",
      (* pattern [vV][aA][rR] *) token env tok
    )
  | `Visi_modi x -> R.Case ("Visi_modi",
      map_visibility_modifier env x
    )
  | `Static_modi x -> R.Case ("Static_modi",
      map_static_modifier env x
    )
  | `Final_modi x -> R.Case ("Final_modi",
      map_final_modifier env x
    )
  | `Abst_modi x -> R.Case ("Abst_modi",
      map_abstract_modifier env x
    )
  )

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `Parent tok -> R.Case ("Parent",
      (* "parent" *) token env tok
    )
  | `Pat_static x -> R.Case ("Pat_static",
      map_static_modifier env x
    )
  )

let map_namespace_use_group_clause (env : env) ((v1, v2, v3) : CST.namespace_use_group_clause) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_anon_choice_pat_func_6731ab8 env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_namespace_name env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_namespace_aliasing_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_qualified_name (env : env) ((v1, v2) : CST.qualified_name) =
  let v1 = map_namespace_name_as_prefix env v1 in
  let v2 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
  in
  R.Tuple [v1; v2]

let map_declare_directive (env : env) ((v1, v2, v3) : CST.declare_directive) =
  let v1 =
    (match v1 with
    | `Ticks tok -> R.Case ("Ticks",
        (* "ticks" *) token env tok
      )
    | `Enco tok -> R.Case ("Enco",
        (* "encoding" *) token env tok
      )
    | `Strict_types tok -> R.Case ("Strict_types",
        (* "strict_types" *) token env tok
      )
    )
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_literal env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_name_9dd129a (env : env) (x : CST.anon_choice_name_9dd129a) =
  (match x with
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
    )
  | `Rese_id x -> R.Case ("Rese_id",
      map_reserved_identifier env x
    )
  )

let map_namespace_use_group (env : env) ((v1, v2, v3, v4) : CST.namespace_use_group) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_namespace_use_group_clause env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_namespace_use_group_clause env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_named_type (env : env) (x : CST.named_type) =
  (match x with
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
    )
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  )

let map_anon_choice_name_062e4f2 (env : env) (x : CST.anon_choice_name_062e4f2) =
  (match x with
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
    )
  | `Rese_id x -> R.Case ("Rese_id",
      map_reserved_identifier env x
    )
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  )

let map_type_list (env : env) ((v1, v2) : CST.type_list) =
  let v1 = map_named_type env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_named_type env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_base_clause (env : env) ((v1, v2, v3) : CST.base_clause) =
  let v1 = map_pat_extends env v1 in
  let v2 = map_anon_choice_name_062e4f2 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_name_062e4f2 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_class_interface_clause (env : env) ((v1, v2, v3) : CST.class_interface_clause) =
  let v1 = map_pat_imples env v1 in
  let v2 = map_anon_choice_name_062e4f2 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_name_062e4f2 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_namespace_use_clause (env : env) ((v1, v2) : CST.namespace_use_clause) =
  let v1 = map_anon_choice_name_062e4f2 env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_namespace_aliasing_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_types (env : env) (x : CST.types) =
  (match x with
  | `Opt_type (v1, v2) -> R.Case ("Opt_type",
      let v1 = (* "?" *) token env v1 in
      let v2 =
        (match v2 with
        | `Named_type x -> R.Case ("Named_type",
            map_named_type env x
          )
        | `Prim_type x -> R.Case ("Prim_type",
            map_primitive_type env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Named_type x -> R.Case ("Named_type",
      map_named_type env x
    )
  | `Prim_type x -> R.Case ("Prim_type",
      map_primitive_type env x
    )
  )

let map_union_type (env : env) ((v1, v2) : CST.union_type) =
  let v1 = map_types env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_types env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_type_ (env : env) (x : CST.type_) =
  map_union_type env x

let map_return_type (env : env) ((v1, v2) : CST.return_type) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

let rec map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 (env : env) ((v1, v2) : CST.anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4) =
  let v1 = map_array_element_initializer env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_array_element_initializer env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_choice_array_dest_08f4c18 (env : env) (x : CST.anon_choice_array_dest_08f4c18) =
  (match x with
  | `Array_dest x -> R.Case ("Array_dest",
      map_array_destructing env x
    )
  | `Choice_cast_var x -> R.Case ("Choice_cast_var",
      map_variable env x
    )
  )

and map_anon_choice_case_stmt_f1b35bc (env : env) (x : CST.anon_choice_case_stmt_f1b35bc) =
  (match x with
  | `Case_stmt (v1, v2, v3, v4) -> R.Case ("Case_stmt",
      let v1 = map_pat_case env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_anon_choice_COLON_5102e09 env v3 in
      let v4 = R.List (List.map (map_statement env) v4) in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Defa_stmt (v1, v2, v3) -> R.Case ("Defa_stmt",
      let v1 = map_pat_defa env v1 in
      let v2 = map_anon_choice_COLON_5102e09 env v2 in
      let v3 = R.List (List.map (map_statement env) v3) in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_choice_array_dest_abfb170 (env : env) (x : CST.anon_choice_choice_array_dest_abfb170) =
  (match x with
  | `Choice_array_dest x -> R.Case ("Choice_array_dest",
      map_anon_choice_array_dest_08f4c18 env x
    )
  | `Exp_EQGT_choice_array_dest (v1, v2, v3) -> R.Case ("Exp_EQGT_choice_array_dest",
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_array_dest_08f4c18 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_choice_list_dest_c865322 (env : env) (x : CST.anon_choice_choice_list_dest_c865322) =
  (match x with
  | `Choice_list_dest x -> R.Case ("Choice_list_dest",
      map_anon_choice_list_dest_bb41c20 env x
    )
  | `Exp_EQGT_choice_list_dest (v1, v2, v3) -> R.Case ("Exp_EQGT_choice_list_dest",
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_list_dest_bb41c20 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_class_cst_access_exp_18f5288 (env : env) (x : CST.anon_choice_class_cst_access_exp_18f5288) =
  (match x with
  | `Class_cst_access_exp x -> R.Case ("Class_cst_access_exp",
      map_class_constant_access_expression env x
    )
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
    )
  )

and map_anon_choice_list_dest_bb41c20 (env : env) (x : CST.anon_choice_list_dest_bb41c20) =
  (match x with
  | `List_dest x -> R.Case ("List_dest",
      map_list_destructing env x
    )
  | `Choice_cast_var x -> R.Case ("Choice_cast_var",
      map_variable env x
    )
  )

and map_anon_choice_match_cond_exp_d891119 (env : env) (x : CST.anon_choice_match_cond_exp_d891119) =
  (match x with
  | `Match_cond_exp (v1, v2, v3) -> R.Case ("Match_cond_exp",
      let v1 = map_match_condition_list env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Match_defa_exp (v1, v2, v3) -> R.Case ("Match_defa_exp",
      let v1 = map_pat_defa env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_simple_param_5af5eb3 (env : env) (x : CST.anon_choice_simple_param_5af5eb3) =
  (match x with
  | `Simple_param (v1, v2, v3, v4, v5) -> R.Case ("Simple_param",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = map_variable_name env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_property_initializer env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Vari_param (v1, v2, v3, v4, v5) -> R.Case ("Vari_param",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "..." *) token env v4 in
      let v5 = map_variable_name env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Prop_prom_param (v1, v2, v3, v4) -> R.Case ("Prop_prom_param",
      let v1 = map_visibility_modifier env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_variable_name env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_property_initializer env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_argument (env : env) ((v1, v2) : CST.argument) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_named_label_statement env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Vari_unpa x -> R.Case ("Vari_unpa",
        map_variadic_unpacking env x
      )
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_arguments (env : env) ((v1, v2, v3, v4) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_argument env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_argument env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_array_creation_expression (env : env) (x : CST.array_creation_expression) =
  (match x with
  | `Array_LPAR_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Array_LPAR_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RPAR",
      let v1 = (* "array" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `LBRACK_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RBRACK (v1, v2, v3, v4) -> R.Case ("LBRACK_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_array_destructing (env : env) ((v1, v2, v3, v4) : CST.array_destructing) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_choice_array_dest_abfb170 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_choice_array_dest_abfb170 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_array_element_initializer (env : env) (x : CST.array_element_initializer) =
  (match x with
  | `Opt_AMP_exp (v1, v2) -> R.Case ("Opt_AMP_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_EQGT_opt_AMP_exp (v1, v2, v3, v4) -> R.Case ("Exp_EQGT_opt_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Vari_unpa x -> R.Case ("Vari_unpa",
      map_variadic_unpacking env x
    )
  )

and map_attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = map_anon_choice_name_062e4f2 env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_arguments env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_attribute_list (env : env) (xs : CST.attribute_list) =
  R.List (List.map (fun (v1, v2, v3, v4) ->
    let v1 = (* "#[" *) token env v1 in
    let v2 = map_attribute env v2 in
    let v3 =
      R.List (List.map (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_attribute env v2 in
        R.Tuple [v1; v2]
      ) v3)
    in
    let v4 = (* "]" *) token env v4 in
    R.Tuple [v1; v2; v3; v4]
  ) xs)

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Un_exp_pat_inst__choice_qual_name (v1, v2, v3) -> R.Case ("Un_exp_pat_inst__choice_qual_name",
      let v1 = map_unary_expression env v1 in
      let v2 = map_pat_inst_ env v2 in
      let v3 = map_class_type_designator env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_QMARKQMARK_exp (v1, v2, v3) -> R.Case ("Exp_QMARKQMARK_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_pat_e0610ac_exp (v1, v2, v3) -> R.Case ("Exp_pat_e0610ac_exp",
      let v1 = map_expression env v1 in
      let v2 = map_pat_e0610ac env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_pat_48a4c46_exp (v1, v2, v3) -> R.Case ("Exp_pat_48a4c46_exp",
      let v1 = map_expression env v1 in
      let v2 = map_pat_48a4c46 env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_pat_f398476_exp (v1, v2, v3) -> R.Case ("Exp_pat_f398476_exp",
      let v1 = map_expression env v1 in
      let v2 = map_pat_f398476 env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTGT_exp (v1, v2, v3) -> R.Case ("Exp_LTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQGT_exp (v1, v2, v3) -> R.Case ("Exp_LTEQGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DOT_exp (v1, v2, v3) -> R.Case ("Exp_DOT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_callable_expression (env : env) (x : CST.callable_expression) =
  (match x with
  | `Choice_choice_dyna_var_name x -> R.Case ("Choice_choice_dyna_var_name",
      map_callable_variable env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Array_crea_exp x -> R.Case ("Array_crea_exp",
      map_array_creation_expression env x
    )
  | `Str_ x -> R.Case ("Str_",
      map_string__ env x
    )
  )

and map_callable_variable (env : env) (x : CST.callable_variable) =
  (match x with
  | `Choice_dyna_var_name x -> R.Case ("Choice_dyna_var_name",
      map_variable_name_ env x
    )
  | `Subs_exp x -> R.Case ("Subs_exp",
      map_subscript_expression env x
    )
  | `Member_call_exp (v1, v2, v3, v4) -> R.Case ("Member_call_exp",
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Null_member_call_exp (v1, v2, v3, v4) -> R.Case ("Null_member_call_exp",
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "?->" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Scoped_call_exp (v1, v2, v3, v4) -> R.Case ("Scoped_call_exp",
      let v1 = map_scope_resolution_qualifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_call_exp (v1, v2) -> R.Case ("Func_call_exp",
      let v1 =
        (match v1 with
        | `Name tok -> R.Case ("Name",
            (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
          )
        | `Rese_id x -> R.Case ("Rese_id",
            map_reserved_identifier env x
          )
        | `Qual_name x -> R.Case ("Qual_name",
            map_qualified_name env x
          )
        | `Choice_choice_choice_dyna_var_name x -> R.Case ("Choice_choice_choice_dyna_var_name",
            map_callable_expression env x
          )
        )
      in
      let v2 = map_arguments env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_catch_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.catch_clause) =
  let v1 = map_pat_catch env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_list env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_variable_name env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  let v6 = map_compound_statement env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_class_constant_access_expression (env : env) ((v1, v2, v3) : CST.class_constant_access_expression) =
  let v1 = map_scope_resolution_qualifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_anon_choice_name_9dd129a env v3 in
  R.Tuple [v1; v2; v3]

and map_class_type_designator (env : env) (x : CST.class_type_designator) =
  (match x with
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
    )
  | `Rese_id x -> R.Case ("Rese_id",
      map_reserved_identifier env x
    )
  | `Subs_exp x -> R.Case ("Subs_exp",
      map_subscript_expression env x
    )
  | `Member_access_exp x -> R.Case ("Member_access_exp",
      map_member_access_expression env x
    )
  | `Null_member_access_exp x -> R.Case ("Null_member_access_exp",
      map_nullsafe_member_access_expression env x
    )
  | `Scoped_prop_access_exp x -> R.Case ("Scoped_prop_access_exp",
      map_scoped_property_access_expression env x
    )
  | `Choice_dyna_var_name x -> R.Case ("Choice_dyna_var_name",
      map_variable_name_ env x
    )
  )

and map_clone_expression (env : env) ((v1, v2) : CST.clone_expression) =
  let v1 = (* "clone" *) token env v1 in
  let v2 = map_primary_expression env v2 in
  R.Tuple [v1; v2]

and map_colon_block (env : env) ((v1, v2) : CST.colon_block) =
  let v1 = (* ":" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  R.Tuple [v1; v2]

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_const_declaration (env : env) (x : CST.const_declaration) =
  map_const_declaration_ env x

and map_const_declaration_ (env : env) ((v1, v2, v3, v4, v5) : CST.const_declaration_) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_pat_const env v2 in
  let v3 = map_const_element env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_const_element env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 = map_semicolon env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_const_element (env : env) ((v1, v2, v3) : CST.const_element) =
  let v1 = map_anon_choice_name_9dd129a env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_member_declaration env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_dereferencable_expression (env : env) (x : CST.dereferencable_expression) =
  (match x with
  | `Choice_cast_var x -> R.Case ("Choice_cast_var",
      map_variable env x
    )
  | `Class_cst_access_exp x -> R.Case ("Class_cst_access_exp",
      map_class_constant_access_expression env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Array_crea_exp x -> R.Case ("Array_crea_exp",
      map_array_creation_expression env x
    )
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
    )
  | `Rese_id x -> R.Case ("Rese_id",
      map_reserved_identifier env x
    )
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  | `Str_ x -> R.Case ("Str_",
      map_string__ env x
    )
  )

and map_dynamic_variable_name (env : env) (x : CST.dynamic_variable_name) =
  (match x with
  | `DOLLAR_choice_dyna_var_name (v1, v2) -> R.Case ("DOLLAR_choice_dyna_var_name",
      let v1 = (* "$" *) token env v1 in
      let v2 = map_variable_name_ env v2 in
      R.Tuple [v1; v2]
    )
  | `DOLLAR_LCURL_exp_RCURL (v1, v2, v3, v4) -> R.Case ("DOLLAR_LCURL_exp_RCURL",
      let v1 = (* "$" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = map_pat_else env v1 in
  let v2 = map_statement env v2 in
  R.Tuple [v1; v2]

and map_else_clause_2 (env : env) ((v1, v2) : CST.else_clause_2) =
  let v1 = map_pat_else env v1 in
  let v2 = map_colon_block env v2 in
  R.Tuple [v1; v2]

and map_else_if_clause (env : env) ((v1, v2, v3) : CST.else_if_clause) =
  let v1 = map_pat_elseif env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_else_if_clause_2 (env : env) ((v1, v2, v3) : CST.else_if_clause_2) =
  let v1 = map_pat_elseif env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_colon_block env v3 in
  R.Tuple [v1; v2; v3]

and map_enum_declaration_list (env : env) ((v1, v2, v3) : CST.enum_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_enum_member_declaration env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_enum_member_declaration (env : env) (x : CST.enum_member_declaration) =
  (match x with
  | `Enum_case (v1, v2, v3, v4, v5) -> R.Case ("Enum_case",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "case" *) token env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v3
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 =
              (match v2 with
              | `Str tok -> R.Case ("Str",
                  (* string *) token env tok
                )
              | `Int tok -> R.Case ("Int",
                  (* integer *) token env tok
                )
              )
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = map_semicolon env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Meth_decl x -> R.Case ("Meth_decl",
      map_method_declaration env x
    )
  | `Use_decl x -> R.Case ("Use_decl",
      map_use_declaration env x
    )
  )

and map_exponentiation_expression (env : env) ((v1, v2, v3) : CST.exponentiation_expression) =
  let v1 =
    (match v1 with
    | `Clone_exp x -> R.Case ("Clone_exp",
        map_clone_expression env x
      )
    | `Prim_exp x -> R.Case ("Prim_exp",
        map_primary_expression env x
      )
    )
  in
  let v2 = (* "**" *) token env v2 in
  let v3 =
    (match v3 with
    | `Expo_exp x -> R.Case ("Expo_exp",
        map_exponentiation_expression env x
      )
    | `Clone_exp x -> R.Case ("Clone_exp",
        map_clone_expression env x
      )
    | `Prim_exp x -> R.Case ("Prim_exp",
        map_primary_expression env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Cond_exp (v1, v2, v3, v4, v5) -> R.Case ("Cond_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Match_exp (v1, v2, v3) -> R.Case ("Match_exp",
      let v1 = map_pat_match env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_match_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Augm_assign_exp (v1, v2, v3) -> R.Case ("Augm_assign_exp",
      let v1 = map_variable env v1 in
      let v2 =
        (match v2 with
        | `STARSTAREQ tok -> R.Case ("STARSTAREQ",
            (* "**=" *) token env tok
          )
        | `STAREQ tok -> R.Case ("STAREQ",
            (* "*=" *) token env tok
          )
        | `SLASHEQ tok -> R.Case ("SLASHEQ",
            (* "/=" *) token env tok
          )
        | `PERCEQ tok -> R.Case ("PERCEQ",
            (* "%=" *) token env tok
          )
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        | `DASHEQ tok -> R.Case ("DASHEQ",
            (* "-=" *) token env tok
          )
        | `DOTEQ tok -> R.Case ("DOTEQ",
            (* ".=" *) token env tok
          )
        | `LTLTEQ tok -> R.Case ("LTLTEQ",
            (* "<<=" *) token env tok
          )
        | `GTGTEQ tok -> R.Case ("GTGTEQ",
            (* ">>=" *) token env tok
          )
        | `AMPEQ tok -> R.Case ("AMPEQ",
            (* "&=" *) token env tok
          )
        | `HATEQ tok -> R.Case ("HATEQ",
            (* "^=" *) token env tok
          )
        | `BAREQ tok -> R.Case ("BAREQ",
            (* "|=" *) token env tok
          )
        | `QMARKQMARKEQ tok -> R.Case ("QMARKQMARKEQ",
            (* "??=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Assign_exp (v1, v2, v3, v4) -> R.Case ("Assign_exp",
      let v1 =
        (match v1 with
        | `Choice_cast_var x -> R.Case ("Choice_cast_var",
            map_variable env x
          )
        | `List_lit x -> R.Case ("List_lit",
            map_list_literal env x
          )
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Yield_exp (v1, v2) -> R.Case ("Yield_exp",
      let v1 = (* "yield" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Array_elem_init x -> R.Case ("Array_elem_init",
                map_array_element_initializer env x
              )
            | `From_exp (v1, v2) -> R.Case ("From_exp",
                let v1 = (* "from" *) token env v1 in
                let v2 = map_expression env v2 in
                R.Tuple [v1; v2]
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Incl_exp (v1, v2) -> R.Case ("Incl_exp",
      let v1 = map_pat_incl env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Incl_once_exp (v1, v2) -> R.Case ("Incl_once_exp",
      let v1 = map_pat_incl_once env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Requ_exp (v1, v2) -> R.Case ("Requ_exp",
      let v1 = map_pat_requ env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Requ_once_exp (v1, v2) -> R.Case ("Requ_once_exp",
      let v1 = map_pat_requ_once env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_expressions (env : env) (x : CST.expressions) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Seq_exp x -> R.Case ("Seq_exp",
      map_sequence_expression env x
    )
  )

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = map_pat_fina env v1 in
  let v2 = map_compound_statement env v2 in
  R.Tuple [v1; v2]

and map_foreach_pair (env : env) ((v1, v2, v3) : CST.foreach_pair) =
  let v1 = map_expression env v1 in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_foreach_value env v3 in
  R.Tuple [v1; v2; v3]

and map_foreach_value (env : env) (x : CST.foreach_value) =
  (match x with
  | `Opt_AMP_exp (v1, v2) -> R.Case ("Opt_AMP_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `List_lit x -> R.Case ("List_lit",
      map_list_literal env x
    )
  )

and map_formal_parameters (env : env) ((v1, v2, v3, v4) : CST.formal_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_simple_param_5af5eb3 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_simple_param_5af5eb3 env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_function_definition_header (env : env) ((v1, v2, v3, v4, v5) : CST.function_definition_header) =
  let v1 = map_pat_func env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "&" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_choice_name_9dd129a env v3 in
  let v4 = map_formal_parameters env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_return_type env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_list_destructing (env : env) ((v1, v2, v3, v4, v5) : CST.list_destructing) =
  let v1 = (* "list" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_choice_list_dest_c865322 env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_choice_list_dest_c865322 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_list_literal (env : env) (x : CST.list_literal) =
  (match x with
  | `List_dest x -> R.Case ("List_dest",
      map_list_destructing env x
    )
  | `Array_dest x -> R.Case ("Array_dest",
      map_array_destructing env x
    )
  )

and map_match_block (env : env) ((v1, v2, v3, v4, v5) : CST.match_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_anon_choice_match_cond_exp_d891119 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_match_cond_exp_d891119 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_match_condition_list (env : env) ((v1, v2) : CST.match_condition_list) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_member_access_expression (env : env) ((v1, v2, v3) : CST.member_access_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 = (* "->" *) token env v2 in
  let v3 = map_member_name env v3 in
  R.Tuple [v1; v2; v3]

and map_member_declaration (env : env) (x : CST.member_declaration) =
  (match x with
  | `Class_const_decl (v1, v2, v3) -> R.Case ("Class_const_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_final_modifier env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_const_declaration env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prop_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Prop_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_property_element env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_property_element env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 = map_semicolon env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Meth_decl x -> R.Case ("Meth_decl",
      map_method_declaration env x
    )
  | `Use_decl x -> R.Case ("Use_decl",
      map_use_declaration env x
    )
  )

and map_member_name (env : env) (x : CST.member_name) =
  (match x with
  | `Choice_rese_id x -> R.Case ("Choice_rese_id",
      (match x with
      | `Rese_id x -> R.Case ("Rese_id",
          map_reserved_identifier env x
        )
      | `Name tok -> R.Case ("Name",
          (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
        )
      | `Choice_dyna_var_name x -> R.Case ("Choice_dyna_var_name",
          map_variable_name_ env x
        )
      )
    )
  | `LCURL_exp_RCURL (v1, v2, v3) -> R.Case ("LCURL_exp_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_method_declaration (env : env) ((v1, v2, v3, v4) : CST.method_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_list env x
      ))
    | None -> R.Option None)
  in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = map_function_definition_header env v3 in
  let v4 =
    (match v4 with
    | `Comp_stmt x -> R.Case ("Comp_stmt",
        map_compound_statement env x
      )
    | `Choice_auto_semi x -> R.Case ("Choice_auto_semi",
        map_semicolon env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_nullsafe_member_access_expression (env : env) ((v1, v2, v3) : CST.nullsafe_member_access_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 = (* "?->" *) token env v2 in
  let v3 = map_member_name env v3 in
  R.Tuple [v1; v2; v3]

and map_object_creation_expression (env : env) (x : CST.object_creation_expression) =
  (match x with
  | `New_choice_qual_name_opt_args (v1, v2, v3) -> R.Case ("New_choice_qual_name_opt_args",
      let v1 = (* "new" *) token env v1 in
      let v2 = map_class_type_designator env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_arguments env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `New_pat_class_opt_args_opt_base_clause_opt_class_inte_clause_decl_list (v1, v2, v3, v4, v5, v6) -> R.Case ("New_pat_class_opt_args_opt_base_clause_opt_class_inte_clause_decl_list",
      let v1 = (* "new" *) token env v1 in
      let v2 = map_pat_class env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_arguments env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_base_clause env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_class_interface_clause env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_declaration_list env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Choice_cast_var x -> R.Case ("Choice_cast_var",
      map_variable env x
    )
  | `Choice_int x -> R.Case ("Choice_int",
      map_literal env x
    )
  | `Class_cst_access_exp x -> R.Case ("Class_cst_access_exp",
      map_class_constant_access_expression env x
    )
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
    )
  | `Array_crea_exp x -> R.Case ("Array_crea_exp",
      map_array_creation_expression env x
    )
  | `Print_intr (v1, v2) -> R.Case ("Print_intr",
      let v1 = (* "print" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Anon_func_crea_exp (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Anon_func_crea_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_static_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_pat_func env v2 in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = map_formal_parameters env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anonymous_function_use_clause env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_return_type env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_compound_statement env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Arrow_func (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Arrow_func",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_static_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_pat_fn env v2 in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = map_formal_parameters env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_return_type env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* "=>" *) token env v6 in
      let v7 = map_expression env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Obj_crea_exp x -> R.Case ("Obj_crea_exp",
      map_object_creation_expression env x
    )
  | `Update_exp x -> R.Case ("Update_exp",
      map_update_expression env x
    )
  | `Shell_cmd_exp tok -> R.Case ("Shell_cmd_exp",
      (* shell_command_expression *) token env tok
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Throw_exp (v1, v2) -> R.Case ("Throw_exp",
      let v1 = map_pat_throw env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_property_element (env : env) ((v1, v2) : CST.property_element) =
  let v1 = map_variable_name env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_property_initializer env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_property_initializer (env : env) ((v1, v2) : CST.property_initializer) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_scope_resolution_qualifier (env : env) (x : CST.scope_resolution_qualifier) =
  (match x with
  | `Rela_scope x -> R.Case ("Rela_scope",
      map_relative_scope env x
    )
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
    )
  | `Rese_id x -> R.Case ("Rese_id",
      map_reserved_identifier env x
    )
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  | `Dere_exp x -> R.Case ("Dere_exp",
      map_dereferencable_expression env x
    )
  )

and map_scoped_property_access_expression (env : env) ((v1, v2, v3) : CST.scoped_property_access_expression) =
  let v1 = map_scope_resolution_qualifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_variable_name_ env v3 in
  R.Tuple [v1; v2; v3]

and map_sequence_expression (env : env) ((v1, v2, v3) : CST.sequence_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "," *) token env v2 in
  let v3 =
    (match v3 with
    | `Seq_exp x -> R.Case ("Seq_exp",
        map_sequence_expression env x
      )
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Empty_stmt tok -> R.Case ("Empty_stmt",
      (* ";" *) token env tok
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Named_label_stmt x -> R.Case ("Named_label_stmt",
      map_named_label_statement env x
    )
  | `Exp_stmt (v1, v2) -> R.Case ("Exp_stmt",
      let v1 = map_expression env v1 in
      let v2 = map_semicolon env v2 in
      R.Tuple [v1; v2]
    )
  | `If_stmt (v1, v2, v3) -> R.Case ("If_stmt",
      let v1 = map_pat_if env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 =
        (match v3 with
        | `Choice_empty_stmt_rep_else_if_clause_opt_else_clause (v1, v2, v3) -> R.Case ("Choice_empty_stmt_rep_else_if_clause_opt_else_clause",
            let v1 = map_statement env v1 in
            let v2 = R.List (List.map (map_else_if_clause env) v2) in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_else_clause env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        | `Colon_blk_rep_else_if_clause_2_opt_else_clause_2_pat_endif_choice_auto_semi (v1, v2, v3, v4, v5) -> R.Case ("Colon_blk_rep_else_if_clause_2_opt_else_clause_2_pat_endif_choice_auto_semi",
            let v1 = map_colon_block env v1 in
            let v2 = R.List (List.map (map_else_if_clause_2 env) v2) in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_else_clause_2 env x
                ))
              | None -> R.Option None)
            in
            let v4 = map_pat_endif env v4 in
            let v5 = map_semicolon env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Switch_stmt (v1, v2, v3) -> R.Case ("Switch_stmt",
      let v1 = map_pat_switch env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_switch_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `While_stmt (v1, v2, v3) -> R.Case ("While_stmt",
      let v1 = map_pat_while env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 =
        (match v3 with
        | `Choice_empty_stmt x -> R.Case ("Choice_empty_stmt",
            map_statement env x
          )
        | `Colon_blk_pat_endw_choice_auto_semi (v1, v2, v3) -> R.Case ("Colon_blk_pat_endw_choice_auto_semi",
            let v1 = map_colon_block env v1 in
            let v2 = map_pat_endw env v2 in
            let v3 = map_semicolon env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Do_stmt (v1, v2, v3, v4, v5) -> R.Case ("Do_stmt",
      let v1 = map_pat_do env v1 in
      let v2 = map_statement env v2 in
      let v3 = map_pat_while env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = map_semicolon env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("For_stmt",
      let v1 = map_pat_for env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* ";" *) token env v6 in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      in
      let v8 = (* ")" *) token env v8 in
      let v9 =
        (match v9 with
        | `Choice_auto_semi x -> R.Case ("Choice_auto_semi",
            map_semicolon env x
          )
        | `Choice_empty_stmt x -> R.Case ("Choice_empty_stmt",
            map_statement env x
          )
        | `COLON_rep_choice_empty_stmt_pat_endfor_choice_auto_semi (v1, v2, v3, v4) -> R.Case ("COLON_rep_choice_empty_stmt_pat_endfor_choice_auto_semi",
            let v1 = (* ":" *) token env v1 in
            let v2 = R.List (List.map (map_statement env) v2) in
            let v3 = map_pat_endfor env v3 in
            let v4 = map_semicolon env v4 in
            R.Tuple [v1; v2; v3; v4]
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Fore_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Fore_stmt",
      let v1 = map_pat_fore env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = map_pat_as env v4 in
      let v5 =
        (match v5 with
        | `Fore_pair x -> R.Case ("Fore_pair",
            map_foreach_pair env x
          )
        | `Choice_opt_AMP_exp x -> R.Case ("Choice_opt_AMP_exp",
            map_foreach_value env x
          )
        )
      in
      let v6 = (* ")" *) token env v6 in
      let v7 =
        (match v7 with
        | `Choice_auto_semi x -> R.Case ("Choice_auto_semi",
            map_semicolon env x
          )
        | `Choice_empty_stmt x -> R.Case ("Choice_empty_stmt",
            map_statement env x
          )
        | `Colon_blk_pat_endf_choice_auto_semi (v1, v2, v3) -> R.Case ("Colon_blk_pat_endf_choice_auto_semi",
            let v1 = map_colon_block env v1 in
            let v2 = map_pat_endf env v2 in
            let v3 = map_semicolon env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Goto_stmt (v1, v2, v3) -> R.Case ("Goto_stmt",
      let v1 = map_pat_goto env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cont_stmt (v1, v2, v3) -> R.Case ("Cont_stmt",
      let v1 = map_pat_cont env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Brk_stmt (v1, v2, v3) -> R.Case ("Brk_stmt",
      let v1 = map_pat_brk env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ret_stmt (v1, v2, v3) -> R.Case ("Ret_stmt",
      let v1 = map_pat_ret env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Try_stmt (v1, v2, v3) -> R.Case ("Try_stmt",
      let v1 = map_pat_try env v1 in
      let v2 = map_compound_statement env v2 in
      let v3 =
        R.List (List.map (fun x ->
          (match x with
          | `Catch_clause x -> R.Case ("Catch_clause",
              map_catch_clause env x
            )
          | `Fina_clause x -> R.Case ("Fina_clause",
              map_finally_clause env x
            )
          )
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Decl_stmt (v1, v2, v3, v4, v5) -> R.Case ("Decl_stmt",
      let v1 = (* "declare" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_declare_directive env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        (match v5 with
        | `Choice_empty_stmt x -> R.Case ("Choice_empty_stmt",
            map_statement env x
          )
        | `COLON_rep_choice_empty_stmt_pat_endd_choice_auto_semi (v1, v2, v3, v4) -> R.Case ("COLON_rep_choice_empty_stmt_pat_endd_choice_auto_semi",
            let v1 = (* ":" *) token env v1 in
            let v2 = R.List (List.map (map_statement env) v2) in
            let v3 = map_pat_endd env v3 in
            let v4 = map_semicolon env v4 in
            R.Tuple [v1; v2; v3; v4]
          )
        | `Choice_auto_semi x -> R.Case ("Choice_auto_semi",
            map_semicolon env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Echo_stmt (v1, v2, v3) -> R.Case ("Echo_stmt",
      let v1 = map_pat_echo env v1 in
      let v2 = map_expressions env v2 in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Unset_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("Unset_stmt",
      let v1 = (* "unset" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_variable env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_variable env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = map_semicolon env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Const_decl x -> R.Case ("Const_decl",
      map_const_declaration env x
    )
  | `Func_defi (v1, v2, v3) -> R.Case ("Func_defi",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_function_definition_header env v2 in
      let v3 = map_compound_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Class_decl (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Class_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Final_modi x -> R.Case ("Final_modi",
                map_final_modifier env x
              )
            | `Abst_modi x -> R.Case ("Abst_modi",
                map_abstract_modifier env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 = map_pat_class env v3 in
      let v4 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v4
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_base_clause env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_class_interface_clause env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_declaration_list env v7 in
      let v8 =
        (match v8 with
        | Some x -> R.Option (Some (
            map_semicolon env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Inte_decl (v1, v2, v3, v4) -> R.Case ("Inte_decl",
      let v1 = map_pat_inte env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_base_clause env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_declaration_list env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Trait_decl (v1, v2, v3) -> R.Case ("Trait_decl",
      let v1 = map_pat_trait env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
      in
      let v3 = map_declaration_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Enum_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Enum_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_pat_enum env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v3
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_return_type env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_class_interface_clause env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_enum_declaration_list env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Name_defi (v1, v2) -> R.Case ("Name_defi",
      let v1 = map_pat_name env v1 in
      let v2 =
        (match v2 with
        | `Name_name_choice_auto_semi (v1, v2) -> R.Case ("Name_name_choice_auto_semi",
            let v1 = map_namespace_name env v1 in
            let v2 = map_semicolon env v2 in
            R.Tuple [v1; v2]
          )
        | `Opt_name_name_comp_stmt (v1, v2) -> R.Case ("Opt_name_name_comp_stmt",
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_namespace_name env x
                ))
              | None -> R.Option None)
            in
            let v2 = map_compound_statement env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Name_use_decl (v1, v2, v3, v4) -> R.Case ("Name_use_decl",
      let v1 = map_pat_use env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_pat_func_6731ab8 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Name_use_clause_rep_COMMA_name_use_clause (v1, v2) -> R.Case ("Name_use_clause_rep_COMMA_name_use_clause",
            let v1 = map_namespace_use_clause env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_namespace_use_clause env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          )
        | `Opt_BSLASH_name_name_BSLASH_name_use_group (v1, v2, v3, v4) -> R.Case ("Opt_BSLASH_name_name_BSLASH_name_use_group",
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "\\" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = map_namespace_name env v2 in
            let v3 = (* "\\" *) token env v3 in
            let v4 = map_namespace_use_group env v4 in
            R.Tuple [v1; v2; v3; v4]
          )
        )
      in
      let v4 = map_semicolon env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Global_decl (v1, v2, v3, v4) -> R.Case ("Global_decl",
      let v1 = map_pat_global env v1 in
      let v2 = map_variable_name_ env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_variable_name_ env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = map_semicolon env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_static_decl (v1, v2, v3, v4) -> R.Case ("Func_static_decl",
      let v1 = map_static_modifier env v1 in
      let v2 = map_static_variable_declaration env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_static_variable_declaration env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = map_semicolon env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_static_variable_declaration (env : env) ((v1, v2) : CST.static_variable_declaration) =
  let v1 = map_variable_name env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_property_initializer env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_subscript_expression (env : env) ((v1, v2) : CST.subscript_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 =
    (match v2 with
    | `LBRACK_opt_exp_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_opt_exp_RBRACK",
        let v1 = (* "[" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_expression env x
            ))
          | None -> R.Option None)
        in
        let v3 = (* "]" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `LCURL_exp_RCURL (v1, v2, v3) -> R.Case ("LCURL_exp_RCURL",
        let v1 = (* "{" *) token env v1 in
        let v2 = map_expression env v2 in
        let v3 = (* "}" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  R.Tuple [v1; v2]

and map_switch_block (env : env) (x : CST.switch_block) =
  (match x with
  | `LCURL_rep_choice_case_stmt_RCURL (v1, v2, v3) -> R.Case ("LCURL_rep_choice_case_stmt_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_case_stmt_f1b35bc env) v2)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `COLON_rep_choice_case_stmt_pat_ends_choice_auto_semi (v1, v2, v3, v4) -> R.Case ("COLON_rep_choice_case_stmt_pat_ends_choice_auto_semi",
      let v1 = (* ":" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_case_stmt_f1b35bc env) v2)
      in
      let v3 = map_pat_ends env v3 in
      let v4 = map_semicolon env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Clone_exp x -> R.Case ("Clone_exp",
      map_clone_expression env x
    )
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  | `Expo_exp x -> R.Case ("Expo_exp",
      map_exponentiation_expression env x
    )
  | `Un_op_exp x -> R.Case ("Un_op_exp",
      map_unary_op_expression env x
    )
  | `Cast_exp (v1, v2, v3, v4) -> R.Case ("Cast_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_cast_type env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_unary_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_unary_op_expression (env : env) (x : CST.unary_op_expression) =
  (match x with
  | `AT_exp (v1, v2) -> R.Case ("AT_exp",
      let v1 = (* "@" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_PLUS_exp (v1, v2) -> R.Case ("Choice_PLUS_exp",
      let v1 =
        (match v1 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `TILDE tok -> R.Case ("TILDE",
            (* "~" *) token env tok
          )
        | `BANG tok -> R.Case ("BANG",
            (* "!" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Choice_cast_var_PLUSPLUS (v1, v2) -> R.Case ("Choice_cast_var_PLUSPLUS",
      let v1 = map_variable env v1 in
      let v2 = (* "++" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_cast_var_DASHDASH (v1, v2) -> R.Case ("Choice_cast_var_DASHDASH",
      let v1 = map_variable env v1 in
      let v2 = (* "--" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `PLUSPLUS_choice_cast_var (v1, v2) -> R.Case ("PLUSPLUS_choice_cast_var",
      let v1 = (* "++" *) token env v1 in
      let v2 = map_variable env v2 in
      R.Tuple [v1; v2]
    )
  | `DASHDASH_choice_cast_var (v1, v2) -> R.Case ("DASHDASH_choice_cast_var",
      let v1 = (* "--" *) token env v1 in
      let v2 = map_variable env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_use_as_clause (env : env) ((v1, v2, v3) : CST.use_as_clause) =
  let v1 =
    map_anon_choice_class_cst_access_exp_18f5288 env v1
  in
  let v2 = map_pat_as env v2 in
  let v3 =
    (match v3 with
    | `Opt_visi_modi_name (v1, v2) -> R.Case ("Opt_visi_modi_name",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_visibility_modifier env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v2
        in
        R.Tuple [v1; v2]
      )
    | `Visi_modi_opt_name (v1, v2) -> R.Case ("Visi_modi_opt_name",
        let v1 = map_visibility_modifier env v1 in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_use_declaration (env : env) ((v1, v2, v3, v4) : CST.use_declaration) =
  let v1 = map_pat_use env v1 in
  let v2 = map_anon_choice_name_062e4f2 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_name_062e4f2 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | `Use_list x -> R.Case ("Use_list",
        map_use_list env x
      )
    | `Choice_auto_semi x -> R.Case ("Choice_auto_semi",
        map_semicolon env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_use_instead_of_clause (env : env) ((v1, v2, v3) : CST.use_instead_of_clause) =
  let v1 =
    map_anon_choice_class_cst_access_exp_18f5288 env v1
  in
  let v2 = map_pat_inst env v2 in
  let v3 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v3
  in
  R.Tuple [v1; v2; v3]

and map_use_list (env : env) ((v1, v2, v3) : CST.use_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `Use_inst_of_clause x -> R.Case ("Use_inst_of_clause",
            map_use_instead_of_clause env x
          )
        | `Use_as_clause x -> R.Case ("Use_as_clause",
            map_use_as_clause env x
          )
        )
      in
      let v2 = map_semicolon env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_variable (env : env) (x : CST.variable) =
  (match x with
  | `Cast_var (v1, v2, v3, v4) -> R.Case ("Cast_var",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_cast_type env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_variable env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Choice_choice_dyna_var_name x -> R.Case ("Choice_choice_dyna_var_name",
      map_callable_variable env x
    )
  | `Scoped_prop_access_exp x -> R.Case ("Scoped_prop_access_exp",
      map_scoped_property_access_expression env x
    )
  | `Member_access_exp x -> R.Case ("Member_access_exp",
      map_member_access_expression env x
    )
  | `Null_member_access_exp x -> R.Case ("Null_member_access_exp",
      map_nullsafe_member_access_expression env x
    )
  )

and map_variable_name_ (env : env) (x : CST.variable_name_) =
  (match x with
  | `Dyna_var_name x -> R.Case ("Dyna_var_name",
      map_dynamic_variable_name env x
    )
  | `Var_name x -> R.Case ("Var_name",
      map_variable_name env x
    )
  )

and map_variadic_unpacking (env : env) ((v1, v2) : CST.variadic_unpacking) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_text env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern <\?([pP][hH][pP]|=)? *) token env v1 in
        let v2 = R.List (List.map (map_statement env) v2) in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_text_interpolation (env : env) ((v1, v2, v3) : CST.text_interpolation) =
  let v1 = (* "?>" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_text env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Php_tag tok -> R.Case ("Php_tag",
        (* pattern <\?([pP][hH][pP]|=)? *) token env tok
      )
    | `Eof tok -> R.Case ("Eof",
        (* eof *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

let dump_tree root =
  map_program () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Text_interpolation (_loc, x) -> ("text_interpolation", "text_interpolation", map_text_interpolation env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
