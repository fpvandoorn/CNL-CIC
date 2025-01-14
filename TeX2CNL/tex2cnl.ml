(* types *)

open Type;;


(* string operations *)
let token_to_string = function
  | Natural i -> (string_of_int i)
  | Numeric s -> s
  | Eol -> "\n"
  | Par -> ""
  | Comment -> ""
  | Input s -> "\\input{"^s ^"}"
  | Cnlenvdel s -> "\\CnlEnvDelete{"^s^"}"
  | ControlSeq s -> "[\\"^s^"]"
  | BeginSeq s -> "\\begin{"^s^"}"
  | EndSeq s -> "\\end{"^s^"}"
  | BeginCnl -> "\\begin{cnl}"
  | EndCnl -> "\\end{cnl}"
  | Arg i -> "#"^(string_of_int i)
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Dollar -> "$"
  | Display -> "$$"
  | Sub -> "_"
  | Comma -> ","
  | Semi -> ";"
  | FormatEol -> "\\"
  | FormatCol -> "&"
  | Label s -> s
  | Tok s -> s
  | Error s -> "[TeX2CnlError \"" ^s^ "\"]"
  | Warn s -> "[TeX2CnlWarning \"" ^s^ "\"]"
  | Eof -> "EOF"
  | Ignore -> ""
  | NotImplemented _ -> "NotImplemented"
  | _ -> "";;


let token_to_verbose_string = function
  | Natural i -> "Nat"^(string_of_int i)
  | Numeric s -> "Num"^s
  | Eol -> "Eol"
  | Par -> "Par"
  | Comment -> "Comment"
  | Input s -> "Input-"^s
  | Cnlenvdel s -> "Cnlenvdel-"^s
  | ControlSeq s -> "ControlSeq-"^s
  | BeginSeq s -> "BeginSeq-"^s
  | EndSeq s -> "EndSeq-"^s
  | BeginCnl -> "BeginCnl"
  | EndCnl -> "EndCnl"
  | Arg i -> "Arg-"^(string_of_int i)
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LBrack -> "LBrack"
  | RBrack -> "RBrack"
  | LBrace -> "LBrace"
  | RBrace -> "RBrace"
  | LDisplay -> "LDisplay"
  | RDisplay -> "RDisplay"
  | Dollar -> "Dollar"
  | Display -> "Display"
  | Sub -> "Sub"
  | Comma -> "Comma"
  | Semi -> "Semi"
  | FormatEol -> "FormatEol"
  | FormatCol -> "FormatCol"
  | Label s -> "Label-"^s
  | Tok s -> "Tok-"^s
  | Error s -> "Error-"^s
  | Warn s -> "Warn-"^s
  | Eof -> "Eof"
  | Ignore -> "Ignore"
  | NotImplemented s -> "NotImplemented-"^s;;

let token_to_string_output = function
  | Dollar -> " "
  | Display -> " "
  | Sub -> "\\sb "
  | Eof -> " "
  | BeginCnl -> " "
  | EndCnl -> " "
  | BeginSeq _ -> " "
  | EndSeq _ -> " "
  | Cnlenvdel _ -> " "
  | ControlSeq s -> "\\"^s
  | Input s -> "[read \"" ^(Filename.remove_extension s)^ ".cnl\"]"
  | NotImplemented _ -> "[TeX2CnlError \"not implemented\"]"
  | _ as t -> (token_to_string t)^" ";;

let _ = token_to_string_output (Input "file");;

let string_clean =
  let f c = match c with
    | 'a' .. 'z' -> c
    | 'A' .. 'Z' -> c
    | '0' .. '9' -> c
    | _ -> '_' in
  String.map f;;

let _ = string_clean "a;sldfkj7698769as2!&*()*&)(&:";;

let token_to_clean_string default = function 
  | Natural i -> (string_of_int i)
  | Numeric s -> string_clean s
  | ControlSeq s -> string_clean s
  | Tok s -> string_clean s
  | Label s -> string_clean s
  | Sub -> "_"
  | _ -> default;;

let tokens_to_clean_string toks = 
  let cs = List.map (token_to_clean_string "_") toks in 
  let s = String.concat "_" cs in 
  String.sub s 0 (min 64 (String.length s));; (* debug truncated *)

let tokens_to_string sep toks = 
  let cs = List.map token_to_string toks in
  String.concat sep cs;;

let rec concat_toks =
  function
  | Tok t :: Tok s :: rest -> (concat_toks (Tok (t ^ s) :: rest))
    | [Tok t] -> Tok t
    | _ -> Error "concat_toks must be nonempty token list";;

let debug = true;;

let print_debug_endline s = if debug then print_endline s else ();;
let print_debug s = if debug then print_string s else ();;

let print_debug_token tok = 
  if debug then print_string ((token_to_string_output tok)^" ") else ();;

let mk_label toks = 
  let t = "Label_"^(tokens_to_clean_string toks) in (* force initial letter *)
  Label t ;;

let tokens_to_clean_string_short toks = 
  let cs = List.map (token_to_clean_string "") toks in 
  String.concat "" cs;;

let mk_var toks = 
  Tok ("V__"^(tokens_to_clean_string_short toks));;

let mk_id toks = 
  Tok ("id_"^(tokens_to_clean_string_short toks));;

let _ = mk_var [ControlSeq "alpha";Sub;Natural 3];;
let _ = mk_var [ControlSeq "alpha";Sub;LBrace;Natural 33;RBrace];;

let no_expand = ControlSeq "noexpand";;

let id_fun x = x;;


(* I/O output *)


let mk_outfile s = 
  (Filename.remove_extension s)^".cnl";;

let mk_infile s = 
  (Filename.remove_extension s)^".tex";;

let _ = mk_outfile "myfile.txt";;

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [];;

let mk_iochannels f s = (* f should terminate lines with Eol *)
let infile = mk_infile s in
let outfile = mk_outfile s in
let rs = read_lines infile in 
let toks = List.map f rs in
  {
    infile = infile; 
    outfile = outfile;
    outc = open_out outfile;
    intoks = List.flatten toks;
  };;

let output_token ios tok = 
  let _ = print_debug_token tok in 
  output_string (ios.outc) (token_to_string_output tok);;

let output_token_list ios toks = 
  ignore(List.map (output_token ios) toks);;

let (output_error,get_error_count) =
  let error_limit = 20 in
  let error_count = ref 0 in 
  let o_error ios s = output_token ios (Error s) in
  ((fun ios s ->
        let _ = o_error ios s in
        let _ = error_count := 1 + !error_count in
        if !error_count > error_limit then 
          let _ = o_error ios "error_limit exceeded" in
          failwith "error limit exceeded"  (* debug, close all files *)
        else ()),
  (fun () -> !error_count));;

let output_warning ios s = 
  output_token ios (Warn s);;

(* I/O input, peek, and return *)

(* let stream = ref [];; *)

 (* We preserve line numbers in transforming from input to output. The
   LF is placed in the output the first time encountered. Except I/O
   doesn't exactly preserve lines during macro expansions and other
   operations that return tokens to the input stream. 
  *)

let regard_par = function
  | TrackPar -> true
    | _ -> false;;


let input_ios = 
  let input_1 ios partrack =
    if ios.intoks = [] then Eof else 
      let tok = List.hd ios.intoks in
      let _ = ios.intoks <- List.tl ios.intoks in
      let tok' = if regard_par partrack && (tok = Eol) && not(ios.intoks =[]) && (Eol = List.hd ios.intoks)
               then Par 
               else tok in
      tok' in
  let rec input_rec ios acc k partrack = 
    if (k=0) then List.rev acc
    else 
      match input_1 ios partrack with 
      | Comment -> (input_rec ios acc k partrack) (* no paragraph *)
      | Eol -> (output_token ios Eol; input_rec ios acc k partrack)
      | Par -> (output_token ios Eol; input_rec ios (Par::acc) (k-1) partrack)
      | _ as t -> input_rec ios (t::acc) (k-1) partrack in
  fun ios partrack k -> input_rec ios [] k partrack;;

let input ios b = List.hd(input_ios ios b 1);;

let returnstream = 0;;
let return_input ios ls = 
  let ls' = List.filter (fun t -> not(t = Par)) ls in
  ios.intoks <- ls' @ ios.intoks;;

let input_filter ios b f s = 
  let t = input ios b in
  let s' = ", found "^(token_to_verbose_string t) in
  let _ = f t || (output_error ios (s^s') ; true) in 
  t;;

let input_tok ios b tok = input_filter ios b ((=) tok) 
  ("expected "^token_to_verbose_string tok)

let peek ios b = 
  let tok = input ios b in
  let _ = return_input ios [tok] in
  tok;;

let is_controlseq t = 
  match t with 
  | ControlSeq _ -> true
  | _ -> false;;

let get_csname = function
  | ControlSeq s -> s
  | _ -> "";; (* quiet failure *)

let input_cs ios b = input_filter ios b is_controlseq "control sequence  expected" ;;


(* delimiters {} [] ()  *)

 (* wrappers *)

let paren(toks) = LParen :: toks @ [RParen];;

let brace(toks) = LBrace :: toks @ [RBrace];;

let bracket(toks) = LBrack :: toks @ [RBrack];;

 (*
   Delimiter matching often takes place before macro
   expansion.  We are therefore a strong invariance of delimiter matching;
   delimiters should match no matter how fully expanded the macros might be.
  *)

let left_mate = 
  function 
  | RParen -> LParen
  | RBrace -> LBrace
  | RBrack -> LBrack
  | RDisplay -> LDisplay
  | ControlSeq ")" -> ControlSeq "("
  | ControlSeq "]" -> ControlSeq "["
(*  | EndSeq s -> BeginSeq s . Macros can contain unmatched \begin \end *)
  | _ -> NotImplemented "";;

let check_mate (ldlims,tok) = 
  match ldlims with 
  | t :: ldlims' -> (ldlims',(if t = left_mate tok then [] else [Error "mismatched end group"]))
  | _ -> ldlims,[Error "unexpected end group"];;

let record_level (ldlims,tok) = (* ldlims = left delimiter stack  *)
  match tok with
  | ControlSeq "(" | ControlSeq "[" | LParen | LBrace | LBrack | LDisplay -> 
     (tok :: ldlims,[tok])
  | ControlSeq ")" | ControlSeq "]" | RParen | RBrace | RBrack | RDisplay -> 
     let (ldlims',err) = check_mate (ldlims,tok) in (ldlims', (tok :: err))
  | Dollar -> if (match ldlims with | Dollar :: _ -> true | _ -> false) then 
                (List.tl ldlims,[])
              else (Dollar :: ldlims,[])
  | Display -> if (match ldlims with | Display :: _ -> true | _ -> false) then 
                (List.tl ldlims,[])
              else (Display :: ldlims,[])
  | _ -> (ldlims,[tok]);;

 (*    We read the tokens using an arbitrary read function with state. *)

 (*
let rec leveled_input_until acc ldlims endif =
  let tok = input_() in
  if (endif (ldlims,tok)) then (return_input [tok]; List.rev acc)
  else 
    let (ldlims',tok') = record_level (ldlims,tok) in 
    leveled_input_until (tok' @ acc) ldlims' endif;;
  *)

 (* tr is an arbitrary token transformation.name.
    read pulls tokens from an arbitrary state. 
  *)
let rec leveled_read_until acc read state ldlims tr endif = 
  try (
    let (tok,state') = read state in
    if endif (ldlims,tok) then ([tok],List.rev acc,state)
    else 
      let (ldlims',tok') = record_level (ldlims,tok) in 
      leveled_read_until 
        ((List.map (fun t -> tr(ldlims',t)) tok') @ acc) read state' ldlims' tr endif)
  with _ -> ([],List.rev acc,state);;

 (* 
   The left,right are the delimiters to be matched.

   The ldlims is the nesting of left delimiters.
   The code is organized in such a way that the first left is often already
   encountered when mate_delim is initialized with say ldlims=[LBrace].
 *)


let mate_delim read state right ldlims = 
  let left = left_mate right in
  let (_,toks,state') = leveled_read_until [] read state ldlims snd 
   (fun (ldlims,tok) -> (ldlims = [left] && (tok = right))) in 
  (toks,state');;

let mate_from_list = 
  let f ls = (List.hd ls,List.tl ls) in 
  fun right ls -> let (toks,unused) = mate_delim f ls right [left_mate right] in 
            (toks,unused);;

(* input_matched_brace input balanced expression, output doesn't include delimiters.  *)
let input_to_right ios b right = 
  let _ = input_tok ios b (left_mate right) in 
  fst(mate_delim (fun () -> input ios b, ()) () right [left_mate right]);;

let input_to_right_wo_par ios right = 
  let toks = input_to_right ios TrackPar right in
  let toks' = List.filter (fun t -> not(t = Par)) toks in
  let errormsg = "unexpected par before "^token_to_verbose_string right in
  let error = if List.length toks = List.length toks'
              then []
              else [Error errormsg] in 
  error @ toks' ;;

let rec input_matched_brace_list ios acc k = 
  if (k=0) then List.rev acc else 
    let t = input_to_right_wo_par ios RBrace in 
    input_matched_brace_list ios (t::acc) (k-1);;

let input_brack_num ios =
  match (peek ios TrackPar) with
  | LBrack -> (
    let toks = input_to_right_wo_par ios RBrack in 
    (match toks with 
     | [Natural i] -> i
       | _ -> (output_error ios ("[nat] expected; replacing input "^(tokens_to_string " " toks)^ " with [0]");0))
  )
    | _ -> 0;;


(* tuples *)
 (* convert tuple to list at outermost layer, with respect to () {}:
   (a,b,(c,d),e,\frac{f}{g,h}) --> [a;b;(c,d);e;\frac{f}{g,h}]
 *)

let list_of_tuple insep outsep toks = (* outer brackets not included *)
  let (_,toks',_) = leveled_read_until [] (fun ls -> (List.hd ls,List.tl ls)) toks []
   (fun (ds,tok) -> if (ds = [] && tok = insep) then outsep else tok)
   (fun (_,_) -> false) in (* read until List.hd gives error *)
  toks';;

let list_of_noparen_tuple toks =
  bracket(list_of_tuple Comma Semi toks);;

let curryargs toks = 
  list_of_tuple Comma Ignore toks;;

let _ = list_of_noparen_tuple [LParen;RParen];;
let _ = list_of_noparen_tuple [ControlSeq "A";Comma;ControlSeq "B";Comma;ControlSeq "C"];;
let _ = curryargs [ControlSeq "A";Comma;ControlSeq "B";Comma;ControlSeq "C"];;



(* macro expansion. 
   TeX arg numbering starts at offset 1: #1, #2, etc. 
   args are doubly reversed. 
 *)

let rec macroexpand acc pattern args = 
  match pattern with
  | [] -> List.rev acc
  | (Arg i) :: rest -> macroexpand (List.rev(List.nth args (i-1)) @ acc) rest args
  | t :: rest -> macroexpand (t :: acc) rest args;;

let macrotable = ref [];;

let debug_macro_to_string (s,(i,toks)) = 
 "[macro:("^s^",("^string_of_int i^","^(tokens_to_string "-" toks)^"))]";;

let debug_show_macrotable () = 
  let f = List.map fst !macrotable in
  "<"^(String.concat "+" f)^">";;

let setmacro f = 
  let _ = (macrotable := f :: !macrotable) in
(*  let _ = print_debug (debug_macro_to_string f) in *)
  ();;

let envdeltable = ref []  ;;

let setdelenv f = (envdeltable := f :: !envdeltable);;

let rec opt_assoc ls s = 
  match ls with
  | [] -> None
  | e::tl -> if (s = fst e)  then Some (snd e) else opt_assoc tl s;;

let rec default_assoc ls s = 
  match ls with 
  | [] -> s
  | e::tl -> if (s = fst e) then snd e else default_assoc tl s;;

(* control sequence processing *)

let is_math_font cs_string = 
  (String.length cs_string > 3) && ("math" = String.sub cs_string 0 4);;

let _ = is_math_font "mathfrak";;
let _ = is_math_font "Bbb";;

let nopar ios toks s = 
  let toks' = List.filter (fun t -> not(t = Par)) toks in
  let _ = (List.length toks' = List.length toks) || (output_error ios s; true) in
  toks';;

let process_controlseq ios cs_string =
(*  let _ = print_debug ("[process-cs:"^cs_string^"]") in *)
  match cs_string with
  | "CnlDelete" -> 
      let i = input_brack_num ios in
      let cs = input_cs ios TrackPar in 
      setmacro (get_csname cs,(i,[])); []
  | "CnlNoExpand" -> 
      let i = input_brack_num ios in
      let cs = input_cs ios TrackPar in 
      setmacro (get_csname cs,(i,[no_expand;cs])); []
  | "CnlCustom" | "CnlDef"  ->
      let i = input_brack_num ios in
      let cs = input_cs ios TrackPar in
      let toks = input_to_right_wo_par ios RBrace in 
      setmacro (get_csname cs,(i,toks)); []
  | "CnlError" ->       
      let i = input_brack_num ios in
      let cs = input_cs ios TrackPar in
      let cname = get_csname cs in
      setmacro (cname,(i,[Warn ("prohibited: \\"^cname)])); []
  | "noexpand" -> 
      let cs = input_cs ios TrackPar in [cs]
  | "var" -> [mk_var (input_to_right_wo_par ios RBrace)]
  | "id" -> [mk_id (input_to_right_wo_par ios RBrace)]
  | "list" ->
      let toks = input_to_right_wo_par ios RBrace in 
      (return_input ios (list_of_noparen_tuple toks) ; [])
  | "concat" ->
      let t1 = input_to_right_wo_par ios RBrace in
      let t2 = input_to_right_wo_par ios RBrace in
      [concat_toks (t1 @ t2)]
  | "app" ->
      let ftoks = (input_to_right_wo_par ios RBrace) in 
      let argtoks = (input_to_right_wo_par ios RBrace) in 
      let cargs = nopar ios argtoks "illegal par in cargs" in
      let ls = paren(paren(ftoks) @ cargs) in 
       (return_input ios (ls) ; [])
  | _ -> let otoks = opt_assoc !macrotable cs_string in
         (match otoks with
         | Some (k,pat) -> 
(*             let _ = if (k>0) then print_debug "[expanding macro," else () in  *)
             let args = (input_matched_brace_list ios [] k) in 
             let repl_text = macroexpand [] pat args in 
(*             let _ = if (k>0) then 
                       (print_debug "expanded macro:";
                        ignore(List.map print_debug_token repl_text);
                        print_debug "]") in  *)
             (return_input ios repl_text;[])
         | None ->
             if is_math_font(cs_string) then (* \mathfrak{C} -> C__mathfrak *)
               let toks = input_to_right_wo_par ios RBrace in 
               if (List.length toks != 1) then (return_input ios toks; [])
               else 
                 match List.hd toks with 
                 | Tok s -> [Tok (s^"__"^cs_string)]
                 | _ -> (return_input ios toks; [])
             else [ControlSeq cs_string]);;

(* environments  *) 

 (* move one token to output, raising exception if Eof *)

let transcribe_token ios ifexpand outputif tr tok = 
  let p tok = if outputif tok then output_token ios (default_assoc tr tok) else () in
  let ps toks = output_token_list ios 
                  (List.map (default_assoc tr) (List.filter outputif toks)) in 
  match tok with
  | ControlSeq s -> if ifexpand then ps(process_controlseq ios s) else p tok
  | Arg _ -> output_error ios "# parameters must appear in macro patterns"
 (*  
  | FormatEol -> output_error ios "FormatEol must appear in environment"
  | FormatCol -> output_error ios "FormatCol must appear in environment"
  *)
  | Eol -> output_error ios "unexpected EOL"
  | Eof -> raise End_of_file
  | _ -> p tok;;

let null_env  = 
  {
    name = "NULL_ENV";
    begin_token = Ignore;
    end_token = Ignore;
    tr_token = [];
    drop_toks = [];
    is_delete = false;
    stay_in_par = NoTrackPar;
  };;

let mk_drop_env s drop is_delete = 
  {
    name = s;
    begin_token = Ignore;
    end_token = Ignore;
    tr_token = [];
    drop_toks = drop;
    is_delete = is_delete;
    stay_in_par = NoTrackPar;
  };;


let mk_delete_env s = 
  {
    name = s;
    begin_token = Ignore;
    end_token = Ignore;
    tr_token = [];
    drop_toks = [];
    is_delete = true;
    stay_in_par = NoTrackPar;
  };;

let mk_itemize_env s item is_delete =
  {
    name = s;
    begin_token = LBrace;
    end_token = RBrace;
    tr_token = [(ControlSeq "item",item)];
    drop_toks = [];
    is_delete = is_delete;
    stay_in_par = TrackPar;
  };;

let mk_case_env s is_delete =
  {
    name = s;
    begin_token = Tok "case";
    end_token = Tok "end";
    tr_token = [];
    drop_toks = [FormatCol;FormatEol];
    is_delete = is_delete;
    stay_in_par = TrackPar;
  };;


let mk_eqn_env s is_delete = 
  {
    name = s;
    begin_token = if is_delete then Ignore else Tok "\\eqnarray{[(";
    end_token = if is_delete then Ignore else Tok ")]}";
    tr_token = if is_delete then [] else [(FormatEol,Tok "); (")];
    drop_toks = [FormatCol];
    is_delete = is_delete;
    stay_in_par = TrackPar;
  };;

let mk_matrix_env s is_delete = 
  {
    name = s; 
    begin_token = if is_delete then Ignore else Tok ("\\"^s^"{[[(");
    end_token = if is_delete then Ignore else Tok (")]]}");
    tr_token = (if is_delete then []
               else [(FormatEol,Tok ")]; [(");(FormatCol,Tok "); (")]);
    drop_toks = [];
    is_delete = is_delete;
    stay_in_par = TrackPar;
  };;

let is_prologue_env s =
  match s with
  | "def" | "definition" | "Definition" 
  | "thm" | "theorem" | "Theorem"
  | "prop" | "proposition" | "Proposition"
  | "axiom" | "Axiom"
  | "hyp" | "hypothesis" | "Hypothesis"
  | "cor" | "corollary" | "Corollary" -> true
  | _ -> false;;


let mk_e_item s e =
  if List.mem s (!envdeltable) then mk_delete_env s
  else if is_prologue_env s then mk_drop_env s [] (e.is_delete)
  else 
    match s with
    | "itemize" | "structure" | "make" ->
                                  mk_itemize_env s (Semi) (e.is_delete)
      | "center" -> mk_drop_env s [] (e.is_delete)
      | "envMatch" -> mk_drop_env s [FormatEol;FormatCol] e.is_delete
      | "cases" -> mk_case_env s e.is_delete
      | "flushleft" | "flushright" | "minipage" 
      | "quotation" | "quote" | "verse" -> mk_drop_env s [FormatEol] (e.is_delete)
      | "tabbing" -> mk_drop_env s [FormatEol;FormatCol;ControlSeq "=";ControlSeq ">";ControlSeq "+";ControlSeq "-"] e.is_delete
      | "array" -> mk_matrix_env s (e.is_delete) 
      | "align" | "align*" | "eqnarray" | "eqnarray*" 
      | "gather" | "gather*" | "equation" | "equation*" -> mk_eqn_env s e.is_delete
      | "figure" | "picture" | "remark" 
      | "thebibliography" | "titlepage" -> mk_delete_env s
      | "multline" | "split" -> mk_drop_env s [FormatEol;FormatCol] (e.is_delete)
      | "matrix" | "pmatrix" | "bmatrix" | "Bmatrix" | "vmatrix" | "Vmatrix"
      | "smallmatrix" -> mk_matrix_env s e.is_delete
      | _ -> mk_delete_env s ;;

(* declarations *)
let delete_matching_brack ios b = (* material in [] *)
  let l = peek ios b in
  if (l = LBrack) then 
    ignore(input_to_right ios b RBrack)
  else ();;

let get_label ios = (* read \label *)
 let l = input ios TrackPar in 
 match l with
 | ControlSeq "label" -> 
     let toks = input_to_right_wo_par ios RBrace in Some(mk_label toks)
 | _ -> return_input ios [l]; None;;

let declaration_table = 
  [
    ("def","Definition");
    ("thm","Theorem");
    ("cor","Corollary");
    ("hyp","Hypothesis");
    ("prop","Proposition");
  ];;

let output_decl_prologue ios env_name = 
 let _ = delete_matching_brack ios NoTrackPar in
 let label = get_label ios in 
 let odecl = opt_assoc declaration_table env_name in 
 let s = match odecl with 
   | None -> String.capitalize_ascii env_name
   | Some s' -> s' in
 let _ = output_token ios (Tok s) in
 let _ = match label with | None -> () | Some t -> output_token ios t in
 output_token ios (Tok ".");;

let output_prologue ios s = 
  if is_prologue_env s then output_decl_prologue ios s else ();;

 (*
   output token will be unhandled EndCnl, Input, Eof 
  *)

let rec process_environ ios e_stack =
  let out tok is_delete = if (is_delete) then () else output_token ios tok in 
  let par_filter e = 
    let nrp = not(regard_par e.stay_in_par) in
    let _ = nrp || (output_error ios 
                      ("illegal paragraph ending in environment "^e.name); true) in
    nrp in
  let rec output_err_stack es = 
    if (List.length e_stack <= 1) then () 
    else 
      (output_error ios ("expected \\end{"^((List.hd e_stack).name)^"}"); 
       output_err_stack (List.tl es)) in
  if (e_stack=[]) 
  then (output_error ios "unexpected empty environment. ending cnl envir"; EndCnl) else 
    let e = List.hd e_stack in
    let tok = input ios (e.stay_in_par) in 
    match tok with
    | Cnlenvdel s -> (setdelenv s; process_environ ios e_stack)
    | Par -> (output_error ios "illegal paragraph ended before envir end."; 
              let ignore_par_stack = 
                List.filter par_filter  e_stack in 
              process_environ ios ignore_par_stack)
    | BeginCnl -> (output_error ios "improper nesting of \\begin{cnl}; input ignored"; 
                   process_environ ios e_stack)
    | EndCnl | Input _ | Eof -> (output_err_stack e_stack; tok) 
    | EndSeq s -> if (s = null_env.name) then
                    let msg = null_env.name^" is an illegal environment name; ignored" in
                    (output_error ios msg; process_environ ios e_stack)
                  else if (s = e.name) then
                    (out e.end_token e.is_delete; process_environ ios (List.tl e_stack))
                  else 
                    let msg = "\\end{" ^(e.name)^ "} expected, \\end{" 
                              ^s^ "} found; input ignored." in
                    (output_error ios msg; process_environ ios e_stack) 
    | BeginSeq s -> if (s = null_env.name) then
                    let msg = null_env.name^" is an illegal environment name; ignored" in
                    (output_error ios msg; process_environ ios e_stack)
                    else
                      let e' = mk_e_item s e in
                      let _ = if not(e'.is_delete)
                              then output_prologue ios s ; out e'.begin_token e'.is_delete in
                      process_environ ios (e' :: e_stack)
    | _ -> let ifexpand = not(e.is_delete) in
           let outputif tok = not(e.is_delete) && not(List.mem tok (e.drop_toks)) in 
           let tr = e.tr_token in
           (transcribe_token ios ifexpand outputif tr tok;
            process_environ ios e_stack)
;;

let rec debug_process_environ ios e  = 
  let tok = input ios NoTrackPar in 
  match tok with
  | EndCnl | Input _ | Eof -> (tok) 
    | _ ->          (transcribe_token ios false (fun _ -> true) [] tok;
            debug_process_environ ios e)
;;

let rec seek_cnl_block ios = 
  match (input ios NoTrackPar) with
  | BeginCnl -> BeginCnl 
  | Eof -> Eof 
  | _ -> seek_cnl_block ios;;

  (* returns Eof, or Input *)
let rec process_cnl_block ios = 
  match seek_cnl_block ios with
  | Eof -> Eof
  | _ -> (match (process_environ ios [null_env]) with
         | EndCnl -> process_cnl_block ios 
         | Eof -> Eof
         | Input _ as t -> t
         | _ -> (output_error ios "fatal, ending file"; Eof));;  

let rec process_ios convert_toks io_stack = 
  if io_stack = [] then true 
  else
    try 
      let ios = List.hd io_stack in
      match (process_cnl_block ios) with
      | Eof -> let _ = close_out ios.outc in process_ios convert_toks (List.tl io_stack)
      | Input filename -> (let ios' = mk_iochannels convert_toks filename in
                           process_ios convert_toks (ios' :: io_stack))
      | _ -> (output_error ios ("fatal file error, closing: "^ios.outfile); close_out(ios.outc);
              process_ios convert_toks (List.tl io_stack))
    with Failure _ ->
           let _ = List.map (fun x -> close_out(x.outc)) io_stack in
           false;;
  
let process_doc convert_toks filename = 
  process_ios convert_toks [mk_iochannels convert_toks filename];;


(* fin *)
