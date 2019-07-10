 (*

Grammar for CNL-CIC.
Thomas C. Hales
July 2019.

 *)

(* documentation *)

 (* 
 Beware of conflicting terminology between Forthel and Lean. 
 Conflicts include variables, attributes, sections, proof,
 names vs. namespaces. if,then vs. if-then-else.

 Forthel notions become Lean types. 

 The global outline of the file is given by
 start of line comments (regexp "^(").

 TDOP top down operator precedence terms and formulas are
 identified by the grammar but not reduced.
 
 *)

%{

 (* open Program *)
type exp_t =
| prop_t
| proof_t
| type_t
| term_t
[@@deriving show]

 %}



%start <string> program

%%

 (* program : {} *)

punctuation : delimiter | separator {}
lexeme : NUMERIC | identifier | FIELD_ACCESSOR | SYMBOL | punctuation {}


(* parametrized nonterminals *)

paren(X) : L_PAREN x = X R_PAREN { x }
 (* opt_paren(X) : 
| X
| paren(X) {} *)
bracket(X) : L_BRACK x = X R_BRACK { x }
brace(X) : L_BRACE x = X R_BRACE { x }
brace_semi(X) : brace(separated_nonempty_list(SEMI,X) {}) {}

comma_nonempty_list(X) : separated_nonempty_list(COMMA,X) {}
comma_list(X) : separated_list(COMMA,X) {}
opt_comma_nonempty_list(X) : separated_nonempty_list(option(COMMA),X) {}
sep_list(X) : separated_list(sep_and_comma,X) {}

 (* from phrase_lists.txt. These will need to be
    expanded in the working parser. *)
phrase_list_transition : NOT_IMPLEMENTED {}
phrase_list_filler : NOT_IMPLEMENTED {}
phrase_list_proof_statement : NOT_IMPLEMENTED {}

(* literals *)

lit_a : LIT_A | LIT_AN {}
article : lit_a | LIT_THE {}
sep_and_comma : LIT_AND | COMMA {}
separator : COMMA | SEMI | PERIOD {}
delimiter : L_PAREN | R_PAREN | L_BRACK | R_BRACK | L_BRACE | R_BRACE {}
identifier : ATOMIC_IDENTIFIER | HIERARCHICAL_IDENTIFIER {}
lit_binder_comma : COMMA {}

lit_defined_as : LIT_SAID LIT_TO LIT_BE
| LIT_DEFINED LIT_AS
| LIT_DEFINED LIT_TO LIT_BE {}
lit_iff : LIT_IFF | LIT_IF LIT_AND LIT_ONLY LIT_IF {}
lit_denote : LIT_STAND LIT_FOR | LIT_DENOTE {}
lit_do : LIT_DO | LIT_DOES {}
lit_is : LIT_IS | LIT_ARE | option(LIT_TO) LIT_BE {}
lit_equal : LIT_EQUAL LIT_TO {}
lit_has : LIT_HAS | LIT_HAVE {}
lit_with : LIT_WITH | LIT_OF | LIT_HAVING {}
lit_true : LIT_ON | LIT_TRUE | LIT_YES {}
lit_false : LIT_OFF | LIT_FALSE | LIT_NO {}
lit_its_wrong : LIT_IT LIT_IS LIT_WRONG LIT_THAT {}
lit_any : (* can extend: finitely many, almost all, etc. *)
| LIT_EVERY
| LIT_EACH
| LIT_EACH LIT_AND LIT_EVERY
| LIT_ALL
| LIT_ANY
| LIT_SOME
| LIT_NO {}
lit_exist : LIT_EXIST | LIT_EXISTS {}
lit_lets : LIT_LET LIT_US | LIT_WE option(LIT_CAN) {}
lit_assume : LIT_ASSUME | LIT_SUPPOSE {}
lit_then : LIT_THEN | LIT_THEREFORE | LIT_HENCE {}
lit_choose : LIT_TAKE | LIT_CHOOSE {}
lit_prove : LIT_PROVE | LIT_SHOW {}
lit_we_say : LIT_WE LIT_SAY option(LIT_THAT) {}
lit_left : LIT_LEFT | LIT_RIGHT | LIT_NO {}
lit_field_key : LIT_EMBEDDED
| LIT_PARAMETRIC
| LIT_TYPEABLE
| LIT_APPLICABLE {}
lit_qed : LIT_END | LIT_QED | LIT_OBVIOUS | LIT_TRIVIAL {}
lit_document :
| LIT_DOCUMENT
| LIT_ARTICLE
| LIT_SECTION
| LIT_SUBSECTION
| LIT_SUBSUBSECTION {}
lit_def : LIT_DEF | LIT_DEFINITION {}
lit_axiom : LIT_AXIOM | LIT_CONJECTURE | LIT_HYPOTHESIS {}
lit_theorem :
| LIT_PROPOSITION
| LIT_THEOREM
| LIT_LEMMA
| LIT_COROLLARY {}
lit_location :
| lit_document 
| lit_theorem 
| lit_axiom
| lit_def {}
lit_implicit : LIT_FIXED | LIT_IMPLICIT | LIT_RESOLVED {}
lit_sort : LIT_TYPE | LIT_PROP {}

label : ATOMIC_IDENTIFIER {}

(* fake rules suppressing errors for unused nonterminals. *)
fake_nonterminal :
| fake_prim
| fake_token
| fake_misc
{}

fake_token :
| EOF
| LIT_DONE
| LIT_NOTION
| NOT_DEBUGGED
{}

fake_misc :
| where_term
| controlseq_macro
| delimiter
| lexeme
| punctuation
{}

fake_prim :
| prim_classifier
| prim_symbol_cs
| prim_alpha_cs
| prim_pi_binder
| prim_binder_prop
| prim_typed_name
| prim_free_predicate
| prim_adjective
| prim_adjective_multisubject
| prim_simple_adjective
| prim_simple_adjective_multisubject
| prim_definite_noun
| prim_plain_noun
| prim_prefix_function
| prim_possessed_noun
| prim_verb
| prim_verb_multisubject
| prim_structure
| prim_term_op 
| prim_binary_relation_op 
| prim_propositional_op  
| prim_relation {}

(* primitives *)

prim_classifier : {} 
  (* meta-type words like "function" "element" "object" 
   "number" "quotient" "dependent function" "thing" "class" 
   "map" "structure" "term" "binary relation" "relation" 
   "operator" "binary operator" "pair" "pairs" "result" 
   *)

prim_symbol_cs : {}
prim_alpha_cs : {}

prim_lambda_binder : {} (* term binders *)
prim_pi_binder : {} (* type binders *)
prim_binder_prop : {}

prim_typed_name : {} (* correspond to Lean types, quotient of type by term, type of term, etc. *)
prim_free_predicate : {} (* used in quantifier scoping *)
prim_adjective : {}
prim_adjective_multisubject : {}

prim_simple_adjective : {} (* autogenerated, those with no args *)
prim_simple_adjective_multisubject : {} (* multisubject version, autogenerated from prim_adjective_multisubject, no args *)

prim_definite_noun : {} (* functions and terms *)
prim_plain_noun : {} (* autogenerated from prim_definite_noun *)
prim_prefix_function : NOT_IMPLEMENTED {} (* functions like sin,cos,exp *)
prim_possessed_noun : {} (* autogenerated as in Forthel *)
prim_verb : {}
prim_verb_multisubject : {} (* multisubject version *)
prim_structure : {} (* CIC structures *)
prim_term_op : {} (* = < > etc. *)
prim_binary_relation_op : {} (* + - * / etc. *)
prim_propositional_op : {} (* logical connectives *)
prim_relation : {} (* prop-valued *)

(* sections test:Sections *)

section_preamble : section_tag option(label) PERIOD {}
  section_tag : lit_document {}

(* namespaces  *)

namespace : NOT_IMPLEMENTED {}

(* instructions - 
   See Naproche-SAD github Instr.hs. Test:Instructions  
 *)

instruction :
| instruct_command
| instruct_synonym
| instruct_string
| instruct_bool
| instruct_int {}

  instruct_keyword_command : LIT_EXIT {}
  instruct_keyword_int : LIT_TIMELIMIT {}
  instruct_keyword_bool : LIT_PRINTGOAL | LIT_DUMP | LIT_ONTORED {}

  instruct_keyword_string : LIT_READ | LIT_LIBRARY {}

  instruct_command : bracket( instruct_keyword_command) {}
  instruct_int : bracket(instruct_keyword_int NUMBER {}) {}
  bool_tf : lit_true | lit_false {}
  instruct_bool : bracket(instruct_keyword_bool bool_tf {}){}
  instruct_string : bracket(instruct_keyword_string TOKEN {}) {}
  instruct_sep : SLASH | SLASHDASH {}
  instruct_synonym : bracket(LIT_SYNONYM
    separated_nonempty_list (instruct_sep,nonempty_list(TOKEN)) {}) {}

(* variables *)

 (* modifiers
    fixed = Lean-style section parameter.
    implicit = Lean-style { } implicit function parameter.
  *)

var_modifier : option(lit_implicit) {}
annotated_var : paren(var_modifier VAR colon_type {}) {}
annotated_vars : paren(var_modifier nonempty_list(VAR) colon_type {}) {}

tvar : VAR | annotated_var {}

 (* tvars : VAR | annotated_vars {} *)

 (* vars_bound : nonempty_list(tvars) {} *)

var_multisubject :
| tvar COMMA tvar
| paren(VAR COMMA VAR colon_type {}) {}

 (*
  vars_name :
 (* names have same type *)
  | paren(comma_nonempty_list(VAR) colon_type {}) {} 
  | list(VAR) {}
  *)

record_assign_term :
  brace_semi(var_or_atomic opt_colon_type ASSIGN term {})  {}

app_args :
  record_assign_term nonempty_list(expr_nonapp) {}


(* function and binder parameters *)
args : opt_args required_args {}
  opt_args : 
    option(brace_semi(var_or_atomics opt_colon_type {}) {}) {}

  required_args : list(required_arg) {}

  (* convention - all types are the same within parentheses *)
    required_arg :
    | paren(var_or_atomics opt_colon_type {})
    | var_or_atomic {}

  var_or_atomic : VAR | ATOMIC_IDENTIFIER {}
  var_or_atomics : nonempty_list(var_or_atomic) {} 

 (*
arg_call : opt_arg_assign_term required_arg_call {}

  required_args_call : list(required_arg_call) {}
  required_arg_call : 
                          | paren(term opt_colon_type)
                          | term                          
 *)
  

(* expressions *)

expr : type_expr | term | prop | proof_expr | sort_expr {}

expr_nonapp : (* what is allowed as an arg to function calls *)
| term_nonapp
| NOT_IMPLEMENTED 
{}

(* sorts *)

sort_expr : (* args should be nonempty *)
| option(args ARROW {}) lit_sort 
{}

colon_sort : COLON sort_expr {}

opt_colon_sort : option(colon_sort) {}

(* types *)

type_nonapp_expr :
| arrow_type
| agda_pi_type
| paren_type
| annotated_type
| controlseq_type
| const_type
| var_type
| subtype
| inductive_type
| mutual_inductive_type
| structure
{}

type_expr :
| app_type
| type_nonapp_expr {}

colon_type : COLON type_expr {}

opt_colon_type : option(colon_type) {}

arrow_type : type_expr ARROW type_expr {}

agda_pi_type : nonempty_list(annotated_vars) ARROW type_expr {}

paren_type : paren(type_expr) {}

annotated_type : paren(type_expr COLON LIT_TYPE {}) {}

app_type : type_nonapp_expr app_args {}


controlseq_type : controlseq {}

const_type : type_identifier {}
  type_identifier : identifier {}

var_type : VAR {}

subtype :  brace(term fixing_var LIT_SUBTYPEMID term {}) {}
  fixing_var : option(LIT_FIXING comma_nonempty_list(VAR) {}) {}

(** inductive types *)

inductive_type : LIT_INDUCTIVE identifier args 
  opt_colon_sort list(opt_alt_constructor) LIT_END {}

  opt_alt_constructor : ALT identifier args opt_colon_type {}
  alt_constructor : ALT identifier args colon_type {}

mutual_inductive_type : LIT_INDUCTIVE
 comma_nonempty_list(identifier) args 
 list(LIT_WITH ATOMIC_IDENTIFIER args colon_type
       list(alt_constructor) {}) 
 LIT_END{}

(** structure *)

 (* for the declaration - 
structure : option(lit_a) prim_structure LIT_IS 
  lit_a anon_structure {}
 *)

structure : option(LIT_NOTATIONAL) LIT_STRUCTURE 
  option(lit_param) args
  option(LIT_WITH) option(brace_semi(field))
  option(LIT_SATISFYING satisfying_preds {}) {}

  lit_param : LIT_WITH LIT_PARAMETERS {}
  field : field_prefix field_name option(field_suffix) {}
  field_name :
  | ATOMIC_IDENTIFIER colon_type
  | VAR colon_type {}
  field_prefix : ALT list(lit_field_key) {}
  field_suffix : LIT_WITHOUT LIT_NOTATION | field_assign {}
  field_assign : ASSIGN term {}
  satisfying_preds : brace_semi(satisfying_pred) {}
  satisfying_pred : ALT option(ATOMIC_IDENTIFIER COLON {}) prop {}

(* props *)

prop : 
| binder_prop
| identifier_prop 
| tdop_rel_op
| tdop_prop

{}

binder_prop : prim_binder_prop args
  lit_binder_comma prop {}

identifier_prop : identifier {}

(* proof expressions (distinct from proof scripts). *)

proof_expr : SYMBOL_QED {}

(* terms *)

term_nonapp :
| controlseq
| match_term
| case_term
| make_term
| lambda_term
| lambda_fun
| lambda_function
| identifier_term
| var_term
| lambda_term
| let_term
| annotated_term
| list_term
| tuple_term
| set_enum_term
| set_comprehension_term
| if_then_else_term
| tdop_term
| NUMERIC
| STRING
| DECIMAL
| BLANK {}

term : 
| term_nonapp
| app_term {}

terms : sep_list(term) {}

symbol_term : tvar | NOT_IMPLEMENTED {}

app_term : term_nonapp app_args {}

var_term : VAR {}

annotated_term : paren(term colon_type {}) {}

identifier_term : identifier {}

controlseq : CONTROLSEQ list(cs_arg) {}
  cs_arg : brace(expr) {} 

match_term : LIT_MATCH match_seq LIT_WITH 
  nonempty_list(ALT match_pats ASSIGN term {}) LIT_END {}
  match_seq : comma_nonempty_list(term) {}
  match_pats : comma_nonempty_list(match_pat) {}
  match_pat : term {}

(** case statement. *)
case_term : LIT_CASE term LIT_OF 
  nonempty_list(alt_case) LIT_END {}
  alt_case : ALT prop ASSIGN term {}

(** where (Haskell style) *)
where_term : brace_semi(tvar opt_colon_type ASSIGN term {}) {}

(** make. *)
make_term : brace_semi(identifier 
  option(ASSIGN term {}) option(SEMI BLANK {}) {}) 
  opt_colon_type {}

lambda_term : prim_lambda_binder args 
  lit_binder_comma term {}

lambda_fun : LIT_FUN identifier args 
  opt_colon_type ASSIGN term {}

lambda_function : LIT_FUNCTION identifier args 
  opt_colon_type nonempty_list(ALT match_pats ASSIGN term {})
  LIT_END {}

list_term : bracket(separated_list(SEMI,term) {}) {}

tuple_term : paren(term COMMA comma_nonempty_list(term) {}) {}

set_enum_term : brace(comma_list(term) {}) {}

set_comprehension_term : brace(term fixing_var LIT_MID term {}) {}

  (* let includes destructuring*)
let_term : LIT_LET term ASSIGN term LIT_IN term {}

if_then_else_term : LIT_IF prop LIT_THEN term LIT_ELSE term {}

(* TDOP symbolic terms and formulas *)
 (* top down operator precedence formulas. 

    There are three general precedence categories built into 
    the grammar.  
    * prop operators; (precedence < 0) 
    * binary relation operators such as "="; (precedence=0)
    * term operators.  (precedence > 0).
    This allows us to distinguish terms from props.
  *)

 (* prec > 0 *)
tdop_term : option(tdop_arg) prim_term_ops
  list(tdop_arg prim_term_ops {}) option(tdop_arg) {}

  prim_term_ops : nonempty_list(prim_term_op) {}

  tdop_arg : (* any "simple" term with unambiguous boundaries *)
  | tvar
  | STRING
  | NUMBER
  | controlseq 
  | tdop_app
  | make_term
  | list_term
  | tuple_term
  | set_enum_term
  | set_comprehension_term
  | annotated_term
  | NUMERIC
  | STRING
  | DECIMAL
  | BLANK {}

  tdop_app : (* including paren(term) *)
  | paren(term) option(app_args)
  | prim_prefix_function option(app_args) {}


 (* prec = 0 *)
 (* We allow x,y < z < w. The first arg can be a list. *)
tdop_rel_op : 
  tdop_rel_args 
  nonempty_list(prim_binary_relation_op tdop_rel_arg {}) {}

  tdop_rel_arg :
  | tdop_term
  | tdop_arg {}

  tdop_rel_args :
  | tdop_rel_arg
  | comma_nonempty_list(tdop_arg) {}

 (* prec < 0 *)
tdop_prop :
  option(tdop_prop_arg) prim_prop_ops
  list(tdop_prop_arg prim_prop_ops {}) option(tdop_prop_arg) {}

  prim_prop_ops : nonempty_list(prim_propositional_op) {}

  tdop_prop_arg : (* any prop with unambiguous boundaries *)
  | paren(statement)
  | tdop_rel_op 
  | NOT_IMPLEMENTED
  {}



(* statements *)
  (* NOT_DEBUGGED 
     statements, tdop_prop, and prop need further integration. 
   *)

(** predicates *)
does_pred : option(lit_do) option(LIT_NOT) prim_verb {}
| option(lit_do) option(LIT_NOT) prim_verb_multisubject
| lit_has has_pred
| lit_is sep_list(is_pred)
| lit_is sep_list(is_aPred {}) {}

is_pred : option(LIT_NOT) prim_adjective {}
| option(LIT_NOT) option(LIT_PAIRWISE) prim_adjective_multisubject
| lit_with has_pred {}

is_aPred : option(LIT_NOT) option(lit_a) type_expr {}
| option(LIT_NOT) definite_term {}

  definite_term : 
  | symbol_term
  | option(LIT_THE) prim_definite_noun
  | paren(option(LIT_THE) prim_definite_noun {}) {}

has_pred : 
| sep_list(article possessed_noun {}) {}
| LIT_NO possessed_noun {}

  possessed_noun : attribute(prim_possessed_noun) {}


(** attributes (forthel style) *)

attribute(X) : list(left_attribute) X option(right_attribute) {}

  left_attribute : 
  |prim_simple_adjective 
  | prim_simple_adjective_multisubject {}

  right_attribute : 
  | sep_list(is_pred) {}
  | LIT_THAT sep_list(does_pred) {}
  | LIT_SUCH LIT_THAT statement {}

typed_name : attribute(typed_name_without_attribute) {}
  typed_name_without_attribute : 
  | prim_typed_name
  | tvar
  | VAR lit_with LIT_TYPE type_expr
  | paren(typed_name_without_attribute)
  {}

free_predicate : attribute(free_predicate_without_attribute) {} 
  free_predicate_without_attribute : 
  | prim_free_predicate
  | paren(prim_free_predicate) {}

named_terms : sep_list(option(lit_a) named_term {}) {}

named_term : typed_name | free_predicate {} (* type_expr *)



(** statement *)

statement : head_statement | chain_statement {}

  head_statement : 
  | LIT_FOR sep_list(any_type {}) option(COMMA) statement {}
  | LIT_IF statement COMMA LIT_THEN statement (* != if-then-else *)
  | lit_its_wrong statement {}

  chain_statement : and_chain option(LIT_AND head_statement {})
  | or_chain option(LIT_OR head_statement {}) {}
  | and_or_chain LIT_IFF statement {}
  and_or_chain : and_chain | or_chain {}
  and_chain : separated_nonempty_list(LIT_AND, primary_statement {}) {}
  or_chain : separated_nonempty_list(LIT_OR, primary_statement {}) {}
  any_type : lit_any type_expr {} 


(** primary statement *)
primary_statement :
  | simple_statement {}
  | there_is_statement
  | option(phrase_list_filler) symbol_statement
  | option(phrase_list_filler) const_statement {}

  simple_statement : terms separated_nonempty_list(LIT_AND, does_pred) {}

  there_is_statement : LIT_THERE lit_exist named_terms {}
  | LIT_THERE lit_exist LIT_NO named_term {}

  const_statement : option(LIT_THE) LIT_THESIS {}
  | option(LIT_THE) LIT_CONTRARY
  | lit_a LIT_CONTRADICTION {}

  symbol_statement :
  | LIT_FORALL free_predicate lit_binder_comma
    symbol_statement {}
  | LIT_EXISTS free_predicate lit_binder_comma symbol_statement
  | prim_relation
  | LIT_NOT symbol_statement
  | paren(statement)
  | prop
  | symbol_predicate {}

  symbol_predicate : NOT_IMPLEMENTED {}

(* text *)
text : list(text_item) {}
  text_item : 
    | namespace
    | macro
    | section_preamble
    | declaration
    | instruction {}

(* declaration test:declaration *)
declaration : axiom | definition | theorem  {}

(** axiom *)
axiom : axiom_preamble list(assumption) 
  then_prefix statement PERIOD {}

  axiom_preamble : lit_axiom option(label) PERIOD {}

  assumption :
  | assumption_prefix statement PERIOD
  | let_annotation PERIOD {}
  assumption_prefix : 
  | LIT_LET 
  | lit_lets lit_assume option(LIT_THAT) {}

  then_prefix : option(lit_then) {}

(** definition *)
definition : definition_preamble list(assumption) 
  definition_affirm {}
  definition_preamble : lit_def option(label) PERIOD {}
  definition_affirm : definition_statement PERIOD 
    option(this_exists) {}

definition_statement :
| type_def
| function_def
| predicate_def {}

  copula : lit_is option(lit_defined_as) | ASSIGN | lit_denote {}
  iff_junction : lit_iff {}
  opt_say : option(lit_we_say) {}
  opt_define : 
  | option(lit_lets) option(LIT_DEFINE) 
  | option(LIT_LET) {}

  type_def : type_head copula lit_a type_expr {}
    type_head :  type_pattern {}

  function_def : opt_define function_head
    copula option(lit_equal) option(LIT_THE) term {}

    function_head :
    | function_token_pattern
    | symbol_pattern option(paren_precedence_level)
    | identifier_pattern 
    | controlseq_pattern {}

  predicate_def : opt_say predicate_head iff_junction statement {}
    predicate_head :
    | predicate_token_pattern
    | symbol_pattern option(paren_precedence_level)
    | identifier_pattern 
    | controlseq_pattern {}

 (*
  According to Paskevich 1.3.6. existing primitives are allowed for
  the purpose of signature extensions. We do not include them. 

  The restrictions of Paskevich 1.3.6 hold for functions.
  * In the head unit, the argument places hold variables.
  * No repeated variables occur in the head. 
  * Every free variable on the right-hand appears on the left. 
  * etc.

  *)

  (*
  Disambiguation:
  predicates use iff_junction.
  predicates token patterns start with tvar.
  identifier patterns start with an identifier.
  symbol patterns contain a symbol.

  Functions use the copula.
  token patterns starts with LIT_THE token ...
  symbol patterns contain a symbol.
  identifier patterns start with an identifier.

  Types use the copula.

  *)

(** theorem *)
theorem : theorem_preamble list(assumption) affirm {}
  theorem_preamble : lit_theorem option(label) PERIOD {}

(** affirm *)
affirm : 
| affirm_with_proof
| affirm_with_short_proof
| affirm_with_goal_proof {}
 
 affirm_with_proof : then_prefix statement by_ref PERIOD
   option(proof_preamble proof_script {}) {}

   proof_preamble : LIT_PROOF by_method PERIOD
   | LIT_INDEED {}

 affirm_with_short_proof : then_prefix statement by_ref PERIOD
   short_proof_script {}

   short_proof_script : LIT_INDEED affirm {}

 affirm_with_goal_proof : goal_prefix statement by_ref PERIOD 
   proof_script {}




(** proof scripts *)
proof_script : option(list(canned_prefix proof_body {}) 
  canned_prefix proof_tail {}) lit_qed PERIOD {}

  canned_prefix : sep_list(phrase_list_transition) option(COMMA) {}

  canned_proof : phrase_list_proof_statement {}
  proof_body : proof_tail | assumption {}
  proof_tail : affirm | choose | case | canned_proof {}

  goal_prefix : option(lit_lets)
    lit_prove by_method option(LIT_THAT) {}
  case : LIT_CASE statement PERIOD proof_script {}
  choose : choose_prefix named_terms by_ref PERIOD choose_justify {}
  choose_justify : 
  | option(proof_preamble proof_script {})
  | short_proof_script {}
  choose_prefix : then_prefix option(lit_lets) lit_choose {}

  by_method : option(LIT_BY proof_method {}) {}
  proof_method : LIT_CONTRADICTION
    | LIT_CASE LIT_ANALYSIS
    | LIT_INDUCTION option(LIT_ON term {}) {}
  by_ref : option(paren(LIT_BY ref_item {})) {}
  ref_item : sep_list(option(lit_location) label {}) {}


(** This exists and is well-defined. *)
this_exists : LIT_THIS
  sep_list(this_directive_pred) PERIOD {}
  this_directive_pred : LIT_IS
    sep_list(this_directive_adjective)
    | this_directive_verb {}
  this_directive_adjective :
    | LIT_UNIQUE
    | LIT_CANONICAL
    | LIT_WELLDEFINED
    | LIT_WELL_DEFINED
    | LIT_WELL LIT_DEFINED
    | LIT_TOTAL
    | LIT_WELL LIT_PROPPED
    | LIT_WELL_PROPPED
    | LIT_EXHAUSTIVE {}
  this_directive_right_attr : LIT_BY LIT_RECURSION {}
  this_directive_verb : LIT_EXISTS option(this_directive_right_attr){}

(* patterns and macros *)

 (* NOT_DEBUGGED *)

macro : option(insection) sep_list(assuming) macro_body {}
  assuming : LIT_ASSUMING option(LIT_THAT) statement {}
  insection : LIT_IN LIT_THIS section_tag {}
  macro_body : type_macro | function_macro | predicate_macro {}
  type_macro : LIT_LET type_pattern
    lit_denote lit_a type_expr PERIOD {}
  function_macro : LIT_LET function_token_pattern
    lit_denote term PERIOD {}
  controlseq_macro : NOT_IMPLEMENTED {}
  predicate_macro :
  | LIT_LET predicate_token_pattern lit_denote statement PERIOD {}
  | LIT_LET symbol_pattern lit_denote statement PERIOD {}
  let_annotation : LIT_LET comma_nonempty_list(annotated_vars) {}

 (* restriction: tokens in pattern cannot be a variant of
    "to be", called, no iff,
    cannot start with "the"  *)

token_pattern : tokens list(tvar tokens {}) option(tvar) {}
tokens : nonempty_list(TOKEN) {}
type_pattern : option(lit_a) token_pattern {}
function_token_pattern : LIT_THE token_pattern {}

predicate_token_pattern :
| tvar LIT_IS option(LIT_CALLED) token_pattern {} (* adjective *)
| var_multisubject LIT_ARE option(LIT_CALLED) token_pattern {} 
| tvar token_pattern {} (* verb *)
| var_multisubject token_pattern {} (* multisubject verb *)

identifier_pattern :
| identifier args opt_colon_type {}

controlseq_pattern : CONTROLSEQ list(cs_var) {}
  cs_var : brace(tvar) {} 

symbol_pattern : option(tvar) SYMBOL list(tvar SYMBOL {}) 
  option(tvar) {}

paren_precedence_level :
| precedence_level
| paren(precedence_level) {}

  precedence_level :
  | LIT_WITH LIT_PRECEDENCE NUMBER
    option(LIT_AND lit_left LIT_ASSOCIATIVITY {}) {}



program : text | fake_nonterminal { "done" }

