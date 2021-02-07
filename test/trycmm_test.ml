open Core
open Llambda.Trycmm

let%expect_test "compile" =
  let source = {|
    let f x = 1234 + x;;
  |} in
  let config : Frontend.t =
    { source_prefix = "melse"; dump_ast = true; dump_typed_ast = true }
  in
  let structure = Frontend.parse config source in
  [%expect {|
    let f x = 1234 + x |}];
  let typed_ast = Frontend.type_impl config structure in
  [%expect
    {|
    [
      structure_item ([2,1+4]..[2,1+22])
        Tstr_value Nonrec
        [
          <def>
            pattern ([2,1+8]..[2,1+9])
              Tpat_var "f/80"
            expression ([2,1+10]..[2,1+22]) ghost
              Texp_function
              Nolabel
              [
                <case>
                  pattern ([2,1+10]..[2,1+11])
                    Tpat_var "x/82"
                  expression ([2,1+14]..[2,1+22])
                    Texp_apply
                    expression ([2,1+19]..[2,1+20])
                      Texp_ident "Stdlib!.+"
                    [
                      <arg>
                        Nolabel
                        expression ([2,1+14]..[2,1+18])
                          Texp_constant Const_int 1234
                      <arg>
                        Nolabel
                        expression ([2,1+21]..[2,1+22])
                          Texp_ident "x/82"
                    ]
              ]
        ]
    ] |}];
  let config : Backend.t =
    { prefix_name = "melse"; dump_cmm = false; dump_clambda = true; dump_lambda = true }
  in
  let cmm = Backend.to_cmm config typed_ast in
  [%expect
    {|
    (seq
      (let (f/80 = (function x/82[int] : int (+ 1234 x/82)))
        (setfield_ptr(root-init) 0 (global Melse!) f/80))
    File "_none_", line 1:
    Warning 58: no cmx file was found in path for module Melse, and its interface was not compiled with -opaque

      0a)(seq
           (let
             (f/80 (closure  (fun caml__f_80:int 1  x/82[int] (+ 1234 x/82)) ))
             (setfield_ptr(root-init) 0 (read_symbol camlMelse) f/80))
           0a)
    ("preallocated block" (symbol caml) (exported true) (tag 0) (field_count 1)) |}];
  List.iter cmm ~f:(function
      | Cfunction { fun_name; _ } -> print_s [%message "fundecl" (fun_name : string)]
      | Cdata items ->
        print_s [%message "data"];
        List.iter items ~f:(function
            | Cint value -> print_s [%message "cint" (value : nativeint)]
            | Cdefine_symbol name -> print_s [%message "define_symbol" (name : string)]
            | Cglobal_symbol name -> print_s [%message "global_symbol" (name : string)]
            | Csymbol_address address ->
              print_s [%message "symbol_address" (address : string)]
            | _ -> print_s [%message "other"]));
  [%expect
    {|
    data
    data
    (cint (value 3063))
    (define_symbol (name caml__1))
    (symbol_address (address caml__f_80))
    (cint (value 3))
    data
    (cint (value 1792))
    (global_symbol (name caml))
    (define_symbol (name caml))
    (cint (value 1))
    data
    (global_symbol (name caml__gc_roots))
    (define_symbol (name caml__gc_roots))
    (symbol_address (address caml))
    (cint (value 0))
    (fundecl (fun_name caml__f_80))
    (fundecl (fun_name caml__entry))
    (fundecl (fun_name caml_program))
    (fundecl (fun_name caml_apply3))
    (fundecl (fun_name caml_apply2))
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Out_of_memory))
    (define_symbol (name caml_exn_Out_of_memory))
    (symbol_address (address caml_startup__2))
    (cint (value -1))
    (cint (value 3068))
    (define_symbol (name caml_startup__2))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Sys_error))
    (define_symbol (name caml_exn_Sys_error))
    (symbol_address (address caml_startup__3))
    (cint (value -3))
    (cint (value 3068))
    (define_symbol (name caml_startup__3))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Failure))
    (define_symbol (name caml_exn_Failure))
    (symbol_address (address caml_startup__4))
    (cint (value -5))
    (cint (value 2044))
    (define_symbol (name caml_startup__4))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Invalid_argument))
    (define_symbol (name caml_exn_Invalid_argument))
    (symbol_address (address caml_startup__5))
    (cint (value -7))
    (cint (value 4092))
    (define_symbol (name caml_startup__5))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_End_of_file))
    (define_symbol (name caml_exn_End_of_file))
    (symbol_address (address caml_startup__6))
    (cint (value -9))
    (cint (value 3068))
    (define_symbol (name caml_startup__6))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Division_by_zero))
    (define_symbol (name caml_exn_Division_by_zero))
    (symbol_address (address caml_startup__7))
    (cint (value -11))
    (cint (value 4092))
    (define_symbol (name caml_startup__7))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Not_found))
    (define_symbol (name caml_exn_Not_found))
    (symbol_address (address caml_startup__8))
    (cint (value -13))
    (cint (value 3068))
    (define_symbol (name caml_startup__8))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Match_failure))
    (define_symbol (name caml_exn_Match_failure))
    (symbol_address (address caml_startup__9))
    (cint (value -15))
    (cint (value 3068))
    (define_symbol (name caml_startup__9))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Stack_overflow))
    (define_symbol (name caml_exn_Stack_overflow))
    (symbol_address (address caml_startup__10))
    (cint (value -17))
    (cint (value 3068))
    (define_symbol (name caml_startup__10))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Sys_blocked_io))
    (define_symbol (name caml_exn_Sys_blocked_io))
    (symbol_address (address caml_startup__11))
    (cint (value -19))
    (cint (value 3068))
    (define_symbol (name caml_startup__11))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Assert_failure))
    (define_symbol (name caml_exn_Assert_failure))
    (symbol_address (address caml_startup__12))
    (cint (value -21))
    (cint (value 3068))
    (define_symbol (name caml_startup__12))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Undefined_recursive_module))
    (define_symbol (name caml_exn_Undefined_recursive_module))
    (symbol_address (address caml_startup__13))
    (cint (value -23))
    (cint (value 5116))
    (define_symbol (name caml_startup__13))
    other
    other
    other
    data
    (global_symbol (name caml_globals))
    (define_symbol (name caml_globals))
    (symbol_address (address camlMelse__gc_roots))
    (cint (value 0))
    data
    (global_symbol (name caml_data_segments))
    (define_symbol (name caml_data_segments))
    (symbol_address (address caml_startup__data_begin))
    (symbol_address (address caml_startup__data_end))
    (symbol_address (address camlMelse__data_begin))
    (symbol_address (address camlMelse__data_end))
    (cint (value 0))
    data
    (global_symbol (name caml_code_segments))
    (define_symbol (name caml_code_segments))
    (symbol_address (address caml_startup__code_begin))
    (symbol_address (address caml_startup__code_end))
    (symbol_address (address camlMelse__code_begin))
    (symbol_address (address camlMelse__code_end))
    (cint (value 0))
    data
    (global_symbol (name caml_frametable))
    (define_symbol (name caml_frametable))
    (symbol_address (address caml_startup__frametable))
    (symbol_address (address caml_system__frametable))
    (symbol_address (address camlMelse__frametable))
    (cint (value 0)) |}]
;;

let%expect_test "" =
  let source = {| let rec sum x = match x with | [] -> 0 | x :: xs -> x + (sum xs);; |} in
  let config : Frontend.t =
    { source_prefix = "melse"; dump_ast = true; dump_typed_ast = true }
  in
  let structure = Frontend.parse config source in
  [%expect {|
    let rec sum x = match x with | [] -> 0 | x::xs -> x + (sum xs) |}];
  let typed_ast = Frontend.type_impl config structure in
  [%expect
    {|
    [
      structure_item ([1,0+1]..[1,0+65])
        Tstr_value Rec
        [
          <def>
            pattern ([1,0+9]..[1,0+12])
              Tpat_var "sum/80"
            expression ([1,0+13]..[1,0+65]) ghost
              Texp_function
              Nolabel
              [
                <case>
                  pattern ([1,0+13]..[1,0+14])
                    Tpat_var "x/81"
                  expression ([1,0+17]..[1,0+65])
                    Texp_match
                    expression ([1,0+23]..[1,0+24])
                      Texp_ident "x/81"
                    [
                      <case>
                        pattern ([1,0+32]..[1,0+34])
                          Tpat_value
                          pattern ([1,0+32]..[1,0+34])
                            Tpat_construct "[]"
                            []
                        expression ([1,0+38]..[1,0+39])
                          Texp_constant Const_int 0
                      <case>
                        pattern ([1,0+42]..[1,0+49])
                          Tpat_value
                          pattern ([1,0+42]..[1,0+49])
                            Tpat_construct "::"
                            [
                              pattern ([1,0+42]..[1,0+43])
                                Tpat_var "x/82"
                              pattern ([1,0+47]..[1,0+49])
                                Tpat_var "xs/83"
                            ]
                        expression ([1,0+53]..[1,0+65])
                          Texp_apply
                          expression ([1,0+55]..[1,0+56])
                            Texp_ident "Stdlib!.+"
                          [
                            <arg>
                              Nolabel
                              expression ([1,0+53]..[1,0+54])
                                Texp_ident "x/82"
                            <arg>
                              Nolabel
                              expression ([1,0+57]..[1,0+65])
                                Texp_apply
                                expression ([1,0+58]..[1,0+61])
                                  Texp_ident "sum/80"
                                [
                                  <arg>
                                    Nolabel
                                    expression ([1,0+62]..[1,0+64])
                                      Texp_ident "xs/83"
                                ]
                          ]
                    ]
              ]
        ]
    ] |}];
  let config : Backend.t =
    { prefix_name = "melse"; dump_cmm = true; dump_clambda = true; dump_lambda = true }
  in
  let cmm = Backend.to_cmm config typed_ast in
  [%expect
    {|
    (seq
      (letrec
        (sum/80
           (function x/81 : int
             (if x/81 (+ (field 0 x/81) (apply sum/80 (field 1 x/81))) 0)))
        (setfield_ptr(root-init) 0 (global Melse!) sum/80))
    File "_none_", line 1:
    Warning 58: no cmx file was found in path for module Melse, and its interface was not compiled with -opaque

      0a)(seq
           (let
             (clos/86
                (closure
                  (fun caml_startup__sum_80:int 1  x/81
                    (if x/81
                      (+ (field 0 x/81)
                        (apply* caml_startup__sum_80  (field 1 x/81)))
                      0)) ))
             (setfield_ptr(root-init) 0 (read_symbol camlMelse)
               (offset clos/86 0)))
           0a)
    ("preallocated block" (symbol caml_startup) (exported true) (tag 0)
     (field_count 1))
    (data)(data int 3063 "caml_startup__14": addr "caml_startup__sum_80" int 3)
    (data
     int 1792
     global "caml_startup"
     "caml_startup":
     int 1)(data
            global "caml_startup__gc_roots"
            "caml_startup__gc_roots":
            addr "caml_startup"
            int 0)(function{:1,13-65} caml_startup__sum_80 (x/81: val)
                   (if (!= x/81 1)
                     (+
                       (+ (load_mut val x/81)
                         (app{:1,57-65} "caml_startup__sum_80"
                           (load_mut val (+a x/81 8)) val))
                       -1)
                     1))
    (function caml_startup__entry ()
     (let clos/86 "caml_startup__14" (store val(root-init) "camlMelse" clos/86))
     1a)
    (function caml_program ()
     (app "camlMelse__entry" unit)
     (store int "caml_globals_inited" (+ (load_mut int "caml_globals_inited") 1))
     1)
    (function caml_apply3 (arg/91: val arg/92: val arg/93: val clos/94: val)
     (if (== (load_mut val (+a clos/94 8)) 7)
       (app (load_mut val (+a clos/94 16)) arg/91 arg/92 arg/93 clos/94 val)
       (let
         (clos/95 (app (load_mut val clos/94) arg/91 clos/94 val)
          clos/96 (app (load_mut val clos/95) arg/92 clos/95 val))
         (app (load_mut val clos/96) arg/93 clos/96 val))))
    (function caml_apply2 (arg/87: val arg/88: val clos/89: val)
     (if (== (load_mut val (+a clos/89 8)) 5)
       (app (load_mut val (+a clos/89 16)) arg/87 arg/88 clos/89 val)
       (let clos/90 (app (load_mut val clos/89) arg/87 clos/89 val)
         (app (load_mut val clos/90) arg/88 clos/90 val))))
    (data
     int 3064
     global "caml_exn_Out_of_memory"
     "caml_exn_Out_of_memory":
     addr "caml_startup__15"
     int -1
     int 3068
     "caml_startup__15":
     string "Out_of_memory"
     skip 2
     byte 2)(data
             int 3064
             global "caml_exn_Sys_error"
             "caml_exn_Sys_error":
             addr "caml_startup__16"
             int -3
             int 3068
             "caml_startup__16":
             string "Sys_error"
             skip 6
             byte 6)(data
                     int 3064
                     global "caml_exn_Failure"
                     "caml_exn_Failure":
                     addr "caml_startup__17"
                     int -5
                     int 2044
                     "caml_startup__17":
                     string "Failure"
                     skip 0
                     byte 0)(data
                             int 3064
                             global "caml_exn_Invalid_argument"
                             "caml_exn_Invalid_argument":
                             addr "caml_startup__18"
                             int -7
                             int 4092
                             "caml_startup__18":
                             string "Invalid_argument"
                             skip 7
                             byte 7)(data
                                     int 3064
                                     global "caml_exn_End_of_file"
                                     "caml_exn_End_of_file":
                                     addr "caml_startup__19"
                                     int -9
                                     int 3068
                                     "caml_startup__19":
                                     string "End_of_file"
                                     skip 4
                                     byte 4)(data
                                             int 3064
                                             global "caml_exn_Division_by_zero"
                                             "caml_exn_Division_by_zero":
                                             addr "caml_startup__20"
                                             int -11
                                             int 4092
                                             "caml_startup__20":
                                             string "Division_by_zero"
                                             skip 7
                                             byte 7)(data
                                                     int 3064
                                                     global "caml_exn_Not_found"
                                                     "caml_exn_Not_found":
                                                     addr "caml_startup__21"
                                                     int -13
                                                     int 3068
                                                     "caml_startup__21":
                                                     string "Not_found"
                                                     skip 6
                                                     byte 6)(data
                                                             int 3064
                                                             global "caml_exn_Match_failure"
                                                             "caml_exn_Match_failure":
                                                             addr "caml_startup__22"
                                                             int -15
                                                             int 3068
                                                             "caml_startup__22":
                                                             string "Match_failure"
                                                             skip 2
                                                             byte 2)(data
                                                                     int 3064
                                                                     global "caml_exn_Stack_overflow"
                                                                     "caml_exn_Stack_overflow":
                                                                     addr "caml_startup__23"
                                                                     int -17
                                                                     int 3068
                                                                     "caml_startup__23":
                                                                     string "Stack_overflow"
                                                                     skip 1
                                                                     byte 1)
    (data
     int 3064
     global "caml_exn_Sys_blocked_io"
     "caml_exn_Sys_blocked_io":
     addr "caml_startup__24"
     int -19
     int 3068
     "caml_startup__24":
     string "Sys_blocked_io"
     skip 1
     byte 1)(data
             int 3064
             global "caml_exn_Assert_failure"
             "caml_exn_Assert_failure":
             addr "caml_startup__25"
             int -21
             int 3068
             "caml_startup__25":
             string "Assert_failure"
             skip 1
             byte 1)(data
                     int 3064
                     global "caml_exn_Undefined_recursive_module"
                     "caml_exn_Undefined_recursive_module":
                     addr "caml_startup__26"
                     int -23
                     int 5116
                     "caml_startup__26":
                     string "Undefined_recursive_module"
                     skip 5
                     byte 5)(data
                             global "caml_globals"
                             "caml_globals":
                             addr "camlMelse__gc_roots"
                             int 0)(data
                                    global "caml_data_segments"
                                    "caml_data_segments":
                                    addr "caml_startup__data_begin"
                                    addr "caml_startup__data_end"
                                    addr "camlMelse__data_begin"
                                    addr "camlMelse__data_end"
                                    int 0)(data
                                           global "caml_code_segments"
                                           "caml_code_segments":
                                           addr "caml_startup__code_begin"
                                           addr "caml_startup__code_end"
                                           addr "camlMelse__code_begin"
                                           addr "camlMelse__code_end"
                                           int 0)(data
                                                  global "caml_frametable"
                                                  "caml_frametable":
                                                  addr "caml_startup__frametable"
                                                  addr "caml_system__frametable"
                                                  addr "camlMelse__frametable"
                                                  int 0) |}];
  List.iter cmm ~f:(function
      | Cfunction { fun_name; _ } -> print_s [%message "fundecl" (fun_name : string)]
      | Cdata items ->
        print_s [%message "data"];
        List.iter items ~f:(function
            | Cint value -> print_s [%message "cint" (value : nativeint)]
            | Cdefine_symbol name -> print_s [%message "define_symbol" (name : string)]
            | Cglobal_symbol name -> print_s [%message "global_symbol" (name : string)]
            | Csymbol_address address ->
              print_s [%message "symbol_address" (address : string)]
            | _ -> print_s [%message "other"]));
  [%expect
    {|
    data
    data
    (cint (value 3063))
    (define_symbol (name caml_startup__14))
    (symbol_address (address caml_startup__sum_80))
    (cint (value 3))
    data
    (cint (value 1792))
    (global_symbol (name caml_startup))
    (define_symbol (name caml_startup))
    (cint (value 1))
    data
    (global_symbol (name caml_startup__gc_roots))
    (define_symbol (name caml_startup__gc_roots))
    (symbol_address (address caml_startup))
    (cint (value 0))
    (fundecl (fun_name caml_startup__sum_80))
    (fundecl (fun_name caml_startup__entry))
    (fundecl (fun_name caml_program))
    (fundecl (fun_name caml_apply3))
    (fundecl (fun_name caml_apply2))
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Out_of_memory))
    (define_symbol (name caml_exn_Out_of_memory))
    (symbol_address (address caml_startup__15))
    (cint (value -1))
    (cint (value 3068))
    (define_symbol (name caml_startup__15))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Sys_error))
    (define_symbol (name caml_exn_Sys_error))
    (symbol_address (address caml_startup__16))
    (cint (value -3))
    (cint (value 3068))
    (define_symbol (name caml_startup__16))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Failure))
    (define_symbol (name caml_exn_Failure))
    (symbol_address (address caml_startup__17))
    (cint (value -5))
    (cint (value 2044))
    (define_symbol (name caml_startup__17))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Invalid_argument))
    (define_symbol (name caml_exn_Invalid_argument))
    (symbol_address (address caml_startup__18))
    (cint (value -7))
    (cint (value 4092))
    (define_symbol (name caml_startup__18))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_End_of_file))
    (define_symbol (name caml_exn_End_of_file))
    (symbol_address (address caml_startup__19))
    (cint (value -9))
    (cint (value 3068))
    (define_symbol (name caml_startup__19))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Division_by_zero))
    (define_symbol (name caml_exn_Division_by_zero))
    (symbol_address (address caml_startup__20))
    (cint (value -11))
    (cint (value 4092))
    (define_symbol (name caml_startup__20))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Not_found))
    (define_symbol (name caml_exn_Not_found))
    (symbol_address (address caml_startup__21))
    (cint (value -13))
    (cint (value 3068))
    (define_symbol (name caml_startup__21))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Match_failure))
    (define_symbol (name caml_exn_Match_failure))
    (symbol_address (address caml_startup__22))
    (cint (value -15))
    (cint (value 3068))
    (define_symbol (name caml_startup__22))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Stack_overflow))
    (define_symbol (name caml_exn_Stack_overflow))
    (symbol_address (address caml_startup__23))
    (cint (value -17))
    (cint (value 3068))
    (define_symbol (name caml_startup__23))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Sys_blocked_io))
    (define_symbol (name caml_exn_Sys_blocked_io))
    (symbol_address (address caml_startup__24))
    (cint (value -19))
    (cint (value 3068))
    (define_symbol (name caml_startup__24))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Assert_failure))
    (define_symbol (name caml_exn_Assert_failure))
    (symbol_address (address caml_startup__25))
    (cint (value -21))
    (cint (value 3068))
    (define_symbol (name caml_startup__25))
    other
    other
    other
    data
    (cint (value 3064))
    (global_symbol (name caml_exn_Undefined_recursive_module))
    (define_symbol (name caml_exn_Undefined_recursive_module))
    (symbol_address (address caml_startup__26))
    (cint (value -23))
    (cint (value 5116))
    (define_symbol (name caml_startup__26))
    other
    other
    other
    data
    (global_symbol (name caml_globals))
    (define_symbol (name caml_globals))
    (symbol_address (address camlMelse__gc_roots))
    (cint (value 0))
    data
    (global_symbol (name caml_data_segments))
    (define_symbol (name caml_data_segments))
    (symbol_address (address caml_startup__data_begin))
    (symbol_address (address caml_startup__data_end))
    (symbol_address (address camlMelse__data_begin))
    (symbol_address (address camlMelse__data_end))
    (cint (value 0))
    data
    (global_symbol (name caml_code_segments))
    (define_symbol (name caml_code_segments))
    (symbol_address (address caml_startup__code_begin))
    (symbol_address (address caml_startup__code_end))
    (symbol_address (address camlMelse__code_begin))
    (symbol_address (address camlMelse__code_end))
    (cint (value 0))
    data
    (global_symbol (name caml_frametable))
    (define_symbol (name caml_frametable))
    (symbol_address (address caml_startup__frametable))
    (symbol_address (address caml_system__frametable))
    (symbol_address (address camlMelse__frametable))
    (cint (value 0)) |}]
;;
