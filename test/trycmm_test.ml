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
    (fundecl (fun_name caml__entry)) |}]
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

      0a)(seq
           (let
             (clos/86
                (closure
                  (fun caml__sum_80:int 1  x/81
                    (if x/81
                      (+ (field 0 x/81) (apply* caml__sum_80  (field 1 x/81))) 0)) ))
             (setfield_ptr(root-init) 0 (read_symbol camlMelse)
               (offset clos/86 0)))
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
    (define_symbol (name caml__2))
    (symbol_address (address caml__sum_80))
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
    (fundecl (fun_name caml__sum_80))
    (fundecl (fun_name caml__entry)) |}]
;;
