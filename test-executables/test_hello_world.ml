open Core
open Async
open Ppxlib

let loc = Location.none
let configs = [ `Llvm "-O0"; `Llvm "-O1"; `Llvm "-O2"; `Llvm "-O3"; `Ocamlopt ]

let run_code ~options code =
  let%bind pwd = Unix.getcwd () in
  Expect_test_helpers_async.within_temp_dir (fun () ->
      let filename = "test.ml" in
      let contents = Ppxlib.Pprintast.string_of_structure code in
      Out_channel.write_all filename ~data:contents;
      let%bind () =
        match options with
        | `Llvm llvm_flags ->
          Expect_test_helpers_async.run
            (pwd ^ "/../bin/driver.exe")
            (List.concat [ [ "-llvm-flags"; llvm_flags ]; [ filename ] ])
        | `Ocamlopt ->
          Expect_test_helpers_async.run "ocamlopt" (List.concat [ [ filename ] ])
      in
      Expect_test_helpers_async.run "./a.out" [])
;;

let%expect_test "run hello world" =
  Deferred.List.iter configs ~f:(fun options ->
      let%bind () = run_code ~options [%str let () = print_endline "hello, world!"] in
      [%expect {| hello, world! |}])
;;

let%expect_test "run for loop" =
  Deferred.List.iter configs ~f:(fun options ->
      let%bind () =
        run_code
          ~options
          [%str
            let () =
              for i = 0 to 10 do
                print_int i;
                print_char ' '
              done;
              print_endline ""
            ;;]
      in
      [%expect {| 0 1 2 3 4 5 6 7 8 9 10 |}])
;;

let%expect_test "run function call" =
  Deferred.List.iter configs ~f:(fun options ->
      let%bind () = run_code ~options [%str let f x = 10 * x
                                            let () = print_int (f 99)] in
      [%expect {| 990 |}])
;;

let%expect_test "recursive function" =
  Deferred.List.iter configs ~f:(fun options ->
      let%bind () =
        run_code
          ~options
          [%str let rec f x = if x < 0 then 0 else x + f (x - 1)
                let () = print_int (f 99)]
      in
      [%expect {| 4950 |}])
;;

let%expect_test "match statement" =
  Deferred.List.iter configs ~f:(fun options ->
      let%bind () =
        run_code
          ~options
          [%str
            (* Return 0, rather than none here to just test pattern matching, rather than allocations. *)
            let hd (x : int list) = match x with x :: _ -> x | _ -> 0

            let () =
              print_int (hd [ 1; 2; 3; 4 ]);
              print_char '\n';
              print_int (hd [])
            ;;]
      in
      [%expect {|
        1
        0 |}])
;;

let%expect_test "allocations" =
  Deferred.List.iter configs ~f:(fun options ->
      let%bind () =
        run_code
          ~options
          [%str
            (* Return 0, rather than none here to just test pattern matching, rather than allocations. *)
            let hd (x : 'a list) : 'a option =
              match x with x :: _ -> Some x | [] -> None
            ;;

            let print_int_option t =
              match t with
              | None -> print_endline "None"
              | Some x ->
                print_string "Some ";
                print_int x;
                print_char '\n'
            ;;

            let () =
              print_int_option (hd [ 1; 2; 3; 4 ]);
              print_int_option (hd [])
            ;;]
      in
      [%expect {|
        (* CR expect_test: Collector ran multiple times with different outputs *)
        =========================================================================
        ("Unclean exit" (Exit_non_zero 2))
        --- STDERR ---
        /home/melse/development/llambda/external/llvm/llvm-project/build/bin/opt: /tmp/build12349d.dune/camlasmfdf4a0.ll:129:3: error: instruction expected to be numbered '%8'
          %7 = load i8*, i8** %alloc_ptr, align 8
          ^
        [1mFile "test.ml", line 1[0m:
        [1;31mError[0m: Assembler error, input left in file /tmp/build12349d.dune/camlasmfdf4a0.ll
        ("Process creation failed"
          (prog ./a.out)
          (args ())
          (error (
            Unix.Unix_error
            "No such file or directory"
            Core.Unix.create_process
            "((prog ./a.out) (args ()) (env (Extend ((OCAMLRUNPARAM b=0)))))")))

        =========================================================================
        ("Unclean exit" (Exit_non_zero 2))
        --- STDERR ---
        /home/melse/development/llambda/external/llvm/llvm-project/build/bin/opt: /tmp/build12349d.dune/camlasmfe68ea.ll:129:3: error: instruction expected to be numbered '%8'
          %7 = load i8*, i8** %alloc_ptr, align 8
          ^
        [1mFile "test.ml", line 1[0m:
        [1;31mError[0m: Assembler error, input left in file /tmp/build12349d.dune/camlasmfe68ea.ll
        ("Process creation failed"
          (prog ./a.out)
          (args ())
          (error (
            Unix.Unix_error
            "No such file or directory"
            Core.Unix.create_process
            "((prog ./a.out) (args ()) (env (Extend ((OCAMLRUNPARAM b=0)))))")))

        =========================================================================
        ("Unclean exit" (Exit_non_zero 2))
        --- STDERR ---
        /home/melse/development/llambda/external/llvm/llvm-project/build/bin/opt: /tmp/build12349d.dune/camlasm04a3fe.ll:129:3: error: instruction expected to be numbered '%8'
          %7 = load i8*, i8** %alloc_ptr, align 8
          ^
        [1mFile "test.ml", line 1[0m:
        [1;31mError[0m: Assembler error, input left in file /tmp/build12349d.dune/camlasm04a3fe.ll
        ("Process creation failed"
          (prog ./a.out)
          (args ())
          (error (
            Unix.Unix_error
            "No such file or directory"
            Core.Unix.create_process
            "((prog ./a.out) (args ()) (env (Extend ((OCAMLRUNPARAM b=0)))))")))

        =========================================================================
        ("Unclean exit" (Exit_non_zero 2))
        --- STDERR ---
        /home/melse/development/llambda/external/llvm/llvm-project/build/bin/opt: /tmp/build12349d.dune/camlasmf2a959.ll:129:3: error: instruction expected to be numbered '%8'
          %7 = load i8*, i8** %alloc_ptr, align 8
          ^
        [1mFile "test.ml", line 1[0m:
        [1;31mError[0m: Assembler error, input left in file /tmp/build12349d.dune/camlasmf2a959.ll
        ("Process creation failed"
          (prog ./a.out)
          (args ())
          (error (
            Unix.Unix_error
            "No such file or directory"
            Core.Unix.create_process
            "((prog ./a.out) (args ()) (env (Extend ((OCAMLRUNPARAM b=0)))))")))

        =========================================================================
        Some 1
        None |}])
;;
