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

let%expect_test "compile and run hello world" =
  Deferred.List.iter configs ~f:(fun options ->
      let%bind () = run_code ~options [%str let () = print_endline "hello, world!"] in
      [%expect {| hello, world! |}])
;;

let%expect_test "compile and run for loop" =
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

let%expect_test "compile and run function call" =
  Deferred.List.iter configs ~f:(fun options ->
      let%bind () =
        run_code
          ~options
          [%str
            let f x = 10 * x

            let () =
              for i = 0 to 10 do
                print_int (f i);
                print_char ' '
              done;
              print_endline ""
            ;;]
      in
      [%expect {| 0 10 20 30 40 50 60 70 80 90 100 |}])
;;
