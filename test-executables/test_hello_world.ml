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
        Some 1
        None |}])
;;

let%expect_test "test minor heap" =
  Deferred.List.iter configs ~f:(fun options ->
      let%bind () =
        run_code
          ~options
          [%str
            let f l r =
              let start = Gc.get_minor_free () in
              let l' = Some l in
              let middle = Gc.get_minor_free () in
              let r' = Some r in
              let end_ = Gc.get_minor_free () in
              Printf.printf "start: %d, middle: %d, end: %d\n" start middle end_;
              l', r'
            ;;

            let () =
              Gc.minor ();
              let (_ : _ * _) = f 100 900 in
              let before_gc = Gc.get_minor_free () in
              Gc.minor ();
              let after_gc = Gc.get_minor_free () in
              Printf.printf "before gc: %d, after gc: %d\n" before_gc after_gc;
              ()
            ;;]
      in
      [%expect
        {|
        start: 262144, middle: 262142, end: 262140
        before gc: 262086, after gc: 262144 |}])
;;

let%expect_test "test minor heap (keep something alive)" =
  Deferred.List.iter configs ~f:(fun options ->
      let%bind () =
        run_code
          ~options
          [%str
            let print_counters () =
              let minor, promoted, major = Gc.counters () in
              Printf.printf
                "minor: %.0f, promoted: %.0f, major: %.0f\n"
                minor
                promoted
                major
            ;;

            let f l r =
              let start = Gc.get_minor_free () in
              let l' = Some l in
              let middle = Gc.get_minor_free () in
              let r' = Some r in
              let end_ = Gc.get_minor_free () in
              Printf.printf "start: %d, middle: %d, end: %d\n" start middle end_;
              print_counters ();
              l', r'
            ;;

            let[@cold] main () =
              Gc.minor ();
              print_counters ();
              (* We should probably have a GC root pointing at x, but we don't. *)
              let x, _ = f 100 900 in
              let before_gc = Gc.get_minor_free () in
              print_counters ();
              Gc.minor ();
              let after_gc = Gc.get_minor_free () in
              print_counters ();
              Printf.printf "before gc: %d, after gc: %d\n" before_gc after_gc;
              (match x with
              | None -> print_endline "x = None"
              | Some x -> Printf.printf "x = %d\n" x);
              print_counters ()
            ;;

            let () = main ()]
      in
      [%expect
        {|
        (* CR expect_test: Collector ran multiple times with different outputs *)
        =========================================================================
        minor: 54, promoted: 54, major: 54
        start: 262050, middle: 262048, end: 262046
        minor: 203, promoted: 54, major: 54
        minor: 300, promoted: 54, major: 54
        minor: 394, promoted: 54, major: 54
        before gc: 261898, after gc: 262144
        x = 2387032
        minor: 542, promoted: 54, major: 54

        =========================================================================
        minor: 54, promoted: 54, major: 54
        start: 262050, middle: 262048, end: 262046
        minor: 203, promoted: 54, major: 54
        minor: 300, promoted: 54, major: 54
        minor: 394, promoted: 56, major: 56
        before gc: 261898, after gc: 262144
        x = 100
        minor: 542, promoted: 56, major: 56 |}])
;;
