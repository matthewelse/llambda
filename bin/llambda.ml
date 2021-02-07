open Core

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Try to compile an OCaml file to LLVM IR"
    [%map_open
      let filename = anon ("filename" %: string)
      and dcmm = flag "-dcmm" no_arg ~doc:"dump c-- IR" in
      fun () ->
        let source_code = In_channel.read_all filename in
        let cmm = Llambda.Trycmm.cmm_of_source ~dump_cmm:dcmm source_code in
        Llambda.Emit_llvm.emit_llvm cmm]
;;

let () = Command.run command
