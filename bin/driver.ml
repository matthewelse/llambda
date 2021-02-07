let () =
  Llambda.Compat_driver.main ~use_llvm:true ();
  Profile.print Format.std_formatter !Clflags.profile_columns;
  exit 0
;;
