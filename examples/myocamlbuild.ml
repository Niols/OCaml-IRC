
open Ocamlbuild_plugin
open Command

let () =
  dispatch
    (
      function
      | After_rules ->
         ocaml_lib ~extern:true ~dir:"../../lib" "irc"
      | _ -> ()
    )
(* note: we have to add one more '../' because we will be building
    from _build/ *)
