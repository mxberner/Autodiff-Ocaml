let () =
  let open Oplot.Plt in
  let p = plot sin (-2.) 20. in
  let a = axis 0. 0. in

  let output = "oplot.fig" in
  display ~dev:fig ~output [ Color red; p; Color black; a ];
  assert (Sys.file_exists output)