open Soup


(* arguments must be valid html, header and footer will be inserted,
   respectively, at the top and bottom of the body *)
let edit_html ?header ?footer ~head_childs html =
  let html = parse html in
  let body = html $ "body" in
  let opt_inserter f o id =
    Option.iter (fun e ->
        f ( parse e $$ id ) ) o
  in
  ignore header;
  opt_inserter
    (fun e -> rev e |> iter (fun e -> prepend_child body e))
    header
    "#header";
  opt_inserter
    (iter (fun e -> append_child body e))
    footer
    "#footer";
  let head = html $ "head" in
  List.iter (fun (id,child) -> 
      opt_inserter
        (iter (fun e -> append_child head e))
        (Some child)
        id)
    head_childs;
  to_string html
