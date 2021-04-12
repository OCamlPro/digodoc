open Cmi_format
open Cmt_format
open Format
(*
let pp_pair pp_fst pp_snd fmt (a,b) =
    fprintf fmt "(%a,%a)" pp_fst a pp_snd b

let pp_arg_label fmt arg_label =
    let str = match arg_label with
        | Nolabel -> "Nolabel"
        | Labelled l -> sprintf "Labelled(%s)" l
        | Optional l -> sprintf "Optional(%s)" l
    in
    fprintf fmt "%s" str

let rec pp_field_kind fmt kind =
    match kind with
    | Fvar kindopt -> fprintf fmt "%a" (pp_print_option pp_field_kind) !kindopt
    | Fpresent -> fprintf fmt "Fpresent"
    | Fabsent -> fprintf fmt "Fabsent"

let rec pp_row_fields fmt field = 
    match field with
    | Rpresent texpropt -> 
        fprintf fmt "%a" (pp_print_option pp_type_expr) texpropt
    | Reither (b1,texp_l,b2,fieldopt) ->
        fprintf fmt "(%a,%a,%a,%a)"
            pp_print_bool
            b1
            (pp_print_list
                ~pp_sep:(fun fmt _ -> Format.fprintf fmt " ; ")
                pp_type_expr)
            texp_l
            pp_print_bool
            b2
            (pp_print_option pp_row_fields)
            !fieldopt
    | Rabsent ->
        fprintf fmt "Rabsent"
and pp_type_descr fmt typed = 
    match typed with
    | Tvar opt -> 
        fprintf fmt "Tvar(%a)" (pp_print_option pp_print_string) opt
    | Tarrow (arg_label,texpr1,texpr2,_) -> 
        fprintf fmt "Tarrow(%a,%a,%a)" pp_arg_label arg_label pp_type_expr texpr1 pp_type_expr texpr2 
    | Ttuple texpr_l ->
        fprintf fmt "Ttuple(%a)"
            (pp_print_list
                ~pp_sep:(fun fmt _ -> Format.fprintf fmt " * ")
                pp_type_expr)
            texpr_l
    | Tconstr (path, texpr_l,_) ->
        fprintf fmt "Tconstr(%a,%a)"
            Path.print
            path
            (pp_print_list
                ~pp_sep:(fun fmt _ -> Format.fprintf fmt " ; ")
                pp_type_expr)
            texpr_l
    | Tobject (texpr, class_opt) ->
        fprintf fmt "Tobject(%a,%a)"
            pp_type_expr
            texpr
            (pp_print_option
                (fun fmt (a,b) ->
                    Format.fprintf fmt "(%a,%a)"
                        Path.print
                        a
                        (pp_print_list
                            ~pp_sep:(fun fmt _ -> Format.fprintf fmt " ; ")
                            pp_type_expr)
                        b))
            !class_opt
    | Tfield (name,kind,t,ts) ->
        fprintf fmt "Tfield(%s,%a,%a,%a)"
            name
            pp_field_kind
            kind
            pp_type_expr
            t
            pp_type_expr
            ts
    | Tnil ->
        fprintf fmt "Tnil"
    | Tlink texpr ->
        fprintf fmt "Tlink(%a)"
        pp_type_expr
        texpr
    | Tvariant { row_fields; row_more; row_closed; row_name ; _} ->
        fprintf fmt {|Tvariant {
                row_fields:%a;
                row_more:%a;
                row_closed:%a;
                row_name:%a;
            }|}
            (pp_print_list
                ~pp_sep:(fun fmt _ -> Format.fprintf fmt " ; ")
                (pp_pair pp_print_string pp_row_fields))
            row_fields
            pp_type_expr
            row_more
            pp_print_bool
            row_closed
            (pp_print_option
                (pp_pair
                    Path.print
                    (pp_print_list
                       ~pp_sep:(fun fmt _ -> Format.fprintf fmt " ; ")
                       pp_type_expr)))
            row_name
    | Tunivar varopt ->
        fprintf fmt "Tunivar(%a)" (pp_print_option pp_print_string) varopt
    | Tpoly (texpr,texpr_l) ->
        fprintf fmt "Tpoly(%a,%a)" 
            pp_type_expr
            texpr
            (pp_print_list
                ~pp_sep:(fun fmt _ -> Format.fprintf fmt " ; ")
                pp_type_expr)
            texpr_l
    | Tpackage (path,longid_l, texpr_l) ->
        fprintf fmt "Tpackage(%a,%a,%a)"
            Path.print
            path
            Longident
    | _ -> "!A FAIRE!"

and pp_type_expr fmt typee =
    Format.fprintf fmt "%a" pp_type_descr typee.desc

let pp_ident fmt ident =
    Format.fprintf fmt "%s" (Ident.name ident)

let pp_signature_item fmt signature_item =
    match signature_item with 
    | Sig_value*)

let pp_pair pp_fst pp_snd fmt (a,b) =
    fprintf fmt "(%a,%a)" pp_fst a pp_snd b

let pp_list pp_elt fmt l =
    Format.fprintf fmt "[%a]"
        (pp_print_list 
            ~pp_sep:(fun fmt _ -> Format.fprintf fmt "; ") 
            pp_elt) 
        l

let pp_array pp_elt fmt a =
    pp_list pp_elt fmt (Array.to_list a)

let pp_cmi_infos fmt {cmi_name; cmi_sign; cmi_crcs; _} =
    Format.fprintf fmt 
        {|{
            cmi_name = %a;
            cmi_sign =  %a;
            cmi_crcs = %a;
            cmi_flags = ..
        }|}  
        pp_print_string
        cmi_name
        Printtyp.signature
        cmi_sign
        (pp_print_list
            ~pp_sep:(fun fmt _ ->  Format.fprintf fmt "\n")
            (pp_pair
                pp_print_string
                (pp_print_option
                    (fun fmt digist -> 
                        Format.fprintf fmt "%s" (Digest.to_hex digist)))))
        cmi_crcs

let pp_binary_annots fmt ba =
    match ba with
    | Packed (signature,liste) ->
        fprintf fmt "Packed(%a,%a)"
            Printtyp.signature
            signature
            (pp_print_list
                ~pp_sep:(fun fmt _ -> Format.fprintf fmt ";")
                pp_print_string)
            liste
    | Implementation structure ->
        fprintf fmt "Implementation(%a)"
            Printtyped.implementation
            structure
    | Interface signature ->
        fprintf fmt "Interface(%a)"
            Printtyped.interface
            signature
    | _ -> fprintf fmt "A FAIRE"

let pp_cmt_infos fmt cmt_info =
    let pp_value_description fmt vd =
        Format.fprintf fmt "%a" Printtyp.type_expr vd.Types.val_type
    in 
    Format.fprintf fmt 
        {|{
            cmi_modname = %s;
            cmt_annots = %a;
            cmt_value_dependencies = %a;
            cmt_comments = %a;
            cmt_args = %a;
            cmt_sourcefile = %a;
            cmt_loadpath = %a;
            cmt_source_digest = %a;
            cmt_imports = %a;
            cmt_interface_digest = %a;
            cmt_use_summaries = %a
        }|}
        cmt_info.cmt_modname
        pp_binary_annots
        cmt_info.cmt_annots
        (pp_list
            (pp_pair
                pp_value_description
                pp_value_description))
        cmt_info.cmt_value_dependencies
        (pp_list
            (pp_pair
                pp_print_string
                Location.print_loc))
        cmt_info.cmt_comments
        (pp_array pp_print_string)
        cmt_info.cmt_args
        (pp_print_option pp_print_string)
        cmt_info.cmt_sourcefile
        (pp_list pp_print_string)
        cmt_info.cmt_loadpath
        (pp_print_option 
            (fun fmt digist -> 
                        Format.fprintf fmt "%s" (Digest.to_hex digist)))
        cmt_info.cmt_source_digest
        (pp_list
            (pp_pair
                pp_print_string
                (pp_print_option
                    (fun fmt digist -> 
                        Format.fprintf fmt "%s" (Digest.to_hex digist)))))
        cmt_info.cmt_imports
        (pp_print_option
            (fun fmt digist -> 
                Format.fprintf fmt "%s" (Digest.to_hex digist)))
        cmt_info.cmt_interface_digest
        pp_print_bool
        cmt_info.cmt_use_summaries
        



        


