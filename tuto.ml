open Eliom_content.Html5.D


(******************************************************************************)
(* Services *)

let main_service = Eliom_service.service ~path:[""]
  ~get_params:Eliom_parameter.unit ()

let user_service = Eliom_service.service ~path:["users"]
  ~get_params:Eliom_parameter.(suffix (string "name")) ()

let connection_service = Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()

let disconnection_service = Eliom_service.void_coservice'

(******************************************************************************)
(* Users db *)

let users = ref [("Calvin", "123"); ("Hobbes", "456")]

let user_links () =
  let f (name,_) = li [a ~service:user_service [pcdata name] name] in
  ul (List.map f !users)

let check_pwd name pwd =
  try List.assoc name !users = pwd with Not_found -> false

(******************************************************************************)
(* Session data *)

let username = Eliom_reference.eref
  ~scope:Eliom_common.default_session_scope
  ~persistent:"username" None


(******************************************************************************)
(* Forms *)

let connection_box () =
  lwt u = Eliom_reference.get username in
  Lwt.return
    (match u with
    | Some s -> p [pcdata "You are connected as "; pcdata s]
    | None ->
      post_form ~service:connection_service
        (fun (name1, name2) ->
          [fieldset
              [label ~a:[a_for name1] [pcdata "Login: "];
               string_input ~input_type:`Text ~name:name1 ();
               br ();
               label ~a:[a_for name2] [pcdata "Password: "];
               string_input ~input_type:`Password ~name:name2 ();
               br ();
               string_input ~input_type:`Submit ~value:"Connect" ()]]) ())

let disconnection_box =
  post_form disconnection_service
    (fun _ -> [p [string_input ~input_type:`Submit ~value:"Log out" ()]]) ()


(******************************************************************************)
(* Eliom registration *)


let _ =
  Eliom_registration.Html5.register
    ~service:main_service
    (fun () () ->
      lwt cf = connection_box () in
    Lwt.return
      (html (head (title (pcdata "")) [])
         (body [h1 [pcdata "Hello"];
                cf;
                user_links ()])));

  Eliom_registration.Html5.register
    ~service:user_service
    (fun name () ->
      lwt cf = connection_box () in
    Lwt.return
      (html (head (title (pcdata name)) [])
         (body [h1 [pcdata name];
                cf;
                p [a ~service:main_service [pcdata "Home"] ()]]))) ;

  Eliom_registration.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      if check_pwd name password
      then Eliom_reference.set username (Some name)
      else Lwt.return ())


