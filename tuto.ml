open Eliom_content.Html5.D


(******************************************************************************)
(* Services *)

let main_service = Eliom_service.service
  ~path:[""]
  ~get_params:Eliom_parameter.unit ()

let user_service = Eliom_service.service
  ~path:["users"]
  ~get_params:Eliom_parameter.(suffix (string "name")) ()

let connection_service = Eliom_service.post_coservice'
  ~post_params:Eliom_parameter.(string "name" ** string "password") ()

let disconnection_service = Eliom_service.post_coservice'
  ~post_params:Eliom_parameter.unit ()

let new_user_form_service = Eliom_service.service
  ~path:["registration"]
  ~get_params:Eliom_parameter.unit ()

let account_confirmation_service = Eliom_service.post_coservice
  ~fallback:new_user_form_service
  ~post_params:Eliom_parameter.(string "name" ** string "password") ()


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

(* Request data *)
let wrong_pwd = Eliom_reference.eref
  ~scope:Eliom_common.request_scope false


(******************************************************************************)
(* Forms *)

let disconnection_box () =
  post_form disconnection_service
    (fun _ -> [p [string_input ~input_type:`Submit ~value:"Log out" ()]]) ()

let connection_box () =
  lwt u = Eliom_reference.get username in
  lwt wp = Eliom_reference.get wrong_pwd in
  Lwt.return
    (match u with
    | Some s -> div [p [pcdata "You are connected as "; pcdata s];
                     disconnection_box ()]
    | None ->
      let l =
        [post_form ~service:connection_service
            (fun (name1, name2) ->
              [fieldset
                  [label ~a:[a_for name1] [pcdata "Login: "];
                   string_input ~input_type:`Text ~name:name1 ();
                   br ();
                   label ~a:[a_for name2] [pcdata "Password: "];
                   string_input ~input_type:`Password ~name:name2 ();
                   br ();
                   string_input ~input_type:`Submit ~value:"Connect" ()]]) ();
         p [a new_user_form_service [pcdata "Create an account"] ()]] in
      div (if wp
           then (p [em [pcdata "Wrong user or password"]])::l
           else l))

let account_form =
  post_form ~service:account_confirmation_service
    (fun (name1, name2) ->
      [fieldset
          [label ~a:[a_for name1] [pcdata "Login: "];
           string_input ~input_type:`Text ~name:name1 ();
           br ();
           label ~a:[a_for name2] [pcdata "Password: "];
           string_input ~input_type:`Password ~name:name2 ();
           br ();
           string_input ~input_type:`Submit ~value:"Connect" ()]]) ()

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

  Eliom_registration.Any.register
    ~service:user_service
    (fun name () ->
      if List.exists (fun (n,_) -> n = name) !users
      then begin
        lwt cf = connection_box () in
           Eliom_registration.Html5.send
             (html (head (title (pcdata name)) [])
                (body [h1 [pcdata name];
                       cf;
                       p [a ~service:main_service [pcdata "Home"] ()]])) end
      else Eliom_registration.Html5.send
         ~code:404
         (html (head (title (pcdata "404")) [])
            (body [h1 [pcdata "404"];
                   p [pcdata "That page does not exist"]]))) ;

  Eliom_registration.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      if check_pwd name password
      then Eliom_reference.set username (Some name)
      else Eliom_reference.set wrong_pwd true);

  Eliom_registration.Action.register
    ~service:disconnection_service
    (fun () () -> Eliom_state.discard
      ~scope:Eliom_common.default_session_scope ()) ;

  Eliom_registration.Html5.register
    ~service:new_user_form_service
    (fun () () ->
      Lwt.return
        (html (head (title (pcdata "")) [])
           (body [h1 [pcdata "Create an account"];
                  account_form]))) ;

  Eliom_registration.Html5.register
    ~service:account_confirmation_service
    (fun () (name, pwd) ->
      let create_account_service =
        Eliom_registration.Redirection.register_coservice
          ~fallback:main_service
          ~get_params:Eliom_parameter.unit
          ~timeout:60.
          (fun () () ->
            users := (name, pwd) :: !users;
            Lwt.bind
              (Eliom_reference.set username (Some name))
              (fun _ ->
                let s = Eliom_service.preapply ~service:user_service name in
                Lwt.return s)) in
      Lwt.return
        (html (head (title (pcdata "")) [])
           (body
              [h1 [pcdata "Confirm account creation for "; pcdata name];
               p [a ~service:create_account_service [pcdata "Yes"] ();
                  pcdata " ";
                  a ~service:main_service [pcdata "No"] ()]])));



