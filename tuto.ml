open Eliom_content.Html5.D

let main_service = Eliom_service.service ~path:[""]
  ~get_params:Eliom_parameter.unit ()

let user_service = Eliom_service.service ~path:["users"]
  ~get_params:Eliom_parameter.(suffix (string "name")) ()

(* User names and passwords: *)
let users = ref [("Calvin", "123"); ("Hobbes", "456")]

let user_links () =
  let f (name,_) = li [a ~service:user_service [pcdata name] name] in
  ul (List.map f !users)

let _ =
  Eliom_registration.Html5.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (html (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hello"];
                  user_links ()])));

  Eliom_registration.Html5.register
    ~service:user_service
    (fun name () ->
      Lwt.return
        (html (head (title (pcdata name)) [])
           (body [h1 [pcdata name];
                  p [a ~service:main_service [pcdata "Home"] ()]])))

let connection_service = Eliom_service.post_service
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    ()
