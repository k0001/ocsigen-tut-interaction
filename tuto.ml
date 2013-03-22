open Eliom_content.Html5.D

let main_service =
  Eliom_registration.Html5.register_service
    ~path:[""] ~get_params:Eliom_parameter.unit
    (fun () () ->
      Lwt.return
        (html (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hello"]])))

