open Dream

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (fun _ -> Dream.html "Hello, world! This is a Dream demo!");
  ]
  @@ Dream.not_found

  