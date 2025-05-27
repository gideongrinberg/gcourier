import gleam/erlang/process
import gleam/hackney
import gleam/http/request
import gleam/io
import shellout

@external(erlang, "priv", "find_bin")
fn find_bin(name: String) -> String

/// Starts an SMTP server that captures outgoing mail and
/// displays it in the browser. By default, the server runs on localhost:1025, 
/// and the UI runs on localhost:8025. It accepts username and password combination.
pub fn dev_server() {
  let assert Ok(req) = request.to("http://localhost:8025")
  let resp = req |> hackney.send
  case resp {
    Error(_) -> {
      let binary = find_bin("mailpit")
      process.start(
        fn() {
          shellout.command(
            run: binary,
            with: ["--smtp-auth-accept-any", "--smtp-auth-allow-insecure"],
            in: ".",
            opt: [],
          )
        },
        True,
      )

      check_live()
      Nil
    }
    Ok(_) -> {
      io.println("Successfully started SMTP server on localhost:1025.")
      io.println("Running web UI at http://localhost:8025.")
      Nil
    }
  }
}

fn check_live() {
  let assert Ok(req) = request.to("http://localhost:8025")
  let resp = req |> hackney.send
  case resp {
    Error(_) -> {
      process.sleep(1000)
      check_live()
    }
    Ok(_) -> {
      True
    }
  }
}
