/// This module provides the logic for sending mail using a [`Mailer`](/gcourier/gcourier/types.html#Mailer).
/// As of writing, the library implements only one `Mailer`, which is `SmtpMailer`.
/// 
/// The module also provides access to gcourier's built-in dev server, which intercepts emails and 
/// displays them in the browser.
import envoy
import gcourier/message.{type Message}
import gcourier/transports/smtp.{send_smtp}
import gcourier/types.{type Mailer, SmtpMailer}
import gleam/bytes_tree
import gleam/dict
import gleam/erlang/process
import gleam/hackney
import gleam/http/request
import gleam/io
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import shellout
import simplifile

pub fn send(mailer: Mailer, message: Message) {
  case mailer {
    SmtpMailer(_, _, _, _, _, _) -> {
      send_smtp(mailer, message)
    }
  }
}

pub fn dev_server() {
  case has_mailpit() {
    False -> install_mailpit()
    True -> Nil
  }

  process.start(
    fn() {
      shellout.command(
        run: home_dir() <> "/mailpit",
        with: ["--smtp-auth-allow-insecure", "--smtp-auth-accept-any"],
        in: ".",
        opt: [],
      )
    },
    True,
  )

  io.println(
    "Started SMTP server on :1025, web UI accessible at http://localhost:8025/.\nBy default, the server accepts any auth credentials.",
  )
}

fn has_mailpit() {
  shellout.command(
    run: "test",
    with: ["-e", home_dir() <> "/mailpit"],
    in: ".",
    opt: [],
  )
  |> result.map(with: fn(_) { True })
  |> result.map_error(with: fn(_) { False })
  |> result.unwrap(False)
}

fn install_mailpit() {
  io.println("Installing mailpit.")
  let assert Some(binary) = get_binary_name()

  let url =
    "https://github.com/axllent/mailpit/releases/download/v1.24.2/" <> binary
  let tar = fetch(url).body

  let assert Ok(home) = envoy.get("HOME")
  let home = case string.ends_with(home, "/") {
    False -> home <> "/"
    True -> home
  }
  let tar_path = home_dir() <> "/mailpit.tar.gz"
  simplifile.create_directory(home_dir())
  simplifile.write_bits(tar_path, tar)
  case untar(tar_path, home_dir()) {
    Error(error) -> io.print_error(error)
    Ok(_) -> Nil
  }
}

fn fetch(url: String) {
  let assert Ok(request) = request.to(url)
  let request =
    request.Request(
      method: request.method,
      headers: request.headers,
      body: bytes_tree.from_string(url),
      scheme: request.scheme,
      host: request.host,
      port: request.port,
      path: request.path,
      query: request.query,
    )
  let assert Ok(response) = request |> hackney.send_bits()
  case response.status {
    301 | 302 | 307 | 308 -> {
      let assert Ok(location) =
        dict.from_list(response.headers) |> dict.get("location")

      fetch(location)
    }

    _ -> {
      response
    }
  }
}

fn get_binary_name() {
  let parts = string.split(sys_arch(), "-")
  case parts {
    [arch, vendor, ..] -> {
      case arch, vendor {
        "x86_64", "apple" -> Some("mailpit-darwin-amd64.tar.gz")
        "aarch64", "apple" -> Some("mailpit-darwin-arm64.tar.gz")
        "x86_64", "pc" -> Some("mailpit-windows-amd64.zip")
        "aarch64", "pc" -> Some("mailpit-windows-arm64.zip")
        "x86_64", "unknown" -> Some("mailpit-linux-amd64.tar.gz")
        "i686", "unknown" -> Some("mailpit-linux-386.tar.gz")
        "armv7l", "unknown" -> Some("mailpit-linux-arm.tar.gz")
        "aarch64", "unknown" -> Some("mailpit-linux-arm64.tar.gz")
        _, _ -> None
      }
    }

    _ -> {
      None
    }
  }
}

fn home_dir() {
  let assert Ok(home) = envoy.get("HOME")
  let home = case string.ends_with(home, "/") {
    False -> home <> "/"
    True -> home
  }

  home <> ".gcourier"
}

@external(erlang, "extern", "arch")
fn sys_arch() -> String

@external(erlang, "extern", "untar")
pub fn untar(path: String, dest: String) -> Result(Nil, String)
