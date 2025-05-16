/// This module provides the logic for sending mail using a [`Mailer`](/gcourier/gcourier/types.html#Mailer).
/// As of writing, the library implements only one `Mailer`, which is `SmtpMailer`.
import gcourier/message.{type Message}
import gleam/bit_array
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import mug

type Mailer {
  SmtpMailer(
    host: String,
    port: Int,
    username: String,
    password: String,
    auth: Bool,
  )
}

pub fn send(
  host: String,
  port: Int,
  auth: Option(#(String, String)),
  message: Message,
) {
  let mailer = case auth {
    Some(#(username, password)) ->
      SmtpMailer(host:, port:, username:, password:, auth: True)
    None -> SmtpMailer(host:, port:, username: "", password: "", auth: False)
  }

  send_smtp(mailer, message)
}

fn send_smtp(mailer: Mailer, msg: Message) {
  let socket =
    connect_smtp(SmtpMailer(
      host: mailer.host,
      port: mailer.port,
      username: mailer.username,
      password: mailer.password,
      auth: mailer.auth,
    ))
  let from_cmd = "MAIL FROM:<" <> mailer.username <> ">"
  socket_send_checked(socket, from_cmd)
  socket_receive(socket)

  list.map(msg.to, fn(r) {
    let to_cmd = "RCPT TO:<" <> r <> ">"
    socket_send_checked(socket, to_cmd)
    socket_receive(socket)
  })

  socket_send_checked(socket, "DATA")
  socket_receive(socket)

  socket_send_checked(socket, message.render(msg))
  socket_receive(socket)

  socket_send_checked(socket, "QUIT")
  socket_receive(socket)
  Nil
}

fn socket_send_checked(socket: mug.Socket, value: String) {
  let assert Ok(_) = socket_send(socket, value)
  Nil
}

fn socket_send(socket: mug.Socket, value: String) {
  mug.send(socket, <<{ value <> "\r\n" }:utf8>>)
}

fn socket_receive(socket: mug.Socket) {
  let assert Ok(packet) = mug.receive(socket, 5000)
  let assert Ok(resp) = bit_array.to_string(packet)
  resp
}

fn connect_smtp(mailer: Mailer) {
  let assert Ok(socket) =
    mug.new(mailer.host, mailer.port)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()

  let resp = socket_receive(socket)
  let assert Ok(_) = case string.contains(resp, "ESMTP") {
    True -> socket_send(socket, "EHLO " <> mailer.host)
    False -> socket_send(socket, "HELO " <> mailer.host)
  }

  let helo_resp = socket_receive(socket)
  let #(helo_resp, socket) = case string.contains(helo_resp, "STARTTLS") {
    False -> #(helo_resp, socket)
    True -> {
      socket_send_checked(socket, "STARTTLS")
      let assert Ok(_) = mug.receive(socket, 5000)
      let assert Ok(socket) =
        mug.upgrade(socket, mug.DangerouslyDisableVerification, 10_000)
      socket_send_checked(socket, "EHLO " <> mailer.host)
      #(socket_receive(socket), socket)
    }
  }

  echo helo_resp

  case mailer.auth {
    False -> Nil
    True -> auth_user(socket, mailer, helo_resp)
  }

  socket
}

fn auth_user(socket: mug.Socket, mailer: Mailer, helo_resp: String) {
  case string.contains(helo_resp, "AUTH") {
    False -> {
      io.println_error(
        "SMTP server does not support authentication. Proceeding as unauthenticated user.",
      )
    }
    True -> {
      socket_send_checked(socket, "AUTH LOGIN")
      socket_receive(socket)
      // todo: check resp
      socket_send_checked(
        socket,
        mailer.username
          |> bit_array.from_string()
          |> bit_array.base64_encode(True),
      )

      socket_receive(socket)
      socket_send_checked(
        socket,
        mailer.password
          |> bit_array.from_string()
          |> bit_array.base64_encode(True),
      )
      socket_receive(socket)
      Nil
    }
  }

  Nil
}
