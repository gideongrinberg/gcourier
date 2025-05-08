import gcourier/message.{type Message}
import gcourier/types.{type Mailer, SmtpMailer}
import gleam/bit_array
import gleam/list
import gleam/string
import mug

pub fn send_smtp(mailer: Mailer, msg: Message) {
  let socket = connect_smtp(mailer)
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
}

fn socket_send_checked(socket: mug.Socket, value: String) {
  let assert Ok(_) = socket_send(socket, value)
  Nil
}

fn socket_send(socket: mug.Socket, value: String) {
  mug.send(socket, <<{ value <> "\r\n" }:utf8>>)
}

fn socket_receive(socket: mug.Socket) {
  let assert Ok(packet) = mug.receive(socket, 500)
  let assert Ok(resp) = bit_array.to_string(packet)
  resp
}

fn connect_smtp(mailer: Mailer) {
  let SmtpMailer(domain, port, username, password, ssl, auth) = mailer

  let assert Ok(socket) =
    mug.new(domain, port)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()

  let resp = socket_receive(socket)
  let assert Ok(_) = case string.contains(resp, "ESMTP") {
    True -> socket_send(socket, "EHLO " <> mailer.domain)
    False -> socket_send(socket, "HELO " <> mailer.domain)
  }

  let helo_resp = socket_receive(socket)
  // TODO check for extensions
  case mailer.ssl {
    False -> Nil
    True -> todo
  }

  case mailer.auth {
    False -> Nil
    True -> todo
  }

  socket
}
