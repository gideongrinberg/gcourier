import gcourier/mug/mug
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/time/calendar
import gleam/time/timestamp
import youid/uuid

// messages ---------------------------------------------------------------------

pub type Attachment {
  Attachment(name: String, content_type: String, content: String)
}

pub opaque type Message {
  /// primary_recipient ***HAS*** to contain at least one 
  /// done in `new_message`
  Simple(data: MessageData)
  MultiPart(data: MessageData, attachments: List(Attachment))
}

type MessageData {
  MessageData(
    // Required
    from: Address,
    content: Content,
    // Optional 
    subject: Option(String),
    content_type_override: Option(String),
    to: List(Recipient),
    cc: List(Recipient),
    bcc: List(Recipient),
    timestamp: Option(timestamp.Timestamp),
    sender: Option(Address),
    custom_headers: List(CustomHeader),
  )
}

/// create a new message
///
/// the IMF (rfc5322) doesn't require a recipient 
/// but since SMTP (rfc5321) does, we also do
///
pub fn new_message(from from: Address, to recipient: Address) -> Message {
  Simple(
    data: MessageData(
      from:,
      content: Text(""),
      subject: None,
      to: [To(recipient)],
      cc: [],
      bcc: [],
      timestamp: None,
      content_type_override: None,
      sender: None,
      custom_headers: [],
    ),
  )
}

/// Render a `Message` into *I*nternet *M*essage *F*ormat (*IMF* rfc5322) compliant form
///
pub fn render(message: Message) -> String {
  case message {
    Simple(data:) -> render_single(data)
    MultiPart(data:, attachments:) -> render_multipart(data, attachments)
  }
}

fn render_single(message: MessageData) -> String {
  let headers = format_headers(message)

  headers <> "\r\n" <> message.content.text
}

fn render_multipart(
  message: MessageData,
  attachments: List(Attachment),
) -> String {
  let boundary = uuid.v4_string()

  let message =
    MessageData(
      ..message,
      content_type_override: Some(
        "multipart/mixed; boundary=\"" <> boundary <> "\"",
      ),
    )

  let content =
    "--"
    <> boundary
    <> "\r\nContent-Type: "
    <> content_type(message.content)
    <> "\r\n\r\n"
    <> message.content.text
    <> "\r\n"
    <> {
      list.map(list.reverse(attachments), fn(a) {
        render_attachment(boundary, a)
      })
      |> string.join(with: "\r\n")
    }
    <> "\r\n--"
    <> boundary
    <> "--\r\n"

  let headers = format_headers(message)

  headers <> "\r\n" <> content
}

fn render_attachment(boundary: String, attachment: Attachment) -> String {
  "--"
  <> boundary
  <> "\r\nContent-Type: "
  <> attachment.content_type
  <> "\r\nContent-Disposition: "
  <> "attachment; filename=\""
  <> attachment.name
  <> "\"\r\n"
  <> "Content-Transfer-Encoding: base64\r\n\r\n"
  <> attachment.content
}

fn format_headers(message: MessageData) -> String {
  let with_key = fn(key, value) { key <> ": " <> value <> "\r\n" }

  let optional_with_key = fn(key, optional_value) {
    case optional_value {
      Some(value) -> with_key(key, value)
      None -> ""
    }
  }

  let format_recipients = fn(key: String, list: List(Recipient)) -> String {
    case list {
      [] -> ""
      [_, ..] ->
        key
        <> ": "
        <> list.map(list, fn(recipient) { format_address(recipient.address) })
        |> string.join(", ")
        <> "\r\n"
    }
  }

  let format_custom_headers = fn(headers: List(CustomHeader)) {
    list.fold(headers, "", fn(acc, header) {
      header.name <> ": " <> header.value <> "\r\n" <> acc
    })
  }

  let _ =
    with_key(
      "Date",
      message.timestamp
        |> option.unwrap(timestamp.system_time())
        |> date_from_timestamp(),
    )
    <> with_key("From", format_address(message.from))
    <> format_recipients("To", message.to)
    <> optional_with_key("Sender", message.sender |> option.map(format_address))
    <> format_recipients("Cc", message.cc)
    <> optional_with_key("Subject", message.subject)
    <> with_key(
      "Content-Type",
      message.content_type_override
        |> option.unwrap(message.content |> content_type()),
    )
    <> format_custom_headers(message.custom_headers)
}

// Recipients & Address ---------------------------------------------------------

pub type Address {
  Address(name: Option(String), address: String)
}

fn format_address(address: Address) -> String {
  let Address(name:, address:) = address

  case name {
    Some(name) -> {
      name <> " <" <> address <> ">"
    }
    None -> address
  }
}

pub type Recipient {
  To(address: Address)
  Cc(address: Address)
  Bcc(address: Address)
}

/// Add the provided address to the list of recipients.
/// 
/// recipient_type should be one of To, Cc, or Bcc.
///
/// We implement the first variant of bcc handling layed out in [RFC 5322](https://www.rfc-editor.org/rfc/rfc5322#section-3.6.3)
/// i.e.: The bcc header does not end up in the actual message
/// if you need one of the other implementations, feel free to [open an issue](https://github.com/gideongrinberg/gcourier/issues)
///
pub fn add_recipient(message: Message, recipient: Recipient) -> Message {
  case recipient {
    To(_) -> MessageData(..message.data, to: [recipient, ..message.data.to])
    Cc(_) -> MessageData(..message.data, cc: [recipient, ..message.data.cc])
    Bcc(_) -> MessageData(..message.data, bcc: [recipient, ..message.data.bcc])
  }
  |> update_message_data(message, _)
}

/// Set the _optional_ sender header. 
/// 
/// This field is useful when the email is sent on behalf of 
/// a third party or there are multiple emails in the FROM field.
pub fn set_sender(message: Message, sender: Address) -> Message {
  update_message_data(
    message,
    MessageData(..message.data, sender: Some(sender)),
  )
}

// Content ----------------------------------------------------------------------

/// set the content of a message
///
/// there can only be one content set. i.e. a second usage of this function will overwrite the first
///
pub fn set_content(message: Message, content: Content) -> Message {
  update_message_data(message, MessageData(..message.data, content:))
}

pub type Content {
  Text(text: String)
  Html(text: String)
}

fn content_type(content: Content) -> String {
  case content {
    Text(text: _) -> "text/plain"
    Html(text: _) -> "text/html"
  }
}

/// Add an attachment to a message
///
pub fn add_attachment(
  message: Message,
  content: BitArray,
  name: String,
  content_type: String,
) -> Message {
  let content = bit_array.base64_encode(content, False)

  let attachment = Attachment(name:, content_type:, content:)

  case message {
    Simple(data:) -> MultiPart(data:, attachments: [attachment])
    MultiPart(data:, attachments:) ->
      MultiPart(data:, attachments: [attachment, ..attachments])
  }
}

// Misc headers -----------------------------------------------------------------

/// Set the message's subject line. Optional.
///
pub fn set_subject(message: Message, subject: String) -> Message {
  update_message_data(
    message,
    MessageData(..message.data, subject: Some(subject)),
  )
}

/// Set the Date header for the email. Optional.
///
/// If this is not explicitly set, the current system time will be used automatically
/// when the message is sent. This header indicates when the email was created.
///
pub fn set_timestamp(
  message: Message,
  timestamp: timestamp.Timestamp,
) -> Message {
  update_message_data(
    message,
    MessageData(..message.data, timestamp: Some(timestamp)),
  )
}

// Custom headers ---------------------------------------------------------------

pub type CustomHeader {
  CustomHeader(name: String, value: String)
}

/// Add a non-standard header
///
pub fn add_custom_header(message: Message, header: CustomHeader) -> Message {
  update_message_data(
    message,
    MessageData(..message.data, custom_headers: [
      header,
      ..message.data.custom_headers
    ]),
  )
}

// Utility functions ------------------------------------------------------------

fn update_message_data(message: Message, data: MessageData) -> Message {
  case message {
    Simple(..) -> Simple(data:)
    MultiPart(..) -> MultiPart(..message, data:)
  }
}

fn date_from_timestamp(timestamp: timestamp.Timestamp) -> String {
  let #(cal, time) = timestamp |> timestamp.to_calendar(calendar.utc_offset)

  let month = {
    calendar.month_to_string(cal.month) |> string.slice(0, 3)
  }

  day_of_week(cal.day, calendar.month_to_int(cal.month), cal.year)
  <> ", "
  <> int.to_string(cal.day)
  <> " "
  <> month
  <> " "
  <> int.to_string(cal.year)
  <> " "
  <> int.to_string(time.hours)
  <> ":"
  <> int.to_string(time.minutes)
  <> ":"
  <> int.to_string(time.seconds)
  // since we always use UTC
  <> " +0000"
}

pub fn day_of_week(day q: Int, month m: Int, year y: Int) -> String {
  let y = case m < 3 {
    True -> y - 1
    False -> y
  }

  let m = case m < 3 {
    True -> m + 12
    False -> m
  }

  let k = y % 100
  let j = y / 100

  let h = q + { 13 * { m + 1 } / 5 } + k + k / 4 + j / 4 + 5 * j
  case h % 7 {
    0 -> "Sat"
    1 -> "Sun"
    2 -> "Mon"
    3 -> "Tue"
    4 -> "Wed"
    5 -> "Thu"
    _ -> "Fri"
  }
}

// mailer agnostic --------------------------------------------------------------

/// All errors that may occur
///
pub type Error {
  /// There was an issue with the SMTP connection
  ///
  SmtpError(SmtpError)
  /// There was an issue with the underlying socket
  ///
  SocketError(SocketError)
  /// We failed to send the message, and something went wrong while attempting to reset the transaction
  ///
  ResetError(ResetError)

  /// We failed to send the message, but resetting the transaction went Ok
  /// i.e. the `Mailer` should still be usable
  /// if you got a different error, you'll probably need to recreate the `Mailer`
  ///
  FailedToSendButResetOk
}

/// Errors related to SMTP processes
/// Like unexpected response codes, the server lacking features or issues during a transaction
///
pub type SmtpError {
  // only happens during initial connection
  /// You supplied credentials, but the server doesnt support authentication
  ///
  ServerDoesntSupportAuthentication

  /// The SMTP server doesnt support TLS
  /// And you supplied `RejectNonTls` as your `TlsStance`
  ///
  ServerDoesntSupportTls

  // happens during sending 
  /// `MAIL FROM:` got a response that wasnt the expected `250 OK`
  ///
  FailedToSendStart(response: String)
  /// `DATA` got non 354 response
  ///
  NotAllowedToSendData(response: String)

  /// finishing `<CRLF>.<CRLF>` got a non `250 OK` response
  /// 
  /// i.e. the connection is in an odd state and should probably be recreated
  ///
  FailedToFinishTransaction(response: String)
}

/// Issues with the underlying TCP socket
/// i.e. Errors while connecting, upgrading to TLS, sending or recieving
///
pub type SocketError {

  // only happens during startup
  /// The underlying socket failed to connect
  ///
  FailedToConnect(mug.Error)

  /// Failed to upgrade the underlying socket to TLS
  ///
  FailedToUpgrade(mug.Error)

  // only happens during `stop`
  /// The underlying socket failed to close
  ///
  FailedToClose(mug.Error)

  // primarily during message sending
  /// An error occured while receiving a message from the socket
  ///
  FailedToReceive(mug.Error)
  /// The received content wasn't UTF-8
  ///
  InvalidUtf8Response(BitArray)
  /// Something went wrong while sending a packet
  ///
  FailedToSend(failed_to_send: String, with_error: mug.Error)
}

pub type ResetError {
  /// A mail sending transaction failed, we tried to reset but that failed
  ///
  /// i.e. stop and recreate the `Mailer`
  ///
  FailedToResetSmtpError(original_error: SmtpError, reset_error: String)
}

pub opaque type Mailer {
  Smtp(socket: mug.Socket)
}

pub type Auth {
  Auth(username: String, password: String)
}

pub type TlsStance {
  AllowNonTls
  RejectNonTls
}

/// send a message through a given mailer
///
pub fn send(mailer: Mailer, message: Message) -> Result(Nil, Error) {
  case mailer {
    Smtp(socket) ->
      send_smtp(socket, message)
      |> result.try_recover(reset_smtp(_, socket))
  }
}

/// stop a given mailer
/// 
pub fn stop(mailer: Mailer) -> Result(Nil, Error) {
  case mailer {
    Smtp(socket) -> {
      // close the socket
      use _ <- result.try(socket_send(socket, "QUIT"))
      use _ <- result.try(socket_receive(socket))

      mug.shutdown(socket)
      |> result.map_error(FailedToClose)
      |> result.map_error(SocketError)
    }
  }
}

// smtp specific ----------------------------------------------------------------

/// Start a new smtp mailer
/// > Make sure to stop it using `gcourier.stop` once you are done with it
///
/// Connects to the server using the supplied host, port and auth
/// 
/// We always try to upgrade to TLS. 
/// The `TlsStance` is used to decide what to do when the server doesnt support TLS
/// - `AllowNonTls` -> will simply continue without TLS
/// - `RejectNonTls` -> will return `Error(ServerDoesntSupportTls)`
///
pub fn start_smtp(
  host host: String,
  port port: Int,
  auth auth: Option(Auth),
  tls tls: TlsStance,
) -> Result(Mailer, Error) {
  use socket <- result.try(
    mug.new(host, port)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()
    |> result.map_error(FailedToConnect)
    |> result.map_error(SocketError),
  )

  use resp <- result.try(socket_receive(socket))

  let ehlo = case string.contains(resp, "ESMTP") {
    True -> socket_send(socket, "EHLO " <> host)
    False -> socket_send(socket, "HELO " <> host)
  }
  use _ <- result.try(ehlo)

  use helo_resp <- result.try(socket_receive(socket))

  let ehlo = case string.contains(helo_resp, "STARTTLS") {
    False -> {
      case tls {
        // if the user is ok with not having tls, so be it
        AllowNonTls -> #(helo_resp, socket) |> Ok
        // otherwise error
        RejectNonTls -> Error(SmtpError(ServerDoesntSupportTls))
      }
    }
    True -> {
      use _ <- result.try(socket_send(socket, "STARTTLS"))
      use _ <- result.try(socket_receive(socket))

      use socket <- result.try(
        // TODO: proper cert verification?
        mug.upgrade(socket, mug.DangerouslyDisableVerification, 10_000)
        |> result.map_error(FailedToUpgrade)
        |> result.map_error(SocketError),
      )
      use _ <- result.try(socket_send(socket, "EHLO " <> host))
      use resp <- result.try(socket_receive(socket))
      #(resp, socket) |> Ok
    }
  }
  use #(helo_resp, socket) <- result.try(ehlo)

  use _ <- result.try(case auth {
    None -> Ok(Nil)
    Some(auth) -> auth_user(socket, auth, helo_resp)
  })

  Ok(Smtp(socket))
}

fn auth_user(
  socket: mug.Socket,
  auth: Auth,
  helo_resp: String,
) -> Result(Nil, Error) {
  // return Error if helo doesnt contain auth i.e. the server doesnt support it
  // it is up to the user of the library to decide how to deal with that
  use <- bool.guard(
    when: !string.contains(helo_resp, "AUTH"),
    return: Error(SmtpError(ServerDoesntSupportAuthentication)),
  )

  use _ <- result.try(socket_send(socket, "AUTH LOGIN"))
  use _ <- result.try(socket_receive(socket))
  // todo: check resp
  use _ <- result.try(socket_send(
    socket,
    auth.username
      |> bit_array.from_string()
      |> bit_array.base64_encode(True),
  ))

  use _ <- result.try(socket_receive(socket))
  use _ <- result.try(socket_send(
    socket,
    auth.password
      |> bit_array.from_string()
      |> bit_array.base64_encode(True),
  ))
  use _ <- result.try(socket_receive(socket))
  Ok(Nil)
}

/// reset a failed smtp transaction
///
fn reset_smtp(error: Error, socket: mug.Socket) -> Result(Nil, Error) {
  case error {
    // Something went wrong during the transaction;
    // maybe RSET will get the connection back to a usable state
    // 
    SmtpError(error) -> {
      use _ <- result.try(socket_send(socket, "RSET"))

      use resp <- result.try(socket_receive(socket))
      case string.contains(resp, "250") {
        True -> Error(FailedToSendButResetOk)
        False -> Error(ResetError(FailedToResetSmtpError(error, resp)))
      }
    }

    // something went wrong with the socket 
    // theres not much we can do here
    //
    SocketError(_) -> Error(error)

    // and these last two dont happen
    FailedToSendButResetOk -> Error(error)
    ResetError(_) -> Error(error)
  }
}

/// send a message via smtp through a given mug socket
/// 
fn send_smtp(socket: mug.Socket, msg: Message) -> Result(Nil, Error) {
  let from_cmd = "MAIL FROM:<" <> msg.data.from.address <> ">"
  use _ <- result.try(socket_send(socket, from_cmd))

  use resp <- result.try(socket_receive(socket))
  use <- bool.guard(
    when: !string.contains(resp, "250"),
    return: Error(SmtpError(FailedToSendStart(resp))),
  )

  // TODO: cc & bcc
  let rcpt =
    list.map(msg.data.to, fn(recipient) {
      let to_cmd = "RCPT TO:<" <> recipient.address.address <> ">"
      use _ <- result.try(socket_send(socket, to_cmd))
      socket_receive(socket)
    })
    |> result.all()
  use _ <- result.try(rcpt)

  // start message data 
  // i.e. everything from here until <CRLF>.<CRLF> is the content of the message
  // and no response will be sent until that 
  use _ <- result.try(socket_send(socket, "DATA"))
  use resp <- result.try(socket_receive(socket))

  // code 354 -> ok, send data
  use <- bool.guard(
    when: !string.contains(resp, "354"),
    return: Error(SmtpError(NotAllowedToSendData(resp))),
  )

  // send the message
  let message = render(msg)
  use _ <- result.try(socket_send(socket, message))

  // tell the server this message is done
  use _ <- result.try(socket_send(socket, "\r\n."))
  use resp <- result.try(socket_receive(socket))

  // 250 -> OK, transaction finished
  use <- bool.guard(
    when: !string.contains(resp, "250"),
    return: Error(SmtpError(FailedToFinishTransaction(resp))),
  )

  Ok(Nil)
}

/// sends a given message *with* "\r\n" appended
///
fn socket_send(socket: mug.Socket, value: String) -> Result(Nil, Error) {
  mug.send(socket, <<{ value <> "\r\n" }:utf8>>)
  |> result.map_error(FailedToSend(value, _))
  |> result.map_error(SocketError)
}

/// receive one message from the socket
/// and try to turn it into a string
///
fn socket_receive(socket: mug.Socket) -> Result(String, Error) {
  use packet <- result.try(
    mug.receive(socket, 5000)
    |> result.map_error(FailedToReceive)
    |> result.map_error(SocketError),
  )
  bit_array.to_string(packet)
  |> result.replace_error(SocketError(InvalidUtf8Response(packet)))
}
