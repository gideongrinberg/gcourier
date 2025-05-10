import gcourier/message
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

pub fn basic_build_test() {
  let msg = message.build()

  msg.headers |> dict.size |> should.equal(0)
  msg.to |> list.is_empty |> should.be_true
  msg.cc |> list.is_empty |> should.be_true
  msg.bcc |> list.is_empty |> should.be_true
  msg.content |> should.equal("")
}

pub fn from_address_test() {
  let msg =
    message.build() |> message.set_from("test@example.com", Some("Test User"))

  let assert Ok(from) = dict.get(msg.headers, "From")
  from |> should.equal("Test User <test@example.com>")

  let msg = message.build() |> message.set_from("test@example.com", None)

  let assert Ok(from) = dict.get(msg.headers, "From")
  from |> should.equal("test@example.com")
}

pub fn sender_header_test() {
  let msg =
    message.build()
    |> message.set_sender("sender@example.com", Some("Sender Name"))

  dict.get(msg.headers, "Sender")
  |> should.equal(Ok("Sender Name <sender@example.com>"))
}

pub fn recipients_test() {
  let msg =
    message.build()
    |> message.add_recipient("to@example.com", message.To)
    |> message.add_recipient("cc@example.com", message.CC)
    |> message.add_recipient("bcc@example.com", message.BCC)

  msg.to |> should.equal(["to@example.com"])
  msg.cc |> should.equal(["cc@example.com"])
  msg.bcc |> should.equal(["bcc@example.com"])

  let msg =
    msg
    |> message.add_recipient("to2@example.com", message.To)
    |> message.add_recipient("cc2@example.com", message.CC)
    |> message.add_recipient("bcc2@example.com", message.BCC)

  msg.to |> should.equal(["to2@example.com", "to@example.com"])
  msg.cc |> should.equal(["cc2@example.com", "cc@example.com"])
  msg.bcc |> should.equal(["bcc2@example.com", "bcc@example.com"])
}

pub fn subject_test() {
  let msg =
    message.build()
    |> message.set_subject("Test Subject")

  dict.get(msg.headers, "Subject")
  |> should.equal(Ok("Test Subject"))
}

pub fn content_type_test() {
  let msg = message.build()
  dict.get(msg.headers, "Content-Type") |> should.be_error()

  let text_msg =
    message.build()
    |> message.set_text("Test message")

  dict.get(text_msg.headers, "Content-Type")
  |> should.equal(Ok("text/plain"))

  let html_msg =
    message.build()
    |> message.set_html("<p>Test HTML</p>")

  dict.get(html_msg.headers, "Content-Type")
  |> should.equal(Ok("text/html"))

  html_msg.content
  |> should.equal("<p>Test HTML</p>")
}

pub fn render_headers_test() {
  let msg =
    message.build()
    |> message.set_from("from@example.com", None)
    |> message.add_recipient("to@example.com", message.To)
    |> message.set_subject("Test Subject")
    |> message.set_text("Hello world")

  let rendered = message.render(msg)

  let should_contain = fn(a: String, b: String) {
    should.be_true(string.contains(a, b))
  }

  rendered |> should_contain("From: from@example.com")
  rendered |> should_contain("To: to@example.com")
  rendered |> should_contain("Subject: Test Subject")
  rendered |> should_contain("Content-Type: text/plain")

  rendered |> should_contain("Hello world")
}
// TODO: missing from, missing to
