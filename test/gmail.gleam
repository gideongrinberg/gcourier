import gcourier/message
import gcourier/smtp
import gleam/erlang
import gleam/option.{None, Some}
import gleam/string

pub fn main() {
  let sender_email = input("Enter sender's gmail address: ")
  let sender_password = input("Enter sender's gmail password: ")
  let recipient_email = input("Enter recipient's email: ")
  let subject = input("Subject: ")
  let body = input("Body: ")

  let msg =
    message.build()
    |> message.set_from(sender_email, None)
    |> message.add_recipient(recipient_email, message.To)
    |> message.set_subject(subject)
    |> message.set_text(body)

  smtp.send("smtp.gmail.com", 587, Some(#(sender_email, sender_password)), msg)
}

fn input(prompt: String) -> String {
  let assert Ok(input) = erlang.get_line(prompt)
  string.trim_end(input)
}
