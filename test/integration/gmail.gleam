import gcourier/message
import gcourier/smtp
// Note: gleam/erlang removed get_line in v1.0.0
import gleam/option.{None, Some}
import gleam/string

pub fn main() {
  let sender_email = input("Enter sender's gmail address: ")
  let sender_password = input("Enter sender's gmail password: ")
  let recipient_email = input("Enter recipient's email: ")
  let subject = input("Subject: ")
  let body = input("Body: ")
  let attach = input("Attach file? (y/N)")

  let msg =
    message.build()
    |> message.set_from(sender_email, None)
    |> message.add_recipient(recipient_email, message.To)
    |> message.set_subject(subject)
    |> message.set_text(body)

  let msg = case attach {
    "y" ->
      msg |> message.add_attachment("./README.md", "README.md", "text/markdown")

    _ -> msg
  }

  smtp.send("smtp.gmail.com", 587, Some(#(sender_email, sender_password)), msg)
}

// External function to get user input (replacement for removed erlang.get_line)
@external(erlang, "io", "get_line")
fn get_line(prompt: String) -> String

fn input(prompt: String) -> String {
  let input = get_line(prompt)
  string.trim_end(input)
}
