import gcourier/mailer
import gcourier/message
import gcourier/types.{SmtpMailer}
import gleam/option.{Some}

pub fn main() {
  let message =
    message.build()
    |> message.set_from("party@funclub.org", Some("The Fun Club 🎉"))
    |> message.add_recipient("jane.doe@example.com", message.To)
    |> message.add_recipient("john.doe@example.net", message.CC)
    |> message.set_subject("You're Invited: Pizza & Ping Pong Night!")

  let mail =
    SmtpMailer(
      domain: "localhost",
      port: 1025,
      username: "",
      password: "",
      ssl: False,
      auth: False,
    )

  mailer.send(mail, message)
}
