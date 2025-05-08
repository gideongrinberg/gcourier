import gcourier/mailer
import gcourier/message
import gcourier/types.{SmtpMailer}
import gleam/erlang/process
import gleam/option.{Some}

pub fn main() {
  // Spin up the built-in SMTP server
  mailer.dev_server()
  let mailer =
    SmtpMailer(
      domain: "localhost",
      port: 1025,
      username: "user1",
      password: "password1",
      ssl: False,
      auth: True,
    )

  // Compose the message
  let message =
    message.build()
    |> message.set_from("party@funclub.org", Some("The Fun Club 🎉"))
    |> message.add_recipient("jane.doe@example.com", message.To)
    |> message.add_recipient("john.doe@example.net", message.CC)
    |> message.set_subject("You're Invited: Pizza & Ping Pong Night!")
    |> message.set_html(
      "
        <html>
            <body>
                <h1 style='color:tomato;'>🎈 You're Invited! 🎈</h1>
                <p>Hey friend,</p>
                <p>We're hosting a <strong>Pizza & Ping Pong Night</strong> this Friday at 7 PM. 
                Expect good vibes, cheesy slices, and fierce paddle battles!</p>
                <p>Let us know if you're in. And bring your A-game. 🏓</p>
                <p>Cheers,<br/>The Fun Club</p>
            </body>
        </html>
    ",
    )

  // Send the message
  mailer.send(mailer, message)

  // Keep the server alive – navigate to https://localhost:8025 to view the email!
  process.sleep_forever()
}
