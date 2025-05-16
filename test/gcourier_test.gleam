import gcourier/message
import gcourier/smtp
import gleam/io
import gleam/option.{Some}
import gleeunit

pub fn main() {
  integration_test()
  gleeunit.main()
}

fn integration_test() {
  // starts an SMTP server that captures and displays emails.
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

  smtp.send("localhost", 1025, Some(#("user1", "password1")), message)
  io.println("Navigate to localhost:8025 to view the test email.")
}
