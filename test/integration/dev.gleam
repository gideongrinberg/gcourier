import gcourier
import gleam/option.{Some}

pub fn main() {
  test_regular()
}

fn test_regular() {
  let message =
    gcourier.new_message(
      gcourier.Address(Some("The Fun Club 🎉"), "party@funclub.org"),
      gcourier.Address(Some("Jane Doe"), "jane.doe@example.com"),
    )
    |> gcourier.add_recipient(
      gcourier.Cc(gcourier.Address(Some("John Doe"), "john.doe@example.net")),
    )
    |> gcourier.set_subject("You're Invited: Pizza & Ping Pong Night!")
    |> gcourier.set_content(gcourier.Html(
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
    ))

  // start a new smtp mailer
  let assert Ok(mailer) =
    gcourier.start_smtp(
      "localhost",
      1025,
      // Some(gcourier.Auth("user1", "password1")),
      option.None,
      gcourier.AllowNonTls,
    )
    as "did you start mailpit?"

  // Send the email
  // Navigate to localhost:8025 to view it in the browser.
  let assert Ok(_) = gcourier.send(mailer, message)

  // stop the mailer
  let assert Ok(_) = gcourier.stop(mailer)
}
