# gcourier

[![Package Version](https://img.shields.io/hexpm/v/gcourier)](https://hex.pm/packages/gcourier)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gcourier/)

`gcourier` provides a simple and easy-to-use interface for sending emails from Gleam.

```sh
gleam add gcourier@1.3.0
```

```gleam
import gcourier
import gcourier/message
import gcourier/smtp
import gleam/erlang/process
import gleam/option.{Some}

pub fn main() {
  let message =
    gcourier.new_message(gcourier.Sender(Address("The Fun Club 🎉"), "party@funclub.org"))
    |> gcourier.add_recipient(gcourier.To("jane.doe@example.com"))
    |> gcourier.add_recipient(gcourier.Cc("john.doe@example.net"))
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
      Some(gcourier.Auth("user1", "password1")),
      gcourier.AllowNonTls,
    )

  // Send the email
  // Navigate to localhost:8025 to view it in the browser.
  let assert Ok(_) = gcourier.send(mailer, message)

  // stop the mailer
  let assert Ok(_) = gcourier.stop(mailer)

 
  process.sleep_forever()
}
```

Further documentation can be found at <https://hexdocs.pm/gcourier>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

### SMTP test server

If you need an smtp server to test against, or have previously used `gcourier.dev_server` consider [mailpit](https://github.com/axllent/mailpit)
