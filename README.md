# gcourier

[![Package Version](https://img.shields.io/hexpm/v/gcourier)](https://hex.pm/packages/gcourier)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gcourier/)

`gcourier` provides a simple and easy-to-use interface for sending emails from Gleam.

```sh
gleam add gcourier@1
```

```gleam
import gcourier
import gcourier/message
import gcourier/smtp
import gleam/erlang/process
import gleam/option.{Some}

pub fn main() {
  gcourier.dev_server() // starts an SMTP server that captures and displays emails.
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

  // Send the email
  // Navigate to localhost:8025 to view it in the browser.
  smtp.send("localhost", 1025, Some(#("user1", "password1")), message)
  process.sleep_forever()
}
```

Further documentation can be found at <https://hexdocs.pm/gcourier>.

## Features

- [x] Send emails over SMTP
- [x] Implement SMTP authentication  
- [ ] Add support for TLS and OAUTH2
- [ ] Bundle an SMTP server for development
- [ ] Add support for additional transports like sendmail and SES
- [ ] Improve test coverage and error handling

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
