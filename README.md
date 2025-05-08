# gcourier

[![Package Version](https://img.shields.io/hexpm/v/gcourier)](https://hex.pm/packages/gcourier)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gcourier/)

`gcourier` provides a simple and easy-to-use interface for sending emails from Gleam.

```sh
gleam add gcourier@1
```
```gleam
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

  let mailer =
    SmtpMailer(
      domain: "localhost",
      port: 1025,
      username: "",
      password: "",
      ssl: False,
      auth: False,
    )

  mailer.send(mailer, message)
}
```

Further documentation can be found at <https://hexdocs.pm/gcourier>.

## Roadmap

This library is not production ready.

- [x] Send emails over SMTP
- [ ] Implement ESMTP features (auth, TLS, etc)  
- [ ] Add robust error handling
- [ ] Write comprehensive tests
- [ ] Add other `Mailer` transports (SES, sendgrid, etc.)
- [ ] Provide a built-in dev server for testing

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
