## Amazon SES

`gcourier` can send emails using Amazon SES's [SMTP interface](https://docs.aws.amazon.com/ses/latest/dg/send-email-smtp.html). You will need to create SMTP credentials by following [these instructions](https://docs.aws.amazon.com/ses/latest/dg/smtp-credentials.html). The SMTP endpoint will be `email.<region>.amazonaws.com` (for example, `email.us-east-1.amazonaws.com`), and the port will be 587. Do not use port 465 or 2465.

```gleam
let ses_username = "AKIAIOSFODNN7EXAMPLE"
let password = "BcD12345EXAMPLEencryptedKey=="

let msg = message.build() |> message.set_from("jeff@amazon.com") // etc...

smtp.send("email.us-east-1.amazonaws.com", 587, Some(#(email, password)), msg)
```