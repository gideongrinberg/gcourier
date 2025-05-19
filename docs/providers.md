`gcourier` supports any mail provider that offers SMTP (which includes the vast majority of providers). We **do not support SMTPS** (implicit TLS, typically on port 465), but we **do support STARTTLS** (explicit TLS upgrade). If your provider offers multiple ports, you should generally choose **port 587**. If 587 is not available, try **port 25** or **2525**.

Most consumer email providers (like Gmail, Outlook, Yahoo) require you to generate an **app-specific password**. Others (like Mailgun or Amazon SES) will provide you with SMTP credentials.

The username you pass to `smtp.send` should be the SMTP credential provided by your email provider—not necessarily the email address you're sending from.

If you are using a provider not documented here and encounter issues, please feel free to open an issue, and we will try to provide support. We would also appreciate it if you update this document with any provider-specific idiosyncrasies you discover.

## Amazon SES

`gcourier` can send emails using Amazon SES's [SMTP interface](https://docs.aws.amazon.com/ses/latest/dg/send-email-smtp.html). You will need to create SMTP credentials by following [these instructions](https://docs.aws.amazon.com/ses/latest/dg/smtp-credentials.html). The SMTP endpoint will be `email.<region>.amazonaws.com` (for example, `email.us-east-1.amazonaws.com`), and the port will be 587. Do not use port 465 or 2465.

```gleam
let ses_username = "AKIAIOSFODNN7EXAMPLE"
let password = "BcD12345EXAMPLEencryptedKey=="

let msg = message.build() |> message.set_from("jeff@amazon.com") // etc...

smtp.send("email.us-east-1.amazonaws.com", 587, Some(#(email, password)), msg)
```


## Gmail/Outlook/Yahoo

`gcourier` does not support OAUTH, but it's still possible to use Gmail, Outlook, or Yahoo with an app-specific password. The instructions for creating an app-specific password:

- [Gmail](https://myaccount.google.com/apppasswords)
- [Outlook](https://support.microsoft.com/en-us/account-billing/how-to-get-and-use-app-passwords-5896ed9b-4263-e681-128a-a6f2979a7944)
- [Yahoo](https://help.yahoo.com/kb/SLN15241.html)

Then, you can send emails as follows:

```gleam
let email = "me@gmail.com" // or me@outlook.com, me@yahoo.com
let password = "APP_SPECIFIC_PASSWORD"

smtp.send("SMTP_SERVER", 587, Some(#(email, password)), msg)
```

Replace `SMTP_SERVER` with the correct one for your provider:

| Provider | SMTP Server             | Port |
|----------|-------------------------|------|
| Gmail    | smtp.gmail.com          | 587  |
| Outlook  | smtp.outlook.com        | 587  |
| Yahoo    | smtp.mail.yahoo.com     | 587  |


These instructions should be similar for most other providers like Zoho and Fastmail.