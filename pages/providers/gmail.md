## Gmail
You can use Gmail with gcourier by simply inputting your credentials. If you have two-factor authentication enabled, you'll need to generate an app-specific password by navigating to [this page](https://myaccount.google.com/apppasswords). Make sure to save the password. It won't be visible again!

Then, you can send emails as follows:

```gleam
let email = "me@gmail.com" // or me@outlook.com, me@yahoo.com
let password = "APP_SPECIFIC_PASSWORD"

smtp.send("smtp.gmail.com", 587, Some(#(email, password)), msg)
```