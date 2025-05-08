pub type Mailer {
  SmtpMailer(
    domain: String,
    port: Int,
    username: String,
    password: String,
    ssl: Bool,
    auth: Bool,
  )
}
