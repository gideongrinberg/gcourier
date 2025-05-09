/// This module provides the logic for sending mail using a [`Mailer`](/gcourier/gcourier/types.html#Mailer).
/// As of writing, the library implements only one `Mailer`, which is `SmtpMailer`.
import gcourier/message.{type Message}
import gcourier/transports/smtp.{send_smtp}
import gcourier/types.{type Mailer, SmtpMailer}

pub fn send(mailer: Mailer, message: Message) {
  case mailer {
    SmtpMailer(_, _, _, _, _, _) -> {
      send_smtp(mailer, message)
    }
  }
}
