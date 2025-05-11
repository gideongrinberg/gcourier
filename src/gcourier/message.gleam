//// This module provides tools for constructing RFC-compliant email messages.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/time/calendar
import gleam/time/duration
import gleam/time/timestamp

pub type Message {
  Message(
    headers: Dict(String, String),
    to: List(String),
    cc: List(String),
    bcc: List(String),
    content: String,
  )
}

pub type RecipientType {
  To
  CC
  BCC
}

pub fn build() -> Message {
  Message(dict.from_list([]), [], [], [], "")
}

pub fn render(message: Message) {
  let headers =
    get_headers(message)
    |> list.map(fn(header) { header.0 <> ": " <> header.1 })
    |> string.join("\r\n")

  headers <> "\r\n" <> message.content <> "\r\n."
}

fn get_headers(message: Message) -> List(#(String, String)) {
  let get = fn(name) { dict.get(message.headers, name) }
  let optional = fn(name) {
    case get(name) {
      Ok(val) if val != "" -> Some(#(name, val))
      _ -> None
    }
  }

  // Required fields have defaults
  let required = fn(name, default) {
    case get(name) {
      Ok(val) if val != "" -> Some(#(name, val))
      _ -> Some(#(name, default))
    }
  }

  // Mandatory fields must be provided by the user
  let mandatory = fn(name) {
    case get(name) {
      Ok(val) if val != "" -> Some(#(name, val))
      _ -> panic as { "Missing required header: " <> name }
    }
  }

  let recipient_field = fn(message: Message, recipient_type: RecipientType) {
    case recipient_type {
      CC ->
        case message.cc {
          [] -> None
          _ -> Some(#("Cc", string.join(message.cc, ", ")))
        }

      To ->
        case message.to {
          [] -> panic as "Missing to field"
          _ -> Some(#("To", string.join(message.to, ", ")))
        }

      _ -> None
    }
  }

  list.filter(
    [
      required("Date", current_date()),
      mandatory("From"),
      recipient_field(message, To),
      optional("Sender"),
      recipient_field(message, CC),
      optional("Subject"),
      required("Content-Type", "text/plain"),
    ],
    fn(a) {
      case a {
        None -> False
        Some(_) -> True
      }
    },
  )
  |> list.map(fn(a) {
    case a {
      Some(field) -> field
      None -> panic
    }
  })
}

// Header setting functions

/// Set the FROM header in the email.
pub fn set_from(
  message: Message,
  sender_address address: String,
  sender_name name: Option(String),
) {
  message |> set_header("From", format_address(address, name))
}

/// Add the provided address to the list of recipients.
/// 
/// recipient_type should be one of To, Cc, or Bcc.
pub fn add_recipient(
  message: Message,
  email: String,
  recipient_type: RecipientType,
) {
  case recipient_type {
    To -> Message(..message, to: [email, ..message.to])
    CC -> Message(..message, cc: [email, ..message.cc])
    BCC -> Message(..message, bcc: [email, ..message.bcc])
  }
}

/// Set the message's subject line. Optional.
pub fn set_subject(message: Message, subject: String) {
  message |> set_header("Subject", subject)
}

/// Set the _optional_ sender header. Prefer FROM in most cases.
/// 
/// This field is useful when the email is sent on behalf of 
/// a third party or there are multiple emails in the FROM field.
pub fn set_sender(
  message: Message,
  sender_address address: String,
  sender_name name: Option(String),
) {
  message |> set_header("Sender", format_address(address, name))
}

/// Set the Date header for the email. Optional.
///
/// If this is not explicitly set, the current system time will be used automatically
/// when the message is sent. This header indicates when the email was created.
pub fn set_date(message: Message, date: String) {
  message |> set_header("Date", date)
}

// Content functions

pub fn set_html(message: Message, html: String) {
  message
  |> set_header("Content-Type", "text/html")
  |> set_content(html)
}

pub fn set_text(message: Message, text: String) {
  message
  |> set_header("Content-Type", "text/plain")
  |> set_content(text)
}

fn set_content(message: Message, text: String) {
  Message(..message, content: text)
}

// Utility functions

fn set_header(message: Message, name: String, value: String) {
  Message(..message, headers: dict.insert(message.headers, name, value))
}

fn format_address(address: String, name: Option(String)) {
  case name {
    Some(name) -> {
      name <> " <" <> address <> ">"
    }
    None -> address
  }
}

@internal
pub fn date_from_cal(date cal: calendar.Date, time time: calendar.TimeOfDay) {
  let month = {
    calendar.month_to_string(cal.month) |> string.slice(0, 3)
  }

  let offset = float.round(duration.to_seconds(calendar.local_offset()))
  let offset_sign = case offset > 0 {
    True -> "+"
    False -> ""
  }

  let offset_hours =
    { offset / 3600 } |> int.to_string |> string.pad_start(2, "0")
  let offset_minutes =
    { { offset % 3600 } / 60 } |> int.to_string |> string.pad_start(2, "0")

  day_of_week(cal.day, calendar.month_to_int(cal.month), cal.year)
  <> ", "
  <> int.to_string(cal.day)
  <> " "
  <> month
  <> " "
  <> int.to_string(cal.year)
  <> " "
  <> int.to_string(time.hours)
  <> ":"
  <> int.to_string(time.minutes)
  <> ":"
  <> int.to_string(time.seconds)
  <> " "
  <> offset_sign
  <> offset_hours
  <> ":"
  <> offset_minutes
}

@internal
pub fn day_of_week(day q: Int, month m: Int, year y: Int) {
  let y = case m < 3 {
    True -> y - 1
    False -> y
  }

  let m = case m < 3 {
    True -> m + 12
    False -> m
  }

  let k = y % 100
  let j = y / 100

  let h = q + { 13 * { m + 1 } / 5 } + k + k / 4 + j / 4 + 5 * j
  case h % 7 {
    0 -> "Sat"
    1 -> "Sun"
    2 -> "Mon"
    3 -> "Tue"
    4 -> "Wed"
    5 -> "Thu"
    6 -> "Fri"
    _ -> panic
  }
}

fn current_date() {
  let now =
    timestamp.system_time() |> timestamp.to_calendar(duration.seconds(0))

  date_from_cal(now.0, now.1)
}
