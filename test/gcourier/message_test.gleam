import birdie
import gcourier
import gleam/option.{None, Some}
import gleam/time/calendar
import gleam/time/timestamp
import gleeunit/should

fn consistent_time(message: gcourier.Message) -> gcourier.Message {
  message
  |> gcourier.set_timestamp(timestamp.from_calendar(
    calendar.Date(year: 2026, month: calendar.April, day: 14),
    calendar.TimeOfDay(hours: 20, minutes: 26, seconds: 23, nanoseconds: 1),
    calendar.utc_offset,
  ))
}

pub fn minimal_message_test() -> Nil {
  gcourier.new_message(
    gcourier.Address(Some("Test User"), "test@example.com"),
    gcourier.Address(None, "test1@example.com"),
  )
  |> consistent_time()
  |> gcourier.render()
  |> birdie.snap("from: Test User <test@example.com> | to: <test1@example.com>")
}

pub fn sender_header_test() -> Nil {
  gcourier.new_message(
    gcourier.Address(Some("Test User"), "test@example.com"),
    gcourier.Address(None, "test1@example.com"),
  )
  |> consistent_time()
  |> gcourier.set_sender(gcourier.Address(
    Some("Sender Name"),
    "sender@example.com",
  ))
  |> gcourier.render()
  |> birdie.snap("Sender: Sender Name <sender@example.com>")
}

pub fn recipients_test() -> Nil {
  gcourier.new_message(
    gcourier.Address(None, "from@example.com"),
    gcourier.Address(None, "to@example.com"),
  )
  |> gcourier.add_recipient(
    gcourier.Cc(gcourier.Address(None, "cc@example.com")),
  )
  |> gcourier.add_recipient(
    gcourier.Bcc(gcourier.Address(None, "bcc@example.com")),
  )
  |> gcourier.add_recipient(
    gcourier.To(gcourier.Address(None, "to2@example.com")),
  )
  |> gcourier.add_recipient(
    gcourier.Cc(gcourier.Address(None, "cc2@example.com")),
  )
  |> gcourier.add_recipient(
    gcourier.Bcc(gcourier.Address(None, "bcc2@example.com")),
  )
  |> consistent_time()
  |> gcourier.render()
  |> birdie.snap("to, cc, bcc, to2, cc2, bcc2")
}

pub fn subject_test() -> Nil {
  gcourier.new_message(
    gcourier.Address(None, "from@example.com"),
    gcourier.Address(None, "test1@example.com"),
  )
  |> gcourier.set_subject("Test Subject")
  |> consistent_time()
  |> gcourier.render()
  |> birdie.snap("Test Subject")
}

pub fn content_type_test() -> Nil {
  gcourier.new_message(
    gcourier.Address(None, "from@example.com"),
    gcourier.Address(None, "test1@example.com"),
  )
  |> consistent_time()
  |> gcourier.render()
  |> birdie.snap("No content, Content-Type 'text/plain'")

  gcourier.new_message(
    gcourier.Address(None, "from@example.com"),
    gcourier.Address(None, "test1@example.com"),
  )
  |> gcourier.set_content(gcourier.Text("Test text"))
  |> consistent_time()
  |> gcourier.render()
  |> birdie.snap("Content 'Test text', Content-Type 'text/plain'")

  gcourier.new_message(
    gcourier.Address(None, "from@example.com"),
    gcourier.Address(None, "test1@example.com"),
  )
  |> gcourier.set_content(gcourier.Html("<p>Test HTML</p>"))
  |> consistent_time()
  |> gcourier.render()
  |> birdie.snap("Content '<p>Test HTML</p>', Content-Type 'text/html'")
}

pub fn custom_header_test() -> Nil {
  gcourier.new_message(
    gcourier.Address(Some("Test User"), "test@example.com"),
    gcourier.Address(None, "test1@example.com"),
  )
  |> gcourier.add_custom_header(gcourier.CustomHeader(
    "MyCustomHeader",
    "MyCustomHeaderValue",
  ))
  |> gcourier.add_custom_header(gcourier.CustomHeader(
    "MyOtherCustomHeader",
    "MyOtherCustomHeaderValue",
  ))
  |> consistent_time()
  |> gcourier.render()
  |> birdie.snap("MyCustomHeader and MyOtherCustomHeader")
}

pub fn week_day_test() -> Nil {
  // Basic tests for each day of the week
  gcourier.day_of_week(10, 5, 2025) |> should.equal("Sat")
  gcourier.day_of_week(11, 5, 2025) |> should.equal("Sun")
  gcourier.day_of_week(12, 5, 2025) |> should.equal("Mon")
  gcourier.day_of_week(13, 5, 2025) |> should.equal("Tue")
  gcourier.day_of_week(14, 5, 2025) |> should.equal("Wed")
  gcourier.day_of_week(15, 5, 2025) |> should.equal("Thu")
  gcourier.day_of_week(16, 5, 2025) |> should.equal("Fri")

  // Month boundaries
  gcourier.day_of_week(1, 1, 2025) |> should.equal("Wed")
  gcourier.day_of_week(31, 1, 2025) |> should.equal("Fri")
  gcourier.day_of_week(1, 2, 2025) |> should.equal("Sat")
  gcourier.day_of_week(28, 2, 2025) |> should.equal("Fri")

  // Leap year tests
  gcourier.day_of_week(29, 2, 2024) |> should.equal("Thu")
  gcourier.day_of_week(1, 3, 2024) |> should.equal("Fri")
  gcourier.day_of_week(1, 3, 2025) |> should.equal("Sat")

  // Various months and years
  gcourier.day_of_week(4, 7, 1776) |> should.equal("Thu")
  gcourier.day_of_week(25, 12, 2024) |> should.equal("Wed")
  gcourier.day_of_week(31, 10, 2025) |> should.equal("Fri")
  gcourier.day_of_week(11, 11, 2025) |> should.equal("Tue")

  // Historical dates
  gcourier.day_of_week(1, 1, 2000) |> should.equal("Sat")
  gcourier.day_of_week(30, 6, 1969) |> should.equal("Mon")
  gcourier.day_of_week(20, 7, 1969) |> should.equal("Sun")
  gcourier.day_of_week(21, 12, 2012) |> should.equal("Fri")

  // Edge cases
  gcourier.day_of_week(29, 2, 2000) |> should.equal("Tue")
  gcourier.day_of_week(31, 12, 1999) |> should.equal("Fri")
  gcourier.day_of_week(1, 1, 1900) |> should.equal("Mon")
}
